/*
 *  linux/fs/tfs/balloc.c
 *
 * Copyright (C) 1992, 1993, 1994, 1995
 * Remy Card (card@masi.ibp.fr)
 * Laboratoire MASI - Institut Blaise Pascal
 * Universite Pierre et Marie Curie (Paris VI)
 *
 *  Enhanced block allocation by Stephen Tweedie (sct@redhat.com), 1993
 *  Big-endian to little-endian byte-swapping/bitmaps by
 *        David S. Miller (davem@caip.rutgers.edu), 1995
 */

#include <linux/config.h>
#include "tfs.h"
#include <linux/quotaops.h>
#include <linux/sched.h>
#include <linux/buffer_head.h>

/*
 * balloc.c contains the blocks allocation and deallocation routines
 */

/*
 * The free blocks are managed by bitmaps.  A file system contains several
 * blocks groups.  Each group contains 1 bitmap block for blocks, 1 bitmap
 * block for inodes, N blocks for the inode table and data blocks.
 *
 * The file system contains group descriptors which are located after the
 * super block.  Each descriptor contains the number of the bitmap block and
 * the free blocks count in the block.  The descriptors are loaded in memory
 * when a file system is mounted (see tfs_read_super).
 */


#define in_range(b, first, len)	((b) >= (first) && (b) <= (first) + (len) - 1)

struct tfs_group_desc * tfs_get_group_desc(struct super_block * sb,
					     unsigned int block_group,
					     struct buffer_head ** bh)
{
	unsigned long group_desc;
	unsigned long offset;
	struct tfs_group_desc * desc;
	struct tfs_sb_info *sbi = TFS_SB(sb);

	if (block_group >= sbi->s_groups_count) {
		tfs_error (sb, "tfs_get_group_desc",
			    "block_group >= groups_count - "
			    "block_group = %d, groups_count = %lu",
			    block_group, sbi->s_groups_count);

		return NULL;
	}

	group_desc = block_group >> TFS_DESC_PER_BLOCK_BITS(sb);
	offset = block_group & (TFS_DESC_PER_BLOCK(sb) - 1);
	if (!sbi->s_group_desc[group_desc]) {
		tfs_error (sb, "tfs_get_group_desc",
			    "Group descriptor not loaded - "
			    "block_group = %d, group_desc = %lu, desc = %lu",
			     block_group, group_desc, offset);
		return NULL;
	}

	desc = (struct tfs_group_desc *) sbi->s_group_desc[group_desc]->b_data;
	if (bh)
		*bh = sbi->s_group_desc[group_desc];
	return desc + offset;
}

/*
 * Read the bitmap for a given block_group, reading into the specified 
 * slot in the superblock's bitmap cache.
 *
 * Return buffer_head on success or NULL in case of failure.
 */
static struct buffer_head *
read_block_bitmap(struct super_block *sb, unsigned int block_group)
{
	struct tfs_group_desc * desc;
	struct buffer_head * bh = NULL;
	
	desc = tfs_get_group_desc (sb, block_group, NULL);
	if (!desc)
		goto error_out;
	bh = sb_bread(sb, le32_to_cpu(desc->bg_block_bitmap));
	if (!bh)
		tfs_error (sb, "read_block_bitmap",
			    "Cannot read block bitmap - "
			    "block_group = %d, block_bitmap = %u",
			    block_group, le32_to_cpu(desc->bg_block_bitmap));
error_out:
	return bh;
}

/*
 * Set sb->s_dirt here because the superblock was "logically" altered.  We
 * need to recalculate its free blocks count and flush it out.
 */
static int reserve_blocks(struct super_block *sb, int count)
{
	struct tfs_sb_info *sbi = TFS_SB(sb);
	struct tfs_super_block *es = sbi->s_es;
	unsigned free_blocks;
	unsigned root_blocks;

	free_blocks = percpu_counter_read_positive(&sbi->s_freeblocks_counter);
	root_blocks = le32_to_cpu(es->s_r_blocks_count);

	if (free_blocks < count)
		count = free_blocks;

	if (free_blocks < root_blocks + count && !capable(CAP_SYS_RESOURCE) &&
	    sbi->s_resuid != current->fsuid &&
	    (sbi->s_resgid == 0 || !in_group_p (sbi->s_resgid))) {
		/*
		 * We are too close to reserve and we are not privileged.
		 * Can we allocate anything at all?
		 */
		if (free_blocks > root_blocks)
			count = free_blocks - root_blocks;
		else
			return 0;
	}

	percpu_counter_mod(&sbi->s_freeblocks_counter, -count);
	sb->s_dirt = 1;
	return count;
}

static void release_blocks(struct super_block *sb, int count)
{
	if (count) {
		struct tfs_sb_info *sbi = TFS_SB(sb);

		percpu_counter_mod(&sbi->s_freeblocks_counter, count);
		sb->s_dirt = 1;
	}
}

static int group_reserve_blocks(struct tfs_sb_info *sbi, int group_no,
	struct tfs_group_desc *desc, struct buffer_head *bh, int count)
{
	unsigned free_blocks;

	if (!desc->bg_free_blocks_count)
		return 0;

	spin_lock(sb_bgl_lock(sbi, group_no));
	free_blocks = le16_to_cpu(desc->bg_free_blocks_count);
	if (free_blocks < count)
		count = free_blocks;
	desc->bg_free_blocks_count = cpu_to_le16(free_blocks - count);
	spin_unlock(sb_bgl_lock(sbi, group_no));
	mark_buffer_dirty(bh);
	return count;
}

static void group_release_blocks(struct super_block *sb, int group_no,
	struct tfs_group_desc *desc, struct buffer_head *bh, int count)
{
	if (count) {
		struct tfs_sb_info *sbi = TFS_SB(sb);
		unsigned free_blocks;

		spin_lock(sb_bgl_lock(sbi, group_no));
		free_blocks = le16_to_cpu(desc->bg_free_blocks_count);
		desc->bg_free_blocks_count = cpu_to_le16(free_blocks + count);
		spin_unlock(sb_bgl_lock(sbi, group_no));
		sb->s_dirt = 1;
		mark_buffer_dirty(bh);
	}
}

/* Free given blocks, update quota and i_blocks field */
void tfs_free_blocks (struct inode * inode, unsigned long block,
		       unsigned long count)
{
	struct buffer_head *bitmap_bh = NULL;
	struct buffer_head * bh2;
	unsigned long block_group;
	unsigned long bit;
	unsigned long i;
	unsigned long overflow;
	struct super_block * sb = inode->i_sb;
	struct tfs_sb_info * sbi = TFS_SB(sb);
	struct tfs_group_desc * desc;
	struct tfs_super_block * es = sbi->s_es;
	unsigned freed = 0, group_freed;

	if (block < le32_to_cpu(es->s_first_data_block) ||
	    block + count < block ||
	    block + count > le32_to_cpu(es->s_blocks_count)) {
		tfs_error (sb, "tfs_free_blocks",
			    "Freeing blocks not in datazone - "
			    "block = %lu, count = %lu", block, count);
		goto error_return;
	}

	tfs_debug ("freeing block(s) %lu-%lu\n", block, block + count - 1);

do_more:
	overflow = 0;
	block_group = (block - le32_to_cpu(es->s_first_data_block)) /
		      TFS_BLOCKS_PER_GROUP(sb);
	bit = (block - le32_to_cpu(es->s_first_data_block)) %
		      TFS_BLOCKS_PER_GROUP(sb);
	/*
	 * Check to see if we are freeing blocks across a group
	 * boundary.
	 */
	if (bit + count > TFS_BLOCKS_PER_GROUP(sb)) {
		overflow = bit + count - TFS_BLOCKS_PER_GROUP(sb);
		count -= overflow;
	}
	brelse(bitmap_bh);
	bitmap_bh = read_block_bitmap(sb, block_group);
	if (!bitmap_bh)
		goto error_return;

	desc = tfs_get_group_desc (sb, block_group, &bh2);
	if (!desc)
		goto error_return;

	if (in_range (le32_to_cpu(desc->bg_block_bitmap), block, count) ||
	    in_range (le32_to_cpu(desc->bg_inode_bitmap), block, count) ||
	    in_range (block, le32_to_cpu(desc->bg_inode_table),
		      sbi->s_itb_per_group) ||
	    in_range (block + count - 1, le32_to_cpu(desc->bg_inode_table),
		      sbi->s_itb_per_group))
		tfs_error (sb, "tfs_free_blocks",
			    "Freeing blocks in system zones - "
			    "Block = %lu, count = %lu",
			    block, count);

	for (i = 0, group_freed = 0; i < count; i++) {
		if (!tfs_clear_bit_atomic(sb_bgl_lock(sbi, block_group),
						bit + i, bitmap_bh->b_data)) {
			tfs_error(sb, __FUNCTION__,
				"bit already cleared for block %lu", block + i);
		} else {
			group_freed++;
		}
	}

	mark_buffer_dirty(bitmap_bh);
	if (sb->s_flags & MS_SYNCHRONOUS)
		sync_dirty_buffer(bitmap_bh);

	group_release_blocks(sb, block_group, desc, bh2, group_freed);
	freed += group_freed;

	if (overflow) {
		block += count;
		count = overflow;
		goto do_more;
	}
error_return:
	brelse(bitmap_bh);
	release_blocks(sb, freed);
	DQUOT_FREE_BLOCK(inode, freed);
}

static int grab_block(spinlock_t *lock, char *map, unsigned size, int goal)
{
	int k;
	char *p, *r;

	if (!tfs_test_bit(goal, map))
		goto got_it;

repeat:
	if (goal) {
		/*
		 * The goal was occupied; search forward for a free 
		 * block within the next XX blocks.
		 *
		 * end_goal is more or less random, but it has to be
		 * less than TFS_BLOCKS_PER_GROUP. Aligning up to the
		 * next 64-bit boundary is simple..
		 */
		k = (goal + 63) & ~63;
		goal = tfs_find_next_zero_bit(map, k, goal);
		if (goal < k)
			goto got_it;
		/*
		 * Search in the remainder of the current group.
		 */
	}

	p = map + (goal >> 3);
	r = memscan(p, 0, (size - goal + 7) >> 3);
	k = (r - map) << 3;
	if (k < size) {
		/* 
		 * We have succeeded in finding a free byte in the block
		 * bitmap.  Now search backwards to find the start of this
		 * group of free blocks - won't take more than 7 iterations.
		 */
		for (goal = k; goal && !tfs_test_bit (goal - 1, map); goal--)
			;
		goto got_it;
	}

	k = tfs_find_next_zero_bit ((u32 *)map, size, goal);
	if (k < size) {
		goal = k;
		goto got_it;
	}
	return -1;
got_it:
	if (tfs_set_bit_atomic(lock, goal, (void *) map)) 
		goto repeat;	
	return goal;
}

/*
 * tfs_new_block uses a goal block to assist allocation.  If the goal is
 * free, or there is a free block within 32 blocks of the goal, that block
 * is allocated.  Otherwise a forward search is made for a free block; within 
 * each block group the search first looks for an entire free byte in the block
 * bitmap, and then for any free bit if that fails.
 * This function also updates quota and i_blocks field.
 */
int tfs_new_block(struct inode *inode, unsigned long goal,
			u32 *prealloc_count, u32 *prealloc_block, int *err)
{
	struct buffer_head *bitmap_bh = NULL;
	struct buffer_head *gdp_bh;	/* bh2 */
	struct tfs_group_desc *desc;
	int group_no;			/* i */
	int ret_block;			/* j */
	int group_idx;			/* k */
	int target_block;		/* tmp */
	int block = 0;
	struct super_block *sb = inode->i_sb;
	struct tfs_sb_info *sbi = TFS_SB(sb);
	struct tfs_super_block *es = sbi->s_es;
	unsigned group_size = TFS_BLOCKS_PER_GROUP(sb);
	unsigned prealloc_goal = es->s_prealloc_blocks;
	unsigned group_alloc = 0, es_alloc, dq_alloc;
	int nr_scanned_groups;

	if (!prealloc_goal--)
		prealloc_goal = TFS_DEFAULT_PREALLOC_BLOCKS - 1;
	if (!prealloc_count || *prealloc_count)
		prealloc_goal = 0;

	if (DQUOT_ALLOC_BLOCK(inode, 1)) {
		*err = -EDQUOT;
		goto out;
	}

	while (prealloc_goal && DQUOT_PREALLOC_BLOCK(inode, prealloc_goal))
		prealloc_goal--;

	dq_alloc = prealloc_goal + 1;
	es_alloc = reserve_blocks(sb, dq_alloc);
	if (!es_alloc) {
		*err = -ENOSPC;
		goto out_dquot;
	}

	tfs_debug ("goal=%lu.\n", goal);

	if (goal < le32_to_cpu(es->s_first_data_block) ||
	    goal >= le32_to_cpu(es->s_blocks_count))
		goal = le32_to_cpu(es->s_first_data_block);
	group_no = (goal - le32_to_cpu(es->s_first_data_block)) / group_size;
	desc = tfs_get_group_desc (sb, group_no, &gdp_bh);
	if (!desc) {
		/*
		 * gdp_bh may still be uninitialised.  But group_release_blocks
		 * will not touch it because group_alloc is zero.
		 */
		goto io_error;
	}

	group_alloc = group_reserve_blocks(sbi, group_no, desc,
					gdp_bh, es_alloc);
	if (group_alloc) {
		ret_block = ((goal - le32_to_cpu(es->s_first_data_block)) %
					group_size);
		brelse(bitmap_bh);
		bitmap_bh = read_block_bitmap(sb, group_no);
		if (!bitmap_bh)
			goto io_error;
		
		tfs_debug("goal is at %d:%d.\n", group_no, ret_block);

		ret_block = grab_block(sb_bgl_lock(sbi, group_no),
				bitmap_bh->b_data, group_size, ret_block);
		if (ret_block >= 0)
			goto got_block;
		group_release_blocks(sb, group_no, desc, gdp_bh, group_alloc);
		group_alloc = 0;
	}

	tfs_debug ("Bit not found in block group %d.\n", group_no);

	/*
	 * Now search the rest of the groups.  We assume that 
	 * i and desc correctly point to the last group visited.
	 */
	nr_scanned_groups = 0;
retry:
	for (group_idx = 0; !group_alloc &&
			group_idx < sbi->s_groups_count; group_idx++) {
		group_no++;
		if (group_no >= sbi->s_groups_count)
			group_no = 0;
		desc = tfs_get_group_desc(sb, group_no, &gdp_bh);
		if (!desc)
			goto io_error;
		group_alloc = group_reserve_blocks(sbi, group_no, desc,
						gdp_bh, es_alloc);
	}
	if (!group_alloc) {
		*err = -ENOSPC;
		goto out_release;
	}
	brelse(bitmap_bh);
	bitmap_bh = read_block_bitmap(sb, group_no);
	if (!bitmap_bh)
		goto io_error;

	ret_block = grab_block(sb_bgl_lock(sbi, group_no), bitmap_bh->b_data,
				group_size, 0);
	if (ret_block < 0) {
		/*
		 * If a free block counter is corrupted we can loop inifintely.
		 * Detect that here.
		 */
		nr_scanned_groups++;
		if (nr_scanned_groups > 2 * sbi->s_groups_count) {
			tfs_error(sb, "tfs_new_block",
				"corrupted free blocks counters");
			goto io_error;
		}
		/*
		 * Someone else grabbed the last free block in this blockgroup
		 * before us.  Retry the scan.
		 */
		group_release_blocks(sb, group_no, desc, gdp_bh, group_alloc);
		group_alloc = 0;
		goto retry;
	}

got_block:
	tfs_debug("using block group %d(%d)\n",
		group_no, desc->bg_free_blocks_count);

	target_block = ret_block + group_no * group_size +
			le32_to_cpu(es->s_first_data_block);

	if (target_block == le32_to_cpu(desc->bg_block_bitmap) ||
	    target_block == le32_to_cpu(desc->bg_inode_bitmap) ||
	    in_range(target_block, le32_to_cpu(desc->bg_inode_table),
		      sbi->s_itb_per_group))
		tfs_error (sb, "tfs_new_block",
			    "Allocating block in system zone - "
			    "block = %u", target_block);

	if (target_block >= le32_to_cpu(es->s_blocks_count)) {
		tfs_error (sb, "tfs_new_block",
			    "block(%d) >= blocks count(%d) - "
			    "block_group = %d, es == %p ", ret_block,
			le32_to_cpu(es->s_blocks_count), group_no, es);
		goto io_error;
	}
	block = target_block;

	/* OK, we _had_ allocated something */
	tfs_debug("found bit %d\n", ret_block);

	dq_alloc--;
	es_alloc--;
	group_alloc--;

	/*
	 * Do block preallocation now if required.
	 */
	write_lock(&TFS_I(inode)->i_meta_lock);
	if (group_alloc && !*prealloc_count) {
		unsigned n;

		for (n = 0; n < group_alloc && ++ret_block < group_size; n++) {
			if (tfs_set_bit_atomic(sb_bgl_lock(sbi, group_no),
						ret_block,
						(void*) bitmap_bh->b_data))
 				break;
		}
		*prealloc_block = block + 1;
		*prealloc_count = n;
		es_alloc -= n;
		dq_alloc -= n;
		group_alloc -= n;
	}
	write_unlock(&TFS_I(inode)->i_meta_lock);

	mark_buffer_dirty(bitmap_bh);
	if (sb->s_flags & MS_SYNCHRONOUS)
		sync_dirty_buffer(bitmap_bh);

	tfs_debug ("allocating block %d. ", block);

	*err = 0;
out_release:
	group_release_blocks(sb, group_no, desc, gdp_bh, group_alloc);
	release_blocks(sb, es_alloc);
out_dquot:
	DQUOT_FREE_BLOCK(inode, dq_alloc);
out:
	brelse(bitmap_bh);
	return block;

io_error:
	*err = -EIO;
	goto out_release;
}

unsigned long tfs_count_free_blocks (struct super_block * sb)
{
	struct tfs_group_desc * desc;
	unsigned long desc_count = 0;
	int i;
#ifdef TFSFS_DEBUG
	unsigned long bitmap_count, x;
	struct tfs_super_block *es;

	lock_super (sb);
	es = TFS_SB(sb)->s_es;
	desc_count = 0;
	bitmap_count = 0;
	desc = NULL;
	for (i = 0; i < TFS_SB(sb)->s_groups_count; i++) {
		struct buffer_head *bitmap_bh;
		desc = tfs_get_group_desc (sb, i, NULL);
		if (!desc)
			continue;
		desc_count += le16_to_cpu(desc->bg_free_blocks_count);
		bitmap_bh = read_block_bitmap(sb, i);
		if (!bitmap_bh)
			continue;
		
		x = tfs_count_free(bitmap_bh, sb->s_blocksize);
		printk ("group %d: stored = %d, counted = %lu\n",
			i, le16_to_cpu(desc->bg_free_blocks_count), x);
		bitmap_count += x;
		brelse(bitmap_bh);
	}
	printk("tfs_count_free_blocks: stored = %lu, computed = %lu, %lu\n",
		(long)le32_to_cpu(es->s_free_blocks_count),
		desc_count, bitmap_count);
	unlock_super (sb);
	return bitmap_count;
#else
        for (i = 0; i < TFS_SB(sb)->s_groups_count; i++) {
                desc = tfs_get_group_desc (sb, i, NULL);
                if (!desc)
                        continue;
                desc_count += le16_to_cpu(desc->bg_free_blocks_count);
	}
	return desc_count;
#endif
}

static inline int
block_in_use(unsigned long block, struct super_block *sb, unsigned char *map)
{
	return tfs_test_bit ((block -
		le32_to_cpu(TFS_SB(sb)->s_es->s_first_data_block)) %
			 TFS_BLOCKS_PER_GROUP(sb), map);
}

static inline int test_root(int a, int b)
{
	int num = b;

	while (a > num)
		num *= b;
	return num == a;
}

static int tfs_group_sparse(int group)
{
	if (group <= 1)
		return 1;
	return (test_root(group, 3) || test_root(group, 5) ||
		test_root(group, 7));
}

/**
 *	tfs_bg_has_super - number of blocks used by the superblock in group
 *	@sb: superblock for filesystem
 *	@group: group number to check
 *
 *	Return the number of blocks used by the superblock (primary or backup)
 *	in this group.  Currently this will be only 0 or 1.
 */
int tfs_bg_has_super(struct super_block *sb, int group)
{
	if (TFS_HAS_RO_COMPAT_FEATURE(sb,TFS_FEATURE_RO_COMPAT_SPARSE_SUPER)&&
	    !tfs_group_sparse(group))
		return 0;
	return 1;
}

/**
 *	tfs_bg_num_gdb - number of blocks used by the group table in group
 *	@sb: superblock for filesystem
 *	@group: group number to check
 *
 *	Return the number of blocks used by the group descriptor table
 *	(primary or backup) in this group.  In the future there may be a
 *	different number of descriptor blocks in each group.
 */
unsigned long tfs_bg_num_gdb(struct super_block *sb, int group)
{
	if (TFS_HAS_RO_COMPAT_FEATURE(sb,TFS_FEATURE_RO_COMPAT_SPARSE_SUPER)&&
	    !tfs_group_sparse(group))
		return 0;
	return TFS_SB(sb)->s_gdb_count;
}

#ifdef CONFIG_TFS_CHECK
/* Called at mount-time, super-block is locked */
void tfs_check_blocks_bitmap (struct super_block * sb)
{
	struct buffer_head *bitmap_bh = NULL;
	struct tfs_super_block * es;
	unsigned long desc_count, bitmap_count, x, j;
	unsigned long desc_blocks;
	struct tfs_group_desc * desc;
	int i;

	es = TFS_SB(sb)->s_es;
	desc_count = 0;
	bitmap_count = 0;
	desc = NULL;
	for (i = 0; i < TFS_SB(sb)->s_groups_count; i++) {
		desc = tfs_get_group_desc (sb, i, NULL);
		if (!desc)
			continue;
		desc_count += le16_to_cpu(desc->bg_free_blocks_count);
		brelse(bitmap_bh);
		bitmap_bh = read_block_bitmap(sb, i);
		if (!bitmap_bh)
			continue;

		if (tfs_bg_has_super(sb, i) &&
				!tfs_test_bit(0, bitmap_bh->b_data))
			tfs_error(sb, __FUNCTION__,
				   "Superblock in group %d is marked free", i);

		desc_blocks = tfs_bg_num_gdb(sb, i);
		for (j = 0; j < desc_blocks; j++)
			if (!tfs_test_bit(j + 1, bitmap_bh->b_data))
				tfs_error(sb, __FUNCTION__,
					   "Descriptor block #%ld in group "
					   "%d is marked free", j, i);

		if (!block_in_use(le32_to_cpu(desc->bg_block_bitmap),
					sb, bitmap_bh->b_data))
			tfs_error(sb, "tfs_check_blocks_bitmap",
				    "Block bitmap for group %d is marked free",
				    i);

		if (!block_in_use(le32_to_cpu(desc->bg_inode_bitmap),
					sb, bitmap_bh->b_data))
			tfs_error(sb, "tfs_check_blocks_bitmap",
				    "Inode bitmap for group %d is marked free",
				    i);

		for (j = 0; j < TFS_SB(sb)->s_itb_per_group; j++)
			if (!block_in_use(le32_to_cpu(desc->bg_inode_table) + j,
						sb, bitmap_bh->b_data))
				tfs_error (sb, "tfs_check_blocks_bitmap",
					    "Block #%ld of the inode table in "
					    "group %d is marked free", j, i);

		x = tfs_count_free(bitmap_bh, sb->s_blocksize);
		if (le16_to_cpu(desc->bg_free_blocks_count) != x)
			tfs_error (sb, "tfs_check_blocks_bitmap",
				    "Wrong free blocks count for group %d, "
				    "stored = %d, counted = %lu", i,
				    le16_to_cpu(desc->bg_free_blocks_count), x);
		bitmap_count += x;
	}
	if (le32_to_cpu(es->s_free_blocks_count) != bitmap_count)
		tfs_error (sb, "tfs_check_blocks_bitmap",
			"Wrong free blocks count in super block, "
			"stored = %lu, counted = %lu",
			(unsigned long)le32_to_cpu(es->s_free_blocks_count),
			bitmap_count);
	brelse(bitmap_bh);
}
#endif
