/*
 *  linux/fs/tfs/ialloc.c
 *
 * Copyright (C) 1992, 1993, 1994, 1995
 * Remy Card (card@masi.ibp.fr)
 * Laboratoire MASI - Institut Blaise Pascal
 * Universite Pierre et Marie Curie (Paris VI)
 *
 *  BSD ufs-inspired inode and directory allocation by 
 *  Stephen Tweedie (sct@dcs.ed.ac.uk), 1993
 *  Big-endian to little-endian byte-swapping/bitmaps by
 *        David S. Miller (davem@caip.rutgers.edu), 1995
 */

#include <linux/config.h>
#include <linux/quotaops.h>
#include <linux/sched.h>
#include <linux/backing-dev.h>
#include <linux/buffer_head.h>
#include <linux/random.h>
#include "tfs.h"
#include "xattr.h"
#include "acl.h"

/*
 * ialloc.c contains the inodes allocation and deallocation routines
 */

/*
 * The free inodes are managed by bitmaps.  A file system contains several
 * blocks groups.  Each group contains 1 bitmap block for blocks, 1 bitmap
 * block for inodes, N blocks for the inode table and data blocks.
 *
 * The file system contains group descriptors which are located after the
 * super block.  Each descriptor contains the number of the bitmap block and
 * the free blocks count in the block.
 */


/*
 * Read the inode allocation bitmap for a given block_group, reading
 * into the specified slot in the superblock's bitmap cache.
 *
 * Return buffer_head of bitmap on success or NULL.
 */
static struct buffer_head *
read_inode_bitmap(struct super_block * sb, unsigned long block_group)
{
	struct tfs_group_desc *desc;
	struct buffer_head *bh = NULL;

	desc = tfs_get_group_desc(sb, block_group, NULL);
	if (!desc)
		goto error_out;

	bh = sb_bread(sb, le32_to_cpu(desc->bg_inode_bitmap));
	if (!bh)
		tfs_error(sb, "read_inode_bitmap",
			    "Cannot read inode bitmap - "
			    "block_group = %lu, inode_bitmap = %u",
			    block_group, le32_to_cpu(desc->bg_inode_bitmap));
error_out:
	return bh;
}

static void tfs_release_inode(struct super_block *sb, int group, int dir)
{
	struct tfs_group_desc * desc;
	struct buffer_head *bh;

	desc = tfs_get_group_desc(sb, group, &bh);
	if (!desc) {
		tfs_error(sb, "tfs_release_inode",
			"can't get descriptor for group %d", group);
		return;
	}

	spin_lock(sb_bgl_lock(TFS_SB(sb), group));
	desc->bg_free_inodes_count =
		cpu_to_le16(le16_to_cpu(desc->bg_free_inodes_count) + 1);
	if (dir)
		desc->bg_used_dirs_count =
			cpu_to_le16(le16_to_cpu(desc->bg_used_dirs_count) - 1);
	spin_unlock(sb_bgl_lock(TFS_SB(sb), group));
	if (dir)
		percpu_counter_dec(&TFS_SB(sb)->s_dirs_counter);
	sb->s_dirt = 1;
	mark_buffer_dirty(bh);
}

/*
 * NOTE! When we get the inode, we're the only people
 * that have access to it, and as such there are no
 * race conditions we have to worry about. The inode
 * is not on the hash-lists, and it cannot be reached
 * through the filesystem because the directory entry
 * has been deleted earlier.
 *
 * HOWEVER: we must make sure that we get no aliases,
 * which means that we have to call "clear_inode()"
 * _before_ we mark the inode not in use in the inode
 * bitmaps. Otherwise a newly created file might use
 * the same inode number (not actually the same pointer
 * though), and then we'd have two inodes sharing the
 * same inode number and space on the harddisk.
 */
void tfs_free_inode (struct inode * inode)
{
	struct super_block * sb = inode->i_sb;
	int is_directory;
	unsigned long ino;
	struct buffer_head *bitmap_bh = NULL;
	unsigned long block_group;
	unsigned long bit;
	struct tfs_super_block * es;

	ino = inode->i_ino;
	tfs_debug ("freeing inode %lu\n", ino);

	/*
	 * Note: we must free any quota before locking the superblock,
	 * as writing the quota to disk may need the lock as well.
	 */
	if (!is_bad_inode(inode)) {
		/* Quota is already initialized in iput() */
		tfs_xattr_delete_inode(inode);
	    	DQUOT_FREE_INODE(inode);
		DQUOT_DROP(inode);
	}

	es = TFS_SB(sb)->s_es;
	is_directory = S_ISDIR(inode->i_mode);

	/* Do this BEFORE marking the inode not in use or returning an error */
	clear_inode (inode);

	if (ino < TFS_FIRST_INO(sb) ||
	    ino > le32_to_cpu(es->s_inodes_count)) {
		tfs_error (sb, "tfs_free_inode",
			    "reserved or nonexistent inode %lu", ino);
		goto error_return;
	}
	block_group = (ino - 1) / TFS_INODES_PER_GROUP(sb);
	bit = (ino - 1) % TFS_INODES_PER_GROUP(sb);
	brelse(bitmap_bh);
	bitmap_bh = read_inode_bitmap(sb, block_group);
	if (!bitmap_bh)
		goto error_return;

	/* Ok, now we can actually update the inode bitmaps.. */
	if (!tfs_clear_bit_atomic(sb_bgl_lock(TFS_SB(sb), block_group),
				bit, (void *) bitmap_bh->b_data))
		tfs_error (sb, "tfs_free_inode",
			      "bit already cleared for inode %lu", ino);
	else
		tfs_release_inode(sb, block_group, is_directory);
	mark_buffer_dirty(bitmap_bh);
	if (sb->s_flags & MS_SYNCHRONOUS)
		sync_dirty_buffer(bitmap_bh);
error_return:
	brelse(bitmap_bh);
}

/*
 * We perform asynchronous prereading of the new inode's inode block when
 * we create the inode, in the expectation that the inode will be written
 * back soon.  There are two reasons:
 *
 * - When creating a large number of files, the async prereads will be
 *   nicely merged into large reads
 * - When writing out a large number of inodes, we don't need to keep on
 *   stalling the writes while we read the inode block.
 *
 * FIXME: tfs_get_group_desc() needs to be simplified.
 */
static void tfs_preread_inode(struct inode *inode)
{
	unsigned long block_group;
	unsigned long offset;
	unsigned long block;
	struct buffer_head *bh;
	struct tfs_group_desc * gdp;
	struct backing_dev_info *bdi;

	bdi = inode->i_mapping->backing_dev_info;
	if (bdi_read_congested(bdi))
		return;
	if (bdi_write_congested(bdi))
		return;

	block_group = (inode->i_ino - 1) / TFS_INODES_PER_GROUP(inode->i_sb);
	gdp = tfs_get_group_desc(inode->i_sb, block_group, &bh);
	if (gdp == NULL)
		return;

	/*
	 * Figure out the offset within the block group inode table
	 */
	offset = ((inode->i_ino - 1) % TFS_INODES_PER_GROUP(inode->i_sb)) *
				TFS_INODE_SIZE(inode->i_sb);
	block = le32_to_cpu(gdp->bg_inode_table) +
				(offset >> TFS_BLOCK_SIZE_BITS(inode->i_sb));
	sb_breadahead(inode->i_sb, block);
}

/*
 * There are two policies for allocating an inode.  If the new inode is
 * a directory, then a forward search is made for a block group with both
 * free space and a low directory-to-inode ratio; if that fails, then of
 * the groups with above-average free space, that group with the fewest
 * directories already is chosen.
 *
 * For other inodes, search forward from the parent directory\'s block
 * group to find a free inode.
 */
static int find_group_dir(struct super_block *sb, struct inode *parent)
{
	int ngroups = TFS_SB(sb)->s_groups_count;
	int avefreei = tfs_count_free_inodes(sb) / ngroups;
	struct tfs_group_desc *desc, *best_desc = NULL;
	struct buffer_head *bh, *best_bh = NULL;
	int group, best_group = -1;

	for (group = 0; group < ngroups; group++) {
		desc = tfs_get_group_desc (sb, group, &bh);
		if (!desc || !desc->bg_free_inodes_count)
			continue;
		if (le16_to_cpu(desc->bg_free_inodes_count) < avefreei)
			continue;
		if (!best_desc || 
		    (le16_to_cpu(desc->bg_free_blocks_count) >
		     le16_to_cpu(best_desc->bg_free_blocks_count))) {
			best_group = group;
			best_desc = desc;
			best_bh = bh;
		}
	}
	if (!best_desc)
		return -1;

	return best_group;
}

/* 
 * Orlov's allocator for directories. 
 * 
 * We always try to spread first-level directories.
 *
 * If there are blockgroups with both free inodes and free blocks counts 
 * not worse than average we return one with smallest directory count. 
 * Otherwise we simply return a random group. 
 * 
 * For the rest rules look so: 
 * 
 * It's OK to put directory into a group unless 
 * it has too many directories already (max_dirs) or 
 * it has too few free inodes left (min_inodes) or 
 * it has too few free blocks left (min_blocks) or 
 * it's already running too large debt (max_debt). 
 * Parent's group is prefered, if it doesn't satisfy these 
 * conditions we search cyclically through the rest. If none 
 * of the groups look good we just look for a group with more 
 * free inodes than average (starting at parent's group). 
 * 
 * Debt is incremented each time we allocate a directory and decremented 
 * when we allocate an inode, within 0--255. 
 */ 

#define INODE_COST 64
#define BLOCK_COST 256

static int find_group_orlov(struct super_block *sb, struct inode *parent)
{
	int parent_group = TFS_I(parent)->i_block_group;
	struct tfs_sb_info *sbi = TFS_SB(sb);
	struct tfs_super_block *es = sbi->s_es;
	int ngroups = sbi->s_groups_count;
	int inodes_per_group = TFS_INODES_PER_GROUP(sb);
	int freei;
	int avefreei;
	int free_blocks;
	int avefreeb;
	int blocks_per_dir;
	int ndirs;
	int max_debt, max_dirs, min_blocks, min_inodes;
	int group = -1, i;
	struct tfs_group_desc *desc;
	struct buffer_head *bh;

	freei = percpu_counter_read_positive(&sbi->s_freeinodes_counter);
	avefreei = freei / ngroups;
	free_blocks = percpu_counter_read_positive(&sbi->s_freeblocks_counter);
	avefreeb = free_blocks / ngroups;
	ndirs = percpu_counter_read_positive(&sbi->s_dirs_counter);

	if ((parent == sb->s_root->d_inode) ||
	    (TFS_I(parent)->i_flags & TFS_TOPDIR_FL)) {
		struct tfs_group_desc *best_desc = NULL;
		struct buffer_head *best_bh = NULL;
		int best_ndir = inodes_per_group;
		int best_group = -1;

		get_random_bytes(&group, sizeof(group));
		parent_group = (unsigned)group % ngroups;
		for (i = 0; i < ngroups; i++) {
			group = (parent_group + i) % ngroups;
			desc = tfs_get_group_desc (sb, group, &bh);
			if (!desc || !desc->bg_free_inodes_count)
				continue;
			if (le16_to_cpu(desc->bg_used_dirs_count) >= best_ndir)
				continue;
			if (le16_to_cpu(desc->bg_free_inodes_count) < avefreei)
				continue;
			if (le16_to_cpu(desc->bg_free_blocks_count) < avefreeb)
				continue;
			best_group = group;
			best_ndir = le16_to_cpu(desc->bg_used_dirs_count);
			best_desc = desc;
			best_bh = bh;
		}
		if (best_group >= 0) {
			desc = best_desc;
			bh = best_bh;
			group = best_group;
			goto found;
		}
		goto fallback;
	}

	if (ndirs == 0)
		ndirs = 1;	/* percpu_counters are approximate... */

	blocks_per_dir = (le32_to_cpu(es->s_blocks_count)-free_blocks) / ndirs;

	max_dirs = ndirs / ngroups + inodes_per_group / 16;
	min_inodes = avefreei - inodes_per_group / 4;
	min_blocks = avefreeb - TFS_BLOCKS_PER_GROUP(sb) / 4;

	max_debt = TFS_BLOCKS_PER_GROUP(sb) / max(blocks_per_dir, BLOCK_COST);
	if (max_debt * INODE_COST > inodes_per_group)
		max_debt = inodes_per_group / INODE_COST;
	if (max_debt > 255)
		max_debt = 255;
	if (max_debt == 0)
		max_debt = 1;

	for (i = 0; i < ngroups; i++) {
		group = (parent_group + i) % ngroups;
		desc = tfs_get_group_desc (sb, group, &bh);
		if (!desc || !desc->bg_free_inodes_count)
			continue;
		if (sbi->s_debts[group] >= max_debt)
			continue;
		if (le16_to_cpu(desc->bg_used_dirs_count) >= max_dirs)
			continue;
		if (le16_to_cpu(desc->bg_free_inodes_count) < min_inodes)
			continue;
		if (le16_to_cpu(desc->bg_free_blocks_count) < min_blocks)
			continue;
		goto found;
	}

fallback:
	for (i = 0; i < ngroups; i++) {
		group = (parent_group + i) % ngroups;
		desc = tfs_get_group_desc (sb, group, &bh);
		if (!desc || !desc->bg_free_inodes_count)
			continue;
		if (le16_to_cpu(desc->bg_free_inodes_count) >= avefreei)
			goto found;
	}

	if (avefreei) {
		/*
		 * The free-inodes counter is approximate, and for really small
		 * filesystems the above test can fail to find any blockgroups
		 */
		avefreei = 0;
		goto fallback;
	}

	return -1;

found:
	return group;
}

static int find_group_other(struct super_block *sb, struct inode *parent)
{
	int parent_group = TFS_I(parent)->i_block_group;
	int ngroups = TFS_SB(sb)->s_groups_count;
	struct tfs_group_desc *desc;
	struct buffer_head *bh;
	int group, i;

	/*
	 * Try to place the inode in its parent directory
	 */
	group = parent_group;
	desc = tfs_get_group_desc (sb, group, &bh);
	if (desc && le16_to_cpu(desc->bg_free_inodes_count) &&
			le16_to_cpu(desc->bg_free_blocks_count))
		goto found;

	/*
	 * We're going to place this inode in a different blockgroup from its
	 * parent.  We want to cause files in a common directory to all land in
	 * the same blockgroup.  But we want files which are in a different
	 * directory which shares a blockgroup with our parent to land in a
	 * different blockgroup.
	 *
	 * So add our directory's i_ino into the starting point for the hash.
	 */
	group = (group + parent->i_ino) % ngroups;

	/*
	 * Use a quadratic hash to find a group with a free inode and some
	 * free blocks.
	 */
	for (i = 1; i < ngroups; i <<= 1) {
		group += i;
		if (group >= ngroups)
			group -= ngroups;
		desc = tfs_get_group_desc (sb, group, &bh);
		if (desc && le16_to_cpu(desc->bg_free_inodes_count) &&
				le16_to_cpu(desc->bg_free_blocks_count))
			goto found;
	}

	/*
	 * That failed: try linear search for a free inode, even if that group
	 * has no free blocks.
	 */
	group = parent_group;
	for (i = 0; i < ngroups; i++) {
		if (++group >= ngroups)
			group = 0;
		desc = tfs_get_group_desc (sb, group, &bh);
		if (desc && le16_to_cpu(desc->bg_free_inodes_count))
			goto found;
	}

	return -1;

found:
	return group;
}

struct inode *tfs_new_inode(struct inode *dir, int mode)
{
	struct super_block *sb;
	struct buffer_head *bitmap_bh = NULL;
	struct buffer_head *bh2;
	int group, i;
	ino_t ino = 0;
	struct inode * inode;
	struct tfs_group_desc *gdp;
	struct tfs_super_block *es;
	struct tfs_inode_info *ei;
	struct tfs_sb_info *sbi;
	int err;

	sb = dir->i_sb;
	inode = new_inode(sb);
	if (!inode)
		return ERR_PTR(-ENOMEM);

	ei = TFS_I(inode);
	sbi = TFS_SB(sb);
	es = sbi->s_es;
	if (S_ISDIR(mode)) {
		if (test_opt(sb, OLDALLOC))
			group = find_group_dir(sb, dir);
		else
			group = find_group_orlov(sb, dir);
	} else 
		group = find_group_other(sb, dir);

	if (group == -1) {
		err = -ENOSPC;
		goto fail;
	}

	for (i = 0; i < sbi->s_groups_count; i++) {
		gdp = tfs_get_group_desc(sb, group, &bh2);
		brelse(bitmap_bh);
		bitmap_bh = read_inode_bitmap(sb, group);
		if (!bitmap_bh) {
			err = -EIO;
			goto fail;
		}
		ino = 0;

repeat_in_this_group:
		ino = tfs_find_next_zero_bit((unsigned long *)bitmap_bh->b_data,
					      TFS_INODES_PER_GROUP(sb), ino);
		if (ino >= TFS_INODES_PER_GROUP(sb)) {
			/*
			 * Rare race: find_group_xx() decided that there were
			 * free inodes in this group, but by the time we tried
			 * to allocate one, they're all gone.  This can also
			 * occur because the counters which find_group_orlov()
			 * uses are approximate.  So just go and search the
			 * next block group.
			 */
			if (++group == sbi->s_groups_count)
				group = 0;
			continue;
		}
		if (tfs_set_bit_atomic(sb_bgl_lock(sbi, group),
						ino, bitmap_bh->b_data)) {
			/* we lost this inode */
			if (++ino >= TFS_INODES_PER_GROUP(sb)) {
				/* this group is exhausted, try next group */
				if (++group == sbi->s_groups_count)
					group = 0;
				continue;
			}
			/* try to find free inode in the same group */
			goto repeat_in_this_group;
		}
		goto got;
	}

	/*
	 * Scanned all blockgroups.
	 */
	err = -ENOSPC;
	goto fail;
got:
	mark_buffer_dirty(bitmap_bh);
	if (sb->s_flags & MS_SYNCHRONOUS)
		sync_dirty_buffer(bitmap_bh);
	brelse(bitmap_bh);

	ino += group * TFS_INODES_PER_GROUP(sb) + 1;
	if (ino < TFS_FIRST_INO(sb) || ino > le32_to_cpu(es->s_inodes_count)) {
		tfs_error (sb, "tfs_new_inode",
			    "reserved inode or inode > inodes count - "
			    "block_group = %d,inode=%lu", group,
			    (unsigned long) ino);
		err = -EIO;
		goto fail;
	}

	percpu_counter_mod(&sbi->s_freeinodes_counter, -1);
	if (S_ISDIR(mode))
		percpu_counter_inc(&sbi->s_dirs_counter);

	spin_lock(sb_bgl_lock(sbi, group));
	gdp->bg_free_inodes_count =
                cpu_to_le16(le16_to_cpu(gdp->bg_free_inodes_count) - 1);
	if (S_ISDIR(mode)) {
		if (sbi->s_debts[group] < 255)
			sbi->s_debts[group]++;
		gdp->bg_used_dirs_count =
			cpu_to_le16(le16_to_cpu(gdp->bg_used_dirs_count) + 1);
	} else {
		if (sbi->s_debts[group])
			sbi->s_debts[group]--;
	}
	spin_unlock(sb_bgl_lock(sbi, group));

	sb->s_dirt = 1;
	mark_buffer_dirty(bh2);
	inode->i_uid = current->fsuid;
	if (test_opt (sb, GRPID))
		inode->i_gid = dir->i_gid;
	else if (dir->i_mode & S_ISGID) {
		inode->i_gid = dir->i_gid;
		if (S_ISDIR(mode))
			mode |= S_ISGID;
	} else
		inode->i_gid = current->fsgid;
	inode->i_mode = mode;

	inode->i_ino = ino;
	inode->i_blksize = PAGE_SIZE;	/* This is the optimal IO size (for stat), not the fs block size */
	inode->i_blocks = 0;
	inode->i_mtime = inode->i_atime = inode->i_ctime = CURRENT_TIME_SEC;
	memset(ei->i_data, 0, sizeof(ei->i_data));
	ei->i_flags = TFS_I(dir)->i_flags & ~TFS_BTREE_FL;
	if (S_ISLNK(mode))
		ei->i_flags &= ~(TFS_IMMUTABLE_FL|TFS_APPEND_FL);
	/* dirsync is only applied to directories */
	if (!S_ISDIR(mode))
		ei->i_flags &= ~TFS_DIRSYNC_FL;
	ei->i_faddr = 0;
	ei->i_frag_no = 0;
	ei->i_frag_size = 0;
	ei->i_file_acl = 0;
	ei->i_dir_acl = 0;
	ei->i_dtime = 0;
	ei->i_block_group = group;
	ei->i_next_alloc_block = 0;
	ei->i_next_alloc_goal = 0;
	ei->i_prealloc_block = 0;
	ei->i_prealloc_count = 0;
	ei->i_dir_start_lookup = 0;
	ei->i_state = TFS_STATE_NEW;

	//JC - inode inherits opacity of parent
	ei->i_transparency=TFS_I(dir)->i_transparency;
	
	tfs_set_inode_flags(inode);
	spin_lock(&sbi->s_next_gen_lock);
	inode->i_generation = sbi->s_next_generation++;
	spin_unlock(&sbi->s_next_gen_lock);
	insert_inode_hash(inode);

	if (DQUOT_ALLOC_INODE(inode)) {
		err = -ENOSPC;
		goto fail_drop;
	}

	err = tfs_init_acl(inode, dir);
	if (err)
		goto fail_free_drop;

	err = tfs_init_security(inode,dir);
	if (err)
		goto fail_free_drop;

	mark_inode_dirty(inode);
	tfs_debug("allocating inode %lu\n", inode->i_ino);
	tfs_preread_inode(inode);
	return inode;

fail_free_drop:
	DQUOT_FREE_INODE(inode);

fail_drop:
	DQUOT_DROP(inode);
	inode->i_flags |= S_NOQUOTA;
	inode->i_nlink = 0;
	iput(inode);
	return ERR_PTR(err);

fail:
	make_bad_inode(inode);
	iput(inode);
	return ERR_PTR(err);
}

unsigned long tfs_count_free_inodes (struct super_block * sb)
{
	struct tfs_group_desc *desc;
	unsigned long desc_count = 0;
	int i;	

#ifdef TFSFS_DEBUG
	struct tfs_super_block *es;
	unsigned long bitmap_count = 0;
	struct buffer_head *bitmap_bh = NULL;

	lock_super (sb);
	es = TFS_SB(sb)->s_es;
	for (i = 0; i < TFS_SB(sb)->s_groups_count; i++) {
		unsigned x;

		desc = tfs_get_group_desc (sb, i, NULL);
		if (!desc)
			continue;
		desc_count += le16_to_cpu(desc->bg_free_inodes_count);
		brelse(bitmap_bh);
		bitmap_bh = read_inode_bitmap(sb, i);
		if (!bitmap_bh)
			continue;

		x = tfs_count_free(bitmap_bh, TFS_INODES_PER_GROUP(sb) / 8);
		printk("group %d: stored = %d, counted = %u\n",
			i, le16_to_cpu(desc->bg_free_inodes_count), x);
		bitmap_count += x;
	}
	brelse(bitmap_bh);
	printk("tfs_count_free_inodes: stored = %lu, computed = %lu, %lu\n",
		percpu_counter_read(&TFS_SB(sb)->s_freeinodes_counter),
		desc_count, bitmap_count);
	unlock_super(sb);
	return desc_count;
#else
	for (i = 0; i < TFS_SB(sb)->s_groups_count; i++) {
		desc = tfs_get_group_desc (sb, i, NULL);
		if (!desc)
			continue;
		desc_count += le16_to_cpu(desc->bg_free_inodes_count);
	}
	return desc_count;
#endif
}

/* Called at mount-time, super-block is locked */
unsigned long tfs_count_dirs (struct super_block * sb)
{
	unsigned long count = 0;
	int i;

	for (i = 0; i < TFS_SB(sb)->s_groups_count; i++) {
		struct tfs_group_desc *gdp = tfs_get_group_desc (sb, i, NULL);
		if (!gdp)
			continue;
		count += le16_to_cpu(gdp->bg_used_dirs_count);
	}
	return count;
}

#ifdef CONFIG_TFS_CHECK
/* Called at mount-time, super-block is locked */
void tfs_check_inodes_bitmap (struct super_block * sb)
{
	struct tfs_super_block * es = TFS_SB(sb)->s_es;
	unsigned long desc_count = 0, bitmap_count = 0;
	struct buffer_head *bitmap_bh = NULL;
	int i;

	for (i = 0; i < TFS_SB(sb)->s_groups_count; i++) {
		struct tfs_group_desc *desc;
		unsigned x;

		desc = tfs_get_group_desc(sb, i, NULL);
		if (!desc)
			continue;
		desc_count += le16_to_cpu(desc->bg_free_inodes_count);
		brelse(bitmap_bh);
		bitmap_bh = read_inode_bitmap(sb, i);
		if (!bitmap_bh)
			continue;
		
		x = tfs_count_free(bitmap_bh, TFS_INODES_PER_GROUP(sb) / 8);
		if (le16_to_cpu(desc->bg_free_inodes_count) != x)
			tfs_error (sb, "tfs_check_inodes_bitmap",
				    "Wrong free inodes count in group %d, "
				    "stored = %d, counted = %lu", i,
				    le16_to_cpu(desc->bg_free_inodes_count), x);
		bitmap_count += x;
	}
	brelse(bitmap_bh);
	if (percpu_counter_read(&TFS_SB(sb)->s_freeinodes_counter) !=
				bitmap_count)
		tfs_error(sb, "tfs_check_inodes_bitmap",
			    "Wrong free inodes count in super block, "
			    "stored = %lu, counted = %lu",
			    (unsigned long)le32_to_cpu(es->s_free_inodes_count),
			    bitmap_count);
}
#endif
