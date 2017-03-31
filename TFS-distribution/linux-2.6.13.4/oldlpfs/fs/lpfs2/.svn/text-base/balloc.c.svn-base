/*
 *  linux/fs/lpfs/balloc.c
 *
 * Copyright (C) 1992, 1993, 1994, 1995
 * Remy Card (card@masi.ibp.fr)
 * Laboratoire MASI - Institut Blaise Pascal
 * Universite Pierre et Marie Curie (Paris VI)
 *
 *  Enhanced block allocation by Stephen Tweedie (sct@dcs.ed.ac.uk), 1993
 *  Big-endian to little-endian byte-swapping/bitmaps by
 *        David S. Miller (davem@caip.rutgers.edu), 1995
 */

#include <linux/config.h>
#include "lpfs.h"
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
 * when a file system is mounted (see lpfs_read_super).
 */


#define in_range(b, first, len)	((b) >= (first) && (b) <= (first) + (len) - 1)

struct lpfs_group_desc * lpfs_get_group_desc(struct super_block * sb,
					     unsigned int block_group,
					     struct buffer_head ** bh)
{
	unsigned long group_desc;
	unsigned long offset;
	struct lpfs_group_desc * desc;
	struct lpfs_sb_info *sbi = LPFS_SB(sb);

	if (block_group >= sbi->s_groups_count) {
		lpfs_error (sb, "lpfs_get_group_desc",
			    "block_group >= groups_count - "
			    "block_group = %d, groups_count = %lu",
			    block_group, sbi->s_groups_count);

		return NULL;
	}
	
	group_desc = block_group / LPFS_DESC_PER_BLOCK(sb);
	offset = block_group % LPFS_DESC_PER_BLOCK(sb);
	if (!sbi->s_group_desc[group_desc]) {
		lpfs_error (sb, "lpfs_get_group_desc",
			    "Group descriptor not loaded - "
			    "block_group = %d, group_desc = %lu, desc = %lu",
			     block_group, group_desc, offset);
		return NULL;
	}
	
	desc = (struct lpfs_group_desc *) sbi->s_group_desc[group_desc]->b_data;
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
	struct lpfs_group_desc * desc;
	struct buffer_head * bh = NULL;
	
	desc = lpfs_get_group_desc (sb, block_group, NULL);
	if (!desc)
		goto error_out;
	bh = sb_bread(sb, le32_to_cpu(desc->bg_block_bitmap));
	if (!bh)
		lpfs_error (sb, "read_block_bitmap",
			    "Cannot read block bitmap - "
			    "block_group = %d, block_bitmap = %u",
			    block_group, le32_to_cpu(desc->bg_block_bitmap));
error_out:
	return bh;
}

/* JC -read_lowPriority_bitmap */
static struct buffer_head *
read_lowPriority_bitmap(struct super_block *sb, unsigned int block_group)
{
	struct lpfs_group_desc * desc;
	struct buffer_head * bh = NULL;
	
	desc = lpfs_get_group_desc (sb, block_group, NULL);
	if (!desc)
		goto error_out;
	bh = sb_bread(sb, le32_to_cpu(desc->bg_lowPriority_bitmap));
	if (!bh)
		lpfs_error (sb, "read_lowPriority_bitmap",
			    "Cannot read low priority bitmap - "
			    "block_group = %d, lowPriority_bitmap = %u",
			    block_group, le32_to_cpu(desc->bg_lowPriority_bitmap));
error_out:
	return bh;
}


static struct buffer_head *
read_dirty_bitmap(struct super_block *sb, unsigned int block_group)
{
	struct lpfs_group_desc * desc;
	struct buffer_head * bh = NULL;
	
	desc = lpfs_get_group_desc (sb, block_group, NULL);
	if (!desc)
		goto error_out;
	bh = sb_bread(sb, le32_to_cpu(desc->bg_dirty_bitmap));
	if (!bh)
		lpfs_error (sb, "read_dirty_bitmap",
			    "Cannot read block bitmap - "
			    "block_group = %d, dirty_bitmap = %u",
			    block_group, le32_to_cpu(desc->bg_dirty_bitmap));
error_out:
	return bh;
}
/* /JC */


/*
 * Set sb->s_dirt here because the superblock was "logically" altered.  We
 * need to recalculate its free blocks count and flush it out.
 */
static int reserve_blocks(struct super_block *sb, int count)
{
	struct lpfs_sb_info *sbi = LPFS_SB(sb);
	struct lpfs_super_block *es = sbi->s_es;
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
		struct lpfs_sb_info *sbi = LPFS_SB(sb);

		percpu_counter_mod(&sbi->s_freeblocks_counter, count);
		sb->s_dirt = 1;
	}
}

static int group_reserve_blocks(struct lpfs_sb_info *sbi, int group_no,
	struct lpfs_group_desc *desc, struct buffer_head *bh, int count)
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
	struct lpfs_group_desc *desc, struct buffer_head *bh, int count)
{
	if (count) {
		struct lpfs_sb_info *sbi = LPFS_SB(sb);
		unsigned free_blocks;

		spin_lock(sb_bgl_lock(sbi, group_no));
		free_blocks = le16_to_cpu(desc->bg_free_blocks_count);
		desc->bg_free_blocks_count = cpu_to_le16(free_blocks + count);
		spin_unlock(sb_bgl_lock(sbi, group_no));
		sb->s_dirt = 1;
		mark_buffer_dirty(bh);
	}
}



/* JC lock and unlock block functions */
//balloc.c touches the blocklock directly, file.c should not
int lpfs_lockBlock(struct super_block *sb, int goal){
	struct lpfs_super_block * es;
	struct lpfs_sb_info *sbi;
	int i,j;
	sbi = LPFS_SB(sb);
	es=sbi->s_es;
	if (goal < le32_to_cpu(es->s_first_data_block) ||
	goal >= le32_to_cpu(es->s_blocks_count)){
		return 1;
	}
	i = (goal - le32_to_cpu(es->s_first_data_block)) / LPFS_BLOCKS_PER_GROUP(sb);
	j = ((goal - le32_to_cpu(es->s_first_data_block)) % LPFS_BLOCKS_PER_GROUP(sb));
	spin_lock(sb_bgl_lock(sbi, i));
	j=lpfs_set_bit(j,sbi->s_blocklock_bitmaps[i]);
	spin_unlock(sb_bgl_lock(sbi, i));
	return j;
}
int lpfs_unlockBlock(struct super_block *sb, int goal){
	struct lpfs_super_block * es;
	struct lpfs_sb_info *sbi;
	int i,j;
	sbi = LPFS_SB(sb);
	es=sbi->s_es;
	if (goal < le32_to_cpu(es->s_first_data_block) ||
	goal >= le32_to_cpu(es->s_blocks_count)){
		return -1;
	}
	i = (goal - le32_to_cpu(es->s_first_data_block)) / LPFS_BLOCKS_PER_GROUP(sb);
	j = ((goal - le32_to_cpu(es->s_first_data_block)) % LPFS_BLOCKS_PER_GROUP(sb));
	spin_lock(sb_bgl_lock(sbi, i));
	j=lpfs_clear_bit(j,sbi->s_blocklock_bitmaps[i]);
	spin_unlock(sb_bgl_lock(sbi, i));
	return j;
}

int lpfs_isBlockStillMine(struct inode *inode, int block){
	struct buffer_head *dbh=NULL;
	unsigned long block_group;
	unsigned long bit;
	int bitmap_nr;
	struct super_block * sb;
	struct lpfs_super_block * es;
	struct lpfs_sb_info *sbi;
	sb = inode->i_sb;
	sbi = LPFS_SB(sb);
	es=sbi->s_es;
	if (!sb) {
		printk ("lpfs_isBlockStillMine: nonexistent device");
		return -1;
	}
	if (block < le32_to_cpu(es->s_first_data_block) ||
	    (block ) > le32_to_cpu(es->s_blocks_count)) {
		lpfs_error (sb, "lpfs_isBlockStillMine",
			    "Checking blocks not in datazone - "
			    "block = %i", block);
		goto error_return;
	}

	lpfs_debug ("checking block(s) %lu-%lu\n", block, block + count - 1);

	block_group = (block - le32_to_cpu(es->s_first_data_block)) /
		      LPFS_BLOCKS_PER_GROUP(sb);
	bit = (block - le32_to_cpu(es->s_first_data_block)) %
		      LPFS_BLOCKS_PER_GROUP(sb);
	spin_lock(sb_bgl_lock(sbi, block_group));
	
	//if (bitmap_nr < 0)
	//	goto unlock_return;
	
	brelse(dbh);
	dbh = read_dirty_bitmap(sb, block_group);
	if(dbh==NULL){goto unlock_return; }
	bitmap_nr=lpfs_test_bit(bit,dbh->b_data);
	brelse(dbh);
	spin_unlock(sb_bgl_lock(sbi,block_group));
	return bitmap_nr;
unlock_return:
	spin_unlock(sb_bgl_lock(sbi, block_group));
error_return:
	
	return -1;
}
/* /JC */




/* Free given blocks, update quota and i_blocks field */
void lpfs_free_blocks (struct inode * inode, unsigned long block,
		       unsigned long count)
{
	struct buffer_head *bitmap_bh = NULL;
	struct buffer_head *lpbh=NULL;
	struct buffer_head *dbh=NULL;
	struct buffer_head * bh2;
	unsigned long block_group;
	unsigned long bit;
	unsigned long i;
	unsigned long overflow;
	struct super_block * sb = inode->i_sb;
	struct lpfs_sb_info * sbi = LPFS_SB(sb);
	struct lpfs_group_desc * desc;
	struct lpfs_super_block * es = sbi->s_es;
	unsigned freed = 0, group_freed;
	int priority=LPFS_I(inode)->i_priority;	//JC - variable to store priority;

	

	if (block < le32_to_cpu(es->s_first_data_block) ||
	    block + count < block ||
	    block + count > le32_to_cpu(es->s_blocks_count)) {
		lpfs_error (sb, "lpfs_free_blocks",
			    "Freeing blocks not in datazone - "
			    "block = %lu, count = %lu", block, count);
		goto error_return;
	}

	lpfs_debug ("freeing block(s) %lu-%lu\n", block, block + count - 1);

do_more:
	overflow = 0;
	block_group = (block - le32_to_cpu(es->s_first_data_block)) /
		      LPFS_BLOCKS_PER_GROUP(sb);
	bit = (block - le32_to_cpu(es->s_first_data_block)) %
		      LPFS_BLOCKS_PER_GROUP(sb);
	/*
	 * Check to see if we are freeing blocks across a group
	 * boundary.
	 */
	if (bit + count > LPFS_BLOCKS_PER_GROUP(sb)) {
		overflow = bit + count - LPFS_BLOCKS_PER_GROUP(sb);
		count -= overflow;
	}
	brelse(bitmap_bh);
	/* JC - free_blocks releasing lp buffer heads */
	brelse(lpbh);
	brelse(dbh);
	/* /JC */
	bitmap_bh = read_block_bitmap(sb, block_group);
	/* JC - free_blocks read lp buffer heads */
	lpbh = read_lowPriority_bitmap(sb,block_group);
	dbh = read_dirty_bitmap(sb,block_group);
	/* /JC */
	if (!bitmap_bh || !lpbh || !dbh)
		goto error_return;

	desc = lpfs_get_group_desc (sb, block_group, &bh2);
	if (!desc)
		goto error_return;

	if (in_range (le32_to_cpu(desc->bg_block_bitmap), block, count) ||
	    in_range (le32_to_cpu(desc->bg_inode_bitmap), block, count) ||
	    in_range (block, le32_to_cpu(desc->bg_inode_table),
		      sbi->s_itb_per_group) ||
	    in_range (block + count - 1, le32_to_cpu(desc->bg_inode_table),
		      sbi->s_itb_per_group))
		lpfs_error (sb, "lpfs_free_blocks",
			    "Freeing blocks in system zones - "
			    "Block = %lu, count = %lu",
			    block, count);

/* ORIGINAL
	for (i = 0, group_freed = 0; i < count; i++) {
		if (!lpfs_clear_bit_atomic(sb_bgl_lock(sbi, block_group),
					bit + i, (void *) bitmap_bh->b_data))
			lpfs_error (sb, "lpfs_free_blocks",
				      "bit already cleared for block %lu",
				      block + i);
		else
			group_freed++;
	}
*/
/* JC - free blocks */
//FIXME - does this *really* work?
	for(i=0, group_freed = 0; i<count; i++){
		spin_lock(sb_bgl_lock(sbi, block_group));
			//printk("freeing block %i - pri= %i -  %i %i %i\n",
			//	block+i, priority,
			//	lpfs_test_bit(bit+i, (void *)bitmap_bh->b_data),
			//	lpfs_test_bit(bit+i, (void *)lpbh->b_data),
			//	lpfs_test_bit(bit+i, (void *)dbh->b_data));
		if(priority==0){
			
			/*if(!lpfs_clear_bit_atomic(sb_bgl_lock(sbi, block_group),
						bit+i, (void *) lpbh->b_data)){
				if(!lpfs_clear_bit_atomic(sb_bgl_lock(sbi, block_goup),
							bit+i, (void *)bitmap_bh->b_data)){
					lpfs_clear_bit_atomic(sb_bgl_lock(sbi,block_group),
							bit+i, (void *)dbh->b_data);
					lpfs_error(sb, "lpfs_free_blocks",
						"all bits cleared for block %lu",
						block+i);
				}else{	//freed -1 priority block, dont clear dirty
					group_freed++;
				}
				*/
		//	spin_lock(sb_bgl_lock(sbi, block_group));
		/*	if(!lpfs_clear_bit(bit+i, (void *) lpbh->b_data)){
				if(!lpfs_clear_bit(bit+i, (void *)bitmap_bh->b_data)){
					lpfs_clear_bit(bit+i, (void *)dbh->b_data);
					lpfs_error(sb, "lpfs_free_blocks",
						"all bits cleared for block %lu",
						block+i);
				}else{	//freed -1 priority block, dont clear dirty
					group_freed++;
				}
			}else{	//freed low priority block, clear dirty
				lpfs_clear_bit(bit+i, (void *)dbh->b_data);
				group_freed++;
			}*/
			if(!lpfs_test_bit(bit+i, (void *) lpbh->b_data)){
				if(!lpfs_test_bit(bit+i, (void *)bitmap_bh->b_data)){
					lpfs_clear_bit(bit+i, (void *)dbh->b_data);
					lpfs_error(sb, "lpfs_free_blocks",
						"all bits cleared for block %lu",
						block+i);
				}else{	//freed -1 priority block, dont clear dirty
					lpfs_clear_bit(bit+i,(void *)bitmap_bh->b_data);
					group_freed++;
				}
			}else{	//freed low priority block, clear dirty
				lpfs_clear_bit(bit+i, (void *)lpbh->b_data);
				lpfs_clear_bit(bit+i, (void *)dbh->b_data);
				//lp blocks are made to affect free block for benchmarking
				//the aging test needs to know how many *really* free blocks
				//are in the filesystem, this next line should be commented out
				//in the final system
				group_freed++;  //comment out this line - lp blocks don't affect free block counts
			}
			lpfs_clear_bit(bit+i, sbi->s_blocklock_bitmaps[block_group]);
			spin_unlock(sb_bgl_lock(sbi, block_group));
		}else{	//free as high priority
		//	spin_unlock(sb_bgl_lock(sbi, block_group));
			if (!lpfs_clear_bit(bit + i, (void *) bitmap_bh->b_data)){
				lpfs_error (sb, "lpfs_free_blocks",
					      "bit already cleared for block %lu",
					      block + i);
			}else{
				group_freed++;
			}
			spin_unlock(sb_bgl_lock(sbi, block_group));
		}
	}
/* /JC */


	mark_buffer_dirty(bitmap_bh);
	/* JC - free_blocks dirty lp buffer heads */
	mark_buffer_dirty(lpbh);
	mark_buffer_dirty(dbh);
	/* /JC */
	if (sb->s_flags & MS_SYNCHRONOUS){
		sync_dirty_buffer(bitmap_bh);
		sync_dirty_buffer(lpbh);
		sync_dirty_buffer(dbh);
	}
	group_release_blocks(sb, block_group, desc, bh2, group_freed);
	freed += group_freed;

	if (overflow) {
		block += count;
		count = overflow;
		goto do_more;
	}
error_return:
	brelse(bitmap_bh);
	brelse(dbh);
	brelse(lpbh);
	release_blocks(sb, freed);
	DQUOT_FREE_BLOCK(inode, freed);
}

static int grab_block(spinlock_t *lock, char *map, char *lpm, char *dm, char *bl, unsigned size, int goal, int priority)
{
	int k;
	char *p, *r;
	//int priority=1;	//JC
//	void *lpm,*dm, *bl; //JC
	//spin_lock(lock);
	//if (!lpfs_test_bit(goal, map))	ORIGINAL
//	if(priority==-1){ priority=0;}//////////////////////////////////////////////////////////////
	if(!lpfs_p_test_bit(goal, map, lpm, dm, bl, priority))	//JC
		goto got_it;

repeat:
	if (goal) {
		/*
		 * The goal was occupied; search forward for a free 
		 * block within the next XX blocks.
		 *
		 * end_goal is more or less random, but it has to be
		 * less than LPFS_BLOCKS_PER_GROUP. Aligning up to the
		 * next 64-bit boundary is simple..
		 */
		k = (goal + 63) & ~63;
		//goal = lpfs_find_next_zero_bit(map, k, goal);	ORIGINAL
		goal = lpfs_p_find_next_zero_bit(map, lpm, dm, bl, k, goal, priority);	//JC
		if (goal < k)
			goto got_it;
		/*
		 * Search in the remainder of the current group.
		 */
	}

	p = map + (goal >> 3);
//	r = memscan(p, 0, (size - goal + 7) >> 3);	ORIGINAL
	k = lpfs_p_find_first_zero_byte(map, lpm,dm,bl, (goal+7)>>3, (size)>>3,priority);	//JC
	//k = (r - map) << 3;	//ORIGINAL
	k=k<<3;		//find_first_zero_byte gives us a byte offset, we want a bit offset
	if (k < size) {
		/* 
		 * We have succeeded in finding a free byte in the block
		 * bitmap.  Now search backwards to find the start of this
		 * group of free blocks - won't take more than 7 iterations.
		 */
//		for (goal = k; goal && !lpfs_test_bit (goal - 1, map); goal--)	ORIGINAL
//			;	ORIGINAL
		for(goal=k; goal && !lpfs_p_test_bit(goal-1,map, lpm, dm, bl, priority) ; goal --);
		goto got_it;
	}

//	k = lpfs_find_next_zero_bit ((u32 *)map, size, goal);	ORIGINAL
	k = lpfs_p_find_next_zero_bit(map, lpm,dm,bl,size, goal, priority);	//JC
	if (k < size) {
		goal = k;
		goto got_it;
	}
	//spin_unlock(lock);
	return -1;
got_it:
	//if (lpfs_set_bit_atomic(lock, goal, (void *) map)) 	ORIGINAL
	if(lpfs_p_set_bit_atomic(lock, goal, (void *)map, lpm, dm, bl,priority)){
		BUG();	//FIXME: this can happen in a multithreaded environment, but not
			//for my debugging
		goto repeat;
	}
//	spin_unlock(lock);	
	return goal;
}

/*
 * lpfs_new_block uses a goal block to assist allocation.  If the goal is
 * free, or there is a free block within 32 blocks of the goal, that block
 * is allocated.  Otherwise a forward search is made for a free block; within 
 * each block group the search first looks for an entire free byte in the block
 * bitmap, and then for any free bit if that fails.
 * This function also updates quota and i_blocks field.
 */
int lpfs_new_block(struct inode *inode, unsigned long goal,
			u32 *prealloc_count, u32 *prealloc_block, int *err, int priority)
{
	struct buffer_head *bitmap_bh = NULL;
/* JC */
	struct buffer_head *lowPriority_bh = NULL;
	struct buffer_head *dirty_bh = NULL;
/* /JC */
	struct buffer_head *gdp_bh;	/* bh2 */
	struct lpfs_group_desc *desc;
	int group_no;			/* i */
	int ret_block=-1;			/* j */
	int group_idx;			/* k */
	int target_block;		/* tmp */
	int block = 0;
	struct super_block *sb = inode->i_sb;
	struct lpfs_sb_info *sbi = LPFS_SB(sb);
	struct lpfs_super_block *es = sbi->s_es;
	unsigned group_size = LPFS_BLOCKS_PER_GROUP(sb);
	unsigned prealloc_goal = es->s_prealloc_blocks;
	unsigned group_alloc = 0, es_alloc, dq_alloc;
	int nr_scanned_groups;
	int tdq_alloc, tes_alloc, tgroup_alloc;
	//int priority=1;

	if (!prealloc_goal--)
		prealloc_goal = LPFS_DEFAULT_PREALLOC_BLOCKS - 1;
	if (!prealloc_count || *prealloc_count)
		prealloc_goal = 0;
	//JC - FIXME: should low priority files affect quotas?
	//probably not - fix it later
	if (DQUOT_ALLOC_BLOCK(inode, 1)) {
		*err = -EDQUOT;
		goto out;
	}

	while (prealloc_goal && DQUOT_PREALLOC_BLOCK(inode, prealloc_goal))
		prealloc_goal--;

	dq_alloc = prealloc_goal + 1;
	//JC - FIXME should low priority files be able to reserve blocks?
	//probably not.  If they reserve blocks, they will prevent high
	//priority allocations.  Maybe have a separate reserve_blocks() for
	//high and low priority?  right now they can reserve blocks . . .
	es_alloc = reserve_blocks(sb, dq_alloc);
	if (!es_alloc) {
		*err = -ENOSPC;
		printk("no free blocks in fs\n");
		goto out_dquot;
	}

	lpfs_debug ("goal=%lu.\n", goal);

	if (goal < le32_to_cpu(es->s_first_data_block) ||
	    goal >= le32_to_cpu(es->s_blocks_count))
		goal = le32_to_cpu(es->s_first_data_block);
	group_no = (goal - le32_to_cpu(es->s_first_data_block)) / group_size;
	desc = lpfs_get_group_desc (sb, group_no, &gdp_bh);
	if (!desc) {
		/*
		 * gdp_bh may still be uninitialised.  But group_release_blocks
		 * will not touch it because group_alloc is zero.
		 */
		goto io_error;
	}
	//JC - FIXME again with the reserving blocks.
	//We can leave it like this for now, but it should
	//be fixed once other things are working
	group_alloc = group_reserve_blocks(sbi, group_no, desc,
					gdp_bh, es_alloc);
	if (group_alloc) {
		ret_block = ((goal - le32_to_cpu(es->s_first_data_block)) %
					group_size);
		brelse(bitmap_bh);
		brelse(lowPriority_bh);		//JC
		brelse(dirty_bh);		//JC
		bitmap_bh = read_block_bitmap(sb, group_no);
		lowPriority_bh = read_lowPriority_bitmap(sb, group_no); //JC
		dirty_bh = read_dirty_bitmap(sb, group_no);		//JC
		if (!bitmap_bh || ! lowPriority_bh || !dirty_bh)	//JC
			goto io_error;
		
		lpfs_debug("goal is at %d:%d.\n", group_no, ret_block);

	//	ret_block = grab_block(sb_bgl_lock(sbi, group_no),	//ORIGINAL
	//			bitmap_bh->b_data, group_size, ret_block); //ORIGINAL
		ret_block = grab_block(sb_bgl_lock(sbi, group_no),
				bitmap_bh->b_data, lowPriority_bh->b_data, dirty_bh->b_data, 
				sbi->s_blocklock_bitmaps[group_no],
				group_size, ret_block,priority);
		if (ret_block >= 0)
			goto got_block;
		group_release_blocks(sb, group_no, desc, gdp_bh, group_alloc);
		group_alloc = 0;
	}else{
	//	printk("no free blocks in group\n");
	}

	lpfs_debug ("Bit not found in block group %d.\n", group_no);

	/*
	 * Now search the rest of the groups.  We assume that 
	 * i and desc correctly point to the last group visited.
	 */
	nr_scanned_groups = 0;
retry:
//FIXME - group_idx should be zerod before retry, so that
// its value persists between jumps to retry.  This way lp
// files don't enter an endless loop
	for (group_idx = 0; !group_alloc &&
			group_idx < sbi->s_groups_count; group_idx++) {
		group_no++;
		if (group_no >= sbi->s_groups_count)
			group_no = 0;
		desc = lpfs_get_group_desc(sb, group_no, &gdp_bh);
		if (!desc)
			goto io_error;
		group_alloc = group_reserve_blocks(sbi, group_no, desc,
						gdp_bh, es_alloc);
		if(!group_alloc){
			//printk("no free blcoks in group\n");

		}
	}
	if (!group_alloc) {
		*err = -ENOSPC;
		goto out_release;
	}
	brelse(bitmap_bh);
	brelse(lowPriority_bh);	//	JC
	brelse(dirty_bh);		//JC
	bitmap_bh = read_block_bitmap(sb, group_no);
	lowPriority_bh = read_lowPriority_bitmap(sb, group_no); //JC
	dirty_bh = read_dirty_bitmap(sb, group_no);		//JC
	if (!bitmap_bh || ! lowPriority_bh || !dirty_bh)	//JC
		goto io_error;

	//ret_block = grab_block(sb_bgl_lock(sbi, group_no), bitmap_bh->b_data,
	//			group_size, 0);
	ret_block = grab_block(sb_bgl_lock(sbi, group_no),
			bitmap_bh->b_data, lowPriority_bh->b_data, dirty_bh->b_data, 
			sbi->s_blocklock_bitmaps[group_no],
			group_size, 0,priority);
	if (ret_block < 0) {
		/*
		 * If a free block counter is corrupted we can loop inifintely.
		 * Detect that here.
		 */
		nr_scanned_groups++;
		if (nr_scanned_groups > 2 * sbi->s_groups_count) {
			//if we are allocating low priority, we can't trust
			//the free block counters, so this is not neccessarily
			//an io error
			if(priority!=1){
				*err = -ENOSPC;
				goto out_release;
			}
			lpfs_error(sb, "lpfs_new_block",
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
	lpfs_debug("using block group %d(%d)\n",
		group_no, desc->bg_free_blocks_count);

	target_block = ret_block + group_no * group_size +
			le32_to_cpu(es->s_first_data_block);

	if (target_block == le32_to_cpu(desc->bg_block_bitmap) ||
	    target_block == le32_to_cpu(desc->bg_inode_bitmap) ||
	    in_range(target_block, le32_to_cpu(desc->bg_inode_table),
		      sbi->s_itb_per_group))
		lpfs_error (sb, "lpfs_new_block",
			    "Allocating block in system zone - "
			    "block = %u", target_block);

	if (target_block >= le32_to_cpu(es->s_blocks_count)) {
		lpfs_error (sb, "lpfs_new_block",
			    "block(%d) >= blocks count(%d) - "
			    "block_group = %d, es == %p ", ret_block,
			le32_to_cpu(es->s_blocks_count), group_no, es);
		goto io_error;
	}
	block = target_block;

	/* OK, we _had_ allocated something */
	lpfs_debug("found bit %d\n", ret_block);

	tdq_alloc=dq_alloc;
	tes_alloc=es_alloc;
	tgroup_alloc=group_alloc;
	dq_alloc--;
	es_alloc--;
	group_alloc--;

	//spin_lock(sb_bgl_lock(sbi, group_no));
	//	printk("allocating block %i - pri=%i -  %i %i %i\n",
	//		block, priority,
	//		lpfs_test_bit(ret_block, bitmap_bh->b_data),
	//		lpfs_test_bit(ret_block, lowPriority_bh->b_data),
	//		lpfs_test_bit(ret_block, dirty_bh->b_data));
	//spin_unlock(sb_bgl_lock(sbi, group_no));
	/*
	 * Do block preallocation now if required.
	 */
	write_lock(&LPFS_I(inode)->i_meta_lock);
//if(priority==-1){ priority=0;}		//////////////////////////////////////////////////////////////////////////////////
	if (group_alloc && !*prealloc_count) {
		unsigned n;

		for (n = 0; n < group_alloc && ++ret_block < group_size; n++) {
			if (lpfs_p_set_bit_atomic(sb_bgl_lock(sbi, group_no),
						ret_block,
						(void*) bitmap_bh->b_data,
						(void *) lowPriority_bh->b_data,
						(void *) dirty_bh->b_data,
						(void *)sbi->s_blocklock_bitmaps[group_no],
						priority))
 				break;
			//spin_lock(sb_bgl_lock(sbi, group_no));
			//	printk("allocating block %i ~ pri=%i ~  %i %i %i\n",
			//		block, priority,
			//		lpfs_test_bit(ret_block, bitmap_bh->b_data),
			//		lpfs_test_bit(ret_block, lowPriority_bh->b_data),
			//		lpfs_test_bit(ret_block, dirty_bh->b_data));
			//spin_unlock(sb_bgl_lock(sbi, group_no));
		}
		*prealloc_block = block + 1;
		*prealloc_count = n;
		es_alloc -= n;
		dq_alloc -= n;
		group_alloc -= n;
	}
	write_unlock(&LPFS_I(inode)->i_meta_lock);

	mark_buffer_dirty(bitmap_bh);
	mark_buffer_dirty(lowPriority_bh);
	mark_buffer_dirty(dirty_bh);
	if (sb->s_flags & MS_SYNCHRONOUS){
		sync_dirty_buffer(bitmap_bh);
		sync_dirty_buffer(lowPriority_bh);
		sync_dirty_buffer(dirty_bh);
	}

	lpfs_debug ("allocating block %d. ", block);

	*err = 0;
	//JC - FIXME: low priority allocations should not affect quotas/reservations
	//but they do for testing purposes
	if(priority==0){ 		//FIXME: put this back in for release
		//printk("un-reserving blocks %i %i %i\n",tgroup_alloc, tes_alloc, tdq_alloc);
		group_release_blocks(sb, group_no, desc, gdp_bh, tgroup_alloc);
		release_blocks(sb, tes_alloc);
		DQUOT_FREE_BLOCK(inode, tdq_alloc);
	}
out_release:
	group_release_blocks(sb, group_no, desc, gdp_bh, group_alloc);
	release_blocks(sb, es_alloc);
out_dquot:
	DQUOT_FREE_BLOCK(inode, dq_alloc);
out:
	brelse(bitmap_bh);
	brelse(lowPriority_bh);
	brelse(dirty_bh);
	return block;

io_error:
	*err = -EIO;
	goto out_release;
}

unsigned long lpfs_count_free_blocks (struct super_block * sb)
{
	struct lpfs_group_desc * desc;
	unsigned long desc_count = 0;
	int i;
#ifdef LPFSFS_DEBUG
	unsigned long bitmap_count, x;
	struct lpfs_super_block *es;

	lock_super (sb);
	es = LPFS_SB(sb)->s_es;
	desc_count = 0;
	bitmap_count = 0;
	desc = NULL;
	for (i = 0; i < LPFS_SB(sb)->s_groups_count; i++) {
		struct buffer_head *bitmap_bh;
		desc = lpfs_get_group_desc (sb, i, NULL);
		if (!desc)
			continue;
		desc_count += le16_to_cpu(desc->bg_free_blocks_count);
		bitmap_bh = read_block_bitmap(sb, i);
		if (!bitmap_bh)
			continue;
		
		x = lpfs_count_free(bitmap_bh, sb->s_blocksize);
		//printk ("group %d: stored = %d, counted = %lu\n",
		//	i, le16_to_cpu(desc->bg_free_blocks_count), x);
		bitmap_count += x;
		brelse(bitmap_bh);
	}
	//printk("lpfs_count_free_blocks: stored = %lu, computed = %lu, %lu\n",
	//	(long)le32_to_cpu(es->s_free_blocks_count),
	//	desc_count, bitmap_count);
	unlock_super (sb);
	return bitmap_count;
#else
        for (i = 0; i < LPFS_SB(sb)->s_groups_count; i++) {
                desc = lpfs_get_group_desc (sb, i, NULL);
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
	return lpfs_test_bit ((block - le32_to_cpu(LPFS_SB(sb)->s_es->s_first_data_block)) %
			 LPFS_BLOCKS_PER_GROUP(sb), map);
}

static inline int test_root(int a, int b)
{
	if (a == 0)
		return 1;
	while (1) {
		if (a == 1)
			return 1;
		if (a % b)
			return 0;
		a = a / b;
	}
}

static int lpfs_group_sparse(int group)
{
	return (test_root(group, 3) || test_root(group, 5) ||
		test_root(group, 7));
}

/**
 *	lpfs_bg_has_super - number of blocks used by the superblock in group
 *	@sb: superblock for filesystem
 *	@group: group number to check
 *
 *	Return the number of blocks used by the superblock (primary or backup)
 *	in this group.  Currently this will be only 0 or 1.
 */
int lpfs_bg_has_super(struct super_block *sb, int group)
{
	if (LPFS_HAS_RO_COMPAT_FEATURE(sb,LPFS_FEATURE_RO_COMPAT_SPARSE_SUPER)&&
	    !lpfs_group_sparse(group))
		return 0;
	return 1;
}

/**
 *	lpfs_bg_num_gdb - number of blocks used by the group table in group
 *	@sb: superblock for filesystem
 *	@group: group number to check
 *
 *	Return the number of blocks used by the group descriptor table
 *	(primary or backup) in this group.  In the future there may be a
 *	different number of descriptor blocks in each group.
 */
unsigned long lpfs_bg_num_gdb(struct super_block *sb, int group)
{
	if (LPFS_HAS_RO_COMPAT_FEATURE(sb,LPFS_FEATURE_RO_COMPAT_SPARSE_SUPER)&&
	    !lpfs_group_sparse(group))
		return 0;
	return LPFS_SB(sb)->s_gdb_count;
}

#ifdef CONFIG_LPFS_CHECK
/* Called at mount-time, super-block is locked */
void lpfs_check_blocks_bitmap (struct super_block * sb)
{
	struct buffer_head *bitmap_bh = NULL;
	struct lpfs_super_block * es;
	unsigned long desc_count, bitmap_count, x, j;
	unsigned long desc_blocks;
	struct lpfs_group_desc * desc;
	int i;

	es = LPFS_SB(sb)->s_es;
	desc_count = 0;
	bitmap_count = 0;
	desc = NULL;
	for (i = 0; i < LPFS_SB(sb)->s_groups_count; i++) {
		desc = lpfs_get_group_desc (sb, i, NULL);
		if (!desc)
			continue;
		desc_count += le16_to_cpu(desc->bg_free_blocks_count);
		brelse(bitmap_bh);
		bitmap_bh = read_block_bitmap(sb, i);
		if (!bitmap_bh)
			continue;

		if (lpfs_bg_has_super(sb, i) &&
				!lpfs_test_bit(0, bitmap_bh->b_data))
			lpfs_error(sb, __FUNCTION__,
				   "Superblock in group %d is marked free", i);

		desc_blocks = lpfs_bg_num_gdb(sb, i);
		for (j = 0; j < desc_blocks; j++)
			if (!lpfs_test_bit(j + 1, bitmap_bh->b_data))
				lpfs_error(sb, __FUNCTION__,
					   "Descriptor block #%ld in group "
					   "%d is marked free", j, i);

		if (!block_in_use(le32_to_cpu(desc->bg_block_bitmap),
					sb, bitmap_bh->b_data))
			lpfs_error(sb, "lpfs_check_blocks_bitmap",
				    "Block bitmap for group %d is marked free",
				    i);

		if (!block_in_use(le32_to_cpu(desc->bg_inode_bitmap),
					sb, bitmap_bh->b_data))
			lpfs_error(sb, "lpfs_check_blocks_bitmap",
				    "Inode bitmap for group %d is marked free",
				    i);

		for (j = 0; j < LPFS_SB(sb)->s_itb_per_group; j++)
			if (!block_in_use(le32_to_cpu(desc->bg_inode_table) + j,
						sb, bitmap_bh->b_data))
				lpfs_error (sb, "lpfs_check_blocks_bitmap",
					    "Block #%ld of the inode table in "
					    "group %d is marked free", j, i);

		x = lpfs_count_free(bitmap_bh, sb->s_blocksize);
		if (le16_to_cpu(desc->bg_free_blocks_count) != x)
			lpfs_error (sb, "lpfs_check_blocks_bitmap",
				    "Wrong free blocks count for group %d, "
				    "stored = %d, counted = %lu", i,
				    le16_to_cpu(desc->bg_free_blocks_count), x);
		bitmap_count += x;
	}
	if (le32_to_cpu(es->s_free_blocks_count) != bitmap_count)
		lpfs_error (sb, "lpfs_check_blocks_bitmap",
			"Wrong free blocks count in super block, "
			"stored = %lu, counted = %lu",
			(unsigned long)le32_to_cpu(es->s_free_blocks_count),
			bitmap_count);
	brelse(bitmap_bh);
}
#endif
