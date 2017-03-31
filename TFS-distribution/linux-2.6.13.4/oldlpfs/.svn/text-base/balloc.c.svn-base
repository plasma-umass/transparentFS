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
#include <linux/fs.h>
#include "lpfs_fs.h"
//#include <linux/locks.h>
#include <linux/quotaops.h>

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

/* JC variables to store timing information */
#ifdef LPFS_BENCHMARK
//FIXME these variables should be in the superblock, not global
__u64 lpfs_hpballoc_time_total=0;
__u64 lpfs_hpballoc_time_count=0;
__u64 lpfs_hpialloc_time_total=0;
__u64 lpfs_hpialloc_time_count=0;
__u64 lpfs_hpread_time_total=0;
__u64 lpfs_hpread_time_count=0;
__u64 lpfs_hpwrite_time_total=0;
__u64 lpfs_hpwrite_time_count=0;



__u64 lpfs_lpballoc_time_total=0;
__u64 lpfs_lpballoc_time_count=0;
__u64 lpfs_lpialloc_time_total=0;
__u64 lpfs_lpialloc_time_count=0;
__u64 lpfs_lpread_time_total=0;
__u64 lpfs_lpread_time_count=0;
__u64 lpfs_lpwrite_time_total=0;
__u64 lpfs_lpwrite_time_count=0;


#endif
/* /JC */

#define in_range(b, first, len)		((b) >= (first) && (b) <= (first) + (len) - 1)

struct lpfs_group_desc * lpfs_get_group_desc(struct super_block * sb,
					     unsigned int block_group,
					     struct buffer_head ** bh)
{
	unsigned long group_desc;
	unsigned long desc;
	struct lpfs_group_desc * gdp;

	if (block_group >= LPFS_SBI(sb).s_groups_count) {
		lpfs_error (sb, "lpfs_get_group_desc",
			    "block_group >= groups_count - "
			    "block_group = %d, groups_count = %lu",
			    block_group, LPFS_SBI(sb).s_groups_count);

		return NULL;
	}
	
	group_desc = block_group / LPFS_DESC_PER_BLOCK(sb);
	desc = block_group % LPFS_DESC_PER_BLOCK(sb);
	if (!LPFS_SBI(sb).s_group_desc[group_desc]) {
		lpfs_error (sb, "lpfs_get_group_desc",
			    "Group descriptor not loaded - "
			    "block_group = %d, group_desc = %lu, desc = %lu",
			     block_group, group_desc, desc);
		return NULL;
	}
	
	gdp = (struct lpfs_group_desc *) 
	      (LPFS_SBI(sb).s_group_desc[group_desc])->b_data;
	if (bh)
		*bh = LPFS_SBI(sb).s_group_desc[group_desc];
	return gdp + desc;
}

/*
 * Read the bitmap for a given block_group, reading into the specified 
 * slot in the superblock's bitmap cache.
 *
 * Return >=0 on success or a -ve error code.
 */

static int read_block_bitmap (struct super_block * sb,
			       unsigned int block_group,
			       unsigned long bitmap_nr)
{
	struct lpfs_group_desc * gdp;
	struct buffer_head * bh = NULL;
	int retval = -EIO;
	/* JC */
	//buffer heads for low priority and dirty bitmaps
	struct buffer_head *lpbh=NULL;
	struct buffer_head *dbh=NULL;
	/* /JC */
	
	gdp = lpfs_get_group_desc (sb, block_group, NULL);
	if (!gdp)
		goto error_out;
	retval = 0;
	bh = sb_bread(sb, le32_to_cpu(gdp->bg_block_bitmap));
	/* JC */
	//read low priority and dirty buffer heads
	lpbh = sb_bread(sb, le32_to_cpu(gdp->bg_lowPriority_bitmap));
	dbh = sb_bread(sb, le32_to_cpu(gdp->bg_dirty_bitmap));
	//check all buffer heads for io errors
	//if (!bh) {	REPLACED
	if(!bh || !lpbh || !dbh){
	//added dirty and low priority bitmaps to the error message 
		lpfs_error (sb, "read_block_bitmap",		//ORIGINAL
			    "Cannot read block bitmap - "	//ORIGINAL
			    "block_group = %d, block_bitmap = %lu"	//ORIGINAL
			    " low priority bitmap = %lu "
			    "dirty bitmap = %lu",
			    block_group, (unsigned long) gdp->bg_block_bitmap,	//ORIGINAL
			    (unsigned long) gdp->bg_lowPriority_bitmap,
			    (unsigned long) gdp->bg_dirty_bitmap);
		//release buffer heads that actually were read
		if(bh){ brelse(bh); bh=NULL; }
		if(lpbh){ brelse(lpbh); lpbh=NULL; }
		if(dbh){ brelse(dbh); dbh=NULL; }
	/* /JC */
		retval = -EIO;
	}
	/*
	 * On IO error, just leave a zero in the superblock's block pointer for
	 * this group.  The IO will be retried next time.
	 */
error_out:
	LPFS_SBI(sb).s_block_bitmap_number[bitmap_nr] = block_group;
	LPFS_SBI(sb).s_block_bitmap[bitmap_nr] = bh;
	/* JC */
	//set low priority and dirty bitmaps too
	LPFS_SBI(sb).s_lowPriority_bitmap[bitmap_nr]=lpbh;
	LPFS_SBI(sb).s_dirty_bitmap[bitmap_nr]=dbh;
	/* /JC */
	return retval;
}

/*
 * load_block_bitmap loads the block bitmap for a blocks group
 *
 * It maintains a cache for the last bitmaps loaded.  This cache is managed
 * with a LRU algorithm.
 *
 * Notes:
 * 1/ There is one cache per mounted file system.
 * 2/ If the file system contains less than LPFS_MAX_GROUP_LOADED groups,
 *    this function reads the bitmap without maintaining a LRU cache.
 * 
 * Return the slot used to store the bitmap, or a -ve error code.
 */
static int __load_block_bitmap (struct super_block * sb,
			        unsigned int block_group)
{
	int i, j, retval = 0;
	unsigned long block_bitmap_number;
	struct buffer_head * block_bitmap;
	/* JC */
	//buffer heads for low priority and dirty bitmaps
	struct buffer_head * lowPriority_bitmap;
	struct buffer_head * dirty_bitmap;
	/* /JC */

	if (block_group >= LPFS_SBI(sb).s_groups_count)
		lpfs_panic (sb, "load_block_bitmap",
			    "block_group >= groups_count - "
			    "block_group = %d, groups_count = %lu",
			    block_group, LPFS_SBI(sb).s_groups_count);

	if (LPFS_SBI(sb).s_groups_count <= LPFS_MAX_GROUP_LOADED) {
		/* JC */
		//check low priority and dirty bitmaps too
		//if (LPFS_SBI(sb).s_block_bitmap[block_group]) {	REPLACED
		if (LPFS_SBI(sb).s_block_bitmap[block_group] 
				&&LPFS_SBI(sb).s_lowPriority_bitmap[block_group]
				&&LPFS_SBI(sb).s_dirty_bitmap[block_group]) {
		/* /JC */
			if (LPFS_SBI(sb).s_block_bitmap_number[block_group] ==
			    block_group)
				return block_group;
			lpfs_error (sb, "__load_block_bitmap",
				    "block_group != block_bitmap_number");
		}
		retval = read_block_bitmap (sb, block_group, block_group);
		if (retval < 0)
			return retval;
		return block_group;
	}

	for (i = 0; i < LPFS_SBI(sb).s_loaded_block_bitmaps &&
		    LPFS_SBI(sb).s_block_bitmap_number[i] != block_group; i++)
		;
	if (i < LPFS_SBI(sb).s_loaded_block_bitmaps &&
  	    LPFS_SBI(sb).s_block_bitmap_number[i] == block_group) {
		block_bitmap_number = LPFS_SBI(sb).s_block_bitmap_number[i];
		block_bitmap = LPFS_SBI(sb).s_block_bitmap[i];
		/* JC */
		//get low priority and dirty bitmaps too
		lowPriority_bitmap = LPFS_SBI(sb).s_lowPriority_bitmap[i];
		dirty_bitmap = LPFS_SBI(sb).s_dirty_bitmap[i];
		/* /JC */
		for (j = i; j > 0; j--) {
			LPFS_SBI(sb).s_block_bitmap_number[j] =
				LPFS_SBI(sb).s_block_bitmap_number[j - 1];
			LPFS_SBI(sb).s_block_bitmap[j] =
				LPFS_SBI(sb).s_block_bitmap[j - 1];
			/* JC */
			//shift low priority and dirty bitmaps up a notch too
			LPFS_SBI(sb).s_lowPriority_bitmap[j] =
				LPFS_SBI(sb).s_lowPriority_bitmap[j - 1];
			LPFS_SBI(sb).s_dirty_bitmap[j] =
				LPFS_SBI(sb).s_dirty_bitmap[j - 1];
			/* /JC */
		}
		LPFS_SBI(sb).s_block_bitmap_number[0] = block_bitmap_number;
		LPFS_SBI(sb).s_block_bitmap[0] = block_bitmap;
		/* JC */
		//set the top one in the LRU list to be the ones requested
		LPFS_SBI(sb).s_lowPriority_bitmap[0] = lowPriority_bitmap;
		LPFS_SBI(sb).s_dirty_bitmap[0] = dirty_bitmap;
		/* /JC */

		/*
		 * There's still one special case here --- if block_bitmap == 0
		 * then our last attempt to read the bitmap failed and we have
		 * just ended up caching that failure.  Try again to read it.
		 */
		 /* JC */
		 //we need to check the lowPriority and dirty bitmaps too
		//if (!block_bitmap)	REPLACED
		if(!block_bitmap || !lowPriority_bitmap || !dirty_bitmap){
			//printk("__load_block_bitmap REREADING BITMAPS\n");
			retval = read_block_bitmap (sb, block_group, 0);	//ORIGINAL
		}
		/* /JC */
	} else {
		/* JC */
		//add some curley brackets to these if statements
		//if (LPFS_SBI(sb).s_loaded_block_bitmaps < LPFS_MAX_GROUP_LOADED)	REPLACED
		if (LPFS_SBI(sb).s_loaded_block_bitmaps < LPFS_MAX_GROUP_LOADED){
			LPFS_SBI(sb).s_loaded_block_bitmaps++;	//ORIGINAL
		//else	REPLACED
		}else{
			brelse (LPFS_SBI(sb).s_block_bitmap[LPFS_MAX_GROUP_LOADED - 1]);	//ORIGINAL
			//release low priority and dirty bitmaps too
			brelse (LPFS_SBI(sb).s_lowPriority_bitmap[LPFS_MAX_GROUP_LOADED - 1]);
			brelse (LPFS_SBI(sb).s_dirty_bitmap[LPFS_MAX_GROUP_LOADED - 1]);
		}
		/* /JC */
		for (j = LPFS_SBI(sb).s_loaded_block_bitmaps - 1; j > 0; j--) {
			LPFS_SBI(sb).s_block_bitmap_number[j] =
				LPFS_SBI(sb).s_block_bitmap_number[j - 1];
			LPFS_SBI(sb).s_block_bitmap[j] =
				LPFS_SBI(sb).s_block_bitmap[j - 1];
			/* JC */
			//shift low priority and dirty bitmaps too
			LPFS_SBI(sb).s_lowPriority_bitmap[j] =
				LPFS_SBI(sb).s_lowPriority_bitmap[j - 1];
			LPFS_SBI(sb).s_dirty_bitmap[j] =
				LPFS_SBI(sb).s_dirty_bitmap[j - 1];
			/* /JC */
		}
		retval = read_block_bitmap (sb, block_group, 0);
	}
	return retval;
}

/*
 * Load the block bitmap for a given block group.  First of all do a couple
 * of fast lookups for common cases and then pass the request onto the guts
 * of the bitmap loader.
 *
 * Return the slot number of the group in the superblock bitmap cache's on
 * success, or a -ve error code.
 *
 * There is still one inconsistency here --- if the number of groups in this
 * filesystems is <= LPFS_MAX_GROUP_LOADED, then we have no way of 
 * differentiating between a group for which we have never performed a bitmap
 * IO request, and a group for which the last bitmap read request failed.
 */
static inline int load_block_bitmap (struct super_block * sb,
				     unsigned int block_group)
{
	int slot;
	
	/*
	 * Do the lookup for the slot.  First of all, check if we're asking
	 * for the same slot as last time, and did we succeed that last time?
	 */
	 /* JC */
	 //check low priority and dirty bitmaps too
	if (LPFS_SBI(sb).s_loaded_block_bitmaps > 0 &&		//ORIGINAL
	    LPFS_SBI(sb).s_block_bitmap_number[0] == block_group &&	//ORIGINAL
	    //LPFS_SBI(sb).s_block_bitmap[0]) {	REPLACED
	    LPFS_SBI(sb).s_block_bitmap[0] &&
	    LPFS_SBI(sb).s_lowPriority_bitmap[0] &&
	    LPFS_SBI(sb).s_dirty_bitmap[0]){
		return 0;	//ORIGINAL
	}	//ORIGINAL
	/*
	 * Or can we do a fast lookup based on a loaded group on a filesystem
	 * small enough to be mapped directly into the superblock?
	 */
	else if (LPFS_SBI(sb).s_groups_count <= LPFS_MAX_GROUP_LOADED && 	//ORIGINAL
			LPFS_SBI(sb).s_block_bitmap_number[block_group] == block_group &&	//ORIGINAL
		// LPFS_SBI(sb).s_block_bitmap[block_group]) {		REPLACED
			LPFS_SBI(sb).s_block_bitmap[block_group]&&
			LPFS_SBI(sb).s_lowPriority_bitmap[block_group] &&
			LPFS_SBI(sb).s_dirty_bitmap[block_group]){
		slot = block_group;	//ORIGINAL
	}	//ORIGINAL
	/* /JC */
	/*
	 * If not, then do a full lookup for this block group.
	 */
	else {
		slot = __load_block_bitmap (sb, block_group);
	}

	/*
	 * <0 means we just got an error
	 */
	if (slot < 0)
		return slot;
	
	/*
	 * If it's a valid slot, we may still have cached a previous IO error,
	 * in which case the bh in the superblock cache will be zero.
	 */
	 /* JC */
	 //check low priority and dirty bitmaps too
	//if (!LPFS_SBI(sb).s_block_bitmap[slot])	REPLACED
	if(!LPFS_SBI(sb).s_block_bitmap[slot] ||
			!LPFS_SBI(sb).s_lowPriority_bitmap[slot] ||
			!LPFS_SBI(sb).s_dirty_bitmap[slot]){
		printk("LPFS IO ERROR IN LOAD BLOCK BITMAP\n");
		if(!LPFS_SBI(sb).s_block_bitmap[slot]){
			printk("\tproblem with hp bitmap\n");
		}
		if(!LPFS_SBI(sb).s_lowPriority_bitmap[slot]){
			printk("\tproblem with lp bitmap\n");
		}
		if(!LPFS_SBI(sb).s_dirty_bitmap[slot]){
			printk("\tproblem with dirty bitmap\n");
		}
		return -EIO;	//ORIGINAL
	}
	/* /JC */
	
	/*
	 * Must have been read in OK to get this far.
	 */
	return slot;
}


/* Free given blocks, update quota and i_blocks field */
void lpfs_free_blocks (struct inode * inode, unsigned long block,
		       unsigned long count)
{
	struct buffer_head * bh;
	struct buffer_head * bh2;
	/* JC */
	//low priority and dirty buffer heads
	struct buffer_head *lpbh;
	struct buffer_head *dbh;
	/* /JC */
	unsigned long block_group;
	unsigned long bit;
	unsigned long i;
	int bitmap_nr;
	unsigned long overflow;
	struct super_block * sb;
	struct lpfs_group_desc * gdp;
	struct lpfs_super_block * es;

	sb = inode->i_sb;
	if (!sb) {
		printk ("lpfs_free_blocks: nonexistent device");
		return;
	}
	lock_super (sb);
	es = LPFS_SBI(sb).s_es;
	if (block < le32_to_cpu(es->s_first_data_block) ||
	    block + count < block ||
	    (block + count) > le32_to_cpu(es->s_blocks_count)) {
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
	bitmap_nr = load_block_bitmap (sb, block_group);
	if (bitmap_nr < 0)
		goto error_return;
	
	bh = LPFS_SBI(sb).s_block_bitmap[bitmap_nr];
	/* JC */
	//get low priority and dirty bitmaps too
	lpbh = LPFS_SBI(sb).s_lowPriority_bitmap[bitmap_nr];
	dbh = LPFS_SBI(sb).s_dirty_bitmap[bitmap_nr];
	/* /JC */
	gdp = lpfs_get_group_desc (sb, block_group, &bh2);
	if (!gdp)
		goto error_return;

	for (i = 0; i < count; i++, block++) {
		if (block == le32_to_cpu(gdp->bg_block_bitmap) ||
		    block == le32_to_cpu(gdp->bg_inode_bitmap) ||
		    in_range(block, le32_to_cpu(gdp->bg_inode_table),
			     LPFS_SBI(sb).s_itb_per_group)) {
			lpfs_error(sb, __FUNCTION__,
				   "Freeing block in system zone - block = %lu",
				   block);
			continue;
		}
/* JC */
//We need to clear the appropriate bit
//if priority == 1 we clear the hp bit
//if priority == 0 and lp is set, we clear the lp and dirty bits
//if priority == 0 and lp is not set, we clear the hp bit (this is useful
//for lp indirect blocks, which are marked as hp, but never overwrite lp
//blocks)
/* REPLACED
		if (!lpfs_clear_bit (bit + i, bh->b_data))
			lpfs_error(sb, __FUNCTION__,
				   "bit already cleared for block %lu", block);

		else {
			DQUOT_FREE_BLOCK(inode, 1);
			gdp->bg_free_blocks_count =
				cpu_to_le16(le16_to_cpu(gdp->bg_free_blocks_count)+1);
			es->s_free_blocks_count =
				cpu_to_le32(le32_to_cpu(es->s_free_blocks_count)+1);
		}
*/
		if(LPFS_IN(inode).i_priority==1 || (LPFS_IN(inode).i_priority==0 &&  
		  !lpfs_test_bit(bit+i, lpbh->b_data))  ){
			if(!lpfs_clear_bit(bit+i, bh->b_data)){
				lpfs_error(sb, __FUNCTION__,
					"bit already cleared for block %lu",block);
			}else{
				DQUOT_FREE_BLOCK(inode, 1);
				gdp->bg_free_blocks_count =
					cpu_to_le16(le16_to_cpu(gdp->bg_free_blocks_count)+1);
				es->s_free_blocks_count =
					cpu_to_le32(le32_to_cpu(es->s_free_blocks_count)+1);
			}
		}else if(LPFS_IN(inode).i_priority==0 && lpfs_test_bit(bit+i, lpbh->b_data)){
			lpfs_clear_bit(bit+i, lpbh->b_data);
			lpfs_clear_bit(bit+i, dbh->b_data);
			lpfs_clear_bit(bit+i, LPFS_SBI(sb).s_blockLock_bitmaps[block_group]);
			DQUOT_FREE_BLOCK(inode, 1);
		}
/* /JC */
	}
	
	mark_buffer_dirty(bh2);
	mark_buffer_dirty(LPFS_SBI(sb).s_sbh);

	mark_buffer_dirty(bh);
	/* JC */
	//write out low priority and dirty buffer heads too
	//FIXME only write out changed buffer heads
	//this can be optimised to only write out buffer heads that we have actually changed
	mark_buffer_dirty(lpbh);
	mark_buffer_dirty(dbh);
	/* /JC */
	if (sb->s_flags & MS_SYNCHRONOUS) {
		ll_rw_block (WRITE, 1, &bh);
		wait_on_buffer (bh);
		/* JC */
		//synchronize low priority and dirty buffer heads too
		ll_rw_block (WRITE, 1, &lpbh);
		wait_on_buffer (lpbh);
		ll_rw_block (WRITE, 1, &dbh);
		wait_on_buffer (dbh);
		/* /JC */
	}
	if (overflow) {
		count = overflow;
		goto do_more;
	}
	sb->s_dirt = 1;
error_return:
	unlock_super (sb);
	return;
}

/*
 * lpfs_new_block uses a goal block to assist allocation.  If the goal is
 * free, or there is a free block within 32 blocks of the goal, that block
 * is allocated.  Otherwise a forward search is made for a free block; within 
 * each block group the search first looks for an entire free byte in the block
 * bitmap, and then for any free bit if that fails.
 * This function also updates quota and i_blocks field.
 */
 /* JC */
 //added a priority argument to new block
 //even though the inode knows its priority, we may be allocating
 //an indirect block, so we need inode.c to tell us how to allocate this one
 //FIXME check priority of inode and argument
 //right now we completely trust the priority argument, we may want to override
 //this in the case of high priority files


int lpfs_new_block_helper (struct inode * inode, unsigned long goal,
 	u32 * prealloc_count, u32 * prealloc_block, int * err,int priority);

int lpfs_new_block (struct inode * inode, unsigned long goal,	//ORIGINAL
 	u32 * prealloc_count, u32 * prealloc_block, int * err,int priority)
{
	unsigned int i,j;
	int r;
#ifdef LPFS_BENCHMARK
	j=get_cycles();
#endif
	r=lpfs_new_block_helper(inode,goal,prealloc_count, 
		prealloc_block, err,priority);
#ifdef LPFS_BENCHMARK
	i=get_cycles();
	if(i % LPFS_SAMPLE_FREQUENCY==0){
		//take care of i get_cycles() wrapping around
		//this happens less than once every 3500 seconds
		//so if it ends up happening more than once, 
		//something is pretty broken
		if(i<j){ j= (4294967295-j)+i; }
		else { j=i-j; }
		if(priority==1){
			lpfs_hpballoc_time_total+=j;
			lpfs_hpballoc_time_count++;
		}else{
			lpfs_lpballoc_time_total+=j;
			lpfs_lpballoc_time_count++;
		}
	}
#endif
	return r;
}


int lpfs_new_block_helper  (struct inode * inode, unsigned long goal,
 //   u32 * prealloc_count, u32 * prealloc_block, int * err)	REPLACED
 	u32 * prealloc_count, u32 * prealloc_block, int * err,int priority)
/* /JC */
{
	struct buffer_head * bh;
	struct buffer_head * bh2;
	/* JC */
	//add buffer heads for low priority and dirty bitmaps
	struct buffer_head * lpbh;
	struct buffer_head * dbh;
	//char * p, * r;	REPLACED
	char *p;
	char *blocklock;
	/* /JC */
	int i, j, k, tmp;
	int bitmap_nr;
	struct super_block * sb;
	struct lpfs_group_desc * gdp;
	struct lpfs_super_block * es;
#ifdef LPFSFS_DEBUG
	static int goal_hits = 0, goal_attempts = 0;
#endif
	*err = -ENOSPC;
	sb = inode->i_sb;
	if (!sb) {
		printk ("lpfs_new_block: nonexistent device");
		return 0;
	}

	lock_super (sb);
	es = LPFS_SBI(sb).s_es;
	if (le32_to_cpu(es->s_free_blocks_count) <= le32_to_cpu(es->s_r_blocks_count) &&
	    ((LPFS_SBI(sb).s_resuid != current->fsuid) &&
	     (LPFS_SBI(sb).s_resgid == 0 ||
	      !in_group_p (LPFS_SBI(sb).s_resgid)) && 
	     !capable(CAP_SYS_RESOURCE)))
		goto out;

	lpfs_debug ("goal=%lu.\n", goal);
repeat:
	/*
	 * First, test whether the goal block is free.
	 */
	if (goal < le32_to_cpu(es->s_first_data_block) ||
	    goal >= le32_to_cpu(es->s_blocks_count))
		goal = le32_to_cpu(es->s_first_data_block);
	i = (goal - le32_to_cpu(es->s_first_data_block)) / LPFS_BLOCKS_PER_GROUP(sb);
	gdp = lpfs_get_group_desc (sb, i, &bh2);
	if (!gdp)
		goto io_error;
	if (le16_to_cpu(gdp->bg_free_blocks_count) > 0) {
		j = ((goal - le32_to_cpu(es->s_first_data_block)) % LPFS_BLOCKS_PER_GROUP(sb));
#ifdef LPFSFS_DEBUG
		if (j)
			goal_attempts++;
#endif
		bitmap_nr = load_block_bitmap (sb, i);
		if (bitmap_nr < 0)
			goto io_error;		
		bh = LPFS_SBI(sb).s_block_bitmap[bitmap_nr];
		/* JC */
		//get low priority and dirty buffer heads
		lpbh = LPFS_SBI(sb).s_lowPriority_bitmap[bitmap_nr];
		dbh = LPFS_SBI(sb).s_dirty_bitmap[bitmap_nr];
		blocklock=LPFS_SBI(sb).s_blockLock_bitmaps[i];
		/* /JC */

		lpfs_debug ("goal is at %d:%d.\n", i, j);

		/* JC */
		//rather than calling test bit, we now call a function which
		//tests a bit for allocation using the appropriate tests
		//if (!lpfs_test_bit(j, bh->b_data)) {		ORIGINAL
		if(!lpfs_p_test_bit(j, bh->b_data, lpbh->b_data, dbh->b_data,blocklock, priority)){
		/* /JC */
			lpfs_debug("goal bit allocated, %d hits\n",++goal_hits);
			goto got_block;
		}
		if (j) {
//printk("lpfs_new_block %i\n",2); //JC
			/*
			 * The goal was occupied; search forward for a free 
			 * block within the next XX blocks.
			 *
			 * end_goal is more or less random, but it has to be
			 * less than LPFS_BLOCKS_PER_GROUP. Aligning up to the
			 * next 64-bit boundary is simple..
			 */
			int end_goal = (j + 63) & ~63;
			/* JC */
			//instead of looking at just one bitmap, we need a functon that
			//checks all relevant bitmaps
			//j = lpfs_find_next_zero_bit(bh->b_data, end_goal, j);	//ORIGINAL
			j = lpfs_p_find_next_zero_bit(bh->b_data, lpbh->b_data, dbh->b_data,blocklock,
				end_goal, j, priority);
//printk("lpfs_new_block %i\n",3); //JC
			//if (j < end_goal)	//REPLACED
			if(j < end_goal){
//printk("lpfs_new_block %i\n",4); //JC
				goto got_block;	//ORIGINAL
			}
			/* /JC */
		}
	
		lpfs_debug ("Bit not found near goal\n");

		/*
		 * There has been no free block found in the near vicinity
		 * of the goal: do a search forward through the block groups,
		 * searching in each group first for an entire free byte in
		 * the bitmap and then for any free bit.
		 * 
		 * Search first in the remainder of the current group; then,
		 * cyclicly search through the rest of the groups.
		 */
		p = ((char *) bh->b_data) + (j >> 3);
		/* JC */
		//lpfs_p_find_first_zero_byte searches for the first zero byte
		//shared by all appropriate buffer heads
		//arguments are hpbh, lpbh, dbh, start offset, end offset
		//r = memscan(p, 0, (LPFS_BLOCKS_PER_GROUP(sb) - j + 7) >> 3);	REPLACED
		k = lpfs_p_find_first_zero_byte(bh->b_data, lpbh->b_data, dbh->b_data,blocklock, j>>3, 
			(LPFS_BLOCKS_PER_GROUP(sb)) >> 3, priority);
		//k = (r - ((char *) bh->b_data)) << 3; REPLACED
		k=k<<3;
		/* /JC */
		if (k < LPFS_BLOCKS_PER_GROUP(sb)) {
			j = k;
			goto search_back;
		}
		/* JC */
		//we need to used the prioritized function for finding zero bits
		//k = lpfs_find_next_zero_bit ((unsigned long *) bh->b_data, 	//REPLACED
		//			LPFS_BLOCKS_PER_GROUP(sb),		//REPLACED
		//			j);					//REPLACED
//printk("lpfs_new_block %i\n",8); //JC
		k = lpfs_p_find_next_zero_bit(bh->b_data, lpbh->b_data, dbh->b_data, blocklock,
					LPFS_BLOCKS_PER_GROUP(sb), j, priority);
		/* /JC */
//printk("lpfs_new_block %i\n",9); //JC
		if (k < LPFS_BLOCKS_PER_GROUP(sb)) {
			j = k;
//printk("lpfs_new_block %i\n",10); //JC
			goto got_block;
		}
	}

	lpfs_debug ("Bit not found in block group %d.\n", i);

	/*
	 * Now search the rest of the groups.  We assume that 
	 * i and gdp correctly point to the last group visited.
	 */
	 /* JC */
	 //I had to change this code to put searching a block group inside
	 //the loop, because we dont know until we have actually searched
	 //the group if we can allocate there.  Low priority files have no
	 //idea how much space they have availiable to them.  High priority
	 //files can look at the free blocks count, but if all of the free
	 //blocks belong to currently open low priority files, then we still
	 //cant allocate here
	 /* REPLACED
	for (k = 0; k < LPFS_SBI(sb).s_groups_count; k++) {
		i++;
		if (i >= LPFS_SBI(sb).s_groups_count)
			i = 0;
		gdp = lpfs_get_group_desc (sb, i, &bh2);
		if (!gdp)
			goto io_error;
		if (le16_to_cpu(gdp->bg_free_blocks_count) > 0)
			break;
	}
	if (k >= LPFS_SBI(sb).s_groups_count)
		goto out;
	bitmap_nr = load_block_bitmap (sb, i);
	if (bitmap_nr < 0)
		goto io_error;
	
	bh = LPFS_SBI(sb).s_block_bitmap[bitmap_nr];
	r = memscan(bh->b_data, 0, LPFS_BLOCKS_PER_GROUP(sb) >> 3);
	j = (r - bh->b_data) << 3;
	if (j < LPFS_BLOCKS_PER_GROUP(sb))
		goto search_back;
	else
		j = lpfs_find_first_zero_bit ((unsigned long *) bh->b_data,
					 LPFS_BLOCKS_PER_GROUP(sb));
	if (j >= LPFS_BLOCKS_PER_GROUP(sb)) {
		lpfs_error (sb, "lpfs_new_block",
			    "Free blocks count corrupted for block group %d", i);
		goto out;
	}
	REPLACED */
	//FIXME if the lpfs allocator has any problems, this is probably where they are
	for (k = 0; k < LPFS_SBI(sb).s_groups_count; k++) {
//printk("lpfs_new_block %i\n",11); //JC
		i++;
		if (i >= LPFS_SBI(sb).s_groups_count)
			i = 0;
		gdp = lpfs_get_group_desc (sb, i, &bh2);
		if (!gdp)
			goto io_error;
		if (le16_to_cpu(gdp->bg_free_blocks_count) > 0){
			bitmap_nr = load_block_bitmap (sb, i);
			if (bitmap_nr < 0)
				goto io_error;
			bh = LPFS_SBI(sb).s_block_bitmap[bitmap_nr];
			lpbh = LPFS_SBI(sb).s_lowPriority_bitmap[bitmap_nr];
			dbh = LPFS_SBI(sb).s_dirty_bitmap[bitmap_nr];
			blocklock=LPFS_SBI(sb).s_blockLock_bitmaps[i];

//printk("lpfs_new_block %i\n",12); //JC
			//FIXME warning: assignment makes pointer from integer without a cast
			j = lpfs_p_find_first_zero_byte( bh->b_data, lpbh->b_data,
				dbh->b_data, blocklock, 0, LPFS_BLOCKS_PER_GROUP(sb) >> 3,priority);
			j = (j) << 3;
//printk("lpfs_new_block %i\n",13); //JC
			if (j < LPFS_BLOCKS_PER_GROUP(sb)){
//printk("lpfs_new_block %i\n",14); //JC
				goto search_back;
			}else{
//printk("lpfs_new_block %i\n",15); //JC
			//	j=lpfs_find_next_zero_bit(bh->b_data, LPFS_BLOCKS_PER_GROUP(sb),0);
				j = lpfs_p_find_next_zero_bit(bh->b_data,
					lpbh->b_data, dbh->b_data, blocklock,
					 LPFS_BLOCKS_PER_GROUP(sb),0, priority);
//printk("lpfs_new_block %i\n",16); //JC
			}
			if(j<LPFS_BLOCKS_PER_GROUP(sb)){
//printk("lpfs_new_block %i\n",17); //JC
				goto got_block;
			}
		}	
	}
//	if (k >= LPFS_SBI(sb).s_groups_count) ORIGINAL
		goto out;	//ORIGINAL
	/* /JC */
search_back:
	/* 
	 * We have succeeded in finding a free byte in the block
	 * bitmap.  Now search backwards up to 7 bits to find the
	 * start of this group of free blocks.
	 */
	 /* JC */
	 //use the prioritized test bit
	//for (k = 0; k < 7 && j > 0 && !lpfs_p_test_bit (j - 1, bh->b_data); k++, j--);	REPLACED
	
	for (k = 0;
	 k < 7 && j > 0 && !lpfs_p_test_bit (j - 1, bh->b_data, lpbh->b_data, dbh->b_data,blocklock, priority);
	  k++, j--);
	 /* /JC */
	
got_block:

	lpfs_debug ("using block group %d(%d)\n", i, gdp->bg_free_blocks_count);

	/*
	 * Check quota for allocation of this block.
	 */
	if(DQUOT_ALLOC_BLOCK(inode, 1)) {
		*err = -EDQUOT;
		goto out;
	}

	tmp = j + i * LPFS_BLOCKS_PER_GROUP(sb) + le32_to_cpu(es->s_first_data_block);

	if (tmp == le32_to_cpu(gdp->bg_block_bitmap) ||
	    tmp == le32_to_cpu(gdp->bg_inode_bitmap) ||
	    in_range (tmp, le32_to_cpu(gdp->bg_inode_table),
		      LPFS_SBI(sb).s_itb_per_group)) {
		lpfs_error (sb, "lpfs_new_block",
			    "Allocating block in system zone - block = %u",
			    tmp);
		/* JC */
		//this should be a high priority allocation because it is
		//a mistakenly unmarked system block
		/* /JC */
		lpfs_set_bit(j, bh->b_data);
		DQUOT_FREE_BLOCK(inode, 1);
		goto repeat;
	}
/* JC */
//allocate with the given priority
//	if (lpfs_set_bit (j, bh->b_data)) {	//REPLACED
	if (lpfs_p_set_bit(j, bh->b_data, lpbh->b_data, dbh->b_data,blocklock, priority)){
/* /JC */
		lpfs_warning (sb, "lpfs_new_block",
			      "bit already set for block %d", j);
		DQUOT_FREE_BLOCK(inode, 1);
		goto repeat;
	}

	lpfs_debug ("found bit %d\n", j);

	/*
	 * Do block preallocation now if required.
	 */
#ifdef LPFS_PREALLOCATE
	/* Writer: ->i_prealloc* */
	if (prealloc_count && !*prealloc_count) {
		int	prealloc_goal;
		unsigned long next_block = tmp + 1;

		prealloc_goal = es->s_prealloc_blocks ?
			es->s_prealloc_blocks : LPFS_DEFAULT_PREALLOC_BLOCKS;

		*prealloc_block = next_block;
		/* Writer: end */
		for (k = 1;
		     k < prealloc_goal && (j + k) < LPFS_BLOCKS_PER_GROUP(sb);
		     k++, next_block++) {
			if (DQUOT_PREALLOC_BLOCK(inode, 1))
				break;
			/* Writer: ->i_prealloc* */
			/* JC */
			//if (*prealloc_block + *prealloc_count != next_block || 	REPLACED
			//    lpfs_set_bit (j + k, bh->b_data)) {			REPLACED
			if (*prealloc_block + *prealloc_count != next_block ||
			    lpfs_p_test_bit(j + k, bh->b_data,lpbh->b_data, dbh->b_data, blocklock,priority)) {
				/* Writer: end */
				DQUOT_FREE_BLOCK(inode, 1);	//ORIGINAL
 				break;				//ORIGINAL
			//}	REPLACED
			}else{
				lpfs_p_set_bit(j+k,bh->b_data, lpbh->b_data, dbh->b_data, blocklock, priority);
			}
			/* /JC */
				    	
			(*prealloc_count)++;
			/* Writer: end */
		}	
		/*
		 * As soon as we go for per-group spinlocks we'll need these
		 * done inside the loop above.
		 */
		 /* JC lp files shouldnt affect free block counts */
		 if(priority!=0){
			 gdp->bg_free_blocks_count =		//ORIGINAL
				 cpu_to_le16(le16_to_cpu(gdp->bg_free_blocks_count) -	//ORIGINAL
				 	(k - 1));		//ORIGINAL
			es->s_free_blocks_count =		//ORIGINAL
				cpu_to_le32(le32_to_cpu(es->s_free_blocks_count) -	//ORIGINAL
					(k - 1));		// ORIGINAL
		 }
		 /* /JC */
		lpfs_debug ("Preallocated a further %lu bits.\n",
			       (k - 1));
	}
#endif

	j = tmp;

	mark_buffer_dirty(bh);
	/* JC */
	//mark low priority and dirty buffers too
	mark_buffer_dirty(lpbh);
	mark_buffer_dirty(dbh);
	/* /JC */
	if (sb->s_flags & MS_SYNCHRONOUS) {
		ll_rw_block (WRITE, 1, &bh);
		wait_on_buffer (bh);
		/* JC */
		//do the same for the other bitmaps
		ll_rw_block (WRITE, 1, &lpbh);
		wait_on_buffer (lpbh);
		ll_rw_block (WRITE, 1, &dbh);
		wait_on_buffer (dbh);
		/* /JC */
	}

	if (j >= le32_to_cpu(es->s_blocks_count)) {
		lpfs_error (sb, "lpfs_new_block",
			    "block(%d) >= blocks count(%d) - "
			    "block_group = %d, es == %p ",j,
			le32_to_cpu(es->s_blocks_count), i, es);
		goto out;
	}

	lpfs_debug ("allocating block %d. "
		    "Goal hits %d of %d.\n", j, goal_hits, goal_attempts);
	/* JC lp files shouldnt affect free blocks count */
	/* REPLACED 
	gdp->bg_free_blocks_count = cpu_to_le16(le16_to_cpu(gdp->bg_free_blocks_count) - 1);
	mark_buffer_dirty(bh2);
	es->s_free_blocks_count = cpu_to_le32(le32_to_cpu(es->s_free_blocks_count) - 1);
	mark_buffer_dirty(LPFS_SBI(sb).s_sbh);
	sb->s_dirt = 1;
	/REPLACED */
	if(priority!=0){
		gdp->bg_free_blocks_count = cpu_to_le16(le16_to_cpu(gdp->bg_free_blocks_count) - 1);
		mark_buffer_dirty(bh2);
		es->s_free_blocks_count = cpu_to_le32(le32_to_cpu(es->s_free_blocks_count) - 1);
		mark_buffer_dirty(LPFS_SBI(sb).s_sbh);
		sb->s_dirt = 1;
	}
	/* /JC */
	unlock_super (sb);
	*err = 0;
	/* JC */
//	//printk("LPFS_NEW_BLOCK COMPLETED SUCCESSFULLY\n");
	/* /JC */
	return j;
	
io_error:
	/* JC */
	printk("LPFS IO ERROR IN NEW_BLOCK\n");
	/* /JC */
	*err = -EIO;
out:
	/* JC */
	printk("LPFS_NEW_BLOCK FAILED\n");
	/* /JC */
	unlock_super (sb);
	return 0;
	
}

unsigned long lpfs_count_free_blocks (struct super_block * sb)
{
#ifdef LPFSFS_DEBUG
	struct lpfs_super_block * es;
	unsigned long desc_count, bitmap_count, x;
	int bitmap_nr;
	struct lpfs_group_desc * gdp;
	int i;
	
	lock_super (sb);
	es = LPFS_SBI(sb).s_es;
	desc_count = 0;
	bitmap_count = 0;
	gdp = NULL;
	for (i = 0; i < LPFS_SBI(sb).s_groups_count; i++) {
		gdp = lpfs_get_group_desc (sb, i, NULL);
		if (!gdp)
			continue;
		desc_count += le16_to_cpu(gdp->bg_free_blocks_count);
		bitmap_nr = load_block_bitmap (sb, i);
		if (bitmap_nr < 0)
			continue;
		
		x = lpfs_count_free (LPFS_SBI(sb).s_block_bitmap[bitmap_nr],
				     sb->s_blocksize);
		printk ("group %d: stored = %d, counted = %lu\n",
			i, le16_to_cpu(gdp->bg_free_blocks_count), x);
		bitmap_count += x;
	}
	printk("lpfs_count_free_blocks: stored = %lu, computed = %lu, %lu\n",
	       le32_to_cpu(es->s_free_blocks_count), desc_count, bitmap_count);
	unlock_super (sb);
	return bitmap_count;
#else
	return le32_to_cpu(LPFS_SBI(sb).s_es->s_free_blocks_count);
#endif
}

static inline int block_in_use (unsigned long block,
				struct super_block * sb,
				unsigned char * map)
{
	return lpfs_test_bit ((block - le32_to_cpu(LPFS_SBI(sb).s_es->s_first_data_block)) %
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

int lpfs_group_sparse(int group)
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
	return LPFS_SBI(sb).s_gdb_count;
}

#ifdef CONFIG_LPFS_CHECK
/* Called at mount-time, super-block is locked */
void lpfs_check_blocks_bitmap (struct super_block * sb)
{
	struct buffer_head * bh;
	struct lpfs_super_block * es;
	unsigned long desc_count, bitmap_count, x, j;
	unsigned long desc_blocks;
	int bitmap_nr;
	struct lpfs_group_desc * gdp;
	int i;

	es = LPFS_SBI(sb).s_es;
	desc_count = 0;
	bitmap_count = 0;
	gdp = NULL;
	for (i = 0; i < LPFS_SBI(sb).s_groups_count; i++) {
		gdp = lpfs_get_group_desc (sb, i, NULL);
		if (!gdp)
			continue;
		desc_count += le16_to_cpu(gdp->bg_free_blocks_count);
		bitmap_nr = load_block_bitmap (sb, i);
		if (bitmap_nr < 0)
			continue;

		bh = LPFS_SB(sb)->s_block_bitmap[bitmap_nr];

		if (lpfs_bg_has_super(sb, i) && !lpfs_test_bit(0, bh->b_data))
			lpfs_error(sb, __FUNCTION__,
				   "Superblock in group %d is marked free", i);

		desc_blocks = lpfs_bg_num_gdb(sb, i);
		for (j = 0; j < desc_blocks; j++)
			if (!lpfs_test_bit(j + 1, bh->b_data))
				lpfs_error(sb, __FUNCTION__,
					   "Descriptor block #%ld in group "
					   "%d is marked free", j, i);

		if (!block_in_use (le32_to_cpu(gdp->bg_block_bitmap), sb, bh->b_data))
			lpfs_error (sb, "lpfs_check_blocks_bitmap",
				    "Block bitmap for group %d is marked free",
				    i);

		if (!block_in_use (le32_to_cpu(gdp->bg_inode_bitmap), sb, bh->b_data))
			lpfs_error (sb, "lpfs_check_blocks_bitmap",
				    "Inode bitmap for group %d is marked free",
				    i);

		for (j = 0; j < LPFS_SBI(sb).s_itb_per_group; j++)
			if (!block_in_use (le32_to_cpu(gdp->bg_inode_table) + j, sb, bh->b_data))
				lpfs_error (sb, "lpfs_check_blocks_bitmap",
					    "Block #%ld of the inode table in "
					    "group %d is marked free", j, i);

		x = lpfs_count_free (bh, sb->s_blocksize);
		if (le16_to_cpu(gdp->bg_free_blocks_count) != x)
			lpfs_error (sb, "lpfs_check_blocks_bitmap",
				    "Wrong free blocks count for group %d, "
				    "stored = %d, counted = %lu", i,
				    le16_to_cpu(gdp->bg_free_blocks_count), x);
		bitmap_count += x;
	}
	if (le32_to_cpu(es->s_free_blocks_count) != bitmap_count)
		lpfs_error (sb, "lpfs_check_blocks_bitmap",
			    "Wrong free blocks count in super block, "
			    "stored = %lu, counted = %lu",
			    (unsigned long) le32_to_cpu(es->s_free_blocks_count), bitmap_count);
}
#endif
/* JC lock and unlock block options */
//balloc.c touches the blocklock directly, file.c should not
int lpfs_lockBlock(struct super_block *sb, int goal){
	struct lpfs_super_block * es;
	int i,j;
	lock_super(sb);
	es = LPFS_SBI(sb).s_es;
	if (goal < le32_to_cpu(es->s_first_data_block) ||
	goal >= le32_to_cpu(es->s_blocks_count)){
		unlock_super(sb);
		return 1;
	}
	i = (goal - le32_to_cpu(es->s_first_data_block)) / LPFS_BLOCKS_PER_GROUP(sb);
	j = ((goal - le32_to_cpu(es->s_first_data_block)) % LPFS_BLOCKS_PER_GROUP(sb));
	i=lpfs_set_bit(j,LPFS_SBI(sb).s_blockLock_bitmaps[i]);
	unlock_super(sb);
	return i;
}
int lpfs_unlockBlock(struct super_block *sb, int goal){
	struct lpfs_super_block * es;
	int i,j;
	lock_super(sb);
	es = LPFS_SBI(sb).s_es;
	if (goal < le32_to_cpu(es->s_first_data_block) ||
	goal >= le32_to_cpu(es->s_blocks_count)){
		unlock_super(sb);
		return 1;
	}
	i = (goal - le32_to_cpu(es->s_first_data_block)) / LPFS_BLOCKS_PER_GROUP(sb);
	j = ((goal - le32_to_cpu(es->s_first_data_block)) % LPFS_BLOCKS_PER_GROUP(sb));
	
	i=lpfs_clear_bit(j,LPFS_SBI(sb).s_blockLock_bitmaps[i]);
	unlock_super(sb);
	return i;
}

int lpfs_isBlockStillMine(struct inode *inode, int block){
	struct buffer_head *dbh;
	unsigned long block_group;
	unsigned long bit;
	int bitmap_nr;
	struct super_block * sb;
	struct lpfs_super_block * es;

	sb = inode->i_sb;
	if (!sb) {
		printk ("lpfs_isBlockStillMine: nonexistent device");
		return -1;
	}
	lock_super (sb);
	es = LPFS_SBI(sb).s_es;
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

	bitmap_nr = load_block_bitmap (sb, block_group);
	if (bitmap_nr < 0)
		goto error_return;
	
	dbh = LPFS_SBI(sb).s_dirty_bitmap[bitmap_nr];
	
	bitmap_nr=lpfs_test_bit(bit,dbh->b_data);
	unlock_super(sb);
	return bitmap_nr;
	
error_return:
	unlock_super(sb);
	return -1;
}
/* /JC */

