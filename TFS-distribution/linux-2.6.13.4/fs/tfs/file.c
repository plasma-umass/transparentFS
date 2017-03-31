/*
 *  linux/fs/tfs/file.c
 *
 * Copyright (C) 1992, 1993, 1994, 1995
 * Remy Card (card@masi.ibp.fr)
 * Laboratoire MASI - Institut Blaise Pascal
 * Universite Pierre et Marie Curie (Paris VI)
 *
 *  from
 *
 *  linux/fs/minix/file.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 *
 *  tfs fs regular file handling primitives
 *
 *  64-bit file support on 64-bit platforms by Jakub Jelinek
 * 	(jj@sunsite.ms.mff.cuni.cz)
 */

#include <linux/time.h>
#include "tfs.h"
#include "xattr.h"
#include "acl.h"


#undef TFS_TIMERS


/*
 * Called when an inode is released. Note that this is different
 * from tfs_open_file: open gets called at every open, but release
 * gets called only when /all/ the files are closed.
 */
 /* JC tfs_release_file_helper */
static int tfs_release_file_helper(struct inode *inode, int fileblocknum, int diskblocknum){
	if( !(tfs_unlockBlock(inode->i_sb, diskblocknum))){
	//	BUG();
	//	return 1;
//////////////////////////////////////////////////////////////////////////////
	}
	return 0;
}
 
static int tfs_release_file (struct inode * inode, struct file * filp)
{
	unsigned int t,tt;
	static unsigned int timeTotal=0;
	static unsigned int timeSamples=0;
	#ifdef TFS_TIMERS
		t = get_cycles();
	#endif


	if (filp->f_mode & FMODE_WRITE)
		tfs_discard_prealloc (inode);
	/* JC unlock blocks when releasing a file */
	if(TFS_I(inode)->i_transparency==TFS_TRANSPARENT && S_ISREG(inode->i_mode)){
		tfs_step_blocks(inode, tfs_release_file_helper);
	}

	#ifdef TFS_TIMERS
		tt=get_cycles();
		//assume it takes less than a second to do an allocation:
		if(tt<t){
			tt=tt+t;
		}else{
			tt=tt-t;
		}
		if(TFS_I(inode)->i_transparency!=TFS_TRANSPARENT && S_ISREG(inode->i_mode)){
			timeTotal+=tt;
			timeSamples ++;
			if(timeSamples+1 %1000 == 0){
				printk("TFS release total= %i, samples = %i\n",timeTotal, timeSamples);
			}
			if(timeSamples+1 %10000 == 0){
				printk("tfs release resetting\n");
				timeTotal= timeSamples = 0;
			}
		}
	#endif
	return 0;
}


/*
 * We have mostly NULL's here: the current defaults are ok for
 * the tfs filesystem.
 */
struct file_operations tfs_file_operations = {
	.llseek		= generic_file_llseek,
	.read		= generic_file_read,
	.write		= generic_file_write,
	.aio_read	= generic_file_aio_read,
	.aio_write	= generic_file_aio_write,
	.ioctl		= tfs_ioctl,
	.mmap		= generic_file_mmap,
	.open		= tfs_file_open,
	.release	= tfs_release_file,
	.fsync		= tfs_sync_file,
	.readv		= generic_file_readv,
	.writev		= generic_file_writev,
	.sendfile	= generic_file_sendfile,
};

#ifdef CONFIG_TFS_FS_XIP
struct file_operations tfs_xip_file_operations = {
	.llseek		= generic_file_llseek,
	.read		= xip_file_read,
	.write		= xip_file_write,
	.ioctl		= tfs_ioctl,
	.mmap		= xip_file_mmap,
	.open		= generic_file_open,
	.release	= tfs_release_file,
	.fsync		= tfs_sync_file,
	.sendfile	= xip_file_sendfile,
};
#endif

struct inode_operations tfs_file_inode_operations = {
	.truncate	= tfs_truncate,
#ifdef CONFIG_TFS_FS_XATTR
	.setxattr	= generic_setxattr,
	.getxattr	= generic_getxattr,
	.listxattr	= tfs_listxattr,
	.removexattr	= generic_removexattr,
#endif
	.setattr	= tfs_setattr,
	.permission	= tfs_permission,
};

int tfs_checkBlock(struct inode *inode, int fileblocknum, int diskblocknum){
	//if lockBlock returns non-zero it means that the block is already locked
	tfs_lockBlock(inode->i_sb, diskblocknum);
	//At this point, if the block is locked, it will remain locked, if it is
	//not locked, it will remain unlocked
	//FIXME: make sure that the allocation code which alters the bitmaps
	//does a test_and_set not just a set
	//	printk("\ttfs_lockBlock checking block %i\n",diskblocknum);
	if(tfs_isBlockStillMine(inode, diskblocknum)){
		tfs_debug("tfs_lockBlock found bad block %i\n",diskblocknum);
		tfs_unlockBlock(inode->i_sb, diskblocknum);
		return 1;
	}
	//printk("\ttfs_lockBlock block %i is still good\n",diskblocknum);
	return 0;
}

int tfs_printBlock(struct inode *inode, int fileblocknum, int diskblocknum){
	printk("%i\n",diskblocknum);
	return 0;
}

int tfs_file_open(struct inode *inode, struct file *filp){
	int r;
	unsigned int t, tt;
	static unsigned int timeTotal=0;
	static unsigned int timeSamples=0;
	#ifdef TFS_TIMERS
		t = get_cycles();
	#endif
	//printk("lpfs_file_open called\n");
	if(TFS_I(inode)->i_transparency==TFS_TRANSPARENT && S_ISREG(inode->i_mode)){
		if( (r=tfs_step_blocks(inode, tfs_checkBlock))){
		//	printk("lpfs_file_open file is no good\n");
			vfs_unlink(filp->f_dentry->d_parent->d_inode, filp->f_dentry);
			return -ENOENT;
		}else{
		//	printk("file opened just fine\n");
		}
	}
//	tfs_step_blocks(inode, tfs_printBlock);
	r=generic_file_open(inode, filp);
	#ifdef TFS_TIMERS
		tt=get_cycles();
		//assume it takes less than a second to do an allocation:
		if(tt<t){
			tt=tt+t;
		}else{
			tt=tt-t;
		}
		if(TFS_I(inode)->i_transparency!=TFS_TRANSPARENT && S_ISREG(inode->i_mode)){
			timeTotal+=tt;
			timeSamples ++;
			if(timeSamples+1 %1000 == 0){
				printk("TFS open total= %i, samples = %i\n",timeTotal, timeSamples);
			}
			if(timeSamples+1 %10000 == 0){
				printk("tfs open resetting\n");
				timeTotal= timeSamples = 0;
			}
		}
	#endif
	//printk("generic_file_open returned %i\n",r);
	return r;
}
