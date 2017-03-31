/*
 *  linux/fs/lpfs/file.c
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
 *  lpfs fs regular file handling primitives
 *
 *  64-bit file support on 64-bit platforms by Jakub Jelinek
 * 	(jj@sunsite.ms.mff.cuni.cz)
 */

#include <linux/time.h>
#include "lpfs.h"
#include "xattr.h"
#include "acl.h"

/*
 * Called when an inode is released. Note that this is different
 * from lpfs_open_file: open gets called at every open, but release
 * gets called only when /all/ the files are closed.
 */
 /* JC lpfs_release_file_helper */
static int lpfs_release_file_helper(struct inode *inode, int fileblocknum, int diskblocknum){
	if( !(lpfs_unlockBlock(inode->i_sb, diskblocknum))){
	//	BUG();
	//	return 1;
//////////////////////////////////////////////////////////////////////////////
	}
	return 0;
}
 
static int lpfs_release_file (struct inode * inode, struct file * filp)
{
	if (filp->f_mode & FMODE_WRITE)
		lpfs_discard_prealloc (inode);
	/* JC unlock blocks when releasing a file */
	if(LPFS_I(inode)->i_priority==0 && S_ISREG(inode->i_mode)){
		lpfs_step_blocks(inode, lpfs_release_file_helper);
	}
	return 0;
}

/*
 * We have mostly NULL's here: the current defaults are ok for
 * the lpfs filesystem.
 */
struct file_operations lpfs_file_operations = {
	.llseek		= generic_file_llseek,
	.read		= generic_file_read,
	.write		= generic_file_write,
	.aio_read	= generic_file_aio_read,
	.aio_write	= generic_file_aio_write,
	.ioctl		= lpfs_ioctl,
	.mmap		= generic_file_mmap,
	.open		= lpfs_file_open,
	.release	= lpfs_release_file,
	.fsync		= lpfs_sync_file,
	.readv		= generic_file_readv,
	.writev		= generic_file_writev,
	.sendfile	= generic_file_sendfile,
};

struct inode_operations lpfs_file_inode_operations = {
	.truncate	= lpfs_truncate,
#ifdef CONFIG_LPFS_FS_XATTR
	.setxattr	= generic_setxattr,
	.getxattr	= generic_getxattr,
	.listxattr	= lpfs_listxattr,
	.removexattr	= generic_removexattr,
#endif
	.setattr	= lpfs_setattr,
	.permission	= lpfs_permission,
};

/* JC lpfs_file_open */
//when opening a low priority file, we need to make sure
//all of its blocks still belong to it, and lock them

int lpfs_checkBlock(struct inode *inode, int fileblocknum, int diskblocknum){
	//if lockBlock returns non-zero it means that the block is already locked
	lpfs_lockBlock(inode->i_sb, diskblocknum);
	//	printk("\tlpfs_lockBlock checking block %i\n",diskblocknum);
	if(lpfs_isBlockStillMine(inode, diskblocknum)){
		printk("lpfs_lockBlock found bad block %i\n",diskblocknum);
		lpfs_unlockBlock(inode->i_sb, diskblocknum);
		return 1;
	}
	//printk("\tlpfs_lockBlock block %i is still good\n",diskblocknum);
	return 0;
}

int lpfs_file_open(struct inode *inode, struct file *filp){
	int r;
	//printk("lpfs_file_open called\n");
	if(LPFS_I(inode)->i_priority==0 && S_ISREG(inode->i_mode)){
		if( (r=lpfs_step_blocks(inode, lpfs_checkBlock))){
		//	printk("lpfs_file_open file is no good\n");
			vfs_unlink(filp->f_dentry->d_parent->d_inode, filp->f_dentry);
			return -ENOENT;
		}else{
		//	printk("file opened just fine\n");
		}
	}
	r=generic_file_open(inode, filp);
	//printk("generic_file_open returned %i\n",r);
	return r;
}
