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

#include <linux/fs.h>
#include "lpfs_fs.h"
#include <linux/sched.h>

/*
 * Called when an inode is released. Note that this is different
 * from lpfs_open_file: open gets called at every open, but release
 * gets called only when /all/ the files are closed.
 */
 /* JC lpfs_release_file_helper */
static int lpfs_release_file_helper(struct inode *inode, int fileblocknum, int diskblocknum){
	//FIXME this has not been tested
	//originally i did not have the ! there, and stepblocks would always die,
	//because unlocking a locked block would cause this to return 1
	//i have not actually tested the code with the ! there
	return !(lpfs_unlockBlock(inode->i_sb, diskblocknum));
}
 
static int lpfs_release_file (struct inode * inode, struct file * filp)
{
	if (filp->f_mode & FMODE_WRITE)
		lpfs_discard_prealloc (inode);
	/* JC unlock blocks when releasing a file */
	lpfs_step_blocks(inode, lpfs_release_file_helper);
	return 0;
}

/*
 * We have mostly NULL's here: the current defaults are ok for
 * the lpfs filesystem.
 */
struct file_operations lpfs_file_operations = {
	llseek:		generic_file_llseek,
#ifdef LPFS_BENCHMARK
	read:		lpfs_file_read,
	write:		lpfs_file_write,
#else
	read:		generic_file_read,
	write: 		generic_file_write,
#endif
	ioctl:		lpfs_ioctl,
	mmap:		generic_file_mmap,
	open:		lpfs_file_open,
	release:	lpfs_release_file,
	fsync:		lpfs_sync_file,
};

ssize_t lpfs_file_read(struct file * filp, char * buf, size_t count, loff_t *ppos){
	int i,j;
	ssize_t r;
	j=get_cycles();
	r=generic_file_read(filp,buf,count,ppos);	
	i=get_cycles();
	if(i % LPFS_SAMPLE_FREQUENCY==0){
		//take care of i get_cycles() wrapping around
		//this happens less than once every 3500 seconds
		//so if it ends up happening more than once, 
		//something is pretty broken
		if(i<j){ j= (4294967295-j)+i; }
		else { j=i-j; }
		if(LPFS_DIN(filp->f_dentry).i_priority==1){
			lpfs_hpread_time_total+=j;
			lpfs_hpread_time_count++;
		}else{
			lpfs_lpread_time_total+=j;
			lpfs_lpread_time_count++;
		}
	}
	return r;
}


ssize_t lpfs_file_write(struct file *filp,const char *buf,size_t count, loff_t *ppos){
	int i,j;
	ssize_t r;
	j=get_cycles();
	r=generic_file_write(filp,buf,count,ppos);	
	i=get_cycles();
	if(i % LPFS_SAMPLE_FREQUENCY==0){
		//take care of i get_cycles() wrapping around
		//this happens less than once every 3500 seconds
		//so if it ends up happening more than once, 
		//something is pretty broken
		if(i<j){ j= (4294967295-j)+i; }
		else { j=i-j; }
		if(LPFS_DIN(filp->f_dentry).i_priority==1){
			lpfs_hpwrite_time_total+=j;
			lpfs_hpwrite_time_count++;
		}else{
			lpfs_lpwrite_time_total+=j;
			lpfs_lpwrite_time_count++;
		}
	}
	return r;
}



struct inode_operations lpfs_file_inode_operations = {
	truncate:	lpfs_truncate,
};


/* JC lpfs_file_open */
//when opening a low priority file, we need to make sure
//all of its blocks still belong to it, and lock them

int lpfs_checkBlock(struct inode *inode, int fileblocknum, int diskblocknum){
	lpfs_lockBlock(inode->i_sb, diskblocknum);
//	printk("\tlpfs_lockBlock checking block %i\n",diskblocknum);
	if(lpfs_isBlockStillMine(inode, diskblocknum)){
	//	printk("lpfs_lockBlock found bad block %i\n",diskblocknum);
		lpfs_unlockBlock(inode->i_sb, diskblocknum);
		return 1;
	}
	//printk("\tlpfs_lockBlock block %i is still good\n",diskblocknum);
	return 0;
}

int lpfs_file_open(struct inode *inode, struct file *filp){
	int r;
	//printk("lpfs_file_open called\n");
	if(LPFS_IN(inode).i_priority==0 && S_ISREG(inode->i_mode)){
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
