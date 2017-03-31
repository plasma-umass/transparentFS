/*
 * linux/fs/tfs/ioctl.c
 *
 * Copyright (C) 1993, 1994, 1995
 * Remy Card (card@masi.ibp.fr)
 * Laboratoire MASI - Institut Blaise Pascal
 * Universite Pierre et Marie Curie (Paris VI)
 */

#include "tfs.h"
#include <linux/time.h>
#include <linux/sched.h>
#include <asm/current.h>
#include <asm/uaccess.h>


int * tfs_getFileBlocksBuffer;
unsigned int tfs_getFileBlocksCount;
static int tfs_getFileBlocks_helper(struct inode *inode, int fileblocknum, int diskblocknum){
	int r;
	if(tfs_getFileBlocksCount == 0){
		printk("bailing out\n");
		return 1;
	}
	printk("Block %i\n", diskblocknum);
	r = put_user(diskblocknum, (int __user *)tfs_getFileBlocksBuffer);
	tfs_getFileBlocksBuffer++;
	tfs_getFileBlocksCount--;
	return 0;
}

int tfs_ioctl (struct inode * inode, struct file * filp, unsigned int cmd,
		unsigned long arg)
{
	struct tfs_inode_info *ei = TFS_I(inode);
	unsigned int flags;
	int i;

	tfs_debug ("cmd = %u, arg = %lu\n", cmd, arg);

	switch (cmd) {
	case TFS_IOC_GETFLAGS:
		flags = ei->i_flags & TFS_FL_USER_VISIBLE;
		return put_user(flags, (int __user *) arg);
	case TFS_IOC_SETFLAGS: {
		unsigned int oldflags;

		if (IS_RDONLY(inode))
			return -EROFS;

		if ((current->fsuid != inode->i_uid) && !capable(CAP_FOWNER))
			return -EACCES;

		if (get_user(flags, (int __user *) arg))
			return -EFAULT;

		if (!S_ISDIR(inode->i_mode))
			flags &= ~TFS_DIRSYNC_FL;

		oldflags = ei->i_flags;

		/*
		 * The IMMUTABLE and APPEND_ONLY flags can only be changed by
		 * the relevant capability.
		 *
		 * This test looks nicer. Thanks to Pauline Middelink
		 */
		if ((flags ^ oldflags) & (TFS_APPEND_FL | TFS_IMMUTABLE_FL)) {
			if (!capable(CAP_LINUX_IMMUTABLE))
				return -EPERM;
		}

		flags = flags & TFS_FL_USER_MODIFIABLE;
		flags |= oldflags & ~TFS_FL_USER_MODIFIABLE;
		ei->i_flags = flags;

		tfs_set_inode_flags(inode);
		inode->i_ctime = CURRENT_TIME_SEC;
		mark_inode_dirty(inode);
		return 0;
	}
	case TFS_IOC_GETVERSION:
		return put_user(inode->i_generation, (int __user *) arg);
	case TFS_IOC_SETVERSION:
		if ((current->fsuid != inode->i_uid) && !capable(CAP_FOWNER))
			return -EPERM;
		if (IS_RDONLY(inode))
			return -EROFS;
		if (get_user(inode->i_generation, (int __user *) arg))
			return -EFAULT;	
		inode->i_ctime = CURRENT_TIME_SEC;
		mark_inode_dirty(inode);
		return 0;
	/* JC ioctls for setting high and low priority */
	case TFS_IOC_GETTRANSPARENCY:
		i=TFS_I(inode)->i_transparency;
		return put_user(i, (int *) arg);
	case TFS_IOC_SETTRANSPARENCY:
		if (IS_RDONLY(inode))
			return -EROFS;
	
		if ((current->fsuid != inode->i_uid) && !capable(CAP_FOWNER))
			return -EACCES;
	
		if (get_user(i, (int *) arg))
			return -EFAULT;
		if(i !=TFS_OPAQUE && i!=TFS_TRANSPARENT){
			return -EFAULT;
		}
		TFS_I(inode)->i_transparency=(short)i;
		inode->i_ctime = CURRENT_TIME;
		mark_inode_dirty(inode);
		return 0;
	case TFS_IOC_GETALLOCLAYOUT:
		tfs_getFileBlocksBuffer=(int *)arg;
		tfs_getFileBlocksCount = 1048576;
		tfs_step_blocks(inode,tfs_getFileBlocks_helper);
		printk("listed %i blocks\n",1048576 - tfs_getFileBlocksCount);
		return 1048576 - tfs_getFileBlocksCount;
	/* /JC */ 
	default:
		return -ENOTTY;
	}
}
