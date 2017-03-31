/*
 * linux/fs/ext2/ioctl.c
 *
 * Copyright (C) 1993, 1994, 1995
 * Remy Card (card@masi.ibp.fr)
 * Laboratoire MASI - Institut Blaise Pascal
 * Universite Pierre et Marie Curie (Paris VI)
 */

#include "ext2.h"
#include <linux/time.h>
#include <linux/sched.h>
#include <asm/current.h>
#include <asm/uaccess.h>



// JC - needed by block layout ioctl:
extern int ext2_step_blocks(struct inode *inode, int (*func)(struct inode *, int, int));
int * ext2_getFileBlocksBuffer;
unsigned int ext2_getFileBlocksCount;
static int ext2_getFileBlocks_helper(struct inode *inode, int fileblocknum, int diskblocknum){
	int r;
	if(ext2_getFileBlocksCount == 0){
		printk("bailing out\n");
		return 1;
	}
	printk("Block %i\n", diskblocknum);
	r = put_user(diskblocknum, (int __user *)ext2_getFileBlocksBuffer);
	ext2_getFileBlocksBuffer++;
	ext2_getFileBlocksCount--;
	return 0;
}


int ext2_ioctl (struct inode * inode, struct file * filp, unsigned int cmd,
		unsigned long arg)
{
	struct ext2_inode_info *ei = EXT2_I(inode);
	unsigned int flags;

	ext2_debug ("cmd = %u, arg = %lu\n", cmd, arg);

	switch (cmd) {
	case EXT2_IOC_GETFLAGS:
		flags = ei->i_flags & EXT2_FL_USER_VISIBLE;
		return put_user(flags, (int __user *) arg);
	case EXT2_IOC_SETFLAGS: {
		unsigned int oldflags;

		if (IS_RDONLY(inode))
			return -EROFS;

		if ((current->fsuid != inode->i_uid) && !capable(CAP_FOWNER))
			return -EACCES;

		if (get_user(flags, (int __user *) arg))
			return -EFAULT;

		if (!S_ISDIR(inode->i_mode))
			flags &= ~EXT2_DIRSYNC_FL;

		oldflags = ei->i_flags;

		/*
		 * The IMMUTABLE and APPEND_ONLY flags can only be changed by
		 * the relevant capability.
		 *
		 * This test looks nicer. Thanks to Pauline Middelink
		 */
		if ((flags ^ oldflags) & (EXT2_APPEND_FL | EXT2_IMMUTABLE_FL)) {
			if (!capable(CAP_LINUX_IMMUTABLE))
				return -EPERM;
		}

		flags = flags & EXT2_FL_USER_MODIFIABLE;
		flags |= oldflags & ~EXT2_FL_USER_MODIFIABLE;
		ei->i_flags = flags;

		ext2_set_inode_flags(inode);
		inode->i_ctime = CURRENT_TIME_SEC;
		mark_inode_dirty(inode);
		return 0;
	}
	case EXT2_IOC_GETVERSION:
		return put_user(inode->i_generation, (int __user *) arg);
	case EXT2_IOC_SETVERSION:
		if ((current->fsuid != inode->i_uid) && !capable(CAP_FOWNER))
			return -EPERM;
		if (IS_RDONLY(inode))
			return -EROFS;
		if (get_user(inode->i_generation, (int __user *) arg))
			return -EFAULT;	
		inode->i_ctime = CURRENT_TIME_SEC;
		mark_inode_dirty(inode);
		return 0;
	// JC: the TFS get page layout ioctl
	case _IOW('q',2,long):
		ext2_getFileBlocksBuffer=(int *)arg;
		ext2_getFileBlocksCount = 1048576;
		ext2_step_blocks(inode,ext2_getFileBlocks_helper);
		printk("listed %i blocks\n",1048576 - ext2_getFileBlocksCount);
		return 1048576 - ext2_getFileBlocksCount;
	default:
		return -ENOTTY;
	}
}
