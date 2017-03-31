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


int tfs_ioctl (struct inode * inode, struct file * filp, unsigned int cmd,
		unsigned long arg)
{
	struct tfs_inode_info *ei = TFS_I(inode);
	unsigned int flags;

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
	default:
		return -ENOTTY;
	}
}
