/*
 * linux/fs/lpfs/ioctl.c
 *
 * Copyright (C) 1993, 1994, 1995
 * Remy Card (card@masi.ibp.fr)
 * Laboratoire MASI - Institut Blaise Pascal
 * Universite Pierre et Marie Curie (Paris VI)
 */

#include <linux/fs.h>
#include "lpfs_fs.h"
#include <linux/sched.h>
#include <asm/uaccess.h>


int lpfs_ioctl (struct inode * inode, struct file * filp, unsigned int cmd,
		unsigned long arg)
{
	unsigned int flags;
	int i; //JC variable for getting priority

	lpfs_debug ("cmd = %u, arg = %lu\n", cmd, arg);

	switch (cmd) {
	case LPFS_IOC_GETFLAGS:
		flags = LPFS_IN(inode).i_flags & LPFS_FL_USER_VISIBLE;
		return put_user(flags, (int *) arg);
	case LPFS_IOC_SETFLAGS: {
		unsigned int oldflags;

		if (IS_RDONLY(inode))
			return -EROFS;

		if ((current->fsuid != inode->i_uid) && !capable(CAP_FOWNER))
			return -EACCES;

		if (get_user(flags, (int *) arg))
			return -EFAULT;

		oldflags = LPFS_IN(inode).i_flags;

		/*
		 * The IMMUTABLE and APPEND_ONLY flags can only be changed by
		 * the relevant capability.
		 *
		 * This test looks nicer. Thanks to Pauline Middelink
		 */
		if ((flags ^ oldflags) & (LPFS_APPEND_FL | LPFS_IMMUTABLE_FL)) {
			if (!capable(CAP_LINUX_IMMUTABLE))
				return -EPERM;
		}

		flags = flags & LPFS_FL_USER_MODIFIABLE;
		flags |= oldflags & ~LPFS_FL_USER_MODIFIABLE;
		LPFS_IN(inode).i_flags = flags;

		lpfs_set_inode_flags(inode);
		inode->i_ctime = CURRENT_TIME;
		mark_inode_dirty(inode);
		return 0;
	}
	case LPFS_IOC_GETVERSION:
		return put_user(inode->i_generation, (int *) arg);
	case LPFS_IOC_SETVERSION:
		if ((current->fsuid != inode->i_uid) && !capable(CAP_FOWNER))
			return -EPERM;
		if (IS_RDONLY(inode))
			return -EROFS;
		if (get_user(inode->i_generation, (int *) arg))
			return -EFAULT;	
		inode->i_ctime = CURRENT_TIME;
		mark_inode_dirty(inode);
		return 0;
	/* JC ioctls for setting high and low priority */
	case LPFS_IOC_GETLOWPRIORITY:
		i=(int)LPFS_IN(inode).i_priority;
		return put_user(i, (int *) arg);
	case LPFS_IOC_SETLOWPRIORITY:
		if (IS_RDONLY(inode))
			return -EROFS;

		if ((current->fsuid != inode->i_uid) && !capable(CAP_FOWNER))
			return -EACCES;

		if (get_user(i, (int *) arg))
			return -EFAULT;
		LPFS_IN(inode).i_priority=(short)i;
		inode->i_ctime = CURRENT_TIME;
		mark_inode_dirty(inode);
		return 0;
		/* /JC */
	case LPFS_IOC_GETSTATS:
		do{
			__u64 r[16];
			r[0]=lpfs_hpballoc_time_total;
			r[1]=lpfs_hpballoc_time_count;
			r[2]=lpfs_hpialloc_time_total;
			r[3]=lpfs_hpialloc_time_count;

			r[4]=lpfs_hpread_time_total;
			r[5]=lpfs_hpread_time_count;
			r[6]=lpfs_hpwrite_time_total;
			r[7]=lpfs_hpwrite_time_count;

			r[8]=lpfs_lpballoc_time_total;
			r[9]=lpfs_lpballoc_time_count;
			r[10]=lpfs_lpialloc_time_total;
			r[11]=lpfs_lpialloc_time_count;

			r[12]=lpfs_lpread_time_total;
			r[13]=lpfs_lpread_time_count;
			r[14]=lpfs_lpwrite_time_total;
			r[15]=lpfs_lpwrite_time_count;
			return copy_to_user( (void *)arg, r,
				 16*sizeof(__u64));
		}while(0);
	case LPFS_IOC_RESETSTATS:
		lpfs_hpballoc_time_total=0;
		lpfs_hpballoc_time_count=0;
		lpfs_hpialloc_time_total=0;
		lpfs_hpialloc_time_count=0;
		lpfs_lpballoc_time_total=0;
		lpfs_lpballoc_time_count=0;
		lpfs_lpialloc_time_total=0;
		lpfs_lpialloc_time_count=0;
		lpfs_hpread_time_total=0;
		lpfs_hpread_time_count=0;
		lpfs_hpwrite_time_total=0;
		lpfs_hpwrite_time_count=0;
		lpfs_lpread_time_total=0;
		lpfs_lpread_time_count=0;
		lpfs_lpwrite_time_total=0;
		lpfs_lpwrite_time_count=0;
		
		return 0;
	default:
		return -ENOTTY;
	}
}
