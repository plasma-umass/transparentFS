/*
 *  linux/include/linux/lpfs_fs_i.h
 *
 * Copyright (C) 1992, 1993, 1994, 1995
 * Remy Card (card@masi.ibp.fr)
 * Laboratoire MASI - Institut Blaise Pascal
 * Universite Pierre et Marie Curie (Paris VI)
 *
 *  from
 *
 *  linux/include/linux/minix_fs_i.h
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 */

#ifndef _LINUX_LPFS_FS_I
#define _LINUX_LPFS_FS_I

/*
 * second extended file system inode data in memory
 */
struct lpfs_inode_info {
	__u32	i_data[15];
	__u32	i_flags;
	__u32	i_faddr;
	__u8	i_frag_no;
	__u8	i_frag_size;
	__u16	i_state;
	__u32	i_file_acl;
	__u32	i_dir_acl;
	__u32	i_dtime;
	__u32	i_block_group;
	__u32	i_next_alloc_block;
	__u32	i_next_alloc_goal;
	__u32	i_prealloc_block;
	__u32	i_prealloc_count;
	__u32	i_dir_start_lookup;
	/* JC */
	//FIXME
	//this is a 16 bit field because thats how it
	//was in the original lpfs implementation
	//Looking back, it seems like overkill, but
	//i'm sticking with it for now so I dont have
	//to fix mkfs.  There might be a reason I
	//originally went with 16 bits
	__u16	i_priority;
	/* /JC */
};

/*
 * Inode dynamic state flags
 */
#define LPFS_STATE_NEW			0x00000001 /* inode is newly created */

#define LPFS_IN(in)  (*( (struct lpfs_inode_info *) (in->u.generic_ip)))
#define LPFS_DIN(d)  (*( (struct lpfs_inode_info *) (d->d_inode->u.generic_ip)))
#endif	/* _LINUX_LPFS_FS_I */
