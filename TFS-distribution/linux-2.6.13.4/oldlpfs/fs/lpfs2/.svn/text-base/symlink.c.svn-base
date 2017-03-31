/*
 *  linux/fs/lpfs/symlink.c
 *
 * Only fast symlinks left here - the rest is done by generic code. AV, 1999
 *
 * Copyright (C) 1992, 1993, 1994, 1995
 * Remy Card (card@masi.ibp.fr)
 * Laboratoire MASI - Institut Blaise Pascal
 * Universite Pierre et Marie Curie (Paris VI)
 *
 *  from
 *
 *  linux/fs/minix/symlink.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 *
 *  lpfs symlink handling code
 */

#include "lpfs.h"
#include "xattr.h"
#include <linux/namei.h>

static int lpfs_follow_link(struct dentry *dentry, struct nameidata *nd)
{
	struct lpfs_inode_info *ei = LPFS_I(dentry->d_inode);
	nd_set_link(nd, (char *)ei->i_data);
	return 0;
}

struct inode_operations lpfs_symlink_inode_operations = {
	.readlink	= generic_readlink,
	.follow_link	= page_follow_link_light,
	.put_link	= page_put_link,
#ifdef CONFIG_LPFS_FS_XATTR
	.setxattr	= generic_setxattr,
	.getxattr	= generic_getxattr,
	.listxattr	= lpfs_listxattr,
	.removexattr	= generic_removexattr,
#endif
};
 
struct inode_operations lpfs_fast_symlink_inode_operations = {
	.readlink	= generic_readlink,
	.follow_link	= lpfs_follow_link,
#ifdef CONFIG_LPFS_FS_XATTR
	.setxattr	= generic_setxattr,
	.getxattr	= generic_getxattr,
	.listxattr	= lpfs_listxattr,
	.removexattr	= generic_removexattr,
#endif
};
