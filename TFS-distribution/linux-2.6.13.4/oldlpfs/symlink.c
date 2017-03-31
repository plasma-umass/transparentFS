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

#include <linux/fs.h>
#include "lpfs_fs.h"

static int lpfs_readlink(struct dentry *dentry, char *buffer, int buflen)
{
	char *s = (char *)LPFS_DIN(dentry).i_data;
	return vfs_readlink(dentry, buffer, buflen, s);
}

static int lpfs_follow_link(struct dentry *dentry, struct nameidata *nd)
{
	char *s = (char *)LPFS_DIN(dentry).i_data;
	return vfs_follow_link(nd, s);
}

struct inode_operations lpfs_fast_symlink_inode_operations = {
	readlink:	lpfs_readlink,
	follow_link:	lpfs_follow_link,
};
