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

/*
 * Called when an inode is released. Note that this is different
 * from tfs_open_file: open gets called at every open, but release
 * gets called only when /all/ the files are closed.
 */
static int tfs_release_file (struct inode * inode, struct file * filp)
{
	if (filp->f_mode & FMODE_WRITE)
		tfs_discard_prealloc (inode);
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
	.open		= generic_file_open,
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
