/*
 * linux/fs/tfs/namei.c
 *
 * Rewrite to pagecache. Almost all code had been changed, so blame me
 * if the things go wrong. Please, send bug reports to
 * viro@parcelfarce.linux.theplanet.co.uk
 *
 * Stuff here is basically a glue between the VFS and generic UNIXish
 * filesystem that keeps everything in pagecache. All knowledge of the
 * directory layout is in fs/tfs/dir.c - it turned out to be easily separatable
 * and it's easier to debug that way. In principle we might want to
 * generalize that a bit and turn it into a library. Or not.
 *
 * The only non-static object here is tfs_dir_inode_operations.
 *
 * TODO: get rid of kmap() use, add readahead.
 *
 * Copyright (C) 1992, 1993, 1994, 1995
 * Remy Card (card@masi.ibp.fr)
 * Laboratoire MASI - Institut Blaise Pascal
 * Universite Pierre et Marie Curie (Paris VI)
 *
 *  from
 *
 *  linux/fs/minix/namei.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 *
 *  Big-endian to little-endian byte-swapping/bitmaps by
 *        David S. Miller (davem@caip.rutgers.edu), 1995
 */

#include <linux/pagemap.h>
#include "tfs.h"
#include "xattr.h"
#include "acl.h"
#include "xip.h"

/*
 * Couple of helper functions - make the code slightly cleaner.
 */

static inline void tfs_inc_count(struct inode *inode)
{
	inode->i_nlink++;
	mark_inode_dirty(inode);
}

static inline void tfs_dec_count(struct inode *inode)
{
	inode->i_nlink--;
	mark_inode_dirty(inode);
}

static inline int tfs_add_nondir(struct dentry *dentry, struct inode *inode)
{
	int err = tfs_add_link(dentry, inode);
	if (!err) {
		d_instantiate(dentry, inode);
		return 0;
	}
	tfs_dec_count(inode);
	iput(inode);
	return err;
}

/*
 * Methods themselves.
 */

static struct dentry *tfs_lookup(struct inode * dir, struct dentry *dentry, struct nameidata *nd)
{
	struct inode * inode;
	ino_t ino;
	
	if (dentry->d_name.len > TFS_NAME_LEN)
		return ERR_PTR(-ENAMETOOLONG);

	ino = tfs_inode_by_name(dir, dentry);
	inode = NULL;
	if (ino) {
		inode = iget(dir->i_sb, ino);
		if (!inode)
			return ERR_PTR(-EACCES);
	}
	if (inode)
		return d_splice_alias(inode, dentry);
	d_add(dentry, inode);
	return NULL;
}

struct dentry *tfs_get_parent(struct dentry *child)
{
	unsigned long ino;
	struct dentry *parent;
	struct inode *inode;
	struct dentry dotdot;

	dotdot.d_name.name = "..";
	dotdot.d_name.len = 2;

	ino = tfs_inode_by_name(child->d_inode, &dotdot);
	if (!ino)
		return ERR_PTR(-ENOENT);
	inode = iget(child->d_inode->i_sb, ino);

	if (!inode)
		return ERR_PTR(-EACCES);
	parent = d_alloc_anon(inode);
	if (!parent) {
		iput(inode);
		parent = ERR_PTR(-ENOMEM);
	}
	return parent;
} 

/*
 * By the time this is called, we already have created
 * the directory cache entry for the new file, but it
 * is so far negative - it has no inode.
 *
 * If the create succeeds, we fill in the inode information
 * with d_instantiate(). 
 */
static int tfs_create (struct inode * dir, struct dentry * dentry, int mode, struct nameidata *nd)
{
	struct inode * inode = tfs_new_inode (dir, mode);
	int err = PTR_ERR(inode);
	if (!IS_ERR(inode)) {
		inode->i_op = &tfs_file_inode_operations;
		if (tfs_use_xip(inode->i_sb)) {
			inode->i_mapping->a_ops = &tfs_aops_xip;
			inode->i_fop = &tfs_xip_file_operations;
		} else if (test_opt(inode->i_sb, NOBH)) {
			inode->i_mapping->a_ops = &tfs_nobh_aops;
			inode->i_fop = &tfs_file_operations;
		} else {
			inode->i_mapping->a_ops = &tfs_aops;
			inode->i_fop = &tfs_file_operations;
		}
		mark_inode_dirty(inode);
		err = tfs_add_nondir(dentry, inode);
	}
	return err;
}

static int tfs_mknod (struct inode * dir, struct dentry *dentry, int mode, dev_t rdev)
{
	struct inode * inode;
	int err;

	if (!new_valid_dev(rdev))
		return -EINVAL;

	inode = tfs_new_inode (dir, mode);
	err = PTR_ERR(inode);
	if (!IS_ERR(inode)) {
		init_special_inode(inode, inode->i_mode, rdev);
#ifdef CONFIG_TFS_FS_XATTR
		inode->i_op = &tfs_special_inode_operations;
#endif
		mark_inode_dirty(inode);
		err = tfs_add_nondir(dentry, inode);
	}
	return err;
}

static int tfs_symlink (struct inode * dir, struct dentry * dentry,
	const char * symname)
{
	struct super_block * sb = dir->i_sb;
	int err = -ENAMETOOLONG;
	unsigned l = strlen(symname)+1;
	struct inode * inode;

	if (l > sb->s_blocksize)
		goto out;

	inode = tfs_new_inode (dir, S_IFLNK | S_IRWXUGO);
	err = PTR_ERR(inode);
	if (IS_ERR(inode))
		goto out;

	if (l > sizeof (TFS_I(inode)->i_data)) {
		/* slow symlink */
		inode->i_op = &tfs_symlink_inode_operations;
		if (test_opt(inode->i_sb, NOBH))
			inode->i_mapping->a_ops = &tfs_nobh_aops;
		else
			inode->i_mapping->a_ops = &tfs_aops;
		err = page_symlink(inode, symname, l);
		if (err)
			goto out_fail;
	} else {
		/* fast symlink */
		inode->i_op = &tfs_fast_symlink_inode_operations;
		memcpy((char*)(TFS_I(inode)->i_data),symname,l);
		inode->i_size = l-1;
	}
	mark_inode_dirty(inode);

	err = tfs_add_nondir(dentry, inode);
out:
	return err;

out_fail:
	tfs_dec_count(inode);
	iput (inode);
	goto out;
}

static int tfs_link (struct dentry * old_dentry, struct inode * dir,
	struct dentry *dentry)
{
	struct inode *inode = old_dentry->d_inode;

	if (inode->i_nlink >= TFS_LINK_MAX)
		return -EMLINK;

	inode->i_ctime = CURRENT_TIME_SEC;
	tfs_inc_count(inode);
	atomic_inc(&inode->i_count);

	return tfs_add_nondir(dentry, inode);
}

static int tfs_mkdir(struct inode * dir, struct dentry * dentry, int mode)
{
	struct inode * inode;
	int err = -EMLINK;

	if (dir->i_nlink >= TFS_LINK_MAX)
		goto out;

	tfs_inc_count(dir);

	inode = tfs_new_inode (dir, S_IFDIR | mode);
	err = PTR_ERR(inode);
	if (IS_ERR(inode))
		goto out_dir;

	inode->i_op = &tfs_dir_inode_operations;
	inode->i_fop = &tfs_dir_operations;
	if (test_opt(inode->i_sb, NOBH))
		inode->i_mapping->a_ops = &tfs_nobh_aops;
	else
		inode->i_mapping->a_ops = &tfs_aops;

	tfs_inc_count(inode);

	err = tfs_make_empty(inode, dir);
	if (err)
		goto out_fail;

	err = tfs_add_link(dentry, inode);
	if (err)
		goto out_fail;

	d_instantiate(dentry, inode);
out:
	return err;

out_fail:
	tfs_dec_count(inode);
	tfs_dec_count(inode);
	iput(inode);
out_dir:
	tfs_dec_count(dir);
	goto out;
}

static int tfs_unlink(struct inode * dir, struct dentry *dentry)
{
	struct inode * inode = dentry->d_inode;
	struct tfs_dir_entry_2 * de;
	struct page * page;
	int err = -ENOENT;

	de = tfs_find_entry (dir, dentry, &page);
	if (!de)
		goto out;

	err = tfs_delete_entry (de, page);
	if (err)
		goto out;

	inode->i_ctime = dir->i_ctime;
	tfs_dec_count(inode);
	err = 0;
out:
	return err;
}

static int tfs_rmdir (struct inode * dir, struct dentry *dentry)
{
	struct inode * inode = dentry->d_inode;
	int err = -ENOTEMPTY;

	if (tfs_empty_dir(inode)) {
		err = tfs_unlink(dir, dentry);
		if (!err) {
			inode->i_size = 0;
			tfs_dec_count(inode);
			tfs_dec_count(dir);
		}
	}
	return err;
}

static int tfs_rename (struct inode * old_dir, struct dentry * old_dentry,
	struct inode * new_dir,	struct dentry * new_dentry )
{
	struct inode * old_inode = old_dentry->d_inode;
	struct inode * new_inode = new_dentry->d_inode;
	struct page * dir_page = NULL;
	struct tfs_dir_entry_2 * dir_de = NULL;
	struct page * old_page;
	struct tfs_dir_entry_2 * old_de;
	int err = -ENOENT;

	old_de = tfs_find_entry (old_dir, old_dentry, &old_page);
	if (!old_de)
		goto out;

	if (S_ISDIR(old_inode->i_mode)) {
		err = -EIO;
		dir_de = tfs_dotdot(old_inode, &dir_page);
		if (!dir_de)
			goto out_old;
	}

	if (new_inode) {
		struct page *new_page;
		struct tfs_dir_entry_2 *new_de;

		err = -ENOTEMPTY;
		if (dir_de && !tfs_empty_dir (new_inode))
			goto out_dir;

		err = -ENOENT;
		new_de = tfs_find_entry (new_dir, new_dentry, &new_page);
		if (!new_de)
			goto out_dir;
		tfs_inc_count(old_inode);
		tfs_set_link(new_dir, new_de, new_page, old_inode);
		new_inode->i_ctime = CURRENT_TIME_SEC;
		if (dir_de)
			new_inode->i_nlink--;
		tfs_dec_count(new_inode);
	} else {
		if (dir_de) {
			err = -EMLINK;
			if (new_dir->i_nlink >= TFS_LINK_MAX)
				goto out_dir;
		}
		tfs_inc_count(old_inode);
		err = tfs_add_link(new_dentry, old_inode);
		if (err) {
			tfs_dec_count(old_inode);
			goto out_dir;
		}
		if (dir_de)
			tfs_inc_count(new_dir);
	}

	/*
	 * Like most other Unix systems, set the ctime for inodes on a
 	 * rename.
	 * tfs_dec_count() will mark the inode dirty.
	 */
	old_inode->i_ctime = CURRENT_TIME_SEC;

	tfs_delete_entry (old_de, old_page);
	tfs_dec_count(old_inode);

	if (dir_de) {
		tfs_set_link(old_inode, dir_de, dir_page, new_dir);
		tfs_dec_count(old_dir);
	}
	return 0;


out_dir:
	if (dir_de) {
		kunmap(dir_page);
		page_cache_release(dir_page);
	}
out_old:
	kunmap(old_page);
	page_cache_release(old_page);
out:
	return err;
}

struct inode_operations tfs_dir_inode_operations = {
	.create		= tfs_create,
	.lookup		= tfs_lookup,
	.link		= tfs_link,
	.unlink		= tfs_unlink,
	.symlink	= tfs_symlink,
	.mkdir		= tfs_mkdir,
	.rmdir		= tfs_rmdir,
	.mknod		= tfs_mknod,
	.rename		= tfs_rename,
#ifdef CONFIG_TFS_FS_XATTR
	.setxattr	= generic_setxattr,
	.getxattr	= generic_getxattr,
	.listxattr	= tfs_listxattr,
	.removexattr	= generic_removexattr,
#endif
	.setattr	= tfs_setattr,
	.permission	= tfs_permission,
};

struct inode_operations tfs_special_inode_operations = {
#ifdef CONFIG_TFS_FS_XATTR
	.setxattr	= generic_setxattr,
	.getxattr	= generic_getxattr,
	.listxattr	= tfs_listxattr,
	.removexattr	= generic_removexattr,
#endif
	.setattr	= tfs_setattr,
	.permission	= tfs_permission,
};
