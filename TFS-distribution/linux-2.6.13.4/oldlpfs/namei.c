/*
 * linux/fs/lpfs/namei.c
 *
 * Rewrite to pagecache. Almost all code had been changed, so blame me
 * if the things go wrong. Please, send bug reports to viro@math.psu.edu
 *
 * Stuff here is basically a glue between the VFS and generic UNIXish
 * filesystem that keeps everything in pagecache. All knowledge of the
 * directory layout is in fs/lpfs/dir.c - it turned out to be easily separatable
 * and it's easier to debug that way. In principle we might want to
 * generalize that a bit and turn it into a library. Or not.
 *
 * The only non-static object here is lpfs_dir_inode_operations.
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

#include <linux/fs.h>
#include "lpfs_fs.h"
#include <linux/pagemap.h>

/*
 * Couple of helper functions - make the code slightly cleaner.
 */

static inline void lpfs_inc_count(struct inode *inode)
{
	inode->i_nlink++;
	mark_inode_dirty(inode);
}

static inline void lpfs_dec_count(struct inode *inode)
{
	inode->i_nlink--;
	mark_inode_dirty(inode);
}

static inline int lpfs_add_nondir(struct dentry *dentry, struct inode *inode)
{
	int err = lpfs_add_link(dentry, inode);
	if (!err) {
		d_instantiate(dentry, inode);
		return 0;
	}
	lpfs_dec_count(inode);
	iput(inode);
	return err;
}

/*
 * Methods themselves.
 */

static struct dentry *lpfs_lookup(struct inode * dir, struct dentry *dentry, struct nameidata *nd)
{
	struct inode * inode;
	ino_t ino;
	
	if (dentry->d_name.len > LPFS_NAME_LEN)
		return ERR_PTR(-ENAMETOOLONG);

	ino = lpfs_inode_by_name(dir, dentry);
	inode = NULL;
	if (ino) {
		inode = iget(dir->i_sb, ino);
		if (!inode) 
			return ERR_PTR(-EACCES);
	}
	d_add(dentry, inode);
	return NULL;
}

/*
 * By the time this is called, we already have created
 * the directory cache entry for the new file, but it
 * is so far negative - it has no inode.
 *
 * If the create succeeds, we fill in the inode information
 * with d_instantiate(). 
 */
static int lpfs_create (struct inode * dir, struct dentry * dentry, int mode, struct nameidata *nd)
{
	struct inode * inode = lpfs_new_inode (dir, mode);
	int err = PTR_ERR(inode);
	if (!IS_ERR(inode)) {
		inode->i_op = &lpfs_file_inode_operations;
		inode->i_fop = &lpfs_file_operations;
		inode->i_mapping->a_ops = &lpfs_aops;
		mark_inode_dirty(inode);
		err = lpfs_add_nondir(dentry, inode);
	}
	return err;
}

static int lpfs_mknod (struct inode * dir, struct dentry *dentry, int mode, dev_t rdev)
{
	struct inode * inode = lpfs_new_inode (dir, mode);
	int err = PTR_ERR(inode);
	if (!IS_ERR(inode)) {
		init_special_inode(inode, mode, rdev);
		mark_inode_dirty(inode);
		err = lpfs_add_nondir(dentry, inode);
	}
	return err;
}

static int lpfs_symlink (struct inode * dir, struct dentry * dentry,
	const char * symname)
{
	struct super_block * sb = dir->i_sb;
	int err = -ENAMETOOLONG;
	unsigned l = strlen(symname)+1;
	struct inode * inode;

	if (l > sb->s_blocksize)
		goto out;

	inode = lpfs_new_inode (dir, S_IFLNK | S_IRWXUGO);
	err = PTR_ERR(inode);
	if (IS_ERR(inode))
		goto out;

	if (l > sizeof (LPFS_IN(inode).i_data)) {
		/* slow symlink */
		inode->i_op = &page_symlink_inode_operations;
		inode->i_mapping->a_ops = &lpfs_aops;
		err = page_symlink(inode, symname, l);
		if (err)
			goto out_fail;
	} else {
		/* fast symlink */
		inode->i_op = &lpfs_fast_symlink_inode_operations;
		memcpy((char*)&LPFS_IN(inode).i_data,symname,l);
		inode->i_size = l-1;
	}
	mark_inode_dirty(inode);

	err = lpfs_add_nondir(dentry, inode);
out:
	return err;

out_fail:
	lpfs_dec_count(inode);
	iput (inode);
	goto out;
}

static int lpfs_link (struct dentry * old_dentry, struct inode * dir,
	struct dentry *dentry)
{
	struct inode *inode = old_dentry->d_inode;

	if (S_ISDIR(inode->i_mode))
		return -EPERM;

	if (inode->i_nlink >= LPFS_LINK_MAX)
		return -EMLINK;

	inode->i_ctime = CURRENT_TIME;
	lpfs_inc_count(inode);
	atomic_inc(&inode->i_count);

	return lpfs_add_nondir(dentry, inode);
}

static int lpfs_mkdir(struct inode * dir, struct dentry * dentry, int mode)
{
	struct inode * inode;
	int err = -EMLINK;

	if (dir->i_nlink >= LPFS_LINK_MAX)
		goto out;

	lpfs_inc_count(dir);

	inode = lpfs_new_inode (dir, S_IFDIR | mode);
	err = PTR_ERR(inode);
	if (IS_ERR(inode))
		goto out_dir;

	inode->i_op = &lpfs_dir_inode_operations;
	inode->i_fop = &lpfs_dir_operations;
	inode->i_mapping->a_ops = &lpfs_aops;

	lpfs_inc_count(inode);

	err = lpfs_make_empty(inode, dir);
	if (err)
		goto out_fail;

	err = lpfs_add_link(dentry, inode);
	if (err)
		goto out_fail;

	d_instantiate(dentry, inode);
out:
	return err;

out_fail:
	lpfs_dec_count(inode);
	lpfs_dec_count(inode);
	iput(inode);
out_dir:
	lpfs_dec_count(dir);
	goto out;
}

static int lpfs_unlink(struct inode * dir, struct dentry *dentry)
{
	struct inode * inode = dentry->d_inode;
	struct lpfs_dir_entry_2 * de;
	struct page * page;
	int err = -ENOENT;

	de = lpfs_find_entry (dir, dentry, &page);
	if (!de)
		goto out;

	err = lpfs_delete_entry (de, page);
	if (err)
		goto out;

	inode->i_ctime = dir->i_ctime;
	lpfs_dec_count(inode);
	err = 0;
out:
	return err;
}

static int lpfs_rmdir (struct inode * dir, struct dentry *dentry)
{
	struct inode * inode = dentry->d_inode;
	int err = -ENOTEMPTY;

	if (lpfs_empty_dir(inode)) {
		err = lpfs_unlink(dir, dentry);
		if (!err) {
			inode->i_size = 0;
			lpfs_dec_count(inode);
			lpfs_dec_count(dir);
		}
	}
	return err;
}

static int lpfs_rename (struct inode * old_dir, struct dentry * old_dentry,
	struct inode * new_dir,	struct dentry * new_dentry )
{
	struct inode * old_inode = old_dentry->d_inode;
	struct inode * new_inode = new_dentry->d_inode;
	struct page * dir_page = NULL;
	struct lpfs_dir_entry_2 * dir_de = NULL;
	struct page * old_page;
	struct lpfs_dir_entry_2 * old_de;
	int err = -ENOENT;

	old_de = lpfs_find_entry (old_dir, old_dentry, &old_page);
	if (!old_de)
		goto out;

	if (S_ISDIR(old_inode->i_mode)) {
		err = -EIO;
		dir_de = lpfs_dotdot(old_inode, &dir_page);
		if (!dir_de)
			goto out_old;
	}

	if (new_inode) {
		struct page *new_page;
		struct lpfs_dir_entry_2 *new_de;

		err = -ENOTEMPTY;
		if (dir_de && !lpfs_empty_dir (new_inode))
			goto out_dir;

		err = -ENOENT;
		new_de = lpfs_find_entry (new_dir, new_dentry, &new_page);
		if (!new_de)
			goto out_dir;
		lpfs_inc_count(old_inode);
		lpfs_set_link(new_dir, new_de, new_page, old_inode);
		new_inode->i_ctime = CURRENT_TIME;
		if (dir_de)
			new_inode->i_nlink--;
		lpfs_dec_count(new_inode);
	} else {
		if (dir_de) {
			err = -EMLINK;
			if (new_dir->i_nlink >= LPFS_LINK_MAX)
				goto out_dir;
		}
		lpfs_inc_count(old_inode);
		err = lpfs_add_link(new_dentry, old_inode);
		if (err) {
			lpfs_dec_count(old_inode);
			goto out_dir;
		}
		if (dir_de)
			lpfs_inc_count(new_dir);
	}

	lpfs_delete_entry (old_de, old_page);
	lpfs_dec_count(old_inode);

	if (dir_de) {
		lpfs_set_link(old_inode, dir_de, dir_page, new_dir);
		lpfs_dec_count(old_dir);
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

struct inode_operations lpfs_dir_inode_operations = {
	create:		lpfs_create,
	lookup:		lpfs_lookup,
	link:		lpfs_link,
	unlink:		lpfs_unlink,
	symlink:	lpfs_symlink,
	mkdir:		lpfs_mkdir,
	rmdir:		lpfs_rmdir,
	mknod:		lpfs_mknod,
	rename:		lpfs_rename,
};
