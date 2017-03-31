/*
 *  linux/fs/tfs/dir.c
 *
 * Copyright (C) 1992, 1993, 1994, 1995
 * Remy Card (card@masi.ibp.fr)
 * Laboratoire MASI - Institut Blaise Pascal
 * Universite Pierre et Marie Curie (Paris VI)
 *
 *  from
 *
 *  linux/fs/minix/dir.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 *
 *  tfs directory handling functions
 *
 *  Big-endian to little-endian byte-swapping/bitmaps by
 *        David S. Miller (davem@caip.rutgers.edu), 1995
 *
 * All code that works with directory layout had been switched to pagecache
 * and moved here. AV
 */

#include "tfs.h"
#include <linux/pagemap.h>
#include <linux/smp_lock.h>

typedef struct tfs_dir_entry_2 tfs_dirent;

/*
 * tfs uses block-sized chunks. Arguably, sector-sized ones would be
 * more robust, but we have what we have
 */
static inline unsigned tfs_chunk_size(struct inode *inode)
{
	return inode->i_sb->s_blocksize;
}

static inline void tfs_put_page(struct page *page)
{
	kunmap(page);
	page_cache_release(page);
}

static inline unsigned long dir_pages(struct inode *inode)
{
	return (inode->i_size+PAGE_CACHE_SIZE-1)>>PAGE_CACHE_SHIFT;
}

/*
 * Return the offset into page `page_nr' of the last valid
 * byte in that page, plus one.
 */
static unsigned
tfs_last_byte(struct inode *inode, unsigned long page_nr)
{
	unsigned last_byte = inode->i_size;

	last_byte -= page_nr << PAGE_CACHE_SHIFT;
	if (last_byte > PAGE_CACHE_SIZE)
		last_byte = PAGE_CACHE_SIZE;
	return last_byte;
}

static int tfs_commit_chunk(struct page *page, unsigned from, unsigned to)
{
	struct inode *dir = page->mapping->host;
	int err = 0;
	dir->i_version++;
	page->mapping->a_ops->commit_write(NULL, page, from, to);
	if (IS_DIRSYNC(dir))
		err = write_one_page(page, 1);
	else
		unlock_page(page);
	return err;
}

static void tfs_check_page(struct page *page)
{
	struct inode *dir = page->mapping->host;
	struct super_block *sb = dir->i_sb;
	unsigned chunk_size = tfs_chunk_size(dir);
	char *kaddr = page_address(page);
	u32 max_inumber = le32_to_cpu(TFS_SB(sb)->s_es->s_inodes_count);
	unsigned offs, rec_len;
	unsigned limit = PAGE_CACHE_SIZE;
	tfs_dirent *p;
	char *error;

	if ((dir->i_size >> PAGE_CACHE_SHIFT) == page->index) {
		limit = dir->i_size & ~PAGE_CACHE_MASK;
		if (limit & (chunk_size - 1))
			goto Ebadsize;
		if (!limit)
			goto out;
	}
	for (offs = 0; offs <= limit - TFS_DIR_REC_LEN(1); offs += rec_len) {
		p = (tfs_dirent *)(kaddr + offs);
		rec_len = le16_to_cpu(p->rec_len);

		if (rec_len < TFS_DIR_REC_LEN(1))
			goto Eshort;
		if (rec_len & 3)
			goto Ealign;
		if (rec_len < TFS_DIR_REC_LEN(p->name_len))
			goto Enamelen;
		if (((offs + rec_len - 1) ^ offs) & ~(chunk_size-1))
			goto Espan;
		if (le32_to_cpu(p->inode) > max_inumber)
			goto Einumber;
	}
	if (offs != limit)
		goto Eend;
out:
	SetPageChecked(page);
	return;

	/* Too bad, we had an error */

Ebadsize:
	tfs_error(sb, "tfs_check_page",
		"size of directory #%lu is not a multiple of chunk size",
		dir->i_ino
	);
	goto fail;
Eshort:
	error = "rec_len is smaller than minimal";
	goto bad_entry;
Ealign:
	error = "unaligned directory entry";
	goto bad_entry;
Enamelen:
	error = "rec_len is too small for name_len";
	goto bad_entry;
Espan:
	error = "directory entry across blocks";
	goto bad_entry;
Einumber:
	error = "inode out of bounds";
bad_entry:
	tfs_error (sb, "tfs_check_page", "bad entry in directory #%lu: %s - "
		"offset=%lu, inode=%lu, rec_len=%d, name_len=%d",
		dir->i_ino, error, (page->index<<PAGE_CACHE_SHIFT)+offs,
		(unsigned long) le32_to_cpu(p->inode),
		rec_len, p->name_len);
	goto fail;
Eend:
	p = (tfs_dirent *)(kaddr + offs);
	tfs_error (sb, "tfs_check_page",
		"entry in directory #%lu spans the page boundary"
		"offset=%lu, inode=%lu",
		dir->i_ino, (page->index<<PAGE_CACHE_SHIFT)+offs,
		(unsigned long) le32_to_cpu(p->inode));
fail:
	SetPageChecked(page);
	SetPageError(page);
}

static struct page * tfs_get_page(struct inode *dir, unsigned long n)
{
	struct address_space *mapping = dir->i_mapping;
	struct page *page = read_cache_page(mapping, n,
				(filler_t*)mapping->a_ops->readpage, NULL);
	if (!IS_ERR(page)) {
		wait_on_page_locked(page);
		kmap(page);
		if (!PageUptodate(page))
			goto fail;
		if (!PageChecked(page))
			tfs_check_page(page);
		if (PageError(page))
			goto fail;
	}
	return page;

fail:
	tfs_put_page(page);
	return ERR_PTR(-EIO);
}

/*
 * NOTE! unlike strncmp, tfs_match returns 1 for success, 0 for failure.
 *
 * len <= TFS_NAME_LEN and de != NULL are guaranteed by caller.
 */
static inline int tfs_match (int len, const char * const name,
					struct tfs_dir_entry_2 * de)
{
	if (len != de->name_len)
		return 0;
	if (!de->inode)
		return 0;
	return !memcmp(name, de->name, len);
}

/*
 * p is at least 6 bytes before the end of page
 */
static inline tfs_dirent *tfs_next_entry(tfs_dirent *p)
{
	return (tfs_dirent *)((char*)p + le16_to_cpu(p->rec_len));
}

static inline unsigned 
tfs_validate_entry(char *base, unsigned offset, unsigned mask)
{
	tfs_dirent *de = (tfs_dirent*)(base + offset);
	tfs_dirent *p = (tfs_dirent*)(base + (offset&mask));
	while ((char*)p < (char*)de) {
		if (p->rec_len == 0)
			break;
		p = tfs_next_entry(p);
	}
	return (char *)p - base;
}

static unsigned char tfs_filetype_table[TFS_FT_MAX] = {
	[TFS_FT_UNKNOWN]	= DT_UNKNOWN,
	[TFS_FT_REG_FILE]	= DT_REG,
	[TFS_FT_DIR]		= DT_DIR,
	[TFS_FT_CHRDEV]	= DT_CHR,
	[TFS_FT_BLKDEV]	= DT_BLK,
	[TFS_FT_FIFO]		= DT_FIFO,
	[TFS_FT_SOCK]		= DT_SOCK,
	[TFS_FT_SYMLINK]	= DT_LNK,
};

#define S_SHIFT 12
static unsigned char tfs_type_by_mode[S_IFMT >> S_SHIFT] = {
	[S_IFREG >> S_SHIFT]	= TFS_FT_REG_FILE,
	[S_IFDIR >> S_SHIFT]	= TFS_FT_DIR,
	[S_IFCHR >> S_SHIFT]	= TFS_FT_CHRDEV,
	[S_IFBLK >> S_SHIFT]	= TFS_FT_BLKDEV,
	[S_IFIFO >> S_SHIFT]	= TFS_FT_FIFO,
	[S_IFSOCK >> S_SHIFT]	= TFS_FT_SOCK,
	[S_IFLNK >> S_SHIFT]	= TFS_FT_SYMLINK,
};

static inline void tfs_set_de_type(tfs_dirent *de, struct inode *inode)
{
	mode_t mode = inode->i_mode;
	if (TFS_HAS_INCOMPAT_FEATURE(inode->i_sb, TFS_FEATURE_INCOMPAT_FILETYPE))
		de->file_type = tfs_type_by_mode[(mode & S_IFMT)>>S_SHIFT];
	else
		de->file_type = 0;
}

static int
tfs_readdir (struct file * filp, void * dirent, filldir_t filldir)
{
	loff_t pos = filp->f_pos;
	struct inode *inode = filp->f_dentry->d_inode;
	struct super_block *sb = inode->i_sb;
	unsigned int offset = pos & ~PAGE_CACHE_MASK;
	unsigned long n = pos >> PAGE_CACHE_SHIFT;
	unsigned long npages = dir_pages(inode);
	unsigned chunk_mask = ~(tfs_chunk_size(inode)-1);
	unsigned char *types = NULL;
	int need_revalidate = (filp->f_version != inode->i_version);
	int ret;

	if (pos > inode->i_size - TFS_DIR_REC_LEN(1))
		goto success;

	if (TFS_HAS_INCOMPAT_FEATURE(sb, TFS_FEATURE_INCOMPAT_FILETYPE))
		types = tfs_filetype_table;

	for ( ; n < npages; n++, offset = 0) {
		char *kaddr, *limit;
		tfs_dirent *de;
		struct page *page = tfs_get_page(inode, n);

		if (IS_ERR(page)) {
			tfs_error(sb, __FUNCTION__,
				   "bad page in #%lu",
				   inode->i_ino);
			filp->f_pos += PAGE_CACHE_SIZE - offset;
			ret = -EIO;
			goto done;
		}
		kaddr = page_address(page);
		if (need_revalidate) {
			offset = tfs_validate_entry(kaddr, offset, chunk_mask);
			need_revalidate = 0;
		}
		de = (tfs_dirent *)(kaddr+offset);
		limit = kaddr + tfs_last_byte(inode, n) - TFS_DIR_REC_LEN(1);
		for ( ;(char*)de <= limit; de = tfs_next_entry(de)) {
			if (de->rec_len == 0) {
				tfs_error(sb, __FUNCTION__,
					"zero-length directory entry");
				ret = -EIO;
				tfs_put_page(page);
				goto done;
			}
			if (de->inode) {
				int over;
				unsigned char d_type = DT_UNKNOWN;

				if (types && de->file_type < TFS_FT_MAX)
					d_type = types[de->file_type];

				offset = (char *)de - kaddr;
				over = filldir(dirent, de->name, de->name_len,
						(n<<PAGE_CACHE_SHIFT) | offset,
						le32_to_cpu(de->inode), d_type);
				if (over) {
					tfs_put_page(page);
					goto success;
				}
			}
			filp->f_pos += le16_to_cpu(de->rec_len);
		}
		tfs_put_page(page);
	}

success:
	ret = 0;
done:
	filp->f_version = inode->i_version;
	return ret;
}

/*
 *	tfs_find_entry()
 *
 * finds an entry in the specified directory with the wanted name. It
 * returns the page in which the entry was found, and the entry itself
 * (as a parameter - res_dir). Page is returned mapped and unlocked.
 * Entry is guaranteed to be valid.
 */
struct tfs_dir_entry_2 * tfs_find_entry (struct inode * dir,
			struct dentry *dentry, struct page ** res_page)
{
	const char *name = dentry->d_name.name;
	int namelen = dentry->d_name.len;
	unsigned reclen = TFS_DIR_REC_LEN(namelen);
	unsigned long start, n;
	unsigned long npages = dir_pages(dir);
	struct page *page = NULL;
	struct tfs_inode_info *ei = TFS_I(dir);
	tfs_dirent * de;

	if (npages == 0)
		goto out;

	/* OFFSET_CACHE */
	*res_page = NULL;

	start = ei->i_dir_start_lookup;
	if (start >= npages)
		start = 0;
	n = start;
	do {
		char *kaddr;
		page = tfs_get_page(dir, n);
		if (!IS_ERR(page)) {
			kaddr = page_address(page);
			de = (tfs_dirent *) kaddr;
			kaddr += tfs_last_byte(dir, n) - reclen;
			while ((char *) de <= kaddr) {
				if (de->rec_len == 0) {
					tfs_error(dir->i_sb, __FUNCTION__,
						"zero-length directory entry");
					tfs_put_page(page);
					goto out;
				}
				if (tfs_match (namelen, name, de))
					goto found;
				de = tfs_next_entry(de);
			}
			tfs_put_page(page);
		}
		if (++n >= npages)
			n = 0;
	} while (n != start);
out:
	return NULL;

found:
	*res_page = page;
	ei->i_dir_start_lookup = n;
	return de;
}

struct tfs_dir_entry_2 * tfs_dotdot (struct inode *dir, struct page **p)
{
	struct page *page = tfs_get_page(dir, 0);
	tfs_dirent *de = NULL;

	if (!IS_ERR(page)) {
		de = tfs_next_entry((tfs_dirent *) page_address(page));
		*p = page;
	}
	return de;
}

ino_t tfs_inode_by_name(struct inode * dir, struct dentry *dentry)
{
	ino_t res = 0;
	struct tfs_dir_entry_2 * de;
	struct page *page;
	
	de = tfs_find_entry (dir, dentry, &page);
	if (de) {
		res = le32_to_cpu(de->inode);
		kunmap(page);
		page_cache_release(page);
	}
	return res;
}

/* Releases the page */
void tfs_set_link(struct inode *dir, struct tfs_dir_entry_2 *de,
			struct page *page, struct inode *inode)
{
	unsigned from = (char *) de - (char *) page_address(page);
	unsigned to = from + le16_to_cpu(de->rec_len);
	int err;

	lock_page(page);
	err = page->mapping->a_ops->prepare_write(NULL, page, from, to);
	if (err)
		BUG();
	de->inode = cpu_to_le32(inode->i_ino);
	tfs_set_de_type (de, inode);
	err = tfs_commit_chunk(page, from, to);
	tfs_put_page(page);
	dir->i_mtime = dir->i_ctime = CURRENT_TIME_SEC;
	TFS_I(dir)->i_flags &= ~TFS_BTREE_FL;
	mark_inode_dirty(dir);
}

/*
 *	Parent is locked.
 */
int tfs_add_link (struct dentry *dentry, struct inode *inode)
{
	struct inode *dir = dentry->d_parent->d_inode;
	const char *name = dentry->d_name.name;
	int namelen = dentry->d_name.len;
	unsigned chunk_size = tfs_chunk_size(dir);
	unsigned reclen = TFS_DIR_REC_LEN(namelen);
	unsigned short rec_len, name_len;
	struct page *page = NULL;
	tfs_dirent * de;
	unsigned long npages = dir_pages(dir);
	unsigned long n;
	char *kaddr;
	unsigned from, to;
	int err;

	/*
	 * We take care of directory expansion in the same loop.
	 * This code plays outside i_size, so it locks the page
	 * to protect that region.
	 */
	for (n = 0; n <= npages; n++) {
		char *dir_end;

		page = tfs_get_page(dir, n);
		err = PTR_ERR(page);
		if (IS_ERR(page))
			goto out;
		lock_page(page);
		kaddr = page_address(page);
		dir_end = kaddr + tfs_last_byte(dir, n);
		de = (tfs_dirent *)kaddr;
		kaddr += PAGE_CACHE_SIZE - reclen;
		while ((char *)de <= kaddr) {
			if ((char *)de == dir_end) {
				/* We hit i_size */
				name_len = 0;
				rec_len = chunk_size;
				de->rec_len = cpu_to_le16(chunk_size);
				de->inode = 0;
				goto got_it;
			}
			if (de->rec_len == 0) {
				tfs_error(dir->i_sb, __FUNCTION__,
					"zero-length directory entry");
				err = -EIO;
				goto out_unlock;
			}
			err = -EEXIST;
			if (tfs_match (namelen, name, de))
				goto out_unlock;
			name_len = TFS_DIR_REC_LEN(de->name_len);
			rec_len = le16_to_cpu(de->rec_len);
			if (!de->inode && rec_len >= reclen)
				goto got_it;
			if (rec_len >= name_len + reclen)
				goto got_it;
			de = (tfs_dirent *) ((char *) de + rec_len);
		}
		unlock_page(page);
		tfs_put_page(page);
	}
	BUG();
	return -EINVAL;

got_it:
	from = (char*)de - (char*)page_address(page);
	to = from + rec_len;
	err = page->mapping->a_ops->prepare_write(NULL, page, from, to);
	if (err)
		goto out_unlock;
	if (de->inode) {
		tfs_dirent *de1 = (tfs_dirent *) ((char *) de + name_len);
		de1->rec_len = cpu_to_le16(rec_len - name_len);
		de->rec_len = cpu_to_le16(name_len);
		de = de1;
	}
	de->name_len = namelen;
	memcpy (de->name, name, namelen);
	de->inode = cpu_to_le32(inode->i_ino);
	tfs_set_de_type (de, inode);
	err = tfs_commit_chunk(page, from, to);
	dir->i_mtime = dir->i_ctime = CURRENT_TIME_SEC;
	TFS_I(dir)->i_flags &= ~TFS_BTREE_FL;
	mark_inode_dirty(dir);
	/* OFFSET_CACHE */
out_put:
	tfs_put_page(page);
out:
	return err;
out_unlock:
	unlock_page(page);
	goto out_put;
}

/*
 * tfs_delete_entry deletes a directory entry by merging it with the
 * previous entry. Page is up-to-date. Releases the page.
 */
int tfs_delete_entry (struct tfs_dir_entry_2 * dir, struct page * page )
{
	struct address_space *mapping = page->mapping;
	struct inode *inode = mapping->host;
	char *kaddr = page_address(page);
	unsigned from = ((char*)dir - kaddr) & ~(tfs_chunk_size(inode)-1);
	unsigned to = ((char*)dir - kaddr) + le16_to_cpu(dir->rec_len);
	tfs_dirent * pde = NULL;
	tfs_dirent * de = (tfs_dirent *) (kaddr + from);
	int err;

	while ((char*)de < (char*)dir) {
		if (de->rec_len == 0) {
			tfs_error(inode->i_sb, __FUNCTION__,
				"zero-length directory entry");
			err = -EIO;
			goto out;
		}
		pde = de;
		de = tfs_next_entry(de);
	}
	if (pde)
		from = (char*)pde - (char*)page_address(page);
	lock_page(page);
	err = mapping->a_ops->prepare_write(NULL, page, from, to);
	if (err)
		BUG();
	if (pde)
		pde->rec_len = cpu_to_le16(to-from);
	dir->inode = 0;
	err = tfs_commit_chunk(page, from, to);
	inode->i_ctime = inode->i_mtime = CURRENT_TIME_SEC;
	TFS_I(inode)->i_flags &= ~TFS_BTREE_FL;
	mark_inode_dirty(inode);
out:
	tfs_put_page(page);
	return err;
}

/*
 * Set the first fragment of directory.
 */
int tfs_make_empty(struct inode *inode, struct inode *parent)
{
	struct address_space *mapping = inode->i_mapping;
	struct page *page = grab_cache_page(mapping, 0);
	unsigned chunk_size = tfs_chunk_size(inode);
	struct tfs_dir_entry_2 * de;
	int err;
	void *kaddr;

	if (!page)
		return -ENOMEM;
	err = mapping->a_ops->prepare_write(NULL, page, 0, chunk_size);
	if (err) {
		unlock_page(page);
		goto fail;
	}
	kaddr = kmap_atomic(page, KM_USER0);
       memset(kaddr, 0, chunk_size);
	de = (struct tfs_dir_entry_2 *)kaddr;
	de->name_len = 1;
	de->rec_len = cpu_to_le16(TFS_DIR_REC_LEN(1));
	memcpy (de->name, ".\0\0", 4);
	de->inode = cpu_to_le32(inode->i_ino);
	tfs_set_de_type (de, inode);

	de = (struct tfs_dir_entry_2 *)(kaddr + TFS_DIR_REC_LEN(1));
	de->name_len = 2;
	de->rec_len = cpu_to_le16(chunk_size - TFS_DIR_REC_LEN(1));
	de->inode = cpu_to_le32(parent->i_ino);
	memcpy (de->name, "..\0", 4);
	tfs_set_de_type (de, inode);
	kunmap_atomic(kaddr, KM_USER0);
	err = tfs_commit_chunk(page, 0, chunk_size);
fail:
	page_cache_release(page);
	return err;
}

/*
 * routine to check that the specified directory is empty (for rmdir)
 */
int tfs_empty_dir (struct inode * inode)
{
	struct page *page = NULL;
	unsigned long i, npages = dir_pages(inode);

	for (i = 0; i < npages; i++) {
		char *kaddr;
		tfs_dirent * de;
		page = tfs_get_page(inode, i);

		if (IS_ERR(page))
			continue;

		kaddr = page_address(page);
		de = (tfs_dirent *)kaddr;
		kaddr += tfs_last_byte(inode, i) - TFS_DIR_REC_LEN(1);

		while ((char *)de <= kaddr) {
			if (de->rec_len == 0) {
				tfs_error(inode->i_sb, __FUNCTION__,
					"zero-length directory entry");
				printk("kaddr=%p, de=%p\n", kaddr, de);
				goto not_empty;
			}
			if (de->inode != 0) {
				/* check for . and .. */
				if (de->name[0] != '.')
					goto not_empty;
				if (de->name_len > 2)
					goto not_empty;
				if (de->name_len < 2) {
					if (de->inode !=
					    cpu_to_le32(inode->i_ino))
						goto not_empty;
				} else if (de->name[1] != '.')
					goto not_empty;
			}
			de = tfs_next_entry(de);
		}
		tfs_put_page(page);
	}
	return 1;

not_empty:
	tfs_put_page(page);
	return 0;
}

struct file_operations tfs_dir_operations = {
	.llseek		= generic_file_llseek,
	.read		= generic_read_dir,
	.readdir	= tfs_readdir,
	.ioctl		= tfs_ioctl,
	.fsync		= tfs_sync_file,
};
