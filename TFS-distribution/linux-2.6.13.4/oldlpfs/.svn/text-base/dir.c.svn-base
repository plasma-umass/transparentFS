/*
 *  linux/fs/lpfs/dir.c
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
 *  lpfs directory handling functions
 *
 *  Big-endian to little-endian byte-swapping/bitmaps by
 *        David S. Miller (davem@caip.rutgers.edu), 1995
 *
 * All code that works with directory layout had been switched to pagecache
 * and moved here. AV
 */

#include <linux/fs.h>
#include "lpfs_fs.h"
#include <linux/pagemap.h>
#include <linux/smp_lock.h>

typedef struct lpfs_dir_entry_2 lpfs_dirent;

/*
 * lpfs uses block-sized chunks. Arguably, sector-sized ones would be
 * more robust, but we have what we have
 */
static inline unsigned lpfs_chunk_size(struct inode *inode)
{
	return inode->i_sb->s_blocksize;
}

static inline void lpfs_put_page(struct page *page)
{
	kunmap(page);
	page_cache_release(page);
}

static inline unsigned long dir_pages(struct inode *inode)
{
	return (inode->i_size+PAGE_CACHE_SIZE-1)>>PAGE_CACHE_SHIFT;
}

static int lpfs_commit_chunk(struct page *page, unsigned from, unsigned to)
{
	struct inode *dir = page->mapping->host;
	int err = 0;
	dir->i_version++;
	//dir->i_version = ++event;  // JC - FIXME should i have just commented this out?
	page->mapping->a_ops->commit_write(NULL, page, from, to);
	if (IS_SYNC(dir)) {
	//	int err2;
		err = write_one_page(page,1);
	//JC - FIXME - the 2.6 ext2 code is different here
	//	err2 = waitfor_one_page(page);
	//	if (err == 0)
	//		err = err2;
	}
	return err;
}

static void lpfs_check_page(struct page *page)
{
	struct inode *dir = page->mapping->host;
	struct super_block *sb = dir->i_sb;
	unsigned chunk_size = lpfs_chunk_size(dir);
	char *kaddr = page_address(page);
	u32 max_inumber = le32_to_cpu(LPFS_SBI(sb).s_es->s_inodes_count);
	unsigned offs, rec_len;
	unsigned limit = PAGE_CACHE_SIZE;
	lpfs_dirent *p;
	char *error;

	if ((dir->i_size >> PAGE_CACHE_SHIFT) == page->index) {
		limit = dir->i_size & ~PAGE_CACHE_MASK;
		if (limit & (chunk_size - 1))
			goto Ebadsize;
		for (offs = limit; offs<PAGE_CACHE_SIZE; offs += chunk_size) {
			lpfs_dirent *p = (lpfs_dirent*)(kaddr + offs);
			p->rec_len = cpu_to_le16(chunk_size);
		}
		if (!limit)
			goto out;
	}
	for (offs = 0; offs <= limit - LPFS_DIR_REC_LEN(1); offs += rec_len) {
		p = (lpfs_dirent *)(kaddr + offs);
		rec_len = le16_to_cpu(p->rec_len);

		if (rec_len < LPFS_DIR_REC_LEN(1))
			goto Eshort;
		if (rec_len & 3)
			goto Ealign;
		if (rec_len < LPFS_DIR_REC_LEN(p->name_len))
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
	lpfs_error(sb, "lpfs_check_page",
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
	lpfs_error (sb, "lpfs_check_page", "bad entry in directory #%lu: %s - "
		"offset=%lu, inode=%lu, rec_len=%d, name_len=%d",
		dir->i_ino, error, (page->index<<PAGE_CACHE_SHIFT)+offs,
		(unsigned long) le32_to_cpu(p->inode),
		rec_len, p->name_len);
	goto fail;
Eend:
	p = (lpfs_dirent *)(kaddr + offs);
	lpfs_error (sb, "lpfs_check_page",
		"entry in directory #%lu spans the page boundary"
		"offset=%lu, inode=%lu",
		dir->i_ino, (page->index<<PAGE_CACHE_SHIFT)+offs,
		(unsigned long) le32_to_cpu(p->inode));
fail:
	SetPageChecked(page);
	SetPageError(page);
}

static struct page * lpfs_get_page(struct inode *dir, unsigned long n)
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
			lpfs_check_page(page);
		if (PageError(page))
			goto fail;
	}
	return page;

fail:
	lpfs_put_page(page);
	return ERR_PTR(-EIO);
}

/*
 * NOTE! unlike strncmp, lpfs_match returns 1 for success, 0 for failure.
 *
 * len <= LPFS_NAME_LEN and de != NULL are guaranteed by caller.
 */
static inline int lpfs_match (int len, const char * const name,
					struct lpfs_dir_entry_2 * de)
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
static inline lpfs_dirent *lpfs_next_entry(lpfs_dirent *p)
{
	return (lpfs_dirent *)((char*)p + le16_to_cpu(p->rec_len));
}

static inline unsigned 
lpfs_validate_entry(char *base, unsigned offset, unsigned mask)
{
	lpfs_dirent *de = (lpfs_dirent*)(base + offset);
	lpfs_dirent *p = (lpfs_dirent*)(base + (offset&mask));
	while ((char*)p < (char*)de)
		p = lpfs_next_entry(p);
	return (char *)p - base;
}

static unsigned char lpfs_filetype_table[LPFS_FT_MAX] = {
	[LPFS_FT_UNKNOWN]	DT_UNKNOWN,
	[LPFS_FT_REG_FILE]	DT_REG,
	[LPFS_FT_DIR]		DT_DIR,
	[LPFS_FT_CHRDEV]	DT_CHR,
	[LPFS_FT_BLKDEV]	DT_BLK,
	[LPFS_FT_FIFO]		DT_FIFO,
	[LPFS_FT_SOCK]		DT_SOCK,
	[LPFS_FT_SYMLINK]	DT_LNK,
};

#define S_SHIFT 12
static unsigned char lpfs_type_by_mode[S_IFMT >> S_SHIFT] = {
	[S_IFREG >> S_SHIFT]	LPFS_FT_REG_FILE,
	[S_IFDIR >> S_SHIFT]	LPFS_FT_DIR,
	[S_IFCHR >> S_SHIFT]	LPFS_FT_CHRDEV,
	[S_IFBLK >> S_SHIFT]	LPFS_FT_BLKDEV,
	[S_IFIFO >> S_SHIFT]	LPFS_FT_FIFO,
	[S_IFSOCK >> S_SHIFT]	LPFS_FT_SOCK,
	[S_IFLNK >> S_SHIFT]	LPFS_FT_SYMLINK,
};

static inline void lpfs_set_de_type(lpfs_dirent *de, struct inode *inode)
{
	mode_t mode = inode->i_mode;
	if (LPFS_HAS_INCOMPAT_FEATURE(inode->i_sb, LPFS_FEATURE_INCOMPAT_FILETYPE))
		de->file_type = lpfs_type_by_mode[(mode & S_IFMT)>>S_SHIFT];
	else
		de->file_type = 0;
}

static int
lpfs_readdir (struct file * filp, void * dirent, filldir_t filldir)
{
	loff_t pos = filp->f_pos;
	struct inode *inode = filp->f_dentry->d_inode;
	struct super_block *sb = inode->i_sb;
	unsigned int offset = pos & ~PAGE_CACHE_MASK;
	unsigned long n = pos >> PAGE_CACHE_SHIFT;
	unsigned long npages = dir_pages(inode);
	unsigned chunk_mask = ~(lpfs_chunk_size(inode)-1);
	unsigned char *types = NULL;
	int need_revalidate = (filp->f_version != inode->i_version);

	if (pos > inode->i_size - LPFS_DIR_REC_LEN(1))
		goto done;

	if (LPFS_HAS_INCOMPAT_FEATURE(sb, LPFS_FEATURE_INCOMPAT_FILETYPE))
		types = lpfs_filetype_table;

	for ( ; n < npages; n++, offset = 0) {
		char *kaddr, *limit;
		lpfs_dirent *de;
		struct page *page = lpfs_get_page(inode, n);

		if (IS_ERR(page))
			continue;
		kaddr = page_address(page);
		if (need_revalidate) {
			offset = lpfs_validate_entry(kaddr, offset, chunk_mask);
			need_revalidate = 0;
		}
		de = (lpfs_dirent *)(kaddr+offset);
		limit = kaddr + PAGE_CACHE_SIZE - LPFS_DIR_REC_LEN(1);
		for ( ;(char*)de <= limit; de = lpfs_next_entry(de))
			if (de->inode) {
				int over;
				unsigned char d_type = DT_UNKNOWN;

				if (types && de->file_type < LPFS_FT_MAX)
					d_type = types[de->file_type];

				offset = (char *)de - kaddr;
				over = filldir(dirent, de->name, de->name_len,
						(n<<PAGE_CACHE_SHIFT) | offset,
						le32_to_cpu(de->inode), d_type);
				if (over) {
					lpfs_put_page(page);
					goto done;
				}
			}
		lpfs_put_page(page);
	}

done:
	filp->f_pos = (n << PAGE_CACHE_SHIFT) | offset;
	filp->f_version = inode->i_version;
	//UPDATE_ATIME(inode);
	return 0;
}

/*
 *	lpfs_find_entry()
 *
 * finds an entry in the specified directory with the wanted name. It
 * returns the page in which the entry was found, and the entry itself
 * (as a parameter - res_dir). Page is returned mapped and unlocked.
 * Entry is guaranteed to be valid.
 */
struct lpfs_dir_entry_2 * lpfs_find_entry (struct inode * dir,
			struct dentry *dentry, struct page ** res_page)
{
	const char *name = dentry->d_name.name;
	int namelen = dentry->d_name.len;
	unsigned reclen = LPFS_DIR_REC_LEN(namelen);
	unsigned long start, n;
	unsigned long npages = dir_pages(dir);
	struct page *page = NULL;
	lpfs_dirent * de;

	/* OFFSET_CACHE */
	*res_page = NULL;

	start = LPFS_IN(dir).i_dir_start_lookup;
	if (start >= npages)
		start = 0;
	n = start;
	do {
		char *kaddr;
		page = lpfs_get_page(dir, n);
		if (!IS_ERR(page)) {
			kaddr = page_address(page);
			de = (lpfs_dirent *) kaddr;
			kaddr += PAGE_CACHE_SIZE - reclen;
			while ((char *) de <= kaddr) {
				if (lpfs_match (namelen, name, de))
					goto found;
				de = lpfs_next_entry(de);
			}
			lpfs_put_page(page);
		}
		if (++n >= npages)
			n = 0;
	} while (n != start);
	return NULL;

found:
	*res_page = page;
	LPFS_IN(dir).i_dir_start_lookup = n;
	return de;
}

struct lpfs_dir_entry_2 * lpfs_dotdot (struct inode *dir, struct page **p)
{
	struct page *page = lpfs_get_page(dir, 0);
	lpfs_dirent *de = NULL;

	if (!IS_ERR(page)) {
		de = lpfs_next_entry((lpfs_dirent *) page_address(page));
		*p = page;
	}
	return de;
}

ino_t lpfs_inode_by_name(struct inode * dir, struct dentry *dentry)
{
	ino_t res = 0;
	struct lpfs_dir_entry_2 * de;
	struct page *page;
	
	de = lpfs_find_entry (dir, dentry, &page);
	if (de) {
		res = le32_to_cpu(de->inode);
		kunmap(page);
		page_cache_release(page);
	}
	return res;
}

/* Releases the page */
void lpfs_set_link(struct inode *dir, struct lpfs_dir_entry_2 *de,
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
	lpfs_set_de_type (de, inode);
	err = lpfs_commit_chunk(page, from, to);
	//UnlockPage(page);
	unlock_page(page);
	lpfs_put_page(page);
	dir->i_mtime = dir->i_ctime = CURRENT_TIME;
	LPFS_IN(dir).i_flags &= ~LPFS_BTREE_FL;
	mark_inode_dirty(dir);
}

/*
 *	Parent is locked.
 */
int lpfs_add_link (struct dentry *dentry, struct inode *inode)
{
	struct inode *dir = dentry->d_parent->d_inode;
	const char *name = dentry->d_name.name;
	int namelen = dentry->d_name.len;
	unsigned reclen = LPFS_DIR_REC_LEN(namelen);
	unsigned short rec_len, name_len;
	struct page *page = NULL;
	lpfs_dirent * de;
	unsigned long npages = dir_pages(dir);
	unsigned long n;
	char *kaddr;
	unsigned from, to;
	int err;

	/* We take care of directory expansion in the same loop */
	for (n = 0; n <= npages; n++) {
		page = lpfs_get_page(dir, n);
		err = PTR_ERR(page);
		if (IS_ERR(page))
			goto out;
		kaddr = page_address(page);
		de = (lpfs_dirent *)kaddr;
		kaddr += PAGE_CACHE_SIZE - reclen;
		while ((char *)de <= kaddr) {
			err = -EEXIST;
			if (lpfs_match (namelen, name, de))
				goto out_page;
			name_len = LPFS_DIR_REC_LEN(de->name_len);
			rec_len = le16_to_cpu(de->rec_len);
			if (!de->inode && rec_len >= reclen)
				goto got_it;
			if (rec_len >= name_len + reclen)
				goto got_it;
			de = (lpfs_dirent *) ((char *) de + rec_len);
		}
		lpfs_put_page(page);
	}
	BUG();
	return -EINVAL;

got_it:
	from = (char*)de - (char*)page_address(page);
	to = from + rec_len;
	lock_page(page);
	err = page->mapping->a_ops->prepare_write(NULL, page, from, to);
	if (err)
		goto out_unlock;
	if (de->inode) {
		lpfs_dirent *de1 = (lpfs_dirent *) ((char *) de + name_len);
		de1->rec_len = cpu_to_le16(rec_len - name_len);
		de->rec_len = cpu_to_le16(name_len);
		de = de1;
	}
	de->name_len = namelen;
	memcpy (de->name, name, namelen);
	de->inode = cpu_to_le32(inode->i_ino);
	lpfs_set_de_type (de, inode);
	err = lpfs_commit_chunk(page, from, to);
	dir->i_mtime = dir->i_ctime = CURRENT_TIME;
	LPFS_IN(dir).i_flags &= ~LPFS_BTREE_FL;
	mark_inode_dirty(dir);
	/* OFFSET_CACHE */
out_unlock:
	//UnlockPage(page);
	unlock_page(page);
out_page:
	lpfs_put_page(page);
out:
	return err;
}

/*
 * lpfs_delete_entry deletes a directory entry by merging it with the
 * previous entry. Page is up-to-date. Releases the page.
 */
int lpfs_delete_entry (struct lpfs_dir_entry_2 * dir, struct page * page )
{
	struct address_space *mapping = page->mapping;
	struct inode *inode = mapping->host;
	char *kaddr = page_address(page);
	unsigned from = ((char*)dir - kaddr) & ~(lpfs_chunk_size(inode)-1);
	unsigned to = ((char*)dir - kaddr) + le16_to_cpu(dir->rec_len);
	lpfs_dirent * pde = NULL;
	lpfs_dirent * de = (lpfs_dirent *) (kaddr + from);
	int err;

	while ((char*)de < (char*)dir) {
		pde = de;
		de = lpfs_next_entry(de);
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
	err = lpfs_commit_chunk(page, from, to);
	//UnlockPage(page);
	unlock_page(page);
	lpfs_put_page(page);
	inode->i_ctime = inode->i_mtime = CURRENT_TIME;
	LPFS_IN(inode).i_flags &= ~LPFS_BTREE_FL;
	mark_inode_dirty(inode);
	return err;
}

/*
 * Set the first fragment of directory.
 */
int lpfs_make_empty(struct inode *inode, struct inode *parent)
{
	struct address_space *mapping = inode->i_mapping;
	struct page *page = grab_cache_page(mapping, 0);
	unsigned chunk_size = lpfs_chunk_size(inode);
	struct lpfs_dir_entry_2 * de;
	char *base;
	int err;

	if (!page)
		return -ENOMEM;
	err = mapping->a_ops->prepare_write(NULL, page, 0, chunk_size);
	if (err)
		goto fail;

	base = page_address(page);

	de = (struct lpfs_dir_entry_2 *) base;
	de->name_len = 1;
	de->rec_len = cpu_to_le16(LPFS_DIR_REC_LEN(1));
	memcpy (de->name, ".\0\0", 4);
	de->inode = cpu_to_le32(inode->i_ino);
	lpfs_set_de_type (de, inode);

	de = (struct lpfs_dir_entry_2 *) (base + LPFS_DIR_REC_LEN(1));
	de->name_len = 2;
	de->rec_len = cpu_to_le16(chunk_size - LPFS_DIR_REC_LEN(1));
	de->inode = cpu_to_le32(parent->i_ino);
	memcpy (de->name, "..\0", 4);
	lpfs_set_de_type (de, inode);

	err = lpfs_commit_chunk(page, 0, chunk_size);
fail:
	//UnlockPage(page);
	unlock_page(page);
	page_cache_release(page);
	return err;
}

/*
 * routine to check that the specified directory is empty (for rmdir)
 */
int lpfs_empty_dir (struct inode * inode)
{
	struct page *page = NULL;
	unsigned long i, npages = dir_pages(inode);
	
	for (i = 0; i < npages; i++) {
		char *kaddr;
		lpfs_dirent * de;
		page = lpfs_get_page(inode, i);

		if (IS_ERR(page))
			continue;

		kaddr = page_address(page);
		de = (lpfs_dirent *)kaddr;
		kaddr += PAGE_CACHE_SIZE-LPFS_DIR_REC_LEN(1);

		while ((char *)de <= kaddr) {
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
			de = lpfs_next_entry(de);
		}
		lpfs_put_page(page);
	}
	return 1;

not_empty:
	lpfs_put_page(page);
	return 0;
}

struct file_operations lpfs_dir_operations = {
	read:		generic_read_dir,
	readdir:	lpfs_readdir,
	ioctl:		lpfs_ioctl,
	fsync:		lpfs_sync_file,
};
