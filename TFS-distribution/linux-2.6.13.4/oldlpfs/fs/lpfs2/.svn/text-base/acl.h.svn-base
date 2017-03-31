/*
  File: fs/lpfs/acl.h

  (C) 2001 Andreas Gruenbacher, <a.gruenbacher@computer.org>
*/

//#include <linux/xattr_acl.h>

#define LPFS_ACL_VERSION	0x0001
#define LPFS_ACL_MAX_ENTRIES	32

typedef struct {
	__le16		e_tag;
	__le16		e_perm;
	__le32		e_id;
} lpfs_acl_entry;

typedef struct {
	__le16		e_tag;
	__le16		e_perm;
} lpfs_acl_entry_short;

typedef struct {
	__le32		a_version;
} lpfs_acl_header;

static inline size_t lpfs_acl_size(int count)
{
	if (count <= 4) {
		return sizeof(lpfs_acl_header) +
		       count * sizeof(lpfs_acl_entry_short);
	} else {
		return sizeof(lpfs_acl_header) +
		       4 * sizeof(lpfs_acl_entry_short) +
		       (count - 4) * sizeof(lpfs_acl_entry);
	}
}

static inline int lpfs_acl_count(size_t size)
{
	ssize_t s;
	size -= sizeof(lpfs_acl_header);
	s = size - 4 * sizeof(lpfs_acl_entry_short);
	if (s < 0) {
		if (size % sizeof(lpfs_acl_entry_short))
			return -1;
		return size / sizeof(lpfs_acl_entry_short);
	} else {
		if (s % sizeof(lpfs_acl_entry))
			return -1;
		return s / sizeof(lpfs_acl_entry) + 4;
	}
}

#ifdef CONFIG_LPFS_FS_POSIX_ACL

/* Value for inode->u.lpfs_i.i_acl and inode->u.lpfs_i.i_default_acl
   if the ACL has not been cached */
#define LPFS_ACL_NOT_CACHED ((void *)-1)

/* acl.c */
extern int lpfs_permission (struct inode *, int, struct nameidata *);
extern int lpfs_acl_chmod (struct inode *);
extern int lpfs_init_acl (struct inode *, struct inode *);

#else
#include <linux/sched.h>
#define lpfs_permission NULL
#define lpfs_get_acl	NULL
#define lpfs_set_acl	NULL

static inline int
lpfs_acl_chmod (struct inode *inode)
{
	return 0;
}

static inline int lpfs_init_acl (struct inode *inode, struct inode *dir)
{
	return 0;
}
#endif

