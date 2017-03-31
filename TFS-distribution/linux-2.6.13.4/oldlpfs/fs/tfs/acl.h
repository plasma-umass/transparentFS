/*
  File: fs/tfs/acl.h

  (C) 2001 Andreas Gruenbacher, <a.gruenbacher@computer.org>
*/

#include <linux/posix_acl_xattr.h>

#define TFS_ACL_VERSION	0x0001

typedef struct {
	__le16		e_tag;
	__le16		e_perm;
	__le32		e_id;
} tfs_acl_entry;

typedef struct {
	__le16		e_tag;
	__le16		e_perm;
} tfs_acl_entry_short;

typedef struct {
	__le32		a_version;
} tfs_acl_header;

static inline size_t tfs_acl_size(int count)
{
	if (count <= 4) {
		return sizeof(tfs_acl_header) +
		       count * sizeof(tfs_acl_entry_short);
	} else {
		return sizeof(tfs_acl_header) +
		       4 * sizeof(tfs_acl_entry_short) +
		       (count - 4) * sizeof(tfs_acl_entry);
	}
}

static inline int tfs_acl_count(size_t size)
{
	ssize_t s;
	size -= sizeof(tfs_acl_header);
	s = size - 4 * sizeof(tfs_acl_entry_short);
	if (s < 0) {
		if (size % sizeof(tfs_acl_entry_short))
			return -1;
		return size / sizeof(tfs_acl_entry_short);
	} else {
		if (s % sizeof(tfs_acl_entry))
			return -1;
		return s / sizeof(tfs_acl_entry) + 4;
	}
}

#ifdef CONFIG_TFS_FS_POSIX_ACL

/* Value for inode->u.tfs_i.i_acl and inode->u.tfs_i.i_default_acl
   if the ACL has not been cached */
#define TFS_ACL_NOT_CACHED ((void *)-1)

/* acl.c */
extern int tfs_permission (struct inode *, int, struct nameidata *);
extern int tfs_acl_chmod (struct inode *);
extern int tfs_init_acl (struct inode *, struct inode *);

#else
#include <linux/sched.h>
#define tfs_permission NULL
#define tfs_get_acl	NULL
#define tfs_set_acl	NULL

static inline int
tfs_acl_chmod (struct inode *inode)
{
	return 0;
}

static inline int tfs_init_acl (struct inode *inode, struct inode *dir)
{
	return 0;
}
#endif

