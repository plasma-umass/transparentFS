/*
  File: linux/tfs_xattr.h

  On-disk format of extended attributes for the tfs filesystem.

  (C) 2001 Andreas Gruenbacher, <a.gruenbacher@computer.org>
*/

#include <linux/config.h>
#include <linux/init.h>
#include <linux/xattr.h>

/* Magic value in attribute blocks */
#define TFS_XATTR_MAGIC		0xEA020000

/* Maximum number of references to one attribute block */
#define TFS_XATTR_REFCOUNT_MAX		1024

/* Name indexes */
#define TFS_XATTR_INDEX_USER			1
#define TFS_XATTR_INDEX_POSIX_ACL_ACCESS	2
#define TFS_XATTR_INDEX_POSIX_ACL_DEFAULT	3
#define TFS_XATTR_INDEX_TRUSTED		4
#define	TFS_XATTR_INDEX_LUSTRE			5
#define TFS_XATTR_INDEX_SECURITY	        6

struct tfs_xattr_header {
	__le32	h_magic;	/* magic number for identification */
	__le32	h_refcount;	/* reference count */
	__le32	h_blocks;	/* number of disk blocks used */
	__le32	h_hash;		/* hash value of all attributes */
	__u32	h_reserved[4];	/* zero right now */
};

struct tfs_xattr_entry {
	__u8	e_name_len;	/* length of name */
	__u8	e_name_index;	/* attribute name index */
	__le16	e_value_offs;	/* offset in disk block of value */
	__le32	e_value_block;	/* disk block attribute is stored on (n/i) */
	__le32	e_value_size;	/* size of attribute value */
	__le32	e_hash;		/* hash value of name and value */
	char	e_name[0];	/* attribute name */
};

#define TFS_XATTR_PAD_BITS		2
#define TFS_XATTR_PAD		(1<<TFS_XATTR_PAD_BITS)
#define TFS_XATTR_ROUND		(TFS_XATTR_PAD-1)
#define TFS_XATTR_LEN(name_len) \
	(((name_len) + TFS_XATTR_ROUND + \
	sizeof(struct tfs_xattr_entry)) & ~TFS_XATTR_ROUND)
#define TFS_XATTR_NEXT(entry) \
	( (struct tfs_xattr_entry *)( \
	  (char *)(entry) + TFS_XATTR_LEN((entry)->e_name_len)) )
#define TFS_XATTR_SIZE(size) \
	(((size) + TFS_XATTR_ROUND) & ~TFS_XATTR_ROUND)

# ifdef CONFIG_TFS_FS_XATTR

extern struct xattr_handler tfs_xattr_user_handler;
extern struct xattr_handler tfs_xattr_trusted_handler;
extern struct xattr_handler tfs_xattr_acl_access_handler;
extern struct xattr_handler tfs_xattr_acl_default_handler;
extern struct xattr_handler tfs_xattr_security_handler;

extern ssize_t tfs_listxattr(struct dentry *, char *, size_t);

extern int tfs_xattr_get(struct inode *, int, const char *, void *, size_t);
extern int tfs_xattr_set(struct inode *, int, const char *, const void *, size_t, int);

extern void tfs_xattr_delete_inode(struct inode *);
extern void tfs_xattr_put_super(struct super_block *);

extern int init_tfs_xattr(void);
extern void exit_tfs_xattr(void);

extern struct xattr_handler *tfs_xattr_handlers[];

# else  /* CONFIG_TFS_FS_XATTR */

static inline int
tfs_xattr_get(struct inode *inode, int name_index,
	       const char *name, void *buffer, size_t size)
{
	return -EOPNOTSUPP;
}

static inline int
tfs_xattr_set(struct inode *inode, int name_index, const char *name,
	       const void *value, size_t size, int flags)
{
	return -EOPNOTSUPP;
}

static inline void
tfs_xattr_delete_inode(struct inode *inode)
{
}

static inline void
tfs_xattr_put_super(struct super_block *sb)
{
}

static inline int
init_tfs_xattr(void)
{
	return 0;
}

static inline void
exit_tfs_xattr(void)
{
}

#define tfs_xattr_handlers NULL

# endif  /* CONFIG_TFS_FS_XATTR */

#ifdef CONFIG_TFS_FS_SECURITY
extern int tfs_init_security(struct inode *inode, struct inode *dir);
#else
static inline int tfs_init_security(struct inode *inode, struct inode *dir)
{
	return 0;
}
#endif
