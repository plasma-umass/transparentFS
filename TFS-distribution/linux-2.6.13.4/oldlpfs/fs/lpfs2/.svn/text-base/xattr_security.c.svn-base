/*
 * linux/fs/lpfs/xattr_security.c
 * Handler for storing security labels as extended attributes.
 */

#include <linux/module.h>
#include <linux/string.h>
#include <linux/fs.h>
#include <linux/smp_lock.h>
#include <linux/lpfs_fs.h>
#include "xattr.h"

static size_t
lpfs_xattr_security_list(struct inode *inode, char *list, size_t list_size,
			 const char *name, size_t name_len)
{
	const int prefix_len = sizeof(XATTR_SECURITY_PREFIX)-1;
	const size_t total_len = prefix_len + name_len + 1;

	if (list && total_len <= list_size) {
		memcpy(list, XATTR_SECURITY_PREFIX, prefix_len);
		memcpy(list+prefix_len, name, name_len);
		list[prefix_len + name_len] = '\0';
	}
	return total_len;
}

static int
lpfs_xattr_security_get(struct inode *inode, const char *name,
		       void *buffer, size_t size)
{
	if (strcmp(name, "") == 0)
		return -EINVAL;
	return lpfs_xattr_get(inode, LPFS_XATTR_INDEX_SECURITY, name,
			      buffer, size);
}

static int
lpfs_xattr_security_set(struct inode *inode, const char *name,
		       const void *value, size_t size, int flags)
{
	if (strcmp(name, "") == 0)
		return -EINVAL;
	return lpfs_xattr_set(inode, LPFS_XATTR_INDEX_SECURITY, name,
			      value, size, flags);
}

struct xattr_handler lpfs_xattr_security_handler = {
	.prefix	= XATTR_SECURITY_PREFIX,
	.list	= lpfs_xattr_security_list,
	.get	= lpfs_xattr_security_get,
	.set	= lpfs_xattr_security_set,
};
