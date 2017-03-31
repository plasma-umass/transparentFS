/*
 *  linux/fs/tfs/xip.c
 *
 * Copyright (C) 2005 IBM Corporation
 * Author: Carsten Otte (cotte@de.ibm.com)
 */

#include <linux/mm.h>
#include <linux/fs.h>
#include <linux/genhd.h>
#include <linux/buffer_head.h>
#include <linux/tfs_fs_sb.h>
#include <linux/tfs_fs.h>
#include "tfs.h"
#include "xip.h"

static inline int
__inode_direct_access(struct inode *inode, sector_t sector,
		      unsigned long *data)
{
	BUG_ON(!inode->i_sb->s_bdev->bd_disk->fops->direct_access);
	return inode->i_sb->s_bdev->bd_disk->fops
		->direct_access(inode->i_sb->s_bdev,sector,data);
}

static inline int
__tfs_get_sector(struct inode *inode, sector_t offset, int create,
		   sector_t *result)
{
	struct buffer_head tmp;
	int rc;

	memset(&tmp, 0, sizeof(struct buffer_head));
	rc = tfs_get_block(inode, offset/ (PAGE_SIZE/512), &tmp,
			    create);
	*result = tmp.b_blocknr;

	/* did we get a sparse block (hole in the file)? */
	if (!tmp.b_blocknr && !rc) {
		BUG_ON(create);
		rc = -ENODATA;
	}

	return rc;
}

int
tfs_clear_xip_target(struct inode *inode, int block)
{
	sector_t sector = block * (PAGE_SIZE/512);
	unsigned long data;
	int rc;

	rc = __inode_direct_access(inode, sector, &data);
	if (!rc)
		clear_page((void*)data);
	return rc;
}

void tfs_xip_verify_sb(struct super_block *sb)
{
	struct tfs_sb_info *sbi = TFS_SB(sb);

	if ((sbi->s_mount_opt & TFS_MOUNT_XIP) &&
	    !sb->s_bdev->bd_disk->fops->direct_access) {
		sbi->s_mount_opt &= (~TFS_MOUNT_XIP);
		tfs_warning(sb, __FUNCTION__,
			     "ignoring xip option - not supported by bdev");
	}
}

struct page *
tfs_get_xip_page(struct address_space *mapping, sector_t offset,
		   int create)
{
	int rc;
	unsigned long data;
	sector_t sector;

	/* first, retrieve the sector number */
	rc = __tfs_get_sector(mapping->host, offset, create, &sector);
	if (rc)
		goto error;

	/* retrieve address of the target data */
	rc = __inode_direct_access
		(mapping->host, sector * (PAGE_SIZE/512), &data);
	if (!rc)
		return virt_to_page(data);

 error:
	return ERR_PTR(rc);
}
