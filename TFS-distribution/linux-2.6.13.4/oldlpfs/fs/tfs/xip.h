/*
 *  linux/fs/tfs/xip.h
 *
 * Copyright (C) 2005 IBM Corporation
 * Author: Carsten Otte (cotte@de.ibm.com)
 */

#ifdef CONFIG_TFS_FS_XIP
extern void tfs_xip_verify_sb (struct super_block *);
extern int tfs_clear_xip_target (struct inode *, int);

static inline int tfs_use_xip (struct super_block *sb)
{
	struct tfs_sb_info *sbi = TFS_SB(sb);
	return (sbi->s_mount_opt & TFS_MOUNT_XIP);
}
struct page* tfs_get_xip_page (struct address_space *, sector_t, int);
#define mapping_is_xip(map) unlikely(map->a_ops->get_xip_page)
#else
#define mapping_is_xip(map)			0
#define tfs_xip_verify_sb(sb)			do { } while (0)
#define tfs_use_xip(sb)			0
#define tfs_clear_xip_target(inode, chain)	0
#define tfs_get_xip_page			NULL
#endif
