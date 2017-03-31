/*
 *  linux/fs/lpfs/super.c
 *
 * Copyright (C) 1992, 1993, 1994, 1995
 * Remy Card (card@masi.ibp.fr)
 * Laboratoire MASI - Institut Blaise Pascal
 * Universite Pierre et Marie Curie (Paris VI)
 *
 *  from
 *
 *  linux/fs/minix/inode.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 *
 *  Big-endian to little-endian byte-swapping/bitmaps by
 *        David S. Miller (davem@caip.rutgers.edu), 1995
 */

#include <linux/config.h>
#include <linux/module.h>
#include <linux/string.h>
#include <linux/fs.h>
#include "lpfs_fs.h"
#include <linux/slab.h>
#include <linux/init.h>
//#include <linux/locks.h>
#include <linux/blkdev.h>
#include <linux/vfs.h>
#include <asm/uaccess.h>
#include <linux/parser.h>
#include <linux/buffer_head.h>


static void lpfs_sync_super(struct super_block *sb,
			    struct lpfs_super_block *es);

static char error_buf[1024];

void lpfs_error (struct super_block * sb, const char * function,
		 const char * fmt, ...)
{
	va_list args;
	struct lpfs_super_block *es = LPFS_SBIP(sb)->s_es;

	if (!(sb->s_flags & MS_RDONLY)) {
		LPFS_SBI(sb).s_mount_state |= LPFS_ERROR_FS;
		es->s_state =
			cpu_to_le16(le16_to_cpu(es->s_state) | LPFS_ERROR_FS);
		lpfs_sync_super(sb, es);
	}
	va_start (args, fmt);
	vsprintf (error_buf, fmt, args);
	va_end (args);
	if (test_opt (sb, ERRORS_PANIC) ||
	    (le16_to_cpu(LPFS_SBI(sb).s_es->s_errors) == LPFS_ERRORS_PANIC &&
	     !test_opt (sb, ERRORS_CONT) && !test_opt (sb, ERRORS_RO)))
		panic ("LPFS-fs panic (device %s): %s: %s\n",
		       sb->s_id, function, error_buf);
	printk (KERN_CRIT "LPFS-fs error (device %s): %s: %s\n",
		sb->s_id, function, error_buf);
	if (test_opt (sb, ERRORS_RO) ||
	    (le16_to_cpu(LPFS_SBI(sb).s_es->s_errors) == LPFS_ERRORS_RO &&
	     !test_opt (sb, ERRORS_CONT) && !test_opt (sb, ERRORS_PANIC))) {
		printk ("Remounting filesystem read-only\n");
		sb->s_flags |= MS_RDONLY;
	}
}

NORET_TYPE void lpfs_panic (struct super_block * sb, const char * function,
			    const char * fmt, ...)
{
	va_list args;

	if (!(sb->s_flags & MS_RDONLY)) {
		LPFS_SBI(sb).s_mount_state |= LPFS_ERROR_FS;
		LPFS_SBI(sb).s_es->s_state =
			cpu_to_le16(le16_to_cpu(LPFS_SBI(sb).s_es->s_state) | LPFS_ERROR_FS);
		mark_buffer_dirty(LPFS_SBI(sb).s_sbh);
		sb->s_dirt = 1;
	}
	va_start (args, fmt);
	vsprintf (error_buf, fmt, args);
	va_end (args);
	sb->s_flags |= MS_RDONLY;
	panic ("LPFS-fs panic (device %s): %s: %s\n",
	       sb->s_id, function, error_buf);
}

void lpfs_warning (struct super_block * sb, const char * function,
		   const char * fmt, ...)
{
	va_list args;

	va_start (args, fmt);
	vsprintf (error_buf, fmt, args);
	va_end (args);
	printk (KERN_WARNING "LPFS-fs warning (device %s): %s: %s\n",
		sb->s_id, function, error_buf);
}

void lpfs_update_dynamic_rev(struct super_block *sb)
{
	struct lpfs_super_block *es = LPFS_SBIP(sb)->s_es;

	if (le32_to_cpu(es->s_rev_level) > LPFS_GOOD_OLD_REV)
		return;

	lpfs_warning(sb, __FUNCTION__,
		     "updating to rev %d because of new feature flag, "
		     "running e2fsck is recommended",
		     LPFS_DYNAMIC_REV);

	es->s_first_ino = cpu_to_le32(LPFS_GOOD_OLD_FIRST_INO);
	es->s_inode_size = cpu_to_le16(LPFS_GOOD_OLD_INODE_SIZE);
	es->s_rev_level = cpu_to_le32(LPFS_DYNAMIC_REV);
	/* leave es->s_feature_*compat flags alone */
	/* es->s_uuid will be set by e2fsck if empty */

	/*
	 * The rest of the superblock fields should be zero, and if not it
	 * means they are likely already in use, so leave them alone.  We
	 * can leave it up to e2fsck to clean up any inconsistencies there.
	 */
}

void lpfs_put_super (struct super_block * sb)
{
	int db_count;
	int i;

	if (!(sb->s_flags & MS_RDONLY)) {
		struct lpfs_super_block *es = LPFS_SBIP(sb)->s_es;

		es->s_state = le16_to_cpu(LPFS_SBIP(sb)->s_mount_state);
		lpfs_sync_super(sb, es);
	}
	db_count = LPFS_SBIP(sb)->s_gdb_count;
	for (i = 0; i < db_count; i++)
		if (LPFS_SBI(sb).s_group_desc[i])
			brelse (LPFS_SBI(sb).s_group_desc[i]);
	kfree(LPFS_SBI(sb).s_group_desc);
	for (i = 0; i < LPFS_MAX_GROUP_LOADED; i++)
		if (LPFS_SBI(sb).s_inode_bitmap[i])
			brelse (LPFS_SBI(sb).s_inode_bitmap[i]);
	for (i = 0; i < LPFS_MAX_GROUP_LOADED; i++)
		if (LPFS_SBI(sb).s_block_bitmap[i])
			brelse (LPFS_SBI(sb).s_block_bitmap[i]);
	/* JC */
	// WRITE OUT AND FREE DIRTY AND LOW PRIORITY BITMAPS
	//FIXME why are we looping though the groups separately for each bitmap
	//I dont know why they loop through the group descriptors
	//separately to release inode and block bitmaps, but I will
	//follow suit (i also dont like omitting curley brackets)
	for (i=0; i < LPFS_MAX_GROUP_LOADED; i++ )
		if (LPFS_SBI(sb).s_lowPriority_bitmap[i])
			brelse (LPFS_SBI(sb).s_lowPriority_bitmap[i]);
	for (i=0; i < LPFS_MAX_GROUP_LOADED; i++ )
		if(LPFS_SBI(sb).s_dirty_bitmap[i])
			brelse (LPFS_SBI(sb).s_dirty_bitmap[i]);
	//free blockLock
	for(i=0;i<LPFS_SBI(sb).s_groups_count;i++){
		kfree(LPFS_SBI(sb).s_blockLock_bitmaps[i]);
	}
	kfree(LPFS_SBI(sb).s_blockLock_bitmaps);
	/* /JC */
	brelse (LPFS_SBI(sb).s_sbh);

	return;
}

static struct super_operations lpfs_sops = {
	read_inode:	lpfs_read_inode,
	write_inode:	lpfs_write_inode,
	put_inode:	lpfs_put_inode,
	delete_inode:	lpfs_delete_inode,
	put_super:	lpfs_put_super,
	write_super:	lpfs_write_super,
	statfs:		lpfs_statfs,
	remount_fs:	lpfs_remount,
};

/*
 * This function has been shamelessly adapted from the msdos fs
 *
static int parse_options (char * options, unsigned long * sb_block,
			  unsigned short *resuid, unsigned short * resgid,
			  unsigned long * mount_options)
{
	char * this_char;
	char * value;

	if (!options)
		return 1;
	for (this_char = strtok (&options, ",");
	     this_char != NULL;
	     this_char = strtok (NULL, ",")) {
		if ((value = strchr (this_char, '=')) != NULL)
			*value++ = 0;
		if (!strcmp (this_char, "bsddf"))
			clear_opt (*mount_options, MINIX_DF);
		else if (!strcmp (this_char, "nouid32")) {
			set_opt (*mount_options, NO_UID32);
		}
		else if (!strcmp (this_char, "check")) {
			if (!value || !*value || !strcmp (value, "none"))
				clear_opt (*mount_options, CHECK);
			else
#ifdef CONFIG_LPFS_CHECK
				set_opt (*mount_options, CHECK);
#else
				printk("LPFS Check option not supported\n");
#endif
		}
		else if (!strcmp (this_char, "debug"))
			set_opt (*mount_options, DEBUG);
		else if (!strcmp (this_char, "errors")) {
			if (!value || !*value) {
				printk ("LPFS-fs: the errors option requires "
					"an argument\n");
				return 0;
			}
			if (!strcmp (value, "continue")) {
				clear_opt (*mount_options, ERRORS_RO);
				clear_opt (*mount_options, ERRORS_PANIC);
				set_opt (*mount_options, ERRORS_CONT);
			}
			else if (!strcmp (value, "remount-ro")) {
				clear_opt (*mount_options, ERRORS_CONT);
				clear_opt (*mount_options, ERRORS_PANIC);
				set_opt (*mount_options, ERRORS_RO);
			}
			else if (!strcmp (value, "panic")) {
				clear_opt (*mount_options, ERRORS_CONT);
				clear_opt (*mount_options, ERRORS_RO);
				set_opt (*mount_options, ERRORS_PANIC);
			}
			else {
				printk ("LPFS-fs: Invalid errors option: %s\n",
					value);
				return 0;
			}
		}
		else if (!strcmp (this_char, "grpid") ||
			 !strcmp (this_char, "bsdgroups"))
			set_opt (*mount_options, GRPID);
		else if (!strcmp (this_char, "minixdf"))
			set_opt (*mount_options, MINIX_DF);
		else if (!strcmp (this_char, "nocheck"))
			clear_opt (*mount_options, CHECK);
		else if (!strcmp (this_char, "nogrpid") ||
			 !strcmp (this_char, "sysvgroups"))
			clear_opt (*mount_options, GRPID);
		else if (!strcmp (this_char, "resgid")) {
			if (!value || !*value) {
				printk ("LPFS-fs: the resgid option requires "
					"an argument\n");
				return 0;
			}
			*resgid = simple_strtoul (value, &value, 0);
			if (*value) {
				printk ("LPFS-fs: Invalid resgid option: %s\n",
					value);
				return 0;
			}
		}
		else if (!strcmp (this_char, "resuid")) {
			if (!value || !*value) {
				printk ("LPFS-fs: the resuid option requires "
					"an argument");
				return 0;
			}
			*resuid = simple_strtoul (value, &value, 0);
			if (*value) {
				printk ("LPFS-fs: Invalid resuid option: %s\n",
					value);
				return 0;
			}
		}
		else if (!strcmp (this_char, "sb")) {
			if (!value || !*value) {
				printk ("LPFS-fs: the sb option requires "
					"an argument");
				return 0;
			}
			*sb_block = simple_strtoul (value, &value, 0);
			if (*value) {
				printk ("LPFS-fs: Invalid sb option: %s\n",
					value);
				return 0;
			}
		}
		/* Silently ignore the quota options *
		else if (!strcmp (this_char, "grpquota")
		         || !strcmp (this_char, "noquota")
		         || !strcmp (this_char, "quota")
		         || !strcmp (this_char, "usrquota"))
			/* Don't do anything ;-) *;
		else {
			printk ("LPFS-fs: Unrecognized mount option %s\n", this_char);
			return 0;
		}
	}
	return 1;
}
*/










//This was just stolen from ext, if something doesnt work, this is a good bet
enum {
	Opt_bsd_df, Opt_minix_df, Opt_grpid, Opt_nogrpid,
	Opt_resgid, Opt_resuid, Opt_sb, Opt_err_cont, Opt_err_panic, Opt_err_ro,
	Opt_nouid32, Opt_check, Opt_nocheck, Opt_debug, Opt_oldalloc, Opt_orlov, Opt_nobh,
	Opt_user_xattr, Opt_nouser_xattr, Opt_acl, Opt_noacl,
	Opt_ignore, Opt_err,
};

static match_table_t tokens = {
	{Opt_bsd_df, "bsddf"},
	{Opt_minix_df, "minixdf"},
	{Opt_grpid, "grpid"},
	{Opt_grpid, "bsdgroups"},
	{Opt_nogrpid, "nogrpid"},
	{Opt_nogrpid, "sysvgroups"},
	{Opt_resgid, "resgid=%u"},
	{Opt_resuid, "resuid=%u"},
	{Opt_sb, "sb=%u"},
	{Opt_err_cont, "errors=continue"},
	{Opt_err_panic, "errors=panic"},
	{Opt_err_ro, "errors=remount-ro"},
	{Opt_nouid32, "nouid32"},
	{Opt_nocheck, "check=none"},
	{Opt_nocheck, "nocheck"},
	{Opt_check, "check"},
	{Opt_debug, "debug"},
	{Opt_oldalloc, "oldalloc"},
	{Opt_orlov, "orlov"},
	{Opt_nobh, "nobh"},
	{Opt_user_xattr, "user_xattr"},
	{Opt_nouser_xattr, "nouser_xattr"},
	{Opt_acl, "acl"},
	{Opt_noacl, "noacl"},
	{Opt_ignore, "grpquota"},
	{Opt_ignore, "noquota"},
	{Opt_ignore, "quota"},
	{Opt_ignore, "usrquota"},
	{Opt_err, NULL}
};

static int parse_options (char * options,
			  struct lpfs_sb_info *sbi)
{
	char * p;
	substring_t args[MAX_OPT_ARGS];
	unsigned long kind = LPFS_MOUNT_ERRORS_CONT;
	int option;

	if (!options)
		return 1;

	while ((p = strsep (&options, ",")) != NULL) {
		int token;
		if (!*p)
			continue;

		token = match_token(p, tokens, args);
		switch (token) {
		case Opt_bsd_df:
			clear_opt (sbi->s_mount_opt, MINIX_DF);
			break;
		case Opt_minix_df:
			set_opt (sbi->s_mount_opt, MINIX_DF);
			break;
		case Opt_grpid:
			set_opt (sbi->s_mount_opt, GRPID);
			break;
		case Opt_nogrpid:
			clear_opt (sbi->s_mount_opt, GRPID);
			break;
		case Opt_resuid:
			if (match_int(&args[0], &option))
				return 0;
			sbi->s_resuid = option;
			break;
		case Opt_resgid:
			if (match_int(&args[0], &option))
				return 0;
			sbi->s_resgid = option;
			break;
		case Opt_sb:
			/* handled by get_sb_block() instead of here */
			/* *sb_block = match_int(&args[0]); */
			break;
		case Opt_err_panic:
			kind = LPFS_MOUNT_ERRORS_PANIC;
			break;
		case Opt_err_ro:
			kind = LPFS_MOUNT_ERRORS_RO;
			break;
		case Opt_err_cont:
			kind = LPFS_MOUNT_ERRORS_CONT;
			break;
		case Opt_nouid32:
			set_opt (sbi->s_mount_opt, NO_UID32);
			break;
		case Opt_check:
#ifdef CONFIG_LPFS_CHECK
			set_opt (sbi->s_mount_opt, CHECK);
#else
			printk("LPFS Check option not supported\n");
#endif
			break;
		case Opt_nocheck:
			clear_opt (sbi->s_mount_opt, CHECK);
			break;
		case Opt_debug:
			set_opt (sbi->s_mount_opt, DEBUG);
			break;
//		case Opt_oldalloc:
//			set_opt (sbi->s_mount_opt, OLDALLOC);
//			break;
//		case Opt_orlov:
//			clear_opt (sbi->s_mount_opt, OLDALLOC);
//			break;
//		case Opt_nobh:
//			set_opt (sbi->s_mount_opt, NOBH);
//			break;
#ifdef CONFIG_LPFS_FS_XATTR
		case Opt_user_xattr:
			set_opt (sbi->s_mount_opt, XATTR_USER);
			break;
		case Opt_nouser_xattr:
			clear_opt (sbi->s_mount_opt, XATTR_USER);
			break;
#else
		case Opt_user_xattr:
		case Opt_nouser_xattr:
			printk("LPFS (no)user_xattr options not supported\n");
			break;
#endif
#ifdef CONFIG_LPFS_FS_POSIX_ACL
		case Opt_acl:
			set_opt(sbi->s_mount_opt, POSIX_ACL);
			break;
		case Opt_noacl:
			clear_opt(sbi->s_mount_opt, POSIX_ACL);
			break;
#else
		case Opt_acl:
		case Opt_noacl:
			printk("LPFS (no)acl options not supported\n");
			break;
#endif
		case Opt_ignore:
			break;
		default:
			return 0;
		}
	}
	sbi->s_mount_opt |= kind;
	return 1;
}


















static int lpfs_setup_super (struct super_block * sb,
			      struct lpfs_super_block * es,
			      int read_only)
{
	int res = 0;
	if (le32_to_cpu(es->s_rev_level) > LPFS_MAX_SUPP_REV) {
		printk ("LPFS-fs warning: revision level too high, "
			"forcing read-only mode\n");
		res = MS_RDONLY;
	}
	if (read_only)
		return res;
	if (!(LPFS_SBI(sb).s_mount_state & LPFS_VALID_FS))
		printk ("LPFS-fs warning: mounting unchecked fs, "
			"running e2fsck is recommended\n");
	else if ((LPFS_SBI(sb).s_mount_state & LPFS_ERROR_FS))
		printk ("LPFS-fs warning: mounting fs with errors, "
			"running e2fsck is recommended\n");
	else if ((__s16) le16_to_cpu(es->s_max_mnt_count) >= 0 &&
		 le16_to_cpu(es->s_mnt_count) >=
		 (unsigned short) (__s16) le16_to_cpu(es->s_max_mnt_count))
		printk ("LPFS-fs warning: maximal mount count reached, "
			"running e2fsck is recommended\n");
	else if (le32_to_cpu(es->s_checkinterval) &&
		(le32_to_cpu(es->s_lastcheck) + le32_to_cpu(es->s_checkinterval) <= get_seconds()))
		printk ("LPFS-fs warning: checktime reached, "
			"running e2fsck is recommended\n");
	if (!(__s16) le16_to_cpu(es->s_max_mnt_count))
		es->s_max_mnt_count = (__s16) cpu_to_le16(LPFS_DFL_MAX_MNT_COUNT);
	es->s_mnt_count=cpu_to_le16(le16_to_cpu(es->s_mnt_count) + 1);
	lpfs_write_super(sb);
	if (test_opt (sb, DEBUG))
		printk ("[EXT II FS %s, %s, bs=%lu, fs=%lu, gc=%lu, "
			"bpg=%lu, ipg=%lu, mo=%04lx]\n",
			LPFSFS_VERSION, LPFSFS_DATE, sb->s_blocksize,
			LPFS_SBI(sb).s_frag_size,
			LPFS_SBI(sb).s_groups_count,
			LPFS_BLOCKS_PER_GROUP(sb),
			LPFS_INODES_PER_GROUP(sb),
			LPFS_SBI(sb).s_mount_opt);
#ifdef CONFIG_LPFS_CHECK
	if (test_opt (sb, CHECK)) {
		lpfs_check_blocks_bitmap (sb);
		lpfs_check_inodes_bitmap (sb);
	}
#endif
	return res;
}

static int lpfs_check_descriptors (struct super_block * sb)
{
	int i;
	int desc_block = 0;
	unsigned long block = le32_to_cpu(LPFS_SBI(sb).s_es->s_first_data_block);
	struct lpfs_group_desc * gdp = NULL;

	lpfs_debug ("Checking group descriptors");

	for (i = 0; i < LPFS_SBI(sb).s_groups_count; i++)
	{
		if ((i % LPFS_DESC_PER_BLOCK(sb)) == 0)
			gdp = (struct lpfs_group_desc *) LPFS_SBI(sb).s_group_desc[desc_block++]->b_data;
		if (le32_to_cpu(gdp->bg_block_bitmap) < block ||
		    le32_to_cpu(gdp->bg_block_bitmap) >= block + LPFS_BLOCKS_PER_GROUP(sb))
		{
			lpfs_error (sb, "lpfs_check_descriptors",
				    "Block bitmap for group %d"
				    " not in group (block %lu)!",
				    i, (unsigned long) le32_to_cpu(gdp->bg_block_bitmap));
			return 0;
		}
		/* JC */
		//similar to above, make sure that low priority and dirty bitmaps are in the goup
		if (le32_to_cpu(gdp->bg_lowPriority_bitmap) < block ||
		    le32_to_cpu(gdp->bg_lowPriority_bitmap) >= block + LPFS_BLOCKS_PER_GROUP(sb))
		{
			lpfs_error (sb, "lpfs_check_descriptors",
				    "Low priority bitmap for group %d"
				    " not in group (block %lu)!",
				    i, (unsigned long) le32_to_cpu(gdp->bg_lowPriority_bitmap));
			return 0;
		}
		if (le32_to_cpu(gdp->bg_dirty_bitmap) < block ||
		    le32_to_cpu(gdp->bg_dirty_bitmap) >= block + LPFS_BLOCKS_PER_GROUP(sb))
		{
			lpfs_error (sb, "lpfs_check_descriptors",
				    "Dirty bitmap for group %d"
				    " not in group (block %lu)!",
				    i, (unsigned long) le32_to_cpu(gdp->bg_dirty_bitmap));
			return 0;
		}
		/* /JC */
		if (le32_to_cpu(gdp->bg_inode_bitmap) < block ||
		    le32_to_cpu(gdp->bg_inode_bitmap) >= block + LPFS_BLOCKS_PER_GROUP(sb))
		{
			lpfs_error (sb, "lpfs_check_descriptors",
				    "Inode bitmap for group %d"
				    " not in group (block %lu)!",
				    i, (unsigned long) le32_to_cpu(gdp->bg_inode_bitmap));
			return 0;
		}
		if (le32_to_cpu(gdp->bg_inode_table) < block ||
		    le32_to_cpu(gdp->bg_inode_table) + LPFS_SBI(sb).s_itb_per_group >=
		    block + LPFS_BLOCKS_PER_GROUP(sb))
		{
			lpfs_error (sb, "lpfs_check_descriptors",
				    "Inode table for group %d"
				    " not in group (block %lu)!",
				    i, (unsigned long) le32_to_cpu(gdp->bg_inode_table));
			return 0;
		}
		block += LPFS_BLOCKS_PER_GROUP(sb);
		gdp++;
	}
	return 1;
}

#define log2(n) ffz(~(n))
 
/*
 * Maximal file size.  There is a direct, and {,double-,triple-}indirect
 * block limit, and also a limit of (2^32 - 1) 512-byte sectors in i_blocks.
 * We need to be 1 filesystem block less than the 2^32 sector limit.
 */
static loff_t lpfs_max_size(int bits)
{
	loff_t res = LPFS_NDIR_BLOCKS;
	res += 1LL << (bits-2);
	res += 1LL << (2*(bits-2));
	res += 1LL << (3*(bits-2));
	res <<= bits;
	if (res > (512LL << 32) - (1 << bits))
		res = (512LL << 32) - (1 << bits);
	return res;
}

static unsigned long descriptor_loc(struct super_block *sb,
				    unsigned long logic_sb_block,
				    int nr)
{
	struct lpfs_sb_info *sbi = LPFS_SBIP(sb);
	unsigned long bg, first_data_block, first_meta_bg;
	int has_super = 0;
	
	first_data_block = le32_to_cpu(sbi->s_es->s_first_data_block);
	first_meta_bg = le32_to_cpu(sbi->s_es->s_first_meta_bg);

	if (!LPFS_HAS_INCOMPAT_FEATURE(sb, LPFS_FEATURE_INCOMPAT_META_BG) ||
	    nr < first_meta_bg)
		return (logic_sb_block + nr + 1);
	bg = sbi->s_desc_per_block * nr;
	if (lpfs_bg_has_super(sb, bg))
		has_super = 1;
	return (first_data_block + has_super + (bg * sbi->s_blocks_per_group));
}

int lpfs_read_super (struct super_block * sb, void * data,
				      int silent)
{
	struct buffer_head * bh;
  	struct lpfs_sb_info * sbi;
	struct lpfs_super_block * es;
	unsigned long sb_block = 1;
	unsigned short resuid = LPFS_DEF_RESUID;
	unsigned short resgid = LPFS_DEF_RESGID;
	unsigned long block;
	unsigned long logic_sb_block;
	unsigned long offset = 0;
	dev_t dev = sb->s_dev;
	int blocksize = BLOCK_SIZE;
	int db_count;
	int i, j;

	/*
	 * See what the current blocksize for the device is, and
	 * use that as the blocksize.  Otherwise (or if the blocksize
	 * is smaller than the default) use the default.
	 * This is important for devices that have a hardware
	 * sectorsize that is larger than the default.
	 */
	blocksize = sb_min_blocksize(sb,BLOCK_SIZE);
	if(blocksize < BLOCK_SIZE )
	    blocksize = BLOCK_SIZE;

	sbi=kmalloc(sizeof(*sbi),GFP_KERNEL);
	if(!sbi){
		return -ENOMEM;
	}
	sb->s_fs_info=sbi;
	memset(sbi,0,sizeof(*sbi));
	LPFS_SBI(sb).s_mount_opt = 0;
	if (!parse_options ((char *) data, sbi)) {
		return -EINVAL;
	}

//	if (set_blocksize(dev, blocksize) < 0) {
//		printk ("LPFS-fs: unable to set blocksize %d\n", blocksize);
//		return NULL;
//	}
//	sb->s_blocksize = blocksize;

	/*
	 * If the superblock doesn't start on a sector boundary,
	 * calculate the offset.  FIXME(eric) this doesn't make sense
	 * that we would have to do this.
	 */
	if (blocksize != BLOCK_SIZE) {
		logic_sb_block = (sb_block*BLOCK_SIZE) / blocksize;
		offset = (sb_block*BLOCK_SIZE) % blocksize;
	} else {
		logic_sb_block = sb_block;
	}

	if (!(bh = sb_bread(sb, logic_sb_block))) {
		printk ("LPFS-fs: unable to read superblock\n");
		return -EINVAL;
	}
	/*
	 * Note: s_es must be initialized as soon as possible because
	 *       some lpfs macro-instructions depend on its value
	 */
	es = (struct lpfs_super_block *) (((char *)bh->b_data) + offset);
	LPFS_SBI(sb).s_es = es;
	sb->s_magic = le16_to_cpu(es->s_magic);
	if (sb->s_magic != LPFS_SUPER_MAGIC) {
		if (!silent)
			printk ("VFS: Can't find lpfs filesystem on dev %s.\n",
				sb->s_id);
		goto failed_mount;
	}
	if (le32_to_cpu(es->s_rev_level) == LPFS_GOOD_OLD_REV &&
	    (LPFS_HAS_COMPAT_FEATURE(sb, ~0U) ||
	     LPFS_HAS_RO_COMPAT_FEATURE(sb, ~0U) ||
	     LPFS_HAS_INCOMPAT_FEATURE(sb, ~0U)))
		printk("LPFS-fs warning: feature flags set on rev 0 fs, "
		       "running e2fsck is recommended\n");
	/*
	 * Check feature flags regardless of the revision level, since we
	 * previously didn't change the revision level when setting the flags,
	 * so there is a chance incompat flags are set on a rev 0 filesystem.
	 */
	if ((i = LPFS_HAS_INCOMPAT_FEATURE(sb, ~LPFS_FEATURE_INCOMPAT_SUPP))) {
		printk("LPFS-fs: %s: couldn't mount because of "
		       "unsupported optional features (%x).\n",
		       sb->s_id, i);
		goto failed_mount;
	}
	if (!(sb->s_flags & MS_RDONLY) &&
	    (i = LPFS_HAS_RO_COMPAT_FEATURE(sb, ~LPFS_FEATURE_RO_COMPAT_SUPP))){
		printk("LPFS-fs: %s: couldn't mount RDWR because of "
		       "unsupported optional features (%x).\n",
		       sb->s_id, i);
		goto failed_mount;
	}
	if (LPFS_HAS_COMPAT_FEATURE(sb, EXT3_FEATURE_COMPAT_HAS_JOURNAL))
		lpfs_warning(sb, __FUNCTION__,
			"mounting ext3 filesystem as lpfs\n");
	sb->s_blocksize_bits =
		le32_to_cpu(LPFS_SBIP(sb)->s_es->s_log_block_size) + 10;
	sb->s_blocksize = 1 << sb->s_blocksize_bits;

	sb->s_maxbytes = lpfs_max_size(sb->s_blocksize_bits);

	/* If the blocksize doesn't match, re-read the thing.. */
	if (sb->s_blocksize != blocksize) {
		blocksize = sb->s_blocksize;
		brelse(bh);

		if (!sb_set_blocksize(sb, blocksize)) {
			printk(KERN_ERR "LPFS-fs: blocksize too small for device.\n");
			return -EINVAL;
		}

		logic_sb_block = (sb_block*BLOCK_SIZE) / blocksize;
		offset = (sb_block*BLOCK_SIZE) % blocksize;
		bh = sb_bread(sb, logic_sb_block);
		if(!bh) {
			printk("LPFS-fs: Couldn't read superblock on "
			       "2nd try.\n");
			goto failed_mount;
		}
		es = (struct lpfs_super_block *) (((char *)bh->b_data) + offset);
		LPFS_SBI(sb).s_es = es;
		if (es->s_magic != le16_to_cpu(LPFS_SUPER_MAGIC)) {
			printk ("LPFS-fs: Magic mismatch, very weird !\n");
			goto failed_mount;
		}
	}

	if (le32_to_cpu(es->s_rev_level) == LPFS_GOOD_OLD_REV) {
		sbi->s_inode_size = LPFS_GOOD_OLD_INODE_SIZE;
		sbi->s_first_ino = LPFS_GOOD_OLD_FIRST_INO;
	} else {
		sbi->s_inode_size = le16_to_cpu(es->s_inode_size);
		sbi->s_first_ino = le32_to_cpu(es->s_first_ino);
		if ((sbi->s_inode_size < LPFS_GOOD_OLD_INODE_SIZE) ||
		    (sbi->s_inode_size & (sbi->s_inode_size - 1)) ||
		    (sbi->s_inode_size > blocksize)) {
			printk ("LPFS-fs: unsupported inode size: %d\n",
				sbi->s_inode_size);
			goto failed_mount;
		}
	}
	LPFS_SBI(sb).s_frag_size = LPFS_MIN_FRAG_SIZE <<
				   le32_to_cpu(es->s_log_frag_size);
	if (LPFS_SBI(sb).s_frag_size)
		LPFS_SBI(sb).s_frags_per_block = sb->s_blocksize /
						  LPFS_SBI(sb).s_frag_size;
	else
		sb->s_magic = 0;
	LPFS_SBI(sb).s_blocks_per_group = le32_to_cpu(es->s_blocks_per_group);
	LPFS_SBI(sb).s_frags_per_group = le32_to_cpu(es->s_frags_per_group);
	LPFS_SBI(sb).s_inodes_per_group = le32_to_cpu(es->s_inodes_per_group);
	LPFS_SBI(sb).s_inodes_per_block = sb->s_blocksize /
					   LPFS_INODE_SIZE(sb);
	LPFS_SBI(sb).s_itb_per_group = LPFS_SBI(sb).s_inodes_per_group /
				        LPFS_SBI(sb).s_inodes_per_block;
	LPFS_SBI(sb).s_desc_per_block = sb->s_blocksize /
					 sizeof (struct lpfs_group_desc);
	LPFS_SBI(sb).s_sbh = bh;
	if (resuid != LPFS_DEF_RESUID)
		LPFS_SBI(sb).s_resuid = resuid;
	else
		LPFS_SBI(sb).s_resuid = le16_to_cpu(es->s_def_resuid);
	if (resgid != LPFS_DEF_RESGID)
		LPFS_SBI(sb).s_resgid = resgid;
	else
		LPFS_SBI(sb).s_resgid = le16_to_cpu(es->s_def_resgid);
	LPFS_SBI(sb).s_mount_state = le16_to_cpu(es->s_state);
	LPFS_SBI(sb).s_addr_per_block_bits =
		log2 (LPFS_ADDR_PER_BLOCK(sb));
	LPFS_SBI(sb).s_desc_per_block_bits =
		log2 (LPFS_DESC_PER_BLOCK(sb));
	if (sb->s_magic != LPFS_SUPER_MAGIC) {
		if (!silent)
			printk ("VFS: Can't find an lpfs filesystem on dev "
				"%s.\n",
				sb->s_id);
		goto failed_mount;
	}
	if (sb->s_blocksize != bh->b_size) {
		if (!silent)
			printk ("VFS: Unsupported blocksize on dev "
				"%s.\n", sb->s_id);
		goto failed_mount;
	}

	if (sb->s_blocksize != LPFS_SBI(sb).s_frag_size) {
		printk ("LPFS-fs: fragsize %lu != blocksize %lu (not supported yet)\n",
			LPFS_SBI(sb).s_frag_size, sb->s_blocksize);
		goto failed_mount;
	}

	if (LPFS_SBI(sb).s_blocks_per_group > sb->s_blocksize * 8) {
		printk ("LPFS-fs: #blocks per group too big: %lu\n",
			LPFS_SBI(sb).s_blocks_per_group);
		goto failed_mount;
	}
	if (LPFS_SBI(sb).s_frags_per_group > sb->s_blocksize * 8) {
		printk ("LPFS-fs: #fragments per group too big: %lu\n",
			LPFS_SBI(sb).s_frags_per_group);
		goto failed_mount;
	}
	if (LPFS_SBI(sb).s_inodes_per_group > sb->s_blocksize * 8) {
		printk ("LPFS-fs: #inodes per group too big: %lu\n",
			LPFS_SBI(sb).s_inodes_per_group);
		goto failed_mount;
	}

	LPFS_SBI(sb).s_groups_count = (le32_to_cpu(es->s_blocks_count) -
				        le32_to_cpu(es->s_first_data_block) +
				       LPFS_BLOCKS_PER_GROUP(sb) - 1) /
				       LPFS_BLOCKS_PER_GROUP(sb);
	db_count = (LPFS_SBI(sb).s_groups_count + LPFS_DESC_PER_BLOCK(sb) - 1) /
		   LPFS_DESC_PER_BLOCK(sb);
	LPFS_SBI(sb).s_group_desc = kmalloc (db_count * sizeof (struct buffer_head *), GFP_KERNEL);
	if (LPFS_SBI(sb).s_group_desc == NULL) {
		printk ("LPFS-fs: not enough memory\n");
		goto failed_mount;
	}
	for (i = 0; i < db_count; i++) {
		block = descriptor_loc(sb, logic_sb_block, i);
		sbi->s_group_desc[i] = sb_bread(sb, block);
		if (!sbi->s_group_desc[i]) {
			for (j = 0; j < i; j++)
				brelse (sbi->s_group_desc[j]);
			kfree(sbi->s_group_desc);
			printk ("LPFS-fs: unable to read group descriptors\n");
			goto failed_mount;
		}
	}
	if (!lpfs_check_descriptors (sb)) {
		printk ("LPFS-fs: group descriptors corrupted!\n");
		db_count = i;
		goto failed_mount2;
	}
	for (i = 0; i < LPFS_MAX_GROUP_LOADED; i++) {
		LPFS_SBI(sb).s_inode_bitmap_number[i] = 0;
		LPFS_SBI(sb).s_inode_bitmap[i] = NULL;
		LPFS_SBI(sb).s_block_bitmap_number[i] = 0;
		LPFS_SBI(sb).s_block_bitmap[i] = NULL;
		/* JC */
		//zero the low priority and dirty bitmap buffer heads
		LPFS_SBI(sb).s_dirty_bitmap[i]=NULL;
		LPFS_SBI(sb).s_lowPriority_bitmap[i]=NULL;
		/* /JC */
	}
	LPFS_SBI(sb).s_loaded_inode_bitmaps = 0;
	LPFS_SBI(sb).s_loaded_block_bitmaps = 0;
	LPFS_SBI(sb).s_gdb_count = db_count;
	/* JC allocate blocklock bitmaps */
	LPFS_SBI(sb).s_blockLock_bitmaps=kmalloc(sizeof(char *)*LPFS_SBI(sb).s_groups_count, GFP_KERNEL);
	if(LPFS_SBI(sb).s_blockLock_bitmaps==NULL){
		goto failed_mount2;
	}
	for(i=0;i<LPFS_SBI(sb).s_groups_count;i++){
		int j;
		LPFS_SBI(sb).s_blockLock_bitmaps[i]=kmalloc((LPFS_BLOCKS_PER_GROUP(sb)+7)/8, GFP_KERNEL);
		if(LPFS_SBI(sb).s_blockLock_bitmaps[i]==NULL){
			goto failed_mount3;
		}
		for(j=0;j<(LPFS_BLOCKS_PER_GROUP(sb)+7)/8;j++){
			LPFS_SBI(sb).s_blockLock_bitmaps[i][j]=0;
		}
	}
	/* /JC */	
	/*
	 * set up enough so that it can read an inode
	 */
	sb->s_op = &lpfs_sops;
	sb->s_root = d_alloc_root(iget(sb, LPFS_ROOT_INO));
	if (!sb->s_root || !S_ISDIR(sb->s_root->d_inode->i_mode) ||
	    !sb->s_root->d_inode->i_blocks || !sb->s_root->d_inode->i_size) {
		if (sb->s_root) {
			dput(sb->s_root);
			sb->s_root = NULL;
			printk(KERN_ERR "LPFS-fs: corrupt root inode, run e2fsck\n");
		} else
			printk(KERN_ERR "LPFS-fs: get root inode failed\n");
		goto failed_mount2;
	}
	lpfs_setup_super (sb, es, sb->s_flags & MS_RDONLY);
	return 0;
/* JC free blockLock on mount error */
failed_mount3:
	for(i--;i>=0;i--){
		kfree(LPFS_SBI(sb).s_blockLock_bitmaps[i]);
	}
	kfree(LPFS_SBI(sb).s_blockLock_bitmaps);
/* /JC */
failed_mount2:
	for (i = 0; i < db_count; i++)
		brelse(LPFS_SBI(sb).s_group_desc[i]);
	kfree(LPFS_SBI(sb).s_group_desc);
failed_mount:
	brelse(bh);
	return -EINVAL;
}

static void lpfs_commit_super (struct super_block * sb,
			       struct lpfs_super_block * es)
{
	es->s_wtime = get_seconds();
	mark_buffer_dirty(LPFS_SBI(sb).s_sbh);
	sb->s_dirt = 0;
}

static void lpfs_sync_super(struct super_block *sb, struct lpfs_super_block *es)
{
	es->s_wtime = get_seconds();
	mark_buffer_dirty(LPFS_SBIP(sb)->s_sbh);
	ll_rw_block(WRITE, 1, &LPFS_SBIP(sb)->s_sbh);
	wait_on_buffer(LPFS_SBIP(sb)->s_sbh);
	sb->s_dirt = 0;
}

/*
 * In the second extended file system, it is not necessary to
 * write the super block since we use a mapping of the
 * disk super block in a buffer.
 *
 * However, this function is still used to set the fs valid
 * flags to 0.  We need to set this flag to 0 since the fs
 * may have been checked while mounted and e2fsck may have
 * set s_state to LPFS_VALID_FS after some corrections.
 */

void lpfs_write_super (struct super_block * sb)
{
	struct lpfs_super_block * es;

	if (!(sb->s_flags & MS_RDONLY)) {
		es = LPFS_SBI(sb).s_es;

		if (le16_to_cpu(es->s_state) & LPFS_VALID_FS) {
			lpfs_debug ("setting valid to 0\n");
			es->s_state = cpu_to_le16(le16_to_cpu(es->s_state) &
						  ~LPFS_VALID_FS);
			es->s_mtime = cpu_to_le16(get_seconds());
			lpfs_sync_super(sb, es);
		} else
			lpfs_commit_super (sb, es);
	}
	sb->s_dirt = 0;
}

int lpfs_remount (struct super_block * sb, int * flags, char * data)
{
	struct lpfs_super_block * es;
	unsigned short resuid = LPFS_SBI(sb).s_resuid;
	unsigned short resgid = LPFS_SBI(sb).s_resgid;
	unsigned long new_mount_opt;
	unsigned long tmp;

	/*
	 * Allow the "check" option to be passed as a remount option.
	 */
	new_mount_opt = LPFS_SBI(sb).s_mount_opt;
	if (!parse_options (data,LPFS_SBIP(sb)))
		return -EINVAL;

	LPFS_SBI(sb).s_mount_opt = new_mount_opt;
	LPFS_SBI(sb).s_resuid = resuid;
	LPFS_SBI(sb).s_resgid = resgid;
	es = LPFS_SBI(sb).s_es;
	if ((*flags & MS_RDONLY) == (sb->s_flags & MS_RDONLY))
		return 0;
	if (*flags & MS_RDONLY) {
		if (le16_to_cpu(es->s_state) & LPFS_VALID_FS ||
		    !(LPFS_SBI(sb).s_mount_state & LPFS_VALID_FS))
			return 0;
		/*
		 * OK, we are remounting a valid rw partition rdonly, so set
		 * the rdonly flag and then mark the partition as valid again.
		 */
		es->s_state = cpu_to_le16(LPFS_SBI(sb).s_mount_state);
		es->s_mtime = cpu_to_le32(get_seconds());
	} else {
		int ret;
		if ((ret = LPFS_HAS_RO_COMPAT_FEATURE(sb,
					       ~LPFS_FEATURE_RO_COMPAT_SUPP))) {
			printk("LPFS-fs: %s: couldn't remount RDWR because of "
			       "unsupported optional features (%x).\n",
			       sb->s_id, ret);
			return -EROFS;
		}
		/*
		 * Mounting a RDONLY partition read-write, so reread and
		 * store the current valid flag.  (It may have been changed
		 * by e2fsck since we originally mounted the partition.)
		 */
		LPFS_SBI(sb).s_mount_state = le16_to_cpu(es->s_state);
		if (!lpfs_setup_super (sb, es, 0))
			sb->s_flags &= ~MS_RDONLY;
	}
	lpfs_sync_super(sb, es);
	return 0;
}

int lpfs_statfs (struct super_block * sb, struct kstatfs * buf)
{
	unsigned long overhead;
	int i;

	if (test_opt (sb, MINIX_DF))
		overhead = 0;
	else {
		/*
		 * Compute the overhead (FS structures)
		 */

		/*
		 * All of the blocks before first_data_block are
		 * overhead
		 */
		overhead = le32_to_cpu(LPFS_SBI(sb).s_es->s_first_data_block);

		/*
		 * Add the overhead attributed to the superblock and
		 * block group descriptors.  If the sparse superblocks
		 * feature is turned on, then not all groups have this.
		 */
		for (i = 0; i < LPFS_SBIP(sb)->s_groups_count; i++)
			overhead += lpfs_bg_has_super(sb, i) +
				lpfs_bg_num_gdb(sb, i);

		/*
		 * Every block group has an inode bitmap, a block
		 * bitmap, and an inode table.
		 */
		overhead += (LPFS_SBI(sb).s_groups_count *
			     (2 + LPFS_SBI(sb).s_itb_per_group));
	}

	buf->f_type = LPFS_SUPER_MAGIC;
	buf->f_bsize = sb->s_blocksize;
	buf->f_blocks = le32_to_cpu(LPFS_SBI(sb).s_es->s_blocks_count) - overhead;
	buf->f_bfree = lpfs_count_free_blocks (sb);
	buf->f_bavail = buf->f_bfree - le32_to_cpu(LPFS_SBI(sb).s_es->s_r_blocks_count);
	if (buf->f_bfree < le32_to_cpu(LPFS_SBI(sb).s_es->s_r_blocks_count))
		buf->f_bavail = 0;
	buf->f_files = le32_to_cpu(LPFS_SBI(sb).s_es->s_inodes_count);
	buf->f_ffree = lpfs_count_free_inodes (sb);
	buf->f_namelen = LPFS_NAME_LEN;
	return 0;
}

//i dont think we do this anymore
//static DECLARE_FSTYPE_DEV(lpfs_fs_type, "lpfs", lpfs_read_super);

static struct super_block *lpfs_get_sb(struct file_system_type *fs_type,
		int flags, const char * dev_name, void *data){
	return get_sb_bdev(fs_type, flags, dev_name, data, lpfs_read_super);
}

//we do this instead
static struct file_system_type lpfs_fs_type = {
	.owner		= THIS_MODULE,
	.name		= "lpfs",
	.get_sb		= lpfs_get_sb,
//these next two are in the 2.6 ext2, but not lpfs muhahaha
//	.kill_sb	= kill_block_super,
//	.fs_flags	= FS_REQUIRES_DEV,
};

static int __init init_lpfs_fs(void)
{
        return register_filesystem(&lpfs_fs_type);
}

static void __exit exit_lpfs_fs(void)
{
	unregister_filesystem(&lpfs_fs_type);
}

EXPORT_NO_SYMBOLS;

module_init(init_lpfs_fs)
module_exit(exit_lpfs_fs)
