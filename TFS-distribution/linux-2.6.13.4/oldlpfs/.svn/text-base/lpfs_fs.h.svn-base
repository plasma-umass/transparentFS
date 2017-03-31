/*
 *  linux/include/linux/lpfs_fs.h
 *
 * Copyright (C) 1992, 1993, 1994, 1995
 * Remy Card (card@masi.ibp.fr)
 * Laboratoire MASI - Institut Blaise Pascal
 * Universite Pierre et Marie Curie (Paris VI)
 *
 *  from
 *
 *  linux/include/linux/minix_fs.h
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 */

#ifndef _LINUX_LPFS_FS_H
#define _LINUX_LPFS_FS_H

#include <linux/types.h>
#include <linux/buffer_head.h>
#include "lpfs_fs_sb.h"
#include "lpfs_fs_i.h"


/*
 * The second extended filesystem constants/structures
 */

/*
 * Define LPFSFS_DEBUG to produce debug messages
 */
#undef LPFSFS_DEBUG

/*
 * Define LPFS_PREALLOCATE to preallocate data blocks for expanding files
 */
#define LPFS_PREALLOCATE
#define LPFS_DEFAULT_PREALLOC_BLOCKS	8

/*
 * The second extended file system version
 */
#define LPFSFS_DATE		"95/08/09"
#define LPFSFS_VERSION		"0.5b"

/*
 * Debug code
 */
#ifdef LPFSFS_DEBUG
#	define lpfs_debug(f, a...)	{ \
					printk ("LPFS-fs DEBUG (%s, %d): %s:", \
						__FILE__, __LINE__, __FUNCTION__); \
				  	printk (f, ## a); \
					}
#else
#	define lpfs_debug(f, a...)	/**/
#endif

/*
 * Special inode numbers
 */
#define	LPFS_BAD_INO		 1	/* Bad blocks inode */
#define LPFS_ROOT_INO		 2	/* Root inode */
#define LPFS_ACL_IDX_INO	 3	/* ACL inode */
#define LPFS_ACL_DATA_INO	 4	/* ACL inode */
#define LPFS_BOOT_LOADER_INO	 5	/* Boot loader inode */
#define LPFS_UNDEL_DIR_INO	 6	/* Undelete directory inode */

/* First non-reserved inode for old lpfs filesystems */
#define LPFS_GOOD_OLD_FIRST_INO	11

/*
 * The second extended file system magic number
 */
 /* JC */
 //lpfs uses a new magic number
 //FIXME
 //I have not verified that it is unique
//#define LPFS_SUPER_MAGIC	0xEF53
#define LPFS_SUPER_MAGIC	0x1234
/* /JC */

/*
 * Maximal count of links to a file
 */
#define LPFS_LINK_MAX		32000

/*
 * Macro-instructions used to manage several block sizes
 */
#define LPFS_MIN_BLOCK_SIZE		1024
#define	LPFS_MAX_BLOCK_SIZE		4096
#define LPFS_MIN_BLOCK_LOG_SIZE		  10
#ifdef __KERNEL__
# define LPFS_BLOCK_SIZE(s)		((s)->s_blocksize)
#else
# define LPFS_BLOCK_SIZE(s)		(LPFS_MIN_BLOCK_SIZE << (s)->s_log_block_size)
#endif
#define LPFS_ACLE_PER_BLOCK(s)		(LPFS_BLOCK_SIZE(s) / sizeof (struct lpfs_acl_entry))
#define	LPFS_ADDR_PER_BLOCK(s)		(LPFS_BLOCK_SIZE(s) / sizeof (__u32))
#ifdef __KERNEL__
# define LPFS_BLOCK_SIZE_BITS(s)	((s)->s_blocksize_bits)
#else
# define LPFS_BLOCK_SIZE_BITS(s)	((s)->s_log_block_size + 10)
#endif
#ifdef __KERNEL__
#define	LPFS_ADDR_PER_BLOCK_BITS(s)	(LPFS_SBI(s).s_addr_per_block_bits)
#define LPFS_INODE_SIZE(s)		(LPFS_SBI(s).s_inode_size)
#define LPFS_FIRST_INO(s)		(LPFS_SBI(s).s_first_ino)
#else
#define LPFS_INODE_SIZE(s)	(((s)->s_rev_level == LPFS_GOOD_OLD_REV) ? \
				 LPFS_GOOD_OLD_INODE_SIZE : \
				 (s)->s_inode_size)
#define LPFS_FIRST_INO(s)	(((s)->s_rev_level == LPFS_GOOD_OLD_REV) ? \
				 LPFS_GOOD_OLD_FIRST_INO : \
				 (s)->s_first_ino)
#endif

/*
 * Macro-instructions used to manage fragments
 */
#define LPFS_MIN_FRAG_SIZE		1024
#define	LPFS_MAX_FRAG_SIZE		4096
#define LPFS_MIN_FRAG_LOG_SIZE		  10
#ifdef __KERNEL__
# define LPFS_FRAG_SIZE(s)		(LPFS_SBI(s).s_frag_size)
# define LPFS_FRAGS_PER_BLOCK(s)	(LPFS_SBI(s).s_frags_per_block)
#else
# define LPFS_FRAG_SIZE(s)		(LPFS_MIN_FRAG_SIZE << (s)->s_log_frag_size)
# define LPFS_FRAGS_PER_BLOCK(s)	(LPFS_BLOCK_SIZE(s) / LPFS_FRAG_SIZE(s))
#endif

/*
 * ACL structures
 */
struct lpfs_acl_header	/* Header of Access Control Lists */
{
	__u32	aclh_size;
	__u32	aclh_file_count;
	__u32	aclh_acle_count;
	__u32	aclh_first_acle;
};

struct lpfs_acl_entry	/* Access Control List Entry */
{
	__u32	acle_size;
	__u16	acle_perms;	/* Access permissions */
	__u16	acle_type;	/* Type of entry */
	__u16	acle_tag;	/* User or group identity */
	__u16	acle_pad1;
	__u32	acle_next;	/* Pointer on next entry for the */
					/* same inode or on next free entry */
};

/*
 * Structure of a blocks group descriptor
 */
struct lpfs_group_desc
{
	__u32	bg_block_bitmap;		/* Blocks bitmap block */
	__u32	bg_inode_bitmap;		/* Inodes bitmap block */
	__u32	bg_inode_table;		/* Inodes table block */
	__u16	bg_free_blocks_count;	/* Free blocks count */
	__u16	bg_free_inodes_count;	/* Free inodes count */
	__u16	bg_used_dirs_count;	/* Directories count */
	__u16	bg_pad;
	/* JC */
	__u32	bg_lowPriority_bitmap;
	__u32	bg_dirty_bitmap;
	__u32	bg_reserved[1];
	//__u32	bg_reserved[3];	ORIGINAL
	/* /JC */
};

/*
 * Macro-instructions used to manage group descriptors
 */
#ifdef __KERNEL__
# define LPFS_BLOCKS_PER_GROUP(s)	(LPFS_SBI(s).s_blocks_per_group)
# define LPFS_DESC_PER_BLOCK(s)		(LPFS_SBI(s).s_desc_per_block)
# define LPFS_INODES_PER_GROUP(s)	(LPFS_SBI(s).s_inodes_per_group)
# define LPFS_DESC_PER_BLOCK_BITS(s)	(LPFS_SBI(s).s_desc_per_block_bits)
#else
# define LPFS_BLOCKS_PER_GROUP(s)	((s)->s_blocks_per_group)
# define LPFS_DESC_PER_BLOCK(s)		(LPFS_BLOCK_SIZE(s) / sizeof (struct lpfs_group_desc))
# define LPFS_INODES_PER_GROUP(s)	((s)->s_inodes_per_group)
#endif

/*
 * Constants relative to the data blocks
 */
#define	LPFS_NDIR_BLOCKS		12
#define	LPFS_IND_BLOCK			LPFS_NDIR_BLOCKS
#define	LPFS_DIND_BLOCK			(LPFS_IND_BLOCK + 1)
#define	LPFS_TIND_BLOCK			(LPFS_DIND_BLOCK + 1)
#define	LPFS_N_BLOCKS			(LPFS_TIND_BLOCK + 1)

/*
 * Inode flags
 */
#define	LPFS_SECRM_FL			0x00000001 /* Secure deletion */
#define	LPFS_UNRM_FL			0x00000002 /* Undelete */
#define	LPFS_COMPR_FL			0x00000004 /* Compress file */
#define LPFS_SYNC_FL			0x00000008 /* Synchronous updates */
#define LPFS_IMMUTABLE_FL		0x00000010 /* Immutable file */
#define LPFS_APPEND_FL			0x00000020 /* writes to file may only append */
#define LPFS_NODUMP_FL			0x00000040 /* do not dump file */
#define LPFS_NOATIME_FL			0x00000080 /* do not update atime */
/* Reserved for compression usage... */
#define LPFS_DIRTY_FL			0x00000100
#define LPFS_COMPRBLK_FL		0x00000200 /* One or more compressed clusters */
#define LPFS_NOCOMP_FL			0x00000400 /* Don't compress */
#define LPFS_ECOMPR_FL			0x00000800 /* Compression error */
/* End compression flags --- maybe not all used */	
#define LPFS_BTREE_FL			0x00001000 /* btree format dir */
#define LPFS_RESERVED_FL		0x80000000 /* reserved for lpfs lib */

#define LPFS_FL_USER_VISIBLE		0x00001FFF /* User visible flags */
#define LPFS_FL_USER_MODIFIABLE		0x000000FF /* User modifiable flags */

/*
 * ioctl commands
 */
#define	LPFS_IOC_GETFLAGS		_IOR('f', 1, long)
#define	LPFS_IOC_SETFLAGS		_IOW('f', 2, long)
#define	LPFS_IOC_GETVERSION		_IOR('v', 1, long)
#define	LPFS_IOC_SETVERSION		_IOW('v', 2, long)
/* JC ioctl number definitions for set and get priority */
#define LPFS_IOC_GETLOWPRIORITY 	_IOR('p',1,long)
#define LPFS_IOC_SETLOWPRIORITY 	_IOW('p',2,long)
/* /JC */

/*
 * Structure of an inode on the disk
 */
struct lpfs_inode {
	__u16	i_mode;		/* File mode */
	__u16	i_uid;		/* Low 16 bits of Owner Uid */
	__u32	i_size;		/* Size in bytes */
	__u32	i_atime;	/* Access time */
	__u32	i_ctime;	/* Creation time */
	__u32	i_mtime;	/* Modification time */
	__u32	i_dtime;	/* Deletion Time */
	__u16	i_gid;		/* Low 16 bits of Group Id */
	__u16	i_links_count;	/* Links count */
	__u32	i_blocks;	/* Blocks count */
	__u32	i_flags;	/* File flags */
	/* JC */
	//FIXME
	//see fixme tag in lpfs_fs_i.h
	//also, I never changed the reserved space count
	//so an lpfs inode is bigger than an ext2 inode
	//again, for compatibility, i'm leaving it that way
	//for now
	__u16	i_priority;	/* File storage priority */
	/* /JC */
	union {
		struct {
			__u32  l_i_reserved1;
		} linux1;
		struct {
			__u32  h_i_translator;
		} hurd1;
		struct {
			__u32  m_i_reserved1;
		} masix1;
	} osd1;				/* OS dependent 1 */
	__u32	i_block[LPFS_N_BLOCKS];/* Pointers to blocks */
	__u32	i_generation;	/* File version (for NFS) */
	__u32	i_file_acl;	/* File ACL */
	__u32	i_dir_acl;	/* Directory ACL */
	__u32	i_faddr;	/* Fragment address */
	union {
		struct {
			__u8	l_i_frag;	/* Fragment number */
			__u8	l_i_fsize;	/* Fragment size */
			__u16	i_pad1;
			__u16	l_i_uid_high;	/* these 2 fields    */
			__u16	l_i_gid_high;	/* were reserved2[0] */
			__u32	l_i_reserved2;
		} linux2;
		struct {
			__u8	h_i_frag;	/* Fragment number */
			__u8	h_i_fsize;	/* Fragment size */
			__u16	h_i_mode_high;
			__u16	h_i_uid_high;
			__u16	h_i_gid_high;
			__u32	h_i_author;
		} hurd2;
		struct {
			__u8	m_i_frag;	/* Fragment number */
			__u8	m_i_fsize;	/* Fragment size */
			__u16	m_pad1;
			__u32	m_i_reserved2[2];
		} masix2;
	} osd2;				/* OS dependent 2 */
};

#define i_size_high	i_dir_acl

#if defined(__KERNEL__) || defined(__linux__)
#define i_reserved1	osd1.linux1.l_i_reserved1
#define i_frag		osd2.linux2.l_i_frag
#define i_fsize		osd2.linux2.l_i_fsize
#define i_uid_low	i_uid
#define i_gid_low	i_gid
#define i_uid_high	osd2.linux2.l_i_uid_high
#define i_gid_high	osd2.linux2.l_i_gid_high
#define i_reserved2	osd2.linux2.l_i_reserved2
#endif

#ifdef	__hurd__
#define i_translator	osd1.hurd1.h_i_translator
#define i_frag		osd2.hurd2.h_i_frag;
#define i_fsize		osd2.hurd2.h_i_fsize;
#define i_uid_high	osd2.hurd2.h_i_uid_high
#define i_gid_high	osd2.hurd2.h_i_gid_high
#define i_author	osd2.hurd2.h_i_author
#endif

#ifdef	__masix__
#define i_reserved1	osd1.masix1.m_i_reserved1
#define i_frag		osd2.masix2.m_i_frag
#define i_fsize		osd2.masix2.m_i_fsize
#define i_reserved2	osd2.masix2.m_i_reserved2
#endif

/*
 * File system states
 */
#define	LPFS_VALID_FS			0x0001	/* Unmounted cleanly */
#define	LPFS_ERROR_FS			0x0002	/* Errors detected */

/*
 * Mount flags
 */
#define LPFS_MOUNT_CHECK		0x0001	/* Do mount-time checks */
#define LPFS_MOUNT_GRPID		0x0004	/* Create files with directory's group */
#define LPFS_MOUNT_DEBUG		0x0008	/* Some debugging messages */
#define LPFS_MOUNT_ERRORS_CONT		0x0010	/* Continue on errors */
#define LPFS_MOUNT_ERRORS_RO		0x0020	/* Remount fs ro on errors */
#define LPFS_MOUNT_ERRORS_PANIC		0x0040	/* Panic on errors */
#define LPFS_MOUNT_MINIX_DF		0x0080	/* Mimics the Minix statfs */
#define LPFS_MOUNT_NO_UID32		0x0200  /* Disable 32-bit UIDs */

#define clear_opt(o, opt)		o &= ~LPFS_MOUNT_##opt
#define set_opt(o, opt)			o |= LPFS_MOUNT_##opt
#define test_opt(sb, opt)		(LPFS_SBI(sb).s_mount_opt & \
					 LPFS_MOUNT_##opt)
/*
 * Maximal mount counts between two filesystem checks
 */
#define LPFS_DFL_MAX_MNT_COUNT		20	/* Allow 20 mounts */
#define LPFS_DFL_CHECKINTERVAL		0	/* Don't use interval check */

/*
 * Behaviour when detecting errors
 */
#define LPFS_ERRORS_CONTINUE		1	/* Continue execution */
#define LPFS_ERRORS_RO			2	/* Remount fs read-only */
#define LPFS_ERRORS_PANIC		3	/* Panic */
#define LPFS_ERRORS_DEFAULT		LPFS_ERRORS_CONTINUE

/*
 * Structure of the super block
 */
struct lpfs_super_block {
	__u32	s_inodes_count;		/* Inodes count */
	__u32	s_blocks_count;		/* Blocks count */
	__u32	s_r_blocks_count;	/* Reserved blocks count */
	__u32	s_free_blocks_count;	/* Free blocks count */
	__u32	s_free_inodes_count;	/* Free inodes count */
	__u32	s_first_data_block;	/* First Data Block */
	__u32	s_log_block_size;	/* Block size */
	__s32	s_log_frag_size;	/* Fragment size */
	__u32	s_blocks_per_group;	/* # Blocks per group */
	__u32	s_frags_per_group;	/* # Fragments per group */
	__u32	s_inodes_per_group;	/* # Inodes per group */
	__u32	s_mtime;		/* Mount time */
	__u32	s_wtime;		/* Write time */
	__u16	s_mnt_count;		/* Mount count */
	__s16	s_max_mnt_count;	/* Maximal mount count */
	__u16	s_magic;		/* Magic signature */
	__u16	s_state;		/* File system state */
	__u16	s_errors;		/* Behaviour when detecting errors */
	__u16	s_minor_rev_level; 	/* minor revision level */
	__u32	s_lastcheck;		/* time of last check */
	__u32	s_checkinterval;	/* max. time between checks */
	__u32	s_creator_os;		/* OS */
	__u32	s_rev_level;		/* Revision level */
	__u16	s_def_resuid;		/* Default uid for reserved blocks */
	__u16	s_def_resgid;		/* Default gid for reserved blocks */
	/*
	 * These fields are for LPFS_DYNAMIC_REV superblocks only.
	 *
	 * Note: the difference between the compatible feature set and
	 * the incompatible feature set is that if there is a bit set
	 * in the incompatible feature set that the kernel doesn't
	 * know about, it should refuse to mount the filesystem.
	 * 
	 * e2fsck's requirements are more strict; if it doesn't know
	 * about a feature in either the compatible or incompatible
	 * feature set, it must abort and not try to meddle with
	 * things it doesn't understand...
	 */
	__u32	s_first_ino; 		/* First non-reserved inode */
	__u16   s_inode_size; 		/* size of inode structure */
	__u16	s_block_group_nr; 	/* block group # of this superblock */
	__u32	s_feature_compat; 	/* compatible feature set */
	__u32	s_feature_incompat; 	/* incompatible feature set */
	__u32	s_feature_ro_compat; 	/* readonly-compatible feature set */
	__u8	s_uuid[16];		/* 128-bit uuid for volume */
	char	s_volume_name[16]; 	/* volume name */
	char	s_last_mounted[64]; 	/* directory where last mounted */
	__u32	s_algorithm_usage_bitmap; /* For compression */
	/*
	 * Performance hints.  Directory preallocation should only
	 * happen if the LPFS_COMPAT_PREALLOC flag is on.
	 */
	__u8	s_prealloc_blocks;	/* Nr of blocks to try to preallocate*/
	__u8	s_prealloc_dir_blocks;	/* Nr to preallocate for dirs */
	__u16	s_padding1;
	/*
	 * Journaling support valid if EXT3_FEATURE_COMPAT_HAS_JOURNAL set.
	 */
	__u8	s_journal_uuid[16];	/* uuid of journal superblock */
	__u32	s_journal_inum;		/* inode number of journal file */
	__u32	s_journal_dev;		/* device number of journal file */
	__u32	s_last_orphan;		/* start of list of inodes to delete */
	__u32	s_hash_seed[4];		/* HTREE hash seed */
	__u8	s_def_hash_version;	/* Default hash version to use */
	__u8	s_reserved_char_pad;
	__u16	s_reserved_word_pad;
	__u32	s_default_mount_opts;
 	__u32	s_first_meta_bg; 	/* First metablock block group */
	__u32	s_reserved[190];	/* Padding to the end of the block */
};

#ifdef __KERNEL__
//#define LPFS_SB(sb)	(&((sb)->u.lpfs_sb))
#define LPFS_SBP(sb)	(    (struct lpfs_super_block *)((LPFS_SBIP(sb)->s_es))   )
#define LPFS_SB(sb)	( *(LPFS_SBP(sb)))
#else
/* Assume that user mode programs are passing in an lpfsfs superblock, not
 * a kernel struct super_block.  This will allow us to call the feature-test
 * macros from user land. */
#define LPFS_SB(sb)	(sb)
#endif

/*
 * Codes for operating systems
 */
#define LPFS_OS_LINUX		0
#define LPFS_OS_HURD		1
#define LPFS_OS_MASIX		2
#define LPFS_OS_FREEBSD		3
#define LPFS_OS_LITES		4

/*
 * Revision levels
 */
#define LPFS_GOOD_OLD_REV	0	/* The good old (original) format */
#define LPFS_DYNAMIC_REV	1 	/* V2 format w/ dynamic inode sizes */

#define LPFS_CURRENT_REV	LPFS_GOOD_OLD_REV
#define LPFS_MAX_SUPP_REV	LPFS_DYNAMIC_REV

#define LPFS_GOOD_OLD_INODE_SIZE 128

/*
 * Feature set definitions
 */

#define LPFS_HAS_COMPAT_FEATURE(sb,mask)			\
	( LPFS_SBI(sb).s_es->s_feature_compat & cpu_to_le32(mask) )
#define LPFS_HAS_RO_COMPAT_FEATURE(sb,mask)			\
	( LPFS_SBI(sb).s_es->s_feature_ro_compat & cpu_to_le32(mask) )
#define LPFS_HAS_INCOMPAT_FEATURE(sb,mask)			\
	( LPFS_SBI(sb).s_es->s_feature_incompat & cpu_to_le32(mask) )
#define LPFS_SET_COMPAT_FEATURE(sb,mask)			\
	LPFS_SBIP(sb)->s_es->s_feature_compat |= cpu_to_le32(mask)
#define LPFS_SET_RO_COMPAT_FEATURE(sb,mask)			\
	LPFS_SBIP(sb)->s_es->s_feature_ro_compat |= cpu_to_le32(mask)
#define LPFS_SET_INCOMPAT_FEATURE(sb,mask)			\
	LPFS_SBIP(sb)->s_es->s_feature_incompat |= cpu_to_le32(mask)
#define LPFS_CLEAR_COMPAT_FEATURE(sb,mask)			\
	LPFS_SBIP(sb)->s_es->s_feature_compat &= ~cpu_to_le32(mask)
#define LPFS_CLEAR_RO_COMPAT_FEATURE(sb,mask)			\
	LPFS_SBIP(sb)->s_es->s_feature_ro_compat &= ~cpu_to_le32(mask)
#define LPFS_CLEAR_INCOMPAT_FEATURE(sb,mask)			\
	LPFS_SBIP(sb)->s_es->s_feature_incompat &= ~cpu_to_le32(mask)

#define LPFS_FEATURE_COMPAT_DIR_PREALLOC	0x0001
#define LPFS_FEATURE_COMPAT_IMAGIC_INODES	0x0002
#define EXT3_FEATURE_COMPAT_HAS_JOURNAL		0x0004
#define LPFS_FEATURE_COMPAT_EXT_ATTR		0x0008
#define LPFS_FEATURE_COMPAT_RESIZE_INO		0x0010
#define LPFS_FEATURE_COMPAT_DIR_INDEX		0x0020
#define LPFS_FEATURE_COMPAT_ANY			0xffffffff

#define LPFS_FEATURE_RO_COMPAT_SPARSE_SUPER	0x0001
#define LPFS_FEATURE_RO_COMPAT_LARGE_FILE	0x0002
#define LPFS_FEATURE_RO_COMPAT_BTREE_DIR	0x0004
#define LPFS_FEATURE_RO_COMPAT_ANY		0xffffffff

#define LPFS_FEATURE_INCOMPAT_COMPRESSION	0x0001
#define LPFS_FEATURE_INCOMPAT_FILETYPE		0x0002
#define EXT3_FEATURE_INCOMPAT_RECOVER		0x0004
#define EXT3_FEATURE_INCOMPAT_JOURNAL_DEV	0x0008
#define LPFS_FEATURE_INCOMPAT_META_BG		0x0010
#define LPFS_FEATURE_INCOMPAT_ANY		0xffffffff

#define LPFS_FEATURE_COMPAT_SUPP	0
#define LPFS_FEATURE_INCOMPAT_SUPP	(LPFS_FEATURE_INCOMPAT_FILETYPE| \
					 LPFS_FEATURE_INCOMPAT_META_BG)
#define LPFS_FEATURE_RO_COMPAT_SUPP	(LPFS_FEATURE_RO_COMPAT_SPARSE_SUPER| \
					 LPFS_FEATURE_RO_COMPAT_LARGE_FILE| \
					 LPFS_FEATURE_RO_COMPAT_BTREE_DIR)
#define LPFS_FEATURE_RO_COMPAT_UNSUPPORTED	~LPFS_FEATURE_RO_COMPAT_SUPP
#define LPFS_FEATURE_INCOMPAT_UNSUPPORTED	~LPFS_FEATURE_INCOMPAT_SUPP

/*
 * Default mount options
 */
#define LPFS_DEFM_DEBUG		0x0001
#define LPFS_DEFM_BSDGROUPS	0x0002
#define LPFS_DEFM_XATTR_USER	0x0004
#define LPFS_DEFM_ACL		0x0008
#define LPFS_DEFM_UID16		0x0010
    /* Not used by lpfs, but reserved for use by ext3 */
#define EXT3_DEFM_JMODE		0x0060 
#define EXT3_DEFM_JMODE_DATA	0x0020
#define EXT3_DEFM_JMODE_ORDERED	0x0040
#define EXT3_DEFM_JMODE_WBACK	0x0060

#define        LPFS_DEF_RESUID         0
#define        LPFS_DEF_RESGID         0

/*
 * Structure of a directory entry
 */
#define LPFS_NAME_LEN 255

struct lpfs_dir_entry {
	__u32	inode;			/* Inode number */
	__u16	rec_len;		/* Directory entry length */
	__u16	name_len;		/* Name length */
	char	name[LPFS_NAME_LEN];	/* File name */
};

/*
 * The new version of the directory entry.  Since LPFS structures are
 * stored in intel byte order, and the name_len field could never be
 * bigger than 255 chars, it's safe to reclaim the extra byte for the
 * file_type field.
 */
struct lpfs_dir_entry_2 {
	__u32	inode;			/* Inode number */
	__u16	rec_len;		/* Directory entry length */
	__u8	name_len;		/* Name length */
	__u8	file_type;
	char	name[LPFS_NAME_LEN];	/* File name */
};

/*
 * Ext2 directory file types.  Only the low 3 bits are used.  The
 * other bits are reserved for now.
 */
enum {
	LPFS_FT_UNKNOWN,
	LPFS_FT_REG_FILE,
	LPFS_FT_DIR,
	LPFS_FT_CHRDEV,
	LPFS_FT_BLKDEV,
	LPFS_FT_FIFO,
	LPFS_FT_SOCK,
	LPFS_FT_SYMLINK,
	LPFS_FT_MAX
};

/*
 * LPFS_DIR_PAD defines the directory entries boundaries
 *
 * NOTE: It must be a multiple of 4
 */
#define LPFS_DIR_PAD		 	4
#define LPFS_DIR_ROUND 			(LPFS_DIR_PAD - 1)
#define LPFS_DIR_REC_LEN(name_len)	(((name_len) + 8 + LPFS_DIR_ROUND) & \
					 ~LPFS_DIR_ROUND)

#ifdef __KERNEL__
/*
 * Function prototypes
 */

/*
 * Ok, these declarations are also in <linux/kernel.h> but none of the
 * lpfs source programs needs to include it so they are duplicated here.
 */
# define NORET_TYPE    /**/
# define ATTRIB_NORET  __attribute__((noreturn))
# define NORET_AND     noreturn,

/* balloc.c */
extern int lpfs_bg_has_super(struct super_block *sb, int group);
extern unsigned long lpfs_bg_num_gdb(struct super_block *sb, int group);
extern int lpfs_new_block (struct inode *, unsigned long,
			   __u32 *, __u32 *, int *, int priority);
extern void lpfs_free_blocks (struct inode *, unsigned long,
			      unsigned long);
extern unsigned long lpfs_count_free_blocks (struct super_block *);
extern void lpfs_check_blocks_bitmap (struct super_block *);
extern struct lpfs_group_desc * lpfs_get_group_desc(struct super_block * sb,
						    unsigned int block_group,
						    struct buffer_head ** bh);
/* JC lock and unlock block headers */
extern int lpfs_lockBlock(struct super_block *sb, int goal);
extern int lpfs_unlockBlock(struct super_block *sb, int goal);
extern int lpfs_isBlockStillMine(struct inode *inode, int diskblocknum);
/* /JC */

/* dir.c */
extern int lpfs_add_link (struct dentry *, struct inode *);
extern ino_t lpfs_inode_by_name(struct inode *, struct dentry *);
extern int lpfs_make_empty(struct inode *, struct inode *);
extern struct lpfs_dir_entry_2 * lpfs_find_entry (struct inode *,struct dentry *, struct page **);
extern int lpfs_delete_entry (struct lpfs_dir_entry_2 *, struct page *);
extern int lpfs_empty_dir (struct inode *);
extern struct lpfs_dir_entry_2 * lpfs_dotdot (struct inode *, struct page **);
extern void lpfs_set_link(struct inode *, struct lpfs_dir_entry_2 *, struct page *, struct inode *);

/* fsync.c */
extern int lpfs_sync_file (struct file *, struct dentry *, int);
extern int lpfs_fsync_inode (struct inode *, int);

/* ialloc.c */
extern struct inode * lpfs_new_inode (const struct inode *, int);
extern void lpfs_free_inode (struct inode *);
extern unsigned long lpfs_count_free_inodes (struct super_block *);
extern void lpfs_check_inodes_bitmap (struct super_block *);
extern unsigned long lpfs_count_free (struct buffer_head *, unsigned);

/* inode.c */
extern void lpfs_read_inode (struct inode *);
extern int lpfs_write_inode (struct inode *, int);
extern void lpfs_put_inode (struct inode *);
extern void lpfs_delete_inode (struct inode *);
extern int lpfs_sync_inode (struct inode *);
extern void lpfs_discard_prealloc (struct inode *);
extern void lpfs_truncate (struct inode *);
extern void lpfs_set_inode_flags(struct inode *inode);

/* ioctl.c */
extern int lpfs_ioctl (struct inode *, struct file *, unsigned int,
		       unsigned long);

//we're going to need this
struct statfs;

/* super.c */
extern void lpfs_error (struct super_block *, const char *, const char *, ...)
	__attribute__ ((format (printf, 3, 4)));
extern NORET_TYPE void lpfs_panic (struct super_block *, const char *,
				   const char *, ...)
	__attribute__ ((NORET_AND format (printf, 3, 4)));
extern void lpfs_warning (struct super_block *, const char *, const char *, ...)
	__attribute__ ((format (printf, 3, 4)));
extern void lpfs_update_dynamic_rev (struct super_block *sb);
extern void lpfs_put_super (struct super_block *);
extern void lpfs_write_super (struct super_block *);
extern int lpfs_remount (struct super_block *, int *, char *);
extern int lpfs_read_super (struct super_block *,void *,int);
extern int lpfs_statfs (struct super_block *, struct kstatfs *);

/*
 * Inodes and files operations
 */

/* dir.c */
extern struct file_operations lpfs_dir_operations;

/* file.c */
extern struct inode_operations lpfs_file_inode_operations;
extern struct file_operations lpfs_file_operations;
/* JC file_open declaration */
int lpfs_file_open(struct inode *inode, struct file *filp);
ssize_t lpfs_file_read(struct file * filp, char * buf, size_t count, loff_t *ppos);
ssize_t lpfs_file_write(struct file *filp,const char *buf,size_t count, loff_t *ppos);
/* /JC */

/* inode.c */
extern struct address_space_operations lpfs_aops;
/* JC declaration of lpfs_step_blocks */
extern int lpfs_step_blocks(struct inode *inode, int (*func)(struct inode *, int, int));
/* /JC */
/* namei.c */
extern struct inode_operations lpfs_dir_inode_operations;

/* symlink.c */
extern struct inode_operations lpfs_fast_symlink_inode_operations;

/* JC */
/* FIXME */
//This is pretty broken
#define lpfs_test_bit			ext2_test_bit
#define lpfs_set_bit			ext2_set_bit
#define lpfs_clear_bit			ext2_clear_bit
#define lpfs_find_next_zero_bit		ext2_find_next_zero_bit
#define lpfs_find_first_zero_bit	ext2_find_first_zero_bit
/* /JC */

/* JC */

#define LPFS_BENCHMARK
#define LPFS_SAMPLE_FREQUENCY 50

#ifdef LPFS_BENCHMARK

#define LPFS_IOC_GETSTATS _IOR('E',1,long)
#define LPFS_IOC_RESETSTATS _IO('E',2)

extern __u64 lpfs_hpballoc_time_total;
extern __u64 lpfs_hpballoc_time_count;
extern __u64 lpfs_hpialloc_time_total;
extern __u64 lpfs_hpialloc_time_count;
extern __u64 lpfs_hpread_time_total;
extern __u64 lpfs_hpread_time_count;
extern __u64 lpfs_hpwrite_time_total;
extern __u64 lpfs_hpwrite_time_count;



extern __u64 lpfs_lpballoc_time_total;
extern __u64 lpfs_lpballoc_time_count;
extern __u64 lpfs_lpialloc_time_total;
extern __u64 lpfs_lpialloc_time_count;
extern __u64 lpfs_lpread_time_total;
extern __u64 lpfs_lpread_time_count;
extern __u64 lpfs_lpwrite_time_total;
extern __u64 lpfs_lpwrite_time_count;

#endif
/* /JC */


/* bitmap.c */
extern int lpfs_find_zero_byte(int count, char ** bitmaps, int start, int end);
extern int lpfs_p_find_first_zero_byte(char *highp, char *lowp, char *dirty, char *blocklock, int start, 
				int length, int priority);
extern int lpfs_p_set_bit(int j, char *highp, char *lowp, char *dirty, char *blocklock, int priority);
extern int lpfs_p_test_bit(int j, char *highp, char *lowp, char *dirty, char *blocklock, int priority);
extern int lpfs_p_find_next_zero_bit(char *highp, char *lowp, char *dirty, char *blocklock,
				int end, int start, int priority);

#endif	/* __KERNEL__ */

#endif	/* _LINUX_LPFS_FS_H */
