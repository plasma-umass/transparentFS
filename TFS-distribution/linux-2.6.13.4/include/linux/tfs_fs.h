/*
 *  linux/include/linux/tfs_fs.h
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

#ifndef _LINUX_TFS_FS_H
#define _LINUX_TFS_FS_H

#include <linux/types.h>
#include <linux/tfs_fs_sb.h>

/*
 * The second extended filesystem constants/structures
 */

/*
 * Define TFSFS_DEBUG to produce debug messages
 */
#undef TFSFS_DEBUG

/*
 * Define TFS_PREALLOCATE to preallocate data blocks for expanding files
 */
#define TFS_PREALLOCATE
#define TFS_DEFAULT_PREALLOC_BLOCKS	8

/*
 * The second extended file system version
 */
#define TFSFS_DATE		"95/08/09"
#define TFSFS_VERSION		"0.5b"

/*
 * Debug code
 */
#ifdef TFSFS_DEBUG
#	define tfs_debug(f, a...)	{ \
					printk ("TFS-fs DEBUG (%s, %d): %s:", \
						__FILE__, __LINE__, __FUNCTION__); \
				  	printk (f, ## a); \
					}
#else
#	define tfs_debug(f, a...)	/**/
#endif

/*
 * Special inode numbers
 */
#define	TFS_BAD_INO		 1	/* Bad blocks inode */
#define TFS_ROOT_INO		 2	/* Root inode */
#define TFS_BOOT_LOADER_INO	 5	/* Boot loader inode */
#define TFS_UNDEL_DIR_INO	 6	/* Undelete directory inode */

/* First non-reserved inode for old tfs filesystems */
#define TFS_GOOD_OLD_FIRST_INO	11

/*
 * The second extended file system magic number
 */
//#define TFS_SUPER_MAGIC	0xEF53
//JC - the TFS magic number
#define TFS_SUPER_MAGIC	0X1234

#ifdef __KERNEL__
static inline struct tfs_sb_info *TFS_SB(struct super_block *sb)
{
	return sb->s_fs_info;
}
#else
/* Assume that user mode programs are passing in an tfsfs superblock, not
 * a kernel struct super_block.  This will allow us to call the feature-test
 * macros from user land. */
#define TFS_SB(sb)	(sb)
#endif

/*
 * Maximal count of links to a file
 */
#define TFS_LINK_MAX		32000

/*
 * Macro-instructions used to manage several block sizes
 */
#define TFS_MIN_BLOCK_SIZE		1024
#define	TFS_MAX_BLOCK_SIZE		4096
#define TFS_MIN_BLOCK_LOG_SIZE		  10
#ifdef __KERNEL__
# define TFS_BLOCK_SIZE(s)		((s)->s_blocksize)
#else
# define TFS_BLOCK_SIZE(s)		(TFS_MIN_BLOCK_SIZE << (s)->s_log_block_size)
#endif
#define	TFS_ADDR_PER_BLOCK(s)		(TFS_BLOCK_SIZE(s) / sizeof (__u32))
#ifdef __KERNEL__
# define TFS_BLOCK_SIZE_BITS(s)	((s)->s_blocksize_bits)
#else
# define TFS_BLOCK_SIZE_BITS(s)	((s)->s_log_block_size + 10)
#endif
#ifdef __KERNEL__
#define	TFS_ADDR_PER_BLOCK_BITS(s)	(TFS_SB(s)->s_addr_per_block_bits)
#define TFS_INODE_SIZE(s)		(TFS_SB(s)->s_inode_size)
#define TFS_FIRST_INO(s)		(TFS_SB(s)->s_first_ino)
#else
#define TFS_INODE_SIZE(s)	(((s)->s_rev_level == TFS_GOOD_OLD_REV) ? \
				 TFS_GOOD_OLD_INODE_SIZE : \
				 (s)->s_inode_size)
#define TFS_FIRST_INO(s)	(((s)->s_rev_level == TFS_GOOD_OLD_REV) ? \
				 TFS_GOOD_OLD_FIRST_INO : \
				 (s)->s_first_ino)
#endif

/*
 * Macro-instructions used to manage fragments
 */
#define TFS_MIN_FRAG_SIZE		1024
#define	TFS_MAX_FRAG_SIZE		4096
#define TFS_MIN_FRAG_LOG_SIZE		  10
#ifdef __KERNEL__
# define TFS_FRAG_SIZE(s)		(TFS_SB(s)->s_frag_size)
# define TFS_FRAGS_PER_BLOCK(s)	(TFS_SB(s)->s_frags_per_block)
#else
# define TFS_FRAG_SIZE(s)		(TFS_MIN_FRAG_SIZE << (s)->s_log_frag_size)
# define TFS_FRAGS_PER_BLOCK(s)	(TFS_BLOCK_SIZE(s) / TFS_FRAG_SIZE(s))
#endif

/*
 * Structure of a blocks group descriptor
 */
struct tfs_group_desc
{
	__le32	bg_block_bitmap;		/* Blocks bitmap block */
	__le32	bg_inode_bitmap;		/* Inodes bitmap block */
	__le32	bg_inode_table;		/* Inodes table block */
	__le16	bg_free_blocks_count;	/* Free blocks count */
	__le16	bg_free_inodes_count;	/* Free inodes count */
	__le16	bg_used_dirs_count;	/* Directories count */
	__le16	bg_pad;
	/* JC */
	__le32  bg_lowPriority_bitmap;
	__le32  bg_dirty_bitmap;
	__le32	bg_trans_blocks;
	//__le32	bg_reserved[1];
	/* /JC */
};

/*
 * Macro-instructions used to manage group descriptors
 */
#ifdef __KERNEL__
# define TFS_BLOCKS_PER_GROUP(s)	(TFS_SB(s)->s_blocks_per_group)
# define TFS_DESC_PER_BLOCK(s)		(TFS_SB(s)->s_desc_per_block)
# define TFS_INODES_PER_GROUP(s)	(TFS_SB(s)->s_inodes_per_group)
# define TFS_DESC_PER_BLOCK_BITS(s)	(TFS_SB(s)->s_desc_per_block_bits)
#else
# define TFS_BLOCKS_PER_GROUP(s)	((s)->s_blocks_per_group)
# define TFS_DESC_PER_BLOCK(s)		(TFS_BLOCK_SIZE(s) / sizeof (struct tfs_group_desc))
# define TFS_INODES_PER_GROUP(s)	((s)->s_inodes_per_group)
#endif

/*
 * Constants relative to the data blocks
 */
#define	TFS_NDIR_BLOCKS		12
#define	TFS_IND_BLOCK			TFS_NDIR_BLOCKS
#define	TFS_DIND_BLOCK			(TFS_IND_BLOCK + 1)
#define	TFS_TIND_BLOCK			(TFS_DIND_BLOCK + 1)
#define	TFS_N_BLOCKS			(TFS_TIND_BLOCK + 1)

/*
 * Inode flags
 */
#define	TFS_SECRM_FL			0x00000001 /* Secure deletion */
#define	TFS_UNRM_FL			0x00000002 /* Undelete */
#define	TFS_COMPR_FL			0x00000004 /* Compress file */
#define TFS_SYNC_FL			0x00000008 /* Synchronous updates */
#define TFS_IMMUTABLE_FL		0x00000010 /* Immutable file */
#define TFS_APPEND_FL			0x00000020 /* writes to file may only append */
#define TFS_NODUMP_FL			0x00000040 /* do not dump file */
#define TFS_NOATIME_FL			0x00000080 /* do not update atime */
/* Reserved for compression usage... */
#define TFS_DIRTY_FL			0x00000100
#define TFS_COMPRBLK_FL		0x00000200 /* One or more compressed clusters */
#define TFS_NOCOMP_FL			0x00000400 /* Don't compress */
#define TFS_ECOMPR_FL			0x00000800 /* Compression error */
/* End compression flags --- maybe not all used */	
#define TFS_BTREE_FL			0x00001000 /* btree format dir */
#define TFS_INDEX_FL			0x00001000 /* hash-indexed directory */
#define TFS_IMAGIC_FL			0x00002000 /* AFS directory */
#define TFS_JOURNAL_DATA_FL		0x00004000 /* Reserved for ext3 */
#define TFS_NOTAIL_FL			0x00008000 /* file tail should not be merged */
#define TFS_DIRSYNC_FL			0x00010000 /* dirsync behaviour (directories only) */
#define TFS_TOPDIR_FL			0x00020000 /* Top of directory hierarchies*/
#define TFS_RESERVED_FL		0x80000000 /* reserved for tfs lib */

#define TFS_FL_USER_VISIBLE		0x0003DFFF /* User visible flags */
#define TFS_FL_USER_MODIFIABLE		0x000380FF /* User modifiable flags */

/*
 * ioctl commands
 */
#define	TFS_IOC_GETFLAGS		_IOR('f', 1, long)
#define	TFS_IOC_SETFLAGS		_IOW('f', 2, long)
#define	TFS_IOC_GETVERSION		_IOR('v', 1, long)
#define	TFS_IOC_SETVERSION		_IOW('v', 2, long)
//JC - ioctl numbers for get and set priority
#define TFS_IOC_GETTRANSPARENCY		_IOR('p',1,long)
#define TFS_IOC_SETTRANSPARENCY		_IOW('p',2,long)
#define TFS_IOC_GETALLOCLAYOUT		_IOW('q',2,long)

/*
 * Structure of an inode on the disk
 */
struct tfs_inode {
	__le16	i_mode;		/* File mode */
	__le16	i_uid;		/* Low 16 bits of Owner Uid */
	__le32	i_size;		/* Size in bytes */
	__le32	i_atime;	/* Access time */
	__le32	i_ctime;	/* Creation time */
	__le32	i_mtime;	/* Modification time */
	__le32	i_dtime;	/* Deletion Time */
	__le16	i_gid;		/* Low 16 bits of Group Id */
	__le16	i_links_count;	/* Links count */
	__le32	i_blocks;	/* Blocks count */
	__le32	i_flags;	/* File flags */
	__le16	i_priority;	//JC
	//FIXME - TFS inode structure
	/*
		I never changed the reserved space
		count, so a tfs inode is bigger than
		an ext2 inode.  I am leaving it this
		way for compatability with the old
		lpfs implementation.  It seems like
		tfs should just be a feature of ext2
		that can be enabled/disabled, rather than
		a separate filesystem, if this is at all
		possible.
	*/
	union {
		struct {
			__le32  l_i_reserved1;
		} linux1;
		struct {
			__le32  h_i_translator;
		} hurd1;
		struct {
			__le32  m_i_reserved1;
		} masix1;
	} osd1;				/* OS dependent 1 */
	__le32	i_block[TFS_N_BLOCKS];/* Pointers to blocks */
	__le32	i_generation;	/* File version (for NFS) */
	__le32	i_file_acl;	/* File ACL */
	__le32	i_dir_acl;	/* Directory ACL */
	__le32	i_faddr;	/* Fragment address */
	union {
		struct {
			__u8	l_i_frag;	/* Fragment number */
			__u8	l_i_fsize;	/* Fragment size */
			__u16	i_pad1;
			__le16	l_i_uid_high;	/* these 2 fields    */
			__le16	l_i_gid_high;	/* were reserved2[0] */
			__u32	l_i_reserved2;
		} linux2;
		struct {
			__u8	h_i_frag;	/* Fragment number */
			__u8	h_i_fsize;	/* Fragment size */
			__le16	h_i_mode_high;
			__le16	h_i_uid_high;
			__le16	h_i_gid_high;
			__le32	h_i_author;
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
#define	TFS_VALID_FS			0x0001	/* Unmounted cleanly */
#define	TFS_ERROR_FS			0x0002	/* Errors detected */

/*
 * Mount flags
 */
#define TFS_MOUNT_CHECK		0x000001  /* Do mount-time checks */
#define TFS_MOUNT_OLDALLOC		0x000002  /* Don't use the new Orlov allocator */
#define TFS_MOUNT_GRPID		0x000004  /* Create files with directory's group */
#define TFS_MOUNT_DEBUG		0x000008  /* Some debugging messages */
#define TFS_MOUNT_ERRORS_CONT		0x000010  /* Continue on errors */
#define TFS_MOUNT_ERRORS_RO		0x000020  /* Remount fs ro on errors */
#define TFS_MOUNT_ERRORS_PANIC		0x000040  /* Panic on errors */
#define TFS_MOUNT_MINIX_DF		0x000080  /* Mimics the Minix statfs */
#define TFS_MOUNT_NOBH			0x000100  /* No buffer_heads */
#define TFS_MOUNT_NO_UID32		0x000200  /* Disable 32-bit UIDs */
#define TFS_MOUNT_XATTR_USER		0x004000  /* Extended user attributes */
#define TFS_MOUNT_POSIX_ACL		0x008000  /* POSIX Access Control Lists */
#define TFS_MOUNT_XIP			0x010000  /* Execute in place */
#define TFS_MOUNT_USRQUOTA		0x020000 /* user quota */
#define TFS_MOUNT_GRPQUOTA		0x040000 /* group quota */


#define clear_opt(o, opt)		o &= ~TFS_MOUNT_##opt
#define set_opt(o, opt)			o |= TFS_MOUNT_##opt
#define test_opt(sb, opt)		(TFS_SB(sb)->s_mount_opt & \
					 TFS_MOUNT_##opt)
/*
 * Maximal mount counts between two filesystem checks
 */
#define TFS_DFL_MAX_MNT_COUNT		20	/* Allow 20 mounts */
#define TFS_DFL_CHECKINTERVAL		0	/* Don't use interval check */

/*
 * Behaviour when detecting errors
 */
#define TFS_ERRORS_CONTINUE		1	/* Continue execution */
#define TFS_ERRORS_RO			2	/* Remount fs read-only */
#define TFS_ERRORS_PANIC		3	/* Panic */
#define TFS_ERRORS_DEFAULT		TFS_ERRORS_CONTINUE

/*
 * Structure of the super block
 */
struct tfs_super_block {
	__le32	s_inodes_count;		/* Inodes count */
	__le32	s_blocks_count;		/* Blocks count */
	__le32	s_r_blocks_count;	/* Reserved blocks count */
	__le32	s_free_blocks_count;	/* Free blocks count */
	__le32	s_free_inodes_count;	/* Free inodes count */
	__le32	s_first_data_block;	/* First Data Block */
	__le32	s_log_block_size;	/* Block size */
	__le32	s_log_frag_size;	/* Fragment size */
	__le32	s_blocks_per_group;	/* # Blocks per group */
	__le32	s_frags_per_group;	/* # Fragments per group */
	__le32	s_inodes_per_group;	/* # Inodes per group */
	__le32	s_mtime;		/* Mount time */
	__le32	s_wtime;		/* Write time */
	__le16	s_mnt_count;		/* Mount count */
	__le16	s_max_mnt_count;	/* Maximal mount count */
	__le16	s_magic;		/* Magic signature */
	__le16	s_state;		/* File system state */
	__le16	s_errors;		/* Behaviour when detecting errors */
	__le16	s_minor_rev_level; 	/* minor revision level */
	__le32	s_lastcheck;		/* time of last check */
	__le32	s_checkinterval;	/* max. time between checks */
	__le32	s_creator_os;		/* OS */
	__le32	s_rev_level;		/* Revision level */
	__le16	s_def_resuid;		/* Default uid for reserved blocks */
	__le16	s_def_resgid;		/* Default gid for reserved blocks */
	/*
	 * These fields are for TFS_DYNAMIC_REV superblocks only.
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
	__le32	s_first_ino; 		/* First non-reserved inode */
	__le16   s_inode_size; 		/* size of inode structure */
	__le16	s_block_group_nr; 	/* block group # of this superblock */
	__le32	s_feature_compat; 	/* compatible feature set */
	__le32	s_feature_incompat; 	/* incompatible feature set */
	__le32	s_feature_ro_compat; 	/* readonly-compatible feature set */
	__u8	s_uuid[16];		/* 128-bit uuid for volume */
	char	s_volume_name[16]; 	/* volume name */
	char	s_last_mounted[64]; 	/* directory where last mounted */
	__le32	s_algorithm_usage_bitmap; /* For compression */
	/*
	 * Performance hints.  Directory preallocation should only
	 * happen if the TFS_COMPAT_PREALLOC flag is on.
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
	__le32	s_default_mount_opts;
 	__le32	s_first_meta_bg; 	/* First metablock block group */
	__u32	s_reserved[190];	/* Padding to the end of the block */
};

/*
 * Codes for operating systems
 */
#define TFS_OS_LINUX		0
#define TFS_OS_HURD		1
#define TFS_OS_MASIX		2
#define TFS_OS_FREEBSD		3
#define TFS_OS_LITES		4

/*
 * Revision levels
 */
#define TFS_GOOD_OLD_REV	0	/* The good old (original) format */
#define TFS_DYNAMIC_REV	1 	/* V2 format w/ dynamic inode sizes */

#define TFS_CURRENT_REV	TFS_GOOD_OLD_REV
#define TFS_MAX_SUPP_REV	TFS_DYNAMIC_REV

#define TFS_GOOD_OLD_INODE_SIZE 128

/*
 * Feature set definitions
 */

#define TFS_HAS_COMPAT_FEATURE(sb,mask)			\
	( TFS_SB(sb)->s_es->s_feature_compat & cpu_to_le32(mask) )
#define TFS_HAS_RO_COMPAT_FEATURE(sb,mask)			\
	( TFS_SB(sb)->s_es->s_feature_ro_compat & cpu_to_le32(mask) )
#define TFS_HAS_INCOMPAT_FEATURE(sb,mask)			\
	( TFS_SB(sb)->s_es->s_feature_incompat & cpu_to_le32(mask) )
#define TFS_SET_COMPAT_FEATURE(sb,mask)			\
	TFS_SB(sb)->s_es->s_feature_compat |= cpu_to_le32(mask)
#define TFS_SET_RO_COMPAT_FEATURE(sb,mask)			\
	TFS_SB(sb)->s_es->s_feature_ro_compat |= cpu_to_le32(mask)
#define TFS_SET_INCOMPAT_FEATURE(sb,mask)			\
	TFS_SB(sb)->s_es->s_feature_incompat |= cpu_to_le32(mask)
#define TFS_CLEAR_COMPAT_FEATURE(sb,mask)			\
	TFS_SB(sb)->s_es->s_feature_compat &= ~cpu_to_le32(mask)
#define TFS_CLEAR_RO_COMPAT_FEATURE(sb,mask)			\
	TFS_SB(sb)->s_es->s_feature_ro_compat &= ~cpu_to_le32(mask)
#define TFS_CLEAR_INCOMPAT_FEATURE(sb,mask)			\
	TFS_SB(sb)->s_es->s_feature_incompat &= ~cpu_to_le32(mask)

#define TFS_FEATURE_COMPAT_DIR_PREALLOC	0x0001
#define TFS_FEATURE_COMPAT_IMAGIC_INODES	0x0002
#define EXT3_FEATURE_COMPAT_HAS_JOURNAL		0x0004
#define TFS_FEATURE_COMPAT_EXT_ATTR		0x0008
#define TFS_FEATURE_COMPAT_RESIZE_INO		0x0010
#define TFS_FEATURE_COMPAT_DIR_INDEX		0x0020
#define TFS_FEATURE_COMPAT_ANY			0xffffffff

#define TFS_FEATURE_RO_COMPAT_SPARSE_SUPER	0x0001
#define TFS_FEATURE_RO_COMPAT_LARGE_FILE	0x0002
#define TFS_FEATURE_RO_COMPAT_BTREE_DIR	0x0004
#define TFS_FEATURE_RO_COMPAT_ANY		0xffffffff

#define TFS_FEATURE_INCOMPAT_COMPRESSION	0x0001
#define TFS_FEATURE_INCOMPAT_FILETYPE		0x0002
#define EXT3_FEATURE_INCOMPAT_RECOVER		0x0004
#define EXT3_FEATURE_INCOMPAT_JOURNAL_DEV	0x0008
#define TFS_FEATURE_INCOMPAT_META_BG		0x0010
#define TFS_FEATURE_INCOMPAT_ANY		0xffffffff

#define TFS_FEATURE_COMPAT_SUPP	TFS_FEATURE_COMPAT_EXT_ATTR
#define TFS_FEATURE_INCOMPAT_SUPP	(TFS_FEATURE_INCOMPAT_FILETYPE| \
					 TFS_FEATURE_INCOMPAT_META_BG)
#define TFS_FEATURE_RO_COMPAT_SUPP	(TFS_FEATURE_RO_COMPAT_SPARSE_SUPER| \
					 TFS_FEATURE_RO_COMPAT_LARGE_FILE| \
					 TFS_FEATURE_RO_COMPAT_BTREE_DIR)
#define TFS_FEATURE_RO_COMPAT_UNSUPPORTED	~TFS_FEATURE_RO_COMPAT_SUPP
#define TFS_FEATURE_INCOMPAT_UNSUPPORTED	~TFS_FEATURE_INCOMPAT_SUPP

/*
 * Default values for user and/or group using reserved blocks
 */
#define	TFS_DEF_RESUID		0
#define	TFS_DEF_RESGID		0

/*
 * Default mount options
 */
#define TFS_DEFM_DEBUG		0x0001
#define TFS_DEFM_BSDGROUPS	0x0002
#define TFS_DEFM_XATTR_USER	0x0004
#define TFS_DEFM_ACL		0x0008
#define TFS_DEFM_UID16		0x0010
    /* Not used by tfs, but reserved for use by ext3 */
#define EXT3_DEFM_JMODE		0x0060 
#define EXT3_DEFM_JMODE_DATA	0x0020
#define EXT3_DEFM_JMODE_ORDERED	0x0040
#define EXT3_DEFM_JMODE_WBACK	0x0060

/*
 * Structure of a directory entry
 */
#define TFS_NAME_LEN 255

struct tfs_dir_entry {
	__le32	inode;			/* Inode number */
	__le16	rec_len;		/* Directory entry length */
	__le16	name_len;		/* Name length */
	char	name[TFS_NAME_LEN];	/* File name */
};

/*
 * The new version of the directory entry.  Since TFS structures are
 * stored in intel byte order, and the name_len field could never be
 * bigger than 255 chars, it's safe to reclaim the extra byte for the
 * file_type field.
 */
struct tfs_dir_entry_2 {
	__le32	inode;			/* Inode number */
	__le16	rec_len;		/* Directory entry length */
	__u8	name_len;		/* Name length */
	__u8	file_type;
	char	name[TFS_NAME_LEN];	/* File name */
};

/*
 * Ext2 directory file types.  Only the low 3 bits are used.  The
 * other bits are reserved for now.
 */
enum {
	TFS_FT_UNKNOWN,
	TFS_FT_REG_FILE,
	TFS_FT_DIR,
	TFS_FT_CHRDEV,
	TFS_FT_BLKDEV,
	TFS_FT_FIFO,
	TFS_FT_SOCK,
	TFS_FT_SYMLINK,
	TFS_FT_MAX
};

/*
 * TFS_DIR_PAD defines the directory entries boundaries
 *
 * NOTE: It must be a multiple of 4
 */
#define TFS_DIR_PAD		 	4
#define TFS_DIR_ROUND 			(TFS_DIR_PAD - 1)
#define TFS_DIR_REC_LEN(name_len)	(((name_len) + 8 + TFS_DIR_ROUND) & \
					 ~TFS_DIR_ROUND)

#endif	/* _LINUX_TFS_FS_H */
