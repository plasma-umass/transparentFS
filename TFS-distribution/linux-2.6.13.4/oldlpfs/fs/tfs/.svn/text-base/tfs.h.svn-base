#include <linux/fs.h>
#include <linux/tfs_fs.h>

/*
 * tfs mount options
 */
struct tfs_mount_options {
	unsigned long s_mount_opt;
	uid_t s_resuid;
	gid_t s_resgid;
};

/*
 * second extended file system inode data in memory
 */
struct tfs_inode_info {
	__le32	i_data[15];
	__u32	i_flags;
	__u32	i_faddr;
	__u8	i_frag_no;
	__u8	i_frag_size;
	__u16	i_state;
	__u32	i_file_acl;
	__u32	i_dir_acl;
	__u32	i_dtime;

	/*
	 * i_block_group is the number of the block group which contains
	 * this file's inode.  Constant across the lifetime of the inode,
	 * it is ued for making block allocation decisions - we try to
	 * place a file's data blocks near its inode block, and new inodes
	 * near to their parent directory's inode.
	 */
	__u32	i_block_group;

	/*
	 * i_next_alloc_block is the logical (file-relative) number of the
	 * most-recently-allocated block in this file.  Yes, it is misnamed.
	 * We use this for detecting linearly ascending allocation requests.
	 */
	__u32	i_next_alloc_block;

	/*
	 * i_next_alloc_goal is the *physical* companion to i_next_alloc_block.
	 * it the the physical block number of the block which was most-recently
	 * allocated to this file.  This give us the goal (target) for the next
	 * allocation when we detect linearly ascending requests.
	 */
	__u32	i_next_alloc_goal;
	__u32	i_prealloc_block;
	__u32	i_prealloc_count;
	__u32	i_dir_start_lookup;
#ifdef CONFIG_TFS_FS_XATTR
	/*
	 * Extended attributes can be read independently of the main file
	 * data. Taking i_sem even when reading would cause contention
	 * between readers of EAs and writers of regular file data, so
	 * instead we synchronize on xattr_sem when reading or changing
	 * EAs.
	 */
	struct rw_semaphore xattr_sem;
#endif
#ifdef CONFIG_TFS_FS_POSIX_ACL
	struct posix_acl	*i_acl;
	struct posix_acl	*i_default_acl;
#endif
	rwlock_t i_meta_lock;
	struct inode	vfs_inode;
};

/*
 * Inode dynamic state flags
 */
#define TFS_STATE_NEW			0x00000001 /* inode is newly created */


/*
 * Function prototypes
 */

/*
 * Ok, these declarations are also in <linux/kernel.h> but none of the
 * tfs source programs needs to include it so they are duplicated here.
 */

static inline struct tfs_inode_info *TFS_I(struct inode *inode)
{
	return container_of(inode, struct tfs_inode_info, vfs_inode);
}

/* balloc.c */
extern int tfs_bg_has_super(struct super_block *sb, int group);
extern unsigned long tfs_bg_num_gdb(struct super_block *sb, int group);
extern int tfs_new_block (struct inode *, unsigned long,
			   __u32 *, __u32 *, int *);
extern void tfs_free_blocks (struct inode *, unsigned long,
			      unsigned long);
extern unsigned long tfs_count_free_blocks (struct super_block *);
extern unsigned long tfs_count_dirs (struct super_block *);
extern void tfs_check_blocks_bitmap (struct super_block *);
extern struct tfs_group_desc * tfs_get_group_desc(struct super_block * sb,
						    unsigned int block_group,
						    struct buffer_head ** bh);

/* dir.c */
extern int tfs_add_link (struct dentry *, struct inode *);
extern ino_t tfs_inode_by_name(struct inode *, struct dentry *);
extern int tfs_make_empty(struct inode *, struct inode *);
extern struct tfs_dir_entry_2 * tfs_find_entry (struct inode *,struct dentry *, struct page **);
extern int tfs_delete_entry (struct tfs_dir_entry_2 *, struct page *);
extern int tfs_empty_dir (struct inode *);
extern struct tfs_dir_entry_2 * tfs_dotdot (struct inode *, struct page **);
extern void tfs_set_link(struct inode *, struct tfs_dir_entry_2 *, struct page *, struct inode *);

/* fsync.c */
extern int tfs_sync_file (struct file *, struct dentry *, int);

/* ialloc.c */
extern struct inode * tfs_new_inode (struct inode *, int);
extern void tfs_free_inode (struct inode *);
extern unsigned long tfs_count_free_inodes (struct super_block *);
extern void tfs_check_inodes_bitmap (struct super_block *);
extern unsigned long tfs_count_free (struct buffer_head *, unsigned);

/* inode.c */
extern void tfs_read_inode (struct inode *);
extern int tfs_write_inode (struct inode *, int);
extern void tfs_put_inode (struct inode *);
extern void tfs_delete_inode (struct inode *);
extern int tfs_sync_inode (struct inode *);
extern void tfs_discard_prealloc (struct inode *);
extern int tfs_get_block(struct inode *, sector_t, struct buffer_head *, int);
extern void tfs_truncate (struct inode *);
extern int tfs_setattr (struct dentry *, struct iattr *);
extern void tfs_set_inode_flags(struct inode *inode);

/* ioctl.c */
extern int tfs_ioctl (struct inode *, struct file *, unsigned int,
		       unsigned long);

/* super.c */
extern void tfs_error (struct super_block *, const char *, const char *, ...)
	__attribute__ ((format (printf, 3, 4)));
extern void tfs_warning (struct super_block *, const char *, const char *, ...)
	__attribute__ ((format (printf, 3, 4)));
extern void tfs_update_dynamic_rev (struct super_block *sb);
extern void tfs_write_super (struct super_block *);

/*
 * Inodes and files operations
 */

/* dir.c */
extern struct file_operations tfs_dir_operations;

/* file.c */
extern struct inode_operations tfs_file_inode_operations;
extern struct file_operations tfs_file_operations;
extern struct file_operations tfs_xip_file_operations;

/* inode.c */
extern struct address_space_operations tfs_aops;
extern struct address_space_operations tfs_aops_xip;
extern struct address_space_operations tfs_nobh_aops;

/* namei.c */
extern struct inode_operations tfs_dir_inode_operations;
extern struct inode_operations tfs_special_inode_operations;

/* symlink.c */
extern struct inode_operations tfs_fast_symlink_inode_operations;
extern struct inode_operations tfs_symlink_inode_operations;




#define tfs_set_bit		ext2_set_bit
#define tfs_set_bit_atomic	ext2_set_bit_atomic
#define tfs_clear_bit		ext2_clear_bit
#define tfs_clear_bit_atomic	ext2_clear_bit_atomic
#define tfs_test_bit		ext2_test_bit
#define tfs_find_next_zero_bit	ext2_find_next_zero_bit
