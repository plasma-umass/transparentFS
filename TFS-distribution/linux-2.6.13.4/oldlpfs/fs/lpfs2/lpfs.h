#include <linux/fs.h>
#include <linux/lpfs_fs.h>

/*
 * second extended file system inode data in memory
 */
struct lpfs_inode_info {
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
	/* JC - priority field in inode_info */
	__u16	i_priority;
	/* /JC */

#ifdef CONFIG_LPFS_FS_XATTR
	/*
	 * Extended attributes can be read independently of the main file
	 * data. Taking i_sem even when reading would cause contention
	 * between readers of EAs and writers of regular file data, so
	 * instead we synchronize on xattr_sem when reading or changing
	 * EAs.
	 */
	struct rw_semaphore xattr_sem;
#endif
#ifdef CONFIG_LPFS_FS_POSIX_ACL
	struct posix_acl	*i_acl;
	struct posix_acl	*i_default_acl;
#endif
	rwlock_t i_meta_lock;
	struct inode	vfs_inode;
};

/*
 * Inode dynamic state flags
 */
#define LPFS_STATE_NEW			0x00000001 /* inode is newly created */


/*
 * Function prototypes
 */

/*
 * Ok, these declarations are also in <linux/kernel.h> but none of the
 * lpfs source programs needs to include it so they are duplicated here.
 */

static inline struct lpfs_inode_info *LPFS_I(struct inode *inode)
{
	return container_of(inode, struct lpfs_inode_info, vfs_inode);
}


//FIXME - this is not the right way to do this
#define lpfs_clear_bit_atomic ext2_clear_bit_atomic
#define lpfs_test_bit ext2_test_bit
#define lpfs_set_bit ext2_set_bit
#define lpfs_find_next_zero_bit ext2_find_next_zero_bit
#define lpfs_set_bit_atomic ext2_set_bit_atomic
#define lpfs_clear_bit ext2_clear_bit

/* balloc.c */
extern int lpfs_bg_has_super(struct super_block *sb, int group);
extern unsigned long lpfs_bg_num_gdb(struct super_block *sb, int group);
extern int lpfs_new_block (struct inode *, unsigned long,
			   __u32 *, __u32 *, int *,int);
extern void lpfs_free_blocks (struct inode *, unsigned long,
			      unsigned long);
extern unsigned long lpfs_count_free_blocks (struct super_block *);
extern unsigned long lpfs_count_dirs (struct super_block *);
extern void lpfs_check_blocks_bitmap (struct super_block *);
extern struct lpfs_group_desc * lpfs_get_group_desc(struct super_block * sb,
						    unsigned int block_group,
						    struct buffer_head ** bh);

/* bitmap.c */
extern int lpfs_find_zero_byte(int count, char ** bitmaps, int start, int end);
extern int lpfs_p_find_first_zero_byte(char *highp, char *lowp, char *dirty, char *blocklock, int start, 
				int length, int priority);
extern int lpfs_p_set_bit(int j, char *highp, char *lowp, char *dirty, char *blocklock, int priority);
extern int lpfs_p_set_bit_atomic(spinlock_t *lock, int j, char *highp, char *lowp, char *dirty, char *blocklock, int priority);
extern int lpfs_p_find_next_zero_bit(char *highp, char *lowp, char *dirty, char *blocklock,
				int end, int start, int priority);
extern int lpfs_p_test_bit(int j, char *highp, char *lowp, char *dirty, char *blocklock, int priority);
extern int lpfs_unlockBlock(struct super_block *sb, int goal);
extern int lpfs_lockBlock(struct super_block *sb, int goal);
extern int lpfs_isBlockStillMine(struct inode *inode, int block);

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

/* ialloc.c */
extern struct inode * lpfs_new_inode (struct inode *, int);
extern void lpfs_free_inode (struct inode *);
extern unsigned long lpfs_count_free_inodes (struct super_block *);
extern void lpfs_check_inodes_bitmap (struct super_block *);
extern unsigned long lpfs_count_free (struct buffer_head *, unsigned);

/* inode.c */
extern void lpfs_read_inode (struct inode *);
extern int lpfs_write_inode (struct inode *, int);
extern void lpfs_delete_inode (struct inode *);
extern int lpfs_sync_inode (struct inode *);
extern void lpfs_discard_prealloc (struct inode *);
extern int lpfs_get_block(struct inode *, sector_t, struct buffer_head *, int);
extern void lpfs_truncate (struct inode *);
extern int lpfs_setattr (struct dentry *, struct iattr *);
extern void lpfs_set_inode_flags(struct inode *inode);
//JC - lpfs_step_blocks works?
extern int lpfs_step_blocks(struct inode *inode, int (*func)(struct inode *, int, int));

/* ioctl.c */
extern int lpfs_ioctl (struct inode *, struct file *, unsigned int,
		       unsigned long);

/* super.c */
extern void lpfs_error (struct super_block *, const char *, const char *, ...)
	__attribute__ ((format (printf, 3, 4)));
extern void lpfs_warning (struct super_block *, const char *, const char *, ...)
	__attribute__ ((format (printf, 3, 4)));
extern void lpfs_update_dynamic_rev (struct super_block *sb);
extern void lpfs_write_super (struct super_block *);

/*
 * Inodes and files operations
 */

/* dir.c */
extern struct file_operations lpfs_dir_operations;

/* file.c */
extern struct inode_operations lpfs_file_inode_operations;
extern struct file_operations lpfs_file_operations;
extern int lpfs_file_open(struct inode *inode, struct file *filp);

/* inode.c */
extern struct address_space_operations lpfs_aops;
extern struct address_space_operations lpfs_nobh_aops;

/* namei.c */
extern struct inode_operations lpfs_dir_inode_operations;
extern struct inode_operations lpfs_special_inode_operations;

/* symlink.c */
extern struct inode_operations lpfs_fast_symlink_inode_operations;
extern struct inode_operations lpfs_symlink_inode_operations;
