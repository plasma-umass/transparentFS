#
# Makefile for the linux ext2-filesystem routines.
#

obj-$(CONFIG_LPFS_FS) += lpfs.o

lpfs-y := balloc.o bitmap.o dir.o file.o fsync.o ialloc.o inode.o \
	  ioctl.o namei.o super.o symlink.o

lpfs-$(CONFIG_LPFS_FS_XATTR)	 += xattr.o xattr_user.o xattr_trusted.o
lpfs-$(CONFIG_LPFS_FS_POSIX_ACL) += acl.o
lpfs-$(CONFIG_LPFS_FS_SECURITY)	 += xattr_security.o
