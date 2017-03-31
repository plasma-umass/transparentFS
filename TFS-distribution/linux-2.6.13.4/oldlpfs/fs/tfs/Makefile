#
# Makefile for the linux tfs-filesystem routines.
#

obj-$(CONFIG_TFS_FS) += tfs.o

tfs-y := balloc.o bitmap.o dir.o file.o fsync.o ialloc.o inode.o \
	  ioctl.o namei.o super.o symlink.o

tfs-$(CONFIG_TFS_FS_XATTR)	 += xattr.o xattr_user.o xattr_trusted.o
tfs-$(CONFIG_TFS_FS_POSIX_ACL) += acl.o
tfs-$(CONFIG_TFS_FS_SECURITY)	 += xattr_security.o
tfs-$(CONFIG_TFS_FS_XIP)	 += xip.o
