/*
 * probe.c - identify a block device by its contents, and return a dev
 *           struct with the details
 *
 * Copyright (C) 1999 by Andries Brouwer
 * Copyright (C) 1999, 2000, 2003 by Theodore Ts'o
 * Copyright (C) 2001 by Andreas Dilger
 *
 * %Begin-Header%
 * This file may be redistributed under the terms of the
 * GNU Lesser General Public License.
 * %End-Header%
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_MKDEV_H
#include <sys/mkdev.h>
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#include "blkidP.h"
#include "uuid/uuid.h"
#include "probe.h"

/*
 * This is a special case code to check for an MDRAID device.  We do
 * this special since it requires checking for a superblock at the end
 * of the device.
 */
static int check_mdraid(int fd, unsigned char *ret_uuid)
{
	struct mdp_superblock_s *md;
	blkid_loff_t		offset;
	char			buf[4096];
	
	if (fd < 0)
		return -BLKID_ERR_PARAM;

	offset = (blkid_get_dev_size(fd) & ~((blkid_loff_t)65535)) - 65536;

	if (blkid_llseek(fd, offset, 0) < 0 ||
	    read(fd, buf, 4096) != 4096)
		return -BLKID_ERR_IO;

	/* Check for magic number */
	if (memcmp("\251+N\374", buf, 4))
		return -BLKID_ERR_PARAM;

	if (!ret_uuid)
		return 0;
	*ret_uuid = 0;

	/* The MD UUID is not contiguous in the superblock, make it so */
	md = (struct mdp_superblock_s *)buf;
	if (md->set_uuid0 || md->set_uuid1 || md->set_uuid2 || md->set_uuid3) {
		memcpy(ret_uuid, &md->set_uuid0, 4);
		memcpy(ret_uuid, &md->set_uuid1, 12);
	}
	return 0;
}

static void set_uuid(blkid_dev dev, uuid_t uuid)
{
	char	str[37];

	if (!uuid_is_null(uuid)) {
		uuid_unparse(uuid, str);
		blkid_set_tag(dev, "UUID", str, sizeof(str));
	}
}

static int probe_ext2(int fd, blkid_cache cache, blkid_dev dev,
		      struct blkid_magic *id, unsigned char *buf)
{
	struct ext2_super_block *es;
	const char *sec_type = 0, *label = 0;

	es = (struct ext2_super_block *)buf;

	DBG(DEBUG_PROBE, printf("ext2_sb.compat = %08X:%08X:%08X\n", 
		   blkid_le32(es->s_feature_compat),
		   blkid_le32(es->s_feature_incompat),
		   blkid_le32(es->s_feature_ro_compat)));

	/* Distinguish between jbd and ext2/3 fs */
	if (id && (blkid_le32(es->s_feature_incompat) &
		   EXT3_FEATURE_INCOMPAT_JOURNAL_DEV))
		return -BLKID_ERR_PARAM;

	if (strlen(es->s_volume_name))
		label = es->s_volume_name;
	blkid_set_tag(dev, "LABEL", label, sizeof(es->s_volume_name));

	set_uuid(dev, es->s_uuid);

	if (blkid_le32(es->s_feature_compat) &
	    EXT3_FEATURE_COMPAT_HAS_JOURNAL)
		sec_type = "ext3";
	
	blkid_set_tag(dev, "SEC_TYPE", sec_type, 0);

	return 0;
}

static int probe_jbd(int fd, blkid_cache cache, blkid_dev dev, 
		     struct blkid_magic *id, unsigned char *buf)
{
	struct ext2_super_block *es = (struct ext2_super_block *) buf;

	if (!(blkid_le32(es->s_feature_incompat) &
	      EXT3_FEATURE_INCOMPAT_JOURNAL_DEV))
		return -BLKID_ERR_PARAM;

	return (probe_ext2(fd, cache, dev, 0, buf));
}

static int probe_vfat(int fd, blkid_cache cache, blkid_dev dev,
		      struct blkid_magic *id, unsigned char *buf)
{
	struct vfat_super_block *vs;
	char serno[10];
	const char *label = 0;

	vs = (struct vfat_super_block *)buf;

	if (strncmp(vs->vs_label, "NO NAME", 7)) {
		char *end = vs->vs_label + sizeof(vs->vs_label) - 1;

		while (*end == ' ' && end >= vs->vs_label)
			--end;
		if (end >= vs->vs_label)
			label = vs->vs_label;
		blkid_set_tag(dev, "LABEL", label, end - vs->vs_label + 1);
	}

	/* We can't just print them as %04X, because they are unaligned */
	sprintf(serno, "%02X%02X-%02X%02X", vs->vs_serno[3], vs->vs_serno[2],
		vs->vs_serno[1], vs->vs_serno[0]);
	blkid_set_tag(dev, "UUID", serno, sizeof(serno));

	return 0;
}

static int probe_msdos(int fd, blkid_cache cache, blkid_dev dev,
		       struct blkid_magic *id, unsigned char *buf)
{
	struct msdos_super_block *ms = (struct msdos_super_block *) buf;
	char serno[10];
	const char *label = 0;

	if (strncmp(ms->ms_label, "NO NAME", 7)) {
		char *end = ms->ms_label + sizeof(ms->ms_label) - 1;

		while (*end == ' ' && end >= ms->ms_label)
			--end;
		if (end >= ms->ms_label)
			label = ms->ms_label;
		blkid_set_tag(dev, "LABEL", label, end - ms->ms_label + 1);
	}

	/* We can't just print them as %04X, because they are unaligned */
	sprintf(serno, "%02X%02X-%02X%02X", ms->ms_serno[3], ms->ms_serno[2],
		ms->ms_serno[1], ms->ms_serno[0]);
	blkid_set_tag(dev, "UUID", serno, 0);

	return 0;
}

static int probe_xfs(int fd, blkid_cache cache, blkid_dev dev,
		     struct blkid_magic *id, unsigned char *buf)
{
	struct xfs_super_block *xs;
	const char *label = 0;

	xs = (struct xfs_super_block *)buf;

	if (strlen(xs->xs_fname))
		label = xs->xs_fname;
	blkid_set_tag(dev, "LABEL", label, sizeof(xs->xs_fname));
	set_uuid(dev, xs->xs_uuid);
	return 0;
}

static int probe_reiserfs(int fd, blkid_cache cache, blkid_dev dev,
			  struct blkid_magic *id, unsigned char *buf)
{
	struct reiserfs_super_block *rs = (struct reiserfs_super_block *) buf;
	unsigned int blocksize;
	const char *label = 0;

	blocksize = blkid_le16(rs->rs_blocksize);

	/* If the superblock is inside the journal, we have the wrong one */
	if (id->bim_kboff/(blocksize>>10) > blkid_le32(rs->rs_journal_block))
		return -BLKID_ERR_BIG;

	/* LABEL/UUID are only valid for later versions of Reiserfs v3.6. */
	if (!strcmp(id->bim_magic, "ReIsEr2Fs") ||
	    !strcmp(id->bim_magic, "ReIsEr3Fs")) {
		if (strlen(rs->rs_label))
			label = rs->rs_label;
		blkid_set_tag(dev, "LABEL", label, sizeof(rs->rs_label));
		set_uuid(dev, rs->rs_uuid);
	}

	return 0;
}

static int probe_jfs(int fd, blkid_cache cache, blkid_dev dev,
		     struct blkid_magic *id, unsigned char *buf)
{
	struct jfs_super_block *js;
	const char *label = 0;

	js = (struct jfs_super_block *)buf;

	if (strlen((char *) js->js_label))
		label = (char *) js->js_label;
	blkid_set_tag(dev, "LABEL", label, sizeof(js->js_label));
	set_uuid(dev, js->js_uuid);
	return 0;
}

static int probe_romfs(int fd, blkid_cache cache, blkid_dev dev,
		       struct blkid_magic *id, unsigned char *buf)
{
	struct romfs_super_block *ros;
	const char *label = 0;

	ros = (struct romfs_super_block *)buf;

	if (strlen((char *) ros->ros_volume))
		label = (char *) ros->ros_volume;
	blkid_set_tag(dev, "LABEL", label, strlen(label));
	return 0;
}

static char
*udf_magic[] = { "BEA01", "BOOT2", "CD001", "CDW02", "NSR02",
		 "NSR03", "TEA01", 0 };

static int probe_udf(int fd, blkid_cache cache, blkid_dev dev,
		       struct blkid_magic *id, unsigned char *buf)
{
	int j, bs;
	struct iso_volume_descriptor isosb;
	char **m;

	/* determine the block size by scanning in 2K increments
	   (block sizes larger than 2K will be null padded) */
	for (bs = 1; bs < 16; bs++) {
		lseek(fd, bs*2048+32768, SEEK_SET);
		if (read(fd, (char *)&isosb, sizeof(isosb)) != sizeof(isosb))
			return 1;
		if (isosb.id[0])
			break;
	}

	/* Scan up to another 64 blocks looking for additional VSD's */
	for (j = 1; j < 64; j++) {
		if (j > 1) {
			lseek(fd, j*bs*2048+32768, SEEK_SET);
			if (read(fd, (char *)&isosb, sizeof(isosb))
			    != sizeof(isosb))
				return 1;
		}
		/* If we find NSR0x then call it udf:
		   NSR01 for UDF 1.00
		   NSR02 for UDF 1.50
		   NSR03 for UDF 2.00 */
		if (!strncmp(isosb.id, "NSR0", 4))
			return 0;
		for (m = udf_magic; *m; m++)
			if (!strncmp(*m, isosb.id, 5))
				break;
		if (*m == 0)
			return 1;
	}
	return 1;
}

/*
 * BLKID_BLK_OFFS is at least as large as the highest bim_kboff defined
 * in the type_array table below + bim_kbalign.
 *
 * When probing for a lot of magics, we handle everything in 1kB buffers so
 * that we don't have to worry about reading each combination of block sizes.
 */
#define BLKID_BLK_OFFS	64	/* currently reiserfs */

/*
 * Various filesystem magics that we can check for.  Note that kboff and
 * sboff are in kilobytes and bytes respectively.  All magics are in
 * byte strings so we don't worry about endian issues.
 */
static struct blkid_magic type_array[] = {
/*  type     kboff   sboff len  magic			probe */
  { "jbd",	 1,   0x38,  2, "\123\357",		probe_jbd },
  { "ext2",	 1,   0x38,  2, "\123\357",		probe_ext2 },
  { "reiserfs",	 8,   0x34,  8, "ReIsErFs",		probe_reiserfs },
  { "reiserfs", 64,   0x34,  9, "ReIsEr2Fs",		probe_reiserfs },
  { "reiserfs", 64,   0x34,  9, "ReIsEr3Fs",		probe_reiserfs },
  { "reiserfs", 64,   0x34,  8, "ReIsErFs",		probe_reiserfs },
  { "reiserfs",	 8,	20,  8, "ReIsErFs",		probe_reiserfs },
  { "ntfs",      0,      3,  8, "NTFS    ",             0 },
  { "vfat",      0,   0x52,  5, "MSWIN",                probe_vfat },
  { "vfat",      0,   0x52,  8, "FAT32   ",             probe_vfat },
  { "msdos",     0,   0x36,  5, "MSDOS",                probe_msdos },
  { "msdos",     0,   0x36,  8, "FAT16   ",             probe_msdos },
  { "msdos",     0,   0x36,  8, "FAT12   ",             probe_msdos },
  { "minix",     1,   0x10,  2, "\177\023",             0 },
  { "minix",     1,   0x10,  2, "\217\023",             0 },
  { "minix",	 1,   0x10,  2, "\150\044",		0 },
  { "minix",	 1,   0x10,  2, "\170\044",		0 },
  { "vxfs",	 1,	 0,  4, "\365\374\001\245",	0 },
  { "xfs",	 0,	 0,  4, "XFSB",			probe_xfs },
  { "romfs",	 0,	 0,  8, "-rom1fs-",		probe_romfs },
  { "bfs",	 0,	 0,  4, "\316\372\173\033",	0 },
  { "cramfs",	 0,	 0,  4, "E=\315\034",		0 },
  { "qnx4",	 0,	 4,  6, "QNX4FS",		0 },
  { "udf",	32,	 1,  5, "BEA01",		probe_udf },
  { "udf",	32,	 1,  5, "BOOT2",		probe_udf },
  { "udf",	32,	 1,  5, "CD001",		probe_udf },
  { "udf",	32,	 1,  5, "CDW02",		probe_udf },
  { "udf",	32,	 1,  5, "NSR02",		probe_udf },
  { "udf",	32,	 1,  5, "NSR03",		probe_udf },
  { "udf",	32,	 1,  5, "TEA01",		probe_udf },
  { "iso9660",	32,	 1,  5, "CD001",		0 },
  { "iso9660",	32,	 9,  5, "CDROM",		0 },
  { "jfs",	32,	 0,  4, "JFS1",			probe_jfs },
  { "hfs",	 1,	 0,  2, "BD",			0 },
  { "ufs",	 8,  0x55c,  4, "T\031\001\000",	0 },
  { "hpfs",	 8,	 0,  4, "I\350\225\371",	0 },
  { "sysv",	 0,  0x3f8,  4, "\020~\030\375",	0 },
  { "swap",	 0,  0xff6, 10, "SWAP-SPACE",		0 },
  { "swap",	 0,  0xff6, 10, "SWAPSPACE2",		0 },
  { "swap",	 0, 0x1ff6, 10, "SWAP-SPACE",		0 },
  { "swap",	 0, 0x1ff6, 10, "SWAPSPACE2",		0 },
  { "swap",	 0, 0x3ff6, 10, "SWAP-SPACE",		0 },
  { "swap",	 0, 0x3ff6, 10, "SWAPSPACE2",		0 },
  {   NULL,	 0,	 0,  0, NULL,			NULL }
};

/*
 * Verify that the data in dev is consistent with what is on the actual
 * block device (using the devname field only).  Normally this will be
 * called when finding items in the cache, but for long running processes
 * is also desirable to revalidate an item before use.
 *
 * If we are unable to revalidate the data, we return the old data and
 * do not set the BLKID_BID_FL_VERIFIED flag on it.
 */
blkid_dev blkid_verify_devname(blkid_cache cache, blkid_dev dev)
{
	struct blkid_magic *id;
	unsigned char *bufs[BLKID_BLK_OFFS + 1], *buf;
	const char *type;
	struct stat st;
	time_t diff;
	int fd, idx;

	if (!dev)
		return NULL;

	diff = time(0) - dev->bid_time;

	if (diff < BLKID_PROBE_MIN || (dev->bid_flags & BLKID_BID_FL_VERIFIED &&
				       diff < BLKID_PROBE_INTERVAL))
		return dev;

	DBG(DEBUG_PROBE,
	    printf("need to revalidate %s (time since last check %lu)\n", 
		   dev->bid_name, diff));

	if (((fd = open(dev->bid_name, O_RDONLY)) < 0) ||
	    (fstat(fd, &st) < 0) || !S_ISBLK(st.st_mode)) {
		if (errno == ENXIO || errno == ENODEV || errno == ENOENT) {
			blkid_free_dev(dev);
			return NULL;
		}
		/* We don't have read permission, just return cache data. */
		DBG(DEBUG_PROBE,
		    printf("returning unverified data for %s\n",
			   dev->bid_name));
		return dev;
	}

	memset(bufs, 0, sizeof(bufs));
	
	/*
	 * Iterate over the type array.  If we already know the type,
	 * then try that first.  If it doesn't work, then blow away
	 * the type information, and try again.
	 * 
	 */
try_again:
	type = 0;
	if (!dev->bid_type || !strcmp(dev->bid_type, "mdraid")) {
		uuid_t	uuid;

		if (check_mdraid(fd, uuid) == 0) {
			set_uuid(dev, uuid);
			type = "mdraid";
			goto found_type;
		}
	}
	for (id = type_array; id->bim_type; id++) {
		if (dev->bid_type &&
		    strcmp(id->bim_type, dev->bid_type))
			continue;

		idx = id->bim_kboff + (id->bim_sboff >> 10);
		if (idx > BLKID_BLK_OFFS || idx < 0)
			continue;
		buf = bufs[idx];
		if (!buf) {
			if (lseek(fd, idx << 10, SEEK_SET) < 0)
				continue;

			if (!(buf = (unsigned char *)malloc(1024)))
				continue;
			
			if (read(fd, buf, 1024) != 1024) {
				free(buf);
				continue;
			}
			bufs[idx] = buf;
		}

		if (memcmp(id->bim_magic, buf + (id->bim_sboff&0x3ff),
			   id->bim_len))
			continue;

		if ((id->bim_probe == NULL) ||
		    (id->bim_probe(fd, cache, dev, id, buf) == 0)) {
			type = id->bim_type;
			goto found_type;
		}
	}

	if (!id->bim_type && dev->bid_type) {
		/*
		 * Zap the device filesystem type and try again
		 */
		blkid_set_tag(dev, "TYPE", 0, 0);
		blkid_set_tag(dev, "SEC_TYPE", 0, 0);
		blkid_set_tag(dev, "LABEL", 0, 0);
		blkid_set_tag(dev, "UUID", 0, 0);
		goto try_again;
	}

	if (!dev->bid_type) {
		blkid_free_dev(dev);
		return NULL;
	}
		
found_type:
	if (dev && type) {
		dev->bid_devno = st.st_rdev;
		dev->bid_time = time(0);
		dev->bid_flags |= BLKID_BID_FL_VERIFIED;
		cache->bic_flags |= BLKID_BIC_FL_CHANGED;

		blkid_set_tag(dev, "TYPE", type, 0);
				
		DBG(DEBUG_PROBE, printf("%s: devno 0x%04Lx, type %s\n",
			   dev->bid_name, st.st_rdev, type));
	}

	close(fd);

	return dev;
}

int blkid_known_fstype(const char *fstype)
{
	struct blkid_magic *id;

	for (id = type_array; id->bim_type; id++) {
		if (strcmp(fstype, id->bim_type) == 0)
			return 1;
	}
	return 0;
}

#ifdef TEST_PROGRAM
int main(int argc, char **argv)
{
	blkid_dev dev;
	blkid_cache cache;
	int ret;

	blkid_debug_mask = DEBUG_ALL;
	if (argc != 2) {
		fprintf(stderr, "Usage: %s device\n"
			"Probe a single device to determine type\n", argv[0]);
		exit(1);
	}
	if ((ret = blkid_get_cache(&cache, "/dev/null")) != 0) {
		fprintf(stderr, "%s: error creating cache (%d)\n",
			argv[0], ret);
		exit(1);
	}
	dev = blkid_get_dev(cache, argv[1], BLKID_DEV_NORMAL);
	if (!dev) {
		printf("%s: %s has an unsupported type\n", argv[0], argv[1]);
		return (1);
	}
	printf("%s is type %s\n", argv[1], dev->bid_type ?
		dev->bid_type : "(null)");
	if (dev->bid_label)
		printf("\tlabel is '%s'\n", dev->bid_label);
	if (dev->bid_uuid)
		printf("\tuuid is %s\n", dev->bid_uuid);
	
	blkid_free_dev(dev);
	return (0);
}
#endif
