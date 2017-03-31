#ifndef __DISK_TRACE_H__
#define __DISK_TRACE_H__

typedef struct DiskEvent{
	unsigned int sector;
	unsigned int bdev;
	int rw;
}DiskEvent;


#define DISK_TRACE_BUFFER_SIZE 20000

void recordDiskEvent(DiskEvent e);
ssize_t readDiskTrace(struct file *file, char __user *buf,
			    size_t count, loff_t *pos);




#endif
