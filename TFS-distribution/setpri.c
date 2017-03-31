#include <stdio.h>
#include <sys/ioctl.h>
#include <linux/ext2cache_fs.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

int main(int argc, char **argv){
	int fd;
	long result;
	long i;
	result=-49;	
	if(argc<2||argc>3){
		printf("Usage: %s filename [priority]\n",argv[0]);
		return 0;
	}
	if(argc==2){
		printf("Calling IOCTL on %s\n",argv[1]);
		fd=open(argv[1], 0);
		if(fd==-1){
			printf("Bad fd\n");
			return 0;
		}
		result=ioctl(fd, _IOR('p',1,long), &i);
		if(result==-1){
			if(errno==EBADF){
				printf("Bad file descriptor\n");
			}else if(errno==EFAULT){
				printf("Bad argument to ioctl\n");
			}else if(errno==ENOTTY){
				printf("D is not a char device\n");
			}else if(errno==EINVAL){
				printf("Invalid request\n");
			}
		}
		printf("IOCTL got result: %i\n",i);
		return 0;
	}else{
		printf("Calling IOCTL on %s\n",argv[1]);
		fd=open(argv[1], 0);
		if(fd==-1){
			printf("Bad fd\n");
			return 0;
		}
		i=atoi(argv[2]);
		result=ioctl(fd, _IOW('p',2,long), &i);
		if(result==-1){
			if(errno==EBADF){
				printf("Bad file descriptor\n");
			}else if(errno==EFAULT){
				printf("Bad argument to ioctl\n");
			}else if(errno==ENOTTY){
				printf("D is not a char device\n");
			}else if(errno==EINVAL){
				printf("Invalid request\n");
			}
		}
		printf("Got back %i when trying to change priority to %i\n",result, atoi(argv[2]));
		return 0;
	}
}
