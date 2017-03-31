#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/statfs.h>
#include <sys/stat.h>
#include <sys/ioctl.h>

//cleaninterval is the timeout in seconds between cleanings
#define CLEANINTERVAL 5
//cleanthresh is the minimum free space percentage that will
//set off cleaning
#define CLEANTHRESH 20
//cleangoal is the percentage of free space to try to acheive
//by cleaning
#define CLEANGOAL 40

typedef struct twoints{
	int a;
	int b;
} twoints;

int needsCleaning(char *dir);
twoints cleanDir(char *dir, int percent);
int isdirectory(char *name);
int touch(char *name);



int main(int argc, char **argv){
	twoints r={0,0};
	int t;
	int p;

	if(argc<2){
		printf("Usage: %s lpdir\n", argv[0]);
		exit(0);
	}

	if(access(argv[1],R_OK|W_OK)){
		printf("%s can't access %s\n", argv[0], argv[1]);
		exit(0);
	}

	printf("Starting cleaning daemon on %s\n", argv[1]);
	srand((int)time(NULL));

	while(1){
//		printf("Checking for cleaning\n");
		if((p=needsCleaning(argv[1]))>0){
			printf("Cleaning %i percent of files in %s\n",p,argv[1]);
			r=cleanDir(argv[1],p);
			printf("cleanDir cleaned %i/%i files\n", r.a, r.b);
			if(r.b==0){
			}else{
				continue;	//we may need to clean again immediately
			}
		}
		sleep(CLEANINTERVAL);
	}

	return 0;
}


int needsCleaning(char *dir){
	struct statfs stats;
	int bp=0;
	int ip=0;
	
	if(statfs(dir, &stats)){
		printf("Could not stat %s",dir);
		return -1;
	}

	if( (stats.f_bavail *100) / (stats.f_blocks) < CLEANTHRESH){
		bp= CLEANGOAL - (stats.f_bavail*100)/(stats.f_blocks);
		printf("Block shortage\n");
	}

	if( (stats.f_ffree *100) / (stats.f_files) < CLEANTHRESH ){
		ip=CLEANGOAL - (stats.f_bavail*100)/(stats.f_blocks);
		printf("Inode shortage\n");
	}
	return (bp > ip) ? -bp : ip;
}


twoints cleanDir(char *dir, int percent){
	DIR *dirp;
	struct dirent *dp;
	twoints r={0,0};
	twoints r2={0,0};
	int l=0;
	char *fname;

	fname=malloc(strlen(dir)+255);
	if(fname==NULL){
		printf("cleanDir could not allocate memory\n");
		r.a=r.b=-1;
		return r;
	}

	memcpy(fname,dir, strlen(dir)+1);
	fname[strlen(dir)]='/';
	fname[strlen(dir)+1]=0;
	l=strlen(fname);

	dirp=opendir(dir);
	if(dirp==NULL){
//		printf("cleanDir could not open directory %s\n",dir);
		perror(NULL);
		free(fname);
		r.a=r.b=-1;
		return r;
	}

	dp=readdir(dirp);

	while(dp!=NULL){
		if(strcmp(dp->d_name, ".")==0 || strcmp(dp->d_name, "..")==0){
			dp=readdir(dirp);
			continue;
		}
		memcpy(fname+l, dp->d_name, strlen(dp->d_name)+1);
//		printf("found a file in %s called %s the full name is %s %i\n", dir, dp->d_name, fname,l);
		if(isdirectory(fname)==1){
//			printf("Descending into direcotry %s\n", fname);
			r2=cleanDir(fname, percent);
			if(r2.a>0 &&r2.b>0){ r.a += r2.a; r.b+=r2.b; }
			dp=readdir(dirp);
			continue;
		}
//		if(setBlock(dir, 1)){
//			printf("Could not call ioctl on %s\n",dir);
//		}
//		if(rand()%100 > percent) { 
//			dp=readdir(dirp);
//			continue; 
//		}
//		printf("found a file in %s called %s the full name is %s\n", dir, dp->d_name, fname);
		if(touch(fname)){ r.a++; }
		r.b++;
		dp=readdir(dirp);
		
	}
	free(fname);
	closedir(dirp);
	return r;
}


int isdirectory(char *name){
	struct stat s;

	if(stat(name, &s)){
		return -1;
	}

	if(S_ISDIR(s.st_mode)){ return 1; }
	return 0;
}
	int r=0;
int touch(char *name){
	FILE *fp;
	if((fp=fopen(name,"r"))==NULL){
		r=1;
	}
	fclose(fp);
	return 0;
}
