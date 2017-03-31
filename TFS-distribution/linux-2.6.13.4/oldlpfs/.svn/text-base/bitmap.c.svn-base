/*
 *  linux/fs/lpfs/bitmap.c
 *
 * Copyright (C) 1992, 1993, 1994, 1995
 * Remy Card (card@masi.ibp.fr)
 * Laboratoire MASI - Institut Blaise Pascal
 * Universite Pierre et Marie Curie (Paris VI)
 */

#include <linux/fs.h>
#include "lpfs_fs.h"


static int nibblemap[] = {4, 3, 3, 2, 3, 2, 2, 1, 3, 2, 2, 1, 2, 1, 1, 0};

unsigned long lpfs_count_free (struct buffer_head * map, unsigned int numchars)
{
	unsigned int i;
	unsigned long sum = 0;
	
	if (!map) 
		return (0);
	for (i = 0; i < numchars; i++)
		sum += nibblemap[map->b_data[i] & 0xf] +
			nibblemap[(map->b_data[i] >> 4) & 0xf];
	return (sum);
}
/* JC */
//this is passed a len, and an array of pointers to (char *)
//as well as a starting offset and ending offset
//it returns the first byte which is zero in all of the given bitmaps
//Are we sure this isnt going over the end of our buffer?
//This can be tested in user space, probably should be
//Made a userland test, seems to work pretty well
inline int lpfs_find_zero_byte(int count, char ** bitmaps, int start, int end){
	int i,j;
	char *c;
	int notdone=1;
	
	j=start;
	do{
		for(i=0;i<count;i++){
			if(j>=end){ break; }
			if( ((char *)bitmaps[i])[j]!=0){
				c=memscan(bitmaps[i]+j,0, end-j);
				j=c - bitmaps[i];
				break;
			}
		}
		if(i>=count || j>=end){ break; }
	}while(notdone);
	return j;
}
		
//This finds the first byte which is writeable for the given priority
//FIXME use priority for find_first_zero_byte
inline int lpfs_p_find_first_zero_byte(char *highp, char *lowp, char *dirty, char *blocklock, int start, 
				int length, int priority){
	char *lpbitmaps[4]={highp,lowp,dirty,blocklock};
	char *hpbitmaps[2]={highp,blocklock};
	if(priority==0){
		return lpfs_find_zero_byte(4,lpbitmaps,start, length);
	}else if(priority==1){
		return lpfs_find_zero_byte(2,hpbitmaps,start,length);
	}else if(priority==-1){
		return lpfs_find_zero_byte(4,lpbitmaps,start, length);
	}else{
		printk("LPFS: SEARCHING BITMAP WITH UNDEFINED PRIORITY, USING LP\n");
		return lpfs_find_zero_byte(4,lpbitmaps,start, length);
	}
}

//set the bits in the bitmaps according to the priority
inline int lpfs_p_set_bit(int j, char *highp, char *lowp, char *dirty, char *blocklock, int priority){
	if(priority==0){
		return lpfs_set_bit(j,lowp)&&lpfs_set_bit(j,blocklock);
	}else if(priority==1){
		if(lpfs_test_bit(j,blocklock)){ return 1; }
		if(lpfs_test_bit(j,lowp)){
			lpfs_set_bit(j,dirty);
		}
		return lpfs_set_bit(j,highp);
	}else if(priority==-1){
		if(lpfs_test_bit(j,blocklock)){ return 1; }
		if(lpfs_test_bit(j,lowp)){
			lpfs_set_bit(j,dirty);
		}
		return lpfs_set_bit(j,highp);
	}else{
		printk("LPFS: MARKING BLOCK WITH UNDEFINED PRIORITY, USING HP\n");
		if(lpfs_test_bit(j,blocklock)){ return 1; }
		if(lpfs_test_bit(j,lowp)){
			lpfs_set_bit(j,dirty);
		}
		return lpfs_set_bit(j,highp);
	}
}

//test a bit according to priority
inline int lpfs_p_test_bit(int j, char *highp, char *lowp, char *dirty, char *blocklock, int priority){
	if(priority==0){
		return lpfs_test_bit(j, highp) || lpfs_test_bit(j,lowp)
			|| lpfs_test_bit(j, dirty)|| lpfs_test_bit(j,blocklock);
	}else if(priority==1){
		return lpfs_test_bit(j, highp) || lpfs_test_bit(j,blocklock);
	}else if(priority==-1){
		return lpfs_test_bit(j, highp) || lpfs_test_bit(j,lowp)
			|| lpfs_test_bit(j, dirty)|| lpfs_test_bit(j,blocklock);
	}else{
		printk("LPFS: TESTING BLOCK WITH UNDEFINED PRIORITY, USING LP\n");
//		printk("	hp %i lp %i dirty %i\n",lpfs_test_bit(j, highp),
//			lpfs_test_bit(j,lowp),lpfs_test_bit(j, dirty));
		return lpfs_test_bit(j, highp) || lpfs_test_bit(j,lowp) 
			|| lpfs_test_bit(j, dirty)||lpfs_test_bit(j,blocklock);
	}
}

//find next zero bit in an array of bitmaps
//FIXME possible memory problem
//are we sure that this works
//can, and should, be tested in user space
inline int lpfs_p_find_next_zero_bith(int count, char **bitmaps, int start, int end){
	int i,j;
	int notdone=1;
	
//printk("lpfs_p_find_next_zero_bith called\n");
	j=start;
	do{
		for(i=0;i<count;i++){
			if(j>=end){ break; }
			if(lpfs_test_bit(j,bitmaps[i])){
				j=lpfs_find_next_zero_bit(bitmaps[i],end, j);
				break;
			}
		}
		if(i>=count || j>=end){ break; }
	}while(notdone);
//printk("lpfs_p_find_next_zero_bith called\n");
	return j;
}

inline int lpfs_p_find_next_zero_bit(char *highp, char *lowp, char *dirty, char *blocklock,
				int end, int start, int priority){
	char *lpbitmaps[4]={highp,lowp,dirty,blocklock};
	char *hpbitmaps[2]={highp,blocklock};
	if(priority==0){
		return lpfs_p_find_next_zero_bith(4,lpbitmaps,start, end);
	}else if(priority==1){
		return lpfs_p_find_next_zero_bith(2,hpbitmaps,start,end);
	}else if(priority==-1){
		return lpfs_p_find_next_zero_bith(4,lpbitmaps,start,end);
	}else{
		printk("LPFS: SEARCHING BITMAP WITH UNDEFINED PRIORITY, USING LP\n");
		return lpfs_p_find_next_zero_bith(4,lpbitmaps,start, end);
	}
}
/* /JC */
