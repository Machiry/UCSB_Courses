#include <stdio.h> 
#include <string.h>
#include <sys/types.h> 
#include <time.h>
#include <pthread.h>
#include <unistd.h>
#define CACHE_MIN (1024*8) /* smallest cache */ 
#define CACHE_MAX (8*1024*1024) /* largest cache */ 
#define SAMPLE 10 /* to get a larger time sample */ 

void *inc_x(void *x_void_ptr)
{

/* increment x to 100 */
int *x_ptr = (int *)x_void_ptr;
while(1) {
    *x_ptr = *x_ptr + 1;
}

//printf("x increment finished\n");

/* the function must return something - NULL will do */
return NULL;

}

  
int x[CACHE_MAX]; /* array going to stride through */ 
  
#include <sys/times.h> 
#ifndef CLK_TCK 
#define CLK_TCK 60 /* number clock ticks per second */ 
#endif 
double get_seconds() { /* routine to read time */ 
    struct tms rusage; 
    times(&rusage); /* UNIX utility: time in clock ticks */ 
    return (double) (rusage.tms_utime)/CLK_TCK; 
} 

int get_no_bits_set(int n) {
    int count = 0;
    while(n) {
        count++;
        n = n&(n-1);
    }
    return count;
}
  
int main(int argc,char *argv[]) 
{ 
    long register i, index, stride, limit, temp=0,base_addr,curr_addr; 
    long steps, tsteps, csize; 
    long double sec0, sec; /* timing variables */ 
    long element_to_access = 0;
    long cache_line_size = 0, cache_line_mask, cache_line_bits;
    long cache_set_size = 0, cache_set_mask, cache_set_bits;
    long assoc = 0;
    long target_set = 0;
    time_t ds;
    pthread_t th[8];
    cache_line_size = atol(argv[1]);
    cache_set_size = atol(argv[2]);
    assoc = atol(argv[3]);
    target_set = atol(argv[4]);
    memset(x,1,sizeof(x));
    ds = time(NULL);

    cache_line_bits = get_no_bits_set(cache_line_size - 1);
    cache_set_bits = get_no_bits_set(cache_set_size - 1);

    cache_line_mask = ~(cache_line_size - 1);
    cache_set_mask = ~((cache_set_size - 1) << cache_line_bits);

    base_addr = (long)(&x) & (cache_set_mask) & (cache_line_mask);
    stride = (1<<(cache_set_bits+cache_line_bits));

    base_addr += stride;
    base_addr = base_addr | (target_set << cache_line_bits);

    /*if(base_addr < (long)x || base_addr > (long)(&x[CACHE_MAX-1])) {
        printf("Array is too small\n");
    }*/
    
    //printf("%p,%p\n",&x,&x[CACHE_MAX-1]);



    curr_addr = base_addr;
    for(i=0;i<assoc;i++) {
        //curr_addr += stride;
        pthread_create(&th[i], NULL, inc_x, (void*)curr_addr);
        curr_addr += stride;
        //printf("Addr:%p\n",curr_addr);
    }
    
    //printf("Stabilized\n");
    //printf("Base Addr:%p\n",base_addr);
    sleep(1);
    ds += 3;
    printf("ThreadsCreated:%d\n",(int)ds);
    fflush(stdout);

    /*while(1) {
        curr_addr = base_addr;
        for(i=0;i<assoc;i++) {
            curr_addr += stride;
            *(int*)curr_addr = *(int*)curr_addr + 1;
        }        
    }*/
    for(i=0;i<assoc;i++) {
        pthread_join(th[i], NULL);
    }

}
    
