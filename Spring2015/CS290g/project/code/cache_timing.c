/* cache_timer.c */ 
#include <stdio.h> 
#include <sys/types.h> 
#include <time.h>
#include <unistd.h>
#define CACHE_MIN (1024*8) /* smallest cache */ 
#define CACHE_MAX (8*1024*1024) /* largest cache */ 
#define SAMPLE 10 /* to get a larger time sample */ 
  
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
int get_no_bits_set(int n);
int get_no_bits_set(int n) {
    int count = 0;
    while(n) {
        count++;
        n = n&(n-1);
    }
    return count;
}

int main(int argc, char* argv[]) 
{ 
    long register i, index, stride, limit, temp=0,base_addr; 
    long steps, tsteps, csize; 
    long double sec0, sec; /* timing variables */ 
    long element_to_access = 0;
    long cache_line_size = 0, cache_line_mask, cache_line_bits;
    long cache_set_size = 0, cache_set_mask, cache_set_bits;
    csize = 1024*8;
    int *to_acc = NULL;
    unsigned int target_set;
    cache_line_size = atol(argv[1]);
    cache_set_size = atol(argv[2]);
    cache_line_bits = get_no_bits_set(cache_line_size - 1);
    cache_set_bits = get_no_bits_set(cache_set_size - 1);
    srand(time(NULL));
    
    target_set = ((unsigned int)rand())%cache_set_size;
    
    cache_line_mask = ~(cache_line_size - 1);
    cache_set_mask = ~((cache_set_size - 1) << cache_line_bits);

    base_addr = (long)(&x) & (cache_set_mask) & (cache_line_mask);
    
    to_acc =  (int*)( base_addr | (target_set << cache_line_bits));
    printf("TargetSet:%d\n",target_set);
    
  
        //for (stride=1; stride <= csize/2; stride=stride*2) {
        while(1){
            sec = 0;
            limit = csize - stride + 1;
            steps = 0; 
            
            do { /* repeat until collect n seconds */ 
                sec0 = get_seconds(); /* start timer */ 

                /*for (i=SAMPLE*stride;i!=0;i=i-1) {
                    for (index=0; index < limit; index=index+stride) {
                        x[index] = x[index] + 1; 
                    }
                }*/
                for(i=0;i<CACHE_MAX;i++) {                
                    to_acc[0] = to_acc[0] + 1;
                    to_acc[1] = to_acc[1] + 1;
                    to_acc[1] = to_acc[1] + 1;
                }

                steps = steps+1;

                //usleep(100);
                sec = sec + (get_seconds() - sec0);                
                //usleep(100);
 
            } while (sec < 0.5); /* until collect n second */ 
            tsteps = 0;
            do { /* repeat until collect n seconds */ 
                sec0 = get_seconds(); /* start timer */ 
                //for (index=0; index < limit; index=index+stride) {
                for(i=0;i<CACHE_MAX;i++) {
                    temp = temp + index; /* cache access */ 
                    temp = temp + index;
                    temp = temp + index;
                }
                //}
                tsteps = tsteps+1;
                //usleep(100);
                sec = sec - (get_seconds() - sec0);                
                //usleep(100);
 
            } while (tsteps < steps); /* until collect n second */ 
            
        //printf("%f,%d,%d,%d,%d,%d,%d,%d,%d\n",sec,steps,SAMPLE,stride,limit,stride,steps*SAMPLE*stride*(((limit-1)/stride)+1),(((limit-1)/stride)+1),steps*SAMPLE*stride);
        
        printf("%d:%4.0f\n",(int)time(NULL),(double)(sec*1000000000)/(steps*CACHE_MAX));
        fflush(stdout);
       }
    //}
}
    
