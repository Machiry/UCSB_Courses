/* cache_timer.c */ 
#include <stdio.h> 
#include <sys/types.h> 
#include <time.h> 
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
  
int main() 
{ 
    long register i, index, stride, limit, temp=0; 
    long steps, tsteps, csize; 
    long double sec0, sec; /* timing variables */ 
  
    for (csize=CACHE_MIN; csize <= CACHE_MAX; csize=csize*2) 
        for (stride=1; stride <= csize/2; stride=stride*2) {
            sec = 0;
            limit = csize - stride + 1;
            steps = 0; 
            do { /* repeat until collect n seconds */ 
                sec0 = get_seconds(); /* start timer */ 
                for (i=SAMPLE*stride;i!=0;i=i-1) {
                    for (index=0; index < limit; index=index+stride) {
                        x[index] = x[index] + 1; /* cache access */ 
                    }
                }
                steps = steps+1;
                sec = sec + (get_seconds() - sec0);                
 
            } while (sec < 4.0); /* until collect n second */ 
            tsteps = 0;
            do { /* repeat until collect n seconds */ 
                sec0 = get_seconds(); /* start timer */ 
                for (i=SAMPLE*stride;i!=0;i=i-1) {
                    for (index=0; index < limit; index=index+stride) {
                        temp = temp + index; /* cache access */ 
                    }
                }
                tsteps = tsteps+1;
                sec = sec - (get_seconds() - sec0);                
 
            } while (tsteps < steps); /* until collect n second */ 
            
        //printf("%f,%d,%d,%d,%d,%d,%d,%d,%d\n",sec,steps,SAMPLE,stride,limit,stride,steps*SAMPLE*stride*(((limit-1)/stride)+1),(((limit-1)/stride)+1),steps*SAMPLE*stride);
        
        printf("steps %d Size:%7d Stride:%7d read+write:%4.0f ns\n",steps,csize*sizeof(int),stride*sizeof(int),(double)(sec*1000000000)/(steps*SAMPLE*stride*((limit-1)/stride+1)));
    }
}
    
