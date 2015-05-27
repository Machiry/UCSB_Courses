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
  
int main() 
{ 
    long register i, index, stride, limit, temp=0; 
    long steps, tsteps, csize; 
    long double sec0, sec; /* timing variables */ 
    long element_to_access = 0;
    csize = 1024*8;
    
  
        //for (stride=1; stride <= csize/2; stride=stride*2) {
        while(1){
            sec = 0;
            limit = csize - stride + 1;
            steps = 0; 
            printf("%p\n",&x[element_to_access]);
            do { /* repeat until collect n seconds */ 
                sec0 = get_seconds(); /* start timer */ 

                /*for (i=SAMPLE*stride;i!=0;i=i-1) {
                    for (index=0; index < limit; index=index+stride) {
                        x[index] = x[index] + 1; 
                    }
                }*/
                for(i=0;i<CACHE_MAX;i++) {                
                    x[element_to_access] = x[element_to_access] + 1;
                    x[element_to_access+1] = x[element_to_access+1] + 1;
                    x[element_to_access+1] = x[element_to_access+1] + 1;
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
        
        printf("read+write:%4.0f ns\n",(double)(sec*1000000000)/(steps*CACHE_MAX));
       }
    //}
}
    
