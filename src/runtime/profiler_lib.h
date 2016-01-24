#ifndef __PROFILER_LIB__H
#define __PROFILER_LIB__H

#include <sys/param.h>
#include <sys/time.h>

void prof_init() __attribute__((constructor));
void prof_end()  __attribute__((destructor));

int prof_start();
int prof_stop();

extern size_t max_samples;
extern suseconds_t sample_interval; // usec
extern int profile_all;
extern int save_backtrace;

#endif
