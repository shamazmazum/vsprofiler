#ifndef __PROFILER_LIB__H
#define __PROFILER_LIB__H

#include "vector.h"

void prof_init() __attribute__((constructor));
void prof_end()  __attribute__((destructor));

typedef size_t sample_t;
int prof_start();
int prof_stop();

extern vector_t samples_vector_;
extern vector_t *samples_vector;
extern elem_t max_samples;
extern suseconds_t sample_interval; // usec
extern int profile_all;

#endif
