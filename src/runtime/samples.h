#ifndef __SAMPLES_H__
#define __SAMPLES_H__

#include <stdint.h>

#define STACK_DEPTH 24
extern uintptr_t (*samples)[STACK_DEPTH];

uintptr_t* allocate_sample ();
uintptr_t* get_sample (size_t n);
void init_sample_array ();
void destroy_sample_array();

#endif
