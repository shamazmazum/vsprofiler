#include <stdlib.h>
#include <strings.h>
#include "samples.h"
#include "profiler_lib.h"
#include "verbose.h"

uintptr_t (*samples)[STACK_DEPTH];
volatile static size_t count = 0;

void init_sample_array()
{
    samples = malloc (max_samples*STACK_DEPTH*sizeof(uintptr_t));
    bzero (samples, max_samples*STACK_DEPTH*sizeof(uintptr_t));
}

void destroy_sample_array ()
{
    free (samples);
}

/*
  We are inside a signal handler.
  No locks are allowed.
*/
uintptr_t* allocate_sample ()
{
    uintptr_t *sample = NULL;
    size_t current_count = __sync_fetch_and_add (&count, 1);
    if (current_count < max_samples) sample = samples[current_count];
    return sample;
}

uintptr_t* get_sample (size_t n)
{
    uintptr_t *res = NULL;
    if (n < max_samples) res = samples[n];
    return res;
}
