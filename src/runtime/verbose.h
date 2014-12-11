#ifndef __DEBUG_H__
#define __DEBUG_H__

#include <stdio.h>
extern int verbose;
#define PRINT_VERBOSE(lvl, fmt, ...) if (verbose>=lvl) printf (fmt, ##__VA_ARGS__)
#define PRINT_ERROR(fmt, ...) fprintf(stderr, fmt, ##__VA_ARGS__)

#endif
