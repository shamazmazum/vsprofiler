#ifndef __DEBUG_H__
#define __DEBUG_H__

#ifdef WITH_DEBUG
#include <stdio.h>
#define PRINT_DEBUG(fmt, ...) printf(fmt, ##__VA_ARGS__)
#define PRINT_ERROR(fmt, ...) fprintf(stderr, fmt, ##__VA_ARGS__)
#else
#define PRINT_DEBUG(fmt, ...)
#define PRINT_ERROR(fmt, ...)
#endif

#endif
