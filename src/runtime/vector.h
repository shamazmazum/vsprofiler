#ifndef __VECTOR_H__
#define __VECTOR_H__

#include <unistd.h>

#define START_SIZE 3000

// For storing the number of element etc.
typedef unsigned int elem_t;

struct vector
{
    elem_t cur_element;
    elem_t max_elements;

    size_t mem_pos;
    size_t datasize;
    void *data;
};
typedef struct vector vector_t;

elem_t elements (vector_t*);
int push (vector_t*, void*);
int pop  (vector_t*, void*);
/*
int  insert_at (vector_t*, elem_t, void*);
void get_at    (vector_t*, elem_t);
*/

int  init_vector (vector_t*, size_t);
void free_vector (vector_t*);

#endif
