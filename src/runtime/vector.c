#include <stdlib.h>
#include <string.h>
#include "vector.h"
#include "debug.h"

elem_t elements (vector_t *vect) {return vect->cur_element;}

int init_vector (vector_t *vect, size_t elem_size)
{
    vect->cur_element = 0;
    vect->max_elements = START_SIZE;
    vect->mem_pos = 0;
    vect->datasize = elem_size;
    vect->data = malloc (START_SIZE*elem_size);
    if (vect->data == NULL)
    {
        PRINT_ERROR ("Cannot allocate memory for samples vector\n");
        return -1;
    }
    else return 0;
}

void free_vector (vector_t *vect)
{
    free (vect->data);
}

static void* check_space (vector_t *vect)
{
    void *space = vect->data;
    if (!(vect->cur_element < vect->max_elements))
    {
        PRINT_DEBUG ("Doubling the size for vector %p\n", vect);
        size_t new_size = 2*vect->mem_pos;
        vect->max_elements *= 2;
        space = realloc (space, new_size);
        if (space != NULL) vect->data = space;
        else PRINT_ERROR ("Cannot add memory for samples vector\n");
    }
    return space;
}

int push (vector_t *vect, void *data)
{
    void *space = check_space (vect);
    if (space == NULL) return -1;
    memcpy (vect->data+vect->mem_pos, data, vect->datasize);
    vect->cur_element++;
    vect->mem_pos += vect->datasize;
    return 0;
}

int pop (vector_t *vect, void *data)
{
    if ((vect->cur_element == 0) || (vect->mem_pos == 0))
    {
        PRINT_ERROR ("Nothing in the vector %p\n", vect);
        return -1;
    }
    vect->cur_element--;
    vect->mem_pos -= vect->datasize;
    memcpy (data, vect->data+vect->mem_pos, vect->datasize);
    return 0;
}
