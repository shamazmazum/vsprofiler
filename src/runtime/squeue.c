#include <stdlib.h>
#include "squeue.h"
#include "verbose.h"

elem_t squeue_samples (squeue_t *queue) {return queue->nsmpl;}
elem_t squeue_entries (squeue_t *queue) {return queue->cur_entry_end - queue->cur_entry_beg;}

int squeue_init (squeue_t *queue)
{
    queue->nsmpl = 0;
    queue->cur_entry_beg = 0;
    queue->cur_entry_end = 0;
    queue->max_entries = START_ENTR;
    queue->data = malloc (START_ENTR*sizeof(smpl_t));
    return (queue->data == NULL) ? -1 : 0;
}

void squeue_free (squeue_t *queue)
{
    free (queue->data);
}

static void* check_space (squeue_t *queue)
{
    void *space = queue->data;
    if (!(queue->cur_entry_end < queue->max_entries))
    {
        PRINT_VERBOSE (2, "Doubling the size for sample queue %p\n", queue);
        queue->max_entries *= 2;
        size_t new_size = queue->max_entries*sizeof(smpl_t);
        space = realloc (space, new_size);
        if (space != NULL) queue->data = space;
    }
    return space;
}

int squeue_push_entry (squeue_t *queue, smpl_t entry)
{
    void *space = check_space (queue);
    if (space == NULL) return -1;
    queue->data[queue->cur_entry_end] = entry;
    queue->cur_entry_end++;
    return 0;
}

int squeue_pop_entry (squeue_t *queue, smpl_t *entry)
{
    if (queue->cur_entry_beg == queue->cur_entry_end)
    {
        PRINT_VERBOSE (2, "Nothing is in the queue %p\n", queue);
        return -1;
    }
    *entry = queue->data[queue->cur_entry_beg];
    queue->cur_entry_beg++;
    if (*entry == SAMPLE_TERM) queue->nsmpl--;
    return 0;
}

int squeue_finalize_sample (squeue_t *queue)
{
    if (squeue_push_entry (queue, SAMPLE_TERM)) return -1;
    queue->nsmpl++;
    return 0;
}
