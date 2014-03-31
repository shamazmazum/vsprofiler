#ifndef __SQUEUE_H__
#define __SQUEUE_H__

#define START_ENTR 3000
#define SAMPLE_TERM 0x0

// For storing the number of element etc.
typedef unsigned int elem_t;
typedef long smpl_t;

struct squeue
{
    elem_t nsmpl;
    elem_t cur_entry_beg;
    elem_t cur_entry_end;
    elem_t max_entries;

    smpl_t *data;
};
typedef struct squeue squeue_t;

elem_t squeue_samples (squeue_t*);
elem_t squeue_entries (squeue_t*);
int squeue_push_entry (squeue_t*, smpl_t);
int squeue_pop_entry  (squeue_t*, smpl_t*);
int squeue_finalize_sample (squeue_t*);

int  squeue_init (squeue_t*);
void squeue_free (squeue_t*);

#endif
