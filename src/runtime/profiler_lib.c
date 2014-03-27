#include <sys/time.h>
#include <sys/ucontext.h>
#include <sys/param.h>
#include <signal.h>

#include <stdlib.h>
#include <stdio.h>

#include "profiler_lib.h"
#include "profiler_lib_util.h"
#include "debug.h"

vector_t samples_vector_;
vector_t *samples_vector = &samples_vector_;
elem_t max_samples = 10000;
suseconds_t sample_interval = 1000; // usec
int profile_all = 0;

// FIXME: machine dependent
static void prof_signal_handler (int signal, siginfo_t *info, ucontext_t *context)
{
    if (elements (samples_vector) < max_samples)
    {
        mcontext_t regs = context->uc_mcontext;
        size_t rip = regs.mc_rip;
        push (samples_vector, &rip);
    }
}

int prof_start ()
{
    struct itimerval val;
    val.it_interval.tv_sec  = 0;
    val.it_interval.tv_usec = sample_interval;
    val.it_value.tv_sec  = 0;
    val.it_value.tv_usec = sample_interval;

    if (setitimer (ITIMER_PROF, &val, NULL)) return -1;
    return 0;
}

int prof_stop ()
{
    struct itimerval val;
    val.it_interval.tv_sec  = 0;
    val.it_interval.tv_usec = 0;
    val.it_value.tv_sec  = 0;
    val.it_value.tv_usec = 0;

    if (setitimer (ITIMER_PROF, &val, NULL)) return -1;
    return 0;
}

void prof_init ()
{
    int res;
    PRINT_DEBUG ("Parse parameters\n");
    parse_parameters ();
    PRINT_DEBUG ("max_samples=%i, sample_interval=%li, profile_all=%i\n",
                 max_samples, sample_interval, profile_all);
    PRINT_DEBUG ("Initializing sample vector\n");
    res = init_vector (samples_vector, sizeof (sample_t));
    if (res) abort();
    PRINT_DEBUG ("Setting interrupt handler\n");
    struct sigaction sa;
    sa.sa_sigaction = prof_signal_handler;
    sigemptyset (&sa.sa_mask);
    sa.sa_flags = SA_SIGINFO;
    res = sigaction (SIGPROF, &sa, NULL);
    if (res) abort();
    if (profile_all)
    {
        PRINT_DEBUG ("Running timer\n");
        if (prof_start ()) abort ();
    }
}

static int save_samples (FILE *out)
{
    int i;
    size_t rip;
    while (elements (samples_vector) != 0)
    {
        pop (samples_vector, &rip);
        fprintf (out, "%p\n", rip);
    }
    return 0;
}

void prof_end ()
{
    int op_res;
    PRINT_DEBUG ("Stopping timer, anyway\n");
    if (prof_stop ()) abort();
    
    PRINT_DEBUG ("Saving results\n");
    FILE *out = fopen ("prof.smpl", "w");
    op_res = (out == NULL) ? -1 : 0;
    if (!op_res) op_res = op_res || save_samples (out);
    fclose (out);
    if (op_res) PRINT_ERROR ("Cannot write sample file\n");

    PRINT_DEBUG ("Saving process map\n");
    op_res = copy_file ("/proc/curproc/map", "prof.map");
    if (op_res) PRINT_ERROR ("Cannot write map file\n");

    PRINT_DEBUG ("Freeing sample vector\n");
    free_vector (samples_vector);
}
