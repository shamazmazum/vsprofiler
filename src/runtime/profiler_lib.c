#include <sys/time.h>
#include <sys/ucontext.h>
#include <sys/param.h>
#include <signal.h>

#include <stdlib.h>
#include <stdio.h>

#include "profiler_lib.h"
#include "profiler_lib_util.h"
#include "debug.h"

static squeue_t sample_queue_;
static squeue_t *sample_queue = &sample_queue_;
elem_t max_samples = 10000;
suseconds_t sample_interval = 1000; // usec
int profile_all = 0;
int save_backtrace = 0;

#if defined __x86_64__
#define IP(ctx) ctx->uc_mcontext.mc_rip
#define BP(ctx) ctx->uc_mcontext.mc_rbp
#elif defined __i386
#define IP(ctx) ctx->uc_mcontext.mc_eip
#define BP(ctx) ctx->uc_mcontext.mc_ebp
#else
#error "Unsupported platform"
#endif

#define PUSH_DEBUG(queue,val) if (squeue_push_entry (queue, val)) \
        PRINT_ERROR ("Cannot push value in queue %p", sample_queue)

#define MAX_STACK_DEPTH 40

// FIXME: machine dependent

static void backtrace (ucontext_t *context)
{
    smpl_t *bp = (smpl_t*)BP(context);
    int i = 0;
    while (bp && (i < MAX_STACK_DEPTH))
    {
        PUSH_DEBUG (sample_queue, *(bp+1));
        bp = (smpl_t*)*bp;
        i++;
    }
    if (i == MAX_STACK_DEPTH) PRINT_DEBUG ("Control stack is too large or unsupported optimizations were used\n");
}

static void prof_signal_handler (int signal, siginfo_t *info, ucontext_t *context)
{
    if (squeue_samples (sample_queue) < max_samples)
    {
        smpl_t ip = IP(context);
        PUSH_DEBUG (sample_queue, ip);
        if (save_backtrace) backtrace (context);
        if (squeue_finalize_sample (sample_queue))
            PRINT_ERROR ("Cannot finalize sample in queue %p\n", sample_queue);
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
    PRINT_DEBUG ("max_samples=%i, sample_interval=%li, profile_all=%i save_backtrace=%i\n",
                 max_samples, sample_interval, profile_all, save_backtrace);
    PRINT_DEBUG ("Initializing sample queue\n");
    res = squeue_init (sample_queue);
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
    smpl_t ip;
    while (squeue_entries (sample_queue) != 0)
    {
        if (squeue_pop_entry (sample_queue, &ip))
            PRINT_ERROR ("Cannot get value from queue %p\n", sample_queue);
        fprintf (out, "%p%c", (void*)ip, (ip == SAMPLE_TERM) ? '\n' : ' ');
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

    PRINT_DEBUG ("Freeing sample queue\n");
    squeue_free (sample_queue);
}
