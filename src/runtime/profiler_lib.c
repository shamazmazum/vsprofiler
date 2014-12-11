#include <sys/time.h>
#include <sys/ucontext.h>
#include <sys/param.h>
#include <signal.h>

#include <stdlib.h>

#include "profiler_lib.h"
#include "profiler_lib_util.h"
#include "verbose.h"

// Sample queue
static squeue_t sample_queue_;
static squeue_t *sample_queue = &sample_queue_;
// Parameters shadowed by environment variables
elem_t max_samples = 10000;
suseconds_t sample_interval = 1000; // usec
int profile_all = 0; // Start profiling timer implicitly
int save_backtrace = 0; // Save content of the stack too (experimental)
int verbose = 1;
// Other
static int inside_backtrace = 0; // Is control inside function backtrace()?
static int frames_restored = 0; // Number of restored stack frames
static int sucession_flags = 0; // Contains any prombles encountered

#define TOO_MANY_FRAMES  0x01

#if defined __x86_64__
#if defined(__DragonFly__) || defined(__FreeBSD__)
#define PROCMAP "/proc/curproc/map"
#define IP(ctx) ctx->uc_mcontext.mc_rip
#define BP(ctx) ctx->uc_mcontext.mc_rbp
#define SP(ctx) ctx->uc_mcontext.mc_rsp
#define SUPPORTED_PLATFORM
#elif defined(__NetBSD__)
#define PROCMAP "/proc/curproc/maps"
#define IP(ctx) ctx->uc_mcontext.__gregs[_REG_RIP]
#define BP(ctx) ctx->uc_mcontext.__gregs[_REG_RBP]
#define SP(ctx) ctx->uc_mcontext.__gregs[_REG_RSP]
#define SUPPORTED_PLATFORM
#endif // defined(__NetBSD__)
#endif // defined __x86_64__

#ifndef SUPPORTED_PLATFORM
#error "Unsupported platform"
#else
#undef SUPPORTED_PLATFORM
#endif

#define PUSH(queue,val) if (squeue_push_entry (queue, val)) \
        PRINT_ERROR ("Cannot push value in queue %p", sample_queue)

#define MAX_STACK_DEPTH 40

// FIXME: machine dependent
static void backtrace (ucontext_t *context)
{
    inside_backtrace = 1;
    smpl_t *bp = (smpl_t*)BP(context);
    int i = 0;
    while (bp)
    {
        if (i == MAX_STACK_DEPTH)
        {
            sucession_flags |= TOO_MANY_FRAMES;
            break;
        }
        PUSH (sample_queue, *(bp+1));
        bp = (smpl_t*)*bp;
        i++;
    }
    inside_backtrace = 0;
}

static void restore_frame_if_needed (ucontext_t *context)
{
    // Fix the situations where we've just entered in a new function and
    // the stack frame was not properly created (or we are ready to leave
    // and the frame is already destroyed)
    // Works just for x86-64, of course.
    unsigned char *ip = (void*)IP(context);
    
    // KLUDGE: There can be other instructions between push %rbp and mov %rsp,%rbp
    // But this type of prologue is the most common.
    
    // Not really restores frame pointer. Works together with the next case
    if (*ip == 0x55) /* push %rbp */
    {
        IP(context)++;
        ip = (void*)IP(context);
        SP(context) -= sizeof(smpl_t);
        *((smpl_t*)SP(context)) = BP(context);
    }
    if ((*ip == 0x48) && (*(ip+1) == 0x89) && (*(ip+2) == 0xe5)) /* mov %rsp,%rbp */
    {
        IP(context) += 3;
        BP(context) = SP(context);
        frames_restored++;
    }
    else if (*ip == 0xc3) /* retq */
    {
        IP(context) = *((smpl_t*)SP(context));
        SP(context) += sizeof(smpl_t);
        frames_restored++;
    }
}

// FIXME: what if running program will replace this handler by its own?
// Can it be fixed?
static void sigsegv_signal_handler (int signal)
{
    if (inside_backtrace)
    {
        PRINT_ERROR ("\n--------------------\n");
        PRINT_ERROR ("Segmentation fault while saving backtrace!\n");
        PRINT_ERROR ("This error can mean what your program was build with unsupported optimization\n");
        PRINT_ERROR ("flags (like -fomit-frame-pointer). Try to run profiler without PROF_BACKTRACE\n");
        PRINT_ERROR ("\n--------------------\n");
    }
}

static void prof_signal_handler (int signal, siginfo_t *info, ucontext_t *context)
{
    if (squeue_samples (sample_queue) < max_samples)
    {
        if (save_backtrace)
        {
            restore_frame_if_needed (context);
            PUSH (sample_queue, IP(context));
            backtrace (context);
        }
        else PUSH (sample_queue, IP(context));
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

    return setitimer (ITIMER_PROF, &val, NULL);
}

int prof_stop ()
{
    struct itimerval val;
    val.it_interval.tv_sec  = 0;
    val.it_interval.tv_usec = 0;
    val.it_value.tv_sec  = 0;
    val.it_value.tv_usec = 0;

    return setitimer (ITIMER_PROF, &val, NULL);
}

void prof_init ()
{
    int res;
    PRINT_VERBOSE (1, "Parsing parameters\n");
    parse_parameters ();
    PRINT_VERBOSE (1, "max_samples=%i, sample_interval=%li, profile_all=%i save_backtrace=%i verbose=%i\n",
                   max_samples, sample_interval, profile_all, save_backtrace, verbose);
    
    PRINT_VERBOSE (2, "Initializing sample queue\n");
    res = squeue_init (sample_queue);
    if (res)
    {
        PRINT_ERROR ("Cannot allocate memory for sample queue\n");
        exit (EXIT_FAILURE);
    }
    
    PRINT_VERBOSE (2, "Setting interrupt handlers\n");
    struct sigaction sa;
    sa.sa_sigaction = prof_signal_handler;
    sigemptyset (&sa.sa_mask);
    sa.sa_flags = SA_SIGINFO;
    res = sigaction (SIGPROF, &sa, NULL);
    if (res)
    {
        PRINT_ERROR ("Cannot set signal handler\n");
        exit (EXIT_FAILURE);
    }

    sa.sa_handler = sigsegv_signal_handler;
    sa.sa_flags = SA_RESETHAND;
    sigemptyset (&sa.sa_mask);
    res = sigaction (SIGSEGV, &sa, NULL);
    if (res)
    {
        PRINT_ERROR ("Cannot set signal handler\n");
        exit (EXIT_FAILURE);
    }

    if (profile_all)
    {
        PRINT_VERBOSE (1, "Running timer\n");
        if (prof_start ())
        {
            PRINT_ERROR ("Cannot start timer\n");
            exit (EXIT_FAILURE);
        }
    }
}

static int save_samples (FILE *out)
{
    int i;
    smpl_t ip;
    while (squeue_entries (sample_queue) != 0)
    {
        if (squeue_pop_entry (sample_queue, &ip))
        {
            PRINT_ERROR ("Cannot get value from queue %p\n", sample_queue);
            return -1;
        }
        fprintf (out, "0x%lx%c", ip, (ip == SAMPLE_TERM) ? '\n' : ' ');
    }
    return 0;
}

void prof_end ()
{
    int op_res;
    PRINT_VERBOSE (1, "Stopping timer, anyway\n");
    if (prof_stop ())
    {
        PRINT_ERROR ("Cannot stop timer\n");
        exit (EXIT_FAILURE);
    }

    char samples[256];
    char procmap[256];
    get_output_names (samples, procmap);
    
    PRINT_VERBOSE (1, "Saving samples in %s\n", samples);
    FILE *out = fopen (samples, "w");
    op_res = (out == NULL) ? -1 : 0;
    if (!op_res)
    {
        op_res = op_res || save_samples (out);
        fclose (out);
    }
    if (op_res) PRINT_ERROR ("Cannot write sample file\n");

    PRINT_VERBOSE (1, "Saving process map in %s\n", procmap);
    op_res = copy_file (PROCMAP, procmap);
    if (op_res) PRINT_ERROR ("Cannot write map file\n");

    PRINT_VERBOSE (2, "Freeing sample queue\n");
    squeue_free (sample_queue);

    if (save_backtrace)
        PRINT_VERBOSE (2, "%i stack frames was restored during execution\n", frames_restored);
    if (sucession_flags & TOO_MANY_FRAMES)
        PRINT_VERBOSE (2, "Control stack is too large or unsupported optimizations were used\n");
}
