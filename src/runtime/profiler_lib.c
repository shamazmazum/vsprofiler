#include <sys/param.h>
#include <sys/time.h>
#include <sys/ucontext.h>
#include <signal.h>
#include <pthread_np.h>
#include <assert.h>

#include <stdlib.h>

#include "samples.h"
#include "profiler_lib.h"
#include "profiler_lib_util.h"
#include "verbose.h"

// Parameters shadowed by environment variables
size_t max_samples = 10000;
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

// FIXME: machine dependent
static void backtrace (ucontext_t *context, uintptr_t sample[])
{
    inside_backtrace = 1;
    uintptr_t *bp = (uintptr_t*)BP(context);
    int i = 2;
    while (bp)
    {
        if (i == STACK_DEPTH-1)
        {
            sucession_flags |= TOO_MANY_FRAMES;
            sample[i] = 0x0;
            break;
        }
        sample[i] = *(bp+1);
        bp = (uintptr_t*)*bp;
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
        SP(context) -= sizeof(uintptr_t);
        *((uintptr_t*)SP(context)) = BP(context);
    }
    if ((*ip == 0x48) && (*(ip+1) == 0x89) && (*(ip+2) == 0xe5)) /* mov %rsp,%rbp */
    {
        IP(context) += 3;
        BP(context) = SP(context);
        frames_restored++;
    }
    else if (*ip == 0xc3) /* retq */
    {
        IP(context) = *((uintptr_t*)SP(context));
        SP(context) += sizeof(uintptr_t);
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
    uintptr_t *sample = allocate_sample ();
    if (sample != NULL)
    {
        if (save_backtrace) restore_frame_if_needed (context);
        sample[0] = pthread_getthreadid_np ();
        sample[1] = IP(context);
        if (save_backtrace) backtrace (context, sample);
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
    PRINT_VERBOSE (1, "Parsing parameters\n");
    parse_parameters ();
    PRINT_VERBOSE (1,
                   "MAX_SAMPLES=%lu, SAMPLE_INTERVAL=%li, PROF_AUTOSTART=%i "
                   "PROF_BACKTRACE=%i PROF_VERBOSE=%i\n",
                   max_samples, sample_interval, profile_all, save_backtrace, verbose);
    PRINT_VERBOSE (1, "Capitalized ones are environment variables\n");
    
    PRINT_VERBOSE (2, "Initializing sample array\n");
    init_sample_array ();

    PRINT_VERBOSE (2, "Setting interrupt handlers\n");
    struct sigaction sa;
    sa.sa_sigaction = prof_signal_handler;
    sigemptyset (&sa.sa_mask);
    sa.sa_flags = SA_SIGINFO;
    sigaction (SIGPROF, &sa, NULL);

    sa.sa_handler = sigsegv_signal_handler;
    sa.sa_flags = SA_RESETHAND;
    sigemptyset (&sa.sa_mask);
    sigaction (SIGSEGV, &sa, NULL);

    if (profile_all)
    {
        PRINT_VERBOSE (1, "Running timer\n");
        if (prof_start ())
        {
            perror ("Cannot start timer");
            exit (EXIT_FAILURE);
        }
    }
}

static void save_samples (FILE *out)
{
    uintptr_t ip;
    int i,j;
    uintptr_t *sample;

    i = 0;
    while ((i < max_samples) &&
           ((sample = get_sample(i)) != NULL) &&
           (sample[0] != 0x0))
    {
        j = 0;
        while (sample[j] != 0x0)
        {
            fprintf (out, "0x%lx ", sample[j]);
            j++;
        }
        fprintf (out, "\n");
        assert (j != STACK_DEPTH);
        i++;
    }
}

void prof_end ()
{
    int res;
    PRINT_VERBOSE (1, "Stopping timer, anyway\n");
    prof_stop ();

    char samples[MAXPATHLEN];
    char procmap[MAXPATHLEN];
    get_output_names (samples, procmap);
    
    PRINT_VERBOSE (1, "Saving samples in %s\n", samples);
    FILE *out = fopen (samples, "w");
    if (out != NULL)
    {
        save_samples (out);
        fclose (out);
    }
    else PRINT_ERROR ("Cannot write sample file\n");

    PRINT_VERBOSE (1, "Saving process map in %s\n", procmap);
    res = copy_file (PROCMAP, procmap);
    if (res) PRINT_ERROR ("Cannot write map file\n");

    PRINT_VERBOSE (2, "Freeing sample queue\n");
    destroy_sample_array ();

    if (save_backtrace)
        PRINT_VERBOSE (2, "%i stack frames was restored during execution\n", frames_restored);
    if (sucession_flags & TOO_MANY_FRAMES)
        PRINT_VERBOSE (1,
                       "Control stack is too large or unsupported optimizations\n"
                       "were used. Backtrace may be incomplete or corrupt.\n");
}
