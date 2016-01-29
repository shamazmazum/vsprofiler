#include <sys/stat.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vsprof.h"
#include "profiler_util.h"
#include "verbose.h"

extern char **environ;

int copy_file (const char *in, const char *out)
{
    int fin = open (in, O_RDONLY);
    if (fin == -1) goto err;
    int fout = open (out, O_WRONLY|O_CREAT|O_TRUNC, DEFFILEMODE);
    if (fout == -1) goto err;

#define BLOCK_SIZE 4096
    uint8_t buf[BLOCK_SIZE];
    ssize_t bytes_read, bytes_written;
    do
    {
        bytes_read = read (fin, buf, sizeof(buf));
        if (bytes_read < 0) goto err;
        bytes_written = write (fout, buf, bytes_read);
        if (bytes_written != bytes_read) goto err;
    }
    while (bytes_read != 0);
#undef BLOCK_SIZE

    close (fin);
    close (fout);
    return 0;

err:
    if (fin != -1) close (fin);
    if (fout != -1) close (fout);
    return -1;
}

void parse_parameters ()
{
    char **env;
    char *entry;
    for (env = environ; *env; env++)
    {
        entry = *env;
        if (strncmp ("MAX_SAMPLES=", entry, 12) == 0)
            max_samples = atoi (entry+12);
        else if (strncmp ("SAMPLE_INTERVAL=", entry, 16) == 0)
            sample_interval = atoi (entry+16);
        else if (strncmp ("PROF_AUTOSTART=", entry, 15) == 0)
            profile_all = atoi (entry+15);
        else if (strncmp ("PROF_BACKTRACE=", entry, 15) == 0)
            save_backtrace = atoi (entry+15);
        else if (strncmp ("PROF_VERBOSE=", entry, 13) == 0)
            verbose = atoi (entry+13);
    }
}

void get_output_names (char *samples, char *procmap)
{
    struct stat sb;
    int i, res;
    res = 0;
    for (i=0; !(res); i++)
    {
        sprintf (samples, "prof%i.smpl", i);
        sprintf (procmap, "prof%i.map", i);
        res =  stat (samples, &sb);
        res &= stat (procmap, &sb);
    }
}
