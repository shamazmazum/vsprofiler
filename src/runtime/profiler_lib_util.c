#include <sys/stat.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "profiler_lib.h"
#include "profiler_lib_util.h"

extern char **environ;

int copy_file (const char *in, const char *out)
{
    int copy_ok = 1;
    int fin = open (in, O_RDONLY);
    if (fin == -1) goto ret;
    int fout = open (out, O_WRONLY|O_CREAT|O_TRUNC, DEFFILEMODE);
    if (fout == -1) goto close_in;

#define BLOCK_SIZE 4096
    uint8_t buf[BLOCK_SIZE];
    ssize_t bytes_read, bytes_written;
    do
    {
        bytes_read = read (fin, buf, sizeof(buf));
        if (bytes_read < 0) {copy_ok = 0; goto close_out;}
        bytes_written = write (fout, buf, bytes_read);
        if (bytes_written != bytes_read) {copy_ok = 0; goto close_out;}
    }
    while (bytes_read == 0);

close_out:
    close (fout);
close_in: ;
    close (fin);
ret:
    return ((fin != -1) && (fout != -1) && copy_ok) ? 0 : -1;
}

void parse_parameters ()
{
    char **env;
    char *entry;
    for (env = environ; *env; env++)
    {
        entry = *env;
        if (strncmp ("MAX_SAMPLES=", entry, 12) == 0) max_samples = atoi (entry+12);
        if (strncmp ("SAMPLE_INTERVAL=", entry, 16) == 0) sample_interval = atoi (entry+16);
        if (strncmp ("PROF_AUTOSTART=", entry, 15) == 0) profile_all = atoi (entry+15);
    }
}
