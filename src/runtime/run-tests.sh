#!/bin/sh

set -e

./$RUNTESTS

NSMPLS="wc -l prof.smpl"

echo "Running profiler without saving backtrace..."
LD_PRELOAD=./$PROFLIB PROF_AUTOSTART=1 ./$EXAMPLE
if (test -s prof.map && test -s prof.smpl) then
    echo "Done. Number of samples:"
    wc -l prof.smpl
else
    echo "Test failure"
fi

rm prof.smpl prof.map
echo "Running profiler with saving backtrace..."
LD_PRELOAD=./$PROFLIB PROF_AUTOSTART=1 PROF_BACKTRACE=1 ./$EXAMPLE
if (test -s prof.map && test -s prof.smpl) then
    echo "Done. Number of samples:"
    wc -l prof.smpl
else
    echo "Test failure"
fi
