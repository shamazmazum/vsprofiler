#!/bin/sh

set -e

./$RUNTESTS

rm -f prof0.smpl prof0.map
echo "Running profiler without saving backtrace..."
LD_PRELOAD=./$PROFLIB PROF_AUTOSTART=1 PROF_VERBOSE=2 ./$EXAMPLE
if (test -s prof0.map && test -s prof0.smpl) then
    echo -e "\e[32m"
    echo "Done. Number of samples:"
    wc -l prof0.smpl
    echo -e "\e[0m"
else
    echo -e "\e[31mTest failed\e[0m"
fi

rm -f prof1.smpl prof1.map
echo "Running profiler with saving backtrace..."
LD_PRELOAD=./$PROFLIB PROF_AUTOSTART=1 PROF_BACKTRACE=1 PROF_VERBOSE=2 ./$EXAMPLE
if (test -s prof1.map && test -s prof1.smpl) then
    echo -e "\e[32m"
    echo "Test passed. Number of samples:"
    wc -l prof1.smpl
    echo -e "\e[0m"
else
    echo -e "\e[31mTest failed\e[0m"
fi
