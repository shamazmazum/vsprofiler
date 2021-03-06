VSProfiler
==========

VSProfiler (Vasily's statistical profiler) is a simple statistical profiler for Unix-like systems (currently FreeBSD
and DragonFlyBSD are supported, but it is easy to add support for e.g. Linux). Its primary goal is to make it
possible to profile programs without recompilation.

Its pluses:
-----------
* No need to recompile program to profile (unlike gprof)
* Supports shared libraries
* It's simple
* Graphviz understandable output
* Can profile threaded programs (experimental)

Its minuses:
------------
* Small amount of supported platforms (FreeBSD/DragonFlyBSD/NetBSD on x86-64 machine)
* Requires Common Lisp implementation for analizer (clisp or sbcl will do) with cl-elf and esrap 
  (sorry, CL is my weakness)
* Cannot calculate precise number of calls to function (it's very hard in C without recompilation)

How to build/use:
----------------

Run the following:

    mkdir build
    cd build
    cmake -DLISP_IMPL=your_lisp_implementation ..
    make

It will build src/runtime/libvsprof.so runtime library and src/analizer/vsanalizer program which is an analizer
tool. The lisp implementation defaults to SBCL.

Now you can run your program with the profiler by preloading the library:
```
$ LD_PRELOAD=/path/to/libvsprof.so PROF_AUTOSTART=1 program_to_profile
```

It will create two files: profX.smpl and profX.map in the current working directory, where X is some number. Then
run vsanalizer with these two files as arguments:
```
$ /path/to/vsanalizer flat prof.smpl prof.map
```

and get something like this:

```
      Self         Cumul                    Name        Object file
      4527          4527                        e "/home/vasily/test/test"
      3205          3205                        c "/home/vasily/test/test"
      2217          2217                        b "/home/vasily/test/test"
        51            51                        d "/home/vasily/test/test"

```

Another way to use the profiler is to link against libvsprof.so in build time and use
prof_start() and prof_stop() functions (declared in vsprof.h) to start and stop
profiler explicitly.

The runtime library
------------------
The runtime library, named libvsprof.so is what you need to link to a program you want to profile. As mentioned
above, it can be done either in build time or with LD_PRELOAD environment variable. The library understands
following additional environment variables:

* ```MAX_SAMPLES``` is used to specify maximum limit of samples. Defaults to 10000
* ```SAMPLE_INTERVAL``` is an interval in microseconds between two samples taken. Defaults to 1000 (1ms)
* ```PROF_AUTOSTART``` starts profiling immediately when set to nonzero. Otherwise the program must call
  prof_start() and prof_stop() explicitly.
* ```PROF_BACKTRACE``` enables saving backtrace for each sample when set to nonzero, so you can print graph
  reports.
* ```PROF_VERBOSE``` is the verbosity level. Can be 0, 1 or 2.

Note, that ```PROF_BACKTRACE``` will not work and likely crash your program if it (or any system library used,
including libc) is compilled with -fomit-frame-pointer optimization.

The analizer tool
----------------

The analizer tool can be used as follows:

    vsanalizer flat [--sorting-method cumul|self] [--strip-unknown] prof.smpl prof.map
    vsanalizer graph [--strip-unknown] prof.smpl prof.map
    vsanalizer histogram <func-name> prof.smpl prof.map

The first mandatory argument is a type of report being produced, prof.smpl and prof.map are what the
runtime library creates. Report types can be ```flat```, ```graph``` or ```histogram```. Additionly
you can use the following arguments:

* ```--strip-unknown```. Do not show functions profiler does not recognize (like there is no symbol for
  it in the symbol table).
* ```--sorting-method``` selects the column (```Self``` or ```Cumul```) according to which rows will be sorted in the
  flat report.

A note on what exactly ```Self``` and ```Cumul``` mean. ```Self``` means the number of samples when a function was
on top of the stack. ```Cumul``` means the number of samples when function was on stack regarless to its
position. ```Self``` is always less or equal to ```Cumul```. ```main``` function usually has the biggest
```Cumul``` unless there are 'heavy' recursive functions.

A word or two on report types: ```flat``` report is what you saw above, ```graph``` report prints a
call graph in format understandable by GraphViz, ```histogram``` works with a particular function
and prints for each instruction address the number of samples for which this address were stored in
instruction pointer.

Debug messages
--------------

You may see the following message if your verbosity level is greater or equal to 1.

    Control stack is too large or unsupported optimizations
    were used. Backtrace may be incomplete or corrupt.

To fix this try to set STACK_DEPTH in src/runtime/samples.h to 24 (or any other multiple of 8 greater than 16). I plan
to add an environment variable for this later.

Also you will see error messages if sample or map file cannot be saved in current directory.

How to port
-----------

If you want this tool in, in example, Linux you can easily port it (as to any other POSIX OS with procfs). First
of all, you need to provide the runtime library with ```PROCMAP```, ```IP```, ```BP``` and ```SP``` macros (see
```src/runtime/profiler_lib.c```). The three last point where rip, rbp and rsp registers are saved in
context. Then you should create rules to parse map files in ```src/analizer/input-parser.lisp```. It should work
then.
