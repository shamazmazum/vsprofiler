VSProfiler
==========

VSProfiler (Vasily's statistical profiler) is a simple statistical profiler for Unix-like systems (currently FreeBSD
and DragonFlyBSD are supported, but is is easy to add support for e.g. Linux). Its primary goal is to make it
possible to profile programs without recompilation.

Its pluses:
-----------
    * No need to recompile program to profile (unlike gprof)
    * Supports shared libraries
    * It's simple 

Its minuses:
------------
    * No multithreading support
    * Cannot build the call graph, only prints flat report (by now...)
    * Small amount of supported platforms (FreeBSD/DragonFlyBSD on x86/x86-64 machine)
    * Requires Common Lisp implementation for analizer (clisp or sbcl will do) with cl-elf and esrap 
      (sorry, CL is my weakness)
    * Cannot calculate precise number of calls to function (it's very hard in C without recompilation)

UPD: Vorsicht!
-------------
You can also analize stack when each sample is taken. To do so run your program with PROF_BACKTRACE environment 
variable set to non-zero like so:
```
$ LD_PRELOAD=/path/to/libvsprof.so PROF_AUTOSTART=1 PROF_BACKTRACE=1 program_to_profile
```

You will get a report like this:
```
(#S(VSANALIZER::REPORT-ENTRY
    :ID 34370456112
    :SELF 381
    :CUMUL 381
    :NAME "log in /lib/libm.so.5")
 #S(VSANALIZER::REPORT-ENTRY
    :ID 4196464
    :SELF 51
    :CUMUL 421
    :NAME "func in /home/vasily/mycprofiler/test")
 #S(VSANALIZER::REPORT-ENTRY
    :ID 4196496
    :SELF 28
    :CUMUL 454
    :NAME "dich in /home/vasily/mycprofiler/test")
 #S(VSANALIZER::REPORT-ENTRY
    :ID 4195584
    :SELF 5
    :CUMUL 5
    :NAME "<Unknown function at address 400500>")
 #S(VSANALIZER::REPORT-ENTRY
    :ID 34366148608
    :SELF 0
    :CUMUL 465
    :NAME "<Unknown function at address 80061D000>")
 #S(VSANALIZER::REPORT-ENTRY
    :ID 4195744
    :SELF 0
    :CUMUL 465
    :NAME "_start in /home/vasily/mycprofiler/test")
 #S(VSANALIZER::REPORT-ENTRY
    :ID 4195632
    :SELF 0
    :CUMUL 465
    :NAME "main in /home/vasily/mycprofiler/test"))
```

The "CUMUL" slot is the number of samples the function was on stack and the "SELF" slot is the number of samples
the function was on top of it.

Unfortunately, when the program was compiled with "bad" optimization flags (like -fomit-frame-pointer), the profiler
can even crash with this option enabled.

How to build/use:
----------------

Run (g)make from this directory. It will build src/runtime/libvsprof.so runtime library and src/analizer/vsanalizer
 program which is analizer tool.

Now you can run your program with the profiler by preloading the library:
```
$ LD_PRELOAD=/path/to/libvsprof.so PROF_AUTOSTART=1 program_to_profile
```

It will create two files: prof.smpl and prof.map in the current working directory. Then run vsanalizer with these
two files as arguments:
```
$ /path/to/vsanalizer prof.smpl prof.map
```

and get something like this:

```
(#S(VSANALIZER::REPORT-ENTRY
    :NAME "<Unknown function at address 400534>"
    :FILE "/home/vasily/mycprofiler/test"
    :COUNT 7)
 #S(VSANALIZER::REPORT-ENTRY :NAME "log" :FILE "/lib/libm.so.5" :COUNT 207)
 #S(VSANALIZER::REPORT-ENTRY
    :NAME "func"
    :FILE "/home/vasily/mycprofiler/test"
    :COUNT 27)
 #S(VSANALIZER::REPORT-ENTRY
    :NAME "dich"
    :FILE "/home/vasily/mycprofiler/test"
    :COUNT 72))
```

(I've got this from testingprog.c in this directory)

Another way to use the profiler is to link with libvsprof.so in build time and use
prof_start() and prof_stop() functions (declared in profiler_lib.h) to start and stop
profiler explicitly.
