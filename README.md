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

Its minuses:
------------
* No multithreading support
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
      Self         Cumul                    Name        Object file
      7029          7029                     crc8 "/home/vasily/vsprofiler/src/runtime/example"
      2374          2374                   factor "/home/vasily/vsprofiler/src/runtime/example"
       350          9999                     main "/home/vasily/vsprofiler/src/runtime/example"
       246          2620                get_value "/home/vasily/vsprofiler/src/runtime/example"
         1             1       <Unknown function> "/lib/libc.so.7"
         0             1                   atexit "/lib/libc.so.7"
         0         10000                   _start "/home/vasily/vsprofiler/src/runtime/example"
         0         10000       <Unknown function> NIL

```

Numbers in the "Cumul" column are numbers of samples the function was on stack and in the "Self" column are numbers
of samples the function was on top of it.

Unfortunately, when the program was compiled with "bad" optimization flags (like -fomit-frame-pointer), the profiler
can even crash with this option enabled.

How to build/use:
----------------

Run (g)make from this directory. It will build src/runtime/libvsprof.so runtime library and src/analizer/vsanalizer
 program which is an analizer tool.

Now you can run your program with the profiler by preloading the library:
```
$ LD_PRELOAD=/path/to/libvsprof.so PROF_AUTOSTART=1 program_to_profile
```

It will create two files: prof.smpl and prof.map in the current working directory. Then run vsanalizer with these
two files as arguments (see below what "flat" means):
```
$ /path/to/vsanalizer prof.smpl prof.map flat
```

and get something like this:

```
      Self         Cumul                    Name        Object file
      4527          4527                        e "/home/vasily/test/test"
      3205          3205                        c "/home/vasily/test/test"
      2217          2217                        b "/home/vasily/test/test"
        51            51                        d "/home/vasily/test/test"

```

Another way to use the profiler is to link with libvsprof.so in build time and use
prof_start() and prof_stop() functions (declared in profiler_lib.h) to start and stop
profiler explicitly.

Analizer tool
------------

The analizer tool can be used as follows:

```
vsanalizer prof.smpl prof.map flat|graph [--strip-unknown t|nil] [--sorting-method self|cumul] [--output out]
```

The first two arguments are what the runtime library creates. The third mandatory argument specifies what kind of
report will be generated. "flat" creates reports you saw above, and "graph" creates reports you can visualize with
GraphViz. With --output key you can save your report to a file. --strip-unknown and --sorting-method are only usable
with flat reports. --strip-unknown hides functions profiler does not recognize and --sorting-method selects the
column (Self or Cumul) according to which rows will be sorted.
