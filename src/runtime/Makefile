SRCS = squeue.c profiler_lib.c profiler_lib_util.c
OBJS = $(SRCS:.c=.o)
TARGET = libvsprof.so

CFLAGS = -DWITH_DEBUG -O2
LFLAGS = -Wl,--version-script vsprof.ld

CUNIT_INCLUDE = -I/usr/local/include
CUNIT_LIB = -L/usr/local/lib -lcunit

all: $(TARGET)

%.o: %.c
	$(CC) -c -fPIC $(CFLAGS) -o $@ $^

$(TARGET): $(OBJS)
	$(CC) $(LFLAGS) -shared -o $@ $^

tests: $(OBJS) tests.c 
	$(CC) $(CUNIT_LIB) $(CUNIT_INCLUDE) -o $@ $^
example: example.c
	$(CC) -lm -o $@ $^

clean:
	rm -f $(TARGET) *.o tests example prof.map prof.smpl
.PHONY: clean

test: tests example $(TARGET) run-tests.sh
	RUNTESTS=tests EXAMPLE=example PROFLIB=$(TARGET) ./run-tests.sh
.PHONY: test
