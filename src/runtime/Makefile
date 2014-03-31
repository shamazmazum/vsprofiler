SRCS = squeue.c profiler_lib.c profiler_lib_util.c
OBJS = $(SRCS:.c=.o)
TARGET = libvsprof.so
VERSION_SCRIPT = vsprof.ld

all: $(TARGET)

%.o: %.c
	$(CC) -c -fPIC -DWITH_DEBUG -O2 -o $@ $^

$(TARGET): $(OBJS)
	$(CC) -Wl,--version-script $(VERSION_SCRIPT) -shared -o $@ $^

clean:
	rm -f $(TARGET) *.o
