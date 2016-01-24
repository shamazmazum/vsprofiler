SRCS = samples.c profiler_lib.c profiler_lib_util.c
OBJS = $(SRCS:.c=.o)
TARGET = libvsprof.so

CFLAGS = -DWITH_DEBUG -O2
LFLAGS = -Wl,--version-script vsprof.ld

all: $(TARGET)

%.o: %.c
	$(CC) -c -fPIC $(CFLAGS) -o $@ $^

$(TARGET): $(OBJS)
	$(CC) $(LFLAGS) -shared -lpthread -o $@ $^

clean:
	rm -f $(TARGET) *.o tests example prof*.map prof*.smpl
.PHONY: clean
