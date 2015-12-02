TESTS_CFLAGS = -I/usr/local/include -I../src/runtime
TESTS_LFLAGS = -L/usr/local/lib -lcunit

tests: tests.c ../src/runtime/squeue.c # Ugly
	$(CC) $(TESTS_CFLAGS) $(TESTS_LFLAGS) -o $@ $^

libexample.so: libexample.c
	$(CC) -shared -fPIC -o $@ $^

example: libexample.so example.c
	$(CC) -L. -lexample -o $@ example.c

clean:
	rm -rf libexample.so tests example

test: tests example
	./run-tests.sh

.PHONY: clean test
