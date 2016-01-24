libexample.so: libexample.c
	$(CC) -shared -fPIC -o $@ $^

example: libexample.so example.c
	$(CC) -L. -lexample -o $@ example.c

clean:
	rm -rf libexample.so tests example

test: example
	./run-tests.sh

.PHONY: clean test
