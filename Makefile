GNUMAKE= gmake

all: src/runtime/libvsprof.so src/analizer/vsanalizer

src/runtime/libvsprof.so:
	$(GNUMAKE) -C src/runtime

src/analizer/vsanalizer:
	$(GNUMAKE) -C src/analizer

clean:
	$(GNUMAKE) -C src/runtime clean
	$(GNUMAKE) -C src/analizer clean
	$(GNUMAKE) -C tests clean

test: src/runtime/libvsprof.so src/analizer/vsanalizer
	$(GNUMAKE) -C tests test
	$(GNUMAKE) -C src/analizer test

.PHONY: clean src/analizer/vsanalizer src/runtime/libvsprof.so test
