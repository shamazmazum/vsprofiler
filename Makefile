GNUMAKE= gmake

all: src/runtime/libvsprof.so src/analizer/vsanalizer

src/runtime/libvsprof.so:
	$(GNUMAKE) -C src/runtime

src/analizer/vsanalizer:
	$(GNUMAKE) -C src/analizer

clean:
	$(GNUMAKE) -C src/runtime clean
	$(GNUMAKE) -C src/analizer clean
.PHONY: clean
