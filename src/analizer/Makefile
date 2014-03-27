LISP_IMPL= sbcl --script
#LISP_IMPL= clisp

all: vsanalizer

vsanalizer:
	sh os-name.sh
	$(LISP_IMPL) build.lisp

clean:
	rm vsanalizer os-name.lisp-expr
.PHONY: clean
