LISP_IMPL= sbcl --script
#LISP_IMPL= clisp
SRC = package.lisp analizer.lisp procmap-parser.lisp symbol-table.lisp

all: vsanalizer

clean:
	rm -f vsanalizer os-name.lisp-expr
.PHONY: clean

vsanalizer: $(SRC)
	sh os-name.sh
	$(LISP_IMPL) build.lisp
