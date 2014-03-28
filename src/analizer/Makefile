LISP_IMPL= sbcl --script
SRC = package.lisp analizer.lisp procmap-parser.lisp symbol-table.lisp
#LISP_IMPL= clisp

all: vsanalizer

clean:
	rm -f vsanalizer os-name.lisp-expr
.PHONY: clean

vsanalizer: $(SRC)
	sh os-name.sh
	$(LISP_IMPL) build.lisp
