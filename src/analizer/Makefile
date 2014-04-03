LISP_IMPL?= sbcl --script
#LISP_IMPL= clisp
SRC = package.lisp analizer.lisp input-parser.lisp symbol-table.lisp build.lisp

all: vsanalizer

clean:
	rm -f vsanalizer os-name.lisp-expr
.PHONY: clean

vsanalizer: $(SRC) os-name.sh
	sh os-name.sh
	$(LISP_IMPL) build.lisp
