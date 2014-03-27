#!/bin/sh

set -e

#LISP_IMPL="clisp"
LISP_IMPL="sbcl --script"
MAKE="gmake"

# Building runtime lib
$MAKE -C src/runtime

# Build analizer
case `uname` in
    Linux)
        echo ":linux" > os-name.lisp-expr
        ;;
    FreeBSD)
        echo ":freebsd" > os-name.lisp-expr
        echo ":bsd" >> os-name.lisp-expr
        ;;
    DragonFly)
        echo ":dragonfly" >  os-name.lisp-expr
        echo ":bsd" >>  os-name.lisp-expr
        ;;
    *)
        echo unsupported OS type: `uname`
        exit 1
        ;;
esac
if (test -f vsanalizer)
then
    echo "Nothing to build"
else
    $LISP_IMPL build.lisp
fi
