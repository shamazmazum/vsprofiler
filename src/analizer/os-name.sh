#!/bin/sh

set -e

case `uname` in
#    Linux)
#       echo ":linux" > os-name.lisp-expr
#        ;;
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
