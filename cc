#!/bin/sh
LISP=ccl
case $LISP in
    *ccl)
        ccl -l driver.lisp -- $@
        ;;
    *sbcl)
        sbcl --end-toplevel-options $@
        ;;
esac
