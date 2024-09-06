(defpackage #:org.sdf.ldbeth.minimal.intercal.c
  (:nicknames #:mic)
  (:use #:common-lisp)
  (:shadow #:keyword #:read-char)
  (:export #:tokenizer #:lexing-error))

(defpackage #:org.sdf.ldbeth.minimal.intercal.c.symbols
  (:nicknames #:mic-symbols))

(defpackage #:org.sdf.ldbeth.minimal.intercal.c.parsec
  (:nicknames #:mic-pc)
  (:use #:common-lisp #:maxpc))

