(defpackage #:org.sdf.ldbeth.minimal.intercal.c
  (:nicknames #:mic)
  (:use #:common-lisp)
  (:shadow #:keyword #:read-char
           #:intern #:type-of)
  (:export #:tokenizer #:lexing-error))

(defpackage #:org.sdf.ldbeth.minimal.intercal.c.symbols
  (:nicknames #:mic-symbols))

(defpackage #:org.sdf.ldbeth.minimal.intercal.c.ast
  (:nicknames #:mic-ast)
  (:use #:common-lisp)
  (:shadow #:function) 
  (:export #:c-number #:c-array))

(defpackage #:org.sdf.ldbeth.minimal.intercal.c.cst
  (:nicknames #:mic-ast #:mic)
  (:use #:common-lisp)
  (:export #:cst
           #:expression #:binary-application #:unary-application
           #:type #:simple-type
           #:statement))

(defpackage #:org.sdf.ldbeth.minimal.intercal.c.parsec
  (:nicknames #:mic-pc)
  (:use #:common-lisp #:maxpc))

