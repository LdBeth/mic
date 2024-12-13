(defpackage #:org.sdf.ldbeth.minimal.intercal.c.lex
  (:nicknames #:mic-lex)
  (:use #:common-lisp)
  (:shadow #:keyword #:read-char #:type-of)
  (:export #:tokenizer #:lexing-error
           #:content #:token-content
           #:keyword #:identifier
           #:constant #:string-literal
           #:punctuator #:comment
           #:preprocessor))

(defpackage #:org.sdf.ldbeth.minimal.intercal.c.symbols
  (:nicknames #:mic-symbols))

(defpackage #:org.sdf.ldbeth.minimal.intercal.c.ast
  (:nicknames #:mic-ast)
  (:use #:common-lisp)
  (:shadow #:function)
  (:export #:c-number #:c-array))

(defpackage #:org.sdf.ldbeth.minimal.intercal.c.cst
  (:nicknames #:mic-cst)
  (:use #:common-lisp)
  (:shadow #:type)
  (:export #:cst
           #:expression #:binary-application #:unary-application
           #:type #:simple-type
           #:statement))

(defpackage #:org.sdf.ldbeth.minimal.intercal.c.parsec
  (:nicknames #:mic-pc)
  (:use #:common-lisp #:maxpc)
  (:export #:parse))

(defpackage #:org.sdf.ldbeth.minimal.intercal.c
  (:nicknames #:mic)
  (:use #:mic-lex #:mic-pc)
  (:export #:tokenizer #:lexing-error
           #:parse #:compiler-error))
