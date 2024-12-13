(defpackage #:org.sdf.ldbeth.minimal.intercal.c.lex
  (:nicknames #:mic-lex)
  (:use #:common-lisp)
  (:shadow #:keyword #:read-char #:type-of)
  (:export #:tokenizer #:cleanup-tokens #:lexing-error
           #:content #:token-content #:token-position #:token-data
           #:keyword #:identifier
           #:constant #:string-literal
           #:punctuator #:comment
           #:preprocessor))

(defpackage #:org.sdf.ldbeth.minimal.intercal.c.symbols
  (:nicknames #:mic-symbols))

(defpackage #:org.sdf.ldbeth.minimal.intercal.c.ast
  (:nicknames #:mic-ast)
  (:use #:common-lisp)
  (:shadow #:gensym)
  (:export #:pass-1 #:code-gen))

(defpackage #:org.sdf.ldbeth.minimal.intercal.c.parsec
  (:nicknames #:mic-pc)
  (:use #:common-lisp #:maxpc)
  (:export #:parse #:parsing-error))

(defpackage #:org.sdf.ldbeth.minimal.intercal.c
  (:nicknames #:mic)
  (:use #:mic-lex #:mic-pc)
  (:export #:tokenizer #:cleanup-tokens #:lexing-error #:parsing-error
           #:parse #:compiler-error))
