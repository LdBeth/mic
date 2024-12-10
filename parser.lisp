;;; list token -> tree
(in-package #:mic-cst)

(defclass cst ()
  ())

(defclass expression (cst)
  ())

(defclass simple-subexpression-mixin ()
  ((expr :initarg :expr :type expression)))

(defclass operator-mixin ()
  ((operator :initarg :op :type symbol)))

(defclass binary-application (expression operator-mixin)
   ((arg1 :initarg :left :type expression)
    (arg2 :initarg :right :type expression)))

(defclass unary-application (expression operator-mixin)
  ((arg :initarg :arg :type expression)))

(defclass function-application (expression)
  ((function-name :initarg :function :type identifier)
   (arglist :initarg :args :type cons)))

(defclass type (cst)
  ())

(defclass builtin-type (type)
  ((name :initarg :name :type symbol)))

;; TODO complex type expressions like function type or struct

(defclass statement ()
  ())

(defclass return (statement simple-subexpression-mixin)
  ())

(defclass side-effect (statement simple-subexpression-mixin)
  ())

(defclass block (statement)
  ())
