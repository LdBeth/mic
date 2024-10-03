(in-package #:mic-ast)


(defclass program ()
  ((symbol-table :initform :table)
   (content :initform :content :type (list function))))

(defclass function ()
  ((intputs :initform :args :type (list symbol))
   (body :initform :body :type (list node))))

(defclass node (ast)
  ((content :initform :type cons)))

