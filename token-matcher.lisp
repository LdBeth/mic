(in-package #:mic-pc)

(defmacro token-test (arg type &body body)
  `(lambda (,arg)
     (and (typep ,arg type)
          (progn
            ,@body))))

(defun ?keyword (keyword)
  "Matches if the token represents the keyword."
  (?satisfies (token-test o 'mic:keyword
                (eq (mic:token-content o) keyword))))

(defun ?identifier ()
  (?satisfies (token-test o 'mic:keyword)))

(defun ?constant ()
  (?satisfies (token-test o 'mic:constant)))

(defun =identifier ()
  (%and (?identifier)
        (=element)))

(defun =constant ()
  (%and (?constant)
        (=element)))

(defun =program ()
  (=subseq (%any (%or =declaration =function))))


(defun =declaration ()
  (=destructure (type var _ expr)
                (=list (=type) (=var)
                       (?op '=) (=expr)
                       (?op '\;))
    (make-instance 'declaration :type type :var var :expr expr)))

