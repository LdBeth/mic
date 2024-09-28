(in-package #:mic-pc)

(defun ?keyword (keyword)
  "Matches if the token represents the keyword."
  (?fail))

(defun ?identifier ()
  (?fail))

(defun ?constant ()
  (?fail))

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

