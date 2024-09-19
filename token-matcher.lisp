(in-package #:mic-mpc)

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
  (?seq 
