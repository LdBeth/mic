(in-package #:mic)

;;; The symbol table is the mic-symbols package
;; Each symbol has propterty 'type 'location
(defun intern (text &optional location)
  (let ((s (cl:intern text (find-package '#:mic-symbols))))
    (when location
      (setf (get s 'location) location))
    s))
