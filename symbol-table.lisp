(in-package #:mic)

;;; The symbol table is the mic-symbols package
;; Each symbol has propterty 'type 'location
(defun intern (text &optional
                      location
                      (c-package (find-package '#:mic-symbols)))
  (let ((s (cl:intern text c-package)))
    (when location
      (setf (get s 'location) location))
    s))

