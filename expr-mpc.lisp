(in-package #:mic-pc)
;;; This is a propotype file.

(defun ?value ()
  (?satisfies 'numberp))

(defun =value ()
  (%and (?value) (=element)))

(defun =paren-expr ()
  (=destructure (_ a _)
                (=list (?eq '\() 'rec/=term (?eq '\)))))

(defun =prime-expr ()
  (%or (=value) (=paren-expr)))

(defun =term ()
   (%or
    (=destructure (a op b)
                  (=list
                   (=prime-expr)
                   (%and (%or (?eq '*) (?eq '/))
                         (=element))
                   'rec/=term)
      (list op a b))
    (=prime-expr)))

(defun =expr ()
  ;; FIXME
  (=subseq
   (=list (=term)
          (%maybe (=list (%or (?eq '+) (?eq '-))
                         'rec/=expr)))))

(setf (fdefinition 'rec/=expr) (=expr))
(setf (fdefinition 'rec/=term) (=term))

;;(parse '(2 * 1 * 2 * 3 / 3) (=term))

(parse '(\( 2 * 3 \) * 2) (=term))

