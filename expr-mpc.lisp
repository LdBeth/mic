(in-package #:mic-pc)

(defun ?value ()
  (?satisfies 'numberp))

(defun =value ()
  (%and (?value) (=element)))

(defun =term ()
   (%or
    (=destructure (a op b)
                  (=list
                   (=value)
                   (%and (%or (?eq '*) (?eq '/))
                         (=element))
                   'rec/=term)
      (list op a b))
    (=value)))

(defun =expr ()
  (=subseq
   (=list (=term)
          (%maybe (=list (%or (?eq '+) (?eq '-))
                         'rec/=expr)))))

(setf (fdefinition 'rec/=expr) (=expr))
(setf (fdefinition 'rec/=term) (=term))

(parse '(1 * 2 * 3 / 3) (=term))
