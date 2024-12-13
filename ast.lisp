(in-package #:mic-ast)

(define-condition variable-error (mic:compiler-error)
  ((variable :initarg :variable)
   (error-message :initarg :message))
  (:report (lambda (c stream)
             (let ((var (slot-value c 'variable))
                   (msg (slot-value c 'error-message)))
             (format stream "Error: ~S: ~A."
                     var
                     msg)))))


(defvar *id*)

;; This pass expands variables.  We also conviently assume all data
;; types are integer as they are stripped here.
(defun pass-1 (program)
  (let (globals fns (*id* 0))
    (loop for toplevel in (cdr program)
                if (eq 'defvar (car toplevel))
                   do (if (assoc (mic-lex:token-content (third toplevel))
                                 globals
                                 :test #'string=)
                          (error 'variable-error :variable (third toplevel)
                                                 :message "Has already been defined.")
                          (push (cons (mic-lex:token-content (third toplevel))
                                      (fourth toplevel))
                                globals)))
    (setf fns
          (loop for toplevel in (cdr program)
                if (eq 'defun (car toplevel))
                  collect (elaborate toplevel globals)))
    `(prog ,globals ,fns)))

(defun adding-var (toplevel variables)
  (if (assoc (mic-lex:token-content (third toplevel))
             variables
             :test #'string=)
      (error 'variable-error :variable (third toplevel)
                             :message "Has been redefined.")
      (cons
       (cons (mic-lex:token-content (third toplevel))
             (gensym))
       variables)))

(defun gensym ()
  (prog1 (symbol-name (cl:gensym *id*))
    (incf *id*)))

(defun elaborate (toplevel variables)
  (let ((fn-name (mic-lex:token-content (second (second toplevel))))
        body)
    (setf *id* 0)
    (loop for i in (third (second toplevel))
          do (setf variables (cons
                              (cons (mic-lex:token-content i) (gensym))
                              variables)))
    (setf body (loop for line in (cddr toplevel)
                     if (eq (car line) 'defvar)
                       do (loop for v in (third line)
                                do (setf variables (cons
                                                    (cons (mic-lex:token-content v) (gensym))
                                                    variables)))
                     else collect (fix-variable line variables)))
    `(,fn-name ,variables ,@body)))

(defun ref (id vars)
  (if (typep id 'mic-lex:identifier)
      (let ((p (assoc (mic-lex:token-content id)
                      vars
                      :test #'string=)))
        (if p
            (if (stringp (cdr p))
                (cdr p)
                (car p))
            (error 'variable-error :variable id
                                   :message "Has not been defined.")))
      id))

(defun fix-variable (line vars)
  (case (car line)
    (incf `(incf ,(ref (second line) vars)))
    (decf `(decf ,(ref (second line) vars)))
    (setq `(setq ,(ref (second line) vars) ,(if (consp (third line))
                                                (mapcar (lambda (v) (ref v vars))
                                                        (third line))
                                                (ref (third line) vars))))
    (return `(return ,(if (consp (second line))
                          (mapcar (lambda (v) (ref v vars))
                                  (second line))
                          (ref (second line) vars))))))

  

(defun code-gen (ast))
