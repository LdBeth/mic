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
    `(prog ,globals ,fns ,*id*)))

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
        body (argc 0) (init *id*))
    (loop for i in (third (second toplevel))
          do (setf variables (cons
                              (cons (mic-lex:token-content i) (gensym))
                              variables)
                   argc (1+ argc)))
    (setf body (loop for line in (cddr toplevel)
                     if (eq (car line) 'defvar)
                       do (loop for v in (third line)
                                do (setf variables (cons
                                                    (cons (mic-lex:token-content v) (gensym))
                                                    variables)))
                     else collect (fix-variable line variables)))
    `(,fn-name ,(list init argc (- *id* init)) ,@body)))

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

;; The second pass expands expressions and
;; the result is close to target asm 
(defun pass-2 (progm)
  (let ((globals (second progm))
        (fns (third progm)))
    (setf fns (mapcar #'ssa fns))
    `(prog ,globals
        ,fns
        ,(fourth progm))))

(defun expand-exp (var exp)
  (if (consp exp)
      `(("mov" "eax" ,(second exp))
        ,(case (car exp)
           (+ `("add" "eax" ,(third exp)))
           (- `("sub" "eax" ,(third exp))))
        ("mov" ,var "eax"))
      `(("mov" ,var ,exp))))

(defun expand-exp* (exp)
  (if (consp exp)
      `(("mov" "eax" ,(second exp))
        ,(case (car exp)
           (+ `("add" "eax" ,(third exp)))
           (- `("sub" "eax" ,(third exp))))
        ("ret"))
      `(("mov" "eax" exp) ("ret"))))

(defun ssa (fn)
  (list* (first fn)
         (second fn)
         (mapcan (lambda (line)
                   (case (car line)
                     (incf `(("inc" ,(cadr line))))
                     (decf `(("dec" ,(cadr line))))
                     (setq (expand-exp (second line) (third line)))
                     (return (expand-exp* (second line)))))
                 (cddr fn))
         ))

;; Final codegen pass
(defun code-gen (ast)
  (format t ".code~%")
  (loop for f in (third ast)
        do (assemble f))
  (format t ".data~%")
  (loop for p in (second ast)
        do (format t "~A sdword ~S~%" (car p) (cdr p)))
  (loop for i from 0 to (1- (fourth ast))
        do (format t "G~A sdword ?~%" i))
  (format t " end~%"))

(defun assemble (fn)
  (format t "~A proc~%" (car fn))
  (let ((args (second fn)))
    (if (> (second args) 0)
        (loop for i from (car args) to (+ (car args) (cadr args) -1)
              do (format t " pop G~A~%" i))))
  (loop for i in (cddr fn)
        do (format t " ~a ~{~a~^, ~}~%" (car i) (cdr i)))
  (format t "~A endp~%" (car fn)))
