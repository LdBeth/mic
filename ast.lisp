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
    `(prog ,globals ,fns ,(make-array *id* :initial-element t))))

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
                          (ref (second line) vars))))
    (labels `(labels ,(mic-lex:token-content (second line))))
    (if `(if (,(first (second line)) ,(ref (second (second line)) vars)
              ,(ref (third (second line)) vars))
             (go ,(mic-lex:token-content (second (third line))))))))

;; Eliminate assignment to unused variable
(defun pass-2 (progm)
  (let ((table (make-array (length (fourth progm))
                           :initial-element nil)))
    (loop for fn in (third progm)
          do (loop for i in (cddr fn)
                   do (mapc (lambda (x)
                              (if (and (stringp x)
                                       (eql (aref x 0) #\G))
                                  (setf (aref table
                                              (parse-integer (subseq x 1)))
                                        t)))
                            (cond
                              ((eq (car i) 'setq)
                               (alexandria:flatten (cddr i)))
                              ((or (eq (car i) 'incf) (eq (car i) 'decf))
                               nil)
                              (t (alexandria:flatten i))))))

    (let ((fns (third progm)))
      (setf fns (mapcar (lambda (x)
                          (remove-if (lambda (l)
                                       (and (consp l)
                                            (or (eq (car l) 'setq)
                                                (eq (car l) 'incf)
                                                (eq (car l) 'decf))
                                            (not (aref table
                                                       (parse-integer (subseq (cadr l) 1))))))
                                     x))
                        fns))
      `(prog ,(second progm)
          ,fns
          ,table))))

;; Constant folding on arith expresions and compare expression
(defun pass-3 (progm)
  (let ((fns (third progm)))
    (setf fns (mapcar #'arith-opt fns))
    `(prog ,(second progm)
        ,fns
        ,(fourth progm))))

(defun arith-opt (fn)
  (list* (first fn)
         (second fn)
         (mapcar (lambda (line)
                   (case (car line)
                     (setq (if (and (consp (third line))
                                    (numberp (second (third line)))
                                    (numberp (third (third line))))
                               `(setq ,(second line)
                                      ,(eval (third line)))
                               line))
                     (if (if (and (consp (second line))
                                  (numberp (second (second line)))
                                  (numberp (third (second line))))
                             (if (eval (second line))
                                 (third line)
                                 `(nop))
                             line))
                     (otherwise line)))
                 (cddr fn))))

;; The next pass expands expressions and
;; the result is close to target asm
(defun pass-4 (progm)
  (let ((globals (second progm))
        (fns (third progm)))
    (setf fns (mapcar #'unexpr fns))
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
      `(("mov" "eax" ,exp) ("ret"))))

(defun unexpr (fn)
  (list* (first fn)
         (second fn)
         (mapcan (lambda (line)
                   (copy-list
                    (case (car line)
                      (incf `(("inc" ,(cadr line))))
                      (decf `(("dec" ,(cadr line))))
                      (if `(("cmp" ,(second (second line))
                                   ,(third (second line)))
                            (,(case (first (second line))
                                (> "jg")
                                (< "jl")
                                (== "je"))
                             ,(second (third line)))))
                      (go `(("jmp" ,(second line))))
                      (labels `((labels ,(second line))))
                      (setq (expand-exp (second line) (third line)))
                      (return (expand-exp* (second line)))
                      (otherwise line))))
                 (cddr fn))
         ))

;; Eliminate the dead code caused by forward unconditinal goto.
(defun pass-5 (progm)
  (let ((globals (second progm))
        (fns (third progm)))
    (setf fns (mapcar #'jmp-opt fns))
    `(prog ,globals
        ,fns
        ,(fourth progm))))

(defun jmp-opt (fn)
  (let* ((body (cddr fn))
         (rest body)
         skip
         acc)
    (loop for line in body
          do (progn
               (setf rest (cdr rest))
               (when (eq (car line) 'labels)
                 (setq skip nil))
               (when (string= (car line) "jmp")
                 (when (find (list 'labels (cadr line)) rest :test #'equal)
                   (setq skip t)
                   (push line acc)))
               (unless skip
                 (push line acc))))
    (setq body (nreverse acc)
          rest body
          acc nil
          skip nil)
    ;; Fix up for jmp and label next to each other
    (loop for line in body
          do (progn
               (setf rest (cdr rest))
               (when (string= (car line) "jmp")
                 (when (equal (list 'labels (cadr line))
                              (car rest))
                   (setq skip t)))
               (unless skip
                 (push line acc))
               (when (eq (car line) 'labels)
                 (setq skip nil))))
    (setq body (nreverse acc))
    (list* (first fn)
           (second fn)
           body)))

;; Final codegen pass
(defun code-gen (ast)
  (let ((g-table (fourth ast)))
    (format t ".code~%")
    (loop for f in (third ast)
          do (assemble f g-table))
    (format t ".data~%")
    (loop for p in (second ast)
          do (format t "~A sdword ~S~%" (car p) (cdr p)))
    (loop for i from 0 to (1- (length g-table))
          do (if (aref g-table i)
                 (format t "G~A sdword ?~%" i)))
    (format t " end~%")))

(defun assemble (fn table)
  (format t "~A proc~%" (car fn))
  (let ((args (second fn)))
    (if (> (second args) 0)
        (loop for i from (car args) to (+ (car args) (cadr args) -1)
              do (if (aref table i)
                     (format t " pop G~A~%" i)
                     (format t " pop eax~%")))))
  (loop for i in (cddr fn)
        do (if (eq (car i) 'labels)
               (format t "~a~%" (cadr i))
               (format t " ~a ~{~a~^, ~}~%" (car i) (cdr i))))
  (format t "~A endp~%" (car fn)))
