(in-package #:mic-pc)

(define-condition parsing-error (mic:compiler-error)
  ((token :initarg :token)
   (error-message :initarg :message))
  (:report (lambda (c stream)
             (let ((token (slot-value c 'token))
                   (msg (slot-value c 'error-message)))
             (format stream "Error: ~A: ~A"
                     token msg)))))

(defmacro ?token-test (arg type &body body)
  `(?satisfies (lambda (,arg)
                 (and (typep ,arg ,type)
                      (progn
                        ,@body)))))

(defun ?keyword (keyword)
  "Matches if the token represents the keyword."
  (?token-test o 'mic-lex:keyword
               (eq (mic-lex:token-content o)
                   (intern keyword '#:mic-symbols))))

(defun ?identifier ()
  (?token-test o 'mic-lex:identifier t))

(defun ?constant ()
  (?token-test o 'mic-lex:constant t))

(defun =identifier ()
  (%and (?identifier)
        (=element)))

(defun =constant ()
  (%and (?constant)
        (=element)))

(defun =expr ()
  (%or (=identifier)
       (=constant)))

(defun ?op (symbol)
  "Matches if the token represents the punctator."
  (?token-test o 'mic-lex:punctuator
               (eq (mic-lex:token-content o)
                   (intern (symbol-name symbol) '#:mic-symbols))))

(defun =declaration ()
  (=destructure (type var _ expr)
                (=list (=type) (=var)
                       (?op '=) (=expr)
                       (?op '\;))
    (make-instance 'declaration :type type :var var :expr expr)))

(defun =program ()
  (=subseq (%any (%or (=declaration) (=function)
                      (?fail (error 'parsing-error
                                    :token (current-token)
                                    :message "Not a supported toplevel statement."))))))

(defun ?type-specifier ()
  (?token-test o 'mic-lex:keyword
               (member (mic-lex:token-content o)
                       (mapcar (lambda (w)
                                 (intern w '#:mic-symbols))
                               '("void" "char" "short"
                                 "int" "long" "float"
                                 "double" "signed" "unsigned" "_Bool"
                                 "_Complex")))))

(defun =type-specifier ()
  (%and (?type-specifier)
        (=element)))

(defun current-token ()
  (when maxpc::*input-fail*
    (maxpc.input:input-first maxpc::*input-fail*)))

(defun =return-statement ()
  (=list (?keyword "return")
         (%maybe (=expr))
         (?op '\;)))

(defun =statement ()
  (%or
   (=return-statement)))

(defun =function ()
  (=list (=type-specifier) (=identifier) (?op '\() (?op '\))
         (?op '{)
         (%any (=statement))
         (?op '})))

