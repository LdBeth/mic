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
        (=transform (=element)
                    #'mic-lex:token-data)))

(defun =binary-op ()
  (=destructure (e1 op e2)
                (=list (=atom-expr)
                       (%and (%or (?op '+) (?op '-))
                             (=transform (=element)
                                         #'mic-lex:token-content))
                       (=atom-expr))
    `(,op ,e1 ,e2)))

(defun =atom-expr ()
  (%or (=identifier)
       (=constant)))

(defun =expr ()
  (%or (=binary-op)
       (=atom-expr)))

(defun ?op (symbol)
  "Matches if the token represents the punctator."
  (?token-test o 'mic-lex:punctuator
               (eq (mic-lex:token-content o)
                   (intern (symbol-name symbol) '#:mic-symbols))))

(defun =declaration ()
  (=destructure (type star name _ expr _ )
                (=list (=type) (%maybe (=star)) (=identifier) (?op '=) (=constant) (?op '\;))
    `(defvar ,(if star `(* ,type) type) ,name ,expr)))

(defun =program ()
  (=transform (=list
               (%some (%or (=declaration)
                           (=function))))
              (lambda (x) (cons 'progn (car x)))))

(defun ?type-specifier ()
  (?token-test o 'mic-lex:keyword
               (member (mic-lex:token-content o)
                       (mapcar (lambda (w)
                                 (intern w '#:mic-symbols))
                               '("void" "char" "short"
                                 "int" "long" "signed" "unsigned")))))

(defun =type-specifier ()
  (%and (?type-specifier)
        (=transform (=element)
                    (lambda (x) (symbol-name (mic-lex:token-content x))))))

(defun current-token ()
  (when maxpc::*input-fail*
    (maxpc.input:input-first maxpc::*input-fail*)))

(defun =return-statement ()
  (=destructure (_ e)
                (=list (?keyword "return")
                       (%maybe (=expr)))
    `(return ,e)))

(defun =var-decl ()
  (=destructure (type star name)
                (=list (=type) (%maybe (=star)) (=varlist))
    `(defvar ,(if star `(* ,type) type) ,name)))

(defun =assignment ()
  (=destructure (id _ e)
                (=list (=identifier) (?op '=) (=expr))
    `(setq ,id ,e)))

(defun =incr ()
  (=destructure (e _)
                (=list (=identifier) (?op '++))
    `(incf ,e)))

(defun =decr ()
  (=destructure (e _)
                (=list (=identifier) (?op '--))
    `(decf ,e)))

(defun =statement ()
  (=destructure (x _)
                (=list (%or
                        (=var-decl)
                        (=return-statement)
                        (=assignment)
                        (=incr)
                        (=decr)
                        )
                       (?op '\;))
    x))

(defun =type ()
  (%or (%some (=type-specifier))
       #+(or)
       (?fail (error 'parsing-error
                     :token (current-token)
                     :message "Not a supported data type."))))

(defun =varlist ()
  (=transform (=subseq (?seq (%any (?seq (=identifier) (?op '\,)))
                             (=identifier)))
              (lambda (x)
                (coerce (remove-if (lambda (x)
                                     (typep x 'mic-lex:punctuator))
                                   x)
                        'list))))

(defun =star ()
  (%and (?op '*)
        (=element)))

(defun =function ()
  (=destructure (type star name _ args _ _ _ body _)
                (=list (=type) (%maybe (=star)) (=identifier)
                       (?op '\()
                       (%maybe (=varlist))
                       (?op '\))
                       (%any (=statement))
                       (?op '{)
                       (%any (=statement))
                       (%or (?op '})
                            (?fail (error 'parsing-error
                                          :token (current-token)
                                          :message "Failed to parse function."))))
    `(defun (,(if star `(* ,type) type) ,name ,args)
       ,@body)))


