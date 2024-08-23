;;; string -> list token
(in-package #:mic)

;; The token protocol
(defclass token ()
  ((pos :reader token-position)))

(defmethod print-object ((obj token) stream)
  (format stream "#<token at ~S>" (token-position obj)))

;; according to C99 lexical elements
(defclass keyword (token)
  ((kword :initarg :keyword :type symbol)))

(defclass identifier (token)
  ((string :initarg :id :type string)))
(defclass constant (token)
  ())
(defclass string-literal (token)
  ())
(defclass punctuactor (token)
  ())

(defclass parse-state ()
  ((pos :initform 0 :type integer)
   (current-state :initform :free :type symbol)
   (buffer :initform
           (make-array 72 :element-type 'character
                          :adjustable t
                          :fill-pointer 0)
           :accessor
           parse-buffer)
   (tokens :initform
           (make-array 640 :adjustable t
                           :initial-element nil :fill-pointer 0))))

(defun get-a-token (state stream)
  (let (char)
    (loop while (whitespacep (peek-char nil stream))
              do (prog (read-char stream)
                    (incf (parse-state-pos state))))
    (setf char (peek-char stream))
    (funcall (cond ((letterp char)
                    #'read-identifier)
                   ((digitp char)
                    #'read-number)
                   ((quotep char)
                    #'read-string)
                   (t
                    (read-punctuactor)))
             state stream)))

(defconstant +c-keywords+
  (re:compile-re
   (concatenate 'string
                "auto|enum|restrict|unsigned|break|"
                "extern|return|void|case|float|short|volatile|"
                "char|for|signed|while|const|goto|sizeof|"
                "_Bool|continue|if|static|_Complex|"
                "default|inline|struct|_Imaginary|do|int|"
                "switch|double|long|typedef|else|register|union")))

;; We do not plan to support universal-character-name for now
(defconstant +c-identifier+
  (re:compile-re "[_a-zA-Z][_0-9a-zA-Z]*"))

;; Note that "0" is not a decimal number in ISO C99.
(defconstant +c-decimal-integer+
  (re:compile-re "[1-9]+[0-9]*(?[uUlL]|ll|LL)?"))

(defun match-keyword (string)
  "if the string matches a C keyword, return the keyword."
  (re:match-string (re:match-re +c-keywords+ string :exact t)))

(defun match-identifier (string)
  "if the string matches a C keyword, return the keyword."
  (re:match-string (re:match-re +c-keywords+ string :exact t)))

(defun read-till-punctuactor (buffer stream)
  "Handles both identifier or numbers."
  (loop never (let ((c (peek-char nil stream nil nil)))
                (or (whitespacep c)
                    (find c "[](){}.+-&*+-~!/%<>=^?:;|,#")))
        do (vector-push-extend (read-char stream) buffer)))

(defun read-identifier (state stream)
  "Handles identifier or keywords"
  (let ((buffer (parse-buffer state)))
    ;; clear buffer
    (setf (fill-pointer buffer) 0)
    (read-till-punctuactor buffer stream)
    (or
     (let ((w (match-keyword buffer)))
       (and w (make-instance 'keyword :keyword (intern w))))
     (let ((w (match-indentifier buffer)))
       (and w (make-instance 'identifier :string w)))
     (error "~S is not a valid identifier or keyword." buffer))))

(defun tokenizer (stream)
  "Tokenize text from a input stream."
  )
