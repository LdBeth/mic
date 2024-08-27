;;; string -> list token
(in-package #:mic)

;; The token protocol
(defclass token ()
  ((pos :reader token-position)))

(defmethod print-object ((obj token) stream)
  (format stream "#<token at ~S>" (token-position obj)))

;; according to C99 lexical elements
(defclass keyword (token)
  ((kword :initarg :content :type symbol)))

(defclass identifier (token)
  ((string :initarg :content :type string)))
(defclass constant (token)
  ((value :initarg :content :type string)
   (type :initarg :type :type symbol)))
(defclass string-literal (token)
  ())
(defclass punctuactor (token)
  ((punct :initarg :content :type symbol)))
(defclass comment (token)
  ((text :initarg :content :type string)
   (is-block :initarg :block-p :type boolean)))

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

;; The way ISO C99 intended
(defconstant +c-octal-integer+
  (re:compile-re "0[0-7]*"))

(defun match (re string)
  "if the string matches a regex, return the matched string."
  (re:match-string (re:match-re re string :exact t)))

(defun match-identifier (string)
  "if the string matches a C keyword, return the keyword."
  (re:match-string (re:match-re +c-keywords+ string :exact t)))

(defun punctuactorp (char)
  (find c "[](){}.+-&*+-~!/%<>=^?:;|,#"))

(defun read-till-punctuactor (buffer stream)
  "Handles both identifier or numbers."
  (loop never (let ((c (peek-char nil stream nil nil)))
                (or (whitespacep c)
                    (punctuactorp c)))
        do (vector-push-extend (read-char stream) buffer)))

(defun read-identifier (state stream)
  "Handles identifier or keywords"
  (let ((buffer (parse-buffer state)))
    ;; clear buffer
    (setf (fill-pointer buffer) 0)
    (read-till-punctuactor buffer stream)
    (or
     (let ((w (match +c-keywords+ buffer)))
       (and w (make-instance 'keyword :content (intern w))))
     (let ((w (match +c-identifier+ buffer)))
       (and w (make-instance 'identifier :content w)))
     (error "~S is not a valid identifier or keyword." buffer))))

(defun read-number (state stream)
  "Handles identifier or keywords. So far only decimal and octal number
planned."
  (let ((buffer (parse-buffer state)))
    (setf (fill-pointer buffer) 0)
    (read-till-punctuactor buffer stream)
    (or
     (let ((w (match +c-decimal-integer+ buffer)))
       (and w (make-instance 'constant :content w :type 'decimal)))
     (let ((w (match +c-octal-integer+ buffer)))
       (and w (make-instance 'constant :content w :type 'octal)))
     (error "~S is not a valid number." buffer))))

(defconstant +c-1-2-punctuator+
  (re:compile-re "[[](){}.]"))

(defconstant +c-comment-begin+
  (re:compile-re "/[*/]"))

(defun read-comment (buffer stream is-block)
  ;; Clear buffer first
  (setf (fill-pointer buffer) 0)
  (let ((c1 #\*)
        (c2 #\/)
        c)
    (if is-block
        (loop never (progn
                      (setf c (read-char stream nil nil))
                      (or (null c)
                          (and (eql c c1)
                               (eql (peek-char nil stream t) c2))))
              do (vector-push-extend c buffer)
              finally (read-char stream))
        (loop never (progn
                      (setf c (read-char stream nil nil))
                      (eql c #\newline))
              do (vector-push-extend c buffer)))))

(defun read-punctuactor (state stream)
  "Read punctuators. This also handles comment, which would
not be ignored by the parser."
  (let ((buffer (parse-buffer state)))
    (setf (fill-pointer buffer) 0)
    (vector-push (read-char stream) buffer)
    (let ((c (peek-char nil stream nil nil)))
      (when (punctuactorp c)
        (vector-push c buffer)))
    (or
     ;; one char operators, these are simple case
     (and (= (length buffer) 1)
          (make-instance 'punctuactor :content (intern buffer)))
     ;; comment
     (let ((w (match +c-comment-begin+ buffer)))
       (and w
            (read-comment buffer stream (string= "/*" is-block))))
     #|TODO other cases|#)))

(defun tokenizer (stream)
  "Tokenize text from a input stream."
  )
