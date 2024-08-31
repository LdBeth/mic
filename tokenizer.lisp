;;; string -> list token
(in-package #:mic)

;; The token protocol
(defclass token ()
  ((pos :initarg :pos :reader token-position)))

(defmethod print-object ((obj token) stream)
  (format stream "#<token #|~A|# at ~S>"
          (token-content obj) (token-position obj)))

;; according to C99 lexical elements
(defclass keyword (token)
  ((kword :initarg :content :type symbol
          :reader token-content)))

(defclass identifier (token)
  ((string :initarg :content :type string
           :reader token-content)))
(defclass constant (token)
  ((value :initarg :content :type string
          :reader token-content)
   (type :initarg :type :type symbol)))
(defclass string-literal (token)
  ())
(defclass punctuactor (token)
  ((punct :initarg :content :type symbol
          :reader token-content)))
(defclass comment (token)
  ((text :initarg :content :type string
         :reader token-content)
   (is-block :initarg :block-p :type boolean)))
(defclass preprocessor (token)
  ((instruction :initarg :content :type string
                :reader token-content)))

(defmethod print-object ((obj comment) stream)
  (let* ((s (token-content obj))
         (is-long (> (length s) 10)))
    (format stream "#<comment ~:[~S~*~;~S ... ~S~] at ~S>"
            is-long
            (if is-long
                (subseq s 0 4)
                s)
            (if is-long (subseq s (- (length s) 4)))
            (token-position obj))))

(defmethod print-object ((obj preprocessor) stream)
  (let ((s (token-content obj)))
    (format stream "#<pp ~:[~S~;~S ...~] at ~S>"
            (> (length s) 5) (subseq s 0 (min (length s) 5)) (token-position obj))))

(defclass parse-state ()
  ((pos :initform 0 :type integer)
   (col :initform 0 :type integer)
   (row :initform 0 :type integer)
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

;; redefine standard `read-char' function.
(defun read-char (stream &optional (eof-error-p t) eof-value)
  (declare (special *state*))
  (let ((c (cl:read-char stream eof-error-p eof-value)))
    (with-slots (pos col row) *state*
      (if (eql c #\newline)
          (progn
            (setf col 0)
            (incf row)
            (incf pos))
          (progn
            (incf col))))
    c))

(defun whitespacep (char)
  (or (eql char #\space)
      (eql char #\tab)
      (eql char #\newline)))

(defun get-a-token (*state* stream)
  "Return nil if reach end of file."
  (declare (special *state*))
  (let (char)
    ;; skip whitespace
    (loop while (whitespacep (peek-char nil stream nil nil))
          do (read-char stream))
    (setf char (peek-char nil stream nil nil))
    (when char
      (funcall (cond ((cl:alpha-char-p char)
                      #'read-identifier)
                     ((cl:digit-char-p char)
                      #'read-number)
                     ((or (eql #\" char) (eql #\' char))
                      #'read-string)
                     (t
                      #'read-punctuactor))
               *state* stream))))

(defconstant +c-keywords+
  (re:compile-re
   ;; automata constructed using Emacs
   (concatenate 'string
                "(?_(?Bool|Complex|Imaginary)|auto|break|c(?ase|har|on(?st|tinue))|"
                "d(?efault|o(?uble)?)|e(?lse|num|xtern)|f(?loat|or)|goto|"
                "i(?f|n(?line|t))|long|re(?gister|strict|turn)|s(?hort|i(?gned|zeof)|"
                "t(?atic|ruct)|witch)|typedef|un(?ion|signed)|vo(?id|latile)|while)")))

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
  (let ((m (re:match-re re string :exact t)))
    (and m (re:match-string m))))

(defun match-identifier (string)
  "if the string matches a C keyword, return the keyword."
  (re:match-string (re:match-re +c-keywords+ string :exact t)))

(defun punctuactorp (char)
  (find char "[](){}.+-&*+-~!/%<>=^?:;|,#"))

(defun read-till-punctuactor (buffer stream)
  "Handles both identifier or numbers."
  (loop never (let ((c (peek-char nil stream nil nil)))
                (or (whitespacep c)
                    (punctuactorp c)
                    (null c)))
        do (vector-push-extend (read-char stream) buffer)))

(defun make-pos (state)
  (cons (slot-value state 'col)
        (slot-value state 'row)))

(defun read-identifier (*state* stream)
  "Handles identifier or keywords"
  (declare (special *state*))
  (let ((buffer (parse-buffer *state*))
        (pos (make-pos *state*)))
    ;; clear buffer
    (setf (fill-pointer buffer) 0)
    (read-till-punctuactor buffer stream)
    (or
     (let ((w (match +c-keywords+ buffer)))
       (and w (make-instance 'keyword :content (intern w '#:mic-symbols)
                                      :pos pos)))
     (let ((w (match +c-identifier+ buffer)))
       (and w (make-instance 'identifier :content w :pos pos)))
     (error "~S is not a valid identifier or keyword." buffer))))

(defun read-number (*state* stream)
  "Handles identifier or keywords. So far only decimal and octal number
planned."
  (declare (special *state*))
  (let ((buffer (parse-buffer *state*))
        (pos (make-pos *state*)))
    (setf (fill-pointer buffer) 0)
    (read-till-punctuactor buffer stream)
    (or
     (let ((w (match +c-decimal-integer+ buffer)))
       (and w (make-instance 'constant :content w :type 'decimal
                             :pos pos)))
     (let ((w (match +c-octal-integer+ buffer)))
       (and w (make-instance 'constant :content w :type 'octal
                             :pos pos)))
     (error "~S is not a valid number." buffer))))

(defconstant +c-2-punctuator+
  (re:compile-re
   "!=|%%[:=>]|&[&=]|%*=|%+[+=]|%-[=-]|/[/*=]|:>|<[%%:<=]|==|>[=>]|^=|%|[=|]"))

(defconstant +c-operators+
  '("[" "]" "(" ")" "{" "}" "." "->" "++" "--" "&" "*" "+" "-" "~" "!"
    "/" "%" "<<" ">>" "<" ">" "<=" ">=" "==" "!=" "^" "|" "&&" "||"
    "?" ":" ";" "..." "=" "*=" "/=" "%=" "+=" "-=" "<<=" ">>=" "&=" "^="
    "|=" "," "<:" ":>" "<%" "%>"))

(defconstant +c-comment-begin+
  (re:compile-re "/[*/]"))

(defconstant +c-macro-begin+
  (re:compile-re "#|%%:"))

(defun read-comment (buffer stream is-block)
  ;; Clear buffer first
  (setf (fill-pointer buffer) 0)
  (let (c)
    (if is-block
        (loop while (progn
                      (setf c (read-char stream nil nil))
                      (and c
                          (not (and (eql c #\*)
                                    (eql (peek-char nil stream t) #\/)))))
              do (vector-push-extend c buffer)
              finally (read-char stream))
        (loop always (setf c (read-char stream nil nil))
              do (vector-push-extend c buffer)
              until (and (not (eql c #\\))
                         (eql (peek-char nil stream t) #\newline))))
    (copy-seq buffer)))

(defun read-punctuactor (*state* stream)
  "Read punctuators. This also handles comment, which would
not be ignored by the parser."
  (declare (special *state*))
  (let ((buffer (parse-buffer *state*))
        (pos (make-pos *state*)))
    (setf (fill-pointer buffer) 0)
    (vector-push (read-char stream) buffer)
    (let ((c (peek-char nil stream nil nil)))
      (when (punctuactorp c)
        (vector-push c buffer)))
    (unless (and
             (> (length buffer) 1)
             (and (re:match-re +c-2-punctuator+ buffer)
                  (read-char stream)))
      (setf (fill-pointer buffer) 1))
    (or
     (let ((m (find buffer +c-operators+ :test #'string=)))
       (and m
            (make-instance 'punctuactor :content (intern m '#:mic-symbols)
                                        :pos pos)))
     ;; comment
     (let* ((m (match +c-comment-begin+ buffer))
            (block-p (string= "/*" m))
            (w (and m
                    (read-comment buffer stream block-p))))
       (and w (make-instance 'comment :content w :pos pos :block-p block-p)))
     ;; preprocessor instructions
     (let* ((m (match +c-macro-begin+ buffer))
            (w (and m
                    (read-comment buffer stream nil))))
       (and w (make-instance 'preprocessor :content w :pos pos)))
     ;; other cases
     (error "~S is not a valid punctuactor." buffer))))

(defun tokenizer (stream)
  "Tokenize text from a input stream."
  (let ((state (make-instance 'parse-state))
        token)
    (loop while (setf token (get-a-token state stream))
          collect token)))

