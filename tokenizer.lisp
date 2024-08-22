;;; string -> list token
(in-package #:mic)

;; The token protocol
(defclass token ()
  ((pos :reader token-position)))

(defmethod print-object ((obj token) stream)
  (format stream "#<token at ~S>" (token-position obj)))

;; according to C99 lexical elements
(defclass keyword (token))
(defclass identifier (token))
(defclass constant (token))
(defclass string-literal (token))
(defclass punctuactor (token))

(defgeneric read-token (driver input)
  (:documentation
   "The state machine"))

(defstruct parse-state
  (pos 0 :type integer)
  (current-state :free :type symbol)
  (buffer (make-array 64 :element-type 'character
                         :adjustable t
                         :fill-pointer 0))
  (tokens (make-array 640 :adjustable t :initial-element nil :fill-pointer 0)))

(defun get-a-token (state stream)
  (let (char)
    (while (whitespacep (peek-char nil stream))
      (read-char stream)
      (incf (parse-state-pos state)))
    (while ())))
  
