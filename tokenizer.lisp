;;; string -> list token
(in-package #:mic)

;; The token protocol
(defclass token ()
  ((pos :reader token-position)))

(defmethod print-object ((obj token) stream)
  (format stream "#<token at ~S>" (token-position obj)))

;; according to C99 lexical elements
(defclass keyword (token)
  ())
(defclass identifier (token)
  ())
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
    (vector-push-extend (setf char (read-char stream))
                        (parse-buffer state))
    (read-till-end (parse-buffer state) stream)))
