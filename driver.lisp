(load "mic.asd")
(asdf:load-system "mic")

(in-package #:cl-user)
(defun flagp (arg)
  (> (length arg) 0)
  (eql #\- (char arg 0)))

(defun user-quit (exit-code)
  #+ccl
  (ccl:quit exit-code)
  #+sbcl
  (sb-ext:exit exit-code))

(defparameter *command-line-arguments*
  #+ccl
  ccl:*unprocessed-command-line-arguments*
  #+sbcl
  (cadr sb-ext:*posix-argv*))

(progn
  (if *command-line-arguments*
      (let (arg lexing file)
        (loop while *command-line-arguments*
              always (when (flagp (car *command-line-arguments*))
                       (setf arg (pop *command-line-arguments*))
                       (cond ((string= "-L" arg)
                              (setf lexing t))
                             (t
                              (format *error-output* "Unknown flag ~A.~%" arg)))))
        (if (> (length *command-line-arguments*) 0)
            (progn
              (setf file (pop *command-line-arguments*))
              (cond
                (lexing
                 (format *standard-output* "Lexing file ~A.~%" file)
                 (handler-bind ((mic:lexing-error (lambda (condition)
                                                    (format *error-output* "~&~A~&" condition)
                                                    (user-quit -1)))
                                (error (lambda (condition)
                                         (format *error-output* "Uncaught error: ~A~&" condition))))
                     (with-open-file (f file :direction :input)
                       (let ((tokens (mic:tokenizer f)))
                         (format t "~{~a~^~%~}" tokens))))
                 (user-quit 0))
                (t
                 (format t "Compiling file ~A.~%" file)
                 (user-quit 0)))))))
  (format *error-output* "No input file.~%")
  (user-quit -2))

