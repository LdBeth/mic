(load "mic.asd")
(asdf:load-system "mic")

(in-package #:cl-user)
(defun flagp (arg)
  (> (length arg) 0)
  (eql #\- (char arg 0)))

#+ccl
(progn
  (if ccl:*unprocessed-command-line-arguments*
      (let (arg lexing file)
        (loop while ccl:*unprocessed-command-line-arguments*
              always (when (flagp (car ccl:*unprocessed-command-line-arguments*))
                       (setf arg (pop ccl:*unprocessed-command-line-arguments*))
                       (cond ((string= "-L" arg)
                              (setf lexing t))
                             (t
                              (format *error-output* "Unknown flag ~A.~%" arg)))))
        (if (> (length ccl:*unprocessed-command-line-arguments*) 0)
            (progn
              (setf file (pop ccl:*unprocessed-command-line-arguments*))
              (cond
                (lexing
                 (format *standard-output* "Lexing file ~A.~%" file)
                 (ccl:quit 0))
                (t
                 (format *standard-output* "Compiling file ~A.~%" file)
                 (ccl:quit 0)))))))
  (format *error-output* "No input file.~%")
  (ccl:quit -2))

