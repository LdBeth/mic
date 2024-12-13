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

(defun parse (source &optional (driver (mic-pc::=program)))
  "Return AST of the source."
  (let ((tokens (mic-lex:cleanup-tokens (mic:tokenizer source))))
    (maxpc:parse tokens driver)
    ))

(defparameter *command-line-arguments*
  #+ccl
  ccl:*unprocessed-command-line-arguments*
  #+sbcl
  (cadr sb-ext:*posix-argv*))

(progn
  (if *command-line-arguments*
      (let (arg lexing parsing file (pass 0))
        (loop while *command-line-arguments*
              always (when (flagp (car *command-line-arguments*))
                       (setf arg (pop *command-line-arguments*))
                       (cond ((string= "-L" arg)
                              (setf lexing t))
                             ((string= "-P" arg)
                              (setf parsing t))
                             ((string= "-U" arg)
                              (setf pass (parse-integer (pop *command-line-arguments*))))
                             (t
                              (format *error-output* "Unknown flag ~A.~%" arg)))))
        (if (> (length *command-line-arguments*) 0)
            (progn
              (setf file (pop *command-line-arguments*))
              (cond
                (lexing
                 (format *standard-output* "Lexing file ~A.~%" file)
                 (handler-bind ((mic:compiler-error (lambda (condition)
                                                    (format *error-output* "~&~A~&" condition)
                                                    (user-quit -1)))
                                (error (lambda (condition)
                                         (format *error-output* "Uncaught error: ~A~&" condition))))
                     (with-open-file (f file :direction :input)
                       (let ((tokens (mic:tokenizer f)))
                         (format t "~{~a~^~%~}" (coerce tokens 'list)))))
                 (user-quit 0))
                (parsing
                 (format *standard-output* "Parsing file ~A.~%" file)
                 (handler-bind ((mic:compiler-error (lambda (condition)
                                                    (format *error-output* "~&~A~&" condition)
                                                    (user-quit -1)))
                                (error (lambda (condition)
                                         (format *error-output* "Uncaught error: ~A~&" condition))))
                     (with-open-file (f file :direction :input)
                       (let ((ast (parse f)))
                         (pprint ast)
                         (loop for i from 1 to pass
                               do (progn
                                    (format *standard-output* "~%Generate pass ~A.~%" i)
                                    (setf ast (funcall (intern (format nil "PASS-~A" i)
                                                               '#:mic-ast)
                                                       ast))
                                    (pprint ast))))))
                 (user-quit 0))
                (t
                 (format t "Compiling file ~A.~%" file)
                 (handler-bind ((mic:compiler-error (lambda (condition)
                                                    (format *error-output* "~&~A~&" condition)
                                                    (user-quit -1)))
                                (error (lambda (condition)
                                         (format *error-output* "Uncaught error: ~A~&" condition))))
                     (with-open-file (f file :direction :input)
                       (let ((ast (parse f)))
                         (setf ast (mic-ast:pass-1 ast))
                         (setf ast (mic-ast:pass-2 ast))
                         (setf ast (mic-ast:pass-3 ast))
                         (mic-ast:code-gen ast))))
                 (user-quit 0)))))))
  (format *error-output* "No input file.~%")
  (user-quit -2))

