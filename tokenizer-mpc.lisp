(in-package #:mic-pc)

(defun ?string-chars ()
  (?not (?test ('member '(#\" #\\)))))

(defun ?escape-sequence-char ()
  (?satisfies (lambda (c)
                (find c "'\"?\\abfnrtv" :test #'char-equal))))

(defun =c-string ()
  (=destructure (_ s _)
      (=list (?eq #\")
             (=subseq (%some (%or (?string-chars)
                                  (?seq (?eq #\\)
                                        (?escape-sequence-char)))))
             (?eq #\"))))

(parse "\"abcds\\n\"" (=c-string))
