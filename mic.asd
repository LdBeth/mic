(asdf:defsystem "mic"
  :depends-on ("re" "maxpc" "alexandria")
  :components ((:file "packages")
               ;(:file "symbol-table" :depends-on ("packages"))
               (:file "tokenizer" :depends-on ("packages"))
               ;; (:file "parser" :depends-on ("packages"))
               (:file "token-matcher" :depends-on ("packages" "tokenizer"))
               (:file "ast" :depends-on ("packages"))))
