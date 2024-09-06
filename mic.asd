(asdf:defsystem "mic"
  :depends-on ("re" "maxpc")
  :components ((:file "packages")
               (:file "tokenizer" :depends-on ("packages"))
               (:file "parser" :depends-on ("packages"))))
