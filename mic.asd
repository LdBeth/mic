(asdf:defsystem "mic"
  :depends-on ("re")
  :components ((:file "packages")
               (:file "tokenizer" :depends-on ("packages"))))
