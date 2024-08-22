(asdf:defsystem "mic"
  :components ((:file "packages")
               (:file "tokenizer" :depends-on ("packages"))))
