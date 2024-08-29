(in-package #:cl-user)

(with-input-from-string (o "  fo+1 ")
  (mic::get-a-token (make-instance 'mic::parse-state) o))


(with-input-from-string (o "   
 12L ")
  (mic::get-a-token (make-instance 'mic::parse-state) o))


(with-input-from-string (o " /* foo */")
  (mic::get-a-token (make-instance 'mic::parse-state) o))

(with-input-from-string (o "  fo+1 ")
  (mic:tokenizer o))

(with-input-from-string (o "  return 1+1;
goto foo;")
  (mic:tokenizer o))


(with-input-from-string (o " int/* asd */foo =1+2;")
  (mic:tokenizer o))

(with-input-from-string (o "{return 023; // oct
  }")
  (mic:tokenizer o))

(with-input-from-string (o "i+++1")
  (mic:tokenizer o))
