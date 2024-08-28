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
