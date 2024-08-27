(in-package #:cl-user)

(with-input-from-string (o "  fo+1 ")
  (mic::get-a-token (make-instance 'mic::parse-state) o))
