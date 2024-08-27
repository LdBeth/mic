(in-package #:cl-user)

(with-input-from-string (o "  auto ")
  (mic::get-a-token (make-instance 'mic::parse-state) o))
