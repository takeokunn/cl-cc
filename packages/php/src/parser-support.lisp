(in-package :cl-cc/php)

(defun %php-assignment-op-p (stream)
  "Return true when STREAM begins with PHP simple assignment '='."
  (and (eq (php-peek-type stream) :T-OP)
       (equal "=" (php-peek-value stream))))
