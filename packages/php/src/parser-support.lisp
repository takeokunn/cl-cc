(in-package :cl-cc/php)

(defun %php-assignment-op (stream)
  "Return the PHP assignment operator string at STREAM, or NIL."
  (when (eq (php-peek-type stream) :T-OP)
    (let ((op (php-peek-value stream)))
      (when (member op '("=" "+=" "-=" "*=" "/=" ".=" "%=" "**="
                         "&=" "|=" "^=" "<<=" ">>=" "??=")
                    :test #'equal)
        op))))

(defun %php-assignment-op-p (stream)
  "Return true when STREAM begins with a PHP assignment operator."
  (not (null (%php-assignment-op stream))))

(defun %php-reference-token-p (stream)
  "Return true when STREAM begins with PHP's single ampersand token."
  (and (eq (php-peek-type stream) :T-OP)
       (equal "&" (php-peek-value stream))))
