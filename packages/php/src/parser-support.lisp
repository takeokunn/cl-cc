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

(defun %php-callable-body (body-stmts)
  "Wrap a PHP function/method/closure BODY-STMTS list in (block nil ...).

PHP `return' lowers to (return-from nil ...), but a bare ast-defun/ast-lambda
establishes no block named NIL. Without this wrapper the body fails to compile,
the top-level handler-case silently drops the whole defun, and every call then
hits `Undefined function'. Returns a one-element body list; a NIL/empty body
(abstract method signature) is returned unchanged so it stays body-less."
  (if body-stmts
      (list (make-ast-block :name nil :body body-stmts))
      body-stmts))
