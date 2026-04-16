;;;; tests/unit/parse/php/grammar-tests.lisp — PHP grammar token-stream tests
(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(defun %php-ts (tokens)
  (cl-cc/parse::make-php-token-stream :tokens tokens :source "" :diagnostics nil))

(deftest php-grammar-token-stream-peek-advance
  "Token-stream helpers expose the current token and advance correctly."
  (let* ((ts (%php-ts (list (list :type :T-IDENT :value "foo")
                            (list :type :T-EOF   :value nil)))))
    (assert-eq :T-IDENT (cl-cc/parse::php-ts-peek-type ts))
    (assert-equal "foo" (cl-cc/parse::php-ts-peek-value ts))
    (assert-equal (list :type :T-IDENT :value "foo")
                  (cl-cc/parse::php-ts-advance ts))
    (assert-eq :T-EOF (cl-cc/parse::php-ts-peek-type ts))))

(deftest php-grammar-token-stream-skip-semis
  "Semicolon skipping consumes only semicolon tokens."
  (let* ((ts (%php-ts (list (list :type :T-SEMI :value ";")
                            (list :type :T-SEMI :value ";")
                            (list :type :T-IDENT :value "x")
                            (list :type :T-EOF :value nil)))))
    (cl-cc/parse::php-ts-skip-semis ts)
    (assert-eq :T-IDENT (cl-cc/parse::php-ts-peek-type ts))
    (assert-equal "x" (cl-cc/parse::php-ts-peek-value ts))))

(deftest php-grammar-token-stream-at-end
  "Token-stream end detection treats nil and EOF as finished."
  (assert-true (cl-cc/parse::php-ts-at-end-p (%php-ts nil)))
  (assert-true (cl-cc/parse::php-ts-at-end-p (%php-ts (list (list :type :T-EOF :value nil))))))
