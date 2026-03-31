(in-package :cl-cc/test)

(defsuite macro-lambda-list-suite
  :description "Lambda list parser unit tests"
  :parent cl-cc-suite)

(in-suite macro-lambda-list-suite)

(deftest macro-lambda-list-required-only
  "parse-lambda-list preserves required parameters in order."
  (let ((info (cl-cc::parse-lambda-list '(a b c))))
    (assert-equal '(a b c) (cl-cc::lambda-list-info-required info))))

(deftest macro-lambda-list-key-and-aux
  "parse-lambda-list records &key and &aux sections."
  (let ((info (cl-cc::parse-lambda-list '(&key verbose &aux (count 0)))))
    (assert-true (cl-cc::lambda-list-info-key-params info))
    (assert-equal '((count 0)) (cl-cc::lambda-list-info-aux info))))

(deftest macro-lambda-list-bindings-shape
  "generate-lambda-bindings and destructure-lambda-list return bindings."
  (assert-true (assoc 'args (cl-cc::generate-lambda-bindings '(&rest args) 'form)))
  (assert-true (assoc 'x (cl-cc::destructure-lambda-list '(x &optional y) 'form))))
