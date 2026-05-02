(in-package :cl-cc/test)

(defsuite macro-lambda-list-suite
  :description "Lambda list parser unit tests"
  :parent cl-cc-unit-suite)

(in-suite macro-lambda-list-suite)

(deftest macro-lambda-list-required-only
  "parse-lambda-list preserves required parameters in order."
  (let ((info (cl-cc/expand:parse-lambda-list '(a b c))))
    (assert-equal '(a b c) (cl-cc/expand::lambda-list-info-required info))))

(deftest macro-lambda-list-key-and-aux
  "parse-lambda-list records &key and &aux sections."
  (let ((info (cl-cc/expand:parse-lambda-list '(&key verbose &aux (count 0)))))
    (assert-true (cl-cc/expand::lambda-list-info-key-params info))
    (assert-equal '((count 0)) (cl-cc/expand::lambda-list-info-aux info))))

(deftest macro-lambda-list-bindings-shape
  "generate-lambda-bindings and destructure-lambda-list return bindings."
  (assert-true (assoc 'args (cl-cc/expand:generate-lambda-bindings '(&rest args) 'form)))
  (assert-true (assoc 'x (cl-cc/expand:destructure-lambda-list '(x &optional y) 'form))))

(deftest macro-lambda-list-optional-rest-body-environment
  "parse-lambda-list records optional/rest/body/environment sections correctly."
  (let ((info (cl-cc/expand:parse-lambda-list '(a &optional (b 10 b-p) &body body &environment env))))
    (assert-equal '(a) (cl-cc/expand::lambda-list-info-required info))
    (assert-equal '((b 10 b-p)) (cl-cc/expand::lambda-list-info-optional info))
    (assert-eq 'body (cl-cc/expand::lambda-list-info-body info))
    (assert-eq 'env (cl-cc/expand:lambda-list-info-environment info))))

(deftest macro-lambda-list-allow-other-keys-and-key-spec
  "parse-lambda-list preserves explicit keyword specs and &allow-other-keys." 
  (let ((info (cl-cc/expand:parse-lambda-list '(&key ((:size n) 3 supplied-p) &allow-other-keys))))
    (assert-true (cl-cc/expand::lambda-list-info-allow-other-keys info))
    (assert-equal '(((:size n) 3 supplied-p))
                  (mapcar (lambda (spec) (list (first spec) (second spec) (third spec)))
                          (cl-cc/expand::lambda-list-info-key-params info)))))

(deftest macro-lambda-list-generate-bindings-covers-key-and-aux
  "generate-lambda-bindings emits bindings for &key supplied-p and &aux init forms." 
  (let ((bindings (cl-cc/expand:generate-lambda-bindings
                   '(x &key ((:size n) 3 n-p) &aux (count 0))
                   'form)))
    (assert-true (assoc 'x bindings))
    (assert-true (assoc 'n bindings))
    (assert-true (assoc 'n-p bindings))
    (assert-true (assoc 'count bindings))))

(deftest macro-lambda-list-destructure-covers-nested-required-and-key
  "destructure-lambda-list handles nested required patterns and keyword parameters." 
  (let ((bindings (cl-cc/expand:destructure-lambda-list
                   '((head tail) &key ((:limit lim) 5 lim-p) &aux (done nil))
                   'form)))
    (assert-true (assoc 'head bindings))
    (assert-true (assoc 'tail bindings))
    (assert-true (assoc 'lim bindings))
    (assert-true (assoc 'lim-p bindings))
    (assert-true (assoc 'done bindings))))
