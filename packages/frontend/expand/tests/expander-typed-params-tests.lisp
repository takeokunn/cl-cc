;;;; tests/unit/expand/expander-typed-params-tests.lisp
;;;; Unit tests for src/expand/expander-typed-params.lisp
;;;;
;;;; Covers: lambda-list-has-typed-p, strip-typed-params,
;;;;         register-function-type / *function-type-registry*,
;;;;         expand-type-alias.

(in-package :cl-cc/test)

(defsuite expander-typed-params-suite
  :description "Typed-param expander unit tests"
  :parent cl-cc-unit-suite)

(in-suite expander-typed-params-suite)

;;; ── lambda-list-has-typed-p ─────────────────────────────────────────────────

(deftest typed-params-has-typed-plain-symbol-list
  "lambda-list-has-typed-p returns nil for a plain symbol list."
  (assert-false (cl-cc/expand::lambda-list-has-typed-p '(x y z))))

(deftest typed-params-has-typed-with-typed-pair
  "lambda-list-has-typed-p returns t when at least one param has a type annotation."
  (assert-true (cl-cc/expand::lambda-list-has-typed-p '((x fixnum) (y string)))))

(deftest typed-params-has-typed-mixed-params
  "lambda-list-has-typed-p returns t when typed and plain params are mixed."
  (assert-true (cl-cc/expand::lambda-list-has-typed-p '((x integer) y z))))

(deftest typed-params-has-typed-stops-at-lambda-keywords
  "lambda-list-has-typed-p returns nil when lambda keyword precedes typed pair."
  (assert-false (cl-cc/expand::lambda-list-has-typed-p '(&optional (x fixnum)))))

(deftest typed-params-has-typed-empty-list
  "lambda-list-has-typed-p returns nil for an empty list."
  (assert-false (cl-cc/expand::lambda-list-has-typed-p '())))

(deftest-each typed-params-has-typed-builtin-types
  "lambda-list-has-typed-p recognizes all built-in type keywords."
  :cases (("fixnum"    '((n fixnum)))
          ("integer"   '((n integer)))
          ("string"    '((s string)))
          ("boolean"   '((b boolean)))
          ("character" '((c character)))
          ("list"      '((l list)))
          ("function"  '((f function))))
  (params)
  (assert-true (cl-cc/expand::lambda-list-has-typed-p params)))

;;; ── strip-typed-params ──────────────────────────────────────────────────────

(deftest strip-typed-params-extracts-names
  "strip-typed-params returns plain names as first value."
  (multiple-value-bind (plain type-alist)
      (cl-cc/expand::strip-typed-params '((x fixnum) (y string)))
    (declare (ignore type-alist))
    (assert-equal '(x y) plain)))

(deftest strip-typed-params-extracts-type-alist
  "strip-typed-params returns (name . type) pairs as second value."
  (multiple-value-bind (plain type-alist)
      (cl-cc/expand::strip-typed-params '((x fixnum) (y string)))
    (declare (ignore plain))
    (assert-= 2 (length type-alist))
    (assert-eq 'fixnum (cdr (assoc 'x type-alist)))
    (assert-eq 'string (cdr (assoc 'y type-alist)))))

(deftest strip-typed-params-preserves-plain-symbols
  "Plain symbol params pass through to the plain list unchanged."
  (multiple-value-bind (plain type-alist)
      (cl-cc/expand::strip-typed-params '((x fixnum) z))
    (assert-equal '(x z) plain)
    (assert-= 1 (length type-alist))))

(deftest strip-typed-params-preserves-lambda-keywords
  "&optional and &rest pass through into the plain param list."
  (multiple-value-bind (plain type-alist)
      (cl-cc/expand::strip-typed-params '(x &rest args))
    (declare (ignore type-alist))
    (assert-true (member '&rest plain))))

(deftest strip-typed-params-empty-list
  "strip-typed-params on empty list returns two empty lists."
  (multiple-value-bind (plain type-alist)
      (cl-cc/expand::strip-typed-params '())
    (assert-null plain)
    (assert-null type-alist)))

;;; ── register-function-type / *function-type-registry* ──────────────────────

(deftest register-function-type-stores-entry
  "register-function-type stores the (param-types . return-type) pair."
  (let ((cl-cc/expand::*function-type-registry*
         (make-hash-table :test #'eq)))
    (cl-cc/expand::register-function-type 'my-fn '(fixnum) 'string)
    (let ((entry (gethash 'my-fn cl-cc/expand::*function-type-registry*)))
      (assert-true entry)
      (assert-equal '(fixnum) (car entry))
      (assert-eq 'string (cdr entry)))))

(deftest register-function-type-overwrites-previous
  "A second register-function-type call overwrites the previous entry."
  (let ((cl-cc/expand::*function-type-registry*
         (make-hash-table :test #'eq)))
    (cl-cc/expand::register-function-type 'f '(fixnum) 'fixnum)
    (cl-cc/expand::register-function-type 'f '(string) 'string)
    (let ((entry (gethash 'f cl-cc/expand::*function-type-registry*)))
      (assert-equal '(string) (car entry))
      (assert-eq 'string (cdr entry)))))

(deftest function-type-registry-unknown-name-returns-nil
  "Lookup of an unregistered name returns nil."
  (let ((cl-cc/expand::*function-type-registry*
         (make-hash-table :test #'eq)))
    (assert-null (gethash 'no-such-fn cl-cc/expand::*function-type-registry*))))
