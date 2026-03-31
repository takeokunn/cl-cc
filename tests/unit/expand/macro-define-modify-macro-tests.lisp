;;;; tests/unit/expand/macro-define-modify-macro-tests.lisp — DEFINE-MODIFY-MACRO tests

(in-package :cl-cc/test)

(defsuite macro-define-modify-macro-suite
  :description "DEFINE-MODIFY-MACRO expansion tests"
  :parent cl-cc-suite)

(in-suite macro-define-modify-macro-suite)

(deftest define-modify-macro-expands-to-our-defmacro
  "DEFINE-MODIFY-MACRO expands to an OUR-DEFMACRO form"
  (let ((result (our-macroexpand-1 '(define-modify-macro incf-by (delta) +))))
    (assert-eq (car result) 'cl-cc:our-defmacro)))

(deftest define-modify-macro-names-the-macro
  "DEFINE-MODIFY-MACRO names the generated macro correctly"
  (let ((result (our-macroexpand-1 '(define-modify-macro my-incf (n) +))))
    (assert-eq (car result) 'cl-cc:our-defmacro)
    (assert-eq (cadr result) 'my-incf)))

(deftest define-modify-macro-lambda-list-has-place-first
  "DEFINE-MODIFY-MACRO generated macro takes PLACE as first parameter"
  (let* ((result (our-macroexpand-1 '(define-modify-macro my-push (item) cons)))
         (params (caddr result)))
    (assert-eq (car result) 'cl-cc:our-defmacro)
    (assert-= (length params) 2)))

(deftest define-modify-macro-no-extra-args
  "DEFINE-MODIFY-MACRO with empty lambda list still generates a valid macro"
  (let ((result (our-macroexpand-1 '(define-modify-macro toggle-flag () not))))
    (assert-eq (car result) 'cl-cc:our-defmacro)
    (assert-eq (cadr result) 'toggle-flag)
    (assert-= (length (caddr result)) 1)))

(deftest define-modify-macro-body-contains-setf
  "DEFINE-MODIFY-MACRO generated macro body contains a SETF form"
  (let* ((result (our-macroexpand-1 '(define-modify-macro my-incf (n) +)))
         (body (cadddr result)))
    (assert-true (not (null body)))))

(deftest define-modify-macro-with-optional-arg
  "DEFINE-MODIFY-MACRO with &optional in lambda list works"
  (let ((result (our-macroexpand-1 '(define-modify-macro my-add (&optional (n 1)) +))))
    (assert-eq (car result) 'cl-cc:our-defmacro)
    (assert-eq (cadr result) 'my-add)))

(deftest define-modify-macro-with-docstring
  "DEFINE-MODIFY-MACRO accepts an optional docstring without error"
  (let ((result (our-macroexpand-1
                  '(define-modify-macro my-mul (factor) * "Multiply place by factor."))))
    (assert-eq (car result) 'cl-cc:our-defmacro)
    (assert-eq (cadr result) 'my-mul)))
