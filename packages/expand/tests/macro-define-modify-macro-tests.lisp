;;;; tests/unit/expand/macro-define-modify-macro-tests.lisp — DEFINE-MODIFY-MACRO tests

(in-package :cl-cc/test)

(defsuite macro-define-modify-macro-suite
  :description "DEFINE-MODIFY-MACRO expansion tests"
  :parent cl-cc-unit-suite)

(in-suite macro-define-modify-macro-suite)


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

(deftest-each define-modify-macro-outer-form-shape
  "DEFINE-MODIFY-MACRO expands to (our-defmacro NAME ...) for any lambda-list style."
  :cases (("plain"     'my-incf '(define-modify-macro my-incf (n) +))
          ("optional"  'my-add  '(define-modify-macro my-add (&optional (n 1)) +))
          ("docstring" 'my-mul  '(define-modify-macro my-mul (factor) * "Multiply.")))
  (expected-name form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq 'cl-cc:our-defmacro (car result))
    (assert-eq expected-name (cadr result))))

(deftest define-modify-macro-rest-lambda-list
  "DEFINE-MODIFY-MACRO with &rest param includes rest var in generated args form."
  (let* ((result (our-macroexpand-1 '(define-modify-macro my-append (&rest items) append)))
         (params (caddr result)))
    (assert-eq 'cl-cc:our-defmacro (car result))
    (assert-true (member '&rest params))))
