;;;; tests/unit/expand/macro-shiftf-tests.lisp — SHIFTF macro tests

(in-package :cl-cc/test)

(defsuite macro-shiftf-suite
  :description "SHIFTF expansion tests"
  :parent cl-cc-suite)

(in-suite macro-shiftf-suite)

(deftest shiftf-two-place-structure
  "SHIFTF (shiftf place newval) expands to LET + SETF returning old value"
  (let ((result (our-macroexpand-1 '(shiftf x 99))))
    (assert-eq (car result) 'let)
    (assert-= (length (cadr result)) 1)
    (assert-eq (cadr (caadr result)) 'x)
    (assert-eq (car (caddr result)) 'setf)
    (assert-eq (cadr (caddr result)) 'x)
    (let ((tmp-var (first (caadr result))))
      (assert-eq (car (last result)) tmp-var))))

(deftest shiftf-returns-first-old-value
  "SHIFTF returns the old value of the first place (the leading temp)"
  (let ((result (our-macroexpand-1 '(shiftf a b 0))))
    (let ((first-temp (first (caadr result))))
      (assert-eq (car (last result)) first-temp))))

(deftest shiftf-three-place-chain
  "SHIFTF with three places shifts values through a chain"
  (let ((result (our-macroexpand-1 '(shiftf a b c 0))))
    (assert-eq (car result) 'let)
    (assert-= (length (cadr result)) 3)
    (assert-eq (car (caddr result)) 'setf)
    (assert-eq (car (cadddr result)) 'setf)
    (let ((first-temp (first (caadr result))))
      (assert-eq (car (last result)) first-temp))))

(deftest-each shiftf-insufficient-args-signals-error
  "SHIFTF signals an error when called with fewer than two arguments."
  :cases (("no-args"  '(shiftf))
          ("one-arg"  '(shiftf x)))
  (form)
  (assert-signals error (our-macroexpand-1 form)))

(deftest integration-shiftf-full-expansion
  "Full expansion of SHIFTF does not contain the SHIFTF symbol"
  (let ((result (our-macroexpand '(shiftf a b 0))))
    (assert-false (search "shiftf" (string-downcase (format nil "~S" result))))))
