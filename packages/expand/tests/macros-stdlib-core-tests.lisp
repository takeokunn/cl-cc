;;;; tests/unit/expand/macros-stdlib-core-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib.lisp

(in-package :cl-cc/test)

(defsuite macros-stdlib-core-suite
  :description "Tests for macros-stdlib.lisp: core arithmetic and return forms"
  :parent cl-cc-unit-suite)

(in-suite macros-stdlib-core-suite)

(deftest-each 1+-1--expansion
  "1+ and 1- are shorthand for (+ n 1) and (- n 1)."
  :cases (("1+" '(1+ n) '(+ n 1))
          ("1-" '(1- n) '(- n 1)))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest signum-expansion
  "SIGNUM wraps n in a LET (avoids double evaluation) and dispatches via COND."
  (let* ((result (our-macroexpand-1 '(signum n)))
         (body   (caddr result)))
    (assert-eq (car result) 'let)
    (assert-eq (car body)   'cond)))

(deftest-each return-expansion
  "return expands to (return-from nil ...) with optional value."
  :cases (("with-value" '(return v)  '(return-from nil v))
          ("no-value"   '(return)    '(return-from nil nil)))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest fr-217-await-non-wasm-synchronous-fallback
  "AWAIT preserves the form for VM/native synchronous targets."
  (let ((cl-cc/expand::*target-backend* nil))
    (assert-equal '(compute-value)
                  (our-macroexpand-1 '(await (compute-value))))))

(deftest fr-217-async-handler-non-wasm-handler-case-fallback
  "ASYNC-HANDLER uses HANDLER-CASE on non-Wasm targets."
  (let ((cl-cc/expand::*target-backend* nil))
    (assert-equal '(handler-case (cl-cc/expand::await (compute-value))
                    (error (e) e))
                  (our-macroexpand-1
                   '(async-handler (compute-value)
                      (error (e) e))))))

(deftest fr-217-await-wasm-lowers-through-js-await-intrinsic
  "AWAIT on Wasm evaluates once and lowers Promise refs through the JS await intrinsic."
  (let ((cl-cc/expand::*target-backend* :wasm32))
    (let ((result (our-macroexpand-1 '(await (compute-promise)))))
      (assert-eq 'let (car result))
      (assert-true (%tree-contains-head-p 'cl-cc/expand::%wasm-promise-reference-p result))
      (assert-true (%tree-contains-head-p 'cl-cc/expand::%wasm-js-await result)))))

(deftest fr-217-async-handler-wasm-lowers-rejection-catch
  "ASYNC-HANDLER on Wasm wires Promise .catch() into condition clauses."
  (let ((cl-cc/expand::*target-backend* :wasm32))
    (let ((result (our-macroexpand-1
                   '(async-handler (compute-promise)
                      (error (e) :caught)))))
      (assert-eq 'handler-case (car result))
      (assert-true (%tree-contains-head-p 'cl-cc/expand::%wasm-js-catch result))
      (assert-true (%tree-contains-head-p 'cl-cc/expand::await result)))))
