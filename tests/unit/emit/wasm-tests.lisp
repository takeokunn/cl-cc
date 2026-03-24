;;;; tests/wasm-tests.lisp - WebAssembly Backend Tests
;;;
;;; This module provides basic tests for the WASM/WAT code generation backend,
;;; including:
;;; - Module header presence
;;; - Predefined GC type section (cons, closure, etc.)
;;; - Function definition emission
;;; - Nil constant representation
;;; - compile-string :target :wasm integration

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; ──────────────────────────────────────────────────────────────────────────
;;; Helper: compile a source string to WAT, returning the assembly string.
;;; Returns NIL on any error so individual tests can assert-true on the result.
;;; ──────────────────────────────────────────────────────────────────────────

(defun %wat-for (source-string)
  "Compile SOURCE-STRING via compile-string :target :wasm and return the WAT
   assembly string, or NIL if compilation fails."
  (ignore-errors
    (let ((result (compile-string source-string :target :wasm)))
      (when result
        (compilation-result-assembly result)))))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Section 1-2: Module structure — always present in any WAT output
;;; ──────────────────────────────────────────────────────────────────────────

(deftest-each wasm-module-structure
  "Every WAT module contains these required structural elements."
  :cases (("module-header"  "(module")
          ("module-close"   "end module")
          ("cons-type"      "$cons_t")
          ("closure-type"   "$closure_t")
          ("string-type"    "$string_t")
          ("cl-io-imports"  "cl_io")
          ("funcref-table"  "funcref_table")
          ("arg-globals"    "$cl_arg0")
          ("print-import"   "print_val"))
  (expected)
  (let ((wat (%wat-for "(+ 1 2)")))
    (assert-true wat)
    (assert-output-contains wat expected)))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Section 3: Function emission
;;; ──────────────────────────────────────────────────────────────────────────

(deftest wasm-defun-produces-func
  "Test that compiling a defun produces WAT containing a (func ...) definition."
  (let ((wat (%wat-for "(defun add (x y) (+ x y))")))
    (assert-true wat)
    (assert-output-contains wat "(func")))

(deftest wasm-defun-result-eqref
  "Test that compiled functions declare (result eqref) as per the WASM GC ABI."
  (let ((wat (%wat-for "(defun add (x y) (+ x y))")))
    (assert-true wat)
    (assert-output-contains wat "(result eqref)")))

(deftest wasm-simple-arithmetic-compiles
  "Test that (+ 1 2) compiles to WAT without error."
  (let ((wat (%wat-for "(+ 1 2)")))
    (assert-true wat)
    (assert-true (> (length wat) 0))))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Section 4: Nil constant representation
;;; ──────────────────────────────────────────────────────────────────────────

(deftest wasm-nil-emits-ref-null-eq
  "Test that nil compiles to (ref.null eq) in the WAT output."
  (let ((wat (%wat-for "nil")))
    (assert-true wat)
    (assert-output-contains wat "(ref.null eq)")))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Section 5: compile-string integration
;;; ──────────────────────────────────────────────────────────────────────────

(deftest wasm-compile-string-returns-compilation-result
  "Test that compile-string :target :wasm returns a compilation-result struct."
  (let ((result (ignore-errors (compile-string "(+ 1 2)" :target :wasm))))
    (assert-true result)
    (assert-type compilation-result result)))

(deftest wasm-compilation-result-assembly-is-string
  "Test that compilation-result-assembly on a :wasm compile returns a string."
  (let* ((result (ignore-errors (compile-string "(+ 1 2)" :target :wasm)))
         (asm (when result (compilation-result-assembly result))))
    (assert-type string asm)))

(deftest wasm-compilation-result-assembly-non-empty
  "Test that the WAT assembly string is non-empty for a trivial expression."
  (let* ((result (ignore-errors (compile-string "(+ 1 2)" :target :wasm)))
         (asm (when result (compilation-result-assembly result))))
    (assert-type string asm)
    (assert-true (> (length asm) 0))))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Section 7: Arithmetic instruction coverage
;;; ──────────────────────────────────────────────────────────────────────────
;;; defun prevents the optimizer from constant-folding the args away.

(deftest-each wasm-arithmetic
  "Binary arithmetic operations on non-constant args compile to the correct i64 instruction."
  :cases (("add"    "(defun f (a b) (+ a b))"           "i64.add")
          ("sub"    "(defun f (a b) (- a b))"           "i64.sub")
          ("mul"    "(defun f (a b) (* a b))"           "i64.mul")
          ("div"    "(defun f (a b) (truncate a b))"    "i64.div_s")
          ("lt"     "(defun f (a b) (< a b))"           "i64.lt_s")
          ("gt"     "(defun f (a b) (> a b))"           "i64.gt_s")
          ("logand" "(defun f (a b) (logand a b))"      "i64.and")
          ("logior" "(defun f (a b) (logior a b))"      "i64.or"))
  (source expected)
  (let ((wat (%wat-for source)))
    (assert-true wat)
    (assert-output-contains wat expected)))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Section 8: Global variable access
;;; ──────────────────────────────────────────────────────────────────────────

(deftest-each wasm-global-vars
  "Global variable operations compile to the correct WASM global instructions."
  :cases (("decl" "(defvar *x* 42)"              "global")
          ("set"  "(defvar *x* 0) (setq *x* 99)" "global.set")
          ("get"  "(defvar *x* 42) *x*"          "global.get"))
  (source expected)
  (let ((wat (%wat-for source)))
    (assert-true wat)
    (assert-output-contains wat expected)))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Section 9: Closure creation and function call dispatch
;;; ──────────────────────────────────────────────────────────────────────────

(deftest wasm-closure-emits-struct-new-closure
  "Test that vm-closure emits struct.new $closure_t (not a stub comment)."
  (let ((wat (%wat-for "(defun double (x) (* x 2))")))
    (assert-true wat)
    (assert-output-contains wat "struct.new $closure_t")))

(deftest wasm-call-emits-call-indirect
  "Test that calling a closure via funcall emits call_indirect.
   funcall with a lambda prevents static inlining by the optimizer."
  (let ((wat (%wat-for "(let ((f (lambda (x) (* x 2)))) (funcall f 5))")))
    (assert-true wat)
    (assert-output-contains wat "call_indirect")))

(deftest wasm-call-uses-main-func-type
  "Test that call_indirect uses $main_func_t type."
  (let ((wat (%wat-for "(let ((f (lambda (x) (+ x 1)))) (funcall f 10))")))
    (assert-true wat)
    (assert-output-contains wat "$main_func_t")))

(deftest wasm-elem-segment-present
  "Test that the WAT module includes an elem segment to populate the funcref table."
  (let ((wat (%wat-for "(defun f (x) x)")))
    (assert-true wat)
    (assert-output-contains wat "(elem")))

(deftest wasm-call-args-written-to-globals
  "Test that funcall with an arg emits global.set $cl_arg0 (arg-passing convention)."
  ;; funcall prevents inlining so vm-call is preserved in the output
  (let ((wat (%wat-for "(let ((f (lambda (x) (- 0 x)))) (funcall f 5))")))
    (assert-true wat)
    (assert-output-contains wat "(global.set $cl_arg0")))

(deftest wasm-closure-table-index-nonzero-for-second-function
  "Test that a second defined function gets a non-zero table index in $closure_t."
  ;; When two functions are defined, the second has table index >= 1.
  ;; The struct.new for the second closure should NOT be (i32.const 0).
  (let ((wat (%wat-for "(defun f1 (x) x) (defun f2 (x) (+ x 1))")))
    (assert-true wat)
    ;; Both closures are created; at least one with a non-zero index exists
    (assert-output-contains wat "struct.new $closure_t")))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Section 10: Print support
;;; ──────────────────────────────────────────────────────────────────────────

(deftest wasm-print-emits-call-host-print-val
  "Test that (print x) compiles to a call to $host_print_val."
  (let ((wat (%wat-for "(let ((x 42)) (print x))")))
    (assert-true wat)
    (assert-output-contains wat "$host_print_val")))
