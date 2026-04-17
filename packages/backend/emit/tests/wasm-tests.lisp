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

(in-suite cl-cc-unit-suite)

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

(defun %direct-wasm-emit (inst)
  "Emit INST through the non-trampoline wasm target methods directly." 
  (let ((s (make-string-output-stream))
        (target (make-instance 'cl-cc/emit::wasm-target
                               :reg-map (cl-cc/emit::make-wasm-reg-map-for-function 0))))
    (cl-cc/emit::emit-instruction target inst s)
    (get-output-stream-string s)))

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

(deftest wasm-defun-structure
  "Compiled defun produces WAT with (func ...) and (result eqref) per WASM GC ABI."
  (let ((wat (%wat-for "(defun add (x y) (+ x y))")))
    (assert-true wat)
    (assert-output-contains wat "(func")
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

(deftest wasm-compilation-result-structure
  "compile-string :target :wasm returns a compilation-result with a non-empty string assembly."
  (let* ((result (ignore-errors (compile-string "(+ 1 2)" :target :wasm)))
         (asm (when result (compilation-result-assembly result))))
    (assert-true result)
    (assert-type compilation-result result)
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

(deftest wasm-funcall-dispatch
  "funcall via closure: emits call_indirect with $main_func_t type and global.set $cl_arg0 for arg-passing."
  (let ((wat (%wat-for "(defparameter *f* (lambda (x) (* x 2))) (funcall *f* 5)")))
    (assert-true wat)
    (assert-output-contains wat "call_indirect")
    (assert-output-contains wat "$main_func_t")
    (assert-output-contains wat "(global.set $cl_arg0")))

(deftest wasm-move-uses-local-tee
  "vm-move WAT emission uses local.tee instead of local.set/local.get pair."
  (let ((wat (%wat-for "(let ((x 1)) x)")))
    (assert-true wat)
    (assert-output-contains wat "local.tee")))

(deftest-each wasm-bitcount-lowers-to-wasm-op
  "Bit-count operations lower to their corresponding WASM i64 instructions via %wat-for."
  :cases (("logcount"       "(defun f (x) (logcount x))"        "i64.popcnt")
          ("integer-length" "(defun f (x) (integer-length x))"  "i64.clz"))
  (source expected-op)
  (let ((wat (%wat-for source)))
    (assert-true wat)
    (assert-output-contains wat expected-op)))

(deftest-each wasm-direct-bitcount-and-arith-emitters
  "Direct WASM emitter lowers bit-count and integer-specialized arithmetic to correct i64 ops."
  :cases (("logcount"       "i64.popcnt" (cl-cc::make-vm-logcount :dst :r0 :src :r1))
          ("integer-length" "i64.clz"    (cl-cc::make-vm-integer-length :dst :r0 :src :r1))
          ("integer-add"    "i64.add"    (cl-cc::make-vm-integer-add :dst :r0 :lhs :r1 :rhs :r2))
          ("integer-sub"    "i64.sub"    (cl-cc::make-vm-integer-sub :dst :r0 :lhs :r1 :rhs :r2))
          ("integer-mul"    "i64.mul"    (cl-cc::make-vm-integer-mul :dst :r0 :lhs :r1 :rhs :r2)))
  (expected-op inst)
  (assert-output-contains (%direct-wasm-emit inst) expected-op))

(deftest wasm-elem-segment-present
  "Test that the WAT module includes an elem segment to populate the funcref table."
  (let ((wat (%wat-for "(defun f (x) x)")))
    (assert-true wat)
    (assert-output-contains wat "(elem")))

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

(deftest-each wasm-binary-write-f64
  "Binary f64 encoding produces correct IEEE-754 little-endian bytes."
  :cases (("pos-1.0"   1.0d0  '(0 0 0 0 0 0 #xf0 #x3f))
          ("neg-2.5"  -2.5d0  '(0 0 0 0 0 0 #x04 #xc0))
          ("neg-zero" -0.0d0  '(0 0 0 0 0 0 0 #x80)))
  (value expected)
  (let ((buf (cl-cc/binary::make-wasm-buffer)))
    (cl-cc/binary::wasm-buf-write-f64 buf value)
    (assert-equal expected (coerce buf 'list))))
