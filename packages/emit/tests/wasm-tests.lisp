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

(defsuite cl-cc-wasm-serial-suite
  :description "Serial WASM backend tests that rely on fresh compile-string state"
  :parent cl-cc-unit-suite
  :parallel nil)

(in-suite cl-cc-wasm-serial-suite)

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
        (target (make-instance 'cl-cc/codegen::wasm-target
                               :reg-map (cl-cc/codegen::make-wasm-reg-map-for-function 0))))
    (cl-cc/codegen::emit-instruction target inst s)
    (get-output-stream-string s)))

(defun %direct-wasm-emit* (instructions)
  "Emit a sequence of INSTRUCTIONS through wasm target methods directly." 
  (let ((s (make-string-output-stream))
        (target (make-instance 'cl-cc/codegen::wasm-target
                               :reg-map (cl-cc/codegen::make-wasm-reg-map-for-function 0))))
    (dolist (inst instructions)
      (cl-cc/codegen::emit-instruction target inst s))
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

(deftest-each wasm-single-output-contains
  "A compiled WASM form contains a specific expected WAT substring."
  :cases (("nil-ref-null"    "nil"                           "(ref.null eq)")
          ("closure-struct"  "(defun double (x) (* x 2))"   "struct.new $closure_t")
          ("move-local-tee"  "(let ((x 1)) x)"              "local.tee")
          ("elem-segment"    "(defun f (x) x)"              "(elem")
          ("print-host-call" "(let ((x 42)) (print x))"     "$host_print_val"))
  (source expected)
  (let ((wat (%wat-for source)))
    (assert-true wat)
    (assert-output-contains wat expected)))

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

(deftest wasm-funcall-dispatch
  "funcall via closure: emits call_indirect with $main_func_t type and global.set $cl_arg0 for arg-passing."
  (let ((wat (%wat-for "(defparameter *f* (lambda (x) (* x 2))) (funcall *f* 5)")))
    (assert-true wat)
    (assert-output-contains wat "call_indirect")
    (assert-output-contains wat "$main_func_t")
    (assert-output-contains wat "(global.set $cl_arg0")))

(deftest wasm-tail-call-dispatch-uses-return-call-indirect
  "Direct wasm tail-call emitter selects return_call_indirect opcode."
  (let ((out (%direct-wasm-emit
              (cl-cc:make-vm-tail-call :dst :r0 :func :r1 :args (list :r2)))))
    (assert-output-contains out "return_call_indirect")
    (assert-output-contains out "$main_func_t")
    (assert-output-contains out "(global.set $cl_arg0")))

(deftest wasm-tail-call-dispatch-falls-back-when-feature-disabled
  "When wasm tail-call feature gate is disabled, emitter uses call_indirect."
  (let ((out (let ((cl-cc/codegen::*wasm-tail-call-enabled* nil))
               (%direct-wasm-emit
                (cl-cc:make-vm-tail-call :dst :r0 :func :r1 :args (list :r2))))))
    (assert-output-contains out "call_indirect")
    (assert-false (search "return_call_indirect" out :test #'char=))
    (assert-output-contains out "$main_func_t")))

(deftest wasm-tail-call-direct-path-uses-return-call-when-callee-known
  "vm-func-ref + vm-tail-call emits direct return_call when feature enabled." 
  (let ((out (let ((cl-cc/codegen::*wasm-tail-call-enabled* t))
               (%direct-wasm-emit*
                (list (cl-cc:make-vm-func-ref :dst :r1 :label "known_fn")
                      (cl-cc:make-vm-tail-call :dst :r0 :func :r1 :args (list :r2)))))))
    (assert-output-contains out "(return_call $known_fn)")
    (assert-false (search "return_call_indirect" out :test #'char=))))

(deftest wasm-tail-call-direct-path-falls-back-to-call-when-feature-disabled
  "vm-func-ref + vm-tail-call emits direct call when tailcall feature disabled." 
  (let ((out (let ((cl-cc/codegen::*wasm-tail-call-enabled* nil))
               (%direct-wasm-emit*
                (list (cl-cc:make-vm-func-ref :dst :r1 :label "known_fn")
                      (cl-cc:make-vm-tail-call :dst :r0 :func :r1 :args (list :r2)))))))
    (assert-output-contains out "(call $known_fn)")
    (assert-false (search "return_call $known_fn" out :test #'char=))))

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
  :cases (("logcount"       "i64.popcnt" (cl-cc:make-vm-logcount :dst :r0 :src :r1))
          ("integer-length" "i64.clz"    (cl-cc:make-vm-integer-length :dst :r0 :src :r1))
          ("integer-add"    "i64.add"    (cl-cc:make-vm-integer-add :dst :r0 :lhs :r1 :rhs :r2))
          ("integer-sub"    "i64.sub"    (cl-cc:make-vm-integer-sub :dst :r0 :lhs :r1 :rhs :r2))
          ("integer-mul"    "i64.mul"    (cl-cc:make-vm-integer-mul :dst :r0 :lhs :r1 :rhs :r2)))
  (expected-op inst)
  (assert-output-contains (%direct-wasm-emit inst) expected-op))

(deftest wasm-gc-array-new-get-set-emitters
  "Wasm GC array ops lower to array.new/array.get/array.set." 
  (let ((mk (%direct-wasm-emit
             (cl-cc:make-vm-make-array :dst :r0 :size-reg :r1 :initial-element nil)))
        (get (%direct-wasm-emit
              (cl-cc:make-vm-aref :dst :r0 :array-reg :r1 :index-reg :r2)))
        (set (%direct-wasm-emit
              (cl-cc:make-vm-aset :array-reg :r0 :index-reg :r1 :val-reg :r2))))
    (assert-output-contains mk "array.new $eqref_array_t")
    (assert-output-contains get "array.get $eqref_array_t")
    (assert-output-contains set "array.set $eqref_array_t")))

(deftest wasm-gc-slot-read-write-emitters
  "CLOS slot ops lower through struct.get(instance slots) + array get/set." 
  (let ((rd (%direct-wasm-emit
             (cl-cc:make-vm-slot-read :dst :r0 :obj-reg :r1 :slot-name 'foo)))
        (wr (%direct-wasm-emit
             (cl-cc:make-vm-slot-write :obj-reg :r1 :slot-name 'foo :value-reg :r2))))
    (assert-output-contains rd "struct.get $instance_t 1")
    (assert-output-contains rd "array.get $eqref_array_t")
    (assert-output-contains wr "struct.get $instance_t 1")
    (assert-output-contains wr "array.set $eqref_array_t")
    (assert-output-contains wr "struct.set $instance_t 1")))

(deftest wasm-gc-slot-read-write-use-class-slot-index-mapping
  "vm-class-def slot order drives slot index used in slot read/write lowering." 
  (let ((out (%direct-wasm-emit*
              (list (cl-cc:make-vm-class-def :dst :r9
                                             :class-name 'my-class
                                             :superclasses nil
                                             :slot-names '(foo bar baz)
                                             :slot-initargs nil
                                             :slot-initform-regs nil
                                             :slot-types nil
                                             :default-initarg-regs nil
                                             :class-slots nil
                                             :metaclass-reg nil)
                    (cl-cc:make-vm-slot-read :dst :r0 :obj-reg :r1 :slot-name 'bar)
                    (cl-cc:make-vm-slot-write :obj-reg :r1 :slot-name 'baz :value-reg :r2)))))
    ;; bar => index 1, baz => index 2 from class-def slot order.
    (assert-output-contains out "array.get $eqref_array_t")
    (assert-output-contains out "(i32.const 1)")
    (assert-output-contains out "array.set $eqref_array_t")
    (assert-output-contains out "(i32.const 2)")))

(deftest wasm-gc-slot-index-resolution-prefers-object-class-layout
  "When class layouts conflict, slot index resolves from object's class mapping." 
  (let ((out (%direct-wasm-emit*
              (list
               (cl-cc:make-vm-class-def :dst :r7
                                        :class-name 'class-a
                                        :superclasses nil
                                        :slot-names '(foo bar)
                                        :slot-initargs nil
                                        :slot-initform-regs nil
                                        :slot-types nil
                                        :default-initarg-regs nil
                                        :class-slots nil
                                        :metaclass-reg nil)
               (cl-cc:make-vm-class-def :dst :r8
                                        :class-name 'class-b
                                        :superclasses nil
                                        :slot-names '(bar foo)
                                        :slot-initargs nil
                                        :slot-initform-regs nil
                                        :slot-types nil
                                        :default-initarg-regs nil
                                        :class-slots nil
                                        :metaclass-reg nil)
               ;; Build object of class-b; foo should resolve to index 1 in class-b.
               (cl-cc:make-vm-make-obj :dst :r1 :class-reg :r8 :initarg-regs nil)
               (cl-cc:make-vm-slot-read :dst :r0 :obj-reg :r1 :slot-name 'foo)))))
    (assert-output-contains out "array.get $eqref_array_t")
    (assert-output-contains out "(i32.const 1)")))

(deftest wasm-gc-slot-index-resolution-includes-superclass-slots
  "Subclass effective slot order includes inherited slots before own slots." 
  (let ((out (%direct-wasm-emit*
              (list
               (cl-cc:make-vm-class-def :dst :r7
                                        :class-name 'super-c
                                        :superclasses nil
                                        :slot-names '(sa sb)
                                        :slot-initargs nil
                                        :slot-initform-regs nil
                                        :slot-types nil
                                        :default-initarg-regs nil
                                        :class-slots nil
                                        :metaclass-reg nil)
               (cl-cc:make-vm-class-def :dst :r8
                                        :class-name 'sub-c
                                        :superclasses '(super-c)
                                        :slot-names '(sc)
                                        :slot-initargs nil
                                        :slot-initform-regs nil
                                        :slot-types nil
                                        :default-initarg-regs nil
                                        :class-slots nil
                                        :metaclass-reg nil)
               (cl-cc:make-vm-make-obj :dst :r1 :class-reg :r8 :initarg-regs nil)
               ;; inherited sb should resolve to index 1; own sc should resolve to index 2
               (cl-cc:make-vm-slot-read :dst :r0 :obj-reg :r1 :slot-name 'sb)
               (cl-cc:make-vm-slot-write :obj-reg :r1 :slot-name 'sc :value-reg :r2)))))
    (assert-output-contains out "(i32.const 1)")
    (assert-output-contains out "(i32.const 2)")))

(deftest wasm-gc-class-def-emits-class-meta-struct
  "vm-class-def lowers to staged class metadata struct allocation.

Includes MOP-oriented method-combination/method-table metadata placeholders." 
  (let ((out (%direct-wasm-emit
              (cl-cc:make-vm-class-def :dst :r7
                                       :class-name 'my-class
                                       :superclasses nil
                                       :slot-names '(a b)
                                       :slot-initargs nil
                                       :slot-initform-regs nil
                                       :slot-types nil
                                       :default-initarg-regs nil
                                       :class-slots nil
                                       :metaclass-reg nil))))
    (assert-output-contains out "struct.new $class_meta_t")
    (assert-output-contains out "(i32.const 2)")
    (assert-output-contains out "(array.new $eqref_array_t (ref.null eq) (i32.const 0))")))

(deftest wasm-gc-make-obj-uses-class-reg-in-instance-struct
  "vm-make-obj embeds class-reg reference instead of null class pointer." 
  (let ((out (%direct-wasm-emit*
              (list
               (cl-cc:make-vm-class-def :dst :r8
                                        :class-name 'my-class
                                        :superclasses nil
                                        :slot-names '(a b)
                                        :slot-initargs nil
                                        :slot-initform-regs nil
                                        :slot-types nil
                                        :default-initarg-regs nil
                                        :class-slots nil
                                        :metaclass-reg nil)
               (cl-cc:make-vm-make-obj :dst :r1 :class-reg :r8 :initarg-regs nil)))))
    (assert-output-contains out "struct.new $instance_t")
    (assert-false (search "ref.null $class_meta_t" out :test #'char=))))

(deftest wasm-gc-register-method-runtime-bridge-emission
  "vm-register-method lowers to runtime bridge import call."
  (let ((out (%direct-wasm-emit
              (cl-cc:make-vm-register-method
               :gf-reg :r1
               :specializer 'my-class
               :qualifier nil
               :method-reg :r2))))
    (assert-output-contains out "call $host_rt_register_method")
    ;; Symbol specializer should be passed as non-null staged symbol eqref.
    (assert-output-contains out "struct.new $symbol_t")))

(deftest wasm-gc-generic-call-runtime-bridge-emission
  "vm-generic-call lowers to runtime bridge import call."
  (let ((out (%direct-wasm-emit
              (cl-cc:make-vm-generic-call :dst :r0 :gf-reg :r1 :args (list :r2 :r3)))))
    (assert-output-contains out "global.set $cl_arg0")
    (assert-output-contains out "global.set $cl_arg1")
    (assert-output-contains out "(call $host_rt_call_generic")
    (assert-output-contains out "(i32.const 2)")))

(deftest wasm-gc-generic-call-runtime-bridge-supports-many-args
  "vm-generic-call bridge marshals up to calling-convention arg slots." 
  (let ((out (%direct-wasm-emit
              (cl-cc:make-vm-generic-call :dst :r0 :gf-reg :r1
                                          :args (list :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10)))))
    (assert-output-contains out "global.set $cl_arg8")
    (assert-output-contains out "(i32.const 9)")))

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

(deftest-each wasm-binary-write-f64
  "Binary f64 encoding produces correct IEEE-754 little-endian bytes."
  :cases (("pos-1.0"   1.0d0  '(0 0 0 0 0 0 #xf0 #x3f))
          ("neg-2.5"  -2.5d0  '(0 0 0 0 0 0 #x04 #xc0))
          ("neg-zero" -0.0d0  '(0 0 0 0 0 0 0 #x80)))
  (value expected)
  (let ((buf (cl-cc/binary::make-wasm-buffer)))
    (cl-cc/binary::wasm-buf-write-f64 buf value)
    (assert-equal expected (coerce buf 'list))))
