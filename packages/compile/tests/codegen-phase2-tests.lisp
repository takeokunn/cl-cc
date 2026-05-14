;;;; tests/unit/compile/codegen-phase2-tests.lisp — Phase-2 AST-Introspecting Builtin Handler Tests
;;;
;;; Tests for compile-ast dispatch to Phase-2 handlers in codegen-phase2.lisp.
;;; Each handler is triggered when an ast-call is compiled with a matching
;;; function symbol; returns result-reg on success or nil to fall through.
;;;
;;; Helpers make-codegen-ctx / codegen-instructions / codegen-find-inst are
;;; defined in codegen-tests.lisp (same suite, loaded before this file).

(in-package :cl-cc/test)
(in-suite cl-cc-codegen-unit-serial-suite)

;;; ─── Section 1: GETHASH ─────────────────────────────────────────────────────

(deftest-each codegen-phase2-gethash-cases
  "gethash: 2-arg form has nil default slot; 3-arg form sets the default register."
  :cases (("two-args"
           (list (make-ast-quote :value :k) (make-ast-quote :value :ht))
           nil)
          ("three-args"
           (list (make-ast-quote :value :k) (make-ast-quote :value :ht) (make-ast-int :value 0))
           t))
  (args default-set-p)
  (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast (make-ast-call :func 'gethash :args args) ctx)))
      (let ((inst (codegen-find-inst ctx 'cl-cc:vm-gethash)))
        (assert-true inst)
        (assert-true (keywordp reg))
        (if default-set-p
            (assert-true  (cl-cc::vm-gethash-default inst))
            (assert-true  (null (cl-cc::vm-gethash-default inst))))))))


;;; ─── Section 2: MAPHASH ─────────────────────────────────────────────────────

(deftest codegen-phase2-maphash-compilation
  "Compiling (maphash fn ht): emits vm-hash-table-keys, vm-call, and a nil-const for void return."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'maphash
                                :args (list (make-ast-quote :value 'my-fn)
                                            (make-ast-quote :value 'my-ht)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-hash-table-keys))
    (assert-true (codegen-find-inst ctx 'cl-cc:vm-call))
    (let ((consts (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-const))
                                  (codegen-instructions ctx))))
      (assert-true (some (lambda (i) (null (cl-cc::vm-const-value i))) consts)))))

;;; ─── Section 3: MAKE-ARRAY ──────────────────────────────────────────────────

(deftest codegen-phase2-make-array
  "Compiling (make-array n): emits vm-make-array; no fill-pointer or adjustable; works with size zero."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-array
                                :args (list (make-ast-int :value 5)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-array)))
      (assert-true inst)
      (assert-true (null (cl-cc::vm-make-array-fill-pointer inst)))
      (assert-true (null (cl-cc::vm-make-array-adjustable inst)))))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-array
                                :args (list (make-ast-int :value 0)))
                  ctx)
     (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-make-array))))

(deftest codegen-phase2-make-array-element-type
  "Compiling (make-array n :element-type 'character) stores element-type in vm-make-array."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-array
                                :args (list (make-ast-int :value 3)
                                            (make-ast-var :name :element-type)
                                            (make-ast-quote :value 'character)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-array)))
      (assert-true inst)
      (assert-eq 'character (cl-cc/vm::vm-element-type inst)))))

(deftest-each codegen-phase2-make-array-dynamic-keywords-fall-through
  "MAKE-ARRAY only lowers keyword metadata that is statically known."
  :cases (("dynamic-fill-pointer"
           (list (make-ast-int :value 5)
                 (make-ast-var :name :fill-pointer)
                 (make-ast-var :name 'fp)))
          ("dynamic-adjustable"
           (list (make-ast-int :value 5)
                  (make-ast-var :name :adjustable)
                  (make-ast-var :name 'adj)))
           ("dynamic-initial-contents"
            (list (make-ast-int :value 5)
                  (make-ast-var :name :initial-contents)
                  (make-ast-var :name 'contents)))
          ("dynamic-element-type"
           (list (make-ast-int :value 5)
                 (make-ast-var :name :element-type)
                 (make-ast-var :name 'etype))))
  (args)
  (let ((ctx (make-ctx-with-vars 'fp 'adj 'contents 'etype)))
    (compile-ast (make-ast-call :func 'make-array :args args) ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc/vm::vm-make-array)))))

;;; ─── Section 4: MAKE-ADJUSTABLE-VECTOR ─────────────────────────────────────

(deftest codegen-phase2-make-adjustable-vector-compilation
  "Compiling (make-adjustable-vector n): emits vm-make-array with fill-pointer t and adjustable t."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-adjustable-vector
                                :args (list (make-ast-int :value 8)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-array)))
      (assert-true inst)
      (assert-true (cl-cc::vm-make-array-fill-pointer inst))
      (assert-true (cl-cc::vm-make-array-adjustable inst)))))

;;; ─── Section 5: TYPEP ───────────────────────────────────────────────────────

(deftest codegen-phase2-typep-quoted-emits-vm-typep
  "Compiling (typep x 'integer) emits vm-typep."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'typep
                                :args (list (make-ast-int :value 42)
                                            (make-ast-quote :value 'integer)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-typep))))

(deftest-each codegen-phase2-typep-type-name-stored
  "Compiling (typep x 'TYPE) stores TYPE as the type-name in the vm-typep instruction."
  :cases (("string" 'string)
          ("list"   'list))
  (type-sym)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'typep
                                :args (list (make-ast-int :value 42)
                                            (make-ast-quote :value type-sym)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-typep)))
      (assert-true inst)
      (assert-eq type-sym (cl-cc::vm-typep-type-name inst)))))

(deftest codegen-phase2-typep-unquoted-falls-through
  "Compiling (typep x integer) with unquoted type does not emit vm-typep."
  (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'integer :R99)))
    (compile-ast (make-ast-call :func 'typep
                                :args (list (make-ast-int :value 1)
                                            (make-ast-var :name 'integer)))
                 ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc/vm::vm-typep)))))

;;; ─── Section 6: FORMAT ──────────────────────────────────────────────────────

(deftest-each codegen-phase2-format-dispatch
  "format: nil-dest folds to vm-const; t-dest emits vm-princ; 1-arg falls through."
  :cases (("nil-dest"
           (list (make-ast-var :name nil) (make-ast-quote :value "hello"))
           :nil-dest)
          ("t-dest"
           (list (make-ast-var :name t) (make-ast-quote :value "hello"))
           :t-dest)
          ("one-arg"
           (list (make-ast-var :name nil))
           :one-arg))
  (args scenario)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'format :args args) ctx)
    (ecase scenario
      (:nil-dest
       (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
         (assert-true inst)
         (assert-equal "hello" (cl-cc::vm-const-value inst))
         (assert-true (null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst)))
         (assert-true (null (codegen-find-inst ctx 'cl-cc/vm::vm-princ)))))
      (:t-dest
       (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-princ))
       (assert-true (null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst))))
      (:one-arg
       (assert-true (null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst)))))))

;;; ─── Section 7: MAKE-HASH-TABLE ─────────────────────────────────────────────

(deftest-each codegen-phase2-make-hash-table-cases
  "make-hash-table: no-args has nil test slot; :test 'equal sets the test register."
  :cases (("no-args"    '()  nil)
          ("test-equal" (list (make-ast-var :name :test) (make-ast-quote :value 'equal)) t))
  (args test-set-p)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-hash-table :args args) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-hash-table)))
      (assert-true inst)
      (if test-set-p
          (assert-true  (cl-cc::vm-make-hash-table-test inst))
          (assert-true  (null (cl-cc::vm-make-hash-table-test inst)))))))

;;; ─── Section 8: CONCATENATE ─────────────────────────────────────────────────

(deftest codegen-phase2-concatenate-constant-strings-fold-to-vm-const
  "Compiling (concatenate 'string foo bar) folds to a single vm-const."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'concatenate
                                :args (list (make-ast-quote :value 'string)
                                            (make-ast-quote :value "foo")
                                            (make-ast-quote :value "bar")))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (assert-true inst)
      (assert-equal "foobar" (cl-cc::vm-const-value inst))
      (assert-true (null (codegen-find-inst ctx 'cl-cc/vm::vm-concatenate))))))

(deftest codegen-phase2-concatenate-three-strings-emits-two-concats
  "Compiling (concatenate 'string a b c) folds the chain with two vm-concatenate instructions."
  (let ((ctx (make-ctx-with-vars 'a 'b 'c)))
    (compile-ast (make-ast-call :func 'concatenate
                                :args (list (make-ast-quote :value 'string)
                                            (make-ast-var :name 'a)
                                            (make-ast-var :name 'b)
                                            (make-ast-var :name 'c)))
                 ctx)
    (assert-eql 2 (codegen-count-inst ctx 'cl-cc/vm::vm-concatenate))))

(deftest-each codegen-phase2-concatenate-non-string-falls-through
  "Compiling (concatenate TYPE ...) with non-string or unquoted type does NOT emit vm-concatenate."
  :cases (("non-string-type"
           nil
           (list (make-ast-quote :value 'list)
                 (make-ast-quote :value "a")
                 (make-ast-quote :value "b")))
          ("unquoted-type"
           (list (cons 'string :R99))
           (list (make-ast-var :name 'string)
                 (make-ast-quote :value "a")
                 (make-ast-quote :value "b"))))
  (extra-env args)
  (let ((ctx (make-codegen-ctx)))
    (when extra-env
      (setf (cl-cc/compile:ctx-env ctx) extra-env))
    (compile-ast (make-ast-call :func 'concatenate :args args) ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc/vm::vm-concatenate)))))
