;;;; tests/unit/compile/codegen-phase2-tests.lisp — Phase-2 AST-Introspecting Builtin Handler Tests
;;;
;;; Tests for compile-ast dispatch to Phase-2 handlers in codegen-phase2.lisp.
;;; Each handler is triggered when an ast-call is compiled with a matching
;;; function symbol; returns result-reg on success or nil to fall through.
;;;
;;; Helpers make-codegen-ctx / codegen-instructions / codegen-find-inst are
;;; defined in codegen-tests.lisp (same suite, loaded before this file).

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── Section 1: GETHASH ─────────────────────────────────────────────────────

(deftest codegen-phase2-gethash-two-args
  "Compiling (gethash key table): emits vm-gethash with nil default; returns register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-call :func 'gethash
                                          :args (list (make-ast-quote :value :k)
                                                      (make-ast-quote :value :ht)))
                           ctx))
         (inst (codegen-find-inst ctx 'cl-cc:vm-gethash)))
    (assert-true inst)
    (assert-true (null (cl-cc::vm-gethash-default inst)))
    (assert-true (keywordp reg))))

(deftest codegen-phase2-gethash-three-args-default-register-set
  "Compiling (gethash key table default) sets the default register in vm-gethash."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'gethash
                                :args (list (make-ast-quote :value :k)
                                            (make-ast-quote :value :ht)
                                            (make-ast-int :value 0)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc:vm-gethash)))
      (assert-true inst)
      (assert-true (cl-cc::vm-gethash-default inst)))))


;;; ─── Section 2: MAPHASH ─────────────────────────────────────────────────────

(deftest codegen-phase2-maphash-compilation
  "Compiling (maphash fn ht): emits vm-hash-table-keys, vm-call, and a nil-const for void return."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'maphash
                                :args (list (make-ast-quote :value 'my-fn)
                                            (make-ast-quote :value 'my-ht)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-hash-table-keys))
    (assert-true (codegen-find-inst ctx 'cl-cc:vm-call))
    (let ((consts (remove-if-not (lambda (i) (typep i 'cl-cc::vm-const))
                                  (codegen-instructions ctx))))
      (assert-true (some (lambda (i) (null (cl-cc::vm-const-value i))) consts)))))

;;; ─── Section 3: MAKE-ARRAY ──────────────────────────────────────────────────

(deftest codegen-phase2-make-array
  "Compiling (make-array n): emits vm-make-array; no fill-pointer or adjustable; works with size zero."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-array
                                :args (list (make-ast-int :value 5)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-array)))
      (assert-true inst)
      (assert-true (null (cl-cc::vm-make-array-fill-pointer inst)))
      (assert-true (null (cl-cc::vm-make-array-adjustable inst)))))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-array
                                :args (list (make-ast-int :value 0)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-array))))

;;; ─── Section 4: MAKE-ADJUSTABLE-VECTOR ─────────────────────────────────────

(deftest codegen-phase2-make-adjustable-vector-compilation
  "Compiling (make-adjustable-vector n): emits vm-make-array with fill-pointer t and adjustable t."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-adjustable-vector
                                :args (list (make-ast-int :value 8)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-array)))
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
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-typep))))

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
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-typep)))
      (assert-true inst)
      (assert-eq type-sym (cl-cc::vm-typep-type-name inst)))))

(deftest codegen-phase2-typep-unquoted-falls-through
  "Compiling (typep x integer) with unquoted type does not emit vm-typep."
  (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc::ctx-env ctx) (list (cons 'integer :R99)))
    (compile-ast (make-ast-call :func 'typep
                                :args (list (make-ast-int :value 1)
                                            (make-ast-var :name 'integer)))
                 ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-typep)))))

;;; ─── Section 6: FORMAT ──────────────────────────────────────────────────────

(deftest codegen-phase2-format-nil-dest
  "Compiling (format nil \"fmt\") folds to a constant string."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'format
                                :args (list (make-ast-var :name nil)
                                            (make-ast-quote :value "hello")))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-equal "hello" (cl-cc::vm-const-value inst))
      (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-format-inst)))
      (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-princ))))))

(deftest codegen-phase2-format-t-dest-emits-format-and-princ
  "Compiling (format t \"fmt\") emits vm-princ without vm-format-inst."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'format
                                :args (list (make-ast-var :name t)
                                            (make-ast-quote :value "hello")))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-princ))
    (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-format-inst)))))

(deftest codegen-phase2-format-requires-at-least-two-args
  "Compiling (format nil) with only 1 arg falls through — no vm-format-inst."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'format
                                :args (list (make-ast-var :name nil)))
                 ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-format-inst)))))

;;; ─── Section 7: MAKE-HASH-TABLE ─────────────────────────────────────────────

(deftest codegen-phase2-make-hash-table-no-args
  "Compiling (make-hash-table): emits vm-make-hash-table with nil test slot."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-hash-table :args '())
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-hash-table)))
      (assert-true inst)
      (assert-true (null (cl-cc::vm-make-hash-table-test inst))))))

(deftest codegen-phase2-make-hash-table-quoted-test-equal
  "Compiling (make-hash-table :test 'equal) sets test register."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-hash-table
                                :args (list (make-ast-var :name :test)
                                            (make-ast-quote :value 'equal)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-hash-table)))
      (assert-true inst)
      (assert-true (cl-cc::vm-make-hash-table-test inst)))))

;;; ─── Section 8: CONCATENATE ─────────────────────────────────────────────────

(deftest codegen-phase2-concatenate-constant-strings-fold-to-vm-const
  "Compiling (concatenate 'string foo bar) folds to a single vm-const."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'concatenate
                                :args (list (make-ast-quote :value 'string)
                                            (make-ast-quote :value "foo")
                                            (make-ast-quote :value "bar")))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-equal "foobar" (cl-cc::vm-const-value inst))
      (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-concatenate))))))

(deftest codegen-phase2-concatenate-three-strings-emits-two-concats
  "Compiling (concatenate 'string a b c) folds the chain with two vm-concatenate instructions."
  (let ((ctx (make-ctx-with-vars 'a 'b 'c)))
    (compile-ast (make-ast-call :func 'concatenate
                                :args (list (make-ast-quote :value 'string)
                                            (make-ast-var :name 'a)
                                            (make-ast-var :name 'b)
                                            (make-ast-var :name 'c)))
                 ctx)
    (assert-eql 2 (codegen-count-inst ctx 'cl-cc::vm-concatenate))))

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
      (setf (cl-cc::ctx-env ctx) extra-env))
    (compile-ast (make-ast-call :func 'concatenate :args args) ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-concatenate)))))

;;; ─── Section 9: MAKE-STRING-INPUT-STREAM ────────────────────────────────────

(deftest codegen-phase2-make-string-input-stream-compilation
  "Compiling (make-string-input-stream str): emits vm-make-string-stream with :input direction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-string-input-stream
                                :args (list (make-ast-quote :value "hello")))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-string-stream)))
      (assert-true inst)
      (assert-eq :input (cl-cc::vm-make-string-stream-direction inst)))))

;;; ─── Section 10: OPEN ───────────────────────────────────────────────────────

(deftest-each codegen-phase2-open-direction
  "(open path) defaults to :input; explicit :direction :output sets :output."
  :cases (("default-input"
           (list (make-ast-quote :value "/tmp/in.txt"))
           :input)
          ("explicit-output"
           (list (make-ast-quote :value "/tmp/out.txt")
                 (make-ast-var :name :direction)
                 (make-ast-var :name :output))
           :output))
  (args expected-dir)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'open :args args) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-open-file)))
      (assert-true inst)
      (assert-eq expected-dir (cl-cc::vm-open-file-direction inst)))))

;;; ─── Section 11: PEEK-CHAR ──────────────────────────────────────────────────

(deftest-each codegen-phase2-peek-char-emits-vm-peek-char
  "Compiling peek-char with 1 or 2 args always emits vm-peek-char."
  :cases (("one-arg" :R10 (list (make-ast-var :name 'handle)))
          ("two-args" :R11 (list (make-ast-var :name nil)
                                  (make-ast-var :name 'handle))))
  (reg args)
  (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc::ctx-env ctx) (list (cons 'handle reg)))
    (compile-ast (make-ast-call :func 'peek-char :args args) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-peek-char))))

;;; ─── Section 12: WRITE-STRING ───────────────────────────────────────────────

(deftest-each codegen-phase2-write-string-arg-dispatch
  "write-string routes to vm-princ (no stream) or vm-stream-write-string-inst (with stream)."
  :cases (("one-arg"
           nil
           (list (make-ast-quote :value "hello"))
           'cl-cc::vm-princ)
          ("two-args"
           (list (cons 'out :R20))
           (list (make-ast-quote :value "hello")
                 (make-ast-var :name 'out))
           'cl-cc::vm-stream-write-string-inst))
  (env args inst-type)
  (let ((ctx (make-codegen-ctx)))
    (when env
      (setf (cl-cc::ctx-env ctx) env))
    (compile-ast (make-ast-call :func 'write-string :args args) ctx)
    (assert-true (codegen-find-inst ctx inst-type))))
