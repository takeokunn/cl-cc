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

(deftest codegen-phase2-gethash-two-args-emits-vm-gethash
  "Compiling (gethash key table) emits a vm-gethash instruction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'gethash
                                :args (list (make-ast-quote :value 'my-key)
                                            (make-ast-quote :value 'my-ht)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc:vm-gethash))))

(deftest codegen-phase2-gethash-two-args-default-is-nil
  "Compiling (gethash key table) with no default leaves default register nil."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'gethash
                                :args (list (make-ast-quote :value :k)
                                            (make-ast-quote :value :ht)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc:vm-gethash)))
      (assert-true inst)
      (assert-true (null (cl-cc::vm-gethash-default inst))))))

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

(deftest codegen-phase2-gethash-returns-register
  "Compiling (gethash key table) returns a keyword register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-call :func 'gethash
                                          :args (list (make-ast-quote :value :k)
                                                      (make-ast-quote :value :ht)))
                           ctx)))
    (assert-true (keywordp reg))))

;;; ─── Section 2: MAPHASH ─────────────────────────────────────────────────────

(deftest codegen-phase2-maphash-emits-vm-hash-table-keys
  "Compiling (maphash fn ht) emits a vm-hash-table-keys instruction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'maphash
                                :args (list (make-ast-quote :value 'my-fn)
                                            (make-ast-quote :value 'my-ht)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-hash-table-keys))))

(deftest codegen-phase2-maphash-emits-vm-call
  "Compiling (maphash fn ht) emits a vm-call to apply the function to each entry."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'maphash
                                :args (list (make-ast-quote :value 'my-fn)
                                            (make-ast-quote :value 'my-ht)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc:vm-call))))

(deftest codegen-phase2-maphash-result-is-nil
  "Compiling (maphash fn ht) emits a vm-const nil for the void return value."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'maphash
                                :args (list (make-ast-quote :value 'my-fn)
                                            (make-ast-quote :value 'my-ht)))
                 ctx)
    (let ((consts (remove-if-not (lambda (i) (typep i 'cl-cc::vm-const))
                                  (codegen-instructions ctx))))
      (assert-true (some (lambda (i) (null (cl-cc::vm-const-value i))) consts)))))

;;; ─── Section 3: MAKE-ARRAY ──────────────────────────────────────────────────

(deftest codegen-phase2-make-array-emits-vm-make-array
  "Compiling (make-array n) emits a vm-make-array instruction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-array
                                :args (list (make-ast-int :value 10)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-array))))

(deftest codegen-phase2-make-array-fixed-not-adjustable
  "Compiling (make-array n) produces a non-adjustable array with no fill-pointer."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-array
                                :args (list (make-ast-int :value 5)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-array)))
      (assert-true inst)
      (assert-true (null (cl-cc::vm-make-array-fill-pointer inst)))
      (assert-true (null (cl-cc::vm-make-array-adjustable inst))))))

(deftest codegen-phase2-make-array-size-zero
  "Compiling (make-array 0) emits vm-make-array with a zero-size constant."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-array
                                :args (list (make-ast-int :value 0)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-array))))

;;; ─── Section 4: MAKE-ADJUSTABLE-VECTOR ─────────────────────────────────────

(deftest codegen-phase2-make-adjustable-vector-emits-vm-make-array
  "Compiling (make-adjustable-vector n) emits a vm-make-array instruction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-adjustable-vector
                                :args (list (make-ast-int :value 8)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-array))))

(deftest codegen-phase2-make-adjustable-vector-is-adjustable
  "Compiling (make-adjustable-vector n) sets fill-pointer t and adjustable t."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-adjustable-vector
                                :args (list (make-ast-int :value 8)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-array)))
      (assert-true inst)
      (assert-true (cl-cc::vm-make-array-fill-pointer inst))
      (assert-true (cl-cc::vm-make-array-adjustable inst)))))

;;; ─── Section 5: TYPEP ───────────────────────────────────────────────────────

(deftest codegen-phase2-typep-quoted-integer-emits-vm-typep
  "Compiling (typep 42 'integer) emits a vm-typep instruction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'typep
                                :args (list (make-ast-int :value 42)
                                            (make-ast-quote :value 'integer)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-typep))))

(deftest codegen-phase2-typep-type-name-stored
  "Compiling (typep x 'string) stores 'string as the type-name in vm-typep."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'typep
                                :args (list (make-ast-int :value 42)
                                            (make-ast-quote :value 'string)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-typep)))
      (assert-true inst)
      (assert-eq 'string (cl-cc::vm-typep-type-name inst)))))

(deftest codegen-phase2-typep-quoted-list-type
  "Compiling (typep x 'list) emits vm-typep with type-name list."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'typep
                                :args (list (make-ast-int :value 99)
                                            (make-ast-quote :value 'list)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-typep)))
      (assert-true inst)
      (assert-eq 'list (cl-cc::vm-typep-type-name inst)))))

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

(deftest codegen-phase2-format-nil-dest-emits-vm-format-inst
  "Compiling (format nil fmt arg) emits vm-format-inst."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'format
                                :args (list (make-ast-var :name nil)
                                            (make-ast-quote :value "~A")
                                            (make-ast-int :value 42)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-format-inst))))

(deftest codegen-phase2-format-nil-dest-no-princ
  "Compiling (format nil fmt) does NOT emit vm-princ — result stays in register."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'format
                                :args (list (make-ast-var :name nil)
                                            (make-ast-quote :value "~A")
                                            (make-ast-int :value 1)))
                 ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-princ)))))

(deftest codegen-phase2-format-t-dest-emits-format-and-princ
  "Compiling (format t fmt) emits both vm-format-inst and vm-princ."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'format
                                :args (list (make-ast-var :name t)
                                            (make-ast-quote :value "~A")))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-format-inst))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-princ))))

(deftest codegen-phase2-format-requires-at-least-two-args
  "Compiling (format nil) with only 1 arg falls through — no vm-format-inst."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'format
                                :args (list (make-ast-var :name nil)))
                 ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-format-inst)))))

;;; ─── Section 7: MAKE-HASH-TABLE ─────────────────────────────────────────────

(deftest codegen-phase2-make-hash-table-no-args-emits-instruction
  "Compiling (make-hash-table) with no args emits vm-make-hash-table."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-hash-table :args '())
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-hash-table))))

(deftest codegen-phase2-make-hash-table-no-args-test-slot-nil
  "Compiling (make-hash-table) without :test leaves test register nil."
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

(deftest codegen-phase2-concatenate-string-type-emits-vm-concatenate
  "Compiling (concatenate 'string a b) emits vm-concatenate."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'concatenate
                                :args (list (make-ast-quote :value 'string)
                                            (make-ast-quote :value "foo")
                                            (make-ast-quote :value "bar")))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-concatenate))))

(deftest codegen-phase2-concatenate-non-string-type-falls-through
  "Compiling (concatenate 'list a b) does NOT emit vm-concatenate."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'concatenate
                                :args (list (make-ast-quote :value 'list)
                                            (make-ast-quote :value "a")
                                            (make-ast-quote :value "b")))
                 ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-concatenate)))))

(deftest codegen-phase2-concatenate-unquoted-type-falls-through
  "Compiling (concatenate string a b) with unquoted type falls through."
  (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc::ctx-env ctx) (list (cons 'string :R99)))
    (compile-ast (make-ast-call :func 'concatenate
                                :args (list (make-ast-var :name 'string)
                                            (make-ast-quote :value "a")
                                            (make-ast-quote :value "b")))
                 ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-concatenate)))))

;;; ─── Section 9: MAKE-STRING-INPUT-STREAM ────────────────────────────────────

(deftest codegen-phase2-make-string-input-stream-emits-vm-make-string-stream
  "Compiling (make-string-input-stream str) emits vm-make-string-stream."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-string-input-stream
                                :args (list (make-ast-quote :value "hello")))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-string-stream))))

(deftest codegen-phase2-make-string-input-stream-direction-is-input
  "Compiling (make-string-input-stream str) sets direction to :input."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-string-input-stream
                                :args (list (make-ast-quote :value "world")))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-string-stream)))
      (assert-true inst)
      (assert-eq :input (cl-cc::vm-make-string-stream-direction inst)))))

;;; ─── Section 10: OPEN ───────────────────────────────────────────────────────

(deftest codegen-phase2-open-emits-vm-open-file
  "Compiling (open path) emits a vm-open-file instruction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'open
                                :args (list (make-ast-quote :value "/tmp/test.txt")))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-open-file))))

(deftest codegen-phase2-open-defaults-to-input-direction
  "Compiling (open path) with no :direction keyword defaults to :input."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'open
                                :args (list (make-ast-quote :value "/tmp/in.txt")))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-open-file)))
      (assert-true inst)
      (assert-eq :input (cl-cc::vm-open-file-direction inst)))))

(deftest codegen-phase2-open-with-output-direction
  "Compiling (open path :direction :output) sets direction to :output."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'open
                                :args (list (make-ast-quote :value "/tmp/out.txt")
                                            (make-ast-var :name :direction)
                                            (make-ast-var :name :output)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-open-file)))
      (assert-true inst)
      (assert-eq :output (cl-cc::vm-open-file-direction inst)))))

;;; ─── Section 11: PEEK-CHAR ──────────────────────────────────────────────────

(deftest codegen-phase2-peek-char-one-arg-emits-vm-peek-char
  "Compiling (peek-char handle) with 1 arg emits vm-peek-char."
  (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc::ctx-env ctx) (list (cons 'handle :R10)))
    (compile-ast (make-ast-call :func 'peek-char
                                :args (list (make-ast-var :name 'handle)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-peek-char))))

(deftest codegen-phase2-peek-char-two-args-emits-vm-peek-char
  "Compiling (peek-char nil handle) with 2 args emits vm-peek-char."
  (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc::ctx-env ctx) (list (cons 'handle :R11)))
    (compile-ast (make-ast-call :func 'peek-char
                                :args (list (make-ast-var :name nil)
                                            (make-ast-var :name 'handle)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-peek-char))))

;;; ─── Section 12: WRITE-STRING ───────────────────────────────────────────────

(deftest codegen-phase2-write-string-one-arg-emits-vm-princ
  "Compiling (write-string str) with no stream emits vm-princ."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'write-string
                                :args (list (make-ast-quote :value "hello")))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-princ))))

(deftest codegen-phase2-write-string-two-args-emits-stream-write
  "Compiling (write-string str stream) emits vm-stream-write-string-inst."
  (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc::ctx-env ctx) (list (cons 'out :R20)))
    (compile-ast (make-ast-call :func 'write-string
                                :args (list (make-ast-quote :value "hello")
                                            (make-ast-var :name 'out)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-stream-write-string-inst))))
