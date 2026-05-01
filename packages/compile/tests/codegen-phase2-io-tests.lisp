;;;; tests/unit/compile/codegen-phase2-io-tests.lisp — Phase-2 I/O Handler Tests
;;;
;;; Continuation of codegen-phase2-tests.lisp.
;;; Tests for make-string-input-stream, open, peek-char, and write-string handlers.
;;;
;;; Helpers make-codegen-ctx / codegen-instructions / codegen-find-inst are
;;; defined in codegen-tests.lisp (same suite, loaded before this file).

(in-package :cl-cc/test)
(in-suite cl-cc-codegen-unit-serial-suite)

;;; ─── Section 9: MAKE-STRING-INPUT-STREAM ────────────────────────────────────

(deftest codegen-phase2-make-string-input-stream-compilation
  "Compiling (make-string-input-stream str): emits vm-make-string-stream with :input direction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-string-input-stream
                                :args (list (make-ast-quote :value "hello")))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-string-stream)))
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
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-open-file)))
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
    (setf (cl-cc/compile::ctx-env ctx) (list (cons 'handle reg)))
    (compile-ast (make-ast-call :func 'peek-char :args args) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-peek-char))))

;;; ─── Section 12: WRITE-STRING ───────────────────────────────────────────────

(deftest-each codegen-phase2-write-string-arg-dispatch
  "write-string routes to vm-princ (no stream) or vm-stream-write-string-inst (with stream)."
  :cases (("one-arg"
           nil
           (list (make-ast-quote :value "hello"))
           'cl-cc/vm::vm-princ)
          ("two-args"
           (list (cons 'out :R20))
           (list (make-ast-quote :value "hello")
                 (make-ast-var :name 'out))
           'cl-cc/vm::vm-stream-write-string-inst))
  (env args inst-type)
  (let ((ctx (make-codegen-ctx)))
    (when env
      (setf (cl-cc/compile::ctx-env ctx) env))
    (compile-ast (make-ast-call :func 'write-string :args args) ctx)
    (assert-true (codegen-find-inst ctx inst-type))))
