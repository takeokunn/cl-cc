;;;; tests/unit/compile/codegen-io-tests.lisp — Codegen I/O tests

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ── WRITE-STRING ─────────────────────────────────────────────────────────

(deftest phase2-write-string-one-arg-emits-princ
  "(write-string str) with no stream emits vm-princ"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'write-string (make-quoted "hello")) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-princ))))

(deftest phase2-write-string-two-args-emits-stream-write
  "(write-string str stream) emits vm-stream-write-string-inst"
  (let ((ctx (make-ctx-with-vars 'stream)))
    (compile-ast (make-call 'write-string (make-quoted "hello") (make-var 'stream)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-stream-write-string-inst))))

;;; ── FORMAT ────────────────────────────────────────────────────────────────

(deftest phase2-format-destinations
  "format emits correct instructions for nil, t, and stream destinations"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "~A")
                            (make-int 42))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-format-inst)))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "~A")
                            (make-int 1))
                 ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-princ))))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 't)
                            (make-quoted "~A")
                            (make-int 1))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-format-inst))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-princ)))
  (let ((ctx (make-ctx-with-vars 'out-stream)))
    (compile-ast (make-call 'format
                            (make-var 'out-stream)
                            (make-quoted "hello")
                            (make-int 1))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-format-inst))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-stream-write-string-inst))))

(deftest phase2-format-requires-two-args
  "(format nil) with only 1 arg falls through (handler guard: >= 2 args)"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format (make-var 'nil)) ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-format-inst)))))

;;; ── OPEN ──────────────────────────────────────────────────────────────────

(deftest phase2-open-variants
  "open emits vm-open-file with correct direction for all call forms"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'open (make-quoted "/tmp/f")) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-open-file)))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'open (make-quoted "/tmp/f")) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-open-file)))
      (assert-eq :input (cl-cc::vm-open-file-direction inst))))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'open
                            (make-quoted "/tmp/f")
                            (make-var :direction)
                            (make-var :output))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-open-file)))
      (assert-eq :output (cl-cc::vm-open-file-direction inst)))))

;;; ── PEEK-CHAR ────────────────────────────────────────────────────────────

(deftest phase2-peek-char-arities
  "peek-char emits vm-peek-char for both 1-arg and 2-arg forms"
  (let ((ctx (make-ctx-with-vars 'handle)))
    (compile-ast (make-call 'peek-char (make-var 'handle)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-peek-char)))
  (let ((ctx (make-ctx-with-vars 'handle)))
    (compile-ast (make-call 'peek-char (make-var 'nil) (make-var 'handle)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-peek-char))))

;;; ── MAKE-STRING-INPUT-STREAM ─────────────────────────────────────────────

(deftest phase2-make-string-input-stream-emits-instruction
  "(make-string-input-stream str) emits vm-make-string-stream"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-string-input-stream (make-quoted "hello")) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-string-stream))))

(deftest phase2-make-string-input-stream-direction
  "(make-string-input-stream str) sets direction to :input"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-string-input-stream (make-quoted "hi")) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-string-stream)))
      (assert-eq :input (cl-cc::vm-make-string-stream-direction inst)))))

;;; ── CONCATENATE ──────────────────────────────────────────────────────────

(deftest phase2-concatenate-variants
  "concatenate emits vm-concatenate only for quoted 'string type"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'concatenate
                            (make-quoted 'string)
                            (make-quoted "hello")
                            (make-quoted " world"))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-concatenate)))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'concatenate
                            (make-quoted 'list)
                            (make-quoted "a")
                            (make-quoted "b"))
                 ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-concatenate))))
  (let ((ctx (make-ctx-with-vars 'string)))
    (compile-ast (make-call 'concatenate
                            (make-var 'string)
                            (make-quoted "a")
                            (make-quoted "b"))
                 ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-concatenate)))))
