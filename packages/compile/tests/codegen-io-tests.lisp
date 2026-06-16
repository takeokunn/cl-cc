;;;; tests/unit/compile/codegen-io-tests.lisp — Codegen I/O output tests
;;;; Read/print tests → codegen-io-read-tests.lisp.

(in-package :cl-cc/test)
(in-suite cl-cc-codegen-unit-suite)

;;; ── WRITE-STRING ─────────────────────────────────────────────────────────

(deftest-each phase2-write-string-arg-dispatch
  "write-string routes to vm-princ (no stream) or vm-stream-write-string-inst (with stream)."
  :cases (("one-arg"  (make-codegen-ctx)          (make-call 'write-string (make-quoted "hello"))                          'cl-cc/vm::vm-princ)
          ("two-args" (make-ctx-with-vars 'stream) (make-call 'write-string (make-quoted "hello") (make-var 'stream))      'cl-cc/vm::vm-stream-write-string-inst))
  (ctx form inst-type)
  (compile-ast form ctx)
  (assert-true (codegen-find-inst ctx inst-type)))

;;; ── FORMAT ────────────────────────────────────────────────────────────────

(deftest phase2-format-destinations
  "format emits correct instructions for nil, t, and stream destinations"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "~A")
                            (make-int 42))
                 ctx)
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-princ-to-string-inst)))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "~A")
                            (make-int 1))
                 ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc/vm::vm-princ))))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 't)
                            (make-quoted "~A")
                            (make-int 1))
                 ctx)
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-princ)))
  (let ((ctx (make-ctx-with-vars 'out-stream)))
    (compile-ast (make-call 'format
                            (make-var 'out-stream)
                            (make-quoted "hello")
                            (make-int 1))
                 ctx)
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-stream-write-string-inst))))

(deftest phase2-format-static-string-lowering
  "static format strings lower supported directives without vm-format-inst."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "~A ~S~~~%")
                            (make-quoted "x")
                            (make-quoted "y"))
                 ctx)
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-princ-to-string-inst))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-write-to-string-inst))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-concatenate))))

(deftest phase2-format-static-repeat-literal-lowering
  "static repeat literal directives lower without vm-format-inst."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "a~2%~3~~2|z"))
                 ctx)
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-concatenate))))

(deftest phase2-format-static-empty-string-lowering
  "empty static format output lowers without vm-format-inst."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "~0%"))
                 ctx)
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst))))

(deftest phase2-format-static-fallbacks
  "dynamic format strings and unsupported static directives still use vm-format-inst."
  (let ((ctx (make-ctx-with-vars 'fmt)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-var 'fmt)
                            (make-int 1))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst)))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "~{~A~}")
                            (make-quoted '(1 2)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst)))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "~A ~A")
                            (make-int 1))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst)))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "~2A")
                            (make-int 1))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst))))

(deftest phase2-format-requires-two-args
  "(format nil) with only 1 arg falls through (handler guard: >= 2 args)"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format (make-var 'nil)) ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst)))))

;;; ── OPEN ──────────────────────────────────────────────────────────────────

(deftest phase2-open-variants
  "open emits vm-open-file with correct direction for all call forms"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'open (make-quoted "/tmp/f")) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-open-file)))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'open (make-quoted "/tmp/f")) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-open-file)))
      (assert-eq :input (cl-cc::vm-open-file-direction inst))))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'open
                            (make-quoted "/tmp/f")
                            (make-var :direction)
                            (make-var :output))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-open-file)))
      (assert-eq :output (cl-cc::vm-open-file-direction inst)))))

;;; ── PEEK-CHAR ────────────────────────────────────────────────────────────

(deftest phase2-peek-char-arities
  "peek-char emits vm-peek-char for both 1-arg and 2-arg forms"
  (let ((ctx (make-ctx-with-vars 'handle)))
    (compile-ast (make-call 'peek-char (make-var 'handle)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-peek-char)))
  (let ((ctx (make-ctx-with-vars 'handle)))
    (compile-ast (make-call 'peek-char (make-var 'nil) (make-var 'handle)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-peek-char))))

;;; ── MAKE-STRING-INPUT-STREAM ─────────────────────────────────────────────

(deftest phase2-make-string-input-stream-compilation
  "(make-string-input-stream str) emits vm-make-string-stream with :input direction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-string-input-stream (make-quoted "hello")) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-string-stream)))
      (assert-true inst)
      (assert-eq :input (cl-cc::vm-make-string-stream-direction inst)))))

;;; ── CONCATENATE ──────────────────────────────────────────────────────────

(deftest phase2-concatenate-variants
  "concatenate emits vm-concatenate only for quoted 'string type with a
non-literal string argument. All-literal strings get constant-folded by
phase2 into a single vm-const, so at least one argument must be a variable
for vm-concatenate to actually be emitted."
  (let ((ctx (make-ctx-with-vars 'suffix)))
    (compile-ast (make-call 'concatenate
                            (make-quoted 'string)
                            (make-quoted "hello ")
                            (make-var 'suffix))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-concatenate)))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'concatenate
                            (make-quoted 'list)
                            (make-quoted "a")
                            (make-quoted "b"))
                 ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc/vm::vm-concatenate))))
  (let ((ctx (make-ctx-with-vars 'string)))
    (compile-ast (make-call 'concatenate
                            (make-var 'string)
                            (make-quoted "a")
                            (make-quoted "b"))
                 ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc/vm::vm-concatenate)))))
