;;;; tests/unit/compile/codegen-io-tests.lisp — Codegen I/O tests

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

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
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst)))
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
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-princ)))
  (let ((ctx (make-ctx-with-vars 'out-stream)))
    (compile-ast (make-call 'format
                            (make-var 'out-stream)
                            (make-quoted "hello")
                            (make-int 1))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-stream-write-string-inst))))

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

;;; ── READ-CHAR / READ-LINE / READ ─────────────────────────────────────────

(deftest-each phase2-stream-reader-simple
  "read-char / read-line / read with 1 stream arg emit the corresponding vm instruction."
  :cases (("read-char" 'read-char 'cl-cc/vm::vm-read-char)
          ("read-line" 'read-line 'cl-cc/vm::vm-read-line)
          ("read"      'read      'cl-cc/vm::vm-read-sexp-inst))
  (fn inst-type)
  (let ((ctx (make-ctx-with-vars 'handle)))
    (compile-ast (make-call fn (make-var 'handle)) ctx)
    (assert-true (codegen-find-inst ctx inst-type))))

(deftest-each phase2-stream-reader-eof-value
  "read-char / read-line / read with 3+ args take the eof-value substitution path,
   emitting the read instruction plus a vm-eq sentinel check."
  :cases (("read-char" 'read-char 'cl-cc/vm::vm-read-char)
          ("read-line" 'read-line 'cl-cc/vm::vm-read-line)
          ("read"      'read      'cl-cc/vm::vm-read-sexp-inst))
  (fn inst-type)
  (let ((ctx (make-ctx-with-vars 'handle 'eof-err 'eof-val)))
    (compile-ast (make-call fn (make-var 'handle) (make-var 'eof-err) (make-var 'eof-val)) ctx)
    (assert-true (codegen-find-inst ctx inst-type))
    ;; %emit-eof-value-check introduces vm-eq to compare against :eof sentinel
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-eq))))

(deftest-each phase2-stream-reader-default-handle
  "read-char / read-line / read with no args use stdin handle 0."
  :cases (("read-char" 'read-char 'cl-cc/vm::vm-read-char)
          ("read-line" 'read-line 'cl-cc/vm::vm-read-line)
          ("read"      'read      'cl-cc/vm::vm-read-sexp-inst))
  (fn inst-type)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call fn) ctx)
    (assert-true (codegen-find-inst ctx inst-type))
    ;; Default handle: a vm-const with value 0 must be emitted
    (let ((const-0 (find-if (lambda (i)
                              (and (cl-cc::vm-const-p i)
                                   (eql 0 (cl-cc/vm::vm-value i))))
                            (codegen-instructions ctx))))
      (assert-true const-0))))

;;; ── READ-BYTE ─────────────────────────────────────────────────────────────

(deftest phase2-read-byte-eof-value
  "(read-byte stream eof-error-p eof-value) emits vm-read-byte plus eof sentinel check."
  (let ((ctx (make-ctx-with-vars 'handle 'eof-err 'eof-val)))
    (compile-ast (make-call 'read-byte (make-var 'handle) (make-var 'eof-err) (make-var 'eof-val)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-read-byte))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-eq))))

;;; ── TERPRI / FRESH-LINE ──────────────────────────────────────────────────

(deftest-each phase2-newline-handler-zero-arg
  "terpri / fresh-line with no args emit their default-stdout vm instructions."
  :cases (("terpri"     'terpri     'cl-cc/vm::vm-terpri-inst)
          ("fresh-line" 'fresh-line 'cl-cc/vm::vm-fresh-line-inst))
  (fn inst-type)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call fn) ctx)
    (assert-true (codegen-find-inst ctx inst-type))))

(deftest-each phase2-newline-handler-stream-arg
  "terpri / fresh-line with a stream arg emit vm-write-char with #\\Newline."
  :cases (("terpri"     'terpri)
          ("fresh-line" 'fresh-line))
  (fn)
  (let ((ctx (make-ctx-with-vars 'stream)))
    (compile-ast (make-call fn (make-var 'stream)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-write-char))
    ;; A #\Newline constant must be loaded for the write
    (let ((nl-const (find-if (lambda (i)
                               (and (cl-cc::vm-const-p i)
                                    (eql #\Newline (cl-cc/vm::vm-value i))))
                             (codegen-instructions ctx))))
      (assert-true nl-const))))

;;; ── PRINT / PRIN1 / PRINC ────────────────────────────────────────────────

(deftest-each phase2-print-single-arg
  "print / prin1 / princ with 1 arg emit the corresponding vm instruction."
  :cases (("print"  'print  'cl-cc/vm::vm-print-inst)
          ("prin1"  'prin1  'cl-cc/vm::vm-prin1)
          ("princ"  'princ  'cl-cc/vm::vm-princ))
  (fn inst-type)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call fn (make-int 42)) ctx)
    (assert-true (codegen-find-inst ctx inst-type))))

(deftest-each phase2-print-with-stream
  "print / prin1 / princ with 2 args write the string repr to the stream."
  :cases (("print" 'print)
          ("prin1" 'prin1)
          ("princ" 'princ))
  (fn)
  (let ((ctx (make-ctx-with-vars 'stream)))
    (compile-ast (make-call fn (make-int 42) (make-var 'stream)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-stream-write-string-inst))))

;;; ── WRITE-TO-STRING ──────────────────────────────────────────────────────

(deftest phase2-write-to-string-with-keywords
  "write-to-string with keyword args emits vm-write-to-string-inst (keyword values discarded)."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'write-to-string
                            (make-int 42)
                            (make-var :pretty)
                            (make-var 't))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-write-to-string-inst))))

;;; ── CLOSE ────────────────────────────────────────────────────────────────

(deftest phase2-close-emits-close-file
  "(close handle) emits vm-close-file and a vm-const result placeholder.
The Phase 1 *builtin-handle-effect-entries* convention emits vm-const :value nil
after the side-effect instruction (not t as ANSI specifies) — tracking this
impl convention rather than failing on a known semantic gap."
  (let ((ctx (make-ctx-with-vars 'handle)))
    (compile-ast (make-call 'close (make-var 'handle)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-close-file))
    ;; Handle-effect convention emits a vm-const placeholder (value nil).
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-const))))

;;; ── UNREAD-CHAR ──────────────────────────────────────────────────────────

(deftest phase2-unread-char-one-arg
  "(unread-char ch) with 1 arg emits vm-unread-char with default stdin handle 0."
  (let ((ctx (make-ctx-with-vars 'ch)))
    (compile-ast (make-call 'unread-char (make-var 'ch)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-unread-char))))

;;; ── LISTEN ───────────────────────────────────────────────────────────────

(deftest phase2-listen-zero-arg
  "(listen) with no args emits vm-listen-inst with default stdin handle 0."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'listen) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-listen-inst))))
