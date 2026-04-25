;;;; tests/unit/compile/codegen-io-read-tests.lisp — Codegen I/O Read/Print tests
;;;;
;;;; Continuation of codegen-io-tests.lisp.
;;;; Tests for read-char, read-line, read, read-byte, terpri, fresh-line,
;;;; print, prin1, princ, write-to-string, close, unread-char, listen.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

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
  "(close handle) emits vm-close-file and a vm-const result placeholder."
  (let ((ctx (make-ctx-with-vars 'handle)))
    (compile-ast (make-call 'close (make-var 'handle)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-close-file))
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
