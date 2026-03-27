;;;; tests/unit/compile/phase2-handler-tests.lisp
;;;;
;;;; Unit tests for the *phase2-builtin-handlers* dispatch table in codegen.lisp.
;;;;
;;;; Each of the 17 registered Phase 2 handlers gets dedicated coverage:
;;;;   - the correct VM instruction is emitted
;;;;   - structural properties are verified (optional args, keyword parsing)
;;;;   - guard conditions that trigger fallthrough return nil
;;;;
;;;; Helpers: reuses make-codegen-ctx / codegen-instructions / codegen-find-inst
;;;; from codegen-tests.lisp (same suite, loaded before this file).

(in-package :cl-cc/test)

(defsuite phase2-handler-suite
  :description "Phase 2 builtin handler dispatch (AST-introspecting builtins)"
  :parent cl-cc-suite)

;;; ── Local helpers ─────────────────────────────────────────────────────────

(defun codegen-count-inst (ctx type)
  "Count instructions of TYPE emitted into CTX."
  (count-if (lambda (i) (typep i type)) (codegen-instructions ctx)))

(defun make-call (func &rest arg-forms)
  "Build an ast-call node. FUNC is a symbol; ARG-FORMS are already-built AST nodes."
  (make-ast-call :func func :args arg-forms))

(defun make-int (n)    (make-ast-int :value n))
(defun make-var (s)    (make-ast-var :name s))
(defun make-quoted (v) (make-ast-quote :value v))
(defun make-fn (name)  (make-ast-function :name name))

(defun make-ctx-with-vars (&rest names)
  "Create a codegen ctx with NAMES pre-bound to fresh registers.
Each name maps to its own unique register so compile-ast on ast-var
nodes referring to these names succeeds without signaling 'unbound variable'."
  (let ((ctx (make-codegen-ctx)))
    (dolist (name names)
      (let ((reg (cl-cc::make-register ctx)))
        (push (cons name reg) (cl-cc::ctx-env ctx))))
    ctx))

;;; ── MAKE-HASH-TABLE ───────────────────────────────────────────────────────

(deftest-each phase2-make-hash-table-variants
  "make-hash-table emits vm-make-hash-table across all :test forms"
  ((no-test
    (let ((ctx (make-codegen-ctx)))
      (compile-ast (make-call 'make-hash-table) ctx)
      (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-hash-table))))
   (quoted-test
    (let ((ctx (make-codegen-ctx)))
      (compile-ast (make-call 'make-hash-table
                              (make-var :test)
                              (make-quoted 'equal))
                   ctx)
      (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-hash-table)))
        (assert-true inst)
        ;; test-reg should be non-nil — a register was allocated for the test sym
        (assert-true (cl-cc::vm-make-hash-table-test inst)))))
   (var-test
    (let ((ctx (make-codegen-ctx)))
      (compile-ast (make-call 'make-hash-table
                              (make-var :test)
                              (make-var 'equal))
                   ctx)
      (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-hash-table))))
   (function-test
    (let ((ctx (make-codegen-ctx)))
      (compile-ast (make-call 'make-hash-table
                              (make-var :test)
                              (make-fn 'equal))
                   ctx)
      (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-hash-table))))))

;;; ── GETHASH ───────────────────────────────────────────────────────────────

(deftest phase2-gethash-arities
  "gethash emits vm-gethash with correct default slot for 2 and 3 args"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'gethash (make-quoted 'k) (make-quoted 'ht)) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-gethash)))
      (assert-true inst)
      (assert-true (null (cl-cc::vm-gethash-default inst)))))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'gethash
                            (make-quoted 'k) (make-quoted 'ht) (make-int 0))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-gethash)))
      (assert-true inst)
      (assert-true (cl-cc::vm-gethash-default inst)))))

;;; ── MAPHASH ───────────────────────────────────────────────────────────────

(deftest phase2-maphash-codegen
  "(maphash fn ht) emits hash-table-keys, a vm-call, and returns nil"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'maphash (make-quoted 'fn) (make-quoted 'ht)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-hash-table-keys))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-call))
    ;; Last emitted const before halt should be nil (the return value)
    (let ((consts (remove-if-not (lambda (i) (typep i 'cl-cc::vm-const))
                                  (codegen-instructions ctx))))
      (assert-true (some (lambda (i) (null (cl-cc::vm-const-value i))) consts)))))

;;; ── MAKE-ARRAY / MAKE-ADJUSTABLE-VECTOR ──────────────────────────────────

(deftest-each phase2-make-array-variants
  "make-array and make-adjustable-vector emit vm-make-array with correct flags"
  ((fixed-array-emits-inst
    (let ((ctx (make-codegen-ctx)))
      (compile-ast (make-call 'make-array (make-int 10)) ctx)
      (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-array))))
   (fixed-array-not-adjustable
    (let ((ctx (make-codegen-ctx)))
      (compile-ast (make-call 'make-array (make-int 10)) ctx)
      (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-array)))
        (assert-true (null (cl-cc::vm-make-array-fill-pointer inst)))
        (assert-true (null (cl-cc::vm-make-array-adjustable inst))))))
   (adjustable-vector-emits-inst
    (let ((ctx (make-codegen-ctx)))
      (compile-ast (make-call 'make-adjustable-vector (make-int 10)) ctx)
      (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-array))))
   (adjustable-vector-is-adjustable
    (let ((ctx (make-codegen-ctx)))
      (compile-ast (make-call 'make-adjustable-vector (make-int 10)) ctx)
      (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-array)))
        (assert-true (cl-cc::vm-make-array-fill-pointer inst))
        (assert-true (cl-cc::vm-make-array-adjustable inst)))))))

;;; ── ARRAY-ROW-MAJOR-INDEX ────────────────────────────────────────────────

(deftest phase2-array-row-major-index
  "array-row-major-index emits the instruction and builds a cons chain for subscripts"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'array-row-major-index (make-int 10) (make-int 0)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-array-row-major-index)))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'array-row-major-index
                            (make-int 10) (make-int 0) (make-int 1))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-array-row-major-index)))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'array-row-major-index
                            (make-int 10) (make-int 2) (make-int 3))
                 ctx)
    ;; Subscripts are accumulated via vm-cons cells
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-cons))))

;;; ── ENCODE-UNIVERSAL-TIME ────────────────────────────────────────────────

(deftest phase2-encode-universal-time-six-args
  "(encode-universal-time s m h d mo y) emits vm-encode-universal-time"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'encode-universal-time
                            (make-int 0) (make-int 0) (make-int 12)
                            (make-int 1) (make-int 1) (make-int 2024))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-encode-universal-time))))

(deftest phase2-encode-universal-time-wrong-arity-falls-through
  "encode-universal-time with wrong arity (< 6 args) returns nil (falls through)"
  (let ((ctx (make-codegen-ctx)))
    ;; Only 3 args — handler guard fails, falls through to normal call
    (compile-ast (make-call 'encode-universal-time
                            (make-int 0) (make-int 0) (make-int 12))
                 ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-encode-universal-time)))))

;;; ── MAKE-STRING ──────────────────────────────────────────────────────────

(deftest phase2-make-string-variants
  "make-string emits vm-make-string with correct char slot"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-string (make-int 5)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-string)))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-string (make-int 5)) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-string)))
      (assert-true (null (cl-cc::vm-make-string-char inst)))))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-string
                            (make-int 5)
                            (make-var :initial-element)
                            (make-quoted #\x))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-string)))
      (assert-true inst)
      (assert-true (cl-cc::vm-make-string-char inst)))))

;;; ── TYPEP ─────────────────────────────────────────────────────────────────

(deftest phase2-typep-variants
  "typep emits vm-typep for quoted types and falls through for unquoted"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'typep (make-int 42) (make-quoted 'integer)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-typep)))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'typep (make-int 42) (make-quoted 'string)) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-typep)))
      (assert-eq 'string (cl-cc::vm-typep-type-name inst))))
  (let ((ctx (make-ctx-with-vars 'integer)))
    ;; Pass unquoted ast-var — handler guard fails
    (compile-ast (make-call 'typep (make-int 42) (make-var 'integer)) ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-typep)))))

;;; ── SLOT-BOUNDP ──────────────────────────────────────────────────────────

(deftest phase2-slot-boundp-emits-instruction
  "(slot-boundp obj 'slot) emits vm-slot-boundp"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'slot-boundp (make-int 0) (make-quoted 'name)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-slot-boundp))))

(deftest phase2-slot-boundp-stores-slot-name
  "(slot-boundp obj 'foo) stores the slot symbol in the instruction"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'slot-boundp (make-int 0) (make-quoted 'foo)) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-slot-boundp)))
      (assert-eq 'foo (cl-cc::vm-slot-name-sym inst)))))

;;; ── SLOT-EXISTS-P ────────────────────────────────────────────────────────

(deftest phase2-slot-exists-p-emits-instruction
  "(slot-exists-p obj 'slot) emits vm-slot-exists-p"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'slot-exists-p (make-int 0) (make-quoted 'name)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-slot-exists-p))))

;;; ── SLOT-MAKUNBOUND ──────────────────────────────────────────────────────

(deftest phase2-slot-makunbound-emits-instruction
  "(slot-makunbound obj 'slot) emits vm-slot-makunbound"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'slot-makunbound (make-int 0) (make-quoted 'name)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-slot-makunbound))))

;;; ── CALL-NEXT-METHOD ─────────────────────────────────────────────────────

(deftest phase2-call-next-method-no-args
  "(call-next-method) with no args emits vm-call-next-method with nil args-reg"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'call-next-method) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-call-next-method)))
      (assert-true inst)
      (assert-true (null (cl-cc::vm-call-next-method-args-reg inst))))))

(deftest phase2-call-next-method-with-args
  "(call-next-method x) with args emits vm-call-next-method with args-reg set"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'call-next-method (make-int 42)) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-call-next-method)))
      (assert-true inst)
      (assert-true (cl-cc::vm-call-next-method-args-reg inst)))))

(deftest phase2-call-next-method-args-is-cons-list
  "(call-next-method x y) builds cons list for args"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'call-next-method (make-int 1) (make-int 2)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-cons))))

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
                            (make-var 'string)   ; NOT ast-quote
                            (make-quoted "a")
                            (make-quoted "b"))
                 ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-concatenate)))))

;;; ── Handler table completeness ────────────────────────────────────────────

(deftest phase2-all-17-handlers-registered
  "All 17 Phase 2 handlers are in *phase2-builtin-handlers*"
  (let ((expected '("MAKE-HASH-TABLE" "GETHASH" "MAPHASH"
                    "MAKE-ARRAY" "MAKE-ADJUSTABLE-VECTOR"
                    "ARRAY-ROW-MAJOR-INDEX" "ENCODE-UNIVERSAL-TIME"
                    "MAKE-STRING" "TYPEP"
                    "SLOT-BOUNDP" "SLOT-EXISTS-P" "SLOT-MAKUNBOUND"
                    "CALL-NEXT-METHOD" "WRITE-STRING"
                    "FORMAT" "OPEN" "PEEK-CHAR"
                    "MAKE-STRING-INPUT-STREAM" "CONCATENATE")))
    (dolist (name expected)
      (assert-true (gethash name cl-cc::*phase2-builtin-handlers*)))))
