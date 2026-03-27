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

(deftest phase2-make-hash-table-no-test
  "(make-hash-table) emits vm-make-hash-table"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-hash-table) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-hash-table))))

(deftest phase2-make-hash-table-quoted-test
  "(make-hash-table :test 'equal) extracts the test symbol"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-hash-table
                            (make-var :test)
                            (make-quoted 'equal))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-hash-table)))
      (assert-true inst)
      ;; test-reg should be non-nil — a register was allocated for the test sym
      (assert-true (cl-cc::vm-make-hash-table-test inst)))))

(deftest phase2-make-hash-table-var-test
  "(make-hash-table :test equal) where equal is an ast-var extracts test symbol"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-hash-table
                            (make-var :test)
                            (make-var 'equal))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-hash-table))))

(deftest phase2-make-hash-table-function-test
  "(make-hash-table :test #'equal) via ast-function extracts test symbol"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-hash-table
                            (make-var :test)
                            (make-fn 'equal))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-hash-table))))

;;; ── GETHASH ───────────────────────────────────────────────────────────────

(deftest phase2-gethash-two-args
  "(gethash key table) emits vm-gethash with nil default"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'gethash (make-quoted 'k) (make-quoted 'ht)) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-gethash)))
      (assert-true inst)
      (assert-true (null (cl-cc::vm-gethash-default inst))))))

(deftest phase2-gethash-three-args
  "(gethash key table default) emits vm-gethash with default register"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'gethash
                            (make-quoted 'k) (make-quoted 'ht) (make-int 0))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-gethash)))
      (assert-true inst)
      (assert-true (cl-cc::vm-gethash-default inst)))))

;;; ── MAPHASH ───────────────────────────────────────────────────────────────

(deftest phase2-maphash-emits-hash-table-keys
  "(maphash fn ht) generates an iteration loop beginning with hash-table-keys"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'maphash (make-quoted 'fn) (make-quoted 'ht)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-hash-table-keys))))

(deftest phase2-maphash-emits-call-to-fn
  "(maphash fn ht) emits a vm-call for the function application"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'maphash (make-quoted 'fn) (make-quoted 'ht)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-call))))

(deftest phase2-maphash-returns-nil
  "(maphash fn ht) result register holds nil (maphash is void)"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'maphash (make-quoted 'fn) (make-quoted 'ht)) ctx)
    ;; Last emitted const before halt should be nil (the return value)
    (let ((consts (remove-if-not (lambda (i) (typep i 'cl-cc::vm-const))
                                  (codegen-instructions ctx))))
      (assert-true (some (lambda (i) (null (cl-cc::vm-const-value i))) consts)))))

;;; ── MAKE-ARRAY ────────────────────────────────────────────────────────────

(deftest phase2-make-array-emits-vm-make-array
  "(make-array n) emits vm-make-array"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-array (make-int 10)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-array))))

(deftest phase2-make-array-not-adjustable
  "(make-array n) produces a fixed array (fill-pointer nil, adjustable nil)"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-array (make-int 10)) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-array)))
      (assert-true (null (cl-cc::vm-make-array-fill-pointer inst)))
      (assert-true (null (cl-cc::vm-make-array-adjustable inst))))))

;;; ── MAKE-ADJUSTABLE-VECTOR ────────────────────────────────────────────────

(deftest phase2-make-adjustable-vector-emits-vm-make-array
  "(make-adjustable-vector n) emits vm-make-array"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-adjustable-vector (make-int 10)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-array))))

(deftest phase2-make-adjustable-vector-is-adjustable
  "(make-adjustable-vector n) sets fill-pointer and adjustable"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-adjustable-vector (make-int 10)) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-array)))
      (assert-true (cl-cc::vm-make-array-fill-pointer inst))
      (assert-true (cl-cc::vm-make-array-adjustable inst)))))

;;; ── ARRAY-ROW-MAJOR-INDEX ────────────────────────────────────────────────

(deftest phase2-array-row-major-index-one-subscript
  "(array-row-major-index arr i) emits vm-array-row-major-index"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'array-row-major-index (make-int 10) (make-int 0)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-array-row-major-index))))

(deftest phase2-array-row-major-index-two-subscripts
  "(array-row-major-index arr i j) also emits vm-array-row-major-index"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'array-row-major-index
                            (make-int 10) (make-int 0) (make-int 1))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-array-row-major-index))))

(deftest phase2-array-row-major-index-builds-subscript-cons-chain
  "(array-row-major-index arr i j) builds a cons-chain for the subscripts"
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

(deftest phase2-make-string-basic
  "(make-string n) emits vm-make-string"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-string (make-int 5)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-string))))

(deftest phase2-make-string-no-char-by-default
  "(make-string n) without :initial-element leaves char slot nil"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-string (make-int 5)) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-make-string)))
      (assert-true (null (cl-cc::vm-make-string-char inst))))))

(deftest phase2-make-string-with-initial-element
  "(make-string n :initial-element ch) sets the char register"
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

(deftest phase2-typep-quoted-type
  "(typep x 'integer) with quoted type emits vm-typep"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'typep (make-int 42) (make-quoted 'integer)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-typep))))

(deftest phase2-typep-quoted-type-symbol
  "(typep x 'string) stores the type symbol in the instruction"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'typep (make-int 42) (make-quoted 'string)) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-typep)))
      (assert-eq 'string (cl-cc::vm-typep-type-name inst)))))

(deftest phase2-typep-unquoted-falls-through
  "(typep x integer) without quote does NOT emit vm-typep (falls through)"
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

(deftest phase2-format-nil-dest-emits-format-inst
  "(format nil fmt) emits vm-format-inst"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "~A")
                            (make-int 42))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-format-inst))))

(deftest phase2-format-nil-dest-no-princ
  "(format nil fmt) does NOT emit vm-princ — result stays in register"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "~A")
                            (make-int 1))
                 ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-princ)))))

(deftest phase2-format-t-dest-emits-format-and-princ
  "(format t fmt) emits both vm-format-inst and vm-princ"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 't)
                            (make-quoted "~A")
                            (make-int 1))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-format-inst))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-princ))))

(deftest phase2-format-stream-dest-emits-stream-write
  "(format stream fmt) emits vm-format-inst and vm-stream-write-string-inst"
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

(deftest phase2-open-emits-vm-open-file
  "(open path) emits vm-open-file"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'open (make-quoted "/tmp/f")) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-open-file))))

(deftest phase2-open-defaults-to-input
  "(open path) with no :direction defaults to :input"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'open (make-quoted "/tmp/f")) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-open-file)))
      (assert-eq :input (cl-cc::vm-open-file-direction inst)))))

(deftest phase2-open-with-direction-output
  "(open path :direction :output) sets direction to :output"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'open
                            (make-quoted "/tmp/f")
                            (make-var :direction)
                            (make-var :output))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-open-file)))
      (assert-eq :output (cl-cc::vm-open-file-direction inst)))))

;;; ── PEEK-CHAR ────────────────────────────────────────────────────────────

(deftest phase2-peek-char-one-arg
  "(peek-char handle) with 1 arg uses that arg as handle"
  (let ((ctx (make-ctx-with-vars 'handle)))
    (compile-ast (make-call 'peek-char (make-var 'handle)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-peek-char))))

(deftest phase2-peek-char-two-args
  "(peek-char nil handle) with 2 args uses second arg as handle"
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

(deftest phase2-concatenate-string-type-emits-vm-concatenate
  "(concatenate 'string a b) emits vm-concatenate"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'concatenate
                            (make-quoted 'string)
                            (make-quoted "hello")
                            (make-quoted " world"))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-concatenate))))

(deftest phase2-concatenate-non-string-falls-through
  "(concatenate 'list a b) does NOT emit vm-concatenate (guard: 'string only)"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'concatenate
                            (make-quoted 'list)
                            (make-quoted "a")
                            (make-quoted "b"))
                 ctx)
    (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-concatenate)))))

(deftest phase2-concatenate-requires-quoted-type
  "(concatenate string a b) with unquoted type falls through"
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
