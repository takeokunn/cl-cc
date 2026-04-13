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
  :description "Phase 2 builtin handler dispatch (AST-introspecting builtins)
(in-suite phase2-handler-suite)
"
  :parent cl-cc-suite)

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

;;; ── CALL-NEXT-METHOD ─────────────────────────────────────────────────────

;;; ── I/O AND STRING HANDLERS MOVED TO codegen-io-tests ───────────────────────

;;; ── Handler table completeness ────────────────────────────────────────────

(deftest phase2-all-handlers-registered
  "All Phase 2 handlers are in *phase2-builtin-handlers*"
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
