;;;; wasm-features-tests.lisp — Feature flag and opcode validation tests
;;;;
;;;; Tests that all wasm feature flags, opcode constants, and
;;;; WAT helpers from docs/wasm.md are properly defined.

(in-package :cl-user)

(defpackage :wasm-features-tests
  (:use :cl :fiveam)
  (:export :run-wasm-features-tests))

(in-package :wasm-features-tests)

(defsuite wasm-features-suite "Wasm features validation tests.")

;; ── Phase 4 Standard (MVP v1.1) ──
(deftest test-non-trapping-float-to-int-constants (wasm-features-suite)
  (finish)
  ;; FR-233: Verify non-trapping float-to-int opcodes are defined
  (let ((pkg (find-package :cl-cc/codegen)))
    (is (not (null pkg)) "cl-cc/codegen package exists")
    (when pkg
      (is (boundp (find-symbol "+WASM-I32-TRUNC-SAT-F32-S+" pkg))
          "+wasm-i32-trunc-sat-f32-s+ defined")
      (is (boundp (find-symbol "+WASM-I32-TRUNC-SAT-F64-S+" pkg))
          "+wasm-i32-trunc-sat-f64-s+ defined")
      (is (boundp (find-symbol "+WASM-I64-TRUNC-SAT-F64-S+" pkg))
          "+wasm-i64-trunc-sat-f64-s+ defined"))))

;; FR-234: Sign-extension
(deftest test-sign-extension-constants (wasm-features-suite)
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (is (boundp (find-symbol "+WASM-I32-EXTEND8-S+" pkg)))
      (is (boundp (find-symbol "+WASM-I32-EXTEND16-S+" pkg)))
      (is (boundp (find-symbol "+WASM-I64-EXTEND8-S+" pkg)))
      (is (boundp (find-symbol "+WASM-I64-EXTEND16-S+" pkg)))
      (is (boundp (find-symbol "+WASM-I64-EXTEND32-S+" pkg))))))

;; FR-228: Bulk Memory
(deftest test-bulk-memory-constants (wasm-features-suite)
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (is (boundp (find-symbol "+WASM-MEMORY-COPY+" pkg)))
      (is (boundp (find-symbol "+WASM-MEMORY-FILL+" pkg)))
      (is (boundp (find-symbol "+WASM-MEMORY-INIT+" pkg)))
      (is (boundp (find-symbol "+WASM-DATA-DROP+" pkg))))))

;; FR-237: Bulk Table
(deftest test-bulk-table-constants (wasm-features-suite)
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (is (boundp (find-symbol "+WASM-TABLE-INIT+" pkg)))
      (is (boundp (find-symbol "+WASM-TABLE-COPY+" pkg)))
      (is (boundp (find-symbol "+WASM-TABLE-FILL+" pkg)))
      (is (boundp (find-symbol "+WASM-ELEM-DROP+" pkg))))))

;; FR-143: Tail-call
(deftest test-tail-call-constants (wasm-features-suite)
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (is (boundp (find-symbol "+WASM-RETURN-CALL+" pkg)))
      (is (boundp (find-symbol "+WASM-RETURN-CALL-INDIRECT+" pkg))))))

;; FR-213: Memory64
(deftest test-memory64-constants (wasm-features-suite)
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (is (boundp (find-symbol "+WASM-MEMORY-SIZE64+" pkg)))
      (is (boundp (find-symbol "+WASM-MEMORY-GROW64+" pkg))))))

;; FR-324: copysign
(deftest test-copysign-constants (wasm-features-suite)
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (is (boundp (find-symbol "+WASM-F64-COPYSIGN+" pkg)))
      (is (boundp (find-symbol "+WASM-F32-COPYSIGN+" pkg))))))

;; ── GC Proposal ──
(deftest test-gc-constants (wasm-features-suite)
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (is (boundp (find-symbol "+WASM-GC-STRUCT-NEW+" pkg)))
      (is (boundp (find-symbol "+WASM-GC-STRUCT-NEW-DEFAULT+" pkg)))
      (is (boundp (find-symbol "+WASM-GC-STRUCT-GET+" pkg)))
      (is (boundp (find-symbol "+WASM-GC-STRUCT-SET+" pkg)))
      (is (boundp (find-symbol "+WASM-GC-ARRAY-NEW+" pkg)))
      (is (boundp (find-symbol "+WASM-GC-ARRAY-NEW-FIXED+" pkg)))
      (is (boundp (find-symbol "+WASM-GC-ARRAY-GET+" pkg)))
      (is (boundp (find-symbol "+WASM-GC-ARRAY-SET+" pkg)))
      (is (boundp (find-symbol "+WASM-GC-ARRAY-LEN+" pkg)))
      (is (boundp (find-symbol "+WASM-GC-REF-TEST+" pkg)))
      (is (boundp (find-symbol "+WASM-GC-REF-CAST+" pkg)))
      (is (boundp (find-symbol "+WASM-GC-REF-I31+" pkg)))
      (is (boundp (find-symbol "+WASM-GC-I31-GET-S+" pkg)))
      (is (boundp (find-symbol "+WASM-GC-I31-GET-U+" pkg)))
      (is (boundp (find-symbol "+WASM-GC-BR-ON-CAST+" pkg)))
      (is (boundp (find-symbol "+WASM-GC-BR-ON-CAST-FAIL+" pkg))))))

;; ── Exception Handling ──
(deftest test-eh-constants (wasm-features-suite)
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (is (boundp (find-symbol "+WASM-TRY+" pkg)))
      (is (boundp (find-symbol "+WASM-CATCH+" pkg)))
      (is (boundp (find-symbol "+WASM-THROW+" pkg)))
      (is (boundp (find-symbol "+WASM-TRY-TABLE+" pkg)) "EH v2 try_table")  ; FR-252
      (is (boundp (find-symbol "+WASM-THROW-REF+" pkg)) "EH v2 throw_ref"))))  ; FR-252

;; ── Feature flags ──
(deftest test-feature-flags-exist (wasm-features-suite)
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (dolist (flag-sym '(*WASM-NON-TRAPPING-FLOAT-TO-INT-ENABLED*
                          *WASM-SIGN-EXTENSION-ENABLED*
                          *WASM-BULK-MEMORY-ENABLED*
                          *WASM-BULK-TABLE-ENABLED*
                          *WASM-SIMD128-ENABLED*
                          *WASM-TAIL-CALL-ENABLED*
                          *WASM-THREADS-ENABLED*
                          *WASM-EXCEPTION-HANDLING-ENABLED*
                          *WASM-MEMORY64-ENABLED*
                          *WASM-GC-ENABLED*
                          *WASM-I31REF-OPTIMIZE-ENABLED*
                          *WASM-GC-STRUCT-TYPES-ENABLED*
                          *WASM-GC-ARRAY-TYPES-ENABLED*
                          *WASM-REF-CAST-ELIMINATION-ENABLED*
                          *WASM-INTEGER-RANGE-ANNOTATION-ENABLED*
                          *WASM-TYPED-CLOSURE-ENV-ENABLED*
                          *WASM-AOT-MODE-ENABLED*
                          *WASM-PGO-ENABLED*
                          *WASM-DEAD-IMPORT-ELIMINATION-ENABLED*
                          *WASM-SOURCE-MAP-ENABLED*
                          *WASM-DWARF-DEBUG-INFO-ENABLED*
                          *WASM-JS-PROMISE-INTEGRATION-ENABLED*
                          *WASM-STRING-BUILTINS-ENABLED*
                          *WASM-DYNAMIC-LINKING-ENABLED*
                          *WASM-STACK-SWITCHING-ENABLED*))
        (is (boundp (find-symbol (symbol-name flag-sym) pkg))
            (format nil "~A defined" flag-sym))))))

;; ── WAT helpers ──
(deftest test-wat-helpers-exist (wasm-features-suite)
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (dolist (fn-sym '("WASM-FIXNUM-UNBOX" "WASM-FIXNUM-BOX" "WASM-BOOL-TO-I31"
                         "REG-LOCAL-REF" "REG-LOCAL-SET" "REG-RECORD-TYPE"
                         "REG-KNOWN-TYPE" "REG-CLEAR-TYPE" "WASM-REF-CAST-MAYBE"
                         "WASM-CLOSURE-REF-WAT" "EMIT-WASM-CLOSURE-ALLOCATION"
                         "WASM-MEMORY-COPY-WAT" "WASM-MEMORY-FILL-WAT"
                         "WASM-COPYSIGN-WAT" "WASM-TRUNC-SAT-F64-I64-WAT"
                         "WASM-SIGN-EXTEND-32-8-WAT" "WASM-I64-CLZ-WAT"
                         "WASM-MEMORY-GROW-CHECKED-WAT"))
        (is (fboundp (find-symbol fn-sym pkg))
            (format nil "~A function defined" fn-sym))))))

;; ── Dispatch tables ──
(deftest test-dispatch-tables-exist (wasm-features-suite)
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (dolist (table-sym '("*WASM-I64-BINOP-TABLE*" "*WASM-I64-CMP-TABLE*"
                            "*WASM-UNARY-FIXNUM-TABLE*" "*WASM-MINMAX-TABLE*"
                            "*WASM-STRUCT-GET-TABLE*" "*WASM-SIGN-EXTEND-TABLE*"
                            "*WASM-FLOAT-TO-INT-TABLE*"))
        (is (boundp (find-symbol table-sym pkg))
            (format nil "~A dispatch table defined" table-sym))))))

(run! :wasm-features-suite)
(print 'all-wasm-features-tests-executed)
(finish-output)
