;;;; wasm-features-tests.lisp — Feature flag and opcode validation tests
;;;;
;;;; Tests that all wasm feature flags, opcode constants, and
;;;; WAT helpers from docs/wasm.md are properly defined.
;;;; Uses cl-cc/testing framework. assert-true takes exactly 1 argument.

(in-package :cl-cc/test)

(defsuite wasm-features-suite :description "Wasm features validation tests."
  :parent cl-cc-unit-suite)

(in-suite wasm-features-suite)

;; ── Phase 4 Standard (MVP v1.1) ──
(deftest wasm-test-non-trapping-float-to-int-constants
  "FR-233: Verify non-trapping float-to-int opcodes are defined."
  (let ((pkg (find-package :cl-cc/codegen)))
    (assert-true (not (null pkg)))
    (when pkg
      (assert-true (boundp (find-symbol "+WASM-I32-TRUNC-SAT-F32-S+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-I32-TRUNC-SAT-F64-S+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-I64-TRUNC-SAT-F64-S+" pkg))))))

;; FR-234: Sign-extension
(deftest wasm-test-sign-extension-constants
  "FR-234: Verify sign-extension opcodes are defined."
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (assert-true (boundp (find-symbol "+WASM-I32-EXTEND8-S+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-I32-EXTEND16-S+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-I64-EXTEND8-S+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-I64-EXTEND16-S+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-I64-EXTEND32-S+" pkg))))))

;; FR-228: Bulk Memory
(deftest wasm-test-bulk-memory-constants
  "FR-228: Verify bulk memory opcodes are defined."
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (assert-true (boundp (find-symbol "+WASM-MEMORY-COPY+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-MEMORY-FILL+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-MEMORY-INIT+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-DATA-DROP+" pkg))))))

;; FR-237: Bulk Table
(deftest wasm-test-bulk-table-constants
  "FR-237: Verify bulk table opcodes are defined."
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (assert-true (boundp (find-symbol "+WASM-TABLE-INIT+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-TABLE-COPY+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-TABLE-FILL+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-ELEM-DROP+" pkg))))))

;; FR-143: Tail-call
(deftest wasm-test-tail-call-constants
  "FR-143: Verify tail-call opcodes are defined."
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (assert-true (boundp (find-symbol "+WASM-RETURN-CALL+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-RETURN-CALL-INDIRECT+" pkg))))))

;; FR-213: Memory64
(deftest wasm-test-memory64-constants
  "FR-213: Verify memory64 opcodes are defined."
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (assert-true (boundp (find-symbol "+WASM-MEMORY-SIZE64+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-MEMORY-GROW64+" pkg))))))

;; FR-324: copysign
(deftest wasm-test-copysign-constants
  "FR-324: Verify copysign opcodes are defined."
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (assert-true (boundp (find-symbol "+WASM-F64-COPYSIGN+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-F32-COPYSIGN+" pkg))))))

;; ── GC Proposal ──
(deftest wasm-test-gc-constants
  "FR-209,211: Verify GC struct/array opcodes are defined."
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (assert-true (boundp (find-symbol "+WASM-GC-STRUCT-NEW+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-GC-STRUCT-GET+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-GC-STRUCT-SET+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-GC-ARRAY-NEW+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-GC-ARRAY-NEW-FIXED+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-GC-ARRAY-GET+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-GC-ARRAY-SET+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-GC-ARRAY-LEN+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-GC-REF-TEST+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-GC-REF-CAST+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-GC-REF-I31+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-GC-I31-GET-S+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-GC-BR-ON-CAST+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-GC-BR-ON-CAST-FAIL+" pkg))))))

;; ── Exception Handling ──
(deftest wasm-test-eh-constants
  "FR-204,252: Verify EH opcodes are defined."
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (assert-true (boundp (find-symbol "+WASM-TRY+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-CATCH+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-THROW+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-TRY-TABLE+" pkg)))
      (assert-true (boundp (find-symbol "+WASM-THROW-REF+" pkg))))))

;; ── FR coverage check ──
(deftest wasm-test-doc-fr-count
  "Verify docs/wasm.md has exactly 130 FR headings."
  (let* ((repo-root (asdf:system-relative-pathname :cl-cc ""))
         (wasm-md (merge-pathnames "docs/wasm.md" repo-root)))
    (when (probe-file wasm-md)
      (with-open-file (f wasm-md :direction :input)
        (let ((count 0))
          (loop for line = (read-line f nil nil)
                while line
                when (and (>= (length line) 7)
                          (string= (subseq line 0 7) "#### FR-"))
                  do (incf count))
          (assert-equal 130 count))))))

;; ── Feature flags existence ──
(deftest wasm-test-key-feature-flags-exist
  "Verify key feature flags are bound in cl-cc/codegen."
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (dolist (flag-sym '("NON-TRAPPING-FLOAT-TO-INT-ENABLED"
                          "SIGN-EXTENSION-ENABLED" "BULK-MEMORY-ENABLED"
                          "TAIL-CALL-ENABLED" "GC-ENABLED" "SIMD128-ENABLED"
                          "EXCEPTION-HANDLING-ENABLED" "AOT-MODE-ENABLED"))
        (let ((full-sym (find-symbol (format nil "*WASM-~A*" flag-sym) pkg)))
          (assert-true (and full-sym (boundp full-sym))))))))

;; ── WAT helpers ──
(deftest wasm-test-wat-helpers-exist
  "Verify key WAT helper functions are fboundp."
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (dolist (fn-name '("WASM-FIXNUM-UNBOX" "WASM-FIXNUM-BOX"
                          "REG-LOCAL-REF" "REG-LOCAL-SET"
                          "REG-RECORD-TYPE" "REG-KNOWN-TYPE"
                          "WASM-REF-CAST-MAYBE" "WASM-CLOSURE-REF-WAT"))
        (let ((fn-sym (find-symbol fn-name pkg)))
          (assert-true (and fn-sym (fboundp fn-sym))))))))

;; ── Dispatch tables ──
(deftest wasm-test-dispatch-tables-exist
  "Verify dispatch tables are bound."
  (let ((pkg (find-package :cl-cc/codegen)))
    (when pkg
      (dolist (table-name '("*WASM-I64-BINOP-TABLE*" "*WASM-I64-CMP-TABLE*"
                            "*WASM-UNARY-FIXNUM-TABLE*" "*WASM-MINMAX-TABLE*"
                            "*WASM-STRUCT-GET-TABLE*" "*WASM-SIGN-EXTEND-TABLE*"
                            "*WASM-FLOAT-TO-INT-TABLE*"))
        (let ((table-sym (find-symbol table-name pkg)))
          (assert-true (and table-sym (boundp table-sym))))))))

;; ── All 130 FRs covered in wasm-features.lisp ──
(deftest wasm-test-all-doc-frs-in-features
  "Verify every FR in docs/wasm.md has a reference in wasm-features.lisp."
  (let* ((repo-root (asdf:system-relative-pathname :cl-cc ""))
         (wasm-md (merge-pathnames "docs/wasm.md" repo-root))
         (features-file (merge-pathnames "packages/codegen/src/wasm-features.lisp" repo-root)))
    (when (and (probe-file wasm-md) (probe-file features-file))
      (let ((fr-ids nil))
        (with-open-file (f wasm-md :direction :input)
          (setf fr-ids
                (loop for line = (read-line f nil nil)
                      while line
                      when (and (>= (length line) 7)
                                (string= (subseq line 0 7) "#### FR-"))
                        collect (let* ((rest (subseq line 7))
                                       (space-pos (position #\Space rest)))
                                  (if space-pos
                                      (parse-integer rest :end space-pos :junk-allowed t)
                                      0)))))
        (let ((missing nil))
          (with-open-file (ff features-file :direction :input)
            (let ((ff-content (make-string (file-length ff))))
              (read-sequence ff-content ff)
              (dolist (fr-id fr-ids)
                (let ((fr-str (format nil "FR-~D" fr-id)))
                  (unless (search fr-str ff-content :test #'char-equal)
                    (push fr-str missing))))))
          (assert-true (null missing)))))))
