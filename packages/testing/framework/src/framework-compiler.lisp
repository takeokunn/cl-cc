;;;; tests/framework-compiler.lisp — CL-CC Test Framework (Compiler-Specific)
;;;; DSL helpers, differential testing, pattern matching, performance, cross-backend.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Section 1: Compiler DSL Helpers (FR-019)
;;; ------------------------------------------------------------

(defun %get-instructions (compilation-result)
  "Extract the instruction list from a compilation-result."
  (vm-program-instructions (compilation-result-program compilation-result)))

(defmacro assert-compiles-to (expr &key contains)
  "Assert that compiling EXPR produces an instruction of type CONTAINS.
   CONTAINS should be a quoted type symbol like 'vm-add."
  (let ((result (gensym "RESULT"))
        (instrs (gensym "INSTRS"))
        (found  (gensym "FOUND")))
    `(let* ((,result (ignore-errors (compile-string ,expr)))
            (,instrs (when ,result (%get-instructions ,result)))
            (,found  (and ,instrs
                          (find-if (lambda (i)
                                     (typep i (find-symbol (symbol-name ,contains) :cl-cc)))
                                    ,instrs))))
       (unless ,found
          (%fail-test (format nil "assert-compiles-to: ~S does not contain instruction of type ~S"
                              ,expr ,contains)
                      :expected ,contains
                      :actual   (and ,instrs (mapcar #'type-of ,instrs))
                      :form     (list 'assert-compiles-to ,expr :contains ,contains)))
       t)))

(defmacro assert-evaluates-to (expr expected &key stdlib)
  "Assert that running EXPR via run-string returns a value EQUAL to EXPECTED.
   :STDLIB keyword is accepted but ignored (reserved for future use)."
  (declare (ignore stdlib))
  (let ((exp (gensym "EXP"))
        (act (gensym "ACT")))
    `(let ((,exp ,expected)
           (,act (ignore-errors (run-string ,expr))))
       (unless (equal ,act ,exp)
          (%fail-test (format nil "assert-evaluates-to: ~S evaluated to ~S, expected ~S"
                              ,expr ,act ,exp)
                      :expected ,exp
                      :actual   ,act
                      :form     (list 'assert-evaluates-to ,expr ,expected)))
       t)))

(defmacro assert-macro-expands-to (form expected)
  "Assert that (our-macroexpand FORM) is EQUAL to EXPECTED."
  (let ((exp (gensym "EXP"))
        (act (gensym "ACT")))
    `(let ((,exp ,expected)
           (,act (ignore-errors (our-macroexpand ,form))))
       (unless (equal ,act ,exp)
          (%fail-test (format nil "assert-macro-expands-to: ~S expanded to ~S, expected ~S"
                              ,form ,act ,exp)
                      :expected ,exp
                      :actual   ,act
                      :form     (list 'assert-macro-expands-to ,form ,expected)))
       t)))

(defmacro assert-infers-type (expr expected-type)
  "Assert that compiling EXPR with run-string-typed produces a type
   whose type-primitive-name (or type string) equals EXPECTED-TYPE."
  (let ((result (gensym "RESULT"))
        (inferred (gensym "INFERRED")))
    `(let* ((,result   (ignore-errors (run-string-typed ,expr)))
            (,inferred (when ,result (compilation-result-type ,result))))
       (unless (and ,inferred
                    (ignore-errors
                      (or (equal ,inferred ',expected-type)
                          (and (typep ,inferred 'cl-cc/type::type-primitive)
                               (equal (cl-cc/type::type-primitive-name ,inferred)
                                      ',expected-type)))))
          (%fail-test (format nil "assert-infers-type: ~S inferred ~S, expected ~S"
                              ,expr ,inferred ',expected-type)
                      :expected ',expected-type
                      :actual   ,inferred
                      :form     (list 'assert-infers-type ,expr ',expected-type)))
       t)))


;;; ── Run-and-compare macros (used by optimizer-tests.lisp and wasm-tests.lisp) ────

(defmacro assert-run= (expected expr-string)
  "Compile and run EXPR-STRING in the CL-CC VM, assert result equals EXPECTED."
  (let ((result (gensym "RESULT")))
    `(let ((,result (ignore-errors (run-string ,expr-string))))
       (unless (equal ,result ,expected)
         (%fail-test
          (format nil "assert-run=: expected ~S, got ~S for ~S"
                  ,expected ,result ,expr-string)
          :expected ,expected
          :actual   ,result
          :form     (list 'assert-run= ,expected ,expr-string))))))

(defmacro assert-run-equal (expected expr-string)
  "Alias for assert-run=."
  `(assert-run= ,expected ,expr-string))

(defmacro assert-run-true (expr-string)
  "Compile and run EXPR-STRING, assert result is non-nil."
  (let ((result (gensym "RESULT")))
    `(let ((,result (ignore-errors (run-string ,expr-string))))
       (unless ,result
         (%fail-test
          (format nil "assert-run-true: expected non-nil, got ~S for ~S"
                  ,result ,expr-string)
          :expected t
          :actual   ,result
          :form     (list 'assert-run-true ,expr-string))))))

(defmacro assert-run-false (expr-string)
  "Compile and run EXPR-STRING, assert result is nil."
  (let ((result (gensym "RESULT")))
    `(let ((,result (ignore-errors (run-string ,expr-string))))
       (when ,result
         (%fail-test
          (format nil "assert-run-false: expected nil, got ~S for ~S"
                  ,result ,expr-string)
          :expected nil
          :actual   ,result
          :form     (list 'assert-run-false ,expr-string))))))

(defmacro assert-run-signals (condition-type expr-string)
  "Compile and run EXPR-STRING, assert it signals CONDITION-TYPE."
  `(handler-case
       (progn (run-string ,expr-string)
              (%fail-test
               (format nil "assert-run-signals: expected ~S but no condition for ~S"
                       ',condition-type ,expr-string)
               :form (list 'assert-run-signals ',condition-type ,expr-string)))
     (,condition-type () t)))

(defmacro assert-run-string= (expected expr-string)
  "Compile and run EXPR-STRING, assert result string= EXPECTED."
  (let ((result (gensym "RESULT")))
    `(let ((,result (ignore-errors (run-string ,expr-string))))
       (unless (and (stringp ,result) (string= ,result ,expected))
         (%fail-test
          (format nil "assert-run-string=: expected ~S, got ~S for ~S"
                  ,expected ,result ,expr-string)
          :expected ,expected
          :actual   ,result
          :form     (list 'assert-run-string= ,expected ,expr-string))))))

(defun assert-output-contains (output substring)
  "Assert that OUTPUT string contains SUBSTRING. Used by wasm-tests.lisp."
  (unless (and (stringp output)
               (search substring output))
    (%fail-test
     (format nil "assert-output-contains: ~S not found in output (~D chars)"
             substring (if (stringp output) (length output) 0))
     :expected substring
     :actual   output
     :form     (list 'assert-output-contains output substring))))

;;; ------------------------------------------------------------
;;; Section N: Prolog Database Isolation (opt-in library)
;;; ------------------------------------------------------------
;;;
;;; The compiler's Prolog fact DB (`cl-cc/prolog::*prolog-rules*`) is populated at
;;; startup with baseline rules (type-of facts, peephole rules, etc.). Some
;;; tests mutate it as part of their exercise and the state can leak into
;;; subsequent tests under randomized ordering (see PROLOG-TYPE-OF-CMP flake
;;; mentioned in MEMORY.md).
;;;
;;; We expose snapshot / restore as explicit helpers plus WITH-PROLOG-DB-ISOLATED
;;; so individual tests can opt in. We deliberately DO NOT install this as a
;;; blanket defafter :each fixture: the copy-list + clrhash pass runs in O(facts)
;;; and, applied to every test, dominates the per-test budget for the compile-
;;; heavy suites and blows through the default timeout.

(defmacro with-prolog-db-isolated (&body body)
  "Run BODY with the Prolog fact DB restored to its pre-BODY state afterwards.
Use for tests that add rules and must not leak into siblings — cheaper than
snapshotting at load time because most tests don't need this."
  (let ((snap (gensym "SNAP")))
    `(let ((,snap (%snapshot-prolog-db :copy-value #'copy-list)))
       (unwind-protect (progn ,@body)
         (%restore-prolog-db ,snap :copy-value #'copy-list)))))


;;; ------------------------------------------------------------
;;; Section N+1: VM Hash-Cons Cache Isolation
;;; ------------------------------------------------------------
;;;
;;; vm-hash-cons memoizes cons cells by (car . cdr) value keys. When test A
;;; mutates a cached cell via rplaca/rplacd, test B that hash-conses the same
;;; pre-mutation key sees the mutated value. This violates test isolation and
;;; produces puzzling failures like "expected 10, got 99" where the 99 came
;;; from a prior rplaca test's mutation on a structurally-equal pair.
;;;
;;; Fix: clear the cache before every test. The table is small (<100 entries
;;; typical), so clrhash overhead is well under 1 µs per test — negligible
;;; compared to the prolog restore we removed in iter 2.

(defbefore :each (cl-cc-suite)
  (when (fboundp 'cl-cc/vm::vm-clear-hash-cons-table)
    (cl-cc/vm::vm-clear-hash-cons-table)))

;;; ------------------------------------------------------------
;;; Section N+2: Macroexpansion Cache Isolation
;;; ------------------------------------------------------------
;;;
;;; *macroexpansion-cache* memoizes macro expansions keyed on (env, form).
;;; Under random test ordering, test A's expansion of (setf (cadr x) newval)
;;; can land a cached entry whose expansion references shared gensym vars,
;;; then test B running in an environment that has different macro bindings
;;; triggers infinite expansion looking up the same cache entry. This
;;; manifests as the 4 expander-test timeouts (EXPANDER-SETF-CXR-COMPOUND-
;;; PLACES [cadr], EXPANDER-LAMBDA-EXPANDS-TYPED-PARAMS,
;;; EXPAND-MAKE-ARRAY-ADJUSTABLE-PROMOTES, EXPAND-MAKE-ARRAY-FORM-FILL-
;;; POINTER) that pass in isolation but hang in the full-run context.
;;;
;;; Fix: clear the cache before every test. The cache is :weakness :key
;;; (weak on env), so clearing it only drops value entries — the envs stay
;;; alive through their own references. clrhash is O(entries), which is
;;; typically < 100 µs.

(defbefore :each (cl-cc-suite)
  (when (boundp 'cl-cc/expand::*macroexpand-step-cache*)
    (clrhash cl-cc/expand::*macroexpand-step-cache*))
  (when (boundp 'cl-cc/expand::*macroexpand-all-cache*)
    (clrhash cl-cc/expand::*macroexpand-all-cache*)))
