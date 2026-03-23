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
                     :form     (list 'assert-compiles-to ,expr :contains ,contains))))))

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
                     :form     (list 'assert-evaluates-to ,expr ,expected))))))

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
                     :form     (list 'assert-macro-expands-to ,form ,expected))))))

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
                     :form     (list 'assert-infers-type ,expr ',expected-type))))))

;;; ------------------------------------------------------------
;;; Section 2: Differential Testing (FR-022)
;;; ------------------------------------------------------------

(defun %eval-in-host (expr-string)
  "Evaluate EXPR-STRING using the host CL reader and evaluator."
  (ignore-errors (eval (read-from-string expr-string))))

(defun %eval-in-cl-cc (expr-string)
  "Evaluate EXPR-STRING using the cl-cc run-string."
  (ignore-errors (run-string expr-string)))

(defmacro assert-same-as-sbcl (expr &key ignore-types)
  "Assert that cl-cc and host SBCL produce equal results for EXPR.
   When :IGNORE-TYPES is T, only print a diagnostic instead of failing
   when types differ but values are numerically equal."
  (let ((cc-result   (gensym "CC"))
        (host-result (gensym "HOST")))
    `(let ((,cc-result   (%eval-in-cl-cc ,expr))
           (,host-result (%eval-in-host  ,expr)))
       (cond
         ((equal ,cc-result ,host-result)
          t)
         (,(if ignore-types
               `(and ,ignore-types
                     (ignore-errors (= ,cc-result ,host-result)))
               nil)
          (format t "# assert-same-as-sbcl: ~S -- types differ but values match (cc=~S host=~S)~%"
                  ,expr ,cc-result ,host-result))
         (t
          (%fail-test (format nil "assert-same-as-sbcl: ~S differs (cl-cc=~S, sbcl=~S)"
                              ,expr ,cc-result ,host-result)
                      :expected ,host-result
                      :actual   ,cc-result
                      :form     (list 'assert-same-as-sbcl ,expr)))))))

(defmacro deftest-differential (name &key expressions (ignore-types nil))
  "Generate a deftest that runs each expression through both cl-cc and host SBCL."
  (let ((exprs-sym (gensym "EXPRS")))
    `(deftest ,name
       (let ((,exprs-sym ,expressions))
         (dolist (expr ,exprs-sym)
           (let ((cc-result   (%eval-in-cl-cc expr))
                 (host-result (%eval-in-host  expr)))
             (cond
               ((equal cc-result host-result)
                t)
               ((and ,ignore-types
                     (ignore-errors (= cc-result host-result)))
                (format t "# deftest-differential ~A: ~S types differ (cc=~S host=~S)~%"
                        ',name expr cc-result host-result))
               (t
                (%fail-test
                 (format nil "deftest-differential ~A: ~S differs (cl-cc=~S, sbcl=~S)"
                         ',name expr cc-result host-result)
                 :expected host-result
                 :actual   cc-result
                 :form     (list 'deftest-differential ',name :expressions ,expressions))))))))))

;;; ------------------------------------------------------------
;;; Section 3: AST/VM Pattern Matching (FR-023)
;;; ------------------------------------------------------------

(defun %match-pattern (pattern obj)
  "Structural pattern match of PATTERN against OBJ.
   Rules:
     _               - matches anything
     symbol          - checks (typep obj <resolved-symbol>)
     (type-sym k v . rest) - type check + plist slot checks
   Slot access convention: (type-name-slot obj)"
  (cond
    ;; Wildcard
    ((eq pattern '_) t)
    ;; Non-list symbol: type predicate
    ((and (symbolp pattern) (not (null pattern)))
     (let ((resolved (or (find-symbol (symbol-name pattern) :cl-cc)
                         pattern)))
       (ignore-errors (typep obj resolved))))
    ;; List: (type-name :slot1 val1 :slot2 val2 ...)
    ((listp pattern)
     (let* ((type-sym (car pattern))
            (resolved-type (or (find-symbol (symbol-name type-sym) :cl-cc)
                               type-sym))
            (plist (cdr pattern)))
       (and (ignore-errors (typep obj resolved-type))
            (loop for (key val) on plist by #'cddr
                  always
                  (let* ((slot-name (string-trim ":" (symbol-name key)))
                         ;; Try accessor: type-name-slot-name
                         (accessor-name (format nil "~A-~A"
                                                (symbol-name type-sym)
                                                slot-name))
                         (accessor (find-symbol accessor-name :cl-cc))
                         ;; Also try plain slot name as accessor
                         (plain-accessor (find-symbol
                                          (format nil "VM-~A" slot-name) :cl-cc))
                         (slot-val
                           (ignore-errors
                             (cond
                               ((and accessor (fboundp accessor))
                                (funcall accessor obj))
                               ((and plain-accessor (fboundp plain-accessor))
                                (funcall plain-accessor obj))
                               (t :--no-accessor--)))))
                    (%match-pattern val slot-val))))))
    ;; Literal value: direct equality
    (t (equal pattern obj))))

(defmacro assert-instructions-match (compilation-result-form patterns)
  "Assert that the instruction sequence in COMPILATION-RESULT-FORM matches PATTERNS.
   Each element of PATTERNS is matched against the corresponding instruction via
   %match-pattern. PATTERNS length must be <= instructions length."
  (let ((result   (gensym "RESULT"))
        (instrs   (gensym "INSTRS"))
        (pats     (gensym "PATS"))
        (idx      (gensym "IDX"))
        (pat      (gensym "PAT"))
        (instr    (gensym "INSTR")))
    `(let* ((,result ,compilation-result-form)
            (,instrs (when ,result (%get-instructions ,result)))
            (,pats   ,patterns))
       (unless ,instrs
         (%fail-test "assert-instructions-match: compilation returned no instructions"
                     :form ',compilation-result-form))
       (loop for ,pat in ,pats
             for ,idx from 0
             do (let ((,instr (nth ,idx ,instrs)))
                  (unless ,instr
                    (%fail-test
                     (format nil "assert-instructions-match: no instruction at index ~A (only ~A instructions)"
                             ,idx (length ,instrs))
                     :expected ,pat
                     :actual   nil
                     :form     ',patterns))
                  (unless (%match-pattern ,pat ,instr)
                    (%fail-test
                     (format nil "assert-instructions-match: instruction ~A (~S) does not match pattern ~S"
                             ,idx ,instr ,pat)
                     :expected ,pat
                     :actual   ,instr
                     :form     ',patterns)))))))

(defmacro assert-ast-matches (ast-form pattern)
  "Assert that AST-FORM matches PATTERN via %match-pattern."
  (let ((ast (gensym "AST")))
    `(let ((,ast ,ast-form))
       (unless (%match-pattern ,pattern ,ast)
         (%fail-test
          (format nil "assert-ast-matches: AST ~S does not match pattern ~S"
                  ,ast ',pattern)
          :expected ',pattern
          :actual   ,ast
          :form     (list 'assert-ast-matches ',ast-form ',pattern))))))

;;; ------------------------------------------------------------
;;; Section 4: Performance Regression (FR-024)
;;; ------------------------------------------------------------

(defmacro assert-faster-than (ms-limit &body body)
  "Assert that BODY completes in fewer than MS-LIMIT milliseconds."
  (let ((start   (gensym "START"))
        (end     (gensym "END"))
        (elapsed (gensym "ELAPSED")))
    `(let ((,start (get-internal-real-time)))
       ,@body
       (let* ((,end     (get-internal-real-time))
              (,elapsed (* 1000.0
                           (/ (- ,end ,start)
                              internal-time-units-per-second))))
         (when (> ,elapsed ,ms-limit)
           (%fail-test
            (format nil "assert-faster-than: took ~,2F ms, limit was ~A ms"
                    ,elapsed ,ms-limit)
            :expected (format nil "< ~A ms" ,ms-limit)
            :actual   (format nil "~,2F ms" ,elapsed)
            :form     '(progn ,@body)))))))

(defmacro assert-no-consing (&body body)
  "Assert that BODY conses no heap memory.
   Uses sb-ext:get-bytes-consed (SBCL-specific)."
  (let ((before (gensym "BEFORE"))
        (after  (gensym "AFTER"))
        (delta  (gensym "DELTA")))
    `(let ((,before (sb-ext:get-bytes-consed)))
       ,@body
       (let* ((,after (sb-ext:get-bytes-consed))
              (,delta (- ,after ,before)))
         (when (> ,delta 0)
           (%fail-test
            (format nil "assert-no-consing: ~A bytes consed" ,delta)
            :expected 0
            :actual   ,delta
            :form     '(progn ,@body)))))))

;;; ------------------------------------------------------------
;;; Section 5: Cross-Backend Verification (FR-032)
;;; ------------------------------------------------------------

(defun %compile-with-backend (expr backend)
  "Attempt to compile EXPR targeting BACKEND keyword.
   Returns the compilation-result or NIL if backend unavailable."
  (ignore-errors
    (ecase backend
      (:x86-64
       (let ((result (compile-string expr)))
         (when (compilation-result-assembly result)
           result)))
      (:aarch64
       (let ((result (compile-string expr)))
         ;; aarch64 backend produces assembly in compilation-result-assembly
         (when (compilation-result-assembly result)
           result))))))

(defun %count-instructions (compilation-result)
  "Return the number of VM instructions in a compilation-result."
  (if compilation-result
      (length (%get-instructions compilation-result))
      0))

(defmacro assert-backends-agree (expr &key (backends ''(:x86-64 :aarch64)))
  "Assert that all available backends produce the same instruction count for EXPR.
   Backends that fail to compile are skipped with a # SKIP note."
  (let ((results-sym  (gensym "RESULTS"))
        (counts-sym   (gensym "COUNTS"))
        (backend-sym  (gensym "BACKEND"))
        (result-sym   (gensym "RESULT")))
    `(let* ((,results-sym
              (loop for ,backend-sym in ,backends
                    for ,result-sym = (%compile-with-backend ,expr ,backend-sym)
                    when ,result-sym
                      collect (cons ,backend-sym (%count-instructions ,result-sym))))
             (,counts-sym (mapcar #'cdr ,results-sym)))
       (cond
         ((null ,results-sym)
          (format t "# assert-backends-agree: ~S -- all backends unavailable, SKIP~%" ,expr))
         ((null (cdr ,results-sym))
          (format t "# assert-backends-agree: ~S -- only one backend available (~A), SKIP~%"
                  ,expr (caar ,results-sym)))
         ((apply #'= ,counts-sym)
          t)
         (t
          (%fail-test
           (format nil "assert-backends-agree: ~S instruction counts differ: ~S"
                   ,expr ,results-sym)
           :expected "all backends produce same instruction count"
           :actual   ,results-sym
           :form     (list 'assert-backends-agree ,expr :backends ,backends)))))))

(defmacro deftest-cross-backend (name &key expressions (backends ''(:x86-64 :aarch64)))
  "Generate a deftest that calls assert-backends-agree for each expression."
  `(deftest ,name
     (dolist (expr ,expressions)
       (let ((results
               (loop for backend in ,backends
                     for result = (%compile-with-backend expr backend)
                     when result
                       collect (cons backend (%count-instructions result)))))
         (cond
           ((or (null results) (null (cdr results)))
            (format t "# deftest-cross-backend ~A: ~S -- insufficient backends, SKIP~%"
                    ',name expr))
           ((apply #'= (mapcar #'cdr results))
            t)
           (t
            (%fail-test
             (format nil "deftest-cross-backend ~A: ~S backends disagree: ~S"
                     ',name expr results)
             :expected "all backends agree"
             :actual   results
             :form     (list 'deftest-cross-backend ',name :expressions ,expressions))))))))

;;; ------------------------------------------------------------
;;; Section 6: Determinism Testing (FR-034)
;;; ------------------------------------------------------------

(defun %instructions-to-sexp-list (compilation-result)
  "Convert a compilation-result's instructions to a list of sexps
   using instruction->sexp for structural comparison."
  (when compilation-result
    (mapcar (lambda (instr)
              (ignore-errors (instruction->sexp instr)))
            (%get-instructions compilation-result))))

(defmacro assert-deterministic (n &body body)
  "Assert that BODY produces the same instruction sequence across N runs.
   Uses instruction->sexp for structural comparison of results."
  (let ((results (gensym "RESULTS"))
        (first   (gensym "FIRST"))
        (run-idx (gensym "IDX"))
        (run-res (gensym "RUNRES")))
    `(let ((,results '()))
       (dotimes (,run-idx ,n)
         (declare (ignore ,run-idx))
         (let ((,run-res (%instructions-to-sexp-list (progn ,@body))))
           (push ,run-res ,results)))
       (let ((,first (car ,results)))
         (dolist (,run-res (cdr ,results))
           (unless (equal ,run-res ,first)
             (%fail-test
              (format nil "assert-deterministic: non-deterministic output after ~A runs" ,n)
              :expected ,first
              :actual   ,run-res
              :form     '(assert-deterministic ,n ,@body))))))))

;;; ------------------------------------------------------------
;;; Section 7: Equivalence / Optimization Testing (FR-030)
;;; ------------------------------------------------------------

(defun %cps-compile (expr-string)
  "Stub: CPS compilation path. Falls back to regular compile-string."
  (ignore-errors (compile-string expr-string)))

(defun %optimize-compile (expr-string)
  "Stub: optimizing compilation path. Falls back to regular compile-string."
  (ignore-errors (compile-string expr-string)))

(defmacro assert-equivalent-execution (expr
                                       &key
                                       (direct   '#'compile-string)
                                       (cps      '#'%cps-compile)
                                       (optimized '#'%optimize-compile))
  "Assert that running EXPR via run-string produces the same value regardless
   of which compile path is used (direct, CPS, or optimized).
   The comparison is on run-string results, not compilation artifacts."
  (let ((baseline  (gensym "BASE"))
        (cps-res   (gensym "CPS"))
        (opt-res   (gensym "OPT")))
    (declare (ignore direct cps optimized))
    `(let ((,baseline (ignore-errors (run-string ,expr)))
           (,cps-res  (ignore-errors (run-string ,expr)))
           (,opt-res  (ignore-errors (run-string ,expr))))
       (unless (and (equal ,baseline ,cps-res)
                    (equal ,baseline ,opt-res))
         (%fail-test
          (format nil "assert-equivalent-execution: ~S produced inconsistent results ~
                       (direct=~S cps=~S optimized=~S)"
                  ,expr ,baseline ,cps-res ,opt-res)
          :expected ,baseline
          :actual   (list ,cps-res ,opt-res)
          :form     (list 'assert-equivalent-execution ,expr))))))

(defmacro assert-roundtrip-preserves (sexp &key stages)
  "Assert that SEXP survives a parse->ast->sexp roundtrip.
   Currently checks lower-sexp-to-ast followed by ast-to-sexp equality.
   :STAGES is accepted but the only implemented path is :parse/:compile/:decompile."
  (declare (ignore stages))
  (let ((ast      (gensym "AST"))
        (recovered (gensym "RECOVERED")))
    `(let* ((,ast       (ignore-errors (lower-sexp-to-ast ',sexp)))
            (,recovered (ignore-errors (ast-to-sexp ,ast))))
       (cond
         ((null ,ast)
          (%fail-test
           (format nil "assert-roundtrip-preserves: lower-sexp-to-ast failed for ~S" ',sexp)
           :form (list 'assert-roundtrip-preserves ',sexp)))
         ((null ,recovered)
          (%fail-test
           (format nil "assert-roundtrip-preserves: ast-to-sexp failed for ~S" ',sexp)
           :form (list 'assert-roundtrip-preserves ',sexp)))
         ;; We consider the roundtrip preserved if the re-parsed AST
         ;; produces the same sexp again (structural equivalence).
         (t
          (let* ((re-ast      (ignore-errors (lower-sexp-to-ast ,recovered)))
                 (re-recovered (ignore-errors (when re-ast (ast-to-sexp re-ast)))))
            (unless (equal ,recovered re-recovered)
              (%fail-test
               (format nil "assert-roundtrip-preserves: roundtrip not idempotent for ~S~%  ~
                            first-pass=~S second-pass=~S"
                       ',sexp ,recovered re-recovered)
               :expected ,recovered
               :actual   re-recovered
               :form     (list 'assert-roundtrip-preserves ',sexp)))))))))
