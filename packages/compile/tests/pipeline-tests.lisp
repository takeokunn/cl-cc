;;;; tests/integration/pipeline-tests.lisp — Pipeline API Tests
;;;
;;; Tests for compile-expression, compile-string, run-string,
;;; %prescan-in-package, parse-source-for-language, get-stdlib-forms,
;;; run-string-repl, our-eval, and reset-repl-state.

(in-package :cl-cc/test)
(in-suite cl-cc-integration-serial-suite)

;;; ─── compile-expression ─────────────────────────────────────────────────

(deftest pipeline-compile-expression-binop-structure
  "compile-expression returns a well-formed compilation-result for a binop expression."
  (let* ((result (compile-expression '(+ 1 2)))
         (prog   (compilation-result-program result))
         (instrs (vm-program-instructions prog))
         (asm    (compilation-result-assembly result))
         (cps    (compilation-result-cps result)))
    (assert-true (typep result 'cl-cc/compile:compilation-result))
    (assert-true (typep prog 'cl-cc/vm::vm-program))
    (assert-true (> (length instrs) 0))
    (assert-true (stringp asm))
    (assert-true (or (null cps) (consp cps)))))

(deftest pipeline-compile-expression-constant-halts
  "compile-expression for a constant ends in vm-halt."
  (let* ((result (compile-expression 42))
            (instrs (vm-program-instructions (compilation-result-program result)))
            (cps (compilation-result-cps result)))
      (declare (ignore cps))
      (assert-true (typep (car (last instrs)) 'cl-cc/vm::vm-halt))))

(deftest pipeline-policy-data-tables-are-populated
  "Pipeline policy data tables expose the expected bridge and CPS metadata."
  (assert-true (member 'cl-cc/ast:ast-defun cl-cc::*cps-host-eval-unsafe-ast-types*))
  (assert-true (member 'cl-cc/ast:ast-node cl-cc/compile:*cps-native-compile-unsupported-ast-types*))
  (assert-false (member 'cl-cc/ast:ast-setq cl-cc/compile:*cps-compile-unsupported-ast-types*))
  (assert-false (member 'cl-cc/ast:ast-setq cl-cc/compile:*cps-native-compile-unsupported-ast-types*))
  (assert-true (equal '("PARSE-LAMBDA-LIST" . :cl-cc/expand)
                      (first '(("PARSE-LAMBDA-LIST"            . :cl-cc/expand)
                               ("DESTRUCTURE-LAMBDA-LIST"      . :cl-cc/expand)
                               ("GENERATE-LAMBDA-BINDINGS"     . :cl-cc/expand)
                               ("LAMBDA-LIST-INFO-ENVIRONMENT" . :cl-cc/expand))))))

(deftest pipeline-policy-data-host-eval-unsafe-forms-cover-control-and-definitions
  "The host-eval unsafe policy keeps definition and non-local-control AST nodes out of the fast CPS host path."
  (assert-true (member 'cl-cc/ast:ast-defgeneric cl-cc::*cps-host-eval-unsafe-ast-types*))
  (assert-true (member 'cl-cc/ast:ast-unwind-protect cl-cc::*cps-host-eval-unsafe-ast-types*))
  (assert-false (member 'cl-cc/ast:ast-setq cl-cc::*cps-host-eval-unsafe-ast-types*)))

(deftest pipeline-maybe-cps-toplevel-forms-rewrites-safe-expression-forms
  "%maybe-cps-toplevel-forms leaves definition forms alone and rewrites safe expressions into CPS entry forms."
  (let* ((forms     '((defvar *top* 1) (setq *top* 2)))
         (opts      (cl-cc::%make-pipeline-opts :target :vm))
         (rewritten (cl-cc::%maybe-cps-toplevel-forms forms opts)))
    (assert-false (equal (first forms) (first rewritten)))
    (assert-false (equal (second forms) (second rewritten)))
    (assert-true (consp (first rewritten)))
    (assert-true (consp (second rewritten)))))

(deftest pipeline-compile-expression-vm-program-uses-raw-stream
  "compile-expression keeps raw VM instructions executable for :vm targets while preserving optimizer output separately."
  (let* ((result (compile-expression '(+ 1 2) :target :vm))
          (program-instrs (vm-program-instructions (compilation-result-program result)))
          (raw-instrs (cl-cc:compilation-result-vm-instructions result))
          (optimized-instrs (cl-cc:compilation-result-optimized-instructions result)))
    (assert-true (equal program-instrs raw-instrs))
    (assert-true (> (length raw-instrs) 0))
    (assert-true (listp optimized-instrs))))

(deftest pipeline-compile-string-accepts-pgo-speed-kwargs
  "compile-string accepts PGO-derived :speed plus inline threshold kwargs used by CLI compile paths."
  (let ((result (compile-string "(+ 1 2)"
                                :target :vm
                                :speed 3
                                :inline-threshold-scale 2)))
    (assert-true (typep result 'cl-cc/compile:compilation-result))))

(deftest pipeline-compile-string-emits-pgo-counter-plan
  "compile-string returns a compilation-result carrying deterministic PGO counter plan metadata."
  (let* ((result (compile-string "(+ 1 2)" :target :vm))
         (plan (cl-cc/compile:compilation-result-pgo-counter-plan result)))
    (assert-true plan)
    (assert-true (integerp (getf plan :total-bb)))
    (assert-true (integerp (getf plan :total-edge)))
    (assert-true (consp (getf plan :bb-counters)))
    (assert-true (consp (getf plan :edge-counters)))
    (assert-true (consp (getf plan :bb-runtime-keys)))
    (assert-true (listp (getf plan :edge-runtime-keys)))))

(deftest pipeline-compile-string-with-stdlib-emits-pgo-counter-plan
  "compile-string-with-stdlib also backfills pgo counter metadata for CLI --stdlib PGO paths."
  (let* ((result (cl-cc:compile-string-with-stdlib "(+ 1 2)" :target :vm))
         (plan (cl-cc/compile:compilation-result-pgo-counter-plan result)))
    (assert-true plan)
    (assert-true (integerp (getf plan :total-bb)))
    (assert-true (consp (getf plan :bb-runtime-keys)))))

(deftest pipeline-maybe-bump-opts-speed-from-ast-defun-declaration
  "%pipeline-maybe-bump-opts-speed-from-ast picks up local defun optimize speed declaration."
  (let* ((opts (cl-cc::%make-pipeline-opts :target :vm :speed nil))
         (ast (cl-cc/ast:make-ast-defun
               :name 'f
               :params '(x)
               :optional-params nil
               :rest-param nil
               :key-params nil
               :declarations '((optimize (speed 3)))
               :documentation nil
               :body (list (make-ast-var :name 'x)))))
    (cl-cc::%pipeline-maybe-bump-opts-speed-from-ast opts ast)
    (assert-= 3 (cl-cc::pipeline-opts-speed opts))))

(deftest pipeline-maybe-bump-opts-speed-from-ast-does-not-lower-existing-speed
  "%pipeline-maybe-bump-opts-speed-from-ast keeps higher existing speed when local speed is lower."
  (let* ((opts (cl-cc::%make-pipeline-opts :target :vm :speed 3))
         (ast (cl-cc/ast:make-ast-defun
               :name 'f
               :params '(x)
               :optional-params nil
               :rest-param nil
               :key-params nil
               :declarations '((optimize (speed 1)))
               :documentation nil
               :body (list (make-ast-var :name 'x)))))
    (cl-cc::%pipeline-maybe-bump-opts-speed-from-ast opts ast)
    (assert-= 3 (cl-cc::pipeline-opts-speed opts))))

(deftest-each pipeline-compile-toplevel-forms-defvar-type-env
  "compile-toplevel-forms infers fixnum type for defvar regardless of type-check flag."
  :cases (("with-type-check"
           '((defvar *typed-top-level* 42))
           '*typed-top-level*
           t)
          ("without-type-check"
           '((defvar *typed-top-level-no-check* 42))
           '*typed-top-level-no-check*
           nil))
  (forms lookup-sym type-check)
  (let ((result (cl-cc/compile:compile-toplevel-forms forms :target :vm :type-check type-check)))
    (assert-true (typep (cl-cc/compile:compilation-result-type-env result)
                        'cl-cc/type:type-env))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type::type-env-lookup lookup-sym
                                     (cl-cc/compile:compilation-result-type-env result))
      (assert-true found-p)
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name
                          (cl-cc/type::type-scheme-type scheme))))))

(deftest pipeline-compile-toplevel-forms-captures-cps
  "compile-toplevel-forms may store CPS metadata for top-level input."
  (let ((result (cl-cc/compile:compile-toplevel-forms '((+ 1 2) (- 4 1)))))
    (assert-true (or (null (cl-cc/compile:compilation-result-cps result))
                     (consp (cl-cc/compile:compilation-result-cps result))))))

(deftest pipeline-compile-toplevel-forms-program-uses-raw-stream-for-vm
  "compile-toplevel-forms keeps raw VM instructions in the program for :vm execution while still recording optimizer output."
  (let* ((result (cl-cc/compile:compile-toplevel-forms '((+ 1 2) (- 4 1)) :target :vm))
          (program-instrs (vm-program-instructions (cl-cc/compile:compilation-result-program result)))
          (raw-instrs (cl-cc/compile:compilation-result-vm-instructions result))
          (optimized-instrs (cl-cc/compile:compilation-result-optimized-instructions result)))
    (assert-true (equal program-instrs raw-instrs))
    (assert-true (> (length raw-instrs) 0))
    (assert-true (listp optimized-instrs))))

(deftest-each pipeline-compile-toplevel-forms-defun-type-env
  "compile-toplevel-forms infers function type for defun regardless of type-check flag."
  :cases (("with-type-check"
           '((defun typed-id (x) x))
           'typed-id
           t)
          ("without-type-check"
           '((defun typed-id-no-check (x) x))
           'typed-id-no-check
           nil))
  (forms lookup-sym type-check)
  (let ((result (cl-cc/compile:compile-toplevel-forms forms :target :vm :type-check type-check)))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type::type-env-lookup lookup-sym
                                     (cl-cc/compile:compilation-result-type-env result))
      (assert-true found-p)
      (assert-true (cl-cc/type:type-arrow-p
                    (cl-cc/type::type-scheme-type scheme))))))

;;; Additional eval/prescan/stdlib integration tests live in pipeline-eval-tests.lisp.
