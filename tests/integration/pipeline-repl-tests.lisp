;;;; pipeline-repl-tests.lisp — run-string-repl, reset-repl-state, compile-with-stdlib, whitespace-symbol-p tests
(in-package :cl-cc/test)

;; REPL integration tests intentionally mutate process-global compiler / VM state
;; (`*repl-vm-state*`, label counters, current package, stdlib snapshots). Keep
;; them in a dedicated serial child suite so randomized mixed-mode execution does
;; not reintroduce order-dependent flakes.
(defsuite cl-cc-pipeline-repl-serial-suite
  :description "Serial pipeline REPL integration tests"
  :parent cl-cc-integration-suite
  :parallel nil)

(in-suite cl-cc-pipeline-repl-serial-suite)

;;; ─── run-string-repl (persistent state) ─────────────────────────────────

(deftest pipeline-repl-simple-eval
  "run-string-repl evaluates a simple expression."
  (with-reset-repl-state
    (let ((result (run-string-repl "42")))
      (assert-= 42 result))))

(deftest pipeline-repl-policy-data-tables-are-populated
  "REPL host-load policy tables include expected top-level forms, macros, and registration helpers."
  (assert-true (member "DEFMETHOD" cl-cc::*host-only-top-level-form-names* :test #'string=))
  (assert-true (member "DEFINE-PROLOG-DECLARATIVE-RULES" cl-cc::*host-only-top-level-macro-names* :test #'string=))
  (assert-true (member "REGISTER-TARGET" cl-cc::*host-only-registration-helper-names* :test #'string=))
  (assert-true (member "*SETF-COMPOUND-PLACE-HANDLERS*" cl-cc::*host-only-registration-symbol-names* :test #'string=)))

(deftest-each pipeline-repl-persistence
  "run-string-repl persists defun and defvar definitions across REPL calls."
  :cases (("defun"   "(defun repl-test-double (x) (* x 2))"  "(repl-test-double 21)"  42)
          ("defvar"  "(defvar *repl-test-val* 99)"             "*repl-test-val*"        99))
  (setup check expected)
  (with-reset-repl-state
    (run-string-repl setup)
    (assert-= expected (run-string-repl check))))

(deftest pipeline-run-form-repl-registers-top-level-defmacro-on-host
  "run-form-repl handles top-level defmacro by registering a host expander immediately." 
  (let* ((*package* (find-package :cl-cc/compile))
         (macro-name (intern "PIPELINE-REPL-TEMP-DEFMACRO" *package*))
         (form (first (cl-cc/parse:parse-all-forms
                       "(defmacro pipeline-repl-temp-defmacro (&body body) `(progn ,@body))")))
         (table (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*)))
    (unwind-protect
         (progn
           (assert-eq macro-name (cl-cc::run-form-repl form))
           (let ((expander (gethash macro-name table)))
              (assert-true expander)
              (assert-equal '(progn (print 1))
                           (funcall expander '(pipeline-repl-temp-defmacro (print 1)) nil))))
      (remhash macro-name table))))

(deftest pipeline-run-form-repl-registers-destructuring-defmacro-on-host
  "run-form-repl supports top-level defmacro lambda lists with nested destructuring." 
  (let* ((*package* (find-package :cl-cc/compile))
         (macro-name (intern "PIPELINE-REPL-TEMP-DESTRUCTURING-DEFMACRO" *package*))
         (form (first (cl-cc/parse:parse-all-forms
                       "(defmacro pipeline-repl-temp-destructuring-defmacro (name (parent) &body body) `(list ',name ',parent ',body))")))
         (table (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*)))
    (unwind-protect
         (progn
           (assert-eq macro-name (cl-cc::run-form-repl form))
           (let ((expander (gethash macro-name table)))
             (assert-true expander)
             (assert-equal '(cons 'foo
                                  (cons 'bar
                                        (cons '(baz quux) nil)))
                           (funcall expander '(pipeline-repl-temp-destructuring-defmacro foo (bar) baz quux) nil))))
      (remhash macro-name table))))

(deftest pipeline-run-form-repl-normalizes-register-macro-lambda-body
  "run-form-repl normalizes top-level register-macro lambda bodies before storing the host expander." 
  (let* ((*package* (find-package :cl-cc/compile))
         (macro-name (intern "PIPELINE-REPL-TEMP-REGISTER-MACRO" *package*))
         (form (first (cl-cc/parse:parse-all-forms
                       "(register-macro 'pipeline-repl-temp-register-macro (lambda (form env) (declare (ignore env)) (let ((x (second form))) `(progn ,x))))")))
         (table (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*)))
    (unwind-protect
         (progn
           (assert-eq macro-name (cl-cc::run-form-repl form))
           (let ((expander (gethash macro-name table)))
             (assert-true expander)
              (assert-equal '(progn 42)
                            (funcall expander '(pipeline-repl-temp-register-macro 42) nil))))
      (remhash macro-name table))))

(deftest pipeline-run-string-uses-cps-fast-path-for-safe-single-form
  "run-string uses the CPS fast path for a safe single-form Lisp expression." 
  (let ((hook-called nil)
        (hook-source nil)
        (hook-form nil)
        (hook-value nil))
    (let ((cl-cc::*run-string-cps-fast-path-hook*
            (lambda (source form value)
              (setf hook-called t
                    hook-source source
                    hook-form form
                    hook-value value))))
      (assert-eql 3 (cl-cc:run-string "(+ 1 2)"))
      (assert-true hook-called)
      (assert-equal "(+ 1 2)" hook-source)
      (assert-equal '(+ 1 2) hook-form)
      (assert-eql 3 hook-value))))

(deftest pipeline-run-string-skips-cps-fast-path-for-definition-forms
  "run-string does not use the CPS fast path for definition-like top-level forms." 
  (let ((hook-called nil))
    (let ((cl-cc::*run-string-cps-fast-path-hook*
            (lambda (&rest args)
              (declare (ignore args))
              (setf hook-called t))))
      (assert-true (cl-cc:run-string "(defun pipeline-cps-fast-path-def () 42)"))
      (assert-false hook-called))))

;;; ─── reset-repl-state ──────────────────────────────────────────────────

(deftest pipeline-reset-clears-state
  "reset-repl-state clears all persistent REPL variables."
  (with-reset-repl-state
    (run-string-repl "42")
    (assert-true (not (null cl-cc::*repl-vm-state*)))
    (reset-repl-state)
    (assert-null cl-cc::*repl-vm-state*)
    (assert-null cl-cc::*repl-pool-instructions*)
    (assert-null cl-cc::*repl-pool-labels*)))

;;; ─── compile-string-with-stdlib ─────────────────────────────────────────

(deftest pipeline-compile-with-stdlib
  "compile-string-with-stdlib includes stdlib definitions."
  (let ((result (cl-cc::compile-string-with-stdlib "(+ 1 2)" :target :vm)))
    (assert-true (typep result 'cl-cc/compile::compilation-result))))

;;; ─── run-string with :stdlib ────────────────────────────────────────────

(deftest-each pipeline-run-string-stdlib-forms
  "run-string with :stdlib enables stdlib functions."
  :cases ((mapcar-inc  '(2 3 4)
           "(mapcar (lambda (x) (+ x 1)) '(1 2 3))")
          (reduce-sum  10
           "(reduce (lambda (a b) (+ a b)) '(1 2 3 4) 0 t)"))
  (expected expr)
  (assert-equal expected (run-string expr :stdlib t)))

;;; ─── %whitespace-symbol-p ───────────────────────────────────────────────

(deftest-each pipeline-whitespace-symbol-p
  "%whitespace-symbol-p identifies symbols whose name is all whitespace."
  :cases (("plain-symbol"      nil 'hello)
          ("nil"               nil nil)
          ("keyword"           nil :foo)
          ("number"            nil 42)
          ("empty-string"      nil ""))
  (expected form)
  (assert-equal expected (cl-cc::%whitespace-symbol-p form)))

(deftest pipeline-whitespace-symbol-p-space-sym
  "%whitespace-symbol-p returns T for a symbol named with only spaces."
  ;; Intern a whitespace-only symbol to test the predicate directly.
  (let ((ws-sym (intern " " (find-package :cl-cc))))
    (assert-true (cl-cc::%whitespace-symbol-p ws-sym))))

;;; ─── %ensure-repl-state ─────────────────────────────────────────────────

(deftest pipeline-ensure-repl-state-initializes
  "%ensure-repl-state lazily initializes all REPL state variables."
  (with-reset-repl-state
    ;; All state vars should be nil after reset
    (assert-null cl-cc::*repl-vm-state*)
    (assert-null cl-cc::*repl-pool-instructions*)
    ;; Trigger lazy init
    (cl-cc::%ensure-repl-state)
    ;; All should now be initialized
    (assert-true (not (null cl-cc::*repl-vm-state*)))
    (assert-true (not (null cl-cc::*repl-pool-instructions*)))
    (assert-true (not (null cl-cc::*repl-pool-labels*)))
    (assert-true (not (null cl-cc::*repl-global-vars-persistent*)))))

(deftest pipeline-ensure-repl-state-idempotent
  "%ensure-repl-state is idempotent — second call does not reset existing state."
  (with-reset-repl-state
    (cl-cc::%ensure-repl-state)
    (let ((first-state cl-cc::*repl-vm-state*))
      (cl-cc::%ensure-repl-state)
      (assert-eq first-state cl-cc::*repl-vm-state*))))
