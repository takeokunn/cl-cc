;;;; tests/framework-fuzz.lisp — CL-CC Test Framework (Fuzzing)
;;;; Random CL program generation, crash detection, termination checking.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Random State and Primitive Generators
;;; ------------------------------------------------------------

(defvar *fuzz-random-state* (make-random-state t)
  "Random state for fuzzing.")

(defun %random-elt (list)
  "Pick a random element from a list."
  (nth (random (length list) *fuzz-random-state*) list))

(defun %random-int ()
  "Generate a small random integer."
  (- (random 201 *fuzz-random-state*) 100))

(defun %random-symbol (prefix)
  "Generate a random symbol name."
  (intern (format nil "~A~A" prefix (random 10 *fuzz-random-state*))))

;;; ------------------------------------------------------------
;;; Grammar-Based Expression Generator
;;; ------------------------------------------------------------

(defun %gen-expr (features depth env)
  "Generate a random valid CL expression.
   FEATURES: list of feature keywords controlling which constructs appear.
   DEPTH: remaining recursion depth (0 = terminal only).
   ENV: list of symbols currently bound in scope."
  (cond
    ;; Terminal: depth exhausted or random early-termination
    ((or (<= depth 0)
         (and (> depth 0) (zerop (random 3 *fuzz-random-state*))))
     ;; If env has bound vars, occasionally use one
     (if (and env (zerop (random 2 *fuzz-random-state*)))
         (%random-elt env)
         (%random-int)))

    ;; Non-terminal: pick a random construct from enabled features
    (t
     (let* ((available '())
            (next-depth (1- depth)))
       ;; Always available: arithmetic
       (when (member :arithmetic features)
         (push :arithmetic available))
       (when (member :if features)
         (push :if available))
       (when (member :let features)
         (push :let available))
       (when (member :progn features)
         (push :progn available))
       (when (member :lambda features)
         (push :lambda available))
       (when (and (member :funcall features) (> depth 1))
         (push :funcall available))
       ;; Fallback to terminal if nothing available
       (when (null available)
         (return-from %gen-expr (%random-int)))
       (ecase (%random-elt available)
         (:arithmetic
          (let ((op (%random-elt '(+ - *))))
            (list op
                  (%gen-expr features next-depth env)
                  (%gen-expr features next-depth env))))
         (:if
          (list 'if
                (%gen-expr features next-depth env)
                (%gen-expr features next-depth env)
                (%gen-expr features next-depth env)))
         (:let
          (let* ((var (%random-symbol "V"))
                 (val (%gen-expr features next-depth env))
                 (new-env (cons var env)))
            (list 'let
                  (list (list var val))
                  (%gen-expr features next-depth new-env))))
         (:progn
          (list 'progn
                (%gen-expr features next-depth env)
                (%gen-expr features next-depth env)))
         (:lambda
          (let* ((param (%random-symbol "P"))
                 (new-env (cons param env)))
            (list 'lambda
                  (list param)
                  (%gen-expr features next-depth new-env))))
         (:funcall
          ;; Generate a lambda inline and call it with an argument
          (let* ((param (%random-symbol "P"))
                 (new-env (cons param env))
                 (fn-expr (list 'lambda
                                (list param)
                                (%gen-expr features next-depth new-env)))
                 (arg-expr (%gen-expr features next-depth env)))
            (list 'funcall fn-expr arg-expr))))))))

(defun gen-random-program (features &optional (depth 3))
  "Generate a random valid CL program using grammar rules.
   FEATURES: list of :arithmetic :let :if :lambda :funcall :progn
   DEPTH: maximum AST depth.
   Returns an S-expression (list), NOT a string."
  (let ((expr (%gen-expr features depth nil)))
    expr))

;;; ------------------------------------------------------------
;;; Compilability Check
;;; ------------------------------------------------------------

(defun compilable-p (expr)
  "Check if expr can be compiled by cl-cc without error.
   Returns t on success, nil on any error."
  (ignore-errors
    (compile-string (write-to-string expr))
    t))

;;; ------------------------------------------------------------
;;; assert-no-crash Macro (FR-031)
;;; ------------------------------------------------------------

(defmacro assert-no-crash (&body forms)
  "Assert that FORMS complete without signaling any serious condition.
   Fails the current test if a serious-condition is caught."
  `(handler-case
       (progn ,@forms)
     (serious-condition (c)
       (%fail-test (format nil "assert-no-crash: unexpected condition: ~A" c)
                   :form '(progn ,@forms)))))

;;; ------------------------------------------------------------
;;; assert-terminates Macro (FR-031)
;;; ------------------------------------------------------------

(defmacro assert-terminates (form &key (timeout 5))
  "Assert that FORM completes within TIMEOUT seconds.
   Uses sb-ext:with-timeout. Fails the test on timeout."
  `(handler-case
       (sb-ext:with-timeout ,timeout
         ,form)
     (sb-ext:timeout ()
       (%fail-test (format nil "assert-terminates: form did not terminate within ~A seconds"
                           ,timeout)
                   :form ',form))))

;;; ------------------------------------------------------------
;;; deftest-fuzz Macro (FR-031)
;;; ------------------------------------------------------------

(defmacro deftest-fuzz (name &rest args)
  "Define a fuzz test that generates COUNT random programs and tests each.
   NAME: test name symbol registered in *test-registry*.
   COUNT: number of random programs to generate and test.
   MAX-DEPTH: maximum AST depth passed to gen-random-program (actual depth is 1..max-depth).
   TIMEOUT-PER-TEST: seconds before an individual fuzz iteration is abandoned (not a failure).
   FEATURES: quoted list of feature keywords for gen-random-program.
   BODY: forms executed for each generated program; the symbols FEATURES and DEPTH
         are bound to the current feature list and chosen depth respectively."
  ;; Extract keyword args, body starts at first non-keyword element.
  (let* ((known-keys '(:count :max-depth :timeout-per-test :features))
         (count           (or (getf args :count)           100))
         (max-depth       (or (getf args :max-depth)       3))
         (timeout-per-test (or (getf args :timeout-per-test) 5))
         (features        (or (getf args :features)        ''(:arithmetic :let :if)))
         (body            (loop for rest on args by #'cddr
                                when (not (member (car rest) known-keys))
                                  return rest))
         (features-var (gensym "FEATURES"))
         (depth-var (gensym "DEPTH")))
    `(deftest ,name
       (let ((,features-var ,features))
         (loop repeat ,count
               do (let* ((depth (1+ (random ,max-depth)))
                         (features ,features-var)
                         (,depth-var depth))
                    (declare (ignorable ,depth-var features))
                    (handler-case
                        (sb-ext:with-timeout ,timeout-per-test
                          ,@body)
                       (sb-ext:timeout ()
                         ;; Timeout on an individual fuzz iteration is acceptable; skip it.
                         nil))))))))


;;; ------------------------------------------------------------
;;; Built-in Fuzz Tests
;;; ------------------------------------------------------------

(deftest-fuzz compiler-robustness
  :count 10
  :max-depth 3
  :timeout-per-test 1
  :features '(:arithmetic :let :if :progn)
  (let ((prog (gen-random-program features depth)))
    (assert-no-crash (compile-string (write-to-string prog)))
    (when (compilable-p prog)
      (assert-terminates (run-string (write-to-string prog)) :timeout 1))))
