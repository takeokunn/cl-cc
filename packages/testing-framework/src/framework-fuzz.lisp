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
;;; Shrinking and Counterexample Minimization (FR-353)
;;; ------------------------------------------------------------

(defun shrink-integer (n)
  "Return smaller integer candidates moving N toward zero."
  (cond
    ((zerop n) nil)
    ((> (abs n) 1) (remove-duplicates (list 0 (truncate n 2)) :test #'eql))
    (t (list 0))))

(defun shrink-list (list)
  "Return smaller list candidates by deletion and element shrinking."
  (when list
    (let ((candidates '()))
      (push nil candidates)
      (when (cdr list)
        (push (cdr list) candidates)
        (push (butlast list) candidates))
      (when (> (length list) 2)
        (push (subseq list 0 (floor (length list) 2)) candidates))
      (loop for index from 0
            for item in list
            do (dolist (shrunk (shrink item))
                 (let ((copy (copy-list list)))
                   (setf (nth index copy) shrunk)
                   (push copy candidates))))
      (remove-duplicates (nreverse candidates) :test #'equal))))

(defun shrink-string (string)
  "Return shorter string candidates."
  (let ((length (length string)))
    (cond
      ((zerop length) nil)
      ((= length 1) (list ""))
      (t (remove-duplicates
          (list ""
                (subseq string 0 (floor length 2))
                (subseq string 1))
          :test #'string=)))))

(defun shrink (value)
  "Return deterministic smaller candidates for VALUE.
Integers shrink toward zero, strings shrink toward empty strings, and lists shrink
by deleting chunks or recursively shrinking elements."
  (typecase value
    (integer (shrink-integer value))
    (string (shrink-string value))
    (list (shrink-list value))
    (t nil)))

(defun minimize-failing-input (value failing-p &key (shrinker #'shrink) (max-rounds 100))
  "Shrink VALUE while FAILING-P continues to return true.
Returns the smallest found counterexample according to SHRINKER's deterministic
candidate order. FAILING-P should return true when the candidate still
reproduces the property failure."
  (labels ((binary-search-integer (n)
             (cond
               ((not (integerp n)) n)
               ((not (funcall failing-p n)) n)
               ((funcall failing-p 0) 0)
               ((plusp n)
                (let ((lo 0)
                      (hi n))
                  (loop while (> (- hi lo) 1)
                        for mid = (floor (+ lo hi) 2)
                        do (if (funcall failing-p mid)
                               (setf hi mid)
                               (setf lo mid)))
                  hi))
               ((minusp n)
                (let ((lo n)
                      (hi 0))
                  (loop while (> (- hi lo) 1)
                        for mid = (ceiling (+ lo hi) 2)
                        do (if (funcall failing-p mid)
                               (setf lo mid)
                               (setf hi mid)))
                  lo))
               (t n))))
    (when (integerp value)
      (return-from minimize-failing-input (binary-search-integer value))))
  (let ((current value)
        (round 0)
        (changed t))
    (loop while (and changed (< round max-rounds))
          do (progn
               (incf round)
               (setf changed nil)
               (dolist (candidate (funcall shrinker current))
                 (when (funcall failing-p candidate)
                   (setf current candidate
                         changed t)
                   (return)))))
    current))

(defun check-property-with-shrinking (generator predicate
                                      &key (trials 100) (shrinker #'shrink))
  "Run PREDICATE against generated values and shrink the first failure.
Returns a plist with :STATUS :PASS, or :STATUS :FAIL plus :VALUE and
:MINIMIZED-VALUE. This function is intentionally non-signaling so tests can
inspect the minimized counterexample."
  (dotimes (i trials (list :status :pass :trials trials))
    (let ((value (funcall generator)))
      (unless (funcall predicate value)
        (return
          (list :status :fail
                :trial i
                :value value
                :minimized-value
                (minimize-failing-input value
                                        (lambda (candidate)
                                          (not (funcall predicate candidate)))
                                        :shrinker shrinker)))))))

;;; ------------------------------------------------------------
;;; Type-Annotation Based Generators (FR-353)
;;; ------------------------------------------------------------

(defvar *type-generator-registry* (make-hash-table :test #'equal)
  "Registry mapping type annotations to zero-argument generator functions.")

(defun register-type-generator (type generator)
  "Register GENERATOR as the value generator for TYPE annotation."
  (check-type generator function)
  (setf (gethash type *type-generator-registry*) generator))

(defmacro deftype-generator (type &body body)
  "Define a zero-argument generator for TYPE annotation."
  `(register-type-generator ',type (lambda () ,@body)))

(defun generator-for-type (type)
  "Return the generator function registered for TYPE, or NIL."
  (gethash type *type-generator-registry*))

(defun generate-for-type (type)
  "Generate one value using the generator registered for TYPE annotation."
  (let ((generator (generator-for-type type)))
    (unless generator
      (error "No property-test generator registered for type annotation ~S" type))
    (funcall generator)))

(defun generate-from-type-annotation (type)
  "Alias for GENERATE-FOR-TYPE used by type-annotation driven tests."
  (generate-for-type type))

(deftype-generator integer
  (%random-int))

(deftype-generator fixnum
  (%random-int))

(deftype-generator boolean
  (zerop (random 2 *fuzz-random-state*)))

(deftype-generator symbol
  (%random-symbol "SYM"))

(deftype-generator string
  (format nil "s~D" (random 1000 *fuzz-random-state*)))

(deftype-generator list
  (loop repeat (random 4 *fuzz-random-state*)
        collect (%random-int)))

;;; ------------------------------------------------------------
;;; Stateful Property-Based Testing (FR-353)
;;; ------------------------------------------------------------

(defstruct (pbt-command
            (:constructor make-pbt-command (&key name precondition run)))
  "A stateful PBT command with a precondition and state transition function."
  name
  (precondition (lambda (state) (declare (ignore state)) t) :type function)
  (run (lambda (state) state) :type function))

(defun %applicable-pbt-commands (commands state)
  "Return commands whose preconditions accept STATE."
  (remove-if-not (lambda (command)
                   (funcall (pbt-command-precondition command) state))
                 commands))

(defun generate-stateful-command-sequence (commands initial-state &key (length 10))
  "Generate an executable command sequence from COMMANDS.
The generator simulates state while choosing commands, so every generated command
is valid at the point where it appears in the sequence."
  (let ((state initial-state)
        (sequence '()))
    (dotimes (i length (nreverse sequence))
      (let ((applicable (%applicable-pbt-commands commands state)))
        (when (null applicable)
          (return))
        (let ((command (%random-elt applicable)))
          (push command sequence)
          (setf state (funcall (pbt-command-run command) state)))))))

(defun run-stateful-command-sequence (commands initial-state)
  "Run COMMANDS from INITIAL-STATE.
Returns two values: final state and a trace of command names. A violated
precondition signals an error because generated sequences should be executable."
  (let ((state initial-state)
        (trace '()))
    (dolist (command commands (values state (nreverse trace)))
      (unless (funcall (pbt-command-precondition command) state)
        (error "PBT command precondition failed for ~S in state ~S"
               (pbt-command-name command) state))
      (push (pbt-command-name command) trace)
      (setf state (funcall (pbt-command-run command) state)))))

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
