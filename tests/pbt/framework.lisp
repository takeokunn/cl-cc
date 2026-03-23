;;;; tests/pbt/framework.lisp - Property-Based Testing Framework Implementation
;;;
;;; This module provides a complete property-based testing framework for CL-CC,
;;; including generators, combinators, and integration with the cl-cc/test framework.

(in-package :cl-cc/pbt)

;;; Configuration Variables

(defvar *test-count* 10
  "Default number of test cases to run for each property.")

(defvar *max-list-length* 20
  "Maximum length for generated lists.")

(defvar *max-string-length* 50
  "Maximum length for generated strings.")

(defvar *size* 0
  "Current size parameter for generators (0-100).")

(defvar *pbt-random-state* (make-random-state t)
  "Random state for reproducible PBT test runs.")

;;; Generator Protocol

(defclass generator ()
  ((generate-fn :initarg :generate-fn :reader generator-generate-fn
                :documentation "Function that generates a random value.")
   (shrink-fn :initarg :shrink-fn :initform (lambda (x) (declare (ignore x)) nil)
              :reader generator-shrink-fn
              :documentation "Function that returns a list of shrunk values."))
  (:documentation "Base class for all generators."))

(defun generate (generator)
  "Generate a random value using GENERATOR.
   Accepts either a generator object or a plain function (as returned by gen-fn)."
  (if (functionp generator)
      (funcall generator)
      (funcall (generator-generate-fn generator))))

(defun gen-fn (generator)
  "Convert a generator object to a plain function.
   Returns a lambda that calls generate on the generator."
  (lambda () (generate generator)))

(defun shrink-value (generator value)
  "Return a list of shrunk values for VALUE using GENERATOR."
  (funcall (generator-shrink-fn generator) value))

;;; Generator Construction

(defun make-generator (generate-fn &key (shrink-fn (lambda (x) (declare (ignore x)) nil)))
  "Create a new generator with the given generate and shrink functions."
  (make-instance 'generator
                 :generate-fn generate-fn
                 :shrink-fn shrink-fn))

;;; Built-in Generators

(defun gen-integer (&key (min most-negative-fixnum) (max most-positive-fixnum))
  "Generate random integers between MIN and MAX (inclusive)."
  (make-generator
   (lambda ()
     (+ min (random (1+ (- max min)) *pbt-random-state*)))
   :shrink-fn (lambda (n)
                (let ((shrinks '()))
                  (when (and (minusp n) (< min (floor n 2)))
                    (push (floor n 2) shrinks))
                  (when (and (plusp n) (> max (floor n 2)))
                    (push (floor n 2) shrinks))
                  (when (and (minusp n) (<= min 0))
                    (push 0 shrinks))
                  (when (and (plusp n) (>= max 0))
                    (push 0 shrinks))
                  shrinks))))

(defun gen-float (&key (min -1.0d6) (max 1.0d6))
  "Generate random floating-point numbers between MIN and MAX."
  (make-generator
   (lambda ()
     (+ min (random (- max min) *pbt-random-state*)))))

(defun gen-boolean ()
  "Generate random boolean values (T or NIL)."
  (make-generator
   (lambda ()
     (zerop (random 2 *pbt-random-state*)))))

(defun gen-character (&key (alphanumeric-only t))
  "Generate random characters."
  (make-generator
   (lambda ()
     (if alphanumeric-only
         (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
           (char chars (random (length chars) *pbt-random-state*)))
         (code-char (random 256 *pbt-random-state*))))))

(defun gen-symbol (&key (package nil) (prefix "SYM"))
  "Generate random symbols.
   PACKAGE can be NIL (uninterned), :keyword, or a package object.
   PREFIX is prepended to a random number."
  (make-generator
   (lambda ()
     (let ((name (format nil "~A-~D" prefix (random 100000 *pbt-random-state*))))
       (case package
         (:keyword (intern name :keyword))
         ((nil) (gensym prefix))
         (t (intern name (if (packagep package) package (find-package package)))))))))

(defun gen-string (&key (length nil) (min-length 0) (max-length *max-string-length*)
                        (alphanumeric-only t))
  "Generate random strings."
  (make-generator
   (lambda ()
     (let* ((range (- max-length min-length))
            (len (or length (+ min-length (if (zerop range) 0 (random range *pbt-random-state*)))))
            (chars (loop repeat len
                         collect (generate (gen-character :alphanumeric-only alphanumeric-only)))))
       (coerce chars 'string)))))

(defun gen-list (&key (element-gen (gen-integer))
                      (min-length 0) (max-length *max-list-length*))
  "Generate random lists with elements from ELEMENT-GEN."
  (make-generator
   (lambda ()
     (let ((range (- max-length min-length)))
       (loop repeat (+ min-length (if (zerop range) 0 (random range *pbt-random-state*)))
             collect (generate element-gen))))
   :shrink-fn (lambda (lst)
                (let ((shrinks '()))
                  ;; Try shorter lists
                  (when (> (length lst) min-length)
                    (push (subseq lst 0 (1- (length lst))) shrinks))
                  ;; Try removing elements
                  (loop for i from 0 below (length lst)
                        for shorter = (append (subseq lst 0 i) (subseq lst (1+ i)))
                        when (>= (length shorter) min-length)
                          do (push shorter shrinks))
                  shrinks))))

(defun gen-cons (&key (car-gen (gen-integer)) (cdr-gen (gen-integer)))
  "Generate random cons cells."
  (make-generator
   (lambda ()
     (cons (generate car-gen) (generate cdr-gen)))))

(defun gen-vector (&key (element-gen (gen-integer))
                        (min-length 0) (max-length *max-list-length*))
  "Generate random vectors with elements from ELEMENT-GEN."
  (make-generator
   (lambda ()
     (coerce (generate (gen-list :element-gen element-gen
                                  :min-length min-length
                                  :max-length max-length))
             'vector))))

;;; Combinators

(defun gen-one-of (choices)
  "Generate one of the elements from CHOICES."
  (make-generator
   (lambda ()
     (nth (random (length choices) *pbt-random-state*) choices))))

(defun gen-tuple (&rest generators)
  "Generate a tuple with one element from each generator."
  (make-generator
   (lambda ()
     (mapcar #'generate generators))
   :shrink-fn (lambda (tuple)
                (loop for i from 0 below (length tuple)
                      for gen in generators
                      for elem = (nth i tuple)
                      append (mapcar (lambda (shrunk)
                                       (let ((copy (copy-list tuple)))
                                         (setf (nth i copy) shrunk)
                                         copy))
                                     (shrink-value gen elem))))))

(defun gen-list-of (element-gen &key (min-length 0) (max-length *max-list-length*))
  "Generate lists of elements from ELEMENT-GEN."
  (gen-list :element-gen element-gen :min-length min-length :max-length max-length))

(defun gen-alist (&key (key-gen (gen-symbol :prefix "KEY"))
                       (value-gen (gen-integer))
                       (min-length 0) (max-length *max-list-length*))
  "Generate association lists."
  (gen-list-of (gen-tuple key-gen value-gen)
               :min-length min-length
               :max-length max-length))

(defun gen-map (key-gen value-gen)
  "Generate property lists (alternating keys and values)."
  (make-generator
   (lambda ()
     (let ((alist (generate (gen-alist :key-gen key-gen
                                        :value-gen value-gen))))
       (loop for (key . value) in alist
             append (list key value))))))

(defun gen-bind (generator fn)
  "Monadic bind: apply FN to the generated value to get a new generator."
  (make-generator
   (lambda ()
     (generate (funcall fn (generate generator))))))

(defun gen-fmap (fn generator)
  "Map FN over the generated value."
  (make-generator
   (lambda ()
     (funcall fn (generate generator)))))

(defun gen-such-that (generator predicate &key (max-tries 100))
  "Generate values from GENERATOR that satisfy PREDICATE."
  (make-generator
   (lambda ()
     (loop repeat max-tries
           for value = (generate generator)
           when (funcall predicate value)
             do (return-from nil value)
           finally (error "Could not generate value satisfying predicate after ~D tries"
                          max-tries)))))

(defun gen-resize (size generator)
  "Create a generator that runs with a specific SIZE."
  (make-generator
   (lambda ()
     (let ((*size* size))
       (generate generator)))))

(defun gen-scale (scale-fn generator)
  "Create a generator that scales the size parameter."
  (make-generator
   (lambda ()
     (let ((*size* (funcall scale-fn *size*)))
       (generate generator)))))

;;; AST Generators

(defvar *ast-terminal-generators* nil
  "List of generators for terminal AST nodes.")

(defvar *ast-recursive-generators* nil
  "List of generators for recursive AST nodes.")

(defun init-ast-generators ()
  "Initialize AST generator lists based on current SIZE."
  (setf *ast-terminal-generators*
        (list
         ;; Integer literal
         (make-generator
          (lambda () (make-ast-int :value (generate (gen-integer :min -100 :max 100)))))
         ;; Variable
         (make-generator
          (lambda () (make-ast-var :name (generate (gen-symbol :prefix "VAR")))))))

  (setf *ast-recursive-generators*
        (list
         ;; Binary operation
         (make-generator
          (lambda ()
            (make-ast-binop
                           :op (generate (gen-one-of '(+ - *)))
                           :lhs (generate (gen-ast-node))
                           :rhs (generate (gen-ast-node)))))

         ;; If expression
         (make-generator
          (lambda ()
            (make-ast-if
                           :cond (generate (gen-ast-node))
                           :then (generate (gen-ast-node))
                           :else (generate (gen-ast-node)))))

         ;; Progn
         (make-generator
          (lambda ()
            (make-ast-progn
                           :forms (generate (gen-list-of (gen-ast-node)
                                                          :min-length 1 :max-length 3)))))

         ;; Let binding
         (make-generator
          (lambda ()
            (let ((var-name (generate (gen-symbol :prefix "VAR"))))
              (make-ast-let
                             :bindings (list (cons var-name (generate (gen-ast-node))))
                             :body (list (make-ast-var :name var-name))))))

         ;; Lambda
         (make-generator
          (lambda ()
            (make-ast-lambda
                           :params (generate (gen-list-of (gen-symbol :prefix "ARG")
                                                           :min-length 0 :max-length 3))
                           :body (list (generate (gen-ast-node))))))

         ;; Function call
         (make-generator
          (lambda ()
            (make-ast-call
                           :func (generate (gen-symbol :prefix "FN"))
                           :args (generate (gen-list-of (gen-ast-node)
                                                         :min-length 0 :max-length 3))))))))

(defun gen-ast-node (&key (max-depth 3))
  "Generate random AST nodes.
   Uses SIZE parameter to control complexity and avoid infinite recursion."
  (make-generator
   (lambda ()
     ;; Initialize generators if needed
     (unless *ast-terminal-generators*
       (init-ast-generators))

     ;; Use size to decide between terminal and recursive nodes
     ;; As size decreases, prefer terminal nodes
     (let ((effective-depth (max 0 (min max-depth (floor (- 100 *size*) 20)))))
       (if (or (zerop effective-depth)
               (and (> effective-depth 0)
                    (< (random 100 *pbt-random-state*)
                       (- 100 (/ *size* 2)))))
           ;; Generate terminal node
           (generate (gen-one-of *ast-terminal-generators*))
           ;; Generate recursive node with reduced depth
           (let ((*size* (min 100 (+ *size* 20))))
             (generate (gen-one-of *ast-recursive-generators*))))))))

(defun gen-expr (&key (max-depth 3))
  "Generate random Lisp expressions (S-expressions).
   These can be fed to the CL-CC frontend for testing."
  (gen-fmap #'ast-to-sexp (gen-ast-node :max-depth max-depth)))

;;; Shrinking

(defun shrink (value)
  "Generic shrink function that dispatches based on VALUE type."
  (typecase value
    (integer (shrink-integer value))
    (list (shrink-list value))
    (t nil)))

(defun shrink-integer (n)
  "Return a list of shrunk integers toward 0."
  (cond
    ((zerop n) nil)
    ((> (abs n) 1) (list 0 (floor n 2)))
    (t (list 0))))

(defun shrink-list (lst)
  "Return a list of shrunk versions of LST."
  (when (null lst)
    (return-from shrink-list nil))
  (let ((shrinks '()))
    ;; Try empty list
    (push nil shrinks)
    ;; Try removing first element
    (when (cdr lst)
      (push (cdr lst) shrinks))
    ;; Try half-length
    (when (> (length lst) 2)
      (push (subseq lst 0 (floor (length lst) 2)) shrinks))
    shrinks))

;;; Property Definition Macros

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun extract-generators (args)
    "Extract generator bindings from argument list.
     ARGS is either a flat list (var gen var gen ...) or a list of (var gen) pairs."
    (if (and (consp args) (consp (car args)))
        ;; Nested format: ((var gen) (var gen) ...)
        (loop for pair in args
              collect (if (and (consp pair) (= (length pair) 2))
                          pair
                          (list (car pair) (cadr pair))))
        ;; Flat format: (var gen var gen ...)
        (loop for (var gen) on args by #'cddr
              collect (list var gen))))

  (defun generate-binding-forms (gen-bindings)
    "Generate LET binding forms from generator bindings."
    (loop for (var gen) in gen-bindings
          collect (list var `(generate ,gen))))

  (defun format-args (args)
    "Format argument list for display in error messages."
    (loop for (var gen) in (extract-generators args)
          collect (list var (string-downcase (symbol-name (type-of gen)))))))

(defmacro defproperty (name args &body body)
  "Define a property-based test with automatic test generation.

   NAME is the test name (symbol).
   ARGS is a list of (variable generator) pairs.
   BODY is the property to test.

   Example:
     (defproperty addition-commutativity (a (gen-integer) b (gen-integer))
       (= (+ a b) (+ b a)))"
  (let ((gen-bindings (extract-generators args))
        (test-count-var (gensym "COUNT"))
        (iteration-var (gensym "I"))
        (failure-var (gensym "FAILURE"))
        (args-var (gensym "ARGS")))
    `(deftest ,name
       (let ((,test-count-var *test-count*)
             (,failure-var nil)
             (,args-var nil))
         (loop for ,iteration-var from 1 to ,test-count-var
               do (let* ,(generate-binding-forms gen-bindings)
                    (setf ,args-var (list ,@(mapcar #'car gen-bindings)))
                    (handler-case
                        (let ((result (progn ,@body)))
                          (unless result
                            (setf ,failure-var (list :failed ,iteration-var ,args-var))
                            (return)))
                      (error (e)
                        (setf ,failure-var (list :error ,iteration-var ,args-var e))
                        (return)))))
         (when ,failure-var
           (destructuring-bind (type iteration args &optional error) ,failure-var
             (case type
               (:failed
                (%fail-test (format nil "Property ~S failed on iteration ~D with args ~S"
                                    ',name iteration args))
               (:error
                (%fail-test (format nil "Property ~S raised error ~A on iteration ~D with args ~S"
                                    ',name error iteration args)))))))))))

(defmacro for-all (args &body body)
  "Run property tests inline without defining a named test.

   ARGS is a list of (variable generator) pairs.
   BODY is the property to test.

   Returns T if all tests pass, NIL otherwise.

   Example:
     (for-all (a (gen-integer) b (gen-integer))
       (= (+ a b) (+ b a)))"
  (let ((gen-bindings (extract-generators args))
        (test-count-var (gensym "COUNT"))
        (iteration-var (gensym "I"))
        (success-var (gensym "SUCCESS"))
        (args-var (gensym "ARGS")))
    `(let ((,test-count-var *test-count*)
           (,success-var t)
           (,args-var nil))
       (loop for ,iteration-var from 1 to ,test-count-var
             do (let* ,(generate-binding-forms gen-bindings)
                  (setf ,args-var (list ,@(mapcar #'car gen-bindings)))
                  (handler-case
                      (unless (progn ,@body)
                        (format t "~&Property failed on iteration ~D with args: ~S~%"
                                ,iteration-var ,args-var)
                        (setf ,success-var nil)
                        (return))
                    (error (e)
                      (format t "~&Property raised error ~A on iteration ~D with args: ~S~%"
                              e ,iteration-var ,args-var)
                      (setf ,success-var nil)
                      (return)))))
       ,success-var)))

(defun check (property-fn &key (count *test-count*) (seed nil))
  "Run a property function COUNT times with optional SEED for reproducibility.

   PROPERTY-FN is a function of no arguments that returns T or NIL.
   COUNT is the number of test cases to run.
   SEED is an optional random seed for reproducibility."
  (let* ((*random-state* (if seed
                             (make-random-state t)
                             (make-random-state t)))
         (*test-count* count)
         (failures nil))
    (when seed
      (setf *random-state* (sb-ext:seed-random-state seed)))
    (loop for i from 1 to count
          do (handler-case
                 (unless (funcall property-fn)
                   (push i failures))
               (error (e)
                 (declare (ignore e))
                 (push i failures))))
    (if failures
        (values nil (nreverse failures))
        (values t nil))))

;;; Custom Generator Definition

(defmacro defgenerator (name args &body body)
  "Define a custom generator function.

   NAME is the generator name (will be available as gen-NAME).
   ARGS are the generator arguments.
   BODY should return a generator object.

   Example:
     (defgenerator point ()
       (gen-tuple (gen-integer) (gen-integer)))"
  (let ((generator-name (intern (format nil "GEN-~A" (symbol-name name)))))
    `(defun ,generator-name ,args
       ,@body)))

;;; Test Utilities

(defun run-property-tests ()
  "Run all property-based tests."
  (run-suite 'cl-cc-pbt-suite))

(defun report-failure (test-name iteration args &optional (error nil))
  "Report a property test failure in a standardized format."
  (format t "~&~%=== Property Test Failure ===~%")
  (format t "Test: ~S~%" test-name)
  (format t "Iteration: ~D~%" iteration)
  (format t "Arguments: ~S~%" args)
  (when error
    (format t "Error: ~A~%" error))
  (format t "----------------------------~%~%")
  nil)

;;; Test Suite Definition

(defsuite cl-cc-pbt-suite
  :description "Property-Based Testing suite for CL-CC"
  :parent cl-cc-suite)

;;; Example Properties

(in-suite cl-cc-pbt-suite)

(defproperty integer-addition-commutativity
    (a (gen-integer :min -1000 :max 1000)
       b (gen-integer :min -1000 :max 1000))
  (= (+ a b) (+ b a)))

(defproperty integer-multiplication-commutativity
    (a (gen-integer :min -100 :max 100)
       b (gen-integer :min -100 :max 100))
  (= (* a b) (* b a)))

(defproperty list-length-positive
    (lst (gen-list :element-gen (gen-integer)
                   :min-length 0 :max-length 100))
  (>= (length lst) 0))

(defproperty string-length-positive
    (s (gen-string :min-length 0 :max-length 100))
  (>= (length s) 0))

(defproperty reverse-involution
    (lst (gen-list :element-gen (gen-integer)
                   :min-length 0 :max-length 20))
  (equal lst (reverse (reverse lst))))

(defproperty append-associativity
    (a (gen-list :element-gen (gen-integer) :max-length 10)
       b (gen-list :element-gen (gen-integer) :max-length 10)
       c (gen-list :element-gen (gen-integer) :max-length 10))
  (equal (append (append a b) c)
         (append a (append b c))))
