;;;; tests/pbt/framework-dsl.lisp — PBT property macros, utilities, suite, and examples
(in-package :cl-cc/pbt)

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
                                    ',name iteration args)))
                (:error
                 (%fail-test (format nil "Property ~S raised error ~A on iteration ~D with args ~S"
                                     ',name error iteration args))))))))))

(defmacro for-all (args &body body)
  "Run property tests inline without defining a named test.

   ARGS is a list of (variable generator) pairs.
   BODY is the property to test.

   Returns T if all tests pass. On failure, fails the enclosing test immediately.

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
                        (%fail-test (format nil "Property failed on iteration ~D with args ~S"
                                            ,iteration-var ,args-var)))
                    (error (e)
                      (%fail-test (format nil "Property raised error ~A on iteration ~D with args ~S"
                                          e ,iteration-var ,args-var))))))
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
  :parent cl-cc-integration-suite)

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

(cl-cc/test:deftest defproperty-error-path-fails-test
  "An erroring property must fail the enclosing test result."
  (let ((name (gensym "PBT-ERROR-PROP-")))
    (unwind-protect
         (progn
           (eval `(defproperty ,name (x (gen-integer))
                    (let ((ignored x))
                      (declare (ignore ignored)))
                    (error "boom")))
           (let ((result (cl-cc/test::%run-single-test
                          (append (copy-list (cl-cc/test:persist-lookup
                                              cl-cc/test::*test-registry* name))
                                  (list :number 1))
                          1
                          nil)))
             (cl-cc/test:assert-equal :fail (getf result :status))
             (cl-cc/test:assert-true
              (search "raised error" (or (getf result :detail) "")))
             (cl-cc/test:assert-true
              (search "boom" (or (getf result :detail) "")))))
      (setf cl-cc/test::*test-registry*
            (cl-cc/test:persist-remove cl-cc/test::*test-registry* name)))))
