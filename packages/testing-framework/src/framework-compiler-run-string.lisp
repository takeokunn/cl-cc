;;;; tests/framework-compiler-run-string.lisp — CL-CC Test Framework
;;;; (run-string / macroexpand focused helpers)

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Run-string based assertions
;;; ------------------------------------------------------------

(defun %get-instructions (compilation-result)
  "Extract the instruction list from a compilation-result."
  (vm-program-instructions (compilation-result-program compilation-result)))

(defvar *run-string-assertion-error* nil
  "Holds the last condition captured by %WITH-RUN-STRING-RESULT.")

(defmacro %with-run-string-result ((result expr-string) &body body)
  "Bind RESULT to the outcome of RUN-STRING on EXPR-STRING.
If RUN-STRING signals, store the condition in *RUN-STRING-ASSERTION-ERROR* so
assertion helpers can fail explicitly instead of silently treating errors as NIL."
  `(let ((*run-string-assertion-error* nil)
         (,result nil))
     (handler-case
         (setf ,result (run-string ,expr-string))
       (error (e)
         (setf *run-string-assertion-error* e)))
     ,@body))

(defmacro %with-run-string-assertion ((result expr-string)
                                      (&key condition message expected actual form))
  "Run EXPR-STRING once, bind RESULT, and signal a standardized assertion failure."
  `(%with-run-string-result (,result ,expr-string)
     (when *run-string-assertion-error*
       (%fail-test (format nil "run-string signaled ~A for ~S"
                           *run-string-assertion-error* ,expr-string)
                   :expected ,expected
                   :actual *run-string-assertion-error*
                   :form ,form))
     (unless ,condition
       (%fail-test ,message
                   :expected ,expected
                   :actual ,actual
                   :form ,form))
     t))

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

When :STDLIB is true, evaluate through `(run-string EXPR :stdlib t)` so callers
can share the same high-level assertion for both plain and stdlib-backed cases."
  (let ((exp (gensym "EXP"))
        (act (gensym "ACT"))
        (condition-var (gensym "CONDITION")))
    `(let ((,exp ,expected)
           (,act nil)
           (,condition-var nil))
       (handler-case
           (setf ,act (run-string ,expr ,@(when stdlib '(:stdlib t))))
         (error (e)
           (setf ,condition-var e)))
       (when ,condition-var
         (%fail-test (format nil "assert-evaluates-to: ~S signaled ~A"
                             ,expr ,condition-var)
                     :expected ,exp
                     :actual ,condition-var
                     :form (list 'assert-evaluates-to ,expr ,expected :stdlib ,stdlib)))
       (unless (equal ,act ,exp)
         (%fail-test (format nil "assert-evaluates-to: ~S evaluated to ~S, expected ~S"
                             ,expr ,act ,exp)
                     :expected ,exp
                     :actual   ,act
                     :form     (list 'assert-evaluates-to ,expr ,expected :stdlib ,stdlib)))
       t)))

(defmacro assert-macro-expands-to (form expected)
  "Assert that (our-macroexpand FORM) is EQUAL to EXPECTED."
  (let ((exp (gensym "EXP"))
        (act (gensym "ACT"))
        (condition-var (gensym "CONDITION")))
    `(let ((,exp ,expected)
           (,act nil)
           (,condition-var nil))
       (handler-case
           (setf ,act (our-macroexpand ,form))
         (error (e)
           (setf ,condition-var e)))
       (when ,condition-var
         (%fail-test (format nil "assert-macro-expands-to: ~S signaled ~A"
                             ,form ,condition-var)
                     :expected ,exp
                     :actual ,condition-var
                     :form (list 'assert-macro-expands-to ,form ,expected)))
       (unless (equal ,act ,exp)
         (%fail-test (format nil "assert-macro-expands-to: ~S expanded to ~S, expected ~S"
                             ,form ,act ,exp)
                     :expected ,exp
                     :actual   ,act
                     :form     (list 'assert-macro-expands-to ,form ,expected)))
       t)))

(defmacro assert-infers-type (expr expected-type)
  "Assert that compiling EXPR with run-string-typed produces a type whose type matches EXPECTED-TYPE."
  (let ((value (gensym "VALUE"))
        (inferred (gensym "INFERRED")))
    `(multiple-value-bind (,value ,inferred)
         (ignore-errors (run-string-typed ,expr))
       (declare (ignore ,value))
       (unless (and ,inferred
                    (ignore-errors
                      (or (equal ,inferred ',expected-type)
                          (and (typep ,inferred 'cl-cc/type:type-primitive)
                               (equal (cl-cc/type:type-primitive-name ,inferred)
                                      ',expected-type)))))
         (%fail-test (format nil "assert-infers-type: ~S inferred ~S, expected ~S"
                             ,expr ,inferred ',expected-type)
                     :expected ',expected-type
                     :actual   ,inferred
                     :form     (list 'assert-infers-type ,expr ',expected-type)))
       t)))

(defmacro assert-run= (expected expr-string)
  "Compile and run EXPR-STRING in the CL-CC VM, assert result equals EXPECTED."
  (let ((result (gensym "RESULT")))
    `(%with-run-string-assertion
         (,result ,expr-string)
         (:condition (equal ,result ,expected)
          :message (format nil "assert-run=: expected ~S, got ~S for ~S"
                           ,expected ,result ,expr-string)
          :expected ,expected
          :actual ,result
          :form (list 'assert-run= ,expected ,expr-string)))))

(defmacro assert-run-true (expr-string)
  "Compile and run EXPR-STRING, assert result is non-nil."
  (let ((result (gensym "RESULT")))
    `(%with-run-string-assertion
         (,result ,expr-string)
         (:condition ,result
          :message (format nil "assert-run-true: expected non-nil, got ~S for ~S"
                           ,result ,expr-string)
          :expected t
          :actual ,result
          :form (list 'assert-run-true ,expr-string)))))

(defmacro assert-run-false (expr-string)
  "Compile and run EXPR-STRING, assert result is nil."
  (let ((result (gensym "RESULT")))
    `(%with-run-string-assertion
         (,result ,expr-string)
         (:condition (null ,result)
          :message (format nil "assert-run-false: expected nil, got ~S for ~S"
                           ,result ,expr-string)
          :expected nil
          :actual ,result
          :form (list 'assert-run-false ,expr-string)))))

(defmacro assert-run-signals (condition-type expr-string)
  "Compile and run EXPR-STRING, assert it signals CONDITION-TYPE."
  (let ((signaledp (gensym "SIGNALEDP")))
    `(let ((,signaledp nil))
       (handler-case
           (run-string ,expr-string)
         (,condition-type ()
           (setf ,signaledp t)))
       (unless ,signaledp
         (%fail-test
          (format nil "assert-run-signals: expected ~S but no condition for ~S"
                  ',condition-type ,expr-string)
          :form (list 'assert-run-signals ',condition-type ,expr-string)))
       t)))

(defmacro assert-run-string= (expected expr-string)
  "Compile and run EXPR-STRING, assert result string= EXPECTED."
  (let ((result (gensym "RESULT")))
    `(%with-run-string-assertion
         (,result ,expr-string)
         (:condition (and (stringp ,result) (string= ,result ,expected))
          :message (format nil "assert-run-string=: expected ~S, got ~S for ~S"
                           ,expected ,result ,expr-string)
          :expected ,expected
          :actual ,result
          :form (list 'assert-run-string= ,expected ,expr-string)))))

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
