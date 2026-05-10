(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Assertion helpers
;;; ------------------------------------------------------------

(defun %format-value (val)
  "Format a value for diagnostic output."
  (let ((*print-length* 20)
        (*print-level* 5))
    (format nil "~S" val)))

(defun %fail-test (message &key expected actual form at)
  "Record a test failure and signal test-failure condition."
  (let ((yaml (with-output-to-string (s)
                (format s "  ---~%")
                (format s "  message: ~S~%" message)
                (when expected
                  (format s "  expected: ~A~%" (%format-value expected)))
                (when actual
                  (format s "  actual: ~A~%" (%format-value actual)))
                (when form
                  (format s "  form: ~A~%" form))
                (when at
                  (format s "  at: ~A~%" at))
                (format s "  ..."))))
    (error 'test-failure :message yaml)))

;;; ------------------------------------------------------------
;;; Core assertion macros
;;; ------------------------------------------------------------

(defmacro %assert-binary (predicate failure-message expected actual)
  "Shared implementation for binary assertion macros."
  (let ((e (gensym "E"))
        (a (gensym "A")))
    `(let ((,e ,expected)
           (,a ,actual))
       (unless (,predicate ,e ,a)
         (%fail-test ,failure-message
                     :expected ,e
                     :actual ,a
                     :form '(,predicate ,expected ,actual)))
       t)))

(defmacro %assert-unary (predicate failure-message expected-value form)
  "Shared implementation for unary assertion macros."
  (let ((v (gensym "V")))
    `(let ((,v ,form))
       (unless (,predicate ,v)
         (%fail-test ,failure-message
                     :expected ',expected-value
                     :actual ,v
                     :form ',form))
       t)))

(defmacro %define-binary-assertion (name predicate failure-message docstring)
  "Define a binary assertion macro backed by %ASSERT-BINARY."
  (let ((expected (gensym "EXPECTED"))
        (actual (gensym "ACTUAL")))
    `(defmacro ,name (,expected ,actual)
       ,docstring
       (list '%assert-binary ',predicate ,failure-message ,expected ,actual))))

(defmacro %define-unary-assertion (name predicate failure-message expected-value docstring)
  "Define a unary assertion macro backed by %ASSERT-UNARY."
  (let ((form (gensym "FORM")))
    `(defmacro ,name (,form)
       ,docstring
       (list '%assert-unary ',predicate ,failure-message ',expected-value ,form))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (%define-binary-assertion assert-= = "assert-= failed"
    "Assert numeric equality.")
  (%define-binary-assertion assert-eq eq "assert-eq failed"
    "Assert pointer equality (eq).")
  (%define-binary-assertion assert-eql eql "assert-eql failed"
    "Assert eql equality.")
  (%define-binary-assertion assert-equal equal "assert-equal failed"
    "Assert structural equality (equal).")
  (%define-binary-assertion assert-string= string= "assert-string= failed"
    "Assert string equality.")
  (%define-unary-assertion assert-null null "assert-null failed" nil
    "Assert form evaluates to nil.")
  (%define-unary-assertion assert-true identity "assert-true failed" t
    "Assert form evaluates to a truthy value.")
  (%define-unary-assertion assert-false null "assert-false failed" nil
    "Assert form evaluates to a falsy value."))

(defmacro assert-type (type-name object)
  "Assert object is of type type-name. Note: type-name comes first."
  (let ((o (gensym "O")))
    `(let ((,o ,object))
       (unless (typep ,o ',type-name)
         (%fail-test "assert-type failed"
                     :expected ',type-name
                     :actual (type-of ,o)
                     :form '(typep ,object ,type-name)))
       t)))

(defmacro assert-signals (condition-type form)
  "Assert that form signals a condition of condition-type."
  `(handler-case
       (progn
         ,form
         (%fail-test (format nil "assert-signals: expected ~S to be signaled, but no condition was raised"
                             ',condition-type)
                     :form ',form))
     (,condition-type () t)
     (error (c)
       (%fail-test (format nil "assert-signals: expected ~S but got ~S: ~A"
                           ',condition-type (type-of c) c)
                   :form ',form))))

(defmacro assert-values (form &rest expected-values)
  "Assert multiple return values of form."
  (let ((actuals (gensym "ACTUALS")))
    `(let ((,actuals (multiple-value-list ,form)))
       (let ((expected-list (list ,@expected-values)))
         (unless (equal ,actuals expected-list)
           (%fail-test "assert-values failed"
                       :expected expected-list
                       :actual ,actuals
                       :form ',form))
         t))))

(defmacro assert-type-equal (expected actual)
  "Assert that two type-nodes are structurally equal via type-equal-p."
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,actual))
       (unless (type-equal-p ,e ,a)
         (%fail-test "assert-type-equal: types not equal"
                     :expected (type-to-string ,e)
                     :actual   (type-to-string ,a)
                     :form '(type-equal-p ,expected ,actual)))
       t)))

(defmacro assert-unifies (t1 t2)
  "Assert that types T1 and T2 unify successfully."
  (let ((s (gensym "S")) (ok (gensym "OK")))
    `(multiple-value-bind (,s ,ok) (type-unify ,t1 ,t2)
       (declare (ignore ,s))
       (unless ,ok
         (%fail-test "assert-unifies: types failed to unify"
                     :expected "unification success"
                     :actual   "unification failure"
                     :form '(type-unify ,t1 ,t2)))
       t)))

(defmacro assert-not-unifies (t1 t2)
  "Assert that types T1 and T2 fail to unify."
  (let ((s (gensym "S")) (ok (gensym "OK")))
    `(multiple-value-bind (,s ,ok) (type-unify ,t1 ,t2)
       (declare (ignore ,s))
       (when ,ok
         (%fail-test "assert-not-unifies: types unexpectedly unified"
                     :expected "unification failure"
                     :actual   "unification success"
                     :form '(type-unify ,t1 ,t2)))
       t)))

;;; ------------------------------------------------------------
;;; Composite assertion macros — reduce boilerplate
;;; ------------------------------------------------------------

(defmacro assert-bool (expected form)
  "Assert FORM is truthy when EXPECTED is truthy, falsy when EXPECTED is falsy.
Eliminates the (if pred (assert-true ...) (assert-false ...)) pattern."
  (let ((v (gensym "V")) (e (gensym "E")))
    `(let ((,v ,form) (,e ,expected))
       (if ,e
           (unless ,v
             (%fail-test "assert-bool: expected truthy value"
                         :expected t :actual ,v :form ',form))
           (when ,v
             (%fail-test "assert-bool: expected falsy value"
                         :expected nil :actual ,v :form ',form)))
       t)))

(defmacro assert-list-contains (list-form members-form &key length)
  "Assert LIST-FORM contains every element of MEMBERS-FORM (evaluated at runtime, equal test).
MEMBERS-FORM must evaluate to a list. Optionally assert LIST-FORM has exactly LENGTH elements."
  (let ((ls (gensym "LIST"))
        (ms (gensym "MEMBERS")))
    `(let ((,ls ,list-form)
           (,ms ,members-form))
       (dolist (m ,ms)
         (unless (member m ,ls :test #'equal)
           (%fail-test "assert-list-contains: missing member"
                       :expected m :actual ,ls :form ',list-form)))
       ,@(when length
           `((let ((actual-len (length ,ls)))
               (unless (= ,length actual-len)
                 (%fail-test "assert-list-contains: wrong length"
                             :expected ,length :actual actual-len
                             :form '(length ,list-form))))))
       t)))

(defmacro assert-bitfield (word-form &rest field-specs)
  "Assert each byte field of WORD-FORM equals its expected value.
Each field-spec is (byte-position byte-width expected-value)."
  (let ((w (gensym "WORD")))
    `(let ((,w ,word-form))
       ,@(mapcar (lambda (spec)
                   (destructuring-bind (pos width expected) spec
                     `(let ((actual (ldb (byte ,width ,pos) ,w)))
                        (unless (= ,expected actual)
                          (%fail-test "assert-bitfield: field mismatch"
                                      :expected ,expected :actual actual
                                      :at ,(format nil "byte(~A,~A)" width pos))))))
                 field-specs)
       t)))
