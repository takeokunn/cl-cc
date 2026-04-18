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

(defmacro assert-= (expected actual)
  "Assert numeric equality."
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,actual))
       (unless (= ,e ,a)
         (%fail-test "assert-= failed"
                     :expected ,e
                     :actual ,a
                     :form '(= ,expected ,actual)))
       t)))

(defmacro assert-eq (expected actual)
  "Assert pointer equality (eq)."
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,actual))
       (unless (eq ,e ,a)
         (%fail-test "assert-eq failed"
                     :expected ,e
                     :actual ,a
                     :form '(eq ,expected ,actual)))
       t)))

(defmacro assert-eql (expected actual)
  "Assert eql equality."
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,actual))
       (unless (eql ,e ,a)
         (%fail-test "assert-eql failed"
                     :expected ,e
                     :actual ,a
                     :form '(eql ,expected ,actual)))
       t)))

(defmacro assert-equal (expected actual)
  "Assert structural equality (equal)."
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,actual))
       (unless (equal ,e ,a)
         (%fail-test "assert-equal failed"
                     :expected ,e
                     :actual ,a
                     :form '(equal ,expected ,actual)))
       t)))

(defmacro assert-string= (expected actual)
  "Assert string equality."
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,actual))
       (unless (string= ,e ,a)
         (%fail-test "assert-string= failed"
                     :expected ,e
                     :actual ,a
                     :form '(string= ,expected ,actual)))
       t)))

(defmacro assert-null (form)
  "Assert form evaluates to nil."
  (let ((v (gensym "V")))
    `(let ((,v ,form))
       (unless (null ,v)
         (%fail-test "assert-null failed"
                     :expected nil
                     :actual ,v
                     :form ',form))
       t)))

(defmacro assert-true (form)
  "Assert form evaluates to a truthy value."
  (let ((v (gensym "V")))
    `(let ((,v ,form))
       (unless ,v
         (%fail-test "assert-true failed"
                     :expected t
                     :actual nil
                     :form ',form))
       t)))

(defmacro assert-false (form)
  "Assert form evaluates to a falsy value."
  (let ((v (gensym "V")))
    `(let ((,v ,form))
       (when ,v
         (%fail-test "assert-false failed"
                     :expected nil
                     :actual ,v
                     :form ',form))
       t)))

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
