;;;; tests/unit/type/typeclass-tests.lisp — Typeclass System Tests
;;;;
;;;; Tests for src/type/typeclass.lisp:
;;;; typeclass-def, typeclass-instance, registries, dict-env,
;;;; has-typeclass-instance-p, and check-typeclass-constraint.

(in-package :cl-cc/test)

(defsuite typeclass-suite :description "Multi-parameter typeclass system tests"
  :parent cl-cc-unit-suite)

(in-suite typeclass-suite)
(in-suite cl-cc-unit-suite)

;;; ─── typeclass-def struct ──────────────────────────────────────────────────

(deftest typeclass-def-creation
  "typeclass-def stores all fields; superclasses declared; functor/show creation and register/lookup."
  (let ((td (make-typeclass-def
             :name 'eq
             :type-params (list (fresh-type-var "a"))
             :superclasses nil
             :methods '((%equal . nil))
             :associated-types nil
             :functional-deps nil)))
    (assert-true (typeclass-def-p td))
    (assert-eq 'eq (typeclass-def-name td))
    (assert-equal 1 (length (typeclass-def-type-params td)))
    (assert-equal 1 (length (typeclass-def-methods td))))
  (let ((td (make-typeclass-def
             :name 'ord
             :type-params (list (fresh-type-var "a"))
             :superclasses '(eq)
             :methods '((%compare . nil)))))
    (assert-equal '(eq) (cl-cc/type::typeclass-def-superclasses td)))
  (let* ((a  (fresh-type-var 'a))
         (tc (make-typeclass-def
              :name 'functor-test
              :type-params (list a)
              :superclasses nil
              :methods (list (cons 'fmap (make-type-arrow (list type-any) type-any)))
              :associated-types nil
              :functional-deps nil)))
    (assert-true (typeclass-def-p tc))
    (assert-eq 'functor-test (typeclass-def-name tc))
    (assert-= 1 (length (typeclass-def-type-params tc)))
    (assert-= 1 (length (typeclass-def-methods tc))))
  (let* ((a  (fresh-type-var 'a))
         (tc (make-typeclass-def
              :name 'show-test
              :type-params (list a)
              :superclasses nil
              :methods (list (cons 'show (make-type-arrow (list a) type-string)))
              :associated-types nil
              :functional-deps nil)))
    (register-typeclass 'show-test tc)
    (let ((retrieved (lookup-typeclass 'show-test)))
      (assert-true retrieved)
      (assert-true (typeclass-def-p retrieved))
      (assert-eq 'show-test (typeclass-def-name retrieved)))))

;;; ─── typeclass registry ────────────────────────────────────────────────────

(deftest typeclass-registry-operations
  "Typeclass registry: round-trip register+lookup; absent name returns nil."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq)))
    (let ((td (make-typeclass-def :name 'test-tc :type-params nil :methods nil)))
      (register-typeclass 'test-tc td)
      (assert-eq td (lookup-typeclass 'test-tc)))
    (assert-null (lookup-typeclass 'nonexistent))))

(deftest typeclass-registry-rejects-noncanonical-input
  "register-typeclass rejects non-typeclass-def inputs with type-inference-error."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq)))
    (assert-signals type-inference-error
      (register-typeclass 'bad-input '(legacy payload)))))

(deftest typeclass-default-methods-merge-into-instance
  "Instance registration fills missing methods from class defaults."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq))
        (cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (register-typeclass 'pretty-test
                        (make-typeclass-def
                         :name 'pretty-test
                         :type-params nil
                         :methods '((show . string))
                         :defaults '((show . default-show)
                                     (eq . default-eq))))
    (let ((inst (register-typeclass-instance 'pretty-test type-int '((eq . custom-eq)))))
      (assert-eq 'custom-eq (cdr (assoc 'eq (typeclass-instance-methods inst))))
      (assert-eq 'default-show (cdr (assoc 'show (typeclass-instance-methods inst)))))))


(deftest typeclass-instance-registration-new
  "register-typeclass-instance and lookup-typeclass-instance work with new API."
  (register-typeclass-instance 'show-int-test type-int
                               (list (cons 'show (lambda (x) (format nil "~A" x)))))
  (let ((inst (lookup-typeclass-instance 'show-int-test type-int)))
    (assert-true inst)
    (assert-true (typeclass-instance-p inst))
    (assert-eq 'show-int-test (typeclass-instance-class-name inst)))
  (assert-true (has-typeclass-instance-p 'show-int-test type-int))
  (assert-false (has-typeclass-instance-p 'show-int-test type-string)))

;;; ─── typeclass-instance registry ───────────────────────────────────────────

(deftest typeclass-instance-registry-operations
  "Instance registry: register+lookup round-trips; unregistered type returns nil."
  (let ((cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (let ((inst (register-typeclass-instance 'eq type-int '((%equal . t)))))
      (assert-true (typeclass-instance-p inst))
      (assert-eq 'eq (cl-cc/type::typeclass-instance-class-name inst))
      (assert-eq inst (lookup-typeclass-instance 'eq type-int)))
    (assert-null (lookup-typeclass-instance 'eq type-string))))

(deftest-each typeclass-instance-registry-rejection-cases
  "Instance registry rejects both duplicate (same type) and overlapping (type-var) registrations."
  :cases (("duplicate" type-int)
          ("overlap"   (fresh-type-var "a")))
  (second-type)
  (let ((cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (register-typeclass-instance 'eq type-int nil)
    (assert-signals type-inference-error
      (register-typeclass-instance 'eq second-type nil))))

(deftest typeclass-instance-registry-enforces-functional-dependencies
  "Functional dependencies reject conflicting instance families."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq))
        (cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (register-typeclass 'collection-test
                       (make-typeclass-def
                        :name 'collection-test
                        :type-params (list (fresh-type-var "c") (fresh-type-var "e"))
                        :superclasses nil
                        :methods nil
                        :associated-types nil
                        :functional-deps '(((c) . (e)))))
    (register-typeclass-instance 'collection-test
                                 (make-type-product :elems (list type-int type-string))
                                 nil)
    (assert-signals type-inference-error
      (register-typeclass-instance 'collection-test
                                   (make-type-product :elems (list type-int type-bool))
                                   nil))))

;;; ─── default-numeric-typeclass-p ──────────────────────────────────────────

(deftest-each default-numeric-typeclass-p-cases
  "default-numeric-typeclass-p: num/numeric → t; other symbols and non-symbols → nil."
  :cases (("num"         t   'num)
          ("numeric"     t   'numeric)
          ("NUM"         t   'NUM)
          ("NUMERIC"     t   'NUMERIC)
          ("eq"          nil 'eq)
          ("non-symbol"  nil 42)
          ("nil-input"   nil nil))
  (expected name)
  (if expected
      (assert-true  (cl-cc/type::default-numeric-typeclass-p name))
      (assert-false (cl-cc/type::default-numeric-typeclass-p name))))

;;; ─── has-typeclass-instance-p ──────────────────────────────────────────────

(deftest has-typeclass-instance-p-cases
  "has-typeclass-instance-p: true after registration/false before; finds instances via superclass chain."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq))
        (cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (assert-false (has-typeclass-instance-p 'eq type-int))
    (register-typeclass-instance 'eq type-int nil)
    (assert-true  (has-typeclass-instance-p 'eq type-int)))
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq))
        (cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (register-typeclass-instance 'eq type-int nil)
    (register-typeclass 'ord (make-typeclass-def
                              :name 'ord
                              :superclasses '(eq)
                              :type-params nil
                              :methods nil))
    (assert-true (has-typeclass-instance-p 'ord type-int))))

;;; ─── check-typeclass-constraint ────────────────────────────────────────────

(deftest check-typeclass-constraint-behavior
  "check-typeclass-constraint: accepts known instance, unknown, and free var; signals error for missing instance."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq))
        (cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (register-typeclass-instance 'eq type-int nil)
    (cl-cc/type::check-typeclass-constraint 'eq type-int       (type-env-empty))
    (cl-cc/type::check-typeclass-constraint 'eq cl-cc/type::+type-unknown+ (type-env-empty))
    (cl-cc/type::check-typeclass-constraint 'eq (fresh-type-var "a") (type-env-empty))
    (assert-true t))
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq))
        (cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (assert-signals type-inference-error
      (cl-cc/type::check-typeclass-constraint 'eq type-int (type-env-empty)))))

;;; ─── dict-env operations ───────────────────────────────────────────────────

(deftest dict-env-operations
  "dict-env: extend+lookup round-trips; lookup in empty env returns nil."
  (let* ((env     (type-env-empty))
         (methods '((method-a . :impl-a)))
         (env2    (cl-cc/type::dict-env-extend 'eq type-int methods env)))
    (assert-equal methods (cl-cc/type::dict-env-lookup 'eq type-int env2))
    (assert-null          (cl-cc/type::dict-env-lookup 'eq type-int env))))

;;; ─── %typeclass-instance-overlaps-p ─────────────────────────────────────────

(deftest typeclass-instance-overlaps-p-cases
  "%typeclass-instance-overlaps-p: type-var overlaps any concrete type; two different concretes don't."
  (assert-true  (cl-cc/type::%typeclass-instance-overlaps-p
                 (fresh-type-var "a") type-int))
  (assert-false (cl-cc/type::%typeclass-instance-overlaps-p
                 type-int type-string))
  (assert-false (cl-cc/type::%typeclass-instance-overlaps-p
                 type-int type-int)))

;;; ─── %typeclass-instance-args ────────────────────────────────────────────────

(deftest typeclass-instance-args-cases
  "%typeclass-instance-args: wraps single type in list; unpacks type-product elems."
  (let ((args (cl-cc/type::%typeclass-instance-args type-int)))
    (assert-= 1 (length args))
    (assert-eq type-int (first args)))
  (let* ((prod (make-type-product :elems (list type-int type-string)))
         (args (cl-cc/type::%typeclass-instance-args prod)))
    (assert-= 2 (length args))
    (assert-eq type-int    (first args))
    (assert-eq type-string (second args))))

;;; ─── %typeclass-param-name ────────────────────────────────────────────────

(deftest-each typeclass-param-name-cases
  "%typeclass-param-name: type-var→var name string; symbol→symbol-name; other→string-upcase."
  :cases (("type-var" nil)
          ("symbol"   'foo)
          ("string"   "bar"))
  (param-val)
  (let ((param (or param-val (fresh-type-var "TestVar"))))
    (assert-true (stringp (cl-cc/type::%typeclass-param-name param)))))


