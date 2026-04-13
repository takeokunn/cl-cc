;;;; tests/unit/type/typeclass-tests.lisp — Typeclass System Tests
;;;;
;;;; Tests for src/type/typeclass.lisp:
;;;; typeclass-def, typeclass-instance, registries, dict-env,
;;;; has-typeclass-instance-p, check-typeclass-constraint,
;;;; backward-compat structs.

(in-package :cl-cc/test)

(defsuite typeclass-suite :description "Multi-parameter typeclass system tests"
  :parent cl-cc-suite)

(in-suite typeclass-suite)
(in-suite cl-cc-suite)

;;; ─── typeclass-def struct ──────────────────────────────────────────────────

(deftest typeclass-def-creation
  "typeclass-def stores all fields correctly; can declare superclasses."
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
    (assert-equal '(eq) (cl-cc/type::typeclass-def-superclasses td))))

;;; ─── typeclass registry ────────────────────────────────────────────────────

(deftest typeclass-registry-operations
  "Typeclass registry: round-trip register+lookup; absent name returns nil."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq)))
    (let ((td (make-typeclass-def :name 'test-tc :type-params nil :methods nil)))
      (register-typeclass 'test-tc td)
      (assert-eq td (lookup-typeclass 'test-tc)))
    (assert-null (lookup-typeclass 'nonexistent))))

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

;;; ─── typeclass-instance registry ───────────────────────────────────────────

(deftest typeclass-instance-registry-operations
  "Instance registry: register+lookup round-trips; unregistered type returns nil."
  (let ((cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (let ((inst (register-typeclass-instance 'eq type-int '((%equal . t)))))
      (assert-true (typeclass-instance-p inst))
      (assert-eq 'eq (cl-cc/type::typeclass-instance-class-name inst))
      (assert-eq inst (lookup-typeclass-instance 'eq type-int)))
    (assert-null (lookup-typeclass-instance 'eq type-string))))

(deftest typeclass-instance-registry-rejects-duplicates
  "Instance registry rejects duplicate registrations for the same class/type pair."
  (let ((cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (register-typeclass-instance 'eq type-int nil)
    (assert-signals type-inference-error
      (register-typeclass-instance 'eq type-int nil))))

(deftest typeclass-instance-registry-rejects-overlaps
  "Instance registry rejects overlapping registrations for the same class."
  (let ((cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (register-typeclass-instance 'eq type-int nil)
    (assert-signals type-inference-error
      (register-typeclass-instance 'eq (fresh-type-var "a") nil))))

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

;;; ─── has-typeclass-instance-p ──────────────────────────────────────────────

(deftest has-typeclass-instance-p-basic
  "has-typeclass-instance-p: true after registration; false before."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq))
        (cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (assert-false (has-typeclass-instance-p 'eq type-int))
    (register-typeclass-instance 'eq type-int nil)
    (assert-true  (has-typeclass-instance-p 'eq type-int))))

(deftest has-instance-via-superclass
  "has-typeclass-instance-p finds instances via superclass chain."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq))
        (cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    ;; Register 'eq instance for int
    (register-typeclass-instance 'eq type-int nil)
    ;; Register 'ord with superclass 'eq
    (register-typeclass 'ord (make-typeclass-def
                              :name 'ord
                              :superclasses '(eq)
                              :type-params nil
                              :methods nil))
    ;; int has 'eq, and 'ord's superclass is 'eq, so 'ord should find it
    (assert-true (has-typeclass-instance-p 'ord type-int))))

;;; ─── check-typeclass-constraint ────────────────────────────────────────────

(deftest check-typeclass-constraint-behavior
  "check-typeclass-constraint: accepts known instance, unknown, and free var; signals error for missing instance."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq))
        (cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (register-typeclass-instance 'eq type-int nil)
    (cl-cc/type::check-typeclass-constraint 'eq type-int       (type-env-empty))
    (cl-cc/type::check-typeclass-constraint 'eq +type-unknown+ (type-env-empty))
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

;;; ─── Backward-compat: type-class struct ────────────────────────────────────

(deftest compat-type-class-struct-and-registry
  "type-class backward-compat struct: correct fields; storable in typeclass registry."
  (let ((tc (make-type-class :name 'show
                             :type-param (fresh-type-var "a")
                             :methods '((show-method . nil))
                             :defaults '((show-method . default-show)))))
    (assert-true   (type-class-p tc))
    (assert-eq 'show (type-class-name tc))
    (assert-true   (type-var-p (cl-cc/type::type-class-type-param tc)))
    (assert-equal 1 (length (cl-cc/type::type-class-methods tc)))
    (assert-equal '((show-method . default-show))
                  (cl-cc/type::type-class-defaults tc)))
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq)))
    (let ((tc (make-type-class :name 'test-compat :type-param nil :methods nil)))
      (register-typeclass 'test-compat tc)
      (assert-eq tc (lookup-typeclass 'test-compat)))))

;;; ─── Backward-compat: type-skolem ──────────────────────────────────────────

(deftest compat-type-skolem-behavior
  "make-type-skolem: unique IDs; type-skolem-equal-p checks ID identity."
  (let ((s1 (cl-cc/type::make-type-skolem "x"))
        (s2 (cl-cc/type::make-type-skolem "y")))
    (assert-true  (cl-cc/type::type-skolem-p s1))
    (assert-true  (cl-cc/type::type-skolem-p s2))
    (assert-false (= (cl-cc/type::type-skolem-id s1) (cl-cc/type::type-skolem-id s2)))
    (assert-true  (cl-cc/type::type-skolem-equal-p s1 s1))
    (assert-false (cl-cc/type::type-skolem-equal-p s1 s2))))

;;; ─── Backward-compat: type-effect ──────────────────────────────────────────

(deftest compat-type-effect-creation
  "make-type-effect creates a valid effect node with the given name."
  (let ((e (cl-cc/type::make-type-effect :name 'io)))
    (assert-true (cl-cc/type::type-effect-p e))
    (assert-eq 'io (cl-cc/type::type-effect-name e))))

(deftest-each compat-type-effect-name-dispatch
  "type-effect-name works on both type-effect (old) and type-effect-op (new)."
  :cases (("old-style" (cl-cc/type::make-type-effect :name 'io)              'io)
          ("new-style" (make-type-effect-op          :name 'state :args nil) 'state))
  (effect expected-name)
  (assert-eq expected-name (cl-cc/type::type-effect-name effect)))

;;; ─── Backward-compat: type-effectful-function ──────────────────────────────

(deftest compat-type-effectful-function
  "type-effectful-function extends type-arrow."
  (let ((ef (cl-cc/type::make-type-effectful-function
             :params (list type-int) :return type-string
             :effects +io-effect-row+)))
    (assert-true (type-arrow-p ef))
    (assert-equal 1 (length (type-arrow-params ef)))
    (assert-true (type-equal-p type-string (type-arrow-return ef)))
    (assert-true (type-effect-row-p (type-arrow-effects ef)))))

;;; ─── Backward-compat: type-forall-type / type-qualified-type ───────────────

(deftest compat-type-accessor-aliases
  "Accessor aliases: type-forall-type=type-forall-body; type-qualified-type=type-qualified-body."
  (let* ((v (fresh-type-var "a"))
         (f (make-type-forall :var v :body type-int)))
    (assert-true (type-equal-p type-int (cl-cc/type::type-forall-type f))))
  (let ((q (make-type-qualified :constraints nil :body type-int)))
    (assert-true (type-equal-p type-int (cl-cc/type::type-qualified-type q)))))
