;;;; tests/unit/type/typeclass-tests.lisp — Typeclass System Tests
;;;;
;;;; Tests for src/type/typeclass.lisp:
;;;; typeclass-def, typeclass-instance, registries, dict-env,
;;;; has-typeclass-instance-p, check-typeclass-constraint,
;;;; backward-compat structs.

(in-package :cl-cc/test)

(defsuite typeclass-suite :description "Multi-parameter typeclass system tests")

;;; ─── typeclass-def struct ──────────────────────────────────────────────────

(deftest typeclass-def-creation
  "typeclass-def stores all fields."
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
    (assert-equal 1 (length (typeclass-def-methods td)))))

(deftest typeclass-def-with-superclasses
  "typeclass-def can declare superclasses."
  (let ((td (make-typeclass-def
             :name 'ord
             :type-params (list (fresh-type-var "a"))
             :superclasses '(eq)
             :methods '((%compare . nil)))))
    (assert-equal '(eq) (cl-cc/type::typeclass-def-superclasses td))))

;;; ─── typeclass registry ────────────────────────────────────────────────────

(deftest typeclass-registry-roundtrip
  "register-typeclass + lookup-typeclass round-trips."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq)))
    (let ((td (make-typeclass-def :name 'test-tc :type-params nil :methods nil)))
      (register-typeclass 'test-tc td)
      (assert-eq td (lookup-typeclass 'test-tc)))))

(deftest typeclass-registry-missing
  "lookup-typeclass returns nil for unregistered name."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq)))
    (assert-null (lookup-typeclass 'nonexistent))))

;;; ─── typeclass-instance registry ───────────────────────────────────────────

(deftest typeclass-instance-register-lookup
  "register-typeclass-instance + lookup round-trips."
  (let ((cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (let ((inst (register-typeclass-instance 'eq type-int '((%equal . t)))))
      (assert-true (typeclass-instance-p inst))
      (assert-eq 'eq (cl-cc/type::typeclass-instance-class-name inst))
      (let ((found (lookup-typeclass-instance 'eq type-int)))
        (assert-eq inst found)))))

(deftest typeclass-instance-missing
  "lookup-typeclass-instance returns nil when not registered."
  (let ((cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (assert-null (lookup-typeclass-instance 'eq type-string))))

;;; ─── has-typeclass-instance-p ──────────────────────────────────────────────

(deftest has-instance-direct
  "has-typeclass-instance-p finds directly registered instances."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq))
        (cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (register-typeclass-instance 'eq type-int nil)
    (assert-true (has-typeclass-instance-p 'eq type-int))))

(deftest has-instance-missing
  "has-typeclass-instance-p returns nil for missing instance."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq))
        (cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (assert-false (has-typeclass-instance-p 'eq type-int))))

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

(deftest check-constraint-success
  "check-typeclass-constraint succeeds when instance exists."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq))
        (cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (register-typeclass-instance 'eq type-int nil)
    ;; Should not signal
    (cl-cc/type::check-typeclass-constraint 'eq type-int (type-env-empty))))

(deftest check-constraint-gradual-unknown
  "check-typeclass-constraint accepts unknown types (gradual typing)."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq))
        (cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (cl-cc/type::check-typeclass-constraint 'eq +type-unknown+ (type-env-empty))))

(deftest check-constraint-gradual-var
  "check-typeclass-constraint accepts free type-vars."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq))
        (cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (cl-cc/type::check-typeclass-constraint 'eq (fresh-type-var "a") (type-env-empty))))

(deftest check-constraint-failure
  "check-typeclass-constraint signals error for missing instance."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq))
        (cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (assert-signals type-inference-error
      (cl-cc/type::check-typeclass-constraint 'eq type-int (type-env-empty)))))

;;; ─── dict-env operations ───────────────────────────────────────────────────

(deftest dict-env-extend-lookup
  "dict-env-extend + dict-env-lookup round-trips."
  (let ((env (type-env-empty))
        (methods '((method-a . :impl-a))))
    (let ((env2 (cl-cc/type::dict-env-extend 'eq type-int methods env)))
      (let ((found (cl-cc/type::dict-env-lookup 'eq type-int env2)))
        (assert-equal methods found)))))

(deftest dict-env-lookup-missing
  "dict-env-lookup returns nil for unregistered pair."
  (let ((env (type-env-empty)))
    (assert-null (cl-cc/type::dict-env-lookup 'eq type-int env))))

;;; ─── Backward-compat: type-class struct ────────────────────────────────────

(deftest compat-type-class-struct
  "type-class backward-compat struct works."
  (let ((tc (make-type-class :name 'show
                             :type-param (fresh-type-var "a")
                             :methods '((show-method . nil)))))
    (assert-true (type-class-p tc))
    (assert-eq 'show (type-class-name tc))
    (assert-true (type-var-p (cl-cc/type::type-class-type-param tc)))
    (assert-equal 1 (length (cl-cc/type::type-class-methods tc)))))

(deftest compat-type-class-in-registry
  "type-class struct can be stored in typeclass registry."
  (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq)))
    (let ((tc (make-type-class :name 'test-compat :type-param nil :methods nil)))
      (register-typeclass 'test-compat tc)
      (assert-eq tc (lookup-typeclass 'test-compat)))))

;;; ─── Backward-compat: type-skolem ──────────────────────────────────────────

(deftest compat-type-skolem-creation
  "make-type-skolem creates skolem with unique ID."
  (let ((s1 (cl-cc/type::make-type-skolem "x"))
        (s2 (cl-cc/type::make-type-skolem "y")))
    (assert-true (cl-cc/type::type-skolem-p s1))
    (assert-true (cl-cc/type::type-skolem-p s2))
    (assert-false (= (cl-cc/type::type-skolem-id s1) (cl-cc/type::type-skolem-id s2)))))

(deftest compat-type-skolem-equality
  "type-skolem-equal-p checks ID equality."
  (let ((s1 (cl-cc/type::make-type-skolem)))
    (assert-true (cl-cc/type::type-skolem-equal-p s1 s1))
    (assert-false (cl-cc/type::type-skolem-equal-p s1 (cl-cc/type::make-type-skolem)))))

;;; ─── Backward-compat: type-effect ──────────────────────────────────────────

(deftest compat-type-effect-creation
  "make-type-effect creates backward-compat effect node."
  (let ((e (cl-cc/type::make-type-effect :name 'io)))
    (assert-true (cl-cc/type::type-effect-p e))
    (assert-eq 'io (cl-cc/type::type-effect-name e))))

(deftest compat-type-effect-name-polymorphic
  "type-effect-name works on both type-effect and type-effect-op."
  (let ((old (cl-cc/type::make-type-effect :name 'io))
        (new (make-type-effect-op :name 'state :args nil)))
    (assert-eq 'io (cl-cc/type::type-effect-name old))
    (assert-eq 'state (cl-cc/type::type-effect-name new))))

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

(deftest compat-type-forall-type-alias
  "type-forall-type is an alias for type-forall-body."
  (let* ((v (fresh-type-var "a"))
         (f (make-type-forall :var v :body type-int)))
    (assert-true (type-equal-p type-int (cl-cc/type::type-forall-type f)))))

(deftest compat-type-qualified-type-alias
  "type-qualified-type is an alias for type-qualified-body."
  (let ((q (make-type-qualified :constraints nil :body type-int)))
    (assert-true (type-equal-p type-int (cl-cc/type::type-qualified-type q)))))
