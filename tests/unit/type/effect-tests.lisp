;;;; tests/unit/type/effect-tests.lisp — Algebraic Effect System Tests
;;;;
;;;; Tests for src/type/effect.lisp:
;;;; effect-def, effect registry, effect-row-union, effect-row-subset-p.

(in-package :cl-cc/test)

(defsuite effect-suite :description "Algebraic effect system tests"
  :parent cl-cc-unit-suite)


(in-suite effect-suite)
;;; ─── Helpers ─────────────────────────────────────────────────────────────────

(defun make-effect-op (name)
  "Shorthand for a simple effect-op with no args."
  (cl-cc/type::make-type-effect-op :name name :args nil))

(defun make-effect-row (&rest names)
  "Build a closed effect row from effect name symbols."
  (make-type-effect-row
   :effects (mapcar #'make-effect-op names)
   :row-var nil))

(defun make-open-effect-row (var &rest names)
  "Build an open effect row with a row variable."
  (make-type-effect-row
   :effects (mapcar #'make-effect-op names)
   :row-var var))

;;; ─── effect-def struct ──────────────────────────────────────────────────────

(deftest effect-def-creation
  "effect-def struct stores name and operations."
  (let ((ed (cl-cc/type::make-effect-def
             :name 'io
             :type-params nil
             :operations '((read-line . nil) (write-line . nil)))))
    (assert-eq 'io (cl-cc/type::effect-def-name ed))
    (assert-equal 2 (length (cl-cc/type::effect-def-operations ed)))))

;;; ─── effect registry ────────────────────────────────────────────────────────

(deftest effect-registry-operations
  "Effect registry: round-trip register+lookup; lookup of absent effect returns nil."
  (let ((cl-cc/type::*effect-registry* (make-hash-table :test #'eq)))
    (let ((ed (cl-cc/type::make-effect-def :name 'test-eff :type-params nil :operations nil)))
      (cl-cc/type::register-effect 'test-eff ed)
      (assert-eq ed (cl-cc/type::lookup-effect 'test-eff)))
    (assert-null (cl-cc/type::lookup-effect 'nonexistent))))

;;; ─── effect-row-union ───────────────────────────────────────────────────────

(deftest effect-row-union-behavior
  "effect-row-union: disjoint (2 effects), overlapping deduplicates (3 not 4), empty stays empty, row-var preserved."
  (assert-equal 2 (length (type-effect-row-effects
                            (cl-cc/type::effect-row-union (make-effect-row :io) (make-effect-row :state)))))
  (assert-equal 3 (length (type-effect-row-effects
                            (cl-cc/type::effect-row-union (make-effect-row :io :state) (make-effect-row :io :exn)))))
  (assert-null (type-effect-row-effects
                (cl-cc/type::effect-row-union (make-effect-row) (make-effect-row))))
  (let* ((rv (fresh-type-var))
         (result (cl-cc/type::effect-row-union (make-effect-row :io) (make-open-effect-row rv :state))))
    (assert-true (type-variable-p (type-effect-row-row-var result)))))

;;; ─── effect-row-subset-p ────────────────────────────────────────────────────

(deftest effect-row-subset-p-behavior
  "effect-row-subset-p: empty⊆any, self⊆self, {io}⊆{io,state}, {io,state}⊄{io}, anything⊆open-row."
  (assert-true  (cl-cc/type::effect-row-subset-p (make-effect-row) (make-effect-row :io :state)))
  (let ((r (make-effect-row :io :state)))
    (assert-true  (cl-cc/type::effect-row-subset-p r r)))
  (assert-true  (cl-cc/type::effect-row-subset-p (make-effect-row :io) (make-effect-row :io :state)))
  (assert-false (cl-cc/type::effect-row-subset-p (make-effect-row :io :state) (make-effect-row :io)))
  (assert-true  (cl-cc/type::effect-row-subset-p (make-effect-row :io :state :exn)
                                                  (make-open-effect-row (fresh-type-var)))))
