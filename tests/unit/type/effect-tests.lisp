;;;; tests/unit/type/effect-tests.lisp — Algebraic Effect System Tests
;;;;
;;;; Tests for src/type/effect.lisp:
;;;; effect-def, effect registry, effect-row-union, effect-row-subset-p.

(in-package :cl-cc/test)

(defsuite effect-suite :description "Algebraic effect system tests")

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

(deftest effect-registry-roundtrip
  "register-effect + lookup-effect round-trips."
  (let ((cl-cc/type::*effect-registry* (make-hash-table :test #'eq)))
    (let ((ed (cl-cc/type::make-effect-def :name 'test-eff :type-params nil :operations nil)))
      (cl-cc/type::register-effect 'test-eff ed)
      (assert-eq ed (cl-cc/type::lookup-effect 'test-eff)))))

(deftest effect-registry-missing
  "lookup-effect returns nil for unregistered effects."
  (let ((cl-cc/type::*effect-registry* (make-hash-table :test #'eq)))
    (assert-null (cl-cc/type::lookup-effect 'nonexistent))))

;;; ─── effect-row-union ───────────────────────────────────────────────────────

(deftest effect-row-union-disjoint
  "Union of disjoint rows contains all effects."
  (let* ((r1 (make-effect-row :io))
         (r2 (make-effect-row :state))
         (result (cl-cc/type::effect-row-union r1 r2)))
    (assert-equal 2 (length (type-effect-row-effects result)))))

(deftest effect-row-union-overlapping
  "Union deduplicates by name."
  (let* ((r1 (make-effect-row :io :state))
         (r2 (make-effect-row :io :exn))
         (result (cl-cc/type::effect-row-union r1 r2)))
    ;; r1 has io+state, r2 has io+exn → result has io+state+exn (3, not 4)
    (assert-equal 3 (length (type-effect-row-effects result)))))

(deftest effect-row-union-empty
  "Union of two empty rows is empty."
  (let* ((r1 (make-effect-row))
         (r2 (make-effect-row))
         (result (cl-cc/type::effect-row-union r1 r2)))
    (assert-null (type-effect-row-effects result))))

(deftest effect-row-union-preserves-row-var
  "Union preserves row-var from row2 (or row1 if row2 has none)."
  (let* ((rv (fresh-type-var))
         (r1 (make-effect-row :io))
         (r2 (make-open-effect-row rv :state))
         (result (cl-cc/type::effect-row-union r1 r2)))
    (assert-true (type-variable-p (type-effect-row-row-var result)))))

;;; ─── effect-row-subset-p ────────────────────────────────────────────────────

(deftest effect-row-subset-empty
  "Empty row is a subset of any row."
  (let ((empty (make-effect-row))
        (full (make-effect-row :io :state)))
    (assert-true (cl-cc/type::effect-row-subset-p empty full))))

(deftest effect-row-subset-self
  "A row is a subset of itself."
  (let ((r (make-effect-row :io :state)))
    (assert-true (cl-cc/type::effect-row-subset-p r r))))

(deftest effect-row-subset-proper
  "{io} ⊆ {io, state}."
  (let ((r1 (make-effect-row :io))
        (r2 (make-effect-row :io :state)))
    (assert-true (cl-cc/type::effect-row-subset-p r1 r2))))

(deftest effect-row-not-subset
  "{io, state} is NOT ⊆ {io}."
  (let ((r1 (make-effect-row :io :state))
        (r2 (make-effect-row :io)))
    (assert-false (cl-cc/type::effect-row-subset-p r1 r2))))

(deftest effect-row-subset-open-row-is-superset
  "An open row (with row-var) is a superset of everything."
  (let ((r1 (make-effect-row :io :state :exn))
        (r2 (make-open-effect-row (fresh-type-var))))
    (assert-true (cl-cc/type::effect-row-subset-p r1 r2))))
