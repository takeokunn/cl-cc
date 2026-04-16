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

(deftest effect-row-extend
  "effect-row-extend prepends an op and preserves the original row variable."
  (let* ((op  (cl-cc/type:make-type-effect-op :name 'state :args nil))
         (row (cl-cc/type:effect-row-extend op cl-cc/type:+pure-effect-row+)))
    (assert-true (cl-cc/type:type-effect-row-p row))
    (assert-equal 1 (length (cl-cc/type:type-effect-row-effects row)))
    (assert-eq 'state (cl-cc/type:type-effect-op-name
                       (first (cl-cc/type:type-effect-row-effects row)))))
  (let* ((rv   (cl-cc/type:fresh-type-var "e"))
         (base (cl-cc/type:make-type-effect-row :effects nil :row-var rv))
         (op   (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (ext  (cl-cc/type:effect-row-extend op base)))
    (assert-true (cl-cc/type:type-var-p (cl-cc/type:type-effect-row-row-var ext)))))

(deftest effect-row-restrict
  "effect-row-restrict removes named effects; absent names are a no-op."
  (let* ((op1 (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (op2 (cl-cc/type:make-type-effect-op :name 'state :args nil))
         (row (cl-cc/type:make-type-effect-row :effects (list op1 op2) :row-var nil))
         (restricted (cl-cc/type:effect-row-restrict 'io row)))
    (assert-equal 1 (length (cl-cc/type:type-effect-row-effects restricted)))
    (assert-eq 'state (cl-cc/type:type-effect-op-name
                       (first (cl-cc/type:type-effect-row-effects restricted)))))
  (let* ((op  (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (row (cl-cc/type:make-type-effect-row :effects (list op) :row-var nil))
         (restricted (cl-cc/type:effect-row-restrict 'state row)))
    (assert-equal 1 (length (cl-cc/type:type-effect-row-effects restricted)))))

(deftest effect-row-member-p
  "effect-row-member-p: true when present, nil when absent, nil for pure row."
  (let* ((op  (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (row (cl-cc/type:make-type-effect-row :effects (list op) :row-var nil)))
    (assert-true  (cl-cc/type:effect-row-member-p 'io row))
    (assert-false (cl-cc/type:effect-row-member-p 'state row))
    (assert-false (cl-cc/type:effect-row-member-p 'io cl-cc/type:+pure-effect-row+))))

(deftest effect-registry
  "register-effect stores; lookup-effect retrieves or returns nil for misses."
  (let ((cl-cc/type:*effect-registry* (make-hash-table :test #'eq)))
    (let ((edef (cl-cc/type:make-effect-def :name 'state :type-params nil :operations nil)))
      (cl-cc/type:register-effect 'state edef)
      (let ((found (cl-cc/type:lookup-effect 'state)))
        (assert-true (cl-cc/type:effect-def-p found))
        (assert-eq 'state (cl-cc/type:effect-def-name found))))
    (assert-null (cl-cc/type:lookup-effect 'nonexistent))))

(deftest effect-row-union
  "effect-row-union: deduplicates effects and prefers row2's row-var."
  (let* ((op-io    (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (op-state (cl-cc/type:make-type-effect-op :name 'state :args nil))
         (op-io2   (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (row1 (cl-cc/type:make-type-effect-row :effects (list op-io) :row-var nil))
         (row2 (cl-cc/type:make-type-effect-row :effects (list op-io2 op-state) :row-var nil))
         (merged (cl-cc/type:effect-row-union row1 row2)))
    (assert-equal 2 (length (cl-cc/type:type-effect-row-effects merged))))
  (let* ((rv (cl-cc/type:fresh-type-var "e"))
         (row1 (cl-cc/type:make-type-effect-row :effects nil :row-var nil))
         (row2 (cl-cc/type:make-type-effect-row :effects nil :row-var rv))
         (merged (cl-cc/type:effect-row-union row1 row2)))
    (assert-true (cl-cc/type:type-var-p (cl-cc/type:type-effect-row-row-var merged)))))

(deftest effect-row-subset-p
  "effect-row-subset-p: empty ⊆ anything; open row is superset of everything; extra effects break subset."
  (let* ((op  (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (row (cl-cc/type:make-type-effect-row :effects (list op) :row-var nil)))
    (assert-true (cl-cc/type:effect-row-subset-p cl-cc/type:+pure-effect-row+ row)))
  (let* ((rv   (cl-cc/type:fresh-type-var "e"))
         (op   (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (row1 (cl-cc/type:make-type-effect-row :effects (list op) :row-var nil))
         (row2 (cl-cc/type:make-type-effect-row :effects nil :row-var rv)))
    (assert-true (cl-cc/type:effect-row-subset-p row1 row2)))
  (let* ((op-io    (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (op-state (cl-cc/type:make-type-effect-op :name 'state :args nil))
         (big   (cl-cc/type:make-type-effect-row :effects (list op-io op-state) :row-var nil))
         (small (cl-cc/type:make-type-effect-row :effects (list op-io) :row-var nil)))
    (assert-false (cl-cc/type:effect-row-subset-p big small))))
