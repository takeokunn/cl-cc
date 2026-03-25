;;;; tests/unit/type/row-tests.lisp — Row Polymorphism Tests
;;;;
;;;; Tests for src/type/row.lisp:
;;;; row-extend, row-restrict, row-select, row-labels,
;;;; row-closed-p, row-open-p,
;;;; effect-row-extend, effect-row-restrict, effect-row-member-p.

(in-package :cl-cc/test)

(defsuite row-suite :description "Row polymorphism operation tests")

;;; ─── Helpers ─────────────────────────────────────────────────────────────────

(defun make-closed-record (&rest label-type-pairs)
  "Build a closed type-record from alternating label type pairs."
  (make-type-record
   :fields (loop for (l ty) on label-type-pairs by #'cddr
                 collect (cons l ty))
   :row-var nil))

(defun make-open-record (row-var &rest label-type-pairs)
  "Build an open type-record with ROW-VAR."
  (make-type-record
   :fields (loop for (l ty) on label-type-pairs by #'cddr
                 collect (cons l ty))
   :row-var row-var))

;;; ─── row-extend ──────────────────────────────────────────────────────────────

(deftest row-extend-adds-field
  "row-extend prepends a new label:type pair."
  (let* ((r (make-closed-record :x type-int))
         (r2 (row-extend :y type-string r)))
    (assert-true (type-record-p r2))
    (assert-equal 2 (length (type-record-fields r2)))))

(deftest row-extend-preserves-row-var
  "row-extend preserves the row variable."
  (let* ((rv (fresh-type-var))
         (r (make-open-record rv :x type-int))
         (r2 (row-extend :y type-string r)))
    (assert-true (type-variable-p (type-record-row-var r2)))))

(deftest row-extend-shadows-existing
  "row-extend with existing label shadows (doesn't remove) old binding."
  (let* ((r (make-closed-record :x type-int))
         (r2 (row-extend :x type-string r)))
    ;; Two entries for :x — the new one shadows
    (assert-equal 2 (length (type-record-fields r2)))
    ;; row-select returns the first (new) binding
    (assert-true (type-equal-p type-string (row-select :x r2)))))

;;; ─── row-restrict ────────────────────────────────────────────────────────────

(deftest row-restrict-removes-label
  "row-restrict removes a label from the record."
  (let* ((r (make-closed-record :x type-int :y type-string))
         (r2 (row-restrict :x r)))
    (assert-equal 1 (length (type-record-fields r2)))
    (assert-null (row-select :x r2))
    (assert-true (type-equal-p type-string (row-select :y r2)))))

(deftest row-restrict-nonexistent-is-noop
  "row-restrict on absent label returns same fields."
  (let* ((r (make-closed-record :x type-int))
         (r2 (row-restrict :z r)))
    (assert-equal 1 (length (type-record-fields r2)))))

(deftest row-restrict-preserves-row-var
  "row-restrict preserves the row variable."
  (let* ((rv (fresh-type-var))
         (r (make-open-record rv :x type-int))
         (r2 (row-restrict :x r)))
    (assert-true (type-variable-p (type-record-row-var r2)))))

;;; ─── row-select ──────────────────────────────────────────────────────────────

(deftest row-select-found
  "row-select returns the type for a present label."
  (let ((r (make-closed-record :x type-int :y type-string)))
    (assert-true (type-equal-p type-int (row-select :x r)))
    (assert-true (type-equal-p type-string (row-select :y r)))))

(deftest row-select-absent
  "row-select returns nil for an absent label."
  (let ((r (make-closed-record :x type-int)))
    (assert-null (row-select :z r))))

(deftest row-select-variant
  "row-select works on type-variant too."
  (let ((v (make-type-variant :cases (list (cons :some type-int)
                                           (cons :none type-null))
                              :row-var nil)))
    (assert-true (type-equal-p type-int (row-select :some v)))
    (assert-true (type-equal-p type-null (row-select :none v)))))

;;; ─── row-labels ──────────────────────────────────────────────────────────────

(deftest row-labels-record
  "row-labels returns all label names from a record."
  (let ((r (make-closed-record :x type-int :y type-string)))
    (assert-equal '(:x :y) (row-labels r))))

(deftest row-labels-variant
  "row-labels returns all label names from a variant."
  (let ((v (make-type-variant :cases (list (cons :a type-int)
                                           (cons :b type-string))
                              :row-var nil)))
    (assert-equal '(:a :b) (row-labels v))))

(deftest row-labels-empty
  "row-labels on empty record returns nil."
  (let ((r (make-closed-record)))
    (assert-null (row-labels r))))

;;; ─── row-closed-p / row-open-p ──────────────────────────────────────────────

(deftest row-closed-record
  "Closed record has no row-var."
  (let ((r (make-closed-record :x type-int)))
    (assert-true (row-closed-p r))
    (assert-false (row-open-p r))))

(deftest row-open-record
  "Open record has a row-var."
  (let ((r (make-open-record (fresh-type-var) :x type-int)))
    (assert-true (row-open-p r))
    (assert-false (row-closed-p r))))

(deftest row-closed-variant
  "Closed variant (row-var nil) is closed."
  (let ((v (make-type-variant :cases nil :row-var nil)))
    (assert-true (row-closed-p v))))

(deftest row-open-variant
  "Open variant (row-var non-nil) is open."
  (let ((v (make-type-variant :cases nil :row-var (fresh-type-var))))
    (assert-true (row-open-p v))))

;;; ─── effect-row operations ──────────────────────────────────────────────────

(deftest effect-row-extend-adds-op
  "effect-row-extend prepends an effect-op."
  (let* ((op (cl-cc/type::make-type-effect-op :name :io :args nil))
         (row (make-type-effect-row :effects nil :row-var nil))
         (row2 (cl-cc/type::effect-row-extend op row)))
    (assert-equal 1 (length (type-effect-row-effects row2)))))

(deftest effect-row-restrict-removes
  "effect-row-restrict removes by name."
  (let* ((op1 (cl-cc/type::make-type-effect-op :name :io :args nil))
         (op2 (cl-cc/type::make-type-effect-op :name :state :args nil))
         (row (make-type-effect-row :effects (list op1 op2) :row-var nil))
         (row2 (cl-cc/type::effect-row-restrict :io row)))
    (assert-equal 1 (length (type-effect-row-effects row2)))))

(deftest effect-row-member-present
  "effect-row-member-p returns true for present effect."
  (let* ((op (cl-cc/type::make-type-effect-op :name :io :args nil))
         (row (make-type-effect-row :effects (list op) :row-var nil)))
    (assert-true (cl-cc/type::effect-row-member-p :io row))))

(deftest effect-row-member-absent
  "effect-row-member-p returns nil for absent effect."
  (let* ((row (make-type-effect-row :effects nil :row-var nil)))
    (assert-false (cl-cc/type::effect-row-member-p :io row))))
