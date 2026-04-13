;;;; tests/unit/type/row-tests.lisp — Row Polymorphism Tests
;;;;
;;;; Tests for src/type/row.lisp:
;;;; row-extend, row-restrict, row-select, row-labels,
;;;; row-closed-p, row-open-p,
;;;; effect-row-extend, effect-row-restrict, effect-row-member-p.

(in-package :cl-cc/test)

(defsuite row-suite :description "Row polymorphism operation tests"
  :parent cl-cc-suite)


(in-suite row-suite)
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

(deftest row-extend-behavior
  "row-extend: adds field (2 fields); preserves row-var; shadows existing label."
  (let* ((r (make-closed-record :x type-int))
         (r2 (row-extend :y type-string r)))
    (assert-true (type-record-p r2))
    (assert-equal 2 (length (type-record-fields r2))))
  (let* ((rv (fresh-type-var))
         (r2 (row-extend :y type-string (make-open-record rv :x type-int))))
    (assert-true (type-variable-p (type-record-row-var r2))))
  (let* ((r (make-closed-record :x type-int))
         (r2 (row-extend :x type-string r)))
    (assert-equal 2 (length (type-record-fields r2)))
    (assert-true (type-equal-p type-string (row-select :x r2)))))

;;; ─── row-restrict ────────────────────────────────────────────────────────────

(deftest row-restrict-behavior
  "row-restrict: removes present label; absent label is no-op; preserves row-var."
  (let* ((r (make-closed-record :x type-int :y type-string))
         (r2 (row-restrict :x r)))
    (assert-equal 1 (length (type-record-fields r2)))
    (assert-null (row-select :x r2))
    (assert-true (type-equal-p type-string (row-select :y r2))))
  (assert-equal 1 (length (type-record-fields
                             (row-restrict :z (make-closed-record :x type-int)))))
  (let* ((rv (fresh-type-var))
         (r2 (row-restrict :x (make-open-record rv :x type-int))))
    (assert-true (type-variable-p (type-record-row-var r2)))))

;;; ─── row-select ──────────────────────────────────────────────────────────────

(deftest row-select-behavior
  "row-select: returns type for present label; nil for absent; works on type-variant."
  (let ((r (make-closed-record :x type-int :y type-string)))
    (assert-true (type-equal-p type-int (row-select :x r)))
    (assert-true (type-equal-p type-string (row-select :y r))))
  (assert-null (row-select :z (make-closed-record :x type-int)))
  (let ((v (make-type-variant :cases (list (cons :some type-int) (cons :none type-null))
                              :row-var nil)))
    (assert-true (type-equal-p type-int  (row-select :some v)))
    (assert-true (type-equal-p type-null (row-select :none v)))))

;;; ─── row-labels ──────────────────────────────────────────────────────────────

(deftest row-labels-behavior
  "row-labels: returns label list for record; for variant; nil for empty record."
  (assert-equal '(:x :y) (row-labels (make-closed-record :x type-int :y type-string)))
  (assert-equal '(:a :b) (row-labels (make-type-variant :cases (list (cons :a type-int)
                                                                      (cons :b type-string))
                                                         :row-var nil)))
  (assert-null (row-labels (make-closed-record))))

;;; ─── row-closed-p / row-open-p ──────────────────────────────────────────────

(deftest row-closed-open-predicate-behavior
  "Closed record: closed-p=true, open-p=false. Open record: opposite. Same for variants."
  (let ((r (make-closed-record :x type-int)))
    (assert-true  (row-closed-p r))
    (assert-false (row-open-p r)))
  (let ((r (make-open-record (fresh-type-var) :x type-int)))
    (assert-true  (row-open-p r))
    (assert-false (row-closed-p r)))
  (assert-true (row-closed-p (make-type-variant :cases nil :row-var nil)))
  (assert-true (row-open-p   (make-type-variant :cases nil :row-var (fresh-type-var)))))

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

(deftest effect-row-member-p-behavior
  "effect-row-member-p: true for present effect; false for absent."
  (let ((row-with-io (make-type-effect-row :effects (list (cl-cc/type::make-type-effect-op :name :io :args nil))
                                            :row-var nil))
        (empty-row   (make-type-effect-row :effects nil :row-var nil)))
    (assert-true  (cl-cc/type::effect-row-member-p :io row-with-io))
    (assert-false (cl-cc/type::effect-row-member-p :io empty-row))))
