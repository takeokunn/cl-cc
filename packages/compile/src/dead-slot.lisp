;;;; packages/compile/src/dead-slot.lisp — FR-567 Dead Slot Elimination
;;;; Remove unused CLOS slots via whole-program analysis.
;;;; LTO-mode: scan all slot-value references, eliminate never-read slots.

(in-package :cl-cc/compile)

;;; ──── Slot usage tracking ────
(defvar *slot-read-table* (make-hash-table :test #'eq)
  "Class → hash-set of slot names that are read anywhere in the program.")

(defvar *slot-written-table* (make-hash-table :test #'eq)
  "Class → hash-set of slot names that are written anywhere in the program.")

(defvar *slot-keep-table* (make-hash-table :test #'eq)
  "Class → hash-set of slot names marked with KEEP-SLOT declaration.")

;;; ──── Analysis ────
(defun record-slot-read (class-name slot-name)
  "Record that SLOT-NAME of CLASS-NAME is read."
  (let ((set (or (gethash class-name *slot-read-table*)
                 (setf (gethash class-name *slot-read-table*)
                       (make-hash-table :test #'eq)))))
    (setf (gethash slot-name set) t)))

(defun record-slot-write (class-name slot-name)
  "Record that SLOT-NAME of CLASS-NAME is written."
  (let ((set (or (gethash class-name *slot-written-table*)
                 (setf (gethash class-name *slot-written-table*)
                       (make-hash-table :test #'eq)))))
    (setf (gethash slot-name set) t)))

(defun slot-is-dead-p (class-name slot-name)
  "Return T if SLOT-NAME of CLASS-NAME is dead (never read AND not kept).
A slot is dead if:
1. It is never read (via slot-value/slot-boundp)
2. It is not marked with (declare (cl-cc:keep-slot ...))
Note: written-only slots are also dead (we remove them)."
  (let ((reads (gethash class-name *slot-read-table*))
        (writes (gethash class-name *slot-written-table*))
        (kept (gethash class-name *slot-keep-table*)))
    (and (or (not reads) (not (gethash slot-name reads)))
         (or (not kept) (not (gethash slot-name kept))))))

;;; ──── Slot analysis pass ────
(defun analyze-slot-usage (ast)
  "Walk AST and record all slot-value/slot-boundp/setf-slot-value usage.
Returns (values read-table write-table)."
  (labels ((walk (node)
             (typecase node
               (cons
                (case (car node)
                  ((slot-value slot-boundp)
                   (when (and (cdr node) (cddr node))
                     (let ((class (second node))
                           (slot (third node)))
                       (when (symbolp class)
                         (record-slot-read class (if (quote-form-p slot)
                                                     (second slot)
                                                     slot))))))
                  (setf
                   (let ((place (second node)))
                     (when (and (consp place)
                                (eq (car place) 'slot-value))
                       (let ((class (second place))
                             (slot (third place)))
                         (when (symbolp class)
                           (record-slot-write class (if (quote-form-p slot)
                                                        (second slot)
                                                        slot)))))))))
               (t nil))
             ;; Recurse into sub-forms
             (when (consp node)
               (dolist (child (cdr node))
                 (walk child)))))
    (walk ast))
  (values *slot-read-table* *slot-written-table*))

;;; ──── Dead slot removal ────
(defun remove-dead-slots (class-name slots)
  "Remove dead slots from SLOTS list for CLASS-NAME.
Returns the filtered slot list."
  (remove-if (lambda (slot)
               (let ((slot-name (if (consp slot) (car slot) slot)))
                 (slot-is-dead-p class-name slot-name)))
             slots))

;;; ──── Keep slot declaration ────
(defun mark-keep-slot (class-name slot-name)
  "Mark SLOT-NAME of CLASS-NAME as kept (must not be eliminated).
Usage: (declare (cl-cc:keep-slot x)) in defclass body."
  (let ((set (or (gethash class-name *slot-keep-table*)
                 (setf (gethash class-name *slot-keep-table*)
                       (make-hash-table :test #'eq)))))
    (setf (gethash slot-name set) t)))

;;; ──── Integration with LTO ────
(defun dead-slot-elimination-pass (program-ast)
  "Whole-program dead slot elimination pass.
Runs in LTO mode: analyzes all slot usage across the entire program,
then removes slots that are never read."
  (analyze-slot-usage program-ast)
  ;; Remove dead slots from class definitions
  ;; (actual removal happens in codegen when laying out objects)
  (values))

;;; ──── Helper ────
(defun quote-form-p (form)
  "Return T if FORM is (quote ...) or 'symbol."
  (and (consp form) (eq (car form) 'quote)))
