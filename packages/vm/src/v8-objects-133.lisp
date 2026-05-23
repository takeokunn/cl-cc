;;;; packages/vm/src/v8-objects-133.lisp — Phase 133: V8-Style Object Optimization
;;;; FR-740 Hidden Classes/Object Shapes, FR-741 Inline Method Dispatch Tables,
;;;; FR-742 Object Header Layout, FR-743 Value Types/Inline Structs

(in-package :cl-cc/vm)

;;; ──── FR-740: Hidden Classes / Object Shapes ────
(defstruct object-shape
  "A Shape (Hidden Class) shared by all instances with the same slot set.
V8 Hidden Classes / SpiderMonkey Shapes equivalent."
  (name nil :type symbol)
  (slot-offsets (make-hash-table :test #'eq))
  (transition-table (make-hash-table :test #'equal))
  (parent-shape nil))

(defvar *shape-registry* (make-hash-table :test #'eq)
  "Global shape registry: shape-name → object-shape.")

(defun find-or-create-shape (class-name slot-names)
  "Find or create an object shape for CLASS-NAME with SLOT-NAMES."
  (or (gethash class-name *shape-registry*)
      (let ((shape (make-object-shape :name class-name)))
        (loop for slot in slot-names
              for i from 0
              do (setf (gethash slot (object-shape-slot-offsets shape)) i))
        (setf (gethash class-name *shape-registry*) shape)
        shape)))

(defun shape-slot-offset (shape slot-name)
  "Get the offset index for SLOT-NAME in SHAPE."
  (gethash slot-name (object-shape-slot-offsets shape)))

;;; ──── FR-741: Inline Method Dispatch Tables / IMT ────
(defconstant +imt-size+ 64
  "Fixed size of Inline Method Table (64 entries).")

(defstruct imt-entry
  "An entry in the Inline Method Dispatch Table."
  (selector nil :type symbol)
  (implementation nil :type (or null function))
  (conflict-chain nil :type list))

(defstruct inline-method-table
  "Per-class Inline Method Table for O(1) generic function dispatch."
  (entries (make-array +imt-size+ :initial-element nil)))

(defun imt-insert (imt selector impl)
  "Insert SELECTOR→IMPL into IMT. Handle hash collisions with conflict stubs."
  (let* ((hash (mod (sxhash selector) +imt-size+))
         (entry (aref (inline-method-table-entries imt) hash)))
    (if (or (null entry) (eq (imt-entry-selector entry) selector))
        (setf (aref (inline-method-table-entries imt) hash)
              (make-imt-entry :selector selector :implementation impl))
        ;; Conflict: chain
        (push (make-imt-entry :selector selector :implementation impl)
              (imt-entry-conflict-chain entry)))))

(defun imt-lookup (imt selector)
  "Look up SELECTOR in IMT. Returns implementation or NIL."
  (let* ((hash (mod (sxhash selector) +imt-size+))
         (entry (aref (inline-method-table-entries imt) hash)))
    (cond ((null entry) nil)
          ((eq (imt-entry-selector entry) selector)
           (imt-entry-implementation entry))
          ((imt-entry-conflict-chain entry)
           (let ((found (find selector (imt-entry-conflict-chain entry)
                             :test #'eq :key #'imt-entry-selector)))
             (when found
               (imt-entry-implementation found)))))))

;;; ──── FR-742: Object Header Layout Optimization ────
(defconstant +mark-word-gc-color-bits+ 2
  "Bits [0:1] of mark word: GC color (white=0, gray=1, black=2).")
(defconstant +mark-word-kind-bits+ 2
  "Bits [2:3] of mark word: object kind.")
(defconstant +mark-word-hash-bits+ 28
  "Bits [4:31] of mark word: identity hash code (lazy computation).")
(defconstant +mark-word-lock-bits+ 32
  "Bits [32:63] of mark word: lock state / class index.")

(defun make-mark-word (&key (gc-color 0) (object-kind 0) (identity-hash 0) (lock-state 0))
  "Create a 64-bit mark word with specified fields."
  (logior gc-color
          (ash object-kind +mark-word-gc-color-bits+)
          (ash identity-hash (+ +mark-word-gc-color-bits+ +mark-word-kind-bits+))
          (ash lock-state (+ +mark-word-gc-color-bits+ +mark-word-kind-bits+ +mark-word-hash-bits+))))

(defun mark-word-gc-color (mw)
  "Extract GC color from mark word MW."
  (logand mw (1- (ash 1 +mark-word-gc-color-bits+))))

;;; ──── FR-743: Value Types / Inline Structs ────
(defstruct value-type-def
  "Definition of a value type (inline struct) for stack/register storage."
  (name nil :type symbol)
  (fields nil :type list)
  (total-size 0 :type fixnum))

(defvar *value-types* (make-hash-table :test #'eq)
  "Registered value type definitions.")

(defmacro defvalue-type (name &rest fields)
  "Define a value type NAME with FIELDS (stack/register inline composite value).
Example: (defvalue-type point2d (x :type single-float) (y :type single-float))"
  `(progn
     (setf (gethash ',name *value-types*)
           (make-value-type-def :name ',name :fields ',fields
                                :total-size ,(length fields)))
     ',name))

;; ── Exports ──
(export '(object-shape make-object-shape *shape-registry*
          find-or-create-shape shape-slot-offset
          +imt-size+ imt-entry make-imt-entry
          inline-method-table make-inline-method-table imt-insert imt-lookup
          make-mark-word mark-word-gc-color
          defvalue-type value-type-def *value-types*))
