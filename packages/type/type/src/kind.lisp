;;;; kind.lisp - Kind System
;;;;
;;;; Kinds are the "types of types". Every type node carries a kind:
;;;;   Int :: *,  List :: * -> *,  Map :: * -> * -> *
;;;;
;;;; kind-node (base)
;;;;   kind-type        ; * — ordinary types
;;;;   kind-arrow       ; κ₁ -> κ₂ — type constructors
;;;;   kind-effect      ; Effect — effect labels
;;;;   kind-row         ; Row κ — row types
;;;;   kind-constraint  ; Constraint — typeclass constraints
;;;;   kind-multiplicity; Multiplicity — {0,1,ω} grades
;;;;   kind-var         ; κ variable — for kind inference

(in-package :cl-cc/type)

;;; ─── Base ─────────────────────────────────────────────────────────────────

(defstruct kind-node
  "Base struct for all kind representations."
  (source-location nil))

;;; ─── kind-type : * ────────────────────────────────────────────────────────

(defstruct (kind-type (:include kind-node))
  "The kind * — ordinary fully-applied types: Int, String, Bool.")

(defvar +kind-type+ (make-kind-type)
  "Singleton * kind.")

;;; ─── kind-arrow : κ₁ -> κ₂ ───────────────────────────────────────────────

(defstruct (kind-arrow (:include kind-node))
  "Higher-kinded arrow. List :: * -> *, Map :: * -> * -> *"
  (from nil)
  (to   nil))

(defun kind-fun (from to)
  "Build a kind arrow FROM -> TO."
  (make-kind-arrow :from from :to to))

;;; ─── kind-effect : Effect ─────────────────────────────────────────────────

(defstruct (kind-effect (:include kind-node))
  "The Effect kind — individual effect labels like IO, State.")

(defvar +kind-effect+ (make-kind-effect)
  "Singleton Effect kind.")

;;; ─── kind-row : Row κ ─────────────────────────────────────────────────────

(defstruct (kind-row (:include kind-node))
  "Row κ — the kind of row types. Row * for records, Row Effect for effect rows."
  (elem nil))

(defvar +kind-row-type+   (make-kind-row :elem (make-kind-type))   "Row *")
(defvar +kind-row-effect+ (make-kind-row :elem (make-kind-effect)) "Row Effect")

;;; ─── kind-constraint : Constraint ────────────────────────────────────────

(defstruct (kind-constraint (:include kind-node))
  "The Constraint kind — (Eq a), (Num a) etc.")

(defvar +kind-constraint+ (make-kind-constraint)
  "Singleton Constraint kind.")

;;; ─── kind-multiplicity : Multiplicity ────────────────────────────────────

(defstruct (kind-multiplicity (:include kind-node))
  "The Multiplicity kind — classifies usage grades {0, 1, ω}.")

(defvar +kind-multiplicity+ (make-kind-multiplicity)
  "Singleton Multiplicity kind.")

;;; ─── kind-var : kind variable ─────────────────────────────────────────────

(defvar *kind-var-counter* 0)

(defstruct (kind-var (:include kind-node) (:constructor %make-kind-var))
  "A kind variable for kind inference."
  (id   0   :type fixnum)
  (name nil))

(defun fresh-kind-var (&optional name)
  "Return a fresh kind variable."
  (incf *kind-var-counter*)
  (%make-kind-var :id *kind-var-counter* :name name))

(defun kind-var-equal-p (kv1 kv2)
  (and (kind-var-p kv1) (kind-var-p kv2)
       (= (kind-var-id kv1) (kind-var-id kv2))))

;;; ─── Equality ─────────────────────────────────────────────────────────────

(defun kind-equal-p (k1 k2)
  "Structural equality of two kinds."
  (cond
    ((and (kind-type-p k1)         (kind-type-p k2))         t)
    ((and (kind-effect-p k1)       (kind-effect-p k2))       t)
    ((and (kind-constraint-p k1)   (kind-constraint-p k2))   t)
    ((and (kind-multiplicity-p k1) (kind-multiplicity-p k2)) t)
    ((and (kind-arrow-p k1) (kind-arrow-p k2))
     (and (kind-equal-p (kind-arrow-from k1) (kind-arrow-from k2))
          (kind-equal-p (kind-arrow-to   k1) (kind-arrow-to   k2))))
    ((and (kind-row-p k1) (kind-row-p k2))
     (kind-equal-p (kind-row-elem k1) (kind-row-elem k2)))
    ((and (kind-var-p k1) (kind-var-p k2))
     (kind-var-equal-p k1 k2))
    (t nil)))

;;; ─── Pretty-printing ──────────────────────────────────────────────────────

(defun kind-to-string (k)
  "Convert a kind to a human-readable string."
  (cond
    ((kind-type-p k)         "*")
    ((kind-effect-p k)       "Effect")
    ((kind-constraint-p k)   "Constraint")
    ((kind-multiplicity-p k) "Multiplicity")
    ((kind-arrow-p k)
     (let ((fs (kind-to-string (kind-arrow-from k)))
           (ts (kind-to-string (kind-arrow-to   k))))
       (if (kind-arrow-p (kind-arrow-from k))
           (format nil "(~A) -> ~A" fs ts)
           (format nil "~A -> ~A"   fs ts))))
    ((kind-row-p k)
     (format nil "Row ~A" (kind-to-string (kind-row-elem k))))
    ((kind-var-p k)
     (if (kind-var-name k)
         (format nil "k~A" (kind-var-name k))
         (format nil "k~D" (kind-var-id   k))))
    (t "?")))
