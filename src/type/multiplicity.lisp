;;;; multiplicity.lisp - Graded Modal Types (Linearity Grades)
;;;;
;;;; Graded modalities à la Granule / QTT (Quantitative Type Theory):
;;;;   0 — erased:      never used at runtime (proof-irrelevant)
;;;;   1 — linear:      used exactly once     (Rust move semantics)
;;;;   ω — unrestricted: used any number of times (default CL behaviour)
;;;;
;;;; Semiring laws:
;;;;   0 + q = q,  q + 0 = q,  q + q = q   (join)
;;;;   0 * q = 0,  1 * q = q,  ω * 1 = ω,  ω * ω = ω  (scale)
;;;;
;;;; Arrow types carry a multiplicity: A -q-> B
;;;;   (->1 A B)  — function must be called exactly once
;;;;   (->0 A B)  — function is irrelevant (erased at runtime)
;;;;   (-> A B)   — ordinary unrestricted function (ω default)

(in-package :cl-cc/type)

;;; ─── Multiplicity values ──────────────────────────────────────────────────

(deftype multiplicity () '(member :zero :one :omega))

(defconstant +mult-zero+  :zero  "Usage grade 0 — erased.")
(defconstant +mult-one+   :one   "Usage grade 1 — linear (used exactly once).")
(defconstant +mult-omega+ :omega "Usage grade ω — unrestricted.")

(defun multiplicity-p (x)
  "True iff X is a valid multiplicity grade."
  (member x '(:zero :one :omega)))

;;; ─── Semiring operations ──────────────────────────────────────────────────

(defun mult-add (q1 q2)
  "Semiring addition (join): q1 + q2.
  Represents the combined usage when a resource is used in EITHER branch."
  (cond
    ((eq q1 :zero) q2)
    ((eq q2 :zero) q1)
    ((eq q1 q2)    q1)     ; 1+1=1 (or ω+ω=ω) under join semantics
    (t :omega)))           ; 1+ω = ω

(defun mult-mul (q1 q2)
  "Semiring multiplication (scale): q1 * q2.
  Represents the usage when a resource is used q2 times inside a context used q1 times."
  (cond
    ((eq q1 :zero)  :zero)
    ((eq q2 :zero)  :zero)
    ((eq q1 :one)   q2)
    ((eq q2 :one)   q1)
    (t :omega)))            ; ω * ω = ω

(defun mult-leq (q1 q2)
  "Multiplicity ordering: q1 ≤ q2.
  0 ≤ 1 ≤ ω — more permissive grades are larger."
  (or (eq q1 q2)
      (eq q1 :zero)
      (eq q2 :omega)))

;;; ─── Pretty-printing ──────────────────────────────────────────────────────

(defparameter *mult-to-string-table*
  '((:zero . "0") (:one . "1") (:omega . "ω"))
  "Alist mapping multiplicity keyword to its human-readable string.")

(defun mult-to-string (q)
  "Human-readable grade."
  (or (cdr (assoc q *mult-to-string-table*))
      (error "Invalid multiplicity: ~S" q)))
