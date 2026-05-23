;;;; packages/type/src/type-level.lisp — FR-595 Type-Level Computation
;;;; Type families, conditional types, type-level natural numbers.
;;;; Haskell TypeFamilies / TypeLits / C++ std::conditional_t equivalent.

(in-package :cl-cc/type)

;;; ──── Type families ────
(defvar *type-families* (make-hash-table :test #'eq)
  "Type family name → ((arg-pattern . result-type) ...) alist.")

(defmacro deftype-family (name (param) &rest cases)
  "Define a type-level function: maps type PARAM to result type.
Usage: (deftype-family ElementType (T) ((list a) a) ((vector a) a) (t T))"
  `(setf (gethash ',name *type-families*)
         ',cases))

(defun apply-type-family (name arg-type)
  "Apply type family NAME to ARG-TYPE.
Pattern-matches ARG-TYPE against each case and returns the result."
  (let ((family (gethash name *type-families*)))
    (unless family
      (error "Unknown type family: ~A" name))
    (loop for (pattern result) in family
          when (match-type-pattern pattern arg-type)
            return (subst-type-vars (match-type-bindings pattern arg-type) result)
          finally (error "No case in ~A matches ~A" name arg-type))))

;;; ──── Conditional types ────
(defmacro if-type (condition then-type else-type)
  "Type-level if: CONDITION must be a subtype check known at compile time.
Usage: (if-type (subtype-p T fixnum) T float)"
  (if (eval condition)
      then-type
      else-type))

;;; ──── Type-level natural numbers ────
(defstruct (type-nat (:conc-name tn-))
  "A type-level natural number (Peano-style or literal)."
  (value 0 :type integer))

(defun type-nat (n)
  "Create a type-level natural number N."
  (make-type-nat :value n))

(defun type-nat-add (a b)
  "Type-level addition."
  (make-type-nat :value (+ (tn-value a) (tn-value b))))

(defun type-nat-mul (a b)
  "Type-level multiplication."
  (make-type-nat :value (* (tn-value a) (tn-value b))))

;;; ──── Array length types ────
(defun array-length-type (n)
  "Create a fixed-length array type: (fixed-array T N).
Compile-time known length enables bounds-check elimination."
  `(fixed-array ,n))

(defun array-length (array-type)
  "Extract the length from a fixed-array type, or NIL."
  (when (and (consp array-type) (eq (car array-type) 'fixed-array))
    (second array-type)))

;;; ──── Type pattern matching ────
(defun match-type-pattern (pattern type)
  "Check if TYPE matches PATTERN.
PATTERN may contain type variables (symbols starting with lowercase)."
  (typecase pattern
    (symbol
     (or (lowercase-symbol-p pattern)  ; type variable matches anything
         (eq pattern type)))
    (cons
     (and (consp type)
          (match-type-pattern (car pattern) (car type))
          (match-type-pattern (cdr pattern) (cdr type))))
    (t (equal pattern type))))

(defun match-type-bindings (pattern type)
  "Return alist of (type-var . matched-type) from matching PATTERN against TYPE."
  (let ((bindings nil))
    (labels ((collect (p t)
               (typecase p
                 (symbol
                  (when (lowercase-symbol-p p)
                    (push (cons p t) bindings)))
                 (cons
                  (when (consp t)
                    (collect (car p) (car t))
                    (collect (cdr p) (cdr t))))))
      (collect pattern type))
    bindings))

(defun subst-type-vars (bindings type)
  "Substitute type variables in TYPE with their BINDINGS."
  (typecase type
    (symbol
     (let ((binding (assoc type bindings :test #'eq)))
       (if binding (cdr binding) type)))
    (cons
     (cons (subst-type-vars bindings (car type))
           (subst-type-vars bindings (cdr type))))
    (t type)))

;;; ──── Helpers ────
(defun lowercase-symbol-p (sym)
  "Return T if SYM's name starts with a lowercase letter (type variable)."
  (and (symbolp sym)
       (let ((name (symbol-name sym)))
         (and (> (length name) 0)
              (lower-case-p (char name 0))))))
