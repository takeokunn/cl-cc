;;;; printer.lisp - Type Pretty-Printer
;;;;
;;;; Provides `type-to-string` for all type-node subtypes via defgeneric dispatch.
;;;; Each type-node class owns its printing rule: extensible without touching this file.
;;;; Architecture: defgeneric = Prolog-style clause database keyed on CLOS type hierarchy.
;;;;
;;;; Load order: after representation.lisp, typeclass.lisp.

(in-package :cl-cc/type)

;;; ─── Promote stub defun → defgeneric ─────────────────────────────────────
;;;
;;; types-env.lisp and typeclass-compat-legacy.lisp define plain-function stubs
;;; for early use during loading.  Here we promote to a generic function so that
;;; each type-node class owns its own printing clause (Prolog-style dispatch).

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Guard for upgrade from DEFUN stub (types-env.lisp) to DEFGENERIC.
  ;; ignore-errors on fdefinition makes this safe in both SBCL and the
  ;; self-hosting VM (where fdefinition bridges to SBCL's host namespace
  ;; but fboundp checks the VM's own registry, causing a mismatch).
  (when (and (fboundp 'type-to-string)
             (not (ignore-errors
                    (typep (fdefinition 'type-to-string) 'generic-function))))
    (ignore-errors (fmakunbound 'type-to-string))))

;;; ─── Primary protocol ─────────────────────────────────────────────────────
;;;
;;; type-to-string acts as a Prolog predicate: each defmethod is a clause.
;;; New type nodes add new methods rather than editing a monolithic typecase.

(defgeneric type-to-string (ty)
  (:documentation "Convert a type-node TY to a human-readable string.")
  (:method (ty)
    "Fallback: unknown type-node subclass."
    (format nil "#<type ~A>" (type-of ty))))

;;; ─── Leaf / atomic types ──────────────────────────────────────────────────

(defmethod type-to-string ((ty null))
  "NIL")

(defmethod type-to-string ((ty type-primitive))
  (symbol-name (type-primitive-name ty)))

(defmethod type-to-string ((ty type-var))
  (let ((link (type-var-link ty)))
    (cond
      (link              (type-to-string link))
      ((type-var-name ty) (format nil "?~A" (type-var-name ty)))
      (t                 (format nil "?t~D" (type-var-id ty))))))

(defmethod type-to-string ((ty type-rigid))
  (if (type-rigid-name ty)
      (format nil "sk~D[~A]" (type-rigid-id ty) (type-rigid-name ty))
      (format nil "sk~D"     (type-rigid-id ty))))

(defmethod type-to-string ((ty type-error))
  ;; type-unknown is a deftype alias over type-error with message "unknown".
  ;; PCL cannot specialise on deftype aliases, so we dispatch here.
  (if (type-unknown-p ty)
      "?"
      (format nil "<error: ~A>" (type-error-message ty))))

;;; ─── Arrow / function types ───────────────────────────────────────────────

(defparameter *arrow-mult-strings*
  '((:zero  . "-0->")
    (:one   . "-1->")
    (:omega . "->"))
  "Prolog-style fact table: multiplicity keyword → arrow string.")

(defun %arrow-string (mult)
  "Look up MULT in *arrow-mult-strings*; signal error for unknown multiplicities."
  (or (cdr (assoc mult *arrow-mult-strings*))
      (error "Unknown arrow multiplicity: ~S" mult)))

(defun %format-arrow-params (params arrow)
  "Format PARAMS as a single type or a parenthesised list with ARROW interleaved."
  (if (= 1 (length params))
      (type-to-string (first params))
      (format nil "(~{~A~^ ~A ~})"
              (list-interleave (mapcar #'type-to-string params) arrow))))

(defmethod type-to-string ((ty type-arrow))
  (let* ((params  (type-arrow-params ty))
         (ret     (type-to-string (type-arrow-return ty)))
         (effects (type-arrow-effects ty))
         (arrow   (%arrow-string (type-arrow-mult ty)))
         (params-str (%format-arrow-params params arrow)))
    (if (and effects
             (not (eq effects +pure-effect-row+))
             (type-effect-row-effects effects))
        (format nil "~A -[~A]~A ~A" params-str (type-to-string effects) arrow ret)
        (format nil "~A ~A ~A"       params-str arrow ret))))

;;; ─── Product / record / variant types ────────────────────────────────────

(defmethod type-to-string ((ty type-product))
  (format nil "(~{~A~^, ~})" (mapcar #'type-to-string (type-product-elems ty))))

(defun %format-row-fields (fields sep-open sep-close row-var)
  "Format field alist FIELDS as SEP-OPEN name: type, ... [| ROW-VAR] SEP-CLOSE."
  (let ((field-strs (mapcan (lambda (f)
                              (list (car f) (type-to-string (cdr f))))
                            fields)))
    (if row-var
        (format nil "~A~{~A: ~A~^, ~} | ~A~A" sep-open field-strs (type-to-string row-var) sep-close)
        (format nil "~A~{~A: ~A~^, ~}~A"       sep-open field-strs sep-close))))

(defmethod type-to-string ((ty type-record))
  (%format-row-fields (type-record-fields ty) "{" "}" (type-record-row-var ty)))

(defmethod type-to-string ((ty type-variant))
  (%format-row-fields (type-variant-cases ty) "<" ">" (type-variant-row-var ty)))

;;; ─── Set-like composite types ─────────────────────────────────────────────

(defmethod type-to-string ((ty type-union))
  (format nil "(~{~A~^ | ~})" (mapcar #'type-to-string (type-union-types ty))))

(defmethod type-to-string ((ty type-intersection))
  (format nil "(~{~A~^ & ~})" (mapcar #'type-to-string (type-intersection-types ty))))

;;; ─── Binder types ─────────────────────────────────────────────────────────

(defun %format-binder (fmt var body)
  (format nil fmt (type-to-string var) (type-to-string body)))

(defmethod type-to-string ((ty type-forall))
  (%format-binder "(∀~A. ~A)"
                  (type-forall-var ty)
                  (type-forall-body ty)))

(defmethod type-to-string ((ty type-exists))
  (%format-binder "(∃~A. ~A)"
                  (type-exists-var ty)
                  (type-exists-body ty)))

(defmethod type-to-string ((ty type-lambda))
  (%format-binder "(λ~A. ~A)"
                  (type-lambda-var ty)
                  (type-lambda-body ty)))

(defmethod type-to-string ((ty type-mu))
  (%format-binder "(μ~A. ~A)"
                  (type-mu-var ty)
                  (type-mu-body ty)))

(defmethod type-to-string ((ty type-app))
  (format nil "(~A ~A)"
          (type-to-string (type-app-fun ty))
          (type-to-string (type-app-arg ty))))

;;; ─── Annotated / graded types ─────────────────────────────────────────────

(defmethod type-to-string ((ty type-refinement))
  (format nil "{~A | <pred>}" (type-to-string (type-refinement-base ty))))

(defmethod type-to-string ((ty type-linear))
  (format nil "!~A ~A"
          (mult-to-string (type-linear-grade ty))
          (type-to-string (type-linear-base ty))))

(defmethod type-to-string ((ty type-capability))
  (format nil "~A^{~A}"
          (type-to-string (type-capability-base ty))
          (type-capability-cap ty)))

;;; ─── Effect system types ──────────────────────────────────────────────────

(defmethod type-to-string ((ty type-effect-row))
  (let ((effs (type-effect-row-effects ty))
        (rv   (type-effect-row-row-var ty)))
    (cond
      ((and (null effs) (null rv)) "{}")
      ((null rv)   (format nil "{~{~A~^, ~}}"     (mapcar #'type-to-string effs)))
      ((null effs) (format nil "{| ~A}"             (type-to-string rv)))
      (t           (format nil "{~{~A~^, ~} | ~A}" (mapcar #'type-to-string effs)
                                                     (type-to-string rv))))))

(defmethod type-to-string ((ty type-effect-op))
  (if (type-effect-op-args ty)
      (format nil "(~A~{ ~A~})"
              (type-effect-op-name ty)
              (mapcar #'type-to-string (type-effect-op-args ty)))
      (symbol-name (type-effect-op-name ty))))

(defmethod type-to-string ((ty type-handler))
  (format nil "[~A => ~A / ~A]"
          (type-to-string (type-handler-effect ty))
          (type-to-string (type-handler-input ty))
          (type-to-string (type-handler-output ty))))

(defmethod type-to-string ((ty type-gadt-con))
  (format nil "~A :: ~{~A ~}-> ~A"
          (type-gadt-con-name ty)
          (mapcar #'type-to-string (type-gadt-con-arg-types ty))
          (type-to-string (type-gadt-con-index-type ty))))

;;; ─── Polymorphism / type-class constraints ────────────────────────────────

(defmethod type-to-string ((ty type-constraint))
  (format nil "(~A ~A)"
          (type-constraint-class-name ty)
          (type-to-string (type-constraint-type-arg ty))))

(defmethod type-to-string ((ty type-qualified))
  (let ((constraints (type-qualified-constraints ty)))
    (if (null constraints)
        (type-to-string (type-qualified-body ty))
        (format nil "(~{~A~^, ~}) => ~A"
                (mapcar #'type-to-string constraints)
                (type-to-string (type-qualified-body ty))))))

(defmethod type-to-string ((ty type-scheme))
  (let ((vars (type-scheme-quantified-vars ty)))
    (if (null vars)
        (type-to-string (type-scheme-type ty))
        (format nil "(∀~{~A~^ ~}. ~A)"
                (mapcar #'type-to-string vars)
                (type-to-string (type-scheme-type ty))))))

;;; ─── Backward-compat structs (defined in typeclass-compat-legacy.lisp) ───

(defmethod type-to-string ((ty type-class-constraint))
  (format nil "(~A ~A)"
          (type-class-constraint-class-name ty)
          (type-to-string (type-class-constraint-type-arg ty))))

(defmethod type-to-string ((ty type-skolem))
  (format nil "!sk~D" (type-skolem-id ty)))

(defmethod type-to-string ((ty type-effect))
  (symbol-name (%type-effect-name ty)))

;;; ─── Utilities ────────────────────────────────────────────────────────────

(defun list-interleave (items sep)
  "Interleave SEP between each element of ITEMS."
  (if (null items)
      nil
      (loop for (item . rest) on items
            collect item
            when rest collect sep)))

;;; ─── unparse-type — inverse of parse-type-specifier ─────────────────────

(defun unparse-type (ty)
  "Convert a type-node back to a type specifier s-expression."
  (typecase ty
    (type-primitive (type-primitive-name ty))
    (type-var
     (if (type-var-name ty)
         (type-var-name ty)
         (intern (format nil "?T~D" (type-var-id ty)))))
    (type-arrow
     `(-> ,@(mapcar #'unparse-type (type-arrow-params ty))
          ,(unparse-type (type-arrow-return ty))))
    (type-product
     `(values ,@(mapcar #'unparse-type (type-product-elems ty))))
    (type-union
     (let ((name (type-union-constructor-name ty)))
       (if name
           `(,name ,@(mapcar #'unparse-type (type-constructor-args ty)))
           `(or ,@(mapcar #'unparse-type (type-union-types ty))))))
    (type-intersection
     `(and ,@(mapcar #'unparse-type (type-intersection-types ty))))
    (type-forall
     `(forall ,(type-var-name (type-forall-var ty))
              ,(unparse-type (type-forall-body ty))))
    (type-app
     ;; Flatten curried type-app: ((F A) B) → (F A B) for roundtrip compatibility
     (let ((name (type-constructor-name ty))
           (args (type-constructor-args ty)))
       (if name
           `(,name ,@(mapcar #'unparse-type args))
           (list (unparse-type (type-app-fun ty))
                 (unparse-type (type-app-arg ty))))))
    (t ty)))

;;; ─── looks-like-type-specifier-p ─────────────────────────────────────────

(defparameter *primitive-type-name-strings*
  '("FIXNUM" "STRING" "BOOLEAN" "SYMBOL" "CONS" "NULL" "T" "FLOAT" "CHARACTER" "CHAR")
  "CL primitive type names recognised as type specifiers.")

(defparameter *shorthand-type-name-strings*
  '("INT" "BOOL" "TOP" "?")
  "Short-form type names recognised as type specifiers.")

(defparameter *composite-type-head-strings*
  '("OR" "AND" "FUNCTION" "VALUES" "CONS" "LIST" "VECTOR" "ARRAY" "OPTION"
    "->" "->0" "->1" "FORALL" "∀" "=>" "EXISTS" "MU" "REFINE" "RECORD" "VARIANT")
  "Head symbols that introduce composite type specifiers.")

(defparameter *atomic-type-name-strings*
  (append *primitive-type-name-strings* *shorthand-type-name-strings*)
  "Union of primitive and shorthand names for single-pass atomic type lookup.")

(defun looks-like-type-specifier-p (spec)
  "True iff SPEC looks like it might be a type specifier.
Uses string= for non-CL symbols to ensure package-independent matching."
  (flet ((sym-name-in (sym table)
           (and (symbolp sym) (member (symbol-name sym) table :test #'string=))))
    (or (and (symbolp spec)
             (or (sym-name-in spec *atomic-type-name-strings*)
                 (lookup-type-alias spec)))
        (and (consp spec)
             (let ((head (car spec)))
               (or (sym-name-in head *composite-type-head-strings*)
                   (and (symbolp head)
                        (char= (char (symbol-name head) 0) #\!))))))))
