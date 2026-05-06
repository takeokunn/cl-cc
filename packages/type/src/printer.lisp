;;;; printer.lisp - Type Pretty-Printer
;;;;
;;;; Provides `type-to-string` for all type-node subtypes via defgeneric dispatch.
;;;; Each type-node class owns its printing rule: extensible without touching this file.
;;;; Architecture: defgeneric = Prolog-style clause database keyed on CLOS type hierarchy.
;;;;
;;;; Load order: after representation.lisp, typeclass.lisp.

(in-package :cl-cc/type)

;;; ─── Primary protocol ─────────────────────────────────────────────────────
;;;
;;; type-to-string acts as a Prolog predicate: each defmethod is a clause.
;;; New type nodes add new methods rather than editing a monolithic typecase.

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

(defun %advanced-display-value (value)
  "Render nested advanced payload values using type printers where applicable."
  (cond
    ((typep value 'type-node) (type-to-string value))
    ((consp value)
     (format nil "(~{~A~^ ~})" (mapcar #'%advanced-display-value value)))
    (t (prin1-to-string value))))

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

(defmethod type-to-string ((ty type-advanced))
  (let* ((surface-head (type-advanced-name ty))
         (head (if (and (symbolp surface-head)
                        (not (string= (symbol-name surface-head) "ADVANCED")))
                   surface-head
                    'advanced))
         (feature-id (type-advanced-feature-id ty)))
    (with-output-to-string (out)
      (format out "(~A" (symbol-name head))
      (when (eq head 'advanced)
        (format out " ~A" feature-id))
      (dolist (arg (type-advanced-args ty))
        (format out " ~A" (%advanced-display-value arg)))
      (dolist (entry (type-advanced-properties ty))
        (format out " ~A ~A"
                (symbol-name (car entry))
                (%advanced-display-value (cdr entry))))
      (when (type-advanced-evidence ty)
        (format out " :EVIDENCE ~A"
                (%advanced-display-value (type-advanced-evidence ty))))
      (write-char #\) out))))

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

;;; ─── Utilities ────────────────────────────────────────────────────────────

(defun list-interleave (items sep)
  "Interleave SEP between each element of ITEMS."
  (if (null items)
      nil
      (loop for (item . rest) on items
            collect item
            when rest collect sep)))
