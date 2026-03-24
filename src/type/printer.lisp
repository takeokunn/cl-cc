;;;; printer.lisp - Type Pretty-Printer
;;;;
;;;; Provides `type-to-string` for all type-node subtypes.
;;;; This file must be loaded after representation.lisp; it overrides
;;;; the stub definition placed there as a forward declaration.

(in-package :cl-cc/type)

;;; ─── Main entry point ─────────────────────────────────────────────────────

(defun type-to-string (ty)
  "Convert a type-node TY to a human-readable string."
  (when (null ty) (return-from type-to-string "()"))
  (cond
    ;; Primitive
    ((type-primitive-p ty)
     (symbol-name (type-primitive-name ty)))

    ;; Unification variable
    ((type-var-p ty)
     (let ((link (type-var-link ty)))
       (if link
           (type-to-string link)
           (if (type-var-name ty)
               (format nil "?~A" (type-var-name ty))
               (format nil "?t~D" (type-var-id ty))))))

    ;; Rigid (skolem)
    ((type-rigid-p ty)
     (if (type-rigid-name ty)
         (format nil "sk~D[~A]" (type-rigid-id ty) (type-rigid-name ty))
         (format nil "sk~D" (type-rigid-id ty))))

    ;; Arrow
    ((type-arrow-p ty)
     (let* ((params  (type-arrow-params ty))
            (ret     (type-to-string (type-arrow-return ty)))
            (effects (type-arrow-effects ty))
            (mult    (type-arrow-mult ty))
            (arrow   (ecase mult
                       (:zero  "-0->")
                       (:one   "-1->")
                       (:omega "->"))))
       (if (and effects (not (eq effects +pure-effect-row+))
                (type-effect-row-effects effects))
           ;; Effectful arrow
           (if (= 1 (length params))
               (format nil "~A -[~A]~A ~A"
                       (type-to-string (first params))
                       (type-to-string effects)
                       arrow ret)
               (format nil "(~{~A~^ ~A ~}) -[~A]~A ~A"
                       (list-interleave (mapcar #'type-to-string params) arrow)
                       (type-to-string effects)
                       arrow ret))
           ;; Pure arrow
           (if (= 1 (length params))
               (format nil "~A ~A ~A" (type-to-string (first params)) arrow ret)
               (format nil "(~{~A~^ ~A ~}) ~A ~A"
                       (list-interleave (mapcar #'type-to-string params) arrow)
                       arrow ret)))))

    ;; Product (tuple)
    ((type-product-p ty)
     (format nil "(~{~A~^, ~})" (mapcar #'type-to-string (type-product-elems ty))))

    ;; Record
    ((type-record-p ty)
     (let ((fields (type-record-fields ty))
           (rv     (type-record-row-var ty)))
       (if rv
           (format nil "{~{~A: ~A~^, ~} | ~A}"
                   (mapcan (lambda (f) (list (car f) (type-to-string (cdr f)))) fields)
                   (type-to-string rv))
           (format nil "{~{~A: ~A~^, ~}}"
                   (mapcan (lambda (f) (list (car f) (type-to-string (cdr f)))) fields)))))

    ;; Variant
    ((type-variant-p ty)
     (let ((cases (type-variant-cases ty))
           (rv    (type-variant-row-var ty)))
       (if rv
           (format nil "<~{~A: ~A~^, ~} | ~A>"
                   (mapcan (lambda (c) (list (car c) (type-to-string (cdr c)))) cases)
                   (type-to-string rv))
           (format nil "<~{~A: ~A~^, ~}>"
                   (mapcan (lambda (c) (list (car c) (type-to-string (cdr c)))) cases)))))

    ;; Union
    ((type-union-p ty)
     (format nil "(~{~A~^ | ~})" (mapcar #'type-to-string (type-union-types ty))))

    ;; Intersection
    ((type-intersection-p ty)
     (format nil "(~{~A~^ & ~})" (mapcar #'type-to-string (type-intersection-types ty))))

    ;; Forall
    ((type-forall-p ty)
     (format nil "(∀~A. ~A)"
             (type-to-string (type-forall-var ty))
             (type-to-string (type-forall-body ty))))

    ;; Exists
    ((type-exists-p ty)
     (format nil "(∃~A. ~A)"
             (type-to-string (type-exists-var ty))
             (type-to-string (type-exists-body ty))))

    ;; Type application
    ((type-app-p ty)
     (format nil "(~A ~A)"
             (type-to-string (type-app-fun ty))
             (type-to-string (type-app-arg ty))))

    ;; Type lambda
    ((type-lambda-p ty)
     (format nil "(λ~A. ~A)"
             (type-to-string (type-lambda-var ty))
             (type-to-string (type-lambda-body ty))))

    ;; Mu (recursive)
    ((type-mu-p ty)
     (format nil "(μ~A. ~A)"
             (type-to-string (type-mu-var ty))
             (type-to-string (type-mu-body ty))))

    ;; Refinement
    ((type-refinement-p ty)
     (format nil "{~A | <pred>}" (type-to-string (type-refinement-base ty))))

    ;; Linear / graded modal
    ((type-linear-p ty)
     (format nil "!~A ~A"
             (mult-to-string (type-linear-grade ty))
             (type-to-string (type-linear-base ty))))

    ;; Capability
    ((type-capability-p ty)
     (format nil "~A^{~A}"
             (type-to-string (type-capability-base ty))
             (type-capability-cap ty)))

    ;; Effect row
    ((type-effect-row-p ty)
     (let ((effs (type-effect-row-effects ty))
           (rv   (type-effect-row-row-var ty)))
       (cond
         ((and (null effs) (null rv)) "{}")
         ((null rv)
          (format nil "{~{~A~^, ~}}" (mapcar #'type-to-string effs)))
         ((null effs)
          (format nil "{| ~A}" (type-to-string rv)))
         (t
          (format nil "{~{~A~^, ~} | ~A}"
                  (mapcar #'type-to-string effs)
                  (type-to-string rv))))))

    ;; Effect op label
    ((type-effect-op-p ty)
     (if (type-effect-op-args ty)
         (format nil "(~A~{ ~A~})"
                 (type-effect-op-name ty)
                 (mapcar #'type-to-string (type-effect-op-args ty)))
         (symbol-name (type-effect-op-name ty))))

    ;; Handler
    ((type-handler-p ty)
     (format nil "[~A => ~A / ~A]"
             (type-to-string (type-handler-effect ty))
             (type-to-string (type-handler-input ty))
             (type-to-string (type-handler-output ty))))

    ;; GADT constructor
    ((type-gadt-con-p ty)
     (format nil "~A :: ~{~A ~}-> ~A"
             (type-gadt-con-name ty)
             (mapcar #'type-to-string (type-gadt-con-arg-types ty))
             (type-to-string (type-gadt-con-index-type ty))))

    ;; Typeclass constraint
    ((type-constraint-p ty)
     (format nil "(~A ~A)"
             (type-constraint-class-name ty)
             (type-to-string (type-constraint-type-arg ty))))

    ;; Qualified type
    ((type-qualified-p ty)
     (if (null (type-qualified-constraints ty))
         (type-to-string (type-qualified-body ty))
         (format nil "(~{~A~^, ~}) => ~A"
                 (mapcar #'type-to-string (type-qualified-constraints ty))
                 (type-to-string (type-qualified-body ty)))))

    ;; Error sentinel
    ((type-error-p ty)
     (format nil "<error: ~A>" (type-error-message ty)))

    ;; Type scheme
    ((type-scheme-p ty)
     (if (null (type-scheme-quantified-vars ty))
         (type-to-string (type-scheme-type ty))
         (format nil "(∀~{~A~^ ~}. ~A)"
                 (mapcar #'type-to-string (type-scheme-quantified-vars ty))
                 (type-to-string (type-scheme-type ty)))))

    (t (format nil "#<type ~A>" (type-of ty)))))

;;; ─── Backward-compat methods (old code used defmethod type-to-string) ─────
;;; Since type-to-string is now a plain function, no methods needed.
;;; Old callers are satisfied by the cond dispatch above.

;;; ─── Utility ──────────────────────────────────────────────────────────────

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
  (cond
    ((type-primitive-p ty) (type-primitive-name ty))
    ((type-var-p ty)
     (if (type-var-name ty) (type-var-name ty) (intern (format nil "?T~D" (type-var-id ty)))))
    ((type-arrow-p ty)
     `(-> ,@(mapcar #'unparse-type (type-arrow-params ty))
          ,(unparse-type (type-arrow-return ty))))
    ((type-product-p ty)
     `(values ,@(mapcar #'unparse-type (type-product-elems ty))))
    ((type-union-p ty)
     `(or ,@(mapcar #'unparse-type (type-union-types ty))))
    ((type-intersection-p ty)
     `(and ,@(mapcar #'unparse-type (type-intersection-types ty))))
    ((type-forall-p ty)
     `(forall ,(type-var-name (type-forall-var ty))
              ,(unparse-type (type-forall-body ty))))
    ((type-app-p ty)
     (list (unparse-type (type-app-fun ty)) (unparse-type (type-app-arg ty))))
    (t ty)))

;;; ─── looks-like-type-specifier-p ─────────────────────────────────────────

(defun looks-like-type-specifier-p (spec)
  "True iff SPEC looks like it might be a type specifier."
  (or (and (symbolp spec)
           (member spec '(fixnum string boolean symbol cons null t
                          int bool top float character)))
      (eq spec '?)
      (and (consp spec)
           (member (car spec) '(or and function values cons list vector array
                                -> ->0 ->1 forall ∀ => exists mu refine
                                record variant)))))
