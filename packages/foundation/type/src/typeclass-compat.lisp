;;;; packages/foundation/type/src/typeclass-compat.lisp
;;;; Backward-compat type nodes and adapters retained for printers/tests.
;;;; Active typeclass instance registry logic now lives in
;;;; typeclass-instance-registry.lisp.

(in-package :cl-cc/type)

;;; ─── Backward-compat API (remaining legacy nodes) ─────────────────────────

(defstruct (type-class-constraint
            (:constructor make-type-class-constraint))
  "Internal compatibility node retained for printer/unparser support.
It is no longer part of the public exported API."
  (class-name nil :type symbol)
  (type-arg   nil))

(defstruct (type-effect (:constructor make-type-effect)
                        (:conc-name %type-effect-))
  "Backward-compat effect label node (old API).
New code should use type-effect-op."
  (name nil :type symbol))

(defun type-effect-name (e)
  "Return the effect name symbol for E, which may be a type-effect or type-effect-op."
  (cond
    ((type-effect-p e)    (%type-effect-name e))
    ((type-effect-op-p e) (type-effect-op-name e))
    (t (error "type-effect-name: not an effect node: ~S" e))))

(defstruct (type-effectful-function
            (:include type-arrow)
            (:constructor make-type-effectful-function
                          (&key params return effects mult
                           &aux (params params) (return return)
                                (effects effects) (mult (or mult :omega)))))
  "A function type annotated with an explicit effect row.")

(defvar *skolem-counter* 0
  "Counter for unique skolem IDs.")

(defstruct (type-skolem (:constructor %make-type-skolem))
  "Backward-compat skolem / rigid variable (old API)."
  (id   0   :type fixnum)
  (name nil))

(defun make-type-skolem (&optional (name "a"))
  "Create a fresh skolem constant with NAME."
  (incf *skolem-counter*)
  (%make-type-skolem :id *skolem-counter* :name name))

(defun type-skolem-equal-p (s1 s2)
  "True iff S1 and S2 are the same skolem (by ID)."
  (and (type-skolem-p s1) (type-skolem-p s2)
       (= (type-skolem-id s1) (type-skolem-id s2))))
