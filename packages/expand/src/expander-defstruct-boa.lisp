(in-package :cl-cc/expand)
;;;; DEFSTRUCT BOA lambda-list and constructor helpers.

;;; ── BOA (By-Order-of-Arguments) lambda list helpers ──────────────────────

(defun %defstruct-extract-boa-parts (boa-args)
  "Split a BOA lambda list into (normal-params . aux-bindings)."
  (loop with in-aux = nil
        for arg in boa-args
        if (eq arg '&aux)
          do (setq in-aux t)
        else if in-aux
          collect (if (consp arg) arg (%expander-form arg nil)) into aux
        else
          collect arg into normal
        finally (return (cons normal aux))))

(defun %defstruct-boa-param-names (normal-params)
  "Extract bound parameter names from a BOA lambda list (excludes &aux)."
  (loop for p in normal-params
        unless (member p '(&key &optional &rest &body &allow-other-keys) :test #'eq)
          collect (cond
                    ((and (consp p) (consp (first p))) (second (first p)))
                    ((consp p) (first p))
                    (t p))))

;;; ── Constructor generation ───────────────────────────────────────────────

(defun %defstruct-build-constructor (ctor-name boa-args all-slots body-fn)
  "Build a DEFUN constructor form. BODY-FN receives the effective slot-value forms."
  (if boa-args
      (let* ((boa-data     (%defstruct-boa-bindings boa-args))
             (normal-params (first boa-data))
             (bound-names  (car (cdddr boa-data)))
             (aux-lets     (car (cddddr boa-data))))
        (%expander-form 'defun ctor-name normal-params
                        (%expander-form
                         'let* aux-lets
                         (funcall body-fn
                                  (%defstruct-resolve-slot-values all-slots bound-names)))))
      (let ((key-bindings (loop for slot in all-slots
                                collect (%expander-form (first slot) (second slot))))
            (slot-values  (mapcar #'first all-slots)))
        (%expander-form 'defun ctor-name
                        (cons '&key key-bindings)
                        (funcall body-fn slot-values)))))

(defun %defstruct-make-constructor (ctor-name class-name boa-args all-slots)
  "Generate a DEFUN form for a defstruct constructor using CLOS make-instance."
  (%defstruct-build-constructor
   ctor-name boa-args all-slots
   (lambda (slot-values)
     (let ((args (loop for slot in all-slots
                       for value in slot-values
                       nconc (list (%defstruct-make-keyword (first slot)) value))))
       (cons 'make-instance
             (cons (%expander-form 'quote class-name) args))))))
