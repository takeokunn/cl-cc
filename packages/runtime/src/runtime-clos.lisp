;;;; packages/runtime/src/runtime-clos.lisp - CL-CC Runtime: CLOS Descriptors + Generic Dispatch
;;;
;;; Contains: *rt-class-registry*, rt-defclass, rt-make-instance, rt-slot-*,
;;; rt-class-*, *rt-primitive-type-classifiers*, rt-register-method, rt-call-generic.
;;;
;;; Depends on runtime.lisp. Load order: after runtime-misc.lisp.

(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; Class Registry
;;; ------------------------------------------------------------

(defvar *rt-class-registry* (make-hash-table :test #'eq)
  "Runtime class registry for native/self-hosted CLOS descriptors.")

(defun %rt-cpl-walk (name seen)
  "Accumulate class precedence list starting from NAME with SEEN already visited."
  (if (member name seen :test #'eq)
      seen
      (let* ((class-ht (gethash name *rt-class-registry*))
             (supers (and class-ht (gethash :__superclasses__ class-ht))))
        (reduce (lambda (acc super) (%rt-cpl-walk super acc))
                supers
                :initial-value (append seen (list name))))))

(defun %rt-compute-class-precedence-list (class-name)
  "Compute a simple class precedence list from *rt-class-registry*."
  (%rt-cpl-walk class-name '()))

(defun rt-defclass (name direct-supers slots)
  (let ((class-ht (or (gethash name *rt-class-registry*)
                      (make-hash-table :test #'eq))))
    (setf (gethash :__name__         class-ht) name
          (gethash :__superclasses__ class-ht) direct-supers
          (gethash :__slots__        class-ht) slots
          (gethash :__methods__      class-ht) (or (gethash :__methods__  class-ht)
                                                   (make-hash-table :test #'equal))
          (gethash :__eql-index__    class-ht) (or (gethash :__eql-index__ class-ht)
                                                   (make-hash-table :test #'equal))
          (gethash name *rt-class-registry*)   class-ht
          (gethash :__cpl__          class-ht) (%rt-compute-class-precedence-list name))
    class-ht))

;;; ------------------------------------------------------------
;;; Instance Access
;;; ------------------------------------------------------------

(defun rt-make-instance (class &rest initargs)
  (apply #'make-instance class initargs))

(defun rt-make-instance-0 (class)
  (make-instance class))

(defun rt-slot-value (obj slot-name)
  (slot-value obj slot-name))

(defun rt-slot-set (obj slot-name val)
  (setf (slot-value obj slot-name) val))

(defun rt-slot-boundp (obj slot-name)
  (if (slot-boundp obj slot-name) 1 0))

(defun rt-slot-makunbound (obj slot-name)
  (slot-makunbound obj slot-name))

(defun rt-slot-exists-p (obj slot-name)
  (if (slot-exists-p obj slot-name) 1 0))

(defun rt-class-name (class)
  (if (hash-table-p class)
      (gethash :__name__ class)
      (class-name class)))

(defun rt-class-of (obj) (class-of obj))

(defun rt-find-class (name)
  (or (gethash name *rt-class-registry*)
      (find-class name nil)))

;;; ------------------------------------------------------------
;;; EQL Specializer Support
;;; ------------------------------------------------------------

(defun %rt-eql-specializer-p (key)
  (and (consp key) (eq (car key) 'eql)))

(defun %rt-extract-eql-specializer-keys (specializer)
  (cond
    ((%rt-eql-specializer-p specializer)
     (list (second specializer)))
    ((and (consp specializer)
          (= (length specializer) 1)
          (%rt-eql-specializer-p (car specializer)))
     (list (second (car specializer))))
     (t nil)))

(defun %rt-method-key-qualifier (specs)
  "Return qualifier keyword for SPECS, or NIL for primary methods.

Accepted qualified forms:
- (:__BEFORE__ class)
- (:__AFTER__ class)
- (:__AROUND__ class)"
  (when (and (consp specs)
             (= (length specs) 2)
             (keywordp (first specs))
             (member (first specs) '(:__BEFORE__ :__AFTER__ :__AROUND__) :test #'eq))
    (first specs)))

(defun %rt-method-key-specializer (specs)
  "Return the base specializer key from SPECS.

Qualified method keys like `(:__BEFORE__ class)` normalize to `class` for
classification/EQL indexing purposes; unqualified keys return unchanged." 
  (or (and (%rt-method-key-qualifier specs)
           (second specs))
      specs))

;;; ------------------------------------------------------------
;;; Generic Dispatch
;;; ------------------------------------------------------------

(defparameter *rt-primitive-type-classifiers*
  '((integer . integer)
    (string  . string)
    (symbol  . symbol))
  "Ordered (CL-type . dispatch-name) pairs for primitive argument classification.
Used by %rt-classify-arg for generic dispatch on non-CLOS values.")

(defun %rt-classify-arg (arg)
  "Return the dispatch type name for ARG.
CLOS descriptor hash tables → slot :__name__; primitives → *rt-primitive-type-classifiers*."
  (if (hash-table-p arg)
      (let ((class-ht (gethash :__class__ arg)))
        (if class-ht (gethash :__name__ class-ht) t))
      (or (cdr (assoc-if (lambda (type) (typep arg type))
                         *rt-primitive-type-classifiers*))
          t)))

(defun rt-register-method (gf specs method &optional qualifier)
  "Register METHOD in GF under SPECS, optionally qualified by QUALIFIER.

When QUALIFIER is provided, SPECS are normalized to a qualified method key
`(:__<QUALIFIER>__ <SPECIALIZER>)` to match runtime dispatch table layout."
  (unless (hash-table-p gf)
    (error "rt-register-method expects a generic-function descriptor hash table, got ~S" gf))
  (let* ((normalized-specs
           (if qualifier
               (list (intern (format nil "__~A__" (string-upcase (string qualifier))) :keyword)
                     specs)
               specs))
         (methods-ht (or (gethash :__methods__ gf)
                          (setf (gethash :__methods__ gf) (make-hash-table :test #'equal))))
        (eql-index (or (gethash :__eql-index__ gf)
                        (setf (gethash :__eql-index__ gf) (make-hash-table :test #'equal))))
         (specializer (%rt-method-key-specializer normalized-specs)))
    (setf (gethash normalized-specs methods-ht) method)
    (dolist (key (%rt-extract-eql-specializer-keys specializer))
      (pushnew method (gethash key eql-index) :test #'eq))
    method))

(defun %rt-lookup-qualified-methods (methods-ht qualifier class-name)
  "Return applicable qualified methods for QUALIFIER and CLASS-NAME in CPL order."
  (let ((result nil)
        (class-ht (and class-name (gethash class-name *rt-class-registry*))))
    (flet ((maybe-push (spec)
             (let ((m (gethash spec methods-ht)))
               (when m (push m result)))))
      (maybe-push (list qualifier class-name))
      (when class-ht
        (dolist (ancestor (cdr (gethash :__cpl__ class-ht)))
          (maybe-push (list qualifier ancestor))))
      (maybe-push (list qualifier t)))
    (nreverse result)))

(defun %rt-resolve-combination-operator (combination)
  "Resolve custom method-combination COMBINATION symbol to host operator."
  (or (and (symbolp combination)
           (fboundp combination)
           (symbol-function combination))
      (error "Unsupported runtime method combination operator: ~S" combination)))

(defun %rt-lookup-method-by-class (methods-ht class-name)
  (or (gethash class-name methods-ht)
      (let ((class-ht (gethash class-name *rt-class-registry*)))
        (when class-ht
          (loop for ancestor in (cdr (gethash :__cpl__ class-ht))
                for method = (gethash ancestor methods-ht)
                when method return method)))
      (gethash t methods-ht)))

(defun rt-call-generic (gf &rest args)
  (if (hash-table-p gf)
      (let* ((methods-ht  (gethash :__methods__ gf))
             (eql-index   (gethash :__eql-index__ gf))
             (first-arg   (first args))
             (class-name  (%rt-classify-arg first-arg))
             (combination (gethash :__method-combination__ gf))
             (before-methods (%rt-lookup-qualified-methods methods-ht :__BEFORE__ class-name))
             (after-methods (%rt-lookup-qualified-methods methods-ht :__AFTER__ class-name))
             (around-methods (%rt-lookup-qualified-methods methods-ht :__AROUND__ class-name))
             (primary-method (or (and eql-index
                                      (let ((bucket (gethash first-arg eql-index)))
                                        (cond
                                          ((null bucket) nil)
                                          ((listp bucket) (first bucket))
                                          (t bucket))))
                                  (%rt-lookup-method-by-class methods-ht class-name))))
        (cond
          ((and combination (not (eq combination 'standard)))
           (let* ((qual-key (intern (format nil "__~A__" (string-upcase (string combination))) :keyword))
                  (combo-methods (%rt-lookup-qualified-methods methods-ht qual-key class-name))
                  (methods (or combo-methods (and primary-method (list primary-method)))))
             (unless methods
               (error "No applicable runtime generic method for ~S on ~S"
                      (gethash :__name__ gf) class-name))
             (apply (%rt-resolve-combination-operator combination)
                    (mapcar (lambda (m) (apply m args)) methods))))
          ((or around-methods before-methods after-methods)
           ;; Conservative standard-combination runtime behavior:
           ;; around (most specific, single) -> before* -> primary -> after* (reverse)
           (let ((result nil))
             (when around-methods
               (setf result (apply (first around-methods) args)))
             (dolist (m before-methods) (apply m args))
             (unless primary-method
               (error "No applicable runtime generic primary method for ~S on ~S"
                      (gethash :__name__ gf) class-name))
             (setf result (apply primary-method args))
             (dolist (m (reverse after-methods)) (apply m args))
             result))
          (t
           (unless primary-method
             (error "No applicable runtime generic method for ~S on ~S"
                    (gethash :__name__ gf) class-name))
           (apply primary-method args))))
      (apply gf args)))
