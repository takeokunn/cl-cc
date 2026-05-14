(in-package :cl-cc/expand)
;;;; DEFSTRUCT :TYPE list/vector expansion helpers.

;;; ── List/Vector-based struct expansion (FR-546) ──────────────────────────

(defun %defstruct-typed-container-form (struct-type struct-name slot-values)
  "Build the concrete list/vector container form for a typed defstruct."
  (if (eq struct-type 'list)
      (cons 'list (cons (%expander-form 'quote struct-name) slot-values))
      (cons 'vector (cons (%expander-form 'quote struct-name) slot-values))))

(defun %defstruct-typed-constructor (ctor-name struct-name struct-type boa-args all-slots)
  "Generate a constructor for :type list or :type vector defstruct."
  (%defstruct-build-constructor
   ctor-name boa-args all-slots
   (lambda (slot-values)
     (%defstruct-typed-container-form struct-type struct-name slot-values))))

(defun %defstruct-typed-accessors (struct-type conc-name all-slots)
  "Generate accessor functions for :type list or :type vector defstruct."
  (loop for slot in all-slots
        for idx from 1
        collect (let ((acc-name (%defstruct-accessor-name conc-name (first slot))))
                  (if (eq struct-type 'list)
                      (%expander-form 'defun acc-name
                                      (%expander-form 'obj)
                                      (%expander-form 'nth idx 'obj))
                      (%expander-form 'defun acc-name
                                      (%expander-form 'obj)
                                      (%expander-form 'aref 'obj idx))))))

(defun %defstruct-typed-predicate (pred-name struct-name struct-type slot-count)
  "Generate a predicate for :type list or :type vector defstruct."
  (when pred-name
    (let* ((total-len   (+ slot-count 1))
           (quoted-name (%expander-form 'quote struct-name))
           (list-pred   (%expander-form 'and
                                        (%expander-form 'listp 'obj)
                                        (%expander-form 'eq (%expander-form 'car 'obj) quoted-name)
                                        (%expander-form '= (%expander-form 'length 'obj) total-len)))
           (vector-pred (%expander-form 'and
                                        (%expander-form 'vectorp 'obj)
                                        (%expander-form '> (%expander-form 'length 'obj) 0)
                                        (%expander-form 'eq (%expander-form 'aref 'obj 0) quoted-name)
                                        (%expander-form '= (%expander-form 'length 'obj) total-len)))
           (pred-body   (if (eq struct-type 'list) list-pred vector-pred)))
      (%expander-form 'defun pred-name (%expander-form 'obj) pred-body))))

(defun %defstruct-typed-expansion-parts (model)
  "Build ctor/accessor/predicate/copier parts for typed defstruct expansion."
  (let ((name        (getf model :name))
        (struct-type (getf model :struct-type))
        (ctor-name   (getf model :ctor-name))
        (boa-args    (getf model :boa-args))
        (all-slots   (getf model :all-slots))
        (conc-name   (getf model :conc-name))
        (pred-name   (getf model :pred-name))
        (copier-name (getf model :copier-name)))
    (let ((ctor-form      (when ctor-name
                            (%defstruct-typed-constructor ctor-name name struct-type boa-args all-slots)))
          (accessor-forms (%defstruct-typed-accessors struct-type conc-name all-slots))
          (pred-form      (when pred-name
                            (%defstruct-typed-predicate pred-name name struct-type (length all-slots))))
          (copier-form    (%defstruct-copier-form copier-name name all-slots struct-type)))
      (%expander-form name ctor-form accessor-forms pred-form copier-form))))

(defun %defstruct-typed-expansion-forms (name ctor-form accessor-forms pred-form copier-form)
  "Build the ordered PROGN tail forms for typed expansion."
  (append (when ctor-form   (list ctor-form))
          accessor-forms
          (when pred-form   (list pred-form))
          (when copier-form (list copier-form))
          (list (%expander-form 'quote name))))

(defun %defstruct-typed-expansion (model)
  "Emit the full expansion for a :type list/vector defstruct MODEL."
  (let ((parts (%defstruct-typed-expansion-parts model)))
    (cons 'progn
          (%defstruct-typed-expansion-forms (first parts)
                                            (second parts)
                                            (third parts)
                                            (fourth parts)
                                            (fifth parts)))))
