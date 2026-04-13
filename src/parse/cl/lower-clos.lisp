;;;; src/parse/cl/lower-clos.lisp — CLOS/OOP Form Lowerers
;;;
;;; Extracted from lower-definitions.lisp.
;;; Contains lowerers for CLOS and OOP forms:
;;;   defclass, defgeneric, defmethod, make-instance, slot-value
;;;
;;; The general-purpose definition and control-flow lowerers remain in
;;; lower-definitions.lisp (flet, labels, defun, defvar, defmacro,
;;; values, mvb, mvc, mvp1, apply, catch, throw, unwind-protect,
;;; handler-case, quote, the, funcall).
;;;
;;; Depends on lower-definitions.lisp (define-list-lowerer, lower-sexp-to-ast,
;;;   parse-slot-spec, %lower-local-fn-bindings, make-ast-* constructors).
;;; Load order: immediately after lower-definitions.lisp.

(in-package :cl-cc)

;;; ── Defclass ─────────────────────────────────────────────────────────────────

(define-list-lowerer (defclass) (node sf sl sc)
  (unless (>= (length node) 4)
    (error "defclass requires name, superclasses, and slot definitions"))
  (let* ((name (second node)) (superclasses (third node)) (slot-specs (fourth node))
         ;; Parse class options (5th+ elements): (:default-initargs key val ...)
         (class-options (nthcdr 4 node))
         (default-initargs-opt (find :default-initargs class-options
                                     :key (lambda (o) (when (listp o) (first o)))))
         (default-initargs (when default-initargs-opt
                             (loop for (k v) on (rest default-initargs-opt) by #'cddr
                                   collect (cons k (lower-sexp-to-ast v))))))
    (unless (symbolp name)      (error "defclass name must be a symbol"))
    (unless (listp superclasses) (error "defclass superclasses must be a list"))
    (unless (listp slot-specs)   (error "defclass slots must be a list"))
    (make-ast-defclass :name name
                       :superclasses superclasses
                       :slots (mapcar #'parse-slot-spec slot-specs)
                       :default-initargs default-initargs
                       :source-file sf :source-line sl :source-column sc)))

;;; ── Defgeneric ───────────────────────────────────────────────────────────────

(define-list-lowerer (defgeneric) (node sf sl sc)
  (unless (>= (length node) 3)
    (error "defgeneric requires name and lambda-list"))
  (let* ((name (second node))
         (lambda-list (third node))
         (options (cdddr node))
         (inline-methods nil))
    (unless (symbolp name) (error "defgeneric name must be a symbol"))
    ;; Parse options: extract (:method ...) forms, :method-combination, accept and ignore others
    (let ((combination nil))
      (dolist (opt options)
        (when (consp opt)
          (case (car opt)
            (:method
             (push (cons 'defmethod (cons name (cdr opt))) inline-methods))
            (:method-combination
             (setf combination (second opt))))))
    (setf inline-methods (nreverse inline-methods))
    (let ((gf-node (make-ast-defgeneric :name name :params lambda-list
                                        :combination combination
                                        :source-file sf :source-line sl
                                        :source-column sc)))
      (if inline-methods
          ;; Wrap in progn: defgeneric + defmethod forms
          (make-ast-progn
           :forms (cons gf-node
                        (mapcar (lambda (m)
                                  (lower-sexp-to-ast m :source-file sf
                                                       :source-line sl
                                                       :source-column sc))
                                inline-methods))
           :source-file sf :source-line sl :source-column sc)
          gf-node)))))

;;; ── Defmethod ────────────────────────────────────────────────────────────────

(define-list-lowerer (defmethod) (node sf sl sc)
  (unless (>= (length node) 4)
    (error "defmethod requires name, parameters, and body"))
  (let* ((name (second node))
         (rest-after-name (cddr node))
         ;; Skip optional method qualifier (:before, :after, :around)
         (qualifier (when (and (symbolp (car rest-after-name))
                               (not (listp (car rest-after-name))))
                      (pop rest-after-name)))
         (raw-params (car rest-after-name))
         (body-forms (cdr rest-after-name))
         (specializers nil) (param-names nil))
    (unless (symbolp name)    (error "defmethod name must be a symbol"))
    (unless (listp raw-params) (error "defmethod parameters must be a list"))
    (dolist (p raw-params)
      (if (listp p)
          (progn (push (cons (first p) (second p)) specializers)
                 (push (first p) param-names))
          (progn (push nil specializers)
                 (push p param-names))))
    (make-ast-defmethod
     :name name
     :qualifier qualifier
     :specializers (nreverse specializers)
     :params       (nreverse param-names)
     :body         (list (lower-sexp-to-ast (list* 'block name body-forms)))
     :source-file sf :source-line sl :source-column sc)))

;;; ── Make-instance ────────────────────────────────────────────────────────────

(define-list-lowerer (make-instance) (node sf sl sc)
  (unless (>= (length node) 2)
    (error "make-instance requires at least a class name"))
  (let ((initargs nil) (rest (cddr node)))
    (loop while rest
          do (let ((key (pop rest)))
               (unless (keywordp key)
                 (error "make-instance initarg must be a keyword, got ~S" key))
               (unless rest
                 (error "make-instance initarg ~S missing value" key))
               (push (cons key (lower-sexp-to-ast (pop rest))) initargs)))
    (make-ast-make-instance :class (lower-sexp-to-ast (second node))
                            :initargs (nreverse initargs)
                            :source-file sf :source-line sl :source-column sc)))

;;; ── Slot-value ───────────────────────────────────────────────────────────────

(define-list-lowerer (slot-value) (node sf sl sc)
  (unless (= (length node) 3)
    (error "slot-value requires object and slot-name"))
  (make-ast-slot-value
   :object (lower-sexp-to-ast (second node))
   :slot   (let ((sn (third node)))
             (if (and (listp sn) (eq (car sn) 'quote)) (second sn) sn))
   :source-file sf :source-line sl :source-column sc))
