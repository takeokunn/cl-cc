(in-package :cl-cc/expand)

;;; Shared gensym helper used by CLOS protocol macros below.
(defmacro with-gensyms (names &body body)
  "Bind each NAME in NAMES to a fresh gensym named after the symbol, then evaluate BODY."
  (cons 'let
        (cons (mapcar (lambda (n) (list n (list 'gensym (symbol-name n)))) names)
              body)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Compat — CLOS Protocol, MOP Introspection, and Print Macros
;;;
;;; Contains: print-unreadable-object, print-object, describe-object, describe,
;;; update-instance-for-different-class, update-instance-for-changed-class,
;;; ensure-class, change-class, define-method-combination,
;;; MOP introspection macros (class-direct-superclasses, class-direct-slots,
;;; class-slots, class-direct-default-initargs, generic-function-methods,
;;; generic-function-method-combination, class-precedence-list),
;;; parse-float, reinitialize-instance, shared-initialize.
;;;
;;; Package/declaration/file-IO/hash-table/coerce/load-time-value/feature system
;;; macros are in macros-compat.lisp (loads before).
;;;
;;; Load order: after macros-compat.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


;;; FR-1004: print-unreadable-object
;;; Note: uses flat (spec &body body) lambda list because our-defmacro does not
;;; support nested destructuring in required params.

(register-macro 'print-unreadable-object
  (lambda (form env)
    (declare (ignore env))
    (let* ((spec (second form))
           (body (cddr form))
           (object (first spec))
           (stream-frm (second spec))
           (rest-keys (cddr spec))
           (type-expr (getf rest-keys :type))
           (id-expr (getf rest-keys :identity))
           (obj-var (gensym "OBJ"))
           (str-var (gensym "STR"))
           (space-forms (when (and type-expr body)
                          (list (list 'format str-var " ")))))
      (cons 'let
            (cons (list (list obj-var object)
                        (list str-var stream-frm))
                  (append
                   (list (list 'format str-var "#<")
                         (list 'when type-expr
                               (list 'format str-var "~A" (list 'type-of obj-var))))
                   space-forms
                   body
                   (when id-expr
                     (list (list 'format str-var " {~X}"
                                 (list 'if (list 'integerp obj-var) obj-var 0))))
                   (list (list 'format str-var ">")
                         nil)))))))

;;; FR-1004: print-object and describe

(register-macro 'print-object
  (lambda (form env)
    (declare (ignore env))
    (let ((object (second form))
          (stream (third form)))
      (with-gensyms (obj-v str-v cls-v mth-v)
        (list 'let*
              (list (list obj-v object)
                    (list str-v stream)
                    (list cls-v (list 'when (list 'hash-table-p obj-v)
                                      (list 'gethash :__class__ obj-v)))
                    (list mth-v (list 'when (list 'hash-table-p cls-v)
                                      (list 'gethash :print-object
                                            (list 'gethash :__methods__ cls-v (list 'make-hash-table))))))
              (list 'if mth-v
                    (list 'funcall mth-v obj-v str-v)
                    (list 'prin1 obj-v str-v)))))))

(register-macro 'describe-object
  (lambda (form env)
    (declare (ignore env))
    (let ((object (second form))
          (stream (third form)))
      (with-gensyms (obj-v str-v cls-v slots-v)
        (list 'let*
              (list (list obj-v object)
                    (list str-v stream)
                    (list cls-v (list 'when (list 'hash-table-p obj-v)
                                      (list 'gethash :__class__ obj-v))))
              (list 'if (list 'hash-table-p cls-v)
                    (list 'let (list (list slots-v (list 'gethash :__slots__ cls-v)))
                          (list 'format str-v "~A is an instance of ~A~%" obj-v (list 'gethash :__name__ cls-v))
                          (list 'dolist (list 'slot slots-v)
                                (list 'format str-v "  ~S = ~S~%" 'slot (list 'gethash 'slot obj-v))))
                    (list 'format str-v "~S~%" obj-v)))))))

(register-macro 'describe
  (lambda (form env)
    (declare (ignore env))
    (let ((object (second form))
          (stream (third form))
          (str-v (gensym "STR")))
      (list 'let (list (list str-v (list 'or stream '*standard-output*)))
            (list 'describe-object object str-v)
            '(values)))))

;;; FR-1005: update-instance-for-different-class / update-instance-for-changed-class

(register-macro 'update-instance-for-different-class
  (lambda (form env)
    (declare (ignore env))
    (cons 'reinitialize-instance (cons (third form) (cdddr form)))))

(register-macro 'update-instance-for-changed-class
  (lambda (form env)
    (declare (ignore env))
    (cons 'reinitialize-instance (cons (second form) (cddr form)))))

;;; FR-1005: ensure-class — create or update a class definition

(register-macro 'ensure-class
  (lambda (form env)
    (declare (ignore env))
    (let* ((name (second form))
           (options (cddr form))
           (direct-superclasses (or (getf options :direct-superclasses) '()))
           (direct-slots (or (getf options :direct-slots) '())))
      (list 'defclass name direct-superclasses direct-slots))))

;;; FR-1003: change-class — change the class of a CLOS instance

(register-macro 'change-class
  (lambda (form env)
    (declare (ignore env))
    (let ((instance (second form))
          (new-class (third form))
          (initargs (cdddr form)))
      (with-gensyms (inst-var new-class-var old-slots-var new-slots-var s-var)
        (cons 'let*
              (cons (list (list inst-var instance)
                          (list new-class-var new-class)
                          (list old-slots-var (list 'when (list 'hash-table-p (list 'gethash :__class__ inst-var))
                                                    (list 'gethash :__slots__ (list 'gethash :__class__ inst-var))))
                          (list new-slots-var (list 'when (list 'hash-table-p new-class-var)
                                                    (list 'gethash :__slots__ new-class-var))) )
                    (append
                     (list (list 'unless (list 'hash-table-p new-class-var)
                                 (list 'error "change-class: new-class must be a class descriptor hash table"))
                           (list 'dolist (list s-var old-slots-var)
                                 (list 'unless (list 'member s-var new-slots-var :test '(function equal))
                                       (list 'remhash s-var inst-var)))
                           (list 'dolist (list s-var new-slots-var)
                                 (list 'unless (list 'member s-var old-slots-var :test '(function equal))
                                       (list 'setf (list 'gethash s-var inst-var) nil)))
                           (list 'setf (list 'gethash :__class__ inst-var) new-class-var))
                     (when initargs
                       (list (cons 'update-instance-for-different-class
                                   (cons nil (cons inst-var initargs)))))
                     (list inst-var))))))))

;;; FR-376: define-method-combination (short form)
;;; Registers the combination name so defgeneric can reference it.
;;; The actual dispatch is handled by the VM in vm-dispatch-generic-call.
;;; Supported short-form: (define-method-combination name &key operator identity-with-one-argument)

(register-macro 'define-method-combination
  (lambda (form env)
    (declare (ignore env))
    (list 'quote (second form))))

