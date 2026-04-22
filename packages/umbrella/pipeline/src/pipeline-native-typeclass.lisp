;;;; pipeline-native-typeclass.lisp — Typeclass macros for the pipeline
;;;;
;;;; Contains: deftype-class and deftype-instance macro registrations.
;;;; These are registered at pipeline load time because cl-cc/type loads
;;;; before the compiler.
;;;;
;;;; Load order: after pipeline-native.lisp.
(in-package :cl-cc)

;;; Typeclass Macros (Phase 4) — registered here because cl-cc/type loads before compiler

(register-macro 'deftype-class
  (lambda (form env)
    (declare (ignore env))
    ;; Syntax: (deftype-class Name [(:super S1 S2)] (type-param) method-specs...)
    (let* ((class-name (second form))
           (rest (cddr form))
           ;; Check for optional superclass declaration
           (superclasses (when (and (consp (first rest))
                                    (eq (caar rest) :super))
                           (cdar rest)))
           (rest2 (if superclasses (cdr rest) rest))
           (type-param-list (first rest2))
           (type-param (first type-param-list))
           (method-specs (rest rest2))
           (methods-forms nil)
           (default-forms nil))
      (dolist (spec method-specs)
        (let ((method-name (first spec))
              (type-spec (second spec))
              (rest-spec (cddr spec)))
          (push (list 'cons
                      (list 'quote method-name)
                      (list 'cl-cc/type:parse-type-specifier
                            (list 'quote type-spec)))
                methods-forms)
          (when (and rest-spec (eq (first rest-spec) :default))
            (push (list 'cons (list 'quote method-name) (second rest-spec))
                  default-forms))))
      (list 'progn
             (list 'cl-cc/type:register-typeclass
                   (list 'quote class-name)
                    (list 'cl-cc/type:make-typeclass-def
                          :name (list 'quote class-name)
                          :type-params (list 'list (list 'cl-cc/type:fresh-type-var (list 'quote type-param)))
                          :methods (cons 'list (nreverse methods-forms))
                          :defaults (cons 'list (nreverse default-forms))
                          :superclasses (list 'quote superclasses)))
             (list 'quote class-name)))))

(register-macro 'deftype-instance
  (lambda (form env)
    (declare (ignore env))
    ;; Syntax: (deftype-instance Class TypeSpec (method impl) ...)
    (let* ((class-name (second form))
           (type-spec (third form))
           (method-impls (cdddr form))
           (dict-var (intern (format nil "*~A-~A-DICT*"
                                     class-name type-spec)
                             :cl-cc))
           (method-forms nil))
      (dolist (impl method-impls)
        (let ((method-name (first impl))
              (impl-form (second impl)))
          (push (list 'cons (list 'quote method-name) impl-form)
                method-forms)))
      (setf method-forms (nreverse method-forms))
      (list 'progn
            (list 'cl-cc/type:register-typeclass-instance
                  (list 'quote class-name)
                  (list 'cl-cc/type:parse-type-specifier (list 'quote type-spec))
                  (cons 'list method-forms))
            (list 'defvar dict-var (cons 'list method-forms))
            (list 'quote class-name)))))
