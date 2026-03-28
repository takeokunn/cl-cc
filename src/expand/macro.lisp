(in-package :cl-cc)

;;; CL-CC Macro System
;;; A complete macro system implementation with:
;;; - Full destructuring-bind for lambda lists
;;; - Environment classes for lexical scoping
;;; - Macro expansion (single and full)
;;; - Built-in macros for bootstrap

;;; Macro Environment - Available at compile-time for our-defmacro

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass macro-env ()
  ((macros :initform (make-hash-table :test 'eq) :reader macro-env-table))
  (:documentation "Global environment for macro definitions."))

) ; end eval-when

;;; Lambda List Parsing - Available at compile-time

(eval-when (:compile-toplevel :load-toplevel :execute)

(defstruct lambda-list-info
  "Information about a parsed lambda list."
  required        ; List of required parameter symbols
  optional        ; List of (name default supplied-p) for &optional
  rest            ; Symbol for &rest or nil
  body            ; Symbol for &body or nil
  key-params      ; List of ((keyword-name name) default supplied-p) for &key
  allow-other-keys ; Boolean for &allow-other-keys
  aux             ; List of (name init) for &aux
  environment     ; Symbol for &environment or nil
  )

(defun parse-lambda-list (lambda-list)
  "Parse LAMBDA-LIST into a lambda-list-info structure.
   Supports: required, &optional, &rest, &body, &key, &allow-other-keys, &aux"
  (let ((state :required)
        (required nil)
        (optional nil)
        (rest nil)
        (body nil)
        (key-params nil)
        (allow-other-keys nil)
        (aux nil)
        (environment nil))
    ;; Use a manual loop to handle both proper and improper (dotted) lists.
    ;; A dotted tail like (a b . rest) means 'rest' is an implicit &rest var.
    (do ((current lambda-list (cdr current)))
        ((atom current)
         ;; Dotted rest: non-nil atom at end means implicit &rest variable
         (when current
           (setf rest current)))
      (let ((item (car current)))
        (case item
          (&optional
           (setf state :optional))
          (&rest
           (setf state :rest))
          (&body
           (setf state :body))
          (&key
           (setf state :key))
          (&allow-other-keys
           (when (eq state :key)
             (setf allow-other-keys t)))
          (&aux
           (setf state :aux))
          (&environment
           (setf state :environment))
          (t
           (case state
             (:required
              (push item required))
             (:optional
              (cond ((symbolp item)
                     (push (list item nil nil) optional))
                    ((consp item)
                     (let ((name (first item))
                           (default (if (cdr item) (second item) nil))
                           (supplied-p (if (cddr item) (third item) nil)))
                       (push (list name default supplied-p) optional)))
                    (t (error "Invalid &optional parameter: ~S" item))))
             (:rest
              (setf rest item
                    state :after-rest))
             (:body
              (setf body item
                    state :after-body))
             (:key
              (cond ((symbolp item)
                     (let ((keyword (intern (symbol-name item) :keyword)))
                       (push (list (list keyword item) nil nil) key-params)))
                    ((consp item)
                     (let* ((name-spec (first item))
                            (name (if (consp name-spec)
                                      (progn
                                        (unless (= (length name-spec) 2)
                                          (error "Invalid key name spec: ~S" name-spec))
                                        name-spec)
                                      (list (intern (symbol-name name-spec) :keyword) name-spec)))
                            (default (if (cdr item) (second item) nil))
                            (supplied-p (if (cddr item) (third item) nil)))
                       (push (list name default supplied-p) key-params)))
                    (t (error "Invalid &key parameter: ~S" item))))
             (:aux
              (cond ((symbolp item)
                     (push (list item nil) aux))
                    ((consp item)
                     (push (list (first item) (if (cdr item) (second item) nil)) aux))
                    (t (error "Invalid &aux parameter: ~S" item))))
             (:environment
              (setf environment item
                    state :required))
             ((:after-rest :after-body)
              (error "Unexpected parameter after ~A: ~S"
                     (if (eq state :after-rest) "&rest" "&body") item)))))))
    (make-lambda-list-info
     :required (nreverse required)
     :optional (nreverse optional)
     :rest rest
     :body body
     :key-params (nreverse key-params)
     :allow-other-keys allow-other-keys
     :aux (nreverse aux)
     :environment environment)))

(defun generate-lambda-bindings (lambda-list form-var)
  "Generate LET bindings from LAMBDA-LIST for the macro form in FORM-VAR."
  (let ((info (parse-lambda-list lambda-list))
        (bindings nil)
        (temp-counter 0))
    (labels ((gensym-local (prefix)
               (intern (format nil "~A-~D" prefix (incf temp-counter))
                       (find-package :cl-cc))))
      ;; Process required parameters (skip the macro name)
      (let ((current-arg `(cdr ,form-var)))
        (dolist (req (lambda-list-info-required info))
          (let ((temp (gensym-local "REQ")))
            (push `(,temp (car ,current-arg)) bindings)
            (push `(,req ,temp) bindings)
            (setf current-arg `(cdr ,current-arg))))
        
        ;; Process &optional parameters
        (dolist (opt (lambda-list-info-optional info))
          (destructuring-bind (opt-name default supplied-p) opt
            (let ((temp (gensym-local "OPT"))
                  (supplied-temp (gensym-local "SUPPLIED")))
              (push `(,temp (if ,current-arg (car ,current-arg) ,default)) bindings)
              (push `(,opt-name ,temp) bindings)
              (when supplied-p
                (push `(,supplied-temp (not (null ,current-arg))) bindings)
                (push `(,supplied-p ,supplied-temp) bindings))
              (setf current-arg `(cdr ,current-arg)))))
        
        ;; Process &rest or &body
        (when (or (lambda-list-info-rest info)
                  (lambda-list-info-body info))
          (let ((rest-sym (or (lambda-list-info-rest info)
                              (lambda-list-info-body info))))
            (push `(,rest-sym ,current-arg) bindings)))
        
        ;; Process &key parameters
        (when (lambda-list-info-key-params info)
          (dolist (key-spec (lambda-list-info-key-params info))
            (destructuring-bind ((keyword name) default supplied-p) key-spec
              (let ((found (gensym-local "FOUND"))
                    (val (gensym-local "VAL")))
                (push `(,val (getf ,current-arg ,keyword ,default)) bindings)
                (push `(,name ,val) bindings)
                (when supplied-p
                  (push `(,found (not (eq (getf ,current-arg ,keyword :not-found)
                                         :not-found))) bindings)
                  (push `(,supplied-p ,found) bindings))))))
        
        ;; Process &aux parameters
        (dolist (aux-spec (lambda-list-info-aux info))
          (destructuring-bind (aux-name init) aux-spec
            (push `(,aux-name ,init) bindings))))
      
      (nreverse bindings))))

;;; Macro Environment and Registration - also available at compile-time

(defvar *macro-environment* (make-instance 'macro-env)
  "Global macro environment for macro definitions.")

(defun register-macro (name expander)
  "Register NAME as a macro with EXPANDER function in the global environment."
  (setf (gethash name (macro-env-table *macro-environment*)) expander))

(defun lookup-macro (name &optional env)
  "Look up macro NAME in the global macro environment."
  (declare (ignore env))
  (gethash name (macro-env-table *macro-environment*)))

) ; end eval-when

;;; Macro Expansion

(defun our-macroexpand-1 (form &optional env)
  "Perform a single macro expansion on FORM.
   Returns (VALUES expanded-form expanded-p)."
  (if (and (consp form) (symbolp (car form)))
      (let ((macro-fn (lookup-macro (car form) env)))
        (if macro-fn
            (values (funcall macro-fn form env) t)
            (values form nil)))
      (values form nil)))

(defun our-macroexpand (form &optional env)
  "Fully expand FORM by repeatedly applying macroexpand-1.
   Returns (VALUES expanded-form expanded-p)."
  (loop
    (multiple-value-bind (expanded expanded-p)
        (our-macroexpand-1 form env)
      (unless expanded-p
        (return (values form (not (eq form expanded)))))
      (setf form expanded))))

(defun %expand-quasiquote (template)
  "Transform a quasiquote template into list/cons/append calls.
Handles (cl-cc::unquote x) and (cl-cc::unquote-splicing x) within template."
  (cond
    ;; (cl-cc::unquote x) at top level => just x
    ((and (consp template) (eq (car template) 'cl-cc::unquote))
     (second template))
    ;; A list => process each element for unquote/splicing
    ((consp template)
     (let ((parts nil))
       (dolist (elem template)
         (cond
           ((and (consp elem) (eq (car elem) 'cl-cc::unquote))
            (push (list 'list (second elem)) parts))
           ((and (consp elem) (eq (car elem) 'cl-cc::unquote-splicing))
            (push (second elem) parts))
           (t
            (push (list 'list (%expand-quasiquote elem)) parts))))
       (let ((reversed (nreverse parts)))
         (if (= (length reversed) 1)
             (first reversed)
             (cons 'append reversed)))))
    ;; Atom => quote it
    (t (list 'quote template))))

(defun our-macroexpand-all (form &optional env)
  "Recursively expand all macros in FORM, including in subforms."
  (cond
    ;; Quasiquote — expand before further processing
    ((and (consp form) (eq (car form) 'cl-cc::backquote))
     (our-macroexpand-all (%expand-quasiquote (second form)) env))
    ;; Quote — never recurse into quoted data
    ((and (consp form) (eq (car form) 'quote))
     form)
    ;; General: try macro expansion first, then recurse
    (t
     (multiple-value-bind (exp expanded-p)
         (our-macroexpand-1 form env)
       (if expanded-p
           (our-macroexpand-all exp env)
           (typecase form
             (cons
              (mapcar (lambda (x) (our-macroexpand-all x env)) form))
             (t form)))))))

;;; Macro Definition Macro

(defmacro our-defmacro (name lambda-list &body body)
  "Define NAME as a macro with LAMBDA-LIST and BODY.
   The macro expander function receives (FORM ENV) as arguments."
  (let* ((form-var (gensym "FORM"))
         (env-var (gensym "ENV"))
         (info (parse-lambda-list lambda-list))
         (env-sym (lambda-list-info-environment info)))
    `(register-macro ',name
                     (lambda (,form-var ,env-var)
                       ,@(if env-sym
                             `((let ((,env-sym ,env-var))
                                 (let* ,(generate-lambda-bindings lambda-list form-var)
                                   ,@body)))
                             `((declare (ignore ,env-var))
                               (let* ,(generate-lambda-bindings lambda-list form-var)
                                 ,@body)))))))

;;; Destructuring Bind Implementation

(defun destructure-lambda-list (pattern arg)
  "Destructure ARG according to PATTERN lambda list.
   Returns an alist of (symbol . binding-form) pairs.
   Supports: required, &optional, &rest, &body, &key, &aux"
  (let ((info (parse-lambda-list pattern))
        (bindings nil)
        (arg-var (gensym "ARG"))
        (remaining-var (gensym "REMAINING"))
        (temp-counter 0))
    (labels ((gensym-local (prefix)
               (intern (format nil "~A-~D" prefix (incf temp-counter))
                       (find-package :cl-cc))))
      ;; Bind the argument to a temp variable
      (push (list arg-var arg) bindings)
      
      ;; Process required parameters
      (let ((current-arg arg-var))
        (dolist (req (lambda-list-info-required info))
          (let ((temp (gensym-local "REQ")))
            (push `(,temp (car ,current-arg)) bindings)
            (if (listp req)
                ;; Nested pattern: recursively destructure
                (let ((sub-bindings (destructure-lambda-list req temp)))
                  (dolist (b sub-bindings)
                    (push b bindings)))
                (push `(,req ,temp) bindings))
            (push `(,current-arg (cdr ,current-arg)) bindings)))
        
        ;; Track remaining args for &optional, &rest, &body, &key
        (push `(,remaining-var ,current-arg) bindings))
      
      ;; Process &optional parameters
      (let ((opt-remaining remaining-var))
        (dolist (opt (lambda-list-info-optional info))
          (destructuring-bind (name default supplied-p) opt
            (let ((temp (gensym-local "OPT"))
                  (supplied-temp (gensym-local "SUPPLIED")))
              (push `(,temp (if ,opt-remaining (car ,opt-remaining) ,default)) bindings)
              (push `(,name ,temp) bindings)
              (when supplied-p
                (push `(,supplied-temp (if ,opt-remaining t nil)) bindings)
                (push `(,supplied-p ,supplied-temp) bindings))
              (push `(,opt-remaining (cdr ,opt-remaining)) bindings))))
        
        ;; After &optional, update remaining for &rest/&body/&key
        (setf remaining-var opt-remaining))
      
      ;; Process &rest or &body
      (when (or (lambda-list-info-rest info)
                (lambda-list-info-body info))
        (let ((rest-sym (or (lambda-list-info-rest info)
                            (lambda-list-info-body info))))
          (push `(,rest-sym ,remaining-var) bindings)))
      
      ;; Process &key parameters
      (when (lambda-list-info-key-params info)
        (let ((key-args remaining-var))
          (dolist (key-spec (lambda-list-info-key-params info))
            (destructuring-bind ((keyword name) default supplied-p) key-spec
              (let ((val-temp (gensym-local "VAL")))
                (push `(,val-temp (getf ,key-args ,keyword ,default)) bindings)
                (push `(,name ,val-temp) bindings)
                (when supplied-p
                  (let ((found-temp (gensym-local "FOUND")))
                    (push `(,found-temp (not (eq (getf ,key-args ,keyword :not-found)
                                                 :not-found))) bindings)
                    (push `(,supplied-p ,found-temp) bindings))))))))
      
      ;; Process &aux parameters
      (dolist (aux-spec (lambda-list-info-aux info))
        (destructuring-bind (name init) aux-spec
          (push `(,name ,init) bindings)))
      
      (nreverse bindings))))
