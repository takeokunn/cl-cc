(in-package :cl-cc)

;;; CL-CC Macro System
;;; A complete macro system implementation with:
;;; - Full destructuring-bind for lambda lists
;;; - Environment classes for lexical scoping
;;; - Macro expansion (single and full)
;;; - Built-in macros for bootstrap

;;; Environment Classes - Available at compile-time for our-defmacro

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass macro-env ()
  ((macros :initform (make-hash-table :test 'eq) :reader macro-env-table))
  (:documentation "Environment for macro definitions."))

(defclass compilation-env ()
  ((macros :initform (make-hash-table :test 'eq) :reader env-macros)
   (functions :initform (make-hash-table :test 'eq) :reader env-functions)
   (variables :initform (make-hash-table :test 'eq) :reader env-variables)
   (parent :initarg :parent :initform nil :reader env-parent))
  (:documentation "Full compilation environment with lexical scoping support."))

(defun make-compilation-env (&optional parent)
  "Create a new compilation environment optionally with a PARENT."
  (make-instance 'compilation-env :parent parent))

(defun env-lookup-macro (symbol env)
  "Look up SYMBOL as a macro in ENV, checking parent environments."
  (or (gethash symbol (env-macros env))
      (when (env-parent env)
        (env-lookup-macro symbol (env-parent env)))))

(defun env-lookup-function (symbol env)
  "Look up SYMBOL as a function in ENV, checking parent environments."
  (or (gethash symbol (env-functions env))
      (when (env-parent env)
        (env-lookup-function symbol (env-parent env)))))

(defun env-lookup-variable (symbol env)
  "Look up SYMBOL as a variable in ENV, checking parent environments."
  (or (gethash symbol (env-variables env))
      (when (env-parent env)
        (env-lookup-variable symbol (env-parent env)))))

(defun env-add-macro (symbol macro-fn env)
  "Add SYMBOL with MACRO-FN to ENV."
  (setf (gethash symbol (env-macros env)) macro-fn))

(defun env-add-function (symbol fn env)
  "Add SYMBOL with FN to ENV."
  (setf (gethash symbol (env-functions env)) fn))

(defun env-add-variable (symbol value env)
  "Add SYMBOL with VALUE to ENV."
  (setf (gethash symbol (env-variables env)) value))

) ; end first eval-when

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
        (aux nil))
    (dolist (item lambda-list)
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
           ((:after-rest :after-body)
            (error "Unexpected parameter after ~A: ~S"
                   (if (eq state :after-rest) "&rest" "&body") item))))))
    (make-lambda-list-info
     :required (nreverse required)
     :optional (nreverse optional)
     :rest rest
     :body body
     :key-params (nreverse key-params)
     :allow-other-keys allow-other-keys
     :aux (nreverse aux))))

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
  "Look up macro NAME in ENV (or global environment if nil)."
  (if env
      (env-lookup-macro name env)
      (gethash name (macro-env-table *macro-environment*))))

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

(defun our-macroexpand-all (form &optional env)
  "Recursively expand all macros in FORM, including in subforms."
  (multiple-value-bind (exp expanded-p)
      (our-macroexpand-1 form env)
    (if expanded-p
        ;; Form was a macro call, expand it and continue
        (our-macroexpand-all exp env)
        ;; Not a macro call, recurse into subforms
        (typecase form
          (cons
           (cons (our-macroexpand-all (car form) env)
                 (our-macroexpand-all (cdr form) env)))
          (t form)))))

;;; Macro Definition Macro

(defmacro our-defmacro (name lambda-list &body body)
  "Define NAME as a macro with LAMBDA-LIST and BODY.
   The macro expander function receives (FORM ENV) as arguments."
  (let ((form-var (gensym "FORM"))
        (env-var (gensym "ENV")))
    `(register-macro ',name
                     (lambda (,form-var ,env-var)
                       (declare (ignore ,env-var))
                       (let* ,(generate-lambda-bindings lambda-list form-var)
                         ,@body)))))

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

;;; Built-in Macros for Bootstrap

;; WHEN macro
(our-defmacro when (test &body body)
  `(if ,test (progn ,@body) nil))

;; UNLESS macro
(our-defmacro unless (test &body body)
  `(if ,test nil (progn ,@body)))

;; COND macro
(our-defmacro cond (&rest clauses)
  (if (null clauses)
      nil
      (let ((clause (car clauses)))
        (if (null (cdr clause))
            ;; Single expression clause: (cond (x) ...) => (or x (cond ...))
            `(or ,(car clause) (cond ,@(cdr clauses)))
            `(if ,(car clause)
                 (progn ,@(cdr clause))
                 (cond ,@(cdr clauses)))))))

;; AND macro
(our-defmacro and (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(if ,(car args) (and ,@(cdr args)) nil))))

;; OR macro
(our-defmacro or (&rest args)
  (cond ((null args) nil)
        ((null (cdr args)) (car args))
        (t (let ((tmp (gensym "OR")))
             `(let ((,tmp ,(car args)))
                (if ,tmp ,tmp (or ,@(cdr args))))))))

;; LET* macro (recursive let)
(our-defmacro let* (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      (let ((binding (car bindings)))
        `(let (,binding)
           (let* ,(cdr bindings) ,@body)))))

;; DEFUN macro
(our-defmacro defun (name params &body body)
  `(setf (fdefinition ',name)
         (lambda ,params ,@body)))

;; PROG1 macro
(our-defmacro prog1 (first-form &body body)
  (let ((result (gensym "RESULT")))
    `(let ((,result ,first-form))
       ,@body
       ,result)))

;; PROG2 macro
(our-defmacro prog2 (first-form second-form &body body)
  (let ((result (gensym "RESULT")))
    `(progn
       ,first-form
       (let ((,result ,second-form))
         ,@body
         ,result))))

;; CHECK-TYPE macro
(our-defmacro check-type (place type &optional type-string)
  `(unless (typep ,place ',type)
     (error ,(if type-string
                 (format nil "The value of ~S is not of type ~A" place type-string)
                 (format nil "The value of ~S is not of type ~S" place type)))))

;; SETF macro (simplified)
(our-defmacro setf (place value)
  (cond
    ((symbolp place)
     `(setq ,place ,value))
    ((and (consp place) (eq (car place) 'car))
     (let ((v (gensym "V")))
       `(let ((,v ,value))
          (rplaca ,(second place) ,v)
          ,v)))
    ((and (consp place) (eq (car place) 'cdr))
     (let ((v (gensym "V")))
       `(let ((,v ,value))
          (rplacd ,(second place) ,v)
          ,v)))
    ((and (consp place) (eq (car place) 'first))
     (let ((v (gensym "V")))
       `(let ((,v ,value))
          (rplaca ,(second place) ,v)
          ,v)))
    ((and (consp place) (eq (car place) 'rest))
     (let ((v (gensym "V")))
       `(let ((,v ,value))
          (rplacd ,(second place) ,v)
          ,v)))
    ((and (consp place) (eq (car place) 'nth))
     (let ((v (gensym "V")))
       `(let ((,v ,value))
          (rplaca (nthcdr ,(second place) ,(third place)) ,v)
          ,v)))
    ((and (consp place) (eq (car place) 'gethash))
     ;; (setf (gethash key table) value) — handled by parser, but macro fallback
     `(setf-gethash ,(second place) ,(third place) ,value))
    ((and (consp place) (eq (car place) 'slot-value))
     ;; (setf (slot-value obj slot) value) — handled by parser, but macro fallback
     `(setf-slot-value ,(second place) ,(third place) ,value))
    (t
     (error "SETF: Unsupported place ~S" place))))

;; PSETQ macro (parallel setq)
(our-defmacro psetq (&rest pairs)
  (when pairs
    (let ((bindings (loop for (var val) on pairs by #'cddr
                          collect (list (gensym (symbol-name var)) val)))
          (vars (loop for (var) on pairs by #'cddr collect var)))
      `(let ,bindings
         ,@(mapcar (lambda (var binding)
                     `(setq ,var ,(car binding)))
                   vars bindings)
         nil))))

;; MULTIPLE-VALUE-BIND macro
(our-defmacro multiple-value-bind (vars form &body body)
  `(multiple-value-call
    (lambda ,vars ,@body)
    ,form))

;; MULTIPLE-VALUE-SETQ macro
(our-defmacro multiple-value-setq (vars form)
  (let ((temp (gensym "MVS")))
    `(let ((,temp (multiple-value-list ,form)))
       ,@(mapcar (lambda (var i)
                   `(setq ,var (nth ,i ,temp)))
                 vars
                 (loop for i below (length vars) collect i))
        (car ,temp))))

;; MULTIPLE-VALUE-LIST macro
(our-defmacro multiple-value-list (form)
  (let ((temp (gensym "MVL"))
        (acc (gensym "ACC"))
        (list-builder (gensym "LB")))
    `(let ((,acc nil))
       (multiple-value-call
        (lambda (&rest ,temp)
          (dolist (,list-builder ,temp)
            (push ,list-builder ,acc))
          (nreverse ,acc)))
        ,form)))

;;; Control Flow Macros

;; DOLIST macro
(our-defmacro dolist (binding-spec &body body)
  "Iterate over a list, binding each element to a variable in BODY.
   BINDING-SPEC is (var list-form &optional result-form)."
  (let ((var (first binding-spec))
        (list-form (second binding-spec))
        (result-form (third binding-spec))
        (list-var (gensym "LIST"))
        (start-tag (gensym "START"))
        (end-tag (gensym "END")))
    `(block nil
       (let ((,list-var ,list-form)
             (,var nil))
         (tagbody
            ,start-tag
            (if (null ,list-var)
                (go ,end-tag))
            (setq ,var (car ,list-var))
            (setq ,list-var (cdr ,list-var))
            ,@body
            (go ,start-tag)
            ,end-tag)
         ,result-form))))

;; DOTIMES macro
(our-defmacro dotimes (binding-spec &body body)
  "Iterate count times, binding a variable to 0, 1, 2, ... count-1 in BODY.
   BINDING-SPEC is (var count-form &optional result-form)."
  (let ((var (first binding-spec))
        (count-form (second binding-spec))
        (result-form (third binding-spec))
        (count-var (gensym "COUNT"))
        (start-tag (gensym "START"))
        (end-tag (gensym "END")))
    `(block nil
       (let ((,var 0)
             (,count-var ,count-form))
         (tagbody
            ,start-tag
            (if (>= ,var ,count-var)
                (go ,end-tag))
            ,@body
            (setq ,var (+ ,var 1))
            (go ,start-tag)
            ,end-tag)
         ,result-form))))

;; DO macro
(our-defmacro do (var-clauses end-clause &body body)
  "General iteration construct.
   VAR-CLAUSES: ((var init step) ...)
   END-CLAUSE: (test result ...)
   Evaluates BODY with VAR bindings, then tests END-CLAUSE.
   If TEST is true, returns RESULT; otherwise, evaluates STEP forms and repeats."
  (let ((start-tag (gensym "START"))
        (end-tag (gensym "END"))
        (vars (mapcar #'car var-clauses))
        (inits (mapcar #'(lambda (c) (if (consp (cdr c)) (cadr c) nil)) var-clauses))
        (steps (mapcar #'(lambda (c) (if (cddr c) (caddr c) (car c))) var-clauses))
        (test (car end-clause))
        (results (cdr end-clause)))
    `(block nil
       (let ,(mapcar #'list vars inits)
         (tagbody
            ,start-tag
            (if ,test (go ,end-tag))
            ,@body
            (psetq ,@(mapcan #'(lambda (var step) (list var step)) vars steps))
            (go ,start-tag)
            ,end-tag)
         (progn ,@results)))))

;; DO* macro (sequential binding version)
(our-defmacro do* (var-clauses end-clause &body body)
  "General iteration construct with sequential binding.
   VAR-CLAUSES: ((var init step) ...)
   END-CLAUSE: (test result ...)
   Like DO but bindings are sequential (like LET*)."
  (let ((start-tag (gensym "START"))
        (end-tag (gensym "END"))
        (vars (mapcar #'car var-clauses))
        (inits (mapcar #'(lambda (c) (if (consp (cdr c)) (cadr c) nil)) var-clauses))
        (steps (mapcar #'(lambda (c) (if (cddr c) (caddr c) (car c))) var-clauses))
        (test (car end-clause))
        (results (cdr end-clause)))
    `(block nil
       (let* ,(mapcar #'list vars inits)
         (tagbody
            ,start-tag
            (if ,test (go ,end-tag))
            ,@body
            ,@(mapcar #'(lambda (var step) `(setq ,var ,step)) vars steps)
            (go ,start-tag)
            ,end-tag)
         (progn ,@results)))))

;; CASE macro
(our-defmacro case (keyform &body cases)
  "Match KEYFORM against CASES.
   Each case is (key body...) or (otherwise body...) or (t body...).
   Keys are compared with EQL (not evaluated)."
  (let ((key-var (gensym "KEY")))
    `(let ((,key-var ,keyform))
       ,(labels ((build-case (cases)
                   (if (null cases)
                       nil
                       (let ((case (car cases))
                             (rest (cdr cases)))
                         (let ((keys (car case))
                               (body (cdr case)))
                           (cond
                             ((or (eq keys 'otherwise) (eq keys 't))
                              `(progn ,@body))
                             ((listp keys)
                              `(if (or ,@(mapcar #'(lambda (k) `(eql ,key-var ',k)) keys))
                                   (progn ,@body)
                                   ,(build-case rest)))
                             (t
                              `(if (eql ,key-var ',keys)
                                   (progn ,@body)
                                   ,(build-case rest)))))))))
           (build-case cases)))))

;; TYPECASE macro
(our-defmacro typecase (keyform &body cases)
  "Match KEYFORM against TYPE-CASES.
   Each case is (type body...) or (otherwise body...) or (t body...).
   Types are checked with TYPEP."
  (let ((key-var (gensym "KEY")))
    `(let ((,key-var ,keyform))
       ,(labels ((build-typecase (cases)
                   (if (null cases)
                       nil
                       (let ((case (car cases))
                             (rest (cdr cases)))
                         (let ((type (car case))
                               (body (cdr case)))
                           (cond
                             ((or (eq type 'otherwise) (eq type 't))
                              `(progn ,@body))
                             (t
                              `(if (typep ,key-var ',type)
                                   (progn ,@body)
                                   ,(build-typecase rest)))))))))
           (build-typecase cases)))))

;;; LOOP Macro (Simplified Implementation)

;; Helper to parse loop clauses
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun loop-kw-p (sym name)
    "Check if SYM is a loop keyword with the given NAME (case-insensitive string comparison)."
    (and (symbolp sym) (string-equal (symbol-name sym) name)))

  (defun loop-kw-member-p (sym)
    "Check if SYM is any recognized loop keyword."
    (and (symbolp sym)
         (member (symbol-name sym)
                 '("FOR" "COLLECT" "COLLECTING" "SUM" "SUMMING" "COUNT" "COUNTING"
                   "MAXIMIZE" "MAXIMIZING" "MINIMIZE" "MINIMIZING"
                   "WHILE" "UNTIL" "WHEN" "IF" "UNLESS" "DO" "INITIALLY" "FINALLY" "END"
                   "REPEAT" "WITH" "APPEND" "APPENDING" "NCONC" "NCONCING"
                   "BEING" "USING" "=" "ALWAYS" "NEVER" "THEREIS" "BY" "INTO")
                 :test #'string-equal)))

  (defun parse-loop-clauses (clauses)
    "Parse LOOP clauses and return iteration specs and body forms."
    (let ((iterations nil)
          (body-forms nil)
          (accumulations nil)
          (conditions nil)
          (finally nil)
          (initially nil)
          (remaining clauses)
          (pending-filter nil))
      (loop
        (when (null remaining) (return))
        (let ((clause (car remaining)))
          (cond
            ;; FOR iterations
            ((loop-kw-p clause "FOR")
             (pop remaining)
             (let* ((var (pop remaining))
                    (kw (pop remaining))
                    (spec (list :for var)))
               (cond
                 ((loop-kw-p kw "FROM")
                  (setf (getf spec :type) :from)
                  (let ((from (pop remaining)))
                    (setf (getf spec :from) from)
                    (let ((next-kw (car remaining)))
                      (cond
                        ((loop-kw-p next-kw "TO")
                         (pop remaining)
                         (setf (getf spec :to) (pop remaining)))
                        ((loop-kw-p next-kw "BELOW")
                         (pop remaining)
                         (setf (getf spec :below) (pop remaining)))
                        ((loop-kw-p next-kw "UPTO")
                         (pop remaining)
                         (setf (getf spec :to) (pop remaining)))))
                    ;; Optional BY step for numeric
                    (when (and remaining (loop-kw-p (car remaining) "BY"))
                      (pop remaining)
                      (setf (getf spec :by) (pop remaining)))))
                 ((loop-kw-p kw "IN")
                  (setf (getf spec :type) :in)
                  (setf (getf spec :in) (pop remaining))
                  ;; Optional BY step function for IN
                  (when (and remaining (loop-kw-p (car remaining) "BY"))
                    (pop remaining)
                    (setf (getf spec :by) (pop remaining))))
                 ((loop-kw-p kw "ON")
                  (setf (getf spec :type) :on)
                  (setf (getf spec :on) (pop remaining))
                  ;; Optional BY step function for ON
                  (when (and remaining (loop-kw-p (car remaining) "BY"))
                    (pop remaining)
                    (setf (getf spec :by) (pop remaining))))
                 ((loop-kw-p kw "ACROSS")
                  (setf (getf spec :type) :across)
                  (setf (getf spec :across) (pop remaining)))
                 ;; FOR var BEING THE HASH-KEYS/HASH-VALUES OF table [USING (HASH-VALUE/HASH-KEY other-var)]
                 ((loop-kw-p kw "BEING")
                  ;; Skip "THE" if present
                  (when (and remaining (loop-kw-p (car remaining) "THE"))
                    (pop remaining))
                  (let ((hash-kw (pop remaining)))
                    (cond
                      ((or (loop-kw-p hash-kw "HASH-KEYS") (loop-kw-p hash-kw "HASH-KEY"))
                       (setf (getf spec :type) :hash-keys))
                      ((or (loop-kw-p hash-kw "HASH-VALUES") (loop-kw-p hash-kw "HASH-VALUE"))
                       (setf (getf spec :type) :hash-values))
                      (t (error "Expected HASH-KEYS or HASH-VALUES after BEING THE, got ~S" hash-kw)))
                    ;; Expect OF/IN
                    (let ((of-kw (pop remaining)))
                      (unless (or (loop-kw-p of-kw "OF") (loop-kw-p of-kw "IN"))
                        (error "Expected OF or IN after HASH-KEYS/HASH-VALUES, got ~S" of-kw)))
                    (setf (getf spec :hash-table) (pop remaining))
                    ;; Optional USING (HASH-VALUE val) or (HASH-KEY key)
                    (when (and remaining (loop-kw-p (car remaining) "USING"))
                      (pop remaining)
                      (let ((using-spec (pop remaining)))
                        (when (and (consp using-spec) (>= (length using-spec) 2))
                          (setf (getf spec :using-type)
                                (cond ((loop-kw-p (car using-spec) "HASH-VALUE") :hash-value)
                                      ((loop-kw-p (car using-spec) "HASH-KEY") :hash-key)
                                      (t (error "Expected HASH-VALUE or HASH-KEY in USING clause"))))
                          (setf (getf spec :using-var) (cadr using-spec))))))))
               (push spec iterations)))
            ;; Accumulation clauses (with optional INTO var and pending filter)
            ((or (loop-kw-p clause "COLLECT") (loop-kw-p clause "COLLECTING"))
             (pop remaining)
             (let ((form (pop remaining))
                   (into-var nil)
                   (filter pending-filter))
               (setf pending-filter nil)
               (when (and remaining (loop-kw-p (car remaining) "INTO"))
                 (pop remaining)
                 (setf into-var (pop remaining)))
               (push (list :collect form into-var filter) accumulations)))
            ((or (loop-kw-p clause "SUM") (loop-kw-p clause "SUMMING"))
             (pop remaining)
             (let ((form (pop remaining))
                   (into-var nil)
                   (filter pending-filter))
               (setf pending-filter nil)
               (when (and remaining (loop-kw-p (car remaining) "INTO"))
                 (pop remaining)
                 (setf into-var (pop remaining)))
               (push (list :sum form into-var filter) accumulations)))
            ((or (loop-kw-p clause "COUNT") (loop-kw-p clause "COUNTING"))
             (pop remaining)
             (let ((form (pop remaining))
                   (into-var nil)
                   (filter pending-filter))
               (setf pending-filter nil)
               (when (and remaining (loop-kw-p (car remaining) "INTO"))
                 (pop remaining)
                 (setf into-var (pop remaining)))
               (push (list :count form into-var filter) accumulations)))
            ((or (loop-kw-p clause "MAXIMIZE") (loop-kw-p clause "MAXIMIZING"))
             (pop remaining)
             (let ((form (pop remaining))
                   (into-var nil)
                   (filter pending-filter))
               (setf pending-filter nil)
               (when (and remaining (loop-kw-p (car remaining) "INTO"))
                 (pop remaining)
                 (setf into-var (pop remaining)))
               (push (list :maximize form into-var filter) accumulations)))
            ((or (loop-kw-p clause "MINIMIZE") (loop-kw-p clause "MINIMIZING"))
             (pop remaining)
             (let ((form (pop remaining))
                   (into-var nil)
                   (filter pending-filter))
               (setf pending-filter nil)
               (when (and remaining (loop-kw-p (car remaining) "INTO"))
                 (pop remaining)
                 (setf into-var (pop remaining)))
               (push (list :minimize form into-var filter) accumulations)))
            ;; APPEND/NCONC accumulation
            ((or (loop-kw-p clause "APPEND") (loop-kw-p clause "APPENDING"))
             (pop remaining)
             (let ((form (pop remaining))
                   (into-var nil)
                   (filter pending-filter))
               (setf pending-filter nil)
               (when (and remaining (loop-kw-p (car remaining) "INTO"))
                 (pop remaining)
                 (setf into-var (pop remaining)))
               (push (list :append form into-var filter) accumulations)))
            ((or (loop-kw-p clause "NCONC") (loop-kw-p clause "NCONCING"))
             (pop remaining)
             (let ((form (pop remaining))
                   (into-var nil)
                   (filter pending-filter))
               (setf pending-filter nil)
               (when (and remaining (loop-kw-p (car remaining) "INTO"))
                 (pop remaining)
                 (setf into-var (pop remaining)))
               (push (list :nconc form into-var filter) accumulations)))
            ;; WITH clause (auxiliary variable binding)
            ((loop-kw-p clause "WITH")
             (pop remaining)
             (let* ((var (pop remaining))
                    (val nil))
               ;; Check for = initializer
               (when (and remaining (loop-kw-p (car remaining) "="))
                 (pop remaining)
                 (setf val (pop remaining)))
               (push (list :with var val) iterations)))
            ;; Condition clauses
            ((loop-kw-p clause "WHILE")
             (pop remaining)
             (push (list :while (pop remaining)) conditions))
            ((loop-kw-p clause "UNTIL")
             (pop remaining)
             (push (list :until (pop remaining)) conditions))
            ((or (loop-kw-p clause "WHEN") (loop-kw-p clause "IF"))
             (pop remaining)
             (setf pending-filter (list :when (pop remaining))))
            ((loop-kw-p clause "UNLESS")
             (pop remaining)
             (setf pending-filter (list :unless (pop remaining))))
            ;; ALWAYS/NEVER/THEREIS termination tests
            ((loop-kw-p clause "ALWAYS")
             (pop remaining)
             (push (list :always (pop remaining)) conditions))
            ((loop-kw-p clause "NEVER")
             (pop remaining)
             (push (list :never (pop remaining)) conditions))
            ((loop-kw-p clause "THEREIS")
             (pop remaining)
             (push (list :thereis (pop remaining)) conditions))
            ;; REPEAT clause
            ((loop-kw-p clause "REPEAT")
             (pop remaining)
             (let ((count-form (pop remaining))
                   (count-var (gensym "COUNT")))
               (push (list :repeat count-var count-form) iterations)))
            ;; BODY forms
            ((loop-kw-p clause "DO")
             (pop remaining)
             (let ((do-forms nil)
                   (filter pending-filter))
               (setf pending-filter nil)
               (loop while (and remaining (not (loop-kw-member-p (car remaining))))
                     do (push (pop remaining) do-forms))
               (setf do-forms (nreverse do-forms))
               (if filter
                   (let ((wrapper (if (eq (car filter) :when) 'when 'unless)))
                     (push (list* wrapper (cadr filter) do-forms) body-forms))
                   (dolist (f do-forms) (push f body-forms)))))
            ;; INITIALLY/FINALLY
            ((loop-kw-p clause "INITIALLY")
             (pop remaining)
             (loop while (and remaining (not (loop-kw-member-p (car remaining))))
                   do (push (pop remaining) initially)))
            ((loop-kw-p clause "FINALLY")
             (pop remaining)
             (loop while (and remaining (not (loop-kw-member-p (car remaining))))
                   do (push (pop remaining) finally)))
            ;; Default: treat as body form
            (t
             (push (pop remaining) body-forms)))))
      (list :iterations (nreverse iterations)
            :body body-forms              ; NOT nreversed — loop macro will nreverse
            :accumulations (nreverse accumulations)
            :conditions (nreverse conditions)
            :initially initially          ; NOT nreversed — loop macro will nreverse
            :finally finally)))           ; NOT nreversed — loop macro will nreverse
)

;; LOOP macro
(our-defmacro loop (&rest clauses)
  "Simplified LOOP macro supporting basic iteration forms.
   Supports: for ... from ... to/below, for ... in, for ... on
             collect, sum, count, maximize, minimize
             while, until, do, initially, finally"
  (let* ((parsed (parse-loop-clauses clauses))
         (iterations (getf parsed :iterations))
         (body (getf parsed :body))
         (accumulations (getf parsed :accumulations))
         (conditions (getf parsed :conditions))
         (initially (getf parsed :initially))
         (finally (getf parsed :finally)))
    (let ((start-tag (gensym "START"))
          (end-tag (gensym "END"))
          (loop-block (gensym "LOOP"))
          (bindings nil)
          (step-forms nil)
          (end-tests nil)
          (pre-body nil)
          (acc-vars nil)
          (init-acc nil)
          (result-form nil))
      ;; Process iterations
      (dolist (iter iterations)
        (cond
          ;; REPEAT n: (:repeat count-var count-form)
          ((eq (car iter) :repeat)
           (let ((count-var (second iter))
                 (count-form (third iter)))
             (push (list count-var count-form) bindings)
             (push (list '<= count-var 0) end-tests)
             (push (list 'setq count-var (list '- count-var 1)) step-forms)))
          ;; WITH auxiliary binding: (:with var val)
          ((eq (car iter) :with)
           (let ((var (second iter))
                 (val (third iter)))
             (push (list var val) bindings)))
          ;; FOR-based iterations
          (t
           (let ((var (getf iter :for))
                 (iter-type (getf iter :type)))
             (case iter-type
               ;; FOR var FROM ... TO/BELOW [BY step]
               (:from
                (let ((from (getf iter :from))
                      (to (getf iter :to))
                      (below (getf iter :below))
                      (by-form (getf iter :by)))
                  (push (list var from) bindings)
                  (if by-form
                      (push (list 'setq var (list '+ var by-form)) step-forms)
                      (push (list 'setq var (list '+ var 1)) step-forms))
                  (if to
                      (push (list '> var to) end-tests)
                      (push (list '>= var below) end-tests))))
               ;; FOR var IN list [BY step-fn]
               (:in
                (let ((list-var (gensym (symbol-name var)))
                      (list-form (getf iter :in))
                      (by-fn (getf iter :by)))
                  (push (list list-var list-form) bindings)
                  (push (list var (list 'car list-var)) bindings)
                  (push (list 'null list-var) end-tests)
                  (if by-fn
                      (push (list 'setq list-var (list 'funcall by-fn list-var)) step-forms)
                      (push (list 'setq list-var (list 'cdr list-var)) step-forms))
                  (push (list 'setq var (list 'car list-var)) step-forms)))
               ;; FOR var ON list [BY step-fn]
               (:on
                (let ((list-form (getf iter :on))
                      (by-fn (getf iter :by)))
                  (push (list var list-form) bindings)
                  (if by-fn
                      (push (list 'setq var (list 'funcall by-fn var)) step-forms)
                      (push (list 'setq var (list 'cdr var)) step-forms))
                  (push (list 'null var) end-tests)))
               ;; FOR var ACROSS vector
               (:across
                (let ((vec-var (gensym "VEC"))
                      (idx-var (gensym "IDX"))
                      (len-var (gensym "LEN"))
                      (vec-form (getf iter :across)))
                  (push (list vec-var vec-form) bindings)
                  (push (list len-var (list 'length vec-var)) bindings)
                  (push (list idx-var 0) bindings)
                  (push (list var nil) bindings)
                  (push (list '>= idx-var len-var) end-tests)
                  ;; Set var from current index before body
                  (push (list 'setq var (list 'aref vec-var idx-var)) pre-body)
                  ;; Step: increment index
                  (push (list 'setq idx-var (list '+ idx-var 1)) step-forms)))
               ;; FOR var BEING THE HASH-KEYS OF table [USING (HASH-VALUE val)]
               (:hash-keys
                (let ((keys-var (gensym "KEYS"))
                      (table-form (getf iter :hash-table))
                      (ht-var (gensym "HT"))
                      (using-type (getf iter :using-type))
                      (using-var (getf iter :using-var)))
                  (push (list ht-var table-form) bindings)
                  (push (list keys-var (list 'hash-table-keys ht-var)) bindings)
                  (push (list var (list 'car keys-var)) bindings)
                  (push (list 'null keys-var) end-tests)
                  ;; If USING (HASH-VALUE val), bind val from gethash
                  (when (and using-var (eq using-type :hash-value))
                    (push (list using-var nil) bindings)
                    (push (list 'setq using-var (list 'gethash var ht-var)) pre-body))
                  (push (list 'setq keys-var (list 'cdr keys-var)) step-forms)
                  (push (list 'setq var (list 'car keys-var)) step-forms)))
               ;; FOR var BEING THE HASH-VALUES OF table [USING (HASH-KEY key)]
               (:hash-values
                (let ((vals-var (gensym "VALS"))
                      (table-form (getf iter :hash-table))
                      (ht-var (gensym "HT"))
                      (using-type (getf iter :using-type))
                      (using-var (getf iter :using-var)))
                  (push (list ht-var table-form) bindings)
                  (push (list vals-var (list 'hash-table-values ht-var)) bindings)
                  (push (list var (list 'car vals-var)) bindings)
                  (push (list 'null vals-var) end-tests)
                  ;; If USING (HASH-KEY key), we need keys list too
                  (when (and using-var (eq using-type :hash-key))
                    (let ((keys-var (gensym "KEYS")))
                      (push (list keys-var (list 'hash-table-keys ht-var)) bindings)
                      (push (list using-var nil) bindings)
                      (push (list 'setq using-var (list 'car keys-var)) pre-body)
                      (push (list 'setq keys-var (list 'cdr keys-var)) step-forms)))
                  (push (list 'setq vals-var (list 'cdr vals-var)) step-forms)
                  (push (list 'setq var (list 'car vals-var)) step-forms))))))))
      ;; Process accumulations
      (dolist (acc accumulations)
        (let* ((acc-type (car acc))
               (acc-form (cadr acc))
               (into-var (caddr acc))
               (filter (cadddr acc))
               (acc-var (or into-var (gensym "ACC"))))
          ;; Only add to implicit result for unnamed accumulators
          (unless into-var (push acc-var acc-vars))
          ;; Generate the accumulation body form
          (let ((acc-body
                  (case acc-type
                    (:collect
                     (push (list acc-var nil) bindings)
                     (unless into-var
                       (push (list 'nreverse acc-var) result-form))
                     (list 'setq acc-var (list 'cons acc-form acc-var)))
                    (:sum
                     (push (list acc-var 0) bindings)
                     (list 'setq acc-var (list '+ acc-var acc-form)))
                    (:count
                     (push (list acc-var 0) bindings)
                     (list 'when acc-form (list 'setq acc-var (list '+ acc-var 1))))
                    (:maximize
                     (push (list acc-var nil) bindings)
                     (list 'if acc-var
                           (list 'when (list '> acc-form acc-var)
                                 (list 'setq acc-var acc-form))
                           (list 'setq acc-var acc-form)))
                    (:minimize
                     (push (list acc-var nil) bindings)
                     (list 'if acc-var
                           (list 'when (list '< acc-form acc-var)
                                 (list 'setq acc-var acc-form))
                           (list 'setq acc-var acc-form)))
                    (:append
                     (push (list acc-var nil) bindings)
                     (list 'setq acc-var (list 'append acc-var acc-form)))
                    (:nconc
                     (push (list acc-var nil) bindings)
                     (list 'setq acc-var (list 'nconc acc-var acc-form))))))
            ;; Wrap in filter condition if present
            (if filter
                (let ((wrapper (if (eq (car filter) :when) 'when 'unless)))
                  (push (list wrapper (cadr filter) acc-body) body))
                (push acc-body body)))))
      ;; Process conditions (flow-control only: while/until/always/never/thereis)
      ;; when/unless filters are now handled per-accumulation/per-do above
      (dolist (cond conditions)
        (let ((cond-type (car cond))
              (cond-form (cadr cond)))
          (case cond-type
            (:while
             (push (list 'unless cond-form (list 'go end-tag)) body))
            (:until
             (push (list 'when cond-form (list 'go end-tag)) body))
            (:always
             (push (list 'unless cond-form '(return nil)) body))
            (:never
             (push (list 'when cond-form '(return nil)) body))
            (:thereis
             (let ((thereis-var (gensym "THEREIS")))
               (push (list 'let (list (list thereis-var cond-form))
                           (list 'when thereis-var (list 'return thereis-var))) body))))))
      ;; Build the loop
      `(block nil
         (let* ,(nreverse bindings)
           ,@(nreverse initially)
           (tagbody
              ,start-tag
              ;; End tests
              ,@(mapcar (lambda (test) `(when ,test (go ,end-tag))) (nreverse end-tests))
              ;; Pre-body forms (e.g. set loop var from vector index)
              ,@(nreverse pre-body)
              ;; Body forms
              ,@(nreverse body)
              ;; Step forms
              ,@(nreverse step-forms)
              (go ,start-tag)
              ,end-tag)
           ,@(nreverse finally)
           ;; Return accumulation result if any
           ,@(let ((has-always-never (some (lambda (c) (member (car c) '(:always :never))) conditions)))
               (cond (result-form
                      (list (if (= (length result-form) 1)
                                (car result-form)
                                (cons 'values result-form))))
                     (acc-vars
                      (list (cons 'values (nreverse acc-vars))))
                     ;; ALWAYS/NEVER: return T if loop completes without early exit
                     (has-always-never '(t))
                     (t nil))))))))

;;; List Mutation Macros

;; PUSH macro
(our-defmacro push (value place)
  "Push VALUE onto the front of list PLACE."
  `(setq ,place (cons ,value ,place)))

;; POP macro
(our-defmacro pop (place)
  "Remove and return the first element of list PLACE."
  (let ((tmp (gensym "TMP")))
    `(let ((,tmp (car ,place)))
       (setq ,place (cdr ,place))
       ,tmp)))

;; INCF macro
(our-defmacro incf (place &optional (delta 1))
  "Increment PLACE by DELTA (default 1)."
  `(setq ,place (+ ,place ,delta)))

;; DECF macro
(our-defmacro decf (place &optional (delta 1))
  "Decrement PLACE by DELTA (default 1)."
  `(setq ,place (- ,place ,delta)))

;; RETURN macro (return from nil block)
(our-defmacro return (&optional value)
  "Return VALUE from the nearest NIL block."
  `(return-from nil ,value))

;; ROTATEF macro
(our-defmacro rotatef (a b)
  "Swap the values of A and B."
  (let ((tmp (gensym "TMP")))
    `(let ((,tmp ,a))
       (setq ,a ,b)
       (setq ,b ,tmp)
       nil)))

;; DESTRUCTURING-BIND macro
(our-defmacro destructuring-bind (pattern expr &body body)
  "Bind variables in PATTERN to corresponding parts of EXPR.
   Supports: required, &optional, &rest, &body, &key, &aux."
  (let ((expr-var (gensym "EXPR")))
    `(let ((,expr-var ,expr))
       (let* ,(destructure-lambda-list pattern expr-var)
         ,@body))))

;; PROG macro - let + tagbody + block
(our-defmacro prog (vars &body body)
  "Establish bindings with LET, wrap body in BLOCK NIL + TAGBODY."
  `(block nil
     (let ,vars
       (tagbody ,@body))))

;; PROG* macro - let* + tagbody + block
(our-defmacro prog* (vars &body body)
  "Like PROG but with sequential bindings (LET*)."
  `(block nil
     (let* ,vars
       (tagbody ,@body))))

;; WITH-SLOTS macro - bind slot accessors as local variables
(our-defmacro with-slots (slot-names instance &body body)
  "Evaluate BODY with SLOT-NAMES bound to slot values of INSTANCE."
  (let ((inst-var (gensym "INST")))
    `(let ((,inst-var ,instance))
       (let ,(mapcar (lambda (slot)
                       (if (listp slot)
                           `(,(first slot) (slot-value ,inst-var ',(second slot)))
                           `(,slot (slot-value ,inst-var ',slot))))
                     slot-names)
         ,@body))))

;; NTH-VALUE macro - extract nth return value from multiple-values form
(our-defmacro nth-value (n form)
  "Return the Nth value from a multiple-values-producing FORM."
  (let ((vals-var (gensym "VALS")))
    `(let ((,vals-var (multiple-value-list ,form)))
       (nth ,n ,vals-var))))

;; ECASE macro (exhaustive case)
(our-defmacro ecase (keyform &body cases)
  "Like CASE but signals error if no case matches."
  (let ((key-var (gensym "KEY")))
    `(let ((,key-var ,keyform))
       (case ,key-var
         ,@cases
         (otherwise (error "ECASE: no matching clause"))))))

;; ETYPECASE macro (exhaustive typecase)
(our-defmacro etypecase (keyform &body cases)
  "Like TYPECASE but signals error if no case matches."
  (let ((key-var (gensym "KEY")))
    `(let ((,key-var ,keyform))
       (typecase ,key-var
         ,@cases
         (otherwise (error "ETYPECASE: no matching clause"))))))

;;; Test/Debug Utilities

(defun macroexpand-test (form)
  "Test macro expansion on FORM and print results."
  (let ((result (our-macroexpand-1 form)))
    (format t "Form: ~S~%" form)
    (format t "Expanded: ~S~%" result)
    result))

(defun full-macroexpand-test (form)
  "Test full macro expansion on FORM and print results."
  (let ((result (our-macroexpand form)))
    (format t "Form: ~S~%" form)
    (format t "Fully expanded: ~S~%" result)
    result))
