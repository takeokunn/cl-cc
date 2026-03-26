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
    ((and (consp place) (member (car place) '(car first)))
     (let ((v (gensym "V")))
       `(let ((,v ,value))
          (rplaca ,(second place) ,v)
          ,v)))
    ((and (consp place) (member (car place) '(cdr rest)))
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
    ((and (consp place) (eq (car place) 'getf))
     ;; (setf (getf plist indicator) value) — rebuild plist with new value
     (let ((v (gensym "V")))
       `(let ((,v ,value))
          (setq ,(second place) (rt-plist-put ,(second place) ,(third place) ,v))
          ,v)))
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
;; Evaluates FORM (capturing its values side-effect), then drains the internal
;; values buffer via %values-to-list (a VM intrinsic).
(our-defmacro multiple-value-list (form)
  "Collect all values of FORM into a list."
  (let ((acc (gensym "MVL-ACC")))
    `(let ((,acc nil))
       (multiple-value-call (lambda (&rest vals)
                              (dolist (v vals) (push v ,acc)))
                            ,form)
       (nreverse ,acc))))

;; LIST macro — expands to nested CONS calls; avoids a VM list instruction.
(our-defmacro list (&rest args)
  (if (null args)
      nil
      (reduce (lambda (x acc) `(cons ,x ,acc))
              args :from-end t :initial-value nil)))

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

(defun expand-do-form (var-clauses end-clause body let-kw make-step-forms)
  "Shared expansion for DO and DO*. LET-KW is 'let or 'let*.
MAKE-STEP-FORMS is (vars steps) -> list of step forms to splice into tagbody."
  (let ((start-tag (gensym "START"))
        (end-tag   (gensym "END"))
        (vars   (mapcar #'car var-clauses))
        (inits  (mapcar (lambda (c) (if (consp (cdr c)) (cadr c) nil)) var-clauses))
        (steps  (mapcar (lambda (c) (if (cddr c) (caddr c) (car c))) var-clauses))
        (test    (car end-clause))
        (results (cdr end-clause)))
    `(block nil
       (,let-kw ,(mapcar #'list vars inits)
         (tagbody
            ,start-tag
            (if ,test (go ,end-tag))
            ,@body
            ,@(funcall make-step-forms vars steps)
            (go ,start-tag)
            ,end-tag)
         (progn ,@results)))))

;; DO macro (parallel steps)
(our-defmacro do (var-clauses end-clause &body body)
  "General iteration construct.
   VAR-CLAUSES: ((var init step) ...)  END-CLAUSE: (test result ...)
   Evaluates BODY, then updates all vars simultaneously (psetq), then tests."
  (expand-do-form var-clauses end-clause body 'let
                  (lambda (vars steps) `((psetq ,@(mapcan #'list vars steps))))))

;; DO* macro (sequential steps)
(our-defmacro do* (var-clauses end-clause &body body)
  "General iteration construct with sequential binding.
   VAR-CLAUSES: ((var init step) ...)  END-CLAUSE: (test result ...)
   Like DO but bindings and steps are sequential (LET*/SETQ)."
  (expand-do-form var-clauses end-clause body 'let*
                  (lambda (vars steps)
                    (mapcar (lambda (var step) `(setq ,var ,step)) vars steps))))

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

;; Loop iteration emitter table: maps iteration-type keyword → emitter function.
;; Each emitter receives (var iter bindings end-tests pre-body step-forms) and
;; destructively pushes its contributions onto the binding/step lists.
;; Returns nothing; side-effects the accumulators.
(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar *loop-iter-emitters* (make-hash-table :test 'eq)
  "Dispatch table: iteration-type keyword → emitter function.")

(defvar *loop-acc-emitters* (make-hash-table :test 'eq)
  "Dispatch table: accumulation-type keyword → (lambda (acc-var acc-form bindings) body-form).")

) ; end eval-when for dispatch tables

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
      (flet ((consume-accum! (kw-symbol)
               "Consume the accumulation keyword, its form, optional INTO var,
                drain pending-filter, and record the accumulation entry."
               (pop remaining)
               (let ((form (pop remaining))
                     (filter pending-filter))
                 (setf pending-filter nil)
                 (let ((into-var nil))
                   (when (and remaining (loop-kw-p (car remaining) "INTO"))
                     (pop remaining)
                     (setf into-var (pop remaining)))
                   (push (list kw-symbol form into-var filter) accumulations)))))
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
                 ;; FOR var = expr [THEN step-expr]
                 ((loop-kw-p kw "=")
                  (setf (getf spec :type) :equals)
                  (setf (getf spec :equals) (pop remaining))
                  (when (and remaining (loop-kw-p (car remaining) "THEN"))
                    (pop remaining)
                    (setf (getf spec :then) (pop remaining))))
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
            ;; Accumulation clauses — each consumed by consume-accum!
            ((or (loop-kw-p clause "COLLECT")  (loop-kw-p clause "COLLECTING"))  (consume-accum! :collect))
            ((or (loop-kw-p clause "SUM")      (loop-kw-p clause "SUMMING"))     (consume-accum! :sum))
            ((or (loop-kw-p clause "COUNT")    (loop-kw-p clause "COUNTING"))    (consume-accum! :count))
            ((or (loop-kw-p clause "MAXIMIZE") (loop-kw-p clause "MAXIMIZING"))  (consume-accum! :maximize))
            ((or (loop-kw-p clause "MINIMIZE") (loop-kw-p clause "MINIMIZING"))  (consume-accum! :minimize))
            ((or (loop-kw-p clause "APPEND")   (loop-kw-p clause "APPENDING"))   (consume-accum! :append))
            ((or (loop-kw-p clause "NCONC")    (loop-kw-p clause "NCONCING"))    (consume-accum! :nconc))
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
            :finally finally))))          ; NOT nreversed — loop macro will nreverse

;;; --- Accumulation emitters ---
;; Each returns the body-form to push into the loop body.
;; Side-effects: pushes (acc-var init) into bindings, optionally pushes into result-form.

(defmacro define-loop-acc-emitter (type (acc-var acc-form bindings-var result-form-var into-var-var) &body body)
  "Register an accumulation emitter for TYPE in *loop-acc-emitters*."
  `(setf (gethash ,type *loop-acc-emitters*)
         (lambda (,acc-var ,acc-form ,bindings-var ,result-form-var ,into-var-var)
           ,@body)))

(define-loop-acc-emitter :collect (acc-var acc-form bindings result-form into-var)
  (push (list acc-var nil) bindings)
  (unless into-var
    (push (list 'nreverse acc-var) result-form))
  (values (list 'setq acc-var (list 'cons acc-form acc-var)) bindings result-form))

(define-loop-acc-emitter :sum (acc-var acc-form bindings result-form into-var)
  (declare (ignore result-form into-var))
  (push (list acc-var 0) bindings)
  (values (list 'setq acc-var (list '+ acc-var acc-form)) bindings nil))

(define-loop-acc-emitter :count (acc-var acc-form bindings result-form into-var)
  (declare (ignore result-form into-var))
  (push (list acc-var 0) bindings)
  (values (list 'when acc-form (list 'setq acc-var (list '+ acc-var 1))) bindings nil))

(define-loop-acc-emitter :maximize (acc-var acc-form bindings result-form into-var)
  (declare (ignore result-form into-var))
  (push (list acc-var nil) bindings)
  (values (list 'if acc-var
                (list 'when (list '> acc-form acc-var)
                      (list 'setq acc-var acc-form))
                (list 'setq acc-var acc-form))
          bindings nil))

(define-loop-acc-emitter :minimize (acc-var acc-form bindings result-form into-var)
  (declare (ignore result-form into-var))
  (push (list acc-var nil) bindings)
  (values (list 'if acc-var
                (list 'when (list '< acc-form acc-var)
                      (list 'setq acc-var acc-form))
                (list 'setq acc-var acc-form))
          bindings nil))

(define-loop-acc-emitter :append (acc-var acc-form bindings result-form into-var)
  (declare (ignore result-form into-var))
  (push (list acc-var nil) bindings)
  (values (list 'setq acc-var (list 'append acc-var acc-form)) bindings nil))

(define-loop-acc-emitter :nconc (acc-var acc-form bindings result-form into-var)
  (declare (ignore result-form into-var))
  (push (list acc-var nil) bindings)
  (values (list 'setq acc-var (list 'nconc acc-var acc-form)) bindings nil))

;;; --- Iteration emitters ---
;; Each receives (var iter) and returns (values bindings end-tests pre-body step-forms)
;; as fresh lists to be appended to the loop's accumulators.

(defmacro define-loop-iter-emitter (type (var iter) &body body)
  "Register an iteration emitter for TYPE in *loop-iter-emitters*."
  `(setf (gethash ,type *loop-iter-emitters*)
         (lambda (,var ,iter) ,@body)))

(define-loop-iter-emitter :from (var iter)
  (let ((from (getf iter :from))
        (to (getf iter :to))
        (below (getf iter :below))
        (by-form (getf iter :by))
        (bindings nil) (end-tests nil) (step-forms nil))
    (push (list var from) bindings)
    (if by-form
        (push (list 'setq var (list '+ var by-form)) step-forms)
        (push (list 'setq var (list '+ var 1)) step-forms))
    (cond (to    (push (list '> var to) end-tests))
          (below (push (list '>= var below) end-tests)))
    (values bindings end-tests nil step-forms)))

(defun %loop-destructure-var (pattern accessor)
  "Generate (var accessor-form) bindings for a destructuring PATTERN applied to ACCESSOR.
Supports proper lists (a b c), dotted pairs (a . b), and nested patterns."
  (cond
    ((null pattern) nil)
    ((symbolp pattern) (list (list pattern accessor)))
    ((consp pattern)
     (append (%loop-destructure-var (car pattern) (list 'car accessor))
             (if (and (cdr pattern) (atom (cdr pattern)))
                 ;; Dotted pair: (a . b) → b = (cdr accessor)
                 (list (list (cdr pattern) (list 'cdr accessor)))
                 ;; Proper list: recurse on cdr
                 (%loop-destructure-var (cdr pattern) (list 'cdr accessor)))))
    (t nil)))

(define-loop-iter-emitter :in (var iter)
  (let* ((destructuring-p (consp var))
         (real-var (if destructuring-p (gensym "DVAR") var))
         (list-var (gensym (if destructuring-p "DLIST" (symbol-name var))))
         (list-form (getf iter :in))
         (by-fn (getf iter :by))
         (bindings nil) (end-tests nil) (pre-body nil) (step-forms nil))
    (push (list list-var list-form) bindings)
    (push (list real-var (list 'car list-var)) bindings)
    ;; For destructuring patterns, bind sub-variables and set them in pre-body/step
    (when destructuring-p
      (let ((destr-bindings (%loop-destructure-var var real-var)))
        (dolist (b destr-bindings) (push (list (first b) nil) bindings))
        (let ((setqs (mapcar (lambda (b) `(setq ,(first b) ,(second b))) destr-bindings)))
          (dolist (s setqs) (push s pre-body))
          ;; Also set after step
          (dolist (s (reverse setqs)) (push s step-forms)))))
    (push (list 'null list-var) end-tests)
    (if by-fn
        (push (list 'setq list-var (list 'funcall by-fn list-var)) step-forms)
        (push (list 'setq list-var (list 'cdr list-var)) step-forms))
    (push (list 'setq real-var (list 'car list-var)) step-forms)
    (values bindings end-tests pre-body step-forms)))

(define-loop-iter-emitter :on (var iter)
  (let* ((destructuring-p (consp var))
         (real-var (if destructuring-p (gensym "DVAR") var))
         (list-form (getf iter :on))
         (by-fn (getf iter :by))
         (bindings nil) (end-tests nil) (pre-body nil) (step-forms nil))
    (push (list real-var list-form) bindings)
    ;; For destructuring patterns like (for (k v) on plist by #'cddr)
    (when destructuring-p
      (let ((destr-bindings (%loop-destructure-var var real-var)))
        (dolist (b destr-bindings) (push (list (first b) nil) bindings))
        (let ((setqs (mapcar (lambda (b) `(setq ,(first b) ,(second b))) destr-bindings)))
          (dolist (s setqs) (push s pre-body))
          (dolist (s (reverse setqs)) (push s step-forms)))))
    (if by-fn
        (push (list 'setq real-var (list 'funcall by-fn real-var)) step-forms)
        (push (list 'setq real-var (list 'cdr real-var)) step-forms))
    (push (list 'null real-var) end-tests)
    (values bindings end-tests pre-body step-forms)))

(define-loop-iter-emitter :across (var iter)
  (let ((vec-var (gensym "VEC"))
        (idx-var (gensym "IDX"))
        (len-var (gensym "LEN"))
        (vec-form (getf iter :across))
        (bindings nil) (end-tests nil) (pre-body nil) (step-forms nil))
    (push (list vec-var vec-form) bindings)
    (push (list len-var (list 'length vec-var)) bindings)
    (push (list idx-var 0) bindings)
    (push (list var nil) bindings)
    (push (list '>= idx-var len-var) end-tests)
    (push (list 'setq var (list 'aref vec-var idx-var)) pre-body)
    (push (list 'setq idx-var (list '+ idx-var 1)) step-forms)
    (values bindings end-tests pre-body step-forms)))

(define-loop-iter-emitter :hash-keys (var iter)
  (let ((keys-var (gensym "KEYS"))
        (table-form (getf iter :hash-table))
        (ht-var (gensym "HT"))
        (using-type (getf iter :using-type))
        (using-var (getf iter :using-var))
        (bindings nil) (end-tests nil) (pre-body nil) (step-forms nil))
    (push (list ht-var table-form) bindings)
    (push (list keys-var (list 'hash-table-keys ht-var)) bindings)
    (push (list var (list 'car keys-var)) bindings)
    (push (list 'null keys-var) end-tests)
    (when (and using-var (eq using-type :hash-value))
      (push (list using-var nil) bindings)
      (push (list 'setq using-var (list 'gethash var ht-var)) pre-body))
    (push (list 'setq keys-var (list 'cdr keys-var)) step-forms)
    (push (list 'setq var (list 'car keys-var)) step-forms)
    (values bindings end-tests pre-body step-forms)))

(define-loop-iter-emitter :hash-values (var iter)
  (let ((vals-var (gensym "VALS"))
        (table-form (getf iter :hash-table))
        (ht-var (gensym "HT"))
        (using-type (getf iter :using-type))
        (using-var (getf iter :using-var))
        (bindings nil) (end-tests nil) (pre-body nil) (step-forms nil))
    (push (list ht-var table-form) bindings)
    (push (list vals-var (list 'hash-table-values ht-var)) bindings)
    (push (list var (list 'car vals-var)) bindings)
    (push (list 'null vals-var) end-tests)
    (when (and using-var (eq using-type :hash-key))
      (let ((keys-var (gensym "KEYS")))
        (push (list keys-var (list 'hash-table-keys ht-var)) bindings)
        (push (list using-var nil) bindings)
        (push (list 'setq using-var (list 'car keys-var)) pre-body)
        (push (list 'setq keys-var (list 'cdr keys-var)) step-forms)))
    (push (list 'setq vals-var (list 'cdr vals-var)) step-forms)
    (push (list 'setq var (list 'car vals-var)) step-forms)
    (values bindings end-tests pre-body step-forms)))

(define-loop-iter-emitter :equals (var iter)
  (let ((init-form (getf iter :equals))
        (then-form (getf iter :then))
        (bindings nil) (pre-body nil) (step-forms nil))
    (push (list var init-form) bindings)
    (if then-form
        (push (list 'setq var then-form) step-forms)
        (push (list 'setq var init-form) pre-body))
    (values bindings nil pre-body step-forms)))

) ; end eval-when

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
      ;; Process iterations — table-driven dispatch
      (dolist (iter iterations)
        (cond
          ;; REPEAT n: (:repeat count-var count-form) — special form, not FOR-based
          ((eq (car iter) :repeat)
           (let ((count-var (second iter))
                 (count-form (third iter)))
             (push (list count-var count-form) bindings)
             (push (list '<= count-var 0) end-tests)
             (push (list 'setq count-var (list '- count-var 1)) step-forms)))
          ;; WITH auxiliary binding: (:with var val) — special form, not FOR-based
          ((eq (car iter) :with)
           (let ((var (second iter))
                 (val (third iter)))
             (push (list var val) bindings)))
          ;; FOR-based iterations — dispatch via *loop-iter-emitters*
          (t
           (let* ((var (getf iter :for))
                  (iter-type (getf iter :type))
                  (emitter (gethash iter-type *loop-iter-emitters*)))
             (if emitter
                 (multiple-value-bind (new-binds new-ends new-pre new-steps)
                     (funcall emitter var iter)
                   ;; Emitters push in forward order, so their lists are reversed.
                   ;; We splice them onto the caller's reversed lists preserving order.
                   (setf bindings   (nconc new-binds bindings))
                   (setf end-tests  (nconc new-ends end-tests))
                   (setf pre-body   (nconc new-pre pre-body))
                   (setf step-forms (nconc new-steps step-forms)))
                 (error "Unknown loop iteration type: ~S" iter-type))))))
      ;; Process accumulations — table-driven dispatch
      (dolist (acc accumulations)
        (let* ((acc-type (car acc))
               (acc-form (cadr acc))
               (into-var (caddr acc))
               (filter (cadddr acc))
               (acc-var (or into-var (gensym "ACC")))
               (emitter (gethash acc-type *loop-acc-emitters*)))
          (unless into-var (push acc-var acc-vars))
          (unless emitter (error "Unknown loop accumulation type: ~S" acc-type))
          (multiple-value-bind (acc-body new-bindings new-result)
              (funcall emitter acc-var acc-form bindings result-form into-var)
            (setf bindings new-bindings)
            (when new-result (setf result-form new-result))
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
  `(setf ,place (cons ,value ,place)))

;; POP macro
(our-defmacro pop (place)
  "Remove and return the first element of list PLACE."
  (let ((tmp (gensym "TMP")))
    `(let ((,tmp (car ,place)))
       (setf ,place (cdr ,place))
       ,tmp)))

;; INCF macro
(our-defmacro incf (place &optional (delta 1))
  "Increment PLACE by DELTA (default 1)."
  `(setf ,place (+ ,place ,delta)))

;; DECF macro
(our-defmacro decf (place &optional (delta 1))
  "Decrement PLACE by DELTA (default 1)."
  `(setf ,place (- ,place ,delta)))

;; 1+ and 1- utility functions
(our-defmacro 1+ (n)
  `(+ ,n 1))

(our-defmacro 1- (n)
  `(- ,n 1))

;; SIGNUM: returns -1, 0, or 1 based on sign
(our-defmacro signum (n)
  (let ((nv (gensym "N")))
    `(let ((,nv ,n))
       (cond ((zerop ,nv) 0)
             ((plusp ,nv) 1)
             (t -1)))))

;; ISQRT: integer square root (floor of real square root)
(our-defmacro isqrt (n)
  `(floor (sqrt (float ,n))))

;; WITH-OPEN-STREAM: like with-open-file but for existing streams
(our-defmacro with-open-stream (var-stream &body body)
  (let ((var (first var-stream))
        (stream (second var-stream)))
    `(let ((,var ,stream))
       (unwind-protect (progn ,@body)
         (close ,var)))))

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

;;; ------------------------------------------------------------
;;; ANSI CL Phase 1 Macros (FR-201 through FR-212)
;;; ------------------------------------------------------------

;; PSETF (FR-207) — parallel setf: evaluate all new values before setting any place
(our-defmacro psetf (&rest pairs)
  "Parallel SETF: evaluates all new values before assigning to any place.
   (psetf p1 v1 p2 v2 ...) — all vi are evaluated, then all pi are set."
  (when (oddp (length pairs))
    (error "PSETF requires an even number of arguments"))
  (let (places vals)
    (do ((rest pairs (cddr rest)))
        ((null rest))
      (push (first rest) places)
      (push (second rest) vals))
    (setf places (nreverse places)
          vals   (nreverse vals))
    (let ((temps (mapcar (lambda (v) (declare (ignore v)) (gensym "PSETF")) vals)))
      `(let ,(mapcar #'list temps vals)
         ,@(mapcar (lambda (place temp) `(setf ,place ,temp))
                   places temps)
         nil))))

;; SHIFTF (FR-206) — shift values through a series of places
(our-defmacro shiftf (&rest args)
  "Shift values: (shiftf place1 ... placeN newval) sets each place to the
   next place's old value, stores NEWVAL in the last place; returns old
   value of first place."
  (when (< (length args) 2)
    (error "SHIFTF requires at least 2 arguments (one place and one new value)"))
  (let* ((places (butlast args))
         (newval (car (last args)))
         (temps  (mapcar (lambda (p) (declare (ignore p)) (gensym "SHIFT")) places)))
    `(let ,(mapcar #'list temps places)
       ,@(mapcar (lambda (place val)
                   `(setf ,place ,val))
                 places
                 (append (cdr temps) (list newval)))
       ,(car temps))))

;; WITH-ACCESSORS (FR-205) — bind local vars to accessor function results
(our-defmacro with-accessors (slot-entries instance &body body)
  "Binds local variables to slot values read via accessor functions.
   Each entry is (local-var-name accessor-function-name)."
  (let ((inst-var (gensym "INST")))
    `(let ((,inst-var ,instance))
       (let ,(mapcar (lambda (entry)
                       (destructuring-bind (var-name accessor-name) entry
                         `(,var-name (,accessor-name ,inst-var))))
                     slot-entries)
         ,@body))))

;; DEFINE-MODIFY-MACRO (FR-208) — define a read-modify-write macro
(our-defmacro define-modify-macro (name lambda-list function &optional doc)
  "Define a macro NAME that reads PLACE, applies FUNCTION (with extra args),
   and stores the result back into PLACE."
  (let* ((place-var (gensym "PLACE"))
         (param-names
           (mapcar (lambda (p) (if (listp p) (first p) p))
                   (remove-if (lambda (p)
                                (member p '(&optional &rest &key &aux &allow-other-keys)))
                              lambda-list))))
    (when doc)
    `(our-defmacro ,name (,place-var ,@lambda-list)
       `(setf ,,place-var (,',function ,,place-var ,,@param-names)))))

;; ASSERT (FR-203) — signal an error if a test fails
(our-defmacro assert (test &optional places datum &rest args)
  "Signal a correctable error if TEST is false."
  (when places)
  `(unless ,test
     ,(if datum
          `(error ,datum ,@args)
          `(error "Assertion failed: ~S" ',test))))

;; DEFINE-CONDITION (FR-204) — define a condition type (expands to defclass)
(our-defmacro define-condition (name parent-list slot-specs &rest options)
  "Define a condition type NAME."
  (let ((parent-names (if parent-list parent-list '(error))))
    (when options)
    `(defclass ,name ,parent-names
       ,slot-specs)))

;; HANDLER-BIND (FR-201) — simplified via HANDLER-CASE
(our-defmacro handler-bind (bindings &body body)
  "Establish condition handlers around BODY (simplified: stack unwinds)."
  (if (null bindings)
      `(progn ,@body)
      `(handler-case (progn ,@body)
         ,@(mapcar (lambda (binding)
                     (let ((cvar (gensym "COND")))
                       `(,(first binding) (,cvar)
                          (funcall ,(second binding) ,cvar))))
                   bindings))))

;; RESTART-CASE (FR-202) — simplified
(our-defmacro restart-case (form &rest clauses)
  "Execute FORM with named restarts (simplified; restarts not invokable)."
  (if (null clauses)
      form
      (let* ((first-clause (first clauses))
             (first-body   (cdddr first-clause))
             (err-var (gensym "ERR")))
        `(handler-case ,form
           (error (,err-var)
             (when ,err-var)
             ,@(if first-body first-body '(nil)))))))

;; RESTART-BIND (FR-202) — stub
(our-defmacro restart-bind (bindings &body body)
  "Establish restart bindings (stub: ignored)."
  (when bindings)
  `(progn ,@body))

;;; FR-804/FR-805: Restart utility functions (stubs for ANSI compatibility)
;;; Note: declare after docstring is invalid in our-defmacro bodies (docstring
;;; is first body form; declare must be first). All stubs use minimal bodies.

(our-defmacro invoke-restart (name &rest args)
  `(error "No restart named ~S is active" ,name ,@args))

(our-defmacro find-restart (name &optional condition)
  (let ((_n name) (_c condition)) (declare (ignore _n _c))
    `nil))

(our-defmacro compute-restarts (&optional condition)
  (let ((_c condition)) (declare (ignore _c))
    `nil))

(our-defmacro restart-name (restart)
  `(if (hash-table-p ,restart)
       (gethash :name ,restart)
       ,restart))

;;; FR-805: Standard restart functions

(our-defmacro abort (&optional condition)
  (let ((_c condition)) (declare (ignore _c))
    `(error "ABORT invoked")))

(our-defmacro continue (&optional condition)
  (let ((_c condition)) (declare (ignore _c))
    `nil))

(our-defmacro muffle-warning (&optional condition)
  (let ((_c condition)) (declare (ignore _c))
    `nil))

(our-defmacro use-value (value &optional condition)
  (let ((_c condition)) (declare (ignore _c))
    `,value))

(our-defmacro store-value (value &optional condition)
  (let ((_c condition)) (declare (ignore _c))
    `,value))

;; WITH-INPUT-FROM-STRING (FR-209)
(our-defmacro with-input-from-string (binding &body body)
  "Execute BODY with (first binding) bound to a string input stream."
  (let* ((var    (first binding))
         (string (second binding)))
    `(let ((,var (make-string-input-stream ,string)))
       ,@body)))

;; WITH-OUTPUT-TO-STRING
(our-defmacro with-output-to-string (binding &body body)
  "Execute BODY with (first binding) bound to a string output stream."
  (let ((var (first binding)))
    `(let ((,var (make-string-output-stream)))
       ,@body
       (get-output-stream-string ,var))))

;; WITH-STANDARD-IO-SYNTAX (FR-210) — stub
(our-defmacro with-standard-io-syntax (&body body)
  "Execute BODY with standard I/O syntax bindings (stub)."
  `(progn ,@body))

;; WITH-PACKAGE-ITERATOR (FR-211) — stub
(our-defmacro with-package-iterator (binding &body body)
  "Iterate over package symbols (stub: iterator always exhausted)."
  (let ((name (first binding)))
    `(let ((,name (lambda () (values nil nil nil nil))))
       ,@body)))

;; DEFINE-COMPILER-MACRO (FR-212) — stub
(our-defmacro define-compiler-macro (name lambda-list &body body)
  "Define a compiler macro for NAME (stub: ignored)."
  (when (or name lambda-list body))
  nil)

;;; ------------------------------------------------------------
;;; CXR Accessor Macros (algorithmic registration)
;;; ------------------------------------------------------------
;;; cXXr..cXXXXr forms are registered programmatically by
;;; analysing the sequence of a/d letters between the outer c/r.

(let ((cxr-names '(caar cadr cdar cddr
                   caaar cdaar cadar cddar
                   caadr cdadr caddr cdddr
                   caaaar cadaar caadar caddar
                   cdaaar cddaar cdadar cdddar
                   caaadr cadadr caaddr cadddr
                   cdaadr cddadr cdaddr cddddr)))
  (flet ((expand-cxr (sym arg)
           (let* ((name (symbol-name sym))
                  (letters (subseq name 1 (1- (length name)))))
             (reduce (lambda (acc letter)
                       (if (char-equal letter #\a) `(car ,acc) `(cdr ,acc)))
                     (reverse (coerce letters 'list))
                     :initial-value arg))))
    (dolist (cxr-sym cxr-names)
      (let ((cxr cxr-sym))
        (register-macro cxr
                        (lambda (form env)
                          (declare (ignore env))
                          (expand-cxr cxr (second form))))))))

;;; ------------------------------------------------------------
;;; Higher-Order Function Macros
;;; ------------------------------------------------------------
;;; Each HOF is expanded to an explicit dolist-based loop.
;;; The compiler can handle dolist directly; no HOF primitives needed.

;; MAPCAR: apply fn to each element, collect results
(our-defmacro mapcar (fn list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,fn-var ,fn)
           (,acc nil))
       (dolist (,x ,list (nreverse ,acc))
         (setq ,acc (cons (funcall ,fn-var ,x) ,acc))))))

;; MAPC: side-effect loop, returns the original list
(our-defmacro mapc (fn list)
  (let ((fn-var (gensym "FN"))
        (lst (gensym "LST"))
        (x (gensym "X")))
    `(let ((,fn-var ,fn)
           (,lst ,list))
       (dolist (,x ,lst ,lst)
         (funcall ,fn-var ,x)))))

;; MAPCAN: apply fn to each element, nconc all results
(our-defmacro mapcan (fn list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,fn-var ,fn)
           (,acc nil))
       (dolist (,x ,list ,acc)
         (setq ,acc (nconc ,acc (funcall ,fn-var ,x)))))))

;; EVERY: true iff pred returns non-nil for every element
(our-defmacro every (pred list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X")))
    `(let ((,fn-var ,pred))
       (block nil
         (dolist (,x ,list t)
           (unless (funcall ,fn-var ,x)
             (return nil)))))))

;; SOME: returns first truthy pred result, or nil
(our-defmacro some (pred list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (result (gensym "R")))
    `(let ((,fn-var ,pred))
       (block nil
         (dolist (,x ,list nil)
           (let ((,result (funcall ,fn-var ,x)))
             (when ,result (return ,result))))))))

;; NOTANY: true iff pred returns nil for every element
(our-defmacro notany (pred list)
  `(not (some ,pred ,list)))

;; NOTEVERY: true iff pred returns nil for at least one element
(our-defmacro notevery (pred list)
  `(not (every ,pred ,list)))

;; REMOVE-IF: keep elements for which pred is false
(our-defmacro remove-if (pred list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,fn-var ,pred)
           (,acc nil))
       (dolist (,x ,list (nreverse ,acc))
         (unless (funcall ,fn-var ,x)
           (setq ,acc (cons ,x ,acc)))))))

;; REMOVE-IF-NOT: keep elements for which pred is true
(our-defmacro remove-if-not (pred list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,fn-var ,pred)
           (,acc nil))
       (dolist (,x ,list (nreverse ,acc))
         (when (funcall ,fn-var ,x)
           (setq ,acc (cons ,x ,acc)))))))

;; FIND: first element eql to item, or nil
(our-defmacro find (item list &rest keys)
  (if keys
      ;; keyword args present — key/test-aware loop
      (let* ((key-expr  (or (getf keys :key)  '#'identity))
             (test-expr (or (getf keys :test) '#'eql))
             (item-var  (gensym "ITEM"))
             (key-var   (gensym "KEY"))
             (test-var  (gensym "TEST"))
             (x         (gensym "X")))
        `(let ((,item-var ,item) (,key-var ,key-expr) (,test-var ,test-expr))
           (block nil
             (dolist (,x ,list nil)
               (when (funcall ,test-var ,item-var (funcall ,key-var ,x))
                 (return ,x))))))
      ;; no keyword args — fast eql check
      (let ((item-var (gensym "ITEM")) (x (gensym "X")))
        `(let ((,item-var ,item))
           (block nil
             (dolist (,x ,list nil)
               (when (eql ,item-var ,x) (return ,x))))))))

;; FIND-IF: first element for which pred is true, or nil
(our-defmacro find-if (pred list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X")))
    `(let ((,fn-var ,pred))
       (block nil
         (dolist (,x ,list nil)
           (when (funcall ,fn-var ,x)
             (return ,x)))))))

;; POSITION: index of first element eql to item, or nil
(our-defmacro position (item list)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X"))
        (idx (gensym "IDX")))
    `(let ((,item-var ,item)
           (,idx 0))
       (block nil
         (dolist (,x ,list nil)
           (when (eql ,item-var ,x)
             (return ,idx))
           (setq ,idx (+ ,idx 1)))))))

;; COUNT: number of elements eql to item
(our-defmacro count (item list)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X"))
        (cnt (gensym "CNT")))
    `(let ((,item-var ,item)
           (,cnt 0))
       (dolist (,x ,list ,cnt)
         (when (eql ,item-var ,x)
           (setq ,cnt (+ ,cnt 1)))))))

;; COUNT-IF: number of elements for which pred is true
(our-defmacro count-if (pred list)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (cnt (gensym "CNT")))
    `(let ((,fn-var ,pred)
           (,cnt 0))
       (dolist (,x ,list ,cnt)
         (when (funcall ,fn-var ,x)
           (setq ,cnt (+ ,cnt 1)))))))

;; REMOVE: remove all elements eql to item
(our-defmacro remove (item list)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,item-var ,item)
           (,acc nil))
       (dolist (,x ,list (nreverse ,acc))
         (unless (eql ,item-var ,x)
           (setq ,acc (cons ,x ,acc)))))))

;; REMOVE-DUPLICATES: keep only the first occurrence of each element
(our-defmacro remove-duplicates (list)
  (let ((x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,acc nil))
       (dolist (,x ,list (nreverse ,acc))
         (unless (member ,x ,acc)
           (setq ,acc (cons ,x ,acc)))))))

;; UNION: all elements present in either list, no duplicates
(our-defmacro union (list1 list2)
  (let ((l1 (gensym "L1"))
        (l2 (gensym "L2"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,l1 ,list1)
           (,l2 ,list2)
           (,acc nil))
       (dolist (,x ,l2)
         (setq ,acc (cons ,x ,acc)))
       (dolist (,x ,l1 (nreverse ,acc))
         (unless (member ,x ,l2)
           (setq ,acc (cons ,x ,acc)))))))

;; SET-DIFFERENCE: elements in list1 not present in list2
(our-defmacro set-difference (list1 list2)
  (let ((l2 (gensym "L2"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,l2 ,list2)
           (,acc nil))
       (dolist (,x ,list1 (nreverse ,acc))
         (unless (member ,x ,l2)
           (setq ,acc (cons ,x ,acc)))))))

;; INTERSECTION: elements present in both lists
(our-defmacro intersection (list1 list2)
  (let ((l2 (gensym "L2"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,l2 ,list2)
           (,acc nil))
       (dolist (,x ,list1 (nreverse ,acc))
         (when (member ,x ,l2)
           (setq ,acc (cons ,x ,acc)))))))

;; SUBSETP: true iff every element of list1 is in list2
(our-defmacro subsetp (list1 list2)
  (let ((l2 (gensym "L2"))
        (x (gensym "X")))
    `(let ((,l2 ,list2))
       (every (lambda (,x) (member ,x ,l2)) ,list1))))

;; ADJOIN: cons item onto list only if not already present
(our-defmacro adjoin (item list)
  (let ((item-var (gensym "ITEM"))
        (lst (gensym "LST")))
    `(let ((,item-var ,item)
           (,lst ,list))
       (if (member ,item-var ,lst) ,lst (cons ,item-var ,lst)))))

;; RASSOC: find association pair by cdr value
(our-defmacro rassoc (item alist)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X")))
    `(let ((,item-var ,item))
       (block nil
         (dolist (,x ,alist nil)
           (when (and (consp ,x) (eql ,item-var (cdr ,x)))
             (return ,x)))))))

;; PAIRLIS: zip keys and data lists into an association list
(our-defmacro pairlis (keys data &optional alist)
  (let ((ks (gensym "KS"))
        (ds (gensym "DS"))
        (acc (gensym "ACC")))
    `(let ((,ks ,keys)
           (,ds ,data)
           (,acc ,alist))
       (tagbody
        pairlis-loop
          (when (and ,ks ,ds)
            (setq ,acc (cons (cons (car ,ks) (car ,ds)) ,acc))
            (setq ,ks (cdr ,ks))
            (setq ,ds (cdr ,ds))
            (go pairlis-loop)))
       ,acc)))

;; SORT: stable merge sort
(our-defmacro sort (list predicate)
  (let ((pred (gensym "PRED"))
        (len (gensym "LEN"))
        (mid (gensym "MID"))
        (left (gensym "LEFT"))
        (right (gensym "RIGHT"))
        (msort (gensym "MSORT"))
        (mmerge (gensym "MMERGE"))
        (take-n (gensym "TAKEN")))
    `(let ((,pred ,predicate))
       (labels ((,take-n (lst n)
                  (if (= n 0) nil
                      (cons (car lst) (,take-n (cdr lst) (- n 1)))))
                (,mmerge (a b)
                  (cond ((null a) b)
                        ((null b) a)
                        ((funcall ,pred (car a) (car b))
                         (cons (car a) (,mmerge (cdr a) b)))
                        (t (cons (car b) (,mmerge a (cdr b))))))
                (,msort (lst)
                  (let ((,len (length lst)))
                    (if (<= ,len 1) lst
                        (let* ((,mid (truncate ,len 2))
                               (,left (,take-n lst ,mid))
                               (,right (nthcdr ,mid lst)))
                          (,mmerge (,msort ,left) (,msort ,right)))))))
         (,msort ,list)))))

;; STABLE-SORT: same as sort (merge sort is inherently stable)
(our-defmacro stable-sort (list predicate)
  `(sort ,list ,predicate))

;; MAP: map fn over seq, coerce result to result-type
(our-defmacro map (result-type fn seq)
  `(coerce (mapcar ,fn (coerce ,seq 'list)) ,result-type))

;; IGNORE-ERRORS: catch all errors, return nil on error
(our-defmacro ignore-errors (&body forms)
  (let ((e-var (gensym "E")))
    `(handler-case (progn ,@forms)
       (error (,e-var) nil))))

;; CONCATENATE: concatenate strings via nested string-concat calls
(our-defmacro concatenate (result-type &rest strings)
  (if (null strings)
      ""
      (if (null (cdr strings))
          (car strings)
          (reduce (lambda (acc s) `(string-concat ,acc ,s))
                  (cdr strings)
                  :initial-value (car strings)))))

;;; ------------------------------------------------------------
;;; Phase 3: Sequence Operations (FR-500)
;;; ------------------------------------------------------------

;; COPY-SEQ (FR-507): shallow copy of a sequence
(our-defmacro copy-seq (seq)
  "Return a fresh copy of SEQ."
  `(copy-list ,seq))

;; FILL (FR-502): fill a sequence (list) with item
(our-defmacro fill (seq item &rest keys)
  "Fill SEQ with ITEM (list version; :start/:end ignored)."
  (when keys)
  (let ((s (gensym "SEQ"))
        (ptr (gensym "PTR"))
        (lbl (gensym "FILL-LOOP")))
    `(let* ((,s ,seq)
            (,ptr ,s))
       (tagbody
         ,lbl
         (when ,ptr
           (setf (car ,ptr) ,item)
           (setq ,ptr (cdr ,ptr))
           (go ,lbl)))
       ,s)))

;; REPLACE (FR-502): copy elements from source into destination
(our-defmacro replace (dest source &rest keys)
  "Copy elements from SOURCE into DEST (list version; keys ignored)."
  (when keys)
  (let ((d (gensym "DEST"))
        (s (gensym "SRC"))
        (dp (gensym "DP"))
        (sp (gensym "SP"))
        (lbl (gensym "REPLACE-LOOP")))
    `(let ((,d ,dest)
           (,s ,source))
       (let ((,dp ,d)
             (,sp ,s))
         (tagbody
           ,lbl
           (when (and ,dp ,sp)
             (setf (car ,dp) (car ,sp))
             (setq ,dp (cdr ,dp))
             (setq ,sp (cdr ,sp))
             (go ,lbl))))
       ,d)))

;; MISMATCH (FR-506): first position where sequences differ
(our-defmacro mismatch (seq1 seq2 &rest keys)
  "Return index of first mismatch between SEQ1 and SEQ2, or NIL if equal."
  (when keys)
  (let ((s1 (gensym "S1"))
        (s2 (gensym "S2"))
        (idx (gensym "IDX"))
        (lbl (gensym "MISMATCH-LOOP")))
    `(block nil
       (let ((,s1 ,seq1)
             (,s2 ,seq2)
             (,idx 0))
         (tagbody
           ,lbl
           (cond
             ((and (null ,s1) (null ,s2)) (return nil))
             ((or  (null ,s1) (null ,s2)) (return ,idx))
             ((not (eql (car ,s1) (car ,s2))) (return ,idx))
             (t (setq ,s1 (cdr ,s1))
                (setq ,s2 (cdr ,s2))
                (setq ,idx (+ ,idx 1))
                (go ,lbl))))))))

;; DELETE (FR-504): like REMOVE but destructive (same as remove here)
(our-defmacro delete (item seq &rest keys)
  "Remove all elements EQL to ITEM from SEQ."
  (when keys)
  (let ((item-var (gensym "ITEM"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,item-var ,item)
           (,acc nil))
       (dolist (,x ,seq (nreverse ,acc))
         (unless (eql ,item-var ,x)
           (setq ,acc (cons ,x ,acc)))))))

;; DELETE-IF (FR-504)
(our-defmacro delete-if (pred seq &rest keys)
  "Remove all elements for which PRED is true."
  (when keys)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,fn-var ,pred)
           (,acc nil))
       (dolist (,x ,seq (nreverse ,acc))
         (unless (funcall ,fn-var ,x)
           (setq ,acc (cons ,x ,acc)))))))

;; DELETE-IF-NOT (FR-504)
(our-defmacro delete-if-not (pred seq &rest keys)
  "Remove all elements for which PRED is false."
  (when keys)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,fn-var ,pred)
           (,acc nil))
       (dolist (,x ,seq (nreverse ,acc))
         (when (funcall ,fn-var ,x)
           (setq ,acc (cons ,x ,acc)))))))

;; DELETE-DUPLICATES (FR-504)
(our-defmacro delete-duplicates (seq &rest keys)
  "Remove duplicate elements (keeps first occurrence)."
  (when keys)
  `(remove-duplicates ,seq))

;; SUBSTITUTE (FR-505): replace occurrences of old with new
(our-defmacro substitute (new old seq &rest keys)
  "Return new sequence with each EQL OLD replaced by NEW."
  (when keys)
  (let ((new-var (gensym "NEW"))
        (old-var (gensym "OLD"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,new-var ,new)
           (,old-var ,old)
           (,acc nil))
       (dolist (,x ,seq (nreverse ,acc))
         (setq ,acc (cons (if (eql ,x ,old-var) ,new-var ,x) ,acc))))))

;; SUBSTITUTE-IF (FR-505)
(our-defmacro substitute-if (new pred seq &rest keys)
  "Replace elements for which PRED is true with NEW."
  (when keys)
  (let ((new-var (gensym "NEW"))
        (fn-var (gensym "FN"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,new-var ,new)
           (,fn-var ,pred)
           (,acc nil))
       (dolist (,x ,seq (nreverse ,acc))
         (setq ,acc (cons (if (funcall ,fn-var ,x) ,new-var ,x) ,acc))))))

;; SUBSTITUTE-IF-NOT (FR-505)
(our-defmacro substitute-if-not (new pred seq &rest keys)
  "Replace elements for which PRED is false with NEW."
  (when keys)
  (let ((new-var (gensym "NEW"))
        (fn-var (gensym "FN"))
        (x (gensym "X"))
        (acc (gensym "ACC")))
    `(let ((,new-var ,new)
           (,fn-var ,pred)
           (,acc nil))
       (dolist (,x ,seq (nreverse ,acc))
         (setq ,acc (cons (if (funcall ,fn-var ,x) ,x ,new-var) ,acc))))))

;; NSUBSTITUTE / NSUBSTITUTE-IF / NSUBSTITUTE-IF-NOT (FR-505): same as substitute (non-destructive)
;;; REDUCE (FR-500 adjacent): fold a sequence using a binary function
;;; Uses the proven dolist-style tagbody pattern with (go end-tag) inside.
(our-defmacro reduce (fn seq &rest keys)
  "Fold SEQ using FN. Supports :initial-value, :from-end, :key (static at macro-expand time)."
  (let* ((fn-var   (gensym "FN"))
         (cur-var  (gensym "CUR"))
         (acc-var  (gensym "ACC"))
         (start-lbl (gensym "REDUCE-START"))
         (end-lbl   (gensym "REDUCE-END"))
         (has-iv   (not (null (member :initial-value keys))))
         (iv-expr  (getf keys :initial-value nil))
         (fe-expr  (getf keys :from-end nil))
         (key-fn   (getf keys :key nil))
         (seq-init (if fe-expr `(reverse ,seq) seq))
         (elem-form (if key-fn
                        `(funcall ,key-fn (car ,cur-var))
                        `(car ,cur-var))))
    (if has-iv
        ;; With initial-value: acc starts at iv, cur iterates entire seq
        `(let ((,fn-var ,fn)
               (,acc-var ,iv-expr)
               (,cur-var ,seq-init))
           (tagbody
            ,start-lbl
            (if (null ,cur-var) (go ,end-lbl))
            (setq ,acc-var (funcall ,fn-var ,acc-var ,elem-form))
            (setq ,cur-var (cdr ,cur-var))
            (go ,start-lbl)
            ,end-lbl)
           ,acc-var)
        ;; No initial-value: acc starts at first element, cur at rest
        `(let ((,fn-var ,fn)
               (,cur-var ,seq-init))
           (let ((,acc-var (car ,cur-var)))
             (setq ,cur-var (cdr ,cur-var))
             (tagbody
              ,start-lbl
              (if (null ,cur-var) (go ,end-lbl))
              (setq ,acc-var (funcall ,fn-var ,acc-var ,elem-form))
              (setq ,cur-var (cdr ,cur-var))
              (go ,start-lbl)
              ,end-lbl)
             ,acc-var)))))

(our-defmacro nsubstitute (new old seq &rest keys)
  "Destructive substitute (same as substitute in this impl)."
  (when keys)
  `(substitute ,new ,old ,seq))

(our-defmacro nsubstitute-if (new pred seq &rest keys)
  "Destructive substitute-if (same as substitute-if in this impl)."
  (when keys)
  `(substitute-if ,new ,pred ,seq))

(our-defmacro nsubstitute-if-not (new pred seq &rest keys)
  "Destructive substitute-if-not (same as substitute-if-not in this impl)."
  (when keys)
  `(substitute-if-not ,new ,pred ,seq))

;; MAP-INTO (FR-503): fill sequence with mapped results
(our-defmacro map-into (dest fn &rest seqs)
  "Fill DEST with (fn (elt seq1 i) ...) for each i."
  (if (= (length seqs) 1)
      (let ((d (gensym "DEST"))
            (src (gensym "SRC"))
            (dp (gensym "DP"))
            (sp (gensym "SP"))
            (fn-var (gensym "FN"))
            (lbl (gensym "MAP-INTO-LOOP")))
        `(let ((,d ,dest)
               (,src ,(first seqs))
               (,fn-var ,fn))
           (let ((,dp ,d)
                 (,sp ,src))
             (tagbody
               ,lbl
               (when (and ,dp ,sp)
                 (setf (car ,dp) (funcall ,fn-var (car ,sp)))
                 (setq ,dp (cdr ,dp))
                 (setq ,sp (cdr ,sp))
                 (go ,lbl))))
           ,d))
      `(progn ,dest)))

;;; MERGE (FR-504): merge two sorted sequences using predicate
(our-defmacro merge (result-type seq1 seq2 pred &rest keys)
  "Merge two sorted sequences SEQ1 and SEQ2 into a sorted sequence using PRED."
  (when keys)
  (when result-type)
  (let ((l1 (gensym "L1"))
        (l2 (gensym "L2"))
        (fn-var (gensym "PRED"))
        (acc (gensym "ACC"))
        (lbl1 (gensym "MERGE-LOOP"))
        (lbl2 (gensym "DRAIN1"))
        (lbl3 (gensym "DRAIN2"))
        (end (gensym "MERGE-END")))
    `(let ((,l1 ,seq1)
           (,l2 ,seq2)
           (,fn-var ,pred)
           (,acc nil))
       (tagbody
         ,lbl1
         (when (and ,l1 ,l2)
           (if (funcall ,fn-var (car ,l1) (car ,l2))
               (progn (setq ,acc (cons (car ,l1) ,acc))
                      (setq ,l1 (cdr ,l1)))
               (progn (setq ,acc (cons (car ,l2) ,acc))
                      (setq ,l2 (cdr ,l2))))
           (go ,lbl1))
         ,lbl2
         (when ,l1
           (setq ,acc (cons (car ,l1) ,acc))
           (setq ,l1 (cdr ,l1))
           (go ,lbl2))
         ,lbl3
         (when ,l2
           (setq ,acc (cons (car ,l2) ,acc))
           (setq ,l2 (cdr ,l2))
           (go ,lbl3))
         ,end)
       (nreverse ,acc))))

;;; Package System (no-ops in this compiler)

(our-defmacro in-package (name)
  `(quote ,name))

(our-defmacro defpackage (name &rest options)
  (declare (ignore options))
  `(quote ,name))

(our-defmacro export (symbols &optional package)
  (declare (ignore symbols package))
  nil)

;;; Declaration (silently ignored)

(our-defmacro declare (&rest decls)
  (declare (ignore decls))
  nil)

;;; FR-1201: Property List Macros (getf, remf, get-properties)

(our-defmacro getf (plist indicator &optional default)
  "Return the value for INDICATOR in PLIST, or DEFAULT if not found."
  (let ((pl (gensym "PL")) (ind (gensym "IND")) (found (gensym "FOUND")))
    `(let ((,pl ,plist) (,ind ,indicator))
       (let ((,found (member ,ind ,pl)))
         (if ,found (cadr ,found) ,default)))))

(our-defmacro remf (plist indicator)
  "Remove INDICATOR and its value from PLIST. Returns T if found, NIL otherwise."
  (let ((ind (gensym "IND")) (prev (gensym "PREV")) (cur (gensym "CUR"))
        (found (gensym "FOUND")))
    `(let ((,ind ,indicator) (,prev nil) (,cur ,plist) (,found nil))
       (tagbody
         :loop
         (when ,cur
           (cond
             ((eq (car ,cur) ,ind)
              (setq ,found t)
              (if ,prev
                  (rplacd (cdr ,prev) (cddr ,cur))
                  (setq ,plist (cddr ,cur)))
              (go :done))
             (t
              (setq ,prev ,cur)
              (setq ,cur (cddr ,cur))
              (go :loop))))
         :done)
       ,found)))

;;; Scope with Declarations

(our-defmacro locally (&body forms)
  `(progn ,@(remove-if (lambda (f) (and (consp f) (eq (car f) 'declare)))
                       forms)))

;; PROGV (FR-102) — dynamic variable binding
;; Uses vm-progv-enter/vm-progv-exit to save and restore global-vars around body.
(our-defmacro progv (symbols values &body body)
  "Bind SYMBOLS to VALUES dynamically for the duration of BODY."
  (let ((syms-var (gensym "SYMS"))
        (vals-var (gensym "VALS"))
        (saved-var (gensym "SAVED")))
    `(let* ((,syms-var ,symbols)
            (,vals-var ,values)
            (,saved-var (%progv-enter ,syms-var ,vals-var)))
       (unwind-protect
         (progn ,@body)
         (%progv-exit ,saved-var)))))

;;; File I/O

(our-defmacro with-open-file (stream-spec &body body)
  "Bind VAR to an open stream for PATH, execute BODY, then close the stream.
   STREAM-SPEC is (var path &rest open-options)."
  (let* ((var     (first stream-spec))
         (path    (second stream-spec))
         (options (cddr stream-spec)))
    `(let ((,var (open ,path ,@options)))
       (unwind-protect (progn ,@body)
         (close ,var)))))

;;; Warning Output

(our-defmacro warn (fmt &rest args)
  `(progn
     (format t ,(concatenate 'string "~&WARNING: "
                             (if (stringp fmt) fmt "~A"))
             ,@args)
     nil))

;;; Hash Table Utilities

(our-defmacro copy-hash-table (ht)
  (let ((ht-var (gensym "HT"))
        (new-var (gensym "NEW"))
        (k-var   (gensym "K"))
        (v-var   (gensym "V")))
    `(let ((,ht-var ,ht))
       (let ((,new-var (make-hash-table :test (hash-table-test ,ht-var))))
         (maphash (lambda (,k-var ,v-var)
                    (setf (gethash ,k-var ,new-var) ,v-var))
                  ,ht-var)
         ,new-var))))

;;; Type Coercion

(our-defmacro coerce (value type-form)
  (if (and (consp type-form) (eq (car type-form) 'quote))
      (let ((type (second type-form)))
        (cond
          ((and (symbolp type) (member type '(string simple-string base-string)))
           `(coerce-to-string ,value))
          ((and (symbolp type) (eq type 'list))
           `(coerce-to-list ,value))
          ((and (symbolp type) (member type '(vector simple-vector)))
           `(coerce-to-vector ,value))
          ((and (consp type) (member (car type) '(simple-array array)))
           `(coerce-to-vector ,value))
          ((and (consp type) (eq (car type) 'vector))
           `(coerce-to-vector ,value))
          (t `(coerce-to-string ,value))))
      `(coerce-to-string ,value)))

;;; Compile-time Evaluation

;; LOAD-TIME-VALUE — evaluate at compile time, splice in the quoted result.
(our-defmacro load-time-value (form &optional read-only-p)
  (declare (ignore read-only-p))
  `(quote ,(eval form)))

;;; FR-1206: Module/feature system — *features*, *modules*, provide, require

(our-defmacro provide (module-name)
  "Mark MODULE-NAME as loaded by pushing its string name onto *modules*."
  (let ((mod (gensym "MOD")))
    `(let ((,mod (string ,module-name)))
       (pushnew ,mod *modules* :test #'string=)
       ,mod)))

(our-defmacro require (module-name &optional pathnames)
  "Signal a warning if MODULE-NAME is not already in *modules*.
PATHNAMES is accepted for compatibility but ignored."
  (let ((mod (gensym "MOD")))
    `(let ((,mod (string ,module-name)))
       (unless (member ,mod *modules* :test #'string=)
         (warn "Module ~A not loaded" ,mod))
       ,mod)))

;;; FR-1004: print-unreadable-object
;;; Note: uses flat (spec &body body) lambda list because our-defmacro does not
;;; support nested destructuring in required params.

(our-defmacro print-unreadable-object (spec &body body)
  "Print OBJECT to STREAM in #<...> notation.
SPEC is (object stream &key type identity)."
  (let* ((object     (first spec))
         (stream-frm (second spec))
         (rest-keys  (cddr spec))
         (type-expr  (getf rest-keys :type))
         (id-expr    (getf rest-keys :identity))
         (obj-var    (gensym "OBJ"))
         (str-var    (gensym "STR"))
         (space-forms (when (and type-expr body)
                        (list `(format ,str-var " ")))))
    `(let ((,obj-var ,object)
           (,str-var ,stream-frm))
       (format ,str-var "#<")
       (when ,type-expr
         (format ,str-var "~A" (type-of ,obj-var))
         ,@space-forms)
       ,@body
       (when ,id-expr
         (format ,str-var " {~X}" (if (integerp ,obj-var) ,obj-var 0)))
       (format ,str-var ">")
       nil)))

;;; FR-1004: print-object and describe

(our-defmacro print-object (object stream)
  "Print OBJECT to STREAM using the object's class print method if defined,
otherwise falling back to prin1."
  (let ((obj-v (gensym "OBJ"))
        (str-v (gensym "STR"))
        (cls-v (gensym "CLS"))
        (mth-v (gensym "MTH")))
    `(let* ((,obj-v ,object)
            (,str-v ,stream)
            (,cls-v (when (hash-table-p ,obj-v) (gethash :__class__ ,obj-v)))
            (,mth-v (when (hash-table-p ,cls-v)
                      (gethash :print-object (gethash :__methods__ ,cls-v (make-hash-table))))))
       (if ,mth-v
           (funcall ,mth-v ,obj-v ,str-v)
           (prin1 ,obj-v ,str-v)))))

(our-defmacro describe-object (object stream)
  "Describe OBJECT to STREAM (default: type and slots for CLOS objects)."
  (let ((obj-v (gensym "OBJ"))
        (str-v (gensym "STR"))
        (cls-v (gensym "CLS"))
        (slots-v (gensym "SLS")))
    `(let* ((,obj-v ,object)
            (,str-v ,stream)
            (,cls-v (when (hash-table-p ,obj-v) (gethash :__class__ ,obj-v))))
       (if (hash-table-p ,cls-v)
           (let ((,slots-v (gethash :__slots__ ,cls-v)))
             (format ,str-v "~A is an instance of ~A~%"
                     ,obj-v (gethash :__name__ ,cls-v))
             (dolist (slot ,slots-v)
               (format ,str-v "  ~S = ~S~%"
                       slot (gethash slot ,obj-v))))
           (format ,str-v "~S~%" ,obj-v)))))

(our-defmacro describe (object &optional stream)
  "Print a description of OBJECT to STREAM (default: *standard-output*)."
  (let ((str-v (gensym "STR")))
    `(let ((,str-v (or ,stream *standard-output*)))
       (describe-object ,object ,str-v)
       (values))))

;;; FR-1005: update-instance-for-different-class / update-instance-for-changed-class

(our-defmacro update-instance-for-different-class (previous current &rest initargs)
  "Called after change-class; initializes new slots from INITARGS (stub)."
  (let ((initargs-list initargs)
        (_prev previous))
    (declare (ignore _prev))
    `(reinitialize-instance ,current ,@initargs-list)))

(our-defmacro update-instance-for-changed-class (instance &rest initargs)
  "Called after class redefinition; reinitializes INSTANCE (stub)."
  (let ((initargs-list initargs))
    `(reinitialize-instance ,instance ,@initargs-list)))

;;; FR-1005: ensure-class — create or update a class definition

(our-defmacro ensure-class (name &rest options)
  "Ensure class NAME exists with OPTIONS (stub: delegates to defclass)."
  (let ((direct-superclasses (or (getf options :direct-superclasses) '()))
        (direct-slots (or (getf options :direct-slots) '())))
    `(defclass ,name ,direct-superclasses ,direct-slots)))

;;; FR-1003: change-class — change the class of a CLOS instance

(our-defmacro change-class (instance new-class &rest initargs)
  "Change the class of INSTANCE to NEW-CLASS, preserving matching slots.
INITARGS are passed to update-instance-for-different-class (no-op here)."
  (let ((inst-var (gensym "INST"))
        (new-class-var (gensym "NC")))
    `(let* ((,inst-var ,instance)
            (,new-class-var ,new-class))
       (unless (hash-table-p ,new-class-var)
         (error "change-class: new-class must be a class descriptor hash table"))
       (setf (gethash :__class__ ,inst-var) ,new-class-var)
       ,inst-var)))

;;; FR-302: parse-float — not in ANSI CL but requested; implemented via read-from-string

(our-defmacro parse-float (string &optional start end junk-allowed)
  (let ((sv (gensym "SV")) (_e end) (_j junk-allowed))
    (declare (ignore _e _j))
    `(let ((,sv (if ,start (subseq ,string ,start) ,string)))
       (float (read-from-string ,sv)))))

;;; FR-1005: reinitialize-instance and shared-initialize

(our-defmacro reinitialize-instance (instance &rest initargs)
  "Reinitialize INSTANCE using INITARGS, applying any matching slot values.
For our VM hash-table instances, iterates the class's initarg map."
  (let ((inst-v (gensym "INST"))
        (args-v (gensym "ARGS"))
        (class-v (gensym "CLASS"))
        (imap-v (gensym "IMAP"))
        (pair-v (gensym "PAIR"))
        (slot-v (gensym "SLOT")))
    `(let* ((,inst-v ,instance)
            (,args-v (list ,@initargs))
            (,class-v (gethash :__class__ ,inst-v))
            (,imap-v  (when (hash-table-p ,class-v)
                        (gethash :__initargs__ ,class-v))))
       (when (and ,imap-v ,args-v)
         (let ((,pair-v ,args-v))
           (tagbody
            reinit-loop
            (when (and ,pair-v (cdr ,pair-v))
              (let* ((k (car ,pair-v))
                     (v (cadr ,pair-v))
                     (,slot-v (assoc k ,imap-v)))
                (when ,slot-v
                  (setf (gethash (cdr ,slot-v) ,inst-v) v)))
              (setf ,pair-v (cddr ,pair-v))
              (go reinit-loop)))))
       ,inst-v)))

(our-defmacro shared-initialize (instance slot-names &rest initargs)
  "Initialize SLOT-NAMES in INSTANCE from INITARGS; t means all slots.
For our VM hash-table instances, like reinitialize-instance but slot-filtered."
  (let ((inst-v (gensym "INST"))
        (slots-v (gensym "SLOTS"))
        (args-v (gensym "ARGS"))
        (class-v (gensym "CLASS"))
        (imap-v (gensym "IMAP"))
        (pair-v (gensym "PAIR"))
        (slot-v (gensym "SLOT"))
        (sn-v (gensym "SN")))
    `(let* ((,inst-v ,instance)
            (,slots-v ,slot-names)
            (,args-v (list ,@initargs))
            (,class-v (gethash :__class__ ,inst-v))
            (,imap-v  (when (hash-table-p ,class-v)
                        (gethash :__initargs__ ,class-v))))
       (when (and ,imap-v ,args-v)
         (let ((,pair-v ,args-v))
           (tagbody
            si-loop
            (when (and ,pair-v (cdr ,pair-v))
              (let* ((k (car ,pair-v))
                     (v (cadr ,pair-v))
                     (,slot-v (assoc k ,imap-v)))
                (when ,slot-v
                  (let ((,sn-v (cdr ,slot-v)))
                    (when (or (eq ,slots-v t)
                              (member ,sn-v ,slots-v))
                      (setf (gethash ,sn-v ,inst-v) v)))))
              (setf ,pair-v (cddr ,pair-v))
              (go si-loop)))))
       ,inst-v)))

;;; %plist-put — non-destructive plist update (used by setf getf expansion)
(our-defmacro %plist-put (plist indicator value)
  (let ((p (gensym "P")) (result (gensym "R")) (found (gensym "F"))
        (k (gensym "K")) (v (gensym "V")))
    `(let ((,p ,plist) (,v ,value) (,result nil) (,found nil))
       (loop while ,p do
         (let ((,k (car ,p)))
           (if (eq ,k ,indicator)
               (progn (push ,indicator ,result) (push ,v ,result) (setf ,found t))
               (progn (push ,k ,result) (push (cadr ,p) ,result)))
           (setf ,p (cddr ,p))))
       (unless ,found
         (push ,v ,result) (push ,indicator ,result))
       (nreverse ,result))))

