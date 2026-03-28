(in-package :cl-cc)
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
  (declare (ignore type-string))
  `(unless (typep ,place ',type)
     (error (make-condition 'type-error
              :datum ,place
              :expected-type ',type))))

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

