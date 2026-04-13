;;;; macros-control-flow.lisp — Control-flow bootstrap macros
(in-package :cl-cc)

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

;; DEFUN/C macro
;; Minimal design-by-contract support: :requires, :ensures, :invariant.
(our-defmacro defun/c (name params &rest clauses)
  (let ((requires nil)
        (ensures nil)
        (invariant nil)
        (body clauses))
    (loop while (and body
                     (keywordp (first body))
                     (member (first body) '(:requires :ensures :invariant) :test #'eq)
                     (cdr body))
          do (let ((key (pop body))
                   (value (pop body)))
               (case key
                 (:requires (setf requires value))
                 (:ensures  (setf ensures value))
                 (:invariant (setf invariant value)))))
    (let* ((pre-check  (remove nil (list requires invariant)))
           (post-check (remove nil (list ensures invariant)))
           (pre-form   (when pre-check
                         `(unless (and ,@pre-check)
                            (error "Precondition failed in ~S" ',name))))
           (post-form  (when post-check
                         `(unless (and ,@post-check)
                            (error "Postcondition failed in ~S" ',name)))))
      `(defun ,name ,params
         ,@(when pre-form (list pre-form))
         (let ((result (progn ,@body)))
           ,@(when post-form (list post-form))
           result)))))

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

;; CASE and TYPECASE macros are in macros-control-flow-case.lisp (loaded next).
