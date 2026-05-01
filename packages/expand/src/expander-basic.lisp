(in-package :cl-cc/expand)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Expander — basic application handlers
;;;
;;; Loaded after expander.lisp so define-expander-for and the core helper
;;; functions are available.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; quote — never recurse into quoted forms
(define-expander-for quote (form) form)

;; backquote — expand (backquote template) into list/cons/append
(define-expander-for backquote (form)
  (compiler-macroexpand-all (%expand-quasiquote (second form))))

;; funcall — (funcall 'name ...) with quoted symbol → direct call
(define-expander-for funcall (form)
  (if (and (>= (length form) 2)
           (consp (second form))
           (eq (car (second form)) 'quote)
           (symbolp (second (second form))))
      (compiler-macroexpand-all (cons (second (second form)) (cddr form)))
      (cons 'funcall (mapcar #'compiler-macroexpand-all (cdr form)))))

;; apply — spread-args normalisation + variadic builtin fold
(define-expander-for apply (form)
  (cond
    ;; (apply fn a1 a2 ... list) with spread args → cons-fold
    ((> (length form) 3)
     (let* ((fn         (second form))
             (spread-args (butlast (cddr form)))
             (last-arg    (car (last form)))
             (combined    (reduce (lambda (a rest) (list 'cons a rest))
                                  spread-args :from-end t :initial-value last-arg)))
       (compiler-macroexpand-all (list 'apply fn combined))))
    ;; (apply 'name list) or (apply #'name list)
    ((and (= (length form) 3)
          (consp (second form))
          (or (and (eq (car (second form)) 'quote)    (symbolp (second (second form))))
              (and (eq (car (second form)) 'function) (symbolp (second (second form))))))
     (expand-apply-named-fn (second (second form)) (third form)))
    ;; default: expand args
    (t (cons 'apply (mapcar #'compiler-macroexpand-all (cdr form))))))

;; make-hash-table — :test #'fn → :test 'fn normalisation
(define-expander-for make-hash-table (form)
  (cond
    ((and (>= (length form) 3)
          (eq (second form) :test)
          (consp (third form))
          (eq (car (third form)) 'function)
          (symbolp (second (third form))))
     (compiler-macroexpand-all
      `(,(find-symbol "%MAKE-HASH-TABLE-WITH-TEST" "CL-CC/VM")
         ',(second (third form)))))
    ((and (>= (length form) 3)
          (eq (second form) :test)
          (consp (third form))
          (eq (car (third form)) 'quote)
          (symbolp (second (third form))))
     (compiler-macroexpand-all
      `(,(find-symbol "%MAKE-HASH-TABLE-WITH-TEST" "CL-CC/VM")
         ',(second (third form)))))
    (t
     (cons 'make-hash-table (mapcar #'compiler-macroexpand-all (cdr form))))))

;; function — wrap builtins in first-class lambda
(define-expander-for function (form)
  (let ((name (second form)))
    (if (and (symbolp name) (member name *all-builtin-names*))
        (expand-function-builtin name)
        form)))

;; multiple-value-list — must be here (not our-defmacro): %values-to-list is
;; position-sensitive and must follow the multi-valued form with no gap.
(define-expander-for multiple-value-list (form)
  (let ((values-form (second form)))
    (if (and (consp values-form) (eq (car values-form) 'values))
        (compiler-macroexpand-all `(list ,@(cdr values-form)))
        (let ((tmp (gensym "MVL")))
          (compiler-macroexpand-all
           `(let ((,tmp ,values-form))
              (declare (ignore ,tmp))
              (%values-to-list)))))))

;; multiple-value-bind — canonicalize eagerly so it does not fall into the
;; special-form lowering path before our simpler values-list-based expansion runs.
(define-expander-for multiple-value-bind (form)
  (let ((vars (second form))
        (values-form (third form))
        (body (cdddr form))
        (tmp (gensym "MVB")))
    (if (and (consp values-form) (eq (car values-form) 'values))
        (compiler-macroexpand-all
         `(let* ,(mapcar (lambda (var i)
                           `(,var ,(or (nth (1+ i) values-form) nil)))
                         vars
                         (loop for i below (length vars) collect i))
            ,@body))
        (compiler-macroexpand-all
         `(let ((,tmp (multiple-value-list ,values-form)))
            (let* ,(mapcar (lambda (var i)
                             `(,var (nth ,i ,tmp)))
                           vars
                           (loop for i below (length vars) collect i))
              ,@body))))))

;; multiple-value-call — canonicalize through multiple-value-list + apply so we
;; stay on the simpler, already-verified apply path.
(define-expander-for multiple-value-call (form)
  (let ((func (second form))
        (value-forms (cddr form)))
    (compiler-macroexpand-all
     (cond
       ((null value-forms)
        `(funcall ,func))
       ((null (cdr value-forms))
        `(apply ,func (multiple-value-list ,(first value-forms))))
       (t
        `(apply ,func
                (append ,@(mapcar (lambda (vf) `(multiple-value-list ,vf))
                                  value-forms))))))))

;; format nil — stabilize string-returning format via explicit string stream.
(define-expander-for format (form)
  (let ((dest (second form)))
    (cond
      ((null dest)
       (compiler-macroexpand-all
        `(let ((s (make-string-output-stream)))
           (format s ,(third form) ,@(cdddr form))
           (get-output-stream-string s))))
      ((eq dest t)
       (compiler-macroexpand-all
        `(progn
           (princ (format nil ,(third form) ,@(cdddr form)))
           nil)))
      (t form))))

;; make-string with keyword args — canonicalize to a simple fill loop.
(define-expander-for make-string (form)
  (let* ((args (cdr form))
         (size (first args))
         (plist (rest args))
         (initial (getf plist :initial-element :__none__))
         (has-element-type (not (eq (getf plist :element-type :__none__) :__none__))))
    (cond
      ((and (eq initial :__none__) (not has-element-type))
       form)
      ((eq initial :__none__)
       `(make-string ,size))
      (t
       (let ((len (gensym "LEN"))
             (char (gensym "CHAR"))
             (str (gensym "STR"))
             (idx (gensym "IDX")))
         (compiler-macroexpand-all
          `(let ((,len ,size)
                 (,char ,initial))
             (let ((,str (make-string ,len)))
               (dotimes (,idx ,len ,str)
                 (setf (char ,str ,idx) ,char))))))))))
