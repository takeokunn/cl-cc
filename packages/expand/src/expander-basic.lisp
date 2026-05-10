(in-package :cl-cc/expand)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Expander — basic application handlers
;;;
;;; Loaded after expander.lisp so define-expander-for and the core helper
;;; functions are available.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun %plist-value-or-default (plist indicator default)
  "Return plist value for INDICATOR, or DEFAULT when absent."
  (getf plist indicator default))

(defun %apply-args-to-cons-chain (args)
  "Build nested CONS forms from APPLY argument tail ARGS."
  (if (null (cdr args))
      (car args)
      (list 'cons (car args) (%apply-args-to-cons-chain (cdr args)))))

(defun %build-multiple-value-let-bindings (vars values-form tmp direct-values-p)
  "Build LET* bindings for MULTIPLE-VALUE-BIND."
  (loop for x in vars
        for i from 0
        collect (list x (if direct-values-p
                            (nth (+ i 1) values-form)
                            (list 'nth i tmp)))))

;; quote — never recurse into quoted forms
(define-expander-for quote (form) form)

;; backquote — expand (backquote template) into list/cons/append
(define-expander-for backquote (form)
  (compiler-macroexpand-all (%expand-quasiquote (second form))))

(defun %quoted-function-designator-symbol (designator)
  "Return the symbol named by a quoted function DESIGNATOR, or NIL."
  (and (consp designator)
       (or (eq (car designator) 'quote)
           (eq (car designator) 'function))
       (symbolp (second designator))
       (second designator)))

;; funcall — (funcall 'name ...) / (funcall #'name ...) with known name → direct call
(define-expander-for funcall (form)
  (let ((name (and (>= (length form) 2)
                   (%quoted-function-designator-symbol (second form)))))
    (if name
        (let ((compiler-macro (lookup-compiler-macro name)))
          (if compiler-macro
              (let ((expanded (invoke-registered-expander compiler-macro form nil)))
                (if (equal expanded form)
                    (compiler-macroexpand-all (cons name (cddr form)))
                    (compiler-macroexpand-all expanded)))
              (compiler-macroexpand-all (cons name (cddr form)))))
        (cons 'funcall (mapcar #'compiler-macroexpand-all (cdr form))))))

;; apply — spread-args normalisation + variadic builtin fold
(define-expander-for apply (form)
  (if (cdddr form)
      ;; (apply fn a1 a2 ... list) with spread args → cons-fold
      (let* ((fn         (second form))
             (combined   (%apply-args-to-cons-chain (cddr form))))
        (compiler-macroexpand-all (list 'apply fn combined)))
      ;; (apply 'name list) or (apply #'name list)
      (if (and (cdr form)
               (cddr form)
               (null (cdddr form))
               (consp (second form))
               (or (and (eq (car (second form)) 'quote)    (symbolp (second (second form))))
                   (and (eq (car (second form)) 'function) (symbolp (second (second form))))))
          (expand-apply-named-fn (second (second form)) (third form))
          ;; default: expand args
          (cons 'apply (mapcar #'compiler-macroexpand-all (cdr form))))))

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
    (if (and (symbolp name) (%list-contains-eq name *all-builtin-names*))
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
         `(let* ,(%build-multiple-value-let-bindings vars values-form nil t)
            ,@body))
        (compiler-macroexpand-all
         `(let ((,tmp (multiple-value-list ,values-form)))
            (let* ,(%build-multiple-value-let-bindings vars values-form tmp nil)
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
         (initial (%plist-value-or-default plist :initial-element :__none__))
         (has-element-type (not (eq (%plist-value-or-default plist :element-type :__none__) :__none__))))
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
