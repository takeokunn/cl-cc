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
  (if (and (>= (length form) 3)
           (eq (second form) :test)
           (consp (third form))
           (eq (car (third form)) 'function)
           (symbolp (second (third form))))
      (compiler-macroexpand-all
       (append (list 'make-hash-table :test (list 'quote (second (third form))))
               (cdddr form)))
      (cons 'make-hash-table (mapcar #'compiler-macroexpand-all (cdr form)))))

;; function — wrap builtins in first-class lambda
(define-expander-for function (form)
  (let ((name (second form)))
    (if (and (symbolp name) (member name *all-builtin-names*))
        (expand-function-builtin name)
        form)))

;; multiple-value-list — must be here (not our-defmacro): %values-to-list is
;; position-sensitive and must follow the multi-valued form with no gap.
(define-expander-for multiple-value-list (form)
  (let ((tmp (gensym "MVL")))
    (compiler-macroexpand-all
     (list 'let (list (list tmp (second form)))
           (list 'declare (list 'ignore tmp))
           '(%values-to-list)))))
