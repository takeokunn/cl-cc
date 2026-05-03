(in-package :cl-cc/expand)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Expander — sequence higher-order handlers
;;;
;;; Loaded after expander.lisp so compiler-macroexpand-all is available.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; APPEND (FR-665): 0-arg → nil, 1-arg → identity, N-arg → left fold
(define-expander-for append (form)
  (reduce-variadic-op 'append (mapcar #'compiler-macroexpand-all (cdr form)) nil))

;; NCONC (FR-665): 0-arg → nil, 1-arg → identity, N-arg → left fold
(define-expander-for nconc (form)
  (reduce-variadic-op 'nconc (mapcar #'compiler-macroexpand-all (cdr form)) nil))

;; FR-665: mapcar/mapc/mapcan — multi-sequence support
;; 2-arg → inline single-list expansion, 3+ args → labels recursion over N lists
(defun %sequence-build-null-test (list-vars)
  (cons 'or (mapcar (lambda (v) (list 'null v)) list-vars)))

(defun %sequence-build-car-args (list-vars)
  (mapcar (lambda (v) (list 'car v)) list-vars))

(defun %sequence-build-cdr-args (list-vars)
  (mapcar (lambda (v) (list 'cdr v)) list-vars))

(defun %sequence-build-single-map-form (mode fn-arg list-arg)
  (let ((fn-var (gensym "FN"))
        (lst (gensym "LST"))
        (helper (gensym "MAP1")))
    (if (eq mode :collect)
        (list 'let (list (list fn-var fn-arg))
              (list 'labels
                    (list (list helper (list lst)
                                (list 'if (list 'null lst)
                                      nil
                                      (list 'cons
                                            (list 'funcall fn-var (list 'car lst))
                                            (list helper (list 'cdr lst))))))
                    (list helper list-arg)))
        (if (eq mode :side-effect)
            (let ((input (gensym "INPUT")))
              (list 'let (list (list fn-var fn-arg) (list input list-arg))
                    (list 'labels
                          (list (list helper (list lst)
                                      (list 'if (list 'null lst)
                                            input
                                            (list 'progn
                                                  (list 'funcall fn-var (list 'car lst))
                                                  (list helper (list 'cdr lst))))))
                          (list helper input))))
            (list 'let (list (list fn-var fn-arg))
                  (list 'labels
                        (list (list helper (list lst)
                                    (list 'if (list 'null lst)
                                          nil
                                          (list 'nconc
                                                (list 'funcall fn-var (list 'car lst))
                                                (list helper (list 'cdr lst))))))
                        (list helper list-arg)))))))

(defun %sequence-build-multi-map-form (mode fn-arg list-args)
  (let* ((nlists (length list-args))
         (fn-var (gensym "FN"))
         (list-vars (%make-indexed-gensyms nlists "L"))
         (helper (gensym "MAP"))
         (null-test (%sequence-build-null-test list-vars))
         (car-args (%sequence-build-car-args list-vars))
         (cdr-args (%sequence-build-cdr-args list-vars))
         (call (cons 'funcall (cons fn-var car-args)))
         (recurse (cons helper cdr-args))
         (body (if (eq mode :collect)
                   (list 'if null-test nil (list 'cons call recurse))
                   (if (eq mode :side-effect)
                       (list 'if null-test nil (list 'progn call recurse))
                       (list 'if null-test nil (list 'nconc call recurse))))))
    (list 'let (list (list fn-var fn-arg))
          (list 'labels (list (list helper list-vars body))
                (cons helper list-args)))))

(defun %sequence-build-single-quantifier-form (mode pred-arg list-arg)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (result (gensym "R")))
    (if (eq mode :every)
        (list 'let (list (list fn-var pred-arg))
              (list 'block nil
                    (list 'dolist (list x list-arg t)
                          (list 'unless (list 'funcall fn-var x)
                                '(return nil)))))
        (list 'let (list (list fn-var pred-arg))
              (list 'block nil
                    (list 'dolist (list x list-arg nil)
                          (list 'let (list (list result (list 'funcall fn-var x)))
                                (list 'when result (list 'return result)))))))))

(defun %sequence-build-multi-quantifier-form (mode fn-arg list-args)
  (let* ((nlists (length list-args))
         (fn-var (gensym "FN"))
         (list-vars (%make-indexed-gensyms nlists "L"))
         (helper (gensym "QNT"))
         (null-test (%sequence-build-null-test list-vars))
         (car-args (%sequence-build-car-args list-vars))
         (cdr-args (%sequence-build-cdr-args list-vars))
         (call (cons 'funcall (cons fn-var car-args)))
         (recurse (cons helper cdr-args))
         (body (if (eq mode :every)
                   (list 'if null-test t (list 'if call recurse nil))
                   (let ((result (gensym "R")))
                     (list 'if null-test nil
                           (list 'let (list (list result call))
                                 (list 'if result result recurse)))))))
    (list 'let (list (list fn-var fn-arg))
          (list 'labels (list (list helper list-vars body))
                (cons helper list-args)))))

(dolist (entry '((mapcar :collect) (mapc :side-effect) (mapcan :nconc)))
  (let ((op (first entry)) (mode (second entry)))
    (setf (gethash op *expander-head-table*)
          (lambda (form)
            (let ((nargs (length (cdr form))))
              (cond
                ;; 2-arg: inline the single-list expansion (replaces our-defmacro)
                ((= nargs 2)
                 (compiler-macroexpand-all
                  (%sequence-build-single-map-form mode (second form) (third form))))
                ;; 3+ args: multi-list via labels recursion
                ((>= nargs 3)
                 (compiler-macroexpand-all
                  (%sequence-build-multi-map-form mode (second form) (cddr form))))
                (t (error "~A requires at least 2 arguments" op))))))))

;; FR-650: every/some — multi-sequence support
;; 2-arg → inline single-list expansion, 3+ args → labels recursion over N lists
(dolist (entry '((every :every) (some :some)))
  (let ((op (first entry)) (mode (second entry)))
    (setf (gethash op *expander-head-table*)
          (lambda (form)
            (let ((nargs (length (cdr form))))
              (cond
                ;; 2-arg: inline the single-list expansion (replaces our-defmacro)
                ((= nargs 2)
                 (compiler-macroexpand-all
                  (%sequence-build-single-quantifier-form mode (second form) (third form))))
                ;; 3+ args: multi-list via labels recursion
                ((>= nargs 3)
                 (compiler-macroexpand-all
                  (%sequence-build-multi-quantifier-form mode (second form) (cddr form))))
                (t (error "~A requires at least 2 arguments" op))))))))
