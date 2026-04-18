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
        (x (gensym "X"))
        (acc (gensym "ACC"))
        (lst (gensym "LST")))
    (ecase mode
      (:collect
       (list 'let (list (list fn-var fn-arg) (list acc nil))
             (list 'dolist (list x list-arg (list 'nreverse acc))
                   (list 'setq acc (list 'cons (list 'funcall fn-var x) acc)))))
      (:side-effect
       (list 'let (list (list fn-var fn-arg) (list lst list-arg))
             (list 'dolist (list x lst lst)
                   (list 'funcall fn-var x))))
      (:nconc
       (list 'let (list (list fn-var fn-arg) (list acc nil))
             (list 'dolist (list x list-arg acc)
                   (list 'setq acc (list 'nconc acc (list 'funcall fn-var x)))))))))

(defun %sequence-build-multi-map-form (mode fn-arg list-args)
  (let* ((nlists (length list-args))
         (fn-var (gensym "FN"))
         (list-vars (loop for i from 0 below nlists
                          collect (gensym (format nil "L~D-" i))))
         (helper (gensym "MAP"))
         (null-test (%sequence-build-null-test list-vars))
         (car-args (%sequence-build-car-args list-vars))
         (cdr-args (%sequence-build-cdr-args list-vars))
         (call (cons 'funcall (cons fn-var car-args)))
         (recurse (cons helper cdr-args))
         (body (ecase mode
                 (:collect (list 'if null-test nil (list 'cons call recurse)))
                 (:side-effect (list 'if null-test nil (list 'progn call recurse)))
                 (:nconc (list 'if null-test nil (list 'nconc call recurse))))))
    (list 'let (list (list fn-var fn-arg))
          (list 'labels (list (list helper list-vars body))
                (cons helper list-args)))))

(defun %sequence-build-single-quantifier-form (mode pred-arg list-arg)
  (let ((fn-var (gensym "FN"))
        (x (gensym "X"))
        (result (gensym "R")))
    (ecase mode
      (:every
       (list 'let (list (list fn-var pred-arg))
             (list 'block nil
                   (list 'dolist (list x list-arg t)
                         (list 'unless (list 'funcall fn-var x)
                               '(return nil))))))
      (:some
       (list 'let (list (list fn-var pred-arg))
             (list 'block nil
                   (list 'dolist (list x list-arg nil)
                         (list 'let (list (list result (list 'funcall fn-var x)))
                               (list 'when result (list 'return result))))))))))

(defun %sequence-build-multi-quantifier-form (mode fn-arg list-args)
  (let* ((nlists (length list-args))
         (fn-var (gensym "FN"))
         (list-vars (loop for i from 0 below nlists
                          collect (gensym (format nil "L~D-" i))))
         (helper (gensym "QNT"))
         (null-test (%sequence-build-null-test list-vars))
         (car-args (%sequence-build-car-args list-vars))
         (cdr-args (%sequence-build-cdr-args list-vars))
         (call (cons 'funcall (cons fn-var car-args)))
         (recurse (cons helper cdr-args))
         (body (ecase mode
                 (:every (list 'if null-test t (list 'if call recurse nil)))
                 (:some (list 'if null-test nil
                              (list 'let '((r PLACEHOLDER))
                                    (list 'if 'r 'r recurse)))))))
    (when (eq mode :some)
      (setf body (subst call 'PLACEHOLDER body)))
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
