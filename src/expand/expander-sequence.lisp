(in-package :cl-cc)

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
(dolist (entry '((mapcar :collect) (mapc :side-effect) (mapcan :nconc)))
  (let ((op (first entry)) (mode (second entry)))
    (setf (gethash op *expander-head-table*)
          (lambda (form)
            (let ((nargs (length (cdr form))))
              (cond
                ;; 2-arg: inline the single-list expansion (replaces our-defmacro)
                ((= nargs 2)
                 (let ((fn-var (gensym "FN"))
                       (x (gensym "X"))
                       (acc (gensym "ACC"))
                       (lst (gensym "LST"))
                       (fn-arg (second form))
                       (list-arg (third form)))
                   (compiler-macroexpand-all
                    (ecase mode
                      (:collect
                       `(let ((,fn-var ,fn-arg) (,acc nil))
                          (dolist (,x ,list-arg (nreverse ,acc))
                            (setq ,acc (cons (funcall ,fn-var ,x) ,acc)))))
                      (:side-effect
                       `(let ((,fn-var ,fn-arg) (,lst ,list-arg))
                          (dolist (,x ,lst ,lst)
                            (funcall ,fn-var ,x))))
                      (:nconc
                       `(let ((,fn-var ,fn-arg) (,acc nil))
                          (dolist (,x ,list-arg ,acc)
                            (setq ,acc (nconc ,acc (funcall ,fn-var ,x))))))))))
                ;; 3+ args: multi-list via labels recursion
                ((>= nargs 3)
                 (let* ((fn-arg (second form))
                        (list-args (cddr form))
                        (nlists (length list-args))
                        (fn-var (gensym "FN"))
                        (list-vars (loop for i from 0 below nlists
                                         collect (gensym (format nil "L~D-" i))))
                        (helper (gensym "MAP"))
                        (null-test `(or ,@(mapcar (lambda (v) `(null ,v)) list-vars)))
                        (car-args (mapcar (lambda (v) `(car ,v)) list-vars))
                        (cdr-args (mapcar (lambda (v) `(cdr ,v)) list-vars))
                        (call `(funcall ,fn-var ,@car-args))
                        (recurse `(,helper ,@cdr-args))
                        (body (ecase mode
                                (:collect `(if ,null-test nil
                                             (cons ,call ,recurse)))
                                (:side-effect `(if ,null-test nil
                                                 (progn ,call ,recurse)))
                                (:nconc `(if ,null-test nil
                                            (nconc ,call ,recurse))))))
                   (compiler-macroexpand-all
                    `(let ((,fn-var ,fn-arg))
                       (labels ((,helper ,list-vars ,body))
                         (,helper ,@list-args))))))
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
                 (let ((fn-var (gensym "FN"))
                       (x (gensym "X"))
                       (result (gensym "R"))
                       (pred-arg (second form))
                       (list-arg (third form)))
                   (compiler-macroexpand-all
                    (ecase mode
                      (:every
                       `(let ((,fn-var ,pred-arg))
                          (block nil
                            (dolist (,x ,list-arg t)
                              (unless (funcall ,fn-var ,x)
                                (return nil))))))
                      (:some
                       `(let ((,fn-var ,pred-arg))
                          (block nil
                            (dolist (,x ,list-arg nil)
                              (let ((,result (funcall ,fn-var ,x)))
                                (when ,result (return ,result)))))))))))
                ;; 3+ args: multi-list via labels recursion
                ((>= nargs 3)
                 (let* ((fn-arg (second form))
                        (list-args (cddr form))
                        (nlists (length list-args))
                        (fn-var (gensym "FN"))
                        (list-vars (loop for i from 0 below nlists
                                         collect (gensym (format nil "L~D-" i))))
                        (helper (gensym "QNT"))
                        (null-test `(or ,@(mapcar (lambda (v) `(null ,v)) list-vars)))
                        (car-args (mapcar (lambda (v) `(car ,v)) list-vars))
                        (cdr-args (mapcar (lambda (v) `(cdr ,v)) list-vars))
                        (call `(funcall ,fn-var ,@car-args))
                        (recurse `(,helper ,@cdr-args))
                        (body (ecase mode
                                (:every `(if ,null-test t
                                             (if ,call ,recurse nil)))
                                (:some `(if ,null-test nil
                                            (let ((r ,call))
                                              (if r r ,recurse)))))))
                   (compiler-macroexpand-all
                    `(let ((,fn-var ,fn-arg))
                       (labels ((,helper ,list-vars ,body))
                         (,helper ,@list-args))))))
                (t (error "~A requires at least 2 arguments" op))))))))
