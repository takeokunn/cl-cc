;;;; macros-package-system.lisp — package system and symbol-iteration macros
(in-package :cl-cc/expand)

;;; Package-system macros over the current runtime-registry-backed package layer

(register-macro 'in-package
  (lambda (form env)
    (declare (ignore env))
    (let ((name (second form)))
      (list 'progn
            (list 'setq '*package* (list 'rt-find-package name))
            (list 'quote name)))))

(register-macro 'defpackage
  (lambda (form env)
    (declare (ignore env))
     (let* ((name (second form))
            (options (cddr form))
            (use-list nil)
            (export-list nil))
      ;; Parse :use and :export options
       (dolist (opt options)
         (when (consp opt)
           (case (first opt)
              (:use (setf use-list (rest opt)))
              (:export (setf export-list (rest opt))))))
        `(progn
           (let ((pkg (or (rt-find-package ',name)
                           (rt-make-package ',name ,@(when use-list `(:use ',use-list))))))
             ,@(when export-list
                 `((dolist (sym ',export-list)
                     (rt-export (rt-intern (string sym) pkg) pkg))))
             (quote ,name))))))

(defun %expand-package-iteration (binding-spec symbol-list-form-fn body)
  (let ((var (first binding-spec))
        (package (second binding-spec))
        (result (third binding-spec))
        (pkg-var (gensym "PKG"))
        (syms-var (gensym "SYMS")))
    `(let* ((,pkg-var ,(if package `(rt-find-package ,package) '*package*))
            (,syms-var ,(funcall symbol-list-form-fn pkg-var)))
       (dolist (,var ,syms-var ,result)
          ,@body))))

;; export is handled by the host-backed package layer.

;; do-symbols / do-external-symbols / do-all-symbols (FR-361)
;; Expand to dolist over package symbol lists provided by the runtime package layer.
(register-macro 'do-symbols
  (lambda (form env)
    (declare (ignore env))
    (%expand-package-iteration (second form)
                               (lambda (pkg-var) (list '%package-symbols pkg-var))
                               (cddr form))))

(register-macro 'do-external-symbols
  (lambda (form env)
    (declare (ignore env))
    (%expand-package-iteration (second form)
                               (lambda (pkg-var) (list '%package-external-symbols pkg-var))
                               (cddr form))))

(register-macro 'do-all-symbols
  (lambda (form env)
    (declare (ignore env))
      (let* ((binding-spec (second form))
             (body (cddr form))
             (var (first binding-spec))
             (result (second binding-spec))
             (syms-var (gensym "SYMS")))
      (list 'let (list (list syms-var '(%all-symbols)))
            (cons 'dolist (cons (list var syms-var result) body))))))
