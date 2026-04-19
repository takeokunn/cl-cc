;;;; macros-compat.lisp — ANSI CL compatibility and system macros
(in-package :cl-cc/expand)

;;; Package System — delegates to host CL via vm-host-bridge

(register-macro 'in-package
  (lambda (form env)
    (declare (ignore env))
    (let ((name (second form)))
      (list 'progn
            (list 'setq '*package* (list 'find-package name))
            (list 'quote name)))))

(register-macro 'defpackage
  (lambda (form env)
    (declare (ignore env))
    (let* ((name (second form))
           (options (cddr form))
           (use-list nil)
           (export-list nil)
           (local-nicknames nil))
      ;; Parse :use, :export, and :local-nicknames options
      (dolist (opt options)
        (when (consp opt)
          (case (first opt)
             (:use (setf use-list (rest opt)))
             (:export (setf export-list (rest opt)))
             (:local-nicknames (setf local-nicknames (rest opt))))))
      `(progn
         (let ((pkg (or (find-package ',name)
                         (make-package ',name ,@(when use-list `(:use ',use-list))))))
           ,@(when local-nicknames
               `((dolist (entry ',local-nicknames)
                   (destructuring-bind (nickname target) entry
                     (ignore-errors (sb-ext:remove-package-local-nickname nickname pkg))
                     (sb-ext:add-package-local-nickname nickname target pkg)))))
           ,@(when export-list
               `((dolist (sym ',export-list)
                   (export (intern (string sym) pkg) pkg))))
           (quote ,name))))))

(defun %expand-package-iteration (binding-spec symbol-list-form-fn body)
  (let ((var (first binding-spec))
        (package (second binding-spec))
        (result (third binding-spec))
        (pkg-var (gensym "PKG"))
        (syms-var (gensym "SYMS")))
    `(let* ((,pkg-var ,(if package `(find-package ,package) '*package*))
            (,syms-var ,(funcall symbol-list-form-fn pkg-var)))
       (dolist (,var ,syms-var ,result)
         ,@body))))

(defun %ignore-argument-expand (argument constant)
  (let ((arg-var (gensym "ARG")))
    (list 'let (list (list arg-var argument))
          (list 'declare (list 'ignore arg-var))
          constant)))

;; export — removed no-op stub; now delegates to host CL via vm-host-bridge

;; do-symbols / do-external-symbols / do-all-symbols (FR-361)
;; These expand to dolist over package symbol lists obtained via host bridge.
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

