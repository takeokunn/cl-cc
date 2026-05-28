;;;; macros-package-system.lisp — package system and symbol-iteration macros
(in-package :cl-cc/expand)

;;; Package-system macros over the current runtime-registry-backed package layer

(defun %host-package-designator (designator)
  (if (hash-table-p designator)
      (or (gethash :host-package designator)
          (gethash :name designator))
      designator))

(defun %host-package-local-nickname-function (name)
  (or (find-symbol name :cl)
      (let ((pkg (find-package :sb-ext)))
        (and pkg (find-symbol name pkg)))))

(defun add-package-local-nickname (package local-nickname actual-package)
  (let ((fn (%host-package-local-nickname-function "ADD-PACKAGE-LOCAL-NICKNAME")))
    (funcall fn local-nickname
             (%host-package-designator actual-package)
             (%host-package-designator package))))

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
             (export-list nil)
             (local-nicknames nil))
      ;; Parse package options
        (dolist (opt options)
          (when (consp opt)
            (case (first opt)
               (:use (setf use-list (rest opt)))
               (:export (setf export-list (rest opt)))
               (:local-nicknames (setf local-nicknames (rest opt))))))
         `(progn
            (let ((pkg (or (rt-find-package ',name)
                            (rt-make-package ',name ,@(when use-list `(:use ',use-list))))))
              ,@(when local-nicknames
                  `((dolist (entry ',local-nicknames)
                      (add-package-local-nickname pkg (first entry) (second entry)))))
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

;; export, import, use-package, unuse-package, shadow, unintern,
;; shadowing-import are ANSI CL FUNCTIONS (not macros). They are
;; dispatched via the builtin registry to VM instructions.

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
