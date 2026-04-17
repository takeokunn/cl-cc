;;;; macros-compat.lisp — ANSI CL compatibility and system macros
(in-package :cl-cc/expand)

;;; Package System — delegates to host CL via vm-host-bridge

(our-defmacro in-package (name)
  `(progn (setq *package* (find-package ,name)) (quote ,name)))

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
    `(let ((,arg-var ,argument))
       (declare (ignore ,arg-var))
       ,constant)))

;; export — removed no-op stub; now delegates to host CL via vm-host-bridge

;; do-symbols / do-external-symbols / do-all-symbols (FR-361)
;; These expand to dolist over package symbol lists obtained via host bridge.
(our-defmacro do-symbols (binding-spec &body body)
  (%expand-package-iteration binding-spec
                             (lambda (pkg-var) `(%package-symbols ,pkg-var))
                             body))

(our-defmacro do-external-symbols (binding-spec &body body)
  (%expand-package-iteration binding-spec
                             (lambda (pkg-var) `(%package-external-symbols ,pkg-var))
                             body))

(our-defmacro do-all-symbols (binding-spec &body body)
  (let ((var (first binding-spec))
        (result (second binding-spec))
        (syms-var (gensym "SYMS")))
    `(let ((,syms-var (%all-symbols)))
       (dolist (,var ,syms-var ,result)
         ,@body))))

;;; Declaration (silently ignored)

(our-defmacro declare (&rest decls)
  (declare (ignore decls))
  nil)

;;; Global declaration (silently ignored — same semantics as declare)

(our-defmacro declaim (&rest decls)
  (declare (ignore decls))
  nil)

;;; Scope with Declarations

(our-defmacro locally (&body forms)
  ;; FR-397: preserve declarations by wrapping in (let () decls body)
  (let ((decls (remove-if-not (lambda (f) (and (consp f) (eq (car f) 'declare))) forms))
        (body  (remove-if (lambda (f) (and (consp f) (eq (car f) 'declare))) forms)))
    (if decls
        `(let () ,@decls ,@body)
        `(progn ,@body))))

;; PROGV (FR-102) — dynamic variable binding
;; Uses vm-progv-enter/vm-progv-exit to save and restore global-vars around body.
(our-defmacro progv (symbols values &body body)
  "Bind SYMBOLS to VALUES dynamically for the duration of BODY."
  (let ((syms-var (gensym "SYMS"))
        (vals-var (gensym "VALS"))
        (saved-var (gensym "SAVED")))
    `(let* ((,syms-var ,symbols)
            (,vals-var ,values)
            (,saved-var (%progv-enter ,syms-var ,vals-var)))
       (unwind-protect
         (progn ,@body)
         (%progv-exit ,saved-var)))))

;;; File I/O

(our-defmacro with-open-file (stream-spec &body body)
  "Bind VAR to an open stream for PATH, execute BODY, then close the stream.
   STREAM-SPEC is (var path &rest open-options)."
  (let* ((var     (first stream-spec))
         (path    (second stream-spec))
         (options (cddr stream-spec)))
    `(let ((,var (open ,path ,@options)))
       (unwind-protect (progn ,@body)
         (close ,var)))))

;;; Warning Output

(our-defmacro warn (fmt &rest args)
  `(progn
     (format t ,(concatenate 'string "~&WARNING: "
                             (if (stringp fmt) fmt "~A"))
             ,@args)
     nil))

;;; Hash Table Utilities

(our-defmacro copy-hash-table (ht)
  (let ((ht-var (gensym "HT"))
        (new-var (gensym "NEW"))
        (k-var   (gensym "K"))
        (v-var   (gensym "V")))
    `(let ((,ht-var ,ht))
       (let ((,new-var (make-hash-table :test (hash-table-test ,ht-var))))
         (maphash (lambda (,k-var ,v-var)
                    (setf (gethash ,k-var ,new-var) ,v-var))
                  ,ht-var)
         ,new-var))))

;; FR-555: copy-structure — VM structs are hash-tables with :__class__
(our-defmacro copy-structure (struct)
  `(copy-hash-table ,struct))

;;; Type Coercion

(our-defmacro coerce (value type-form)
  ;; FR-630: quoted literal types → direct call; dynamic types → runtime dispatch
  (if (and (consp type-form) (eq (car type-form) 'quote))
      (let ((type (second type-form)))
        (cond
          ((and (symbolp type) (member type '(string simple-string base-string)))
           `(coerce-to-string ,value))
          ((eq type 'list)
           `(coerce-to-list ,value))
          ((or (and (symbolp type) (member type '(vector simple-vector)))
               (and (consp type) (member (car type) '(vector simple-array array))))
           `(coerce-to-vector ,value))
          ((eq type 'character) `(character ,value))
          ((member type '(float single-float double-float short-float long-float))
           `(float ,value))
          (t `(%coerce-runtime ,value ,type-form))))
      `(%coerce-runtime ,value ,type-form)))

;;; Compile-time Evaluation

(defvar *load-time-value-cache* (make-hash-table :test #'equal)
  "Memoizes LOAD-TIME-VALUE expansion results during compiler macroexpansion.")

;; LOAD-TIME-VALUE — evaluate at compile time, splice in the quoted result.
(our-defmacro load-time-value (form &optional read-only-p)
  (declare (ignore read-only-p))
  (multiple-value-bind (cached present-p)
      (gethash form *load-time-value-cache*)
    (unless present-p
      (setf cached (eval form))
      (setf (gethash form *load-time-value-cache*) cached))
    `(quote ,cached)))

;;; FR-1206: Module/feature system — *features*, *modules*, provide, require

(our-defmacro provide (module-name)
  "Mark MODULE-NAME as loaded by pushing its string name onto *modules*."
  (let ((mod (gensym "MOD")))
    `(let ((,mod (string ,module-name)))
       (pushnew ,mod *modules* :test #'string=)
       ,mod)))

(our-defmacro require (module-name &optional pathnames)
  "Load files in PATHNAMES if MODULE-NAME is not already in *modules*."
  (let ((mod (gensym "MOD"))
        (pn  (gensym "PATHS")))
    `(let ((,mod (string ,module-name))
           (,pn  ,pathnames))
       (unless (member ,mod *modules* :test #'string=)
         (if ,pn
             (dolist (p ,pn) (our-load p))
             (warn "Module ~A not loaded" ,mod)))
       ,mod)))
