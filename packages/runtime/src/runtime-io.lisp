(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; I/O
;;; ------------------------------------------------------------

;;; Macro for optional-stream I/O: (rt-foo args... &optional stream)
;;; delegates to (cl-fn args... stream) or (cl-fn args...) for default stream.
(defmacro define-rt-stream-op (rt-name cl-name (&rest fixed-args))
  (list 'defun rt-name
        (append fixed-args '(&optional stream))
        (list 'if 'stream
              (cons cl-name (append fixed-args '(stream)))
              (cons cl-name fixed-args))))

(defun rt-print (x) (print x))
(defun rt-princ (x) (princ x))
(defun rt-prin1 (x) (prin1 x))
(defun rt-terpri () (terpri))
(defun rt-fresh-line () (fresh-line))
(define-rt-stream-op rt-write-char   write-char   (c))
(define-rt-stream-op rt-write-string write-string (s))
(define-rt-stream-op rt-write-line   write-line   (s))
(define-rt-stream-op rt-unread-char  unread-char  (c))
(define-rt-stream-op rt-read-char    read-char    ())
(define-rt-stream-op rt-read-line    read-line    ())
(define-rt-stream-op rt-finish-output finish-output ())
(define-rt-stream-op rt-force-output  force-output  ())
(define-rt-stream-op rt-clear-output  clear-output  ())
(defun rt-write-byte (byte &optional stream)
  (if stream (write-byte byte stream) (write-byte byte *standard-output*)))
(defun rt-format (stream fmt &rest args)
  (apply #'format stream fmt args))
(defun rt-read-byte (&optional stream)
  (if stream (read-byte stream) (read-byte *standard-input*)))
(defun rt-peek-char (&optional stream)
  (if stream (peek-char nil stream nil nil) (peek-char nil *standard-input* nil nil)))
(defun rt-open-file (path &key (direction :input) if-exists)
  (open path :direction direction :if-exists (or if-exists :supersede)))
(defun rt-close-file (stream) (close stream))
(defun rt-make-string-stream (s &key (direction :input))
  (if (eq direction :input)
      (make-string-input-stream s)
      (make-string-output-stream)))
(defun rt-make-string-output-stream () (make-string-output-stream))
(defun rt-get-output-stream-string (stream) (get-output-stream-string stream))
(defun rt-stream-write-string (stream s) (write-string s stream))
(define-rt-predicate rt-input-stream-p       input-stream-p)
(define-rt-predicate rt-output-stream-p      output-stream-p)
(define-rt-predicate rt-open-stream-p        open-stream-p)
(define-rt-predicate rt-interactive-stream-p interactive-stream-p)
(defun rt-stream-element-type (s) (stream-element-type s))

(defvar *rt-gensym-counter* 0
  "Runtime-local gensym counter used by `rt-gensym`."
  )

(defvar *rt-function-registry* (make-hash-table :test #'eq)
  "Runtime function registry used by `rt-fboundp` and related symbol/meta helpers.")

(defparameter *rt-bootstrap-function-symbols*
  '(+ - * / 1+ 1- < > <= >= eql equal equalp char= char-equal
    boundp makunbound fboundp fdefinition intern gensym symbol-value
    make-string-output-stream get-output-stream-string write-string)
  "Conservative bootstrap function seed for the runtime registry.
This avoids scanning every host package while still covering the small guest-facing
surface that currently relies on `rt-fboundp` during bootstrapping and tests.")

(defparameter *rt-bootstrap-package-names*
  '(:cl :cl-user :keyword :cl-cc/runtime :cl-cc :cl-cc/vm :cl-cc/compile :cl-cc/expand
    :cl-cc/parse :cl-cc/type :cl-cc/prolog :cl-cc/optimize :cl-cc/emit :cl-cc/ast)
  "Conservative bootstrap package seed for the runtime package registry.
This avoids importing the full host package universe while preserving the packages
the compiler/runtime currently names directly during selfhost and test flows.")

(defun %rt-bootstrap-function-registry ()
  "Seed the runtime function registry from a conservative explicit symbol set."
  (clrhash *rt-function-registry*)
  (dolist (sym *rt-bootstrap-function-symbols*)
    (setf (gethash sym *rt-function-registry*) t))
  *rt-function-registry*)

(defun rt-fboundp (sym)
  (if (gethash sym *rt-function-registry*) 1 0))
(defun rt-gensym (&optional (prefix "G"))
  (let ((name (format nil "~A~D" prefix *rt-gensym-counter*)))
    (incf *rt-gensym-counter*)
    (make-symbol name)))


(defvar *rt-package-registry* (make-hash-table :test #'equal)
  "Runtime package metadata keyed by package designator name.")

(defun %rt-package-key (name-or-package)
  (etypecase name-or-package
    (hash-table (gethash :name name-or-package))
    (package (package-name name-or-package))
    (string name-or-package)
    (symbol (string name-or-package))))

(defun %rt-package-metadata (package)
  (if (hash-table-p package)
      package
      (let ((key (%rt-package-key package)))
        (or (gethash key *rt-package-registry*)
            (let ((descriptor (make-hash-table :test #'eq)))
              (setf (gethash :name descriptor) key)
              (setf (gethash :exports descriptor) nil)
              (setf (gethash :symbols descriptor) (make-hash-table :test #'equal))
              (setf (gethash key *rt-package-registry*) descriptor)
              descriptor)))))

(defun %rt-register-package (package)
  "Ensure PACKAGE designator is present in the runtime package registry and return its descriptor."
  (when package
    (%rt-package-metadata package)))

(defun %rt-bootstrap-package-registry ()
  "Seed the runtime package registry from a conservative explicit package set.
This keeps runtime lookup centered on the registry without importing the full
host package universe." 
  (clrhash *rt-package-registry*)
  (dolist (pkg-name *rt-bootstrap-package-names*)
    (%rt-register-package pkg-name))
  *rt-package-registry*)

(defun %rt-bootstrap-symbol (sym-name)
  "Return the bootstrap symbol SYM-NAME from :cl-cc/bootstrap when available."
  (let ((pkg (find-package :cl-cc/bootstrap)))
    (and pkg (find-symbol sym-name pkg))))

(defparameter *rt-bootstrap-hook-names*
  '((:installer           . "*VM-RUNTIME-CALLABLE-INSTALLER*")
    (:register-hook       . "*RUNTIME-VM-CALLABLE-REGISTER-HOOK*")
    (:registry-provider   . "*RUNTIME-PACKAGE-REGISTRY-PROVIDER*")
    (:find-package-fn     . "*RUNTIME-FIND-PACKAGE-FN*")
    (:intern-fn           . "*RUNTIME-INTERN-FN*")
    (:set-symbol-value-fn . "*RUNTIME-SET-SYMBOL-VALUE-FN*"))
  "Maps keyword keys to bootstrap symbol names in :cl-cc/bootstrap.")

(defun %rt-bootstrap-hook-symbol (key)
  "Return the bootstrap symbol for KEY from *rt-bootstrap-hook-names*."
  (%rt-bootstrap-symbol (cdr (assoc key *rt-bootstrap-hook-names*))))

(defun %rt-register-vm-runtime-callables ()
  "Register runtime helper callables into the VM bridge registry when VM is loaded."
  (let* ((installer-sym (%rt-bootstrap-hook-symbol :installer))
         (vm-register (and installer-sym
                           (boundp installer-sym)
                           (symbol-value installer-sym))))
    (when vm-register
      (dolist (entry `(("RT-1+" . ,#'rt-1+)
                       ("RT-1-" . ,#'rt-1-)
                       ("RT-+" . ,#'rt-+)
                       ("RT--" . ,#'rt--)
                       ("RT-*" . ,#'rt-*)
                       ("RT-/" . ,#'rt-/)
                       ("RT-<" . ,#'rt-<)
                       ("RT->" . ,#'rt->)
                       ("RT-<=" . ,#'rt-<=)
                       ("RT->=" . ,#'rt->=)
                       ("RT-MAX" . ,#'rt-max)
                       ("RT-MIN" . ,#'rt-min)
                       ("RT-LENGTH" . ,#'rt-length)
                       ("RT-CHAR-EQUAL" . ,#'rt-char-equal)
                       ("RT-CHAR=" . ,#'rt-char=)
                        ("RT-EQL" . ,#'rt-eql)
                        ("RT-EQUAL" . ,#'rt-equal)
                        ("RT-EQUALP" . ,#'rt-equalp)
                        ("RT-TYPEP" . ,#'rt-typep)
                        ("RT-ELT" . ,#'rt-elt)
                       ("RT-APPEND" . ,#'rt-append)
                       ("RT-FBOUNDP" . ,#'rt-fboundp)
                       ("RT-INTERN" . ,#'rt-intern)
                       ("RT-GENSYM" . ,#'rt-gensym)
                       ("RT-SYMBOL-VALUE" . ,#'rt-symbol-value)))
        (funcall vm-register (car entry) (cdr entry))))))

(defun rt-find-package (name)
  (gethash (%rt-package-key name) *rt-package-registry*))

(defun rt-intern (name &optional package)
  (let* ((pkg (or (and package (%rt-package-metadata package))
                  (and *package* (%rt-package-metadata *package*))))
         (table (gethash :symbols pkg))
         (key (string name)))
    (or (gethash key table)
        (setf (gethash key table) (make-symbol key)))))

(defun rt-make-package (name &key use)
  (let ((pkg (or (rt-find-package name)
                 (%rt-register-package name))))
    (when use
      (setf (gethash :use-list pkg)
            (mapcar #'%rt-package-metadata use)))
    pkg))

(defun rt-export (symbols package)
  (let* ((pkg (%rt-package-metadata package))
         (syms (if (listp symbols) symbols (list symbols))))
    (setf (gethash :exports pkg)
          (union syms (gethash :exports pkg) :test #'eq))
    syms))

(eval-when (:load-toplevel :execute)
  (%rt-bootstrap-package-registry)
  (%rt-bootstrap-function-registry)
  (loop for (key . _) in *rt-bootstrap-hook-names*
        for sym = (%rt-bootstrap-hook-symbol key)
        for value = (case key
                      (:register-hook       #'%rt-register-vm-runtime-callables)
                      (:registry-provider   (lambda () *rt-package-registry*))
                      (:find-package-fn     #'rt-find-package)
                      (:intern-fn           #'rt-intern)
                      (:set-symbol-value-fn #'rt-set-symbol-value)
                      (t nil))
        when (and sym value)
          do (setf (symbol-value sym) value))
  (%rt-register-vm-runtime-callables))
