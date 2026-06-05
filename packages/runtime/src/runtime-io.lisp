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
(define-rt-stream-op rt-listen       listen       ())

(defun rt-write-to-string (obj)
  "Native-callable simple write-to-string. Uses current CL print-control variables."
  (write-to-string obj))

(defun rt-write-to-string-with-controls (obj &key base radix escape level length circle pretty case readably gensym array)
  "Native-callable write-to-string with full ANSI print-control keyword support."
  (let ((*print-base*   (or base *print-base*))
        (*print-radix*  (or radix *print-radix*))
        (*print-escape* (if (eq escape nil) nil t))
        (*print-level*  level)
        (*print-length* length)
        (*print-circle* (or circle *print-circle*))
        (*print-pretty* (or pretty *print-pretty*))
        (*print-case*   (or case *print-case*))
        (*print-readably* (or readably *print-readably*))
        (*print-gensym* (or gensym *print-gensym*))
        (*print-array*  (or array *print-array*)))
    (write-to-string obj)))

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
    make-string-output-stream get-output-stream-string write-string write-to-string
    ;; Wave 4: pathname and file system bootstrap
    make-pathname merge-pathnames namestring pathname-name pathname-type
    pathname-directory pathnamep probe-file delete-file rename-file
    truename load
    make-broadcast-stream make-concatenated-stream make-echo-stream)
  "Conservative bootstrap function seed for the runtime registry.
This avoids scanning every host package while still covering the small guest-facing
surface that currently relies on `rt-fboundp` during bootstrapping and tests.")

(defparameter *rt-bootstrap-package-names*
  '(:cl :cl-user :keyword :cl-cc/bootstrap :cl-cc/runtime :cl-cc :cl-cc/vm :cl-cc/compile :cl-cc/expand
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

(defun %rt-symbol-name-key (symbol-or-name)
  "Return the package-symbol table key for SYMBOL-OR-NAME."
  (etypecase symbol-or-name
    (symbol (symbol-name symbol-or-name))
    (string symbol-or-name)
    (character (string symbol-or-name))))

(defun %rt-ensure-package-slots (descriptor)
  "Ensure DESCRIPTOR has all package-registry tables used by self-host mode."
  (unless (gethash :symbols descriptor)
    (setf (gethash :symbols descriptor) (make-hash-table :test #'equal)))
  (unless (gethash :inherited descriptor)
    (setf (gethash :inherited descriptor) (make-hash-table :test #'equal)))
  (unless (nth-value 1 (gethash :exports descriptor))
    (setf (gethash :exports descriptor) nil))
  (unless (nth-value 1 (gethash :nicknames descriptor))
    (setf (gethash :nicknames descriptor) nil))
  (unless (nth-value 1 (gethash :use-list descriptor))
    (setf (gethash :use-list descriptor) nil))
  descriptor)

(defun %rt-package-metadata (package)
  (if (hash-table-p package)
      (%rt-ensure-package-slots package)
      (let ((key (%rt-package-key package)))
        (or (gethash key *rt-package-registry*)
            (let ((descriptor (make-hash-table :test #'eq)))
              (setf (gethash :name descriptor) key)
              (setf (gethash :host-package descriptor) (find-package key))
              (setf (gethash :exports descriptor) nil)
              (setf (gethash :nicknames descriptor) nil)
              (setf (gethash :use-list descriptor) nil)
              (setf (gethash :inherited descriptor) (make-hash-table :test #'equal))
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
                        ("RT-EXPORT" . ,#'rt-export)
                        ("RT-IMPORT" . ,#'rt-import)
                        ("RT-USE-PACKAGE" . ,#'rt-use-package)
                        ("RT-UNUSE-PACKAGE" . ,#'rt-unuse-package)
                        ("RT-SHADOW" . ,#'rt-shadow)
                        ("RT-UNINTERN" . ,#'rt-unintern)
                        ("RT-FIND-SYMBOL" . ,#'rt-find-symbol)
                        ("RT-PACKAGE-NAME" . ,#'rt-package-name)
                        ("RT-PACKAGE-NICKNAMES" . ,#'rt-package-nicknames)
                        ("RT-RENAME-PACKAGE" . ,#'rt-rename-package)
                        ("RT-DELETE-PACKAGE" . ,#'rt-delete-package)
                        ("RT-LIST-ALL-PACKAGES" . ,#'rt-list-all-packages)
                        ("RT-SYMBOL-PACKAGE" . ,#'rt-symbol-package)
                        ("RT-DO-SYMBOLS" . ,#'rt-do-symbols)
                        ("RT-SYMBOL-NAME" . ,#'rt-symbol-name)
                        ("RT-SYMBOL-VALUE" . ,#'rt-symbol-value)
                        ;; Wave 4: Pathname, file system, compound streams, LOAD
                        ("RT-MAKE-PATHNAME" . ,#'rt-make-pathname)
                        ("RT-MERGE-PATHNAMES" . ,#'rt-merge-pathnames)
                        ("RT-NAMESTRING" . ,#'rt-namestring)
                        ("RT-PATHNAME-NAME" . ,#'rt-pathname-name)
                        ("RT-PATHNAME-TYPE" . ,#'rt-pathname-type)
                        ("RT-PATHNAME-DIRECTORY" . ,#'rt-pathname-directory)
                        ("RT-PATHNAMEP" . ,#'rt-pathnamep)
                        ("RT-PROBE-FILE" . ,#'rt-probe-file)
                        ("RT-DELETE-FILE" . ,#'rt-delete-file)
                        ("RT-RENAME-FILE" . ,#'rt-rename-file)
                        ("RT-FILE-WRITE-DATE" . ,#'rt-file-write-date)
                        ("RT-DIRECTORY" . ,#'rt-directory)
                        ("RT-ENSURE-DIRECTORIES-EXIST" . ,#'rt-ensure-directories-exist)
                        ("RT-TRUENAME" . ,#'rt-truename)
                        ("RT-MAKE-BROADCAST-STREAM" . ,#'rt-make-broadcast-stream)
                        ("RT-MAKE-CONCATENATED-STREAM" . ,#'rt-make-concatenated-stream)
                        ("RT-MAKE-ECHO-STREAM" . ,#'rt-make-echo-stream)
                        ("RT-READ-SEQUENCE" . ,#'rt-read-sequence)
                        ("RT-WRITE-SEQUENCE" . ,#'rt-write-sequence)
                        ("RT-WRITE-TO-STRING" . ,#'rt-write-to-string)
                        ("RT-LOAD" . ,#'rt-load)))
        (funcall vm-register (car entry) (cdr entry))))))

(defun rt-find-package (name)
  (and name (gethash (%rt-package-key name) *rt-package-registry*)))

(defun rt-intern (name &optional package)
  (let* ((pkg (or (and package (%rt-package-metadata package))
                   (and *package* (%rt-package-metadata *package*))))
          (table (gethash :symbols (%rt-ensure-package-slots pkg)))
          (key (string name))
          (host-package (gethash :host-package pkg)))
     (or (gethash key table)
         (setf (gethash key table)
               (if host-package
                   (intern key host-package)
                   (make-symbol key))))))

(defun rt-make-package (name &key nicknames use)
  (let* ((key (%rt-package-key name))
         (pkg (%rt-ensure-package-slots
               (or (rt-find-package key)
                   (%rt-register-package key)))))
    (setf (gethash key *rt-package-registry*) pkg)
    (when nicknames
      (setf (gethash :nicknames pkg) (mapcar #'%rt-package-key nicknames))
      (dolist (nickname (gethash :nicknames pkg))
        (setf (gethash nickname *rt-package-registry*) pkg)))
    (when use
      (rt-use-package (mapcar #'%rt-package-metadata use) pkg))
    pkg))

(defun rt-export (symbols package)
  (let* ((pkg (%rt-package-metadata package))
          (syms (if (listp symbols) symbols (list symbols))))
    (setf (gethash :exports pkg)
          (union syms (gethash :exports pkg) :test #'eq))
    syms))

(defun rt-unexport (symbols &optional package)
  "Make SYMBOLS internal in PACKAGE."
  (let* ((pkg (%rt-package-metadata (or package *package*)))
         (syms (if (listp symbols) symbols (list symbols))))
    (setf (gethash :exports pkg)
          (set-difference (gethash :exports pkg) syms :test #'eq))
    t))

;;; ─── Extended Runtime Package Operations (ANSI CL Ch.11) ──────────────────

(defun rt-import (symbols &optional package)
  "Import SYMBOLS into PACKAGE, making them internal."
  (let* ((pkg (%rt-package-metadata (or package *package*)))
         (table (gethash :symbols pkg))
         (syms (if (listp symbols) symbols (list symbols))))
    (dolist (sym syms)
        (let ((name (%rt-symbol-name-key sym)))
        ;; Check for conflict with existing symbol
        (multiple-value-bind (existing status)
            (rt-find-symbol name pkg)
          (when (and existing (not (eq existing sym)))
            (if (eq status :external)
                ;; Name conflict: existing external symbol
                (error "Name conflict: ~S already exported from ~A" name (rt-package-name pkg))
                ;; Shadow the existing symbol with the imported one
                (rt-unintern existing pkg))))
        (setf (gethash name table) sym)
        ;; Remove from inherited table if present
        (let ((inherited (gethash :inherited pkg)))
          (when inherited
            (remhash name inherited)))))
    t))

(defun rt-use-package (packages-to-use &optional package &key (errorp t))
  "Make PACKAGES-TO-USE accessible to PACKAGE."
  (let* ((pkg (%rt-package-metadata (or package *package*)))
         (use-list (gethash :use-list pkg))
         (inherited (or (gethash :inherited pkg)
                        (setf (gethash :inherited pkg) (make-hash-table :test #'equal))))
         (pkgs (if (listp packages-to-use) packages-to-use (list packages-to-use))))
    (labels ((conflict (control &rest args)
               (if errorp
                   (apply #'error control args)
                   (return-from rt-use-package :conflict))))
      ;; Check for name conflicts among packages to use and with present symbols.
      (let ((exports-by-name (make-hash-table :test #'equal)))
        (dolist (used-pkg pkgs)
          (let ((used-meta (%rt-package-metadata used-pkg)))
            (dolist (ext-sym (gethash :exports used-meta))
              (let* ((name (%rt-symbol-name-key ext-sym))
                     (previous (gethash name exports-by-name)))
                (when (and previous (not (eq previous ext-sym)))
                  (conflict "Name conflict: ~S is exported by multiple used packages" name))
                (setf (gethash name exports-by-name) ext-sym))))))
      (dolist (used-pkg pkgs)
        (let ((used-meta (%rt-package-metadata used-pkg)))
          (dolist (ext-sym (gethash :exports used-meta))
            (let* ((name (%rt-symbol-name-key ext-sym))
                   (existing (gethash name (gethash :symbols pkg))))
              (when (and existing (not (eq existing ext-sym)))
                (conflict "Name conflict: ~S is already present in ~A" name (rt-package-name pkg)))))))
      ;; Merge use-list
      (let ((new-use (append (mapcar #'%rt-package-metadata pkgs)
                             (remove-if (lambda (u) (member (%rt-package-key u)
                                                            (mapcar #'%rt-package-key (mapcar #'%rt-package-metadata pkgs))
                                                            :test #'equal))
                                        use-list))))
        (setf (gethash :use-list pkg) new-use))
      ;; Populate inherited symbols
      (clrhash inherited)
      (dolist (used-meta (gethash :use-list pkg))
        (dolist (ext-sym (gethash :exports used-meta))
          (setf (gethash (%rt-symbol-name-key ext-sym) inherited) ext-sym)))
      t)))

(defun rt-unuse-package (packages-to-unuse &optional package)
  "Remove PACKAGES-TO-UNUSE from the use-list of PACKAGE."
  (let* ((pkg (%rt-package-metadata (or package *package*)))
         (pkgs (if (listp packages-to-unuse) packages-to-unuse (list packages-to-unuse)))
         (keys-to-remove (mapcar #'%rt-package-key (mapcar #'%rt-package-metadata pkgs))))
    (setf (gethash :use-list pkg)
          (remove-if (lambda (u) (member (%rt-package-key u) keys-to-remove :test #'equal))
                     (gethash :use-list pkg)))
    ;; Rebuild inherited table
    (let ((inherited (or (gethash :inherited pkg)
                         (setf (gethash :inherited pkg) (make-hash-table :test #'equal)))))
      (clrhash inherited)
      (dolist (used-meta (gethash :use-list pkg))
        (dolist (ext-sym (gethash :exports used-meta))
          (setf (gethash (%rt-symbol-name-key ext-sym) inherited) ext-sym))))
    t))

(defun rt-shadow (symbol-names &optional package)
  "Create shadowing symbols for SYMBOL-NAMES in PACKAGE."
  (let* ((pkg (%rt-package-metadata (or package *package*)))
         (table (gethash :symbols pkg))
         (names (if (listp symbol-names) symbol-names (list symbol-names))))
    (dolist (name names)
      (let ((key (string name)))
        (unless (gethash key table)
          ;; Create a new internal symbol
          (setf (gethash key table)
                (let ((host-pkg (gethash :host-package pkg)))
                  (if host-pkg
                      (intern key host-pkg)
                      (make-symbol key)))))
        (let ((inherited (gethash :inherited pkg)))
          (when inherited (remhash key inherited)))))
    t))

(defun rt-shadowing-import (symbols &optional package)
  "Import SYMBOLS into PACKAGE and mark them as shadowing present/inherited names."
  (let* ((pkg (%rt-package-metadata (or package *package*)))
         (table (gethash :symbols (%rt-ensure-package-slots pkg)))
         (inherited (gethash :inherited pkg))
         (syms (if (listp symbols) symbols (list symbols))))
    (dolist (sym syms)
      (let ((name (%rt-symbol-name-key sym)))
        (when inherited (remhash name inherited))
        (setf (gethash name table) sym)))
    t))

(defun rt-unintern (symbol &optional package)
  "Remove SYMBOL from PACKAGE."
  (let* ((pkg (%rt-package-metadata (or package *package*)))
         (table (gethash :symbols pkg))
          (key (%rt-symbol-name-key symbol)))
    (remhash key table)
    ;; Remove from exports if present
    (setf (gethash :exports pkg)
          (remove symbol (gethash :exports pkg) :test #'eq))
    t))

(defun rt-find-symbol (name &optional package)
  "Find symbol NAME in PACKAGE. Returns (values symbol status)."
  (let* ((pkg (%rt-package-metadata (or package *package*)))
         (table (gethash :symbols pkg))
          (key (string name))
         (sym (gethash key table)))
    (if sym
        (if (member sym (gethash :exports pkg) :test #'eq)
            (values sym :external)
            (values sym :internal))
        ;; Check inherited symbols
        (let ((inherited (gethash :inherited pkg)))
          (if (and inherited (gethash key inherited))
              (values (gethash key inherited) :inherited)
              (values nil nil))))))

(defun rt-package-name (package)
  "Return the name string of PACKAGE."
  (let ((pkg (%rt-package-metadata package)))
    (gethash :name pkg)))

(defun rt-package-nicknames (package)
  "Return the list of nickname strings for PACKAGE."
  (let ((pkg (%rt-package-metadata package)))
    (gethash :nicknames pkg)))

(defun rt-find-all-symbols (name)
  "Return all present symbols named NAME across registered packages."
  (let ((key (string name))
        (symbols nil))
    (maphash (lambda (_ pkg)
               (declare (ignore _))
               (let ((sym (gethash key (gethash :symbols pkg))))
                 (when sym (pushnew sym symbols :test #'eq))))
             *rt-package-registry*)
    (nreverse symbols)))

(defun rt-rename-package (package new-name &optional new-nicknames)
  "Rename PACKAGE to NEW-NAME with optional NEW-NICKNAMES."
  (let* ((pkg (%rt-package-metadata package))
         (old-name (gethash :name pkg)))
    ;; Remove old entry, add new
    (remhash old-name *rt-package-registry*)
     (setf (gethash :name pkg) (%rt-package-key new-name))
    (when new-nicknames
      (setf (gethash :nicknames pkg) (mapcar #'%rt-package-key new-nicknames)))
     (setf (gethash (gethash :name pkg) *rt-package-registry*) pkg)
    ;; Register nicknames
    (when new-nicknames
      (dolist (nick (gethash :nicknames pkg))
        (setf (gethash nick *rt-package-registry*) pkg)))
    pkg))

(defun rt-delete-package (package)
  "Delete PACKAGE from the runtime registry."
  (let* ((pkg (%rt-package-metadata package))
         (name (gethash :name pkg)))
    (remhash name *rt-package-registry*)
    ;; Remove nickname entries
    (dolist (nick (gethash :nicknames pkg))
      (remhash nick *rt-package-registry*))
    t))

(defun rt-list-all-packages ()
  "Return a list of all registered runtime packages."
  (let ((pkgs '())
        (seen (make-hash-table :test #'eq)))
    (maphash (lambda (key pkg)
               (declare (ignore key))
               (unless (gethash pkg seen)
                 (setf (gethash pkg seen) t)
                 (push pkg pkgs)))
             *rt-package-registry*)
    pkgs))

(defun rt-symbol-package (symbol)
  "Return the home package of SYMBOL, or NIL if uninterned.
In runtime registry, search all package symbol tables."
  (maphash (lambda (key pkg)
             (declare (ignore key))
             (let ((table (gethash :symbols pkg)))
                (multiple-value-bind (present foundp)
                    (gethash (%rt-symbol-name-key symbol) table)
                  (when (and foundp (eq present symbol))
                    (return-from rt-symbol-package pkg)))))
           *rt-package-registry*)
  nil)

(defun rt-do-symbols (fn &optional package)
  "Call FN on each symbol in PACKAGE (present and inherited)."
  (let* ((pkg (%rt-package-metadata (or package *package*)))
         (table (gethash :symbols pkg))
         (inherited (gethash :inherited pkg))
         (seen (make-hash-table :test #'eq)))
    (maphash (lambda (key sym)
               (declare (ignore key))
               (unless (gethash sym seen)
                 (setf (gethash sym seen) t)
                 (funcall fn sym)))
             table)
    (when inherited
      (maphash (lambda (key sym)
                 (declare (ignore key))
                 (unless (gethash sym seen)
                   (setf (gethash sym seen) t)
                   (funcall fn sym)))
                inherited))))

(defun rt-do-external-symbols (fn &optional package)
  "Call FN on each external symbol in PACKAGE."
  (dolist (sym (copy-list (gethash :exports (%rt-package-metadata (or package *package*)))))
    (funcall fn sym)))

(defun rt-do-all-symbols (fn)
  "Call FN on every present symbol in every registered package."
  (let ((seen (make-hash-table :test #'eq)))
    (maphash (lambda (_ pkg)
               (declare (ignore _))
               (maphash (lambda (_ sym)
                          (declare (ignore _))
                          (unless (gethash sym seen)
                            (setf (gethash sym seen) t)
                            (funcall fn sym)))
                        (gethash :symbols pkg)))
             *rt-package-registry*)))

(defun rt-symbol-name (symbol)
  "Return the name string of SYMBOL."
  (string symbol))

;;; ─── Bootstrap Hook Installation ──────────────────────────────────────────

(defun %rt-install-bootstrap-hook (key value)
  "Install VALUE into the bootstrap hook symbol named by KEY when available."
  (let ((sym (%rt-bootstrap-hook-symbol key)))
    (when (and sym value)
      (set sym value))))

(eval-when (:load-toplevel :execute)
  (%rt-bootstrap-package-registry)
  (%rt-bootstrap-function-registry))

#-cl-cc-self-hosting
(eval-when (:load-toplevel :execute)
  (%rt-install-bootstrap-hook :register-hook #'%rt-register-vm-runtime-callables)
  (%rt-install-bootstrap-hook :registry-provider (lambda () *rt-package-registry*))
  (%rt-install-bootstrap-hook :find-package-fn #'rt-find-package)
  (%rt-install-bootstrap-hook :intern-fn #'rt-intern)
  (%rt-install-bootstrap-hook :set-symbol-value-fn #'rt-set-symbol-value)
  (%rt-register-vm-runtime-callables))

;;; ─── Native Bignum Support (ANSI CL Ch.12 Number Tower) ────────────────────
;;;
;;; The native codegen calls these runtime helpers when integer arithmetic
;;; overflows fixnum range.  Bignums are represented as tagged cons cells
;;; (:bignum . cl-integer).  The codegen fast path handles fixnum arithmetic
;;; inline; these functions handle the slow path (overflow / mixed types).

(declaim (inline rt-native-bignum-p))

(defun rt-native-bignum-allocate (n)
  "Allocate a bignum value wrapping the CL integer N."
  (cons :bignum n))

(defun rt-native-bignum-p (value)
  "Return T when VALUE is a bignum."
  (and (consp value) (eq (car value) :bignum)))

(defun rt-native-bignum-to-integer (value)
  "Convert a fixed-size (fixnum or bignum) VALUE to a CL integer."
  (if (rt-native-bignum-p value)
      (cdr value)
      (decode-fixnum value)))

(defun rt-native-integer->value (n)
  "Box a CL integer N into the runtime value representation.
  Returns an encoded fixnum when N fits in 51-bit signed range;
  otherwise returns a bignum."
  (if (and (>= n #.(- (ash 1 50)))
           (<= n #.(1- (ash 1 50))))
      (encode-fixnum n)
      (rt-native-bignum-allocate n)))

(defun rt-native-bignum-add (a b)
  "Add two fixed-size integer values A and B.  Returns fixnum when possible."
  (rt-native-integer->value
   (+ (rt-native-bignum-to-integer a) (rt-native-bignum-to-integer b))))

(defun rt-native-bignum-sub (a b)
  "Subtract B from A.  Returns fixnum when possible."
  (rt-native-integer->value
   (- (rt-native-bignum-to-integer a) (rt-native-bignum-to-integer b))))

(defun rt-native-bignum-mul (a b)
  "Multiply A and B.  Returns fixnum when possible."
  (rt-native-integer->value
   (* (rt-native-bignum-to-integer a) (rt-native-bignum-to-integer b))))

;;; ─── Pathname and File System Operations (Wave 4) ──────────────────────────

(defun rt-make-pathname (&key host device directory name type version defaults case)
  (declare (ignore host device version case))
  (let ((p (if defaults
                (merge-pathnames (make-pathname :directory directory :name name :type type) defaults)
                (make-pathname :directory directory :name name :type type))))
    p))

(defun rt-make-pathname-native (host device directory name type version defaults)
  "Native-callable positional-arg version of make-pathname.  All args may be NIL.
   Calls CL:MAKE-PATHNAME with only non-NIL keyword arguments."
  (let ((args nil))
    (when host (push (cons :host host) args))
    (when device (push (cons :device device) args))
    (when directory (push (cons :directory directory) args))
    (when name (push (cons :name name) args))
    (when type (push (cons :type type) args))
    (when version (push (cons :version version) args))
    (let ((p (apply #'make-pathname (nreverse args))))
      (if defaults
          (merge-pathnames p defaults)
          p))))

(defun rt-merge-pathnames (pathname &optional defaults)
  (merge-pathnames pathname (or defaults *default-pathname-defaults*)))

(defun rt-namestring (pathname)
  (namestring pathname))

(defun rt-pathname-name (pathname)
  (pathname-name pathname))

(defun rt-pathname-type (pathname)
  (pathname-type pathname))

(defun rt-pathname-directory (pathname)
  (pathname-directory pathname))

(defun rt-pathnamep (object)
  (pathnamep object))

(defun rt-probe-file (pathname)
  (probe-file pathname))

(defun rt-delete-file (pathname)
  (delete-file pathname))

(defun rt-rename-file (file new-name)
  (rename-file file new-name))

(defun rt-file-write-date (pathname)
  (file-write-date pathname))

(defun rt-directory (pathname &key)
  (directory pathname))

(defun rt-ensure-directories-exist (pathname &key verbose)
  (ensure-directories-exist pathname :verbose verbose))

(defun rt-truename (pathname)
  (truename pathname))

;;; ─── Compound Streams ───────────────────────────────────────────────────────

(defun rt-make-broadcast-stream (&rest streams)
  (apply #'make-broadcast-stream streams))

(defun rt-make-concatenated-stream (&rest streams)
  (apply #'make-concatenated-stream streams))

(defun rt-make-echo-stream (input-stream output-stream)
  (make-echo-stream input-stream output-stream))

(defun rt-make-two-way-stream (input-stream output-stream)
  (make-two-way-stream input-stream output-stream))

(defun rt-make-synonym-stream (symbol)
  (make-synonym-stream symbol))

;;; ─── Sequence I/O ───────────────────────────────────────────────────────────

(defun rt-read-sequence (sequence stream &key start end)
  (read-sequence sequence stream :start (or start 0) :end (or end (length sequence))))

(defun rt-write-sequence (sequence stream &key start end)
  (write-sequence sequence stream :start (or start 0) :end (or end (length sequence))))

;;; ─── LOAD ───────────────────────────────────────────────────────────────────

(defun rt-load (pathname &key verbose print if-does-not-exist external-format)
  (declare (ignore verbose print if-does-not-exist external-format))
  (load pathname))

;;; ─── JIT-Callable Bignum Bridges (for native codegen slow path) ────────────

(sb-alien:define-alien-callable cl_cc_bignum_add
    sb-alien:long
    ((a sb-alien:long) (b sb-alien:long))
  (rt-native-bignum-add a b))

(sb-alien:define-alien-callable cl_cc_bignum_sub
    sb-alien:long
    ((a sb-alien:long) (b sb-alien:long))
  (rt-native-bignum-sub a b))

(sb-alien:define-alien-callable cl_cc_bignum_mul
    sb-alien:long
    ((a sb-alien:long) (b sb-alien:long))
  (rt-native-bignum-mul a b))
