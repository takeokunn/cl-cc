(in-package :cl-cc/vm)

;;; --- Symbol Table (freeze/thaw, lookup, weak symbols) ---
;;; --- FR-622 / FR-896: Package Locks ---
;;;
;;; Extracted from vm.lisp to give the symbol-table subsystem its own SRP
;;; boundary.  Loaded after vm-dsl.lisp in cl-cc-vm.asd.
;;;

;; FR-622: Package locks (enhanced by FR-896)
(defvar *vm-package-locks* (make-hash-table :test #'eq))

(eval-when (:load-toplevel :execute)
  (let ((cl-package (find-package :cl)))
    (when cl-package
      (setf (gethash cl-package *vm-package-locks*) t))))

(defun vm-lock-package (package)
  (setf (gethash package *vm-package-locks*) t) package)

(defun vm-unlock-package (package)
  (remhash package *vm-package-locks*) package)

(defun vm-package-locked-p (package)
  (gethash package *vm-package-locks*))

(define-condition vm-package-locked-error (error)
  ((package :initarg :package :reader vm-package-locked-error-package))
  (:report (lambda (c s) (format s "Package ~S is locked" (vm-package-locked-error-package c)))))

(defvar *symbol-table* (make-hash-table :test #'equal)
  "Dynamic symbol table mapping string-name -> symbol.
Used by the runtime package layer for intern operations.")

(defvar *symbol-table-frozen* nil
  "When non-NIL, *symbol-table* is frozen and no new symbols may be added.")

(defvar *symbol-table-compact* nil
  "When frozen, a sorted vector of (name-string . symbol) pairs for compact
binary-search lookups. NIL when thawed.")

(defvar *symbol-table-weak* (make-hash-table :test #'equal :weakness :key)
  "Weak hash-table for GC-collectible symbol tracking.
Entries are removed when the symbol is no longer referenced elsewhere.")

(defun freeze-symbol-table ()
  "Freeze *symbol-table*, switching to a read-only compact sorted vector.
Returns the compact table.  While frozen, `(setf lookup-symbol)` signals an error."
  (let* ((pairs (loop for name being the hash-keys of *symbol-table*
                      for sym being the hash-values of *symbol-table*
                      collect (cons name sym)))
         (sorted (sort pairs #'string< :key #'car)))
    (setf *symbol-table-compact* (coerce sorted 'vector))
    (setf *symbol-table-frozen* t)
    (values *symbol-table-compact* (hash-table-count *symbol-table*))))

(defun thaw-symbol-table ()
  "Unfreeze *symbol-table*, restoring dynamic hash-table-based operation."
  (setf *symbol-table-frozen* nil)
  (setf *symbol-table-compact* nil)
  *symbol-table*)

(defun %symbol-table-binary-search (pairs name)
  "Binary-search PAIRS (sorted compact vector of (name . symbol)) for NAME."
  (let ((low 0)
        (high (1- (length pairs))))
    (loop
      (when (> low high) (return nil))
      (let* ((mid (floor (+ low high) 2))
             (pair (aref pairs mid))
             (key (car pair)))
        (cond ((string= key name) (return (cdr pair)))
              ((string< key name) (setf low (1+ mid)))
              (t (setf high (1- mid))))))))

(defun lookup-symbol (name)
  "Look up a symbol by its NAME string.
Uses binary search on the compact vector when frozen, otherwise the hash-table."
  (if *symbol-table-compact*
      (%symbol-table-binary-search *symbol-table-compact* name)
      (gethash name *symbol-table*)))

(defun (setf lookup-symbol) (sym name)
  "Register SYM under NAME.  Signals an error when the table is frozen."
  (when *symbol-table-frozen*
    (error "Cannot intern new symbol ~S: symbol table is frozen" name))
  (setf (gethash name *symbol-table*) sym))

(defun register-weak-symbol (sym)
  "Register SYM in the weak symbol table for GC-collectible tracking.
Returns SYM."
  (setf (gethash (symbol-name sym) *symbol-table-weak*) sym)
  sym)

(defvar *symbol-index-table* nil
  "Lazily-built hash-table mapping symbol -> sequential index for profiling.")

(defun symbol-index (sym)
  "Return a sequential integer index for SYM, suitable for profiler/debugger
metadata.  Indexes are assigned monotonically starting from 0."
  (unless *symbol-index-table*
    (setf *symbol-index-table* (make-hash-table :test #'eq)))
  (or (gethash sym *symbol-index-table*)
      (let ((idx (hash-table-count *symbol-index-table*)))
        (setf (gethash sym *symbol-index-table*) idx)
        idx)))

;;; --- FR-896: Package Lock / Sealed ---

(defvar *locked-packages* (list (find-package :cl))
  "List of locked packages.  Defaults to COMMON-LISP.
Use LOCK-PACKAGE / UNLOCK-PACKAGE to manage.")

(defun lock-package (package &optional (lock t))
  "Lock PACKAGE (when LOCK is non-NIL) or unlock it.
Locked packages prevent new symbol internment, export, import, and shadowing.
Also engages SBCL's native package lock so that CL:INTERN signals an error."
  (if lock
      (progn (pushnew package *locked-packages* :test #'eq)
             (vm-lock-package package)
             (handler-case (sb-ext:lock-package package) (error () nil)))
      (progn (setf *locked-packages* (remove package *locked-packages* :test #'eq))
             (vm-unlock-package package)
             (handler-case (sb-ext:unlock-package package) (error () nil))))
  package)

(defun unlock-package (package)
  "Unlock PACKAGE (convenience wrapper)."
  (lock-package package nil))

(defun package-locked-p (package)
  "Return T when PACKAGE is locked."
  (vm-package-locked-p package))

;;; Alias package-locked-error to SBCL's native type.
;;; Under selfhost, package-locked-error is a standalone condition type.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (find-class 'package-locked-error)
        (find-class 'sb-ext:package-locked-error)))

(defun check-package-lock (package &optional (operation :intern))
  "Signal PACKAGE-LOCKED-ERROR when PACKAGE is locked.
OPERATION is :INTERN, :EXPORT, :IMPORT, or :SHADOW (for error messages)."
  (declare (ignore operation))
  (when (vm-package-locked-p package)
    (error 'package-locked-error :package package)))

(defmacro with-unlocked-packages ((&rest packages) &body body)
  "Execute BODY with PACKAGES temporarily unlocked.
Each element of PACKAGES is a package designator (symbol or string).
Packages are re-locked when BODY exits (via UNWIND-PROTECT).
Also temporarily disengages SBCL's native lock so CL:INTERN works inside."
  (let ((pkg-vars (mapcar (lambda (p) (gensym (format nil "~A-UNLOCK" p))) packages))
        (thunk (gensym "WITH-UNLOCKED-THUNK")))
    `(flet ((,thunk () ,@body))
       (let ,(mapcar (lambda (pv pd) `(,pv (find-package ,pd))) pkg-vars packages)
         (unwind-protect
              (progn
                ,@(mapcar (lambda (pv)
                            `(when (and ,pv (vm-package-locked-p ,pv))
                               (vm-unlock-package ,pv)
                               (handler-case (sb-ext:unlock-package ,pv) (error () nil))))
                          pkg-vars)
                (,thunk))
           ,@(mapcar (lambda (pv)
                        `(when ,pv
                           (vm-lock-package ,pv)
                           (handler-case (sb-ext:lock-package ,pv) (error () nil))))
                      pkg-vars))))))
