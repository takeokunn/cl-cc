;;; ─── Runtime Stdlib-2 Completion ─────────────────────────────────────
;;; Fills remaining gaps from docs/runtime-stdlib-2.md (Phases 138-175).
;;; Features already implemented elsewhere (primitives.lisp, macros-lazy.lisp,
;;; vm-clos.lisp, stream.lisp, etc.) are NOT duplicated here.

(in-package :cl-cc/vm)

;; ── FR-895: Symbol Table Freeze/Thaw ───────────────────────────────────

(defparameter *symbol-table-frozen* nil
  "When T, no new symbols can be interned.")

(defun freeze-symbol-table ()
  "Freeze the current symbol table to a read-only state."
  (setf *symbol-table-frozen* t) t)

(defun thaw-symbol-table ()
  "Thaw the symbol table, allowing new symbols again."
  (setf *symbol-table-frozen* nil) t)

;; ── FR-896: Package Lock ───────────────────────────────────────────────

(defparameter *locked-packages* (make-hash-table :test #'eq)
  "Set of locked package names.")

(defun lock-package (package)
  "Lock PACKAGE to prevent symbol redefinition."
  (setf (gethash (if (packagep package) (package-name package) package)
                 *locked-packages*) t) t)

(defun package-locked-p (package)
  "Return T if PACKAGE is locked."
  (gethash (if (packagep package) (package-name package) package)
           *locked-packages*))

;; ── FR-917: Reproducible Build Fingerprint ─────────────────────────────

(defparameter *build-seed* nil
  "When non-nil, used as fixed seed for reproducible builds.")

(defun build-fingerprint (&rest input-files)
  "Compute a simple fingerprint (hash) of the given INPUT-FILES contents."
  (let ((hash 5381))
    (dolist (file input-files)
      (handler-case
          (with-open-file (in file :direction :input :if-does-not-exist nil)
            (when in
              (loop for c = (read-char in nil nil)
                    while c
                    do (setf hash (logand (+ (* hash 33) (char-code c)) #xFFFFFFFF)))))
        (error () nil)))
    hash))

;; ── FR-920: Forward References ─────────────────────────────────────────

(defstruct vm-forward-reference-cell
  "Mutable function cell used for top-level forward references."
  (name nil :read-only t)
  (value nil))

(defvar *unresolved-forward-refs* nil
  "Alist of (NAME . VM-FORWARD-REFERENCE-CELL) entries still unresolved.")

(defvar *vm-forward-reference-auto-resolve-enabled* t
  "When true, RUN-COMPILED performs a final forward-reference warning pass.")

(defun vm-forward-reference-cell-ref (cell)
  "Return CELL's current function value, or NIL when unresolved."
  (vm-forward-reference-cell-value cell))

(defun (setf vm-forward-reference-cell-ref) (value cell)
  "Publish VALUE into forward-reference CELL."
  (setf (vm-forward-reference-cell-value cell) value))

(defun vm-declare-forward-reference (state name)
  "Declare NAME as a forward-referenced function in STATE and return its cell."
  (let* ((registry (vm-function-registry state))
         (existing (gethash name registry)))
    (cond
      ((vm-forward-reference-cell-p existing) existing)
      (existing
       (let ((cell (make-vm-forward-reference-cell :name name :value existing)))
         (setf (gethash name registry) cell)
         cell))
      (t
       (let ((cell (make-vm-forward-reference-cell :name name)))
         (setf (gethash name registry) cell)
         (pushnew (cons name cell) *unresolved-forward-refs* :key #'car :test #'eq)
         cell)))))

(defun vm-resolve-forward-references (&optional (state *vm-current-state*))
  "Warn for forward-reference cells in STATE that remain unresolved."
  (declare (ignore state))
  (let ((resolved nil)
        (unresolved nil)
        (remaining nil))
    (dolist (entry *unresolved-forward-refs*)
      (let ((name (car entry))
            (cell (cdr entry)))
        (if (and (vm-forward-reference-cell-p cell)
                 (vm-forward-reference-cell-value cell))
            (push name resolved)
            (progn
              (push name unresolved)
              (push entry remaining)
              (warn "Unresolved forward reference: ~S" name)))))
    (setf *unresolved-forward-refs* (nreverse remaining))
    (values (nreverse resolved) (nreverse unresolved))))

(defun declare-forward-reference (name)
  "Compatibility wrapper: declare NAME in the current VM state."
  (if *vm-current-state*
      (vm-declare-forward-reference *vm-current-state* name)
      name))

(defun resolve-forward-reference (name fn)
  "Compatibility wrapper: resolve NAME's cell to FN in the current VM state."
  (when *vm-current-state*
    (let ((entry (gethash name (vm-function-registry *vm-current-state*))))
      (when (vm-forward-reference-cell-p entry)
        (setf (vm-forward-reference-cell-ref entry) fn))))
  name)

(defun resolve-all-forward-references (&optional env)
  "Compatibility wrapper for VM-RESOLVE-FORWARD-REFERENCES."
  (declare (ignore env))
  (vm-resolve-forward-references *vm-current-state*))
