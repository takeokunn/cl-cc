(in-package :cl-cc/compile)

;;; ── Toplevel compilation snapshot / restore ────────────────────────────────
;;;
;;; Provides: %copy-compiler-hash-table,
;;;           toplevel-compilation-snapshot defstruct,
;;;           *toplevel-snapshot-slots*,
;;;           %build-snapshot-slot-fns,
;;;           *toplevel-snapshot-slot-fns*,
;;;           %ensure-snapshot-slot-fns,
;;;           %snapshot-toplevel-compilation-state,
;;;           %restore-toplevel-compilation-state

(defun %copy-compiler-hash-table (table)
  "Return a shallow copy of compiler context hash TABLE."
  (let ((copy (make-hash-table :test (hash-table-test table)
                               :rehash-size (hash-table-rehash-size table)
                               :rehash-threshold (hash-table-rehash-threshold table))))
    (maphash (lambda (key value)
               (setf (gethash key copy) value))
             table)
    copy))

(defstruct toplevel-compilation-snapshot
  "Mutable compiler state saved before compiling one top-level form."
  instructions
  next-register
  next-label
  env
  type-env
  safety
  block-env
  tagbody-env
  global-functions
  global-function-mv-arities
  function-conventions
  global-variables
  global-var-cache
  global-classes
  global-generics
  global-generic-params
  current-function-name
  current-function-label
  current-function-params
  current-function-simple-p
  pending-inline-policy
  top-level-p
  boxed-vars
  noescape-cons-bindings
  noescape-array-bindings
  noescape-instance-bindings
  noescape-closure-bindings
  hash-table-test-bindings
  tail-position
  diagnostics
  compile-time-value-env
  compile-time-function-env
  opts)

;;; Each entry: (field-name copy-fn)
;;;   field-name  — the short field name shared between ctx-<field> and
;;;                 toplevel-compilation-snapshot-<field> accessors
;;;   copy-fn     — unary copy function symbol, or NIL for scalars (no copy)
;;;
;;; %snapshot-toplevel-compilation-state and %restore-toplevel-compilation-state
;;; iterate this table via *toplevel-snapshot-slot-fns* (resolved at load time),
;;; avoiding 60+ lines of repetitive setf forms.
(defparameter *toplevel-snapshot-slots*
  '((instructions              copy-list)
    (next-register             nil)
    (next-label                nil)
    (env                       copy-tree)
    (type-env                  nil)
    (safety                    nil)
    (block-env                 copy-tree)
    (tagbody-env               copy-tree)
    (global-functions          %copy-compiler-hash-table)
    (global-function-mv-arities %copy-compiler-hash-table)
    (function-conventions      %copy-compiler-hash-table)
    (global-variables          %copy-compiler-hash-table)
    (global-var-cache          copy-list)
    (global-classes            %copy-compiler-hash-table)
    (global-generics           %copy-compiler-hash-table)
    (global-generic-params     %copy-compiler-hash-table)
    (current-function-name     nil)
    (current-function-label    nil)
    (current-function-params   copy-list)
    (current-function-simple-p nil)
    (pending-inline-policy     nil)
    (top-level-p               nil)
    (boxed-vars                copy-list)
    (noescape-cons-bindings    copy-tree)
    (noescape-array-bindings   copy-tree)
    (noescape-instance-bindings copy-tree)
    (noescape-closure-bindings  copy-tree)
    (hash-table-test-bindings  copy-tree)
    (tail-position             nil)
    (diagnostics               copy-list))
  "Fields shared between compiler-context and toplevel-compilation-snapshot.
Each entry (field-name copy-fn): copy-fn NIL = scalar, else a unary copy function.")

(defun %build-snapshot-slot-fns ()
  "Resolve accessor/writer functions for *toplevel-snapshot-slots* once at load time.
Returns a list of (ctx-reader snap-writer snap-reader ctx-writer copy-fn) tuples."
  (mapcar (lambda (entry)
            (destructuring-bind (field copy-fn) entry
              (let* ((ctx-sym  (intern (format nil "CTX-~A" field) :cl-cc/compile))
                     (snap-sym (intern (format nil "TOPLEVEL-COMPILATION-SNAPSHOT-~A" field) :cl-cc/compile)))
                (list (fdefinition ctx-sym)
                      (fdefinition (list 'setf snap-sym))
                      (fdefinition snap-sym)
                      (fdefinition (list 'setf ctx-sym))
                      (and copy-fn (fdefinition copy-fn))))))
          *toplevel-snapshot-slots*))

;;; Resolved at load time to avoid per-call fdefinition lookups.
(defvar *toplevel-snapshot-slot-fns* nil)

(defun %ensure-snapshot-slot-fns ()
  (or *toplevel-snapshot-slot-fns*
      (setf *toplevel-snapshot-slot-fns* (%build-snapshot-slot-fns))))

(defun %snapshot-toplevel-compilation-state (ctx opts)
  "Snapshot CTX and dynamic top-level compilation state before one FORM."
  (let ((snap (make-toplevel-compilation-snapshot
               :compile-time-value-env    (copy-list *compile-time-value-env*)
               :compile-time-function-env (copy-list *compile-time-function-env*)
               :opts                      (copy-list opts))))
    (dolist (fns (%ensure-snapshot-slot-fns) snap)
      (destructuring-bind (ctx-reader snap-writer snap-reader ctx-writer copy-fn) fns
        (declare (ignore snap-reader ctx-writer))
        (let* ((raw (funcall ctx-reader ctx))
               (val (if copy-fn (funcall copy-fn raw) raw)))
          (funcall snap-writer val snap))))))

(defun %restore-toplevel-compilation-state (ctx snapshot)
  "Restore CTX and dynamic state from SNAPSHOT. Returns restored compile opts."
  (dolist (fns (%ensure-snapshot-slot-fns))
    (destructuring-bind (ctx-reader snap-writer snap-reader ctx-writer copy-fn) fns
      (declare (ignore ctx-reader snap-writer))
      (let* ((raw (funcall snap-reader snapshot))
             (val (if copy-fn (funcall copy-fn raw) raw)))
        (funcall ctx-writer val ctx))))
  (setf *compile-time-value-env*    (copy-list (toplevel-compilation-snapshot-compile-time-value-env snapshot)))
  (setf *compile-time-function-env* (copy-list (toplevel-compilation-snapshot-compile-time-function-env snapshot)))
  (copy-list (toplevel-compilation-snapshot-opts snapshot)))
