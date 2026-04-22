(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Dispatch Protocol and Call-frame Helpers
;;;
;;; Contains: execution-context state (*vm-exec-flat*, *vm-exec-labels*,
;;; %vm-call-closure-sync), vm-resolve-function, vm-label-table-key/store/lookup,
;;; vm-save/restore-registers, vm-push-call-frame, vm-bind-closure-args,
;;; vm-list-to-lisp-list.
;;;
;;; Generic function dispatch (vm-classify-arg, vm-get-all-applicable-methods,
;;; vm-dispatch-generic-call, %vm-ret-qualified-dispatch, etc.) is in
;;; vm-dispatch-gf.lisp, which loads immediately after this file.
;;;
;;; Load order: after vm.lisp, before vm-dispatch-gf.lisp, vm-execute.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Execution context for sub-invocations (custom method combination) ────

(defvar *vm-exec-flat* nil
  "When non-nil, the flat instruction vector of the currently executing VM program.
Used by custom method combination to do synchronous sub-calls.")

(defvar *vm-exec-labels* nil
  "When non-nil, the label table of the currently executing VM program.")

(defun %vm-call-closure-sync (closure state args)
  "Call a VM closure synchronously, returning its result value.
Requires *vm-exec-flat* and *vm-exec-labels* to be bound.
Saves and restores call stack around the sub-invocation." 
  (let* ((flat (or (vm-closure-program-flat closure) *vm-exec-flat*))
         (labels (or (vm-closure-label-table closure) *vm-exec-labels*)))
    (unless (and flat labels)
      (error "No VM execution context for synchronous sub-call"))
    (let* ((result-reg (intern "R0" :keyword))
           (saved-stack-depth (length (vm-call-stack state)))
           (entry-pc (vm-label-table-lookup labels
                                            (vm-closure-entry-label closure))))
      (unless entry-pc
        (error "Cannot resolve entry label ~A" (vm-closure-entry-label closure)))
      ;; Push a call frame; return-pc is irrelevant since we detect return by stack depth
      (vm-push-call-frame state 0 result-reg)
      (push nil (vm-method-call-stack state))
      (vm-profile-enter-call state (vm-closure-entry-label closure))
      (vm-bind-closure-args closure state args)
      (let ((*vm-exec-flat* flat)
            (*vm-exec-labels* labels))
        ;; Mini execution loop — run until our frame is popped
        (loop with pc = entry-pc
              do (when (or (null pc) (>= pc (length flat)))
                   (return (vm-reg-get state result-reg)))
                 (multiple-value-bind (next-pc halt-p result)
                     (execute-instruction (aref flat pc) state pc labels)
                   (when halt-p (return (or result (vm-reg-get state result-reg))))
                   (when (<= (length (vm-call-stack state)) saved-stack-depth)
                     ;; Our frame was popped — method returned
                     (return (vm-reg-get state result-reg)))
                   (setf pc next-pc)))))))

(defun %vm-closure-object-p (value)
  "Return T when VALUE is a VM closure object." 
  (typep value 'vm-closure-object))

(defparameter *vm-direct-function-designator-resolvers*
  '((%vm-closure-object-p . identity)
    (vm-generic-function-p . identity)
    (functionp . identity))
  "Predicate/resolver pairs for non-symbol function designators accepted by `vm-resolve-function`."
  )

(defun %resolve-direct-function-designator (value)
  "Resolve VALUE through the direct-function-designator table, or return NIL." 
  (dolist (entry *vm-direct-function-designator-resolvers* nil)
    (when (funcall (symbol-function (car entry)) value)
      (return (funcall (symbol-function (cdr entry)) value)))))

(defun %resolve-symbol-function-designator (state value)
  "Resolve symbol VALUE through the VM function registry or host bridge." 
  (let* ((registry (ignore-errors (vm-function-registry state)))
         (entry (and registry (gethash value registry))))
    (cond
      (entry entry)
      ((and (gethash value *vm-host-bridge-functions*)
            (fboundp value))
       (symbol-function value))
      (t (error "Undefined function: ~S" value)))))

(defun vm-resolve-function (state value)
  "Resolve VALUE to a closure, generic function, or host bridge function.
If VALUE is already a closure, return it.
If VALUE is a hash table with :__methods__, return it (generic function).
If VALUE is a symbol, look it up in the function registry first, then
check the host bridge whitelist."
  (or (%resolve-direct-function-designator value)
      (and (symbolp value)
           (%resolve-symbol-function-designator state value))
      (error "Invalid function designator: ~S" value)))

(defun vm-label-table-key (label)
  "Return the integer hash key used by VM label tables for LABEL."
  (sxhash label))

(defun vm-label-table-store (table label pc)
  "Store LABEL → PC in TABLE using an integer-keyed collision bucket."
  (let* ((key (vm-label-table-key label))
         (bucket (gethash key table)))
    (setf (gethash key table)
          (acons label pc (remove label bucket :key #'car :test #'equal)))))

(defun vm-label-table-lookup (table label)
  "Look up LABEL in integer-keyed VM label TABLE and return its PC or NIL."
  (cdr (assoc label (gethash (vm-label-table-key label) table) :test #'equal)))

;;; ── Call-frame helpers ───────────────────────────────────────────────────

(defun vm-save-registers (state)
  "Return a snapshot copy of the current register file."
  (let ((copy (make-hash-table :test (hash-table-test (vm-state-registers state)))))
    (maphash (lambda (k v) (setf (gethash k copy) v))
             (vm-state-registers state))
    copy))

(defun vm-save-registers-subset (state regs)
  "Return a snapshot copy containing only REGS from STATE.

Missing registers are omitted from the returned table. This is a conservative
helper for known-call snapshot trimming experiments." 
  (let ((copy (make-hash-table :test (hash-table-test (vm-state-registers state)))))
    (dolist (reg regs copy)
      (multiple-value-bind (value found-p)
          (gethash reg (vm-state-registers state))
        (when found-p
          (setf (gethash reg copy) value))))))

(defun vm-restore-registers (state saved-regs)
  "Replace the current register file with the SAVED-REGS snapshot."
  (when saved-regs
    (clrhash (vm-state-registers state))
    (maphash (lambda (k v) (setf (gethash k (vm-state-registers state)) v))
             saved-regs)))

(defun vm-restore-registers-subset (state saved-regs)
  "Restore only the bindings present in SAVED-REGS into STATE." 
  (when saved-regs
    (maphash (lambda (k v) (setf (gethash k (vm-state-registers state)) v))
             saved-regs)))

(defun vm-push-call-frame (state return-pc dst-reg)
  "Save current environment and push a call frame onto the call stack."
  (push (list return-pc dst-reg (vm-closure-env state) (vm-save-registers state))
        (vm-call-stack state)))

(defun vm-bind-closure-args (closure state arg-values)
  "Bind ARG-VALUES to CLOSURE's parameter registers in STATE.
Restores captured environment, then handles required, &optional, &rest, and &key."
  (let ((params     (vm-closure-params closure))
        (opt-params (vm-closure-optional-params closure))
        (rest-param (vm-closure-rest-param closure))
        (key-params (vm-closure-key-params closure))
        (captured   (vm-closure-captured-values closure)))
    ;; Reserved argument slots for fast-path experiments / known-call helpers.
    (vm-bind-arg-slots state arg-values)
    ;; Restore captured environment into registers
    (map nil (lambda (binding)
               (vm-reg-set state (car binding) (cdr binding)))
         captured)
    ;; Required parameters
    (loop for param in params
          for val   in arg-values
          do (vm-reg-set state param val))
    ;; &optional parameters
    (let* ((n-req      (length params))
           (n-opt      (length opt-params))
           (after-req  (nthcdr n-req arg-values)))
      (when opt-params
        (loop for (reg default) in opt-params
              for i from 0
              do (vm-reg-set state reg
                             (if (< i (length after-req))
                                 (nth i after-req)
                                 default))))
       ;; &rest parameter
        (when rest-param
          (vm-reg-set state rest-param
                      (vm-build-list state (nthcdr (+ n-req n-opt) arg-values)
                                     :stack-allocate-p (vm-closure-rest-stack-alloc-p closure))))
       ;; &key parameters
       (when key-params
         (let ((kw-args (nthcdr (+ n-req n-opt) arg-values)))
           (if (>= (length key-params) 4)
               (let ((kw-table (make-hash-table :test #'eq)))
                (loop for tail on kw-args by #'cddr
                      while (consp (cdr tail))
                      do (let ((keyword (first tail))
                               (value (second tail)))
                           (multiple-value-bind (existing foundp)
                               (gethash keyword kw-table)
                             (declare (ignore existing))
                             (unless foundp
                               (setf (gethash keyword kw-table) value)))))
                (dolist (entry key-params)
                  (destructuring-bind (keyword reg default) entry
                    (multiple-value-bind (value foundp)
                        (gethash keyword kw-table)
                      (vm-reg-set state reg (if foundp value default))))))
               (loop for (keyword reg default) in key-params
                     do (let ((pos (position keyword kw-args)))
                          (vm-reg-set state reg
                                      (if pos (nth (1+ pos) kw-args) default))))))))
    ;; Activate closure environment for nested closures
    (when captured
      (setf (vm-closure-env state) captured))))

(defun vm-list-to-lisp-list (state value)
  "Convert a VM list (possibly using vm-cons-cell heap objects) to a Lisp list."
  (typecase value
    (null nil)
    (cons value)
    (vm-cons-cell
     (cons (vm-cons-cell-car value)
           (vm-list-to-lisp-list state (vm-cons-cell-cdr value))))
    (integer
     ;; Could be a heap address
     (let ((obj (vm-heap-get state value)))
       (if (typep obj 'vm-cons-cell)
           (cons (vm-cons-cell-car obj)
                 (vm-list-to-lisp-list state (vm-cons-cell-cdr obj)))
           (list value))))
    (t (list value))))

;;; Generic function dispatch (vm-classify-arg and everything below) has been
;;; extracted to vm-dispatch-gf.lisp (loads immediately after this file).
