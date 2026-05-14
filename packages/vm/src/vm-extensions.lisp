(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Language Extensions
;;;
;;; Contains: char case-insensitive comparisons (FR-407), symbol property
;;; list operations (FR-1201), PROGV dynamic binding (FR-102), and
;;; generic polymorphic arithmetic instructions.
;;;
;;; Load order: after vm-numeric.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; FR-1201 / FR-379: Symbol property list operations

(defconstant +vm-symbol-plist-linear-limit+ 4
  "Maximum plist entry count kept on the short linear path before hash promotion.")

(defstruct (vm-symbol-property-hash-store
             (:constructor make-vm-symbol-property-hash-store (table)))
  "Internal promoted property store for symbols with many plist entries."
  (table (make-hash-table :test #'eq) :read-only t))

(defun %vm-symbol-property-entry-p (entry)
  (typep entry 'vm-symbol-property-hash-store))

(defun %vm-symbol-property-entry-table (entry)
  (and (%vm-symbol-property-entry-p entry)
       (vm-symbol-property-hash-store-table entry)))

(defun %vm-plist-entry-count (plist)
  (loop for tail on plist by #'cddr
        while tail
        count 1))

(defun %vm-small-plist-get (plist indicator default)
  (loop for tail on plist by #'cddr
        while tail
        when (eq (car tail) indicator)
          do (return (cadr tail))
        finally (return default)))

(defun %vm-small-plist-set (plist indicator value)
  (loop for tail on plist by #'cddr
        while tail
        when (eq (car tail) indicator)
          do (setf (cadr tail) value)
             (return plist)
        finally (return (list* indicator value plist))))

(defun %vm-small-plist-remprop (plist indicator)
  (cond
    ((null plist) (values nil nil))
    ((eq (car plist) indicator) (values (cddr plist) t))
    (t
     (multiple-value-bind (tail removed-p)
         (%vm-small-plist-remprop (cddr plist) indicator)
       (values (list* (car plist) (cadr plist) tail) removed-p)))))

(defun %vm-symbol-property-hash-table->plist (table)
  (let ((plist nil))
    (maphash (lambda (indicator value)
               (setf plist (list* indicator value plist)))
             table)
    plist))

(defun %vm-symbol-property-entry->plist (entry)
  (cond
    ((null entry) nil)
    ((%vm-symbol-property-entry-p entry)
     (%vm-symbol-property-hash-table->plist
      (vm-symbol-property-hash-store-table entry)))
    (t (copy-list entry))))

(defun %vm-copy-symbol-property-entry (entry)
  (cond
    ((null entry) nil)
    ((%vm-symbol-property-entry-p entry)
     (let ((table (make-hash-table :test #'eq)))
       (maphash (lambda (indicator value)
                  (setf (gethash indicator table) value))
                (vm-symbol-property-hash-store-table entry))
       (make-vm-symbol-property-hash-store table)))
    (t (copy-list entry))))

(defun %vm-promote-symbol-property-plist (plist)
  (let ((table (make-hash-table :test #'eq)))
    (loop for tail on plist by #'cddr
          while tail
          do (setf (gethash (car tail) table) (cadr tail)))
    (make-vm-symbol-property-hash-store table)))

(defun %vm-normalize-symbol-property-entry (plist)
  (let ((plist-copy (copy-list plist)))
    (if (> (%vm-plist-entry-count plist-copy) +vm-symbol-plist-linear-limit+)
        (%vm-promote-symbol-property-plist plist-copy)
        plist-copy)))

(defun %vm-symbol-property-entry-get (entry indicator default)
  (cond
    ((null entry) default)
    ((%vm-symbol-property-entry-p entry)
     (gethash indicator (vm-symbol-property-hash-store-table entry) default))
    (t (%vm-small-plist-get entry indicator default))))

(defun %vm-symbol-property-entry-set (entry indicator value)
  (cond
    ((null entry)
     (list indicator value))
    ((%vm-symbol-property-entry-p entry)
     (setf (gethash indicator (vm-symbol-property-hash-store-table entry)) value)
     entry)
    (t
     (let ((updated (%vm-small-plist-set entry indicator value)))
       (if (> (%vm-plist-entry-count updated) +vm-symbol-plist-linear-limit+)
           (%vm-promote-symbol-property-plist updated)
           updated)))))

(defun %vm-symbol-property-entry-remprop (entry indicator)
  (cond
    ((null entry) (values nil nil))
    ((%vm-symbol-property-entry-p entry)
     (let ((table (vm-symbol-property-hash-store-table entry)))
       (multiple-value-bind (value found-p) (gethash indicator table)
         (declare (ignore value))
         (when found-p
           (remhash indicator table))
         (values (if (zerop (hash-table-count table)) nil entry)
                 found-p))))
    (t (%vm-small-plist-remprop entry indicator))))

(defun %vm-touch-symbol-property-barrier (state)
  (incf (vm-symbol-plist-read-barrier state)))

(defun %vm-sync-host-symbol-plist (sym entry)
  (setf (symbol-plist sym) (%vm-symbol-property-entry->plist entry)))

(defun %vm-symbol-property-get (state table sym indicator default)
  (sb-thread:with-mutex ((vm-symbol-plist-lock state))
    (%vm-symbol-property-entry-get (gethash sym table) indicator default)))

(defun %vm-symbol-property-set (state table sym indicator value &key (sync-host-p nil))
  (sb-thread:with-mutex ((vm-symbol-plist-lock state))
    (let* ((entry (gethash sym table))
           (updated (%vm-symbol-property-entry-set entry indicator value)))
      (setf (gethash sym table) updated)
      (%vm-touch-symbol-property-barrier state)
      (when sync-host-p
        (%vm-sync-host-symbol-plist sym updated))
      value)))

(defun %vm-symbol-property-remprop (state table sym indicator &key (sync-host-p nil))
  (sb-thread:with-mutex ((vm-symbol-plist-lock state))
    (multiple-value-bind (updated found-p)
        (%vm-symbol-property-entry-remprop (gethash sym table) indicator)
      (if updated
          (setf (gethash sym table) updated)
          (remhash sym table))
      (when found-p
        (%vm-touch-symbol-property-barrier state))
      (when sync-host-p
        (%vm-sync-host-symbol-plist sym updated))
      found-p)))

(defun %vm-symbol-property-replace-plist (state table sym plist &key (sync-host-p nil))
  (sb-thread:with-mutex ((vm-symbol-plist-lock state))
    (let ((normalized (and plist (%vm-normalize-symbol-property-entry plist))))
      (if normalized
          (setf (gethash sym table) normalized)
          (remhash sym table))
      (%vm-touch-symbol-property-barrier state)
      (when sync-host-p
        (%vm-sync-host-symbol-plist sym normalized))
      (%vm-symbol-property-entry->plist normalized))))

(defun vm-symbol-plist-read-snapshot (state sym)
  "Return a copy of SYM's user-visible plist and the current read barrier."
  (sb-thread:with-mutex ((vm-symbol-plist-lock state))
    (values (%vm-symbol-property-entry->plist
             (gethash sym (vm-symbol-plists state)))
            (vm-symbol-plist-read-barrier state))))

(defun vm-system-property-get (state sym indicator &optional default)
  "Return VM-only system property INDICATOR from SYM, or DEFAULT if absent."
  (%vm-symbol-property-get state (vm-system-symbol-plists state) sym indicator default))

(defun vm-system-property-set (state sym indicator value)
  "Set VM-only system property INDICATOR on SYM to VALUE."
  (%vm-symbol-property-set state (vm-system-symbol-plists state) sym indicator value))

(defun vm-system-property-remprop (state sym indicator)
  "Remove VM-only system property INDICATOR from SYM."
  (%vm-symbol-property-remprop state (vm-system-symbol-plists state) sym indicator))

(defun vm-system-property-plist (state sym)
  "Return a plist snapshot of VM-only system properties for SYM."
  (sb-thread:with-mutex ((vm-symbol-plist-lock state))
    (%vm-symbol-property-entry->plist
     (gethash sym (vm-system-symbol-plists state)))))

(define-vm-instruction vm-symbol-get (vm-instruction)
  "Get property INDICATOR from symbol SYM's plist. Returns default if absent."
  (dst nil :reader vm-dst) (sym nil :reader vm-sym)
  (indicator nil :reader vm-indicator) (default nil :reader vm-default)
  (:sexp-tag :symbol-get) (:sexp-slots dst sym indicator default))

(defmethod execute-instruction ((inst vm-symbol-get) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-sym inst)))
         (indicator (vm-reg-get state (vm-indicator inst)))
         (default (vm-reg-get state (vm-default inst)))
         (result (%vm-symbol-property-get state (vm-symbol-plists state)
                                          sym indicator default)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-symbol-set (vm-instruction)
  "Set property INDICATOR on symbol SYM's plist to VALUE."
  (dst nil :reader vm-dst) (sym nil :reader vm-sym)
  (indicator nil :reader vm-indicator) (value nil :reader vm-value)
  (:sexp-tag :symbol-set) (:sexp-slots dst sym indicator value))

(defmethod execute-instruction ((inst vm-symbol-set) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-sym inst)))
         (indicator (vm-reg-get state (vm-indicator inst)))
         (val (vm-reg-get state (vm-value inst))))
    (%vm-symbol-property-set state (vm-symbol-plists state)
                             sym indicator val :sync-host-p t)
    (vm-reg-set state (vm-dst inst) val)
    (values (1+ pc) nil nil)))

(define-vm-binary-instruction vm-set-symbol-value :set-symbol-value
  "Set the dynamic value cell of LHS to RHS and return RHS.")

(defmethod execute-instruction ((inst vm-set-symbol-value) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-lhs inst)))
         (val (vm-reg-get state (vm-rhs inst))))
    (setf (gethash sym (vm-global-vars state)) val)
    (setf (symbol-value sym) val)
    (when cl-cc/bootstrap:*runtime-set-symbol-value-fn*
      (funcall cl-cc/bootstrap:*runtime-set-symbol-value-fn* sym val))
    (vm-reg-set state (vm-dst inst) val)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-remprop (vm-instruction)
  "Remove property INDICATOR from symbol SYM's plist."
  (dst nil :reader vm-dst) (sym nil :reader vm-sym)
  (indicator nil :reader vm-indicator)
  (:sexp-tag :remprop) (:sexp-slots dst sym indicator))

(defmethod execute-instruction ((inst vm-remprop) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-sym inst)))
         (indicator (vm-reg-get state (vm-indicator inst)))
         (found-p (%vm-symbol-property-remprop state (vm-symbol-plists state)
                                               sym indicator :sync-host-p t)))
    (vm-reg-set state (vm-dst inst) (if found-p t nil))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-symbol-plist (vm-instruction)
  "Return entire property list of SYM."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :symbol-plist) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-symbol-plist) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-src inst)))
         (plist (nth-value 0 (vm-symbol-plist-read-snapshot state sym))))
    (vm-reg-set state (vm-dst inst) plist)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-set-symbol-plist (vm-instruction)
  "Set the entire property list of SYM to PLIST."
  (dst nil :reader vm-dst) (sym nil :reader vm-sym) (plist-reg nil :reader vm-plist-reg)
  (:sexp-tag :set-symbol-plist) (:sexp-slots dst sym plist-reg))

(defmethod execute-instruction ((inst vm-set-symbol-plist) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-sym inst)))
         (new-plist (vm-reg-get state (vm-plist-reg inst))))
    (vm-reg-set state (vm-dst inst)
                (%vm-symbol-property-replace-plist state (vm-symbol-plists state)
                                                   sym new-plist :sync-host-p t))
    (values (1+ pc) nil nil)))

;;; FR-102: PROGV — dynamic variable binding

(define-vm-instruction vm-progv-enter (vm-instruction)
  "Save current global bindings for SYMS-REG, then bind each to the
   corresponding value in VALS-REG.  Store the saved alist in DST so
   vm-progv-exit can restore it."
  (dst nil :reader vm-dst)
  (syms nil :reader vm-syms-reg)
  (vals nil :reader vm-vals-reg)
  (:sexp-tag :progv-enter)
  (:sexp-slots dst syms vals))

(defmethod execute-instruction ((inst vm-progv-enter) state pc labels)
  (declare (ignore labels))
  (let* ((syms (vm-reg-get state (vm-syms-reg inst)))
         (vals (vm-reg-get state (vm-vals-reg inst)))
         (global-vars (vm-global-vars state))
         (saved nil))
    ;; Save old bindings (or :unbound sentinel)
    (dolist (sym syms)
      (multiple-value-bind (old-val found-p) (gethash sym global-vars)
        (push (cons sym (if found-p old-val :unbound)) saved)))
    ;; Bind new values
    (let ((val-ptr vals))
      (dolist (sym syms)
        (if val-ptr
            (progn
              (setf (gethash sym global-vars) (car val-ptr))
              (setf val-ptr (cdr val-ptr)))
            (setf (gethash sym global-vars) nil))))
    ;; Return saved alist so vm-progv-exit can restore
    (vm-reg-set state (vm-dst inst) (nreverse saved))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-progv-exit (vm-instruction)
  "Restore global bindings from the saved alist produced by vm-progv-enter."
  (saved nil :reader vm-saved-reg)
  (:sexp-tag :progv-exit)
  (:sexp-slots saved))

(defmethod execute-instruction ((inst vm-progv-exit) state pc labels)
  (declare (ignore labels))
  (let ((saved (vm-reg-get state (vm-saved-reg inst)))
        (global-vars (vm-global-vars state)))
    (dolist (pair saved)
      (let ((sym (car pair))
            (old-val (cdr pair)))
        (if (eq old-val :unbound)
            (remhash sym global-vars)
            (setf (gethash sym global-vars) old-val))))
    (values (1+ pc) nil nil)))

;;; Generic (Polymorphic) Arithmetic / Comparison Instructions
;;;
;;; These are emitted by the compiler when the types of operands are not
;;; statically known to be fixnum.  At runtime they dispatch to the same
;;; host operations but carry a different type tag so that:
;;;  - regalloc.lisp can apply the same dst/lhs/rhs convention
;;;  - x86-64-codegen.lisp can emit the identical code sequences as their
;;;    typed counterparts (the fallback path stays correct)
;;;  - future passes can refine them to typed instructions once a type is known

;;; Generic binary instructions: struct + executor generated from data table.
;;; Each entry: (name sexp-tag result-form) where result-form may use L and R.
;;; vm-generic-div is separate: it has a divide-by-zero guard.

(defmacro %define-generic-binop-table (&rest entries)
  `(progn
     ,@(loop for (name tag result-form) in entries
             collect
             `(progn
                (define-vm-instruction ,name (vm-instruction)
                  ,(format nil "Polymorphic ~(~A~)." tag)
                  (dst nil :reader vm-dst)
                  (lhs nil :reader vm-lhs)
                  (rhs nil :reader vm-rhs)
                  (:sexp-tag ,tag)
                  (:sexp-slots dst lhs rhs))
                (defmethod execute-instruction ((inst ,name) state pc labels)
                  (declare (ignore labels))
                  (let* ((l (vm-reg-get state (vm-lhs inst)))
                         (r (vm-reg-get state (vm-rhs inst))))
                    (vm-reg-set state (vm-dst inst) ,result-form)
                    (values (1+ pc) nil nil)))))))

(%define-generic-binop-table
  (vm-generic-add :generic-add (+ l r))
  (vm-generic-sub :generic-sub (- l r))
  (vm-generic-mul :generic-mul (* l r))
  (vm-generic-eq  :generic-eq  (if (equal l r) t nil))
  (vm-generic-lt  :generic-lt  (if (< l r) t nil))
  (vm-generic-gt  :generic-gt  (if (> l r) t nil)))

(define-vm-instruction vm-generic-div (vm-instruction)
  "Polymorphic division (floor)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :generic-div)
  (:sexp-slots dst lhs rhs))

(defmethod execute-instruction ((inst vm-generic-div) state pc labels)
  (declare (ignore labels))
  (let* ((l (vm-reg-get state (vm-lhs inst)))
         (r (vm-reg-get state (vm-rhs inst))))
    (when (zerop r)
      (error "vm-generic-div: division by zero"))
    (vm-reg-set state (vm-dst inst) (floor l r))
    (values (1+ pc) nil nil)))
