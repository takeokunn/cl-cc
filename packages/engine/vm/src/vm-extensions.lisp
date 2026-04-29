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

;;; FR-1201: Symbol property list operations

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
         (plist (gethash sym (vm-symbol-plists state) nil))
         (result (getf plist indicator default)))
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
         (val (vm-reg-get state (vm-value inst)))
         (plist (gethash sym (vm-symbol-plists state) nil)))
    (setf (getf plist indicator) val)
    (setf (gethash sym (vm-symbol-plists state)) plist)
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
    (when cl-cc/bootstrap::*runtime-set-symbol-value-fn*
      (funcall cl-cc/bootstrap::*runtime-set-symbol-value-fn* sym val))
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
         (plist (gethash sym (vm-symbol-plists state) nil))
         (found-p (not (eq (getf plist indicator :__not-found__) :__not-found__))))
    (remf plist indicator)
    (if plist
        (setf (gethash sym (vm-symbol-plists state)) plist)
        (remhash sym (vm-symbol-plists state)))
    (vm-reg-set state (vm-dst inst) (if found-p t nil))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-symbol-plist (vm-instruction)
  "Return entire property list of SYM."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :symbol-plist) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-symbol-plist) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-src inst)))
         (plist (gethash sym (vm-symbol-plists state) nil)))
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
    (if new-plist
        (setf (gethash sym (vm-symbol-plists state)) new-plist)
        (remhash sym (vm-symbol-plists state)))
    (vm-reg-set state (vm-dst inst) new-plist)
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
