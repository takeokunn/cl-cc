(in-package :cl-cc)
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

(define-vm-instruction vm-generic-add (vm-instruction)
  "Polymorphic addition: dispatches to + at runtime."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :generic-add)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-generic-sub (vm-instruction)
  "Polymorphic subtraction."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :generic-sub)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-generic-mul (vm-instruction)
  "Polymorphic multiplication."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :generic-mul)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-generic-div (vm-instruction)
  "Polymorphic division (floor)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :generic-div)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-generic-eq (vm-instruction)
  "Polymorphic equality: dispatches to EQUAL at runtime."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :generic-eq)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-generic-lt (vm-instruction)
  "Polymorphic less-than: dispatches to < at runtime."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :generic-lt)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-generic-gt (vm-instruction)
  "Polymorphic greater-than: dispatches to > at runtime."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :generic-gt)
  (:sexp-slots dst lhs rhs))

;;; Execute methods for generic instructions

(defmethod execute-instruction ((inst vm-generic-add) state pc labels)
  (declare (ignore labels))
  (let* ((l (vm-reg-get state (vm-lhs inst)))
         (r (vm-reg-get state (vm-rhs inst))))
    (vm-reg-set state (vm-dst inst) (+ l r))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-generic-sub) state pc labels)
  (declare (ignore labels))
  (let* ((l (vm-reg-get state (vm-lhs inst)))
         (r (vm-reg-get state (vm-rhs inst))))
    (vm-reg-set state (vm-dst inst) (- l r))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-generic-mul) state pc labels)
  (declare (ignore labels))
  (let* ((l (vm-reg-get state (vm-lhs inst)))
         (r (vm-reg-get state (vm-rhs inst))))
    (vm-reg-set state (vm-dst inst) (* l r))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-generic-div) state pc labels)
  (declare (ignore labels))
  (let* ((l (vm-reg-get state (vm-lhs inst)))
         (r (vm-reg-get state (vm-rhs inst))))
    (when (zerop r)
      (error "vm-generic-div: division by zero"))
    (vm-reg-set state (vm-dst inst) (floor l r))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-generic-eq) state pc labels)
  (declare (ignore labels))
  (let* ((l (vm-reg-get state (vm-lhs inst)))
         (r (vm-reg-get state (vm-rhs inst)))
         (result (if (equal l r) t nil)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-generic-lt) state pc labels)
  (declare (ignore labels))
  (let* ((l (vm-reg-get state (vm-lhs inst)))
         (r (vm-reg-get state (vm-rhs inst)))
         (result (if (< l r) t nil)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-generic-gt) state pc labels)
  (declare (ignore labels))
  (let* ((l (vm-reg-get state (vm-lhs inst)))
         (r (vm-reg-get state (vm-rhs inst)))
         (result (if (> l r) t nil)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))
