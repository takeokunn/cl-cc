(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — execute-instruction Methods for Core Instructions
;;;
;;; Contains: vm-resolve-function, vm-falsep, call-frame helpers
;;; (vm-save/restore-registers, vm-push-call-frame, vm-bind-closure-args),
;;; vm-list-to-lisp-list, multi-dispatch helpers (vm-classify-arg,
;;; vm-resolve-gf-method, vm-resolve-multi-dispatch, vm-get-all-applicable-methods,
;;; vm-dispatch-generic-call, %vm-dispatch-call),
;;; and execute-instruction methods for: const, move, add/sub/mul, label,
;;; jump, jump-zero, print, halt, closure, call, tail-call, ret, func-ref,
;;; make-closure, closure-ref-idx, values, mv-bind, values-to-list,
;;; spread-values, clear-values, ensure-values, next-method-p,
;;; call-next-method, apply, register-function, set-global, get-global.
;;;
;;; Load order: after vm-dispatch.lisp, before vm-clos.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defmethod execute-instruction ((inst vm-const) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (vm-value inst))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-move) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (vm-reg-get state (vm-src inst)))
  (values (1+ pc) nil nil))

(define-simple-instruction vm-add :binary +)
(define-simple-instruction vm-sub :binary -)
(define-simple-instruction vm-mul :binary *)

;;; Checked arithmetic (FR-303): in the VM interpreter, these behave identically
;;; to the unchecked versions since CL arithmetic auto-promotes to bignum.
;;; The overflow detection only matters in the native codegen backends.
(define-simple-instruction vm-add-checked :binary +)
(define-simple-instruction vm-sub-checked :binary -)
(define-simple-instruction vm-mul-checked :binary *)

(defmethod execute-instruction ((inst vm-label) state pc labels)
  (declare (ignore state labels))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-jump) state pc labels)
  (declare (ignore state pc))
  (values (vm-label-table-lookup labels (vm-label-name inst)) nil nil))

(defun vm-falsep (value)
  "Return T if VALUE is falsy.

The cl-cc execution model treats both NIL and numeric zero as false."
  (or (null value)
      (and (numberp value)
           (zerop value))))

(defmethod execute-instruction ((inst vm-jump-zero) state pc labels)
  (if (vm-falsep (vm-reg-get state (vm-reg inst)))
      (values (vm-label-table-lookup labels (vm-label-name inst)) nil nil)
      (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-select) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (if (vm-falsep (vm-reg-get state (vm-select-cond-reg inst)))
                  (vm-reg-get state (vm-select-else-reg inst))
                  (vm-reg-get state (vm-select-then-reg inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-print) state pc labels)
  (declare (ignore labels))
  (format (vm-output-stream state) "~A~%" (vm-reg-get state (vm-reg inst)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-halt) state pc labels)
  (declare (ignore pc labels))
  (values nil t (vm-reg-get state (vm-reg inst))))

(defmethod execute-instruction ((inst vm-closure) state pc labels)
  (declare (ignore labels))
  ;; captured is a list of (symbol . register) pairs from the compiler
  ;; We store values keyed by REGISTER name so vm-call restores to the correct register
  (let* ((dst-reg (vm-dst inst))
         (captured-vals (coerce
                         (mapcar (lambda (binding)
                                   (cons (cdr binding) (vm-reg-get state (cdr binding))))
                                 (vm-captured-vars inst))
                         'vector))
          (closure (make-instance 'vm-closure-object
                                  :entry-label (vm-label-name inst)
                                  :params (vm-closure-params inst)
                                  :optional-params (vm-closure-optional-params inst)
                                  :rest-param (vm-closure-rest-param inst)
                                  :key-params (vm-closure-key-params inst)
                                  :rest-stack-alloc-p (vm-closure-rest-stack-alloc-p inst)
                                  :captured-values captured-vals)))
    (vm-reg-set state dst-reg closure)
    ;; Fix self-references for recursive labels: if a captured register
    ;; is the same as dst-reg, the closure captures itself (not yet created at lookup time)
    (dotimes (i (length captured-vals))
      (let ((cv-pair (aref captured-vals i)))
        (when (eql (car cv-pair) dst-reg)
          (setf (cdr cv-pair) closure))))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-call) state pc labels)
  (let ((func (vm-resolve-function state (vm-reg-get state (vm-func-reg inst)))))
    (%vm-dispatch-call func state pc labels (vm-args inst) (vm-dst inst) nil)))

(defmethod execute-instruction ((inst vm-tail-call) state pc labels)
  (let ((func (vm-resolve-function state (vm-reg-get state (vm-func-reg inst)))))
    (%vm-dispatch-call func state pc labels (vm-args inst) (vm-dst inst) t)))

(defmethod execute-instruction ((inst vm-ret) state pc labels)
  (declare (ignore pc))
  (let ((result (vm-reg-get state (vm-reg inst))))
    ;; Check for qualified method dispatch (standard method combination)
    (let ((method-entry (car (vm-method-call-stack state))))
      (when (and method-entry (listp method-entry)
                 (getf (cdddr method-entry) :qualified))
        ;; We're returning from a qualified method — continue the chain
        (return-from execute-instruction
          (%vm-ret-qualified-dispatch state result labels method-entry))))
    ;; Normal return path
    (if (vm-call-stack state)
        (destructuring-bind (return-pc dst-reg old-closure-env saved-regs)
            (pop (vm-call-stack state))
          ;; Pop method-call-stack in sync with call-stack
          (when (vm-method-call-stack state)
            (pop (vm-method-call-stack state)))
          (vm-profile-return state)
          (vm-restore-registers state saved-regs)
          ;; Write the return value into the destination register
          (vm-reg-set state dst-reg result)
          (when old-closure-env
            (setf (vm-closure-env state) old-closure-env))
          (values return-pc nil nil))
        (values nil t result))))

(defmethod execute-instruction ((inst vm-func-ref) state pc labels)
  (declare (ignore labels))
  ;; Try the function registry first — user-defined functions (defun) register
  ;; closures with proper entry-label, params, and captured-values.
  ;; Then honor the host bridge whitelist for CL functions like 1+/LENGTH.
  ;; Fall back to a bare closure only for local labels in the same compilation unit.
  (let* ((label-str (vm-label-name inst))
          (sym (or (find-symbol label-str :cl-cc)
                   (find-symbol label-str :cl)))
          (registered (when sym (gethash sym (vm-function-registry state)))))
      (vm-reg-set state (vm-dst inst)
                  (or registered
                      (and sym (vm-bridge-callable sym))
            (make-instance 'vm-closure-object
                           :entry-label label-str
                           :params nil
                          :rest-stack-alloc-p nil
                          :captured-values nil))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-make-closure) state pc labels)
  (declare (ignore labels))
  (let* ((captured-values (coerce (mapcar (lambda (reg) (vm-reg-get state reg)) (vm-env-regs inst)) 'vector))
          (closure (make-instance 'vm-closure-object
                                  :entry-label (vm-label-name inst)
                                  :params (vm-make-closure-params inst)
                                  :rest-stack-alloc-p nil
                                  :captured-values captured-values))
         (addr (vm-heap-alloc state closure)))
    (vm-reg-set state (vm-dst inst) addr)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-closure-ref-idx) state pc labels)
  (declare (ignore labels))
  (let* ((addr (vm-reg-get state (vm-closure-reg inst)))
         (closure (vm-heap-get state addr))
         (idx (vm-closure-index inst))
         (values-vec (vm-closure-captured-values closure)))
    (when (>= idx (length values-vec))
      (error "Closure ref index ~D out of bounds (captured ~D values)" idx (length values-vec)))
    (vm-reg-set state (vm-dst inst) (aref values-vec idx))
    (values (1+ pc) nil nil)))
