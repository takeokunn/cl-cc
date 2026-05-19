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

(defconstant +vm-min-fixnum51+ (- (ash 1 50))
  "Smallest integer encodable by the VM's 51-bit signed fixnum representation.")

(defconstant +vm-max-fixnum51+ (1- (ash 1 50))
  "Largest integer encodable by the VM's 51-bit signed fixnum representation.")

(defstruct vm-trampoline-thunk
  "A VM-level zero-argument thunk for external trampoline dispatch."
  (function (lambda () nil) :type function))

(defun vm-force-trampoline-result (value)
  "Force VM trampoline thunks until VALUE is no longer a thunk."
  (loop for current = value then (funcall (vm-trampoline-thunk-function current))
        while (vm-trampoline-thunk-p current)
        finally (return current)))

(defun vm-fixnum51-p (value)
  "Return T when VALUE fits in the VM 51-bit signed fixnum range."
  (and (integerp value)
       (<= +vm-min-fixnum51+ value +vm-max-fixnum51+)))

(defun vm-fixnum51-overflow-p (value)
  "Return T when integer VALUE cannot be represented as a VM fixnum."
  (and (integerp value)
       (not (vm-fixnum51-p value))))

(defun vm-bignum-add-integers (lhs rhs)
  "Host-backed integer addition fallback for VM fixnum overflow."
  (+ lhs rhs))

(defun vm-bignum-subtract-integers (lhs rhs)
  "Host-backed integer subtraction fallback for VM fixnum overflow."
  (- lhs rhs))

(defun %vm-fixnum-rational-p (value)
  "Return T when VALUE is a rational with fixnum numerator and denominator."
  (and (rationalp value)
       (typep (numerator value) 'fixnum)
       (typep (denominator value) 'fixnum)))

(defun %vm-rational-add (lhs rhs)
  "Fast path for rational addition with fixnum numerators and denominators."
  (let* ((a (numerator lhs))
         (b (denominator lhs))
         (c (numerator rhs))
         (d (denominator rhs))
         (g (gcd b d))
         (d/g (/ d g)))
    (/ (+ (* a d/g) (* c (/ b g)))
       (* b d/g))))

(defun %vm-rational-sub (lhs rhs)
  "Fast path for rational subtraction with fixnum numerators and denominators."
  (let* ((a (numerator lhs))
         (b (denominator lhs))
         (c (numerator rhs))
         (d (denominator rhs))
         (g (gcd b d))
         (d/g (/ d g)))
    (/ (- (* a d/g) (* c (/ b g)))
       (* b d/g))))

(defun %vm-rational-mul (lhs rhs)
  "Fast path for rational multiplication with cross-cancellation."
  (let* ((a (numerator lhs))
         (b (denominator lhs))
         (c (numerator rhs))
         (d (denominator rhs))
         (g1 (gcd (abs a) d))
         (g2 (gcd (abs c) b)))
    (/ (* (/ a g1) (/ c g2))
       (* (/ b g2) (/ d g1)))))

(defun %vm-add-with-overflow-fallback (lhs rhs)
  (cond
    ((and (typep lhs 'fixnum) (typep rhs 'fixnum))
     (let ((result (+ lhs rhs)))
       (if (vm-fixnum51-overflow-p result)
           (vm-bignum-add-integers lhs rhs)
           result)))
    ((and (%vm-fixnum-rational-p lhs) (%vm-fixnum-rational-p rhs))
     (%vm-rational-add lhs rhs))
    (t
     (+ lhs rhs))))

(defun %vm-sub-with-overflow-fallback (lhs rhs)
  (cond
    ((and (typep lhs 'fixnum) (typep rhs 'fixnum))
     (let ((result (- lhs rhs)))
       (if (vm-fixnum51-overflow-p result)
           (vm-bignum-subtract-integers lhs rhs)
           result)))
    ((and (%vm-fixnum-rational-p lhs) (%vm-fixnum-rational-p rhs))
     (%vm-rational-sub lhs rhs))
    (t
     (- lhs rhs))))

(defun %vm-mul-with-overflow-fallback (lhs rhs)
  (cond
    ((and (typep lhs 'fixnum) (typep rhs 'fixnum))
     (let ((result (* lhs rhs)))
       (if (vm-fixnum51-overflow-p result)
           (vm-bignum-multiply-integers lhs rhs)
           result)))
    ((and (integerp lhs) (integerp rhs)
          (or (typep lhs 'bignum) (typep rhs 'bignum)))
     (vm-bignum-multiply-integers lhs rhs))
    ((and (%vm-fixnum-rational-p lhs) (%vm-fixnum-rational-p rhs))
     (%vm-rational-mul lhs rhs))
    (t
     (* lhs rhs))))

(defmethod execute-instruction ((inst vm-add) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-add-with-overflow-fallback
               (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-sub) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-sub-with-overflow-fallback
               (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-mul) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-mul-with-overflow-fallback
               (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

;;; Checked arithmetic (FR-149): detect VM fixnum overflow and use the bignum
;;; fallback path. Native backends lower these to hardware overflow checks.
(defmethod execute-instruction ((inst vm-add-checked) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-add-with-overflow-fallback
               (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-sub-checked) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-sub-with-overflow-fallback
               (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-mul-checked) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (%vm-mul-with-overflow-fallback
               (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

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
                                  :dispatch-tag (vm-closure-inst-dispatch-tag inst)
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
    (%vm-dispatch-call func state pc labels (vm-args inst) (vm-dst inst) nil
                       (vm-call-live-regs inst))))

(defmethod execute-instruction ((inst vm-tail-call) state pc labels)
  (let ((func (vm-resolve-function state (vm-reg-get state (vm-func-reg inst)))))
    (%vm-dispatch-call func state pc labels (vm-args inst) (vm-dst inst) t
                       (vm-tail-call-live-regs inst))))

(defmethod execute-instruction ((inst vm-trampoline) state pc labels)
  (declare (ignore labels))
  (let* ((func (vm-resolve-function state (vm-reg-get state (vm-func-reg inst))))
         (arg-values (mapcar (lambda (reg) (vm-reg-get state reg)) (vm-args inst))))
    (vm-reg-set state (vm-dst inst)
                (make-vm-trampoline-thunk
                 :function
                 (lambda ()
                   (cond
                     ((functionp func) (apply func arg-values))
                     ((typep func 'vm-closure-object)
                      (%vm-call-closure-sync func state arg-values))
                      (t (error "Cannot trampoline function designator: ~S" func))))))
    (values (1+ pc) nil nil)))

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
                            :params (vm-closure-params inst)
                            :optional-params (vm-closure-optional-params inst)
                            :rest-param (vm-closure-rest-param inst)
                            :key-params (vm-closure-key-params inst)
                            :rest-stack-alloc-p (vm-closure-rest-stack-alloc-p inst)
                            :dispatch-tag (vm-func-ref-dispatch-tag inst)
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
