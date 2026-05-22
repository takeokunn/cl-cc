(in-package :cl-cc/vm)

(defparameter *safety-level* 1
  "Runtime safety level corresponding to OPTIMIZE (SAFETY N): 0 skips checks, 1 enables bounds/type checks, 3 also protects the stack.")

(defparameter *security-canaries* nil
  "When true, VM call frames carry random stack canaries verified on return.")

(define-condition memory-fault (error)
  ((reason :initarg :reason :reader memory-fault-reason))
  (:report (lambda (condition stream)
             (format stream "VM memory fault: ~A" (memory-fault-reason condition)))))

(defun vm-safety-level ()
  (max 0 (min 3 (or *safety-level* 0))))

(defun vm-safety-checks-enabled-p ()
  (>= (vm-safety-level) 1))

(defun vm-stack-protection-enabled-p ()
  (or *security-canaries* (>= (vm-safety-level) 3)))

(defun vm-random-canary ()
  (logxor (random most-positive-fixnum) (get-universal-time)))

(defun vm-check-index (sequence index &optional operation)
  "Check INDEX for SEQUENCE when safety is enabled; signal TYPE-ERROR on failure."
  (declare (ignore operation))
  (when (vm-safety-checks-enabled-p)
    (unless (and (integerp index) (<= 0 index) (< index (length sequence)))
      (error 'type-error
             :datum index
             :expected-type `(integer 0 ,(max 0 (1- (length sequence)))))))
  index)

(defun vm-check-row-major-index (array index)
  (when (vm-safety-checks-enabled-p)
    (unless (and (integerp index) (<= 0 index) (< index (array-total-size array)))
      (error 'type-error
             :datum index
             :expected-type `(integer 0 ,(max 0 (1- (array-total-size array)))))))
  index)

(defun vm-verify-stack-canary (saved-regs)
  (when (vm-stack-protection-enabled-p)
    (multiple-value-bind (canary foundp) (gethash :__stack-canary__ saved-regs)
      (unless (and foundp (eql canary (gethash :__stack-canary-check__ saved-regs)))
        (error 'memory-fault :reason :stack-canary-mismatch)))))
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

(defmethod execute-instruction ((inst vm-prefetch) state pc labels)
  "Execute a cache prefetch hint as a semantic no-op in the VM interpreter."
  (declare (ignore inst state labels))
  (values (1+ pc) nil nil))

(defconstant +vm-min-fixnum51+ (- (ash 1 50))
  "Smallest integer encodable by the VM's 51-bit signed fixnum representation.")

(defconstant +vm-max-fixnum51+ (1- (ash 1 50))
  "Largest integer encodable by the VM's 51-bit signed fixnum representation.")

(defstruct vm-trampoline-thunk
  "A VM-level zero-argument thunk for external trampoline dispatch."
  (function (lambda () nil) :type function))

(defstruct (vm-continuation (:constructor %make-vm-continuation))
  "Heap-copied VM continuation snapshot.

The register file, call stack, handler stack, method stack, prompt stack, and
closure environment are copied at capture time. Invocation copies the snapshot
again before restoration, keeping continuations multi-shot. KIND is :FULL,
:ESCAPE, or :DELIMITED; escape continuations are one-shot by convention."
  (kind :full :type keyword)
  pc
  dst-reg
  registers
  call-stack
  handler-stack
  method-call-stack
  prompt-stack
  closure-env
  values-list
  (used-p nil :type boolean))

(defun %vm-copy-register-table (table)
  (let ((copy (make-hash-table :test (hash-table-test table))))
    (maphash (lambda (key value) (setf (gethash key copy) value)) table)
    copy))

(defun vm-capture-continuation (state pc dst-reg &key (kind :full) prompt-frame)
  "Copy STATE's VM stack to a heap continuation object." 
  (let* ((call-stack (copy-tree (vm-call-stack state)))
         (method-stack (copy-tree (vm-method-call-stack state)))
         (handler-stack (copy-tree (vm-handler-stack state)))
         (prompt-stack (copy-tree (vm-continuation-prompts state))))
    (when (and prompt-frame (member prompt-frame prompt-stack :test #'equal))
      (setf call-stack (copy-tree (getf prompt-frame :call-stack))
            method-stack (copy-tree (getf prompt-frame :method-call-stack))
            handler-stack (copy-tree (getf prompt-frame :handler-stack))))
    (%make-vm-continuation
     :kind kind
     :pc pc
     :dst-reg dst-reg
     :registers (%vm-copy-register-table (vm-state-registers state))
     :call-stack call-stack
     :handler-stack handler-stack
     :method-call-stack method-stack
     :prompt-stack prompt-stack
     :closure-env (vm-closure-env state)
     :values-list (copy-list (vm-values-list state)))))

(defun vm-invoke-continuation (state continuation value)
  "Restore CONTINUATION into STATE, store VALUE, and return the resume PC." 
  (when (and (eq (vm-continuation-kind continuation) :escape)
             (vm-continuation-used-p continuation))
    (error "Escape continuation invoked more than once: ~S" continuation))
  (setf (vm-continuation-used-p continuation) t)
  (vm-restore-registers state (%vm-copy-register-table (vm-continuation-registers continuation)))
  (setf (vm-call-stack state) (copy-tree (vm-continuation-call-stack continuation))
        (vm-handler-stack state) (copy-tree (vm-continuation-handler-stack continuation))
        (vm-method-call-stack state) (copy-tree (vm-continuation-method-call-stack continuation))
        (vm-continuation-prompts state) (copy-tree (vm-continuation-prompt-stack continuation))
        (vm-closure-env state) (vm-continuation-closure-env continuation)
        (vm-values-list state) (copy-list (vm-continuation-values-list continuation))
        (vm-stack-depth state) (length (vm-continuation-call-stack continuation)))
  (vm-reg-set state (vm-continuation-dst-reg continuation) value)
  (vm-continuation-pc continuation))

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

;;; FR-155: Deoptimization checkpoints and OSR markers.

(defun %vm-current-deopt-info (pc)
  "Return deoptimization metadata for PC from the active program, when present."
  (let ((program-info (and (boundp '*vm-current-program-deopt-info*)
                           (symbol-value '*vm-current-program-deopt-info*))))
    (cond
      ((hash-table-p program-info) (gethash pc program-info))
      ((listp program-info) (cdr (assoc pc program-info :test #'eql)))
      (t nil))))

(defun %vm-deopt-resume-pc (labels label pc)
  "Resolve a deoptimization resume LABEL, falling back to the next instruction."
  (or (and label (vm-label-table-lookup labels label))
      (1+ pc)))

(defun vm-capture-deopt-frame (state pc reason &optional info)
  "Capture enough VM state to rebuild an interpreter frame after deoptimization."
  (let* ((registers (vm-save-registers state))
         (vreg->preg (and info (vm-deopt-info-vreg->preg info)))
         (physical (and vreg->preg
                        (loop for (vreg . preg) in vreg->preg
                              collect (cons preg (gethash vreg registers))))))
    (make-vm-deopt-frame
     :pc pc
     :reason reason
     :registers registers
     :physical-registers physical
     :call-stack (copy-tree (vm-call-stack state))
     :closure-env (vm-closure-env state)
     :values-list (copy-list (vm-values-list state))
     :inline-stack (and info (copy-tree (vm-deopt-info-inline-stack info)))
     :info info)))

(defun vm-reconstruct-interpreter-frame (state frame)
  "Rebuild STATE from a saved FR-522 deoptimization FRAME."
  (when frame
    (when (vm-deopt-frame-registers frame)
      (vm-restore-registers state (vm-deopt-frame-registers frame)))
    (when (vm-deopt-frame-call-stack frame)
      (setf (vm-call-stack state) (vm-deopt-frame-call-stack frame)))
    (when (vm-deopt-frame-closure-env frame)
      (setf (vm-closure-env state) (vm-deopt-frame-closure-env frame)))
    (when (vm-deopt-frame-values-list frame)
      (setf (vm-values-list state) (vm-deopt-frame-values-list frame)))
    (setf (vm-current-deopt-frame state) nil)
    state))

(defun vm-trigger-deopt (state pc labels label reason)
  "Save register/interpreter state and resume at LABEL in the VM interpreter."
  (when *deopt-enabled*
    (let* ((info (%vm-current-deopt-info pc))
           (frame (vm-capture-deopt-frame state pc reason info)))
      (setf (vm-current-deopt-frame state) frame)
      (push frame (vm-deopt-history state))))
  (values (%vm-deopt-resume-pc labels label pc) nil nil))

(defmethod execute-instruction ((inst vm-type-check) state pc labels)
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (vm-typep-check value (vm-type-name inst))
        (values (1+ pc) nil nil)
        (vm-trigger-deopt state pc labels (vm-type-check-deopt-label inst)
                          (list :type-check-failed
                                :register (vm-src inst)
                                :expected (vm-type-name inst)
                                :actual value
                                :id (vm-type-check-deopt-id inst))))))

(defmethod execute-instruction ((inst vm-deopt) state pc labels)
  (vm-trigger-deopt state pc labels (vm-deopt-label inst)
                    (list :deopt (vm-deopt-reason inst) :id (vm-deopt-id inst))))

(defun vm-register-tier1-osr-entry (state id entry)
  "Register simple Tier-1 OSR ENTRY metadata for ID.
ENTRY may be a PC integer, a label, or a plist containing :PC or :LABEL."
  (setf (gethash id (vm-tier1-code state)) entry))

(defun vm-osr-register-map (state &optional live-regs)
  "Return an interpreter virtual-register -> JIT physical-register map.
This infrastructure-level mapping is deterministic and portable; backend-specific
lowering can replace the synthetic physical names later."
  (let ((regs (or live-regs
                  (loop for reg being the hash-keys of (vm-state-registers state)
                        collect reg))))
    (loop for reg in regs
          for index from 0
          collect (cons reg (intern (format nil "P~D" index) :keyword)))))

(defun vm-materialize-osr-frame (state &key pc label id live-regs)
  "Save interpreter state to an OSR stack frame before entering Tier-1 code."
  (list :pc pc
        :label label
        :id id
        :vreg->preg (vm-osr-register-map state live-regs)
        :registers (vm-save-registers state)
        :call-stack (copy-tree (vm-call-stack state))
        :closure-env (vm-closure-env state)
        :values-list (copy-list (vm-values-list state))))

(defun vm-compile-osr-entry-if-hot (state id label pc)
  "Compile/register a conservative OSR entry when the loop header becomes hot.
The hook accepts the current STATE, OSR ID, loop LABEL, and interpreter PC.  If no
hook is installed, this records no target and execution remains in the interpreter."
  (when (and *osr-enabled* *vm-recompile-function-hook*)
    (let ((entry (funcall *vm-recompile-function-hook*
                          (list :osr id :label label :pc pc :state state)
                          1)))
      (when entry
        (vm-register-tier1-osr-entry state id entry)))))

(defun vm-osr-stub-enter (state labels inst pc)
  "FR-521 OSR stub: save interpreter state, enter Tier-1 target, restore on return."
  (let* ((frame (vm-materialize-osr-frame state
                                          :pc pc
                                          :label (vm-osr-label inst)
                                          :id (vm-osr-id inst)))
         (target-pc (or (%vm-tier1-osr-target-pc state labels (vm-osr-id inst) (vm-osr-label inst))
                        (progn
                          (vm-compile-osr-entry-if-hot state (vm-osr-id inst) (vm-osr-label inst) pc)
                          (%vm-tier1-osr-target-pc state labels (vm-osr-id inst) (vm-osr-label inst))))))
    (if target-pc
        (progn
          ;; The OSR frame is kept out of VM-CALL-STACK so normal RET frame
          ;; destructuring remains unchanged; deopt/re-entry can consume it via
          ;; VM-CURRENT-DEOPT-FRAME if the Tier-1 path returns to the interpreter.
          (setf (vm-current-deopt-frame state)
                (make-vm-deopt-frame :pc pc
                                      :reason :osr-entry
                                      :registers (getf frame :registers)
                                      :call-stack (getf frame :call-stack)
                                      :closure-env (getf frame :closure-env)
                                      :values-list (getf frame :values-list)
                                      :info frame))
          (values target-pc nil nil))
        (values (1+ pc) nil nil))))

(defun %vm-tier1-osr-target-pc (state labels id label)
  "Return a Tier-1 OSR target PC for ID/LABEL, or NIL when not ready."
  (let ((entry (or (gethash id (vm-tier1-code state))
                   (and label (gethash label (vm-tier1-code state))))))
    (cond
      ((integerp entry) entry)
      ((and (symbolp entry) labels) (vm-label-table-lookup labels entry))
      ((and (stringp entry) labels) (vm-label-table-lookup labels entry))
      ((and (consp entry) (getf entry :pc)) (getf entry :pc))
      ((and (consp entry) (getf entry :label))
       (vm-label-table-lookup labels (getf entry :label)))
      (t nil))))

(defmethod execute-instruction ((inst vm-osr-entry) state pc labels)
  (unless *osr-enabled*
    (return-from execute-instruction (values (1+ pc) nil nil)))
  ;; Phase 1: Look up current PC in osr-entry-points metadata
  (let ((entry (and (boundp '*vm-current-program-osr-entry-points*)
                    (symbol-value '*vm-current-program-osr-entry-points*)
                    (find pc *vm-current-program-osr-entry-points*
                          :key (lambda (e) (getf e :pc))
                          :test #'eql))))
    (when entry
      ;; Phase 2: If a deopt frame is pending from a prior compiled→interpreter
      ;; transition, reconstruct the interpreter state from its saved registers.
      (let ((deopt-frame (vm-current-deopt-frame state)))
        (when deopt-frame
          (vm-reconstruct-interpreter-frame state deopt-frame))))

    ;; Phase 3: Check for Tier-1 target PC and jump, or continue to next inst.
    (vm-osr-stub-enter state labels inst pc)))

(defmethod execute-instruction ((inst vm-print) state pc labels)
  (declare (ignore labels))
  (format (vm-output-stream state) "~A~%" (vm-reg-get state (vm-reg inst)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-halt) state pc labels)
  (declare (ignore pc labels))
  (values nil t (vm-reg-get state (vm-reg inst))))

(defmethod execute-instruction ((inst vm-closure) state pc labels)
  (declare (ignore labels))
  (let* ((dst-reg (vm-dst inst))
          (captured-bindings (vm-captured-vars inst))
          (captured-regs (coerce (mapcar #'cdr captured-bindings) 'vector))
          (captured-vals (coerce (mapcar (lambda (binding)
                                           (vm-reg-get state (cdr binding)))
                                         captured-bindings)
                                 'vector))
           (closure (make-instance 'vm-closure-object
                                   :entry-label (vm-label-name inst)
                                   :params (vm-closure-params inst)
                                  :optional-params (vm-closure-optional-params inst)
                                  :rest-param (vm-closure-rest-param inst)
                                   :key-params (vm-closure-key-params inst)
                                   :rest-stack-alloc-p (vm-closure-rest-stack-alloc-p inst)
                                    :dispatch-tag (vm-closure-inst-dispatch-tag inst)
                                    :captured-regs captured-regs
                                    :captured-vals captured-vals
                                    :program-flat *vm-exec-flat*
                                    :label-table labels)))
    (vm-reg-set state dst-reg closure)
    ;; Fix self-references for recursive labels: if a captured register
    ;; is the same as dst-reg, the closure captures itself (not yet created at lookup time)
    (dotimes (i (length captured-regs))
      (when (eql (aref captured-regs i) dst-reg)
        (setf (aref captured-vals i) closure)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-call) state pc labels)
  (let ((func (vm-resolve-function state (vm-reg-get state (vm-func-reg inst)))))
    (%vm-dispatch-call func state pc labels (vm-args inst) (vm-dst inst) nil
                       (vm-call-live-regs inst))))

(defmethod execute-instruction ((inst vm-tail-call) state pc labels)
  (let ((func (vm-resolve-function state (vm-reg-get state (vm-func-reg inst)))))
    (%vm-dispatch-call func state pc labels (vm-args inst) (vm-dst inst) t
                        (vm-tail-call-live-regs inst))))

(defmethod execute-instruction ((inst vm-call/cc) state pc labels)
  (let* ((func (vm-resolve-function state (vm-reg-get state (vm-func-reg inst))))
         (cont (vm-capture-continuation state (1+ pc) (vm-dst inst) :kind :full))
         (arg-reg (gensym "CONT-REG")))
    (vm-reg-set state arg-reg cont)
    (%vm-dispatch-call func state pc labels (list arg-reg) (vm-dst inst) nil)))

(defmethod execute-instruction ((inst vm-call-with-prompt) state pc labels)
  (let* ((func (vm-resolve-function state (vm-reg-get state (vm-func-reg inst))))
         (prompt-name (vm-reg-get state (vm-prompt-reg inst)))
         (frame (list :name prompt-name
                      :pc (1+ pc)
                      :dst-reg (vm-dst inst)
                      :call-stack (copy-tree (vm-call-stack state))
                      :handler-stack (copy-tree (vm-handler-stack state))
                      :method-call-stack (copy-tree (vm-method-call-stack state)))))
    (push frame (vm-continuation-prompts state))
    (%vm-dispatch-call func state pc labels nil (vm-dst inst) nil)))

(defmethod execute-instruction ((inst vm-abort-to-prompt) state pc labels)
  (declare (ignore pc labels))
  (let* ((prompt-name (vm-reg-get state (vm-prompt-reg inst)))
         (value (vm-reg-get state (vm-value-reg inst)))
         (frame (find prompt-name (vm-continuation-prompts state)
                      :key (lambda (entry) (getf entry :name))
                      :test #'equal)))
    (unless frame
      (error "No continuation prompt named ~S is active" prompt-name))
    (setf (vm-call-stack state) (copy-tree (getf frame :call-stack))
          (vm-handler-stack state) (copy-tree (getf frame :handler-stack))
          (vm-method-call-stack state) (copy-tree (getf frame :method-call-stack))
          (vm-stack-depth state) (length (getf frame :call-stack)))
    (vm-reg-set state (getf frame :dst-reg) value)
    (setf (vm-continuation-prompts state)
          (rest (member frame (vm-continuation-prompts state) :test #'eq)))
    (values (getf frame :pc) nil nil)))

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

(defmethod execute-instruction ((inst vm-recompile) state pc labels)
  (declare (ignore labels))
  (let ((func (vm-resolve-function state (vm-reg-get state (vm-func-reg inst)))))
    (unless (typep func 'vm-closure-object)
      (error "Cannot recompile non-closure function: ~S" func))
    (vm-maybe-tier-upgrade-closure func (vm-recompile-tier inst))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-ret) state pc labels)
  (declare (ignore pc))
  (let ((result (vm-reg-get state (vm-reg inst))))
    (when (zerop (vm-mv-count state))
      (vm-store-multiple-values state (list result)))
    ;; Check for qualified method dispatch (standard method combination)
    (let ((method-entry (car (vm-method-call-stack state))))
      (when (and method-entry (listp method-entry)
                 (getf (cdddr method-entry) :qualified))
        ;; We're returning from a qualified method — continue the chain
        (return-from execute-instruction
          (%vm-ret-qualified-dispatch state result labels method-entry))))
    ;; Normal return path
    (if (vm-call-stack state)
        (destructuring-bind (return-pc dst-reg old-closure-env saved-regs &optional saved-mv-buffer saved-mv-count)
            (vm-pop-call-frame state)
          (declare (ignore saved-mv-buffer saved-mv-count))
          ;; Pop method-call-stack in sync with call-stack
          (when (vm-method-call-stack state)
            (let ((method-frame (pop (vm-method-call-stack state))))
              (when (and method-frame (listp method-frame)
                         (getf (cdddr method-frame) :combination)
                         (vm-clos-shadow-stack state))
                (pop (vm-clos-shadow-stack state)))
              (when (and method-frame (listp method-frame)
                         (getf (cdddr method-frame) :shadow)
                         (vm-clos-shadow-stack state))
                (pop (vm-clos-shadow-stack state)))))
          (vm-verify-stack-canary saved-regs)
          (vm-profile-return state)
          (vm-restore-registers state saved-regs)
          ;; Write the return value into the destination register
           (vm-reg-set state dst-reg result)
          (when (and (vm-continuation-prompts state)
                     (eql (getf (first (vm-continuation-prompts state)) :pc) return-pc)
                     (eql (getf (first (vm-continuation-prompts state)) :dst-reg) dst-reg))
            (pop (vm-continuation-prompts state)))
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
                              :captured-regs #()
                              :captured-vals #()
                              :program-flat *vm-exec-flat*
                              :label-table labels))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-make-closure) state pc labels)
  (declare (ignore labels))
  (let* ((captured-values (coerce (mapcar (lambda (reg) (vm-reg-get state reg)) (vm-env-regs inst)) 'vector))
           (closure (make-instance 'vm-closure-object
                                   :entry-label (vm-label-name inst)
                                   :params (vm-make-closure-params inst)
                                    :rest-stack-alloc-p nil
                                    :captured-regs #()
                                    :captured-vals captured-values
                                    :program-flat *vm-exec-flat*
                                    :label-table labels))
         (addr (vm-heap-alloc state closure)))
    (vm-reg-set state (vm-dst inst) addr)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-closure-ref-idx) state pc labels)
  (declare (ignore labels))
  (let* ((addr (vm-reg-get state (vm-closure-reg inst)))
         (closure (vm-heap-get state addr))
         (idx (vm-closure-index inst))
          (values-vec (vm-closure-captured-vals closure)))
    (when (>= idx (length values-vec))
      (error "Closure ref index ~D out of bounds (captured ~D values)" idx (length values-vec)))
    (vm-reg-set state (vm-dst inst) (aref values-vec idx))
    (values (1+ pc) nil nil)))
