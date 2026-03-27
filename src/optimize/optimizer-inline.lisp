;;;; optimizer-inline.lisp — Function Inlining Pass
;;;;
;;;; Inlines small, non-capturing, linear functions at call sites.
;;;; Eligibility: vm-closure with no captured vars, body ≤ threshold insts,
;;;;              no internal jumps (linear body ending in vm-ret).
;;;;
;;;; Algorithm:
;;;;   1. Scan once to map label → (params, linear-body).
;;;;   2. Forward scan: when a known small function's register is called,
;;;;      replace vm-call with: arg-moves + renamed body + result-move.
;;;;   Register renaming uses fresh :R<N> indices above the program's max.

(in-package :cl-cc)

(defun opt-max-reg-index (instructions)
  "Return the maximum integer N where :RN is used in INSTRUCTIONS, or -1."
  (let ((max-idx -1))
    (dolist (inst instructions max-idx)
      (dolist (reg (cons (opt-inst-dst inst) (opt-inst-read-regs inst)))
        (when (and reg (opt-register-keyword-p reg))
          (let* ((name (symbol-name reg))
                 (idx (ignore-errors (parse-integer name :start 1))))
            (when (and idx (> idx max-idx))
              (setf max-idx idx))))))))

(defun opt-make-renaming (body-instructions base-index)
  "Build renaming table: existing-register → fresh :R<N> (starting at BASE-INDEX).
   Uses opt-inst-dst + opt-inst-read-regs to discover ALL registers, including
   those not serialized by instruction->sexp (e.g., vm-make-obj initarg-regs)."
  (let ((seen (make-hash-table :test #'eq))
        (counter base-index)
        (renaming (make-hash-table :test #'eq)))
    (flet ((add (r)
             (when (and r (opt-register-keyword-p r))
               (unless (gethash r seen)
                 (setf (gethash r seen) t)
                 (setf (gethash r renaming)
                       (intern (format nil "R~A" counter) :keyword))
                 (incf counter)))))
      (dolist (inst body-instructions)
        (add (opt-inst-dst inst))
        (dolist (r (opt-inst-read-regs inst)) (add r))))
    renaming))

(defun opt-can-safely-rename-p (body-instructions)
  "T if all instructions in BODY can be safely register-renamed via sexp roundtrip.
   An instruction is safe when instruction->sexp captures all its registers —
   i.e., opt-inst-read-regs reports the same registers as appear in the sexp.
   Instructions with custom sexp methods (vm-make-obj, vm-slot-read, etc.) that
   omit registers from their sexp representation will cause this to return NIL."
  (dolist (inst body-instructions t)
    (let ((explicit-regs (remove nil
                                 (cons (opt-inst-dst inst)
                                       (opt-inst-read-regs inst))))
          (sexp-regs nil))
      (labels ((visit (x)
                 (cond ((and (keywordp x) (opt-register-keyword-p x)) (push x sexp-regs))
                       ((consp x) (visit (car x)) (visit (cdr x))))))
        (handler-case (visit (instruction->sexp inst))
          (error () (return-from opt-can-safely-rename-p nil))))
      (unless (every (lambda (r) (member r sexp-regs)) explicit-regs)
        (return-from opt-can-safely-rename-p nil)))))

(defun opt-rename-regs-in-inst (inst renaming)
  "Return INST with all VM register keywords substituted per RENAMING.
   Uses instruction→sexp roundtrip; returns INST unchanged on any error."
  (flet ((sub (x)
           (if (and (keywordp x) (opt-register-keyword-p x))
               (or (gethash x renaming) x)
               x)))
    (handler-case
        (sexp->instruction (opt-map-tree #'sub (instruction->sexp inst)))
      (error () inst))))

(defun opt-collect-function-defs (instructions)
  "Return hash-table label → (:params params :body body-insts).
   Only includes functions that are:
   - Registered via vm-closure with known params
   - Linear: no internal jumps; body ends with exactly one vm-ret"
  (let ((label-to-params (make-hash-table :test #'equal))
        (label-to-body   (make-hash-table :test #'equal))
        (in-fn nil) (cur-label nil) (cur-body nil) (has-jump nil))
    ;; Collect params from vm-closure instructions
    (dolist (inst instructions)
      (when (and (vm-closure-p inst) (vm-closure-params inst))
        (setf (gethash (vm-label-name inst) label-to-params)
              (vm-closure-params inst))))
    ;; Collect linear bodies (label → instructions ending in vm-ret)
    (dolist (inst instructions)
      (typecase inst
        (vm-label
         ;; Nested label: abandon any in-progress body (non-linear)
         (when in-fn
           (setf in-fn nil cur-label nil cur-body nil has-jump nil))
         (setf in-fn t cur-label (vm-name inst) cur-body nil has-jump nil))
        ((or vm-jump vm-jump-zero)
         (setf has-jump t)
         (when in-fn (push inst cur-body)))
        (vm-ret
         (when (and in-fn (not has-jump))
           (push inst cur-body)
           (setf (gethash cur-label label-to-body) (nreverse cur-body)))
         (setf in-fn nil cur-label nil cur-body nil has-jump nil))
        (t (when in-fn (push inst cur-body)))))
    ;; Combine: label must appear in both tables
    ;; Also build reverse map: label → the vm-closure instruction (for captured-var check)
    (let ((label-to-closure (make-hash-table :test #'equal))
          (result (make-hash-table :test #'equal)))
      (dolist (inst instructions)
        (when (vm-closure-p inst)
          (setf (gethash (vm-label-name inst) label-to-closure) inst)))
      (maphash (lambda (lbl params)
                 (let ((body    (gethash lbl label-to-body))
                       (closure (gethash lbl label-to-closure)))
                   (when (and body closure)
                     (setf (gethash lbl result)
                           (list :closure closure :params params :body body)))))
               label-to-params)
      result)))

(defun opt-body-has-global-refs-p (body-instructions params)
  "Return T if BODY-INSTRUCTIONS read any register that is neither in PARAMS
   nor defined as a DST by a prior body instruction.  Such 'global registers'
   (e.g., class descriptors set by defclass at the top level) would be renamed
   to fresh uninitialized registers if the function were inlined, breaking
   correctness.  Functions with global refs must not be inlined."
  (let ((safe (make-hash-table :test #'eq)))
    (dolist (p params) (setf (gethash p safe) t))
    (dolist (inst body-instructions nil)
      (dolist (r (opt-inst-read-regs inst))
        (unless (gethash r safe)
          (return-from opt-body-has-global-refs-p t)))
      (let ((dst (opt-inst-dst inst)))
        (when dst (setf (gethash dst safe) t))))))

(defun opt-inline-eligible-p (def threshold)
  "Return T if the function definition DEF (a plist from opt-collect-function-defs)
   satisfies all inlining preconditions:
   1. Has a vm-closure with zero captured variables
   2. Has only required params (no &optional/&rest/&key)
   3. Body size ≤ THRESHOLD (excluding the final vm-ret)
   4. All body instructions support lossless register renaming
   5. Body reads no 'global' registers (ones not defined by params or body)"
  (let ((ci   (getf def :closure))
        (body (getf def :body)))
    (and (vm-closure-p ci)
         (null (vm-captured-vars ci))
         (null (vm-closure-optional-params ci))
         (null (vm-closure-rest-param ci))
         (null (vm-closure-key-params ci))
         (<= (1- (length body)) threshold)
         (opt-can-safely-rename-p body)
         (not (opt-body-has-global-refs-p body (vm-closure-params ci))))))

(defun opt-pass-inline (instructions &key (threshold 15))
  "Replace vm-call of a known small function with the inlined body.
   The function must be: captured-var-free, linear, and have ≤ THRESHOLD
   body instructions (not counting the final vm-ret)."
  (let* ((func-defs (opt-collect-function-defs instructions))
         (base-idx  (1+ (opt-max-reg-index instructions)))
         (reg-track (make-hash-table :test #'eq)) ; reg → label of known function
         ;; Build name→label mapping from vm-register-function instructions
         (name-to-label (let ((ht (make-hash-table :test #'eq)))
                          (dolist (inst instructions ht)
                            (when (vm-register-function-p inst)
                              (let ((label (gethash (vm-src inst) reg-track)))
                                ;; reg-track may not be populated yet; scan closures first
                                (when (null label)
                                  ;; Find the closure that the register came from
                                  (dolist (i instructions)
                                    (when (and (vm-closure-p i)
                                               (eq (vm-dst i) (vm-src inst)))
                                      (setf label (vm-label-name i))
                                      (return))))
                                (when label
                                  (setf (gethash (vm-func-name inst) ht) label)))))))
         (result nil))
    (dolist (inst instructions)
      (typecase inst
        ;; Record which register now holds which function
        ((or vm-closure vm-func-ref)
         (let ((label (vm-label-name inst)))
           (when (gethash label func-defs)
             (setf (gethash (vm-dst inst) reg-track) label)))
         (push inst result))
        ;; Track vm-const loading a function name symbol
        (vm-const
         (let* ((val (vm-value inst))
                (label (when (symbolp val) (gethash val name-to-label))))
           (when (and label (gethash label func-defs))
             (setf (gethash (vm-dst inst) reg-track) label)))
         (push inst result))
        ;; Attempt inlining
        (vm-call
         (let* ((label  (gethash (vm-func-reg inst) reg-track))
                (def    (and label (gethash label func-defs))))
           (if (and def (opt-inline-eligible-p def threshold))
               ;; ── Inline ──
               (let* ((body    (getf def :body))
                      (ci      (getf def :closure))
                      (params  (vm-closure-params ci))
                      (rename  (opt-make-renaming body base-idx)))
                 (incf base-idx (hash-table-count rename))
                 ;; 1. Move call arguments into renamed parameter registers
                 (loop for param in params
                       for arg  in (vm-args inst)
                       do (push (make-vm-move :dst (or (gethash param rename) param)
                                              :src arg)
                                result))
                 ;; 2. Emit renamed body (all but the last vm-ret)
                 (dolist (bi (butlast body))
                   (push (opt-rename-regs-in-inst bi rename) result))
                 ;; 3. Move renamed return register to call's dst
                 (let* ((ret-inst (car (last body)))
                        (ret-src  (or (gethash (vm-reg ret-inst) rename)
                                      (vm-reg ret-inst))))
                   (push (make-vm-move :dst (vm-dst inst) :src ret-src) result))
                 ;; The func-reg binding remains in reg-track (safe: reg still points to same fn)
                 )
               ;; ── Keep call as-is ──
               (push inst result))))
        ;; Invalidate tracking for any overwritten register
        (t
         (let ((dst (opt-inst-dst inst)))
           (when dst (remhash dst reg-track)))
         (push inst result))))
    (nreverse result)))
