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

(in-package :cl-cc/optimize)

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

(defun %opt-collect-sexp-regs-into-cell (form cell)
  "Recursively push register keywords from sexp FORM onto (car CELL)."
  (cond ((and (keywordp form) (opt-register-keyword-p form))
         (push form (car cell)))
        ((consp form)
         (%opt-collect-sexp-regs-into-cell (car form) cell)
         (%opt-collect-sexp-regs-into-cell (cdr form) cell))))

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
          (sexp-regs-cell (list nil)))
      (handler-case (%opt-collect-sexp-regs-into-cell (instruction->sexp inst) sexp-regs-cell)
        (error () (return-from opt-can-safely-rename-p nil)))
      (let ((sexp-regs (car sexp-regs-cell)))
        (unless (every (lambda (r) (member r sexp-regs)) explicit-regs)
          (return-from opt-can-safely-rename-p nil))))))

(defun opt-rename-regs-in-inst (inst renaming)
  "Return INST with all VM register keywords substituted per RENAMING.
   Uses instruction→sexp roundtrip; returns INST unchanged on any error."
  (handler-case
      (sexp->instruction
       (opt-map-tree (lambda (x) (if (and (keywordp x) (opt-register-keyword-p x))
                                     (or (gethash x renaming) x)
                                     x))
                     (instruction->sexp inst)))
    (error () inst)))

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

(defun opt-build-function-name-map (instructions)
  "Return symbol → function-label mapping for top-level function registrations."
  (let ((reg-track (make-hash-table :test #'eq))
        (name-to-label (make-hash-table :test #'eq)))
    (dolist (inst instructions)
      (typecase inst
        ((or vm-closure vm-func-ref)
         (let ((label (vm-label-name inst)))
           (when label
             (setf (gethash (vm-dst inst) reg-track) label))))
        (vm-register-function
         (let ((label (or (gethash (vm-src inst) reg-track)
                          (dolist (i instructions)
                            (when (and (vm-closure-p i)
                                       (eq (vm-dst i) (vm-src inst)))
                              (return (vm-label-name i)))))))
            (when label
              (setf (gethash (vm-func-name inst) name-to-label) label))))))
    name-to-label))

(defun opt-known-callee-labels (instructions)
  "Return reg -> known callee label mapping tracked through simple designators." 
  (let ((name-to-label (opt-build-function-name-map instructions))
        (reg-track (make-hash-table :test #'eq)))
    (dolist (inst instructions reg-track)
      (typecase inst
        ((or vm-closure vm-func-ref)
         (setf (gethash (vm-dst inst) reg-track) (vm-label-name inst)))
        (vm-const
         (let ((label (and (symbolp (vm-value inst))
                           (gethash (vm-value inst) name-to-label))))
           (if label
               (setf (gethash (vm-dst inst) reg-track) label)
               (remhash (vm-dst inst) reg-track))))
        (vm-move
         (multiple-value-bind (label found-p)
             (gethash (vm-move-src inst) reg-track)
           (if found-p
                (setf (gethash (vm-dst inst) reg-track) label)
                (remhash (vm-dst inst) reg-track))))
        (t
         (let ((dst (opt-inst-dst inst)))
             (when dst
               (remhash dst reg-track))))))))

(defun %opt-call-site-split-fresh-label (used-labels)
  "Return a fresh after-call label not present in USED-LABELS."
  (loop for i from 0
        for label = (format nil "CALL-SITE-SPLIT-AFTER-~D" i)
        unless (gethash label used-labels)
          do (setf (gethash label used-labels) t)
             (return label)))

(defun %opt-known-callee-before-index (instructions end-index func-reg name-to-label)
  "Return FUNC-REG's known label immediately before END-INDEX, within one block."
  (loop for i downfrom (1- end-index) downto 0
        for inst = (nth i instructions)
        do (cond
             ((or (typep inst 'vm-label)
                  (typep inst 'vm-jump)
                  (typep inst 'vm-jump-zero)
                  (typep inst 'vm-ret)
                  (typep inst 'vm-halt))
              (return nil))
             ((eq (opt-inst-dst inst) func-reg)
              (return
                (typecase inst
                  ((or vm-closure vm-func-ref)
                   (vm-label-name inst))
                  (vm-const
                   (and (symbolp (vm-value inst))
                        (gethash (vm-value inst) name-to-label)))
                  (t nil)))))))

(defun %opt-copy-vm-call (call)
  "Return a fresh copy of CALL for predecessor-local call-site splitting."
  (make-vm-call :dst (vm-dst call)
                :func (vm-func-reg call)
                :args (copy-list (vm-args call))))

(defun opt-pass-call-site-splitting (instructions)
  "Duplicate simple join-block call sites into predecessors with known callees.
This conservative subset handles `jump -> join-label -> vm-call` shapes.  When a
predecessor block assigns the call's function register to a known label before
jumping to the join, the pass replaces that jump with a direct func-ref, a copy
of the call, and a jump to a fresh after-call label.  The original join call is
kept for fall-through/unknown predecessors."
  (let* ((len (length instructions))
         (name-to-label (opt-build-function-name-map instructions))
         (used-labels (make-hash-table :test #'equal))
         (replacements (make-hash-table :test #'eql))
         (after-labels (make-hash-table :test #'eql)))
    (dolist (inst instructions)
      (when (typep inst 'vm-label)
        (setf (gethash (vm-name inst) used-labels) t)))
    (loop for call-index from 1 below len
          for label-inst = (nth (1- call-index) instructions)
          for call-inst = (nth call-index instructions)
          when (and (typep label-inst 'vm-label)
                    (typep call-inst 'vm-call))
            do (let ((join-label (vm-name label-inst))
                     (func-reg (vm-func-reg call-inst))
                     (split-jumps nil))
                 (loop for jump-index from 0 below (1- call-index)
                       for jump-inst = (nth jump-index instructions)
                       when (and (typep jump-inst 'vm-jump)
                                 (equal (vm-label-name jump-inst) join-label))
                         do (let ((callee-label
                                    (%opt-known-callee-before-index
                                     instructions jump-index func-reg name-to-label)))
                              (when callee-label
                                (push (list jump-index callee-label) split-jumps))))
                 (when split-jumps
                   (let ((after-label (%opt-call-site-split-fresh-label used-labels)))
                     (setf (gethash call-index after-labels) after-label)
                     (dolist (split split-jumps)
                       (destructuring-bind (jump-index callee-label) split
                         (setf (gethash jump-index replacements)
                               (list (make-vm-func-ref :dst func-reg :label callee-label)
                                     (%opt-copy-vm-call call-inst)
                                     (make-vm-jump :label after-label)))))))))
    (loop for index from 0 below len
          for inst = (nth index instructions)
          append (or (gethash index replacements)
                     (let ((after-label (gethash index after-labels)))
                       (if after-label
                           (list inst (make-vm-label :name after-label))
                           (list inst)))))))

(defun %opt-devirt-track-designator (inst name-to-label reg-track)
  "Update REG-TRACK with known callee information produced by INST."
  (typecase inst
    ((or vm-closure vm-func-ref)
     (setf (gethash (vm-dst inst) reg-track) (vm-label-name inst)))
    (vm-const
     (let ((label (and (symbolp (vm-value inst))
                       (gethash (vm-value inst) name-to-label))))
       (if label
           (setf (gethash (vm-dst inst) reg-track) label)
           (remhash (vm-dst inst) reg-track))))
    (vm-move
     (multiple-value-bind (label found-p)
         (gethash (vm-move-src inst) reg-track)
       (if found-p
           (setf (gethash (vm-dst inst) reg-track) label)
           (remhash (vm-dst inst) reg-track))))
    (t
     (let ((dst (opt-inst-dst inst)))
       (when dst
         (remhash dst reg-track))))))

(defun %opt-last-emitted-func-ref-p (result reg label)
  "Return T when RESULT already starts with REG's direct reference to LABEL."
  (let ((last (first result)))
    (and (typep last 'vm-func-ref)
         (eq (vm-dst last) reg)
         (equal (vm-label-name last) label))))

(defun %opt-devirt-call (inst reg-track result)
  "Emit a direct callee reference before INST when its callee register is known."
  (let* ((func-reg (vm-func-reg inst))
         (label (gethash func-reg reg-track)))
    (when (and label
               (not (%opt-last-emitted-func-ref-p result func-reg label)))
      (push (make-vm-func-ref :dst func-reg :label label) result))
    (push inst result)
    (let ((dst (opt-inst-dst inst)))
      (when dst
        (remhash dst reg-track)))
    result))

(defun opt-pass-devirtualize (instructions)
  "Insert direct function references before calls whose callee register is known.
This implements a conservative called-value propagation slice: closure,
function-reference, registered symbol, and move designators are tracked through a
single forward scan, and `vm-call`/`vm-tail-call`/`vm-apply` sites with a known
callee receive a `vm-func-ref` immediately before the call.  The call itself is
left in place so existing VM semantics and the inline pass remain unchanged."
  (let ((name-to-label (opt-build-function-name-map instructions))
        (reg-track (make-hash-table :test #'eq))
        (result nil))
    (dolist (inst instructions (nreverse result))
      (typecase inst
        ((or vm-call vm-tail-call vm-apply)
         (setf result (%opt-devirt-call inst reg-track result)))
        (t
         (%opt-devirt-track-designator inst name-to-label reg-track)
         (push inst result))))))
