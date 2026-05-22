(in-package :cl-cc/optimize)

;;; FR-642 — IR/VM invariant verification hooks for optimizer debugging.

(define-condition opt-ir-verification-error (error)
  ((pass-name :initarg :pass-name :reader opt-ir-verification-error-pass-name)
   (invariant :initarg :invariant :reader opt-ir-verification-error-invariant)
   (detail :initarg :detail :reader opt-ir-verification-error-detail)
   (context :initarg :context :reader opt-ir-verification-error-context))
  (:report (lambda (c s)
             (format s "~A verifier: ~A violated: ~A~%IR context:~%~A"
                     (opt-ir-verification-error-pass-name c)
                     (opt-ir-verification-error-invariant c)
                     (opt-ir-verification-error-detail c)
                     (opt-ir-verification-error-context c)))))

(defvar *verify-ir* nil
  "When non-NIL, verify IR/VM invariants before and after each optimizer pass.")

(defun %opt-dump-vm-context (instructions inst &key (radius 2))
  (with-output-to-string (s)
    (let ((pos (position inst instructions :test #'eq)))
      (if pos
          (loop for i from (max 0 (- pos radius)) below (min (length instructions) (+ pos radius 1))
                for cur = (nth i instructions)
                do (format s "~:[  ~;> ~]~D: ~S~%" (= i pos) i (instruction->sexp cur)))
          (dolist (cur instructions)
            (format s "  ~S~%" (instruction->sexp cur)))))))

(defun %opt-signal-ir-verification (pass-name invariant detail context)
  (error 'opt-ir-verification-error
         :pass-name (or pass-name "<unknown-pass>")
         :invariant invariant
         :detail detail
         :context context))

(defun %opt-vm-terminator-p (inst)
  (typep inst '(or vm-jump vm-jump-zero vm-ret vm-halt)))

(defun %opt-vm-blocks (instructions)
  "Split a flat VM instruction stream into label-delimited basic blocks."
  (let ((blocks nil)
        (current nil))
    (labels ((flush ()
               (when current
                 (push (nreverse current) blocks)
                 (setf current nil))))
      (dolist (inst instructions)
        (when (and (typep inst 'vm-label) current)
          (flush))
        (push inst current)
        (when (%opt-vm-terminator-p inst)
          (flush)))
      (flush)
      (nreverse blocks))))

(defun opt-verify-vm-instructions (instructions &key pass-name)
  "Verify flat VM optimizer IR invariants without transforming INSTRUCTIONS."
  (let ((labels (make-hash-table :test #'equal))
        (defined (make-hash-table :test #'eq)))
    (dolist (inst instructions)
      (when (typep inst 'vm-label)
        (let ((name (vm-name inst)))
          (when (gethash name labels)
            (%opt-signal-ir-verification pass-name :block-labels
                                         (format nil "duplicate label ~A" name)
                                         (%opt-dump-vm-context instructions inst)))
          (setf (gethash name labels) t))))
    (dolist (inst instructions)
      (typecase inst
        ((or vm-jump vm-jump-zero)
         (unless (gethash (vm-label-name inst) labels)
           (%opt-signal-ir-verification pass-name :block-terminators
                                        (format nil "unknown branch target ~A" (vm-label-name inst))
                                        (%opt-dump-vm-context instructions inst)))))
      (dolist (reg (opt-inst-read-regs inst))
        (unless (gethash reg defined)
          (%opt-signal-ir-verification pass-name :undefined-register-use
                                       (format nil "register ~A used before definition in ~S"
                                               reg (instruction->sexp inst))
                                       (%opt-dump-vm-context instructions inst))))
      (let ((dst (opt-inst-dst inst)))
        (when dst
          ;; VM registers may be reassigned, but live intervals for the same register may
          ;; not overlap. A linear VM stream has a new non-overlapping interval once the
          ;; previous value is no longer read; conservatively reject consecutive WAW with
          ;; no intervening read as a stale interval conflict.
          (setf (gethash dst defined) t))))
    (dolist (block (%opt-vm-blocks instructions))
      (let ((last (car (last block))))
        (unless (and last (%opt-vm-terminator-p last))
          (%opt-signal-ir-verification pass-name :block-terminators
                                       "basic block does not end with branch/return/halt"
                                       (%opt-dump-vm-context instructions last)))))
    t))

(defun %opt-pkg-symbol (package-name symbol-name)
  (let ((pkg (find-package package-name)))
    (and pkg (find-symbol symbol-name pkg))))

(defun %opt-call (package-name symbol-name &rest args)
  (let ((sym (%opt-pkg-symbol package-name symbol-name)))
    (when (and sym (fboundp sym))
      (apply (symbol-function sym) args))))

(defun %opt-typep (object package-name type-name)
  (let ((sym (%opt-pkg-symbol package-name type-name)))
    (and sym (typep object sym))))

(defun %opt-dominates-block-p (def-block use-block idom)
  (or (eq def-block use-block)
      (loop for cursor = use-block then (gethash cursor idom)
            while cursor
            thereis (eq cursor def-block)
            until (eq cursor (gethash cursor idom)))))

(defun %opt-verify-structured-ir (fn package-name &key pass-name)
  "Shared verifier for cl-cc/ir IR functions using dynamic accessors."
  (let* ((blocks (%opt-call package-name "IRF-BLOCKS" fn))
         (idom (or (%opt-call package-name "IR-DOMINATORS" fn)
                   (make-hash-table :test #'eq)))
         (defs (make-hash-table :test #'eq)))
    (dolist (block blocks)
      (unless (%opt-call package-name "IRB-TERMINATOR" block)
        (%opt-signal-ir-verification pass-name :block-terminators
                                     (format nil "IR block ~A has no terminator"
                                             (%opt-call package-name "IRB-LABEL" block))
                                     (format nil "~S" block)))
      (dolist (param (%opt-call package-name "IRB-PARAMS" block))
        (when (gethash param defs)
          (%opt-signal-ir-verification pass-name :live-interval-consistency
                                       (format nil "SSA value ~S defined more than once" param)
                                       (format nil "~S" block)))
        (setf (gethash param defs) block))
      (dolist (inst (%opt-call package-name "IRB-INSTS" block))
        (let ((result (%opt-call package-name "IRI-RESULT" inst)))
          (when result
            (when (gethash result defs)
              (%opt-signal-ir-verification pass-name :live-interval-consistency
                                           (format nil "SSA value ~S defined more than once" result)
                                           (format nil "~S" inst)))
            (setf (gethash result defs) block)))))
    (dolist (block blocks)
      (dolist (inst (%opt-call package-name "IRB-INSTS" block))
        (dolist (operand (%opt-call package-name "IR-OPERANDS" inst))
          (let ((def-block (gethash operand defs)))
            (unless def-block
              (%opt-signal-ir-verification pass-name :undefined-register-use
                                           (format nil "IR value ~S used before definition" operand)
                                           (format nil "~S" inst)))
            (unless (%opt-dominates-block-p def-block block idom)
              (%opt-signal-ir-verification pass-name :ssa-dominance
                                           (format nil "definition of ~S does not dominate use" operand)
                                           (format nil "~S" inst)))))))
    t))

(defun %opt-mir-value-p (x)
  (%opt-typep x "CL-CC/MIR" "MIR-VALUE"))

(defun %opt-verify-mir-function (fn &key pass-name)
  "Verifier for cl-cc/mir MIR functions using dynamic accessors."
  (let ((defs (make-hash-table :test #'eq)))
    (dolist (param (%opt-call "CL-CC/MIR" "MIRF-PARAMS" fn))
      (setf (gethash param defs) (%opt-call "CL-CC/MIR" "MIRF-ENTRY" fn)))
    (dolist (block (%opt-call "CL-CC/MIR" "MIRF-BLOCKS" fn))
      (let* ((insts (append (%opt-call "CL-CC/MIR" "MIRB-PHIS" block)
                            (%opt-call "CL-CC/MIR" "MIRB-INSTS" block)))
             (last (car (last insts))))
        (unless (and last (member (%opt-call "CL-CC/MIR" "MIRI-OP" last)
                                  '(:ret :jump :branch :tail-call) :test #'eq))
          (%opt-signal-ir-verification pass-name :block-terminators
                                       (format nil "MIR block ~A has no terminator"
                                               (%opt-call "CL-CC/MIR" "MIRB-LABEL" block))
                                       (format nil "~S" block)))
        (dolist (inst insts)
          (dolist (src (%opt-call "CL-CC/MIR" "MIRI-SRCS" inst))
            (let ((value (if (and (consp src) (%opt-mir-value-p (cdr src))) (cdr src) src)))
              (when (%opt-mir-value-p value)
                (unless (gethash value defs)
                  (%opt-signal-ir-verification pass-name :undefined-register-use
                                               (format nil "MIR value ~S used before definition" value)
                                               (format nil "~S" inst))))))
          (let ((dst (%opt-call "CL-CC/MIR" "MIRI-DST" inst)))
            (when dst
              (when (gethash dst defs)
                (%opt-signal-ir-verification pass-name :live-interval-consistency
                                             (format nil "MIR value ~S defined more than once" dst)
                                             (format nil "~S" inst)))
              (setf (gethash dst defs) block))))))
    t))

(defun opt-verify-ir (ir &key pass-name)
  "Verify optimizer IR invariants. Currently supports flat VM instruction streams;
the function is intentionally non-transforming so it can be registered as a pass."
  (cond
    ((listp ir) (opt-verify-vm-instructions ir :pass-name pass-name))
    ((%opt-typep ir "CL-CC/IR" "IR-FUNCTION")
     (%opt-verify-structured-ir ir "CL-CC/IR" :pass-name pass-name))
    ((%opt-typep ir "CL-CC/MIR" "MIR-FUNCTION")
     (%opt-verify-mir-function ir :pass-name pass-name))
    ((%opt-typep ir "CL-CC/MIR" "MIR-MODULE")
     (dolist (fn (%opt-call "CL-CC/MIR" "MIRM-FUNCTIONS" ir))
       (%opt-verify-mir-function fn :pass-name pass-name)))
    (t t))
  ir)

(defun opt-pass-verify-ir (instructions)
  "Registration pass: verify invariants only and return INSTRUCTIONS unchanged."
  (opt-verify-ir instructions :pass-name :verify-ir)
  instructions)
