;;;; optimizer-trans-validate.lisp — FR-752 Translation Validation

(in-package :cl-cc/optimize)

(defvar *translation-validation-enabled* nil
  "When non-NIL, validate before/after IR for every optimizer pass.")

(defun tv-symbolic-execute-block (instructions)
  "Return a conservative symbolic summary for INSTRUCTIONS."
  (let ((env (make-hash-table :test #'eq))
        (outputs nil)
        (control nil))
    (dolist (inst instructions)
      (typecase inst
        (vm-const
         (setf (gethash (vm-dst inst) env) (list :const (vm-value inst))))
        (vm-move
         (setf (gethash (vm-dst inst) env) (or (gethash (vm-src inst) env)
                                               (list :input (vm-src inst)))))
        (vm-binop
         (setf (gethash (vm-dst inst) env)
               (list (type-of inst)
                     (or (gethash (vm-lhs inst) env) (list :input (vm-lhs inst)))
                     (or (gethash (vm-rhs inst) env) (list :input (vm-rhs inst))))))
        ((or vm-jump vm-jump-zero)
         (push (instruction->sexp inst) control))
        ((or vm-ret vm-halt)
         (push (mapcar (lambda (reg) (or (gethash reg env) (list :input reg)))
                       (opt-inst-read-regs inst))
               outputs))
        (t
         (let ((dst (opt-inst-dst inst)))
           (when dst
             (setf (gethash dst env)
                   (cons (type-of inst)
                         (mapcar (lambda (reg)
                                   (or (gethash reg env) (list :input reg)))
                                 (opt-inst-read-regs inst)))))))))
    (list :outputs (nreverse outputs)
          :control (nreverse control))))

(defun %tv-label-set (instructions)
  (sort (loop for inst in instructions
              when (typep inst 'vm-label)
                collect (vm-name inst))
        #'string<))

(defun %tv-observable-shape (instructions)
  "Return a semantic shape used as a safe validation filter."
  (list :labels (%tv-label-set instructions)
        :returns (count-if (lambda (inst) (typep inst '(or vm-ret vm-halt))) instructions)
        :errors (count-if (lambda (inst)
                            (member (type-of inst)
                                    '(vm-signal vm-signal-error vm-error-instruction vm-cerror)
                                    :test #'eq))
                          instructions)))

(defun translation-validation-equivalent-p (before after)
  "Conservatively check same observable outputs for same symbolic inputs."
  (or (equal (mapcar #'instruction->sexp before)
             (mapcar #'instruction->sexp after))
      (and (equal (%tv-observable-shape before) (%tv-observable-shape after))
           (equal (tv-symbolic-execute-block before)
                  (tv-symbolic-execute-block after)))))

(defun validate-optimizer-translation (pass before after)
  "Warn when PASS appears to change IR semantics. Never abort compilation."
  (when (and *translation-validation-enabled*
             (not (translation-validation-equivalent-p before after)))
    (warn "Translation validation failed for ~S" pass))
  after)

(defun opt-pass-translation-validation (instructions)
  "FR-752 registration pass. Actual per-pass validation is pipeline-integrated."
  instructions)
