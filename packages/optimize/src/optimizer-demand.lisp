(in-package :cl-cc/optimize)
;;; FR-182: Demand / Strictness Analysis

(defstruct (opt-demand-summary (:conc-name opt-demand-summary-))
  "Per-function demand summary for VM parameters.
DEMANDS is an alist of (param-reg . (:strict | :lazy | :absent)).  STRICT-PARAMS
are safe candidates for unboxed/stack-local calling-convention treatment;
ABSENT-PARAMS let call-site cleanup replace unused argument values, enabling DCE
of pure argument computations."
  function
  (params nil :type list)
  (demands nil :type list)
  (strict-params nil :type list)
  (absent-params nil :type list))

(defvar *opt-demand-summary-table* (make-hash-table :test #'equal)
  "Latest FR-182 demand summaries keyed by function label.")

(defun %opt-demand-param-state (state param)
  (or (cdr (assoc param state :test #'eq)) :killed))

(defun %opt-demand-mark-read (state param)
  (let ((cell (assoc param state :test #'eq)))
    (cond
      ((null cell) state)
      ((eq (cdr cell) :unseen)
       (acons param :read (remove param state :key #'car :test #'eq)))
      (t state))))

(defun %opt-demand-mark-killed (state param)
  (let ((cell (assoc param state :test #'eq)))
    (cond
      ((null cell) state)
      ((eq (cdr cell) :unseen)
       (acons param :killed (remove param state :key #'car :test #'eq)))
      (t state))))

(defun %opt-demand-step-inst (inst params state)
  "Advance PARAM demand STATE through INST."
  (let ((next state))
    (dolist (reg (opt-inst-read-regs inst))
      (when (member reg params :test #'eq)
        (setf next (%opt-demand-mark-read next reg))))
    (let ((dst (opt-inst-dst inst)))
      (when (and dst (member dst params :test #'eq))
        (setf next (%opt-demand-mark-killed next dst))))
    next))

(defun %opt-demand-block-transfer (block params state)
  (let ((next state))
    (dolist (inst (bb-instructions block) next)
      (setf next (%opt-demand-step-inst inst params next)))))

(defun %opt-demand-exit-block-p (block)
  (let ((insts (bb-instructions block)))
    (or (null insts)
        (typep (car (last insts)) '(or vm-ret vm-halt)))))

(defun %opt-demand-collect-exit-states (cfg params)
  "Path-sensitive demand propagation over CFG."
  (let ((work (list (cons (cfg-entry cfg)
                          (mapcar (lambda (p) (cons p :unseen)) params))))
        (seen (make-hash-table :test #'equal))
        (exits nil))
    (loop while work do
      (destructuring-bind (block . state) (pop work)
        (let ((key (list (bb-id block) state)))
          (unless (gethash key seen)
            (setf (gethash key seen) t)
            (let ((out (%opt-demand-block-transfer block params state)))
              (if (or (%opt-demand-exit-block-p block)
                      (null (bb-successors block)))
                  (push out exits)
                  (dolist (succ (bb-successors block))
                    (push (cons succ out) work))))))))
    (or exits (list (mapcar (lambda (p) (cons p :killed)) params)))))

(defun opt-analyze-function-demand (function params body-instructions)
  "Analyze PARAM usage in BODY-INSTRUCTIONS for FUNCTION label."
  (let* ((cfg (cfg-build body-instructions))
         (exits (%opt-demand-collect-exit-states cfg params))
         (demands
           (loop for param in params
                 collect (let* ((states (mapcar (lambda (state)
                                                  (%opt-demand-param-state state param))
                                                exits))
                                (read-count (count :read states :test #'eq)))
                           (cons param
                                 (cond
                                   ((and states (= read-count (length states))) :strict)
                                   ((plusp read-count) :lazy)
                                   (t :absent))))))
         (strict-params (loop for (param . demand) in demands
                              when (eq demand :strict) collect param))
         (absent-params (loop for (param . demand) in demands
                              when (eq demand :absent) collect param)))
    (make-opt-demand-summary
     :function function
     :params params
     :demands demands
     :strict-params strict-params
     :absent-params absent-params)))

(defun %opt-demand-collect-function-bodies (instructions)
  "Return label -> body-instructions for closures/functions in INSTRUCTIONS."
  (let ((labels (make-hash-table :test #'equal))
        (callable-labels (make-hash-table :test #'equal)))
    (dolist (inst instructions)
      (when (typep inst '(or vm-closure vm-func-ref))
        (setf (gethash (vm-label-name inst) callable-labels) t)))
    (let ((vec (coerce instructions 'vector)))
      (loop for i from 0 below (length vec)
            for inst = (aref vec i)
            when (and (typep inst 'vm-label)
                      (gethash (vm-name inst) callable-labels))
              do (let ((body nil))
                   (loop for j from (1+ i) below (length vec)
                         for body-inst = (aref vec j)
                         do (push body-inst body)
                         when (typep body-inst '(or vm-ret vm-halt))
                           do (return))
                   (setf (gethash (vm-name inst) labels) (nreverse body)))))
    labels))

(defun opt-analyze-program-demand (instructions)
  "Analyze all VM closures/functions in INSTRUCTIONS and return a summary table."
  (let ((bodies (%opt-demand-collect-function-bodies instructions))
        (summaries (make-hash-table :test #'equal)))
    (dolist (inst instructions summaries)
      (when (and (typep inst '(or vm-closure vm-func-ref))
                 (vm-closure-params inst))
        (let* ((label (vm-label-name inst))
               (body (gethash label bodies)))
          (when body
            (setf (gethash label summaries)
                  (opt-analyze-function-demand label (vm-closure-params inst) body))))))))

(defun %opt-demand-rewrite-call (inst new-args)
  (if (typep inst 'vm-tail-call)
      (make-vm-tail-call :dst (vm-dst inst) :func (vm-func-reg inst) :args new-args)
      (make-vm-call :dst (vm-dst inst) :func (vm-func-reg inst) :args new-args)))

(defun %opt-demand-rewrite-call-site (inst summary params nil-const-regs fresh-nil-reg)
  "Rewrite INST's argument list, replacing absent-param args with fresh NIL regs.

Returns (values prefix-consts new-call changed-p).
PREFIX-CONSTS is a list of vm-const instructions to emit before the call
(in forward order).  NEW-CALL is the rewritten call instruction.
CHANGED-P is T when at least one argument was replaced."
  (let ((prefix nil)
        (new-args nil)
        (changed-p nil))
    (loop for arg in (vm-args inst)
          for i from 0
          do (let ((absent-p (member (nth i params)
                                     (opt-demand-summary-absent-params summary)
                                     :test #'eq)))
               (cond
                 ((and absent-p (gethash arg nil-const-regs))
                  (push arg new-args))
                 (absent-p
                  (let ((nil-reg (funcall fresh-nil-reg)))
                    (setf (gethash nil-reg nil-const-regs) t)
                    (push (make-vm-const :dst nil-reg :value nil) prefix)
                    (push nil-reg new-args)
                    (setf changed-p t)))
                 (t
                  (push arg new-args)))))
    (values (nreverse prefix)
            (%opt-demand-rewrite-call inst (nreverse new-args))
            changed-p)))

(defun opt-pass-demand-analysis (instructions)
  "Run FR-182 demand analysis and expose call-site cleanup for absent params.

The pass is conservative: it preserves arity, replacing only absent argument
registers with a fresh NIL constant at known direct call sites. This lets later
DCE remove pure computations that fed those now-unused registers, while effectful
argument computations remain in the instruction stream."
  (let* ((summaries      (opt-analyze-program-demand instructions))
         (reg->label     (make-hash-table :test #'eq))
         (nil-const-regs (make-hash-table :test #'eq))
         (base           (1+ (opt-max-reg-index instructions)))
         (changed        nil)
         (out            nil))
    (setf *opt-demand-summary-table* summaries)
    (flet ((fresh-nil-reg ()
             (prog1 (intern (format nil "R~A" base) :keyword)
               (incf base)))
           (clear-dst (inst)
             (let ((dst (opt-inst-dst inst)))
               (when dst
                 (remhash dst reg->label)
                 (remhash dst nil-const-regs)))))
      (dolist (inst instructions)
        (cond
          ((typep inst '(or vm-closure vm-func-ref))
           (setf (gethash (vm-dst inst) reg->label) (vm-label-name inst))
           (remhash (vm-dst inst) nil-const-regs)
           (push inst out))
          ((typep inst 'vm-const)
           (remhash (vm-dst inst) reg->label)
           (if (null (vm-value inst))
               (setf (gethash (vm-dst inst) nil-const-regs) t)
               (remhash (vm-dst inst) nil-const-regs))
           (push inst out))
          ((typep inst '(or vm-call vm-tail-call))
           (let* ((label   (gethash (vm-func-reg inst) reg->label))
                  (summary (and label (gethash label summaries)))
                  (params  (and summary (opt-demand-summary-params summary)))
                  (args    (vm-args inst)))
             (if (and summary params args)
                 (multiple-value-bind (prefix new-call changed-p)
                     (%opt-demand-rewrite-call-site
                      inst summary params nil-const-regs #'fresh-nil-reg)
                   (dolist (const prefix) (push const out))
                   (push new-call out)
                   (when changed-p (setf changed t)))
                 (push inst out)))
           (clear-dst inst))
          (t
           (clear-dst inst)
           (push inst out)))))
    (if changed (nreverse out) instructions)))

;;; ─── Top-Level Optimizer ─────────────────────────────────────────────────
