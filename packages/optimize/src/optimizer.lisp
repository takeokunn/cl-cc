(in-package :cl-cc/optimize)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Optimizer — Pass Implementations
;;;
;;; All optimization passes over the VM instruction sequence.
;;; Data tables and derived predicates live in optimizer-tables.lisp.
;;;
;;; Pipeline (two iterations for convergence):
;;;   opt-pass-fold        constant folding + algebraic simplification
;;;                        + unary folding + type predicate folding
;;;                        + constant branch elimination
;;;   opt-pass-dce         global-usedness dead code elimination (pure insts only)
;;;   opt-pass-jump        jump threading + dead jump elimination
;;;   opt-pass-unreachable remove instructions after unconditional transfers
;;; FR-020: Allocation Sinking — delays heap allocations (cons etc.) as late as possible, moving them into conditional branches to reduce GC pressure on fast paths
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar *optimization-report-stream* nil
  "When non-NIL, optimizer passes emit one-line optimization reports here.")

(defun %opt-report (kind control &rest args)
  "Emit one optimizer debugging report line when reporting is enabled."
  (when *optimization-report-stream*
    (format *optimization-report-stream* "~&opt-report ~A " kind)
    (apply #'format *optimization-report-stream* control args)
    (terpri *optimization-report-stream*)))

;;; ─── Pass 1: Constant Folding + Algebraic Simplification ─────────────────

;;;; ─── FR-179: Sequence Operation Fusion ──────────────────────────────────

(defun %opt-sequence-fusion-callee-name (inst)
  "Return the normalized callee name for a direct function reference INST."
  (when (typep inst 'vm-func-ref)
    (let ((label (vm-label-name inst)))
      (cond
        ((symbolp label) (symbol-name label))
        ((stringp label) (string-upcase label))
        (t nil)))))

(defun %opt-sequence-op-name-p (name)
  "Return T when NAME is one of the sequence operations handled by FR-179."
  (member name '("MAPCAR" "REMOVE-IF" "REMOVE-IF-NOT") :test #'string=))

(defun %opt-sequence-fusion-candidate-p (instructions)
  "Return T when INSTRUCTIONS still contain an unfused direct sequence-op chain.

The normal FR-179 implementation is source preserving: compiler macros in the
expander rewrite visible sequence chains before MAPCAR/REMOVE-IF expand into
loops.  This predicate is retained in the optimizer so pass tracing can identify
late direct-call chains produced by non-stdlib frontends without risking an
incorrect closure synthesis at VM level."
  (let ((func-refs (make-hash-table :test #'eq))
        (sequence-call-dsts nil))
    (dolist (inst instructions nil)
      (typecase inst
        (vm-func-ref
         (let ((name (%opt-sequence-fusion-callee-name inst)))
           (when (%opt-sequence-op-name-p name)
             (setf (gethash (vm-dst inst) func-refs) name))))
        (vm-call
         (let ((callee (gethash (vm-func-reg inst) func-refs)))
           (when (%opt-sequence-op-name-p callee)
             (when (some (lambda (arg) (member arg sequence-call-dsts :test #'eq))
                         (vm-args inst))
               (return-from %opt-sequence-fusion-candidate-p t))
             (pushnew (vm-dst inst) sequence-call-dsts :test #'eq))))))))

(defun opt-pass-sequence-fusion (instructions)
  "FR-179: fuse chained sequence operations.

Source-level compiler macros lower MAPCAR/REMOVE-IF chains into one explicit
loop before macro expansion, so the instruction stream normally arrives here
already fused.  The optimizer pass is intentionally conservative: it recognizes
late direct-call chains for reporting, but does not synthesize new closures from
VM bytecode.  Macro-expanded loop streams are therefore preserved exactly after
their intermediate allocation has already been eliminated upstream."
  (when (%opt-sequence-fusion-candidate-p instructions)
    (%opt-report :sequence-fusion
                 "late direct sequence call chain left unchanged; source fusion unavailable"))
  instructions)

(defun opt-fold-binop-value (inst lval rval)
  "Fold binary INST with numeric constants LVAL and RVAL.
   Returns (values folded-value t) on success, or (values nil nil) if not foldable.
   Dispatch is data-driven via *opt-binary-fold-table* and *opt-binary-cmp-fold-table*."
  (let* ((tp (type-of inst))
         (arith-fn (gethash tp *opt-binary-fold-table*))
         (cmp-fn   (gethash tp *opt-binary-cmp-fold-table*)))
    (cond
      ;; Instructions that must not be folded (values-list side-channel)
      ((member tp *opt-binary-no-fold-types* :test #'eq)
       (values nil nil))
      ;; Zero-guarded arithmetic (div/mod/rem)
      ((and arith-fn (member tp *opt-binary-zero-guard-types* :test #'eq))
       (if (zerop rval)
           (values nil nil)
           (values (funcall arith-fn lval rval) t)))
      ;; Normal arithmetic fold
      (arith-fn
       (values (funcall arith-fn lval rval) t))
      ;; Comparison fold → 1/0
      (cmp-fn
       (values (if (funcall cmp-fn lval rval) 1 0) t))
      (t (values nil nil)))))

(defun %opt-known-constant-p (v)
  "Return T if V is a known concrete constant (not the :unknown sentinel)."
  (and (not (eq v :unknown)) (numberp v)))

(defun %opt-copy-derived-fact (src dst table)
  "Propagate SRC's derived fact to DST within TABLE, or clear DST."
  (multiple-value-bind (fact found-p) (gethash src table)
    (if found-p
        (setf (gethash dst table) fact)
        (remhash dst table))))

(defun %opt-logand-low-bit-source (lhs rhs lval lfound rval rfound)
  "Return the original source when LOGAND is known to mask with literal 1."
  (cond
    ((and rfound (eql rval 1)) lhs)
    ((and lfound (eql lval 1)) rhs)
    (t nil)))

(defun %opt-rewrite-logand-low-bit-test (inst lhs rhs lval lfound rval rfound low-bit-facts)
  "Rewrite (= (logand x 1) 0/1) style tests into vm-evenp/vm-oddp when known."
  (labels ((rewrite-from (reg constant)
             (multiple-value-bind (source found-p) (gethash reg low-bit-facts)
               (when found-p
                 (case constant
                   (0 (make-vm-evenp :dst (vm-dst inst) :src source))
                   (1 (make-vm-oddp  :dst (vm-dst inst) :src source)))))))
    (or (and lfound (integerp lval) (rewrite-from rhs lval))
        (and rfound (integerp rval) (rewrite-from lhs rval)))))

(defun %opt-apply-algebraic-action (action dst lhs-reg rhs-reg)
  "Produce an instruction from an algebraic simplification ACTION.
   Returns NIL if ACTION is unrecognized."
  (case action
    (:move-lhs (make-vm-move :dst dst :src lhs-reg))
    (:move-rhs (make-vm-move :dst dst :src rhs-reg))
    (otherwise
     (when (consp action)
       (case (car action)
         (:const (make-vm-const :dst dst :value (cadr action)))
         (:neg   (make-vm-neg  :dst dst
                               :src (if (eq (cadr action) :lhs) lhs-reg rhs-reg))))))))

(defun opt-simplify-binop (inst dst lhs-reg rhs-reg lval rval)
  "Algebraic simplification of binary INST via rule table lookup.
   LVAL/RVAL are known constant values or :unknown.
   Returns a simplified instruction, or NIL if no simplification applies."
  (dolist (rule (gethash (type-of inst) *opt-algebraic-identity-rules*) nil)
    (let ((cond   (car rule))
          (action (cdr rule)))
      (when (cond
              ((eq cond :same-reg)
               (eq lhs-reg rhs-reg))
              ((and (consp cond) (eq (car cond) :rconst))
               (and (%opt-known-constant-p rval) (eql rval (cadr cond))))
              ((and (consp cond) (eq (car cond) :lconst))
               (and (%opt-known-constant-p lval) (eql lval (cadr cond)))))
        (return (%opt-apply-algebraic-action action dst lhs-reg rhs-reg))))))

(defun %opt-producer-condition-types (condition)
  "Return producer instruction types named by an algebraic CONDITION, if any."
  (when (and (consp condition) (eq (car condition) :producer))
    (let ((types (cadr condition)))
      (if (listp types) types (list types)))))

(defun %opt-algebraic-producer-type-p (type)
  "Return T when TYPE is referenced by a producer-based algebraic rule."
  (loop for rules being the hash-values of *opt-algebraic-identity-rules*
        thereis (loop for rule in rules
                      for producer-types = (%opt-producer-condition-types (car rule))
                      thereis (member type producer-types :test #'eq))))

(defun opt-simplify-type-pred-with-producer (inst producer-facts)
  "Simplify unary type predicate INST using producer facts for its source register."
  (let ((producer-type (gethash (vm-src inst) producer-facts)))
    (when producer-type
      (dolist (rule (gethash (type-of inst) *opt-algebraic-identity-rules*) nil)
        (let ((producer-types (%opt-producer-condition-types (car rule))))
          (when (member producer-type producer-types :test #'eq)
            (return (%opt-apply-algebraic-action (cdr rule) (vm-dst inst) nil nil))))))))

(defun %opt-clear-derived-facts (dst env low-bit-facts producer-facts)
  "Clear all non-structural derived facts for destination register DST."
  (when dst
    (remhash dst env)
    (remhash dst low-bit-facts)
    (remhash dst producer-facts)))

(defun %opt-record-producer-fact (inst env low-bit-facts producer-facts)
  "Record TYPE-OF INST as a producer fact for its destination when rule-relevant."
  (let ((dst (opt-inst-dst inst)))
    (when dst
      (remhash dst env)
      (remhash dst low-bit-facts)
      (if (%opt-algebraic-producer-type-p (type-of inst))
          (setf (gethash dst producer-facts) (type-of inst))
          (remhash dst producer-facts)))))

(defun %opt-branch-target-labels (instructions)
  "Return a hash table of labels that are explicit branch targets."
  (let ((targets (make-hash-table :test #'equal)))
    (dolist (inst instructions targets)
      (typecase inst
        ((or vm-jump vm-jump-zero)
         (setf (gethash (vm-label-name inst) targets) t))))))

(defun %fold-vm-label (inst env low-bit-facts producer-facts emit target-labels)
  "Flush constant knowledge only at labels that are explicit branch targets."
  (when (gethash (vm-name inst) target-labels)
    (clrhash env)
    (clrhash low-bit-facts)
    (clrhash producer-facts))
  (funcall emit inst))

(defun %fold-vm-const (inst env low-bit-facts producer-facts emit)
  "Record the constant value of INST's destination in ENV, then emit."
  (remhash (vm-dst inst) low-bit-facts)
  (remhash (vm-dst inst) producer-facts)
  (setf (gethash (vm-dst inst) env) (vm-value inst))
  (funcall emit inst))

(defun %fold-vm-move (inst env low-bit-facts producer-facts emit emit-const clear)
  "Propagate constant if src is known in ENV; eliminate self-moves silently."
  (let ((src (vm-src inst)) (dst (vm-dst inst)))
    (cond
      ((eq src dst)) ; self-move: drop silently
      (t
       (multiple-value-bind (sval found) (gethash src env)
          (if found
               (progn
                 (remhash dst low-bit-facts)
                 (remhash dst producer-facts)
                  (funcall emit-const dst sval inst))
               (progn
                 (funcall clear dst)
                 (%opt-copy-derived-fact src dst low-bit-facts)
                 (%opt-copy-derived-fact src dst producer-facts)
                 (funcall emit inst))))))))

(defun %fold-binary-inst (inst env low-bit-facts producer-facts emit emit-const clear)
  "Binary arithmetic/comparison: full fold or algebraic simplification.
   Dispatch derived from opt-binary-lhs-rhs-p (covers *opt-binary-lhs-rhs-types*).
   vm-floor-inst/vm-ceiling-inst/vm-truncate are excluded (values-list side-channel)."
  (let* ((dst (vm-dst inst))
         (lhs (vm-lhs inst))
         (rhs (vm-rhs inst)))
    (multiple-value-bind (lval lfound) (gethash lhs env)
      (multiple-value-bind (rval rfound) (gethash rhs env)
        (cond
          ;; Both operands are known numeric constants → full fold
          ((and lfound rfound (numberp lval) (numberp rval))
           (multiple-value-bind (folded ok)
               (opt-fold-binop-value inst lval rval)
              (if ok
                   (progn
                     (remhash dst low-bit-facts)
                     (remhash dst producer-facts)
                      (funcall emit-const dst folded inst))
                   (progn
                    (%opt-clear-derived-facts dst env low-bit-facts producer-facts)
                     (funcall emit inst)))))
          ;; Try algebraic identity simplification
          (t
           (let ((parity-rewrite
                   (and (typep inst '(or vm-num-eq vm-eq))
                        (%opt-rewrite-logand-low-bit-test
                         inst lhs rhs lval lfound rval rfound low-bit-facts)))
                 (simp (opt-simplify-binop inst dst lhs rhs
                                           (if lfound lval :unknown)
                                           (if rfound rval :unknown))))
             (cond
                (parity-rewrite
                 (%opt-clear-derived-facts dst env low-bit-facts producer-facts)
                 (funcall emit parity-rewrite))
                (simp
                 (when (vm-const-p simp)
                   (setf (gethash dst env) (vm-const-value simp)))
                 (cond
                   ((vm-const-p simp)
                    (remhash dst low-bit-facts)
                    (remhash dst producer-facts))
                   ((typep simp 'vm-move)
                    (funcall clear dst)
                    (%opt-copy-derived-fact (vm-src simp) dst low-bit-facts)
                    (%opt-copy-derived-fact (vm-src simp) dst producer-facts))
                   (t
                    (%opt-clear-derived-facts dst env low-bit-facts producer-facts)))
                  (when (vm-const-p simp)
                    (%opt-report :fold "original=~S result=~S"
                                 (instruction->sexp inst)
                                 (instruction->sexp simp)))
                  (funcall emit simp))
                (t
                 (funcall clear dst)
                 (remhash dst producer-facts)
                 (let ((source (%opt-logand-low-bit-source lhs rhs lval lfound rval rfound)))
                   (if (and (typep inst 'vm-logand) source)
                       (setf (gethash dst low-bit-facts) source)
                      (remhash dst low-bit-facts)))
                (funcall emit inst))))))))))

(defun %fold-unary-constant-eligible-p (inst value)
  "Return T when unary INST can be safely folded for constant VALUE.
   Dispatch is data-driven via *opt-unary-fold-eligible-predicates*;
   absent types default to NUMBERP."
  (let ((pred (gethash (type-of inst) *opt-unary-fold-eligible-predicates*)))
    (if pred
        (funcall pred value)
        (numberp value))))

(defun %fold-vm-char (inst env low-bit-facts producer-facts emit emit-const clear)
  "Fold VM-CHAR only when both string and index operands are known constants."
  (declare (ignore clear))
  (let ((dst (vm-dst inst)))
    (multiple-value-bind (string found-string) (gethash (vm-string-reg inst) env)
      (multiple-value-bind (index found-index) (gethash (vm-index inst) env)
        (if (and (gethash 'vm-char *opt-binary-fold-table*)
                 found-string found-index
                 (stringp string)
                 (integerp index)
                 (<= 0 index)
                 (< index (length string)))
            (progn
              (remhash dst low-bit-facts)
              (remhash dst producer-facts)
               (funcall emit-const dst
                        (funcall (gethash 'vm-char *opt-binary-fold-table*) string index)
                        inst))
            (progn
              (%opt-clear-derived-facts dst env low-bit-facts producer-facts)
              (funcall emit inst)))))))

(defun %fold-unary-inst (inst env low-bit-facts producer-facts emit emit-const clear)
  "Unary arithmetic: data-driven via opt-foldable-unary-arith-p → *opt-unary-fold-table*."
  (declare (ignore clear))
  (let* ((dst (vm-dst inst)) (src (vm-src inst))
         (fold-fn (gethash (type-of inst) *opt-unary-fold-table*)))
    (multiple-value-bind (sval found) (gethash src env)
      (if (and found fold-fn
                (%fold-unary-constant-eligible-p inst sval))
           (progn
             (remhash dst low-bit-facts)
             (remhash dst producer-facts)
              (funcall emit-const dst (funcall fold-fn sval) inst))
           (progn
            (%opt-record-producer-fact inst env low-bit-facts producer-facts)
             (funcall emit inst))))))

(defun %fold-type-pred-inst (inst env low-bit-facts producer-facts emit emit-const clear)
  "Type predicates: data-driven via opt-foldable-type-pred-p → *opt-type-pred-fold-table*."
  (declare (ignore clear))
  (let* ((dst (vm-dst inst)) (src (vm-src inst))
         (pred-fn (gethash (type-of inst) *opt-type-pred-fold-table*)))
    (multiple-value-bind (sval found) (gethash src env)
      (if (and found pred-fn)
           (progn
             (remhash dst low-bit-facts)
             (remhash dst producer-facts)
              (funcall emit-const dst (if (funcall pred-fn sval) 1 0) inst))
          (let ((simp (opt-simplify-type-pred-with-producer inst producer-facts)))
            (if simp
                (progn
                  (remhash dst low-bit-facts)
                  (remhash dst producer-facts)
                  (setf (gethash dst env) (vm-const-value simp))
                  (%opt-report :fold "original=~S result=~S"
                               (instruction->sexp inst)
                               (instruction->sexp simp))
                  (funcall emit simp))
                (progn
                  (%opt-clear-derived-facts dst env low-bit-facts producer-facts)
                  (funcall emit inst))))))))

(defun %fold-vm-jump-zero (inst env emit)
  "Constant branch folding: known-false → unconditional jump; known-true → drop."
  (multiple-value-bind (val found) (gethash (vm-reg inst) env)
    (if found
        (if (opt-falsep val)
            (funcall emit (make-vm-jump :label (vm-label-name inst)))
            nil) ; condition is always true → branch never taken → drop
        (funcall emit inst))))

(defun %fold-default-inst (inst env low-bit-facts producer-facts emit clear)
  "Default: invalidate any written destination register, then emit."
  (declare (ignore clear))
  (let ((dst (opt-inst-dst inst)))
    (when dst
      (%opt-record-producer-fact inst env low-bit-facts producer-facts)))
  (funcall emit inst))

(defun opt-pass-fold (instructions)
  "Forward pass: constant folding, algebraic simplification, constant branch elimination."
  (let ((env (make-hash-table :test #'eq)) ; reg → known constant value
        (low-bit-facts (make-hash-table :test #'eq)) ; reg → original src for (logand src 1)
        (producer-facts (make-hash-table :test #'eq)) ; reg → known producer instruction type
        (target-labels (%opt-branch-target-labels instructions))
        (result nil))
    (flet ((emit (inst) (push inst result))
           (emit-const (dst val &optional original-inst)
              (setf (gethash dst env) val)
              (remhash dst producer-facts)
              (let ((const-inst (make-vm-const :dst dst :value val)))
                (when original-inst
                  (%opt-report :fold "original=~S result=~S"
                               (instruction->sexp original-inst)
                               (instruction->sexp const-inst)))
                 (push const-inst result)))
             (clear (reg)
               (remhash reg env)
               (remhash reg producer-facts)))
      (dolist (inst instructions)
        (typecase inst
          (vm-label    (%fold-vm-label      inst env low-bit-facts producer-facts #'emit target-labels))
          (vm-const    (%fold-vm-const      inst env low-bit-facts producer-facts #'emit))
          (vm-move     (%fold-vm-move       inst env low-bit-facts producer-facts #'emit #'emit-const #'clear))
          (vm-jump-zero (%fold-vm-jump-zero inst env #'emit))
          (t
            (cond
                ((typep inst 'vm-char)             (%fold-vm-char       inst env low-bit-facts producer-facts #'emit #'emit-const #'clear))
                ((opt-binary-lhs-rhs-p inst)      (%fold-binary-inst    inst env low-bit-facts producer-facts #'emit #'emit-const #'clear))
               ((opt-foldable-unary-arith-p inst) (%fold-unary-inst     inst env low-bit-facts producer-facts #'emit #'emit-const #'clear))
               ((opt-foldable-type-pred-p inst)   (%fold-type-pred-inst inst env low-bit-facts producer-facts #'emit #'emit-const #'clear))
               (t                                 (%fold-default-inst   inst env low-bit-facts producer-facts #'emit #'clear))))))
    (nreverse result))))

;;;; ─── Size-oriented passes: function outlining and safepoint polling ─────

(defparameter *opt-outlining-min-length* 3
  "Minimum duplicate straight-line instruction sequence length for outlining.")

(defparameter *opt-outlined-label-prefix* "__clcc_outlined_"
  "Prefix used for generated outlined helper labels.")

(defparameter *opt-safepoint-label* "__clcc_safepoint_poll"
  "Generated no-op safepoint polling helper label.")

(defparameter *opt-safepoint-prefix* "__clcc_safepoint_poll_"
  "Prefix used for generated safepoint poll temporary labels/registers.")

(defparameter *opt-safepoint-flag-name* '*clcc-safepoint-request*
  "Global flag read by inserted safepoint polls.")

(defun %opt-generated-label-present-p (instructions prefix)
  "Return T when INSTRUCTIONS already contain a generated label with PREFIX."
  (some (lambda (inst)
          (and (typep inst 'vm-label)
               (let ((name (vm-name inst)))
                 (and (stringp name)
                      (<= (length prefix) (length name))
                      (string= prefix name :end2 (length prefix))))))
        instructions))

;;; %opt-max-register-index and %opt-fresh-register-generator are defined in
;;; optimizer-size.lisp (loaded before this file). No local copies needed.

(defun %opt-control-or-label-p (inst)
  "Return T when INST should not be part of a straight-line outlined sequence."
  (typep inst 'opt-control-or-label))

(defun %opt-outlinable-subseq-p (seq)
  "Return T when SEQ can be outlined as a nullary helper returning one value."
  (and (>= (length seq) *opt-outlining-min-length*)
       (every (lambda (inst)
                (and (not (%opt-control-or-label-p inst))
                     (opt-inst-pure-p inst)))
              seq)
       (let* ((last-inst (car (last seq)))
              (result-dst (opt-inst-dst last-inst))
              (defs (remove nil (mapcar #'opt-inst-dst seq))))
         (and result-dst
              (loop for inst in seq
                    for pos from 0
                    always (loop for reg in (opt-inst-read-regs inst)
                                 always (member reg (subseq defs 0 pos) :test #'eq)))
              (loop for def in (butlast defs)
                    always (loop for inst in (cdr (member def seq :key #'opt-inst-dst :test #'eq))
                                 thereis (member def (opt-inst-read-regs inst) :test #'eq)))))))

(defun %opt-subseq-key (seq)
  "Return a structural duplicate key for SEQ."
  (mapcar #'instruction->sexp seq))

(defun %opt-find-outline-candidate (instructions)
  "Find one duplicate outlinable subsequence. Returns (values start length seq)."
  (let ((seen (make-hash-table :test #'equal))
        (n (length instructions)))
    (loop for len from (min 8 n) downto *opt-outlining-min-length*
          do (clrhash seen)
             (loop for start from 0 to (- n len)
                   for seq = (subseq instructions start (+ start len))
                   when (%opt-outlinable-subseq-p seq)
                     do (let ((key (%opt-subseq-key seq)))
                          (multiple-value-bind (first-start found-p) (gethash key seen)
                            (if found-p
                                (return-from %opt-find-outline-candidate
                                  (values first-start len seq))
                                (setf (gethash key seen) start))))))))

(defun %opt-replace-outline-occurrences (instructions seq label fresh-reg)
  "Replace all occurrences of SEQ with calls to LABEL."
  (let* ((len (length seq))
         (key (%opt-subseq-key seq))
         (result nil)
         (i 0)
         (n (length instructions)))
    (loop while (< i n)
          do (if (and (<= (+ i len) n)
                      (equal key (%opt-subseq-key (subseq instructions i (+ i len)))))
                 (let ((func-reg (funcall fresh-reg))
                       (dst (opt-inst-dst (nth (1- len) seq))))
                   (push (make-vm-func-ref :dst func-reg :label label) result)
                   (push (make-vm-call :dst dst :func func-reg :args nil) result)
                   (incf i len))
                 (progn
                   (push (nth i instructions) result)
                   (incf i))))
    (nreverse result)))

(defun opt-pass-function-outlining (instructions)
  "FR-294: outline duplicate pure straight-line sequences into shared helpers."
  (if (%opt-generated-label-present-p instructions *opt-outlined-label-prefix*)
      instructions
      (multiple-value-bind (start len seq) (%opt-find-outline-candidate instructions)
        (declare (ignore start len))
        (if seq
            (let* ((label (format nil "~A~D" *opt-outlined-label-prefix* (sxhash (%opt-subseq-key seq))))
                   (fresh-reg (%opt-fresh-register-generator instructions))
                   (rewritten (%opt-replace-outline-occurrences instructions seq label fresh-reg)))
              (append rewritten
                      (list (make-vm-label :name label))
                      seq
                      (list (make-vm-ret :reg (opt-inst-dst (car (last seq)))))))
            instructions))))

(defun %opt-backedge-targets (instructions)
  "Return an EQUAL table of labels that are loop back-edge targets."
  (let ((positions (make-hash-table :test #'equal))
        (targets (make-hash-table :test #'equal)))
    (loop for inst in instructions
          for i from 0
          when (typep inst 'vm-label)
            do (setf (gethash (vm-name inst) positions) i))
    (loop for inst in instructions
          for i from 0
          when (typep inst '(or vm-jump vm-jump-zero))
            do (let ((target (gethash (vm-label-name inst) positions)))
                 (when (and target (< target i))
                   (setf (gethash (vm-label-name inst) targets) t))))
    targets))

(defun %opt-safepoint-poll (fresh-reg skip-label)
  "Build an actual safepoint flag check that calls the helper when requested."
  (let ((flag-reg (funcall fresh-reg))
        (func-reg (funcall fresh-reg))
        (dst-reg (funcall fresh-reg)))
    (list (make-vm-get-global :dst flag-reg :name *opt-safepoint-flag-name*)
          (make-vm-jump-zero :reg flag-reg :label skip-label)
          (make-vm-func-ref :dst func-reg :label *opt-safepoint-label*)
          (make-vm-call :dst dst-reg :func func-reg :args (list flag-reg))
          (make-vm-label :name skip-label))))

(defun opt-pass-safepoint-polling (instructions)
  "FR-233: insert safepoint flag checks at function entries and loop backedges."
  (if (%opt-generated-label-present-p instructions *opt-safepoint-prefix*)
      instructions
      (let ((backedges (%opt-backedge-targets instructions))
            (fresh-reg (%opt-fresh-register-generator instructions))
            (result nil)
            (counter 0)
            (entry-seen-p nil))
        (dolist (inst instructions)
          (push inst result)
          (when (and (typep inst 'vm-label)
                     (or (not entry-seen-p)
                         (gethash (vm-name inst) backedges)))
            (let ((skip-label (format nil "~A~D" *opt-safepoint-prefix* counter)))
              (push (make-vm-label :name (format nil "~Asite_~D" *opt-safepoint-prefix* counter)) result)
              (dolist (poll (reverse (%opt-safepoint-poll fresh-reg skip-label)))
                (push poll result)))
            (incf counter)
            (setf entry-seen-p t)))
        (let ((ret-reg (funcall fresh-reg)))
          (append (nreverse result)
                   (list (make-vm-label :name *opt-safepoint-label*)
                         (make-vm-const :dst ret-reg :value 0)
                          (make-vm-ret :reg ret-reg)))))))

;;;; ─── FR-226: Auto-Vectorization ─────────────────────────────────────────

(defparameter *opt-simd-lane-count* 4
  "Default SIMD lane count used by conservative strip-mined vector loops.")

(defparameter *opt-autovec-label-prefix* "__clcc_autovec_"
  "Prefix for generated auto-vectorization remainder labels.")

(defun %opt-autovec-idempotent-p (instructions)
  "Return T when auto-vectorization markers are already present."
  (some (lambda (inst)
          (or (typep inst 'vm-simd-vector-op)
              (and (typep inst 'vm-label)
                   (let ((name (vm-name inst)))
                     (and (stringp name)
                          (<= (length *opt-autovec-label-prefix*) (length name))
                          (string= *opt-autovec-label-prefix* name
                                   :end2 (length *opt-autovec-label-prefix*)))))))
        instructions))

(defun %opt-autovec-cmp-inst-p (inst)
  "T when INST is a supported counted-loop comparison."
  (typep inst 'opt-autovec-cmp))

(defun %opt-autovec-clone-cmp (inst dst lhs rhs)
  "Clone supported comparison INST with new DST/LHS/RHS registers.
   Dispatch is data-driven via *opt-autovec-cmp-clone-table*."
  (let ((ctor (gethash (type-of inst) *opt-autovec-cmp-clone-table*)))
    (when ctor (funcall ctor dst lhs rhs))))

(defun %opt-autovec-op-kind (inst)
  "Return a backend-neutral SIMD op keyword for scalar binary INST, or NIL.
   Dispatch is data-driven via *opt-autovec-scalar-to-simd-op*."
  (gethash (type-of inst) *opt-autovec-scalar-to-simd-op*))

(defun %opt-autovec-array-loads (body iv-reg)
  "Return a table mapping load destination registers to source array registers."
  (let ((loads (make-hash-table :test #'eq)))
    (dolist (inst body loads)
      (when (and (typep inst 'vm-aref)
                 (eq (vm-index-reg inst) iv-reg))
        (setf (gethash (vm-dst inst) loads) (vm-array-reg inst))))))

(defun %opt-autovec-find-map-op (body iv-reg)
  "Detect independent array map scalar ops in BODY and return SIMD markers.

Accepted scalar shape inside one counted loop iteration:
  (aref t1 a i) (aref t2 b i) (<binop> v t1 t2) (aset c i v)
Multiple such chains are allowed as long as they use the loop IV only for array
indices and do not carry values between iterations."
  (let ((loads (%opt-autovec-array-loads body iv-reg))
        (producers (make-hash-table :test #'eq))
        (simd nil))
    (dolist (inst body)
      (let ((op (%opt-autovec-op-kind inst)))
        (when op
          (multiple-value-bind (lhs-array lhs-ok) (gethash (vm-lhs inst) loads)
            (multiple-value-bind (rhs-array rhs-ok) (gethash (vm-rhs inst) loads)
              (when (and lhs-ok rhs-ok)
                (setf (gethash (vm-dst inst) producers)
                      (list op lhs-array rhs-array))))))))
    (dolist (inst body)
      (when (and (typep inst 'vm-aset)
                 (eq (vm-index-reg inst) iv-reg))
        (destructuring-bind (&optional op lhs-array rhs-array)
            (gethash (vm-val-reg inst) producers)
          (when op
            (push (make-vm-simd-vector-op :op op
                                          :dst-array (vm-array-reg inst)
                                          :lhs-array lhs-array
                                          :rhs-array rhs-array
                                          :index-reg iv-reg
                                          :lanes *opt-simd-lane-count*)
                  simd)))))
    (nreverse simd)))

(defun %opt-autovec-vector-limit (init limit step lanes cmp-inst)
  "Return strip-mined vector-limit value for compile-time counted loops."
  (let ((trip (and init limit step
                   (= step 1)
                   (%opt-loop-unroll-trip-count cmp-inst init limit step))))
    (when (and trip (>= trip lanes))
      (+ init (* lanes (floor trip lanes))))))

(defun %opt-autovec-emit-vector-loop (header cmp-inst jz-inst body step-inst exit-label
                                             vec-limit-reg lane-reg remainder-label simd-ops result)
  "Emit vector loop plus scalar remainder loop into reversed RESULT."
  (push header result)
  (push (%opt-autovec-clone-cmp cmp-inst (vm-dst cmp-inst) (vm-lhs cmp-inst) vec-limit-reg) result)
  (push (make-vm-jump-zero :reg (vm-reg jz-inst) :label remainder-label) result)
  (dolist (op simd-ops) (push op result))
  (push (make-vm-add :dst (vm-dst step-inst) :lhs (vm-lhs step-inst) :rhs lane-reg) result)
  (push (make-vm-jump :label (vm-name header)) result)
  (push (make-vm-label :name remainder-label) result)
  (push cmp-inst result)
  (push (make-vm-jump-zero :reg (vm-reg jz-inst) :label exit-label) result)
  (dolist (inst body) (push inst result))
  (push (make-vm-jump :label remainder-label) result)
  result)

(defun %opt-autovec-try-vectorize-at (vec i n fresh-reg result serial)
  "Attempt to vectorize a counted loop starting at position I in VEC.

Returns (values new-result new-i new-serial vectorized-p).  When the loop at
position I matches the canonical autovec shape and has profitable SIMD ops,
new-result contains the rewritten instructions and vectorized-p is T.
Otherwise new-result is unchanged, new-i is (1+ I), and vectorized-p is NIL."
  (let* ((cur (aref vec i))
         (header cur)
         (cmp-inst (aref vec (+ i 1)))
         (jz-inst  (aref vec (+ i 2)))
         (header-name (vm-name header)))
    (flet ((fail ()
             ;; Emit the current instruction as-is and advance by one position.
             (push cur result)
             (values result (1+ i) serial nil)))
      (if (and (%opt-autovec-cmp-inst-p cmp-inst)
               (typep jz-inst 'vm-jump-zero)
               (eq (vm-reg jz-inst) (vm-dst cmp-inst)))
          (let* ((exit-name (vm-label-name jz-inst))
                 (exit-pos  (cfg-find-label-position vec n exit-name))
                 (back-pos  (and exit-pos (1- exit-pos)))
                 (back-inst (and back-pos (>= back-pos 0) (aref vec back-pos))))
            (if (and exit-pos
                     (> exit-pos (+ i 4))
                     (typep back-inst 'vm-jump)
                     (equal (vm-label-name back-inst) header-name)
                     (not (%opt-has-external-jump-to-label-p vec header-name i exit-pos)))
                (let* ((body      (loop for j from (+ i 3) below back-pos collect (aref vec j)))
                       (step-inst (car (last body)))
                       (const-env (%opt-build-const-env-up-to vec i)))
                  (if (and (typep step-inst 'vm-add)
                           (eq (vm-dst step-inst) (vm-lhs step-inst))
                           (eq (vm-dst step-inst) (vm-lhs cmp-inst)))
                      (let* ((iv-reg  (vm-lhs cmp-inst))
                             (lim-reg (vm-rhs cmp-inst))
                             (step-reg (vm-rhs step-inst))
                             (init    (gethash iv-reg const-env))
                             (limit   (gethash lim-reg const-env))
                             (step    (gethash step-reg const-env))
                             (vector-limit (%opt-autovec-vector-limit
                                            init limit step *opt-simd-lane-count* cmp-inst))
                             (simd-ops (%opt-autovec-find-map-op (butlast body) iv-reg)))
                        (if (and vector-limit simd-ops)
                            (let ((vec-limit-reg    (funcall fresh-reg))
                                  (lane-reg         (funcall fresh-reg))
                                  (remainder-label  (format nil "~A~D" *opt-autovec-label-prefix* serial)))
                              (push (make-vm-const :dst vec-limit-reg :value vector-limit) result)
                              (push (make-vm-const :dst lane-reg :value *opt-simd-lane-count*) result)
                              (setf result (%opt-autovec-emit-vector-loop
                                            header cmp-inst jz-inst body step-inst exit-name
                                            vec-limit-reg lane-reg remainder-label simd-ops result))
                              (push (aref vec exit-pos) result)
                              (values result (1+ exit-pos) (1+ serial) t))
                            (fail)))
                      (fail)))
                (fail)))
          (fail)))))

(defun opt-pass-auto-vectorization (instructions)
  "FR-226: vectorize independent scalar array ops in counted loops.

The pass recognizes a conservative one-dimensional array-map loop, emits a SIMD
vector loop strip-mined by `*opt-simd-lane-count*`, and retains a scalar
remainder loop for tail iterations.  Dynamic trip-count loops are left unchanged;
compile-time trip counts make the generated vector-limit constant explicit."
  (if (%opt-autovec-idempotent-p instructions)
      instructions
      (let* ((vec       (coerce instructions 'vector))
             (n         (length vec))
             (fresh-reg (%opt-fresh-register-generator instructions))
             (result    nil)
             (i         0)
             (changed   nil)
             (serial    0))
        (loop while (< i n)
              do (let ((cur (aref vec i)))
                   (if (and (typep cur 'vm-label) (<= (+ i 5) (1- n)))
                       (multiple-value-bind (new-result new-i new-serial vectorized-p)
                           (%opt-autovec-try-vectorize-at vec i n fresh-reg result serial)
                         (setf result new-result
                               i      new-i
                               serial new-serial)
                         (when vectorized-p (setf changed t)))
                       (progn (push cur result) (incf i)))))
        (if changed (nreverse result) instructions))))

;;;; ─── FR-227: SLP Vectorization ──────────────────────────────────────────

(defun %opt-slp-idempotent-p (instructions)
  "Return T when INSTRUCTIONS already contain SIMD vector markers."
  (some (lambda (inst) (typep inst 'vm-simd-vector-op)) instructions))

(defun %opt-slp-op-kind (inst)
  "Return the SIMD op keyword for scalar SLP arithmetic INST, or NIL.
   Delegates entirely to %opt-autovec-op-kind which covers vm-logand/logior/logxor
   via *opt-autovec-scalar-to-simd-op*."
  (%opt-autovec-op-kind inst))

(defun %opt-slp-index-descriptor (reg values offsets)
  "Return a normalized descriptor for index register REG.

The descriptor is `(BASE . OFFSET)`.  BASE is NIL for literal constants, or the
base index register for an affine `(+ base constant)` value found in the block."
  (multiple-value-bind (value value-p) (gethash reg values)
    (if value-p
        (cons nil value)
        (multiple-value-bind (desc offset-p) (gethash reg offsets)
          (if offset-p desc (cons reg 0))))))

(defun %opt-slp-record-index-fact (inst values offsets)
  "Record simple integer/affine index facts produced by INST."
  (cond
    ((typep inst 'vm-const)
     (if (integerp (vm-value inst))
         (setf (gethash (vm-dst inst) values) (vm-value inst))
         (remhash (vm-dst inst) values))
     (remhash (vm-dst inst) offsets))
    ((typep inst 'vm-add)
     (let ((dst (vm-dst inst))
           (lhs (vm-lhs inst))
           (rhs (vm-rhs inst)))
       (multiple-value-bind (lhs-value lhs-const-p) (gethash lhs values)
         (multiple-value-bind (rhs-value rhs-const-p) (gethash rhs values)
           (cond
             ((and lhs-const-p rhs-const-p)
              (setf (gethash dst values) (+ lhs-value rhs-value))
              (remhash dst offsets))
             (rhs-const-p
              (remhash dst values)
              (setf (gethash dst offsets)
                    (let ((base (%opt-slp-index-descriptor lhs values offsets)))
                      (cons (car base) (+ (cdr base) rhs-value)))))
             (lhs-const-p
              (remhash dst values)
              (setf (gethash dst offsets)
                    (let ((base (%opt-slp-index-descriptor rhs values offsets)))
                      (cons (car base) (+ (cdr base) lhs-value)))))
             (t
              (remhash dst values)
              (remhash dst offsets)))))))
    (t
     (let ((dst (opt-inst-dst inst)))
       (when dst
         (remhash dst values)
         (remhash dst offsets))))))

(defun %opt-slp-analyze-block (insts)
  "Return per-block producer/index facts used by SLP matching."
  (let ((loads (make-hash-table :test #'eq))
        (ops (make-hash-table :test #'eq))
        (indexes (make-hash-table :test #'eq))
        (values (make-hash-table :test #'eq))
        (offsets (make-hash-table :test #'eq))
        (positions (make-hash-table :test #'eq))
        (reads (make-hash-table :test #'eq)))
    (loop for inst in insts
          for pos from 0
          do (setf (gethash inst positions) pos)
             (dolist (reg (opt-inst-read-regs inst))
               (incf (gethash reg reads 0)))
             (%opt-slp-record-index-fact inst values offsets)
             (when (typep inst '(or vm-aref vm-aset))
               (setf (gethash inst indexes)
                     (%opt-slp-index-descriptor (vm-index-reg inst) values offsets)))
             (cond
               ((typep inst 'vm-aref)
                (setf (gethash (vm-dst inst) loads)
                      (list :inst inst
                            :array (vm-array-reg inst)
                            :index-reg (vm-index-reg inst)
                            :index (gethash inst indexes))))
               ((%opt-slp-op-kind inst)
                (setf (gethash (vm-dst inst) ops)
                      (list :inst inst
                            :op (%opt-slp-op-kind inst)
                            :lhs (vm-lhs inst)
                            :rhs (vm-rhs inst))))))
    (values loads ops indexes positions reads)))

(defun %opt-slp-store-lane (store loads ops indexes reads)
  "Return a lane plist for STORE when it is a scalar array-map lane."
  (when (typep store 'vm-aset)
    (let* ((producer (gethash (vm-val-reg store) ops))
           (lhs-load (and producer (gethash (getf producer :lhs) loads)))
           (rhs-load (and producer (gethash (getf producer :rhs) loads)))
           (store-index (gethash store indexes)))
      (when (and producer lhs-load rhs-load
                 (= (gethash (getf producer :lhs) reads 0) 1)
                 (= (gethash (getf producer :rhs) reads 0) 1)
                 (= (gethash (vm-val-reg store) reads 0) 1)
                 (equal (getf lhs-load :index) (getf rhs-load :index))
                 (equal (getf lhs-load :index) store-index))
        (list :store store
              :op-inst (getf producer :inst)
              :lhs-load (getf lhs-load :inst)
              :rhs-load (getf rhs-load :inst)
              :op (getf producer :op)
              :dst-array (vm-array-reg store)
              :lhs-array (getf lhs-load :array)
              :rhs-array (getf rhs-load :array)
              :index-reg (getf lhs-load :index-reg)
              :index (getf lhs-load :index))))))

(defun %opt-slp-lane-key (lane)
  "Return the isomorphism key for LANE, ignoring its lane offset."
  (list (getf lane :op)
        (getf lane :dst-array)
        (getf lane :lhs-array)
        (getf lane :rhs-array)
        (car (getf lane :index))))

(defun %opt-slp-lane-offset (lane)
  "Return the scalar lane offset recorded in LANE."
  (cdr (getf lane :index)))

(defun %opt-slp-supported-lane-count-p (lanes)
  "Return T when LANES is supported by the existing native SIMD emitters."
  (member lanes '(4 8) :test #'=))

(defun %opt-slp-group-contiguous-p (lanes)
  "Return T when LANES form a single contiguous superword."
  (let ((sorted (sort (copy-list lanes) #'< :key #'%opt-slp-lane-offset)))
    (loop for lane in sorted
          for expected from (%opt-slp-lane-offset (first sorted))
          always (= (%opt-slp-lane-offset lane) expected))))

(defun %opt-slp-group-span-safe-p (insts group positions)
  "Return T when replacing GROUP at its first instruction does not cross a barrier."
  (let* ((selected (make-hash-table :test #'eq))
         (indices nil))
    (dolist (lane group)
      (dolist (inst (list (getf lane :lhs-load)
                          (getf lane :rhs-load)
                          (getf lane :op-inst)
                          (getf lane :store)))
        (setf (gethash inst selected) t)
        (push (gethash inst positions) indices)))
    (let ((start (apply #'min indices))
          (end (apply #'max indices)))
      (loop for pos from start to end
            for inst = (nth pos insts)
            always (or (gethash inst selected)
                       (member (vm-inst-effect-kind inst) '(:pure :read-only) :test #'eq))))))

(defun %opt-slp-simd-inst (group)
  "Build a vm-simd-vector-op for GROUP."
  (let* ((lanes (sort (copy-list group) #'< :key #'%opt-slp-lane-offset))
         (first-lane (first lanes)))
    (make-vm-simd-vector-op :op (getf first-lane :op)
                            :dst-array (getf first-lane :dst-array)
                            :lhs-array (getf first-lane :lhs-array)
                            :rhs-array (getf first-lane :rhs-array)
                            :index-reg (getf first-lane :index-reg)
                            :lanes (length lanes)
                            :element-type :i32)))

(defun %opt-slp-find-groups (insts)
  "Find independent straight-line SLP groups in INSTS."
  (multiple-value-bind (loads ops indexes positions reads) (%opt-slp-analyze-block insts)
    (let ((buckets (make-hash-table :test #'equal))
          (groups nil))
      (dolist (inst insts)
        (let ((lane (%opt-slp-store-lane inst loads ops indexes reads)))
          (when lane
            (push lane (gethash (%opt-slp-lane-key lane) buckets)))))
      (loop for lanes being the hash-values of buckets
            do (let* ((ordered (sort (copy-list lanes) #'< :key #'%opt-slp-lane-offset))
                      (want *opt-simd-lane-count*))
                 (loop while (>= (length ordered) want)
                       for candidate = (subseq ordered 0 want)
                       do (if (and (%opt-slp-supported-lane-count-p want)
                                   (%opt-slp-group-contiguous-p candidate)
                                   (%opt-slp-group-span-safe-p insts candidate positions))
                              (progn
                                (push candidate groups)
                                (setf ordered (nthcdr want ordered)))
                              (setf ordered (rest ordered))))))
      (nreverse groups))))

(defun %opt-slp-rewrite-block (insts)
  "Rewrite one basic block's scalar superwords into SIMD vector operations."
  (let ((groups (%opt-slp-find-groups insts)))
    (if (null groups)
        (values insts nil)
        (let ((remove (make-hash-table :test #'eq))
              (insert (make-hash-table :test #'eq)))
          (dolist (group groups)
            (let* ((simd (%opt-slp-simd-inst group))
                   (members (loop for lane in group append
                                  (list (getf lane :lhs-load)
                                        (getf lane :rhs-load)
                                        (getf lane :op-inst)
                                        (getf lane :store))))
                   (anchor (reduce (lambda (a b)
                                     (if (< (position a insts :test #'eq)
                                            (position b insts :test #'eq))
                                         a b))
                                   members)))
              (dolist (inst members) (setf (gethash inst remove) t))
              (setf (gethash anchor insert) simd)))
          (values (loop for inst in insts
                        append (cond
                                 ((gethash inst insert)
                                  (list (gethash inst insert)))
                                 ((gethash inst remove) nil)
                                 (t (list inst))))
                  t)))))

(defun opt-pass-slp-vectorize (instructions)
  "FR-227: pack straight-line scalar array-map lanes into SIMD vector ops.

The pass builds a CFG, scans each basic block for isomorphic independent scalar
chains of adjacent `vm-aref` → arithmetic/bitwise op → `vm-aset` lanes, and
replaces each full superword with one existing `vm-simd-vector-op` marker.  It is
conservative and idempotent: existing SIMD markers cause the input to be left
unchanged, preventing repeated packing on subsequent optimizer iterations."
  (if (%opt-slp-idempotent-p instructions)
      instructions
      (let ((cfg (cfg-build instructions))
            (changed nil))
        (loop for block across (cfg-blocks cfg)
              do (multiple-value-bind (new-insts block-changed)
                     (%opt-slp-rewrite-block (bb-instructions block))
                   (when block-changed
                     (setf (bb-instructions block) new-insts
                           changed t))))
        (if changed (cfg-flatten cfg) instructions))))

;;;; ─── Branch Prediction Weight Analysis ─────────────────────────────────

(defparameter *opt-cold-block-instruction-types*
  '(vm-signal vm-error-instruction vm-cerror vm-warn)
  "Instruction types that make their containing basic block cold/unlikely.")

(defstruct (vm-branch-weighted-jump-zero
            (:include vm-jump-zero)
            (:constructor make-vm-branch-weighted-jump-zero
                (&key reg label branch-weight)))
  "A vm-jump-zero carrying optimizer-only branch prediction metadata."
  (branch-weight nil))

(defun opt-branch-weight (inst)
  "Return branch prediction metadata for INST, or NIL when it is unannotated."
  (when (typep inst 'vm-branch-weighted-jump-zero)
    (vm-branch-weighted-jump-zero-branch-weight inst)))

(defun %opt-cold-block-inst-p (inst)
  "Return T when INST identifies an error/diagnostic cold path."
  (member (type-of inst) *opt-cold-block-instruction-types* :test #'eq))

(defun %opt-label-positions (instructions)
  "Return a label-name → instruction-index table for INSTRUCTIONS."
  (let ((positions (make-hash-table :test #'equal)))
    (loop for inst in instructions
          for i from 0
          when (typep inst 'vm-label)
            do (setf (gethash (vm-name inst) positions) i))
    positions))

(defun %opt-target-block-has-terminal-p (vec start-index predicate)
  "Return T when target block starting at START-INDEX contains a matching terminator."
  (when start-index
    (loop for i from start-index below (length vec)
          for inst = (aref vec i)
          do (cond
               ((and (> i start-index) (typep inst 'vm-label))
                (return nil))
               ((funcall predicate inst)
                (return t)))
          finally (return nil))))

(defun %opt-cold-labels (instructions)
  "Return a table of labels whose basic blocks contain cold instructions."
  (let ((cold-labels (make-hash-table :test #'equal))
        (current-label nil)
        (current-cold-p nil))
    (labels ((flush-current ()
               (when (and current-label current-cold-p)
                 (setf (gethash current-label cold-labels) t))))
      (dolist (inst instructions cold-labels)
        (cond
          ((typep inst 'vm-label)
           (flush-current)
           (setf current-label (vm-name inst)
                 current-cold-p nil))
          ((%opt-cold-block-inst-p inst)
           (setf current-cold-p t))))
      (flush-current))))

(defun %opt-branch-weight-for-jump-zero (inst index vec label-positions cold-labels)
  "Return static branch metadata for INST at INDEX.

Values:
  :UNLIKELY     taken target is a cold diagnostics/error block
  :LIKELY-TAKEN taken target is a loop back-edge
  :NOT-TAKEN    taken target is a return/exit edge"
  (let* ((target-label (vm-label-name inst))
         (target-index (gethash target-label label-positions)))
    (cond
      ((gethash target-label cold-labels)
       :unlikely)
      ((and target-index (< target-index index))
       :likely-taken)
      ((%opt-target-block-has-terminal-p
        vec target-index
        (lambda (target-inst)
          (or (typep target-inst 'vm-ret)
              (typep target-inst 'vm-halt))))
       :not-taken)
      (t nil))))

(defun %opt-annotate-jump-zero (inst weight)
  "Return INST with BRANCH-WEIGHT metadata when WEIGHT is non-NIL."
  (if weight
      (make-vm-branch-weighted-jump-zero :reg (vm-reg inst)
                                         :label (vm-label-name inst)
                                         :branch-weight weight)
      inst))

(defun opt-analyze-branch-weights (instructions)
  "Annotate conditional branches with static branch-prediction metadata.

The analysis is intentionally non-transforming: instruction order and control-flow
targets are preserved.  It marks branches to blocks containing VM condition/error
signaling instructions as cold/unlikely, loop back-edge branches as likely taken,
and branches directly targeting return/exit blocks as not-taken."
  (let* ((vec (coerce instructions 'vector))
         (label-positions (%opt-label-positions instructions))
         (cold-labels (%opt-cold-labels instructions)))
    (loop for inst in instructions
          for i from 0
          collect (if (typep inst 'vm-jump-zero)
                      (%opt-annotate-jump-zero
                       inst
                       (%opt-branch-weight-for-jump-zero
                        inst i vec label-positions cold-labels))
                       inst))))

;;; ─── Software Pipelining (FR-200) ────────────────────────────────────────

(defparameter *opt-software-pipeline-label-prefix* "__clcc_swp_"
  "Prefix for labels generated by FR-200 software pipelining.")

(defun %opt-swp-loop-cmp-p (inst)
  "Return T when INST is a comparison supported by software pipelining."
  (typep inst '(or vm-lt vm-le vm-gt vm-ge vm-eq vm-num-eq)))

(defun %opt-swp-generated-p (instructions)
  "Return T when INSTRUCTIONS already contain FR-200 generated labels."
  (%opt-generated-label-present-p instructions *opt-software-pipeline-label-prefix*))

(defun %opt-swp-parse-loop-at (vec i)
  "Parse the canonical counted-loop shape used by loop optimizers.

Expected shape:
  Lh: cmp/jz body step jump Lh Lexit:
The returned value is a plist to avoid depending on later roadmap structs."
  (when (and (< (+ i 5) (length vec))
             (typep (aref vec i) 'vm-label)
             (%opt-swp-loop-cmp-p (aref vec (1+ i)))
             (typep (aref vec (+ i 2)) 'vm-jump-zero))
    (let* ((header (aref vec i))
           (cmp (aref vec (1+ i)))
           (jz (aref vec (+ i 2)))
           (header-name (vm-name header))
           (exit-name (vm-label-name jz))
           (exit-index (cfg-find-label-position vec (length vec) exit-name)))
      (when (and exit-index (> exit-index (+ i 4)))
        (let* ((back-index (1- exit-index))
               (back (aref vec back-index))
               (body (loop for k from (+ i 3) below back-index collect (aref vec k)))
               (step (car (last body))))
          (when (and (typep back 'vm-jump)
                     (equal (vm-label-name back) header-name)
                     (typep step 'vm-add)
                     (eq (vm-dst step) (vm-lhs step))
                     (eq (vm-dst step) (vm-lhs cmp))
                     (eq (vm-reg jz) (vm-dst cmp))
                     (not (%opt-has-external-jump-to-label-p vec header-name i exit-index)))
            (list :head-index i
                  :cmp-index (1+ i)
                  :jz-index (+ i 2)
                  :back-index back-index
                  :exit-index exit-index
                  :head-label header-name
                  :exit-label exit-name
                  :body body)))))))

(defun %opt-swp-safe-core-p (core)
  "Return T when CORE can be modulo-scheduled without semantic risk."
  (and (>= (length core) 2)
       (every (lambda (inst)
                (and (opt-inst-cse-eligible-p inst)
                     (member (vm-inst-effect-kind inst) '(:pure :read-only) :test #'eq)
                     (not (%opt-control-or-label-p inst))))
              core)))

(defun opt-modulo-schedule-loop-body (body &key (issue-width 1))
  "Build the DDG for BODY, compute MII, and return a modulo schedule.

Values are (scheduled-core ddg mii).  BODY may include the induction update as
its final instruction; the scheduler excludes that update and lets callers emit
it at the end of the kernel."
  (let ((core (butlast body)))
    (if (not (%opt-swp-safe-core-p core))
        (values core (cfg-build-ddg core :latency-fn #'%opt-inst-latency) 1)
        (let* ((ddg (cfg-build-ddg core :latency-fn #'%opt-inst-latency))
               (mii (cfg-compute-mii ddg :issue-width issue-width))
               (scheduled
                 (sort (loop for node across ddg
                             collect (list :inst (opt-ddg-node-inst node)
                                           :index (opt-ddg-node-index node)
                                           :slot (mod (opt-ddg-node-index node) mii)
                                           :latency (opt-ddg-node-latency node)))
                       (lambda (a b)
                         (or (< (getf a :slot) (getf b :slot))
                             (and (= (getf a :slot) (getf b :slot))
                                  (> (getf a :latency) (getf b :latency)))
                             (and (= (getf a :slot) (getf b :slot))
                                  (= (getf a :latency) (getf b :latency))
                                  (< (getf a :index) (getf b :index))))))))
          (values (mapcar (lambda (entry) (getf entry :inst)) scheduled) ddg mii)))))

(defun %opt-swp-loop-sequence (vec lp)
  "Return original instruction sequence for LP from VEC."
  (loop for k from (getf lp :head-index) to (getf lp :exit-index)
        collect (aref vec k)))

(defun %opt-swp-rewrite-loop (vec lp counter)
  "Generate prologue/kernel/epilogue layout for one scheduled loop."
  (let* ((body (getf lp :body))
         (step (car (last body))))
    (multiple-value-bind (scheduled-core ddg mii)
        (opt-modulo-schedule-loop-body body)
      (declare (ignore ddg mii))
      (if (or (null scheduled-core)
              (not (%opt-swp-safe-core-p (butlast body))))
          (values (%opt-swp-loop-sequence vec lp) nil)
          (let* ((base (format nil "~A~D" *opt-software-pipeline-label-prefix* counter))
                 (prologue-label (format nil "~A_prologue" base))
                 (kernel-label (format nil "~A_kernel" base))
                 (epilogue-label (format nil "~A_epilogue" base))
                 (jz (aref vec (getf lp :jz-index))))
            (values (append (list (aref vec (getf lp :head-index))
                                  (aref vec (getf lp :cmp-index))
                                  (make-vm-jump-zero :reg (vm-reg jz) :label epilogue-label)
                                  (make-vm-label :name prologue-label))
                            ;; Prime the pipeline for the current iteration.
                            (subseq scheduled-core 0 1)
                            (list (make-vm-label :name kernel-label))
                            ;; Kernel carries the remaining modulo slots, then the IV update.
                            (append (rest scheduled-core) (list step))
                            (list (aref vec (getf lp :back-index))
                                  (make-vm-label :name epilogue-label)
                                  (aref vec (getf lp :exit-index))))
                    t))))))

(defun opt-pass-software-pipelining (instructions)
  "FR-200: modulo-schedule canonical loop bodies.

The pass builds a DDG for each pure counted-loop body, computes MII, and emits a
prologue/kernel/epilogue layout.  It is intentionally conservative: side-effecting
or non-canonical loops are preserved, and generated labels make the pass
idempotent in the convergence pipeline."
  (if (%opt-swp-generated-p instructions)
      instructions
      (let* ((vec (coerce instructions 'vector))
             (n (length vec))
             (out nil)
             (changed nil)
             (counter 0)
             (i 0))
        (loop while (< i n)
              do (let ((lp (%opt-swp-parse-loop-at vec i)))
                   (if (null lp)
                       (progn
                         (push (aref vec i) out)
                         (incf i))
                       (multiple-value-bind (seq rewritten-p)
                           (%opt-swp-rewrite-loop vec lp counter)
                         (dolist (inst seq)
                           (push inst out))
                         (when rewritten-p
                           (incf counter)
                           (setf changed t))
                         (setf i (1+ (getf lp :exit-index)))))))
        (if changed (nreverse out) instructions))))

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
