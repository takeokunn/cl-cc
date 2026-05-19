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

;;; ─── Pass 1: Constant Folding + Algebraic Simplification ─────────────────

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

(defun %opt-branch-target-labels (instructions)
  "Return a hash table of labels that are explicit branch targets."
  (let ((targets (make-hash-table :test #'equal)))
    (dolist (inst instructions targets)
      (typecase inst
        ((or vm-jump vm-jump-zero)
         (setf (gethash (vm-label-name inst) targets) t))))))

(defun %fold-vm-label (inst env low-bit-facts emit target-labels)
  "Flush constant knowledge only at labels that are explicit branch targets."
  (when (gethash (vm-name inst) target-labels)
    (clrhash env)
    (clrhash low-bit-facts))
  (funcall emit inst))

(defun %fold-vm-const (inst env low-bit-facts emit)
  "Record the constant value of INST's destination in ENV, then emit."
  (remhash (vm-dst inst) low-bit-facts)
  (setf (gethash (vm-dst inst) env) (vm-value inst))
  (funcall emit inst))

(defun %fold-vm-move (inst env low-bit-facts emit emit-const clear)
  "Propagate constant if src is known in ENV; eliminate self-moves silently."
  (let ((src (vm-src inst)) (dst (vm-dst inst)))
    (cond
      ((eq src dst)) ; self-move: drop silently
      (t
       (multiple-value-bind (sval found) (gethash src env)
          (if found
              (progn
                (remhash dst low-bit-facts)
                (funcall emit-const dst sval))
              (progn
                (funcall clear dst)
                (%opt-copy-derived-fact src dst low-bit-facts)
                (funcall emit inst))))))))

(defun %fold-binary-inst (inst env low-bit-facts emit emit-const clear)
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
                    (funcall emit-const dst folded))
                  (progn
                    (remhash dst low-bit-facts)
                    (funcall clear dst)
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
                (remhash dst low-bit-facts)
                (funcall clear dst)
                (funcall emit parity-rewrite))
               (simp
                (when (vm-const-p simp)
                  (setf (gethash dst env) (vm-const-value simp)))
                (cond
                  ((vm-const-p simp)
                   (remhash dst low-bit-facts))
                  ((typep simp 'vm-move)
                   (funcall clear dst)
                   (%opt-copy-derived-fact (vm-src simp) dst low-bit-facts))
                  (t
                   (funcall clear dst)
                   (remhash dst low-bit-facts)))
                (funcall emit simp))
               (t
                (funcall clear dst)
                (let ((source (%opt-logand-low-bit-source lhs rhs lval lfound rval rfound)))
                  (if (and (typep inst 'vm-logand) source)
                      (setf (gethash dst low-bit-facts) source)
                      (remhash dst low-bit-facts)))
                (funcall emit inst))))))))))

(defun %fold-unary-constant-eligible-p (inst value)
  "Return T when unary INST can be safely folded for constant VALUE."
  (or (numberp value)
      (eq (type-of inst) 'vm-not)
      (and (member (type-of inst) '(vm-car vm-cdr) :test #'eq)
           (or (consp value) (null value)))))

(defun %fold-unary-inst (inst env low-bit-facts emit emit-const clear)
  "Unary arithmetic: data-driven via opt-foldable-unary-arith-p → *opt-unary-fold-table*."
  (let* ((dst (vm-dst inst)) (src (vm-src inst))
         (fold-fn (gethash (type-of inst) *opt-unary-fold-table*)))
    (multiple-value-bind (sval found) (gethash src env)
      (if (and found fold-fn
                (%fold-unary-constant-eligible-p inst sval))
          (progn
            (remhash dst low-bit-facts)
            (funcall emit-const dst (funcall fold-fn sval)))
          (progn
            (funcall clear dst)
            (remhash dst low-bit-facts)
            (funcall emit inst))))))

(defun %fold-type-pred-inst (inst env low-bit-facts emit emit-const clear)
  "Type predicates: data-driven via opt-foldable-type-pred-p → *opt-type-pred-fold-table*."
  (let* ((dst (vm-dst inst)) (src (vm-src inst))
         (pred-fn (gethash (type-of inst) *opt-type-pred-fold-table*)))
    (multiple-value-bind (sval found) (gethash src env)
      (if (and found pred-fn)
          (progn
            (remhash dst low-bit-facts)
            (funcall emit-const dst (if (funcall pred-fn sval) 1 0)))
          (progn
            (funcall clear dst)
            (remhash dst low-bit-facts)
            (funcall emit inst))))))

(defun %fold-vm-jump-zero (inst env emit)
  "Constant branch folding: known-false → unconditional jump; known-true → drop."
  (multiple-value-bind (val found) (gethash (vm-reg inst) env)
    (if found
        (if (opt-falsep val)
            (funcall emit (make-vm-jump :label (vm-label-name inst)))
            nil) ; condition is always true → branch never taken → drop
        (funcall emit inst))))

(defun %fold-default-inst (inst env low-bit-facts emit clear)
  "Default: invalidate any written destination register, then emit."
  (declare (ignore env))
  (let ((dst (opt-inst-dst inst)))
    (when dst
      (funcall clear dst)
      (remhash dst low-bit-facts)))
  (funcall emit inst))

(defun opt-pass-fold (instructions)
  "Forward pass: constant folding, algebraic simplification, constant branch elimination."
  (let ((env (make-hash-table :test #'eq)) ; reg → known constant value
        (low-bit-facts (make-hash-table :test #'eq)) ; reg → original src for (logand src 1)
        (target-labels (%opt-branch-target-labels instructions))
        (result nil))
    (flet ((emit (inst) (push inst result))
           (emit-const (dst val)
             (setf (gethash dst env) val)
             (push (make-vm-const :dst dst :value val) result))
            (clear (reg) (remhash reg env)))
      (dolist (inst instructions)
        (typecase inst
          (vm-label    (%fold-vm-label      inst env low-bit-facts #'emit target-labels))
          (vm-const    (%fold-vm-const      inst env low-bit-facts #'emit))
          (vm-move     (%fold-vm-move       inst env low-bit-facts #'emit #'emit-const #'clear))
          (vm-jump-zero (%fold-vm-jump-zero inst env #'emit))
          (t
           (cond
              ((opt-binary-lhs-rhs-p inst)      (%fold-binary-inst    inst env low-bit-facts #'emit #'emit-const #'clear))
              ((opt-foldable-unary-arith-p inst) (%fold-unary-inst     inst env low-bit-facts #'emit #'emit-const #'clear))
              ((opt-foldable-type-pred-p inst)   (%fold-type-pred-inst inst env low-bit-facts #'emit #'emit-const #'clear))
              (t                                 (%fold-default-inst   inst env low-bit-facts #'emit #'clear))))))
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

(defun %opt-max-register-index (instructions)
  "Return the largest numeric :R register index mentioned in INSTRUCTIONS."
  (labels ((scan (x best)
             (cond
               ((and (keywordp x)
                     (let ((name (symbol-name x)))
                       (and (>= (length name) 2)
                            (char= (char name 0) #\R)
                            (every #'digit-char-p (subseq name 1)))))
                (max best (parse-integer (subseq (symbol-name x) 1))))
               ((consp x) (scan (cdr x) (scan (car x) best)))
               (t best))))
    (loop for inst in instructions
          maximize (handler-case (scan (instruction->sexp inst) -1)
                     (error () -1)))))

(defun %opt-fresh-register-generator (instructions)
  "Return a closure yielding fresh VM register keywords for INSTRUCTIONS."
  (let ((next (1+ (%opt-max-register-index instructions))))
    (lambda ()
      (prog1 (intern (format nil "R~D" next) :keyword)
        (incf next)))))

(defun %opt-control-or-label-p (inst)
  "Return T when INST should not be part of a straight-line outlined sequence."
  (or (typep inst 'vm-label)
      (typep inst 'vm-jump)
      (typep inst 'vm-jump-zero)
      (typep inst 'vm-ret)
      (typep inst 'vm-call)
      (typep inst 'vm-tail-call)
      (typep inst 'vm-apply)))

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

;;; ─── Top-Level Optimizer ─────────────────────────────────────────────────
