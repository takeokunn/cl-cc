(in-package :cl-cc)
;;; ─── Alias Analysis + Memory Passes ─────────────────────────────────────────

(defun opt-heap-root-inst-p (inst)
  "Return T when INST produces a fresh heap-like object identity."
  (typep inst '(or vm-cons vm-make-array vm-closure vm-make-closure)))

(defun opt-heap-root-kind (inst)
  "Return a symbolic heap kind for fresh heap-producing INST."
  (cond
    ((typep inst 'vm-cons) :cons)
    ((typep inst 'vm-make-array) :array)
    ((typep inst '(or vm-closure vm-make-closure)) :closure)
    (t nil)))

(defun %opt-build-root-map (instructions)
  "Build a conservative EQ hash-table mapping registers to their canonical heap root.

Fresh heap producers (opt-heap-root-inst-p) start a new root at their destination.
vm-move propagates the source root. Any other destination write kills the root fact."
  (let ((roots (make-hash-table :test #'eq)))
    (dolist (inst instructions roots)
      (let ((dst (opt-inst-dst inst)))
        (cond
          ((and dst (opt-heap-root-inst-p inst))
           (setf (gethash dst roots) dst))
          ((typep inst 'vm-move)
           (multiple-value-bind (root found-p)
               (gethash (vm-move-src inst) roots)
             (if found-p
                 (setf (gethash dst roots) root)
                 (remhash dst roots))))
          (dst
           (remhash dst roots)))))))

(defun opt-compute-heap-aliases (instructions)
  "Compute a conservative EQ hash-table reg -> canonical heap root.
This is a small FR-115 style oracle intended for downstream passes."
  (%opt-build-root-map instructions))

(defun opt-compute-points-to (instructions)
  "Compute a conservative flow-sensitive points-to map for heap-like registers.
The result is an EQ hash-table reg -> canonical root register."
  (%opt-build-root-map instructions))

(defun opt-points-to-root (reg points-to)
  "Return the current root object for REG under POINTS-TO, or NIL."
  (gethash reg points-to))

(defun opt-make-interval (lo hi)
  "Construct a closed integer interval [LO, HI]."
  (cons lo hi))

(defun opt-interval-lo (interval)
  (car interval))

(defun opt-interval-hi (interval)
  (cdr interval))

(defun opt-interval-add (a b)
  "Add two intervals conservatively."
  (opt-make-interval (+ (opt-interval-lo a) (opt-interval-lo b))
                     (+ (opt-interval-hi a) (opt-interval-hi b))))

(defun opt-interval-sub (a b)
  "Subtract interval B from A conservatively."
  (opt-make-interval (- (opt-interval-lo a) (opt-interval-hi b))
                     (- (opt-interval-hi a) (opt-interval-lo b))))

(defun opt-interval-mul (a b)
  "Multiply two intervals conservatively."
  (let* ((p1 (* (opt-interval-lo a) (opt-interval-lo b)))
         (p2 (* (opt-interval-lo a) (opt-interval-hi b)))
         (p3 (* (opt-interval-hi a) (opt-interval-lo b)))
         (p4 (* (opt-interval-hi a) (opt-interval-hi b))))
    (opt-make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

(defun opt-compute-constant-intervals (instructions)
  "Compute a conservative interval map from straight-line constant arithmetic.

Handles vm-const and interval propagation through vm-add/vm-sub/vm-mul when
both operands already have known intervals."
  (let ((intervals (make-hash-table :test #'eq)))
    (dolist (inst instructions intervals)
      (typecase inst
        (vm-const
         (when (integerp (vm-value inst))
           (setf (gethash (vm-dst inst) intervals)
                 (opt-make-interval (vm-value inst) (vm-value inst)))))
        (vm-add
         (let ((lhs (gethash (vm-lhs inst) intervals))
               (rhs (gethash (vm-rhs inst) intervals)))
           (if (and lhs rhs)
               (setf (gethash (vm-dst inst) intervals)
                     (opt-interval-add lhs rhs))
               (remhash (vm-dst inst) intervals))))
        (vm-sub
         (let ((lhs (gethash (vm-lhs inst) intervals))
               (rhs (gethash (vm-rhs inst) intervals)))
           (if (and lhs rhs)
               (setf (gethash (vm-dst inst) intervals)
                     (opt-interval-sub lhs rhs))
               (remhash (vm-dst inst) intervals))))
        (vm-mul
         (let ((lhs (gethash (vm-lhs inst) intervals))
               (rhs (gethash (vm-rhs inst) intervals)))
           (if (and lhs rhs)
               (setf (gethash (vm-dst inst) intervals)
                     (opt-interval-mul lhs rhs))
               (remhash (vm-dst inst) intervals))))
        (t
         (let ((dst (opt-inst-dst inst)))
           (when dst
             (remhash dst intervals))))))))

(defun opt-compute-heap-kinds (instructions)
  "Compute a conservative root -> heap-kind table for TBAA-style checks."
  (let ((points-to (opt-compute-points-to instructions))
        (kinds (make-hash-table :test #'eq)))
    (dolist (inst instructions kinds)
      (let ((dst (opt-inst-dst inst)))
        (when (and dst (opt-heap-root-inst-p inst))
          (let ((root (gethash dst points-to)))
            (when root
              (setf (gethash root kinds) (opt-heap-root-kind inst)))))))
    kinds))

(defun opt-may-alias-by-type-p (reg-a reg-b points-to heap-kinds)
  "Return T if REG-A and REG-B may alias under type-based heap classification.

If both roots and kinds are known and the kinds differ, return NIL. Otherwise
stay conservative and return T."
  (let ((root-a (gethash reg-a points-to))
        (root-b (gethash reg-b points-to)))
    (cond
      ((or (null root-a) (null root-b)) t)
      ((eq root-a root-b) t)
      (t (let ((kind-a (gethash root-a heap-kinds))
               (kind-b (gethash root-b heap-kinds)))
           (if (and kind-a kind-b)
               (eq kind-a kind-b)
               t))))))

(defun opt-must-alias-p (reg-a reg-b alias-roots)
  "Return T when REG-A and REG-B definitely alias under ALIAS-ROOTS."
  (multiple-value-bind (root-a found-a) (gethash reg-a alias-roots)
    (multiple-value-bind (root-b found-b) (gethash reg-b alias-roots)
      (and found-a found-b (eq root-a root-b)))))

(defun opt-may-alias-p (reg-a reg-b alias-roots)
  "Return T when REG-A and REG-B may alias under ALIAS-ROOTS.

Unknown roots remain conservative and therefore return T."
  (multiple-value-bind (root-a found-a) (gethash reg-a alias-roots)
    (multiple-value-bind (root-b found-b) (gethash reg-b alias-roots)
      (or (not found-a)
          (not found-b)
          (eq root-a root-b)))))

(defun opt-slot-alias-key (obj-reg slot-name alias-roots)
  "Return a canonical slot key for OBJ-REG/SLOT-NAME using ALIAS-ROOTS."
  (multiple-value-bind (root found-p) (gethash obj-reg alias-roots)
    (list :slot (if found-p root obj-reg) slot-name)))

(defun opt-rewrite-inst-regs (inst copies)
  "Return INST with all source registers replaced by their canonical copies.
   Uses sexp roundtrip: instruction->sexp rewrites all register-keyword leaves
   except the destination slot (position 1 for instructions with a dst), then
   reconstructs via sexp->instruction.  Falls back to INST unchanged on error."
  (flet ((c (x) (if (opt-register-keyword-p x) (or (gethash x copies) x) x)))
    (handler-case
        (let* ((sexp      (instruction->sexp inst))
               (has-dst   (not (null (opt-inst-dst inst))))
               ;; Rewrite all leaves; for instructions with a dst, the dst sits at
               ;; position 1 (immediately after the opcode tag) — leave it intact.
               (new-sexp  (if has-dst
                              (list* (first sexp) (second sexp)
                                     (opt-map-tree #'c (cddr sexp)))
                              (cons  (first sexp)
                                     (opt-map-tree #'c (cdr sexp))))))
          (if (equal sexp new-sexp) inst (sexp->instruction new-sexp)))
      (error () inst))))

(defun opt-pass-dead-store-elim (instructions)
  "Eliminate overwritten stores in straight-line code.

   Conservative rules:
   - a later store to the same global/slot kills the earlier pending store
   - vm-get-global / vm-slot-read force the matching pending store to be emitted first
   - labels and non-pure instructions flush all pending stores"
  (let ((result nil)
         (pending-order nil)
         (pending-by-name (make-hash-table :test #'equal))
         (alias-roots (opt-compute-heap-aliases instructions)))
    (labels ((emit (inst)
               (push inst result))
             (pending-store (key)
               (gethash key pending-by-name))
             (remember-store (key inst)
               (unless (gethash key pending-by-name)
                 (push key pending-order))
               (setf (gethash key pending-by-name) inst))
             (drop-pending (key)
               (remhash key pending-by-name)
               (setf pending-order (remove key pending-order :test #'equal)))
             (flush-one (key)
               (let ((inst (pending-store key)))
                 (when inst
                   (emit inst)
                   (drop-pending key))))
             (flush-all ()
               (dolist (key (nreverse pending-order))
                  (flush-one key))))
      (dolist (inst instructions)
        (let ((dst (opt-inst-dst inst)))
          (when dst
            (dolist (key (copy-list pending-order))
              (let ((pending (pending-store key)))
                (when (and pending (eq (vm-src pending) dst))
                  (flush-one key))))))
        (typecase inst
          (vm-label
           (flush-all)
           (emit inst))
          ((or vm-jump vm-jump-zero vm-ret vm-halt)
           (flush-all)
           (emit inst))
          (vm-get-global
           (let ((name (vm-global-name inst)))
             (flush-one name)
             (emit inst)))
          (vm-slot-read
           (let ((key (opt-slot-alias-key (vm-obj-reg inst)
                                          (vm-slot-name-sym inst)
                                          alias-roots)))
             (flush-one key)
             (emit inst)))
          (vm-set-global
           (remember-store (vm-global-name inst) inst))
          (vm-slot-write
           (remember-store (opt-slot-alias-key (vm-obj-reg inst)
                                               (vm-slot-name-sym inst)
                                               alias-roots)
                           inst))
          (t
           (unless (opt-inst-pure-p inst)
             (flush-all))
           (emit inst))))
      (flush-all)
      (nreverse result))))

(defun opt-pass-store-to-load-forward (instructions)
  "Forward pending vm-set-global values into matching vm-get-global loads.

   Conservative rules:
   - only straight-line code is considered
   - a vm-get-global/vm-slot-read is replaced with vm-move from the latest
     pending store for the same location
   - labels and non-pure instructions flush all pending stores
   - the store itself is still emitted later, preserving side effects"
  (let ((result nil)
         (pending-order nil)
         (pending-by-name (make-hash-table :test #'equal))
         (alias-roots (opt-compute-heap-aliases instructions)))
    (labels ((emit (inst)
               (push inst result))
             (pending-store (key)
               (gethash key pending-by-name))
             (remember-store (key inst)
               (unless (gethash key pending-by-name)
                 (push key pending-order))
               (setf (gethash key pending-by-name) inst))
             (drop-pending (key)
               (remhash key pending-by-name)
               (setf pending-order (remove key pending-order :test #'equal)))
             (flush-one (key)
               (let ((inst (pending-store key)))
                 (when inst
                   (emit inst)
                   (drop-pending key))))
             (flush-all ()
               (dolist (key (nreverse pending-order))
                 (flush-one key)))
             (flush-dependent-on-reg (reg &key exclude-key)
               (dolist (key (copy-list pending-order))
                 (let ((pending (pending-store key)))
                   (when (and pending (not (equal key exclude-key)))
                     (typecase pending
                       (vm-set-global
                        (when (eq (vm-src pending) reg)
                          (flush-one key)))
                       (vm-slot-write
                        (when (or (eq (vm-obj-reg pending) reg)
                                  (eq (vm-value-reg pending) reg))
                          (flush-one key)))))))))
       (dolist (inst instructions)
          (typecase inst
            (vm-label
             (flush-all)
             (emit inst))
           ((or vm-jump vm-jump-zero vm-ret vm-halt)
            (flush-all)
            (emit inst))
           (vm-get-global
            (let* ((key (list :global (vm-global-name inst)))
                   (store (pending-store key)))
              (if store
                  (progn
                    (emit (make-vm-move :dst (vm-dst inst) :src (vm-src store)))
                    (when (vm-dst inst)
                      (flush-dependent-on-reg (vm-dst inst) :exclude-key key)))
                  (progn
                    (when (vm-dst inst)
                      (flush-dependent-on-reg (vm-dst inst)))
                    (flush-one key)
                    (emit inst)))))
            (vm-slot-read
            (let* ((key (opt-slot-alias-key (vm-obj-reg inst)
                                            (vm-slot-name-sym inst)
                                            alias-roots))
                   (store (pending-store key)))
              (if store
                  (progn
                    (emit (make-vm-move :dst (vm-dst inst) :src (vm-value-reg store)))
                    (when (vm-dst inst)
                      (flush-dependent-on-reg (vm-dst inst) :exclude-key key)))
                  (progn
                    (when (vm-dst inst)
                      (flush-dependent-on-reg (vm-dst inst)))
                    (flush-one key)
                    (emit inst)))))
            (vm-set-global
             (remember-store (list :global (vm-global-name inst)) inst))
            (vm-slot-write
             (remember-store (opt-slot-alias-key (vm-obj-reg inst)
                                                 (vm-slot-name-sym inst)
                                                 alias-roots)
                             inst))
            (t
            (let ((dst (opt-inst-dst inst)))
              (when dst
                (flush-dependent-on-reg dst)))
            (unless (opt-inst-pure-p inst)
              (flush-all))
            (emit inst))))
      (flush-all)
      (nreverse result))))
