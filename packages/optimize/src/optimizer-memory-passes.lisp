(in-package :cl-cc/optimize)
;;; ─── Memory Optimization Passes ─────────────────────────────────────────────
;;;
;;; Extracted from optimizer-memory.lisp.
;;; Depends on: optimizer-memory.lisp (alias analysis: opt-compute-heap-aliases,
;;;   opt-slot-alias-key, opt-rewrite-inst-regs, opt-inst-dst, opt-inst-pure-p).
;;; Load order: immediately after optimizer-memory.lisp.

;;; ─── Shared mem-pass-state struct and helpers ────────────────────────────────

(defstruct (mem-pass-state (:conc-name mps-))
  "Mutable state shared by the DSE and store-to-load-forward passes."
  (result        nil                           :type list)
  (pending-order nil                           :type list)
  (pending-by-name (make-hash-table :test #'equal) :type hash-table))

(defun %mps-emit (state inst)
  (push inst (mps-result state)))

(defun %mps-pending-store (state key)
  (gethash key (mps-pending-by-name state)))

(defun %mps-remember-store (state key inst)
  (unless (gethash key (mps-pending-by-name state))
    (push key (mps-pending-order state)))
  (setf (gethash key (mps-pending-by-name state)) inst))

(defun %mps-drop-pending (state key)
  (remhash key (mps-pending-by-name state))
  (setf (mps-pending-order state)
        (remove key (mps-pending-order state) :test #'equal)))

(defun %mps-flush-one (state key)
  (let ((inst (%mps-pending-store state key)))
    (when inst
      (%mps-emit state inst)
      (%mps-drop-pending state key))))

(defun %mps-flush-all (state)
  (dolist (key (reverse (mps-pending-order state)))
    (%mps-flush-one state key)))

(defun %mps-pending-uses-reg-p (pending reg)
  "Return T if PENDING store instruction reads REG."
  (typecase pending
    (vm-set-global (eq (cl-cc/vm::vm-set-global-src pending) reg))
    (vm-slot-write (or (eq (cl-cc/vm::vm-slot-write-obj-reg pending) reg)
                       (eq (cl-cc/vm::vm-slot-write-value-reg pending) reg)))))

(defun %mps-flush-if-src-overwritten (state dst)
  "Flush pending stores that read DST (i.e., their source register is DST)."
  (dolist (key (copy-list (mps-pending-order state)))
    (let ((pending (%mps-pending-store state key)))
      (when (and pending (%mps-pending-uses-reg-p pending dst))
        (%mps-flush-one state key)))))

(defun %mps-flush-dependent-on-reg (state reg &key exclude-key)
  "Flush pending stores that reference REG (but not EXCLUDE-KEY)."
  (dolist (key (copy-list (mps-pending-order state)))
    (let ((pending (%mps-pending-store state key)))
      (when (and pending
                  (not (equal key exclude-key))
                  (%mps-pending-uses-reg-p pending reg))
        (%mps-flush-one state key)))))

;;; ─── Cons slot forwarding ─────────────────────────────────────────────────

(defun %opt-cons-slot-kill-dependent-on-reg (facts reg)
  "Remove cons-slot facts made stale when REG is overwritten."
  (when reg
    (let ((dead nil))
      (maphash (lambda (cell-reg entry)
                 (when (or (eq cell-reg reg)
                           (eq (car entry) reg)
                           (eq (cdr entry) reg))
                   (push cell-reg dead)))
               facts)
      (dolist (cell-reg dead)
        (remhash cell-reg facts)))))

(defun %opt-cons-slot-safe-source-p (dst car-reg cdr-reg)
  "Return T when a vm-cons DST write does not destroy either slot source."
  (and (not (eq dst car-reg))
       (not (eq dst cdr-reg))))

(defun %opt-cons-slot-mutation-boundary-p (inst)
  "Return T when INST may mutate heap/global state or call arbitrary code."
  (member (vm-inst-effect-kind inst)
          '(:write-global :io :control :unknown)
          :test #'eq))

(defun opt-pass-cons-slot-forward (instructions)
  "Forward car/cdr of a fresh vm-cons to moves from the original slot registers.

The pass is intentionally straight-line and conservative: labels/control-flow,
calls, unknown effects, and heap mutations clear all facts, while overwrites of a
tracked cell register or slot source kill only the dependent facts."
  (let ((facts (make-hash-table :test #'eq))
        (result nil))
    (labels ((emit (inst) (push inst result))
             (remember-cons (dst car-reg cdr-reg)
               (if (%opt-cons-slot-safe-source-p dst car-reg cdr-reg)
                   (setf (gethash dst facts) (cons car-reg cdr-reg))
                   (remhash dst facts)))
             (copy-fact (dst src)
               (multiple-value-bind (entry found-p) (gethash src facts)
                 (if found-p
                     (setf (gethash dst facts) entry)
                     (remhash dst facts)))))
      (dolist (inst instructions (nreverse result))
        (typecase inst
          (vm-cons
           (let ((dst (vm-dst inst)))
             (%opt-cons-slot-kill-dependent-on-reg facts dst)
             (remember-cons dst (vm-car-reg inst) (vm-cdr-reg inst))
             (emit inst)))
          (vm-move
           (let ((dst (vm-dst inst))
                 (src (vm-src inst)))
             (unless (eq dst src)
               (%opt-cons-slot-kill-dependent-on-reg facts dst)
               (copy-fact dst src))
             (emit inst)))
          (vm-car
           (let* ((dst (vm-dst inst))
                  (entry (gethash (vm-src inst) facts)))
             (%opt-cons-slot-kill-dependent-on-reg facts dst)
             (if entry
                 (emit (make-vm-move :dst dst :src (car entry)))
                 (emit inst))))
          (vm-cdr
           (let* ((dst (vm-dst inst))
                  (entry (gethash (vm-src inst) facts)))
             (%opt-cons-slot-kill-dependent-on-reg facts dst)
             (if entry
                 (emit (make-vm-move :dst dst :src (cdr entry)))
                 (emit inst))))
          ((or vm-label vm-jump vm-jump-zero vm-ret vm-halt)
           (clrhash facts)
           (emit inst))
          (t
           (let ((dst (opt-inst-dst inst)))
             (when dst
               (%opt-cons-slot-kill-dependent-on-reg facts dst)))
           (when (%opt-cons-slot-mutation-boundary-p inst)
             (clrhash facts))
           (emit inst)))))))

(defun opt-pass-dead-store-elim (instructions)
  "Eliminate overwritten stores in straight-line code.

   Conservative rules:
   - a later store to the same global/slot kills the earlier pending store
   - vm-get-global / vm-slot-read force the matching pending store to be emitted first
   - labels and non-pure instructions flush all pending stores"
  (let ((state       (make-mem-pass-state))
        (alias-roots (opt-compute-heap-aliases instructions)))
    (dolist (inst instructions)
      (typecase inst
        (vm-label
         (%mps-flush-all state)
         (%mps-emit state inst))
        ((or vm-jump vm-jump-zero vm-ret vm-halt)
         (%mps-flush-all state)
         (%mps-emit state inst))
        (vm-get-global
         (let ((dst (cl-cc/vm::vm-get-global-dst inst)))
           (when dst (%mps-flush-if-src-overwritten state dst)))
         (%mps-flush-one state (cl-cc/vm::vm-get-global-name inst))
         (%mps-emit state inst))
        (vm-slot-read
         (let ((dst (cl-cc/vm::vm-slot-read-dst inst)))
           (when dst (%mps-flush-if-src-overwritten state dst)))
         (%mps-flush-one state (opt-slot-alias-key (cl-cc/vm::vm-slot-read-obj-reg inst)
                                                    (cl-cc/vm::vm-slot-read-slot-name inst)
                                                    alias-roots))
         (%mps-emit state inst))
        (vm-set-global
         (%mps-remember-store state (cl-cc/vm::vm-set-global-name inst) inst))
        (vm-slot-write
         (%mps-remember-store state (opt-slot-alias-key (cl-cc/vm::vm-slot-write-obj-reg inst)
                                                         (cl-cc/vm::vm-slot-write-slot-name inst)
                                                         alias-roots)
                              inst))
        (t
         (let ((dst (opt-inst-dst inst)))
           (when dst (%mps-flush-if-src-overwritten state dst)))
         (unless (opt-inst-pure-p inst) (%mps-flush-all state))
         (%mps-emit state inst))))
    (%mps-flush-all state)
    (nreverse (mps-result state))))

(defun opt-pass-store-to-load-forward (instructions)
  "Forward pending vm-set-global values into matching vm-get-global loads.

   Conservative rules:
   - only straight-line code is considered
   - a vm-get-global/vm-slot-read is replaced with vm-move from the latest
     pending store for the same location
   - labels and non-pure instructions flush all pending stores
   - the store itself is still emitted later, preserving side effects"
  (let ((state       (make-mem-pass-state))
        (alias-roots (opt-compute-heap-aliases instructions)))
    (dolist (inst instructions)
      (typecase inst
        (vm-label
         (%mps-flush-all state)
         (%mps-emit state inst))
        ((or vm-jump vm-jump-zero vm-ret vm-halt)
         (%mps-flush-all state)
         (%mps-emit state inst))
        (vm-get-global
         (let* ((key   (list :global (cl-cc/vm::vm-get-global-name inst)))
                (store (%mps-pending-store state key))
                (dst   (cl-cc/vm::vm-get-global-dst inst)))
           (if store
               (progn (%mps-emit state (make-vm-move :dst dst :src (cl-cc/vm::vm-set-global-src store)))
                      (when dst
                        (%mps-flush-dependent-on-reg state dst :exclude-key key)))
               (progn (when dst
                        (%mps-flush-dependent-on-reg state dst))
                      (%mps-flush-one state key)
                      (%mps-emit state inst)))))
        (vm-slot-read
         (let* ((key   (opt-slot-alias-key (cl-cc/vm::vm-slot-read-obj-reg inst)
                                           (cl-cc/vm::vm-slot-read-slot-name inst)
                                           alias-roots))
                (store (%mps-pending-store state key))
                (dst   (cl-cc/vm::vm-slot-read-dst inst)))
           (if store
               (progn (%mps-emit state (make-vm-move :dst dst :src (cl-cc/vm::vm-slot-write-value-reg store)))
                      (when dst
                        (%mps-flush-dependent-on-reg state dst :exclude-key key)))
               (progn (when dst
                        (%mps-flush-dependent-on-reg state dst))
                      (%mps-flush-one state key)
                      (%mps-emit state inst)))))
        (vm-set-global
         (%mps-remember-store state (list :global (cl-cc/vm::vm-set-global-name inst)) inst))
        (vm-slot-write
         (%mps-remember-store state (opt-slot-alias-key (cl-cc/vm::vm-slot-write-obj-reg inst)
                                                         (cl-cc/vm::vm-slot-write-slot-name inst)
                                                         alias-roots)
                              inst))
        (t
         (let ((dst (opt-inst-dst inst)))
           (when dst (%mps-flush-dependent-on-reg state dst)))
         (unless (opt-inst-pure-p inst) (%mps-flush-all state))
         (%mps-emit state inst))))
    (%mps-flush-all state)
    (nreverse (mps-result state))))
