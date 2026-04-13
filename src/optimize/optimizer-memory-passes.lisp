(in-package :cl-cc)
;;; ─── Memory Optimization Passes ─────────────────────────────────────────────
;;;
;;; Extracted from optimizer-memory.lisp.
;;; Depends on: optimizer-memory.lisp (alias analysis: opt-compute-heap-aliases,
;;;   opt-slot-alias-key, opt-rewrite-inst-regs, opt-inst-dst, opt-inst-pure-p).
;;; Load order: immediately after optimizer-memory.lisp.

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
