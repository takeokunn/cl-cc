;;;; packages/engine/optimize/src/optimizer-strength-ext.lisp — Extended Strength Reduction Passes
;;;;
;;;; Contains:
;;;;   opt-pass-reassociate — arithmetic reassociation
;;;;   opt-pass-batch-concatenate — batch string concatenation
;;;;
;;;; Core strength reduction (opt-pass-strength-reduce, bswap recognition,
;;;; rotate recognition) are in optimizer-strength.lisp (loads before).
;;;;
;;;; Load order: after optimizer-strength.lisp.

(in-package :cl-cc/optimize)


(defun opt-reassociate-commutative-p (inst)
  (member (type-of inst)
          '(vm-add vm-integer-add vm-mul vm-integer-mul
            vm-logand vm-logior vm-logxor)
          :test #'eq))

(defun opt-copy-commutative-binop (inst dst lhs rhs)
  (typecase inst
    (vm-integer-add  (make-vm-integer-add :dst dst :lhs lhs :rhs rhs))
    (vm-add          (make-vm-add :dst dst :lhs lhs :rhs rhs))
    (vm-integer-mul  (make-vm-integer-mul :dst dst :lhs lhs :rhs rhs))
    (vm-mul          (make-vm-mul :dst dst :lhs lhs :rhs rhs))
    (vm-logand       (make-vm-logand :dst dst :lhs lhs :rhs rhs))
    (vm-logior       (make-vm-logior :dst dst :lhs lhs :rhs rhs))
    (vm-logxor       (make-vm-logxor :dst dst :lhs lhs :rhs rhs))
    (otherwise inst)))

(defun opt-pass-reassociate (instructions)
  "Reassociate commutative associative ops to move constants inward.

   Canonicalizes adjacent chains of +, *, logand, logior, and logxor so that
   compile-time constants drift toward the tail of the tree, exposing more
   folding opportunities without changing semantics for exact integer ops."
  (let ((use-counts (make-hash-table :test #'eq))
        (env        (make-hash-table :test #'eq))
        (result     nil))
    (dolist (inst instructions)
      (dolist (reg (opt-inst-read-regs inst))
        (incf (gethash reg use-counts 0))))
    (labels ((const-known-p (reg)
             (multiple-value-bind (val found) (gethash reg env)
               (declare (ignore val))
               found))
           (sorted-triplet (a b c)
             (append (loop for term in (list a b c)
                           unless (const-known-p term)
                           collect term)
                     (loop for term in (list a b c)
                           when (const-known-p term)
                           collect term)))
           (maybe-reassociate (prev cur)
             (let* ((dst (vm-dst prev))
                    (lhs (vm-lhs prev))
                    (rhs (vm-rhs prev))
                    (uses-prev-p (or (eq (vm-lhs cur) dst)
                                     (eq (vm-rhs cur) dst)))
                    (other (if (eq (vm-lhs cur) dst)
                               (vm-rhs cur)
                               (vm-lhs cur))))
               (when (and uses-prev-p
                          (eq (type-of prev) (type-of cur))
                          (opt-reassociate-commutative-p prev)
                          (= (gethash dst use-counts 0) 1))
                 (let* ((sorted (sorted-triplet lhs rhs other))
                        (orig   (list lhs rhs other)))
                   (when (and (not (equal sorted orig))
                              (or (const-known-p lhs)
                                  (const-known-p rhs)
                                  (const-known-p other)))
                     (values (opt-copy-commutative-binop prev dst
                                                         (second sorted)
                                                         (third sorted))
                             (opt-copy-commutative-binop cur (vm-dst cur)
                                                         (first sorted)
                                                         dst))))))))
      (dolist (inst instructions)
        (cond
          ((typep inst 'vm-label)
           (clrhash env)
           (push inst result))
          ((typep inst 'vm-const)
           (setf (gethash (vm-dst inst) env) (vm-value inst))
           (push inst result))
          ((and result
                (opt-reassociate-commutative-p inst)
                (opt-reassociate-commutative-p (car result)))
            (multiple-value-bind (new-prev new-cur)
                (maybe-reassociate (car result) inst)
              (if new-prev
                  (progn
                    (setf (car result) new-prev)
                   (push new-cur result)
                   (remhash (vm-dst new-prev) env)
                   (remhash (vm-dst new-cur) env))
                 (progn
                    (when (opt-inst-dst inst)
                      (remhash (opt-inst-dst inst) env))
                    (push inst result)))))
          (t
           (when (opt-inst-dst inst)
             (remhash (opt-inst-dst inst) env))
           (push inst result)))))
    (nreverse result)))

;;; ─── Pass: Batch Concatenation Packing ───────────────────────────────────

(defun %concat-parts (inst)
  "Return the register list represented by a vm-concatenate instruction."
  (or (vm-parts inst)
      (list (vm-str1 inst) (vm-str2 inst))))

(defun %make-packed-concatenate (dst parts)
  (make-vm-concatenate :dst dst
                       :str1 (first parts)
                       :str2 (car (last parts))
                       :parts parts))

(defun opt-pass-batch-concatenate (instructions)
  "Pack linear vm-concatenate chains into one instruction with a PARTS list."
  (let ((use-counts (make-hash-table :test #'eq)))
    (dolist (inst instructions)
      (dolist (reg (opt-inst-read-regs inst))
        (incf (gethash reg use-counts 0))))
    (labels ((concat-chain-p (current next)
               (and (typep current 'vm-concatenate)
                    (typep next 'vm-concatenate)
                    (= (gethash (vm-dst current) use-counts 0) 1)
                    (eq (vm-str1 next) (vm-dst current))))
             (pack-chain (rest)
               (when rest
                 (let ((inst (car rest)))
                   (if (typep inst 'vm-concatenate)
                       (let ((parts (%concat-parts inst))
                             (dst (vm-dst inst))
                             (tail (cdr rest))
                             (current inst)
                             (merged nil))
                         (loop while (and tail (concat-chain-p current (car tail))) do
                          (let ((next (car tail)))
                            (setf merged t
                                    parts (append parts (rest (%concat-parts next)))
                                    dst (vm-dst next)
                                    current next
                                    tail (cdr tail))))
                         (cons (if merged
                                   (%make-packed-concatenate dst parts)
                                   inst)
                               (pack-chain tail)))
                       (cons inst (pack-chain (cdr rest))))))))
      (pack-chain instructions))))

;;; CSE, GVN, dead-label elimination, and leaf-function detection have been
;;; extracted to optimizer-cse-gvn.lisp (loads immediately after this file).
