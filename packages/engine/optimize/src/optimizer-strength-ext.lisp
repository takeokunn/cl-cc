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


(defparameter *opt-commutative-binop-table*
  `((vm-integer-add . ,#'make-vm-integer-add)
    (vm-add         . ,#'make-vm-add)
    (vm-integer-mul . ,#'make-vm-integer-mul)
    (vm-mul         . ,#'make-vm-mul)
    (vm-logand      . ,#'make-vm-logand)
    (vm-logior      . ,#'make-vm-logior)
    (vm-logxor      . ,#'make-vm-logxor))
  "Maps commutative binary op types to their constructor functions.")

(defun opt-reassociate-commutative-p (inst)
  (not (null (assoc (type-of inst) *opt-commutative-binop-table* :test #'eq))))

(defun opt-copy-commutative-binop (inst dst lhs rhs)
  (let ((entry (assoc (type-of inst) *opt-commutative-binop-table* :test #'eq)))
    (if entry
        (funcall (cdr entry) :dst dst :lhs lhs :rhs rhs)
        inst)))

;;; ─── Reassociate helpers ─────────────────────────────────────────────────

(defun %opt-reassociate-sorted-triplet (a b c env)
  "Return (a b c) reordered so constants (present in ENV) come last."
  (append (loop for term in (list a b c) unless (nth-value 1 (gethash term env)) collect term)
          (loop for term in (list a b c) when   (nth-value 1 (gethash term env)) collect term)))

(defun %opt-maybe-reassociate (prev cur env use-counts)
  "Return (values new-prev new-cur) when PREV/CUR can be reassociated, or NIL."
  (let* ((dst (vm-dst prev))
         (lhs (vm-lhs prev))
         (rhs (vm-rhs prev))
         (uses-prev-p (or (eq (vm-lhs cur) dst) (eq (vm-rhs cur) dst)))
         (other (if (eq (vm-lhs cur) dst) (vm-rhs cur) (vm-lhs cur))))
    (when (and uses-prev-p
               (eq (type-of prev) (type-of cur))
               (opt-reassociate-commutative-p prev)
               (= (gethash dst use-counts 0) 1))
      (let* ((sorted (%opt-reassociate-sorted-triplet lhs rhs other env))
             (orig   (list lhs rhs other)))
        (when (and (not (equal sorted orig))
                   (or (nth-value 1 (gethash lhs env))
                       (nth-value 1 (gethash rhs env))
                       (nth-value 1 (gethash other env))))
          (values (opt-copy-commutative-binop prev dst (second sorted) (third sorted))
                  (opt-copy-commutative-binop cur (vm-dst cur) (first sorted) dst)))))))

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
    (dolist (inst instructions)
        (typecase inst
          (vm-label
           (clrhash env)
           (push inst result))
          (vm-const
           (setf (gethash (vm-dst inst) env) (vm-value inst))
           (push inst result))
          (t
           (cond
             ((and result
                   (opt-reassociate-commutative-p inst)
                   (opt-reassociate-commutative-p (car result)))
              (multiple-value-bind (new-prev new-cur)
                  (%opt-maybe-reassociate (car result) inst env use-counts)
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
    (nreverse result))))

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

;;; ─── Batch concatenate helpers ───────────────────────────────────────────

(defun %opt-concat-chain-p (current next use-counts)
  "Return T when CURRENT and NEXT form a chainable vm-concatenate pair."
  (and (typep current 'vm-concatenate)
       (typep next 'vm-concatenate)
       (= (gethash (vm-dst current) use-counts 0) 1)
       (eq (vm-str1 next) (vm-dst current))))

(defun %opt-pack-chain (rest use-counts)
  "Recursively pack linear vm-concatenate chains in REST into batched instructions."
  (when rest
    (let ((inst (car rest)))
      (if (typep inst 'vm-concatenate)
          (let ((parts   (%concat-parts inst))
                (dst     (vm-dst inst))
                (tail    (cdr rest))
                (current inst)
                (merged  nil))
            (loop while (and tail (%opt-concat-chain-p current (car tail) use-counts)) do
              (let ((next (car tail)))
                (setf merged t
                      parts (append parts (rest (%concat-parts next)))
                      dst (vm-dst next)
                      current next
                      tail (cdr tail))))
            (cons (if merged (%make-packed-concatenate dst parts) inst)
                  (%opt-pack-chain tail use-counts)))
          (cons inst (%opt-pack-chain (cdr rest) use-counts))))))

(defun opt-pass-batch-concatenate (instructions)
  "Pack linear vm-concatenate chains into one instruction with a PARTS list."
  (let ((use-counts (make-hash-table :test #'eq)))
    (dolist (inst instructions)
      (dolist (reg (opt-inst-read-regs inst))
        (incf (gethash reg use-counts 0))))
    (%opt-pack-chain instructions use-counts)))

;;; CSE, GVN, dead-label elimination, and leaf-function detection have been
;;; extracted to optimizer-cse-gvn.lisp (loads immediately after this file).
