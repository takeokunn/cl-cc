(in-package :cl-cc/optimize)
;;; ─── Alias Analysis + Memory Passes ─────────────────────────────────────────

(defun opt-heap-root-inst-p (inst)
  "Return T when INST produces a fresh heap-like object identity."
  (typep inst '(or vm-cons vm-make-array vm-closure vm-make-closure)))

(defparameter *opt-heap-root-kind-table*
  '((vm-cons . :cons)
    (vm-make-array . :array)
    (vm-closure . :closure)
    (vm-make-closure . :closure))
  "Maps heap-producing instruction types to symbolic heap kind keywords.")

(defun opt-heap-root-kind (inst)
  "Return a symbolic heap kind for fresh heap-producing INST."
  (loop for (type . kind) in *opt-heap-root-kind-table*
        when (typep inst type)
        return kind))

(defun %opt-build-root-map (instructions)
  "Build a conservative EQ hash-table mapping registers to their canonical heap root.

Fresh heap producers (opt-heap-root-inst-p) start a new root at their destination.
vm-move propagates the source root. Any other destination write kills the root fact."
  (let ((roots (make-hash-table :test #'eq)))
    (dolist (inst instructions roots)
      (typecase inst
        (vm-move
         (let ((dst (vm-move-dst inst)))
           (when dst
             (multiple-value-bind (root found-p)
                 (gethash (vm-move-src inst) roots)
               (if found-p
                   (setf (gethash dst roots) root)
                   (remhash dst roots))))))
        (vm-get-global
         (let ((dst (cl-cc/vm::vm-get-global-dst inst)))
           (when dst (remhash dst roots))))
        (vm-slot-read
         (let ((dst (cl-cc/vm::vm-slot-read-dst inst)))
           (when dst (remhash dst roots))))
        (t
         (let ((dst (opt-inst-dst inst)))
           (cond
             ((and dst (opt-heap-root-inst-p inst))
              (setf (gethash dst roots) dst))
             (dst
              (remhash dst roots)))))))))

;;; FR-017: Alias Analysis / Memory Disambiguation — Type-Based Alias Analysis (TBAA) determines when two heap references cannot alias, enabling stronger LICM/DSE
(defun opt-compute-heap-aliases (instructions)
  "Compute a conservative EQ hash-table reg -> canonical heap root.
This is a small FR-115 style oracle intended for downstream passes."
  (%opt-build-root-map instructions))

(defun opt-compute-heap-type-facts (instructions &optional alias-roots)
  "Compute conservative heap type facts for TBAA.

Returns an EQ hash-table mapping both fresh allocation roots and currently-known
register aliases to one of `:cons', `:array', or `:closure'.  The table is built
from the same root facts used by `opt-compute-heap-aliases', so downstream
callers can pass it directly to `opt-tbaa-must-not-alias-p'."
  (let ((roots (or alias-roots (opt-compute-heap-aliases instructions)))
        (facts (make-hash-table :test #'eq)))
    (dolist (inst instructions facts)
      (let ((dst (opt-inst-dst inst)))
        (when (and dst (opt-heap-root-inst-p inst))
          (let ((kind (opt-heap-root-kind inst)))
            (when kind
              (setf (gethash dst facts) kind))))))
    (maphash (lambda (reg root)
               (let ((kind (gethash root facts)))
                 (when kind
                   (setf (gethash reg facts) kind))))
             roots)
    facts))

(defun %opt-tbaa-kind (reg type-facts)
  "Return REG's symbolic TBAA kind from TYPE-FACTS, if known."
  (cond
    ((hash-table-p type-facts)
     (gethash reg type-facts))
    ((and (consp type-facts) (keywordp (first type-facts)))
     (getf type-facts reg))
    ((consp type-facts)
     (cdr (assoc reg type-facts :test #'eq)))))

(defun opt-tbaa-must-not-alias-p (obj1-reg obj2-reg type-facts)
  "Return T when TBAA type facts prove OBJ1-REG and OBJ2-REG cannot alias.

Heap identities with different concrete heap kinds (`:cons', `:array',
`:closure') are distinct runtime object families.  Unknown or equal kinds remain
conservative and return NIL."
  (let ((kind1 (%opt-tbaa-kind obj1-reg type-facts))
        (kind2 (%opt-tbaa-kind obj2-reg type-facts)))
    (and kind1 kind2 (not (eq kind1 kind2)))))

(defun opt-compute-points-to (instructions)
  "Compute conservative flow-sensitive points-to roots for INSTRUCTIONS.

This FR-018 helper intentionally models a single canonical fresh heap root per
register in straight-line code. Fresh heap producers create roots, vm-move
propagates them, and later non-heap writes kill stale facts. Branch joins and
field-sensitive object graphs remain out of scope for this helper."
  (opt-compute-heap-aliases instructions))

(defun opt-may-alias-with-tbaa-p (reg-a reg-b alias-roots type-facts)
  "Return T when REG-A and REG-B may alias after points-to and TBAA checks."
  (and (not (opt-tbaa-must-not-alias-p reg-a reg-b type-facts))
       (opt-may-alias-p reg-a reg-b alias-roots)))

(defun opt-memory-def-inst-p (inst)
  "Return T when INST defines memory state for the conservative Memory-SSA model."
  (typep inst '(or vm-set-global vm-slot-write vm-cons)))

(defun opt-memory-use-inst-p (inst)
  "Return T when INST reads memory state for the conservative Memory-SSA model."
  (typep inst '(or vm-get-global vm-slot-read)))

(defun %opt-memory-location-key (inst alias-roots)
  "Return a canonical memory location key for INST, or NIL when not modeled."
  (typecase inst
    (vm-set-global (list :global (cl-cc/vm::vm-set-global-name inst)))
    (vm-get-global (list :global (cl-cc/vm::vm-get-global-name inst)))
    (vm-slot-write (opt-slot-alias-key (cl-cc/vm::vm-slot-write-obj-reg inst)
                                       (cl-cc/vm::vm-slot-write-slot-name inst)
                                       alias-roots))
    (vm-slot-read (opt-slot-alias-key (cl-cc/vm::vm-slot-read-obj-reg inst)
                                      (cl-cc/vm::vm-slot-read-slot-name inst)
                                      alias-roots))
    (vm-cons (list :alloc (vm-dst inst)))
    (t nil)))
