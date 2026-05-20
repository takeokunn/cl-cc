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

(defun opt-memory-unknown-write-inst-p (inst)
  "Return T when INST may write arbitrary memory and invalidate alias facts."
  (or (typep inst '(or vm-call vm-tail-call vm-generic-call vm-apply))
      (eq (vm-inst-effect-kind inst) :unknown)))

(defun %opt-resolve-vm-type-symbol (type)
  "Return TYPE when it names a known class, or the matching VM package symbol.

Some roadmap-level memory op names are intentionally referenced before their VM
instruction structs exist.  Calling TYPEP with those symbols raises an unknown
type error on SBCL, so memory predicates resolve them defensively and simply
treat absent instruction families as non-matches."
  (or (and (find-class type nil) type)
      (multiple-value-bind (vm-type status)
          (find-symbol (symbol-name type) :cl-cc/vm)
        (and status (find-class vm-type nil) vm-type))))

(defun %opt-typep-any (inst types)
  "Return T when INST is of any known type in TYPES. Unknown type names are NIL."
  (some (lambda (type)
          (let ((resolved (%opt-resolve-vm-type-symbol type)))
            (and resolved (typep inst resolved))))
        types))

(defun opt-memory-write-inst-p (inst)
  "Return T when INST may modify memory relevant to load motion safety."
  (or (%opt-typep-any inst '(vm-set-global vm-slot-write
                             vm-rplaca vm-rplacd vm-aset vm-fill vm-svset
                             vm-set-fill-pointer vm-vector-push vm-vector-pop
                             vm-vector-push-extend vm-adjust-array
                             vm-bit-set vm-bit-and vm-bit-or vm-bit-xor vm-bit-not))
      (opt-memory-unknown-write-inst-p inst)))

(defun opt-memory-read-inst-p (inst)
  "Return T when INST reads memory and needs alias checks before code motion."
  (%opt-typep-any inst '(vm-get-global vm-slot-read vm-car vm-cdr
                         vm-aref vm-svref vm-row-major-aref vm-bit-access vm-sbit)))

(defun opt-memory-accesses-may-alias-p (read-inst write-inst alias-roots type-facts)
  "Return T when READ-INST may observe WRITE-INST.

The check is conservative for unknown calls/writes and uses heap alias roots plus
TBAA facts for modeled slot accesses.  Disjoint heap kinds are treated as
non-aliasing while unknown roots remain conservative."
  (cond
    ((opt-memory-unknown-write-inst-p write-inst) t)
    ((and (typep read-inst 'vm-get-global)
          (typep write-inst 'vm-set-global))
     (eql (cl-cc/vm::vm-get-global-name read-inst)
          (cl-cc/vm::vm-set-global-name write-inst)))
    ((and (typep read-inst 'vm-slot-read)
          (typep write-inst 'vm-slot-write))
     (and (eql (cl-cc/vm::vm-slot-read-slot-name read-inst)
               (cl-cc/vm::vm-slot-write-slot-name write-inst))
          (opt-may-alias-p (cl-cc/vm::vm-slot-read-obj-reg read-inst)
                           (cl-cc/vm::vm-slot-write-obj-reg write-inst)
                           alias-roots
                           type-facts)))
    ;; Cons field readers can be invalidated by rplaca/rplacd on the same object.
    ((and (typep read-inst 'vm-car)
          (typep write-inst 'vm-rplaca))
     (opt-may-alias-p (vm-src read-inst) (vm-cons-reg write-inst) alias-roots type-facts))
    ((and (typep read-inst 'vm-cdr)
          (typep write-inst 'vm-rplacd))
     (opt-may-alias-p (vm-src read-inst) (vm-cons-reg write-inst) alias-roots type-facts))
    ;; Array accesses are modeled by object identity but not by index here.
    ((and (typep read-inst 'vm-aref)
          (typep write-inst 'vm-aset))
     (opt-may-alias-p (vm-array-reg read-inst) (vm-array-reg write-inst) alias-roots type-facts))
    ((and (%opt-typep-any read-inst '(vm-svref vm-row-major-aref))
          (%opt-typep-any write-inst '(vm-svset)))
     (opt-may-alias-p (vm-lhs read-inst) (vm-array-reg write-inst) alias-roots type-facts))
    ;; A modeled write of another family cannot affect this modeled read.
    ((and (opt-memory-read-inst-p read-inst)
          (opt-memory-write-inst-p write-inst))
     nil)
    (t nil)))
