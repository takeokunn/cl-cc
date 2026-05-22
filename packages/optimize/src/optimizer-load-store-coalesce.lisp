;;;; packages/optimize/src/optimizer-load-store-coalesce.lisp — FR-723

(in-package :cl-cc/optimize)

(defconstant +opt-coalesce-lane-bits+ 8
  "Bit width of the narrow lane modeled by FR-723 load/store coalescing.

The current VM array/slot memory instructions do not carry an element-width
slot.  This pass therefore recognizes adjacent integer byte lanes and rewrites
them into naturally-aligned packed word operations.  When wider VM memory
opcodes grow an explicit width/unaligned marker, the access constructors below
are the integration point.")

(defconstant +opt-coalesce-max-lanes+ 8
  "Maximum number of adjacent byte lanes to widen in one rewrite.")

(defun %opt-lsc-power-of-two-p (n)
  "Return T when N is a positive power of two."
  (and (integerp n) (plusp n) (zerop (logand n (1- n)))))

(defun %opt-lsc-natural-alignment-p (offset lane-count)
  "Return T when a LANE-COUNT-byte access at OFFSET is naturally aligned.

The VM currently has no explicit unaligned load/store marker, so FR-723 refuses
unaligned groups instead of silently creating an access that a backend may be
unable to lower safely."
  (and (integerp offset)
       (%opt-lsc-power-of-two-p lane-count)
       (zerop (mod offset lane-count))))

(defun %opt-lsc-same-base-p (a b alias-roots)
  "Return T when base registers A and B definitely name the same memory object."
  (or (eql a b)
      (and alias-roots
           (hash-table-p alias-roots)
           (opt-must-alias-p a b alias-roots))))

(defun %opt-lsc-access (inst)
  "Describe a coalescible byte-lane memory access represented by INST.

Returns six values: KIND (:LOAD or :STORE), FAMILY (:ARRAY or :SLOT), BASE
register, integer OFFSET, result/value register, and the original instruction.
Only immediate integer offsets are accepted because adjacency and natural
alignment must be proven locally."
  (typecase inst
    (vm-aref
     (let ((offset (vm-index-reg inst)))
       (when (integerp offset)
         (values :load :array (vm-array-reg inst) offset (vm-dst inst) inst))))
    (vm-aset
     (let ((offset (vm-index-reg inst)))
       (when (integerp offset)
         (values :store :array (vm-array-reg inst) offset (vm-val-reg inst) inst))))
    (vm-slot-read
     (let ((offset (cl-cc/vm::vm-slot-read-slot-name inst)))
       (when (integerp offset)
         (values :load :slot (cl-cc/vm::vm-slot-read-obj-reg inst) offset
                 (cl-cc/vm::vm-slot-read-dst inst) inst))))
    (vm-slot-write
     (let ((offset (cl-cc/vm::vm-slot-write-slot-name inst)))
       (when (integerp offset)
         (values :store :slot (cl-cc/vm::vm-slot-write-obj-reg inst) offset
                 (cl-cc/vm::vm-slot-write-value-reg inst) inst))))))

(defun %opt-lsc-compatible-access-p (first next kind family expected-offset alias-roots)
  "Return T when NEXT extends FIRST's adjacent memory access group."
  (multiple-value-bind (next-kind next-family next-base next-offset)
      (%opt-lsc-access next)
    (multiple-value-bind (_ first-family first-base)
        (%opt-lsc-access first)
      (declare (ignore _))
      (and (eq next-kind kind)
           (eq next-family family)
           (= next-offset expected-offset)
           (%opt-lsc-same-base-p first-base next-base alias-roots)
           (eq first-family family)))))

(defun %opt-lsc-group-prefix (instructions alias-roots)
  "Return the longest legal adjacent load/store prefix from INSTRUCTIONS.

The returned group is limited to a naturally-aligned power-of-two byte width.
If the first two accesses cannot be widened/coalesced, NIL is returned."
  (multiple-value-bind (kind family _base start-offset)
      (%opt-lsc-access (first instructions))
    (declare (ignore _base))
    (when (and kind family)
      (let ((raw (list (first instructions)))
            (expected (1+ start-offset)))
        (loop for inst in (rest instructions)
              while (and (< (length raw) +opt-coalesce-max-lanes+)
                         (%opt-lsc-compatible-access-p (first raw) inst kind family expected alias-roots))
              do (push inst raw)
                 (incf expected))
        (let* ((ordered (nreverse raw))
               (best-len (loop for n from (length ordered) downto 2
                               when (and (%opt-lsc-power-of-two-p n)
                                         (%opt-lsc-natural-alignment-p start-offset n))
                                 return n)))
          (when best-len
            (subseq ordered 0 best-len)))))))

(defun %opt-lsc-make-load (family dst base offset original)
  "Create a widened load matching ORIGINAL's memory family."
  (declare (ignore original))
  (ecase family
    (:array (make-vm-aref :dst dst :array-reg base :index-reg offset))
    (:slot  (make-vm-slot-read :dst dst :obj-reg base :slot-name offset))))

(defun %opt-lsc-make-store (family base offset value original)
  "Create a coalesced store matching ORIGINAL's memory family."
  (declare (ignore original))
  (ecase family
    (:array (make-vm-aset :array-reg base :index-reg offset :val-reg value))
    (:slot  (make-vm-slot-write :obj-reg base :slot-name offset :value-reg value))))

(defun %opt-lsc-byte-mask (lane-count)
  "Return a mask covering LANE-COUNT packed byte lanes."
  (1- (ash 1 (* +opt-coalesce-lane-bits+ lane-count))))

(defun %opt-lsc-extract-loads (group fresh)
  "Rewrite GROUP of adjacent loads into one wider load plus byte extractions."
  (multiple-value-bind (_ family base start-offset _value first-inst)
      (%opt-lsc-access (first group))
    (declare (ignore _ _value))
    (let* ((wide-reg (funcall fresh))
           (mask-reg (funcall fresh))
           (out (list (%opt-lsc-make-load family wide-reg base start-offset first-inst)
                      (make-vm-const :dst mask-reg :value #xff))))
      (loop for inst in group
            for lane from 0
            do (multiple-value-bind (_kind _family _base _offset dst)
                   (%opt-lsc-access inst)
                 (declare (ignore _kind _family _base _offset))
                 (let ((source-reg wide-reg))
                   (when (plusp lane)
                     (let ((shift-reg (funcall fresh))
                           (shifted-reg (funcall fresh)))
                       (push (make-vm-const :dst shift-reg
                                            :value (- (* lane +opt-coalesce-lane-bits+)))
                             out)
                       (push (make-vm-ash :dst shifted-reg :lhs wide-reg :rhs shift-reg) out)
                       (setf source-reg shifted-reg)))
                   (push (make-vm-logand :dst dst :lhs source-reg :rhs mask-reg) out))))
      (nreverse out))))

(defun %opt-lsc-pack-stores (group fresh)
  "Rewrite GROUP of adjacent stores into one packed wider store."
  (multiple-value-bind (_ family base start-offset _value first-inst)
      (%opt-lsc-access (first group))
    (declare (ignore _ _value))
    (let ((mask-reg (funcall fresh))
          (packed-reg nil)
          (out nil))
      (push (make-vm-const :dst mask-reg :value #xff) out)
      (loop for inst in group
            for lane from 0
            do (multiple-value-bind (_kind _family _base _offset value-reg)
                   (%opt-lsc-access inst)
                 (declare (ignore _kind _family _base _offset))
                 (let ((masked-reg (funcall fresh)))
                   (push (make-vm-logand :dst masked-reg :lhs value-reg :rhs mask-reg) out)
                   (let ((lane-reg masked-reg))
                     (when (plusp lane)
                       (let ((shift-reg (funcall fresh))
                             (shifted-reg (funcall fresh)))
                         (push (make-vm-const :dst shift-reg
                                              :value (* lane +opt-coalesce-lane-bits+))
                               out)
                         (push (make-vm-ash :dst shifted-reg :lhs masked-reg :rhs shift-reg) out)
                         (setf lane-reg shifted-reg)))
                     (if packed-reg
                         (let ((next-packed (funcall fresh)))
                           (push (make-vm-logior :dst next-packed :lhs packed-reg :rhs lane-reg) out)
                           (setf packed-reg next-packed))
                         (setf packed-reg lane-reg))))))
      (push (%opt-lsc-make-store family base start-offset packed-reg first-inst) out)
      (nreverse out))))

(defun opt-load-widening-candidate-p (a b &optional alias-roots)
  "Return true when A and B are adjacent loads eligible for FR-723 widening.

A candidate is two byte-lane loads from the same memory family and definitely
the same base object, with integer offsets N and N+1.  The two-byte widened
access must be naturally aligned because the current VM has no unaligned access
marker."
  (multiple-value-bind (kind-a family-a base-a offset-a)
      (%opt-lsc-access a)
    (multiple-value-bind (kind-b family-b base-b offset-b)
        (%opt-lsc-access b)
      (and (eq kind-a :load)
           (eq kind-b :load)
           (eq family-a family-b)
           (= offset-b (1+ offset-a))
           (%opt-lsc-same-base-p base-a base-b alias-roots)
           (%opt-lsc-natural-alignment-p offset-a 2)))))

(defun opt-pass-store-coalescing (instructions &key alias-roots)
  "Combine adjacent narrow stores into packed naturally-aligned wider stores.

This is store coalescing, not dead-store elimination: no overwritten store is
dropped.  Consecutive byte stores to offsets N, N+1, ... are packed with
LOGAND/ASH/LOGIOR and replaced by one store at offset N when the resulting
power-of-two byte access is naturally aligned.  Groups are never formed across
intervening instructions, so alias boundaries and side effects remain intact."
  (let ((fresh (%opt-fresh-register-generator instructions))
        (aliases (or alias-roots (opt-compute-heap-aliases instructions))))
    (labels ((walk (rest acc changed)
               (cond
                 ((endp rest) (values (nreverse acc) changed))
                 (t
                  (let ((group (%opt-lsc-group-prefix rest aliases)))
                    (if (and group (eq (nth-value 0 (%opt-lsc-access (first group))) :store))
                        (walk (nthcdr (length group) rest)
                              (nreconc (%opt-lsc-pack-stores group fresh) acc)
                              t)
                        (walk (cdr rest) (cons (first rest) acc) changed)))))))
      (walk instructions nil nil))))

(defun opt-pass-load-widening-store-coalescing (instructions &key alias-roots)
  "Run FR-723 adjacent load widening and adjacent store coalescing.

Load widening replaces adjacent byte loads with one wider memory read and a set
of extraction instructions that preserve the original destination registers.
Store coalescing replaces adjacent byte stores with packing instructions and one
wider memory write.  Both rewrites operate on VM/MIR memory instructions
(`vm-aref'/`vm-aset' and `vm-slot-read'/`vm-slot-write'), require immediate
integer offsets, use alias-analysis roots when supplied, and reject unaligned
groups until the VM grows an explicit unaligned access marker."
  (let ((fresh (%opt-fresh-register-generator instructions))
        (aliases (or alias-roots (opt-compute-heap-aliases instructions))))
    (labels ((walk (rest acc changed)
               (cond
                 ((endp rest) (values (nreverse acc) changed))
                 (t
                  (let ((group (%opt-lsc-group-prefix rest aliases)))
                    (if group
                        (multiple-value-bind (kind)
                            (%opt-lsc-access (first group))
                          (ecase kind
                            (:load
                             (walk (nthcdr (length group) rest)
                                   (nreconc (%opt-lsc-extract-loads group fresh) acc)
                                   t))
                            (:store
                             (walk (nthcdr (length group) rest)
                                   (nreconc (%opt-lsc-pack-stores group fresh) acc)
                                   t))))
                        (walk (cdr rest) (cons (first rest) acc) changed)))))))
      (walk instructions nil nil))))
