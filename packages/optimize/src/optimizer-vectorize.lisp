(in-package :cl-cc/optimize)
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
