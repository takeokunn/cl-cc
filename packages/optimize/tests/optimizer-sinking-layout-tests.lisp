;;;; tests/unit/optimize/optimizer-sinking-layout-tests.lisp
;;;; Unit tests for hot-cold layout, SLP vectorization, function outlining,
;;;; if-conversion, and code-sinking optimization passes.
;;;;
;;;; Covers: opt-pass-hot-cold-layout, opt-pass-slp-vectorize,
;;;;   opt-pass-function-outlining, opt-pass-if-conversion, opt-pass-code-sinking.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun %sinking-layout-test-label-position (instructions label-name)
  "Return LABEL-NAME's position in INSTRUCTIONS, or NIL."
  (position-if (lambda (inst)
                 (and (typep inst 'cl-cc/vm::vm-label)
                      (equal (cl-cc/vm::vm-name inst) label-name)))
               instructions))

(defun %sinking-layout-test-jump-zero-position (instructions target-label)
  "Return the position of a vm-jump-zero targeting TARGET-LABEL, or NIL."
  (position-if (lambda (inst)
                  (and (typep inst 'cl-cc/vm::vm-jump-zero)
                       (equal (cl-cc/vm::vm-label-name inst) target-label)))
                instructions))

(defun %make-slp-array-map (&key (op :add))
  "Build a four-lane straight-line scalar array map for SLP tests."
  (append
   (loop for i below 4
         collect (make-vm-const :dst (intern (format nil "I~D" i) :keyword) :value i))
   (loop for i below 4
         for idx = (intern (format nil "I~D" i) :keyword)
         for lhs = (intern (format nil "A~D" i) :keyword)
         for rhs = (intern (format nil "B~D" i) :keyword)
         for val = (intern (format nil "V~D" i) :keyword)
         append (list (cl-cc/vm::make-vm-aref :dst lhs :array-reg :array-a :index-reg idx)
                      (cl-cc/vm::make-vm-aref :dst rhs :array-reg :array-b :index-reg idx)
                      (ecase op
                        (:add (make-vm-add :dst val :lhs lhs :rhs rhs))
                        (:logxor (cl-cc/vm::make-vm-logxor :dst val :lhs lhs :rhs rhs)))
                      (cl-cc/vm::make-vm-aset :array-reg :array-c :index-reg idx :val-reg val)))))

;;; ─── opt-pass-function-outlining ──────────────────────────────────────────

(deftest function-outlining-outlines-duplicate-sequences
  "opt-pass-function-outlining replaces duplicate pure straight-line sequences with helper calls."
  (let* ((seq (list (make-vm-const :dst :r0 :value 1)
                    (make-vm-const :dst :r1 :value 2)
                    (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)))
         (insts (append seq seq (list (make-vm-ret :reg :r2))))
         (out (cl-cc/optimize::opt-pass-function-outlining insts)))
    (assert-= 2 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-call)) out))
    (assert-true
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-label)
                  (search cl-cc/optimize::*opt-outlined-label-prefix*
                          (cl-cc/vm::vm-name i))))
           out))))

(deftest function-outlining-leaves-nonduplicate-sequences-unchanged
  "opt-pass-function-outlining does not add helper labels when no duplicate sequence exists."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret :reg :r2)))
         (out (cl-cc/optimize::opt-pass-function-outlining insts)))
    (assert-equal (mapcar #'cl-cc/vm::instruction->sexp insts)
                  (mapcar #'cl-cc/vm::instruction->sexp out))
    (assert-false
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-label)
                  (search cl-cc/optimize::*opt-outlined-label-prefix*
                          (cl-cc/vm::vm-name i))))
            out))))

;;; ─── opt-pass-slp-vectorize ───────────────────────────────────────────────

(deftest-each slp-vectorize-packs-adjacent-lanes
  "FR-227 SLP packs four adjacent scalar array-map lanes into one vm-simd-vector-op."
  :cases (("add"    :add    'cl-cc/vm::vm-add)
          ("logxor" :logxor 'cl-cc/vm::vm-logxor))
  (op scalar-type)
  (let* ((insts (%make-slp-array-map :op op))
         (out (cl-cc/optimize:opt-pass-slp-vectorize insts))
         (simd (find-if (lambda (inst) (typep inst 'cl-cc/vm:vm-simd-vector-op)) out)))
    (assert-true simd)
    (assert-eq op (cl-cc/vm:vm-simd-vector-op-op simd))
    (ecase op
      (:add
       (assert-eq :array-a (cl-cc/vm:vm-simd-vector-op-lhs-array simd))
       (assert-eq :array-b (cl-cc/vm:vm-simd-vector-op-rhs-array simd))
       (assert-eq :array-c (cl-cc/vm:vm-simd-vector-op-dst-array simd))
       (assert-= 4 (cl-cc/vm:vm-simd-vector-op-lanes simd))
       (assert-false (some (lambda (inst) (typep inst 'cl-cc/vm:vm-aset)) out)))
      (:logxor t))
    (assert-false (some (lambda (inst) (typep inst scalar-type)) out))))

(deftest slp-vectorize-is-idempotent
  "Running FR-227 SLP twice leaves the SIMD-packed stream unchanged."
  (let* ((once (cl-cc/optimize:opt-pass-slp-vectorize (%make-slp-array-map :op :add)))
         (twice (cl-cc/optimize:opt-pass-slp-vectorize once)))
    (assert-equal (mapcar #'cl-cc/vm:instruction->sexp once)
                  (mapcar #'cl-cc/vm:instruction->sexp twice))))

;;; ─── opt-pass-if-conversion ───────────────────────────────────────────────

(deftest if-conversion-simple-diamond-emits-vm-select
  "opt-pass-if-conversion converts a simple if diamond into one vm-select."
  (let* ((insts (list (make-vm-lt :dst :c :lhs :r0 :rhs :r1)
                      (make-vm-jump-zero :reg :c :label "else")
                      (make-vm-move :dst :out :src :then)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "else")
                      (make-vm-move :dst :out :src :else)
                      (make-vm-label :name "join")
                      (make-vm-ret :reg :out)))
         (out (cl-cc/optimize::opt-pass-if-conversion insts))
         (selects (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-select)) out)))
    (assert-= 1 (length selects))
    (let ((sel (first selects)))
      (assert-eq :out (cl-cc/vm::vm-dst sel))
      (assert-eq :c (cl-cc/vm::vm-select-cond-reg sel))
      (assert-eq :then (cl-cc/vm::vm-select-then-reg sel))
      (assert-eq :else (cl-cc/vm::vm-select-else-reg sel)))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-lt)) out))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) out))))

(deftest if-conversion-skips-externally-referenced-diamond-label
  "opt-pass-if-conversion preserves diamonds whose internal labels have outside references."
  (let* ((insts (list (make-vm-jump :label "else")
                      (make-vm-lt :dst :c :lhs :r0 :rhs :r1)
                      (make-vm-jump-zero :reg :c :label "else")
                      (make-vm-move :dst :out :src :then)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "else")
                      (make-vm-move :dst :out :src :else)
                      (make-vm-label :name "join")
                      (make-vm-ret :reg :out)))
         (out (cl-cc/optimize::opt-pass-if-conversion insts)))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-select)) out))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out))))

;;; ─── opt-pass-hot-cold-layout ─────────────────────────────────────────────

(deftest hot-cold-layout-registers-before-dce
  "The hot/cold layout pass is registered in the convergence pipeline before DCE."
  (let ((layout-pos (position :hot-cold-layout cl-cc/optimize::*opt-default-convergence-pass-keys*))
        (dce-pos    (position :dce cl-cc/optimize::*opt-default-convergence-pass-keys*)))
    (assert-true (assoc :hot-cold-layout cl-cc/optimize::*opt-pass-table*))
    (assert-true layout-pos)
    (assert-true dce-pos)
    (assert-true (< layout-pos dce-pos))))

(deftest hot-cold-layout-keeps-conditional-fallthrough-contiguous
  "opt-pass-hot-cold-layout places the vm-jump-zero fall-through hot block immediately next."
  (let* ((insts (list (make-vm-const :dst :cond :value 1)
                      (make-vm-jump-zero :reg :cond :label "cold")
                      (make-vm-label :name "hot")
                      (make-vm-const :dst :r0 :value 42)
                      (make-vm-jump :label "end")
                      (make-vm-label :name "cold")
                      (make-vm-signal-error :error-reg :r0)
                      (make-vm-label :name "end")
                      (make-vm-ret :reg :r0)))
         (out      (cl-cc/optimize::opt-pass-hot-cold-layout insts))
         (jz-pos   (%sinking-layout-test-jump-zero-position out "cold"))
         (hot-pos  (%sinking-layout-test-label-position out "hot"))
         (cold-pos (%sinking-layout-test-label-position out "cold")))
    (assert-true jz-pos)
    (assert-true hot-pos)
    (assert-true cold-pos)
    (assert-= (1+ jz-pos) hot-pos)
    (assert-true (< hot-pos cold-pos))))

(deftest hot-cold-layout-moves-signal-block-to-tail
  "opt-pass-hot-cold-layout moves error/signalling cold blocks behind ordinary blocks."
  (let* ((insts (list (make-vm-const :dst :cond :value 1)
                      (make-vm-jump-zero :reg :cond :label "cold")
                      (make-vm-label :name "hot")
                      (make-vm-const :dst :r0 :value 7)
                      (make-vm-jump :label "end")
                      (make-vm-label :name "cold")
                      (make-vm-signal-error :error-reg :r0)
                      (make-vm-label :name "end")
                      (make-vm-ret :reg :r0)))
         (out      (cl-cc/optimize::opt-pass-hot-cold-layout insts))
         (end-pos  (%sinking-layout-test-label-position out "end"))
         (cold-pos (%sinking-layout-test-label-position out "cold")))
    (assert-true end-pos)
    (assert-true cold-pos)
    (assert-true (< end-pos cold-pos))))

(deftest hot-cold-layout-preserves-conditional-jump-target
  "opt-pass-hot-cold-layout preserves vm-jump-zero target labels after reordering."
  (let* ((insts (list (make-vm-const :dst :cond :value 1)
                      (make-vm-jump-zero :reg :cond :label "cold")
                      (make-vm-label :name "hot")
                      (make-vm-ret :reg :cond)
                      (make-vm-label :name "cold")
                      (make-vm-signal-error :error-reg :cond)))
         (out   (cl-cc/optimize::opt-pass-hot-cold-layout insts))
         (jz    (find-if (lambda (inst) (typep inst 'cl-cc/vm::vm-jump-zero)) out)))
    (assert-true jz)
    (assert-equal "cold" (cl-cc/vm::vm-label-name jz))
    (assert-true (%sinking-layout-test-label-position out "cold"))))

;;; ─── opt-pass-code-sinking ────────────────────────────────────────────────

(deftest-each code-sinking-moves-unique-use-value-past-label
  "opt-pass-code-sinking moves a uniquely-used instruction into its jump target block entry."
  :cases (("const" (list (make-vm-const :dst :r1 :value 42)
                          (make-vm-jump :label "Luse")
                          (make-vm-label :name "Ldead")
                          (make-vm-ret :reg :r0)
                          (make-vm-label :name "Luse")
                          (make-vm-add :dst :r2 :lhs :r1 :rhs :r0)
                          (make-vm-ret :reg :r2))
            'cl-cc/vm::vm-const :r1)
          ("cons"  (list (make-vm-cons :dst :pair :car-src :r0 :cdr-src :r1)
                          (make-vm-jump :label "Luse")
                          (make-vm-label :name "Ldead")
                          (make-vm-ret :reg :r0)
                          (make-vm-label :name "Luse")
                          (make-vm-car :dst :r2 :src :pair)
                          (make-vm-ret :reg :r2))
            'cl-cc/vm::vm-cons :pair))
  (insts value-type dst-reg)
  (let* ((out (cl-cc/optimize::opt-pass-code-sinking insts))
         (sinkable-pos (position-if (lambda (x)
                                      (and (typep x value-type)
                                           (eq (cl-cc/vm::vm-dst x) dst-reg)))
                                    out))
         (luse-pos (position-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-label)
                                       (equal (cl-cc/vm::vm-name x) "Luse")))
                                out)))
    (assert-true sinkable-pos)
    (assert-true luse-pos)
    (assert-true (> sinkable-pos luse-pos))))

(deftest-each code-sinking-preserves-multi-use-value-before-jump
  "opt-pass-code-sinking does not move an instruction whose result is read multiple times."
  :cases (("const" (list (make-vm-const :dst :r1 :value 7)
                          (make-vm-jump :label "Luse")
                          (make-vm-label :name "Luse")
                          (make-vm-add :dst :r2 :lhs :r1 :rhs :r0)
                          (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)
                          (make-vm-ret :reg :r3))
            'cl-cc/vm::vm-const :r1)
          ("cons"  (list (make-vm-cons :dst :pair :car-src :r0 :cdr-src :r1)
                          (make-vm-jump :label "Luse")
                          (make-vm-label :name "Luse")
                          (make-vm-car :dst :r2 :src :pair)
                          (make-vm-cdr :dst :r3 :src :pair)
                          (make-vm-ret :reg :r3))
            'cl-cc/vm::vm-cons :pair))
  (insts value-type dst-reg)
  (let* ((out (cl-cc/optimize::opt-pass-code-sinking insts))
         (val-pos (position-if (lambda (x)
                                 (and (typep x value-type)
                                      (eq (cl-cc/vm::vm-dst x) dst-reg)))
                               out))
         (jump-pos (position-if (lambda (x) (typep x 'cl-cc/vm::vm-jump)) out)))
    (assert-true val-pos)
    (assert-true jump-pos)
    (assert-true (< val-pos jump-pos))))

(deftest-each code-sinking-moves-arithmetic-and-move-into-target-block
  "opt-pass-code-sinking moves constant-operand arithmetic and constant-source moves to the use block."
  :cases (("add"  (list (make-vm-const :dst :a :value 2)
                         (make-vm-const :dst :b :value 3)
                         (make-vm-add   :dst :v :lhs :a :rhs :b)
                         (make-vm-jump  :label "Luse")
                         (make-vm-label :name "Luse")
                         (make-vm-ret   :reg :v))
            'cl-cc/vm::vm-add)
          ("move" (list (make-vm-const :dst :a :value 2)
                         (make-vm-move  :dst :v :src :a)
                         (make-vm-jump  :label "Luse")
                         (make-vm-label :name "Luse")
                         (make-vm-ret   :reg :v))
            'cl-cc/vm::vm-move))
  (insts moved-type)
  (let* ((out (cl-cc/optimize::opt-pass-code-sinking insts))
         (moved-pos (position-if (lambda (x)
                                   (and (typep x moved-type)
                                        (eq (cl-cc/optimize::opt-inst-dst x) :v)))
                                 out))
         (luse-pos (position-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-label)
                                       (equal (cl-cc/vm::vm-name x) "Luse")))
                                out)))
    (assert-true moved-pos)
    (assert-true luse-pos)
    (assert-true (> moved-pos luse-pos))))

(deftest code-sinking-does-not-sink-impure-random
  "opt-pass-code-sinking does not move side-effecting vm-random instructions."
  (let* ((insts (list (make-vm-const :dst :limit :value 10)
                      (cl-cc/vm::make-vm-random :dst :v :src :limit)
                      (make-vm-jump :label "Luse")
                      (make-vm-label :name "Luse")
                      (make-vm-ret :reg :v)))
         (out (cl-cc/optimize::opt-pass-code-sinking insts))
         (random-pos (position-if (lambda (x) (typep x 'cl-cc/vm::vm-random)) out))
         (jump-pos (position-if (lambda (x) (typep x 'cl-cc/vm::vm-jump)) out)))
    (assert-true random-pos)
    (assert-true jump-pos)
    (assert-true (< random-pos jump-pos))))

(deftest code-sinking-duplicates-cheap-const-into-conditional-targets
  "opt-pass-code-sinking duplicates a cheap const into both conditional successors when both use it."
  (let* ((insts (list (make-vm-const :dst :v :value 1)
                      (make-vm-jump-zero :reg :cond :label "Lzero")
                      (make-vm-label :name "Lnonzero")
                      (make-vm-add :dst :r1 :lhs :v :rhs :x)
                      (make-vm-ret :reg :r1)
                      (make-vm-label :name "Lzero")
                      (make-vm-add :dst :r2 :lhs :v :rhs :y)
                      (make-vm-ret :reg :r2)))
         (out (cl-cc/optimize::opt-pass-code-sinking insts))
         (const-count (count-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-const)
                                       (eq (cl-cc/vm::vm-dst x) :v)))
                                out)))
    (assert-true (<= 1 const-count 2))))

(deftest code-sinking-does-not-sink-slot-read-across-aliased-write
  "opt-pass-code-sinking does not move a slot read below an aliased slot write."
  (let* ((read  (cl-cc:make-vm-slot-read :dst :v :obj-reg :obj :slot-name 'x))
         (write (cl-cc:make-vm-slot-write :obj-reg :obj :slot-name 'x :value-reg :new))
         (insts (list (make-vm-cons :dst :obj :car-src :r0 :cdr-src :r1)
                      read
                      write
                      (make-vm-jump :label "Luse")
                      (make-vm-label :name "Luse")
                      (make-vm-ret :reg :v)))
         (out (cl-cc/optimize::opt-pass-code-sinking insts)))
    (assert-true (member read out :test #'eq))
    (assert-true (member write out :test #'eq))
    (assert-true (< (position read out :test #'eq)
                    (position write out :test #'eq)))))

(deftest code-sinking-sinks-slot-read-across-tbaa-disjoint-write
  "opt-pass-code-sinking uses TBAA to move a slot read across a disjoint-kind write."
  (let* ((read  (cl-cc:make-vm-slot-read :dst :v :obj-reg :obj :slot-name 'x))
         (write (cl-cc:make-vm-slot-write :obj-reg :arr :slot-name 'x :value-reg :new))
         (insts (list (make-vm-const :dst :n :value 4)
                      (make-vm-cons :dst :obj :car-src :r0 :cdr-src :r1)
                      (cl-cc:make-vm-make-array :dst :arr :size-reg :n
                                                :initial-element nil :fill-pointer nil
                                                :adjustable nil :element-type nil)
                      read
                      write
                      (make-vm-jump :label "Luse")
                      (make-vm-label :name "Luse")
                      (make-vm-ret :reg :v)))
         (out (cl-cc/optimize::opt-pass-code-sinking insts)))
    (assert-true (member read out :test #'eq))
    (assert-true (member write out :test #'eq))
    (assert-true (> (position read out :test #'eq)
                    (position write out :test #'eq)))))
