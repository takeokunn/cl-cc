;;;; tests/unit/optimize/optimizer-memory-pass-tests.lisp
;;;; Unit tests for optimizer-memory.lisp — alias queries, store/load passes
;;;;
;;;; Covers: opt-must-alias-p, opt-may-alias-p, opt-slot-alias-key,
;;;;   opt-rewrite-inst-regs, opt-pass-dead-store-elim,
;;;;   opt-pass-store-to-load-forward, opt-pass-cons-slot-forward,
;;;;   mem-pass-state helpers, *opt-heap-root-kind-table* integrity,
;;;;   *opt-interval-binop-table*, %mps-pending-uses-reg-p,
;;;;   %mps-flush-if-src-overwritten, %mps-flush-dependent-on-reg.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── opt-must-alias-p / opt-may-alias-p ─────────────────────────────────

(defun %make-alias-table (&rest entries)
  "Build a :eq hash-table from alternating key/value ENTRIES."
  (let ((ht (make-hash-table :test #'eq)))
    (loop for (k v) on entries by #'cddr do (setf (gethash k ht) v))
    ht))

(deftest-each must-alias-cases
  "opt-must-alias-p: true iff both registers share the same canonical root."
  :cases (("same-root"    :r0 :r1  '(:r0 :root0 :r1 :root0) t)
          ("diff-roots"   :r0 :r1  '(:r0 :root0 :r1 :root1) nil)
          ("unknown-root" :r0 :r1  '(:r0 :root0)             nil))
  (reg-a reg-b entries expected)
  (let ((alias (apply #'%make-alias-table entries)))
    (if expected
        (assert-true  (cl-cc/optimize:opt-must-alias-p reg-a reg-b alias))
        (assert-false (cl-cc/optimize:opt-must-alias-p reg-a reg-b alias)))))

(deftest-each may-alias-cases
  "opt-may-alias-p: conservatively true when a register is unknown or shares root."
  :cases (("same-root"        :r0 :r1  '(:r0 :root0 :r1 :root0) t)
          ("unknown-register" :r0 :r99 '(:r0 :root0)             t)
          ("diff-known-roots" :r0 :r1  '(:r0 :root0 :r1 :root1)  nil))
  (reg-a reg-b entries expected)
  (let ((alias (apply #'%make-alias-table entries)))
    (if expected
        (assert-true  (cl-cc/optimize:opt-may-alias-p reg-a reg-b alias))
        (assert-false (cl-cc/optimize:opt-may-alias-p reg-a reg-b alias)))))

;;; ─── opt-slot-alias-key / opt-rewrite-inst-regs ────────────────────────

(deftest slot-alias-key-uses-canonical-root-when-known
  "opt-slot-alias-key returns a key using the canonical root when the register is in the alias map."
  (let ((alias (make-hash-table :test #'eq)))
    (setf (gethash :r0 alias) :root0)
    (assert-equal '(:slot :root0 x) (cl-cc/optimize::opt-slot-alias-key :r0 'x alias))))

(deftest slot-alias-key-falls-back-to-register-when-unknown
  "opt-slot-alias-key returns a key using the register itself when not in the alias map."
  (let ((alias (make-hash-table :test #'eq)))
    (assert-equal '(:slot :r5 y) (cl-cc/optimize::opt-slot-alias-key :r5 'y alias))))

(deftest rewrite-inst-regs-substitutes-source-registers
  "opt-rewrite-inst-regs substitutes known source registers; dst is never rewritten."
  (let* ((copies (make-hash-table :test #'eq)))
    (setf (gethash :r0 copies) :r5)
    (let ((inst (cl-cc/optimize::opt-rewrite-inst-regs (make-vm-add :dst :r2 :lhs :r0 :rhs :r1) copies)))
      (assert-eq :r2 (cl-cc/vm::vm-dst inst))
      (assert-eq :r5 (cl-cc/vm::vm-lhs inst)))))

(deftest rewrite-inst-regs-never-rewrites-dst
  "opt-rewrite-inst-regs leaves dst unchanged even when the register appears in the copy map."
  (let* ((copies (make-hash-table :test #'eq)))
    (setf (gethash :r0 copies) :r99)
    (let ((inst (cl-cc/optimize::opt-rewrite-inst-regs (make-vm-add :dst :r0 :lhs :r1 :rhs :r2) copies)))
      (assert-eq :r0 (cl-cc/vm::vm-dst inst)))))

(deftest rewrite-inst-regs-no-op-on-empty-copy-map
  "opt-rewrite-inst-regs leaves source registers unchanged when the copy map is empty."
  (let ((inst (cl-cc/optimize::opt-rewrite-inst-regs
               (make-vm-move :dst :r0 :src :r1)
               (make-hash-table :test #'eq))))
    (assert-eq :r1 (cl-cc/vm::vm-src inst))))

;;; ─── opt-pass-dead-store-elim ────────────────────────────────────────────

(defun %count-insts-of-type (result type-pred)
  "Count instructions in RESULT matching TYPE-PRED."
  (count-if type-pred result))

(deftest dead-store-elim-overwrite-without-read-removes-earlier-store
  "opt-pass-dead-store-elim removes a set-global that is overwritten without an intervening read."
  (let* ((result (cl-cc/optimize::opt-pass-dead-store-elim
                  (list (make-vm-const :dst :r0 :value 1)
                        (make-vm-set-global :src :r0 :name 'g)
                        (make-vm-const :dst :r1 :value 2)
                        (make-vm-set-global :src :r1 :name 'g)
                        (make-vm-ret :reg :r1)))))
    (assert-= 1 (%count-insts-of-type result #'vm-set-global-p))))

(deftest dead-store-elim-read-between-stores-preserves-both
  "opt-pass-dead-store-elim keeps a set-global that is read before the next overwrite."
  (let* ((result (cl-cc/optimize::opt-pass-dead-store-elim
                  (list (make-vm-const :dst :r0 :value 1)
                        (make-vm-set-global :src :r0 :name 'g)
                        (make-vm-get-global :dst :r1 :name 'g)
                        (make-vm-ret :reg :r1)))))
    (assert-= 1 (%count-insts-of-type result #'vm-set-global-p))
    (assert-= 1 (%count-insts-of-type result #'vm-get-global-p))))

(deftest dead-store-elim-store-reaching-ret-is-preserved
  "opt-pass-dead-store-elim preserves a set-global that reaches the terminator."
  (let* ((result (cl-cc/optimize::opt-pass-dead-store-elim
                  (list (make-vm-const :dst :r0 :value 42)
                        (make-vm-set-global :src :r0 :name 'result)
                        (make-vm-ret :reg :r0)))))
    (assert-= 1 (%count-insts-of-type result #'vm-set-global-p))))

;;; ─── opt-pass-store-to-load-forward ──────────────────────────────────────

(deftest store-to-load-forward-prior-store-replaces-get-global
  "opt-pass-store-to-load-forward replaces vm-get-global with vm-move when a preceding set-global is pending."
  (let* ((result (cl-cc/optimize::opt-pass-store-to-load-forward
                  (list (make-vm-const :dst :r0 :value 10)
                        (make-vm-set-global :src :r0 :name 'x)
                        (make-vm-get-global :dst :r1 :name 'x)
                        (make-vm-ret :reg :r1)))))
    (assert-true  (some (lambda (i) (typep i 'cl-cc/vm::vm-move))       result))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-get-global)) result))))

(deftest store-to-load-forward-no-prior-store-preserves-get-global
  "opt-pass-store-to-load-forward leaves vm-get-global intact when no prior store exists."
  (let* ((result (cl-cc/optimize::opt-pass-store-to-load-forward
                  (list (make-vm-get-global :dst :r0 :name 'unknown)
                        (make-vm-ret :reg :r0)))))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-get-global)) result))))

(deftest store-to-load-forward-prior-slot-write-replaces-slot-read
  "opt-pass-store-to-load-forward replaces a matching vm-slot-read with a move from the pending slot write value."
  (let* ((result (cl-cc/optimize::opt-pass-store-to-load-forward
                  (list (cl-cc:make-vm-slot-write :obj-reg :obj :slot-name 'slot :value-reg :value)
                        (cl-cc:make-vm-slot-read  :dst :out :obj-reg :obj :slot-name 'slot)
                        (make-vm-ret :reg :out)))))
    (assert-true
     (some (lambda (i)
              (and (typep i 'cl-cc/vm::vm-move)
                   (eq :out (cl-cc/vm::vm-dst i))
                   (eq :value (cl-cc/vm::vm-src i))))
           result))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-slot-read)) result))))

(deftest store-to-load-forward-cross-block-dominating-store-replaces-get-global
  "A dominating store remains forwardable across a branch/join when every incoming path agrees on the same fact."
  (let* ((result (cl-cc/optimize::opt-pass-store-to-load-forward
                  (list (make-vm-const :dst :r0 :value 10)
                        (make-vm-set-global :src :r0 :name 'x)
                        (make-vm-jump-zero :reg :r9 :label "else")
                        (make-vm-const :dst :r7 :value 1)
                        (make-vm-jump :label "join")
                        (make-vm-label :name "else")
                        (make-vm-const :dst :r8 :value 2)
                        (make-vm-label :name "join")
                        (make-vm-get-global :dst :r1 :name 'x)
                        (make-vm-ret :reg :r1)))))
    (assert-true
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-move)
                  (eq :r1 (cl-cc/vm::vm-dst i))
                  (eq :r0 (cl-cc/vm::vm-src i))))
           result))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-get-global)) result))))

(deftest store-to-load-forward-join-disagree-preserves-get-global
  "A join does not forward when predecessor paths disagree on the last store for the same location."
  (let* ((load (make-vm-get-global :dst :r2 :name 'x))
         (result (cl-cc/optimize::opt-pass-store-to-load-forward
                  (list (make-vm-jump-zero :reg :r9 :label "else")
                        (make-vm-const :dst :r0 :value 1)
                        (make-vm-set-global :src :r0 :name 'x)
                        (make-vm-jump :label "join")
                        (make-vm-label :name "else")
                        (make-vm-const :dst :r1 :value 2)
                        (make-vm-set-global :src :r1 :name 'x)
                        (make-vm-label :name "join")
                        load
                        (make-vm-ret :reg :r2)))))
    (assert-true (member load result :test #'eq))
    (assert-false
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-move)
                  (eq :r2 (cl-cc/vm::vm-dst i))))
            result))))

(deftest store-to-load-forward-cross-block-dominating-slot-write-replaces-slot-read
  "A dominating slot write is forwardable across branch/join when incoming paths agree."
  (let* ((result (cl-cc/optimize::opt-pass-store-to-load-forward
                  (list (cl-cc:make-vm-slot-write :obj-reg :obj :slot-name 'slot :value-reg :val)
                        (make-vm-jump-zero :reg :r9 :label "else")
                        (make-vm-const :dst :r7 :value 1)
                        (make-vm-jump :label "join")
                        (make-vm-label :name "else")
                        (make-vm-const :dst :r8 :value 2)
                        (make-vm-label :name "join")
                        (cl-cc:make-vm-slot-read :dst :out :obj-reg :obj :slot-name 'slot)
                        (make-vm-ret :reg :out)))))
    (assert-true
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-move)
                  (eq :out (cl-cc/vm::vm-dst i))
                  (eq :val (cl-cc/vm::vm-src i))))
           result))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-slot-read)) result))))

(deftest store-to-load-forward-branch-constant-prunes-infeasible-edge
  "When branch condition is locally constant, infeasible join edge is pruned for forwarding.

Path shape:
  set x=1
  cond=0
  jump-zero cond -> else
  then: set x=2 (infeasible)
  else: (no overwrite)
  join: get x

Without path refinement, join disagrees (x=1 vs x=2) and load is preserved.
With refinement, infeasible then-edge is excluded and load forwards from x=1."
  (let* ((result (cl-cc/optimize::opt-pass-store-to-load-forward
                  (list (make-vm-const :dst :r0 :value 1)
                        (make-vm-set-global :src :r0 :name 'x)
                        (make-vm-const :dst :rc :value 0)
                        (make-vm-jump-zero :reg :rc :label "else")
                        (make-vm-const :dst :r1 :value 2)
                        (make-vm-set-global :src :r1 :name 'x)
                        (make-vm-jump :label "join")
                        (make-vm-label :name "else")
                        (make-vm-label :name "join")
                        (make-vm-get-global :dst :r2 :name 'x)
                        (make-vm-ret :reg :r2)))))
    (assert-true
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-move)
                  (eq :r2 (cl-cc/vm::vm-dst i))
                  (eq :r0 (cl-cc/vm::vm-src i))))
           result))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-get-global)) result))))

;;; ─── opt-pass-cons-slot-forward ──────────────────────────────────────────

(deftest cons-slot-forward-replaces-car-with-original-car-register
  "opt-pass-cons-slot-forward rewrites car of a fresh cons to a move from car-src."
  (let* ((result (cl-cc/optimize::opt-pass-cons-slot-forward
                  (list (make-vm-cons :dst :cell :car-src :head :cdr-src :tail)
                        (make-vm-car  :dst :out  :src :cell))))
         (move (second result)))
    (assert-true (typep move 'cl-cc/vm::vm-move))
    (assert-eq :out (cl-cc/vm::vm-dst move))
    (assert-eq :head (cl-cc/vm::vm-src move))))

(deftest cons-slot-forward-replaces-cdr-through-move-alias
  "opt-pass-cons-slot-forward propagates the fresh cons fact through vm-move aliases."
  (let* ((result (cl-cc/optimize::opt-pass-cons-slot-forward
                  (list (make-vm-cons :dst :cell :car-src :head :cdr-src :tail)
                        (make-vm-move :dst :alias :src :cell)
                        (make-vm-cdr  :dst :out   :src :alias))))
         (move (third result)))
    (assert-true (typep move 'cl-cc/vm::vm-move))
    (assert-eq :out (cl-cc/vm::vm-dst move))
    (assert-eq :tail (cl-cc/vm::vm-src move))))

(deftest cons-slot-forward-source-overwrite-kills-fact
  "Overwriting a source register kills dependent cons-slot facts before forwarding."
  (let* ((result (cl-cc/optimize::opt-pass-cons-slot-forward
                  (list (make-vm-cons  :dst :cell :car-src :head :cdr-src :tail)
                        (make-vm-const :dst :head :value 99)
                        (make-vm-car   :dst :out  :src :cell)))))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-car)) result))
    (assert-false (some (lambda (i)
                          (and (typep i 'cl-cc/vm::vm-move)
                               (eq :out (cl-cc/vm::vm-dst i))))
                        result))))

(deftest cons-slot-forward-rplaca-kills-fact
  "Destructive cons mutation prevents stale car/cdr forwarding."
  (let* ((result (cl-cc/optimize::opt-pass-cons-slot-forward
                  (list (make-vm-cons   :dst :cell :car-src :head :cdr-src :tail)
                        (make-vm-rplaca :cons :cell :val :new-head)
                        (make-vm-car    :dst :out :src :cell)))))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-car)) result))))

(deftest cons-slot-forward-cons-overwriting-source-is-conservative
  "When vm-cons overwrites a slot source, the old slot value has no forwarding register."
  (let* ((result (cl-cc/optimize::opt-pass-cons-slot-forward
                  (list (make-vm-cons :dst :head :car-src :head :cdr-src :tail)
                        (make-vm-car  :dst :out  :src :head)))))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-car)) result))))

;;; ─── mem-pass-state struct helpers ──────────────────────────────────────────

(deftest mps-emit-pushes-instruction-onto-result
  "%mps-emit appends an instruction to the state's result list."
  (let ((state (cl-cc/optimize::make-mem-pass-state))
        (inst  (make-vm-const :dst :r0 :value 1)))
    (cl-cc/optimize::%mps-emit state inst)
    (assert-= 1 (length (cl-cc/optimize::mps-result state)))
    (assert-eq inst (first (cl-cc/optimize::mps-result state)))))

(deftest mps-remember-store-then-flush-one-emits-stored-instruction
  "%mps-flush-one emits the instruction registered by %mps-remember-store."
  (let ((state (cl-cc/optimize::make-mem-pass-state))
        (inst  (make-vm-const :dst :r0 :value 7)))
    (cl-cc/optimize::%mps-remember-store state :some-key inst)
    (cl-cc/optimize::%mps-flush-one state :some-key)
    (assert-= 1 (length (cl-cc/optimize::mps-result state)))
    (assert-eq inst (first (cl-cc/optimize::mps-result state)))
    (assert-null (cl-cc/optimize::%mps-pending-store state :some-key))))

(deftest mps-flush-all-emits-all-pending-stores
  "%mps-flush-all emits all pending stores and clears the pending table."
  (let ((state (cl-cc/optimize::make-mem-pass-state))
        (i1    (make-vm-const :dst :r0 :value 1))
        (i2    (make-vm-const :dst :r1 :value 2)))
    (cl-cc/optimize::%mps-remember-store state :k1 i1)
    (cl-cc/optimize::%mps-remember-store state :k2 i2)
    (cl-cc/optimize::%mps-flush-all state)
    (assert-= 2 (length (cl-cc/optimize::mps-result state)))
    (assert-= 0 (hash-table-count (cl-cc/optimize::mps-pending-by-name state)))))

(deftest mps-drop-pending-removes-key-from-pending
  "%mps-drop-pending removes the key from both pending-by-name and pending-order."
  (let ((state (cl-cc/optimize::make-mem-pass-state))
        (inst  (make-vm-const :dst :r0 :value 1)))
    (cl-cc/optimize::%mps-remember-store state :key inst)
    (cl-cc/optimize::%mps-drop-pending state :key)
    (assert-null (cl-cc/optimize::%mps-pending-store state :key))
    (assert-null (cl-cc/optimize::mps-pending-order state))))

;;; ─── *opt-heap-root-kind-table* data integrity ───────────────────────────

(deftest heap-root-kind-table-integrity-and-lookup
  "*opt-heap-root-kind-table* has 4 entries; opt-heap-root-kind dispatches correctly; non-heap returns nil."
  (assert-null (cl-cc/optimize::opt-heap-root-kind (make-vm-const :dst :r0 :value 1)))
  (assert-= 4 (length cl-cc/optimize::*opt-heap-root-kind-table*))
  (assert-eq :cons    (cdr (assoc 'vm-cons          cl-cc/optimize::*opt-heap-root-kind-table*)))
  (assert-eq :array   (cdr (assoc 'vm-make-array    cl-cc/optimize::*opt-heap-root-kind-table*)))
  (assert-eq :closure (cdr (assoc 'vm-closure       cl-cc/optimize::*opt-heap-root-kind-table*)))
  (assert-eq :closure (cdr (assoc 'vm-make-closure  cl-cc/optimize::*opt-heap-root-kind-table*)))
  (let ((inst (make-vm-closure :dst :r0 :label "f" :params '(x) :captured nil)))
    (assert-eq :closure (cl-cc/optimize::opt-heap-root-kind inst))))

;;; ─── *opt-interval-binop-table* / %opt-update-interval-binop ────────────

(deftest interval-binop-table-coverage
  "*opt-interval-binop-table* covers generic and fixnum add/sub/mul variants."
  (assert-= 6 (length cl-cc/optimize::*opt-interval-binop-table*))
  (assert-true (assoc 'vm-add cl-cc/optimize::*opt-interval-binop-table*))
  (assert-true (assoc 'vm-integer-add cl-cc/optimize::*opt-interval-binop-table*))
  (assert-true (assoc 'vm-sub cl-cc/optimize::*opt-interval-binop-table*))
  (assert-true (assoc 'vm-integer-sub cl-cc/optimize::*opt-interval-binop-table*))
  (assert-true (assoc 'vm-mul cl-cc/optimize::*opt-interval-binop-table*))
  (assert-true (assoc 'vm-integer-mul cl-cc/optimize::*opt-interval-binop-table*)))

(deftest-each update-interval-binop-cases
  "%opt-update-interval-binop: known operands → updates dst; unknown operand → kills dst."
  :cases (("add-known"   'opt-interval-add 2 3 5 t)
          ("sub-known"   'opt-interval-sub 5 2 3 t)
          ("add-unknown" 'opt-interval-add nil nil nil nil))
  (fn-sym lhs-val rhs-val expected-lo expected-found)
  (let ((intervals (make-hash-table :test #'eq))
        (inst      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)))
    (when lhs-val
      (setf (gethash :r0 intervals) (cl-cc/optimize::opt-make-interval lhs-val lhs-val)))
    (when rhs-val
      (setf (gethash :r1 intervals) (cl-cc/optimize::opt-make-interval rhs-val rhs-val)))
    (cl-cc/optimize::%opt-update-interval-binop
     inst intervals (symbol-function fn-sym))
     (if expected-found
         (assert-= expected-lo
                   (cl-cc/optimize::opt-interval-lo (gethash :r2 intervals)))
         (assert-false (nth-value 1 (gethash :r2 intervals))))))

(deftest optimize-instructions-rewrites-logand-one-eq-zero-to-evenp
  "The fold pipeline recognizes low-bit equality tests and rewrites them to vm-evenp."
  (let* ((result (cl-cc/optimize:optimize-instructions
                  (list (make-vm-get-global :dst :x :name 'x)
                        (make-vm-const :dst :one :value 1)
                        (make-vm-logand :dst :bit :lhs :x :rhs :one)
                        (make-vm-const :dst :zero :value 0)
                        (make-vm-num-eq :dst :pred :lhs :bit :rhs :zero)
                        (make-vm-ret :reg :pred))
                  :max-iterations 1
                  :pass-pipeline '(:fold :dce)))
         (even-inst (find-if (lambda (inst) (typep inst 'cl-cc/vm::vm-evenp)) result)))
    (assert-true even-inst)
    (assert-eq :pred (cl-cc/vm::vm-dst even-inst))
    (assert-eq :x (cl-cc/vm::vm-src even-inst))
    (assert-false (some (lambda (inst) (typep inst 'cl-cc/vm::vm-logand)) result))
    (assert-false (some (lambda (inst) (typep inst 'cl-cc/vm::vm-num-eq)) result))))

(deftest overflow-check-elim-rewrites-proven-8-bit-add-to-unchecked-integer-add
  "Bit-width range facts let the optimizer remove a checked add when overflow is impossible."
  (let* ((result (cl-cc/optimize:optimize-instructions
                  (list (make-vm-get-global :dst :x :name 'x)
                        (make-vm-get-global :dst :y :name 'y)
                        (make-vm-const :dst :mask :value #xFF)
                        (make-vm-logand :dst :x8 :lhs :x :rhs :mask)
                        (make-vm-logand :dst :y8 :lhs :y :rhs :mask)
                        (cl-cc:make-vm-add-checked :dst :sum :lhs :x8 :rhs :y8)
                        (make-vm-ret :reg :sum))
                  :max-iterations 1
                  :pass-pipeline '(:overflow-check-elim)))
         (unchecked (find-if (lambda (inst) (typep inst 'cl-cc/vm::vm-integer-add)) result)))
    (assert-true unchecked)
    (assert-eq :sum (cl-cc/vm::vm-dst unchecked))
    (assert-eq :x8 (cl-cc/vm::vm-lhs unchecked))
    (assert-eq :y8 (cl-cc/vm::vm-rhs unchecked))
    (assert-false (some (lambda (inst) (typep inst 'cl-cc/vm::vm-add-checked)) result))))

;;; ─── %mps-pending-uses-reg-p ─────────────────────────────────────────────

(deftest-each mps-pending-uses-reg-p-cases
  "%mps-pending-uses-reg-p: T when pending store reads the register; NIL otherwise."
  :cases (("set-global-src-match"
           (make-vm-set-global :src :r0 :name 'g) :r0 t)
          ("set-global-no-match"
           (make-vm-set-global :src :r1 :name 'g) :r0 nil)
          ("slot-write-obj-match"
           (make-vm-slot-write :obj-reg :r0 :slot-name 's :value-reg :r2) :r0 t)
          ("slot-write-value-match"
           (make-vm-slot-write :obj-reg :r1 :slot-name 's :value-reg :r0) :r0 t)
          ("slot-write-no-match"
           (make-vm-slot-write :obj-reg :r1 :slot-name 's :value-reg :r2) :r0 nil))
  (inst reg expected)
  (if expected
      (assert-true  (cl-cc/optimize::%mps-pending-uses-reg-p inst reg))
      (assert-false (cl-cc/optimize::%mps-pending-uses-reg-p inst reg))))

;;; ─── %mps-flush-if-src-overwritten ──────────────────────────────────────────

(deftest mps-flush-if-src-overwritten-matching-reg-triggers-flush
  "%mps-flush-if-src-overwritten emits a pending store when its src register is overwritten."
  (let ((state (cl-cc/optimize::make-mem-pass-state))
        (sg    (make-vm-set-global :src :r0 :name 'g)))
    (cl-cc/optimize::%mps-remember-store state 'g sg)
    (cl-cc/optimize::%mps-flush-if-src-overwritten state :r0)
    (assert-true (member sg (cl-cc/optimize::mps-result state)))
    (assert-null (cl-cc/optimize::%mps-pending-store state 'g))))

(deftest mps-flush-if-src-overwritten-unrelated-reg-leaves-store-pending
  "%mps-flush-if-src-overwritten leaves a pending store intact when the overwritten register is unrelated."
  (let ((state (cl-cc/optimize::make-mem-pass-state))
        (sg    (make-vm-set-global :src :r0 :name 'g)))
    (cl-cc/optimize::%mps-remember-store state 'g sg)
    (cl-cc/optimize::%mps-flush-if-src-overwritten state :r9)
    (assert-true (cl-cc/optimize::%mps-pending-store state 'g))
    (assert-null (cl-cc/optimize::mps-result state))))

;;; ─── %mps-flush-dependent-on-reg ────────────────────────────────────────────

(deftest mps-flush-dependent-on-reg-flushes-matching-stores
  "%mps-flush-dependent-on-reg emits pending stores that reference the register; leaves unrelated stores pending."
  (let ((state (cl-cc/optimize::make-mem-pass-state))
        (sg1   (make-vm-set-global :src :r0 :name 'g1))
        (sg2   (make-vm-set-global :src :r1 :name 'g2)))
    (cl-cc/optimize::%mps-remember-store state 'g1 sg1)
    (cl-cc/optimize::%mps-remember-store state 'g2 sg2)
    (cl-cc/optimize::%mps-flush-dependent-on-reg state :r0)
    (assert-true  (member sg1 (cl-cc/optimize::mps-result state)))
    (assert-null  (cl-cc/optimize::%mps-pending-store state 'g1))
    (assert-true  (cl-cc/optimize::%mps-pending-store state 'g2))))

(deftest mps-flush-dependent-on-reg-exclude-key-skips-matching-store
  "%mps-flush-dependent-on-reg does not flush the store identified by :exclude-key."
  (let ((state (cl-cc/optimize::make-mem-pass-state))
        (sg    (make-vm-set-global :src :r0 :name 'g)))
    (cl-cc/optimize::%mps-remember-store state 'g sg)
    (cl-cc/optimize::%mps-flush-dependent-on-reg state :r0 :exclude-key 'g)
    (assert-true  (cl-cc/optimize::%mps-pending-store state 'g))
    (assert-null  (cl-cc/optimize::mps-result state))))
