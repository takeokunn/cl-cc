;;;; tests/unit/emit/mir-tests.lisp — Unit tests for MIR (Phase 1)
;;;
;;; Covers: mir-value, mir-const, mir-inst, mir-block, mir-function,
;;;         mir-module, builder API, SSA variable tracking, CFG utilities,
;;;         target-desc and predefined targets.

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;;; ─── mir-value ────────────────────────────────────────────────────────

(deftest mir-value-allocation
  "mir-new-value assigns monotonically increasing IDs."
  (let* ((fn (mir-make-function :test-fn))
         (v0 (mir-new-value fn))
         (v1 (mir-new-value fn))
         (v2 (mir-new-value fn :name :x :type :integer)))
    (assert-= 0 (mirv-id v0))
    (assert-= 1 (mirv-id v1))
    (assert-= 2 (mirv-id v2))
    (assert-eq :x (mirv-name v2))
    (assert-eq :integer (mirv-type v2))
    (assert-= 3 (mirf-value-counter fn))))

(deftest mir-value-default-type
  "mir-new-value defaults to :any type."
  (let* ((fn (mir-make-function :f))
         (v  (mir-new-value fn)))
    (assert-eq :any (mirv-type v))))

(deftest mir-value-predicate
  "mir-value-p correctly identifies mir-value structs."
  (let* ((fn (mir-make-function :f))
         (v  (mir-new-value fn))
         (c  (make-mir-const :value 42)))
    (assert-true  (mir-value-p v))
    (assert-false (mir-value-p c))
    (assert-false (mir-value-p 42))))

;;;; ─── mir-const ────────────────────────────────────────────────────────

(deftest mir-const-integer
  "make-mir-const stores integer value and type."
  (let ((c (make-mir-const :value 42 :type :integer)))
    (assert-= 42 (mirc-value c))
    (assert-eq :integer (mirc-type c))
    (assert-true (mir-const-p c))))

(deftest mir-const-nil-value
  "make-mir-const can hold NIL as a value."
  (let ((c (make-mir-const :value nil :type :pointer)))
    (assert-null (mirc-value c))
    (assert-eq :pointer (mirc-type c))))

(deftest mir-const-string-value
  "make-mir-const can hold string values."
  (let ((c (make-mir-const :value "hello")))
    (assert-equal "hello" (mirc-value c))))

;;;; ─── mir-block ────────────────────────────────────────────────────────

(deftest mir-block-allocation
  "mir-new-block assigns unique IDs and auto-generates labels."
  (let* ((fn (mir-make-function :test-fn))
         ;; entry block already created by mir-make-function
         (b1 (mir-new-block fn))
         (b2 (mir-new-block fn :label :then)))
    (assert-false (null (mirf-entry fn)))
    (assert-eq :entry (mirb-label (mirf-entry fn)))
    (assert-eq :then (mirb-label b2))
    (assert-false (= (mirb-id b1) (mirb-id b2)))))

(deftest mir-block-starts-empty
  "A freshly created mir-block has no instructions, preds, or succs."
  (let* ((fn (mir-make-function :f))
         (b  (mir-new-block fn)))
    (assert-null (mirb-insts b))
    (assert-null (mirb-preds b))
    (assert-null (mirb-succs b))
    (assert-null (mirb-phis b))
    (assert-false (mirb-sealed-p b))))

(deftest mir-block-pred-succ-linking
  "mir-add-succ establishes bidirectional predecessor/successor edges."
  (let* ((fn   (mir-make-function :f))
         (b1   (mirf-entry fn))
         (b2   (mir-new-block fn :label :exit)))
    (mir-add-succ b1 b2)
    (assert-true  (member b2 (mirb-succs b1) :test #'eq))
    (assert-true  (member b1 (mirb-preds b2) :test #'eq))
    ;; idempotent
    (mir-add-succ b1 b2)
    (assert-= 1 (length (mirb-succs b1)))))

;;;; ─── mir-function ──────────────────────────────────────────────────────

(deftest mir-make-function-creates-entry
  "mir-make-function initialises name, entry block, and counters."
  (let ((fn (mir-make-function :fib)))
    (assert-eq :fib (mirf-name fn))
    (assert-false (null (mirf-entry fn)))
    (assert-eq :entry (mirb-label (mirf-entry fn)))
    ;; value counter starts at 0, block counter at 1 (entry consumed one)
    (assert-= 0 (mirf-value-counter fn))
    (assert-= 1 (mirf-block-counter fn))))

(deftest mir-make-function-with-params
  "Params passed to mir-make-function are stored in mirf-params."
  (let* ((fn (mir-make-function :add))
         (p0 (mir-new-value fn :name :a :type :integer))
         (p1 (mir-new-value fn :name :b :type :integer)))
    (setf (mirf-params fn) (list p0 p1))
    (assert-= 2 (length (mirf-params fn)))
    (assert-eq :a (mirv-name (first (mirf-params fn))))))

;;;; ─── mir-emit ──────────────────────────────────────────────────────────

(deftest mir-emit-basic-instruction
  "mir-emit appends an instruction to a block."
  (let* ((fn   (mir-make-function :f))
         (blk  (mirf-entry fn))
         (dst  (mir-new-value fn :name :result :type :integer))
         (a    (make-mir-const :value 1 :type :integer))
         (b    (make-mir-const :value 2 :type :integer))
         (inst (mir-emit blk :add :dst dst :srcs (list a b))))
    (assert-true  (mir-inst-p inst))
    (assert-eq    :add (miri-op inst))
    (assert-eq    dst  (miri-dst inst))
    (assert-= 2 (length (miri-srcs inst)))
    (assert-= 1 (length (mirb-insts blk)))
    (assert-eq blk (miri-block inst))))

(deftest mir-emit-sets-def-inst-backpointer
  "mir-emit sets mirv-def-inst on the destination value."
  (let* ((fn   (mir-make-function :f))
         (blk  (mirf-entry fn))
         (dst  (mir-new-value fn))
         (inst (mir-emit blk :const :dst dst :srcs (list (make-mir-const :value 0)))))
    (assert-eq inst (mirv-def-inst dst))))

(deftest mir-emit-terminator-has-no-dst
  "Terminator instructions (:ret :jump :branch :tail-call) get nil dst."
  (let* ((fn  (mir-make-function :f))
         (blk (mirf-entry fn))
         (v   (mir-new-value fn)))
    (let ((ret-inst (mir-emit blk :ret :srcs (list v))))
      (assert-null (miri-dst ret-inst)))))

(deftest mir-emit-phi-goes-to-phi-list
  "Phi instructions are prepended to mirb-phis, not mirb-insts."
  (let* ((fn   (mir-make-function :f))
         (blk  (mir-new-block fn :label :loop))
         (dst  (mir-new-value fn :name :x))
         (phi  (mir-emit blk :phi :dst dst)))
    (assert-true (member phi (mirb-phis blk) :test #'eq))
    (assert-null (mirb-insts blk))))

(deftest mir-emit-multiple-instructions-ordered
  "Instructions appear in emission order in mirb-insts."
  (let* ((fn  (mir-make-function :f))
         (blk (mirf-entry fn))
         (d0  (mir-new-value fn))
         (d1  (mir-new-value fn))
         (d2  (mir-new-value fn))
         (i0  (mir-emit blk :const :dst d0 :srcs (list (make-mir-const :value 1))))
         (i1  (mir-emit blk :const :dst d1 :srcs (list (make-mir-const :value 2))))
         (i2  (mir-emit blk :add   :dst d2 :srcs (list d0 d1))))
    (assert-= 3 (length (mirb-insts blk)))
    (assert-eq i0 (first (mirb-insts blk)))
    (assert-eq i1 (second (mirb-insts blk)))
    (assert-eq i2 (third (mirb-insts blk)))))

;;;; ─── SSA variable tracking ────────────────────────────────────────────

(deftest mir-write-read-var-same-block
  "mir-read-var returns the value written by mir-write-var in the same block."
  (let* ((fn   (mir-make-function :f))
         (blk  (mirf-entry fn))
         (v    (mir-new-value fn :name :x)))
    (mir-write-var fn :x blk v)
    (let ((got (mir-read-var fn :x blk)))
      (assert-eq v got))))

(deftest mir-read-var-single-predecessor
  "mir-read-var follows a single predecessor without inserting phi."
  (let* ((fn    (mir-make-function :f))
         (entry (mirf-entry fn))
         (b1    (mir-new-block fn :label :b1))
         (v     (mir-new-value fn :name :y)))
    (mir-seal-block fn entry)
    (mir-seal-block fn b1)
    (mir-add-succ entry b1)
    (mir-write-var fn :y entry v)
    (let ((got (mir-read-var fn :y b1)))
      (assert-eq v got))
    ;; No phi should have been inserted (single predecessor)
    (assert-null (mirb-phis b1))))

(deftest mir-seal-block-resolves-incomplete-phi
  "Sealing a block with multiple predecessors resolves incomplete phi nodes."
  (let* ((fn    (mir-make-function :f))
         (entry (mirf-entry fn))
         (b1    (mir-new-block fn :label :b1))
         (b2    (mir-new-block fn :label :b2))
         (merge (mir-new-block fn :label :merge))
         (v1    (mir-new-value fn :name :val1))
         (v2    (mir-new-value fn :name :val2)))
    ;; CFG: entry → b1, entry → b2, b1 → merge, b2 → merge
    (mir-add-succ entry b1)
    (mir-add-succ entry b2)
    (mir-add-succ b1 merge)
    (mir-add-succ b2 merge)
    ;; Seal all blocks except merge (its preds must be known first)
    (mir-seal-block fn entry)
    (mir-seal-block fn b1)
    (mir-seal-block fn b2)
    ;; Write different definitions in b1 and b2
    (mir-write-var fn :z b1 v1)
    (mir-write-var fn :z b2 v2)
    ;; Now read :z in merge (not sealed yet) — should create incomplete phi
    (let ((phi-val (mir-read-var fn :z merge)))
      (assert-false (null phi-val))
      ;; There should be one phi in merge's phis or incomplete-phis
      (assert-false (= 0 (hash-table-count (mirb-incomplete-phis merge))))
      ;; Seal merge — should resolve the phi
      (mir-seal-block fn merge)
      (assert-= 0 (hash-table-count (mirb-incomplete-phis merge)))
      ;; After sealing, the phi should have 2 src operands
      (when (mirb-phis merge)
        (let ((phi-inst (first (mirb-phis merge))))
          (assert-= 2 (length (miri-srcs phi-inst))))))))

;;;; ─── CFG utilities ────────────────────────────────────────────────────

(deftest mir-rpo-single-block
  "mir-rpo on a function with only the entry block returns that block."
  (let* ((fn    (mir-make-function :f))
         (entry (mirf-entry fn))
         (rpo   (mir-rpo fn)))
    (assert-= 1 (length rpo))
    (assert-eq entry (first rpo))))

(deftest mir-rpo-linear-chain
  "mir-rpo returns all blocks in a linear chain."
  (let* ((fn  (mir-make-function :f))
         (b0  (mirf-entry fn))
         (b1  (mir-new-block fn :label :b1))
         (b2  (mir-new-block fn :label :b2)))
    (mir-add-succ b0 b1)
    (mir-add-succ b1 b2)
    (let ((rpo (mir-rpo fn)))
      (assert-= 3 (length rpo))
      ;; b0 must appear before b1, b1 before b2 in RPO
      (let ((pos (lambda (b) (position b rpo :test #'eq))))
        (assert-true (< (funcall pos b0) (funcall pos b1)))
        (assert-true (< (funcall pos b1) (funcall pos b2)))))))

(deftest mir-rpo-diamond-cfg
  "mir-rpo visits all blocks in a diamond (if/merge) CFG."
  (let* ((fn    (mir-make-function :f))
         (entry (mirf-entry fn))
         (then  (mir-new-block fn :label :then))
         (else  (mir-new-block fn :label :else))
         (merge (mir-new-block fn :label :merge)))
    (mir-add-succ entry then)
    (mir-add-succ entry else)
    (mir-add-succ then merge)
    (mir-add-succ else merge)
    (let ((rpo (mir-rpo fn)))
      (assert-= 4 (length rpo))
      ;; Entry must come first
      (assert-eq entry (first rpo)))))

(deftest mir-dominators-linear
  "Immediate dominators in a linear chain: each block dominated by predecessor."
  (let* ((fn  (mir-make-function :f))
         (b0  (mirf-entry fn))
         (b1  (mir-new-block fn :label :b1))
         (b2  (mir-new-block fn :label :b2)))
    (mir-add-succ b0 b1)
    (mir-add-succ b1 b2)
    (let ((idom (mir-dominators fn)))
      (assert-eq b0 (gethash (mirb-id b0) idom))  ; entry dom. itself
      (assert-eq b0 (gethash (mirb-id b1) idom))  ; b1 idom = b0
      (assert-eq b1 (gethash (mirb-id b2) idom))))) ; b2 idom = b1

(deftest mir-dominators-diamond
  "Immediate dominators in a diamond CFG: merge dominated by entry."
  (let* ((fn    (mir-make-function :f))
         (entry (mirf-entry fn))
         (then  (mir-new-block fn :label :then))
         (else  (mir-new-block fn :label :else))
         (merge (mir-new-block fn :label :merge)))
    (mir-add-succ entry then)
    (mir-add-succ entry else)
    (mir-add-succ then merge)
    (mir-add-succ else merge)
    (let ((idom (mir-dominators fn)))
      ;; merge is dominated by entry (not then or else — both are branches)
      (assert-eq entry (gethash (mirb-id merge) idom))
      ;; then and else are dominated by entry
      (assert-eq entry (gethash (mirb-id then) idom))
      (assert-eq entry (gethash (mirb-id else) idom)))))

;;;; ─── mir-module ────────────────────────────────────────────────────────

(deftest mir-module-basic
  "make-mir-module initialises with empty function and globals lists."
  (let ((m (make-mir-module)))
    (assert-null (mirm-functions m))
    (assert-null (mirm-globals m))
    (assert-false (null (mirm-string-table m)))))

;;;; ─── mir generic ops vocabulary ──────────────────────────────────────

(deftest mir-generic-ops-contains-core
  "Core ops (:add :sub :call :ret :jump :branch :phi) are in *mir-generic-ops*."
  (dolist (op '(:add :sub :mul :div :mod :neg
                :band :bor :bxor :bnot
                :lt :le :gt :ge :eq :ne
                :load :store :alloca
                :call :tail-call :ret :jump :branch
                :phi :values :mv-bind :safepoint :nop))
    (assert-true (member op *mir-generic-ops*))))

;;;; ─── printer smoke tests ───────────────────────────────────────────────

(deftest mir-format-value-mir-value
  "mir-format-value formats a mir-value as %id/name (CL symbols upcase)."
  (let* ((fn (mir-make-function :f))
         (v  (mir-new-value fn :name :x)))
    (let ((s (mir-format-value v)))
      (assert-true (search "%0" s))
      ;; CL formats :x as "X" (standard upcase printing)
      (assert-true (search "X"  s)))))

(deftest mir-format-value-mir-const
  "mir-format-value formats a mir-const as #value."
  (let ((c (make-mir-const :value 42)))
    (let ((s (mir-format-value c)))
      (assert-true (search "42" s)))))

(deftest mir-print-function-no-error
  "mir-print-function completes without signalling an error."
  (let* ((fn   (mir-make-function :smoke))
         (blk  (mirf-entry fn))
         (dst  (mir-new-value fn :name :r))
         (c    (make-mir-const :value 7 :type :integer)))
    (mir-emit blk :const :dst dst :srcs (list c))
    (mir-emit blk :ret   :srcs (list dst))
    (let ((out (with-output-to-string (s) (mir-print-function fn s))))
      (assert-true (search "SMOKE" out))
      (assert-true (search "ENTRY" out)))))

;;;; ─── target-desc ──────────────────────────────────────────────────────

(deftest target-x86-64-basic
  "x86-64 target has correct name, word-size, and endianness."
  (assert-eq    :x86-64  (target-name *x86-64-target*))
  (assert-=     8        (target-word-size *x86-64-target*))
  (assert-eq    :little  (target-endianness *x86-64-target*))
  (assert-=     16       (target-stack-alignment *x86-64-target*)))

(deftest target-x86-64-calling-convention
  "x86-64 target has correct arg registers and return register."
  (assert-eq :rax (target-ret-reg *x86-64-target*))
  (assert-eq :rdi (first (target-arg-regs *x86-64-target*)))
  (assert-=   6   (length (target-arg-regs *x86-64-target*))))

(deftest target-x86-64-callee-saved
  "x86-64 callee-saved registers include rbx and r12."
  (assert-true (member :rbx (target-callee-saved *x86-64-target*)))
  (assert-true (member :r12 (target-callee-saved *x86-64-target*))))

(deftest target-aarch64-basic
  "aarch64 target has correct name, word-size, and 8 arg registers."
  (assert-eq :aarch64 (target-name *aarch64-target*))
  (assert-=  8        (target-word-size *aarch64-target*))
  (assert-eq :x0      (target-ret-reg *aarch64-target*))
  (assert-=  8        (length (target-arg-regs *aarch64-target*))))

(deftest target-riscv64-basic
  "riscv64 target has 32 GPRs and a0 as return register."
  (assert-eq :riscv64 (target-name *riscv64-target*))
  (assert-=  32       (target-gpr-count *riscv64-target*))
  (assert-eq :a0      (target-ret-reg *riscv64-target*))
  (assert-=  8        (length (target-arg-regs *riscv64-target*))))

(deftest target-wasm32-basic
  "wasm32 target has 4-byte words, 0 GPRs, and stack-alignment 0."
  (assert-eq :wasm32 (target-name *wasm32-target*))
  (assert-=  4       (target-word-size *wasm32-target*))
  (assert-=  0       (target-gpr-count *wasm32-target*))
  (assert-=  0       (target-stack-alignment *wasm32-target*)))

(deftest target-registry-lookup
  "find-target returns the correct target-desc for each registered name."
  (assert-eq *x86-64-target*  (find-target :x86-64))
  (assert-eq *aarch64-target* (find-target :aarch64))
  (assert-eq *riscv64-target* (find-target :riscv64))
  (assert-eq *wasm32-target*  (find-target :wasm32))
  (assert-null (find-target :nonexistent)))

(deftest target-64-bit-predicate
  "target-64-bit-p returns true for 64-bit targets, false for 32-bit."
  (assert-true  (target-64-bit-p *x86-64-target*))
  (assert-true  (target-64-bit-p *aarch64-target*))
  (assert-true  (target-64-bit-p *riscv64-target*))
  (assert-false (target-64-bit-p *wasm32-target*)))

(deftest target-feature-predicate
  "target-has-feature-p correctly detects presence and absence of features."
  (assert-true  (target-has-feature-p *x86-64-target*  :has-fused-cmp-branch))
  (assert-true  (target-has-feature-p *aarch64-target* :has-native-tail-call))
  (assert-true  (target-has-feature-p *riscv64-target* :riscv-elf-psabi))
  (assert-true  (target-has-feature-p *wasm32-target*  :wasi-0.2))
  (assert-false (target-has-feature-p *x86-64-target*  :wasi-0.2))
  (assert-false (target-has-feature-p *wasm32-target*  :sysv-abi)))

(deftest target-allocatable-regs-excludes-scratch
  "target-allocatable-regs never contains scratch registers."
  (dolist (target (list *x86-64-target* *aarch64-target* *riscv64-target*))
    (let ((alloc   (target-allocatable-regs target))
          (scratch (target-scratch-regs target)))
      (dolist (sr scratch)
        (assert-false (member sr alloc))))))

(deftest target-caller-saved-complement
  "target-caller-saved returns allocatable regs minus callee-saved."
  (let* ((alloc  (target-allocatable-regs *x86-64-target*))
         (callee (target-callee-saved *x86-64-target*))
         (caller (target-caller-saved *x86-64-target*)))
    ;; Every caller-saved reg is allocatable
    (dolist (r caller)
      (assert-true (member r alloc)))
    ;; No overlap between caller-saved and callee-saved
    (dolist (r caller)
      (assert-false (member r callee)))))

(deftest target-reg-index-lookup
  "target-reg-index returns correct 0-based physical index."
  (assert-= 0 (target-reg-index *x86-64-target* :rax))  ; first entry
  (assert-eq :rax (aref (target-gpr-names *x86-64-target*) 0))
  ;; unknown register returns nil
  (assert-null (target-reg-index *x86-64-target* :unknown-reg)))

(deftest target-op-legal-default
  "target-op-legal-p returns true for unknown ops (permissive default)."
  (assert-true (target-op-legal-p *x86-64-target* :add))
  (assert-true (target-op-legal-p *x86-64-target* :some-hypothetical-op)))

(deftest target-register-and-find
  "register-target and find-target round-trip correctly for a custom target."
  (let* ((custom (make-target-desc
                   :name      :test-custom
                   :word-size 8
                   :endianness :little
                   :gpr-count 4
                   :gpr-names #(:r0 :r1 :r2 :r3)
                   :arg-regs  '(:r0 :r1)
                   :ret-reg   :r0
                   :stack-alignment 16))
         (_   (register-target custom))
         (got (find-target :test-custom)))
    (assert-eq custom got)
    (assert-eq :test-custom (target-name got))
    ;; Clean up
    (remhash :test-custom *target-registry*)))
