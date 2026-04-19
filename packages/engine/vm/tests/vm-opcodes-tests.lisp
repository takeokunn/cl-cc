;;;; tests/unit/vm/vm-opcodes-tests.lisp
;;;; Unit tests for src/vm/vm-opcodes.lisp
;;;;
;;;; Covers: make-vm2-state (register array, global pre-population),
;;;;   vm2-reg-get / vm2-reg-set (register file read/write),
;;;;   vm2-collect-opcode-bigrams (empty, short, known-opcode bigrams),
;;;;   vm2-top-superoperator-candidates (empty, limit trimming),
;;;;   vm2-fuse-immediate-superinstructions (empty, odd-length passthrough,
;;;;   non-fuselable passthrough).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── make-vm2-state ──────────────────────────────────────────────────────────

(deftest vm2-state-creation-cases
  "make-vm2-state: 256 nil registers; *features* in global-vars; output-stream is *standard-output*."
  (let ((s (cl-cc::make-vm2-state)))
    (assert-true (simple-vector-p (cl-cc::vm2-state-registers s)))
    (assert-= cl-cc/vm::+vm-register-count+ (length (cl-cc::vm2-state-registers s)))
    (assert-null (svref (cl-cc::vm2-state-registers s) 0))
    (assert-null (svref (cl-cc::vm2-state-registers s) 255))
    (assert-true (hash-table-p (cl-cc::vm2-state-global-vars s)))
    (assert-true (nth-value 1 (gethash '*features* (cl-cc::vm2-state-global-vars s))))
    (assert-eq *standard-output* (cl-cc::vm2-state-output-stream s))))

;;; ─── vm2-reg-get / vm2-reg-set ───────────────────────────────────────────────

(deftest vm2-reg-ops-cases
  "vm2-reg: fresh nil; set returns value and stores it; adjacent independent; overwrite takes last."
  (let ((s (cl-cc::make-vm2-state)))
    (assert-null (cl-cc/vm::vm2-reg-get s 0))
    (assert-null (cl-cc/vm::vm2-reg-get s 128))
    (assert-null (cl-cc/vm::vm2-reg-get s 255)))
  (let ((s (cl-cc::make-vm2-state)))
    (let ((ret (cl-cc/vm::vm2-reg-set s 0 42)))
      (assert-= 42 ret)
      (assert-= 42 (cl-cc/vm::vm2-reg-get s 0))))
  (let ((s (cl-cc::make-vm2-state)))
    (cl-cc/vm::vm2-reg-set s 5 :foo)
    (cl-cc/vm::vm2-reg-set s 6 :bar)
    (assert-eq :foo (cl-cc/vm::vm2-reg-get s 5))
    (assert-eq :bar (cl-cc/vm::vm2-reg-get s 6)))
  (let ((s (cl-cc::make-vm2-state)))
    (cl-cc/vm::vm2-reg-set s 10 'first)
    (cl-cc/vm::vm2-reg-set s 10 'second)
    (assert-eq 'second (cl-cc/vm::vm2-reg-get s 10))))

;;; ─── vm2-collect-opcode-bigrams ──────────────────────────────────────────────

(deftest vm2-collect-bigrams-edge-cases
  "vm2-collect-opcode-bigrams: empty vector → empty HT; single 4-word instr → 0 pairs."
  (let ((result (cl-cc/vm::vm2-collect-opcode-bigrams #())))
    (assert-true (hash-table-p result))
    (assert-= 0 (hash-table-count result)))
  (let ((result (cl-cc/vm::vm2-collect-opcode-bigrams #(0 0 0 0))))
    (assert-= 0 (hash-table-count result))))

(deftest vm2-collect-bigrams-known-opcode-pair-counted
  "vm2-collect-opcode-bigrams counts a known opcode pair once."
  ;; Use +op2-const+ followed by +op2-halt2+ — both are in *opcode-name-table*
  (let* ((op-a cl-cc::+op2-const+)
         (op-b cl-cc::+op2-halt2+)
         (code (vector op-a 0 0 0 op-b 0 0 0))
         (result (cl-cc/vm::vm2-collect-opcode-bigrams code)))
    ;; The pair (CONST HALT2) should appear with count 1
    (let ((pair-name-a (aref cl-cc/vm::*opcode-name-table* op-a))
          (pair-name-b (aref cl-cc/vm::*opcode-name-table* op-b)))
      (when (and pair-name-a pair-name-b)
        (assert-= 1 (gethash (list pair-name-a pair-name-b) result 0))))))

;;; ─── vm2-top-superoperator-candidates ────────────────────────────────────────

(deftest vm2-top-candidates-cases
  "vm2-top-superoperator-candidates: empty → nil; :limit 1 returns at most 1 entry."
  (assert-null (cl-cc/vm::vm2-top-superoperator-candidates #()))
  (let* ((op-a cl-cc::+op2-const+)
         (op-b cl-cc::+op2-halt2+)
         (code (vector op-a 0 0 0 op-b 0 0 0)))
    (let ((result (cl-cc/vm::vm2-top-superoperator-candidates code :limit 1)))
      (assert-true (<= (length result) 1)))))

;;; ─── vm2-fuse-immediate-superinstructions ────────────────────────────────────

(deftest vm2-fuse-empty-returns-empty
  "vm2-fuse-immediate-superinstructions on empty vector returns empty vector."
  (let ((result (cl-cc/vm::vm2-fuse-immediate-superinstructions #())))
    (assert-true (vectorp result))
    (assert-= 0 (length result))))

(deftest vm2-fuse-single-instruction-passthrough
  "vm2-fuse-immediate-superinstructions passes through a single 4-word instruction."
  (let* ((op cl-cc::+op2-halt2+)
         (code (vector op 0 0 0))
         (result (cl-cc/vm::vm2-fuse-immediate-superinstructions code)))
    (assert-true (vectorp result))
    (assert-= 4 (length result))
    (assert-= op (svref result 0))))

(deftest vm2-fuse-const-halt-fused-to-const-halt2
  "vm2-fuse-immediate-superinstructions merges const+halt into the const-halt2 superinstruction."
  ;; Pattern: [op2-const dst IMM 0] [op2-halt2 dst 0 0]
  ;; When both instructions reference the same dst, they fuse to [op2-const-halt2 IMM nil nil]
  (let* ((op-const cl-cc::+op2-const+)
         (op-halt  cl-cc::+op2-halt2+)
         (op-fused cl-cc::+op2-const-halt2+)
         (code (vector op-const 7 99 nil op-halt 7 nil nil))
         (result (cl-cc/vm::vm2-fuse-immediate-superinstructions code)))
    ;; Fused → 4 words, first word is the superinstruction opcode
    (assert-= 4 (length result))
    (assert-= op-fused (svref result 0))))
