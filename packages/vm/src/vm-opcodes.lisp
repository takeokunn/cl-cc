(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Phase-A Flat Bytecode Engine
;;;
;;; Contains: opcode dispatch tables, bigram analysis, const-fusion,
;;; defopcode macro, shape macros, vm2-state struct.
;;; Opcode definitions are in vm-opcodes-defs.lisp.
;;;
;;; Load order: after vm-run.lisp (CLOS-based interpreter).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Phase A: defopcode dispatch table + flat-vector interpreter ──────────

;;; Opcode dispatch tables (256-slot vectors)
(defparameter *opcode-dispatch-table*
  (make-array 256 :initial-element nil)
  "Vector mapping opcode integer → handler function (lambda (state code pc regs) ...).")

(defparameter *opcode-name-table*
  (make-array 256 :initial-element nil)
  "Vector mapping opcode integer → symbol name.")

(defparameter *opcode-encoder-table*
  (make-hash-table :test 'eq)
  "Hash table mapping opcode symbol name → opcode integer.")

(defun vm2-collect-opcode-bigrams (code)
  "Collect opcode bigram frequencies from flat VM2 CODE.

CODE is expected to follow the 4-word instruction layout used by run-vm.
Returns an EQUAL hash-table keyed by (opcode-a opcode-b) symbol pairs."
  (let ((counts (make-hash-table :test #'equal))
        (n (length code)))
    (loop for pc from 0 below (- n 4) by 4
          for next-pc = (+ pc 4)
          for op-a = (aref code pc)
          for op-b = (and (< next-pc n) (aref code next-pc))
          for name-a = (and (integerp op-a) (< op-a (length *opcode-name-table*))
                            (aref *opcode-name-table* op-a))
          for name-b = (and (integerp op-b) (< op-b (length *opcode-name-table*))
                            (aref *opcode-name-table* op-b))
          when (and name-a name-b)
            do (incf (gethash (list name-a name-b) counts 0)))
    counts))

(defun vm2-top-superoperator-candidates (code &key (limit 20))
  "Return the top opcode bigram candidates from CODE.

Each result element is (pair count), sorted by descending count then name."
  (let (pairs)
    (maphash (lambda (pair count)
               (push (list pair count) pairs))
             (vm2-collect-opcode-bigrams code))
    (subseq (sort pairs (lambda (a b)
                          (or (> (second a) (second b))
                              (and (= (second a) (second b))
                                   (string< (prin1-to-string (first a))
                                   (prin1-to-string (first b)))))))
            0 (min limit (length pairs)))))

(defparameter *vm2-fusion-opcode-symbol-pairs*
  '((+op2-add2+    . +op2-add-imm2+)
    (+op2-sub2+    . +op2-sub-imm2+)
    (+op2-mul2+    . +op2-mul-imm2+)
    (+op2-num-eq2+ . +op2-num-eq-imm2+)
    (+op2-num-lt2+ . +op2-num-lt-imm2+)
    (+op2-num-gt2+ . +op2-num-gt-imm2+)
    (+op2-num-le2+ . +op2-num-le-imm2+)
    (+op2-num-ge2+ . +op2-num-ge-imm2+))
  "Symbolic (binary-opcode . immediate-opcode) pairs resolved after opcode defs load.")

(defun %vm2-build-fusion-table ()
  "Build the const-fusion table from symbolic opcode pairs.
Resolves opcode constants lazily so this file does not depend on vm-opcodes-defs
being loaded at compile time."
  (mapcar (lambda (pair)
            (cons (symbol-value (car pair))
                  (symbol-value (cdr pair))))
          *vm2-fusion-opcode-symbol-pairs*))

(defvar *vm2-const-fusion-table* nil
  "Lazily initialized list of (binary-opcode . immediate-opcode) fusion rules.")

(defun %vm2-emit4 (out op a b c)
  "Append a 4-element instruction tuple (OP A B C) to the fill-pointer vector OUT."
  (vector-push-extend op out)
  (vector-push-extend a out)
  (vector-push-extend b out)
  (vector-push-extend c out))

(defun vm2-fuse-immediate-superinstructions (code)
  "Fuse const+arith bytecode pairs into existing immediate opcodes.

This is a conservative VM2 superinstruction helper. It recognizes:
  const rX IMM ; add2 dst src rX  -> add-imm2 dst src IMM
  const rX IMM ; add2 dst rX src  -> add-imm2 dst src IMM
  const rX IMM ; sub2 dst src rX  -> sub-imm2 dst src IMM
  const rX IMM ; mul2 dst src rX  -> mul-imm2 dst src IMM
  const rX IMM ; mul2 dst rX src  -> mul-imm2 dst src IMM
The helper leaves all other instruction sequences unchanged."
  (unless *vm2-const-fusion-table*
    (setf *vm2-const-fusion-table* (%vm2-build-fusion-table)))
  (let ((out           (make-array 0 :adjustable t :fill-pointer 0))
        (n             (length code))
        (pc            0)
        (op2-const     (symbol-value '+op2-const+))
        (op2-halt2     (symbol-value '+op2-halt2+))
        (op2-const-halt2 (symbol-value '+op2-const-halt2+)))
    (loop while (< pc n)
          do (if (<= (+ pc 8) n)
                 (let* ((op1   (aref code pc))
                        (dst1  (aref code (+ pc 1)))
                        (imm1  (aref code (+ pc 2)))
                        (aux1  (aref code (+ pc 3)))
                        (op2   (aref code (+ pc 4)))
                        (dst2  (aref code (+ pc 5)))
                        (src21 (aref code (+ pc 6)))
                        (src22 (aref code (+ pc 7))))
                   (let ((fused nil))
                     ;; Try const-fusion rules
                     (when (= op1 op2-const)
                       (dolist (rule *vm2-const-fusion-table*)
                         (when (= op2 (car rule))
                           (cond
                             ((= src22 dst1)
                              (%vm2-emit4 out (cdr rule) dst2 src21 imm1)
                              (incf pc 8)
                              (setf fused t)
                              (return))
                             ((= src21 dst1)
                              (%vm2-emit4 out (cdr rule) dst2 src22 imm1)
                              (incf pc 8)
                              (setf fused t)
                              (return))))))
                     ;; const+halt special case
                     (unless fused
                       (if (and (= op1 op2-const) (= op2 op2-halt2) (= dst1 dst2))
                           (progn (%vm2-emit4 out op2-const-halt2 imm1 nil nil)
                                  (incf pc 8))
                           (progn (%vm2-emit4 out op1 dst1 imm1 aux1)
                                  (incf pc 4))))))
                 (progn
                   (%vm2-emit4 out
                               (aref code pc)
                               (if (< (+ pc 1) n) (aref code (+ pc 1)) nil)
                               (if (< (+ pc 2) n) (aref code (+ pc 2)) nil)
                               (if (< (+ pc 3) n) (aref code (+ pc 3)) nil))
                   (incf pc 4))))
    (coerce out 'simple-vector)))

;;; Opcode counter (auto-incremented by defopcode)
;;; eval-when ensures this is bound at compile time (needed by defconstant in defopcode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *next-opcode* 0
    "Next available opcode integer; incremented by each defopcode form."))

(defmacro defopcode (name &body body)
  "Define a bytecode opcode NAME.
Assigns it the next available opcode number, registers it in all three
dispatch tables, and defines a constant +OP2-NAME+ for the opcode number.
BODY must be a single (lambda (state code pc regs) ...) returning next PC."
  (when (or (eq name 'const)
            (not (boundp '*next-opcode*)))
    (setf (symbol-value '*next-opcode*) 0))
  (let ((op-sym (intern (format nil "+OP2-~A+" (symbol-name name))))
         (op-num (symbol-value '*next-opcode*)))
    (setf (symbol-value '*next-opcode*) (1+ op-num))
    (list 'progn
          (list 'defconstant op-sym op-num)
          (list 'setf (list 'aref '*opcode-dispatch-table* op-sym) (first body))
          (list 'setf (list 'aref '*opcode-name-table* op-sym) (list 'quote name))
          (list 'setf (list 'gethash (list 'quote name) '*opcode-encoder-table*) op-sym)
          (list 'quote name))))
