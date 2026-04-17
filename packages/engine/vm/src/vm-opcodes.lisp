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

(defun %vm2-build-fusion-table ()
  "Build the const-fusion table from known opcode constant pairs."
  (list (cons +op2-add2+    +op2-add-imm2+)
        (cons +op2-sub2+    +op2-sub-imm2+)
        (cons +op2-mul2+    +op2-mul-imm2+)
        (cons +op2-num-eq2+ +op2-num-eq-imm2+)
        (cons +op2-num-lt2+ +op2-num-lt-imm2+)
        (cons +op2-num-gt2+ +op2-num-gt-imm2+)
        (cons +op2-num-le2+ +op2-num-le-imm2+)
        (cons +op2-num-ge2+ +op2-num-ge-imm2+)))

(defvar *vm2-const-fusion-table* nil
  "Lazily initialized list of (binary-opcode . immediate-opcode) fusion rules.")

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
  (let ((out (make-array 0 :adjustable t :fill-pointer 0))
        (n (length code))
        (pc 0)
        (op2-const (symbol-value '+op2-const+))
        (op2-halt2 (symbol-value '+op2-halt2+))
        (op2-const-halt2 (symbol-value '+op2-const-halt2+)))
    (labels ((emit4 (op a b c)
               (vector-push-extend op out)
               (vector-push-extend a out)
               (vector-push-extend b out)
               (vector-push-extend c out)))
      (loop while (< pc n)
            do (if (<= (+ pc 8) n)
                   (let* ((op1 (aref code pc))
                          (dst1 (aref code (+ pc 1)))
                          (imm1 (aref code (+ pc 2)))
                          (aux1 (aref code (+ pc 3)))
                          (op2 (aref code (+ pc 4)))
                          (dst2 (aref code (+ pc 5)))
                          (src21 (aref code (+ pc 6)))
                          (src22 (aref code (+ pc 7))))
                     (let ((fused nil))
                       ;; Try const-fusion rules
                       (when (= op1 op2-const)
                         (dolist (rule *vm2-const-fusion-table*)
                           (when (= op2 (car rule))
                             (cond
                               ((= src22 dst1)
                                (emit4 (cdr rule) dst2 src21 imm1)
                                (incf pc 8)
                                (setf fused t)
                                (return))
                               ((= src21 dst1)
                                (emit4 (cdr rule) dst2 src22 imm1)
                                (incf pc 8)
                                (setf fused t)
                                (return))))))
                       ;; const+halt special case
                       (unless fused
                         (if (and (= op1 op2-const) (= op2 op2-halt2) (= dst1 dst2))
                             (progn
                               (emit4 op2-const-halt2 imm1 nil nil)
                               (incf pc 8))
                             (progn
                               (emit4 op1 dst1 imm1 aux1)
                               (incf pc 4))))))
                   (progn
                     (emit4 (aref code pc)
                            (if (< (+ pc 1) n) (aref code (+ pc 1)) nil)
                            (if (< (+ pc 2) n) (aref code (+ pc 2)) nil)
                            (if (< (+ pc 3) n) (aref code (+ pc 3)) nil))
                     (incf pc 4)))))
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
  (let ((op-sym (intern (format nil "+OP2-~A+" (symbol-name name))))
        (op-num *next-opcode*))
    (incf *next-opcode*)
    `(progn
       (defconstant ,op-sym ,op-num)
       (setf (aref *opcode-dispatch-table* ,op-sym) ,@body)
       (setf (aref *opcode-name-table*     ,op-sym) ',name)
       (setf (gethash ',name *opcode-encoder-table*) ,op-sym)
       ',name)))

;;; ── Opcode shape macros ───────────────────────────────────────────────────
;;; These macros generate defopcode forms for the four common opcode shapes,
;;; eliminating ~40 near-identical lambda bodies while keeping all opcode
;;; numbering and table registration intact via the inner defopcode call.

(defmacro defopcode-unary-fn (name fn)
  "Opcode: dst ← fn(src)."
  `(defopcode ,name
     (lambda (state code pc regs)
       (declare (ignore state))
       (let ((dst (svref code (+ pc 1)))
             (src (svref code (+ pc 2))))
         (setf (svref regs dst) (,fn (svref regs src))))
       (+ pc 4))))

(defmacro defopcode-unary-type-pred (name type)
  "Opcode: dst ← (if (typep src 'type) 1 0).
Used for predicates that lack a simple CL function form (e.g. fixnump)."
  `(defopcode ,name
     (lambda (state code pc regs)
       (declare (ignore state))
       (let ((dst (svref code (+ pc 1)))
             (src (svref code (+ pc 2))))
         (setf (svref regs dst) (if (typep (svref regs src) ',type) 1 0)))
       (+ pc 4))))

(defmacro defopcode-unary-pred (name pred)
  "Opcode: dst ← (if pred(src) 1 0)."
  `(defopcode ,name
     (lambda (state code pc regs)
       (declare (ignore state))
       (let ((dst (svref code (+ pc 1)))
             (src (svref code (+ pc 2))))
         (setf (svref regs dst) (if (,pred (svref regs src)) 1 0)))
       (+ pc 4))))

(defmacro defopcode-binary-fn (name fn)
  "Opcode: dst ← fn(src1, src2)."
  `(defopcode ,name
     (lambda (state code pc regs)
       (declare (ignore state))
       (let ((dst  (svref code (+ pc 1)))
             (src1 (svref code (+ pc 2)))
             (src2 (svref code (+ pc 3))))
         (setf (svref regs dst) (,fn (svref regs src1) (svref regs src2))))
       (+ pc 4))))

(defmacro defopcode-binary-pred (name pred)
  "Opcode: dst ← (if pred(src1, src2) 1 0)."
  `(defopcode ,name
     (lambda (state code pc regs)
       (declare (ignore state))
       (let ((dst  (svref code (+ pc 1)))
             (src1 (svref code (+ pc 2)))
             (src2 (svref code (+ pc 3))))
         (setf (svref regs dst) (if (,pred (svref regs src1) (svref regs src2)) 1 0)))
       (+ pc 4))))

(defmacro defopcode-binary-fn-imm (name fn)
  "Opcode: dst ← fn(src, immediate)."
  `(defopcode ,name
     (lambda (state code pc regs)
       (declare (ignore state))
       (let ((dst (svref code (+ pc 1)))
             (src (svref code (+ pc 2)))
             (imm (svref code (+ pc 3))))
         (setf (svref regs dst) (,fn (svref regs src) imm)))
       (+ pc 4))))

(defmacro defopcode-binary-pred-imm (name pred)
  "Opcode: dst ← (if pred(src, immediate) 1 0)."
  `(defopcode ,name
     (lambda (state code pc regs)
       (declare (ignore state))
       (let ((dst (svref code (+ pc 1)))
             (src (svref code (+ pc 2)))
             (imm (svref code (+ pc 3))))
         (setf (svref regs dst) (if (,pred (svref regs src) imm) 1 0)))
       (+ pc 4))))

;;; Register file size constant
(defconstant +vm-register-count+ 256
  "Number of integer-indexed registers in the vm2-state register file.")

;;; vm2-state: flat-vector interpreter state (distinct from vm-state CLOS class)
(defstruct (vm2-state
             (:constructor make-vm2-state
                (&key (output-stream *standard-output*)
                 &aux (registers (make-array +vm-register-count+ :initial-element nil))
                      (global-vars (let ((ht (make-hash-table)))
                                     (setf (gethash '*features* ht) '(:common-lisp :cl-cc))
                                     (setf (gethash '*active-restarts* ht) nil)
                                     (setf (gethash '*standard-output* ht) *standard-output*)
                                     (setf (gethash '*standard-input* ht) *standard-input*)
                                     (setf (gethash '*error-output* ht) *error-output*)
                                     (setf (gethash '*trace-output* ht) *trace-output*)
                                     (setf (gethash '*debug-io* ht) *debug-io*)
                                     (setf (gethash '*query-io* ht) *query-io*)
                                     ht))
                      (values-buffer nil))))
  "VM2 state for the flat-vector run-vm interpreter.
REGISTERS: simple-vector of 256 integer-indexed slots (fast svref access).
GLOBAL-VARS: hash table for global variable bindings.
OUTPUT-STREAM: stream for I/O."
  (registers    nil :type simple-vector)
  (global-vars  nil :type hash-table)
  (values-buffer nil)
  (output-stream *standard-output*))

(defun vm2-reg-get (state reg)
  "Get register REG (integer) from vm2-state STATE."
  (svref (vm2-state-registers state) reg))

(defun vm2-reg-set (state reg value)
  "Set register REG (integer) to VALUE in vm2-state STATE. Returns VALUE."
  (setf (svref (vm2-state-registers state) reg) value)
  value)
