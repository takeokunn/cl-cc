(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Handler-Case Instructions, Label Table, and Flat-Vector Interpreter
;;;
;;; Contains: vm-establish-handler / vm-remove-handler / vm-sync-handler-regs /
;;; vm-signal-error (defstructs + execute-instruction), build-label-table,
;;; run-program-slice, run-compiled, the Phase-A defopcode bytecode engine
;;; (vm2-state, defopcode, run-vm), and vm2-state compatibility shims.
;;;
;;; Load order: after vm-clos.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Handler-Case VM Instructions ─────────────────────────────────────────

(define-vm-instruction vm-establish-handler (vm-instruction)
  "Push a handler entry onto the handler stack for handler-case."
  (handler-label nil :reader vm-handler-label)
  (result-reg nil :reader vm-handler-result-reg)
  (error-type nil :reader vm-error-type)
  (:sexp-tag :establish-handler))

(define-vm-instruction vm-remove-handler (vm-instruction)
  "Pop the top handler from the handler stack."
  (:sexp-tag :remove-handler))

(define-vm-instruction vm-sync-handler-regs (vm-instruction)
  "Update all handlers' saved-regs with current register state."
  (:sexp-tag :sync-handler-regs))

(define-vm-instruction vm-signal-error (vm-instruction)
  "Signal an error: walk the handler stack to find a matching handler."
  (error-reg nil :reader vm-error-reg)
  (:sexp-tag :signal-error))

(defun vm-handler-entry-saved-regs (entry)
  "Return the saved register snapshot stored in handler stack ENTRY."
  (if (and (consp entry) (eq (first entry) :catch))
      (sixth entry)
      (fifth entry)))

(defun (setf vm-handler-entry-saved-regs) (snapshot entry)
  "Replace the saved register snapshot stored in handler stack ENTRY."
  (if (and (consp entry) (eq (first entry) :catch))
      (setf (sixth entry) snapshot)
      (setf (fifth entry) snapshot)))

(defmethod execute-instruction ((inst vm-establish-handler) state pc labels)
  (declare (ignore labels))
  (let ((saved-regs (let ((copy (make-hash-table :test (hash-table-test (vm-state-registers state)))))
                      (maphash (lambda (k v) (setf (gethash k copy) v))
                               (vm-state-registers state))
                      copy)))
    (push (list (vm-handler-label inst)
                (vm-handler-result-reg inst)
                (vm-error-type inst)
                (vm-call-stack state)
                saved-regs
                (vm-method-call-stack state))
          (vm-handler-stack state)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-remove-handler) state pc labels)
  (declare (ignore labels))
  (when (vm-handler-stack state)
    (pop (vm-handler-stack state)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-sync-handler-regs) state pc labels)
  (declare (ignore labels))
  (let ((snapshot (let ((copy (make-hash-table :test (hash-table-test (vm-state-registers state)))))
                    (maphash (lambda (k v) (setf (gethash k copy) v))
                             (vm-state-registers state))
                    copy)))
    (dolist (entry (vm-handler-stack state))
      (setf (vm-handler-entry-saved-regs entry) snapshot)))
  (values (1+ pc) nil nil))

(defun vm-error-type-matches-p (error-value handler-type)
  "Check if ERROR-VALUE matches HANDLER-TYPE for handler-case dispatch.
String errors (from VM error instruction) match error/condition/t but not subtypes.
CL condition objects use typep."
  (cond
    ((member handler-type '(error condition serious-condition t)) t)
    ((typep error-value 'condition)
     (ignore-errors (typep error-value handler-type)))
    (t nil)))

(defmethod execute-instruction ((inst vm-signal-error) state pc labels)
  (let ((error-value (vm-reg-get state (vm-error-reg inst))))
    (let ((matching-handler nil)
          (handlers-to-skip 0))
      (dolist (entry (vm-handler-stack state))
        (let ((error-type (third entry)))
          (if (vm-error-type-matches-p error-value error-type)
              (progn (setf matching-handler entry) (return))
              (incf handlers-to-skip))))
      (if matching-handler
          (progn
            (dotimes (i (1+ handlers-to-skip))
              (pop (vm-handler-stack state)))
            (destructuring-bind (handler-label result-reg error-type saved-call-stack saved-regs
                                 &optional saved-method-call-stack)
                matching-handler
              (declare (ignore error-type))
              (setf (vm-call-stack state) saved-call-stack)
              (setf (vm-method-call-stack state) (or saved-method-call-stack nil))
              (clrhash (vm-state-registers state))
              (maphash (lambda (k v) (setf (gethash k (vm-state-registers state)) v))
                       saved-regs)
              (vm-reg-set state result-reg error-value)
              (values (vm-label-table-lookup labels handler-label) nil nil)))
          (error "Unhandled error in VM: ~S" error-value)))))

;;; ── Catch/Throw VM Instructions ────────────────────────────────────────

(define-vm-instruction vm-establish-catch (vm-instruction)
  "Push a catch frame onto the handler stack for CATCH."
  (tag-reg nil :reader vm-catch-tag-reg)
  (handler-label nil :reader vm-catch-handler-label)
  (result-reg nil :reader vm-catch-result-reg)
  (:sexp-tag :establish-catch))

(define-vm-instruction vm-throw (vm-instruction)
  "Throw a value to a matching catch tag."
  (tag-reg nil :reader vm-throw-tag-reg)
  (value-reg nil :reader vm-throw-value-reg)
  (:sexp-tag :throw))

(defmethod execute-instruction ((inst vm-establish-catch) state pc labels)
  (declare (ignore labels))
  (let ((tag-value (vm-reg-get state (vm-catch-tag-reg inst)))
        (saved-regs (let ((copy (make-hash-table :test (hash-table-test (vm-state-registers state)))))
                      (maphash (lambda (k v) (setf (gethash k copy) v))
                               (vm-state-registers state))
                      copy)))
    (push (list :catch
                (vm-catch-handler-label inst)
                (vm-catch-result-reg inst)
                tag-value
                (vm-call-stack state)
                saved-regs
                (vm-method-call-stack state))
          (vm-handler-stack state)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-throw) state pc labels)
  (let ((tag-value (vm-reg-get state (vm-throw-tag-reg inst)))
        (throw-value (vm-reg-get state (vm-throw-value-reg inst))))
    ;; Walk handler stack to find matching catch frame
    (let ((matching nil) (to-skip 0))
      (dolist (entry (vm-handler-stack state))
        (if (and (eq (first entry) :catch)
                 (eql (fourth entry) tag-value))
            (progn (setf matching entry) (return))
            (incf to-skip)))
      (if matching
          (progn
            ;; Pop all handlers up to and including the matching one
            (dotimes (i (1+ to-skip))
              (pop (vm-handler-stack state)))
            (let ((handler-label (second matching))
                  (result-reg (third matching))
                  (saved-call-stack (fifth matching))
                  (saved-regs (sixth matching))
                  (saved-method-stack (seventh matching)))
              (setf (vm-call-stack state) saved-call-stack)
              (setf (vm-method-call-stack state) (or saved-method-stack nil))
              (clrhash (vm-state-registers state))
              (maphash (lambda (k v) (setf (gethash k (vm-state-registers state)) v))
                       saved-regs)
               (vm-reg-set state result-reg throw-value)
               (values (vm-label-table-lookup labels handler-label) nil nil)))
          (error "No catch tag ~S is active" tag-value)))))

;;; ── Label table and CLOS-based execution loop ────────────────────────────

(defun build-label-table (instructions)
  "Build an integer-keyed hash-table mapping label-name buckets → pc.

The outer hash table uses EQL on integer hash keys, while collisions are kept
in per-key buckets keyed by the original label object."
  (let ((labels (make-hash-table :test #'eql)))
    (loop for inst in instructions
          for pc from 0
          do (when (typep inst 'vm-label)
               (vm-label-table-store labels (vm-name inst) pc)))
    labels))

(defun run-program-slice (instructions labels start-pc state)
  "Execute INSTRUCTIONS (a vector) from START-PC using LABELS and STATE.
Returns the halted result value, or NIL if execution falls off the end.
Used by run-string-repl for incremental REPL execution."
  (loop with pc = start-pc
        while (< pc (length instructions))
        do (multiple-value-bind (next-pc halted result)
               (execute-instruction (aref instructions pc) state pc labels)
             (when halted
               (return result))
             (setf pc next-pc))
        finally (return nil)))

(defun run-compiled (program &key (output-stream *standard-output*) state)
  "Run a compiled VM program.
If STATE is provided, execute using that existing vm-io-state (for REPL persistence).
Otherwise a fresh state is created from OUTPUT-STREAM."
  (let* ((instructions (vm-program-instructions program))
         (labels (build-label-table instructions))
         (flat (coerce instructions 'vector))
         (state (or state (make-instance 'vm-io-state :output-stream output-stream))))
    (when (and (vm-profile-enabled-p state)
               (null (vm-profile-call-stack state)))
      (setf (vm-profile-call-stack state) (list "<toplevel>")))
    (let ((*vm-exec-flat* flat)
          (*vm-exec-labels* labels))
      (loop with pc = 0
            while (< pc (length flat))
            do (progn
                 (vm-profile-sample state)
                 (multiple-value-bind (next-pc halted result)
                    (execute-instruction (aref flat pc) state pc labels)
                   (when halted
                     (return result))
                   (setf pc next-pc)))
            finally (return nil)))))

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

(defun vm2-fuse-immediate-superinstructions (code)
  "Fuse const+arith bytecode pairs into existing immediate opcodes.

This is a conservative VM2 superinstruction helper. It recognizes:
  const rX IMM ; add2 dst src rX  -> add-imm2 dst src IMM
  const rX IMM ; add2 dst rX src  -> add-imm2 dst src IMM
  const rX IMM ; sub2 dst src rX  -> sub-imm2 dst src IMM
  const rX IMM ; mul2 dst src rX  -> mul-imm2 dst src IMM
  const rX IMM ; mul2 dst rX src  -> mul-imm2 dst src IMM
The helper leaves all other instruction sequences unchanged." 
  (let ((out (make-array 0 :adjustable t :fill-pointer 0))
        (n (length code))
        (pc 0)
        (op2-const (symbol-value '+op2-const+))
        (op2-add2 (symbol-value '+op2-add2+))
        (op2-add-imm2 (symbol-value '+op2-add-imm2+))
        (op2-sub2 (symbol-value '+op2-sub2+))
        (op2-sub-imm2 (symbol-value '+op2-sub-imm2+))
        (op2-mul2 (symbol-value '+op2-mul2+))
        (op2-mul-imm2 (symbol-value '+op2-mul-imm2+))
        (op2-halt2 (symbol-value '+op2-halt2+))
        (op2-const-halt2 (symbol-value '+op2-const-halt2+))
        (op2-num-eq2 (symbol-value '+op2-num-eq2+))
        (op2-num-lt2 (symbol-value '+op2-num-lt2+))
        (op2-num-gt2 (symbol-value '+op2-num-gt2+))
        (op2-num-le2 (symbol-value '+op2-num-le2+))
        (op2-num-ge2 (symbol-value '+op2-num-ge2+))
        (op2-num-eq-imm2 (symbol-value '+op2-num-eq-imm2+))
        (op2-num-lt-imm2 (symbol-value '+op2-num-lt-imm2+))
        (op2-num-gt-imm2 (symbol-value '+op2-num-gt-imm2+))
        (op2-num-le-imm2 (symbol-value '+op2-num-le-imm2+))
        (op2-num-ge-imm2 (symbol-value '+op2-num-ge-imm2+)))
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
                     (cond
                        ((and (= op1 op2-const) (= op2 op2-add2) (= src22 dst1))
                         (emit4 op2-add-imm2 dst2 src21 imm1)
                         (incf pc 8))
                        ((and (= op1 op2-const) (= op2 op2-add2) (= src21 dst1))
                         (emit4 op2-add-imm2 dst2 src22 imm1)
                         (incf pc 8))
                        ((and (= op1 op2-const) (= op2 op2-sub2) (= src22 dst1))
                         (emit4 op2-sub-imm2 dst2 src21 imm1)
                         (incf pc 8))
                        ((and (= op1 op2-const) (= op2 op2-mul2) (= src22 dst1))
                         (emit4 op2-mul-imm2 dst2 src21 imm1)
                         (incf pc 8))
                        ((and (= op1 op2-const) (= op2 op2-mul2) (= src21 dst1))
                         (emit4 op2-mul-imm2 dst2 src22 imm1)
                         (incf pc 8))
                        ((and (= op1 op2-const) (= op2 op2-num-eq2) (= src22 dst1))
                         (emit4 op2-num-eq-imm2 dst2 src21 imm1)
                         (incf pc 8))
                        ((and (= op1 op2-const) (= op2 op2-num-eq2) (= src21 dst1))
                         (emit4 op2-num-eq-imm2 dst2 src22 imm1)
                         (incf pc 8))
                        ((and (= op1 op2-const) (= op2 op2-num-lt2) (= src22 dst1))
                         (emit4 op2-num-lt-imm2 dst2 src21 imm1)
                         (incf pc 8))
                        ((and (= op1 op2-const) (= op2 op2-num-gt2) (= src22 dst1))
                         (emit4 op2-num-gt-imm2 dst2 src21 imm1)
                         (incf pc 8))
                        ((and (= op1 op2-const) (= op2 op2-num-le2) (= src22 dst1))
                         (emit4 op2-num-le-imm2 dst2 src21 imm1)
                         (incf pc 8))
                        ((and (= op1 op2-const) (= op2 op2-num-ge2) (= src22 dst1))
                         (emit4 op2-num-ge-imm2 dst2 src21 imm1)
                         (incf pc 8))
                        ((and (= op1 op2-const) (= op2 op2-halt2) (= dst1 dst2))
                         (emit4 op2-const-halt2 imm1 nil nil)
                         (incf pc 8))
                        (t
                         (emit4 op1 dst1 imm1 aux1)
                         (incf pc 4))))
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

;;; ── Opcode definitions ───────────────────────────────────────────────────

(defopcode const
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (imm (svref code (+ pc 2))))
      (setf (svref regs dst) imm))
    (+ pc 4)))

(defopcode nop
  (lambda (state code pc regs)
    (declare (ignore state code regs))
    (+ pc 4)))

(defopcode load-const
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (literal (svref code (+ pc 2))))
      (setf (svref regs dst) literal))
    (+ pc 4)))

(defopcode load-nil
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1))))
      (setf (svref regs dst) nil))
    (+ pc 4)))

(defopcode load-true
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1))))
      (setf (svref regs dst) t))
    (+ pc 4)))

(defopcode load-fixnum
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (imm (svref code (+ pc 2))))
      (setf (svref regs dst) imm))
    (+ pc 4)))

(defopcode const-halt2
  (lambda (state code pc regs)
    (declare (ignore state regs))
    (throw 'vm-halt (svref code (+ pc 1)))))

(defopcode move
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2))))
      (setf (svref regs dst) (svref regs src)))
    (+ pc 4)))

(defopcode neg
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2))))
      (setf (svref regs dst) (- (svref regs src))))
    (+ pc 4)))

(defopcode inc
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2))))
      (setf (svref regs dst) (1+ (svref regs src))))
    (+ pc 4)))

(defopcode dec
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2))))
      (setf (svref regs dst) (1- (svref regs src))))
    (+ pc 4)))

(defopcode fixnump
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2))))
      (setf (svref regs dst) (if (typep (svref regs src) 'fixnum) 1 0)))
    (+ pc 4)))

(defopcode consp
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2))))
      (setf (svref regs dst) (if (consp (svref regs src)) 1 0)))
    (+ pc 4)))

(defopcode symbolp
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2))))
      (setf (svref regs dst) (if (symbolp (svref regs src)) 1 0)))
    (+ pc 4)))

(defopcode functionp
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2))))
      (setf (svref regs dst) (if (functionp (svref regs src)) 1 0)))
    (+ pc 4)))

(defopcode stringp
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2))))
      (setf (svref regs dst) (if (stringp (svref regs src)) 1 0)))
    (+ pc 4)))

(defopcode cons
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (car-reg (svref code (+ pc 2)))
          (cdr-reg (svref code (+ pc 3))))
      (setf (svref regs dst) (cons (svref regs car-reg) (svref regs cdr-reg))))
    (+ pc 4)))

(defopcode car
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2))))
      (setf (svref regs dst) (car (svref regs src))))
    (+ pc 4)))

(defopcode cdr
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2))))
      (setf (svref regs dst) (cdr (svref regs src))))
    (+ pc 4)))

(defopcode make-vector
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (size-reg (svref code (+ pc 2)))
          (init-reg (svref code (+ pc 3))))
      (setf (svref regs dst)
            (make-array (svref regs size-reg) :initial-element (svref regs init-reg))))
    (+ pc 4)))

(defopcode vector-ref
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (vec-reg (svref code (+ pc 2)))
          (idx-reg (svref code (+ pc 3))))
      (setf (svref regs dst)
            (aref (svref regs vec-reg) (svref regs idx-reg))))
    (+ pc 4)))

(defopcode vector-set
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((vec-reg (svref code (+ pc 1)))
          (idx-reg (svref code (+ pc 2)))
          (src-reg (svref code (+ pc 3))))
      (setf (aref (svref regs vec-reg) (svref regs idx-reg))
            (svref regs src-reg)))
    (+ pc 4)))

(defopcode make-hash
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (size-reg (svref code (+ pc 2))))
      (setf (svref regs dst)
            (make-hash-table :test #'equal :size (max 1 (svref regs size-reg)))))
    (+ pc 4)))

(defopcode hash-ref
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (ht-reg (svref code (+ pc 2)))
          (key-reg (svref code (+ pc 3))))
      (setf (svref regs dst)
            (gethash (svref regs key-reg) (svref regs ht-reg))))
    (+ pc 4)))

(defopcode hash-set
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((ht-reg (svref code (+ pc 1)))
          (key-reg (svref code (+ pc 2)))
          (src-reg (svref code (+ pc 3))))
      (setf (gethash (svref regs key-reg) (svref regs ht-reg))
            (svref regs src-reg)))
    (+ pc 4)))

(defopcode get-global
  (lambda (state code pc regs)
    (let ((dst (svref code (+ pc 1)))
          (name (svref code (+ pc 2))))
      (setf (svref regs dst)
            (gethash name (vm2-state-global-vars state)))
      (+ pc 4))))

(defopcode set-global
  (lambda (state code pc regs)
    (let ((name (svref code (+ pc 1)))
          (src (svref code (+ pc 2))))
      (setf (gethash name (vm2-state-global-vars state))
            (svref regs src))
      (+ pc 4))))

(defopcode make-instance
  (lambda (state code pc regs)
    (declare (ignore state))
    (let* ((dst (svref code (+ pc 1)))
           (class-designator (svref code (+ pc 2)))
           (nargs (svref code (+ pc 3)))
           (initargs (loop for i below nargs collect (svref regs i))))
      (setf (svref regs dst) (apply #'make-instance class-designator initargs))
      (+ pc 4))))

(defopcode eq
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (if (eq (svref regs src1) (svref regs src2)) 1 0)))
    (+ pc 4)))

(defopcode eql
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (if (eql (svref regs src1) (svref regs src2)) 1 0)))
    (+ pc 4)))

(defopcode equal
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (if (equal (svref regs src1) (svref regs src2)) 1 0)))
    (+ pc 4)))

(defopcode add2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (+ (svref regs src1) (svref regs src2))))
    (+ pc 4)))

(defopcode add-imm2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2)))
          (imm (svref code (+ pc 3))))
      (setf (svref regs dst) (+ (svref regs src) imm)))
    (+ pc 4)))

(defopcode sub2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (- (svref regs src1) (svref regs src2))))
    (+ pc 4)))

(defopcode sub-imm2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2)))
          (imm (svref code (+ pc 3))))
      (setf (svref regs dst) (- (svref regs src) imm)))
    (+ pc 4)))

(defopcode mul2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (* (svref regs src1) (svref regs src2))))
    (+ pc 4)))

(defopcode div
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (/ (svref regs src1) (svref regs src2))))
    (+ pc 4)))

(defopcode mod
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (mod (svref regs src1) (svref regs src2))))
    (+ pc 4)))

(defopcode mul-imm2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2)))
          (imm (svref code (+ pc 3))))
      (setf (svref regs dst) (* (svref regs src) imm)))
    (+ pc 4)))

(defopcode num-eq2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (if (= (svref regs src1) (svref regs src2)) 1 0)))
    (+ pc 4)))

(defopcode num-lt2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (if (< (svref regs src1) (svref regs src2)) 1 0)))
    (+ pc 4)))

(defopcode num-gt2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (if (> (svref regs src1) (svref regs src2)) 1 0)))
    (+ pc 4)))

(defopcode num-le2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (if (<= (svref regs src1) (svref regs src2)) 1 0)))
    (+ pc 4)))

(defopcode num-ge2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (if (>= (svref regs src1) (svref regs src2)) 1 0)))
    (+ pc 4)))

(defopcode num-eq-imm2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2)))
          (imm (svref code (+ pc 3))))
      (setf (svref regs dst) (if (= (svref regs src) imm) 1 0)))
    (+ pc 4)))

(defopcode num-lt-imm2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2)))
          (imm (svref code (+ pc 3))))
      (setf (svref regs dst) (if (< (svref regs src) imm) 1 0)))
    (+ pc 4)))

(defopcode num-gt-imm2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2)))
          (imm (svref code (+ pc 3))))
      (setf (svref regs dst) (if (> (svref regs src) imm) 1 0)))
    (+ pc 4)))

(defopcode num-le-imm2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2)))
          (imm (svref code (+ pc 3))))
      (setf (svref regs dst) (if (<= (svref regs src) imm) 1 0)))
    (+ pc 4)))

(defopcode num-ge-imm2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2)))
          (imm (svref code (+ pc 3))))
      (setf (svref regs dst) (if (>= (svref regs src) imm) 1 0)))
    (+ pc 4)))

(defopcode jump
  (lambda (state code pc regs)
    (declare (ignore state regs))
    (+ pc (svref code (+ pc 1)))))

(defopcode jump-if-nil
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((src (svref code (+ pc 1)))
          (offset (svref code (+ pc 2))))
      (if (null (svref regs src))
          (+ pc offset)
          (+ pc 4)))))

(defopcode jump-if-true
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((src (svref code (+ pc 1)))
          (offset (svref code (+ pc 2))))
      (if (svref regs src)
          (+ pc offset)
          (+ pc 4)))))

(defopcode values
  (lambda (state code pc regs)
    (let ((nvals (svref code (+ pc 1))))
      (setf (vm2-state-values-buffer state)
            (loop for i below nvals collect (svref regs i)))
      (+ pc 4))))

(defopcode recv-values
  (lambda (state code pc regs)
    (let* ((nvals (svref code (+ pc 1)))
           (vals (vm2-state-values-buffer state)))
      (loop for i below nvals
            for value in vals
            do (setf (svref regs i) value)
            finally (when (< (length vals) nvals)
                      (loop for j from (length vals) below nvals
                            do (setf (svref regs j) nil))))
      (+ pc 4))))

(defopcode return
  (lambda (state code pc regs)
    (declare (ignore state pc))
    (throw 'vm-halt (svref regs (svref code (+ pc 1))))))

(defopcode return-nil
  (lambda (state code pc regs)
    (declare (ignore state code pc regs))
    (throw 'vm-halt nil)))

(defopcode halt2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((result-reg (svref code (+ pc 1))))
      (throw 'vm-halt (svref regs result-reg)))))

;;; ── Flat-vector interpreter ───────────────────────────────────────────────

(defun %run-vm-core (code state &key bigram-counts)
  "Shared VM2 interpreter core.

When BIGRAM-COUNTS is non-nil, counts executed opcode bigrams keyed by opcode
name symbol pairs." 
  (let* ((code (vm2-fuse-immediate-superinstructions code))
         (regs (vm2-state-registers state))
         (len  (length code))
         (pc   0)
         (prev-op nil))
    (catch 'vm-halt
      (loop while (< pc len)
            do (let ((op (svref code pc)))
                 (when (and bigram-counts prev-op)
                   (let ((name-a (aref *opcode-name-table* prev-op))
                         (name-b (aref *opcode-name-table* op)))
                     (when (and name-a name-b)
                       (incf (gethash (list name-a name-b) bigram-counts 0)))))
                 (setf prev-op op)
                 (setf pc
                       (cond
                         ((= op +op2-const+)
                          (let ((dst (svref code (+ pc 1)))
                                (imm (svref code (+ pc 2))))
                            (setf (svref regs dst) imm)
                            (+ pc 4)))
                         ((= op +op2-move+)
                          (let ((dst (svref code (+ pc 1)))
                                (src (svref code (+ pc 2))))
                            (setf (svref regs dst) (svref regs src))
                            (+ pc 4)))
                         ((= op +op2-add-imm2+)
                          (let ((dst (svref code (+ pc 1)))
                                (src (svref code (+ pc 2)))
                                (imm (svref code (+ pc 3))))
                            (setf (svref regs dst) (+ (svref regs src) imm))
                            (+ pc 4)))
                         ((= op +op2-sub-imm2+)
                          (let ((dst (svref code (+ pc 1)))
                                (src (svref code (+ pc 2)))
                                (imm (svref code (+ pc 3))))
                            (setf (svref regs dst) (- (svref regs src) imm))
                            (+ pc 4)))
                         ((= op +op2-mul-imm2+)
                          (let ((dst (svref code (+ pc 1)))
                                (src (svref code (+ pc 2)))
                                (imm (svref code (+ pc 3))))
                            (setf (svref regs dst) (* (svref regs src) imm))
                            (+ pc 4)))
                         ((= op +op2-halt2+)
                          (throw 'vm-halt (svref regs (svref code (+ pc 1)))))
                         ((= op +op2-const-halt2+)
                          (throw 'vm-halt (svref code (+ pc 1))))
                         (t
                          (let ((handler (aref *opcode-dispatch-table* op)))
                            (funcall handler state code pc regs))))))
            finally (return nil)))))

(defun run-vm (code state)
  "Run bytecode CODE (a simple-vector) using STATE (a vm2-state struct).
Returns the value in the result register when halt2 executes."
  (declare (type simple-vector code)
           (type vm2-state state))
  (%run-vm-core code state))

(defun run-vm-with-opcode-bigrams (code state)
  "Run CODE and return two values: result and executed opcode bigram counts." 
  (declare (type simple-vector code)
           (type vm2-state state))
  (let ((counts (make-hash-table :test #'equal)))
    (values (%run-vm-core code state :bigram-counts counts)
            counts)))

;;; ── vm2-state compatibility shims ────────────────────────────────────────
;;;
;;; These make vm2-state accessible via the same API as vm-state, allowing
;;; tests to use make-vm-state, vm-state-registers, vm-reg-get, vm-reg-set,
;;; vm-output-stream, and vm-global-vars uniformly.

(defun make-vm-state (&key (output-stream *standard-output*))
  "Create a vm2-state. Preferred constructor for new code; vm-io-state is
for the legacy CLOS execute-instruction pipeline."
  (make-vm2-state :output-stream output-stream))

(defmethod vm-state-registers ((s vm2-state))
  (vm2-state-registers s))

(defmethod vm-output-stream ((s vm2-state))
  (vm2-state-output-stream s))

(defmethod vm-global-vars ((s vm2-state))
  (vm2-state-global-vars s))

;;; vm-reg-get/vm-reg-set dispatch on vm2-state (svref) vs vm-state (gethash)
(defun vm-reg-get (state reg)
  (if (vm2-state-p state)
      (svref (vm2-state-registers state) reg)
      (gethash reg (slot-value state 'registers) 0)))

(defun vm-reg-set (state reg value)
  (if (vm2-state-p state)
      (setf (svref (vm2-state-registers state) reg) value)
      (setf (gethash reg (slot-value state 'registers)) value))
  value)
