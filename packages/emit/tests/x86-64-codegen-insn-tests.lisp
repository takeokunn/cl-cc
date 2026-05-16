;;;; tests/unit/emit/x86-64-codegen-insn-tests.lisp — x86-64 Individual Instruction Emitter Tests
;;;;
;;;; Tests for individual x86-64 instruction emitters in src/emit/x86-64-codegen.lisp:
;;;; emit-vm-bswap, emit-vm-add, emit-vm-select, emit-vm-jump-zero-inst,
;;;; emit-vm-logcount, emit-vm-integer-length, emit-vm-call-like-inst,
;;;; emit-vm-tail-call-inst
;;;;
;;;; Depends on %x86-collect-bytes helper defined in x86-64-codegen-tests.lisp
;;;; (loaded before this file via :serial t ASDF).

(in-package :cl-cc/test)

(in-suite x86-64-codegen-suite)

(deftest x86-64-bswap-emitter-encoding
  "emit-vm-bswap emits a MOV followed by BSWAP r32."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-vm-bswap (cl-cc:make-vm-bswap :dst :R0 :src :R1) s)))))
    (assert-= 5 (length bytes))
    (assert-= #x48 (first bytes))
    (assert-= #x89 (second bytes))
    (assert-= #xC8 (third bytes))
    (assert-= #x0F (fourth bytes))
    (assert-= #xC8 (fifth bytes))))

(deftest x86-64-add-emitter-two-address-lowering
  "emit-vm-add lowers to MOV dst,lhs before ADD dst,rhs to satisfy x86 two-address form."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-vm-add
                   (cl-cc:make-vm-add :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (assert-= 6 (length bytes))
    ;; MOV rax,rcx / ADD rax,rdx
    (assert-= #x48 (nth 0 bytes))
    (assert-= #x89 (nth 1 bytes))
    (assert-= #xC8 (nth 2 bytes))
    (assert-= #x48 (nth 3 bytes))
    (assert-= #x01 (nth 4 bytes))
    (assert-= #xD0 (nth 5 bytes))))

(deftest x86-64-select-emitter-encoding
  "emit-vm-select uses MOV + TEST + CMOVNE branchless lowering."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-vm-select
                   (cl-cc:make-vm-select :dst :R0 :cond-reg :R1 :then-reg :R2 :else-reg :R3)
                   s)))))
    (assert-= 10 (length bytes))
    (assert-= #x48 (nth 0 bytes))
    (assert-= #x89 (nth 1 bytes))
    (assert-= #x48 (nth 3 bytes))
    (assert-= #x85 (nth 4 bytes))
    (assert-= #x48 (nth 6 bytes))
    (assert-= #x0F (nth 7 bytes))
    (assert-= #x45 (nth 8 bytes))))

(deftest x86-64-jump-zero-test-je-adjacent
  "emit-vm-jump-zero-inst emits TEST immediately followed by JE rel32."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-vm-jump-zero-inst
                   (cl-cc:make-vm-jump-zero :reg :R1 :label "L1")
                   s 0 (let ((ht (make-hash-table :test #'equal)))
                         (setf (gethash "L1" ht) 9)
                         ht))))))
    (assert-= 9 (length bytes))
    (assert-= #x48 (nth 0 bytes))
    (assert-= #x85 (nth 1 bytes))
    (assert-= #x0F (nth 3 bytes))
    (assert-= #x84 (nth 4 bytes))))

(deftest x86-64-logcount-emitter-encoding
  "emit-vm-logcount emits POPCNT with the expected opcode sequence."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-vm-logcount
                   (cl-cc:make-vm-logcount :dst :R0 :src :R1) s)))))
    (assert-= 5 (length bytes))
    (assert-= #xF3 (nth 0 bytes))
    (assert-= #x48 (nth 1 bytes))
    (assert-= #x0F (nth 2 bytes))
    (assert-= #xB8 (nth 3 bytes))))

(deftest x86-64-integer-length-emitter-encoding
  "emit-vm-integer-length emits xor/test/je/bsr/add sequence."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-vm-integer-length
                   (cl-cc:make-vm-integer-length :dst :R0 :src :R1) s)))))
    (assert-= 16 (length bytes))
    ;; xor rax,rax / test rcx,rcx / je rel8 / bsr rax,rcx / add rax,1
    (assert-= #x48 (nth 0 bytes))
    (assert-= #x31 (nth 1 bytes))
    (assert-= #x48 (nth 3 bytes))
    (assert-= #x85 (nth 4 bytes))
    (assert-= #x74 (nth 6 bytes))
    (assert-= #x48 (nth 8 bytes))
    (assert-= #x0F (nth 9 bytes))
    (assert-= #xBD (nth 10 bytes))))

(deftest x86-64-call-encoding
  "vm-call emits CALL r64 (#xFF /2) followed by MOV dst←rax: 6 bytes total."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-vm-call-like-inst
                   (cl-cc:make-vm-call :dst :R0 :func :R1 :args nil) s)))))
    (assert-= 6 (length bytes))
    (assert-= #x48 (nth 0 bytes))
    (assert-= #xFF (nth 1 bytes))
    (assert-= #xD1 (nth 2 bytes))
    (assert-= #x48 (nth 3 bytes))
    (assert-= #x89 (nth 4 bytes))
    (assert-= #xC0 (nth 5 bytes))))

(deftest x86-64-tail-call-encoding
  "vm-tail-call emits JMP r64 (#xFF /4) only: 3 bytes, no move-to-result register."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-vm-tail-call-inst
                   (cl-cc:make-vm-tail-call :dst :R0 :func :R1 :args nil) s)))))
    (assert-= 3 (length bytes))
    (assert-= #x48 (nth 0 bytes))
    (assert-= #xFF (nth 1 bytes))
    (assert-= #xE1 (nth 2 bytes))))

(deftest x86-64-call-encoding-retpoline
  "vm-call emits retpoline-expanded sequence when enabled."
  (let ((bytes (let ((cl-cc/codegen::*x86-64-use-retpoline* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-call-like-inst
                     (cl-cc:make-vm-call :dst :R0 :func :R1 :args nil) s))))))
    (assert-= 44 (length bytes))
    ;; starts with CALL rel32
    (assert-= #xE8 (nth 0 bytes))
    ;; includes PAUSE + LFENCE in thunk capture loop
    (assert-true (find #xF3 bytes))
    (assert-true (find #xAE bytes))))

(deftest x86-64-tail-call-encoding-retpoline
  "vm-tail-call emits retpoline-expanded sequence when enabled."
  (let ((bytes (let ((cl-cc/codegen::*x86-64-use-retpoline* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-tail-call-inst
                     (cl-cc:make-vm-tail-call :dst :R0 :func :R1 :args nil) s))))))
    (assert-= 20 (length bytes))
    (assert-= #xE8 (nth 0 bytes))
    (assert-true (find #xF3 bytes))
    (assert-= #xC3 (car (last bytes)))))

(deftest x86-64-call-cfi-guard-avoids-clobbering-rax-target
  "CFI guard uses a non-RAX scratch register when indirect target is RAX."
  (let ((bytes (let ((cl-cc/codegen::*x86-64-cfi-enabled* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-call-like-inst
                     (cl-cc:make-vm-call :dst :R1 :func :R0 :args nil) s))))))
    ;; CALL RAX must remain at end of non-retpoline path.
    (assert-equal '(#x48 #xFF #xD0) (subseq bytes (- (length bytes) 6) (- (length bytes) 3)))
    ;; Guard must not clobber target via `mov rax, [rax]`.
    (assert-false (search '(#x48 #x8B #x00) bytes :test #'eql))))

(deftest x86-64-shadow-stack-control-inst-emits-nop-when-shadow-stack-disabled
  "Non-local control VM instructions reserve a 2-byte integration slot when shadow-stack is disabled."
  (let ((bytes (let ((cl-cc/codegen::*x86-64-shadow-stack-enabled* nil))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-instruction-with-labels
                     (cl-cc/vm::make-vm-push-handler :handler-type :error :handler-label "Lh" :result-reg :R0)
                     s
                     0
                     (make-hash-table :test #'equal)))))))
    (assert-= 2 (length bytes))
    (assert-= #x66 (nth 0 bytes))
    (assert-= #x90 (nth 1 bytes))))

(deftest x86-64-shadow-stack-control-inst-emits-saveprevssp-when-enabled
  "Non-local control push/bind paths emit CET SAVEPREVSSP-based save slot when enabled."
  (let ((bytes (let ((cl-cc/codegen::*x86-64-shadow-stack-enabled* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-instruction-with-labels
                     (cl-cc/vm::make-vm-push-handler :handler-type :error :handler-label "Lh" :result-reg :R0)
                     s
                     0
                     (make-hash-table :test #'equal)))))))
    (assert-= 6 (length bytes))
    (assert-equal '(#xF3 #x0F #x01 #xEA #x66 #x90) bytes)))

(deftest x86-64-shadow-stack-control-inst-uses-distinct-restore-marker
  "Pop/invoke restart paths emit CET RSTORSSP-based restore slot."
  (let ((bytes (let ((cl-cc/codegen::*x86-64-shadow-stack-enabled* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-instruction-with-labels
                     (cl-cc/vm::make-vm-pop-handler)
                     s
                     0
                     (make-hash-table :test #'equal)))))))
    (assert-= 6 (length bytes))
    (assert-equal '(#xF3 #x0F #x01 #x28 #x66 #x90) bytes)))

(deftest x86-64-shadow-stack-control-inst-uses-incsspq-for-adjust-paths
  "Condition/error non-local paths emit CET INCSSPQ-based adjust slot."
  (let ((bytes (let ((cl-cc/codegen::*x86-64-shadow-stack-enabled* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-instruction-with-labels
                     (cl-cc/vm::make-vm-signal :condition-reg :R0)
                     s
                     0
                     (make-hash-table :test #'equal)))))))
    (assert-= 6 (length bytes))
    (assert-equal '(#xF3 #x48 #x0F #xAE #xE8 #x90) bytes)))

(deftest x86-64-shadow-stack-control-inst-covers-vm-establish-catch
  "VM catch establishment path also lowers to CET save slot when enabled."
  (let ((bytes (let ((cl-cc/codegen::*x86-64-shadow-stack-enabled* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-instruction-with-labels
                     (cl-cc/vm::make-vm-establish-catch
                      :tag-reg :R0 :handler-label "Lc" :result-reg :R1)
                     s
                     0
                     (make-hash-table :test #'equal)))))))
    (assert-= 6 (length bytes))
    (assert-equal '(#xF3 #x0F #x01 #xEA #x66 #x90) bytes)))

(deftest x86-64-shadow-stack-control-inst-covers-vm-remove-handler
  "VM handler removal path lowers to CET restore slot when enabled."
  (let ((bytes (let ((cl-cc/codegen::*x86-64-shadow-stack-enabled* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-instruction-with-labels
                     (cl-cc/vm::make-vm-remove-handler)
                     s
                     0
                     (make-hash-table :test #'equal)))))))
    (assert-= 6 (length bytes))
    (assert-equal '(#xF3 #x0F #x01 #x28 #x66 #x90) bytes)))

(deftest x86-64-shadow-stack-control-inst-covers-vm-sync-handler-regs
  "VM handler snapshot sync path lowers to CET adjust slot when enabled."
  (let ((bytes (let ((cl-cc/codegen::*x86-64-shadow-stack-enabled* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-instruction-with-labels
                     (cl-cc/vm::make-vm-sync-handler-regs)
                     s
                     0
                     (make-hash-table :test #'equal)))))))
    (assert-= 6 (length bytes))
    (assert-equal '(#xF3 #x48 #x0F #xAE #xE8 #x90) bytes)))

(deftest x86-64-shadow-stack-control-inst-covers-vm-throw
  "VM throw path lowers to CET adjust slot when enabled."
  (let ((bytes (let ((cl-cc/codegen::*x86-64-shadow-stack-enabled* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-instruction-with-labels
                     (cl-cc/vm::make-vm-throw :tag-reg :R0 :value-reg :R1)
                     s
                     0
                     (make-hash-table :test #'equal)))))))
    (assert-= 6 (length bytes))
    (assert-equal '(#xF3 #x48 #x0F #xAE #xE8 #x90) bytes)))
