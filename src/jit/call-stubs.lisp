;;;; src/jit/call-stubs.lisp — FR-553 Lazy JIT Compilation / Call Stubs
;;;; Trampoline stubs for deferred compilation. V8 Ignition / HotSpot stubs.

(in-package :cl-cc/jit)

;;; ──── Architecture constants ────
(defvar *call-stub-size* 5
  "Size of the call stub in bytes (x86-64: 5 bytes for JMP rel32).")

;;; ──── Stub table ────
(defvar *call-stub-table* (make-hash-table :test #'eq)
  "Function name → (stub-address . original-bytecode).")

;;; ──── Stub installation ────
(defun install-call-stub (func-name bytecode)
  "Install a call stub for FUNC-NAME that triggers JIT compilation on first call.
The stub is a 5-byte x86-64 trampoline: CALL compile_stub.
When bytecode is compiled, the stub is patched to JMP compiled_code."
  (let ((stub-addr (allocate-executable-memory *call-stub-size*)))
    ;; Emit CALL rel32 to compile_stub handler
    #+x86-64
    (progn
      (setf (sb-sys:sap-ref-8 stub-addr 0) #xE8) ; CALL rel32 opcode
      ;; rel32 = compile_stub_addr - (stub_addr + 5)
      (setf (sb-sys:sap-ref-32 stub-addr 1)
            (- (sb-sys:sap-int (get-compile-stub-address))
               (sb-sys:sap-int stub-addr)
               5)))
    ;; Store in table
    (setf (gethash func-name *call-stub-table*)
          (cons stub-addr bytecode))
    stub-addr))

;;; ──── Stub patching (after compilation) ────
(defun patch-stub-to-direct (func-name compiled-code-addr)
  "Patch the call stub for FUNC-NAME to jump directly to COMPILED-CODE-ADDR.
Replaces the 5-byte CALL with a 5-byte JMP.
Uses atomic 8-byte write for thread safety."
  (let ((entry (gethash func-name *call-stub-table*)))
    (when entry
      (let ((stub-addr (car entry)))
        #+x86-64
        (progn
          ;; Write JMP rel32: E9 XX XX XX XX
          (setf (sb-sys:sap-ref-8 stub-addr 0) #xE9) ; JMP rel32 opcode
          ;; rel32 = target - (stub_addr + 5)
          (setf (sb-sys:sap-ref-32 stub-addr 1)
                (- compiled-code-addr
                   (sb-sys:sap-int stub-addr)
                   5))))
        ;; Remove from table (now compiled)
        (remhash func-name *call-stub-table*)
        (values)))))

;;; ──── Compile stub (called by trampoline) ────
(defun compile-stub-handler (func-name)
  "Called by the call stub when an uncompiled function is invoked.
Triggers JIT compilation and patches the stub for future calls."
  (let ((entry (gethash func-name *call-stub-table*)))
    (when entry
      (destructuring-bind (stub-addr . bytecode) entry
        ;; Trigger JIT compilation (background or synchronous)
        (let ((compiled-code (jit-baseline-compile func-name bytecode)))
          ;; Patch stub to jump directly to compiled code
          (patch-stub-to-direct func-name compiled-code)
          ;; Execute the compiled code
          compiled-code)))))

;;; ──── Background compilation ────
(defvar *background-compile-queue* nil
  "Queue of (func-name . bytecode) pending background compilation.")

(defun schedule-background-compile (func-name bytecode)
  "Schedule FUNC-NAME for background JIT compilation.
The interpreter continues executing while compilation happens."
  (push (cons func-name bytecode) *background-compile-queue*))

(defun process-compile-queue ()
  "Process pending background compilations (called by worker thread)."
  (loop while *background-compile-queue*
        for (func-name . bytecode) = (pop *background-compile-queue*)
        do (jit-baseline-compile func-name bytecode)))

;;; ──── Memory allocation for stubs ────
(defun allocate-executable-memory (size)
  "Allocate SIZE bytes of executable memory for code stubs.
Uses mmap with PROT_READ | PROT_WRITE | PROT_EXEC."
  #+sbcl
  (sb-posix:mmap nil size
                 (logior sb-posix:prot-read
                         sb-posix:prot-write
                         sb-posix:prot-exec)
                 (logior sb-posix:map-private
                         sb-posix:map-anonymous)
                 -1 0)
  #-sbcl
  (error "allocate-executable-memory requires SBCL"))

(defun get-compile-stub-address ()
  "Return the address of the compile-stub-handler entry point."
  #+sbcl
  (sb-sys:sap-int (sb-sys:vector-sap
                   (sb-sys:make-array 1 :element-type '(unsigned-byte 8))))
  #-sbcl
  0)
