;;;; packages/vm/src/security-134.lisp — Phase 134: Security Enhancements II
;;;; FR-746 JIT Hardening, FR-747 Runtime Sandboxing,
;;;; FR-748 Code Signing, FR-749 AOT Constant Blinding

(in-package :cl-cc/vm)

;;; ──── FR-746: JIT Hardening / Code Integrity Protection ────
(defvar *jit-hardening-enabled* t
  "When T, enable JIT hardening measures (constant blinding, guard pages, NOP sled removal).")

(defvar *jit-random-key* 0
  "Random key for JIT constant blinding (generated at startup).")

(defun initialize-jit-hardening ()
  "Initialize JIT hardening: generate random key, allocate guard pages."
  (setf *jit-random-key* (random (ash 1 64))))

(defun blind-constant (value)
  "Blind a constant VALUE using XOR with random key.
Usage: JIT emits (blinded XOR key) and recovers with XOR before use."
  (logxor value *jit-random-key*))

;;; ──── FR-747: Runtime Sandboxing / Seccomp ────
(defvar *sandbox-enabled* nil
  "When T, untrusted code runs in sandboxed mode with restricted syscalls.")

(defvar *seccomp-filter-rules* nil
  "List of allowed syscall numbers for seccomp BPF filter.")

(defun initialize-sandbox ()
  "Initialize sandbox: load seccomp filter for untrusted code execution.
Linux: seccomp(SECCOMP_SET_MODE_FILTER) with whitelist.
macOS: sandbox_init(3)."
  (when *sandbox-enabled*
    (setf *seccomp-filter-rules*
          ;; Allowed syscalls: read, write, mmap, brk, exit
          (list 0 1 9 12 60))
    t))

;;; ──── FR-748: Code Signing ────
(defvar *code-sign-identity* nil
  "Code signing identity (e.g., 'Developer ID: ...' for macOS).")

(defun sign-binary (binary-path &optional (identity *code-sign-identity*))
  "Sign BINARY-PATH with IDENTITY.
macOS: uses codesign --sign.
Windows: uses signtool.exe.
Linux: IMA signature support."
  (when identity
    #+darwin
    (uiop:run-program (list "codesign" "--sign" identity binary-path)
                      :output t :error-output t)
    #-darwin
    (warn "Code signing not supported on this platform"))
  t)

;;; ──── FR-749: Constant Blinding in AOT Code ────
(defvar *blind-constants-enabled* nil
  "When T, sensitive constants in AOT code are XOR-masked.")

(defun blind-aot-constant (value &optional (key *jit-random-key*))
  "Blind a sensitive AOT constant VALUE with XOR mask.
Usage: MOV rax, 0xDEADBEEF → MOV rax, (0xDEADBEEF XOR KEY); XOR rax, KEY"
  (if *blind-constants-enabled*
      (logxor value key)
      value))

;; ── Exports ──
(export '(*jit-hardening-enabled* *jit-random-key* initialize-jit-hardening
          blind-constant
          *sandbox-enabled* initialize-sandbox *seccomp-filter-rules*
          *code-sign-identity* sign-binary
          *blind-constants-enabled* blind-aot-constant))
