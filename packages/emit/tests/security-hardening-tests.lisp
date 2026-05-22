;;;; packages/emit/tests/security-hardening-tests.lisp — security hardening byte/header evidence

(in-package :cl-cc/test)

(defsuite security-hardening-suite
  :description "Security hardening and binary-protection FR byte/header tests"
  :parent cl-cc-unit-suite)

(in-suite security-hardening-suite)

(defun %security-byte-list (bytes)
  (coerce bytes 'list))

(defun %security-x86-bytes (thunk)
  (%security-byte-list
   (cl-cc/codegen::with-output-to-vector (out)
     (funcall thunk out))))

(defun %security-a64-bytes (thunk)
  (let ((bytes nil))
    (funcall thunk (lambda (byte) (push byte bytes)))
    (nreverse bytes)))

(defun %security-u16le (bytes offset)
  (+ (aref bytes offset)
     (ash (aref bytes (+ offset 1)) 8)))

(defun %security-u32le (bytes offset)
  (+ (aref bytes offset)
     (ash (aref bytes (+ offset 1)) 8)
     (ash (aref bytes (+ offset 2)) 16)
     (ash (aref bytes (+ offset 3)) 24)))

(defun %security-u64le (bytes offset)
  (loop for i below 8
        sum (ash (aref bytes (+ offset i)) (* 8 i))))

(defun %security-c-string (bytes offset)
  (with-output-to-string (out)
    (loop for i from offset below (length bytes)
          for byte = (aref bytes i)
          until (zerop byte)
          do (write-char (code-char byte) out))))

(defun %security-elf-section-names (bytes)
  (let* ((shoff (%security-u64le bytes 40))
         (shentsize (%security-u16le bytes 58))
         (shnum (%security-u16le bytes 60))
         (shstrndx (%security-u16le bytes 62))
         (shstr-header (+ shoff (* shstrndx shentsize)))
         (shstr-offset (%security-u64le bytes (+ shstr-header 24))))
    (loop for i below shnum
          for header = (+ shoff (* i shentsize))
          for name-offset = (%security-u32le bytes header)
          collect (%security-c-string bytes (+ shstr-offset name-offset)))))

(defun %security-indirect-call-program ()
  (cl-cc/vm::make-vm-program
   :instructions (list (cl-cc:make-vm-call :dst :R0 :func :R1 :args nil)
                       (cl-cc:make-vm-halt :reg :R0))
   :result-register :R0
   :leaf-p nil))

(defun %security-stack-buffer-program ()
  (cl-cc/vm::make-vm-program
   :instructions (list (cl-cc:make-vm-const :dst :R0 :value 4)
                       (cl-cc:make-vm-halt :reg :R0))
   :result-register :R0
   :leaf-p nil))

(deftest fr-530-cfi-encoders-emit-endbr64-and-bti-c
  "FR-530: x86-64 ENDBR64 and AArch64 BTI C encoders emit architectural bytes."
  (assert-equal '(#xF3 #x0F #x1E #xFA)
                (%security-x86-bytes
                 (lambda (out)
                   (cl-cc/codegen::emit-x86-64-cfi-entry
                    out (cl-cc/codegen::x86-64-cfi-plan :has-indirect-calls-p t)))))
  (assert-equal '(#x5F #x24 #x03 #xD5)
                (%security-a64-bytes
                 (lambda (out)
                   (cl-cc/codegen::emit-aarch64-cfi-entry
                    out (cl-cc/codegen::aarch64-cfi-plan :has-indirect-calls-p t))))))

(deftest fr-531-aarch64-pac-encoders-and-program-bytes
  "FR-531: AArch64 PACIASP/AUTIASP encoder words appear in hardened program output."
  (assert-equal '(#x3F #x23 #x03 #xD5)
                (%security-a64-bytes
                 (lambda (out) (cl-cc/codegen::emit-a64-instr (cl-cc/codegen::encode-paciasp) out))))
  (assert-equal '(#xBF #x23 #x03 #xD5)
                (%security-a64-bytes
                 (lambda (out) (cl-cc/codegen::emit-a64-instr (cl-cc/codegen::encode-autiasp) out))))
  (let ((bytes (%security-byte-list
                (let ((cl-cc/codegen::*aarch64-pac-enabled* t))
                  (cl-cc/codegen::compile-to-aarch64-bytes
                   (%security-indirect-call-program))))))
    (assert-true (search '(#x3F #x23 #x03 #xD5) bytes :test #'eql))
    (assert-true (search '(#xBF #x23 #x03 #xD5) bytes :test #'eql))))

(deftest fr-532-stack-protector-flag-materializes-canary-bytes
  "FR-532: :STACK-PROTECTOR (CLI --stack-protector backend flag) produces FS canary code."
  (let ((bytes (%security-byte-list
                (cl-cc/codegen::compile-to-x86-64-bytes
                 (%security-stack-buffer-program)
                 :stack-protector t))))
    (assert-true (search '(#x64 #x48 #x8B #x04 #x25 #x28 #x00 #x00 #x00) bytes :test #'eql))
    (assert-true (search '(#x64 #x48 #x3B #x04 #x25 #x28 #x00 #x00 #x00) bytes :test #'eql))
    (assert-true (search '(#x0F #x0B) bytes :test #'eql))))

(deftest fr-534-spectre-mitigations-emit-lfence-retpoline-bytes
  "FR-534: speculation hardening emits LFENCE and retpoline capture bytes for indirect calls."
  (let ((bytes (%security-byte-list
                (cl-cc/codegen::compile-to-x86-64-bytes
                 (%security-indirect-call-program)
                 :spectre-mitigations t))))
    (assert-true (search '(#x0F #xAE #xE8) bytes :test #'eql))
    (assert-true (search '(#xF3 #x90 #x0F #xAE #xE8) bytes :test #'eql))))

(deftest fr-651-elf-got-plt-relocation-entries-are-materialized
  "FR-651: GOT/PLT helpers reserve GOT data, emit PLT stub bytes, and record relocation entries."
  (let ((builder (cl-cc/binary::make-elf64-object)))
    (assert-equal 0 (cl-cc/binary::elf64-add-got-entry builder "puts"))
    (assert-equal 0 (cl-cc/binary::elf64-add-plt-stub builder "puts"))
    (assert-equal '(#xFF #x25 #x00 #x00 #x00 #x00 #x0F #x1F #x40 #x00)
                  (%security-byte-list (cl-cc/binary::binary-buffer-to-array
                                        (cl-cc/binary::elf64-text-buf builder))))
    (let ((reloc (first (cl-cc/binary::elf64-rela-entries builder))))
      (assert-equal 2 (first reloc))
      (assert-equal cl-cc/binary::+r-x86-64-pc32+ (second reloc))
      (assert-equal "puts" (third reloc))
      (assert-equal -4 (fourth reloc)))))

(deftest fr-652-split-dwarf-dwo-sections-and-debuglink-are-well-formed
  "FR-652: DWO helper emits .debug_*.dwo sections and a GNU debuglink CRC payload."
  (let* ((dwo (cl-cc/binary::build-dwo-file "unit.dwo"))
         (names (%security-elf-section-names dwo))
         (skeleton (cl-cc/binary::build-dwarf-skeleton-cu "unit.dwo"))
         (debuglink (cl-cc/binary::build-gnu-debuglink-section "unit.dwo" dwo)))
    (assert-true (member ".debug_info.dwo" names :test #'string=))
    (assert-true (member ".debug_abbrev.dwo" names :test #'string=))
    (assert-true (member ".debug_line.dwo" names :test #'string=))
    (assert-true (search '(#x30 #x21) (%security-byte-list skeleton) :test #'eql))
    (assert-equal "unit.dwo" (%security-c-string debuglink 0))
    (assert-equal 0 (mod (- (length debuglink) 4) 4))))

(deftest fr-771-safestack-dual-stack-hooks-are-in-full-program-output
  "FR-771: SafeStack emits TLS unsafe-stack load/store hooks in x86-64 and AArch64 program output."
  (let ((x86 (%security-byte-list
              (let ((cl-cc/codegen::*x86-64-safe-stack-enabled* t))
                (cl-cc/codegen::compile-to-x86-64-bytes
                 (cl-cc/vm::make-vm-program
                  :instructions (list (cl-cc:make-vm-const :dst :R0 :value 7)
                                      (cl-cc:make-vm-halt :reg :R0))
                  :result-register :R0
                  :leaf-p nil))))))
    (assert-true (search '(#x64 #x4C #x8B #x1C #x25 #x70 #x00 #x00 #x00) x86 :test #'eql))
    (assert-true (search '(#x64 #x4C #x89 #x1C #x25 #x70 #x00 #x00 #x00) x86 :test #'eql)))
  (let ((a64 (%security-byte-list
              (let ((cl-cc/codegen::*aarch64-safe-stack-enabled* t))
                (cl-cc/codegen::compile-to-aarch64-bytes
                 (cl-cc/vm::make-vm-program
                  :instructions (list (cl-cc:make-vm-const :dst :R0 :value 7)
                                      (cl-cc:make-vm-halt :reg :R0))
                  :result-register :R0
                  :leaf-p nil))))))
    ;; Both load and store paths begin by reading TPIDR_EL0 through MRS x16.
    (assert-true (search (%security-a64-bytes
                          (lambda (out)
                            (cl-cc/codegen::emit-a64-safe-stack-load-pointer
                             cl-cc/codegen::+a64-scs-tmp+ out)))
                         a64 :test #'eql))
    (assert-true (search (%security-a64-bytes
                          (lambda (out)
                            (cl-cc/codegen::emit-a64-safe-stack-store-pointer
                             cl-cc/codegen::+a64-scs-tmp+ out)))
                         a64 :test #'eql))))

(deftest fr-772-runtime-xom-effective-protection-removes-read-when-supported
  "FR-772: XOM runtime selects PROT_EXEC-only when enabled and supported, else RX fallback."
  (let ((cl-cc/runtime::*xom-enabled* nil))
    (assert-equal (logior cl-cc/runtime::+rt-prot-read+ cl-cc/runtime::+rt-prot-exec+)
                  (cl-cc/runtime::rt-xom-effective-prot)))
  (let ((cl-cc/runtime::*xom-enabled* t))
    (if (cl-cc/runtime::rt-xom-supported-p)
        (assert-equal cl-cc/runtime::+rt-prot-exec+
                      (cl-cc/runtime::rt-xom-effective-prot))
        (assert-equal (logior cl-cc/runtime::+rt-prot-read+ cl-cc/runtime::+rt-prot-exec+)
                      (cl-cc/runtime::rt-xom-effective-prot)))))
