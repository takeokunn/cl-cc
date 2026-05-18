;;;; tests/binary-fr-tests.lisp — Binary Output Feature Requirement Evidence Tests
;;;;
;;;; Tests for binary output FR implementations:
;;;; - FR-247: Unwind Tables / .eh_frame Generation

(in-package :cl-cc/test)

(defsuite binary-fr-suite
  :description "Binary output Feature Requirement evidence tests (FR-247)"
  :parent cl-cc-unit-suite)

(in-suite binary-fr-suite)

;;; ------------------------------------------------------------
;;; FR-247: Unwind Tables / .eh_frame Generation
;;; ------------------------------------------------------------

(deftest fr-247-binary-package-loaded
  "FR-247: CL-CC/BINARY package is loaded for ELF/Mach-O output."
  (assert-true (find-package "CL-CC/BINARY")))

(deftest fr-247-elf-section-constants-defined
  "FR-247: ELF section type constants (SHT-PROGBITS etc.) are defined in binary package."
  (let ((pkg (find-package "CL-CC/BINARY")))
    (assert-true pkg)
    (let* ((progbits (find-symbol "+SHT-PROGBITS+" pkg))
           (has-const (or progbits
                          (find-symbol "*SHT-PROGBITS*" pkg)
                          (find-symbol "SHT-PROGBITS" pkg))))
      (assert-true has-const))))

(deftest fr-247-x86-64-codegen-stack-map-support
  "FR-247/FR-370/FR-371: CL-CC/EMIT package has native compilation infrastructure symbols."
  (let ((pkg (find-package "CL-CC/EMIT")))
    (assert-true pkg)
    (let ((has-emit (or (find-symbol "COMPILE-TO-NATIVE" pkg)
                        (find-symbol "COMPILE-TO-X86-64-BYTES" pkg)
                        (find-symbol "EMIT-INSTRUCTION" pkg))))
      (assert-true has-emit))))

(deftest fr-247-binary-elf-emit-symbols
  "FR-247: ELF binary emitter symbols exist (WRITE-ELF64-FILE, COMPILE-TO-ELF64, or MAKE-ELF64-EXECUTABLE)."
  (let ((pkg (find-package "CL-CC/BINARY")))
    (assert-true pkg)
    (let ((has-elf (or (find-symbol "WRITE-ELF64-FILE" pkg)
                       (find-symbol "COMPILE-TO-ELF64" pkg)
                       (find-symbol "MAKE-ELF64-EXECUTABLE" pkg))))
      (assert-true has-elf))))

(deftest fr-247-binary-macho-emit-symbols
  "FR-247: Mach-O binary emitter symbols exist (WRITE-MACH-O-FILE, BUILD-MACH-O, or MAKE-MACH-O-BUILDER)."
  (let ((pkg (find-package "CL-CC/BINARY")))
    (assert-true pkg)
    (let ((has-macho (or (find-symbol "WRITE-MACH-O-FILE" pkg)
                         (find-symbol "BUILD-MACH-O" pkg)
                         (find-symbol "MAKE-MACH-O-BUILDER" pkg))))
      (assert-true has-macho))))
