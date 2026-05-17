;;;; packages/binary/src/pe.lisp - PE/COFF Binary Format (FR-292)
;;;;
;;;; Portable Executable format for Windows (.exe, .dll).
;;;; CURRENT STATUS: Skeleton.  Full implementation requires DOS stub,
;;;; PE header, section table, import/export tables, and x86-64 Windows
;;;; ABI (rcx/rdx/r8/r9 register passing, shadow space) support.

(in-package :cl-cc/binary)

;;; PE magic and machine constants
(defconstant +pe-magic-pe32+   #x10B "PE32 (32-bit)")
(defconstant +pe-magic-pe32++  #x20B "PE32+ (64-bit)")
(defconstant +pe-machine-x8664+ #x8664 "AMD64")
(defconstant +pe-machine-i386+  #x14C  "Intel 386")

;;; DOS Header constants
(defconstant +pe-dos-signature+ #x5A4D "MZ")
(defconstant +pe-dos-stub-offset+ 64 "Offset to 'This program cannot be run in DOS mode'")

;;; PE Signature
(defconstant +pe-signature+ #x00004550 "PE\\0\\0")

;;; Section flags
(defconstant +pe-section-text+  #x60000020 "code + execute + read")
(defconstant +pe-section-data+  #xC0000040 "initialized data + read + write")
(defconstant +pe-section-bss+   #xC0000080 "uninitialized data + read + write")
(defconstant +pe-section-rdata+ #x40000040 "read-only initialized data + read")

;;; Subsystem constants
(defconstant +pe-subsystem-native+     1 "Native")
(defconstant +pe-subsystem-windows-gui+ 2 "Windows GUI")
(defconstant +pe-subsystem-windows-cui+ 3 "Windows console")

;;; PE/COFF builder (skeleton)
(defstruct (pe-builder (:conc-name pe-))
  "Accumulates sections for a PE/COFF executable.  FR-292 skeleton."
  (machine +pe-machine-x8664+ :type (unsigned-byte 16))
  (subsystem +pe-subsystem-windows-cui+ :type (unsigned-byte 16))
  (text-bytes nil :type list)  ; list of (unsigned-byte 8)
  (data-bytes nil :type list)
  (entry-point 0 :type (unsigned-byte 64)))

(defun make-pe-executable (&key (machine +pe-machine-x8664+))
  "Create a PE/COFF builder skeleton. FR-292."
  (make-pe-builder :machine machine))

;; TODO: Full PE/COFF generation:
;; - DOS stub (64 bytes)
;; - PE signature + COFF header + optional header
;; - Section table (.text, .rdata, .data, .reloc)
;; - Import table (IAT) for runtime function calls
;; - Export table for DLL symbols
;; - x86-64 Windows ABI: rcx/rdx/r8/r9 arg passing, 32-byte shadow space
;; - Base relocations for ASLR
;; - Resource section for icons/manifests
;; 
;; Reference: Microsoft PE/COFF Specification v11.0
;;            https://docs.microsoft.com/en-us/windows/win32/debug/pe-format
