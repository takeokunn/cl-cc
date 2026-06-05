;;;; packages/binary/src/macho.lisp - Mach-O Binary Format Support
;;;
;;; Implements Mach-O format for macOS executable generation.
;;; Supports x86-64 and AArch64 architectures.
;;;
;;; Pure Common Lisp implementation - no external dependencies.
;;;
;;; References:
;;; - https://github.com/aidansteele/osx-abi-macho-file-format-reference
;;; - /usr/include/mach-o/loader.h
;;; - /usr/include/mach-o/nlist.h

(in-package :cl-cc/binary)

;;; Constants

;; Magic numbers
(defconstant +mh-magic-64+ #xFEEDFACF
  "64-bit Mach-O magic number (little-endian).")

;; CPU Types
(defconstant +cpu-type-x86-64+ #x01000007
  "x86-64 (AMD64) CPU type.")
(defconstant +cpu-type-arm64+ #x0100000C
  "ARM64 CPU type.")
(defconstant +cpu-subtype-x86-64-all+ #x00000003
  "x86-64 all subtypes.")
(defconstant +cpu-subtype-arm64-all+ #x00000000
  "ARM64 all subtypes.")

;; File Types
(defconstant +mh-execute+ 2
  "Executable file type.")

;; Load Command Types
(defconstant +lc-segment-64+ #x19
  "64-bit segment load command.")
(defconstant +lc-symtab+ #x02
  "Symbol table load command (LC_SYMTAB). #x03 is the obsolete LC_SYMSEG.")
(defconstant +lc-dysymtab+ #x0B
  "Dynamic symbol table load command.")
(defconstant +lc-load-dylib+ #x0C
  "Load dynamic library command.")
(defconstant +lc-load-dylinker+ #x0E
  "Dynamic linker load command.")
(defconstant +lc-code-signature+ #x1D
  "Code signature link-edit data command.")
(defconstant +lc-dyld-info-only+ #x80000022
  "Compressed dyld rebase/bind/export information command (LC_DYLD_INFO_ONLY).")
(defconstant +lc-main+ #x80000028
  "Main entry point load command (LC_MAIN | LC_REQ_DYLD).")

;; Symbol flags
(defconstant +n-undef+ #x00
  "Undefined nlist symbol type.")
(defconstant +n-ext+ #x01
  "External nlist symbol flag.")

;; Mach-O x86-64 relocation types
(defconstant +x86-64-reloc-unsigned+ 0
  "X86_64_RELOC_UNSIGNED relocation type.")
(defconstant +x86-64-reloc-signed+ 1
  "X86_64_RELOC_SIGNED relocation type.")
(defconstant +x86-64-reloc-branch+ 2
  "X86_64_RELOC_BRANCH relocation type.")
(defconstant +x86-64-reloc-got-load+ 3
  "X86_64_RELOC_GOT_LOAD relocation type.")

;; Mach-O ARM64 relocation types
(defconstant +arm64-reloc-unsigned+ 0
  "ARM64_RELOC_UNSIGNED relocation type.")
(defconstant +arm64-reloc-branch26+ 2
  "ARM64_RELOC_BRANCH26 relocation type.")
(defconstant +arm64-reloc-page21+ 3
  "ARM64_RELOC_PAGE21 relocation type.")
(defconstant +arm64-reloc-pageoff12+ 4
  "ARM64_RELOC_PAGEOFF12 relocation type.")

;; Header Flags
(defconstant +mh-noundefs+ 1
  "No undefined references.")
(defconstant +mh-dyldlink+ 4
  "Can be dynamically linked.")
(defconstant +mh-pie+ #x200000
  "Position-independent executable.")

;; Section Flags
(defconstant +s-attr-pure-instructions+ #x80000000
  "Section contains only instructions.")
(defconstant +s-attr-some-instructions+ #x400
  "Section contains some instructions.")

;; Compact unwind encodings (x86-64 subset).
;; FR-247: Unwind Tables — Mach-O __unwind_info (compact unwind) for macOS native debugging and crash reporting
(defconstant +compact-unwind-encoding-none+ 0
  "No compact unwind encoding.")
(defconstant +compact-unwind-x86-64-mode-stack-immd+ #x02000000
  "Frameless/RBP-less x86-64 compact unwind mode with immediate stack size.")

;;; Structural size constants -- derived from the Mach-O ABI layout.
;;; Using named constants prevents silent breakage if struct layout ever changes.
(defconstant +macho-segment-command-size+ 72
  "Byte size of a 64-bit LC_SEGMENT_64 command header (no sections).")
(defconstant +macho-section-size+ 80
  "Byte size of one 64-bit section record inside a segment command.")
(defconstant +macho-nlist-size+ 18
  "Byte size of one 64-bit nlist symbol table entry.")

;;; Virtual memory base addresses used by the CL-CC Mach-O emitter.
(defconstant +macho-text-base-addr+ #x100000000
  "Default __TEXT segment virtual base address.")
(defconstant +macho-data-base-addr+ #x100001000
  "Default __DATA segment virtual base address.")
(defconstant +macho-data-const-base-addr+ #x100002000
  "Default __DATA_CONST segment virtual base address.")

;;; Structures

(defstruct mach-header
  "64-bit Mach-O header structure."
  (magic +mh-magic-64+ :type (unsigned-byte 32))
  (cputype +cpu-type-x86-64+ :type (unsigned-byte 32))
  (cpusubtype +cpu-subtype-x86-64-all+ :type (unsigned-byte 32))
  (filetype +mh-execute+ :type (unsigned-byte 32))
  (ncmds 0 :type (unsigned-byte 32))
  (sizeofcmds 0 :type (unsigned-byte 32))
  (flags +mh-noundefs+ :type (unsigned-byte 32))
  (reserved 0 :type (unsigned-byte 32)))

(defstruct segment-command
  "64-bit segment load command."
  (cmd +lc-segment-64+ :type (unsigned-byte 32))
  (cmdsize 72 :type (unsigned-byte 32))
  (segname "" :type string)
  (vmaddr 0 :type (unsigned-byte 64))
  (vmsize 0 :type (unsigned-byte 64))
  (fileoff 0 :type (unsigned-byte 64))
  (filesize 0 :type (unsigned-byte 64))
  (maxprot 7 :type (unsigned-byte 32))   ; rwx
  (initprot 5 :type (unsigned-byte 32))  ; rx
  (nsects 0 :type (unsigned-byte 32))
  (flags 0 :type (unsigned-byte 32))
  (payload (make-array 0 :element-type '(unsigned-byte 8))
           :type (simple-array (unsigned-byte 8) (*)))
  (sections nil :type list))

(defstruct section
  "64-bit section structure."
  (sectname "" :type string)
  (segname "" :type string)
  (addr 0 :type (unsigned-byte 64))
  (size 0 :type (unsigned-byte 64))
  (offset 0 :type (unsigned-byte 32))
  (align 0 :type (unsigned-byte 32))
  (reloff 0 :type (unsigned-byte 32))
  (nreloc 0 :type (unsigned-byte 32))
  (flags 0 :type (unsigned-byte 32))
  (reserved1 0 :type (unsigned-byte 32))
  (reserved2 0 :type (unsigned-byte 32))
  (reserved3 0 :type (unsigned-byte 32)))

(defstruct relocation-info
  "Mach-O relocation_info entry.

R-ADDRESS is section-relative.  R-SYMBOLNUM is either a symbol-table index for
external relocations or a section ordinal for local relocations.  R-LENGTH uses
Mach-O's log2 width encoding (2 means 4 bytes, 3 means 8 bytes)."
  (r-address 0 :type (unsigned-byte 32))
  (r-symbolnum 0 :type (unsigned-byte 32))
  (r-pcrel 0 :type (unsigned-byte 8))
  (r-length 2 :type (unsigned-byte 8))
  (r-extern 1 :type (unsigned-byte 8))
  (r-type 0 :type (unsigned-byte 8)))

(defstruct symtab-command
  "Symbol table load command."
  (cmd +lc-symtab+ :type (unsigned-byte 32))
  (cmdsize 24 :type (unsigned-byte 32))
  (symoff 0 :type (unsigned-byte 32))
  (nsyms 0 :type (unsigned-byte 32))
  (stroff 0 :type (unsigned-byte 32))
  (strsize 0 :type (unsigned-byte 32)))

(defstruct dysymtab-command
  "Dynamic symbol table load command. Minimal zero-filled serialization."
  (cmd +lc-dysymtab+ :type (unsigned-byte 32))
  (cmdsize 80 :type (unsigned-byte 32))
  (ilocalsym 0 :type (unsigned-byte 32))
  (nlocalsym 0 :type (unsigned-byte 32))
  (iextdefsym 0 :type (unsigned-byte 32))
  (nextdefsym 0 :type (unsigned-byte 32))
  (iundefsym 0 :type (unsigned-byte 32))
  (nundefsym 0 :type (unsigned-byte 32))
  (tocoff 0 :type (unsigned-byte 32))
  (ntoc 0 :type (unsigned-byte 32))
  (modtaboff 0 :type (unsigned-byte 32))
  (nmodtab 0 :type (unsigned-byte 32))
  (extrefsymoff 0 :type (unsigned-byte 32))
  (nextrefsyms 0 :type (unsigned-byte 32))
  (indirectsymoff 0 :type (unsigned-byte 32))
  (nindirectsyms 0 :type (unsigned-byte 32))
  (extreloff 0 :type (unsigned-byte 32))
  (nextrel 0 :type (unsigned-byte 32))
  (locreloff 0 :type (unsigned-byte 32))
  (nlocrel 0 :type (unsigned-byte 32)))

(defstruct dyld-info-command
  "LC_DYLD_INFO_ONLY command containing link-edit rebase/bind/export ranges."
  (cmd +lc-dyld-info-only+ :type (unsigned-byte 32))
  (cmdsize 48 :type (unsigned-byte 32))
  (rebase-off 0 :type (unsigned-byte 32))
  (rebase-size 0 :type (unsigned-byte 32))
  (bind-off 0 :type (unsigned-byte 32))
  (bind-size 0 :type (unsigned-byte 32))
  (weak-bind-off 0 :type (unsigned-byte 32))
  (weak-bind-size 0 :type (unsigned-byte 32))
  (lazy-bind-off 0 :type (unsigned-byte 32))
  (lazy-bind-size 0 :type (unsigned-byte 32))
  (export-off 0 :type (unsigned-byte 32))
  (export-size 0 :type (unsigned-byte 32)))

(defstruct dylib-command
  "LC_LOAD_DYLIB command for a dependent dynamic library."
  (cmd +lc-load-dylib+ :type (unsigned-byte 32))
  (cmdsize 56 :type (unsigned-byte 32))
  (name-offset 24 :type (unsigned-byte 32))
  (timestamp 2 :type (unsigned-byte 32))
  (current-version #x00010000 :type (unsigned-byte 32))
  (compatibility-version #x00010000 :type (unsigned-byte 32))
  (name "/usr/lib/libSystem.B.dylib" :type string))

(defstruct linkedit-data-command
  "LC_CODE_SIGNATURE and other link-edit data command payload ranges."
  (cmd +lc-code-signature+ :type (unsigned-byte 32))
  (cmdsize 16 :type (unsigned-byte 32))
  (dataoff 0 :type (unsigned-byte 32))
  (datasize 0 :type (unsigned-byte 32)))

(defstruct entry-point-command
  "LC_MAIN entry point command."
  (cmd +lc-main+ :type (unsigned-byte 32))
  (cmdsize 24 :type (unsigned-byte 32))
  (entryoff 0 :type (unsigned-byte 64))
  (stacksize 0 :type (unsigned-byte 64)))

(defstruct nlist
  "64-bit symbol table entry."
  (n-strx 0 :type (unsigned-byte 32))
  (n-type 0 :type (unsigned-byte 8))
  (n-sect 0 :type (unsigned-byte 8))
  (n-desc 0 :type (unsigned-byte 16))
  (n-value 0 :type (unsigned-byte 64)))

;;; Byte buffer, utilities, and serialization primitives are in
;;; macho-buffer.lisp (loaded after this file).
