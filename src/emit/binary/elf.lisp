;;;; src/binary/elf.lisp - ELF64 Relocatable Object File Builder
;;;
;;; Builds ELF64 .o files for x86-64 Linux (ET_REL).
;;; Sections: NULL, .text, .rela.text, .symtab, .strtab, .shstrtab
;;;
;;; ELF64 reference: System V AMD64 ABI, ELF-64 Object File Format v1.5

(in-package :cl-cc/binary)

;;; ------------------------------------------------------------
;;; ELF64 Constants
;;; ------------------------------------------------------------

(defconstant +elf-magic-0+ #x7f)
(defconstant +elf-magic-1+ #x45)  ; E
(defconstant +elf-magic-2+ #x4c)  ; L
(defconstant +elf-magic-3+ #x46)  ; F

(defconstant +elf-class-64+    2)   ; ELFCLASS64
(defconstant +elf-data-lsb+    1)   ; ELFDATA2LSB (little-endian)
(defconstant +elf-version-cur+ 1)   ; EV_CURRENT
(defconstant +elf-osabi-none+  0)   ; ELFOSABI_NONE
(defconstant +elf-type-rel+    1)   ; ET_REL (relocatable)
(defconstant +elf-machine-x86-64+ #x3e)  ; EM_X86_64
(defconstant +elf-machine-aarch64+ #xB7) ; EM_AARCH64

;;; Section header types
(defconstant +sht-null+     0)
(defconstant +sht-progbits+ 1)
(defconstant +sht-symtab+   2)
(defconstant +sht-strtab+   3)
(defconstant +sht-rela+     4)
(defconstant +sht-nobits+   8)

;;; Section header flags
(defconstant +shf-write+      1)
(defconstant +shf-alloc+      2)
(defconstant +shf-execinstr+  4)

;;; Symbol binding
(defconstant +stb-local+  0)
(defconstant +stb-global+ 1)
(defconstant +stb-weak+   2)

;;; Symbol type
(defconstant +stt-notype+  0)
(defconstant +stt-object+  1)
(defconstant +stt-func+    2)
(defconstant +stt-section+ 3)
(defconstant +stt-file+    4)

;;; Relocation types
(defconstant +r-x86-64-none+   0)
(defconstant +r-x86-64-64+     1)
(defconstant +r-x86-64-pc32+   2)
(defconstant +r-x86-64-plt32+  4)
(defconstant +r-x86-64-32+    10)
(defconstant +r-x86-64-32s+   11)

;;; ELF64 structure sizes (bytes)
(defconstant +elf64-ehdr-size+ 64)   ; ELF header
(defconstant +elf64-shdr-size+ 64)   ; Section header entry
(defconstant +elf64-sym-size+  24)   ; Symbol table entry (Elf64_Sym)
(defconstant +elf64-rela-size+ 24)   ; Relocation entry with addend (Elf64_Rela)

;;; ------------------------------------------------------------
;;; Byte Buffer Helpers
;;; ------------------------------------------------------------

(defun elf-make-buffer ()
  "Create a fresh byte buffer."
  (make-binary-buffer 0))

(defun elf-buf-u8 (buf val)
  "Write 1 byte to buffer."
  (binary-buffer-write-u8 buf (logand val #xff)))

(defun elf-buf-u16le (buf val)
  "Write 16-bit little-endian to buffer."
  (binary-buffer-write-u16le buf val))

(defun elf-buf-u32le (buf val)
  "Write 32-bit little-endian to buffer."
  (binary-buffer-write-u32le buf val))

(defun elf-buf-u64le (buf val)
  "Write 64-bit little-endian to buffer."
  (binary-buffer-write-u64le buf val))

(defun elf-buf-s64le (buf val)
  "Write signed 64-bit little-endian to buffer."
  (binary-buffer-write-s64le buf val))

(defun elf-buf-bytes (buf bytes)
  "Append sequence of bytes to buffer."
  (binary-buffer-write-bytes buf bytes))

(defun elf-buf-pad (buf n)
  "Append N zero bytes."
  (binary-buffer-write-pad buf n))

(defun elf-buf-to-array (buf)
  "Convert adjustable buffer to simple byte array."
  (binary-buffer-to-array buf))

;;; ------------------------------------------------------------
;;; String Table Builder
;;; ------------------------------------------------------------

(defstruct (strtab-builder (:conc-name stb-))
  "Builds an ELF string table section."
  (buf (elf-make-buffer))
  (offset 0)
  (map (make-hash-table :test #'equal)))

(defun make-strtab ()
  "Create a new string table, pre-inserting the empty string at offset 0."
  (let ((st (make-strtab-builder)))
    ;; ELF strtab always starts with a NUL byte (empty string at offset 0)
    (elf-buf-u8 (stb-buf st) 0)
    (setf (stb-offset st) 1)
    (setf (gethash "" (stb-map st)) 0)
    st))

(defun strtab-add (st name)
  "Add NAME to string table, return its byte offset. Deduplicates."
  (or (gethash name (stb-map st))
      (let ((offset (stb-offset st)))
        (setf (gethash name (stb-map st)) offset)
        (loop for c across name do (elf-buf-u8 (stb-buf st) (char-code c)))
        (elf-buf-u8 (stb-buf st) 0)   ; NUL terminator
        (setf (stb-offset st) (+ offset (length name) 1))
        offset)))

(defun strtab-bytes (st)
  "Return the strtab as a byte array."
  (elf-buf-to-array (stb-buf st)))

;;; ------------------------------------------------------------
;;; ELF64 Builder
;;; ------------------------------------------------------------

(defstruct (elf64-builder (:conc-name elf64-))
  "Accumulates sections for an ELF64 relocatable object file."
  (machine +elf-machine-x86-64+ :type (unsigned-byte 16))
  ;; .text section data
  (text-buf    (elf-make-buffer))
  ;; .bss section size in bytes (NOBITS, occupies memory only)
  (bss-size 0 :type integer)
  ;; Relocation entries: list of (offset type sym-name addend)
  ;; offset = byte offset in .text; sym-name = string; addend = integer
  (rela-entries nil)
  ;; Symbol entries: list of (name binding type section-idx value size)
  ;; section-idx: 0=undef, 1=.text
  (symbols nil)
  ;; File symbol (index 0 in symtab is always STN_UNDEF)
  (symbol-count 0))

(defun make-elf64-object (&key (machine +elf-machine-x86-64+))
  "Create a fresh ELF64 builder."
  (make-elf64-builder :machine machine))

(defun elf64-add-text-bytes (builder bytes)
  "Append BYTES (vector or list of (unsigned-byte 8)) to .text section."
  (elf-buf-bytes (elf64-text-buf builder) bytes))

(defun elf64-text-size (builder)
  "Return current .text section size in bytes."
  (length (elf64-text-buf builder)))

(defun elf64-add-bss (builder size)
  "Reserve SIZE bytes in the .bss section."
  (incf (elf64-bss-size builder) size)
  (elf64-bss-size builder))

(defun elf64-add-reloc (builder offset sym-name &key (type +r-x86-64-plt32+) (addend -4))
  "Add a relocation entry for a CALL instruction at OFFSET in .text.
   SYM-NAME is the external symbol to reference.
   TYPE defaults to R_X86_64_PLT32 (appropriate for CALL rel32).
   ADDEND defaults to -4 (PC-relative from end of 4-byte immediate)."
  (push (list offset type sym-name addend) (elf64-rela-entries builder)))

(defun elf64-add-global-symbol (builder name &key (section-idx 0) (value 0) (size 0))
  "Add a global (external) symbol reference to the symbol table.
   section-idx=0 means undefined (external), 1 means defined in .text."
  (let ((idx (elf64-symbol-count builder)))
    (push (list name +stb-global+ +stt-func+ section-idx value size) (elf64-symbols builder))
    (setf (elf64-symbol-count builder) (1+ idx))
    idx))


;;; (elf64-build-symtab, elf64-build-rela, elf64-write-shdr, elf64-finalize,
;;;  write-elf64-file, and compile-to-elf64 are in elf-emit.lisp
;;;  which loads after this file.)

