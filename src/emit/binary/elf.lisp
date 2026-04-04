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

;;; ------------------------------------------------------------
;;; ELF64 Serialization
;;; ------------------------------------------------------------

(defun elf64-build-symtab (builder strtab)
  "Build symbol table bytes. Returns (values symtab-bytes local-count).
   Symbol table format: STN_UNDEF first, then locals, then globals.
   local-count is needed in sh_info."
  (let ((sym-buf (elf-make-buffer))
        (symbols (reverse (elf64-symbols builder))))
    ;; Entry 0: STN_UNDEF (all zeros)
    (elf-buf-pad sym-buf +elf64-sym-size+)
    ;; Add each symbol
    (dolist (sym symbols)
      (destructuring-bind (name binding type section-idx value size) sym
        (let ((name-offset (strtab-add strtab name)))
          ;; st_name(4)
          (elf-buf-u32le sym-buf name-offset)
          ;; st_info(1): (binding << 4) | type
          (elf-buf-u8 sym-buf (logior (ash binding 4) type))
          ;; st_other(1): 0
          (elf-buf-u8 sym-buf 0)
          ;; st_shndx(2): section index (0 = undefined)
          (elf-buf-u16le sym-buf section-idx)
          ;; st_value(8)
          (elf-buf-u64le sym-buf value)
          ;; st_size(8)
          (elf-buf-u64le sym-buf size))))
    ;; local count = 1 (only STN_UNDEF entry is "local")
    (values (elf-buf-to-array sym-buf) 1)))

(defun elf64-build-rela (builder sym-index-map)
  "Build .rela.text section bytes.
   SYM-INDEX-MAP maps sym-name string to its 1-based index in symtab."
  (let ((rela-buf (elf-make-buffer))
        (entries (reverse (elf64-rela-entries builder))))
    (dolist (entry entries)
      (destructuring-bind (offset type sym-name addend) entry
        (let ((sym-idx (or (gethash sym-name sym-index-map) 0)))
          ;; r_offset(8): byte offset in .text
          (elf-buf-u64le rela-buf offset)
          ;; r_info(8): (sym-idx << 32) | type
          (elf-buf-u64le rela-buf (logior (ash sym-idx 32) type))
          ;; r_addend(8): signed addend
          (elf-buf-s64le rela-buf addend))))
    (elf-buf-to-array rela-buf)))

(defun elf64-write-shdr (buf name-off type flags offset size link info align entsize)
  "Write a 64-byte section header entry to BUF."
  (elf-buf-u32le buf name-off)   ; sh_name
  (elf-buf-u32le buf type)       ; sh_type
  (elf-buf-u64le buf flags)      ; sh_flags
  (elf-buf-u64le buf 0)          ; sh_addr (0 for .o files)
  (elf-buf-u64le buf offset)     ; sh_offset
  (elf-buf-u64le buf size)       ; sh_size
  (elf-buf-u32le buf link)       ; sh_link
  (elf-buf-u32le buf info)       ; sh_info
  (elf-buf-u64le buf align)      ; sh_addralign
  (elf-buf-u64le buf entsize))   ; sh_entsize

(defun elf64-finalize (builder)
  "Assemble the complete ELF64 object file. Returns a (simple-array (unsigned-byte 8) (*))."
  (let* (;; String tables
         (shstrtab (make-strtab))
         (strtab   (make-strtab))
         ;; Section name offsets in shstrtab
         (sh-text-off     (strtab-add shstrtab ".text"))
         (sh-bss-off      (strtab-add shstrtab ".bss"))
         (sh-rela-off     (strtab-add shstrtab ".rela.text"))
         (sh-symtab-off   (strtab-add shstrtab ".symtab"))
         (sh-strtab-off   (strtab-add shstrtab ".strtab"))
         (sh-shstrtab-off (strtab-add shstrtab ".shstrtab"))
         ;; Build symbol table (populates strtab); capture both bytes and local-count
         (sym-build (multiple-value-list (elf64-build-symtab builder strtab)))
         (sym-buf (first sym-build))
         (sym-local-count (second sym-build))
         ;; Build symbol index map for relocation
         (sym-index-map (let ((m (make-hash-table :test #'equal))
                              (syms (reverse (elf64-symbols builder))))
                          (loop for sym in syms for i from 1
                                do (setf (gethash (first sym) m) i))
                          m))
         ;; Build rela section
         (rela-buf (elf64-build-rela builder sym-index-map))
         ;; Finalize string tables
         (strtab-bytes   (strtab-bytes strtab))
         (shstrtab-bytes (strtab-bytes shstrtab))
         ;; .text bytes
         (text-bytes (elf-buf-to-array (elf64-text-buf builder)))
         (bss-size        (elf64-bss-size builder))
         ;; Layout: ELF header + sections + section headers
         ;; Section ordering: NULL, .text, .bss, .rela.text, .symtab, .strtab, .shstrtab
         ;; Number of sections: 7
         (n-sections 7)
         (shstrtab-idx 6)  ; index of .shstrtab
         ;; Data starts after ELF header, honoring section alignment requirements.
         (text-offset     (align-up +elf64-ehdr-size+ 16))
         (text-size       (length text-bytes))
         (rela-offset     (align-up (+ text-offset text-size) 8))
         (rela-size       (length rela-buf))
         (symtab-offset   (align-up (+ rela-offset rela-size) 8))
         (symtab-size     (length sym-buf))
         (strtab-offset   (+ symtab-offset symtab-size))
         (strtab-size     (length strtab-bytes))
         (shstrtab-offset (+ strtab-offset strtab-size))
         (shstrtab-size   (length shstrtab-bytes))
         ;; Section headers start after all section data
         (shoff           (align-up (+ shstrtab-offset shstrtab-size) 8))
         ;; Output buffer
         (out (elf-make-buffer)))

    ;; ---- ELF Header (64 bytes) ----
    ;; e_ident[16]
    (elf-buf-u8 out +elf-magic-0+)
    (elf-buf-u8 out +elf-magic-1+)
    (elf-buf-u8 out +elf-magic-2+)
    (elf-buf-u8 out +elf-magic-3+)
    (elf-buf-u8 out +elf-class-64+)     ; EI_CLASS
    (elf-buf-u8 out +elf-data-lsb+)     ; EI_DATA
    (elf-buf-u8 out +elf-version-cur+)  ; EI_VERSION
    (elf-buf-u8 out +elf-osabi-none+)   ; EI_OSABI
    (elf-buf-pad out 8)                  ; EI_ABIVERSION + padding
    ;; e_type(2)
    (elf-buf-u16le out +elf-type-rel+)
     ;; e_machine(2)
     (elf-buf-u16le out (elf64-machine builder))
    ;; e_version(4)
    (elf-buf-u32le out +elf-version-cur+)
    ;; e_entry(8): 0 for .o
    (elf-buf-u64le out 0)
    ;; e_phoff(8): 0 (no program headers)
    (elf-buf-u64le out 0)
    ;; e_shoff(8): section header table offset
    (elf-buf-u64le out shoff)
    ;; e_flags(4): 0
    (elf-buf-u32le out 0)
    ;; e_ehsize(2): 64
    (elf-buf-u16le out +elf64-ehdr-size+)
    ;; e_phentsize(2): 0
    (elf-buf-u16le out 0)
    ;; e_phnum(2): 0
    (elf-buf-u16le out 0)
    ;; e_shentsize(2): 64
    (elf-buf-u16le out +elf64-shdr-size+)
    ;; e_shnum(2)
    (elf-buf-u16le out n-sections)
    ;; e_shstrndx(2)
    (elf-buf-u16le out shstrtab-idx)

     ;; ---- Section Data ----
     (elf-buf-pad out (- text-offset (length out)))
     (elf-buf-bytes out text-bytes)
     (elf-buf-pad out (- rela-offset (length out)))
     (elf-buf-bytes out rela-buf)
     (elf-buf-pad out (- symtab-offset (length out)))
     (elf-buf-bytes out sym-buf)
     (elf-buf-pad out (- strtab-offset (length out)))
     (elf-buf-bytes out strtab-bytes)
     (elf-buf-pad out (- shstrtab-offset (length out)))
     (elf-buf-bytes out shstrtab-bytes)
     (elf-buf-pad out (- shoff (length out)))

    ;; ---- Section Headers ----
    ;; SHN 0: NULL
    (elf64-write-shdr out 0 +sht-null+ 0 0 0 0 0 0 0)
    ;; SHN 1: .text
    (elf64-write-shdr out sh-text-off +sht-progbits+
                      (logior +shf-alloc+ +shf-execinstr+)
                      text-offset text-size
                      0 0 16 0)
     ;; SHN 2: .bss (NOBITS; no file payload)
     (elf64-write-shdr out sh-bss-off +sht-nobits+
                       (logior +shf-alloc+ +shf-write+)
                       0 bss-size
                       0 0 8 0)
     ;; SHN 3: .rela.text  (link=symtab-idx=4, info=text-idx=1)
     ;; sh_flags must be 0 for relocation sections in relocatable .o files
     ;; (SHF_ALLOC would incorrectly mark it as occupying memory at runtime)
     (elf64-write-shdr out sh-rela-off +sht-rela+
                       0
                       rela-offset rela-size
                       4 1  ; link=.symtab idx, info=.text idx
                       8 +elf64-rela-size+)
     ;; SHN 4: .symtab  (link=strtab-idx=5, info=first-global-idx)
     (elf64-write-shdr out sh-symtab-off +sht-symtab+
                       0
                       symtab-offset symtab-size
                       5 sym-local-count  ; link=.strtab, info=first-global
                       8 +elf64-sym-size+)
     ;; SHN 5: .strtab
     (elf64-write-shdr out sh-strtab-off +sht-strtab+
                       0
                       strtab-offset strtab-size
                       0 0 1 0)
     ;; SHN 6: .shstrtab
     (elf64-write-shdr out sh-shstrtab-off +sht-strtab+
                       0
                       shstrtab-offset shstrtab-size
                       0 0 1 0)

    (elf-buf-to-array out)))

;;; ------------------------------------------------------------
;;; Public API
;;; ------------------------------------------------------------

(defun write-elf64-file (filename bytes)
  "Write ELF64 object file BYTES to FILENAME."
  (with-open-file (out filename
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
    (write-sequence bytes out))
  filename)

(defun compile-to-elf64 (code-bytes reloc-entries &key (output-file nil) (arch :x86-64) (bss-size 0))
  "Create an ELF64 relocatable object from CODE-BYTES and RELOC-ENTRIES.
   RELOC-ENTRIES is a list of (byte-offset . symbol-name) pairs from the
   x86-64 code generator.
   Returns the byte array; also writes to OUTPUT-FILE if provided."
  (let ((builder (make-elf64-object :machine (ecase arch
                                               (:x86-64 +elf-machine-x86-64+)
                                               (:arm64 +elf-machine-aarch64+)
                                               (:aarch64 +elf-machine-aarch64+)))))
    ;; Add code
    (elf64-add-text-bytes builder code-bytes)
    (when (plusp bss-size)
      (elf64-add-bss builder bss-size))
    ;; Add relocations and collect unique symbols
    (let ((seen-syms (make-hash-table :test #'equal)))
      (dolist (reloc reloc-entries)
        (let* ((offset (car reloc))
               (sym-name (cdr reloc)))
          (unless (gethash sym-name seen-syms)
            (elf64-add-global-symbol builder sym-name)
            (setf (gethash sym-name seen-syms) t))
          (elf64-add-reloc builder offset sym-name))))
    ;; Finalize
    (let ((bytes (elf64-finalize builder)))
      (when output-file
        (write-elf64-file output-file bytes))
      bytes)))
