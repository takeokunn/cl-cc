;;;; packages/backend/binary/src/elf-emit.lisp — ELF64 serialization and public API
;;;
;;; Contains:
;;;   - elf64-build-symtab — build symbol table bytes
;;;   - elf64-build-rela — build RELA relocation section bytes
;;;   - elf64-write-shdr — write a section header entry
;;;   - elf64-finalize — lay out all ELF sections and produce final byte array
;;;   - write-elf64-file — write byte array to file
;;;   - compile-to-elf64 — public entry point: code+relocs → ELF64 bytes
;;;
;;; ELF64 constants, byte-buffer helpers, strtab-builder, elf64-builder struct,
;;; and basic builder API (add-text, add-bss, add-reloc, add-symbol)
;;; are in elf.lisp (loads before).
;;;
;;; Load order: after emit/binary/elf.lisp.
(in-package :cl-cc/binary)

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
