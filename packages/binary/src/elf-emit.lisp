;;;; packages/binary/src/elf-emit.lisp — ELF64 serialization and public API
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

(defun elf64-write-uleb128 (buf value)
  "Write VALUE as unsigned LEB128 to BUF."
  (loop for v = value then (ash v -7)
        for byte = (logand v #x7f)
        do (elf-buf-u8 buf (if (zerop (ash v -7)) byte (logior byte #x80)))
        until (zerop (ash v -7))))

(defun elf64-write-sleb128 (buf value)
  "Write VALUE as signed LEB128 to BUF."
  (loop with more = t
        with v = value
        while more do
          (let* ((byte (logand v #x7f))
                 (sign-set (not (zerop (logand byte #x40)))))
            (setf v (ash v -7))
            (setf more (not (or (and (zerop v) (not sign-set))
                                (and (= v -1) sign-set))))
            (elf-buf-u8 buf (if more (logior byte #x80) byte)))))

(defun elf64-pad-to-align (buf alignment)
  "Pad BUF with DW_CFA_nop bytes to ALIGNMENT."
  (loop while (not (zerop (mod (length buf) alignment)))
        do (elf-buf-u8 buf 0)))

(defun elf64-build-eh-frame (text-size)
  "Build a minimal x86-64 .eh_frame with one CIE and one FDE for .text.

The CIE uses augmentation \"zR\", code alignment 1, data alignment -8,
return-address register RIP (DWARF register 16), and initial CFA rules for a
normal call frame: CFA = RSP+8, RIP saved at CFA-8.  The FDE covers the current
.text range and is intentionally conservative for frameless/RBP-less code."
  (let ((buf (elf-make-buffer)))
    ;; CIE
    (let* ((cie-start (length buf))
           (cie-body (elf-make-buffer)))
      (binary-buffer-write-u32le cie-body 0) ; CIE_id
      (elf-buf-u8 cie-body 1)                ; version
      (binary-buffer-write-bytes cie-body (map 'vector #'char-code "zR"))
      (elf-buf-u8 cie-body 0)                ; NUL terminator
      (elf64-write-uleb128 cie-body 1)       ; code alignment factor
      (elf64-write-sleb128 cie-body -8)      ; data alignment factor
      (elf64-write-uleb128 cie-body 16)      ; return address register: RIP
      (elf64-write-uleb128 cie-body 1)       ; augmentation data length
      (elf-buf-u8 cie-body #x1b)             ; DW_EH_PE_pcrel | sdata4
      ;; Initial instructions: DW_CFA_def_cfa rsp,8; DW_CFA_offset rip,1
      (elf-buf-u8 cie-body #x0c)
      (elf64-write-uleb128 cie-body 7)
      (elf64-write-uleb128 cie-body 8)
      (elf-buf-u8 cie-body #x90)
      (elf64-write-uleb128 cie-body 1)
      (elf64-pad-to-align cie-body 8)
      (binary-buffer-write-u32le buf (length cie-body))
      (binary-buffer-write-bytes buf (binary-buffer-to-array cie-body))
      ;; FDE. CIE_pointer is the distance from this field back to CIE start.
      (let* ((fde-start (length buf))
             (cie-pointer (- (+ fde-start 4) cie-start))
             (fde-body (elf-make-buffer)))
        (binary-buffer-write-u32le fde-body cie-pointer)
        ;; DW_EH_PE_pcrel|sdata4 payload. Relocatable objects leave the encoded
        ;; location at zero; the linker can resolve final addresses.
        (binary-buffer-write-u32le fde-body 0)
        (binary-buffer-write-u32le fde-body text-size)
        (elf64-write-uleb128 fde-body 0) ; augmentation data length
        ;; Conservative frameless prologue state: CFA remains RSP+8.
        (elf-buf-u8 fde-body #x0c)
        (elf64-write-uleb128 fde-body 7)
        (elf64-write-uleb128 fde-body 8)
        (elf64-pad-to-align fde-body 8)
        (binary-buffer-write-u32le buf (length fde-body))
        (binary-buffer-write-bytes buf (binary-buffer-to-array fde-body))))
    (binary-buffer-to-array buf)))

(defun elf64-build-eh-frame-hdr (eh-frame-offset text-size)
  "Build a compact .eh_frame_hdr with a single binary-search-table FDE row."
  (declare (ignore text-size))
  (let ((buf (elf-make-buffer)))
    (elf-buf-u8 buf 1)     ; version
    (elf-buf-u8 buf #x1b)  ; eh_frame_ptr_enc: DW_EH_PE_pcrel | sdata4
    (elf-buf-u8 buf #x03)  ; fde_count_enc: DW_EH_PE_udata4
    (elf-buf-u8 buf #x3b)  ; table_enc: DW_EH_PE_datarel | sdata4
    (binary-buffer-write-u32le buf eh-frame-offset)
    (binary-buffer-write-u32le buf 1)
    ;; initial_location (relative to .eh_frame_hdr base) and FDE pointer.
    (binary-buffer-write-u32le buf 0)
    (binary-buffer-write-u32le buf 0)
    (binary-buffer-to-array buf)))

(defun elf64-build-symtab (builder strtab)
  "Build symbol table bytes. Returns (values symtab-bytes local-count).
   Symbol table format: STN_UNDEF first, then locals, then globals.
   local-count is needed in sh_info."
  (let ((sym-buf (elf-make-buffer))
        (symbols (reverse (elf64-symbols builder))))
    ;; Entry 0: STN_UNDEF (all zeros)
    (binary-buffer-write-pad sym-buf +elf64-sym-size+)
    ;; Add each symbol
    (dolist (sym symbols)
      (destructuring-bind (name binding type section-idx value size) sym
        (let ((name-offset (strtab-add strtab name)))
          ;; st_name(4)
          (binary-buffer-write-u32le sym-buf name-offset)
          ;; st_info(1): (binding << 4) | type
          (elf-buf-u8 sym-buf (logior (ash binding 4) type))
          ;; st_other(1): 0
          (elf-buf-u8 sym-buf 0)
          ;; st_shndx(2): section index (0 = undefined)
          (binary-buffer-write-u16le sym-buf section-idx)
          ;; st_value(8)
          (binary-buffer-write-u64le sym-buf value)
          ;; st_size(8)
          (binary-buffer-write-u64le sym-buf size))))
    ;; local count = 1 (only STN_UNDEF entry is "local")
    (values (binary-buffer-to-array sym-buf) 1)))

(defun elf64-build-rela (builder sym-index-map)
  "Build .rela.text section bytes.
   SYM-INDEX-MAP maps sym-name string to its 1-based index in symtab."
  (let ((rela-buf (elf-make-buffer))
        (entries (reverse (elf64-rela-entries builder))))
    (dolist (entry entries)
      (destructuring-bind (offset type sym-name addend) entry
        (let ((sym-idx (or (gethash sym-name sym-index-map) 0)))
          ;; r_offset(8): byte offset in .text
          (binary-buffer-write-u64le rela-buf offset)
          ;; r_info(8): (sym-idx << 32) | type
          (binary-buffer-write-u64le rela-buf (logior (ash sym-idx 32) type))
          ;; r_addend(8): signed addend
          (binary-buffer-write-s64le rela-buf addend))))
    (binary-buffer-to-array rela-buf)))

(defun elf64-write-shdr (buf name-off type flags offset size link info align entsize)
  "Write a 64-byte section header entry to BUF."
  (binary-buffer-write-u32le buf name-off)   ; sh_name
  (binary-buffer-write-u32le buf type)       ; sh_type
  (binary-buffer-write-u64le buf flags)      ; sh_flags
  (binary-buffer-write-u64le buf 0)          ; sh_addr (0 for .o files)
  (binary-buffer-write-u64le buf offset)     ; sh_offset
  (binary-buffer-write-u64le buf size)       ; sh_size
  (binary-buffer-write-u32le buf link)       ; sh_link
  (binary-buffer-write-u32le buf info)       ; sh_info
  (binary-buffer-write-u64le buf align)      ; sh_addralign
  (binary-buffer-write-u64le buf entsize))   ; sh_entsize

(defun elf64-finalize (builder)
  "Assemble the complete ELF64 object file. Returns a (simple-array (unsigned-byte 8) (*))."
  (let* (;; String tables
         (shstrtab (make-strtab))
         (strtab   (make-strtab))
         ;; Section name offsets in shstrtab
         (sh-text-off     (strtab-add shstrtab ".text"))
         (sh-rodata-off   (strtab-add shstrtab ".rodata"))
         (sh-bss-off      (strtab-add shstrtab ".bss"))
         (sh-eh-frame-off (strtab-add shstrtab ".eh_frame"))
         (sh-eh-frame-hdr-off (strtab-add shstrtab ".eh_frame_hdr"))
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
         ;; .text/.rodata/.eh_frame bytes
          (text-bytes (binary-buffer-to-array (elf64-text-buf builder)))
          (rodata-bytes (binary-buffer-to-array (elf64-rodata-buf builder)))
          (bss-size        (elf64-bss-size builder))
          (eh-frame-bytes (elf64-build-eh-frame (length text-bytes)))
          ;; Layout: ELF header + sections + section headers.
          ;; Section ordering: NULL, .text, .rodata, .bss, .eh_frame,
          ;; .eh_frame_hdr, .rela.text, .symtab, .strtab, .shstrtab
          (n-sections 10)
          (symtab-idx 7)
          (strtab-idx 8)
          (shstrtab-idx 9)  ; index of .shstrtab
         ;; Data starts after ELF header, honoring section alignment requirements.
          (text-offset     (align-up +elf64-ehdr-size+ 16))
          (text-size       (length text-bytes))
          (rodata-offset   (align-up (+ text-offset text-size) 8))
          (rodata-size     (length rodata-bytes))
          (eh-frame-offset (align-up (+ rodata-offset rodata-size) 8))
          (eh-frame-size   (length eh-frame-bytes))
          (eh-frame-hdr-bytes (elf64-build-eh-frame-hdr eh-frame-offset text-size))
          (eh-frame-hdr-offset (align-up (+ eh-frame-offset eh-frame-size) 4))
          (eh-frame-hdr-size (length eh-frame-hdr-bytes))
          (rela-offset     (align-up (+ eh-frame-hdr-offset eh-frame-hdr-size) 8))
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
    (binary-buffer-write-pad out 8)                  ; EI_ABIVERSION + padding
    ;; e_type(2)
    (binary-buffer-write-u16le out +elf-type-rel+)
     ;; e_machine(2)
     (binary-buffer-write-u16le out (elf64-machine builder))
    ;; e_version(4)
    (binary-buffer-write-u32le out +elf-version-cur+)
    ;; e_entry(8): 0 for .o
    (binary-buffer-write-u64le out 0)
    ;; e_phoff(8): 0 (no program headers)
    (binary-buffer-write-u64le out 0)
    ;; e_shoff(8): section header table offset
    (binary-buffer-write-u64le out shoff)
    ;; e_flags(4): 0
    (binary-buffer-write-u32le out 0)
    ;; e_ehsize(2): 64
    (binary-buffer-write-u16le out +elf64-ehdr-size+)
    ;; e_phentsize(2): 0
    (binary-buffer-write-u16le out 0)
    ;; e_phnum(2): 0
    (binary-buffer-write-u16le out 0)
    ;; e_shentsize(2): 64
    (binary-buffer-write-u16le out +elf64-shdr-size+)
    ;; e_shnum(2)
    (binary-buffer-write-u16le out n-sections)
    ;; e_shstrndx(2)
    (binary-buffer-write-u16le out shstrtab-idx)

     ;; ---- Section Data ----
     (binary-buffer-write-pad out (- text-offset (length out)))
     (binary-buffer-write-bytes out text-bytes)
      (binary-buffer-write-pad out (- rodata-offset (length out)))
      (binary-buffer-write-bytes out rodata-bytes)
      (binary-buffer-write-pad out (- eh-frame-offset (length out)))
      (binary-buffer-write-bytes out eh-frame-bytes)
      (binary-buffer-write-pad out (- eh-frame-hdr-offset (length out)))
      (binary-buffer-write-bytes out eh-frame-hdr-bytes)
      (binary-buffer-write-pad out (- rela-offset (length out)))
     (binary-buffer-write-bytes out rela-buf)
     (binary-buffer-write-pad out (- symtab-offset (length out)))
     (binary-buffer-write-bytes out sym-buf)
     (binary-buffer-write-pad out (- strtab-offset (length out)))
     (binary-buffer-write-bytes out strtab-bytes)
     (binary-buffer-write-pad out (- shstrtab-offset (length out)))
     (binary-buffer-write-bytes out shstrtab-bytes)
     (binary-buffer-write-pad out (- shoff (length out)))

    ;; ---- Section Headers ----
    ;; SHN 0: NULL
    (elf64-write-shdr out 0 +sht-null+ 0 0 0 0 0 0 0)
    ;; SHN 1: .text
    (elf64-write-shdr out sh-text-off +sht-progbits+
                      (logior +shf-alloc+ +shf-execinstr+)
                      text-offset text-size
                      0 0 16 0)
      ;; SHN 2: .rodata (allocated, read-only; no SHF_WRITE)
      (elf64-write-shdr out sh-rodata-off +sht-progbits+
                        +shf-alloc+
                        rodata-offset rodata-size
                        0 0 8 0)
      ;; SHN 3: .bss (NOBITS; no file payload)
      (elf64-write-shdr out sh-bss-off +sht-nobits+
                        (logior +shf-alloc+ +shf-write+)
                        0 bss-size
                        0 0 8 0)
      ;; SHN 4: .eh_frame (allocated unwind table)
      (elf64-write-shdr out sh-eh-frame-off +sht-progbits+
                        +shf-alloc+
                        eh-frame-offset eh-frame-size
                        0 0 8 0)
      ;; SHN 5: .eh_frame_hdr (allocated compact FDE lookup table)
      (elf64-write-shdr out sh-eh-frame-hdr-off +sht-progbits+
                        +shf-alloc+
                        eh-frame-hdr-offset eh-frame-hdr-size
                        0 0 4 0)
      ;; SHN 6: .rela.text  (link=symtab, info=text-idx=1)
      ;; sh_flags must be 0 for relocation sections in relocatable .o files
      ;; (SHF_ALLOC would incorrectly mark it as occupying memory at runtime)
      (elf64-write-shdr out sh-rela-off +sht-rela+
                        0
                        rela-offset rela-size
                        symtab-idx 1  ; link=.symtab idx, info=.text idx
                        8 +elf64-rela-size+)
      ;; SHN 7: .symtab  (link=.strtab, info=first-global-idx)
      (elf64-write-shdr out sh-symtab-off +sht-symtab+
                        0
                        symtab-offset symtab-size
                        strtab-idx sym-local-count  ; link=.strtab, info=first-global
                        8 +elf64-sym-size+)
      ;; SHN 8: .strtab
      (elf64-write-shdr out sh-strtab-off +sht-strtab+
                        0
                        strtab-offset strtab-size
                        0 0 1 0)
      ;; SHN 9: .shstrtab
      (elf64-write-shdr out sh-shstrtab-off +sht-strtab+
                       0
                       shstrtab-offset shstrtab-size
                       0 0 1 0)

    (binary-buffer-to-array out)))

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
