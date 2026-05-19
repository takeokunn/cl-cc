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
  (elf64-write-shdr-with-addr buf name-off type flags 0 offset size link info align entsize))

(defun elf64-write-shdr-with-addr (buf name-off type flags addr offset size link info align entsize)
  "Write a 64-byte section header entry to BUF with explicit virtual ADDR."
  (binary-buffer-write-u32le buf name-off)   ; sh_name
  (binary-buffer-write-u32le buf type)       ; sh_type
  (binary-buffer-write-u64le buf flags)      ; sh_flags
  (binary-buffer-write-u64le buf addr)       ; sh_addr
  (binary-buffer-write-u64le buf offset)     ; sh_offset
  (binary-buffer-write-u64le buf size)       ; sh_size
  (binary-buffer-write-u32le buf link)       ; sh_link
  (binary-buffer-write-u32le buf info)       ; sh_info
  (binary-buffer-write-u64le buf align)      ; sh_addralign
  (binary-buffer-write-u64le buf entsize))   ; sh_entsize

(defun elf64-write-phdr (buf type flags offset vaddr paddr filesz memsz align)
  "Write an ELF64 program header entry to BUF."
  (binary-buffer-write-u32le buf type)
  (binary-buffer-write-u32le buf flags)
  (binary-buffer-write-u64le buf offset)
  (binary-buffer-write-u64le buf vaddr)
  (binary-buffer-write-u64le buf paddr)
  (binary-buffer-write-u64le buf filesz)
  (binary-buffer-write-u64le buf memsz)
  (binary-buffer-write-u64le buf align))

(defun elf64-write-dynamic-entry (buf tag value)
  "Write one Elf64_Dyn entry to BUF."
  (binary-buffer-write-s64le buf tag)
  (binary-buffer-write-u64le buf value))

(defun elf64-build-compressed-section (compressed-bytes original-size &key (addralign 16))
  "Build an ELF64 SHF_COMPRESSED section payload.

The returned bytes start with Elf64_Chdr:
  ch_type=ELFCOMPRESS_ZLIB, ch_reserved=0, ch_size=ORIGINAL-SIZE,
  ch_addralign=ADDRALIGN; followed by zlib-compressed bytes."
  (let ((buf (elf-make-buffer)))
    (binary-buffer-write-u32le buf +elfcompress-zlib+)
    (binary-buffer-write-u32le buf 0)
    (binary-buffer-write-u64le buf original-size)
    (binary-buffer-write-u64le buf addralign)
    (binary-buffer-write-bytes buf compressed-bytes)
    (binary-buffer-to-array buf)))

(defun elf64-finalize-relocatable (builder)
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
           (compressed-text-values
             (multiple-value-list
              (maybe-compress-code-bytes text-bytes :compress (elf64-compress-text builder))))
           (compressed-text-bytes (first compressed-text-values))
           (text-compression-algorithm (second compressed-text-values))
           (text-original-size (third compressed-text-values))
           (text-section-compressed-p (/= text-compression-algorithm +cl-cc-compression-none+))
           (text-section-bytes (if text-section-compressed-p
                                   (elf64-build-compressed-section compressed-text-bytes
                                                                   text-original-size
                                                                   :addralign 16)
                                   text-bytes))
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
           (text-size       (length text-section-bytes))
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
      (binary-buffer-write-bytes out text-section-bytes)
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
                       (logior +shf-alloc+ +shf-execinstr+
                               (if text-section-compressed-p +shf-compressed+ 0))
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

(defun elf64-string-bytes (string &key (nul t))
  "Return STRING as ASCII bytes, optionally NUL terminated."
  (let* ((extra (if nul 1 0))
         (bytes (make-array (+ (length string) extra)
                            :element-type '(unsigned-byte 8))))
    (loop for char across string
          for i from 0
          do (setf (aref bytes i) (char-code char)))
    (when nul
      (setf (aref bytes (length string)) 0))
    bytes))

(defun elf64-build-dynsym ()
  "Build a minimal dynamic symbol table containing only STN_UNDEF."
  (let ((buf (elf-make-buffer)))
    (binary-buffer-write-pad buf +elf64-sym-size+)
    (binary-buffer-to-array buf)))

(defun elf64-build-dynamic-section (needed-offsets dynstr-addr dynstr-size dynsym-addr
                                    rela-addr rela-size jmprel-addr jmprel-size)
  "Build .dynamic entries for ET_DYN output."
  (let ((buf (elf-make-buffer)))
    (dolist (offset needed-offsets)
      (elf64-write-dynamic-entry buf +dt-needed+ offset))
    (elf64-write-dynamic-entry buf +dt-symtab+ dynsym-addr)
    (elf64-write-dynamic-entry buf +dt-strtab+ dynstr-addr)
    (elf64-write-dynamic-entry buf +dt-strsz+ dynstr-size)
    (elf64-write-dynamic-entry buf +dt-syment+ +elf64-sym-size+)
    (elf64-write-dynamic-entry buf +dt-rela+ rela-addr)
    (elf64-write-dynamic-entry buf +dt-relasz+ rela-size)
    (elf64-write-dynamic-entry buf +dt-relaent+ +elf64-rela-size+)
    (elf64-write-dynamic-entry buf +dt-jmprel+ jmprel-addr)
    (elf64-write-dynamic-entry buf +dt-pltrelsz+ jmprel-size)
    (elf64-write-dynamic-entry buf +dt-pltrel+ +dt-rela+)
    (elf64-write-dynamic-entry buf +dt-null+ 0)
    (binary-buffer-to-array buf)))

(defun elf64-finalize-executable (builder)
  "Assemble an ELF64 ET_EXEC or ET_DYN image with program headers."
  (let* ((dynamic-p (= (elf64-elf-type builder) +elf-type-dyn+))
         (base (if dynamic-p 0 +elf64-exec-base+))
         (shstrtab (make-strtab))
         (strtab (make-strtab))
         (dynstr (make-strtab))
         (sh-text-off (strtab-add shstrtab ".text"))
         (sh-rodata-off (strtab-add shstrtab ".rodata"))
         (sh-data-off (strtab-add shstrtab ".data"))
         (sh-bss-off (strtab-add shstrtab ".bss"))
         (sh-eh-frame-off (strtab-add shstrtab ".eh_frame"))
         (sh-eh-frame-hdr-off (strtab-add shstrtab ".eh_frame_hdr"))
         (sh-symtab-off (strtab-add shstrtab ".symtab"))
         (sh-strtab-off (strtab-add shstrtab ".strtab"))
         (sh-shstrtab-off (strtab-add shstrtab ".shstrtab"))
         (sh-interp-off (and dynamic-p (strtab-add shstrtab ".interp")))
         (sh-dynamic-off (and dynamic-p (strtab-add shstrtab ".dynamic")))
         (sh-dynsym-off (and dynamic-p (strtab-add shstrtab ".dynsym")))
         (sh-dynstr-off (and dynamic-p (strtab-add shstrtab ".dynstr")))
         (sh-rela-dyn-off (and dynamic-p (strtab-add shstrtab ".rela.dyn")))
         (sh-rela-plt-off (and dynamic-p (strtab-add shstrtab ".rela.plt")))
         (phnum (if dynamic-p 8 6))
         (phoff +elf64-ehdr-size+)
         (headers-size (+ +elf64-ehdr-size+ (* phnum +elf64-phdr-size+)))
         (interp-bytes (and dynamic-p (elf64-string-bytes (elf64-interpreter builder))))
         (interp-offset (and dynamic-p (align-up headers-size 8)))
         (after-interp (if dynamic-p (+ interp-offset (length interp-bytes)) headers-size))
         (text-offset (align-up after-interp +elf-page-size+))
         (text-bytes (binary-buffer-to-array (elf64-text-buf builder)))
         (text-size (length text-bytes))
         (text-addr (+ base text-offset))
         (rodata-offset (align-up (+ text-offset text-size) +elf-page-size+))
         (rodata-bytes (binary-buffer-to-array (elf64-rodata-buf builder)))
         (rodata-size (length rodata-bytes))
         (rodata-addr (+ base rodata-offset))
         (eh-frame-bytes (elf64-build-eh-frame text-size))
         (eh-frame-offset (align-up (+ rodata-offset rodata-size) 8))
         (eh-frame-size (length eh-frame-bytes))
         (eh-frame-hdr-bytes (elf64-build-eh-frame-hdr eh-frame-offset text-size))
         (eh-frame-hdr-offset (align-up (+ eh-frame-offset eh-frame-size) 4))
         (eh-frame-hdr-size (length eh-frame-hdr-bytes))
         (data-offset (align-up (+ eh-frame-hdr-offset eh-frame-hdr-size) +elf-page-size+))
         (data-bytes (binary-buffer-to-array (elf64-data-buf builder)))
         (data-size (length data-bytes))
         (data-addr (+ base data-offset))
         (dynsym-bytes (and dynamic-p (elf64-build-dynsym)))
         (dynsym-offset (and dynamic-p (align-up (+ data-offset data-size) 8)))
         (dynsym-size (if dynamic-p (length dynsym-bytes) 0))
         (needed-offsets (and dynamic-p
                              (loop for lib in (reverse (elf64-needed-libraries builder))
                                    collect (strtab-add dynstr lib))))
         (dynstr-bytes (and dynamic-p (strtab-bytes dynstr)))
         (dynstr-offset (and dynamic-p (align-up (+ dynsym-offset dynsym-size) 1)))
         (dynstr-size (if dynamic-p (length dynstr-bytes) 0))
         (rela-dyn-bytes (and dynamic-p (make-array 0 :element-type '(unsigned-byte 8))))
         (rela-dyn-offset (and dynamic-p (align-up (+ dynstr-offset dynstr-size) 8)))
         (rela-dyn-size 0)
         (rela-plt-bytes (and dynamic-p (make-array 0 :element-type '(unsigned-byte 8))))
         (rela-plt-offset (and dynamic-p (align-up (+ rela-dyn-offset rela-dyn-size) 8)))
         (rela-plt-size 0)
         (dynamic-offset (and dynamic-p (align-up (+ rela-plt-offset rela-plt-size) 8)))
         (dynamic-bytes (and dynamic-p
                             (elf64-build-dynamic-section
                              needed-offsets
                              (+ base dynstr-offset) dynstr-size
                              (+ base dynsym-offset)
                              (+ base rela-dyn-offset) rela-dyn-size
                              (+ base rela-plt-offset) rela-plt-size)))
         (dynamic-size (if dynamic-p (length dynamic-bytes) 0))
         (bss-size (elf64-bss-size builder))
         (bss-offset (align-up (+ (if dynamic-p (+ dynamic-offset dynamic-size)
                                      (+ data-offset data-size)) 0) 8))
         (bss-addr (+ base bss-offset))
         (sym-build (multiple-value-list (elf64-build-symtab builder strtab)))
         (sym-buf (first sym-build))
         (sym-local-count (second sym-build))
         (strtab-bytes (strtab-bytes strtab))
         (shstrtab-bytes (strtab-bytes shstrtab))
         (symtab-offset (align-up bss-offset 8))
         (symtab-size (length sym-buf))
         (strtab-offset (+ symtab-offset symtab-size))
         (strtab-size (length strtab-bytes))
         (shstrtab-offset (+ strtab-offset strtab-size))
         (shstrtab-size (length shstrtab-bytes))
         (shoff (align-up (+ shstrtab-offset shstrtab-size) 8))
         (n-sections (if dynamic-p 16 10))
         (dynsym-idx (and dynamic-p 8))
         (dynstr-idx (and dynamic-p 9))
         (symtab-idx (if dynamic-p 13 7))
         (strtab-idx (if dynamic-p 14 8))
         (shstrtab-idx (if dynamic-p 15 9))
         (entry-point (if (zerop (elf64-entry-point builder)) text-addr (elf64-entry-point builder)))
         (text-load-filesz (+ (- text-offset 0) text-size))
         (ro-load-filesz (- (+ eh-frame-hdr-offset eh-frame-hdr-size) rodata-offset))
         (data-load-end (if dynamic-p (+ dynamic-offset dynamic-size) (+ data-offset data-size)))
         (data-load-filesz (- data-load-end data-offset))
         (data-load-memsz (+ data-load-filesz bss-size))
         (out (elf-make-buffer)))
    ;; ELF header.
    (elf-buf-u8 out +elf-magic-0+)
    (elf-buf-u8 out +elf-magic-1+)
    (elf-buf-u8 out +elf-magic-2+)
    (elf-buf-u8 out +elf-magic-3+)
    (elf-buf-u8 out +elf-class-64+)
    (elf-buf-u8 out +elf-data-lsb+)
    (elf-buf-u8 out +elf-version-cur+)
    (elf-buf-u8 out +elf-osabi-none+)
    (binary-buffer-write-pad out 8)
    (binary-buffer-write-u16le out (elf64-elf-type builder))
    (binary-buffer-write-u16le out (elf64-machine builder))
    (binary-buffer-write-u32le out +elf-version-cur+)
    (binary-buffer-write-u64le out entry-point)
    (binary-buffer-write-u64le out phoff)
    (binary-buffer-write-u64le out shoff)
    (binary-buffer-write-u32le out 0)
    (binary-buffer-write-u16le out +elf64-ehdr-size+)
    (binary-buffer-write-u16le out +elf64-phdr-size+)
    (binary-buffer-write-u16le out phnum)
    (binary-buffer-write-u16le out +elf64-shdr-size+)
    (binary-buffer-write-u16le out n-sections)
    (binary-buffer-write-u16le out shstrtab-idx)
    ;; Program headers.
    (elf64-write-phdr out +pt-phdr+ +pf-r+ phoff (+ base phoff) (+ base phoff)
                      (* phnum +elf64-phdr-size+) (* phnum +elf64-phdr-size+) 8)
    (when dynamic-p
      (elf64-write-phdr out +pt-interp+ +pf-r+ interp-offset (+ base interp-offset)
                        (+ base interp-offset) (length interp-bytes) (length interp-bytes) 1))
    (elf64-write-phdr out +pt-load+ (logior +pf-r+ +pf-x+) 0 base base
                      text-load-filesz text-load-filesz +elf-page-size+)
    (elf64-write-phdr out +pt-load+ +pf-r+ rodata-offset rodata-addr rodata-addr
                      ro-load-filesz ro-load-filesz +elf-page-size+)
    (elf64-write-phdr out +pt-load+ (logior +pf-r+ +pf-w+) data-offset data-addr data-addr
                      data-load-filesz data-load-memsz +elf-page-size+)
    (when dynamic-p
      (elf64-write-phdr out +pt-dynamic+ (logior +pf-r+ +pf-w+) dynamic-offset
                        (+ base dynamic-offset) (+ base dynamic-offset)
                        dynamic-size dynamic-size 8))
    (elf64-write-phdr out +pt-gnu-stack+ (logior +pf-r+ +pf-w+) 0 0 0 0 0 16)
    (elf64-write-phdr out +pt-gnu-relro+ +pf-r+ data-offset data-addr data-addr
                      data-load-filesz data-load-filesz 1)
    ;; Section payloads.
    (when dynamic-p
      (binary-buffer-write-pad out (- interp-offset (length out)))
      (binary-buffer-write-bytes out interp-bytes))
    (binary-buffer-write-pad out (- text-offset (length out)))
    (binary-buffer-write-bytes out text-bytes)
    (binary-buffer-write-pad out (- rodata-offset (length out)))
    (binary-buffer-write-bytes out rodata-bytes)
    (binary-buffer-write-pad out (- eh-frame-offset (length out)))
    (binary-buffer-write-bytes out eh-frame-bytes)
    (binary-buffer-write-pad out (- eh-frame-hdr-offset (length out)))
    (binary-buffer-write-bytes out eh-frame-hdr-bytes)
    (binary-buffer-write-pad out (- data-offset (length out)))
    (binary-buffer-write-bytes out data-bytes)
    (when dynamic-p
      (binary-buffer-write-pad out (- dynsym-offset (length out)))
      (binary-buffer-write-bytes out dynsym-bytes)
      (binary-buffer-write-pad out (- dynstr-offset (length out)))
      (binary-buffer-write-bytes out dynstr-bytes)
      (binary-buffer-write-pad out (- rela-dyn-offset (length out)))
      (binary-buffer-write-bytes out rela-dyn-bytes)
      (binary-buffer-write-pad out (- rela-plt-offset (length out)))
      (binary-buffer-write-bytes out rela-plt-bytes)
      (binary-buffer-write-pad out (- dynamic-offset (length out)))
      (binary-buffer-write-bytes out dynamic-bytes))
    (binary-buffer-write-pad out (- symtab-offset (length out)))
    (binary-buffer-write-bytes out sym-buf)
    (binary-buffer-write-pad out (- strtab-offset (length out)))
    (binary-buffer-write-bytes out strtab-bytes)
    (binary-buffer-write-pad out (- shstrtab-offset (length out)))
    (binary-buffer-write-bytes out shstrtab-bytes)
    (binary-buffer-write-pad out (- shoff (length out)))
    ;; Section headers.
    (elf64-write-shdr-with-addr out 0 +sht-null+ 0 0 0 0 0 0 0 0)
    (when dynamic-p
      (elf64-write-shdr-with-addr out sh-interp-off +sht-progbits+ +shf-alloc+
                                  (+ base interp-offset) interp-offset (length interp-bytes)
                                  0 0 1 0))
    (elf64-write-shdr-with-addr out sh-text-off +sht-progbits+
                                (logior +shf-alloc+ +shf-execinstr+)
                                text-addr text-offset text-size 0 0 16 0)
    (elf64-write-shdr-with-addr out sh-rodata-off +sht-progbits+ +shf-alloc+
                                rodata-addr rodata-offset rodata-size 0 0 8 0)
    (elf64-write-shdr-with-addr out sh-data-off +sht-progbits+
                                (logior +shf-alloc+ +shf-write+)
                                data-addr data-offset data-size 0 0 8 0)
    (elf64-write-shdr-with-addr out sh-bss-off +sht-nobits+
                                (logior +shf-alloc+ +shf-write+)
                                bss-addr bss-offset bss-size 0 0 8 0)
    (elf64-write-shdr-with-addr out sh-eh-frame-off +sht-progbits+ +shf-alloc+
                                (+ base eh-frame-offset) eh-frame-offset eh-frame-size 0 0 8 0)
    (elf64-write-shdr-with-addr out sh-eh-frame-hdr-off +sht-progbits+ +shf-alloc+
                                (+ base eh-frame-hdr-offset) eh-frame-hdr-offset eh-frame-hdr-size 0 0 4 0)
    (when dynamic-p
      (elf64-write-shdr-with-addr out sh-dynsym-off +sht-dynsym+ +shf-alloc+
                                  (+ base dynsym-offset) dynsym-offset dynsym-size
                                  dynstr-idx 1 8 +elf64-sym-size+)
      (elf64-write-shdr-with-addr out sh-dynstr-off +sht-strtab+ +shf-alloc+
                                  (+ base dynstr-offset) dynstr-offset dynstr-size 0 0 1 0)
      (elf64-write-shdr-with-addr out sh-rela-dyn-off +sht-rela+ +shf-alloc+
                                  (+ base rela-dyn-offset) rela-dyn-offset rela-dyn-size
                                  dynsym-idx 0 8 +elf64-rela-size+)
      (elf64-write-shdr-with-addr out sh-rela-plt-off +sht-rela+ +shf-alloc+
                                  (+ base rela-plt-offset) rela-plt-offset rela-plt-size
                                  dynsym-idx 0 8 +elf64-rela-size+)
      (elf64-write-shdr-with-addr out sh-dynamic-off +sht-dynamic+
                                  (logior +shf-alloc+ +shf-write+)
                                  (+ base dynamic-offset) dynamic-offset dynamic-size
                                  dynstr-idx 0 8 16))
    (elf64-write-shdr-with-addr out sh-symtab-off +sht-symtab+ 0 0
                                symtab-offset symtab-size strtab-idx sym-local-count
                                8 +elf64-sym-size+)
    (elf64-write-shdr-with-addr out sh-strtab-off +sht-strtab+ 0 0
                                strtab-offset strtab-size 0 0 1 0)
    (elf64-write-shdr-with-addr out sh-shstrtab-off +sht-strtab+ 0 0
                                shstrtab-offset shstrtab-size 0 0 1 0)
    (binary-buffer-to-array out)))

(defun elf64-finalize (builder)
  "Assemble an ELF64 file according to BUILDER's e_type."
  (if (= (elf64-elf-type builder) +elf-type-rel+)
      (elf64-finalize-relocatable builder)
      (elf64-finalize-executable builder)))

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

(defun elf64-build-x86-64-start-wrapper (entry-bytes)
  "Build a Linux x86-64 _start wrapper followed by ENTRY-BYTES.

The wrapper reads argc/argv/envp from the initial process stack, calls the
entry function using the SysV ABI argument registers, and exits with the
entry function's return value via the exit syscall."
  (let ((buf (elf-make-buffer)))
    ;; xor %ebp,%ebp
    (binary-buffer-write-bytes buf '(#x31 #xed))
    ;; mov (%rsp),%rdi             ; argc
    (binary-buffer-write-bytes buf '(#x48 #x8b #x3c #x24))
    ;; lea 8(%rsp),%rsi           ; argv
    (binary-buffer-write-bytes buf '(#x48 #x8d #x74 #x24 #x08))
    ;; lea 8(%rsi,%rdi,8),%rdx    ; envp
    (binary-buffer-write-bytes buf '(#x48 #x8d #x54 #xfe #x08))
    ;; call entry
    (let ((call-offset (length buf)))
      (elf-buf-u8 buf #xe8)
      (let ((entry-offset (+ call-offset 5)))
        (binary-buffer-write-u32le buf (- entry-offset (+ call-offset 5)))))
    ;; mov %rax,%rdi
    (binary-buffer-write-bytes buf '(#x48 #x89 #xc7))
    ;; mov $60,%eax; syscall
    (binary-buffer-write-bytes buf '(#xb8 #x3c #x00 #x00 #x00 #x0f #x05))
    (let* ((wrapper-size (length buf))
           (call-immediate-offset 17)
           (rel32 (- wrapper-size (+ call-immediate-offset 4))))
      (setf (aref buf call-immediate-offset) (logand rel32 #xff)
            (aref buf (+ call-immediate-offset 1)) (logand (ash rel32 -8) #xff)
            (aref buf (+ call-immediate-offset 2)) (logand (ash rel32 -16) #xff)
            (aref buf (+ call-immediate-offset 3)) (logand (ash rel32 -24) #xff)))
    (binary-buffer-write-bytes buf entry-bytes)
    (binary-buffer-to-array buf)))

(defun compile-to-elf64 (code-bytes reloc-entries &key (output-file nil) (arch :x86-64) (bss-size 0)
                                                  compress)
  "Create an ELF64 relocatable object from CODE-BYTES and RELOC-ENTRIES.
   RELOC-ENTRIES is a list of (byte-offset . symbol-name) pairs from the
   x86-64 code generator.
   Returns the byte array; also writes to OUTPUT-FILE if provided."
  (let ((builder (make-elf64-object :machine (ecase arch
                                               (:x86-64 +elf-machine-x86-64+)
                                               (:arm64 +elf-machine-aarch64+)
                                               (:aarch64 +elf-machine-aarch64+)))))
    ;; Add code
    (setf (elf64-compress-text builder) (and compress t))
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

(defun compile-to-elf64-exec (code-bytes reloc-entries &key output-file (arch :x86-64)
                                                     (bss-size 0) (type :exec)
                                                     needed-libraries
                                                     (interpreter +elf64-default-interpreter+))
  "Create an ELF64 executable image from CODE-BYTES and RELOC-ENTRIES.

TYPE is either :EXEC for ET_EXEC or :DYN for ET_DYN PIE/shared-object output.
For x86-64 executables, a small _start wrapper is prepended before CODE-BYTES."
  (let* ((machine (ecase arch
                    (:x86-64 +elf-machine-x86-64+)
                    (:arm64 +elf-machine-aarch64+)
                    (:aarch64 +elf-machine-aarch64+)))
         (dynamic-p (ecase type
                      (:exec nil)
                      (:dyn t)
                      (:pie t)
                      (:shared t)))
         (builder (if dynamic-p
                      (make-elf64-dynamic :machine machine :interpreter interpreter)
                      (make-elf64-executable :machine machine)))
         (text-bytes (if (and (not dynamic-p) (eq arch :x86-64))
                         (elf64-build-x86-64-start-wrapper code-bytes)
                         code-bytes)))
    (elf64-add-text-bytes builder text-bytes)
    (elf64-add-global-symbol builder "_start" :section-idx (if dynamic-p 2 1) :value 0
                             :size (length text-bytes))
    (when (plusp bss-size)
      (elf64-add-bss builder bss-size))
    (dolist (library needed-libraries)
      (elf64-add-needed-library builder library))
    (let ((seen-syms (make-hash-table :test #'equal))
          (reloc-offset-delta (if (and (not dynamic-p) (eq arch :x86-64))
                                  (- (length text-bytes) (length code-bytes))
                                  0)))
      (dolist (reloc reloc-entries)
        (let* ((offset (+ (car reloc) reloc-offset-delta))
               (sym-name (cdr reloc)))
          (unless (gethash sym-name seen-syms)
            (elf64-add-global-symbol builder sym-name)
            (setf (gethash sym-name seen-syms) t))
          (elf64-add-reloc builder offset sym-name))))
    (let ((bytes (elf64-finalize builder)))
      (when output-file
        (write-elf64-file output-file bytes))
      bytes)))
