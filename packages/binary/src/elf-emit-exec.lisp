;;;; packages/binary/src/elf-emit-exec.lisp — ELF64 executable/dynamic image assembly
;;;
;;; Contains:
;;;   - elf64-string-bytes — ASCII byte encoding helper
;;;   - elf64-build-dynsym — minimal dynamic symbol table
;;;   - elf64-build-dynamic-section — .dynamic segment entries
;;;   - %elf64-write-exec-ehdr — write ELF header for ET_EXEC/ET_DYN
;;;   - elf64-finalize-executable — assemble ET_EXEC or ET_DYN image
;;;   - elf64-build-x86-64-start-wrapper — Linux x86-64 _start prologue
;;;   - compile-to-elf64-exec — public entry point: code+relocs → executable bytes
;;;
;;; Loads after elf-emit.lisp (which provides elf64-finalize dispatch,
;;; write-elf64-file, and the relocatable path).
(in-package :cl-cc/binary)

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
    (elf64-write-dynamic-entry buf +dt-flags+ +df-bind-now+)
    (elf64-write-dynamic-entry buf +dt-null+ 0)
    (binary-buffer-to-array buf)))

(defun %elf64-write-exec-ehdr (out elf-type machine entry-point phoff shoff phnum n-sections shstrtab-idx)
  "Write the 64-byte ELF header for an ET_EXEC or ET_DYN image into OUT.

Parallel to %elf64-write-rel-ehdr in elf-emit.lisp; differs in e_type,
e_entry, e_phoff, e_phentsize, and e_phnum fields."
  (elf-buf-u8 out +elf-magic-0+)
  (elf-buf-u8 out +elf-magic-1+)
  (elf-buf-u8 out +elf-magic-2+)
  (elf-buf-u8 out +elf-magic-3+)
  (elf-buf-u8 out +elf-class-64+)
  (elf-buf-u8 out +elf-data-lsb+)
  (elf-buf-u8 out +elf-version-cur+)
  (elf-buf-u8 out +elf-osabi-none+)
  (binary-buffer-write-pad out 8)
  (binary-buffer-write-u16le out elf-type)
  (binary-buffer-write-u16le out machine)
  (binary-buffer-write-u32le out +elf-version-cur+)
  (binary-buffer-write-u64le out entry-point)
  (binary-buffer-write-u64le out phoff)
  (binary-buffer-write-u64le out shoff)
  (binary-buffer-write-u32le out 0)               ; e_flags
  (binary-buffer-write-u16le out +elf64-ehdr-size+)
  (binary-buffer-write-u16le out +elf64-phdr-size+)
  (binary-buffer-write-u16le out phnum)
  (binary-buffer-write-u16le out +elf64-shdr-size+)
  (binary-buffer-write-u16le out n-sections)
  (binary-buffer-write-u16le out shstrtab-idx))

(defun elf64-finalize-executable (builder)
  "Assemble an ELF64 ET_EXEC or ET_DYN image with program headers."
  (let* ((dynamic-p (= (elf64-elf-type builder) +elf-type-dyn+))
         (interp-p (and dynamic-p (not (elf64-shared-object builder))))
         (base (if dynamic-p 0 +elf64-exec-base+))
         (shstrtab (make-strtab))
         (strtab (make-strtab))
         (dynstr (make-strtab))
         (sh-text-off (strtab-add shstrtab ".text"))
         (sh-rodata-off (strtab-add shstrtab ".rodata"))
         (sh-rodata-str-off (strtab-add shstrtab ".rodata.str"))
         (sh-data-off (strtab-add shstrtab ".data"))
         (sh-bss-off (strtab-add shstrtab ".bss"))
         (sh-eh-frame-off (strtab-add shstrtab ".eh_frame"))
         (sh-eh-frame-hdr-off (strtab-add shstrtab ".eh_frame_hdr"))
         (sh-symtab-off (strtab-add shstrtab ".symtab"))
         (sh-strtab-off (strtab-add shstrtab ".strtab"))
         (sh-debug-line-off (strtab-add shstrtab ".debug_line"))
         (sh-shstrtab-off (strtab-add shstrtab ".shstrtab"))
         (sh-interp-off (and interp-p (strtab-add shstrtab ".interp")))
         (sh-dynamic-off (and dynamic-p (strtab-add shstrtab ".dynamic")))
         (sh-dynsym-off (and dynamic-p (strtab-add shstrtab ".dynsym")))
         (sh-dynstr-off (and dynamic-p (strtab-add shstrtab ".dynstr")))
         (sh-rela-dyn-off (and dynamic-p (strtab-add shstrtab ".rela.dyn")))
         (sh-rela-plt-off (and dynamic-p (strtab-add shstrtab ".rela.plt")))
         (phnum (if dynamic-p (if interp-p 8 7) 6))
         (phoff +elf64-ehdr-size+)
         (headers-size (+ +elf64-ehdr-size+ (* phnum +elf64-phdr-size+)))
         (interp-bytes (and interp-p (elf64-string-bytes (elf64-interpreter builder))))
         (interp-offset (and interp-p (align-up headers-size 8)))
         (after-interp (if interp-p (+ interp-offset (length interp-bytes)) headers-size))
         (text-offset (align-up after-interp +elf-page-size+))
         (text-bytes (binary-buffer-to-array (elf64-text-buf builder)))
         (text-size (length text-bytes))
         (text-addr (+ base text-offset))
         (rodata-offset (align-up (+ text-offset text-size) +elf-page-size+))
         (rodata-bytes (binary-buffer-to-array (elf64-rodata-buf builder)))
         (rodata-size (length rodata-bytes))
         (rodata-addr (+ base rodata-offset))
         (rodata-str-offset (align-up (+ rodata-offset rodata-size) 1))
         (rodata-str-bytes (binary-buffer-to-array (elf64-rodata-str-buf builder)))
         (rodata-str-size (length rodata-str-bytes))
         (rodata-str-present-p (plusp rodata-str-size))
         (rodata-str-section-delta (if rodata-str-present-p 1 0))
         (rodata-str-addr (+ base rodata-str-offset))
         (eh-frame-bytes (elf64-build-eh-frame text-size))
         (eh-frame-offset (align-up (+ rodata-str-offset rodata-str-size) 8))
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
         (debug-line-bytes (%emit-dwarf-line-info text-size))
         (symtab-offset (align-up bss-offset 8))
         (symtab-size (length sym-buf))
         (strtab-offset (+ symtab-offset symtab-size))
         (strtab-size (length strtab-bytes))
         (debug-line-offset (+ strtab-offset strtab-size))
         (debug-line-size (length debug-line-bytes))
         (shstrtab-offset (+ debug-line-offset debug-line-size))
         (shstrtab-size (length shstrtab-bytes))
         (shoff (align-up (+ shstrtab-offset shstrtab-size) 8))
         (n-sections (+ (if dynamic-p (+ 16 (if interp-p 1 0)) 11)
                        rodata-str-section-delta))
         (dynsym-idx (and dynamic-p (+ 7 (if interp-p 1 0) rodata-str-section-delta)))
         (dynstr-idx (and dynamic-p (1+ dynsym-idx)))
         (symtab-idx (+ (if dynamic-p (+ dynsym-idx 5) 7) rodata-str-section-delta))
         (strtab-idx (1+ symtab-idx))
         (debug-line-idx (1+ strtab-idx))
         (shstrtab-idx (1+ debug-line-idx))
         (entry-point (if (zerop (elf64-entry-point builder)) text-addr (elf64-entry-point builder)))
         (text-load-filesz (+ (- text-offset 0) text-size))
         (ro-load-filesz (- (+ eh-frame-hdr-offset eh-frame-hdr-size) rodata-offset))
         (data-load-end (if dynamic-p (+ dynamic-offset dynamic-size) (+ data-offset data-size)))
         (data-load-filesz (- data-load-end data-offset))
         (data-load-memsz (+ data-load-filesz bss-size))
         (out (elf-make-buffer)))
    ;; ELF header.
    (%elf64-write-exec-ehdr out (elf64-elf-type builder) (elf64-machine builder)
                            entry-point phoff shoff phnum n-sections shstrtab-idx)
    ;; Program headers.
    (elf64-write-phdr out +pt-phdr+ +pf-r+ phoff (+ base phoff) (+ base phoff)
                      (* phnum +elf64-phdr-size+) (* phnum +elf64-phdr-size+) 8)
    (when interp-p
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
    (when interp-p
      (binary-buffer-write-pad out (- interp-offset (length out)))
      (binary-buffer-write-bytes out interp-bytes))
    (binary-buffer-write-pad out (- text-offset (length out)))
    (binary-buffer-write-bytes out text-bytes)
    (binary-buffer-write-pad out (- rodata-offset (length out)))
    (binary-buffer-write-bytes out rodata-bytes)
    (when rodata-str-present-p
      (binary-buffer-write-pad out (- rodata-str-offset (length out)))
      (binary-buffer-write-bytes out rodata-str-bytes))
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
    (binary-buffer-write-pad out (- debug-line-offset (length out)))
    (binary-buffer-write-bytes out debug-line-bytes)
    (binary-buffer-write-pad out (- shstrtab-offset (length out)))
    (binary-buffer-write-bytes out shstrtab-bytes)
    (binary-buffer-write-pad out (- shoff (length out)))
    ;; Section headers.
    (elf64-write-shdr-with-addr out 0 +sht-null+ 0 0 0 0 0 0 0 0)
    (when interp-p
      (elf64-write-shdr-with-addr out sh-interp-off +sht-progbits+ +shf-alloc+
                                  (+ base interp-offset) interp-offset (length interp-bytes)
                                  0 0 1 0))
    (elf64-write-shdr-with-addr out sh-text-off +sht-progbits+
                                (logior +shf-alloc+ +shf-execinstr+)
                                text-addr text-offset text-size 0 0 16 0)
    (elf64-write-shdr-with-addr out sh-rodata-off +sht-progbits+ +shf-alloc+
                                rodata-addr rodata-offset rodata-size 0 0 8 0)
    (when rodata-str-present-p
      (elf64-write-shdr-with-addr out sh-rodata-str-off +sht-progbits+
                                  (logior +shf-alloc+ +shf-merge+ +shf-strings+)
                                  rodata-str-addr rodata-str-offset rodata-str-size 0 0 1 1))
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
    (elf64-write-shdr-with-addr out sh-debug-line-off +sht-progbits+ 0 0
                                debug-line-offset debug-line-size 0 0 1 0)
    (elf64-write-shdr-with-addr out sh-shstrtab-off +sht-strtab+ 0 0
                                shstrtab-offset shstrtab-size 0 0 1 0)
    (binary-buffer-to-array out)))

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
                      (make-elf64-dynamic :machine machine :interpreter interpreter
                                          :shared-object (eq type :shared))
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
