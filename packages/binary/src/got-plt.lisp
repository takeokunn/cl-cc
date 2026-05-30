;;;; packages/binary/src/got-plt.lisp - GOT/PLT Lazy Binding (FR-587)
;;;;
;;;; Generates Procedure Linkage Table and Global Offset Table sections
;;;; for ELF64 x86-64 dynamically linked executables and shared objects.
;;;;
;;;; PLT entry layout (16 bytes each, System V AMD64 ABI):
;;;;   PLT[0]  pushq GOT+8(%rip)    ff 35 XX XX XX XX
;;;;           jmpq  *GOT+16(%rip)  ff 25 XX XX XX XX
;;;;           padding 4 bytes
;;;;   PLT[n]  jmpq  *GOT[n+3](%rip) ff 25 XX XX XX XX
;;;;           push  $n               68 XX XX XX XX
;;;;           jmpq  PLT[0]           e9 XX XX XX XX
;;;;
;;;; GOT layout (.got.plt):
;;;;   GOT[0] = .dynamic address  (filled by static linker)
;;;;   GOT[1] = link_map          (filled by ld.so)
;;;;   GOT[2] = _dl_runtime_resolve (filled by ld.so)
;;;;   GOT[3+] = per-symbol resolved addresses (filled lazily)

(in-package :cl-cc/binary)

;;; ------------------------------------------------------------
;;; add-plt-stubs  — generate .plt section bytes
;;; ------------------------------------------------------------

(defun add-plt-stubs (symbol-names)
  "Generate PLT section bytes for SYMBOL-NAMES (list of strings).
Returns a byte vector: 16-byte PLT[0] resolver + 16-byte per-symbol entries."
  (let* ((n (length symbol-names))
         (buf (elf-make-buffer)))
    ;; PLT[0]: pushq GOT+8(%rip); jmpq *GOT+16(%rip); pad to 16
    (binary-buffer-write-bytes buf #(#xff #x35 0 0 0 0
                                      #xff #x25 0 0 0 0
                                      0 0 0 0))
    ;; PLT[1..n]: jmpq *GOT[i+3](%rip); push $i; jmp PLT[0]
    (dotimes (i n)
      (binary-buffer-write-bytes buf #(#xff #x25))
      (binary-buffer-write-u32le buf 0)  ; disp32 — fixed by static linker
      (binary-buffer-write-bytes buf #(#x68))
      (binary-buffer-write-u32le buf i)  ; push index
      (binary-buffer-write-bytes buf #(#xe9))
      (binary-buffer-write-u32le buf 0)) ; rel32 — fixed by static linker
    (binary-buffer-to-array buf)))

;;; ------------------------------------------------------------
;;; add-got-entries  — generate .got.plt section bytes
;;; ------------------------------------------------------------

(defun add-got-entries (symbol-count)
  "Generate GOT bytes: 3 reserved (GOT[0..2]) + SYMBOL-COUNT slots.
All entries are zero; ld.so fills them at runtime."
  (let ((buf (elf-make-buffer)))
    (loop repeat (+ 3 symbol-count)
          do (binary-buffer-write-u64le buf 0))
    (binary-buffer-to-array buf)))

;;; ------------------------------------------------------------
;;; add-dynamic-relocations  — build .rela.plt R_X86_64_JUMP_SLOT
;;; ------------------------------------------------------------

(defun add-dynamic-relocations (symbol-names got-plt-addr)
  "Build .rela.plt bytes: one Elf64_Rela per symbol.
Each relocation targets GOT[3+i] at GOT-PLT-ADDR.
Symbol indices start at 1 (STN_UNDEF is 0)."
  (let ((buf (elf-make-buffer))
        (i 0))
    (dolist (name symbol-names)
      (declare (ignore name))
      (let ((got-off (+ got-plt-addr (* (+ 3 i) 8))))
        (binary-buffer-write-u64le buf got-off)
        (binary-buffer-write-u64le buf
          (logior (ash (1+ i) 32) +r-x86-64-jump-slot+))
        (binary-buffer-write-s64le buf 0))
      (incf i))
    (binary-buffer-to-array buf)))

;;; ------------------------------------------------------------
;;; bind-now-mode  — DT_FLAGS with DF_BIND_NOW
;;; ------------------------------------------------------------

(defun bind-now-mode ()
  "Return DF_BIND_NOW flag for DT_FLAGS in .dynamic section.
Causes the dynamic linker to resolve all PLT symbols at load time."
  +df-bind-now+)

;;; ------------------------------------------------------------
;;; setup-got-plt  — main entry point
;;; ------------------------------------------------------------

(defun setup-got-plt (symbol-names &key (got-plt-addr 0))
  "Generate all GOT/PLT data for SYMBOL-NAMES.
Returns (values plt-bytes got-plt-bytes rela-plt-bytes bind-now-flag)."
  (values (add-plt-stubs symbol-names)
          (add-got-entries (length symbol-names))
          (add-dynamic-relocations symbol-names got-plt-addr)
          (bind-now-mode)))
