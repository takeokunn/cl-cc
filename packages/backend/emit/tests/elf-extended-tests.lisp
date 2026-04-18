;;;; tests/unit/emit/elf-extended-tests.lisp — ELF64 extended builder and buffer tests

(in-package :cl-cc/test)

(in-suite elf-suite)

;;; ─── Additional Buffer Tests ─────────────────────────────────────────────

(deftest elf-buf-s64le-behavior
  "elf-buf-s64le: -1 → all #xFF; positive values match elf-buf-u64le."
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-s64le buf -1)
    (assert-equal 8 (length buf))
    (assert-true (every (lambda (b) (= b #xFF)) (coerce buf 'list))))
  (let ((buf1 (cl-cc/binary::elf-make-buffer))
        (buf2 (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-s64le buf1 42)
    (cl-cc/binary::elf-buf-u64le buf2 42)
    (assert-equal (coerce buf1 'list) (coerce buf2 'list))))


(deftest elf-buf-u64le-full-check
  "elf-buf-u64le writes all 8 bytes correctly for a known pattern."
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-u64le buf #xDEADBEEFCAFEBABE)
    (assert-equal 8 (length buf))
    ;; little-endian: low byte first
    (assert-equal #xBE (aref buf 0))
    (assert-equal #xBA (aref buf 1))
    (assert-equal #xFE (aref buf 2))
    (assert-equal #xCA (aref buf 3))
    (assert-equal #xEF (aref buf 4))
    (assert-equal #xBE (aref buf 5))
    (assert-equal #xAD (aref buf 6))
    (assert-equal #xDE (aref buf 7))))

;;; ─── Additional String Table Tests ──────────────────────────────────────

(deftest elf-strtab-offset-layout
  "strtab layout: empty string at 0; first string at 1; offsets advance by len+1; total size correct."
  (let ((st (cl-cc/binary::make-strtab)))
    (assert-equal 0 (cl-cc/binary::strtab-add st "")))
  (let* ((st (cl-cc/binary::make-strtab))
         (off1 (cl-cc/binary::strtab-add st "abc"))
         (off2 (cl-cc/binary::strtab-add st "xy")))
    (assert-equal 1 off1)
    (assert-equal 5 off2))
  (let ((st (cl-cc/binary::make-strtab)))
    (cl-cc/binary::strtab-add st "ab")
    (assert-equal 4 (length (cl-cc/binary::strtab-bytes st)))))

;;; ─── Additional ELF64 Builder Tests ──────────────────────────────────────

(deftest elf64-add-multiple-text-bytes
  "Multiple calls to add-text-bytes accumulate."
  (let ((b (cl-cc/binary::make-elf64-object))
        (c1 (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(#x90 #x90)))
        (c2 (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(#xC3 #x00 #x00))))
    (cl-cc/binary::elf64-add-text-bytes b c1)
    (cl-cc/binary::elf64-add-text-bytes b c2)
    (assert-equal 5 (cl-cc/binary::elf64-text-size b))))

(deftest elf64-add-multiple-symbols
  "Multiple global symbols are all registered."
  (let ((b (cl-cc/binary::make-elf64-object)))
    (cl-cc/binary::elf64-add-global-symbol b "_start" :section-idx 1 :value 0 :size 4)
    (cl-cc/binary::elf64-add-global-symbol b "printf" :section-idx 0 :value 0 :size 0)
    (assert-equal 2 (length (cl-cc/binary::elf64-symbols b)))))

(deftest elf64-add-reloc-default-type
  "add-reloc default type is R_X86_64_PLT32."
  (let ((b (cl-cc/binary::make-elf64-object)))
    (cl-cc/binary::elf64-add-reloc b 1 "puts")
    (let ((entry (first (cl-cc/binary::elf64-rela-entries b))))
      (assert-equal 1 (first entry))          ; offset
      (assert-equal cl-cc/binary::+r-x86-64-plt32+ (second entry))  ; type
      (assert-equal "puts" (third entry))     ; sym-name
      (assert-equal -4 (fourth entry)))))     ; addend

(deftest elf64-finalize-header-fields
  "ELF header: e_type=ET_REL at [16]; e_machine=EM_X86_64 at [18]; e_shnum=7 at [60]; e_shstrndx=6 at [62]."
  (let* ((b (cl-cc/binary::make-elf64-object))
         (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (let ((result (cl-cc/binary::elf64-finalize b)))
      (assert-equal 1    (aref result 16))
      (assert-equal 0    (aref result 17))
      (assert-equal #x3E (aref result 18))
      (assert-equal 0    (aref result 19))
      (assert-equal 7    (aref result 60))
      (assert-equal 6    (aref result 62)))))

(deftest elf64-finalize-emits-bss-section-header
  "Finalized ELF includes a .bss NOBITS section header with zero file offset payload and reserved size."
  (let* ((b (cl-cc/binary::make-elf64-object))
         (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3)))
         (result nil)
         (shoff nil)
         (bss-shoff nil))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (cl-cc/binary::elf64-add-bss b 32)
    (setf result (cl-cc/binary::elf64-finalize b))
    (setf shoff (+ (aref result 40)
                   (ash (aref result 41) 8)
                   (ash (aref result 42) 16)
                   (ash (aref result 43) 24)))
    (setf bss-shoff (+ shoff (* 2 64)))
    ;; sh_type = SHT_NOBITS
    (assert-equal 8 (aref result (+ bss-shoff 4)))
    ;; sh_size = 32
    (assert-equal 32 (aref result (+ bss-shoff 32)))))

(deftest elf64-finalize-aligns-text-and-sections
  "ELF section offsets honor the declared 16-byte/8-byte alignments."
  (let* ((b (cl-cc/binary::make-elf64-object))
         (code (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(#x90 #x90 #xC3)))
         (result nil)
         (text-offset nil)
         (shoff nil))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (setf result (cl-cc/binary::elf64-finalize b))
    (setf shoff (+ (aref result 40)
                   (ash (aref result 41) 8)
                   (ash (aref result 42) 16)
                   (ash (aref result 43) 24)))
    (setf text-offset (+ (aref result (+ shoff 64 24))
                         (ash (aref result (+ shoff 64 25)) 8)
                         (ash (aref result (+ shoff 64 26)) 16)
                         (ash (aref result (+ shoff 64 27)) 24)))
    (assert-equal 0 (mod text-offset 16))
    (assert-equal 0 (mod shoff 8))))

(deftest elf64-arm64-header-machine
  "compile-to-elf64 can emit an AArch64 ELF header machine value."
  (let* ((code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC0)))
         (result (cl-cc/binary::compile-to-elf64 code nil :arch :arm64)))
    (assert-equal #xB7 (aref result 18))
    (assert-equal 0 (aref result 19))))

(deftest elf64-compile-to-elf64-with-bss
  "compile-to-elf64 can reserve .bss space without adding file payload bytes."
  (let* ((code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3)))
         (result (cl-cc/binary::compile-to-elf64 code nil :bss-size 64))
         (shoff (+ (aref result 40)
                   (ash (aref result 41) 8)
                   (ash (aref result 42) 16)
                   (ash (aref result 43) 24)))
         (bss-shoff (+ shoff (* 2 64))))
    (assert-true (typep result '(simple-array (unsigned-byte 8) (*))))
    (assert-equal 8 (aref result (+ bss-shoff 4)))
    (assert-equal 64 (aref result (+ bss-shoff 32)))))

(deftest elf64-finalize-size-at-least-header
  "Finalized ELF is at least 64 bytes (ELF header alone)."
  (let ((b (cl-cc/binary::make-elf64-object)))
    (let ((result (cl-cc/binary::elf64-finalize b)))
      (assert-true (>= (length result) 64)))))

(deftest elf64-compile-to-elf64-api
  "compile-to-elf64 builds a valid ELF from code bytes and reloc entries."
  (let* ((code (make-array 5 :element-type '(unsigned-byte 8)
                            :initial-contents '(#xE8 0 0 0 0)))  ; CALL rel32
         (relocs (list (cons 1 "printf")))
         (result (cl-cc/binary::compile-to-elf64 code relocs)))
    (assert-true (typep result '(simple-array (unsigned-byte 8) (*))))
    (assert-true (>= (length result) 64))
    (assert-equal #x7F (aref result 0))
    (assert-equal (char-code #\E) (aref result 1))))

(deftest-each elf64-symbol-and-reloc-type-constants
  "Symbol binding, symbol type, and relocation type constants match the ELF-spec."
  :cases (("stb-local"      0  cl-cc/binary::+stb-local+)
          ("stb-global"     1  cl-cc/binary::+stb-global+)
          ("stb-weak"       2  cl-cc/binary::+stb-weak+)
          ("stt-notype"     0  cl-cc/binary::+stt-notype+)
          ("stt-func"       2  cl-cc/binary::+stt-func+)
          ("r-x86-64-none"  0  cl-cc/binary::+r-x86-64-none+)
          ("r-x86-64-64"    1  cl-cc/binary::+r-x86-64-64+)
          ("r-x86-64-pc32"  2  cl-cc/binary::+r-x86-64-pc32+)
          ("r-x86-64-plt32" 4  cl-cc/binary::+r-x86-64-plt32+)
          ("r-x86-64-32"    10 cl-cc/binary::+r-x86-64-32+))
  (expected actual)
  (assert-equal expected actual))
