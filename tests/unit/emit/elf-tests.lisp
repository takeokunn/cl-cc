;;;; tests/unit/emit/elf-tests.lisp — ELF64 Binary Format Tests
;;;;
;;;; Tests for src/emit/binary/elf.lisp:
;;;; Buffer helpers, strtab builder, ELF64 builder, constants.

(in-package :cl-cc/test)

(defsuite elf-suite :description "ELF64 binary format tests")

;;; ─── ELF Constants ──────────────────────────────────────────────────────

(deftest elf-magic-bytes
  "ELF magic bytes are 0x7F 'E' 'L' 'F'."
  (assert-equal #x7f cl-cc/binary::+elf-magic-0+)
  (assert-equal (char-code #\E) cl-cc/binary::+elf-magic-1+)
  (assert-equal (char-code #\L) cl-cc/binary::+elf-magic-2+)
  (assert-equal (char-code #\F) cl-cc/binary::+elf-magic-3+))

(deftest elf-class-64-value
  "ELFCLASS64 = 2."
  (assert-equal 2 cl-cc/binary::+elf-class-64+))

(deftest elf-machine-x86-64
  "EM_X86_64 = #x3E."
  (assert-equal #x3E cl-cc/binary::+elf-machine-x86-64+))

(deftest elf-structure-sizes
  "ELF64 structure sizes are correct."
  (assert-equal 64 cl-cc/binary::+elf64-ehdr-size+)
  (assert-equal 64 cl-cc/binary::+elf64-shdr-size+)
  (assert-equal 24 cl-cc/binary::+elf64-sym-size+)
  (assert-equal 24 cl-cc/binary::+elf64-rela-size+))

;;; ─── Buffer Helpers ─────────────────────────────────────────────────────

(deftest elf-make-buffer-empty
  "elf-make-buffer returns an empty adjustable byte vector."
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (assert-equal 0 (length buf))
    (assert-true (adjustable-array-p buf))))

(deftest elf-buf-u8-writes-byte
  "elf-buf-u8 appends a single byte."
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-u8 buf #xAB)
    (assert-equal 1 (length buf))
    (assert-equal #xAB (aref buf 0))))

(deftest elf-buf-u8-masks-to-8-bits
  "elf-buf-u8 masks values to 8 bits."
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-u8 buf #x1FF)
    (assert-equal #xFF (aref buf 0))))

(deftest elf-buf-u16le-little-endian
  "elf-buf-u16le writes 16-bit value little-endian."
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-u16le buf #x1234)
    (assert-equal 2 (length buf))
    (assert-equal #x34 (aref buf 0))
    (assert-equal #x12 (aref buf 1))))

(deftest elf-buf-u32le-little-endian
  "elf-buf-u32le writes 32-bit value little-endian."
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-u32le buf #xDEADBEEF)
    (assert-equal 4 (length buf))
    (assert-equal #xEF (aref buf 0))
    (assert-equal #xBE (aref buf 1))
    (assert-equal #xAD (aref buf 2))
    (assert-equal #xDE (aref buf 3))))

(deftest elf-buf-u64le-little-endian
  "elf-buf-u64le writes 64-bit value little-endian."
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-u64le buf #x0102030405060708)
    (assert-equal 8 (length buf))
    (assert-equal #x08 (aref buf 0))
    (assert-equal #x01 (aref buf 7))))

(deftest elf-buf-pad-zeros
  "elf-buf-pad writes N zero bytes."
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-pad buf 4)
    (assert-equal 4 (length buf))
    (assert-true (every #'zerop (coerce buf 'list)))))

(deftest elf-buf-bytes-appends
  "elf-buf-bytes copies byte vector into buffer."
  (let ((buf (cl-cc/binary::elf-make-buffer))
        (data (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3))))
    (cl-cc/binary::elf-buf-bytes buf data)
    (assert-equal 3 (length buf))
    (assert-equal 1 (aref buf 0))
    (assert-equal 3 (aref buf 2))))

(deftest elf-buf-to-array-returns-simple-array
  "elf-buf-to-array returns a simple (unsigned-byte 8) array."
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-u8 buf 42)
    (let ((arr (cl-cc/binary::elf-buf-to-array buf)))
      (assert-true (typep arr '(simple-array (unsigned-byte 8) (*))))
      (assert-equal 1 (length arr))
      (assert-equal 42 (aref arr 0)))))

;;; ─── String Table Builder ───────────────────────────────────────────────

(deftest elf-strtab-initial-null
  "Fresh strtab starts with a null byte."
  (let ((st (cl-cc/binary::make-strtab)))
    (assert-equal 1 (length (cl-cc/binary::strtab-bytes st)))
    (assert-equal 0 (aref (cl-cc/binary::strtab-bytes st) 0))))

(deftest elf-strtab-add-returns-offset
  "strtab-add returns byte offset of added string."
  (let* ((st (cl-cc/binary::make-strtab))
         (off (cl-cc/binary::strtab-add st "hello")))
    (assert-equal 1 off)))

(deftest elf-strtab-add-dedup
  "strtab-add returns same offset for duplicate strings."
  (let* ((st (cl-cc/binary::make-strtab))
         (off1 (cl-cc/binary::strtab-add st "hello"))
         (off2 (cl-cc/binary::strtab-add st "hello")))
    (assert-equal off1 off2)))

(deftest elf-strtab-add-multiple
  "strtab-add tracks multiple distinct strings."
  (let* ((st (cl-cc/binary::make-strtab))
         (off1 (cl-cc/binary::strtab-add st "foo"))
         (off2 (cl-cc/binary::strtab-add st "bar")))
    (assert-false (= off1 off2))))

(deftest elf-strtab-bytes-contains-strings
  "strtab-bytes includes null-terminated strings."
  (let ((st (cl-cc/binary::make-strtab)))
    (cl-cc/binary::strtab-add st "hi")
    (let ((bytes (cl-cc/binary::strtab-bytes st)))
      ;; bytes[0]=\0, bytes[1]='h', bytes[2]='i', bytes[3]=\0
      (assert-true (>= (length bytes) 4))
      (assert-equal 0 (aref bytes 0))
      (assert-equal (char-code #\h) (aref bytes 1))
      (assert-equal (char-code #\i) (aref bytes 2))
      (assert-equal 0 (aref bytes 3)))))

;;; ─── ELF64 Builder ──────────────────────────────────────────────────────

(deftest elf64-builder-fresh
  "Fresh ELF64 builder has empty text and no symbols."
  (let ((b (cl-cc/binary::make-elf64-object)))
    (assert-equal 0 (cl-cc/binary::elf64-text-size b))
    (assert-null (cl-cc/binary::elf64-symbols b))
    (assert-null (cl-cc/binary::elf64-rela-entries b))))

(deftest elf64-add-text-bytes
  "add-text-bytes appends machine code."
  (let ((b (cl-cc/binary::make-elf64-object))
        (code (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(#x48 #x89 #xC0))))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (assert-equal 3 (cl-cc/binary::elf64-text-size b))))

(deftest elf64-add-global-symbol
  "add-global-symbol registers a symbol entry."
  (let ((b (cl-cc/binary::make-elf64-object)))
    (cl-cc/binary::elf64-add-global-symbol b "_main" :section-idx 1 :value 0 :size 10)
    (assert-equal 1 (length (cl-cc/binary::elf64-symbols b)))))

(deftest elf64-add-reloc
  "add-reloc registers a relocation entry."
  (let ((b (cl-cc/binary::make-elf64-object)))
    (cl-cc/binary::elf64-add-reloc b 4 "printf")
    (assert-equal 1 (length (cl-cc/binary::elf64-rela-entries b)))))

(deftest elf64-finalize-produces-bytes
  "elf64-finalize produces a non-empty byte vector."
  (let ((b (cl-cc/binary::make-elf64-object))
        (code (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(#xC3 0 0 0))))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (cl-cc/binary::elf64-add-global-symbol b "_start" :section-idx 1 :value 0 :size 4)
    (let ((result (cl-cc/binary::elf64-finalize b)))
      (assert-true (> (length result) 0))
      (assert-true (typep result '(simple-array (unsigned-byte 8) (*)))))))

(deftest elf64-finalize-starts-with-magic
  "Finalized ELF starts with \\x7FELF magic."
  (let ((b (cl-cc/binary::make-elf64-object))
        (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (cl-cc/binary::elf64-add-global-symbol b "_start" :section-idx 1 :value 0 :size 1)
    (let ((result (cl-cc/binary::elf64-finalize b)))
      (assert-equal #x7F (aref result 0))
      (assert-equal (char-code #\E) (aref result 1))
      (assert-equal (char-code #\L) (aref result 2))
      (assert-equal (char-code #\F) (aref result 3)))))

(deftest elf64-finalize-header-class64
  "ELF header has ELFCLASS64 at byte 4."
  (let ((b (cl-cc/binary::make-elf64-object))
        (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (cl-cc/binary::elf64-add-global-symbol b "_start" :section-idx 1 :value 0 :size 1)
    (let ((result (cl-cc/binary::elf64-finalize b)))
      (assert-equal 2 (aref result 4)))))

(deftest elf64-finalize-header-lsb
  "ELF header has ELFDATA2LSB (little-endian) at byte 5."
  (let ((b (cl-cc/binary::make-elf64-object))
        (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (cl-cc/binary::elf64-add-global-symbol b "_start" :section-idx 1 :value 0 :size 1)
    (let ((result (cl-cc/binary::elf64-finalize b)))
      (assert-equal 1 (aref result 5)))))

;;; ─── Additional Buffer Tests ─────────────────────────────────────────────

(deftest elf-buf-s64le-negative
  "elf-buf-s64le encodes -1 as all 0xFF bytes."
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-s64le buf -1)
    (assert-equal 8 (length buf))
    (assert-true (every (lambda (b) (= b #xFF)) (coerce buf 'list)))))

(deftest elf-buf-s64le-positive
  "elf-buf-s64le encodes positive value identically to elf-buf-u64le."
  (let ((buf1 (cl-cc/binary::elf-make-buffer))
        (buf2 (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-s64le buf1 42)
    (cl-cc/binary::elf-buf-u64le buf2 42)
    (assert-equal (coerce buf1 'list) (coerce buf2 'list))))

(deftest elf-buf-bytes-from-list
  "elf-buf-bytes accepts a list of bytes."
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-bytes buf '(#x10 #x20 #x30))
    (assert-equal 3 (length buf))
    (assert-equal #x10 (aref buf 0))
    (assert-equal #x30 (aref buf 2))))

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

(deftest elf-strtab-empty-string-at-zero
  "Empty string is deduplicated and always at offset 0."
  (let ((st (cl-cc/binary::make-strtab)))
    (assert-equal 0 (cl-cc/binary::strtab-add st ""))))

(deftest elf-strtab-second-string-offset
  "Second string starts at offset 1 + length-of-first + 1 (NUL)."
  (let* ((st (cl-cc/binary::make-strtab))
         (off1 (cl-cc/binary::strtab-add st "abc"))  ; offset 1, len=3, NUL => next at 5
         (off2 (cl-cc/binary::strtab-add st "xy")))  ; offset 5
    (assert-equal 1 off1)
    (assert-equal 5 off2)))

(deftest elf-strtab-size-after-adds
  "strtab byte size accounts for initial NUL + each string + NUL."
  (let ((st (cl-cc/binary::make-strtab)))
    (cl-cc/binary::strtab-add st "ab")  ; 1 + 2 + 1 = 4 bytes total
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

(deftest elf64-finalize-header-type-rel
  "ELF header e_type field is ET_REL (1) at bytes 16-17."
  (let ((b (cl-cc/binary::make-elf64-object))
        (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (let ((result (cl-cc/binary::elf64-finalize b)))
      ;; e_type at offset 16, little-endian: ET_REL=1 => bytes [1, 0]
      (assert-equal 1 (aref result 16))
      (assert-equal 0 (aref result 17)))))

(deftest elf64-finalize-header-machine-x86-64
  "ELF header e_machine field is EM_X86_64 (#x3E) at bytes 18-19."
  (let ((b (cl-cc/binary::make-elf64-object))
        (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (let ((result (cl-cc/binary::elf64-finalize b)))
      ;; e_machine at offset 18, little-endian: #x3E => bytes [#x3E, 0]
      (assert-equal #x3E (aref result 18))
      (assert-equal 0 (aref result 19)))))

(deftest elf64-finalize-section-count
  "ELF header e_shnum is 6 (NULL + .text + .rela.text + .symtab + .strtab + .shstrtab)."
  (let ((b (cl-cc/binary::make-elf64-object))
        (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (let ((result (cl-cc/binary::elf64-finalize b)))
      ;; e_shnum at offset 60 (2 bytes), little-endian
      (assert-equal 6 (aref result 60)))))

(deftest elf64-finalize-shstrndx
  "ELF header e_shstrndx is 5 (.shstrtab is the 6th section, index 5)."
  (let ((b (cl-cc/binary::make-elf64-object))
        (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (let ((result (cl-cc/binary::elf64-finalize b)))
      ;; e_shstrndx at offset 62 (2 bytes), little-endian
      (assert-equal 5 (aref result 62)))))

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

(deftest elf64-symbol-binding-type-constants
  "Symbol binding and type constants have correct ELF-spec values."
  (assert-equal 0 cl-cc/binary::+stb-local+)
  (assert-equal 1 cl-cc/binary::+stb-global+)
  (assert-equal 2 cl-cc/binary::+stb-weak+)
  (assert-equal 0 cl-cc/binary::+stt-notype+)
  (assert-equal 2 cl-cc/binary::+stt-func+))

(deftest elf64-reloc-type-constants
  "Relocation type constants match x86-64 ABI values."
  (assert-equal 0 cl-cc/binary::+r-x86-64-none+)
  (assert-equal 1 cl-cc/binary::+r-x86-64-64+)
  (assert-equal 2 cl-cc/binary::+r-x86-64-pc32+)
  (assert-equal 4 cl-cc/binary::+r-x86-64-plt32+)
  (assert-equal 10 cl-cc/binary::+r-x86-64-32+))
