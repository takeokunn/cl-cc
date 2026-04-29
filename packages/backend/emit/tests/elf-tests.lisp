;;;; tests/unit/emit/elf-tests.lisp — ELF64 Binary Format Tests
;;;;
;;;; Tests for src/emit/binary/elf.lisp:
;;;; Buffer helpers, strtab builder, ELF64 builder, constants.

(in-package :cl-cc/test)

(defsuite elf-suite :description "ELF64 binary format tests"
  :parent cl-cc-unit-suite)


(in-suite elf-suite)
;;; ─── ELF Constants ──────────────────────────────────────────────────────

(deftest-each elf-constants
  "ELF64 constant values match the ELF specification."
  :cases (("magic-0"       #x7f              cl-cc/binary::+elf-magic-0+)
          ("magic-1"       (char-code #\E)   cl-cc/binary::+elf-magic-1+)
          ("magic-2"       (char-code #\L)   cl-cc/binary::+elf-magic-2+)
          ("magic-3"       (char-code #\F)   cl-cc/binary::+elf-magic-3+)
          ("class-64"      2                 cl-cc/binary::+elf-class-64+)
          ("machine-x86"   #x3E              cl-cc/binary::+elf-machine-x86-64+)
          ("machine-arm64" #xB7              cl-cc/binary::+elf-machine-aarch64+))
  (expected actual)
  (assert-equal expected actual))

(deftest-each elf-structure-sizes
  "ELF64 structure sizes match the ELF specification."
  :cases (("ehdr" 64 cl-cc/binary::+elf64-ehdr-size+)
          ("shdr" 64 cl-cc/binary::+elf64-shdr-size+)
          ("sym"  24 cl-cc/binary::+elf64-sym-size+)
          ("rela" 24 cl-cc/binary::+elf64-rela-size+))
  (expected actual)
  (assert-equal expected actual))

;;; ─── Buffer Helpers ─────────────────────────────────────────────────────

(deftest elf-make-buffer-empty
  "elf-make-buffer returns an empty adjustable byte vector."
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (assert-equal 0 (length buf))
    (assert-true (adjustable-array-p buf))))

(deftest elf-buf-u8-behavior
  "elf-buf-u8 appends a single byte and masks values to 8 bits."
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-u8 buf #xAB)
    (assert-equal 1 (length buf))
    (assert-equal #xAB (aref buf 0)))
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-u8 buf #x1FF)
    (assert-equal #xFF (aref buf 0))))

(deftest-each elf-buf-little-endian-writes
  "binary-buffer-write-u16le/u32le/u64le write little-endian: low byte at index 0."
  :cases (("u16le" #'cl-cc/binary::binary-buffer-write-u16le #x1234             2 '((0 #x34) (1 #x12)))
          ("u32le" #'cl-cc/binary::binary-buffer-write-u32le #xDEADBEEF         4 '((0 #xEF) (1 #xBE) (2 #xAD) (3 #xDE)))
          ("u64le" #'cl-cc/binary::binary-buffer-write-u64le #x0102030405060708 8 '((0 #x08) (7 #x01))))
  (writer value expected-len byte-checks)
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (funcall writer buf value)
    (assert-equal expected-len (length buf))
    (loop for (idx expected) in byte-checks
          do (assert-equal expected (aref buf idx)))))

(deftest binary-buffer-write-pad-zeros
  "binary-buffer-write-pad writes N zero bytes."
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::binary-buffer-write-pad buf 4)
    (assert-equal 4 (length buf))
    (assert-true (every #'zerop (coerce buf 'list)))))

(deftest-each binary-buffer-write-bytes-input-types
  "binary-buffer-write-bytes accepts both a byte vector and a plain list, writing 3 bytes."
  :cases (("vector" (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3)))
          ("list"   '(#x10 #x20 #x30)))
  (input)
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::binary-buffer-write-bytes buf input)
    (assert-equal 3 (length buf))
    (assert-equal (elt input 0) (aref buf 0))
    (assert-equal (elt input 2) (aref buf 2))))

(deftest binary-buffer-to-array-returns-simple-array
  "binary-buffer-to-array returns a simple (unsigned-byte 8) array."
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-u8 buf 42)
    (let ((arr (cl-cc/binary::binary-buffer-to-array buf)))
      (assert-true (typep arr '(simple-array (unsigned-byte 8) (*))))
      (assert-equal 1 (length arr))
      (assert-equal 42 (aref arr 0)))))

;;; ─── String Table Builder ───────────────────────────────────────────────

(deftest elf-strtab-initial-null
  "Fresh strtab starts with a null byte."
  (let ((st (cl-cc/binary::make-strtab)))
    (assert-equal 1 (length (cl-cc/binary::strtab-bytes st)))
    (assert-equal 0 (aref (cl-cc/binary::strtab-bytes st) 0))))

(deftest elf-strtab-add-behavior
  "strtab-add: returns offset 1 for first string; deduplicates; distinct strings get distinct offsets."
  (let* ((st (cl-cc/binary::make-strtab))
         (off (cl-cc/binary::strtab-add st "hello")))
    (assert-equal 1 off))
  (let* ((st (cl-cc/binary::make-strtab))
         (off1 (cl-cc/binary::strtab-add st "hello"))
         (off2 (cl-cc/binary::strtab-add st "hello")))
    (assert-equal off1 off2))
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
    (assert-equal 0 (cl-cc/binary::elf64-bss-size b))
    (assert-null (cl-cc/binary::elf64-symbols b))
    (assert-null (cl-cc/binary::elf64-rela-entries b))))

(deftest elf64-add-bss
  "add-bss accumulates reserved NOBITS size."
  (let ((b (cl-cc/binary::make-elf64-object)))
    (cl-cc/binary::elf64-add-bss b 16)
    (cl-cc/binary::elf64-add-bss b 8)
    (assert-equal 24 (cl-cc/binary::elf64-bss-size b))))

(deftest elf64-add-text-bytes
  "add-text-bytes appends machine code."
  (let ((b (cl-cc/binary::make-elf64-object))
        (code (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(#x48 #x89 #xC0))))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (assert-equal 3 (cl-cc/binary::elf64-text-size b))))

(deftest-each elf64-builder-list-accumulation
  "elf64-add-global-symbol and elf64-add-reloc each register exactly one entry."
  :cases (("symbol" (lambda (b) (cl-cc/binary::elf64-add-global-symbol b "_main" :section-idx 1 :value 0 :size 10))
                    #'cl-cc/binary::elf64-symbols)
          ("reloc"  (lambda (b) (cl-cc/binary::elf64-add-reloc b 4 "printf"))
                    #'cl-cc/binary::elf64-rela-entries))
  (add-fn accessor-fn)
  (let ((b (cl-cc/binary::make-elf64-object)))
    (funcall add-fn b)
    (assert-equal 1 (length (funcall accessor-fn b)))))

(deftest elf64-finalize-produces-bytes
  "elf64-finalize produces a non-empty byte vector."
  (let ((b (cl-cc/binary::make-elf64-object))
        (code (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(#xC3 0 0 0))))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (cl-cc/binary::elf64-add-global-symbol b "_start" :section-idx 1 :value 0 :size 4)
    (let ((result (cl-cc/binary::elf64-finalize b)))
      (assert-true (> (length result) 0))
      (assert-true (typep result '(simple-array (unsigned-byte 8) (*)))))))

(deftest elf64-finalize-header-prefix
  "Finalized ELF: magic \\x7FELF at bytes 0-3; ELFCLASS64=2 at byte 4; ELFDATA2LSB=1 at byte 5."
  (let* ((b (cl-cc/binary::make-elf64-object))
         (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (cl-cc/binary::elf64-add-global-symbol b "_start" :section-idx 1 :value 0 :size 1)
    (let ((result (cl-cc/binary::elf64-finalize b)))
      (assert-equal #x7F (aref result 0))
      (assert-equal (char-code #\E) (aref result 1))
      (assert-equal (char-code #\L) (aref result 2))
      (assert-equal (char-code #\F) (aref result 3))
      (assert-equal 2 (aref result 4))
      (assert-equal 1 (aref result 5)))))

