;;;; tests/unit/emit/macho-tests.lisp — Mach-O Binary Format Tests
;;;;
;;;; Tests for src/emit/binary/macho.lisp:
;;;; Constants, structures, buffer helpers, serialization, builder API.

(in-package :cl-cc/test)

(defsuite macho-suite :description "Mach-O binary format tests")

;;; ─── Constants ──────────────────────────────────────────────────────────

(deftest macho-constants
  "Mach-O magic, CPU types, file type, and load command constants."
  (assert-equal #xFEEDFACF cl-cc/binary:+mh-magic-64+)
  (assert-equal #x01000007 cl-cc/binary:+cpu-type-x86-64+)
  (assert-equal #x0100000C cl-cc/binary:+cpu-type-arm64+)
  (assert-equal 2 cl-cc/binary:+mh-execute+)
  (assert-equal #x19 cl-cc/binary:+lc-segment-64+)
  (assert-equal #x03 cl-cc/binary:+lc-symtab+))

;;; ─── Structure Defaults ─────────────────────────────────────────────────

(deftest macho-header-defaults
  "Default mach-header has correct magic and file type."
  (let ((hdr (cl-cc/binary::make-mach-header)))
    (assert-equal #xFEEDFACF (cl-cc/binary:mach-header-magic hdr))
    (assert-equal #x01000007 (cl-cc/binary:mach-header-cputype hdr))
    (assert-equal 2 (cl-cc/binary:mach-header-filetype hdr))
    (assert-equal 0 (cl-cc/binary:mach-header-ncmds hdr))))

(deftest macho-command-defaults
  "Default segment-command has LC_SEGMENT_64; default entry-point-command has LC_MAIN."
  (let ((seg (cl-cc/binary::make-segment-command)))
    (assert-equal #x19 (cl-cc/binary:segment-command-cmd seg))
    (assert-equal "" (cl-cc/binary:segment-command-segname seg)))
  (let ((ep (cl-cc/binary::make-entry-point-command)))
    (assert-equal cl-cc/binary:+lc-main+ (cl-cc/binary:entry-point-command-cmd ep))
    (assert-equal 0 (cl-cc/binary:entry-point-command-entryoff ep))))

;;; ─── Utilities ──────────────────────────────────────────────────────────

(deftest-each macho-align-up
  "align-up rounds to next alignment boundary."
  :cases (("already-aligned" 4096 4096  4096)
          ("needs-rounding"  4097 4096  8192)
          ("zero"            0    4096  0)
          ("one-to-16"       1    16    16)
          ("exact-boundary"  32   16    32))
  (value alignment expected)
  (assert-equal expected (cl-cc/binary:align-up value alignment)))

(deftest macho-string-to-ascii
  "string-to-ascii-bytes converts to byte vector."
  (let ((bytes (cl-cc/binary::string-to-ascii-bytes "ABC")))
    (assert-equal 3 (length bytes))
    (assert-equal 65 (aref bytes 0))
    (assert-equal 66 (aref bytes 1))
    (assert-equal 67 (aref bytes 2))))

;;; ─── Buffer ─────────────────────────────────────────────────────────────

(deftest macho-buffer-operations
  "byte-buffer: fresh → empty; write-byte appends; get-bytes returns simple array."
  (let ((buf (cl-cc/binary::make-byte-buffer)))
    (assert-equal 0 (length (cl-cc/binary::byte-buffer-data buf))))
  (let ((buf (cl-cc/binary::make-byte-buffer)))
    (cl-cc/binary::buffer-write-byte buf 42)
    (assert-equal 1 (length (cl-cc/binary::byte-buffer-data buf)))
    (assert-equal 42 (aref (cl-cc/binary::byte-buffer-data buf) 0)))
  (let ((buf (cl-cc/binary::make-byte-buffer)))
    (cl-cc/binary::buffer-write-byte buf 1)
    (cl-cc/binary::buffer-write-byte buf 2)
    (let ((result (cl-cc/binary::buffer-get-bytes buf)))
      (assert-true (typep result '(simple-array (unsigned-byte 8) (*))))
      (assert-equal 2 (length result)))))

;;; ─── Serialization ──────────────────────────────────────────────────────

(deftest macho-serialize-uint-le
  "serialize-uint32-le writes 4 bytes; serialize-uint64-le writes 8 bytes; low byte first."
  (let ((buf (cl-cc/binary::make-byte-buffer)))
    (cl-cc/binary:serialize-uint32-le #xDEADBEEF buf)
    (let ((data (cl-cc/binary::byte-buffer-data buf)))
      (assert-equal 4 (length data))
      (assert-equal #xEF (aref data 0))
      (assert-equal #xBE (aref data 1))
      (assert-equal #xAD (aref data 2))
      (assert-equal #xDE (aref data 3))))
  (let ((buf (cl-cc/binary::make-byte-buffer)))
    (cl-cc/binary:serialize-uint64-le #x0102030405060708 buf)
    (let ((data (cl-cc/binary::byte-buffer-data buf)))
      (assert-equal 8 (length data))
      (assert-equal #x08 (aref data 0))
      (assert-equal #x01 (aref data 7)))))

(deftest macho-serialize-string-16-pads
  "serialize-string-16 writes exactly 16 bytes with null padding."
  (let ((buf (cl-cc/binary::make-byte-buffer)))
    (cl-cc/binary::serialize-string-16 "hi" buf)
    (let ((data (cl-cc/binary::byte-buffer-data buf)))
      (assert-equal 16 (length data))
      (assert-equal (char-code #\h) (aref data 0))
      (assert-equal (char-code #\i) (aref data 1))
      (assert-equal 0 (aref data 2))
      (assert-equal 0 (aref data 15)))))

(deftest macho-serialize-mach-header
  "Serialized mach-header: 32 bytes; starts with FEEDFACF (little-endian: CF FA ED FE)."
  (let ((buf (cl-cc/binary::make-byte-buffer))
        (hdr (cl-cc/binary::make-mach-header)))
    (cl-cc/binary::serialize-mach-header hdr buf)
    (let ((data (cl-cc/binary::byte-buffer-data buf)))
      (assert-equal 32  (length data))
      (assert-equal #xCF (aref data 0))
      (assert-equal #xFA (aref data 1))
      (assert-equal #xED (aref data 2))
      (assert-equal #xFE (aref data 3)))))

;;; ─── Builder API ────────────────────────────────────────────────────────

(deftest-each macho-builder-creation
  "make-mach-o-builder creates a non-nil builder for all supported targets."
  :cases (("x86-64" :x86-64)
          ("arm64"  :arm64))
  (target)
  (assert-true (cl-cc/binary:make-mach-o-builder target)))

(deftest macho-add-entry-point
  "add-entry-point sets offset in builder."
  (let ((b (cl-cc/binary:make-mach-o-builder :x86-64)))
    (cl-cc/binary:add-entry-point b 0)
    (assert-true b)))

(deftest macho-add-symbol
  "add-symbol adds a symbol entry."
  (let ((b (cl-cc/binary:make-mach-o-builder :x86-64)))
    (cl-cc/binary:add-symbol b "_main" :value 0 :sect 1)
    (assert-true b)))

(deftest macho-build-produces-bytes
  "build-mach-o produces non-empty byte vector."
  (let ((b (cl-cc/binary:make-mach-o-builder :x86-64))
        (code (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(#xC3 0 0 0))))
    (cl-cc/binary:add-entry-point b 0)
    (let ((result (cl-cc/binary:build-mach-o b code)))
      (assert-true (> (length result) 0))
      (assert-true (typep result '(simple-array (unsigned-byte 8) (*)))))))

(deftest macho-build-starts-with-magic
  "Built Mach-O starts with FEEDFACF magic."
  (let ((b (cl-cc/binary:make-mach-o-builder :x86-64))
        (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary:add-entry-point b 0)
    (let ((result (cl-cc/binary:build-mach-o b code)))
      ;; FEEDFACF little-endian: CF FA ED FE
      (assert-equal #xCF (aref result 0))
      (assert-equal #xFA (aref result 1))
      (assert-equal #xED (aref result 2))
      (assert-equal #xFE (aref result 3)))))

(deftest macho-build-arm64-magic
  "ARM64 Mach-O also starts with FEEDFACF (same magic, different cputype)."
  (let ((b (cl-cc/binary:make-mach-o-builder :arm64))
        (code (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(#xD6 #x5F #x03 #xC0))))
    (cl-cc/binary:add-entry-point b 0)
    (let ((result (cl-cc/binary:build-mach-o b code)))
      (assert-equal #xCF (aref result 0))
      ;; CPU type at bytes 4-7 should differ from x86-64
      (let ((b2 (cl-cc/binary:make-mach-o-builder :x86-64)))
        (cl-cc/binary:add-entry-point b2 0)
        (let ((x64 (cl-cc/binary:build-mach-o b2 code)))
          (assert-false (= (aref result 4) (aref x64 4))))))))

;;; ─── Additional Structure Tests ─────────────────────────────────────────

(deftest macho-structure-defaults
  "Default nlist (zeroed), symtab-command (LC_SYMTAB, 24-byte cmdsize), and section (empty names) defaults."
  (let ((nl (cl-cc/binary::make-nlist)))
    (assert-equal 0 (cl-cc/binary::nlist-n-strx nl))
    (assert-equal 0 (cl-cc/binary::nlist-n-type nl))
    (assert-equal 0 (cl-cc/binary::nlist-n-sect nl))
    (assert-equal 0 (cl-cc/binary::nlist-n-desc nl))
    (assert-equal 0 (cl-cc/binary::nlist-n-value nl)))
  (let ((sc (cl-cc/binary::make-symtab-command)))
    (assert-equal cl-cc/binary:+lc-symtab+ (cl-cc/binary::symtab-command-cmd sc))
    (assert-equal 24 (cl-cc/binary::symtab-command-cmdsize sc))
    (assert-equal 0 (cl-cc/binary::symtab-command-nsyms sc)))
  (let ((sect (cl-cc/binary::make-section)))
    (assert-equal "" (cl-cc/binary:section-sectname sect))
    (assert-equal "" (cl-cc/binary:section-segname sect))
    (assert-equal 0 (cl-cc/binary:section-size sect))
    (assert-equal 0 (cl-cc/binary:section-addr sect))))

(deftest macho-lc-main-constant
  "LC_MAIN constant value includes the REQ_DYLD bit."
  (assert-equal #x80000028 cl-cc/binary:+lc-main+))

(deftest macho-extended-constants
  "Header flag constants and CPU subtype constants."
  (assert-equal 1 cl-cc/binary:+mh-noundefs+)
  (assert-equal 4 cl-cc/binary:+mh-dyldlink+)
  (assert-equal #x200000 cl-cc/binary:+mh-pie+)
  (assert-equal #x00000003 cl-cc/binary:+cpu-subtype-x86-64-all+)
  (assert-equal #x00000000 cl-cc/binary:+cpu-subtype-arm64-all+))

;;; ─── Additional Serialization Tests ─────────────────────────────────────

(deftest macho-serialize-nlist-size
  "Serialized nlist is 18 bytes: uint32(n-strx) + u8(n-type) + u8(n-sect) + uint32(n-desc) + uint64(n-value)."
  ;; serialize-nlist: 4 + 1 + 1 + 4 + 8 = 18 bytes
  (let ((buf (cl-cc/binary::make-byte-buffer))
        (nl (cl-cc/binary::make-nlist :n-strx 1 :n-type #x0f :n-sect 1 :n-desc 0 :n-value 0)))
    (cl-cc/binary::serialize-nlist nl buf)
    (assert-equal 18 (length (cl-cc/binary::byte-buffer-data buf)))))

(deftest macho-serialize-nlist-n-strx
  "Serialized nlist first 4 bytes are n-strx in little-endian."
  (let ((buf (cl-cc/binary::make-byte-buffer))
        (nl (cl-cc/binary::make-nlist :n-strx #x00000005)))
    (cl-cc/binary::serialize-nlist nl buf)
    (let ((data (cl-cc/binary::byte-buffer-data buf)))
      (assert-equal 5 (aref data 0))
      (assert-equal 0 (aref data 1)))))

(deftest macho-command-serialization-sizes
  "Serialized command sizes: entry-point=24, symtab=24, section=80 bytes."
  (let ((buf (cl-cc/binary::make-byte-buffer))
        (ep (cl-cc/binary::make-entry-point-command)))
    (cl-cc/binary::serialize-entry-point ep buf)
    (assert-equal 24 (length (cl-cc/binary::byte-buffer-data buf))))
  (let ((buf (cl-cc/binary::make-byte-buffer))
        (sc (cl-cc/binary::make-symtab-command)))
    (cl-cc/binary::serialize-symtab-command sc buf)
    (assert-equal 24 (length (cl-cc/binary::byte-buffer-data buf))))
  (let ((buf (cl-cc/binary::make-byte-buffer))
        (sect (cl-cc/binary::make-section :sectname "__text" :segname "__TEXT")))
    (cl-cc/binary::serialize-section sect buf)
    (assert-equal 80 (length (cl-cc/binary::byte-buffer-data buf)))))

(deftest macho-buffer-write-bytes-sequence
  "buffer-write-bytes appends multiple bytes."
  (let ((buf (cl-cc/binary::make-byte-buffer))
        (bytes (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(10 20 30))))
    (cl-cc/binary::buffer-write-bytes buf bytes)
    (let ((data (cl-cc/binary::byte-buffer-data buf)))
      (assert-equal 3 (length data))
      (assert-equal 10 (aref data 0))
      (assert-equal 20 (aref data 1))
      (assert-equal 30 (aref data 2)))))

;;; ─── Additional Builder API Tests ───────────────────────────────────────

(deftest-each macho-builder-cputypes
  "make-mach-o-builder sets the correct CPU type constant in the header."
  :cases (("x86-64" :x86-64 cl-cc/binary:+cpu-type-x86-64+)
          ("arm64"  :arm64  cl-cc/binary:+cpu-type-arm64+))
  (target expected-cputype)
  (let* ((b (cl-cc/binary:make-mach-o-builder target))
         (hdr (cl-cc/binary::mach-o-builder-header b)))
    (assert-equal expected-cputype (cl-cc/binary:mach-header-cputype hdr))))

(deftest macho-add-text-segment
  "add-text-segment adds one segment to the builder."
  (let* ((b (cl-cc/binary:make-mach-o-builder :x86-64))
         (code (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(#xC3 0 0 0))))
    (cl-cc/binary:add-text-segment b code)
    (assert-equal 1 (length (cl-cc/binary::mach-o-builder-segments b)))))

(deftest macho-add-symbol-behavior
  "add-symbol inserts into symbol-table; multiple calls accumulate."
  (let ((b (cl-cc/binary:make-mach-o-builder :x86-64)))
    (cl-cc/binary:add-symbol b "_main" :value 0 :sect 1)
    (assert-equal 1 (length (cl-cc/binary::mach-o-builder-symbol-table b))))
  (let ((b (cl-cc/binary:make-mach-o-builder :x86-64)))
    (cl-cc/binary:add-symbol b "_start" :value 0 :sect 1)
    (cl-cc/binary:add-symbol b "_exit"  :value 10 :sect 1)
    (assert-equal 2 (length (cl-cc/binary::mach-o-builder-symbol-table b)))))

(deftest macho-build-output-size-at-least-page
  "build-mach-o output is at least 4096 bytes (code is page-aligned)."
  (let ((b (cl-cc/binary:make-mach-o-builder :x86-64))
        (code (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(#xC3 0 0 0))))
    (cl-cc/binary:add-entry-point b 0)
    (let ((result (cl-cc/binary:build-mach-o b code)))
      (assert-true (>= (length result) 4096)))))
