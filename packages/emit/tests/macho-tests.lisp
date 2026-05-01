;;;; tests/unit/emit/macho-tests.lisp — Mach-O Binary Format Tests
;;;;
;;;; Tests for src/emit/binary/macho.lisp:
;;;; Constants, structures, buffer helpers, serialization, builder API.

(in-package :cl-cc/test)

(defsuite macho-suite :description "Mach-O binary format tests"
  :parent cl-cc-unit-suite)


(in-suite macho-suite)
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

