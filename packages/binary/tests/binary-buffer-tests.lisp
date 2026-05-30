;;;; packages/binary/tests/binary-buffer-tests.lisp — Binary buffer operation tests
;;;;
;;;; Tests for: binary buffer write/read, alignment, string conversion,
;;;; serialization primitives (macho-buffer.lisp).

(in-package :cl-cc/test)

(defsuite binary-buffer-suite
  :description "Binary buffer operations and serialization primitives"
  :parent cl-cc-unit-suite)

(in-suite binary-buffer-suite)

;;; ------------------------------------------------------------
;;; Binary Buffer — make-binary-buffer, write/read operations
;;; ------------------------------------------------------------

(deftest binary-buffer-create-empty-buffer
  "make-binary-buffer returns an adjustable buffer with fill-pointer 0."
  (let ((buf (cl-cc/binary::make-binary-buffer 256)))
    (assert-true (typep buf '(array (unsigned-byte 8) (*))))
    (assert-true (array-has-fill-pointer-p buf))
    (assert-equal 0 (length buf))))

(deftest binary-buffer-write-u8-roundtrips
  "Writing a u8 to a buffer appends the byte."
  (let ((buf (cl-cc/binary::make-binary-buffer 16)))
    (cl-cc/binary::binary-buffer-write-u8 buf #xAB)
    (assert-equal 1 (length buf))
    (assert-equal #xAB (aref buf 0))
    (cl-cc/binary::binary-buffer-write-u8 buf #x00)
    (cl-cc/binary::binary-buffer-write-u8 buf #xFF)
    (assert-equal 3 (length buf))
    (assert-equal #x00 (aref buf 1))
    (assert-equal #xFF (aref buf 2))))

(deftest binary-buffer-write-u16le-roundtrips
  "write-u16le appends two bytes in little-endian order."
  (let ((buf (cl-cc/binary::make-binary-buffer 16)))
    (cl-cc/binary::binary-buffer-write-u16le buf #xABCD)
    (assert-equal 2 (length buf))
    (assert-equal #xCD (aref buf 0))
    (assert-equal #xAB (aref buf 1))))

(deftest binary-buffer-write-u32le-roundtrips
  "write-u32le appends four bytes in little-endian order."
  (let ((buf (cl-cc/binary::make-binary-buffer 16)))
    (cl-cc/binary::binary-buffer-write-u32le buf #xDEADBEEF)
    (assert-equal 4 (length buf))
    (assert-equal #xEF (aref buf 0))
    (assert-equal #xBE (aref buf 1))
    (assert-equal #xAD (aref buf 2))
    (assert-equal #xDE (aref buf 3))))

(deftest binary-buffer-write-u64le-roundtrips
  "write-u64le appends eight bytes in little-endian order."
  (let ((buf (cl-cc/binary::make-binary-buffer 16)))
    (cl-cc/binary::binary-buffer-write-u64le buf #x0123456789ABCDEF)
    (assert-equal 8 (length buf))
    (assert-equal #xEF (aref buf 0))
    (assert-equal #xCD (aref buf 1))
    (assert-equal #xAB (aref buf 2))
    (assert-equal #x89 (aref buf 3))
    (assert-equal #x67 (aref buf 4))
    (assert-equal #x45 (aref buf 5))
    (assert-equal #x23 (aref buf 6))
    (assert-equal #x01 (aref buf 7))))

(deftest binary-buffer-write-pad-zero-fills
  "write-pad fills with N zero bytes."
  (let ((buf (cl-cc/binary::make-binary-buffer 16))
        (pad-count 5))
    (cl-cc/binary::binary-buffer-write-pad buf pad-count)
    (assert-equal pad-count (length buf))
    (dotimes (i pad-count)
      (assert-equal 0 (aref buf i)))))

(deftest binary-buffer-write-bytes-vector
  "write-bytes from a vector copies all elements."
  (let ((buf (cl-cc/binary::make-binary-buffer 16))
        (data #(1 2 3 4 5)))
    (cl-cc/binary::binary-buffer-write-bytes buf data)
    (assert-equal 5 (length buf))
    (dotimes (i 5)
      (assert-equal (aref data i) (aref buf i)))))

(deftest binary-buffer-write-bytes-list
  "write-bytes from a list copies all elements."
  (let ((buf (cl-cc/binary::make-binary-buffer 16))
        (data '(10 20 30)))
    (cl-cc/binary::binary-buffer-write-bytes buf data)
    (assert-equal 3 (length buf))
    (assert-equal 10 (aref buf 0))
    (assert-equal 20 (aref buf 1))
    (assert-equal 30 (aref buf 2))))

(deftest binary-buffer-to-array-preserves-contents
  "binary-buffer-to-array returns a copy with same contents."
  (let ((buf (cl-cc/binary::make-binary-buffer 16)))
    (cl-cc/binary::binary-buffer-write-u8 buf 42)
    (cl-cc/binary::binary-buffer-write-u8 buf 99)
    (let ((copy (cl-cc/binary::binary-buffer-to-array buf)))
      (assert-equal 2 (length copy))
      (assert-equal 42 (aref copy 0))
      (assert-equal 99 (aref copy 1)))))

;;; ------------------------------------------------------------
;;; byte-buffer — make-byte-buffer, buffer-write-byte
;;; ------------------------------------------------------------

(deftest byte-buffer-create-and-write
  "make-byte-buffer creates an empty byte-buffer; buffer-write-byte appends."
  (let ((bb (cl-cc/binary::make-byte-buffer 64)))
    (cl-cc/binary::buffer-write-byte bb #x7F)
    (let ((data (cl-cc/binary::buffer-get-bytes bb)))
      (assert-equal 1 (length data))
      (assert-equal #x7F (aref data 0)))))

(deftest byte-buffer-multiple-writes
  "Multiple buffer-write-byte calls append sequentially."
  (let ((bb (cl-cc/binary::make-byte-buffer 64)))
    (dotimes (i 10)
      (cl-cc/binary::buffer-write-byte bb i))
    (let ((data (cl-cc/binary::buffer-get-bytes bb)))
      (assert-equal 10 (length data))
      (dotimes (i 10)
        (assert-equal i (aref data i))))))

(deftest byte-buffer-write-bytes-via-class
  "buffer-write-bytes via CLOS byte-buffer writes correctly."
  (let ((bb (cl-cc/binary::make-byte-buffer 64)))
    (cl-cc/binary::buffer-write-bytes bb #(100 200 255))
    (let ((data (cl-cc/binary::buffer-get-bytes bb)))
      (assert-equal 3 (length data))
      (assert-equal 100 (aref data 0))
      (assert-equal 200 (aref data 1))
      (assert-equal 255 (aref data 2)))))

;;; ------------------------------------------------------------
;;; Utilities — align-up, string-to-ascii-bytes
;;; ------------------------------------------------------------

(deftest-each align-up-cases
  "align-up rounds VALUE up to the nearest multiple of ALIGNMENT."
  :cases (("already-aligned-16-by-8"   16   8   16)
          ("already-aligned-1024"      1024 256 1024)
          ("9-rounds-to-16"            9    8   16)
          ("15-rounds-to-16"           15   8   16)
          ("129-rounds-to-256"         129  128 256)
          ("zero-stays-zero"           0    8   0)
          ("alignment-one-passthrough" 42   1   42))
  (value alignment expected)
  (assert-equal expected (cl-cc/binary::align-up value alignment)))

(deftest string-to-ascii-bytes-converts-correctly
  "string-to-ascii-bytes converts each character to its ASCII code."
  (let ((bytes (cl-cc/binary::string-to-ascii-bytes "ABC")))
    (assert-equal 3 (length bytes))
    (assert-equal (char-code #\A) (aref bytes 0))
    (assert-equal (char-code #\B) (aref bytes 1))
    (assert-equal (char-code #\C) (aref bytes 2))))

(deftest string-to-ascii-bytes-empty-string
  "string-to-ascii-bytes on empty string returns empty vector."
  (let ((bytes (cl-cc/binary::string-to-ascii-bytes "")))
    (assert-equal 0 (length bytes))))

;;; ------------------------------------------------------------
;;; Serialization — serialize-uint32-le, serialize-uint64-le
;;; ------------------------------------------------------------

(deftest serialize-uint32-le-outputs-little-endian
  "serialize-uint32-le writes 4 bytes in little-endian order."
  (let ((bb (cl-cc/binary::make-byte-buffer 16)))
    (cl-cc/binary::serialize-uint32-le #x12345678 bb)
    (let ((data (cl-cc/binary::buffer-get-bytes bb)))
      (assert-equal 4 (length data))
      (assert-equal #x78 (aref data 0))
      (assert-equal #x56 (aref data 1))
      (assert-equal #x34 (aref data 2))
      (assert-equal #x12 (aref data 3)))))

(deftest serialize-uint64-le-outputs-little-endian
  "serialize-uint64-le writes 8 bytes in little-endian order."
  (let ((bb (cl-cc/binary::make-byte-buffer 16)))
    (cl-cc/binary::serialize-uint64-le #xAABBCCDD00112233 bb)
    (let ((data (cl-cc/binary::buffer-get-bytes bb)))
      (assert-equal 8 (length data))
      (assert-equal #x33 (aref data 0))
      (assert-equal #x22 (aref data 1))
      (assert-equal #x11 (aref data 2))
      (assert-equal #x00 (aref data 3))
      (assert-equal #xDD (aref data 4))
      (assert-equal #xCC (aref data 5))
      (assert-equal #xBB (aref data 6))
      (assert-equal #xAA (aref data 7)))))

(deftest serialize-bytes-writes-all
  "serialize-bytes writes all bytes from a simple-array."
  (let ((bb (cl-cc/binary::make-byte-buffer 16))
        (input (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 3 5 7))))
    (cl-cc/binary::serialize-bytes input bb)
    (let ((data (cl-cc/binary::buffer-get-bytes bb)))
      (assert-equal 4 (length data))
      (dotimes (i 4)
        (assert-equal (aref input i) (aref data i))))))

(deftest serialize-string-16-pads-to-16
  "serialize-string-16 writes exactly 16 bytes, null-padding shorter strings."
  (let ((bb (cl-cc/binary::make-byte-buffer 32)))
    (cl-cc/binary::serialize-string-16 "HI" bb)
    (let ((data (cl-cc/binary::buffer-get-bytes bb)))
      (assert-equal 16 (length data))
      (assert-equal (char-code #\H) (aref data 0))
      (assert-equal (char-code #\I) (aref data 1))
      (dotimes (i 14)
        (assert-equal 0 (aref data (+ i 2)))))))
