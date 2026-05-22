;;;; packages/binary/src/macho-fat.lisp — FR-691 Mach-O universal/fat binaries

(in-package :cl-cc/binary)

(defconstant +fat-magic+ #xCAFEBABE
  "Mach-O FAT_MAGIC universal binary magic number (big-endian).")

(defconstant +fat-cputype-x86-64+ +cpu-type-x86-64+
  "Mach-O fat archive CPU type for x86-64 slices.")

(defconstant +fat-cputype-arm64+ +cpu-type-arm64+
  "Mach-O fat archive CPU type for arm64 slices.")

(defstruct mach-o-fat-slice
  "One architecture slice in a Mach-O universal/fat binary."
  (cputype +cpu-type-x86-64+ :type (unsigned-byte 32))
  (cpusubtype +cpu-subtype-x86-64-all+ :type (unsigned-byte 32))
  (align 14 :type (unsigned-byte 32))
  (bytes (make-array 0 :element-type '(unsigned-byte 8))
         :type (simple-array (unsigned-byte 8) (*))))

(defun %macho-write-u32-be (value stream)
  "Write VALUE as a big-endian uint32 to STREAM."
  (dotimes (shift-index 4)
    (let ((shift (* 8 (- 3 shift-index))))
      (write-byte (ldb (byte 8 shift) value) stream))))

(defun %macho-fat-align-up (value align-power)
  "Align VALUE to 2^ALIGN-POWER for Mach-O fat slice placement."
  (let ((alignment (ash 1 align-power)))
    (* alignment (ceiling value alignment))))

(defun %macho-fat-slice-vector (bytes)
  "Coerce BYTES to a simple unsigned-byte vector."
  (coerce bytes '(simple-array (unsigned-byte 8) (*))))

(defun build-mach-o-fat-binary (slices)
  "Build a Mach-O universal/fat binary containing SLICES.

SLICES is a list of MACH-O-FAT-SLICE objects.  The fat header and fat_arch table
are serialized big-endian as required by FAT_MAGIC; slice payload bytes are
copied verbatim at aligned offsets."
  (let* ((slice-count (length slices))
         (header-size (+ 8 (* slice-count 20)))
         (offsets nil)
         (cursor header-size))
    (dolist (slice slices)
      (setf cursor (%macho-fat-align-up cursor (mach-o-fat-slice-align slice)))
      (push cursor offsets)
      (incf cursor (length (mach-o-fat-slice-bytes slice))))
    (setf offsets (nreverse offsets))
    (let ((out (make-binary-buffer cursor)))
      (labels ((write-u32 (value)
                 (dotimes (shift-index 4)
                   (let ((shift (* 8 (- 3 shift-index))))
                     (binary-buffer-write-u8 out (ldb (byte 8 shift) value))))))
        (write-u32 +fat-magic+)
        (write-u32 slice-count)
      (loop for slice in slices
            for offset in offsets
              do (write-u32 (mach-o-fat-slice-cputype slice))
                 (write-u32 (mach-o-fat-slice-cpusubtype slice))
                 (write-u32 offset)
                 (write-u32 (length (mach-o-fat-slice-bytes slice)))
                 (write-u32 (mach-o-fat-slice-align slice)))
      (loop for slice in slices
            for offset in offsets
              do (loop while (< (length out) offset)
                       do (binary-buffer-write-u8 out 0))
                 (binary-buffer-write-bytes out
                                            (%macho-fat-slice-vector
                                             (mach-o-fat-slice-bytes slice))))
        (binary-buffer-to-array out)))))

(defun write-mach-o-fat-file (path slices)
  "Write a Mach-O universal/fat binary containing SLICES to PATH."
  (let ((bytes (build-mach-o-fat-binary slices)))
    (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
      (write-sequence bytes out))
    path))
