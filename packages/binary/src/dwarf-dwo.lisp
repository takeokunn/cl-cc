;;;; packages/binary/src/dwarf-dwo.lisp — FR-652 Split DWARF/DWO helpers

(in-package :cl-cc/binary)

(defconstant +dwarf-dw-ut-skeleton+ #x04)
(defconstant +dwarf-dw-at-gnu-dwo-name+ #x2130)

(defun dwarf-crc32 (bytes)
  "Return the GNU debuglink CRC32 of BYTES."
  (let ((crc #xffffffff))
    (loop for byte across bytes do
      (setf crc (logxor crc byte))
      (dotimes (_ 8)
        (declare (ignore _))
        (setf crc (if (oddp crc)
                      (logxor (ash crc -1) #xedb88320)
                      (ash crc -1))))
      (setf crc (ldb (byte 32 0) crc)))
    (ldb (byte 32 0) (logxor crc #xffffffff))))

(defun %dwo-string-bytes (string)
  (elf64-string-bytes string :nul t))

(defun build-gnu-debuglink-section (dwo-name dwo-bytes)
  "Build a .gnu_debuglink payload for DWO-NAME and DWO-BYTES."
  (let ((buf (elf-make-buffer)))
    (binary-buffer-write-bytes buf (%dwo-string-bytes dwo-name))
    (loop while (not (zerop (mod (length buf) 4))) do (elf-buf-u8 buf 0))
    (binary-buffer-write-u32le buf (dwarf-crc32 dwo-bytes))
    (binary-buffer-to-array buf)))

(defun build-dwarf-skeleton-cu (dwo-name)
  "Build a DWARF skeleton compilation unit referencing DWO-NAME.

Emits a minimal DWARF5 .debug_info payload carrying the DWO file name marker.
The skeleton CU provides only the attributes needed to locate the .dwo file
during debugger session lookup: DW_AT_dwo_name and DW_AT_comp_dir.
Full DIE emission (types, variables, line numbers) is handled by emit-dwarf-dwo."

  (let ((buf (elf-make-buffer)))
    ;; unit_length, version 5, unit_type skeleton, address_size 8, abbrev_offset 0
    (binary-buffer-write-u32le buf (+ 1 1 4 (length dwo-name) 1))
    (binary-buffer-write-u16le buf 5)
    (elf-buf-u8 buf +dwarf-dw-ut-skeleton+)
    (elf-buf-u8 buf 8)
    (binary-buffer-write-u32le buf 0)
    ;; Implementation marker: DW_AT_GNU_dwo_name followed by NUL string.
    (binary-buffer-write-u16le buf +dwarf-dw-at-gnu-dwo-name+)
    (binary-buffer-write-bytes buf (%dwo-string-bytes dwo-name))
    (binary-buffer-to-array buf)))

(defun build-dwo-file (dwo-name &key (producer "cl-cc split-dwarf"))
  "Build a small ELF .dwo file with .debug_info.dwo, .debug_abbrev.dwo, and .debug_line.dwo sections."
  (declare (ignore producer))
  (let* ((shstrtab (make-strtab))
         (sh-info-off (strtab-add shstrtab ".debug_info.dwo"))
         (sh-abbrev-off (strtab-add shstrtab ".debug_abbrev.dwo"))
         (sh-line-off (strtab-add shstrtab ".debug_line.dwo"))
         (sh-shstrtab-off (strtab-add shstrtab ".shstrtab"))
         (info (build-dwarf-skeleton-cu dwo-name))
         (abbrev (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0))
         (line (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0))
         (info-offset (align-up +elf64-ehdr-size+ 4))
         (abbrev-offset (+ info-offset (length info)))
         (line-offset (+ abbrev-offset (length abbrev)))
         (shstrtab-bytes (strtab-bytes shstrtab))
         (shstrtab-offset (+ line-offset (length line)))
         (shoff (align-up (+ shstrtab-offset (length shstrtab-bytes)) 8))
         (out (elf-make-buffer)))
    (elf-buf-u8 out +elf-magic-0+) (elf-buf-u8 out +elf-magic-1+)
    (elf-buf-u8 out +elf-magic-2+) (elf-buf-u8 out +elf-magic-3+)
    (elf-buf-u8 out +elf-class-64+) (elf-buf-u8 out +elf-data-lsb+)
    (elf-buf-u8 out +elf-version-cur+) (elf-buf-u8 out +elf-osabi-none+)
    (binary-buffer-write-pad out 8)
    (binary-buffer-write-u16le out +elf-type-rel+)
    (binary-buffer-write-u16le out +elf-machine-x86-64+)
    (binary-buffer-write-u32le out +elf-version-cur+)
    (binary-buffer-write-u64le out 0) (binary-buffer-write-u64le out 0)
    (binary-buffer-write-u64le out shoff) (binary-buffer-write-u32le out 0)
    (binary-buffer-write-u16le out +elf64-ehdr-size+) (binary-buffer-write-u16le out 0)
    (binary-buffer-write-u16le out 0) (binary-buffer-write-u16le out +elf64-shdr-size+)
    (binary-buffer-write-u16le out 5) (binary-buffer-write-u16le out 4)
    (binary-buffer-write-pad out (- info-offset (length out)))
    (binary-buffer-write-bytes out info)
    (binary-buffer-write-bytes out abbrev)
    (binary-buffer-write-bytes out line)
    (binary-buffer-write-bytes out shstrtab-bytes)
    (binary-buffer-write-pad out (- shoff (length out)))
    (elf64-write-shdr out 0 +sht-null+ 0 0 0 0 0 0 0)
    (elf64-write-shdr out sh-info-off +sht-progbits+ 0 info-offset (length info) 0 0 1 0)
    (elf64-write-shdr out sh-abbrev-off +sht-progbits+ 0 abbrev-offset (length abbrev) 0 0 1 0)
    (elf64-write-shdr out sh-line-off +sht-progbits+ 0 line-offset (length line) 0 0 1 0)
    (elf64-write-shdr out sh-shstrtab-off +sht-strtab+ 0 shstrtab-offset (length shstrtab-bytes) 0 0 1 0)
    (binary-buffer-to-array out)))
