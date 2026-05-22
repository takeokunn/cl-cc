;;;; packages/binary/tests/binary-wxorx-tests.lisp — FR-694 ELF W^X tests

(in-package :cl-cc/test)

(defsuite binary-wxorx-suite
  :description "FR-694 ELF W^X enforcement tests"
  :parent cl-cc-unit-suite)

(in-suite binary-wxorx-suite)

(defun %elf-u16le (bytes offset)
  (+ (aref bytes offset)
     (ash (aref bytes (+ offset 1)) 8)))

(defun %elf-u32le (bytes offset)
  (+ (aref bytes offset)
     (ash (aref bytes (+ offset 1)) 8)
     (ash (aref bytes (+ offset 2)) 16)
     (ash (aref bytes (+ offset 3)) 24)))

(defun %elf-u64le (bytes offset)
  (loop for i below 8
        sum (ash (aref bytes (+ offset i)) (* 8 i))))

(defun %elf-c-string (bytes offset)
  (with-output-to-string (out)
    (loop for i from offset below (length bytes)
          for byte = (aref bytes i)
          until (zerop byte)
          do (write-char (code-char byte) out))))

(defun %elf-section-flags-by-name (bytes)
  (let* ((shoff (%elf-u64le bytes 40))
         (shentsize (%elf-u16le bytes 58))
         (shnum (%elf-u16le bytes 60))
         (shstrndx (%elf-u16le bytes 62))
         (shstr-header (+ shoff (* shstrndx shentsize)))
         (shstr-offset (%elf-u64le bytes (+ shstr-header 24)))
         (result (make-hash-table :test #'equal)))
    (dotimes (i shnum result)
      (let* ((header (+ shoff (* i shentsize)))
             (name-offset (%elf-u32le bytes header))
             (name (%elf-c-string bytes (+ shstr-offset name-offset)))
             (flags (%elf-u64le bytes (+ header 8))))
        (setf (gethash name result) flags)))))

(deftest fr-694-elf-executable-has-non-executable-gnu-stack
  "FR-694: Executables include PT_GNU_STACK with PF_R|PF_W and no PF_X."
  (let* ((bytes (cl-cc/binary::compile-to-elf64-exec #(195) nil))
         (phoff (%elf-u64le bytes 32))
         (phentsize (%elf-u16le bytes 54))
         (phnum (%elf-u16le bytes 56))
         (gnu-stack-flags nil))
    (dotimes (i phnum)
      (let* ((header (+ phoff (* i phentsize)))
             (type (%elf-u32le bytes header))
             (flags (%elf-u32le bytes (+ header 4))))
        (when (= type cl-cc/binary::+pt-gnu-stack+)
          (setf gnu-stack-flags flags))))
    (assert-true gnu-stack-flags)
    (assert-equal (logior cl-cc/binary::+pf-r+ cl-cc/binary::+pf-w+)
                  gnu-stack-flags)
    (assert-equal 0 (logand gnu-stack-flags cl-cc/binary::+pf-x+))))

(deftest fr-694-elf-executable-section-flags-are-wxorx
  "FR-694: .text is RX, while .data/.bss are RW and never executable."
  (let* ((builder (cl-cc/binary::make-elf64-executable))
         (text-flags nil)
         (data-flags nil)
         (bss-flags nil))
    (cl-cc/binary::elf64-add-text-bytes builder #(195))
    (cl-cc/binary::elf64-add-data-bytes builder #(1 2 3 4))
    (cl-cc/binary::elf64-add-bss builder 16)
    (let ((flags (%elf-section-flags-by-name (cl-cc/binary::elf64-finalize builder))))
      (setf text-flags (gethash ".text" flags)
            data-flags (gethash ".data" flags)
            bss-flags (gethash ".bss" flags)))
    (assert-equal (logior cl-cc/binary::+shf-alloc+ cl-cc/binary::+shf-execinstr+)
                  text-flags)
    (assert-equal (logior cl-cc/binary::+shf-alloc+ cl-cc/binary::+shf-write+)
                  data-flags)
    (assert-equal (logior cl-cc/binary::+shf-alloc+ cl-cc/binary::+shf-write+)
                  bss-flags)
    (assert-equal 0 (logand text-flags cl-cc/binary::+shf-write+))
    (assert-equal 0 (logand data-flags cl-cc/binary::+shf-execinstr+))
    (assert-equal 0 (logand bss-flags cl-cc/binary::+shf-execinstr+))))
