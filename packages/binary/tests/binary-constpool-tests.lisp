;;;; packages/binary/tests/binary-constpool-tests.lisp — FR-724 constant pool tests

(in-package :cl-cc/test)

(defsuite binary-constpool-suite
  :description "FR-724 constant pool and literal deduplication tests"
  :parent cl-cc-unit-suite)

(in-suite binary-constpool-suite)

(defun %constpool-ub8 (bytes)
  (make-array (length bytes)
              :element-type '(unsigned-byte 8)
              :initial-contents bytes))

(deftest fr-724-elf-rodata-deduplicates-identical-byte-sequences
  "FR-724: Identical .rodata constants return one shared section offset."
  (let* ((builder (cl-cc/binary::make-elf64-object))
         (first (cl-cc/binary:elf64-add-rodata-bytes builder (%constpool-ub8 '(1 2 3 4))))
         (second (cl-cc/binary:elf64-add-rodata-bytes builder (%constpool-ub8 '(9 8))))
         (third (cl-cc/binary:elf64-add-rodata-bytes builder (%constpool-ub8 '(1 2 3 4)))))
    (assert-equal first third)
    (assert-equal 0 first)
    (assert-equal 4 second)
    (assert-equal 6 (length (cl-cc/binary::elf64-rodata-buf builder)))))

(deftest fr-724-elf-rodata-string-deduplicates-and-sets-merge-flags
  "FR-724: String literals are deduped into SHF_MERGE|SHF_STRINGS .rodata.str."
  (let* ((builder (cl-cc/binary::make-elf64-executable))
         (first (cl-cc/binary:elf64-add-rodata-string builder "hello"))
         (second (cl-cc/binary:elf64-add-rodata-string builder "world"))
         (third (cl-cc/binary:elf64-add-rodata-string builder "hello")))
    (cl-cc/binary::elf64-add-text-bytes builder #(195))
    (assert-equal first third)
    (assert-equal 0 first)
    (assert-equal 6 second)
    (let* ((flags (%elf-section-flags-by-name (cl-cc/binary::elf64-finalize builder)))
           (string-flags (gethash ".rodata.str" flags)))
      (assert-equal (logior cl-cc/binary::+shf-alloc+
                            cl-cc/binary::+shf-merge+
                            cl-cc/binary::+shf-strings+)
                    string-flags)
      (assert-equal 0 (logand string-flags cl-cc/binary::+shf-write+))
      (assert-equal 0 (logand string-flags cl-cc/binary::+shf-execinstr+)))))

(deftest fr-724-macho-data-const-segment-deduplicates-payloads
  "FR-724: Mach-O __DATA_CONST keeps one copy for repeated immutable payloads."
  (let ((builder (cl-cc/binary:make-mach-o-builder :x86-64)))
    (cl-cc/binary:add-data-const-segment builder (%constpool-ub8 '(10 20 30)))
    (cl-cc/binary:add-data-const-segment builder (%constpool-ub8 '(1 2)))
    (cl-cc/binary:add-data-const-segment builder (%constpool-ub8 '(10 20 30)))
    (let* ((segment (find "__DATA_CONST" (cl-cc/binary::mach-o-builder-segments builder)
                          :key #'cl-cc/binary:segment-command-segname
                          :test #'string=))
           (payload (cl-cc/binary::segment-command-payload segment)))
      (assert-true segment)
      (assert-equal 5 (length payload))
      (assert-equal 10 (aref payload 0))
      (assert-equal 20 (aref payload 1))
      (assert-equal 30 (aref payload 2))
      (assert-equal 1 (aref payload 3))
      (assert-equal 2 (aref payload 4)))))
