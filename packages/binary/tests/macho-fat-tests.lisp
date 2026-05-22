;;;; packages/binary/tests/macho-fat-tests.lisp

(in-package :cl-cc/test)

(defsuite macho-fat-suite
  :description "FR-691 Mach-O universal/fat binary tests"
  :parent cl-cc-unit-suite)

(in-suite macho-fat-suite)

(deftest fr-691-mach-o-universal-binary-fat-magic
  "FR-691: Mach-O universal binary writer emits FAT_MAGIC and slice table."
  (let* ((x86 (cl-cc/binary:make-mach-o-fat-slice
               :cputype cl-cc/binary:+fat-cputype-x86-64+
               :cpusubtype cl-cc/binary:+cpu-subtype-x86-64-all+
               :align 2
               :bytes (make-array 4 :element-type '(unsigned-byte 8)
                                     :initial-contents '(1 2 3 4))))
         (arm (cl-cc/binary:make-mach-o-fat-slice
               :cputype cl-cc/binary:+fat-cputype-arm64+
               :cpusubtype cl-cc/binary:+cpu-subtype-arm64-all+
               :align 2
               :bytes (make-array 4 :element-type '(unsigned-byte 8)
                                     :initial-contents '(5 6 7 8))))
         (bytes (cl-cc/binary:build-mach-o-fat-binary (list x86 arm))))
    (assert-equal '(#xCA #xFE #xBA #xBE) (subseq (coerce bytes 'list) 0 4))
    (assert-equal 2 (elt bytes 7))
    (assert-true (> (length bytes) 48))))
