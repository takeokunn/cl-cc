;;;; packages/binary/tests/patchable-entry-tests.lisp — Patchable entry tests
;;;;
;;;; Tests for: emit-nop-sequence, emit-patchable-function-entry,
;;;; and with-patchable-entries (patchable-entry.lisp / FR-584).

(in-package :cl-cc/test)

(defsuite patchable-entry-suite
  :description "Patchable function entry NOP emission tests (FR-584)"
  :parent cl-cc-unit-suite)

(in-suite patchable-entry-suite)

;;; ------------------------------------------------------------
;;; Helper
;;; ------------------------------------------------------------

(defun %nop-bytes (count)
  "Return emitted NOP bytes for COUNT as a list."
  (coerce (cl-cc/binary::with-output-to-vector (stream)
            (cl-cc/binary::emit-nop-sequence stream count))
          'list))

;;; ------------------------------------------------------------
;;; emit-nop-sequence — correct byte count
;;; ------------------------------------------------------------

(deftest-each emit-nop-sequence-correct-total-length
  "emit-nop-sequence emits exactly COUNT bytes total."
  :cases (("count-1"  1)
          ("count-2"  2)
          ("count-3"  3)
          ("count-4"  4)
          ("count-5"  5)
          ("count-6"  6)
          ("count-7"  7)
          ("count-8"  8)
          ("count-9"  9)
          ("count-10" 10)
          ("count-18" 18))
  (count)
  (assert-equal count (length (%nop-bytes count))))

;;; ------------------------------------------------------------
;;; emit-nop-sequence — correct encoding for each single NOP
;;; ------------------------------------------------------------

(deftest-each emit-nop-sequence-exact-bytes
  "emit-nop-sequence emits the correct encoding bytes for each single-NOP count."
  :cases (("1" 1 '(#x90))
          ("2" 2 '(#x66 #x90))
          ("3" 3 '(#x0F #x1F #x00))
          ("4" 4 '(#x0F #x1F #x40 #x00))
          ("5" 5 '(#x0F #x1F #x44 #x00 #x00))
          ("6" 6 '(#x66 #x0F #x1F #x44 #x00 #x00))
          ("7" 7 '(#x0F #x1F #x80 #x00 #x00 #x00 #x00))
          ("8" 8 '(#x0F #x1F #x84 #x00 #x00 #x00 #x00 #x00))
          ("9" 9 '(#x66 #x0F #x1F #x84 #x00 #x00 #x00 #x00 #x00)))
  (count expected)
  (assert-equal expected (%nop-bytes count)))

;;; ------------------------------------------------------------
;;; emit-nop-sequence — multi-chunk decomposition
;;; ------------------------------------------------------------

(deftest emit-nop-sequence-10-decomposes-9-plus-1
  "emit-nop-sequence for count=10 emits 9-byte NOP then 1-byte NOP."
  (let ((bytes (%nop-bytes 10)))
    (assert-equal 10 (length bytes))
    ;; First 9 bytes: the 9-byte NOP prefix
    (assert-equal '(#x66 #x0F #x1F #x84 #x00 #x00 #x00 #x00 #x00)
                  (subseq bytes 0 9))
    ;; Last byte: 1-byte NOP
    (assert-equal '(#x90) (subseq bytes 9 10))))

(deftest emit-nop-sequence-0-emits-nothing
  "emit-nop-sequence for count=0 emits no bytes."
  (assert-equal '() (%nop-bytes 0)))

;;; ------------------------------------------------------------
;;; emit-patchable-function-entry — non-zero before/after
;;; ------------------------------------------------------------

(deftest emit-patchable-function-entry-before-only
  "emit-patchable-function-entry with before=3 emits 3 NOP bytes."
  (let ((bytes (coerce
                (cl-cc/binary::with-output-to-vector (stream)
                  (let ((cl-cc/binary::*patchable-entry-before* 3)
                        (cl-cc/binary::*patchable-entry-after* 0))
                    (cl-cc/binary::emit-patchable-function-entry stream)))
                'list)))
    (assert-equal 3 (length bytes))
    (assert-equal '(#x0F #x1F #x00) bytes)))

(deftest emit-patchable-function-entry-after-only
  "emit-patchable-function-entry with after=2 emits 2 NOP bytes."
  (let ((bytes (coerce
                (cl-cc/binary::with-output-to-vector (stream)
                  (let ((cl-cc/binary::*patchable-entry-before* 0)
                        (cl-cc/binary::*patchable-entry-after* 2))
                    (cl-cc/binary::emit-patchable-function-entry stream)))
                'list)))
    (assert-equal 2 (length bytes))
    (assert-equal '(#x66 #x90) bytes)))

(deftest emit-patchable-function-entry-before-and-after
  "emit-patchable-function-entry with before=5 after=3 emits 8 NOP bytes total."
  (let ((bytes (coerce
                (cl-cc/binary::with-output-to-vector (stream)
                  (let ((cl-cc/binary::*patchable-entry-before* 5)
                        (cl-cc/binary::*patchable-entry-after* 3))
                    (cl-cc/binary::emit-patchable-function-entry stream)))
                'list)))
    (assert-equal 8 (length bytes))
    ;; First 5 bytes: 5-byte NOP
    (assert-equal '(#x0F #x1F #x44 #x00 #x00) (subseq bytes 0 5))
    ;; Next 3 bytes: 3-byte NOP
    (assert-equal '(#x0F #x1F #x00) (subseq bytes 5 8))))

(deftest emit-patchable-function-entry-both-zero
  "emit-patchable-function-entry with before=0 after=0 emits nothing."
  (let ((bytes (coerce
                (cl-cc/binary::with-output-to-vector (stream)
                  (let ((cl-cc/binary::*patchable-entry-before* 0)
                        (cl-cc/binary::*patchable-entry-after* 0))
                    (cl-cc/binary::emit-patchable-function-entry stream)))
                'list)))
    (assert-equal '() bytes)))

;;; ------------------------------------------------------------
;;; with-patchable-entries — dynamic binding
;;; ------------------------------------------------------------

(deftest with-patchable-entries-binds-before-and-after
  "with-patchable-entries correctly binds *patchable-entry-before* and *patchable-entry-after*."
  (cl-cc/binary::with-patchable-entries (:before 4 :after 2)
    (assert-equal 4 cl-cc/binary::*patchable-entry-before*)
    (assert-equal 2 cl-cc/binary::*patchable-entry-after*)))

(deftest with-patchable-entries-restores-after-body
  "with-patchable-entries restores original variable values after body."
  (let ((before-saved cl-cc/binary::*patchable-entry-before*)
        (after-saved  cl-cc/binary::*patchable-entry-after*))
    (cl-cc/binary::with-patchable-entries (:before 7 :after 5)
      (assert-equal 7 cl-cc/binary::*patchable-entry-before*))
    (assert-equal before-saved cl-cc/binary::*patchable-entry-before*)
    (assert-equal after-saved  cl-cc/binary::*patchable-entry-after*)))

;;; ------------------------------------------------------------
;;; patch-function-entry — size guard
;;; ------------------------------------------------------------

(deftest patch-function-entry-rejects-oversized-patch
  "patch-function-entry signals an error when patch is larger than reserved bytes."
  (let ((cl-cc/binary::*patchable-entry-before* 2))
    (assert-signals error
      (cl-cc/binary::patch-function-entry 0 #(#x90 #x90 #x90)))))
