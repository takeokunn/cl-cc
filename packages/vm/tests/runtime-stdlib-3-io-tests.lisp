;;;; runtime-stdlib-3-io-tests.lisp — FR-959/965/1051/1088/1100/1042

(in-package :cl-cc/test)

(defsuite runtime-stdlib-3-io-suite
  :description "Runtime stdlib-3 VM I/O, printing, terminal, and serialization tests"
  :parent cl-cc-unit-suite)

(in-suite runtime-stdlib-3-io-suite)

(deftest stdlib3-external-format-utf8-roundtrip-and-bom
  "vm-encode-string/vm-decode-bytes support UTF-8 and strip BOMs."
  (let* ((text "héλ")
         (bytes (cl-cc/vm:vm-encode-string text :utf-8))
         (bom-bytes (concatenate '(vector (unsigned-byte 8)) #(239 187 191) bytes)))
    (assert-true (typep bytes '(vector (unsigned-byte 8))))
    (assert-equal text (cl-cc/vm:vm-decode-bytes bytes :utf-8))
    (assert-equal text (cl-cc/vm:vm-decode-bytes bom-bytes :utf-8))))

(deftest stdlib3-external-format-ascii-error-modes
  "ASCII encoding supports :REPLACE and decoding supports :IGNORE."
  (assert-true (equalp #(65 63) (cl-cc/vm:vm-encode-string "Aé" :ascii :error-mode :replace)))
  (assert-equal "A" (cl-cc/vm:vm-decode-bytes #(65 255) :ascii :error-mode :ignore)))

(deftest stdlib3-stream-external-format-side-table
  "VM stream external format accessor/setter records canonical formats."
  (let ((stream (make-string-output-stream)))
    (assert-equal :utf-8 (cl-cc/vm:vm-stream-external-format stream))
    (assert-equal :utf-16le (cl-cc/vm:vm-set-stream-external-format stream :utf-16))
    (assert-equal :utf-16le (cl-cc/vm:vm-stream-external-format stream))))

(deftest stdlib3-format-p-and-w-directives
  "Native VM FORMAT implements ~P and ~W."
  (assert-equal "1 file" (cl-cc/vm::%vm-format-native "~D file~:P" '(1)))
  (assert-equal "2 files" (cl-cc/vm::%vm-format-native "~D file~:P" '(2)))
  (assert-equal "1 story" (cl-cc/vm::%vm-format-native "~D stor~:@P" '(1)))
  (assert-equal "2 stories" (cl-cc/vm::%vm-format-native "~D stor~:@P" '(2)))
  (assert-equal "(:A 1)" (cl-cc/vm::%vm-format-native "~W" '((:a 1)))))

(deftest stdlib3-print-control-variables
  "VM object writer honors print case/base/radix/gensym/array controls."
  (let ((cl-cc/vm:*print-case* :downcase)
        (cl-cc/vm:*print-base* 16)
        (cl-cc/vm:*print-radix* t)
        (cl-cc/vm:*print-array* nil))
    (assert-equal "foo" (cl-cc/vm::vm-write-object-to-string 'foo :escape t))
    (assert-equal "#x10" (string-downcase (cl-cc/vm::vm-write-object-to-string 16 :escape t)))
    (assert-true (search "#<" (cl-cc/vm::vm-write-object-to-string #(1 2) :escape t)))))

(deftest stdlib3-ryu-float-to-string
  "vm-float-to-string handles modes and special values."
  (assert-equal "1.5" (cl-cc/vm:vm-float-to-string 1.5d0))
  (assert-true (search "e" (cl-cc/vm:vm-float-to-string 1000.0d0 :mode :exponential)))
  (assert-equal "+inf.0" (cl-cc/vm:vm-float-to-string sb-ext:double-float-positive-infinity))
  (assert-equal "+nan.0" (cl-cc/vm:vm-float-to-string (sb-kernel:make-double-float #x7FF80000 0))))

(deftest stdlib3-terminal-ansi-and-size
  "Terminal helpers emit ANSI sequences and return plausible dimensions."
  (let ((out (make-string-output-stream)))
    (cl-cc/vm:vm-ansi-color out :red)
    (cl-cc/vm:vm-ansi-reset out)
    (assert-equal (format nil "~C[31m~C[0m" #\Esc #\Esc)
                  (get-output-stream-string out)))
  (multiple-value-bind (cols rows) (cl-cc/vm:vm-terminal-size)
    (assert-true (plusp cols))
    (assert-true (plusp rows))))

(deftest stdlib3-fasl-roundtrip
  "vm-write-to-fasl/vm-read-from-fasl round-trip readable objects."
  (let ((stream (make-string-output-stream))
        (object '(:answer 42 :items (a b c))))
    (cl-cc/vm:vm-write-to-fasl object stream)
    (assert-equal object
                  (cl-cc/vm:vm-read-from-fasl
                   (make-string-input-stream (get-output-stream-string stream))))))
