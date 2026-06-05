;;;; wasm-binary-tests.lisp — Tests for wasm-binary, wasm-binary-aot, wasm-binary-debug
;;;
;;; Covers: LEB128 encoding, value-type table, name writing, export name handling,
;;; local decls, function body bytes, base64 encoding, hex-to-bytes round-trip,
;;; debug name cleaning, human local names, WAT/JSON string escaping, and SRI hash
;;; structure. All functions exercised live in the cl-cc/codegen package.

(in-package :cl-cc/test)

(defsuite wasm-binary-suite
  :description "Unit tests for wasm-binary, wasm-binary-aot, wasm-binary-debug"
  :parent cl-cc-unit-suite)

(in-suite wasm-binary-suite)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %make-test-buffer ()
  "Return a fresh byte buffer for testing."
  (cl-cc/binary::make-byte-buffer 64))

(defun %buffer-bytes (buf)
  "Extract bytes from BUF as a list."
  (coerce (cl-cc/binary::buffer-get-bytes buf) 'list))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 1: wasm-binary-value-type-byte — all 9 known types + error path
;;; ─────────────────────────────────────────────────────────────────────────────

(deftest-each wasm-binary-value-type-byte-known
  "wasm-binary-value-type-byte returns the correct encoding byte for every known type keyword."
  :cases (("i32"       :i32       cl-cc/codegen::+wasm-i32+)
          ("i64"       :i64       cl-cc/codegen::+wasm-i64+)
          ("f32"       :f32       cl-cc/codegen::+wasm-f32+)
          ("f64"       :f64       cl-cc/codegen::+wasm-f64+)
          ("f16"       :f16       cl-cc/codegen::+wasm-f16+)
          ("funcref"   :funcref   cl-cc/codegen::+wasm-funcref+)
          ("externref" :externref cl-cc/codegen::+wasm-externref+)
          ("stringref" :stringref cl-cc/codegen::+wasm-stringref+)
          ("eqref"     :eqref     cl-cc/codegen::+wasm-eqref+))
  (keyword expected-byte)
  (assert-equal expected-byte
                (cl-cc/codegen::wasm-binary-value-type-byte keyword)))

(deftest wasm-binary-value-type-byte-unknown-signals-error
  "wasm-binary-value-type-byte signals an error for an unrecognized type keyword."
  (assert-signals error
                  (cl-cc/codegen::wasm-binary-value-type-byte :not-a-type)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 2: wasm-binary-write-name — byte-length prefix + content bytes
;;; ─────────────────────────────────────────────────────────────────────────────

(deftest wasm-binary-write-name-layout
  "wasm-binary-write-name emits a LEB128 length followed by the UTF-8 bytes of the name."
  (let ((buf (%make-test-buffer)))
    (cl-cc/codegen::wasm-binary-write-name buf "abc")
    (assert-equal '(3 97 98 99) (%buffer-bytes buf))))

(deftest wasm-binary-write-name-empty
  "wasm-binary-write-name on an empty string emits a single zero byte (length = 0)."
  (let ((buf (%make-test-buffer)))
    (cl-cc/codegen::wasm-binary-write-name buf "")
    (assert-equal '(0) (%buffer-bytes buf))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 3: wasm-binary-export-name-for-function — $-prefix stripping / gating
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %make-test-wasm-func (&key exported-p export-name wat-name index)
  "Construct a minimal wasm-func for export-name tests."
  (let ((f (cl-cc/codegen::make-wasm-func)))
    (setf (cl-cc/codegen::wasm-func-exported-p f) exported-p
          (cl-cc/codegen::wasm-func-export-name f) export-name
          (cl-cc/codegen::wasm-func-wat-name f) wat-name
          (cl-cc/codegen::wasm-func-index f) index)
    f))

(deftest wasm-binary-export-name-strips-dollar-prefix
  "When export-name is NIL, the $-prefix is stripped from wat-name to form the export name."
  (let ((func (%make-test-wasm-func :exported-p t :export-name nil
                                     :wat-name "$my_func" :index 0)))
    (assert-equal "my_func"
                  (cl-cc/codegen::wasm-binary-export-name-for-function func))))

(deftest wasm-binary-export-name-uses-explicit-export-name
  "When export-name is set, it is returned directly without $-stripping."
  (let ((func (%make-test-wasm-func :exported-p t :export-name "main"
                                     :wat-name "$some_internal" :index 0)))
    (assert-equal "main"
                  (cl-cc/codegen::wasm-binary-export-name-for-function func))))

(deftest wasm-binary-export-name-nil-when-not-exported
  "When exported-p is NIL, wasm-binary-export-name-for-function returns NIL."
  (let ((func (%make-test-wasm-func :exported-p nil :export-name nil
                                     :wat-name "$hidden" :index 0)))
    (assert-equal nil
                  (cl-cc/codegen::wasm-binary-export-name-for-function func))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 4: wasm-binary-write-local-decls — count + type pairs
;;; ─────────────────────────────────────────────────────────────────────────────

(deftest wasm-binary-write-local-decls-empty
  "Writing zero local groups emits a single zero byte (LEB128 count = 0)."
  (let ((buf (%make-test-buffer)))
    (cl-cc/codegen::wasm-binary-write-local-decls buf nil)
    (assert-equal '(0) (%buffer-bytes buf))))

(deftest wasm-binary-write-local-decls-two-groups
  "Writing two local groups emits count then (count type) pairs in binary."
  (let ((buf (%make-test-buffer)))
    ;; 2 groups: (3 :i32) and (1 :i64)
    (cl-cc/codegen::wasm-binary-write-local-decls buf (list (list 3 :i32) (list 1 :i64)))
    (let ((bytes (%buffer-bytes buf)))
      ;; First byte: 2 groups.
      (assert-equal 2 (first bytes))
      ;; Group 0: count=3 + type-byte for :i32.
      (assert-equal 3 (second bytes))
      (assert-equal cl-cc/codegen::+wasm-i32+ (third bytes))
      ;; Group 1: count=1 + type-byte for :i64.
      (assert-equal 1 (fourth bytes))
      (assert-equal cl-cc/codegen::+wasm-i64+ (fifth bytes)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 5: wasm-binary-function-body-bytes — stack-neutral empty body
;;; ─────────────────────────────────────────────────────────────────────────────

(deftest wasm-binary-function-body-bytes-minimal
  "An empty function body consists of local-count=0 followed by the end opcode."
  (let* ((dummy-func (cl-cc/codegen::make-wasm-func))
         (body (cl-cc/codegen::wasm-binary-function-body-bytes dummy-func))
         (body-list (coerce body 'list)))
    (assert-type (simple-array (unsigned-byte 8) (*)) body)
    ;; Minimal body: 1 byte for local-count (0) + 1 byte for end (0x0b).
    (assert-equal 2 (length body-list))
    (assert-equal 0 (first body-list))
    (assert-equal cl-cc/codegen::+wasm-end+ (second body-list))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 6: %wasm-base64-encode — RFC 4648 vectors
;;; ─────────────────────────────────────────────────────────────────────────────

(deftest-each wasm-base64-encode-rfc4648-vectors
  "%wasm-base64-encode matches RFC 4648 base64 test vectors."
  :cases (("empty"       #()                  "")
          ("one-byte"    #(77)                 "TQ==")
          ("two-bytes"   #(77 97)              "TWE=")
          ("three-bytes" #(77 97 110)          "TWFu")
          ("four-bytes"  #(77 97 110 32)       "TWFuIA==")
          ("hello"       #(104 101 108 108 111) "aGVsbG8="))
  (bytes expected)
  (assert-equal expected
                (cl-cc/codegen::%wasm-base64-encode (coerce bytes '(vector (unsigned-byte 8))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 7: %wasm-hex-to-bytes — round-trip
;;; ─────────────────────────────────────────────────────────────────────────────

(deftest wasm-hex-to-bytes-round-trip
  "%wasm-hex-to-bytes decodes a known hex string to the expected byte vector."
  (let ((bytes (cl-cc/codegen::%wasm-hex-to-bytes "deadbeef")))
    (assert-equal '(#xde #xad #xbe #xef)
                  (coerce bytes 'list))))

(deftest wasm-hex-to-bytes-with-spaces-and-colons
  "%wasm-hex-to-bytes strips non-alphanumeric characters before decoding."
  (let ((bytes (cl-cc/codegen::%wasm-hex-to-bytes "de:ad be:ef")))
    (assert-equal '(#xde #xad #xbe #xef)
                  (coerce bytes 'list))))

(deftest wasm-hex-to-bytes-empty
  "%wasm-hex-to-bytes on empty input returns an empty byte vector."
  (let ((bytes (cl-cc/codegen::%wasm-hex-to-bytes "")))
    (assert-equal 0 (length bytes))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 8: %wasm-clean-debug-name — $-stripping and nil input
;;; ─────────────────────────────────────────────────────────────────────────────

(deftest-each wasm-clean-debug-name-cases
  "%wasm-clean-debug-name strips leading $ and handles NIL gracefully."
  :cases (("dollar-prefix"   "$my_func"   "my_func")
          ("no-dollar"       "plain"      "plain")
          ("double-dollar"   "$$weird"    "$weird")
          ("empty-string"    ""           "")
          ("nil-input"       nil          "anonymous"))
  (input expected)
  (assert-equal expected
                (cl-cc/codegen::%wasm-clean-debug-name input)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 9: %wasm-human-local-name — :R0 temp-result, :R1 temp-r1, non-numeric raw
;;; ─────────────────────────────────────────────────────────────────────────────

(deftest-each wasm-human-local-name-cases
  "%wasm-human-local-name maps VM register keywords to DevTools-friendly local names."
  :cases (("r0"           :R0   "temp-result")
          ("r1"           :R1   "temp-r1")
          ("r5"           :R5   "temp-r5")
          ("r10"          :R10  "temp-r10")
          ("non-numeric"  :RAX  "rax"))
  (reg expected)
  (assert-equal expected
                (cl-cc/codegen::%wasm-human-local-name reg)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 10: %wasm-wat-string — quote/backslash/newline escaping
;;; ─────────────────────────────────────────────────────────────────────────────

(deftest-each wasm-wat-string-escaping
  "%wasm-wat-string encodes special characters using WAT hex escape sequences."
  :cases (("plain"      "hello"         "\"hello\"")
          ("double-q"   "say \"hi\""    "\"say \\\"hi\\\"\"")
          ("backslash"  "a\\b"          "\"a\\\\b\"")
          ("newline"    (format nil "x~%y") "\"x\\0ay\"")
          ("return"     (format nil "x~Cy") "\"x\\0dy\"")
          ("tab"        (format nil "x~Cy" #\Tab) nil))  ; tab case handled below
  (input expected)
  (when expected
    (assert-equal expected
                  (cl-cc/codegen::%wasm-wat-string input))))

(deftest wasm-wat-string-tab-escape
  "%wasm-wat-string encodes tab as the WAT hex sequence \\09."
  (let ((result (cl-cc/codegen::%wasm-wat-string (string #\Tab))))
    (assert-equal "\"\\09\"" result)))

(deftest wasm-wat-string-nil-input
  "%wasm-wat-string treats NIL the same as empty string."
  (assert-equal "\"\"" (cl-cc/codegen::%wasm-wat-string nil)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 11: %wasm-json-string — \n vs \0a difference from wat
;;; ─────────────────────────────────────────────────────────────────────────────

(deftest wasm-json-string-newline-uses-backslash-n
  "%wasm-json-string encodes newline as \\n (JSON style), not \\0a (WAT style)."
  (let ((result (cl-cc/codegen::%wasm-json-string (format nil "line1~%line2"))))
    (assert-true (search "\\n" result :test #'char=))
    (assert-equal nil (search "\\0a" result :test #'char=))))

(deftest wasm-json-string-tab-uses-backslash-t
  "%wasm-json-string encodes tab as \\t, not \\09."
  (let ((result (cl-cc/codegen::%wasm-json-string (string #\Tab))))
    (assert-equal "\"\\t\"" result)))

(deftest wasm-json-string-double-quote-escaping
  "%wasm-json-string escapes double-quote as \\\"."
  (let ((result (cl-cc/codegen::%wasm-json-string "say \"hello\"")))
    (assert-true (search "\\\"" result :test #'char=))))

(deftest wasm-json-string-vs-wat-string-newline-differ
  "WAT and JSON string escaping produce different results for newline characters."
  (let* ((input (format nil "~%"))
         (wat (cl-cc/codegen::%wasm-wat-string input))
         (json (cl-cc/codegen::%wasm-json-string input)))
    (assert-equal "\"\\0a\"" wat)
    (assert-equal "\"\\n\"" json)
    (assert-false (string= wat json))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 12: wasm-file-sri-hash — sha384- prefix and base64 body structure
;;; ─────────────────────────────────────────────────────────────────────────────

(deftest wasm-file-sri-hash-structure
  "wasm-file-sri-hash returns a string with sha384- prefix followed by a base64 body."
  ;; Write a tiny file, compute its SRI hash, and verify the structure.
  (let* ((tmp (merge-pathnames (make-pathname :name (format nil "cl-cc-test-sri-~A" (gensym))
                                              :type "bin")
                               (uiop:temporary-directory)))
         (content (make-array 8 :element-type '(unsigned-byte 8)
                                :initial-contents '(1 2 3 4 5 6 7 8))))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp)
           (with-open-file (out tmp :direction :output :if-exists :supersede
                                    :if-does-not-exist :create
                                    :element-type '(unsigned-byte 8))
             (write-sequence content out))
           (let ((sri (cl-cc/codegen:wasm-file-sri-hash tmp)))
             (assert-type string sri)
             ;; Must begin with the algorithm prefix.
             (assert-string= "sha384-" (subseq sri 0 7))
             ;; The base64 body must be non-empty and consist of valid base64 chars.
             (let ((b64-body (subseq sri 7)))
               (assert-true (> (length b64-body) 0))
               (assert-true (every (lambda (c)
                                     (or (alphanumericp c)
                                         (member c '(#\+ #\/ #\=))))
                                   b64-body)))))
      (ignore-errors (delete-file tmp)))))
