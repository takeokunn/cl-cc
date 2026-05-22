;;;; packages/runtime/tests/runtime-crypto-compress-tests.lisp — FR-738..FR-740 evidence tests

(in-package :cl-cc/test)

(defsuite runtime-crypto-compress-suite
  :description "Runtime crypto/Base64/compression tests (FR-738, FR-739, FR-740)"
  :parent cl-cc-unit-suite)

(in-suite runtime-crypto-compress-suite)

(defun %octet-string (octets)
  (map 'string #'code-char octets))

(deftest fr-738-sha256-fips-vector-abc
  "FR-738: SHA-256 matches the FIPS 180-4 abc test vector."
  (assert-equal
   "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
   (cl-cc/runtime:sha256-hex "abc")))

(deftest fr-738-sha512-fips-vector-abc
  "FR-738: SHA-512 matches the FIPS 180-4 abc test vector."
  (assert-equal
   "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"
   (cl-cc/runtime:sha512-hex "abc")))

(deftest fr-738-hmac-sha256-rfc4231-vector
  "FR-738: HMAC-SHA256 matches RFC 4231 test case 1."
  (assert-equal
   "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"
   (cl-cc/runtime:hmac-sha256-hex (make-array 20 :element-type '(unsigned-byte 8) :initial-element #x0b)
                                  "Hi There")))

(deftest fr-739-base64-rfc4648-vectors
  "FR-739: Base64 encode/decode follows RFC 4648 padding vectors."
  (assert-equal "" (cl-cc/runtime:base64-encode ""))
  (assert-equal "Zg==" (cl-cc/runtime:base64-encode "f"))
  (assert-equal "Zm8=" (cl-cc/runtime:base64-encode "fo"))
  (assert-equal "Zm9v" (cl-cc/runtime:base64-encode "foo"))
  (assert-equal "foobar" (%octet-string (cl-cc/runtime:base64-decode "Zm9vYmFy"))))

(deftest fr-739-base64-url-safe-and-streaming
  "FR-739: URL-safe alphabet and streaming helpers round-trip octets."
  (let* ((bytes #(251 255 238 250))
         (encoded (cl-cc/runtime:base64-encode bytes :url-safe t :padding nil)))
    (assert-equal "-__u-g" encoded)
    (assert-true (equalp bytes (cl-cc/runtime:base64-decode encoded :url-safe t))))
  (let ((input (make-string-input-stream "hello"))
        (encoded (make-string-output-stream)))
    (cl-cc/runtime:base64-encode-stream input encoded)
    (assert-equal "aGVsbG8=" (get-output-stream-string encoded))))

(deftest fr-740-deflate-stored-roundtrip
  "FR-740: raw DEFLATE stored-block compression round-trips data."
  (let* ((plain "hello hello hello")
         (compressed (cl-cc/runtime:deflate-compress plain))
         (decompressed (cl-cc/runtime:deflate-decompress compressed)))
    (assert-equal plain (%octet-string decompressed))))

(deftest fr-740-zlib-roundtrip-and-checksum
  "FR-740: zlib wrapper round-trips data and validates Adler-32."
  (let* ((plain "zlib payload")
         (compressed (cl-cc/runtime:zlib-compress plain))
         (decompressed (cl-cc/runtime:zlib-decompress compressed)))
    (assert-equal plain (%octet-string decompressed))))

(deftest fr-740-gzip-roundtrip-and-trailer
  "FR-740: gzip wrapper round-trips data and validates CRC32/ISIZE trailer."
  (let* ((plain "gzip payload")
         (compressed (cl-cc/runtime:gzip-compress plain))
         (decompressed (cl-cc/runtime:gzip-decompress compressed)))
    (assert-equal plain (%octet-string decompressed))))
