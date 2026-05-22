;;;; packages/vm/tests/json-tests.lisp — FR-684 JSON reader/writer tests

(in-package :cl-cc/test)

(defsuite json-suite
  :description "JSON reader/writer unit tests"
  :parent cl-cc-unit-suite)

(in-suite json-suite)

(deftest json-parse-object-array-null
  "json:parse reads objects, arrays, booleans, numbers, strings, and null."
  (let ((json:*json-null* :null))
    (let ((obj (json:parse "{\"a\":1,\"b\":[true,false,null,\"x\"]}")))
      (assert-= 1 (gethash "a" obj))
      (assert-equal '(t nil :null "x") (gethash "b" obj)))))

(deftest json-stringify-roundtrip
  "json:stringify emits parseable JSON for hash-table objects."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "name" table) "cl-cc"
          (gethash "ok" table) t
          (gethash "items" table) #(1 2 3))
    (let ((parsed (json:parse (json:stringify table))))
      (assert-equal "cl-cc" (gethash "name" parsed))
      (assert-true (gethash "ok" parsed))
      (assert-equal '(1 2 3) (gethash "items" parsed)))))

(deftest json-streaming-parse-and-stringify
  "json:parse-stream and json:stringify-stream work on arbitrary streams."
  (with-input-from-string (in " [1, 2, 3] ")
    (assert-equal '(1 2 3) (json:parse-stream in)))
  (let ((out (make-string-output-stream)))
    (json:stringify-stream '("a" "b") out)
    (assert-equal "[\"a\",\"b\"]" (get-output-stream-string out))))
