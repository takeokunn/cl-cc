(in-package :cl-cc/test)

(in-suite pipeline-native-suite)

(deftest pipeline-perf-map-line-validates-format
  "FR-553 perf map lines use HEX_ADDR HEX_SIZE SYMBOL_NAME format."
  (assert-true (cl-cc/pipeline:perf-map-line-valid-p "1000 2A FOO"))
  (assert-false (cl-cc/pipeline:perf-map-line-valid-p "1000 nope FOO"))
  (assert-false (cl-cc/pipeline:perf-map-line-valid-p "1000 2A")))

(deftest pipeline-write-perf-map-entry-emits-hex-fields
  "write-perf-map-entry writes perf-compatible hex address and size fields."
  (let ((stream (make-string-output-stream)))
    (cl-cc/pipeline:write-perf-map-entry stream #x1000 #x2a 'sample-function)
    (let ((line (string-trim '(#\Newline #\Return)
                             (get-output-stream-string stream))))
      (assert-string= "1000 2A SAMPLE-FUNCTION" line)
      (assert-true (cl-cc/pipeline:perf-map-line-valid-p line)))))

(deftest pipeline-write-perf-map-for-native-code-appends-map-line
  "write-perf-map-for-native-code records a map line for native code bytes."
  (uiop:with-temporary-file (:pathname path :type "map" :keep t)
    (let ((program (cl-cc:make-vm-program
                    :instructions (list (cl-cc:make-vm-const :dst :r0 :value 1)
                                        (cl-cc:make-vm-halt :reg :r0))))
          (bytes #(1 2 3 4)))
      (with-open-file (out path :direction :output :if-exists :supersede)
        (let ((cl-cc/pipeline:*perf-map-stream* out))
          (cl-cc/pipeline:write-perf-map-for-native-code program bytes :output-file "unit-main")))
      (let ((line (with-open-file (in path :direction :input) (read-line in nil nil))))
        (assert-true (cl-cc/pipeline:perf-map-line-valid-p line))
        (assert-true (search "unit-main" line :test #'char-equal))))
    (ignore-errors (delete-file path))))
