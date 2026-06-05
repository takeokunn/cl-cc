(in-package :cl-cc/test)

(in-suite cl-cc-cli-pure-suite)

(deftest cli-flamegraph-reads-perf-map-lines
  "FR-702 perf map input is converted to JIT-colored flamegraph samples."
  (uiop:with-temporary-file (:pathname input :type "map" :keep t)
    (with-open-file (out input :direction :output :if-exists :supersede)
      (write-line "1000 20 jit-function" out)
      (write-line "1020 10 normal-function" out))
    (let ((samples (cl-cc/cli::%read-flamegraph-samples-from-file input)))
      (assert-= #x20 (gethash "jit;jit-function" samples))
      (assert-= #x10 (gethash "jit;normal-function" samples)))
    (ignore-errors (delete-file input))))

(deftest cli-flamegraph-svg-contains-rects-and-colors
  "FR-702 SVG output contains rect elements and expected GC/JIT colors."
  (uiop:with-temporary-file (:pathname output :type "svg" :keep t)
    (let ((samples (make-hash-table :test #'equal)))
      (setf (gethash "cpu;hot-function" samples) 4)
      (setf (gethash "gc;minor-gc" samples) 2)
      (setf (gethash "jit;jit-compile" samples) 3)
      (cl-cc/cli::%write-flamegraph-svg output samples)
      (let ((svg (cl-cc/cli::%read-file output)))
        (assert-true (search "<svg" svg))
        (assert-true (search "<rect" svg))
        (assert-true (search "rgb(90,140,255)" svg))
        (assert-true (search "rgb(255,165,0)" svg))))
    (ignore-errors (delete-file output))))
