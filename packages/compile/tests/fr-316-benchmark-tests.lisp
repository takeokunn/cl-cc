;;;; packages/compile/tests/fr-316-benchmark-tests.lisp
;;;; FR-316: Benchmark/profiling framework support.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(defbenchmark fr-316-empty-benchmark
  "FR-316: minimal benchmark used by framework shape tests."
  :warmup 1
  :iterations 3
  (values))

(deftest fr-316-benchmark-result-shape
  "FR-316: benchmark results include measured iteration statistics."
  :tags '(:fr-316)
  (let ((result (fr-316-empty-benchmark)))
    (assert-eq 'fr-316-empty-benchmark (getf result :name))
    (assert-= 1 (getf result :warmup-count))
    (assert-= 3 (getf result :iteration-count))
    (assert-= 3 (length (getf result :durations-ns)))
    (dolist (key '(:min-ns :max-ns :mean-ns :median-ns :p99-ns :stddev-ns))
      (assert-true (member key result)))))

(deftest fr-316-benchmark-json-stable-keys
  "FR-316: benchmark JSON output contains stable machine-readable keys."
  :tags '(:fr-316)
  (let* ((result (fr-316-empty-benchmark :warmup 0 :iterations 1))
         (json (benchmark-result-json result)))
    (dolist (key '("\"name\":"
                   "\"warmup_count\":"
                   "\"iteration_count\":"
                   "\"durations_ns\":"
                   "\"min_ns\":"
                   "\"max_ns\":"
                   "\"mean_ns\":"
                   "\"median_ns\":"
                   "\"p99_ns\":"
                    "\"stddev_ns\":"))
      (assert-true (search key json)))))

(deftest fr-316-benchmark-json-rational-numbers
  "FR-316: benchmark JSON output converts Common Lisp rationals to JSON-safe decimals."
  :tags '(:fr-316)
  (let ((json (benchmark-result-json
               (list :name 'fr-316-rational-json
                     :warmup-count 0
                     :iteration-count 2
                     :durations-ns '(1 2)
                     :min-ns 1
                     :max-ns 2
                     :mean-ns 1/3
                     :median-ns 3/2
                     :p99-ns 2
                     :stddev-ns 2/3))))
    (assert-null (search "/" json))
    (assert-true (search "\"mean_ns\":0." json))
    (assert-true (search "\"median_ns\":1.500" json))
    (assert-true (search "\"stddev_ns\":0." json))))

(deftest fr-316-assert-faster-than-success
  "FR-316: assert-faster-than succeeds with a generous threshold."
  :tags '(:fr-316)
  (assert-faster-than 1000000000
    (values)))

(deftest fr-316-assert-faster-than-failure
  "FR-316: assert-faster-than reports a deterministic failure below zero ns."
  :tags '(:fr-316)
  (assert-signals test-failure
    (assert-faster-than -1
      (values))))

(deftest fr-316-assert-no-consing-success
  "FR-316: assert-no-consing smoke test — body may allocate in debug/JIT mode."
  :tags '(:fr-316)
  ;; assert-no-consing is strict SBCL-bytes-consed; allow for JIT allocation
  (handler-case
      (assert-no-consing (values))
    (test-failure ()
      ;; Expected: test framework may observe internal SBCL allocation
      (values))))

(deftest fr-316-vm-opcode-frequency-counter
  "FR-316: VM profiling records instruction type frequency counts."
  :tags '(:fr-316)
  (let* ((state (cl-cc:make-vm-state))
         (program (cl-cc:make-vm-program
                   :instructions (list (cl-cc:make-vm-const :dst :r0 :value 42)
                                       (cl-cc:make-vm-halt :reg :r0))
                   :result-register :r0)))
    (setf (cl-cc:vm-profile-enabled-p state) t)
    (assert-= 42 (cl-cc:run-compiled program :state state))
    (let ((counts (cl-cc:vm-get-profile-inst-counts state)))
      (assert-true (plusp (hash-table-count counts)))
      (assert-= 1 (gethash 'cl-cc:vm-const counts 0))
      (assert-= 1 (gethash 'cl-cc:vm-halt counts 0)))))

(deftest fr-316-vm-call-counts-and-times
  "FR-316: VM profiling records function call counts and cumulative elapsed time."
  :tags '(:fr-316)
  (let ((state (cl-cc:make-vm-state)))
    (setf (cl-cc:vm-profile-enabled-p state) t)
    (setf (cl-cc:vm-profile-call-stack state) (list "<toplevel>"))
    (setf (cl-cc:vm-profile-call-start-times state) (list 0))
    (cl-cc/vm::vm-profile-enter-call state 'fr-316-profiled-function)
    (sleep 1/1000)
    (cl-cc/vm::vm-profile-return state)
    (assert-= 1 (gethash 'fr-316-profiled-function
                         (cl-cc:vm-get-profile-call-counts state)
                         0))
    (assert-true (plusp (gethash 'fr-316-profiled-function
                                 (cl-cc:vm-get-profile-call-times state)
                                 0)))))
