(in-package :cl-cc/test)

(defsuite runtime-stdlib-2-runtime-suite
  :description "Runtime-stdlib-2 runtime feature tests"
  :parent cl-cc-unit-suite)

(in-suite runtime-stdlib-2-runtime-suite)

(deftest runtime-stdlib-2-runtime-system-loads
  "The runtime system remains loadable with runtime-stdlib-2 features."
  :timeout 5
  (assert-true (asdf:find-system :cl-cc-runtime nil)))

(deftest runtime-structured-logging-json-and-context
  "FR-791: structured JSON logging includes level, message, context, attrs, and time."
  :timeout 5
  (let ((stream (make-string-output-stream)))
    (let ((cl-cc/runtime:*log-level* cl-cc/runtime:+log-level-trace+)
          (cl-cc/runtime:*log-output* stream)
          (cl-cc/runtime:*log-json-output* t))
      (cl-cc/runtime:with-log-context ((:request-id "r1"))
        (cl-cc/runtime:log-info "hello" :component "test")))
    (let ((line (get-output-stream-string stream)))
      (assert-true (search "\"level\":\"info\"" line))
      (assert-true (search "\"message\":\"hello\"" line))
      (assert-true (search "\"request-id\":\"r1\"" line))
      (assert-true (search "\"component\":\"test\"" line))
      (assert-true (search "\"time\":" line)))))

(deftest runtime-metrics-prometheus-output
  "FR-792: counters, gauges, histograms export Prometheus text."
  :timeout 5
  (let ((counter (cl-cc/runtime:make-counter :requests :labels '(:route "/")))
        (gauge (cl-cc/runtime:make-gauge :workers))
        (histogram (cl-cc/runtime:make-histogram :latency '(0.1d0 1.0d0))))
    (cl-cc/runtime:increment! counter 3)
    (cl-cc/runtime:set-gauge! gauge 7)
    (cl-cc/runtime:observe! histogram 0.05d0)
    (cl-cc/runtime:observe! histogram 2.0d0)
    (let ((text (cl-cc/runtime:prometheus-text-format (list counter gauge histogram))))
      (assert-true (search "# TYPE requests counter" text))
      (assert-true (search "requests{route=\"/\"} 3" text))
      (assert-true (search "workers 7" text))
      (assert-true (search "latency_bucket" text))
      (assert-true (search "latency_count 2" text)))))

(deftest runtime-perf-counters-condition-path
  "FR-793: unsupported OS/native performance counter paths signal a condition."
  :timeout 5
  (assert-true (integerp (cl-cc/runtime:rdtsc)))
  (multiple-value-bind (tsc aux) (cl-cc/runtime:rdtscp)
    (assert-true (integerp tsc))
    (assert-= 0 aux))
  (assert-signals cl-cc/runtime:perf-counters-unsupported
    (cl-cc/runtime:with-perf-counters (:cycles)
      :unreachable)))

(deftest runtime-arena-allocator-bulk-reset
  "FR-816: arena allocation is bump-pointer and reset bulk-frees."
  :timeout 5
  (cl-cc/runtime:with-arena (arena :size-hint 2)
    (let ((a (cl-cc/runtime:arena-alloc arena 1))
          (b (cl-cc/runtime:arena-alloc arena 3)))
      (assert-= 0 (cl-cc/runtime:rt-arena-block-offset a))
      (assert-= 1 (cl-cc/runtime:rt-arena-block-offset b))
      (assert-= 4 (cl-cc/runtime:rt-arena-cursor arena))
      (cl-cc/runtime:arena-reset arena)
      (assert-= 0 (cl-cc/runtime:rt-arena-cursor arena)))))

(deftest runtime-object-pool-two-tier-reuse
  "FR-817: object pool acquires and releases reusable objects."
  :timeout 5
  (let* ((pool (cl-cc/runtime:make-object-pool :test-vector
                                               :min-size 1
                                               :max-size 4
                                               :constructor (lambda () (vector :fresh))))
         (obj (cl-cc/runtime:pool-acquire pool)))
    (cl-cc/runtime:pool-release pool obj)
    (assert-eq obj (cl-cc/runtime:pool-acquire pool))))

(deftest runtime-gc-and-vm-runtime-configuration
  "FR-833/FR-834/FR-835: runtime configuration and adaptive tuning are observable."
  :timeout 5
  (let ((old-nursery cl-cc/runtime:*gc-nursery-size*)
        (old-adaptive cl-cc/vm:*adaptive-jit-enabled*))
    (unwind-protect
         (progn
           (setf cl-cc/runtime:*gc-nursery-size* 131072
                 cl-cc/runtime:*gc-young-size-words* 131072)
           (cl-cc/vm:vm-record-gc-pause 100.0d0)
           (assert-true (< cl-cc/runtime:*gc-nursery-size* 131072))
           (setf cl-cc/vm:*adaptive-jit-enabled* nil)
           (cl-cc/vm:vm-handle-runtime-flag '("--adaptive-jit"))
           (assert-true cl-cc/vm:*adaptive-jit-enabled*)
           (cl-cc/vm:enqueue-jit-compilation :cold :hotness 1)
           (cl-cc/vm:enqueue-jit-compilation :hot :hotness 10)
           (assert-equal '(10 . :hot) (cl-cc/vm:dequeue-jit-compilation))
           (let ((report (cl-cc/vm:runtime-tuning-report)))
             (assert-true (getf report :jit-tier1-threshold))
             (assert-true (getf report :gc-nursery-size))))
      (setf cl-cc/runtime:*gc-nursery-size* old-nursery
            cl-cc/runtime:*gc-young-size-words* old-nursery
            cl-cc/vm:*adaptive-jit-enabled* old-adaptive
            cl-cc/vm:*jit-compilation-queue* nil))))
