;;;; packages/runtime/tests/continuous-profile-tests.lisp

(in-package :cl-cc/test)

(defsuite continuous-profile-suite
  :description "FR-701 continuous profiling integration tests"
  :parent cl-cc-unit-suite)

(in-suite continuous-profile-suite)

(deftest fr-701-continuous-profiling-records-and-exports-otel
  "FR-701: continuous profiling records samples and exports OpenTelemetry span data."
  (let ((session (cl-cc/runtime:rt-start-continuous-profile
                  :name "fr701" :output nil
                  :attributes '(("service.name" . "cl-cc-test")))))
    (cl-cc/runtime:rt-record-profile-sample '("main" "worker") :count 3 :session session)
    (assert-equal 3 (gethash "main;worker"
                              (cl-cc/runtime:rt-continuous-profile-session-samples session)))
    (cl-cc/runtime:rt-stop-continuous-profile session)
    (let ((span (cl-cc/runtime:rt-continuous-profile->otel-span session)))
      (assert-true (search "fr701" (cl-cc/runtime:rt-otel-span-to-json span))))))

(deftest fr-701-continuous-profiler-background-sampler-and-json-exports
  "FR-701: the background sampler captures timestamped stack samples and exports OTel/pprof JSON."
  (let ((session (cl-cc/runtime:rt-start-continuous-profile
                  :name "fr701-sampler"
                  :sample-rate-hz 100
                  :output nil
                  :trace-id "0123456789abcdef0123456789abcdef"
                  :span-id "0123456789abcdef")))
    (sleep 0.05)
    (cl-cc/runtime:rt-record-profile-sample
     (list (cl-cc/runtime:make-rt-profile-frame
            :function "manual-worker"
            :source-file "continuous-profile-tests.lisp"
            :source-line 42))
     :thread-id "test-thread"
     :session session)
    (cl-cc/runtime:rt-stop-continuous-profile session)
    (assert-true (> (length (cl-cc/runtime:rt-continuous-profile-session-sample-log session)) 0))
    (let ((otel (cl-cc/runtime:rt-continuous-profile-to-otel-json session))
          (pprof (cl-cc/runtime:rt-continuous-profile-to-pprof-json session)))
      (assert-true (search "\"trace_id\":\"0123456789abcdef0123456789abcdef\"" otel))
      (assert-true (search "\"stack\"" otel))
      (assert-true (search "\"sampleType\"" pprof))
      (assert-true (search "manual-worker" pprof)))))
