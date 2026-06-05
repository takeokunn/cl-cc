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

;;; ---------------------------------------------------------------------------
;;; %rt-profile-resolve-perf-symbol
;;; ---------------------------------------------------------------------------

(deftest continuous-profile-resolve-perf-symbol-hit
  "Address inside an entry returns the symbol name; tests two distinct entries and inclusive-start boundary."
  (let ((perf-map '((100 200 "entry-A") (300 400 "entry-B"))))
    ;; Exact start (inclusive)
    (assert-equal "entry-A" (cl-cc/runtime::%rt-profile-resolve-perf-symbol 100 perf-map))
    ;; Interior address
    (assert-equal "entry-A" (cl-cc/runtime::%rt-profile-resolve-perf-symbol 150 perf-map))
    ;; Second entry, interior
    (assert-equal "entry-B" (cl-cc/runtime::%rt-profile-resolve-perf-symbol 350 perf-map))))

(deftest continuous-profile-resolve-perf-symbol-miss
  "Address before, at-end (exclusive), and nil all return nil."
  (let ((perf-map '((100 200 "entry-A"))))
    ;; Before range
    (assert-equal nil (cl-cc/runtime::%rt-profile-resolve-perf-symbol 50 perf-map))
    ;; Exclusive end
    (assert-equal nil (cl-cc/runtime::%rt-profile-resolve-perf-symbol 200 perf-map))
    ;; Past range
    (assert-equal nil (cl-cc/runtime::%rt-profile-resolve-perf-symbol 999 perf-map))
    ;; Nil address
    (assert-equal nil (cl-cc/runtime::%rt-profile-resolve-perf-symbol nil perf-map))))

;;; ---------------------------------------------------------------------------
;;; %rt-profile-collapsed-stack
;;; ---------------------------------------------------------------------------

(deftest continuous-profile-collapsed-stack-from-frames
  "Three rt-profile-frame objects produce a semicolon-joined string top;middle;bottom."
  (let ((stack (list (cl-cc/runtime:make-rt-profile-frame :function "top")
                     (cl-cc/runtime:make-rt-profile-frame :function "middle")
                     (cl-cc/runtime:make-rt-profile-frame :function "bottom"))))
    (assert-equal "top;middle;bottom"
                  (cl-cc/runtime::%rt-profile-collapsed-stack stack))))

(deftest continuous-profile-collapsed-stack-from-strings
  "Plain string list produces semicolon-joined result."
  (let ((stack (list "alpha" "beta" "gamma")))
    (assert-equal "alpha;beta;gamma"
                  (cl-cc/runtime::%rt-profile-collapsed-stack stack))))

(deftest continuous-profile-collapsed-stack-single-frame
  "Single-element list produces name with no semicolons."
  (let ((stack (list (cl-cc/runtime:make-rt-profile-frame :function "only"))))
    (assert-equal "only"
                  (cl-cc/runtime::%rt-profile-collapsed-stack stack))))

;;; ---------------------------------------------------------------------------
;;; %rt-profile-normalize-frame
;;; ---------------------------------------------------------------------------

(deftest continuous-profile-normalize-frame-string
  "A bare string is coerced into an rt-profile-frame."
  (let ((result (cl-cc/runtime::%rt-profile-normalize-frame "my-fn")))
    (assert-true (cl-cc/runtime::rt-profile-frame-p result))
    (assert-equal "my-fn" (cl-cc/runtime:rt-profile-frame-function result))))

(deftest continuous-profile-normalize-frame-passthrough
  "An already-resolved rt-profile-frame is returned by identity."
  (let* ((frame (cl-cc/runtime:make-rt-profile-frame :function "existing"))
         (result (cl-cc/runtime::%rt-profile-normalize-frame frame)))
    (assert-eq frame result)))

(deftest continuous-profile-normalize-frame-perf-map-enrichment
  "A frame with :address gets its :perf-symbol populated from the perf-map."
  (let* ((perf-map '((1000 2000 "perf-symbol-name")))
         (frame (cl-cc/runtime:make-rt-profile-frame :function "fn" :address 1500))
         (result (cl-cc/runtime::%rt-profile-normalize-frame frame perf-map)))
    (assert-eq frame result)
    (assert-equal "perf-symbol-name" (cl-cc/runtime:rt-profile-frame-perf-symbol result))))

;;; ---------------------------------------------------------------------------
;;; %rt-profile-trim-samples
;;; ---------------------------------------------------------------------------

(deftest continuous-profile-trim-samples-under-limit
  "Fill-pointer below max-samples leaves the log unchanged."
  (let ((session (cl-cc/runtime:make-rt-continuous-profile-session
                  :name "trim-under"
                  :max-samples 10
                  :sample-log (make-array 4 :adjustable t :fill-pointer 0))))
    (vector-push-extend "a" (cl-cc/runtime:rt-continuous-profile-session-sample-log session))
    (vector-push-extend "b" (cl-cc/runtime:rt-continuous-profile-session-sample-log session))
    (cl-cc/runtime::%rt-profile-trim-samples session)
    (assert-equal 2 (fill-pointer (cl-cc/runtime:rt-continuous-profile-session-sample-log session)))))

(deftest continuous-profile-trim-samples-at-limit
  "5 entries with max-samples 3 trims to 3 and retains the three most-recent entries (c,d,e) in order."
  (let ((session (cl-cc/runtime:make-rt-continuous-profile-session
                  :name "trim-at"
                  :max-samples 3
                  :sample-log (make-array 8 :adjustable t :fill-pointer 0))))
    (dolist (x '("a" "b" "c" "d" "e"))
      (vector-push-extend x (cl-cc/runtime:rt-continuous-profile-session-sample-log session)))
    (cl-cc/runtime::%rt-profile-trim-samples session)
    (let ((log (cl-cc/runtime:rt-continuous-profile-session-sample-log session)))
      (assert-equal 3 (fill-pointer log))
      (assert-equal "c" (aref log 0))
      (assert-equal "d" (aref log 1))
      (assert-equal "e" (aref log 2)))))

;;; ---------------------------------------------------------------------------
;;; rt-export-continuous-profile
;;; ---------------------------------------------------------------------------

(deftest continuous-profile-export-nil-output-returns-string
  "output nil returns the OTel JSON string containing resourceProfiles without touching any file."
  (let ((session (cl-cc/runtime:make-rt-continuous-profile-session
                  :name "export-nil"
                  :output nil
                  :format :otel-json
                  :started-at-nanos 0
                  :stopped-at-nanos 1000000)))
    (let ((result (cl-cc/runtime:rt-export-continuous-profile session :output nil)))
      (assert-true (stringp result))
      (assert-true (search "resourceProfiles" result)))))

(deftest continuous-profile-export-pprof-format
  ":format :pprof-json dispatches to pprof-json; payload contains sampleType and periodType."
  (let ((session (cl-cc/runtime:make-rt-continuous-profile-session
                  :name "export-pprof"
                  :output nil
                  :format :pprof-json
                  :started-at-nanos 0
                  :stopped-at-nanos 1000000)))
    (let ((result (cl-cc/runtime:rt-export-continuous-profile session :format :pprof-json :output nil)))
      (assert-true (stringp result))
      (assert-true (search "sampleType" result))
      (assert-true (search "periodType" result)))))

(deftest continuous-profile-export-stdout-returns-payload
  "output :stdout writes to *standard-output* and still returns the payload string; checks for cl-cc/runtime in OTel JSON."
  (let ((session (cl-cc/runtime:make-rt-continuous-profile-session
                  :name "export-stdout"
                  :output :stdout
                  :format :otel-json
                  :started-at-nanos 0
                  :stopped-at-nanos 1000000)))
    (let* ((captured (with-output-to-string (*standard-output*)
                       (let ((result (cl-cc/runtime:rt-export-continuous-profile
                                      session :output :stdout)))
                         (assert-true (stringp result))
                         (assert-true (search "cl-cc/runtime" result))))))
      (assert-true (search "cl-cc/runtime" captured)))))

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

