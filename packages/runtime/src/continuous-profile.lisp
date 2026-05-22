;;;; packages/runtime/src/continuous-profile.lisp — FR-701 continuous profiling

(in-package :cl-cc/runtime)

(defconstant +rt-continuous-profile-default-rate-hz+ 100
  "Default continuous profiler sampling rate.")

(defconstant +rt-continuous-profile-default-max-samples+ 65536
  "Default bounded in-memory sample buffer size.")

(defstruct rt-profile-frame
  "One resolved profile frame."
  (function "<unknown>" :type string)
  (source-file nil :type (or null string))
  (source-line nil :type (or null integer))
  (address nil :type (or null integer))
  (perf-symbol nil :type (or null string)))

(defstruct rt-profile-sample
  "One timestamped profiler sample."
  (timestamp-nanos 0 :type integer)
  (thread-id "unknown" :type string)
  (trace-id nil :type (or null string))
  (span-id nil :type (or null string))
  (stack nil :type list)
  (count 1 :type integer))

(defstruct rt-continuous-profile-session
  "In-process continuous profiling session with pprof/OTel export hooks."
  (name "clcc" :type string)
  (started-at 0 :type integer)
  (started-at-nanos 0 :type integer)
  (stopped-at nil)
  (stopped-at-nanos nil)
  (samples (make-hash-table :test #'equal) :type hash-table)
  (sample-log (make-array 128 :adjustable t :fill-pointer 0) :type vector)
  (attributes nil :type list)
  (running-p nil :type boolean)
  (sample-rate-hz +rt-continuous-profile-default-rate-hz+ :type integer)
  (output :stdout)
  (format :otel-json)
  (endpoint nil :type (or null string))
  (max-samples +rt-continuous-profile-default-max-samples+ :type integer)
  (sampler-thread nil)
  (trace-id nil :type (or null string))
  (span-id nil :type (or null string))
  (perf-map nil :type list)
  (lock #+sbcl (sb-thread:make-mutex :name "cl-cc continuous-profile")
        #-sbcl nil))

(defvar *rt-continuous-profile-session* nil
  "Current FR-701 continuous profiling session, if one is active.")

(defmacro %rt-profile-with-lock ((session) &body body)
  `(let ((%session ,session))
     #+sbcl
     (sb-thread:with-mutex ((rt-continuous-profile-session-lock %session))
       ,@body)
     #-sbcl
     (progn ,@body)))

(defun %rt-profile-now-nanos ()
  "Return a monotonic-ish nanosecond timestamp for profile records."
  (truncate (* (get-internal-real-time)
               (/ 1000000000 internal-time-units-per-second))))

(defun %rt-profile-safe-rate (rate)
  (let ((value (or rate +rt-continuous-profile-default-rate-hz+)))
    (check-type value integer)
    (max 1 value)))

(defun %rt-profile-thread-id ()
  "Return a stable, JSON-safe identifier for the current host thread."
  #+sbcl
  (let ((thread sb-thread:*current-thread*))
    (format nil "~A#~A"
            (or (ignore-errors (sb-thread:thread-name thread)) "thread")
            (sxhash thread)))
  #-sbcl
  "main")

(defun %rt-profile-default-perf-map-path ()
  "Return the conventional FR-553 perf map path for this process."
  (let ((pid (or #+sbcl
                 (ignore-errors
                   (require :sb-posix)
                   (let* ((pkg (find-package "SB-POSIX"))
                          (sym (and pkg (find-symbol "GETPID" pkg))))
                     (and sym (funcall sym))))
                 #-sbcl nil
                 0)))
    (merge-pathnames (format nil "perf-~D.map" pid) #p"/tmp/")))

(defun %rt-profile-hex-token-p (text)
  (and (> (length text) 0)
       (loop for char across text
             always (digit-char-p char 16))))

(defun %rt-profile-read-perf-map (&optional path)
  "Read FR-553 perf map entries as (START END SYMBOL) rows."
  (let ((entries nil)
        (path (or path (%rt-profile-default-perf-map-path))))
    (when (probe-file path)
      (with-open-file (in path :direction :input)
        (loop for line = (read-line in nil nil)
              while line
              do (let ((parts (remove "" (uiop:split-string line :separator '(#\Space #\Tab))
                                      :test #'string=)))
                   (when (and (>= (length parts) 3)
                              (%rt-profile-hex-token-p (first parts))
                              (%rt-profile-hex-token-p (second parts)))
                     (let ((start (parse-integer (first parts) :radix 16))
                           (size (parse-integer (second parts) :radix 16))
                           (name (third parts)))
                       (push (list start (+ start size) name) entries)))))))
    (sort entries #'< :key #'first)))

(defun %rt-profile-resolve-perf-symbol (address perf-map)
  "Resolve ADDRESS through PERF-MAP entries produced by FR-553."
  (when address
    (let ((entry (find-if (lambda (row)
                           (and (<= (first row) address) (< address (second row))))
                         perf-map)))
      (and entry (third entry)))))

(defun %rt-profile-frame-name (frame)
  (cond
    ((rt-profile-frame-p frame) (rt-profile-frame-function frame))
    ((stringp frame) frame)
    (t (princ-to-string frame))))

(defun %rt-profile-normalize-frame (frame &optional perf-map)
  (cond
    ((rt-profile-frame-p frame)
     (unless (rt-profile-frame-perf-symbol frame)
       (setf (rt-profile-frame-perf-symbol frame)
             (%rt-profile-resolve-perf-symbol (rt-profile-frame-address frame) perf-map)))
     frame)
    ((stringp frame) (make-rt-profile-frame :function frame))
    (t (make-rt-profile-frame :function (princ-to-string frame)))))

(defun %rt-profile-collapsed-stack (stack)
  (format nil "~{~A~^;~}" (mapcar #'%rt-profile-frame-name stack)))

#+sbcl
(defun %rt-profile-code-source (code-location)
  (let ((source (ignore-errors (sb-di:code-location-debug-source code-location))))
    (values (ignore-errors (sb-di:debug-source-namestring source))
            (ignore-errors (sb-di:code-location-toplevel-form-offset code-location)))))

#+sbcl
(defun %rt-profile-capture-stack (&key (skip 0) (limit 64))
  "Capture the current SBCL stack as resolved frames."
  (let ((frames nil)
        (index 0)
        (frame (ignore-errors (sb-di:top-frame))))
    (handler-case
        (loop while (and frame (< (length frames) limit))
              do (when (>= index skip)
                   (let* ((debug-fun (ignore-errors (sb-di:frame-debug-fun frame)))
                          (name (ignore-errors (sb-di:debug-fun-name debug-fun)))
                          (code-location (ignore-errors (sb-di:frame-code-location frame))))
                     (multiple-value-bind (file line)
                         (if code-location
                             (%rt-profile-code-source code-location)
                             (values nil nil))
                       (push (make-rt-profile-frame
                              :function (princ-to-string (or name :unknown))
                              :source-file file
                              :source-line line)
                             frames))))
                 (incf index)
                 (setf frame (ignore-errors (sb-di:frame-down frame))))
      (error ()
        (push (make-rt-profile-frame :function "<stack-unavailable>") frames)))
    (nreverse frames)))

#-sbcl
(defun %rt-profile-capture-stack (&key (skip 0) (limit 64))
  (declare (ignore skip limit))
  (list (make-rt-profile-frame :function "<stack-unavailable>")))

(defun %rt-profile-trim-samples (session)
  "Keep SESSION's sample vector bounded to avoid unbounded profiler overhead."
  (let* ((log (rt-continuous-profile-session-sample-log session))
         (max-samples (rt-continuous-profile-session-max-samples session))
         (extra (- (fill-pointer log) max-samples)))
    (when (plusp extra)
      (replace log log :start1 0 :start2 extra :end2 (fill-pointer log))
      (decf (fill-pointer log) extra))))

(defun %rt-profile-add-sample (session sample)
  "Add SAMPLE to SESSION with low allocation pressure and collapsed counts."
  (%rt-profile-with-lock (session)
    (let* ((stack (rt-profile-sample-stack sample))
           (key (%rt-profile-collapsed-stack stack)))
      (incf (gethash key (rt-continuous-profile-session-samples session) 0)
            (rt-profile-sample-count sample))
      (vector-push-extend sample (rt-continuous-profile-session-sample-log session))
      (%rt-profile-trim-samples session)
      key)))

(defun %rt-profile-sample-once (session)
  "Capture and record one profiler sample for the current sampler thread."
  (let* ((perf-map (rt-continuous-profile-session-perf-map session))
         (stack (mapcar (lambda (frame) (%rt-profile-normalize-frame frame perf-map))
                        (%rt-profile-capture-stack :skip 3 :limit 64))))
    (%rt-profile-add-sample
     session
     (make-rt-profile-sample
      :timestamp-nanos (%rt-profile-now-nanos)
      :thread-id (%rt-profile-thread-id)
      :trace-id (rt-continuous-profile-session-trace-id session)
      :span-id (rt-continuous-profile-session-span-id session)
      :stack stack))))

(defun %rt-profile-sampler-loop (session)
  "Background sampler loop. Uses timed sampling as the portable SIGPROF analogue."
  (let ((interval (/ 1.0d0 (rt-continuous-profile-session-sample-rate-hz session))))
    (loop while (rt-continuous-profile-session-running-p session)
          do (handler-case (%rt-profile-sample-once session)
               (error () nil))
             (sleep interval))))

(defun rt-start-continuous-profile (&key (name "clcc") attributes
                                      (sample-rate-hz +rt-continuous-profile-default-rate-hz+)
                                      (output :stdout) endpoint (format :otel-json)
                                      (max-samples +rt-continuous-profile-default-max-samples+)
                                      perf-map-file trace-id span-id)
  "Start and install a continuous profiling SESSION.

The sampler runs on a background thread at SAMPLE-RATE-HZ (100Hz by default),
records timestamped stack samples with thread IDs, and can export pprof-like or
OpenTelemetry Profiling JSON to OUTPUT (:STDOUT, pathname string, or NIL)."
  (let* ((rate (%rt-profile-safe-rate sample-rate-hz))
         (session (make-rt-continuous-profile-session
                   :name name
                   :started-at (get-universal-time)
                   :started-at-nanos (%rt-profile-now-nanos)
                   :attributes attributes
                   :running-p t
                   :sample-rate-hz rate
                   :output output
                   :endpoint endpoint
                   :format format
                   :max-samples (max 1 max-samples)
                   :trace-id (or trace-id (%rt-otel-random-hex 16))
                   :span-id (or span-id (%rt-otel-random-hex 8))
                   :perf-map (%rt-profile-read-perf-map perf-map-file))))
    #+sbcl
    (setf (rt-continuous-profile-session-sampler-thread session)
          (sb-thread:make-thread (lambda () (%rt-profile-sampler-loop session))
                                 :name (format nil "cl-cc profiler ~A" name)))
    #-sbcl
    (%rt-profile-sample-once session)
    (setf *rt-continuous-profile-session* session)))

(defun rt-stop-continuous-profile (&optional (session *rt-continuous-profile-session*))
  "Stop SESSION, join the sampler thread, flush configured output, and return it."
  (when session
    (setf (rt-continuous-profile-session-running-p session) nil
          (rt-continuous-profile-session-stopped-at session) (get-universal-time)
          (rt-continuous-profile-session-stopped-at-nanos session) (%rt-profile-now-nanos))
    #+sbcl
    (let ((thread (rt-continuous-profile-session-sampler-thread session)))
      (when (and thread (sb-thread:thread-alive-p thread))
        (ignore-errors (sb-thread:join-thread thread :timeout 1))))
    (when (rt-continuous-profile-session-output session)
      (ignore-errors (rt-export-continuous-profile session)))
    (when (eq session *rt-continuous-profile-session*)
      (setf *rt-continuous-profile-session* nil)))
  session)

(defun rt-record-profile-sample (stack &key (count 1) (session *rt-continuous-profile-session*)
                                   timestamp-nanos thread-id trace-id span-id)
  "Record COUNT samples for STACK in SESSION.

STACK may be a collapsed-stack string, a list of frame names, or a list of
RT-PROFILE-FRAME objects. This manual API is retained for tests and explicit
instrumentation; background sampling uses the same storage path."
  (unless (and session (rt-continuous-profile-session-running-p session))
    (error "No running continuous profiling session"))
  (let* ((frames (cond
                   ((stringp stack)
                    (mapcar #'%rt-profile-normalize-frame
                            (remove "" (uiop:split-string stack :separator '(#\;))
                                    :test #'string=)))
                   (t (mapcar (lambda (frame)
                                (%rt-profile-normalize-frame
                                 frame (rt-continuous-profile-session-perf-map session)))
                              stack))))
         (sample (make-rt-profile-sample
                  :timestamp-nanos (or timestamp-nanos (%rt-profile-now-nanos))
                  :thread-id (or thread-id (%rt-profile-thread-id))
                  :trace-id (or trace-id (rt-continuous-profile-session-trace-id session))
                  :span-id (or span-id (rt-continuous-profile-session-span-id session))
                  :stack frames
                  :count count)))
    (%rt-profile-add-sample session sample)))

(defun %rt-profile-json-array (items writer)
  (with-output-to-string (out)
    (write-char #\[ out)
    (loop for item in items
          for first = t then nil
          unless first do (write-char #\, out)
          do (write-string (funcall writer item) out))
    (write-char #\] out)))

(defun %rt-profile-frame-to-json (frame)
  (format nil "{\"function\":\"~a\",\"file\":~a,\"line\":~a,\"address\":~a,\"perf_symbol\":~a}"
          (%rt-otel-escape-json-string (rt-profile-frame-function frame))
          (%rt-otel-json-value (rt-profile-frame-source-file frame))
          (%rt-otel-json-value (rt-profile-frame-source-line frame))
          (%rt-otel-json-value (rt-profile-frame-address frame))
          (%rt-otel-json-value (rt-profile-frame-perf-symbol frame))))

(defun %rt-profile-sample-to-json (sample)
  (format nil "{\"timeUnixNano\":~d,\"thread_id\":\"~a\",\"trace_id\":~a,\"span_id\":~a,\"count\":~d,\"stack\":~a}"
          (rt-profile-sample-timestamp-nanos sample)
          (%rt-otel-escape-json-string (rt-profile-sample-thread-id sample))
          (%rt-otel-json-value (rt-profile-sample-trace-id sample))
          (%rt-otel-json-value (rt-profile-sample-span-id sample))
          (rt-profile-sample-count sample)
          (%rt-profile-json-array (rt-profile-sample-stack sample)
                                  #'%rt-profile-frame-to-json)))

(defun rt-continuous-profile-to-otel-json (session)
  "Export SESSION as thin OpenTelemetry Profiling Signal JSON."
  (let ((samples nil))
    (%rt-profile-with-lock (session)
      (setf samples (coerce (rt-continuous-profile-session-sample-log session) 'list)))
    (format nil "{\"resourceProfiles\":[{\"resource\":{\"attributes\":~a},\"scopeProfiles\":[{\"scope\":{\"name\":\"cl-cc/runtime\"},\"profiles\":[{\"profileId\":\"~a\",\"trace_id\":\"~a\",\"span_id\":\"~a\",\"name\":\"~a\",\"sampleType\":\"cpu\",\"periodType\":\"cpu\",\"period\":~d,\"startTimeUnixNano\":~d,\"endTimeUnixNano\":~d,\"samples\":~a}]}]}]}"
            (%rt-otel-attributes-to-json (rt-continuous-profile-session-attributes session))
            (%rt-otel-escape-json-string (rt-continuous-profile-session-name session))
            (rt-continuous-profile-session-trace-id session)
            (rt-continuous-profile-session-span-id session)
            (%rt-otel-escape-json-string (rt-continuous-profile-session-name session))
            (truncate 1000000000 (rt-continuous-profile-session-sample-rate-hz session))
            (rt-continuous-profile-session-started-at-nanos session)
            (or (rt-continuous-profile-session-stopped-at-nanos session) (%rt-profile-now-nanos))
            (%rt-profile-json-array samples #'%rt-profile-sample-to-json))))

(defun rt-continuous-profile-to-pprof-json (session)
  "Export SESSION as a pprof-compatible JSON profile shape.

This JSON mirrors pprof's Profile message concepts (sampleType, sample,
location, function, stringTable) while avoiding a protobuf dependency in the
runtime leaf system."
  (let ((samples nil)
        (function-ids (make-hash-table :test #'equal))
        (location-ids (make-hash-table :test #'equal))
        (functions nil)
        (locations nil))
    (%rt-profile-with-lock (session)
      (setf samples (coerce (rt-continuous-profile-session-sample-log session) 'list)))
    (labels ((function-id (frame)
               (let ((key (list (rt-profile-frame-function frame)
                                (rt-profile-frame-source-file frame))))
                 (or (gethash key function-ids)
                     (setf (gethash key function-ids)
                           (let ((id (1+ (hash-table-count function-ids))))
                             (push (format nil "{\"id\":~d,\"name\":\"~a\",\"filename\":~a}"
                                           id
                                           (%rt-otel-escape-json-string
                                            (rt-profile-frame-function frame))
                                           (%rt-otel-json-value
                                            (rt-profile-frame-source-file frame)))
                                   functions)
                             id)))))
             (location-id (frame)
               (let ((key (list (rt-profile-frame-function frame)
                                (rt-profile-frame-source-file frame)
                                (rt-profile-frame-source-line frame)
                                (rt-profile-frame-perf-symbol frame))))
                 (or (gethash key location-ids)
                     (setf (gethash key location-ids)
                           (let ((id (1+ (hash-table-count location-ids)))
                                 (fid (function-id frame)))
                             (push (format nil "{\"id\":~d,\"address\":~a,\"line\":[{\"functionId\":~d,\"line\":~a}],\"perfSymbol\":~a}"
                                           id
                                           (%rt-otel-json-value (rt-profile-frame-address frame))
                                           fid
                                           (%rt-otel-json-value (rt-profile-frame-source-line frame))
                                           (%rt-otel-json-value (rt-profile-frame-perf-symbol frame)))
                                   locations)
                             id))))))
      (let ((sample-json
              (mapcar (lambda (sample)
                        (format nil "{\"locationId\":~a,\"value\":[~d],\"timeUnixNano\":~d,\"threadId\":\"~a\",\"traceId\":~a,\"spanId\":~a}"
                                (%rt-profile-json-array
                                 (mapcar #'location-id (rt-profile-sample-stack sample))
                                 (lambda (id) (princ-to-string id)))
                                (rt-profile-sample-count sample)
                                (rt-profile-sample-timestamp-nanos sample)
                                (%rt-otel-escape-json-string (rt-profile-sample-thread-id sample))
                                (%rt-otel-json-value (rt-profile-sample-trace-id sample))
                                (%rt-otel-json-value (rt-profile-sample-span-id sample))))
                      samples)))
        (format nil "{\"sampleType\":[{\"type\":\"samples\",\"unit\":\"count\"}],\"periodType\":{\"type\":\"cpu\",\"unit\":\"nanoseconds\"},\"period\":~d,\"timeNanos\":~d,\"durationNanos\":~d,\"sample\":~a,\"location\":~a,\"function\":~a,\"stringTable\":[\"\"]}"
                (truncate 1000000000 (rt-continuous-profile-session-sample-rate-hz session))
                (rt-continuous-profile-session-started-at-nanos session)
                (- (or (rt-continuous-profile-session-stopped-at-nanos session) (%rt-profile-now-nanos))
                   (rt-continuous-profile-session-started-at-nanos session))
                (%rt-profile-json-array sample-json #'identity)
                (%rt-profile-json-array (nreverse locations) #'identity)
                (%rt-profile-json-array (nreverse functions) #'identity))))))

(defun rt-export-continuous-profile (session &key (format (rt-continuous-profile-session-format session))
                                               (output (rt-continuous-profile-session-output session)))
  "Export SESSION to FORMAT (:OTEL-JSON or :PPROF-JSON) and OUTPUT.

OUTPUT may be :STDOUT, NIL (return string only), or a pathname/string file.
ENDPOINT is retained on the session for callers that send the returned payload
over HTTP outside this leaf runtime system."
  (let ((payload (ecase format
                   (:otel-json (rt-continuous-profile-to-otel-json session))
                   (:pprof-json (rt-continuous-profile-to-pprof-json session)))))
    (cond
      ((eq output :stdout) (write-line payload *standard-output*))
      ((null output) nil)
      (t (with-open-file (out output :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
           (write-string payload out))))
    payload))

(defun rt-continuous-profile->otel-span (session)
  "Export SESSION as an OpenTelemetry-compatible span with sample events."
  (let ((span (rt-otel-start-span (format nil "continuous-profile:~A"
                                          (rt-continuous-profile-session-name session)))))
    (setf (rt-otel-span-trace-id span) (rt-continuous-profile-session-trace-id session)
          (rt-otel-span-span-id span) (rt-continuous-profile-session-span-id session))
    (dolist (attr (rt-continuous-profile-session-attributes session))
      (rt-otel-set-attribute (car attr) (cdr attr) span))
    (%rt-profile-with-lock (session)
      (loop for sample across (rt-continuous-profile-session-sample-log session)
            do (rt-otel-add-event "profile.sample"
                                  :attributes `(("thread_id" . ,(rt-profile-sample-thread-id sample))
                                                ("timestamp_nanos" . ,(rt-profile-sample-timestamp-nanos sample))
                                                ("stack" . ,(%rt-profile-collapsed-stack
                                                             (rt-profile-sample-stack sample)))
                                                ("count" . ,(rt-profile-sample-count sample)))
                                  :span span)))
    (rt-otel-end-span span)))
