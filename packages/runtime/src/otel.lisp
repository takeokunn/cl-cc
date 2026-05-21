(in-package :cl-cc/runtime)

(defstruct rt-otel-event
  (name "")
  (time 0)
  (attributes nil))

(defstruct rt-otel-span
  (name "")
  (trace-id nil)
  (span-id nil)
  (parent-span-id nil)
  (start 0)
  (end 0)
  (attributes (make-hash-table :test #'equal))
  (events nil)
  (status-code "UNSET")
  (status-message ""))

(defvar *rt-otel-span* nil)

(defun %rt-otel-random-hex (bytes)
  (with-output-to-string (out)
    (dotimes (i bytes)
      (format out "~2,'0X" (random 256)))))

(defun %rt-otel-now-nanos ()
  (* (get-universal-time) 1000000000))

(defun %rt-otel-escape-json-string (value)
  (with-output-to-string (out)
    (loop for char across (princ-to-string value)
          do (case char
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (otherwise (write-char char out))))))

(defun %rt-otel-json-value (value)
  (cond
    ((null value) "null")
    ((eq value t) "true")
    ((numberp value) (princ-to-string value))
    ((stringp value) (format nil "\"~a\"" (%rt-otel-escape-json-string value)))
    ((symbolp value) (format nil "\"~a\"" (%rt-otel-escape-json-string value)))
    (t (format nil "\"~a\"" (%rt-otel-escape-json-string value)))))

(defun %rt-otel-attributes-to-json (attributes)
  (with-output-to-string (out)
    (write-char #\{ out)
    (let ((first t))
      (cond
        ((hash-table-p attributes)
         (maphash (lambda (key value)
                    (unless first (write-char #\, out))
                    (setf first nil)
                    (format out "\"~a\":~a"
                            (%rt-otel-escape-json-string key)
                            (%rt-otel-json-value value)))
                  attributes))
        (t
         (dolist (pair attributes)
           (unless first (write-char #\, out))
           (setf first nil)
           (format out "\"~a\":~a"
                   (%rt-otel-escape-json-string (car pair))
                   (%rt-otel-json-value (cdr pair)))))))
    (write-char #\} out)))

(defun %rt-otel-events-to-json (events)
  (with-output-to-string (out)
    (write-char #\[ out)
    (loop for event in (reverse events)
          for first = t then nil
          unless first do (write-char #\, out)
          do (format out
                     "{\"name\":\"~a\",\"timeUnixNano\":~d,\"attributes\":~a}"
                     (%rt-otel-escape-json-string (rt-otel-event-name event))
                     (rt-otel-event-time event)
                     (%rt-otel-attributes-to-json
                      (rt-otel-event-attributes event))))
    (write-char #\] out)))

(defun rt-otel-start-span (name &key parent)
  (let ((span (make-rt-otel-span
               :name name
               :trace-id (or (when parent (rt-otel-span-trace-id parent))
                             (%rt-otel-random-hex 16))
               :span-id (%rt-otel-random-hex 8)
               :parent-span-id (when parent (rt-otel-span-span-id parent))
               :start (%rt-otel-now-nanos))))
    (setf *rt-otel-span* span)
    span))

(defun rt-otel-end-span (span)
  (setf (rt-otel-span-end span) (%rt-otel-now-nanos))
  (when (eq *rt-otel-span* span)
    (setf *rt-otel-span* nil))
  span)

(defun rt-otel-set-attribute (key value &optional (span *rt-otel-span*))
  "Set an OpenTelemetry attribute on SPAN or the current span."
  (unless span
    (error "No active OpenTelemetry span"))
  (setf (gethash (princ-to-string key) (rt-otel-span-attributes span)) value)
  value)

(defun rt-otel-add-event (name &key attributes (span *rt-otel-span*))
  "Add a timestamped event to SPAN or the current span."
  (unless span
    (error "No active OpenTelemetry span"))
  (let ((event (make-rt-otel-event
                :name name
                :time (%rt-otel-now-nanos)
                :attributes (or attributes nil))))
    (push event (rt-otel-span-events span))
    event))

(defun rt-otel-set-status (status &optional (message "") (span *rt-otel-span*))
  "Set SPAN status to OK, ERROR, or UNSET."
  (unless span
    (error "No active OpenTelemetry span"))
  (let ((code (string-upcase (princ-to-string status))))
    (unless (member code '("OK" "ERROR" "UNSET") :test #'string=)
      (error "Invalid OpenTelemetry status: ~a" status))
    (setf (rt-otel-span-status-code span) code
          (rt-otel-span-status-message span) (princ-to-string message))
    code))

(defun rt-otel-span-to-json (span)
  "Export SPAN as an OTLP-compatible JSON span object."
  (format nil
          "{\"traceId\":\"~a\",\"spanId\":\"~a\",\"parentSpanId\":\"~a\",\"name\":\"~a\",\"kind\":\"SPAN_KIND_INTERNAL\",\"startTimeUnixNano\":~d,\"endTimeUnixNano\":~d,\"attributes\":~a,\"events\":~a,\"status\":{\"code\":\"~a\",\"message\":\"~a\"}}"
          (rt-otel-span-trace-id span)
          (rt-otel-span-span-id span)
          (or (rt-otel-span-parent-span-id span) "")
          (%rt-otel-escape-json-string (rt-otel-span-name span))
          (rt-otel-span-start span)
          (rt-otel-span-end span)
          (%rt-otel-attributes-to-json (rt-otel-span-attributes span))
          (%rt-otel-events-to-json (rt-otel-span-events span))
          (rt-otel-span-status-code span)
          (%rt-otel-escape-json-string (rt-otel-span-status-message span))))

(defmacro rt-with-span ((name &key attrs) &body body)
  (let ((span-var (gensym "SPAN")))
    `(let ((,span-var (rt-otel-start-span ,name)))
       (dolist (attr ',attrs)
         (rt-otel-set-attribute (car attr) (cdr attr) ,span-var))
       (unwind-protect
            (progn ,@body)
         (rt-otel-end-span ,span-var)))))

(defun rt-otel-init ()
  (setf *rt-otel-span* nil)
  t)

(export '(rt-otel-event make-rt-otel-event rt-otel-event-name
          rt-otel-event-time rt-otel-event-attributes
          rt-otel-span make-rt-otel-span rt-otel-span-name
          rt-otel-span-trace-id rt-otel-span-span-id
          rt-otel-span-parent-span-id rt-otel-span-start rt-otel-span-end
          rt-otel-span-attributes rt-otel-span-events
          rt-otel-span-status-code rt-otel-span-status-message
          *rt-otel-span* rt-otel-start-span rt-otel-end-span
          rt-otel-set-attribute rt-otel-add-event rt-otel-set-status
          rt-otel-span-to-json rt-with-span rt-otel-init))
