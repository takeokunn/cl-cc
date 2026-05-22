;;; ─── Phase 139 Runtime Metrics ────────────────────────────────────────
;;; FR-792: Prometheus-compatible counter/histogram/gauge metrics

(in-package :cl-cc/runtime)

;; ── Counter ────────────────────────────────────────────────────────────

(defstruct rt-counter
  "A monotonically increasing integer counter with optional labels."
  (name "" :type (or string keyword))
  (value 0 :type integer)
  (labels nil :type list))

(defun rt-make-counter (name &key labels)
  (make-rt-counter :name name :labels labels))

(defun rt-counter-increment! (counter &optional (n 1))
  (declare (type fixnum n))
  (incf (rt-counter-value counter) n)
  (rt-counter-value counter))

;; ── Histogram ──────────────────────────────────────────────────────────

(defstruct rt-histogram
  "A histogram tracking value distributions across configurable buckets."
  (name "" :type (or string keyword))
  (buckets nil :type list)
  (counts nil :type list)
  (sum 0d0 :type double-float)
  (count 0 :type fixnum))

(defun rt-make-histogram (name &key buckets)
  (let ((sorted (sort (copy-list buckets) #'<)))
    (make-rt-histogram :name name :buckets sorted
                       :counts (make-list (length sorted) :initial-element 0))))

(defun rt-histogram-observe! (histogram value)
  (incf (rt-histogram-count histogram))
  (incf (rt-histogram-sum histogram) (float value 1d0))
  (let ((buckets (rt-histogram-buckets histogram))
        (counts (rt-histogram-counts histogram)))
    (loop for b in buckets
          for i from 0
          when (<= value b)
          do (incf (nth i counts))
             (return-from rt-histogram-observe! value))
    ;; overflow bucket
    (incf (nth (1- (length counts)) counts))
    value))

;; ── Gauge ──────────────────────────────────────────────────────────────

(defstruct rt-gauge
  "A gauge representing a value that can go up and down."
  (name "" :type (or string keyword))
  (value 0d0 :type double-float))

(defun rt-make-gauge (name)
  (make-rt-gauge :name name))

(defun rt-gauge-set! (gauge value)
  (setf (rt-gauge-value gauge) (float value 1d0)))

;; ── Prometheus Text Format ─────────────────────────────────────────────

(defvar *rt-metrics-registry* (make-hash-table :test #'equal)
  "Global registry of all metric objects, keyed by name.")

(defun rt-register-metric (metric)
  (setf (gethash (typecase metric
                   (rt-counter (rt-counter-name metric))
                   (rt-histogram (rt-histogram-name metric))
                   (rt-gauge (rt-gauge-name metric))
                   (t "unknown"))
                 *rt-metrics-registry*)
        metric))

(defun rt-metrics-format-prometheus (&optional (stream *standard-output*))
  "Write Prometheus text format to STREAM."
  (maphash (lambda (name metric)
             (declare (ignore name))
             (typecase metric
               (rt-counter
                (format stream "# TYPE ~a counter~%" (rt-counter-name metric))
                (format stream "~a ~d~%" (rt-counter-name metric)
                        (rt-counter-value metric)))
               (rt-histogram
                (format stream "# TYPE ~a histogram~%" (rt-histogram-name metric))
                (loop for b in (rt-histogram-buckets metric)
                      for c in (rt-histogram-counts metric)
                      do (format stream "~a_bucket{le=\"~a\"} ~d~%"
                                 (rt-histogram-name metric) b c))
                (format stream "~a_bucket{le=\"+Inf\"} ~d~%"
                        (rt-histogram-name metric)
                        (rt-histogram-count metric))
                (format stream "~a_sum ~f~%" (rt-histogram-name metric)
                        (rt-histogram-sum metric))
                (format stream "~a_count ~d~%" (rt-histogram-name metric)
                        (rt-histogram-count metric)))
               (rt-gauge
                (format stream "# TYPE ~a gauge~%" (rt-gauge-name metric))
                (format stream "~a ~f~%" (rt-gauge-name metric)
                        (rt-gauge-value metric)))))
           *rt-metrics-registry*))

;; ── Public API aliases ─────────────────────────────────────────────────

(defun make-counter (name &key labels)
  (rt-make-counter name :labels labels))
(defun make-gauge (name) (rt-make-gauge name))
(defun make-histogram (name buckets) (rt-make-histogram name :buckets buckets))
(defun increment! (counter &optional (n 1)) (rt-counter-increment! counter n))
(defun set-gauge! (gauge val) (rt-gauge-set! gauge val))
(defun observe! (histogram value) (rt-histogram-observe! histogram value))
(defun prometheus-text-format (metrics)
  "Return Prometheus text for METRICS list."
  (with-output-to-string (s)
    (dolist (m metrics)
      (typecase m
        (rt-counter
         (format s "# TYPE ~a counter~%" (rt-counter-name m))
         (let ((lbls (rt-counter-labels m)))
           (if lbls
               (format s "~a{~{~a=\"~a\"~^,~}} ~d~%"
                       (rt-counter-name m) lbls (rt-counter-value m))
               (format s "~a ~d~%" (rt-counter-name m) (rt-counter-value m)))))
        (rt-histogram
         (format s "# TYPE ~a histogram~%" (rt-histogram-name m))
         (loop for b in (rt-histogram-buckets m)
               for c in (rt-histogram-counts m)
               do (format s "~a_bucket{le=\"~a\"} ~d~%"
                          (rt-histogram-name m) b c))
         (format s "~a_bucket{le=\"+Inf\"} ~d~%"
                 (rt-histogram-name m) (rt-histogram-count m))
         (format s "~a_sum ~f~%" (rt-histogram-name m) (rt-histogram-sum m))
         (format s "~a_count ~d~%" (rt-histogram-name m) (rt-histogram-count m)))
        (rt-gauge
         (format s "# TYPE ~a gauge~%" (rt-gauge-name m))
         (format s "~a ~f~%" (rt-gauge-name m) (rt-gauge-value m)))))))
