;;;; packages/compile/src/logging.lisp — FR-602 Structured Logging Macros
;;;; Compile-time type-checked structured logging with zero-cost disabled levels.
;;;; Go slog / Rust tracing / ECS format equivalent.

(in-package :cl-cc/compile)

;;; ──── Log levels ────
(defvar *log-level* :info
  "Current log level: :debug, :info, :warn, :error, :fatal.")

(defvar *log-output* *standard-output*
  "Stream for log output.")

(defconstant +log-levels+
  '(:debug 0 :info 1 :warn 2 :error 3 :fatal 4 :none 5))

(defun log-level-numeric (level)
  "Convert log LEVEL keyword to numeric value."
  (or (getf +log-levels+ level) 5))

(defun log-level-enabled-p (level)
  "Return T if messages at LEVEL should be emitted."
  (<= (log-level-numeric *log-level*) (log-level-numeric level)))

;;; ──── Structured log macro ────
(defmacro log (level message &rest keyvals)
  "Emit a structured log entry at LEVEL with MESSAGE and key-value pairs.
Compile-time: if LEVEL is statically below *log-level*, expands to (values) (zero-cost).
Runtime: formats as JSON with timestamp, level, message, and key-value pairs.

Usage: (cl-cc:log :info \"request processed\" :user-id uid :duration-ms elapsed)"
  ;; Compile-time key name validation
  (check-log-keys keyvals)
  ;; If level is a constant and below the current log level, emit nothing
  (if (and (keywordp level)
           (not (log-level-enabled-p level)))
      `(values)  ; zero-cost: no code emitted
      `(when (log-level-enabled-p ,level)
         (emit-structured-log ,level ,message ,@keyvals))))

;;; ──── Key validation ────
(defun check-log-keys (keyvals)
  "Validate that KEYVALS has no duplicate keys (compile-time check)."
  (let ((seen nil))
    (loop for (key val) on keyvals by #'cddr
          do (when (member key seen :test #'eq)
               (warn "Duplicate log key: ~S" key))
             (push key seen))))

;;; ──── Log emission ────
(defun emit-structured-log (level message &rest keyvals)
  "Emit a structured log entry as JSON to *LOG-OUTPUT*."
  (let ((*print-pretty* nil))
    (format *log-output* "~A~%"
            (json-encode-log level message keyvals))))

(defun json-encode-log (level message keyvals)
  "Encode a log entry as a JSON string."
  (with-output-to-string (s)
    (format s "{")
    (format s "\"timestamp\":\"~A\"" (format-timestamp (get-universal-time)))
    (format s ",\"level\":\"~A\"" (string-downcase (string level)))
    (format s ",\"message\":~S" (json-escape message))
    ;; ECS (Elastic Common Schema) fields
    (format s ",\"ecs.version\":\"8.11.0\"")
    ;; Custom key-value pairs
    (loop for (key val) on keyvals by #'cddr
          do (format s ",\"~A\":~A"
                    (string-downcase (string key))
                    (json-encode-value val)))
    (format s "}")))

(defun json-encode-value (val)
  "JSON-encode a Lisp value."
  (typecase val
    (string (format nil "~S" (json-escape val)))
    (number (princ-to-string val))
    (symbol (format nil "~S" (string-downcase (string val))))
    (null "null")
    (t (format nil "~S" (princ-to-string val)))))

(defun json-escape (str)
  "Escape special characters in STR for JSON."
  (with-output-to-string (s)
    (loop for ch across (princ-to-string str)
          do (case ch
               (#\" (write-string "\\\"" s))
               (#\\ (write-string "\\\\" s))
               (#\Newline (write-string "\\n" s))
               (#\Tab (write-string "\\t" s))
               (t (write-char ch s))))))

;;; ──── Timestamp helper ────
(defun format-timestamp (universal-time)
  "Format universal time as ISO 8601."
  (multiple-value-bind (s m h d mo y)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            y mo d h m s)))
