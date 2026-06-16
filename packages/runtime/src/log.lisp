(in-package :cl-cc/runtime)

;;; Basic log levels
(defconstant +log-level-trace+ 0)
(defconstant +log-level-debug+ 1)
(defconstant +log-level-info+  2)
(defconstant +log-level-warn+  3)
(defconstant +log-level-error+ 4)

;;; Public dynamic variables
(defvar *log-level*       +log-level-info+)
(defvar *log-output*      *standard-output*)
(defvar *log-json-output* nil)
(defvar *log-context*     nil)

;;; JSON helpers
(defun %log-escape-string (s)
  (with-output-to-string (out)
    (loop for c across s do
      (cond ((char= c #\") (write-string "\\\"" out))
            ((char= c #\\) (write-string "\\\\" out))
            ((char= c #\Newline) (write-string "\\n" out))
            (t (write-char c out))))))

(defun %log-level-name (level)
  (cond ((= level +log-level-trace+) "trace")
        ((= level +log-level-debug+) "debug")
        ((= level +log-level-info+)  "info")
        ((= level +log-level-warn+)  "warn")
        ((= level +log-level-error+) "error")
        (t (format nil "level~D" level))))

(defun %log-key-string (key)
  "Return KEY as a lowercase string for JSON field names."
  (string-downcase (format nil "~A" key)))

(defun %log-emit-json (level message attrs)
  (let ((out *log-output*))
    (write-char #\{ out)
    (format out "\"level\":\"~A\"" (%log-level-name level))
    (format out ",\"message\":\"~A\"" (%log-escape-string (format nil "~A" message)))
    (format out ",\"time\":\"~A\"" (get-universal-time))
    (dolist (pair *log-context*)
      (format out ",\"~A\":\"~A\"" (%log-escape-string (%log-key-string (car pair)))
              (%log-escape-string (format nil "~A" (cdr pair)))))
    (loop for (k v) on attrs by #'cddr do
      (format out ",\"~A\":\"~A\"" (%log-escape-string (%log-key-string k))
              (%log-escape-string (format nil "~A" v))))
    (write-string "}" out)
    (terpri out)
    (force-output out)))

(defun %log-emit-text (level message attrs)
  (let ((out *log-output*))
    (format out "[~A] ~A" (%log-level-name level) message)
    (loop for (k v) on attrs by #'cddr do
      (format out " ~A=~A" k v))
    (terpri out)
    (force-output out)))

(defun rt-log (level msg &key attrs)
  (when (>= level *log-level*)
    (if *log-json-output*
        (%log-emit-json level msg attrs)
        (%log-emit-text level msg attrs)))
  (when (and (>= level *log-level*)
             (not *log-json-output*))
    (format *log-output* "[~D] ~A~%" level msg)
    (force-output *log-output*)))

(defun log-trace (message &rest attrs) (when (<= *log-level* +log-level-trace+) (rt-log +log-level-trace+ message :attrs attrs)))
(defun log-debug (message &rest attrs) (when (<= *log-level* +log-level-debug+) (rt-log +log-level-debug+ message :attrs attrs)))
(defun log-info  (message &rest attrs) (when (<= *log-level* +log-level-info+)  (rt-log +log-level-info+  message :attrs attrs)))
(defun log-warn  (message &rest attrs) (when (<= *log-level* +log-level-warn+)  (rt-log +log-level-warn+  message :attrs attrs)))
(defun log-error (message &rest attrs) (when (<= *log-level* +log-level-error+) (rt-log +log-level-error+ message :attrs attrs)))

(defmacro with-log-context (bindings &body body)
  "Bind logging context entries for the duration of BODY.
BINDINGS is a list of (key value) pairs."
  `(let ((*log-context* (append (list ,@(mapcar (lambda (b) `(cons ,(first b) ,(second b))) bindings))
                                *log-context*)))
     ,@body))
