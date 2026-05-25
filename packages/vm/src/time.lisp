(in-package :cl-cc/vm)

(export '(get-universal-time
          get-internal-real-time
          get-internal-run-time
          internal-time-units-per-second
          sleep
          encode-universal-time
          decode-universal-time
          time
          with-timeout))

(defconstant internal-time-units-per-second cl:internal-time-units-per-second
  "Number of internal-time units in one second for the host-backed VM clock.")

(defun get-universal-time ()
  "Return the current VM universal time in seconds since 1900-01-01 UTC."
  (if *build-seed*
      (build-timestamp)
      (cl:get-universal-time)))

(defun get-internal-real-time ()
  "Return monotonically increasing VM real time in internal-time units."
  (cl:get-internal-real-time))

(defun get-internal-run-time ()
  "Return process CPU run time in internal-time units."
  (cl:get-internal-run-time))

(defun sleep (seconds)
  "Suspend execution for SECONDS, matching Common Lisp SLEEP semantics."
  (check-type seconds (real 0 *))
  (cl:sleep seconds)
  nil)

(defun encode-universal-time (second minute hour date month year &optional time-zone)
  "Encode decoded time fields into a universal time.
TIME-ZONE is the Common Lisp timezone offset in hours west of GMT."
  (if time-zone
      (cl:encode-universal-time second minute hour date month year time-zone)
      (cl:encode-universal-time second minute hour date month year)))

(defun decode-universal-time (universal-time &optional time-zone)
  "Decode UNIVERSAL-TIME into second, minute, hour, date, month, year,
day-of-week, daylight-saving-p, and timezone values."
  (if time-zone
      (cl:decode-universal-time universal-time time-zone)
      (cl:decode-universal-time universal-time)))

(defmacro time (form)
  "Evaluate FORM and print elapsed real time to *TRACE-OUTPUT*.
Returns exactly the values produced by FORM."
  (let ((start (gensym "START-"))
        (values (gensym "VALUES-"))
        (elapsed (gensym "ELAPSED-")))
    `(let ((,start (get-internal-real-time)))
       (let ((,values (multiple-value-list ,form)))
          (let ((,elapsed (/ (- (get-internal-real-time) ,start)
                             (float internal-time-units-per-second))))
            (format *trace-output* "~&; Evaluation took ~,6F seconds.~%" ,elapsed))
          (values-list ,values)))))

(defmacro with-timeout ((seconds &optional timeout-result) &body body)
  "Evaluate BODY with a host-backed timeout where supported.
On timeout, return TIMEOUT-RESULT.  This mirrors SBCL's WITH-TIMEOUT in the VM
  surface while keeping a portable fallback for non-SBCL hosts."
  #+sbcl
  `(sb-ext:with-timeout (,seconds ,timeout-result)
     ,@body)
  #-sbcl
  (declare (ignore seconds))
  #-sbcl
  `(progn ,@body))
