(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Timeout normalization and registry mutation helpers
;;; ------------------------------------------------------------

(defun %normalize-positive-timeout (timeout)
  "Return TIMEOUT when it is a positive real number, otherwise NIL."
  (and (typep timeout 'real)
       (plusp timeout)
       timeout))

(defun %default-test-timeout ()
  "Return the default per-test timeout in seconds."
  (let ((raw (uiop:getenv "CLCC_TEST_TIMEOUT")))
    (or (and raw
             (ignore-errors
               (%normalize-positive-timeout
                (parse-integer raw))))
        10)))

(defun %effective-test-timeout (test-plist)
  "Return the effective timeout for TEST-PLIST.
Invalid or missing per-test timeouts fall back to `%default-test-timeout'."
  (or (%normalize-positive-timeout (getf test-plist :timeout))
      (%default-test-timeout)))

(defun %suite-descends-from-p (suite-name ancestor-suite)
  "Return T when SUITE-NAME is ANCESTOR-SUITE or any of its descendants."
  (loop with current = suite-name
        while current
        for entry = (persist-lookup *suite-registry* current)
        do (when (eq current ancestor-suite)
             (return t))
           (setf current (and entry (getf entry :parent)))
        finally (return nil)))

(defun %map-test-registry (fn)
  "Rebuild `*test-registry*' by applying FN to each registered test plist."
  (let ((updated (persist-empty)))
    (persist-each *test-registry*
                  (lambda (name plist)
                    (setf updated (persist-assoc updated name (funcall fn name plist)))))
    (setf *test-registry* updated)))

(defun %apply-timeout-to-test-plist (plist timeout overwrite)
  "Return a timeout-adjusted copy of PLIST.
When OVERWRITE is NIL, explicit positive timeouts already on PLIST are preserved."
  (let ((existing (%normalize-positive-timeout (getf plist :timeout))))
    (if (and existing (not overwrite))
        plist
        (let ((new-plist (copy-list plist)))
          (setf (getf new-plist :timeout) timeout)
          new-plist))))

(defun set-test-timeouts! (test-names timeout &key overwrite)
  "Set TIMEOUT for each registered test named in TEST-NAMES.
TIMEOUT must be a positive real number. By default, existing explicit per-test
timeouts are preserved."
  (let ((normalized (%normalize-positive-timeout timeout)))
    (unless normalized
      (error "Timeout must be a positive real number: ~S" timeout))
    (%map-test-registry
     (lambda (name plist)
       (if (member name test-names :test #'eq)
           (%apply-timeout-to-test-plist plist normalized overwrite)
           plist)))))

(defun set-test-timeouts-by-prefix! (prefix timeout &key overwrite)
  "Set TIMEOUT for every registered test whose symbol-name starts with PREFIX.
TIMEOUT must be a positive real number. By default, existing explicit per-test
timeouts are preserved."
  (let ((normalized (%normalize-positive-timeout timeout)))
    (unless normalized
      (error "Timeout must be a positive real number: ~S" timeout))
    (%map-test-registry
     (lambda (name plist)
       (if (and (symbolp name)
                (let ((name-str (symbol-name name))
                      (prefix-len (length prefix)))
                  (and (<= prefix-len (length name-str))
                       (string= prefix name-str :end2 prefix-len))))
           (%apply-timeout-to-test-plist plist normalized overwrite)
           plist)))))

(defun set-suite-test-timeout! (suite-name timeout &key recursive overwrite)
  "Set TIMEOUT for tests directly in SUITE-NAME.
TIMEOUT must be a positive real number. When RECURSIVE is true, descendant
suite tests are updated as well. By default, existing explicit per-test
timeouts are preserved."
  (let ((normalized (%normalize-positive-timeout timeout)))
    (unless normalized
      (error "Timeout must be a positive real number: ~S" timeout))
    (%map-test-registry
     (lambda (_name plist)
       (declare (ignore _name))
       (let ((suite (getf plist :suite)))
         (if (if recursive
                 (%suite-descends-from-p suite suite-name)
                 (eq suite suite-name))
             (%apply-timeout-to-test-plist plist normalized overwrite)
             plist))))))
