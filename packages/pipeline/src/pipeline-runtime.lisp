(in-package :cl-cc/pipeline)
;;;; Pipeline — Runtime entrypoints and VM execution helpers

(defun %copy-snapshot-ht (src)
  "Return an EQ hash-table copy of SRC for mutable per-run registry state.

The returned table has the same keys and values as SRC but can be mutated by a
single run-string invocation without changing the stdlib snapshot."
  (let ((dst (make-hash-table :test #'eq :size (+ (hash-table-count src) 8))))
    (maphash (lambda (k v) (setf (gethash k dst) v)) src)
    dst))

(defun %pipeline-runtime-function (name)
  "Return the CL-CC/RUNTIME function named NAME, or NIL when unavailable."
  (let ((package (find-package "CL-CC/RUNTIME")))
    (multiple-value-bind (symbol status)
        (if package
            (find-symbol name package)
            (values nil nil))
      (when (and status symbol (fboundp symbol))
        (symbol-function symbol)))))

(defun %boxed-value-boundary-p (value)
  "Return true for unambiguous NaN-boxed immediates/pointers crossing to host.

Host VM execution stores ordinary CL values in registers, so fixnum/double bit
patterns are intentionally not decoded here: ordinary integers can share those
patterns.  SSO strings, singleton values, characters, and pointers have
distinct tags and are safe to normalize at the run-string boundary."
  (and (typep value '(unsigned-byte 64))
       (let ((val-sso-string-p (%pipeline-runtime-function "VAL-SSO-STRING-P"))
             (val-nil-p        (%pipeline-runtime-function "VAL-NIL-P"))
             (val-t-p          (%pipeline-runtime-function "VAL-T-P"))
             (val-char-p       (%pipeline-runtime-function "VAL-CHAR-P"))
             (val-unbound-p    (%pipeline-runtime-function "VAL-UNBOUND-P"))
             (val-pointer-p    (%pipeline-runtime-function "VAL-POINTER-P")))
         (or (and val-sso-string-p (funcall val-sso-string-p value))
             (and val-nil-p        (funcall val-nil-p value))
             (and val-t-p          (funcall val-t-p value))
             (and val-char-p       (funcall val-char-p value))
             (and val-unbound-p    (funcall val-unbound-p value))
             (and val-pointer-p    (funcall val-pointer-p value))))))

(defun %normalize-vm-boundary-value (state value)
  "Convert VM boundary VALUE to the host CL representation expected by tests."
  (cond
    ((and (typep value '(unsigned-byte 64))
          (fboundp 'cl-cc/vm::%vm-managed-cons-pointer-p)
          (cl-cc/vm::%vm-managed-cons-pointer-p value))
     (cl-cc/vm::%vm-managed-tree-materialize state value))
    ((consp value)
     (cons (%normalize-vm-boundary-value state (car value))
           (%normalize-vm-boundary-value state (cdr value))))
    ((%boxed-value-boundary-p value)
     (let ((val->cl-value (%pipeline-runtime-function "VAL->CL-VALUE")))
       (if val->cl-value
           (funcall val->cl-value value)
           value)))
    (t value)))

(defun %run-compiled-for-run-string (program state)
  "Run PROGRAM and normalize VM boxed results as they leave RUN-STRING."
  (%normalize-vm-boundary-value state (run-compiled program :state state)))

(defun run-string (source &key stdlib pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream (inline-threshold-scale 1))
  "Compile and execute Lisp SOURCE in the VM, returning the program result.

When STDLIB is true, warm or reuse the stdlib VM snapshot and copy stdlib
compile-time registries before compiling SOURCE. Pass/remark/statistics keyword
arguments are forwarded to COMPILE-STRING or COMPILE-STRING-WITH-STDLIB."
  (let ((*package*          (or (find-package :cl-cc) *package*))
        (*labels-boxed-fns* nil)
        (compile-kwargs (list :target :vm
                              :pass-pipeline       pass-pipeline
                              :print-pass-timings  print-pass-timings
                              :timing-stream       timing-stream
                              :print-pass-stats    print-pass-stats
                              :stats-stream        stats-stream
                               :trace-json-stream   trace-json-stream
                               :print-opt-remarks   print-opt-remarks
                               :opt-remarks-stream  opt-remarks-stream
                               :opt-remarks-mode    opt-remarks-mode
                               :inline-threshold-scale inline-threshold-scale)))
    (progn
      (when (and stdlib (null *stdlib-vm-snapshot*))
        (warm-stdlib-cache))
      (if (and stdlib *stdlib-vm-snapshot*)
           (let* ((*accessor-slot-map*       (%copy-snapshot-ht *stdlib-accessor-slot-map*))
                  (*defstruct-read-only-accessor-map* (%copy-snapshot-ht *stdlib-defstruct-read-only-accessor-map*))
                  (*defstruct-slot-registry* (%copy-snapshot-ht *stdlib-defstruct-slot-registry*))
                   (*defstruct-type-registry* (%copy-snapshot-ht *stdlib-defstruct-type-registry*))
                   (*setf-compound-place-handlers* (%copy-snapshot-ht *stdlib-setf-compound-place-handlers*))
                   (result  (apply #'compile-string source compile-kwargs))
                   (program (compilation-result-program result))
                   (state   (clone-vm-state *stdlib-vm-snapshot*)))
              (%run-compiled-for-run-string program state))
            (let* ((*accessor-slot-map*       (make-hash-table :test #'eq))
                   (*defstruct-read-only-accessor-map* (make-hash-table :test #'eq))
                   (*defstruct-slot-registry* (make-hash-table :test #'eq))
                   (*defstruct-type-registry* (make-hash-table :test #'eq))
                   (*setf-compound-place-handlers* (%copy-snapshot-ht *setf-compound-place-handlers*))
                   (compile-fn (if stdlib #'compile-string-with-stdlib #'compile-string))
                  (result     (apply compile-fn source compile-kwargs))
                  (program    (compilation-result-program result))
                  (state      (make-vm-state)))
             (%run-compiled-for-run-string program state))))))
