(in-package :cl-cc/pipeline)
;;;; Pipeline — Runtime entrypoints and VM execution helpers

(defun %copy-snapshot-ht (src)
  "Return an EQ hash-table copy of SRC for mutable per-run registry state.

The returned table has the same keys and values as SRC but can be mutated by a
single run-string invocation without changing the stdlib snapshot."
  (let ((dst (make-hash-table :test #'eq :size (+ (hash-table-count src) 8))))
    (maphash (lambda (k v) (setf (gethash k dst) v)) src)
    dst))

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
                 (*defstruct-slot-registry* (%copy-snapshot-ht *stdlib-defstruct-slot-registry*))
                 (result  (apply #'compile-string source compile-kwargs))
                 (program (compilation-result-program result))
                 (state   (clone-vm-state *stdlib-vm-snapshot*)))
            (run-compiled program :state state))
          (let* ((*accessor-slot-map*       (make-hash-table :test #'eq))
                 (*defstruct-slot-registry* (make-hash-table :test #'eq))
                 (compile-fn (if stdlib #'compile-string-with-stdlib #'compile-string))
                 (result     (apply compile-fn source compile-kwargs))
                 (program    (compilation-result-program result)))
            (run-compiled program))))))
