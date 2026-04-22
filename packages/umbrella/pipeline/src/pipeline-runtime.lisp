(in-package :cl-cc)
;;;; Pipeline — Runtime entrypoints and VM execution helpers

(defun %copy-snapshot-ht (src)
  (let ((dst (make-hash-table :test #'eq :size (+ (hash-table-count src) 8))))
    (maphash (lambda (k v) (setf (gethash k dst) v)) src)
    dst))

(defvar *run-string-cps-fast-path-hook* nil
  "Optional test hook called with (SOURCE FORM VALUE) when RUN-STRING returns via the CPS fast path.")

(defun %try-run-string-cps-fast-path (source stdlib pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream print-pass-stats stats-stream trace-json-stream)
  "Return (values result t) when SOURCE can be executed through the CPS host fast path.
The fast path is intentionally limited to single-form, non-stdlib Lisp inputs so
definition forms and stdlib-dependent execution continue through the established VM pipeline."
  (if (or stdlib
          pass-pipeline
          print-pass-timings
          timing-stream
          print-opt-remarks
          opt-remarks-stream
          print-pass-stats
          stats-stream
          trace-json-stream)
      (values nil nil)
      (let ((forms (parse-source-for-language source :lisp)))
        (if (and (= (length forms) 1)
                 (not (and (consp (first forms)) (eq (caar forms) 'in-package))))
            (let ((form (first forms)))
              (multiple-value-bind (value ok)
                  (%try-cps-host-eval form)
                (when (and ok *run-string-cps-fast-path-hook*)
                  (funcall *run-string-cps-fast-path-hook* source form value))
                (values value ok)))
            (values nil nil)))))

(defun run-string (source &key stdlib pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  "Compile and run SOURCE. When STDLIB is true, include standard library."
  (let ((*package*          (find-package :cl-cc))
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
                              :opt-remarks-mode    opt-remarks-mode)))
    (multiple-value-bind (cps-value cps-ok)
        (%try-run-string-cps-fast-path source stdlib
                                       pass-pipeline print-pass-timings timing-stream
                                       print-opt-remarks opt-remarks-stream
                                       print-pass-stats stats-stream trace-json-stream)
      (if cps-ok
          cps-value
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
                  (run-compiled program))))))))
