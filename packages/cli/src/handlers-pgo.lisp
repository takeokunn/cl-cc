;;;; cli/src/handlers-pgo.lisp — PGO/Profiling Helpers
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;
;;; Profile-guided optimisation helpers: sanitizer flag threading,
;;; instruction-profile collection, PGO profile writing, JIT cache stats,
;;; and the collapsed-stack flamegraph reporter.
;;;
;;; Extracted from handlers.lisp as part of the 2026 modernization pass.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(in-package :cl-cc/cli)

(defun %call-with-runtime-sanitizer-flags (opts thunk)
  "Execute THUNK with runtime sanitizer toggles derived from OPTS."
  (let ((cl-cc/runtime::*rt-asan-enabled* (not (null (compile-opts-asan opts))))
        (cl-cc/runtime::*rt-msan-enabled* (not (null (compile-opts-msan opts))))
        (cl-cc/runtime::*rt-tsan-enabled* (not (null (compile-opts-tsan opts))))
        (cl-cc/runtime::*rt-hwasan-enabled* (not (null (compile-opts-hwasan opts))))
        (cl-cc/runtime::*rt-ubsan-enabled* (not (null (compile-opts-ubsan opts))))
        (cl-cc/runtime:*gc-young-size-words*
         (or (compile-opts-gc-min-heap opts)
             cl-cc/runtime:*gc-young-size-words*))
        (cl-cc/runtime:*gc-old-size-words*
         (or (compile-opts-gc-max-heap opts)
             cl-cc/runtime:*gc-old-size-words*))
        (cl-cc/parse::*werror-p* (not (null (compile-opts-werror opts)))))
    (funcall thunk)))

(defun %pgo-profile-instructions (result)
  "Return instruction list to profile from RESULT, preferring optimized stream."
  (or (cl-cc/compile:compilation-result-optimized-instructions result)
      (cl-cc/compile:compilation-result-vm-instructions result)
      (cl-cc/vm:vm-program-instructions (cl-cc/compile:compilation-result-program result))))

(defun %write-pgo-profile (path result &optional vm-state)
  "Write a lightweight PGO profile for RESULT to PATH.

RESULT supplies instruction streams and the optional counter plan. VM-STATE,
when provided, contributes runtime basic-block, branch, and counter counts.
The file is written as a readable plist-like form and PATH's parent directory
is created as needed."
  (let ((counts (make-hash-table :test #'equal))
         (insts (%pgo-profile-instructions result))
         (bb (and vm-state (cl-cc/vm:vm-get-profile-bb-counts vm-state)))
         (branches (and vm-state (cl-cc/vm:vm-get-profile-branch-counts vm-state)))
         (calls (and vm-state (cl-cc/vm:vm-get-profile-call-counts vm-state)))
         (type-feedback (and vm-state (cl-cc/vm:vm-get-profile-type-feedback vm-state)))
        (counter-plan (cl-cc/compile:compilation-result-pgo-counter-plan result))
        (counter-template nil)
        (bb-counter-counts nil)
        (edge-counter-counts nil))
    (when counter-plan
      (setf counter-template (cl-cc/optimize:opt-pgo-make-profile-template counter-plan))
      (setf bb-counter-counts
            (loop for (bb-id . pc) in (getf counter-plan :bb-runtime-keys)
                  collect (cons bb-id (if bb (gethash pc bb 0) 0))))
      (setf edge-counter-counts
            (loop for (edge-id . runtime-key) in (getf counter-plan :edge-runtime-keys)
                  collect (cons edge-id (if branches (gethash runtime-key branches 0) 0)))))
    (dolist (inst insts)
      (let ((op (string-upcase (symbol-name (type-of inst)))))
        (incf (gethash op counts 0))))
    (ensure-directories-exist path)
    (with-open-file (out path
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (flet ((write-ht-section (key ht)
               (format out " ~A (~%" key)
               (when ht
                 (maphash (lambda (k v) (format out "   (~S . ~D)~%" k v)) ht))
               (format out " )~%")))
        (format out "(:format :cl-cc-pgo-v1~%")
        (format out " :total-instructions ~D~%" (length insts))
        (write-ht-section ":op-counts"            counts)
        (write-ht-section ":bb-counts"            bb)
        (write-ht-section ":branch-counts"        branches)
        (write-ht-section ":function-call-counts" calls)
        (write-ht-section ":type-feedback"        type-feedback)
        (when counter-plan
          (format out " :counter-plan ~S~%"         counter-plan)
          (format out " :counter-template ~S~%"     counter-template)
          (format out " :bb-counter-counts ~S~%"    bb-counter-counts)
          (format out " :edge-counter-counts ~S~%"  edge-counter-counts))
        (format out " )~%")))))

(defun %print-jit-cache-stats (&optional (stream *standard-output*))
  "Print runtime JIT code-cache statistics when requested by the CLI."
  (let ((stats (cl-cc/runtime:rt-code-cache-stats)))
    (format stream "JIT code cache: size=~D capacity=~D entries=~D hits=~D misses=~D hit-rate=~,2F%% evictions=~D~%"
            (getf stats :size)
            (getf stats :capacity)
            (getf stats :entries)
            (getf stats :hits)
            (getf stats :misses)
            (* 100.0 (getf stats :hit-rate))
            (getf stats :evictions))))

(defun %maybe-print-jit-cache-stats (opts)
  "Print JIT cache stats when --jit-cache-stats is set."
  (when (compile-opts-jit-cache-stats opts)
    (%print-jit-cache-stats)))

(defun %maybe-write-pgo-profile (opts result &optional vm-state)
  "Emit a profile file when --pgo-generate is set."
  (let ((path (compile-opts-pgo-generate-path opts)))
    (when path
      (%write-pgo-profile path result vm-state))))

(defun %write-selfhost-instruction-profile (&optional (path *selfhost-profile-path*))
  "Write the self-hosting VM instruction histogram to PATH."
  (cl-cc/vm:vm-write-instruction-profile path)
  (format *error-output* "; cl-cc selfhost: wrote VM instruction profile to ~A~%" path))

(defun print-profile (vm-state &optional (stream *standard-output*))
  "Print a simple collapsed-stack profile report for VM-STATE."
  (let ((samples (and vm-state (cl-cc/vm:vm-get-profile-samples vm-state))))
    (when samples
      (format stream "~&Profile samples:~%")
      (let ((rows nil))
        (maphash (lambda (stack count) (push (cons stack count) rows)) samples)
        (dolist (row (sort rows #'> :key #'cdr))
          (format stream "~D ~A~%" (cdr row) (car row)))))))
