(in-package :cl-cc/optimize)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Optimizer — Pass Pipeline, Reporting, and Public Driver
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun opt-pass-inline-iterative (instructions)
  "Thresholded inline pass used inside the convergence loop."
  (opt-pass-inline instructions :threshold :adaptive))

(defun %maybe-apply-prolog-rewrite (instructions)
  "Apply the Prolog rewrite stage when enabled, preserving INSTRUCTIONS otherwise.
The stage first applies the instruction-level Prolog peephole rules and then runs
the e-graph rewrite engine whose builtin rule set is also sourced from Prolog facts." 
  (if *enable-prolog-peephole*
      (optimize-with-egraph
       (mapcar #'sexp->instruction
               (apply-prolog-peephole (mapcar #'instruction->sexp instructions))))
      instructions))

;;; Single source of truth: ordered keyword → function pairs.
;;; *opt-convergence-passes* and *opt-pass-registry* are both derived from this.
(defparameter *opt-pass-table*
  `((:prolog-rewrite            . ,#'%maybe-apply-prolog-rewrite)
    (:egraph                    . ,#'optimize-with-egraph)
    (:inline                    . ,#'opt-pass-inline-iterative)
    (:fold                      . ,#'opt-pass-fold)
    (:sccp                      . ,#'opt-pass-sccp)
    (:strength-reduce           . ,#'opt-pass-strength-reduce)
    (:bswap-recognition         . ,#'opt-pass-bswap-recognition)
    (:rotate-recognition        . ,#'opt-pass-rotate-recognition)
    (:reassociate               . ,#'opt-pass-reassociate)
    (:copy-prop                 . ,#'opt-pass-copy-prop)
    (:gvn                       . ,#'opt-pass-gvn)
    (:batch-concatenate         . ,#'opt-pass-batch-concatenate)
    (:cse                       . ,#'opt-pass-cse)
    (:jump                      . ,#'opt-pass-jump)
    (:loop-unrolling            . ,#'opt-pass-loop-unrolling)
    (:loop-rotation             . ,#'opt-pass-loop-rotation)
    (:loop-peeling              . ,#'opt-pass-loop-peeling)
    (:code-sinking              . ,#'opt-pass-code-sinking)
    (:unreachable               . ,#'opt-pass-unreachable)
    (:dead-basic-blocks         . ,#'opt-pass-dead-basic-blocks)
    (:store-to-load-forward     . ,#'opt-pass-store-to-load-forward)
    (:dead-store-elim           . ,#'opt-pass-dead-store-elim)
    (:nil-check-elim            . ,#'opt-pass-dominated-type-check-elim)
    (:dominated-type-check-elim . ,#'opt-pass-dominated-type-check-elim)
    (:branch-correlation        . ,#'opt-pass-branch-correlation)
    (:tail-duplication          . ,#'opt-pass-tail-duplication)
    (:block-merge               . ,#'opt-pass-block-merge)
    (:tail-merge                . ,#'opt-pass-tail-merge)
    (:pre                       . ,#'opt-pass-pre)
    (:constant-hoist            . ,#'opt-pass-licm)
    (:global-dce                . ,#'opt-pass-global-dce)
    (:dead-labels               . ,#'opt-pass-dead-labels)
    (:dce                       . ,#'opt-pass-dce))
  "Ordered (keyword . function) pairs — single source for pipeline and registry.")

(defparameter *opt-pass-registry*
  (loop with ht = (make-hash-table :test #'eq)
        for (k . v) in *opt-pass-table*
        do (setf (gethash k ht) v)
        finally (return ht))
  "Keyword → pass function mapping derived from *opt-pass-table*.")

(defparameter *opt-default-convergence-pass-keys*
  '(:prolog-rewrite
    :inline
    :sccp
    :bswap-recognition
    :rotate-recognition
    :reassociate
    :copy-prop
    :gvn
    :batch-concatenate
    :cse
    :jump
    :loop-unrolling
    :loop-rotation
    :loop-peeling
    :code-sinking
    :unreachable
    :dead-basic-blocks
    :store-to-load-forward
    :dead-store-elim
    :nil-check-elim
    :dominated-type-check-elim
    :branch-correlation
    :tail-duplication
    :block-merge
    :tail-merge
    :pre
    :constant-hoist
    :global-dce
    :dead-labels
    :dce)
  "Default convergence pipeline keys.
`:egraph` remains available as an explicit pass, but the default rewrite stage is
`:prolog-rewrite`, which already composes both the Prolog peephole backend and
the e-graph engine.")

(defparameter *opt-convergence-passes*
  (mapcar (lambda (k) (gethash k *opt-pass-registry*))
          *opt-default-convergence-pass-keys*)
  "Ordered default pass functions derived from `*opt-default-convergence-pass-keys*`.")

;;; ─── Reporting / Trace State ─────────────────────────────────────────────

(defstruct (opt-reporting-options (:conc-name opt-report-))
  "Read-only bundle of side-channel reporting flags for the optimizer pipeline."
  (print-pass-timings nil)
  (timing-stream      nil)
  (print-pass-stats   nil)
  (stats-stream       nil)
  (print-opt-remarks  nil)
  (opt-remarks-stream nil)
  (opt-remarks-mode   :all))

(defstruct (opt-trace-state (:conc-name opt-trace-))
  "Mutable accumulator for Chrome-trace-compatible events."
  (enabled     nil)
  (json-stream nil)
  (events      nil)
  (ts-us        0))

(defun %opt-trim-whitespace (s)
  (string-trim '(#\Space #\Tab #\Newline #\Return) s))

(defun opt-parse-pass-pipeline-string (text)
  "Parse a comma-separated optimizer pipeline string into keyword pass names."
  (remove nil
          (mapcar (lambda (part)
                    (let ((name (%opt-trim-whitespace part)))
                      (and (> (length name) 0)
                           (intern (string-upcase name) :keyword))))
                  (uiop:split-string text :separator '(#\,)))))

(defun opt-resolve-pass-pipeline (pipeline)
  "Resolve PIPELINE into a list of pass functions."
  (cond
    ((null pipeline) *opt-convergence-passes*)
    ((stringp pipeline) (opt-resolve-pass-pipeline (opt-parse-pass-pipeline-string pipeline)))
    ((every #'functionp pipeline) pipeline)
    (t
     (mapcar (lambda (entry)
               (or (and (keywordp entry) (gethash entry *opt-pass-registry*))
                   (error "Unknown optimizer pass ~S" entry)))
             pipeline))))

(defun %opt-pass-name-string (f)
  (string-upcase (format nil "~A" f)))

(defun %opt-write-trace-json (stream events)
  "Write Chrome-trace-compatible JSON EVENTS to STREAM."
  (format stream "{\"traceEvents\":[")
  (loop for event in events
        for i from 0
        do (when (> i 0) (format stream ","))
           (format stream
                   "{\"name\":~S,\"ph\":\"X\",\"pid\":1,\"tid\":1,\"ts\":~D,\"dur\":~D}"
                   (getf event :name)
                   (getf event :ts-us)
                   (getf event :dur-us)))
  (format stream "]}~%"))

(defun %opt-remarks-applies-p (changed mode)
  "T when a remarks entry should be emitted given CHANGED status and MODE."
  (or (eq mode :all)
      (and changed      (eq mode :changed))
      (and (not changed)(eq mode :missed))))

(defun %opt-run-passes-once (prog passes reporting trace)
  "Apply PASSES once, mutating TRACE in place and emitting via REPORTING.
Returns the final instruction list."
  (let ((current prog))
    (dolist (f passes current)
      (let* ((before       current)
             (before-count (length before))
             (start        (get-internal-real-time))
             (next         (funcall f current))
             (elapsed-s    (/ (- (get-internal-real-time) start)
                              internal-time-units-per-second))
             (dur-us       (round (* elapsed-s 1000000)))
             (after-count  (length next))
             (changed      (not (opt-converged-p before next)))
             (name         (%opt-pass-name-string f)))
        (when (opt-report-print-pass-timings reporting)
          (format (opt-report-timing-stream reporting) "~A: ~,6Fs~%" f elapsed-s))
        (when (opt-report-print-pass-stats reporting)
          (format (opt-report-stats-stream reporting)
                  "~A: before=~D after=~D delta=~D changed=~A~%"
                  f before-count after-count (- after-count before-count)
                  (if changed "yes" "no")))
        (when (and (opt-report-print-opt-remarks reporting)
                   (%opt-remarks-applies-p changed (opt-report-opt-remarks-mode reporting)))
          (format (opt-report-opt-remarks-stream reporting)
                  "~A: ~A~%" f (if changed "changed" "missed")))
        (when (opt-trace-enabled trace)
          (push (list :name name :ts-us (opt-trace-ts-us trace) :dur-us dur-us)
                (opt-trace-events trace))
          (incf (opt-trace-ts-us trace) dur-us))
        (setf current next)))))

(defun opt-converged-p (prev next)
  "T if a pass-cycle produced no semantic change in the instruction stream."
  (and (= (length prev) (length next))
       (loop for lhs in prev
             for rhs in next
             always (equal (instruction->sexp lhs)
                           (instruction->sexp rhs)))))

(defparameter *opt-iteration-budget-thresholds*
  '((50  . -12)
    (150 . -6)
    (400 . 0)
    (800 . 8))
  "Instruction-count thresholds used by opt-adaptive-max-iterations.")

(defun opt-adaptive-max-iterations (instructions &key (base-iterations 20) (min-iterations 6) (max-iterations 50))
  "Return a conservative adaptive convergence budget for INSTRUCTIONS."
  (let* ((n (length instructions))
         (delta (or (cdr (find-if (lambda (entry) (< n (car entry)))
                                  *opt-iteration-budget-thresholds*))
                    15)))
    (min max-iterations
         (max min-iterations
              (+ base-iterations delta)))))

(defun opt-verify-instructions (instructions &key pass-name)
  "Conservative VM-level verifier for optimizer/debugging use."
  (let ((labels (make-hash-table :test #'equal))
        (defined (make-hash-table :test #'eq))
        (pass-name (or pass-name "<unknown-pass>")))
    (dolist (inst instructions)
      (when (typep inst 'vm-label)
        (let ((name (vm-name inst)))
          (when (gethash name labels)
            (error "~A verifier: duplicate label ~A" pass-name name))
          (setf (gethash name labels) t))))
    (dolist (inst instructions)
      (typecase inst
        ((or vm-jump vm-jump-zero)
         (unless (gethash (vm-label-name inst) labels)
           (error "~A verifier: unknown label target ~A" pass-name (vm-label-name inst)))))
      (dolist (reg (opt-inst-read-regs inst))
        (unless (gethash reg defined)
          (error "~A verifier: register ~A used before definition in ~S"
                 pass-name reg (instruction->sexp inst))))
      (let ((dst (opt-inst-dst inst)))
        (when dst
          (setf (gethash dst defined) t))))
    t))

(defvar *skip-optimizer-passes* nil
  "When non-NIL, optimize-instructions returns its input unchanged.")

(defvar *verify-optimizer-instructions* nil
  "When non-NIL, run opt-verify-instructions after every convergence pass to
catch ill-formed sequences (duplicate labels, unknown jump targets, use-before-define).")

(defun optimize-instructions (instructions &key (max-iterations 20) pass-pipeline
                                                print-pass-timings timing-stream
                                                print-pass-stats   stats-stream
                                                print-opt-remarks  opt-remarks-stream
                                                (opt-remarks-mode :all)
                                                trace-json-stream)
  "Run the full multi-pass optimization pipeline on a VM instruction sequence.
Iterates until convergence or MAX-ITERATIONS. Returns optimized instructions.
When *skip-optimizer-passes* is non-NIL, returns instructions unchanged."
  (when *skip-optimizer-passes*
    (return-from optimize-instructions (values instructions nil)))
  (let* ((reporting (make-opt-reporting-options
                     :print-pass-timings print-pass-timings
                     :timing-stream      (or timing-stream *standard-output*)
                     :print-pass-stats   print-pass-stats
                     :stats-stream       (or stats-stream *standard-output*)
                     :print-opt-remarks  print-opt-remarks
                     :opt-remarks-stream (or opt-remarks-stream *standard-output*)
                     :opt-remarks-mode   opt-remarks-mode))
         (trace     (make-opt-trace-state
                     :enabled     (not (null trace-json-stream))
                     :json-stream trace-json-stream))
         (prog      instructions)
         (max-iter  (if (eq max-iterations :adaptive)
                        (opt-adaptive-max-iterations instructions)
                        max-iterations))
         (passes    (opt-resolve-pass-pipeline pass-pipeline)))
    (loop repeat max-iter
          for prev = prog
          do (setf prog (%opt-run-passes-once prog passes reporting trace))
             (when *verify-optimizer-instructions*
               (opt-verify-instructions prog))
          when (opt-converged-p prev prog)
          return prog)
    (when trace-json-stream
      (%opt-write-trace-json trace-json-stream (nreverse (opt-trace-events trace))))
    (opt-pass-leaf-detect prog)))
