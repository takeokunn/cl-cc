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
    (:unreachable               . ,#'opt-pass-unreachable)
    (:dead-basic-blocks         . ,#'opt-pass-dead-basic-blocks)
    (:store-to-load-forward     . ,#'opt-pass-store-to-load-forward)
    (:dead-store-elim           . ,#'opt-pass-dead-store-elim)
    (:nil-check-elim            . ,#'opt-pass-dominated-type-check-elim)
    (:dominated-type-check-elim . ,#'opt-pass-dominated-type-check-elim)
    (:branch-correlation        . ,#'opt-pass-branch-correlation)
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
    :unreachable
    :dead-basic-blocks
    :store-to-load-forward
    :dead-store-elim
    :nil-check-elim
    :dominated-type-check-elim
    :branch-correlation
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
  "Ordered default pass functions derived from `*opt-default-convergence-pass-keys*`." )

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

(defun opt-run-passes-once-with-reporting (prog passes &key print-pass-timings timing-stream print-pass-stats stats-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) trace-enabled trace-events initial-ts-us)
  "Apply PASSES once while emitting any enabled reports."
  (let ((current prog)
        (events trace-events)
        (ts-us initial-ts-us))
    (dolist (f passes)
      (let* ((before current)
             (before-count (length before))
             (start (get-internal-real-time))
             (next (funcall f current))
             (elapsed-seconds (/ (- (get-internal-real-time) start)
                                 internal-time-units-per-second))
             (dur-us (round (* elapsed-seconds 1000000)))
             (after-count (length next))
             (changed (not (opt-converged-p before next)))
             (name (%opt-pass-name-string f)))
        (when print-pass-timings
          (format timing-stream "~A: ~,6Fs~%" f elapsed-seconds))
        (when print-pass-stats
          (format stats-stream "~A: before=~D after=~D delta=~D changed=~A~%"
                  f before-count after-count (- after-count before-count)
                  (if changed "yes" "no")))
        (when (and print-opt-remarks
                   (or (eq opt-remarks-mode :all)
                       (and changed (eq opt-remarks-mode :changed))
                       (and (not changed) (eq opt-remarks-mode :missed))))
          (format opt-remarks-stream "~A: ~A~%" f (if changed "changed" "missed")))
        (when trace-enabled
          (push (list :name name :ts-us ts-us :dur-us dur-us) events)
          (incf ts-us dur-us))
        (setf current next)))
    (values current events ts-us)))

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

(defun optimize-instructions (instructions &key (max-iterations 20) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  "Run the full multi-pass optimization pipeline on a VM instruction sequence.
Runs until no changes or MAX-ITERATIONS reached.
When *skip-optimizer-passes* is non-NIL, returns (values instructions nil)
immediately without running any passes."
  (when *skip-optimizer-passes*
    (return-from optimize-instructions (values instructions nil)))
  (let ((prog instructions)
        (max-iterations (if (eq max-iterations :adaptive)
                            (opt-adaptive-max-iterations instructions)
                            max-iterations))
        (passes (opt-resolve-pass-pipeline pass-pipeline))
        (timing-stream       (or timing-stream *standard-output*))
        (opt-remarks-stream  (or opt-remarks-stream *standard-output*))
        (stats-stream        (or stats-stream *standard-output*))
        (trace-events nil)
        (trace-ts-us 0))
    (loop repeat max-iterations
          for prev = prog
          do (multiple-value-setq (prog trace-events trace-ts-us)
               (opt-run-passes-once-with-reporting prog passes
                                                   :print-pass-timings print-pass-timings
                                                   :timing-stream timing-stream
                                                   :print-pass-stats print-pass-stats
                                                   :stats-stream stats-stream
                                                   :print-opt-remarks print-opt-remarks
                                                   :opt-remarks-stream opt-remarks-stream
                                                   :opt-remarks-mode opt-remarks-mode
                                                   :trace-enabled (not (null trace-json-stream))
                                                   :trace-events trace-events
                                                   :initial-ts-us trace-ts-us))
          when (opt-converged-p prev prog)
          return prog)
    (when trace-json-stream
      (%opt-write-trace-json trace-json-stream (nreverse trace-events)))
    (opt-pass-leaf-detect prog)))
