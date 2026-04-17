(in-package :cl-cc/optimize)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Optimizer — Pass Pipeline, Reporting, and Top-Level Driver
;;;
;;; Contains: opt-pass-inline-iterative, *opt-convergence-passes*,
;;; *opt-pass-registry*, opt-parse-pass-pipeline-string,
;;; opt-resolve-pass-pipeline, opt-run-passes-once-with-reporting,
;;; *opt-iteration-budget-thresholds*, opt-converged-p,
;;; opt-adaptive-max-iterations, opt-verify-instructions,
;;; and the public entry point optimize-instructions.
;;;
;;; Pass implementations (opt-pass-fold, opt-pass-dce, opt-pass-jump,
;;; opt-pass-unreachable) and their helpers are in optimizer.lisp (loads before).
;;;
;;; Load order: after optimizer.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


(defun opt-pass-inline-iterative (instructions)
  "Thresholded inline pass used inside the convergence loop."
  (opt-pass-inline instructions :threshold :adaptive))

(defparameter *opt-convergence-passes*
  (list #'opt-pass-inline-iterative
        #'opt-pass-fold
        #'opt-pass-sccp
        #'opt-pass-strength-reduce
        #'opt-pass-bswap-recognition
        #'opt-pass-rotate-recognition
        #'opt-pass-reassociate
        #'opt-pass-copy-prop
        #'opt-pass-gvn
        #'opt-pass-batch-concatenate
        #'opt-pass-cse
        #'opt-pass-jump
        #'opt-pass-unreachable
        #'opt-pass-dead-basic-blocks
        #'opt-pass-store-to-load-forward
        #'opt-pass-dead-store-elim
        #'opt-pass-nil-check-elim
        #'opt-pass-dominated-type-check-elim
        #'opt-pass-branch-correlation
        #'opt-pass-block-merge
        #'opt-pass-tail-merge
        #'opt-pass-pre
        #'opt-pass-egraph
        #'opt-pass-constant-hoist
        #'opt-pass-global-dce
        #'opt-pass-dead-labels
        #'opt-pass-dce)
  "Ordered list of passes run to convergence in optimize-instructions.
    Each pass is a function (instructions) -> instructions.
    Add new passes here; the convergence loop requires no other changes.")

(defparameter *opt-pass-registry*
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (entry `((:inline . ,#'opt-pass-inline-iterative)
                     (:fold . ,#'opt-pass-fold)
                     (:sccp . ,#'opt-pass-sccp)
                     (:strength-reduce . ,#'opt-pass-strength-reduce)
                     (:bswap-recognition . ,#'opt-pass-bswap-recognition)
                     (:rotate-recognition . ,#'opt-pass-rotate-recognition)
                     (:reassociate . ,#'opt-pass-reassociate)
                     (:copy-prop . ,#'opt-pass-copy-prop)
                     (:gvn . ,#'opt-pass-gvn)
                     (:batch-concatenate . ,#'opt-pass-batch-concatenate)
                     (:cse . ,#'opt-pass-cse)
                     (:jump . ,#'opt-pass-jump)
                     (:unreachable . ,#'opt-pass-unreachable)
                     (:dead-basic-blocks . ,#'opt-pass-dead-basic-blocks)
                     (:store-to-load-forward . ,#'opt-pass-store-to-load-forward)
                     (:dead-store-elim . ,#'opt-pass-dead-store-elim)
                     (:nil-check-elim . ,#'opt-pass-nil-check-elim)
                     (:dominated-type-check-elim . ,#'opt-pass-dominated-type-check-elim)
                     (:block-merge . ,#'opt-pass-block-merge)
                     (:tail-merge . ,#'opt-pass-tail-merge)
                     (:pre . ,#'opt-pass-pre)
                     (:egraph . ,#'opt-pass-egraph)
                     (:constant-hoist . ,#'opt-pass-constant-hoist)
                     (:dead-labels . ,#'opt-pass-dead-labels)
                     (:dce . ,#'opt-pass-dce)))
      (setf (gethash (car entry) ht) (cdr entry)))
    ht)
  "Keyword pass name -> optimizer function mapping for configurable pipelines.")

(defun opt-parse-pass-pipeline-string (text)
  "Parse a comma-separated optimizer pipeline string into keyword pass names."
  (labels ((trim (s)
             (string-trim '(#\Space #\Tab #\Newline #\Return) s)))
    (remove nil
            (mapcar (lambda (part)
                      (let ((name (trim part)))
                        (and (> (length name) 0)
                             (intern (string-upcase name) :keyword))))
                    (uiop:split-string text :separator '(#\,))))))

(defun opt-resolve-pass-pipeline (pipeline)
  "Resolve PIPELINE into a list of pass functions.
PIPELINE may be NIL (use *opt-convergence-passes*), a list of functions,
or a list of keyword pass names present in *opt-pass-registry*."
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
  "Apply PASSES once while emitting any enabled reports.

Returns three values: next program, updated TRACE-EVENTS list, and updated
timestamp cursor in microseconds."
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
  "T if a pass-cycle produced no change (same length and all instructions eq)."
  (and (= (length prev) (length next))
       (every #'eq prev next)))

(defparameter *opt-iteration-budget-thresholds*
  '((50  . -12)
    (150 . -6)
    (400 . 0)
    (800 . 8))
  "Alist of (instruction-count-upper-bound . budget-delta) for opt-adaptive-max-iterations.
   Entries are tested in order; the delta from the first matching threshold is used.
   If no threshold matches (n >= 800), the fallback delta is 15.")

(defun opt-adaptive-max-iterations (instructions &key (base-iterations 20) (min-iterations 6) (max-iterations 50))
  "Return a conservative adaptive convergence budget for INSTRUCTIONS.

Small instruction streams converge quickly and therefore use fewer iterations,
while larger programs get a modestly larger budget. This is a helper-level
slice of FR-150; it does not use runtime profile counters yet."
  (let* ((n (length instructions))
         (delta (or (cdr (find-if (lambda (entry) (< n (car entry)))
                                  *opt-iteration-budget-thresholds*))
                    15)))
    (min max-iterations
         (max min-iterations
              (+ base-iterations delta)))))

(defun opt-verify-instructions (instructions &key pass-name)
  "Conservative VM-level verifier for optimizer/debugging use.
Checks duplicate labels, unknown jump targets, and obvious use-before-defs in a
single linear instruction stream. Returns T on success, signals ERROR on failure."
  (let ((labels (make-hash-table :test #'equal))
        (defined (make-hash-table :test #'eq))
        (pass-name (or pass-name "<unknown-pass>")))
    ;; Pass 1: label collection / duplicate detection.
    (dolist (inst instructions)
      (when (typep inst 'vm-label)
        (let ((name (vm-name inst)))
          (when (gethash name labels)
            (error "~A verifier: duplicate label ~A" pass-name name))
          (setf (gethash name labels) t))))
    ;; Pass 2: reference + use-before-def checks.
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

(defun optimize-instructions (instructions &key (max-iterations 20) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  "Run the full multi-pass optimization pipeline on a VM instruction sequence.
   Runs until no changes or MAX-ITERATIONS reached."
  (let ((prog instructions)
         (max-iterations (if (eq max-iterations :adaptive)
                             (opt-adaptive-max-iterations instructions)
                             max-iterations))
         (passes (opt-resolve-pass-pipeline pass-pipeline))
         (timing-stream (or timing-stream *standard-output*))
         (opt-remarks-stream (or opt-remarks-stream *standard-output*))
         (stats-stream (or stats-stream *standard-output*))
         (trace-events nil)
         (trace-ts-us 0))
    (loop for iteration from 0 below max-iterations
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
    (when *enable-prolog-peephole*
      (setf prog (mapcar #'sexp->instruction
                         (apply-prolog-peephole (mapcar #'instruction->sexp prog)))))
    (when trace-json-stream
      (%opt-write-trace-json trace-json-stream (nreverse trace-events)))
    (opt-pass-leaf-detect prog)))
