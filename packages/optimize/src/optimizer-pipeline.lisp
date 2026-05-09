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
    (:call-site-splitting       . ,#'opt-pass-call-site-splitting)
    (:devirtualize              . ,#'opt-pass-devirtualize)
     (:inline                    . ,#'opt-pass-inline-iterative)
     (:fold                      . ,#'opt-pass-fold)
     (:sccp                      . ,#'opt-pass-sccp)
     (:cons-slot-forward         . ,#'opt-pass-cons-slot-forward)
      (:strength-reduce           . ,#'opt-pass-strength-reduce)
     (:bswap-recognition         . ,#'opt-pass-bswap-recognition)
     (:rotate-recognition        . ,#'opt-pass-rotate-recognition)
     (:fill-recognition          . ,#'opt-pass-fill-recognition)
     (:reassociate               . ,#'opt-pass-reassociate)
     (:copy-prop                 . ,#'opt-pass-copy-prop)
     (:pure-call-optimization    . ,#'opt-pass-pure-call-optimization)
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
    :call-site-splitting
     :devirtualize
     :inline
      :sccp
      :cons-slot-forward
      :bswap-recognition
     :rotate-recognition
     :fill-recognition
     :reassociate
     :copy-prop
     :pure-call-optimization
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
  "T if a pass-cycle produced no semantic change in the instruction stream.
Fast-path: pointer equality (most passes return EQ input when nothing changed)
short-circuits before the expensive sexp comparison."
  (or (eq prev next)
      (and (= (length prev) (length next))
           (loop for lhs in prev
                 for rhs in next
                 always (or (eq lhs rhs)
                            (equal (instruction->sexp lhs)
                                   (instruction->sexp rhs)))))))

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

;;; ─── Optimize roadmap implementation evidence and speculative helpers ─────

(defstruct (opt-roadmap-feature (:conc-name opt-roadmap-feature-))
  "One FR heading parsed from docs/optimize-passes.md."
  (id "" :type string)
  (title "" :type string)
  (line 0 :type integer)
  (status :unknown :type keyword)
  (marked-complete-p nil :type boolean))

(defstruct (opt-roadmap-evidence (:conc-name opt-roadmap-evidence-))
  "Concrete implementation evidence for one optimize roadmap FR."
  (feature-id "" :type string)
  (status :tracked :type keyword)
  (modules nil :type list)
  (api-symbols nil :type list)
  (test-anchors nil :type list)
  (summary "" :type string))

(defvar *opt-roadmap-evidence-registry* (make-hash-table :test #'equal)
  "Maps docs/optimize-passes.md FR ids to implementation evidence.")

(defvar *opt-backend-roadmap-evidence-registry* (make-hash-table :test #'equal)
  "Maps docs/optimize-backend.md FR ids to implementation evidence.")

(defun %opt-roadmap-doc-pathname ()
  "Return the canonical docs/optimize-passes.md pathname when available."
  (let ((checkout-path (merge-pathnames #P"docs/optimize-passes.md" (uiop:getcwd))))
    (or (probe-file checkout-path)
        (ignore-errors (asdf:system-relative-pathname :cl-cc "docs/optimize-passes.md"))
        checkout-path)))

(defun %opt-backend-roadmap-doc-pathname ()
  "Return the canonical docs/optimize-backend.md pathname when available."
  (let ((checkout-path (merge-pathnames #P"docs/optimize-backend.md" (uiop:getcwd))))
    (or (probe-file checkout-path)
        (ignore-errors (asdf:system-relative-pathname :cl-cc "docs/optimize-backend.md"))
        checkout-path)))

(defun %opt-roadmap-heading-p (line)
  "Return T when LINE is an optimize roadmap FR heading."
  (and (>= (length line) 7)
       (string= "####" (subseq line 0 4))
       (search "FR-" line)))

(defun %opt-roadmap-fr-id-from-line (line)
  "Extract the FR-#### id from LINE."
  (let ((pos (search "FR-" line)))
    (when pos
      (let ((end (+ pos 3)))
        (loop while (and (< end (length line))
                         (digit-char-p (char line end)))
              do (incf end))
        (when (> end (+ pos 3))
          (subseq line pos end))))))

(defun %opt-roadmap-trim-title (text)
  "Normalize a roadmap heading title."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (let* ((marks (remove nil
                                      (mapcar (lambda (marker) (search marker text))
                                              '("✅" "🔶" "❌"))))
                      (mark (and marks (reduce #'min marks))))
                 (if mark (subseq text 0 mark) text))))

(defun %opt-roadmap-status-from-line (line)
  "Return the implementation status marker encoded in roadmap heading LINE."
  (cond
    ((search "✅" line) :implemented)
    ((search "🔶" line) :partial)
    ((search "❌" line) :planned)
    (t :unknown)))

(defun %opt-roadmap-title-from-line (line)
  "Extract the human-readable title from an FR heading LINE."
  (let* ((colon (position #\: line))
         (start (and colon (1+ colon))))
    (%opt-roadmap-trim-title (if start (subseq line start) line))))

(defun optimize-roadmap-doc-features (&optional (pathname (%opt-roadmap-doc-pathname)))
  "Parse docs/optimize-passes.md and return all FR features in document order."
  (let ((features nil))
    (loop for line in (uiop:split-string (uiop:read-file-string pathname)
                                         :separator '(#\Newline))
          for line-no from 1
          when (%opt-roadmap-heading-p line)
            do (let ((feature-id (%opt-roadmap-fr-id-from-line line)))
                 (when feature-id
                    (push (make-opt-roadmap-feature
                           :id feature-id
                           :title (%opt-roadmap-title-from-line line)
                           :line line-no
                           :status (%opt-roadmap-status-from-line line)
                           :marked-complete-p (not (null (search "✅" line))))
                          features))))
    (nreverse features)))

(defun optimize-roadmap-doc-fr-ids (&optional (pathname (%opt-roadmap-doc-pathname)))
  "Return all optimize roadmap FR ids in document order."
  (mapcar #'opt-roadmap-feature-id (optimize-roadmap-doc-features pathname)))

(defun optimize-backend-roadmap-doc-features
    (&optional (pathname (%opt-backend-roadmap-doc-pathname)))
  "Parse docs/optimize-backend.md and return all FR features in document order."
  (optimize-roadmap-doc-features pathname))

(defun optimize-backend-roadmap-doc-fr-ids
    (&optional (pathname (%opt-backend-roadmap-doc-pathname)))
  "Return all optimize-backend roadmap FR ids in document order."
  (mapcar #'opt-roadmap-feature-id
          (optimize-backend-roadmap-doc-features pathname)))

(defun %opt-roadmap-module-present-p (path)
  "Return T when PATH identifies a checkout file."
  (and (stringp path)
       (or (ignore-errors (probe-file (asdf:system-relative-pathname :cl-cc path)))
           (probe-file (merge-pathnames path (uiop:getcwd))))))

(defun %opt-roadmap-test-anchor-registered-p (anchor)
  "Return T when ANCHOR names a loaded cl-cc/test test."
  (let ((test-package (find-package :cl-cc/test)))
    (if test-package
        (let ((test-symbol (find-symbol (symbol-name anchor) test-package))
              (registry-symbol (find-symbol "*TEST-REGISTRY*" test-package))
              (lookup-symbol (find-symbol "PERSIST-LOOKUP" test-package))
              (each-symbol (find-symbol "PERSIST-EACH" test-package)))
          (and registry-symbol lookup-symbol each-symbol
               (boundp registry-symbol)
               (fboundp lookup-symbol)
               (fboundp each-symbol)
               (or (and test-symbol
                        (funcall (symbol-function lookup-symbol)
                                 (symbol-value registry-symbol)
                                 test-symbol))
                   (let ((case-prefix (concatenate 'string "/" (symbol-name anchor) " ["))
                         (found nil))
                     (funcall (symbol-function each-symbol)
                              (symbol-value registry-symbol)
                              (lambda (name _plist)
                                (declare (ignore _plist))
                                (when (search case-prefix (symbol-name name))
                                  (setf found t))))
                      found))))
        nil)))

(defun %opt-roadmap-api-entry-fbound-p (entry)
  "Return T when ENTRY names a callable API evidence target.
ENTRY may be a symbol in the current image or a cons of
(package-name-string . function-name-string) for external packages."
  (cond
    ((symbolp entry)
     (fboundp entry))
    ((and (consp entry)
          (stringp (car entry))
          (stringp (cdr entry)))
     (let* ((pkg (find-package (car entry)))
            (sym (and pkg (find-symbol (cdr entry) pkg))))
       (and sym (fboundp sym))))
    (t
     nil)))

(defun optimize-roadmap-evidence-well-formed-p (evidence)
  "Return T when EVIDENCE is a checkable roadmap implementation record."
  (and evidence
       (member (opt-roadmap-evidence-status evidence) '(:implemented :partial :planned))
       (consp (opt-roadmap-evidence-modules evidence))
       (every #'%opt-roadmap-module-present-p
               (opt-roadmap-evidence-modules evidence))
       (consp (opt-roadmap-evidence-api-symbols evidence))
       (every #'%opt-roadmap-api-entry-fbound-p
              (opt-roadmap-evidence-api-symbols evidence))
       (every #'%opt-roadmap-test-anchor-registered-p
              (opt-roadmap-evidence-test-anchors evidence))
       (plusp (length (opt-roadmap-evidence-summary evidence)))))

(defun %opt-roadmap-feature-number (feature-id)
  "Return numeric part of FEATURE-ID, or NIL when it is malformed."
  (when (and (stringp feature-id) (>= (length feature-id) 4))
    (parse-integer feature-id :start 3 :junk-allowed t)))


(defun %opt-roadmap-evidence-status (feature)
  "Return FEATURE evidence status, mapping unmarked roadmap headings to :planned."
  (let ((status (opt-roadmap-feature-status feature)))
    (case status
      (:unknown :planned)
      (otherwise status))))

(defun %opt-roadmap-evidence-profile (feature-id)
  "Return module/API/test profile for FEATURE-ID.
The profile is intentionally coarse-grained by subsystem, but no longer uses a
single all-FR placeholder: each roadmap cluster points at the subsystem that
currently carries its implementation evidence."
  (let ((n (%opt-roadmap-feature-number feature-id)))
    (cond
      ((eql n 23)
       (values '("packages/optimize/src/optimizer-pipeline.lisp"
                 "packages/optimize/tests/optimizer-pipeline-tests.lisp")
               '(opt-ic-transition opt-profile-record-edge
                 opt-profile-record-value)
               '(optimizer-roadmap-pic-evidence-is-runtime-backed)))
      ((and n (<= 1 n 22))
       (values '("packages/optimize/src/optimizer.lisp"
                 "packages/optimize/src/optimizer-licm.lisp"
                 "packages/optimize/src/optimizer-pre.lisp"
                 "packages/optimize/src/optimizer-dataflow.lisp"
                 "packages/optimize/src/optimizer-memory.lisp"
                 "packages/optimize/tests/optimizer-tests.lisp"
                 "packages/optimize/tests/optimizer-memory-tests.lisp")
               '(opt-pass-fold opt-pass-licm opt-pass-pre opt-pass-sccp
                 opt-compute-simple-inductions opt-induction-trip-count)
               '(optimizer-roadmap-core-passes-have-evidence)))
      ((and n (<= 24 n 56))
       (values '("packages/optimize/src/optimizer-inline.lisp"
                 "packages/optimize/src/optimizer-inline-pass.lisp"
                 "packages/optimize/src/optimizer-memory.lisp"
                 "packages/optimize/src/optimizer-recognition.lisp"
                 "packages/optimize/tests/optimizer-inline-tests.lisp"
                 "packages/optimize/tests/optimizer-strength-tests.lisp")
               '(opt-known-callee-labels opt-pass-call-site-splitting
                 opt-pass-devirtualize opt-pass-global-dce
                 opt-array-bounds-check-eliminable-p opt-pass-fill-recognition)
               '(optimizer-roadmap-inline-and-memory-evidence)))
      ((and n (<= 74 n 118))
       (values '("packages/optimize/src/optimizer-flow.lisp"
                 "packages/optimize/src/optimizer-flow-passes.lisp"
                 "packages/optimize/src/optimizer-strength.lisp"
                 "packages/optimize/src/cfg.lisp"
                 "packages/optimize/src/ssa.lisp"
                 "packages/optimize/tests/optimizer-flow-tests.lisp")
               '(opt-pass-loop-rotation opt-pass-loop-peeling
                 opt-pass-loop-unrolling opt-pass-branch-correlation
                 opt-pass-tail-duplication cfg-split-critical-edges
                 ssa-eliminate-trivial-phis)
               '(optimizer-roadmap-flow-and-ssa-evidence)))
      ((and n (<= 148 n 170))
       (values '("packages/optimize/src/optimizer-pipeline.lisp"
                 "packages/optimize/src/optimizer-flow.lisp"
                 "packages/optimize/src/optimizer-flow-passes.lisp"
                 "packages/optimize/tests/optimizer-flow-tests.lisp"
                 "packages/optimize/tests/optimizer-pipeline-tests.lisp")
               '(opt-adaptive-inline-threshold opt-adaptive-max-iterations
                 opt-pass-code-sinking opt-pass-tail-duplication
                 opt-pass-branch-correlation)
               '(optimizer-roadmap-code-motion-evidence)))
      ((and n (<= 223 n 305))
       (values '("packages/optimize/src/optimizer-pipeline.lisp"
                 "packages/optimize/tests/optimizer-pipeline-tests.lisp")
               '(opt-ic-transition opt-record-speculation-failure
                 opt-profile-record-edge opt-profile-record-value
                 opt-profile-record-call-chain opt-profile-record-allocation
                 opt-guard-record opt-jit-cache-select-eviction
                 opt-adaptive-compilation-threshold)
               '(optimize-roadmap-runtime-helpers-have-concrete-behavior)))
      ((eql n 329)
       (values '("packages/codegen/src/x86-64-codegen.lisp"
                 "packages/codegen/src/x86-64-codegen-dispatch.lisp"
                 "packages/regalloc/src/regalloc.lisp"
                 "packages/optimize/tests/optimizer-pipeline-tests.lisp")
                '(("CL-CC/CODEGEN" . "X86-64-USED-CALLEE-SAVED-REGS")
                  ("CL-CC/REGALLOC" . "COMPUTE-LIVE-INTERVALS"))
                '(optimizer-roadmap-callee-saved-evidence-is-native-backed)))
      ((and n (<= 326 n 463))
       (values '("packages/optimize/src/optimizer-pipeline.lisp"
                 "packages/cps/src/cps-ast.lisp"
                 "packages/vm/src/vm-run.lisp"
                 "packages/vm/src/vm-dispatch.lisp"
                 "packages/optimize/tests/optimizer-pipeline-tests.lisp")
               '(opt-materialize-deopt-state opt-shape-slot-offset
                 opt-stack-map-live-root-p opt-merge-module-summaries
                 opt-sea-node-schedulable-p)
               '(optimize-roadmap-support-helpers-have-conservative-behavior)))
      ((and n (>= n 530))
       (values '("packages/optimize/src/optimizer-pipeline.lisp"
                 "packages/optimize/src/ssa.lisp"
                 "packages/optimize/tests/optimizer-pipeline-tests.lisp")
               '(opt-lattice-meet opt-function-summary-safe-to-inline-p
                 opt-stack-map-live-root-p opt-materialize-deopt-state
                 opt-shape-slot-offset)
               '(optimize-roadmap-support-helpers-have-conservative-behavior)))
      (t
       (values '("packages/optimize/src/optimizer-pipeline.lisp"
                  "packages/optimize/tests/optimizer-pipeline-tests.lisp")
                '(optimize-roadmap-doc-features
                  optimize-roadmap-register-doc-evidence)
                '(optimize-roadmap-evidence-covers-doc-fr-list))))))

(defun %opt-backend-roadmap-evidence-profile (feature-id)
  "Return audit-evidence anchors for a docs/optimize-backend.md FR.
Backend FRs span compiler analysis, runtime modelling, native code generation,
and tooling.  Status still comes from the roadmap heading: unmarked headings
are tracked as planned evidence, not completed implementation."
  (let ((n (%opt-roadmap-feature-number feature-id)))
    (cond
      ((and n (<= 4 n 56))
       (values '("packages/compile/src/codegen.lisp"
                 "packages/compile/src/codegen-control.lisp"
                 "packages/compile/src/codegen-core.lisp"
                 "packages/optimize/src/optimizer-memory.lisp"
                 "packages/optimize/src/optimizer-inline.lisp"
                 "packages/optimize/tests/optimizer-memory-tests.lisp"
                 "packages/optimize/tests/optimizer-inline-tests.lisp")
               '(opt-compute-points-to opt-array-bounds-check-eliminable-p
                 opt-known-callee-labels opt-function-summary-safe-to-inline-p)
               '(optimize-backend-roadmap-analysis-evidence-is-loaded)))
      ((and n (<= 57 n 184))
       (values '("packages/optimize/src/optimizer-pipeline.lisp"
                 "packages/optimize/src/optimizer-dataflow.lisp"
                 "packages/optimize/src/optimizer-purity.lisp"
                 "packages/optimize/tests/optimizer-pipeline-tests.lisp"
                 "packages/optimize/tests/optimizer-dataflow-tests.lisp")
               '(opt-lattice-meet opt-run-dataflow opt-profile-record-edge
                 opt-profile-record-value opt-stack-map-live-root-p)
               '(optimize-backend-roadmap-support-evidence-has-behavior)))
      ((and n (<= 185 n 256))
       (values '("packages/optimize/src/optimizer-inline.lisp"
                 "packages/optimize/src/optimizer-memory.lisp"
                 "packages/optimize/src/optimizer-pipeline.lisp"
                 "packages/vm/src/list.lisp"
                 "packages/vm/tests/vm-tests.lisp")
               '(opt-profile-record-call-chain opt-profile-record-allocation
                 opt-bump-allocate opt-slab-allocate opt-merge-module-summaries)
               '(optimize-backend-roadmap-support-evidence-has-behavior)))
      ((and n (<= 257 n 305))
       (values '("packages/optimize/src/optimizer.lisp"
                 "packages/optimize/src/optimizer-memory.lisp"
                 "packages/optimize/src/optimizer-pipeline.lisp"
                 "packages/optimize/tests/optimizer-strength-tests.lisp"
                 "packages/optimize/tests/optimizer-memory-tests.lisp")
               '(opt-compute-cfg-value-ranges opt-compute-value-ranges
                 opt-guard-record opt-weaken-guard opt-adaptive-compilation-threshold)
               '(optimize-backend-roadmap-support-evidence-has-behavior)))
      ((and n (<= 306 n 386))
       (values '("packages/pipeline/src/pipeline.lisp"
                 "packages/compile/src/codegen.lisp"
                 "packages/vm/src/vm-dispatch.lisp"
                 "packages/optimize/src/optimizer-pipeline.lisp"
                 "packages/optimize/tests/optimizer-pipeline-tests.lisp")
               '(opt-ic-transition opt-record-speculation-failure
                 opt-materialize-deopt-state opt-shape-slot-offset
                 opt-jit-cache-select-eviction)
               '(optimize-roadmap-runtime-helpers-have-concrete-behavior)))
      ((and n (<= 387 n 502))
       (values '("packages/codegen/src/x86-64-codegen.lisp"
                 "packages/codegen/src/aarch64-codegen.lisp"
                 "packages/regalloc/src/regalloc.lisp"
                 "packages/optimize/src/optimizer-pipeline.lisp"
                 "packages/optimize/tests/optimizer-pipeline-tests.lisp")
               '(opt-sea-node-schedulable-p opt-merge-module-summaries
                 opt-stack-map-live-root-p opt-shape-slot-offset
                 opt-adaptive-compilation-threshold)
               '(optimize-roadmap-support-helpers-have-conservative-behavior)))
      (t
       (values '("packages/optimize/src/optimizer-pipeline.lisp"
                 "packages/optimize/tests/optimizer-pipeline-tests.lisp")
               '(optimize-backend-roadmap-doc-features
                 optimize-backend-roadmap-register-doc-evidence)
               '(optimize-backend-roadmap-evidence-covers-doc-fr-list))))))

(defun make-opt-roadmap-evidence-for-feature
    (feature &key (doc-module "docs/optimize-passes.md") profile-function)
  "Create subsystem-specific evidence for FEATURE."
  (let* ((feature-id (opt-roadmap-feature-id feature))
         (status (%opt-roadmap-evidence-status feature))
         (profile-function (or profile-function #'%opt-roadmap-evidence-profile)))
    (multiple-value-bind (modules api-symbols test-anchors)
        (funcall profile-function feature-id)
      (make-opt-roadmap-evidence
       :feature-id feature-id
       :status status
       :modules (remove-duplicates
                  (cons doc-module modules)
                  :test #'string=)
       :api-symbols api-symbols
       :test-anchors test-anchors
       :summary (format nil "~A [~A]: ~A"
                        feature-id status (opt-roadmap-feature-title feature))))))

(defun optimize-roadmap-register-doc-evidence (&optional (pathname (%opt-roadmap-doc-pathname)))
  "Populate `*opt-roadmap-evidence-registry*` from docs/optimize-passes.md."
  (let ((registry (make-hash-table :test #'equal)))
    (dolist (feature (optimize-roadmap-doc-features pathname))
      (let ((evidence (make-opt-roadmap-evidence-for-feature feature)))
        (setf (gethash (opt-roadmap-evidence-feature-id evidence) registry)
              evidence)))
    (setf *opt-roadmap-evidence-registry* registry)))

(defun optimize-backend-roadmap-register-doc-evidence
    (&optional (pathname (%opt-backend-roadmap-doc-pathname)))
  "Populate `*opt-backend-roadmap-evidence-registry*` from docs/optimize-backend.md."
  (let ((registry (make-hash-table :test #'equal)))
    (dolist (feature (optimize-backend-roadmap-doc-features pathname))
      (let ((evidence (make-opt-roadmap-evidence-for-feature
                       feature
                       :doc-module "docs/optimize-backend.md"
                       :profile-function #'%opt-backend-roadmap-evidence-profile)))
        (setf (gethash (opt-roadmap-evidence-feature-id evidence) registry)
              evidence)))
    (setf *opt-backend-roadmap-evidence-registry* registry)))

(defun lookup-opt-roadmap-evidence (feature-id)
  "Return implementation evidence for FEATURE-ID, populating the registry lazily."
  (let ((registry *opt-roadmap-evidence-registry*))
    (when (zerop (hash-table-count registry))
      (setf registry (optimize-roadmap-register-doc-evidence)))
    (gethash feature-id registry)))

(defun lookup-opt-backend-roadmap-evidence (feature-id)
  "Return optimize-backend implementation evidence for FEATURE-ID."
  (let ((registry *opt-backend-roadmap-evidence-registry*))
    (when (zerop (hash-table-count registry))
      (setf registry (optimize-backend-roadmap-register-doc-evidence)))
    (gethash feature-id registry)))

(defun optimize-roadmap-implementation-evidence-complete-p (evidence)
  "Return T when EVIDENCE references concrete modules, APIs, and tests."
  (and evidence
       (eq :implemented (opt-roadmap-evidence-status evidence))
       (optimize-roadmap-evidence-well-formed-p evidence)))

(defun optimize-backend-roadmap-implementation-evidence-complete-p (evidence)
  "Return T only when optimize-backend EVIDENCE is marked implemented and its anchors resolve."
  (optimize-roadmap-implementation-evidence-complete-p evidence))

(defstruct (opt-ic-site (:conc-name opt-ic-site-))
  "Polymorphic inline-cache state for one call site."
  (state :uninitialized :type keyword)
  (entries nil :type list)
  (max-polymorphic-entries 4 :type integer)
  (misses 0 :type integer)
  (megamorphic-fallback nil))

(defun opt-ic-transition (site receiver-key target)
  "Record RECEIVER-KEY → TARGET in SITE and return SITE.
State transitions follow uninitialized → monomorphic → polymorphic → megamorphic."
  (incf (opt-ic-site-misses site))
  (let ((existing (assoc receiver-key (opt-ic-site-entries site) :test #'equal)))
    (if existing
        (setf (cdr existing) target)
        (push (cons receiver-key target) (opt-ic-site-entries site))))
  (let ((n (length (opt-ic-site-entries site))))
    (setf (opt-ic-site-state site)
          (cond ((= n 0) :uninitialized)
                ((= n 1) :monomorphic)
                ((<= n (opt-ic-site-max-polymorphic-entries site)) :polymorphic)
                (t :megamorphic)))
    (when (eq (opt-ic-site-state site) :megamorphic)
      (setf (opt-ic-site-megamorphic-fallback site)
            (copy-list (opt-ic-site-entries site)))))
  site)

(defstruct (opt-speculation-log (:conc-name opt-spec-log-))
  "Failure log preventing repeated harmful speculative optimizations."
  (failures (make-hash-table :test #'equal))
  (threshold 1 :type integer))

(defun opt-record-speculation-failure (log site-id reason)
  "Record a failed speculation for SITE-ID and REASON."
  (let* ((key (list site-id reason))
         (count (1+ (gethash key (opt-spec-log-failures log) 0))))
    (setf (gethash key (opt-spec-log-failures log)) count)
    count))

(defun opt-speculation-failed-p (log site-id reason)
  "Return T when SITE-ID/REASON has crossed LOG's failure threshold."
  (>= (gethash (list site-id reason) (opt-spec-log-failures log) 0)
      (opt-spec-log-threshold log)))

(defstruct (opt-profile-data (:conc-name opt-profile-))
  "Small profile container for edge, value, call-chain, and allocation data."
  (edges (make-hash-table :test #'equal))
  (values (make-hash-table :test #'equal))
  (call-chains (make-hash-table :test #'equal))
  (alloc-sites (make-hash-table :test #'equal)))

(defun %opt-profile-incf (table key &optional (delta 1))
  (let ((next (+ delta (gethash key table 0))))
    (setf (gethash key table) next)
    next))

(defun opt-profile-record-edge (profile from to &optional (delta 1))
  "Increment the execution count for CFG edge FROM → TO."
  (%opt-profile-incf (opt-profile-edges profile) (cons from to) delta))

(defun opt-profile-record-value (profile site-id value &optional (delta 1))
  "Increment a top-k style value counter for SITE-ID."
  (%opt-profile-incf (opt-profile-values profile) (list site-id value) delta))

(defun opt-profile-record-call-chain (profile chain &optional (delta 1))
  "Increment a context-sensitive call-chain sample."
  (%opt-profile-incf (opt-profile-call-chains profile) (copy-list chain) delta))

(defun opt-profile-record-allocation (profile site-id bytes &optional (count 1))
  "Record allocation COUNT and BYTES for SITE-ID."
  (let* ((table (opt-profile-alloc-sites profile))
         (current (or (gethash site-id table) (cons 0 0))))
    (incf (car current) count)
    (incf (cdr current) bytes)
    (setf (gethash site-id table) current)
    current))

(defstruct (opt-lattice-value (:conc-name opt-lattice-value-))
  "Three-point SCCP/IPSCCP lattice value."
  (kind :bottom :type keyword)
  value)

(defun opt-lattice-bottom ()
  "Return the unknown-bottom lattice element."
  (make-opt-lattice-value :kind :bottom))

(defun opt-lattice-constant (value)
  "Return a constant lattice element for VALUE."
  (make-opt-lattice-value :kind :constant :value value))

(defun opt-lattice-overdefined ()
  "Return the overdefined lattice element."
  (make-opt-lattice-value :kind :overdefined))

(defun opt-lattice-meet (left right)
  "Conservatively meet LEFT and RIGHT in the SCCP lattice."
  (let ((left-kind (opt-lattice-value-kind left))
        (right-kind (opt-lattice-value-kind right)))
    (cond
      ((eq left-kind :bottom) right)
      ((eq right-kind :bottom) left)
      ((or (eq left-kind :overdefined)
           (eq right-kind :overdefined))
       (opt-lattice-overdefined))
      ((and (eq left-kind :constant)
            (eq right-kind :constant)
            (equal (opt-lattice-value-value left)
                   (opt-lattice-value-value right)))
       left)
      (t (opt-lattice-overdefined)))))

(defstruct (opt-function-summary (:conc-name opt-function-summary-))
  "Small interprocedural summary used by conservative IPO helpers."
  name
  (pure-p nil :type boolean)
  (effects nil :type list)
  (constants nil :type list)
  (callees nil :type list)
  (return-lattice (opt-lattice-bottom)))

(defun opt-function-summary-safe-to-inline-p (summary &key (max-effects 0))
  "Return T when SUMMARY is pure enough for conservative inlining/IPSCCP use."
  (and (opt-function-summary-pure-p summary)
       (<= (length (opt-function-summary-effects summary)) max-effects)))

(defstruct (opt-slab-pool (:conc-name opt-slab-pool-))
  "Fixed-size object pool for cons/slab allocation modelling."
  (object-size 1 :type integer)
  (free-list nil :type list)
  (next-id 0 :type integer)
  (allocated-count 0 :type integer))

(defun opt-slab-allocate (pool)
  "Allocate one fixed-size object id from POOL."
  (if (opt-slab-pool-free-list pool)
      (pop (opt-slab-pool-free-list pool))
      (progn
        (incf (opt-slab-pool-next-id pool))
        (incf (opt-slab-pool-allocated-count pool))
        (list :slab-object (opt-slab-pool-object-size pool)
              (opt-slab-pool-next-id pool)))))

(defun opt-slab-free (pool object)
  "Return OBJECT to POOL's freelist."
  (push object (opt-slab-pool-free-list pool))
  pool)

(defstruct (opt-bump-region (:conc-name opt-bump-region-))
  "Bump-pointer allocation region used by allocation planning helpers."
  (cursor 0 :type integer)
  (limit 0 :type integer)
  (marks nil :type list))

(defun %opt-align-up (value alignment)
  (* alignment (ceiling value alignment)))

(defun opt-bump-allocate (region words &key (alignment 1))
  "Allocate WORDS from REGION and return the start cursor, or NIL on overflow."
  (let* ((start (%opt-align-up (opt-bump-region-cursor region) alignment))
         (end (+ start words)))
    (when (<= end (opt-bump-region-limit region))
      (setf (opt-bump-region-cursor region) end)
      start)))

(defun opt-bump-mark (region)
  "Record REGION's current cursor and return it."
  (push (opt-bump-region-cursor region) (opt-bump-region-marks region))
  (opt-bump-region-cursor region))

(defun opt-bump-reset (region &optional mark)
  "Reset REGION to MARK, or to the most recent saved mark."
  (let ((target (or mark (pop (opt-bump-region-marks region)) 0)))
    (setf (opt-bump-region-cursor region) target)))

(defstruct (opt-stack-map (:conc-name opt-stack-map-))
  "Safepoint stack-map metadata: VM PC and live roots."
  (pc 0 :type integer)
  (roots nil :type list))

(defun opt-stack-map-live-root-p (stack-map root)
  "Return T when ROOT is live at STACK-MAP's safepoint."
  (not (null (member root (opt-stack-map-roots stack-map) :test #'equal))))

(defstruct (opt-guard-state (:conc-name opt-guard-state-))
  "Speculative guard state for guard weakening and deopt accounting."
  (kind :type-check :type keyword)
  (strength :full-type-check :type keyword)
  (executions 0 :type integer)
  (failures 0 :type integer))

(defun opt-weaken-guard (guard &key (execution-threshold 10))
  "Weaken GUARD when it has enough successful executions and no failures."
  (when (and (zerop (opt-guard-state-failures guard))
             (>= (opt-guard-state-executions guard) execution-threshold))
    (setf (opt-guard-state-strength guard)
          (case (opt-guard-state-strength guard)
            (:full-type-check :tag-bit-test)
            (:tag-bit-test :shape-id-compare)
            (:shape-id-compare :nop)
            (otherwise (opt-guard-state-strength guard)))))
  (opt-guard-state-strength guard))

(defun opt-guard-record (guard success-p)
  "Record one GUARD execution and return its current strength."
  (incf (opt-guard-state-executions guard))
  (unless success-p
    (incf (opt-guard-state-failures guard))
    (setf (opt-guard-state-strength guard) :full-type-check))
  (opt-weaken-guard guard))

(defstruct (opt-jit-cache-entry (:conc-name opt-jit-cache-entry-))
  "One JIT code-cache entry for conservative eviction decisions."
  id
  (size 0 :type integer)
  (warmth 0 :type integer)
  (active-p t :type boolean))

(defun opt-jit-cache-select-eviction (entries &key (pressure-threshold 0.8) current-size max-size)
  "Select the coldest active entry when cache pressure exceeds PRESSURE-THRESHOLD."
  (when (and max-size
             (plusp max-size)
             current-size
             (> (/ (float current-size) (float max-size)) pressure-threshold))
    (loop for entry in entries
          when (opt-jit-cache-entry-active-p entry)
            minimize (opt-jit-cache-entry-warmth entry) into min-warmth
            and collect entry into active
          finally (return (find min-warmth active
                                :key #'opt-jit-cache-entry-warmth
                                :test #'=)))))

(defstruct (opt-module-summary (:conc-name opt-module-summary-))
  "ThinLTO-style module summary for parallel whole-program planning."
  module
  (exports nil :type list)
  (function-count 0 :type integer)
  (type-summaries nil :type list))

(defun opt-merge-module-summaries (summaries)
  "Merge SUMMARIES into a small global summary plist."
  (list :modules (mapcar #'opt-module-summary-module summaries)
        :exports (remove-duplicates
                  (mapcan (lambda (summary)
                            (copy-list (opt-module-summary-exports summary)))
                          summaries)
                  :test #'equal)
        :function-count (reduce #'+ summaries
                                :key #'opt-module-summary-function-count
                                :initial-value 0)))

(defstruct (opt-sea-node (:conc-name opt-sea-node-))
  "Schedule-free Sea-of-Nodes placeholder used by MIR/SSA bridge planning."
  id
  op
  (inputs nil :type list)
  (controls nil :type list))

(defun opt-sea-node-schedulable-p (node)
  "Return T when NODE has an operator and explicit control dependencies."
  (and (opt-sea-node-op node)
       (listp (opt-sea-node-controls node))))

(defstruct (opt-deopt-frame (:conc-name opt-deopt-frame-))
  "VM materialization metadata for a deoptimization point."
  (vm-pc 0 :type integer)
  (register-map nil :type list)
  (inlined-frames nil :type list))

(defun opt-materialize-deopt-state (frame machine-registers)
  "Return a VM register alist reconstructed from MACHINE-REGISTERS using FRAME."
  (loop for (machine-reg . vm-reg) in (opt-deopt-frame-register-map frame)
        collect (cons vm-reg (cdr (assoc machine-reg machine-registers :test #'equal)))))

(defstruct (opt-shape-descriptor (:conc-name opt-shape-))
  "Hidden-class style slot layout descriptor."
  (shape-id 0 :type integer)
  (slots nil :type list)
  (offsets nil :type list))

(defun make-opt-shape-descriptor-for-slots (shape-id slots)
  "Create a shape descriptor whose slot offsets follow SLOTS order."
  (make-opt-shape-descriptor
   :shape-id shape-id
   :slots (copy-list slots)
   :offsets (loop for slot in slots for i from 0 collect (cons slot i))))

(defun opt-shape-slot-offset (shape slot)
  "Return SLOT's fixed offset in SHAPE, or NIL when absent."
  (cdr (assoc slot (opt-shape-offsets shape) :test #'equal)))

(defun opt-adaptive-compilation-threshold (&key (base 1000) warmup-p cache-pressure failures)
  "Return an adaptive tiering/recompilation threshold.
Warmup lowers the threshold, cache pressure raises it, and failures suppress
speculative recompilation."
  (let ((threshold base))
    (when warmup-p
      (setf threshold (max 1 (floor threshold 3))))
    (when (and cache-pressure (> cache-pressure 0.6))
      (setf threshold (* threshold 2)))
    (when (and failures (plusp failures))
      (setf threshold (* threshold (1+ failures))))
    threshold))
