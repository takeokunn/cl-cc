;;;; optimizer-pipeline-roadmap.lisp — Optimizer roadmap document evidence infrastructure
(in-package :cl-cc/optimize)

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

(defparameter +opt-roadmap-evidence-profile-ranges+
  ;; Each entry: (lo hi modules api-symbols test-anchors)
  ;; lo/hi are inclusive bounds; NIL lo/hi means exact match (eql) or open-ended.
  ;; Ordering matters: first matching entry wins (most-specific first).
  `((23 23
     ("packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp")
     (opt-ic-transition opt-profile-record-edge
      opt-profile-record-value)
     (optimizer-roadmap-pic-evidence-is-runtime-backed))
    (329 329
     ("packages/codegen/src/x86-64-codegen.lisp"
      "packages/codegen/src/x86-64-codegen-dispatch.lisp"
      "packages/regalloc/src/regalloc.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp")
     (("CL-CC/CODEGEN" . "X86-64-USED-CALLEE-SAVED-REGS")
      ("CL-CC/REGALLOC" . "COMPUTE-LIVE-INTERVALS"))
     (optimizer-roadmap-callee-saved-evidence-is-native-backed))
    (1 22
     ("packages/optimize/src/optimizer.lisp"
      "packages/optimize/src/optimizer-licm.lisp"
      "packages/optimize/src/optimizer-pre.lisp"
      "packages/optimize/src/optimizer-dataflow.lisp"
      "packages/optimize/src/optimizer-memory.lisp"
      "packages/optimize/tests/optimizer-tests.lisp"
      "packages/optimize/tests/optimizer-memory-tests.lisp")
     (opt-pass-fold opt-pass-licm opt-pass-pre opt-pass-sccp
      opt-compute-simple-inductions opt-induction-trip-count)
     (optimizer-roadmap-core-passes-have-evidence))
    (24 56
     ("packages/optimize/src/optimizer-inline.lisp"
      "packages/optimize/src/optimizer-inline-pass.lisp"
      "packages/optimize/src/optimizer-memory.lisp"
      "packages/optimize/src/optimizer-recognition.lisp"
      "packages/optimize/tests/optimizer-inline-tests.lisp"
      "packages/optimize/tests/optimizer-strength-tests.lisp")
     (opt-known-callee-labels opt-pass-call-site-splitting
      opt-pass-devirtualize opt-pass-global-dce
      opt-array-bounds-check-eliminable-p opt-pass-fill-recognition)
     (optimizer-roadmap-inline-and-memory-evidence))
    (74 118
     ("packages/optimize/src/optimizer-flow.lisp"
      "packages/optimize/src/optimizer-flow-passes.lisp"
      "packages/optimize/src/optimizer-strength.lisp"
      "packages/optimize/src/cfg.lisp"
      "packages/optimize/src/ssa.lisp"
      "packages/optimize/tests/optimizer-flow-tests.lisp")
     (opt-pass-loop-rotation opt-pass-loop-peeling
      opt-pass-loop-unrolling opt-pass-branch-correlation
      opt-pass-tail-duplication cfg-split-critical-edges
      ssa-eliminate-trivial-phis)
     (optimizer-roadmap-flow-and-ssa-evidence))
    (148 170
     ("packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/src/optimizer-flow.lisp"
      "packages/optimize/src/optimizer-flow-passes.lisp"
      "packages/optimize/tests/optimizer-flow-tests.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp")
     (opt-adaptive-inline-threshold opt-adaptive-max-iterations
      opt-pass-code-sinking opt-pass-tail-duplication
      opt-pass-branch-correlation)
     (optimizer-roadmap-code-motion-evidence))
    (223 305
     ("packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp")
     (opt-ic-transition opt-record-speculation-failure
      opt-profile-record-edge opt-profile-record-value
      opt-profile-record-call-chain opt-profile-record-allocation
      opt-guard-record opt-jit-cache-select-eviction
      opt-adaptive-compilation-threshold)
     (optimize-roadmap-runtime-helpers-have-concrete-behavior))
    (326 463
     ("packages/optimize/src/optimizer-pipeline.lisp"
      "packages/cps/src/cps-ast.lisp"
      "packages/vm/src/vm-run.lisp"
      "packages/vm/src/vm-dispatch.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp")
     (opt-materialize-deopt-state opt-shape-slot-offset
      opt-stack-map-live-root-p opt-merge-module-summaries
      opt-sea-node-schedulable-p)
     (optimize-roadmap-support-helpers-have-conservative-behavior))
    (530 nil
     ("packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/src/ssa.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp")
     (opt-lattice-meet opt-function-summary-safe-to-inline-p
      opt-stack-map-live-root-p opt-materialize-deopt-state
      opt-shape-slot-offset)
     (optimize-roadmap-support-helpers-have-conservative-behavior)))
  "Alist of (lo hi modules api-symbols test-anchors) range entries for
`%opt-roadmap-evidence-profile'.  Entries are checked in order; NIL hi means
open-ended (>= lo).  The default fallback is handled by the function itself.")

(defun %opt-roadmap-evidence-profile (feature-id)
  "Return module/API/test profile for FEATURE-ID.
The profile is intentionally coarse-grained by subsystem, but no longer uses a
single all-FR placeholder: each roadmap cluster points at the subsystem that
currently carries its implementation evidence."
  (let ((n (%opt-roadmap-feature-number feature-id)))
    (let ((entry (and n
                      (find-if (lambda (e)
                                 (destructuring-bind (lo hi . _rest) e
                                   (declare (ignore _rest))
                                   (and (<= lo n)
                                        (or (null hi) (<= n hi)))))
                               +opt-roadmap-evidence-profile-ranges+))))
      (if entry
          (destructuring-bind (_lo _hi modules api-symbols test-anchors) entry
            (declare (ignore _lo _hi))
            (values modules api-symbols test-anchors))
          (values '("packages/optimize/src/optimizer-pipeline.lisp"
                    "packages/optimize/tests/optimizer-pipeline-tests.lisp")
                  '(optimize-roadmap-doc-features
                    optimize-roadmap-register-doc-evidence)
                  '(optimize-roadmap-evidence-covers-doc-fr-list))))))

