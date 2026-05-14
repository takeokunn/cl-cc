;;;; tests/unit/optimize/optimizer-roadmap-tests.lisp
;;;; Unit tests for optimizer-pipeline-roadmap.lisp — optimize-passes.md evidence
;;;;
;;;; Covers: optimize-roadmap doc parsing helpers, FR-id coverage,
;;;;   status-aware summaries, completed-heading checks, related-path checks,
;;;;   sidecar contradictions, and per-cluster implementation evidence.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Optimize roadmap evidence and helper contracts ───────────────────────

(defun %optimizer-doc-fr-id-from-line (line)
  "Extract an FR id from an optimize roadmap heading line."
  (let ((fr-pos (and (>= (length line) 4)
                     (string= "####" (subseq line 0 4))
                     (search "FR-" line))))
    (when fr-pos
      (let ((end (+ fr-pos 3)))
        (loop while (and (< end (length line))
                         (digit-char-p (char line end)))
              do (incf end))
        (when (> end (+ fr-pos 3))
          (subseq line fr-pos end))))))

(defun %optimizer-doc-fr-ids ()
  "Return the ordered FR headings from docs/optimize-passes.md."
  (let ((ids nil))
    (dolist (line (uiop:split-string
                   (uiop:read-file-string
                    (merge-pathnames #P"docs/optimize-passes.md" (uiop:getcwd)))
                   :separator '(#\Newline))
             (nreverse ids))
      (let ((feature-id (%optimizer-doc-fr-id-from-line line)))
        (when feature-id
          (push feature-id ids))))))

(defun %doc-content (pathname)
  "Return the text of the roadmap document at PATHNAME (repository-relative)."
  (uiop:read-file-string
   (merge-pathnames pathname (uiop:getcwd))))

(defun %doc-completed-heading-contradictions (pathname needles &key reset-on-section-boundary)
  "Return ✅ FR headings in the document at PATHNAME whose body still contains one of NEEDLES.
When RESET-ON-SECTION-BOUNDARY is non-nil, any ### line that is not an #### FR heading
resets the current block (used by the backend doc which has ### section separators)."
  (let ((contradictions nil)
        (current-heading nil)
        (current-lines nil))
    (labels ((fr-heading-line-p (line)
               (and (>= (length line) 7)
                    (string= "####" (subseq line 0 4))
                    (search "FR-" line)))
             (section-boundary-p (line)
               (and reset-on-section-boundary
                    (>= (length line) 3)
                    (string= "###" (subseq line 0 3))))
             (completed-heading-p (line)
               (search "✅" line))
             (block-text ()
               (with-output-to-string (out)
                 (dolist (line (nreverse current-lines))
                   (format out "~A~%" line))))
             (flush-block ()
               (when (and current-heading (completed-heading-p current-heading))
                 (let ((text (block-text)))
                   (when (some (lambda (needle) (search needle text)) needles)
                     (push current-heading contradictions))))
               (setf current-lines nil)))
      (dolist (line (uiop:split-string (%doc-content pathname)
                                       :separator (list #\Newline)))
        (cond
          ((fr-heading-line-p line)
           (flush-block)
           (setf current-heading line
                 current-lines (list line)))
          ((section-boundary-p line)
           (flush-block)
           (setf current-heading nil
                 current-lines nil))
          (current-heading
           (push line current-lines))))
      (flush-block)
      (nreverse contradictions))))

(defun %optimizer-doc-content ()
  "Return the current optimize roadmap document text."
  (%doc-content #P"docs/optimize-passes.md"))

(defun %optimizer-doc-completed-heading-contradictions ()
  "Return ✅ optimize roadmap headings whose body still says implementation is absent."
  (%doc-completed-heading-contradictions
   #P"docs/optimize-passes.md"
   (list "未実装" "未統合" "欠落" "未接続" "未対応" "未定義" "不可能" "なし")))

(defun %optimizer-doc-fr-ids-from-line (line)
  "Return all FR ids mentioned in LINE."
  (let ((ids nil)
        (start 0))
    (loop for pos = (search "FR-" line :start2 start)
          while pos
          do (let ((end (+ pos 3)))
               (loop while (and (< end (length line))
                                (digit-char-p (char line end)))
                     do (incf end))
               (when (> end (+ pos 3))
                 (push (subseq line pos end) ids))
               (setf start (max (1+ pos) end))))
    (nreverse ids)))

(defun %optimizer-doc-path-token-p (token)
  "Return T when TOKEN is a repository-relative doc/code path."
  (or (and (>= (length token) 9)
           (string= "packages/" token :end2 9))
      (and (>= (length token) 5)
           (string= "docs/" token :end2 5))))

(defun %optimizer-doc-backtick-paths (line)
  "Return repository paths enclosed in backticks on LINE."
  (let ((paths nil)
        (start 0)
        (tick (code-char 96)))
    (loop for open = (position tick line :start start)
          while open
          for close = (position tick line :start (1+ open))
          while close
          do (let ((token (subseq line (1+ open) close)))
               (when (%optimizer-doc-path-token-p token)
                 (push token paths))
               (setf start (1+ close))))
    (nreverse paths)))

(defun %optimizer-doc-related-implementation-missing-paths ()
  "Return `関連実装` repository paths that do not exist in the checkout."
  (let ((missing nil))
    (loop for line in (uiop:split-string (%optimizer-doc-content)
                                         :separator (list #\Newline))
          for line-no from 1
          when (search "関連実装" line)
            do (dolist (path (%optimizer-doc-backtick-paths line))
                 (unless (probe-file (merge-pathnames path (uiop:getcwd)))
                   (push (format nil "~D:~A" line-no path) missing))))
    (nreverse missing)))

(defun %optimizer-doc-heading-status-table ()
  "Return a hash table mapping optimize roadmap FR ids to heading statuses."
  (let ((table (make-hash-table :test (function equal))))
    (dolist (feature (cl-cc/optimize::optimize-roadmap-doc-features) table)
      (setf (gethash (cl-cc/optimize::opt-roadmap-feature-id feature) table)
            (cl-cc/optimize::opt-roadmap-feature-status feature)))))

(defun %optimizer-doc-completed-sidecar-contradictions ()
  "Return `完了済みFR` entries that do not point at ✅ headings."
  (let ((status-by-id (%optimizer-doc-heading-status-table))
        (contradictions nil))
    (dolist (line (uiop:split-string (%optimizer-doc-content)
                                     :separator (list #\Newline))
                  (nreverse contradictions))
      (when (search "完了済みFR" line)
        (dolist (feature-id (%optimizer-doc-fr-ids-from-line line))
          (let ((status (gethash feature-id status-by-id)))
            (unless (eq status :implemented)
              (push (format nil "~A listed complete with status ~A"
                            feature-id status)
                    contradictions))))))))

(deftest optimize-roadmap-doc-summary-is-status-aware
  "The optimize roadmap summary must not claim every FR is fully implemented."
  (let ((doc (%optimizer-doc-content)))
    (assert-false (search "全FR実装済み" doc))
    (assert-true (search "175 FR を追跡中" doc))
    (assert-true (search "✅" doc))
    (assert-true (search "🔶" doc))))

(deftest optimize-roadmap-completed-headings-avoid-incomplete-language
  "FRs marked ✅ must not contain wording that explicitly says the implementation is absent."
  (assert-null (%optimizer-doc-completed-heading-contradictions)))

(deftest optimize-roadmap-related-implementation-paths-exist
  "Backtick paths on `関連実装` lines must point at files in the checkout."
  (assert-null (%optimizer-doc-related-implementation-missing-paths)))

(deftest optimize-roadmap-completed-sidecar-claims-match-heading-status
  "`完了済みFR` sidecar summaries must only list FR headings marked ✅."
  (assert-null (%optimizer-doc-completed-sidecar-contradictions)))

(deftest optimize-roadmap-evidence-covers-doc-fr-list
  "Every docs/optimize-passes.md FR id has status-aware optimizer roadmap evidence."
  (let* ((expected-ids (%optimizer-doc-fr-ids))
          (features (cl-cc/optimize::optimize-roadmap-doc-features))
          (actual-ids (mapcar #'cl-cc/optimize::opt-roadmap-feature-id features))
          (table (cl-cc/optimize::optimize-roadmap-register-doc-evidence))
          (implemented 0)
          (partial 0)
          (profiles nil))
     (assert-true (> (length expected-ids) 100))
     (assert-equal expected-ids actual-ids)
     (assert-= (length expected-ids) (hash-table-count table))
     (assert-true (some #'cl-cc/optimize::opt-roadmap-feature-marked-complete-p features))
     (dolist (feature features)
       (let* ((feature-id (cl-cc/optimize::opt-roadmap-feature-id feature))
              (status (cl-cc/optimize::opt-roadmap-feature-status feature))
              (evidence (cl-cc/optimize::lookup-opt-roadmap-evidence feature-id)))
         (assert-false (search ":" feature-id))
        (assert-true (member status '(:implemented :partial :planned)))
         (assert-true evidence)
        (assert-eq status (cl-cc/optimize::opt-roadmap-evidence-status evidence))
         (assert-equal feature-id
                       (cl-cc/optimize::opt-roadmap-evidence-feature-id evidence))
         (assert-true
          (cl-cc/optimize::optimize-roadmap-evidence-well-formed-p evidence))
        (push (cl-cc/optimize::opt-roadmap-evidence-modules evidence) profiles)
        (case status
          (:implemented
           (incf implemented)
           (assert-true
            (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
             evidence)))
          (:partial
           (incf partial)
           (assert-false
            (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
             evidence))))))
    (assert-true (> implemented 0))
    (assert-true (> partial 0))
    (assert-true (> (length (remove-duplicates profiles :test #'equal)) 5))))

(deftest optimizer-roadmap-core-passes-have-evidence
  "Core optimizer FRs point at concrete pass implementations and tests."
  (let ((evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-001")))
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (assert-true (member "packages/optimize/src/optimizer.lisp"
                         (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                         :test #'string=))
    (assert-true (member 'cl-cc/optimize::opt-pass-fold
                         (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)))
    (assert-true (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                  evidence))))

(deftest optimizer-roadmap-inline-and-memory-evidence
  "Inline, IPA, and memory FRs point at their dedicated optimizer subsystems."
  (let ((evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-051")))
    (assert-eq :partial (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (assert-true (member "packages/optimize/src/optimizer-inline.lisp"
                         (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                         :test #'string=))
    (assert-true (member 'cl-cc/optimize::opt-pass-devirtualize
                         (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)))
    (assert-false (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                    evidence))))

(deftest optimizer-roadmap-pic-evidence-is-runtime-backed
  "FR-023 must point at PIC/runtime helper evidence instead of coarse inline-only markers."
  (let ((evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-023")))
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (assert-true (member "packages/optimize/src/optimizer-pipeline.lisp"
                         (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                         :test #'string=))
    (assert-true (member 'cl-cc/optimize::opt-ic-transition
                         (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)))
    (assert-false (member 'cl-cc/optimize::opt-pass-devirtualize
                          (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)))
    (assert-true (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                  evidence))))

(deftest optimizer-roadmap-flow-and-ssa-evidence
  "Flow and SSA FRs point at CFG/SSA/flow pass implementations."
  (let ((evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-112")))
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (assert-true (member "packages/optimize/src/ssa.lisp"
                         (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                         :test #'string=))
    (assert-true (member 'cl-cc/optimize::cfg-split-critical-edges
                         (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)))
    (assert-true (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                  evidence))))

(deftest optimize-roadmap-pipeline-includes-modern-optimization-passes
  "The default optimizer pipeline exposes the modern backend passes used by roadmap evidence."
  (dolist (key '(:sccp
                 :reassociate
                 :copy-prop
                 :gvn
                 :store-to-load-forward
                 :dead-store-elim
                 :tail-duplication))
    (assert-true (member key cl-cc/optimize::*opt-default-convergence-pass-keys*))
    (assert-true (gethash key cl-cc/optimize::*opt-pass-registry*))))

(deftest optimizer-roadmap-code-motion-evidence
  "Code-motion FRs distinguish conservative partial passes from implemented ones."
  (let ((partial-evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-167"))
        (implemented-evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-164")))
    (assert-eq :partial (cl-cc/optimize::opt-roadmap-evidence-status partial-evidence))
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status implemented-evidence))
    (assert-true (member 'cl-cc/optimize::opt-pass-tail-duplication
                         (cl-cc/optimize::opt-roadmap-evidence-api-symbols partial-evidence)))
    (assert-false (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                   partial-evidence))
    (assert-true (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                  implemented-evidence))))

(deftest optimizer-roadmap-callee-saved-evidence-is-native-backed
  "FR-329 must point at the native backend/regalloc implementation that computes used callee-saved registers."
  (labels ((function-present-p (package-name symbol-name)
             (let* ((pkg (find-package package-name))
                    (sym (and pkg (find-symbol symbol-name pkg))))
               (and sym (fboundp sym)))))
    (let* ((codegen-spec '("CL-CC/CODEGEN" . "X86-64-USED-CALLEE-SAVED-REGS"))
           (regalloc-spec '("CL-CC/REGALLOC" . "COMPUTE-LIVE-INTERVALS"))
           (evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-329")))
      (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
      (assert-true (member "packages/codegen/src/x86-64-codegen.lisp"
                           (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                           :test #'string=))
      (assert-true (member "packages/regalloc/src/regalloc.lisp"
                           (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                           :test #'string=))
      (assert-true (member codegen-spec
                           (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                           :test #'equal))
      (assert-true (member regalloc-spec
                           (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                           :test #'equal))
      (assert-true (function-present-p :cl-cc/codegen "X86-64-USED-CALLEE-SAVED-REGS"))
      (assert-true (function-present-p :cl-cc/regalloc "COMPUTE-LIVE-INTERVALS"))
      (assert-true (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                    evidence)))))
