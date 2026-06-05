;;;; tests/unit/cli/main-utils-tests.lisp — utility helper coverage

(in-package :cl-cc/test)

(in-suite cl-cc-cli-pure-suite)

(deftest cli-dump-phase-label-lowercases-keywords
  "%dump-phase-label converts IR phase keywords to their lowercase string labels."
  (assert-string= "ast" (cl-cc/cli::%dump-phase-label :ast))
  (assert-string= "ssa" (cl-cc/cli::%dump-phase-label :SSA)))

(deftest-each cli-parse-ir-phase-supported-values
  "Each recognized phase string maps to its keyword; unknown strings return nil."
  :cases (("ast"   :ast "ast")
          ("CPS"   :cps "CPS")
          ("ssa"   :ssa "ssa")
          ("vm"    :vm  "vm")
          ("opt"   :opt "opt")
          ("asm"   :asm "asm")
          ("bogus" nil  "bogus"))
  (expected input)
  (assert-eq expected (cl-cc/cli::%parse-ir-phase input)))

(deftest cli-ensure-list-normalizes-inputs
  "%ensure-list returns nil for nil, wraps atoms in a list, and passes lists through unchanged."
  (assert-null (cl-cc/cli::%ensure-list nil))
  (assert-equal '(1 2) (cl-cc/cli::%ensure-list '(1 2)))
  (assert-equal '(x) (cl-cc/cli::%ensure-list 'x)))

(deftest cli-call-with-optional-output-file-passes-nil-when-missing
  "%call-with-optional-output-file calls the callback with nil when no output path is provided."
  (let ((seen :unset))
    (cl-cc/cli::%call-with-optional-output-file nil
                                                (lambda (stream)
                                                  (setf seen stream)
                                                  :ok))
    (assert-null seen)))

(deftest cli-call-with-optional-output-file-writes-file-when-path-present
  "%call-with-optional-output-file opens the output file, passes the stream to the callback, and returns the callback result."
  (uiop:with-temporary-file (:pathname path :type "txt" :keep t)
    (let ((result (cl-cc/cli::%call-with-optional-output-file
                   path
                   (lambda (stream)
                     (write-string "hello" stream)
                     :written))))
      (assert-eq :written result)
      (assert-string= "hello" (cl-cc/cli::%read-file path))
      (ignore-errors (delete-file path)))))

(deftest cli-get-timeout-defaults-to-30-seconds
  "Non-interactive commands receive the documented 30s timeout by default."
  (let ((parsed (cl-cc/cli:parse-args '("eval" "(+ 1 2)"))))
    (assert-= 30 (cl-cc/cli::%get-timeout parsed))))

(deftest cli-get-timeout-uses-explicit-positive-value
  "An explicit --timeout value overrides the default."
  (let ((parsed (cl-cc/cli:parse-args '("eval" "(+ 1 2)" "--timeout" "9"))))
    (assert-= 9 (cl-cc/cli::%get-timeout parsed))))

(deftest cli-get-timeout-no-timeout-disables-timeout
  "--no-timeout disables the default timeout for debugging."
  (let ((parsed (cl-cc/cli:parse-args '("eval" "(+ 1 2)" "--no-timeout"))))
    (assert-null (cl-cc/cli::%get-timeout parsed))))

(deftest cli-get-timeout-keeps-positive-integer-validation
  "--timeout still rejects zero and non-positive values."
  (let ((parsed (cl-cc/cli:parse-args '("eval" "(+ 1 2)" "--timeout" "0"))))
    (assert-signals cl-cc/cli:arg-parse-error
      (cl-cc/cli::%get-timeout parsed))))

(deftest cli-svg-escape-escapes-special-characters
  "%svg-escape replaces <, >, \", and & with their XML entity equivalents."
  (assert-string= "&lt;tag attr=&quot;a&amp;b&quot;&gt;"
                  (cl-cc/cli::%svg-escape "<tag attr=\"a&b\">") ))

(deftest cli-flamegraph-color-minor-gc-is-blue
  "minor-gc frames use the fixed blue RGB color."
  (assert-string= "rgb(90,140,255)" (cl-cc/cli::%flamegraph-color "minor-gc")))

(deftest cli-flamegraph-color-jit-compile-is-orange
  "jit-compile frames use the fixed orange RGB color."
  (assert-string= "rgb(255,165,0)" (cl-cc/cli::%flamegraph-color "jit-compile")))

(deftest cli-flamegraph-color-ordinary-frame-uses-hsl
  "Ordinary frames use hsl() color format."
  (assert-true (search "hsl(" (cl-cc/cli::%flamegraph-color "ordinary-frame"))))

(deftest cli-flamegraph-build-tree-aggregates-counts
  "%flamegraph-build-tree aggregates sample counts and creates a named root node with children."
  (let ((samples (make-hash-table :test #'equal)))
    (setf (gethash "root;alpha" samples) 2)
    (setf (gethash "root;beta" samples) 3)
    (let ((tree (cl-cc/cli::%flamegraph-build-tree samples)))
      (assert-string= "root" (getf tree :name))
      (assert-= 5 (getf tree :count))
      (assert-true (gethash "root" (getf tree :children))))))

(deftest cli-flamegraph-children-list-sorts-by-name
  "%flamegraph-children-list returns a list of child nodes sorted alphabetically by name."
  (let* ((a (list :name "zeta" :count 1 :children (make-hash-table :test #'equal)))
         (b (list :name "alpha" :count 1 :children (make-hash-table :test #'equal)))
         (node (list :name "root" :count 2 :children (make-hash-table :test #'equal))))
    (setf (gethash "zeta" (getf node :children)) a)
    (setf (gethash "alpha" (getf node :children)) b)
    (assert-equal '("alpha" "zeta")
                  (mapcar (lambda (child) (getf child :name))
                          (cl-cc/cli::%flamegraph-children-list node)))))

(deftest cli-write-flamegraph-svg-emits-svg-document
  "%write-flamegraph-svg writes a valid SVG document containing the sampled frame names."
  (uiop:with-temporary-file (:pathname path :type "svg" :keep t)
    (let ((samples (make-hash-table :test #'equal)))
      (setf (gethash "top;child" samples) 4)
      (assert-equal path (cl-cc/cli::%write-flamegraph-svg path samples))
      (let ((svg (cl-cc/cli::%read-file path)))
        (assert-true (search "<svg" svg))
        (assert-true (search "top" svg))
        (assert-true (search "child" svg)))
      (ignore-errors (delete-file path)))))

(deftest fr-702-flamegraph-svg-generation-writes-rectangles-and-sample-titles
  "FR-702: flamegraph SVG generation produces a renderable SVG with frame metadata."
  (uiop:with-temporary-file (:pathname path :type "svg" :keep t)
    (let ((samples (make-hash-table :test #'equal)))
      (setf (gethash "compiler;optimizer;bolt" samples) 5)
      (setf (gethash "compiler;gc" samples) 2)
      (cl-cc/cli::%write-flamegraph-svg path samples)
      (let ((svg (cl-cc/cli::%read-file path)))
        (assert-true (search "<svg" svg))
        (assert-true (search "<rect" svg))
        (assert-true (search "optimizer" svg))
        (assert-true (search "bolt (5 samples)" svg))
        (assert-true (search "rgb(90,140,255)" svg)))
      (ignore-errors (delete-file path)))))

;;; ─── %flamegraph-depth-of (extracted helper) ────────────────────────────────

(deftest flamegraph-depth-of-single-node
  "%flamegraph-depth-of on a leaf node updates max-depth-cell to given depth."
  (let ((node '(:name "root" :count 1 :children nil))
        (cell (list 0)))
    (cl-cc/cli::%flamegraph-depth-of node 3 cell)
    (assert-= 3 (car cell))))

(deftest flamegraph-depth-of-finds-max
  "%flamegraph-depth-of traverses children and finds deepest nesting."
  (let* ((child1 '(:name "c1" :count 1 :children nil))
         (child2 '(:name "c2" :count 1 :children nil))
         (children (let ((ht (make-hash-table :test #'equal)))
                     (setf (gethash "c1" ht) child1
                           (gethash "c2" ht) child2)
                     ht))
         (root (list :name "root" :count 2 :children children))
         (cell (list 0)))
    (cl-cc/cli::%flamegraph-depth-of root 0 cell)
    (assert-= 1 (car cell))))
