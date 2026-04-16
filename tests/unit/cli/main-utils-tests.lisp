;;;; tests/unit/cli/main-utils-tests.lisp — utility helper coverage

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest cli-dump-phase-label-lowercases-keywords
  (assert-string= "ast" (cl-cc/cli::%dump-phase-label :ast))
  (assert-string= "ssa" (cl-cc/cli::%dump-phase-label :SSA)))

(deftest cli-parse-ir-phase-supported-values
  (assert-eq :ast (cl-cc/cli::%parse-ir-phase "ast"))
  (assert-eq :cps (cl-cc/cli::%parse-ir-phase "CPS"))
  (assert-eq :ssa (cl-cc/cli::%parse-ir-phase "ssa"))
  (assert-eq :vm  (cl-cc/cli::%parse-ir-phase "vm"))
  (assert-eq :opt (cl-cc/cli::%parse-ir-phase "opt"))
  (assert-eq :asm (cl-cc/cli::%parse-ir-phase "asm"))
  (assert-null (cl-cc/cli::%parse-ir-phase "bogus")))

(deftest cli-ensure-list-normalizes-inputs
  (assert-null (cl-cc/cli::%ensure-list nil))
  (assert-equal '(1 2) (cl-cc/cli::%ensure-list '(1 2)))
  (assert-equal '(x) (cl-cc/cli::%ensure-list 'x)))

(deftest cli-call-with-optional-output-file-passes-nil-when-missing
  (let ((seen :unset))
    (cl-cc/cli::%call-with-optional-output-file nil
                                                (lambda (stream)
                                                  (setf seen stream)
                                                  :ok))
    (assert-null seen)))

(deftest cli-call-with-optional-output-file-writes-file-when-path-present
  (uiop:with-temporary-file (:pathname path :type "txt" :keep t)
    (let ((result (cl-cc/cli::%call-with-optional-output-file
                   path
                   (lambda (stream)
                     (write-string "hello" stream)
                     :written))))
      (assert-eq :written result)
      (assert-string= "hello" (cl-cc/cli::%read-file path))
      (ignore-errors (delete-file path)))))

(deftest cli-svg-escape-escapes-special-characters
  (assert-string= "&lt;tag attr=&quot;a&amp;b&quot;&gt;"
                  (cl-cc/cli::%svg-escape "<tag attr=\"a&b\">") ))

(deftest cli-flamegraph-color-has-special-cases
  (assert-string= "rgb(90,140,255)" (cl-cc/cli::%flamegraph-color "minor-gc"))
  (assert-string= "rgb(255,165,0)" (cl-cc/cli::%flamegraph-color "jit-compile"))
  (assert-true (search "hsl(" (cl-cc/cli::%flamegraph-color "ordinary-frame"))))

(deftest cli-flamegraph-build-tree-aggregates-counts
  (let ((samples (make-hash-table :test #'equal)))
    (setf (gethash "root;alpha" samples) 2)
    (setf (gethash "root;beta" samples) 3)
    (let ((tree (cl-cc/cli::%flamegraph-build-tree samples)))
      (assert-string= "root" (getf tree :name))
      (assert-= 5 (getf tree :count))
      (assert-true (gethash "root" (getf tree :children))))))

(deftest cli-flamegraph-children-list-sorts-by-name
  (let* ((a (list :name "zeta" :count 1 :children (make-hash-table :test #'equal)))
         (b (list :name "alpha" :count 1 :children (make-hash-table :test #'equal)))
         (node (list :name "root" :count 2 :children (make-hash-table :test #'equal))))
    (setf (gethash "zeta" (getf node :children)) a)
    (setf (gethash "alpha" (getf node :children)) b)
    (assert-equal '("alpha" "zeta")
                  (mapcar (lambda (child) (getf child :name))
                          (cl-cc/cli::%flamegraph-children-list node)))))

(deftest cli-write-flamegraph-svg-emits-svg-document
  (uiop:with-temporary-file (:pathname path :type "svg" :keep t)
    (let ((samples (make-hash-table :test #'equal)))
      (setf (gethash "top;child" samples) 4)
      (assert-equal path (cl-cc/cli::%write-flamegraph-svg path samples))
      (let ((svg (cl-cc/cli::%read-file path)))
        (assert-true (search "<svg" svg))
        (assert-true (search "top" svg))
        (assert-true (search "child" svg)))
      (ignore-errors (delete-file path)))))
