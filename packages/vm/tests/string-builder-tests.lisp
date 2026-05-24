(in-package :cl-cc/test)

(defsuite string-builder-suite
  :description "FR-787 string builder and FR-788 rope tests"
  :parent cl-cc-unit-suite)

(in-suite string-builder-suite)

(deftest string-builder-append-and-finish
  "String builder appends strings, characters, and numbers and returns a flat string."
  :timeout 5
  (let ((builder (cl-cc/vm:make-string-builder :capacity 1)))
    (assert-true (cl-cc/vm:string-builder-p builder))
    (cl-cc/vm:string-builder-append! builder "ab")
    (cl-cc/vm:string-builder-append! builder #\c)
    (cl-cc/vm:string-builder-append! builder 123)
    (assert-equal "abc123" (cl-cc/vm:string-builder-finish builder))))

(deftest string-builder-bulk-length-clear-and-capacity
  "Bulk append, current length, clear, and pre-allocation preserve builder reuse."
  :timeout 5
  (let ((builder (cl-cc/vm:make-string-builder :capacity 1)))
    (cl-cc/vm:string-builder-ensure-capacity! builder 128)
    (cl-cc/vm:string-builder-append-string! builder "hello")
    (assert-equal 5 (cl-cc/vm:string-builder-length builder))
    (assert-equal "hello" (cl-cc/vm:string-builder-finish builder))
    (cl-cc/vm:string-builder-clear! builder)
    (assert-equal 0 (cl-cc/vm:string-builder-length builder))
    (cl-cc/vm:string-builder-append-string! builder "world")
    (assert-equal "world" (cl-cc/vm:string-builder-finish builder))))

(deftest string-builder-performance-metrics-are-linear
  "Performance smoke test: capacity grows linearly with bounded overhead."
  :timeout 5
  (let ((metrics (cl-cc/vm:test-string-builder-performance :iterations 4096 :chunk "x")))
    (assert-equal 4096 (getf metrics :length))
    (assert-true (<= (getf metrics :capacity-per-character) 2))))

(deftest with-string-builder-returns-final-string
  "WITH-STRING-BUILDER has with-output-to-string-like scoped semantics."
  :timeout 5
  (assert-equal "hello world"
                (cl-cc/vm:with-string-builder (out)
                  (cl-cc/vm:string-builder-append! out "hello")
                  (cl-cc/vm:string-builder-append! out #\Space)
                  (cl-cc/vm:string-builder-append! out 'world))))

(deftest rope-concat-split-and-flatten
  "Rope concat creates nodes, split divides at an index, and flatten preserves content."
  :timeout 5
  (let ((rope (cl-cc/vm:rope-concat "hello" (cl-cc/vm:rope-concat " " "world"))))
    (assert-equal 11 (cl-cc/vm:rope-length rope))
    (assert-equal "hello world" (cl-cc/vm:rope-to-string rope))
    (multiple-value-bind (left right) (cl-cc/vm:rope-split rope 6)
      (assert-equal "hello " (cl-cc/vm:rope-to-string left))
      (assert-equal "world" (cl-cc/vm:rope-to-string right)))))

(deftest rope-short-strings-stay-inline
  "Sub-threshold ropes keep a flat string root rather than allocating rope nodes."
  :timeout 5
  (let ((rope (cl-cc/vm:rope-concat "hello" " world")))
    (assert-true (stringp (cl-cc/vm::rope-root rope)))
    (assert-equal "hello world" (cl-cc/vm:rope-to-string rope))))

(deftest rope-long-concat-is-root-node-without-copying-children
  "Long concat is O(1): it creates a root node that points at existing roots."
  :timeout 5
  (let* ((left-text (make-string cl-cc/vm:+rope-inline-threshold+ :initial-element #\a))
         (right-text (make-string cl-cc/vm:+rope-inline-threshold+ :initial-element #\b))
         (left (cl-cc/vm:rope left-text))
         (right (cl-cc/vm:rope right-text))
         (joined (cl-cc/vm:rope-concat left right))
         (root (cl-cc/vm::rope-root joined)))
    (assert-true (cl-cc/vm::rope-node-p root))
    (assert-true (eq left-text (cl-cc/vm::rope-node-left root)))
    (assert-true (eq right-text (cl-cc/vm::rope-node-right root)))
    (assert-equal (* 2 cl-cc/vm:+rope-inline-threshold+)
                  (cl-cc/vm:rope-length joined))))

(deftest rope-insert-delete-substring
  "Rope editing helpers preserve persistent string contents."
  :timeout 5
  (let* ((base (cl-cc/vm:rope-concat
                (make-string cl-cc/vm:+rope-inline-threshold+ :initial-element #\a)
                (make-string cl-cc/vm:+rope-inline-threshold+ :initial-element #\c)))
         (inserted (cl-cc/vm:rope-insert base cl-cc/vm:+rope-inline-threshold+ "bbb"))
         (window (cl-cc/vm:rope-substring inserted (1- cl-cc/vm:+rope-inline-threshold+)
                                          (+ cl-cc/vm:+rope-inline-threshold+ 4)))
         (deleted (cl-cc/vm:rope-delete inserted cl-cc/vm:+rope-inline-threshold+
                                        (+ cl-cc/vm:+rope-inline-threshold+ 3))))
    (assert-equal "abbbc" (cl-cc/vm:rope-to-string window))
    (assert-equal (cl-cc/vm:rope-to-string base)
                  (cl-cc/vm:rope-to-string deleted))))
