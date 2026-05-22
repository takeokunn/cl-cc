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
