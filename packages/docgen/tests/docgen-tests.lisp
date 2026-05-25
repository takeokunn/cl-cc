;;;; docgen-tests.lisp --- Minimal fixture tests for cl-cc-docgen

(defpackage :cl-cc/docgen-test
  (:use :cl)
  (:import-from :cl-cc/docgen
                :generate-api-docs)
  (:export :run-docgen-tests))

(in-package :cl-cc/docgen-test)

(defun %fixture-path ()
  "Return the source fixture pathname."
  (merge-pathnames "fixture-source.lisp" *load-pathname*))

(defun %assert-contains (needle haystack)
  "Signal an error unless NEEDLE appears in HAYSTACK."
  (unless (search needle haystack :test #'char=)
    (error "Expected output to contain ~S.~%Output:~%~A" needle haystack)))

(defun %assert-not-contains (needle haystack)
  "Signal an error if NEEDLE appears in HAYSTACK."
  (when (search needle haystack :test #'char=)
    (error "Expected output not to contain ~S.~%Output:~%~A" needle haystack)))

(defun run-docgen-tests ()
  "Run minimal API documentation generator fixture tests."
  (let* ((fixture (%fixture-path))
         (markdown (generate-api-docs fixture)))
    (%assert-contains (format nil "# File: ~A" (namestring fixture)) markdown)
    (%assert-contains "## defun documented-function" markdown)
    (%assert-contains "Return X unchanged." markdown)
    (%assert-contains "## defmacro documented-macro" markdown)
    (%assert-contains "Evaluate BODY in order." markdown)
    (%assert-contains "## defstruct documented-struct" markdown)
    (%assert-contains "A documented fixture structure." markdown)
    (%assert-contains "## defclass documented-class" markdown)
    (%assert-contains "A documented fixture class." markdown)
    (%assert-contains "## defparameter *documented-parameter*" markdown)
    (%assert-contains "A documented fixture parameter." markdown)
    (%assert-contains "## defvar *documented-var*" markdown)
    (%assert-contains "A documented fixture variable." markdown)
    (%assert-not-contains "undocumented-function" markdown)
    t))
