;;;; packages/runtime/tests/runtime-stdlib-3-image-tests.lisp — FR-1002/FR-1003

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(defparameter *rt-core-test-root* nil)
(defparameter *rt-core-test-closure* nil)
(defparameter *rt-core-test-instance* nil)
(defparameter *rt-core-test-cycle* nil)
(defparameter *rt-core-test-string* nil)

(defclass rt-core-test-node ()
  ((name :initarg :name :accessor rt-core-test-node-name)
   (next :initarg :next :accessor rt-core-test-node-next)))

(defun %rt-core-test-path (name)
  (namestring (merge-pathnames (make-pathname :name name :type "core")
                               (uiop:temporary-directory))))

(defun %rt-core-file-size (path)
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (file-length in)))

(defun %rt-core-register-test-roots ()
  (mapc #'cl-cc/runtime:rt-image-register-global
        '(*rt-core-test-root*
          *rt-core-test-closure*
          *rt-core-test-instance*
          *rt-core-test-cycle*
          *rt-core-test-string*)))

(deftest runtime-core-roundtrip-full-heap-roots
  "FR-1002/FR-1003: core save/load restores registered reachable heap graph."
  (%rt-core-register-test-roots)
  (let ((path (%rt-core-test-path "clcc-roundtrip")))
    (setf *rt-core-test-root* (list :numbers #(1 2 3) :table (make-hash-table :test 'equal)))
    (setf (gethash "answer" (getf *rt-core-test-root* :table)) '(4 2))
    (cl-cc/runtime:rt-save-core path :compression :none)
    (setf *rt-core-test-root* :mutated)
    (cl-cc/runtime:rt-load-core path)
    (assert-equal :numbers (first *rt-core-test-root*))
    (assert-true (equalp #(1 2 3) (second *rt-core-test-root*)))
    (assert-equal '(4 2) (gethash "answer" (getf *rt-core-test-root* :table)))))

(deftest runtime-core-closure-serialization
  "FR-1002: saved host closures are reattached through the function registry."
  (%rt-core-register-test-roots)
  (let ((path (%rt-core-test-path "clcc-closure")))
    (setf *rt-core-test-closure* (let ((n 40)) (lambda (x) (+ n x))))
    (cl-cc/runtime:rt-save-core path :compression :none)
    (setf *rt-core-test-closure* (lambda (x) x))
    (cl-cc/runtime:rt-load-core path)
    (assert-equal 42 (funcall *rt-core-test-closure* 2))))

(deftest runtime-core-clos-instance-serialization
  "FR-1002: core serializer preserves CLOS class identity and slot values."
  (%rt-core-register-test-roots)
  (let ((path (%rt-core-test-path "clcc-clos"))
        (node (make-instance 'rt-core-test-node :name "root")))
    (setf (rt-core-test-node-next node) node
          *rt-core-test-instance* node)
    (cl-cc/runtime:rt-save-core path :compression :none)
    (setf *rt-core-test-instance* nil)
    (cl-cc/runtime:rt-load-core path)
    (assert-equal "root" (rt-core-test-node-name *rt-core-test-instance*))
    (assert-eq *rt-core-test-instance*
               (rt-core-test-node-next *rt-core-test-instance*))))

(deftest runtime-core-circular-structure-serialization
  "FR-1002: forwarding-table graph copy preserves circular cons references."
  (%rt-core-register-test-roots)
  (let ((path (%rt-core-test-path "clcc-cycle"))
        (cell (cons :head nil)))
    (setf (cdr cell) cell
          *rt-core-test-cycle* cell)
    (cl-cc/runtime:rt-save-core path :compression :none)
    (setf *rt-core-test-cycle* nil)
    (cl-cc/runtime:rt-load-core path)
    (assert-eq :head (car *rt-core-test-cycle*))
    (assert-eq *rt-core-test-cycle* (cdr *rt-core-test-cycle*))))

(deftest runtime-core-compression-smaller-file
  "FR-1002: zstd/lz4 portable compression produces a smaller repetitive core."
  (%rt-core-register-test-roots)
  (let ((plain (%rt-core-test-path "clcc-plain"))
        (compressed (%rt-core-test-path "clcc-compressed")))
    (setf *rt-core-test-string* (make-string 8192 :initial-element #\A))
    (cl-cc/runtime:rt-save-core plain :compression :none)
    (cl-cc/runtime:rt-save-core compressed :compression :zstd)
    (assert-true (< (%rt-core-file-size compressed)
                    (%rt-core-file-size plain)))
    (setf *rt-core-test-string* nil)
    (cl-cc/runtime:rt-load-core compressed)
    (assert-equal 8192 (length *rt-core-test-string*))))

(deftest runtime-core-loading-with-mmap
  "FR-1003: rt-load-core records mmap-backed lazy-loading metadata."
  (%rt-core-register-test-roots)
  (let ((path (%rt-core-test-path "clcc-mmap")))
    (setf *rt-core-test-root* '(:mmap t))
    (cl-cc/runtime:rt-save-core path :compression :none)
    (let ((core (cl-cc/runtime:rt-load-core path)))
      (assert-equal (pathname path) cl-cc/runtime:*saved-core-pathname*)
      (assert-true (getf core :lazy-loading))
      (assert-equal :offset-relative (getf core :aslr))
      (assert-equal '(:mmap t) *rt-core-test-root*))))
