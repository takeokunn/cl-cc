;;; runtime-stdlib-3-tests.lisp — FR verification tests for runtime-stdlib-3 gaps

(in-package :cl-cc/test)

(defsuite runtime-stdlib-3-vm-suite
  :description "Runtime-stdlib-3 VM FR verification tests"
  :parent cl-cc-unit-suite)

(in-suite runtime-stdlib-3-vm-suite)

(deftest runtime-stdlib-3-format-p-and-w
  "FR-965: native FORMAT supports ~P and ~W."
  (assert-equal "1 file" (cl-cc/vm::%vm-format-native "~D file~P" '(1)))
  (assert-equal "2 files" (cl-cc/vm::%vm-format-native "~D file~P" '(2)))
  (assert-equal "(1 2)" (cl-cc/vm::%vm-format-native "~W" '((1 2)))))

(deftest runtime-stdlib-3-external-format-roundtrip
  "FR-959: external-format helpers expose UTF-8 encode/decode round trips."
  (let* ((text "hello")
         (octets (cl-cc/vm:encode-external-format text)))
    (assert-true (vectorp octets))
    (assert-equal text (cl-cc/vm:decode-external-format octets))))

(deftest runtime-stdlib-3-type-and-numeric-helpers
  "FR-986/1112: type and numeric helper scaffolding is callable."
  (assert-equal 3 (cl-cc/vm:vm-check-type 3 'integer))
  (assert-equal 5 (cl-cc/vm:clamp 7 0 5))
  (assert-equal 1 (cl-cc/vm:wrap 6 0 5))
  (assert-equal 15 (cl-cc/vm:lerp 10 20 1/2))
  (assert-equal -1 (cl-cc/vm:vm-signum -42)))

(deftest runtime-stdlib-3-environment-and-reader-vars
  "FR-1006/1054/1066/1082: exported state variables and env helpers exist."
  (assert-true (find :cl-cc cl-cc/vm:*features*))
  (assert-equal "cl-cc" (cl-cc/vm:lisp-implementation-type))
  (assert-= 10 cl-cc/vm:*read-base*)
  (assert-true (cl-cc/vm:source-location-p
                (cl-cc/vm:make-source-location :pathname #P"x.lisp" :line 1 :column 0)))
  (let ((name "CL_CC_RUNTIME_STDLIB_3_TEST"))
    (cl-cc/vm:setenv name "ok")
    (assert-equal "ok" (cl-cc/vm:getenv name))))
