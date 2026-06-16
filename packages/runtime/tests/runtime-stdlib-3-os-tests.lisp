;;;; runtime-stdlib-3-os-tests.lisp — Runtime OS/thread/signal FR tests

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest rt-process-management-shell-output
  "FR-1007: rt-shell returns captured stdout."
  (assert-equal "hello" (cl-cc/runtime:rt-shell "printf hello")))

(deftest rt-stackmap-compression-roundtrip
  "FR-1115: stack map delta compression round-trips and safepoint API registers maps."
  (let* ((slots '((8 . :object) (24 . :fixnum) (32 . :object)))
         (compressed (cl-cc/runtime:rt-compress-stackmap-slots slots)))
    (assert-equal slots (cl-cc/runtime:rt-decompress-stackmap-slots compressed))
    (assert-true (search ".gc_map" (cl-cc/runtime:rt-gc-map-section-documentation)))
    (cl-cc/runtime:rt-emit-gc-safepoint :kind :test :frame-id :rt-test-frame :live-slots slots)
    (assert-true (gethash :rt-test-frame cl-cc/runtime::*rt-gc-stackmap-table*))))
