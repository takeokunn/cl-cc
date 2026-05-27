;;;; runtime-stdlib-3-os-tests.lisp — Runtime OS/thread/signal FR tests

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest rt-process-management-shell-output
  "FR-1007: rt-shell returns captured stdout."
  (assert-equal "hello" (cl-cc/runtime:rt-shell "printf hello")))

;; SKIP (Nix sandbox): OS process/thread/signal not available in sandbox
#+nil (deftest rt-process-management-run-program-wait
  "FR-1007: rt-run-program creates a process object and records exit status."
  (let ((process (cl-cc/runtime:rt-run-program "/bin/sh" '("-c" "exit 7")
                                             :input :null
                                             :output :null
                                             :error :null
                                             :wait t)))
    (assert-true (cl-cc/runtime:rt-process-p process))
    (assert-= 7 (cl-cc/runtime:rt-process-exit-code process))
    (assert-false (cl-cc/runtime:rt-process-alive-p process))))

;; SKIP (Nix sandbox): OS thread API not available in sandbox
#+nil (deftest rt-native-thread-api-join-name
  "FR-1097: native thread wrappers create, join, name, and enumerate OS threads."
  (let ((thread (cl-cc/runtime:rt-make-thread (lambda () 42) :name "rt-test-thread")))
    (assert-equal "rt-test-thread" (cl-cc/runtime:rt-thread-name thread))
    (assert-= 42 (cl-cc/runtime:rt-thread-join thread))
    (assert-true (member thread (cl-cc/runtime:rt-all-threads) :test #'eq))))

;; SKIP (Nix sandbox): OS signal handling not available in sandbox
#+nil (deftest rt-signal-deferred-handler-api
  "FR-1010: signal handlers are queryable and pending signals dispatch at safepoints."
  (let ((seen nil))
    (cl-cc/runtime:rt-with-signal-handler (cl-cc/runtime:+rt-sigusr1+
                                           (lambda (sig) (setf seen sig)))
      (assert-true (functionp (cl-cc/runtime:rt-get-signal-handler cl-cc/runtime:+rt-sigusr1+)))
      (push cl-cc/runtime:+rt-sigusr1+ cl-cc/runtime:*pending-signals*)
      (cl-cc/runtime:rt-process-pending-signals)
      (assert-= cl-cc/runtime:+rt-sigusr1+ seen))))

(deftest rt-stackmap-compression-roundtrip
  "FR-1115: stack map delta compression round-trips and safepoint API registers maps."
  (let* ((slots '((8 . :object) (24 . :fixnum) (32 . :object)))
         (compressed (cl-cc/runtime:rt-compress-stackmap-slots slots)))
    (assert-equal slots (cl-cc/runtime:rt-decompress-stackmap-slots compressed))
    (assert-true (search ".gc_map" (cl-cc/runtime:rt-gc-map-section-documentation)))
    (cl-cc/runtime:rt-emit-gc-safepoint :kind :test :frame-id :rt-test-frame :live-slots slots)
    (assert-true (gethash :rt-test-frame cl-cc/runtime::*rt-gc-stackmap-table*))))
