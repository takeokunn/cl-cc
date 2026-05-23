;;;; src/jit/package.lisp — JIT compilation subsystem package
;;;; Phase 104-105: Runtime JIT infrastructure

(defpackage :cl-cc/jit
  (:use :cl :cl-cc/bootstrap)
  (:export
   ;; stack-map (FR-550)
   #:*stack-map-table* #:emit-stack-map #:lookup-stack-map
   #:walk-stack-frame #:register-live-set
   ;; safepoints (FR-551)
   #:*safepoint-enabled* #:emit-safepoint-poll #:*safepoint-flag*
   #:*safepoint-interval* #:with-safepoints
   ;; write-barrier (FR-552)
   #:*card-table* #:card-table-mark #:card-table-clear
   #:emit-write-barrier #:barrier-elision-p
   #:with-satb-barrier
   ;; call-stubs (FR-553)
   #:jit-compile-stub #:install-call-stub #:patch-stub-to-direct
   #:*call-stub-size*
   ;; jit-cache (FR-554)
   #:jit-cache-lookup #:jit-cache-insert #:jit-cache-serialize
   #:jit-cache-deserialize #:*jit-cache-dir*
   ;; baseline JIT (FR-559)
   #:jit-baseline-compile #:jit-tier-compile #:*jit-threshold*
   #:*type-feedback-table* #:record-type-feedback
   ;; speculative inlining (FR-560)
   #:speculative-inline-candidate-p #:emit-guarded-inline
   ;; megamorphic (FR-561)
   #:*megamorphic-threshold* #:megamorphic-p #:emit-megamorphic-dispatch
   ;; warmup (FR-562)
   #:jit-warmup-profile #:aot-pre-warm #:*jit-warmup-profile*
   ;; trace jit (FR-558)
   #:*trace-jit-enabled* #:start-trace-recording #:stop-trace-recording
   #:compile-trace #:*trace-threshold*))
