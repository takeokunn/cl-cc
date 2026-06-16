;;;; packages/runtime/src/package.lisp - CL-CC Runtime Library Package

(defpackage :cl-cc/runtime
  (:use :cl)
  (:shadow #:compute-applicable-methods #:make-hash-table)
  (:export
   ;; Tagged pointer constants (3-bit native tag values, used by runtime.lisp / heap.lisp)
   #:+tag-fixnum+ #:+rt-tag-cons+ #:+rt-tag-symbol+ #:+rt-tag-function+
   #:+tag-character+ #:+tag-array+ #:+rt-tag-string+ #:+tag-other+
   ;; Tag helpers
   #:rt-tag-fixnum #:rt-untag-fixnum #:rt-tag-bits
   ;; Type predicates
   #:rt-consp #:rt-null-p #:rt-symbolp #:rt-functionp
   #:rt-numberp #:rt-integerp #:rt-floatp #:rt-stringp
   #:rt-characterp #:rt-vectorp #:rt-listp #:rt-atomp #:rt-keywordp
   #:rt-typep #:rt-type-of
   ;; Cons/list
   #:rt-cons #:rt-car #:rt-cdr #:rt-rplaca #:rt-rplacd
   #:rt-make-list #:rt-list-length #:rt-append #:rt-nconc
   #:rt-reverse #:rt-nreverse #:rt-member #:rt-nth #:rt-nthcdr
   #:rt-last #:rt-butlast #:rt-copy-list #:rt-copy-tree
   #:rt-assoc #:rt-acons #:rt-subst
   #:rt-first #:rt-second #:rt-third #:rt-fourth #:rt-fifth #:rt-rest
   #:rt-endp #:rt-null #:rt-push-list #:rt-pop-list
   #:rt-equal #:rt-listp #:rt-atomp
   #:rt-string-coerce #:rt-coerce-to-string #:rt-coerce-to-list #:rt-coerce-to-vector
   ;; Arrays/vectors
   #:rt-make-array #:rt-aref #:rt-aset #:rt-array-length
   #:rt-array-rank #:rt-array-dimension #:rt-array-dimensions #:rt-array-total-size
   #:rt-row-major-aref #:rt-array-row-major-index
   #:rt-vector-push #:rt-vector-push-extend #:rt-vector-pop
   #:rt-fill-pointer #:rt-set-fill-pointer
   #:rt-array-has-fill-pointer-p #:rt-array-adjustable-p
   #:rt-adjust-array #:rt-array-displacement
   #:rt-svref #:rt-svset #:rt-vectorp
   #:rt-bit-access #:rt-bit-set #:rt-bit-and #:rt-bit-or #:rt-bit-xor #:rt-bit-not #:rt-sbit
   ;; Arithmetic
   #:rt-add #:rt-sub #:rt-mul #:rt-div #:rt-mod #:rt-rem
   #:rt-neg #:rt-abs #:rt-inc #:rt-dec #:rt-min #:rt-max
   #:rt-cl-and #:rt-cl-or #:rt-not
   ;; Bitwise
   #:rt-ash #:rt-logand #:rt-logior #:rt-logxor #:rt-logeqv #:rt-lognot
   #:rt-logtest #:rt-logbitp #:rt-logcount #:rt-integer-length
   ;; Comparisons
   #:rt-eq #:rt-eql #:rt-equal-fn #:rt-lt #:rt-gt #:rt-le #:rt-ge #:rt-num-eq
   ;; Math
   #:rt-expt #:rt-sqrt #:rt-exp #:rt-log #:rt-sin #:rt-cos #:rt-tan
   #:rt-asin #:rt-acos #:rt-atan #:rt-atan2
   #:rt-sinh #:rt-cosh #:rt-tanh
   #:rt-floor #:rt-ceiling #:rt-truncate #:rt-round
   #:rt-ffloor #:rt-fceiling #:rt-ftruncate #:rt-fround
   #:rt-float #:rt-float-precision #:rt-float-radix #:rt-float-sign #:rt-float-digits
   #:rt-decode-float #:rt-integer-decode-float #:rt-scale-float
   #:rt-rational #:rt-rationalize #:rt-numerator #:rt-denominator
   #:rt-realpart #:rt-imagpart #:rt-conjugate #:rt-phase #:rt-complex
   #:rt-gcd #:rt-lcm
   #:rt-evenp #:rt-oddp #:rt-zerop #:rt-plusp #:rt-minusp
    ;; Closures/functions
    #:rt-make-closure #:rt-closure-ref #:rt-call-fn #:rt-apply-fn
    #:rt-call-next-method #:rt-next-method-p #:rt-register-function
    #:rt-code-cache-lookup #:rt-code-cache-store #:rt-code-cache-evict
    #:rt-code-cache-stats #:*rt-code-cache*
   ;; Multiple values
   #:rt-values-clear #:rt-values-push #:rt-values-count
   #:rt-values-ref #:rt-values-to-list #:rt-spread-values #:rt-ensure-values
   ;; Global bindings
   #:rt-get-global #:rt-set-global
   ;; Strings
   #:rt-make-string #:rt-string-length #:rt-string-ref #:rt-string-set
   #:*rt-string-dedup-table* #:rt-string-dedup #:rt-string-intern
   #:rt-string= #:rt-string< #:rt-string> #:rt-string<= #:rt-string>=
   #:rt-string-equal-ci #:rt-string-lessp #:rt-string-greaterp
   #:rt-string-not-equal #:rt-string-not-greaterp #:rt-string-not-lessp
   #:rt-string-upcase #:rt-string-downcase #:rt-string-capitalize
   #:rt-string-trim #:rt-string-left-trim #:rt-string-right-trim
   #:rt-search-string #:rt-subseq #:rt-concatenate-seqs
   #:rt-stringp
   ;; Characters
   #:rt-char #:rt-char-code #:rt-code-char
   #:rt-char-equal-cs #:rt-char-lt-cs #:rt-char-gt-cs #:rt-char-le-cs #:rt-char-ge-cs #:rt-char-ne-cs
   #:rt-char-equal-ci #:rt-char-not-equal-ci #:rt-char-lessp-ci #:rt-char-greaterp-ci
   #:rt-char-not-lessp-ci #:rt-char-not-greaterp-ci
   #:rt-char-upcase #:rt-char-downcase
   #:rt-alpha-char-p #:rt-digit-char-p #:rt-alphanumericp
   #:rt-upper-case-p #:rt-lower-case-p #:rt-both-case-p
   #:rt-graphic-char-p #:rt-standard-char-p
   #:rt-digit-char #:rt-char-name #:rt-name-char
   #:rt-characterp
   ;; Symbols
   #:rt-symbol-name #:rt-intern #:rt-make-symbol #:rt-gensym
   #:rt-symbol-value #:rt-set-symbol-value
   #:rt-symbol-plist #:rt-get-prop #:rt-put-prop #:rt-remprop
   #:rt-1+ #:rt-1- #:rt-+ #:rt-- #:rt-* #:rt-/ #:rt-< #:rt-> #:rt-<= #:rt->=
   #:rt-max #:rt-min #:rt-length #:rt-char= #:rt-char-equal #:rt-eql #:rt-equal
   #:rt-equalp #:rt-elt #:rt-append
   #:rt-keywordp #:rt-parse-integer
   ;; Hash tables
   #:rt-make-hash-table #:rt-gethash #:rt-sethash #:rt-remhash #:rt-clrhash
   #:rt-hash-count #:rt-hash-test #:rt-maphash #:rt-hash-keys #:rt-hash-values
    #:rt-hash-table-p #:rt-hash-table-weakness
     ;; Weak hash tables (FR-448, FR-449)
     #:rt-weak-hash-table #:rt-weak-hash-table-p
     #:rt-weak-hash-entry #:make-rt-weak-hash-entry
    #:rtwhe-key #:rtwhe-value #:rtwhe-key-ephemeron #:rtwhe-value-ephemeron
   ;; CLOS
   #:rt-defclass #:rt-make-instance #:rt-slot-value #:rt-slot-set
   #:rt-slot-boundp #:rt-slot-makunbound #:rt-slot-exists-p
     #:rt-class-name #:rt-class-of #:rt-find-class #:rt-register-method #:rt-call-generic
     #:rt-compute-applicable-methods #:compute-applicable-methods
     #:rt-make-instance-0
     #:*rt-class-registry* #:*rt-generic-function-registry*
    ;; Conditions
    #:rt-signal-error #:rt-signal #:rt-warn-fn #:rt-cerror
    #:rt-invoke-restart
    #:*handler-stack* #:*restart-stack*
    #:rt-handler #:rt-handler-p #:make-rt-handler
    #:rt-handler-condition-type #:rt-handler-handler-function
    #:rt-restart #:rt-restart-p #:make-rt-restart
    #:rt-restart-name #:rt-restart-function
    #:rt-push-handler #:rt-pop-handler #:rt-establish-handler
    #:rt-find-handler #:rt-dispatch-signal
    #:rt-push-restart #:rt-pop-restart #:rt-find-restart
    #:rt-establish-restart #:rt-restart-bind #:rt-restart-case
    #:rt-dispatch-restart
     ;; Misc
      #:rt-boundp #:rt-fboundp #:rt-makunbound
      ;; C embedding API (FR-812)
      #:cl-cc-state #:cl-cc-state-p #:make-cl-cc-state
      #:cl-cc-state-id #:cl-cc-state-vm #:cl-cc-state-package
      #:cl-cc-state-callbacks #:cl-cc-state-last-error #:cl-cc-state-closed-p
      #:cl-cc-value #:cl-cc-value-p #:make-cl-cc-value
      #:cl-cc-value-kind #:cl-cc-value-payload
      #:cl-cc-error #:cl-cc-error-p #:make-cl-cc-error
      #:cl-cc-error-code #:cl-cc-error-message
      #:cl-cc-init #:cl-cc-eval #:cl-cc-call #:cl-cc-cleanup
      #:cl-cc-last-error #:cl-cc-register-callback #:cl-cc-callback
      #:|cl_cc_init| #:|cl_cc_eval| #:|cl_cc_call| #:|cl_cc_cleanup|
      #:|cl_cc_last_error| #:|cl_cc_register_callback| #:|cl_cc_get_callback|
    #:rt-random #:rt-make-random-state
   #:rt-get-universal-time #:rt-get-internal-real-time #:rt-get-internal-run-time
    #:rt-read-from-string #:rt-read-sexp
    #:rt-coerce
    ;; Runtime region API (FR-254 integration)
    #:rt-make-region #:rt-close-region #:rt-region-active-p
    #:rt-region-alloc #:rt-region-ref-valid-p #:rt-region-deref #:rt-with-region
    #:rt-region-capacity #:rt-region-used
    ;; I/O
   #:rt-print #:rt-princ #:rt-prin1 #:rt-terpri #:rt-fresh-line
   #:rt-write-char #:rt-write-string #:rt-write-line #:rt-write-byte
   #:rt-format #:rt-read-char #:rt-read-line #:rt-read-byte
   #:rt-peek-char #:rt-unread-char
   #:rt-open-file #:rt-close-file
   #:rt-make-string-stream
   #:rt-make-string-output-stream #:rt-get-output-stream-string #:rt-stream-write-string
    #:rt-finish-output #:rt-force-output #:rt-clear-output
    #:rt-input-stream-p #:rt-output-stream-p #:rt-open-stream-p #:rt-interactive-stream-p
    #:rt-stream-element-type
    #:rt-fboundp #:rt-intern #:rt-gensym
    #:rt-symbol-value
    #:rt-find-package #:rt-make-package #:rt-use-package #:rt-unuse-package
    #:rt-export #:rt-package-name
    #:*rt-package-registry*
    #:rt-write-to-string
   ;; ---------------------------------------------------------------
   ;; NaN-boxing value representation (value.lisp)
   ;; ---------------------------------------------------------------
   ;; Bit-pattern constants
   #:+nan-boxing-quiet-nan+
   #:+ptr-base+ #:+ptr-mask+ #:+tag-mask+ #:+addr-mask+
   #:+tag-object+ #:+tag-cons+ #:+tag-symbol+ #:+tag-function+ #:+tag-string+
   #:+tag-char+
   #:+fixnum-tag+ #:+fixnum-mask+ #:+fixnum-shift+
   #:+val-nil+ #:+val-t+ #:+val-unbound+
   #:+nan-tag-base+
   ;; Type predicates
   #:val-fixnum-p #:val-double-p #:val-pointer-p
   #:val-nil-p #:val-t-p #:val-char-p #:val-unbound-p
   #:val-object-p #:val-cons-p #:val-symbol-p #:val-function-p #:val-string-p
   ;; Encode / decode
   #:encode-fixnum #:decode-fixnum
   #:encode-double #:decode-double
   #:encode-pointer #:decode-pointer #:pointer-tag
   #:encode-char #:decode-char
    #:encode-bool
    #:+rt-bignum-type-tag+ #:+rt-bignum-digit-base+
    #:rt-native-bignum-allocate #:rt-native-bignum-p
    #:rt-native-bignum-to-integer #:rt-native-integer->value
    #:rt-native-bignum-add #:rt-native-bignum-sub #:rt-native-bignum-mul
    ;; CL interop
   #:cl-value->val #:val->cl-value
   ;; ---------------------------------------------------------------
   ;; Register file and stack frames (frame.lisp)
   ;; ---------------------------------------------------------------
   ;; Frame constants
   #:+frame-register-count+
   #:+frame-caller-save-start+ #:+frame-caller-save-end+
   #:+frame-callee-save-start+ #:+frame-callee-save-end+
   #:+frame-arg-start+ #:+frame-arg-end+
   #:+frame-return-reg+ #:+frame-spill-start+
   #:+frame-pool-size+
   ;; vm-frame struct + accessors
   #:vm-frame
   #:vm-frame-registers #:vm-frame-sp #:vm-frame-pc
   #:vm-frame-closure #:vm-frame-return-frame
   #:vm-frame-p
   ;; Frame pool
   #:*frame-pool* #:*frame-pool-top*
   #:initialize-frame-pool
   #:frame-pool-acquire #:frame-pool-release
   ;; Register access
   #:frame-reg-get #:frame-reg-set
   ;; Utilities
   #:frame-reset
   ;; ---------------------------------------------------------------
   ;; GC configuration
   #:*gc-young-size-words* #:*gc-old-size-words* #:*gc-tenuring-threshold*
   #:+gc-card-size-words+
   ;; Object header helpers
    #:make-rt-header
    #:rt-header-type-tag #:rt-header-gc-bits #:rt-header-shape-id #:rt-header-size
    #:rt-header-age #:rt-header-increment-age
   #:header-marked-p #:header-gray-p #:header-forwarding-p
   #:header-set-mark #:header-clear-mark #:header-set-gray #:header-clear-gray
   #:header-make-forwarding-ptr #:header-forwarding-ptr
   ;; rt-heap structure
   #:rt-heap #:make-rt-heap
   #:rt-heap-words #:rt-heap-young-from-base #:rt-heap-young-to-base
   #:rt-heap-young-semi-size #:rt-heap-young-free
   #:rt-heap-old-base #:rt-heap-old-size #:rt-heap-old-free
   #:rt-heap-minor-gc-count #:rt-heap-major-gc-count
   #:rt-heap-words-collected #:rt-heap-words-promoted
   #:rt-heap-card-table #:rt-heap-roots #:rt-heap-satb-queue
   #:rt-heap-free-list #:rt-heap-gc-state
    ;; Heap word access
     #:rt-heap-ref #:rt-heap-set
      ;; Heap ASLR / guard pages (FR-373, FR-376)
      #:rt-heap-randomize-base #:rt-install-stack-guard
      #:*rt-stack-guard-registry*
       ;; Compressed object references (FR-347)
       #:rt-compress-object-ref #:rt-decompress-object-ref
       #:serialize #:deserialize
      ;; Heap growth/shrink policy (FR-391, FR-392)
      #:rt-heap-maybe-grow #:rt-heap-maybe-shrink
    ;; Sanitizer runtime controls (FR-489..493)
     #:*rt-asan-enabled* #:*rt-msan-enabled* #:*rt-tsan-enabled* #:*rt-hwasan-enabled* #:*rt-ubsan-enabled*
    #:*rt-tsan-thread-id*
    #:rt-sanitizer-reset-state
    #:rt-sanitizer-poison-address #:rt-sanitizer-unpoison-address
    #:rt-sanitizer-set-address-tag
    #:rt-heap-object-header #:rt-heap-set-header #:rt-heap-object-size
   ;; Card table
   #:rt-card-index #:rt-card-dirty-p #:rt-card-mark-dirty #:rt-card-clear #:rt-card-clear-all
   ;; Address predicates
   #:rt-young-addr-p #:rt-old-addr-p #:rt-heap-addr-p
   ;; Object tracing
   #:rt-object-pointer-slots
   ;; GC public API
   #:rt-gc-alloc #:rt-gc-add-root #:rt-gc-remove-root
    #:rt-gc-write-barrier #:rt-gc-minor-collect #:rt-gc-major-collect
     #:rt-gc-stats #:gc-stats
    #:rt-gc-configure-concurrent-mode
    #:rt-gc-concurrent-assist
     #:*rt-concurrent-gc-enabled-p*
      #:*rt-concurrent-gc-write-barrier-mode*
      #:*rt-concurrent-gc-stw-phases*
       #:*rt-concurrent-gc-mutator-assist-p*
       #:*concurrent-gc-enabled* #:concurrent-mark-worker
       #:rt-gc-concurrent-sweep #:rt-gc-concurrent-sweep-worker
       ;; Concurrent GC SATB (FR-339)
       #:rt-gc-satb-enqueue #:rt-gc-drain-satb-thread-queues
    ;; New GC features (memory-gc.md)
    ;; Card summary (FR-084)
    #:rt-card-summary-update #:rt-card-summary-clean-block-p
    ;; Heap accessors
    #:rt-heap-num-cards #:rt-heap-card-summary #:rt-heap-barrier-buffer
    #:rt-heap-total-alloc-words #:rt-heap-age-hist
    #:rt-heap-large-obj-threshold #:rt-heap-large-obj-base
    #:rt-heap-large-obj-size #:rt-heap-large-obj-free
    #:rt-heap-gc-pause-total #:rt-heap-gc-pause-max
    #:rt-heap-pressure-hooks #:rt-heap-pressure-threshold-high #:rt-heap-pressure-threshold-low
    #:rt-heap-max-heap-words #:rt-heap-shrink-threshold
    #:rt-heap-compaction-trigger-fraction
    #:rt-heap-gc-inhibit #:rt-heap-gc-pending
     ;; Memory pressure (FR-334)
     #:rt-heap-occupancy-pct #:rt-heap-register-pressure-hook
     ;; NUMA-local GC (FR-364)
     #:*rt-numa-enabled*
      #:rt-numa-node-of-thread #:rt-numa-local-alloc
      #:rt-gc-numa-affinity #:rt-heap-interleave
    ;; Fragmentation (FR-380)
    #:rt-heap-fragmentation-pct #:rt-heap-should-compact-p
    #:*compacting-gc-enabled* #:*gc-compact-after-major-cycles*
    #:rt-gc-compact-old-space #:rt-gc-should-run-compaction-p
    ;; Object pinning for FFI / relocation barriers (FR-733)
    #:rt-pin-object #:rt-unpin-object #:rt-object-pinned-p
    #:with-pinned-objects #:*rt-current-pinning-heap*
    ;; GC inhibit (FR-428)
    #:without-gcing #:rt-gc-inhibit-p
     ;; Dynamic tenure (FR-085)
     #:rt-gc-dynamic-tenure
     ;; TLAB allocation (FR-343)
     #:rt-tlab #:rt-tlab-p #:make-rt-tlab
     #:rt-tlab-base #:rt-tlab-free #:rt-tlab-limit
     #:rt-tlab-thread-id #:rt-tlab-waste-bytes
     #:rt-tlab-retired-p
     #:rt-gc-tlab-alloc #:rt-gc-tlab-retire-all
     ;; SIMD zero-fill (FR-345)
     #:rt-gc-simd-zero-fill
    ;; GC verification (FR-413)
    #:rt-gc-verify-heap #:*gc-verify-after-collect*
     ;; GC stress (FR-414)
     #:*gc-stress-mode*
      ;; Immortal objects (FR-377)
      #:*rt-immortal-registry*
      #:rt-make-immortal #:rt-immortal-p #:rt-immortal-objects-count
     ;; Heap census (FR-446)
    #:rt-gc-heap-census
     ;; Barrier batching (FR-453)
    #:rt-gc-flush-barrier-buffer #:*rt-use-barrier-batching*
    ;; Write barrier optimization (FR-542)
    #:*write-barrier-opt-enabled*
    #:rt-gc-current-cycle
    #:rt-gc-register-stack-allocated-object
    #:rt-gc-stack-allocated-object-p
    #:rt-gc-clear-stack-allocated-objects
    ;; Lifecycle hooks (FR-447)
    #:*rt-gc-alloc-hooks* #:*rt-gc-death-hooks*
    #:rt-gc-register-alloc-hook #:rt-gc-register-death-hook
    ;; GC policy (FR-424)
    #:rt-gc-select-policy
    ;; GC Pacer & Back-Pressure (FR-425, FR-427)
    #:*gc-pacer-enabled* #:*gc-allocation-rate-limit-words-per-sec*
    #:*gc-back-pressure-threshold* #:rt-gc-pacer-maybe-throttle
    ;; Reference strength (FR-381-384)
     #:*rt-reference-registry* #:*rt-default-reference-queue*
     #:rt-soft-ref #:rt-weak-ref #:rt-phantom-ref
     #:rt-make-soft-ref #:rt-make-weak-ref #:rt-make-phantom-ref
     #:rt-make-weak-pointer #:rt-weak-pointer-p #:rt-weak-pointer-value
     #:rt-soft-ref-referent #:rt-weak-ref-referent #:%rt-phantom-ref-referent
     #:rt-ref-get #:rt-ref-clear-p #:rt-reference-queue-process
    ;; Ephemerons (FR-246)
    #:rt-ephemeron #:make-rt-ephemeron #:rt-ephemeron-key #:rt-ephemeron-value
    #:*rt-ephemeron-registry* #:rt-make-ephemeron
    ;; GC reference processing (FR-381-384, FR-246)
    #:rt-gc-process-references
    ;; Finalization (FR-459/460/471)
     #:register-finalizer #:*pending-finalizers* #:rt-run-pending-finalizers
     #:header-finalized-p #:header-set-finalized #:header-clear-finalized
     #:*rt-finalizer-registry* #:*rt-finalization-queue*
     #:*large-object-threshold* #:rt-los-alloc #:rt-large-object-size-p
     #:*use-huge-pages* #:rt-huge-page-mmap #:huge-pages-enabled-p #:try-enable-huge-pages
     #:*xom-enabled* #:rt-allocate-xom-code-memory #:rt-finalize-xom-code-memory
     #:rt-release-xom-code-memory #:rt-allocate-code-memory-xom-aware
    ;; Precise roots (FR-332)
     #:rt-gc-add-root-typed #:*gc-threads*
      #:rt-gc-enter-safe-region #:rt-gc-leave-safe-region #:*gc-inhibit-during-signals*
       #:*gc-pending* #:rt-gc-request #:rt-gc-safepoint-check
       #:rt-emit-gc-safepoint #:rt-compress-stackmap-slots
       #:rt-decompress-stackmap-slots #:rt-gc-map-section-documentation
       #:rt-gc-register-thread #:rt-gc-unregister-thread
      #:rt-gc-register-stackmap #:rt-gc-generate-stackmap-stub #:rt-gc-scan-stackmaps
      #:rt-stackmap #:make-rt-stackmap #:rt-stackmap-slots
      #:rt-poison-return-address #:rt-unpoison-return-address
      #:rt-check-stack-overflow #:rt-with-call-stack-guard
      #:rt-oom-condition #:rt-gc-pressure-warning
    ;; Profiling (FR-366)
    #:rt-gc-profile-sample #:rt-gc-profile-report #:*gc-profile-enabled*
    #:*gc-profile-interval*
    ;; Allocation tracing probes (FR-367)
    #:*gc-probes-enabled* #:rt-gc-probe-alloc
    #:rt-gc-probe-gc-start #:rt-gc-probe-gc-end
     ;; Parallel/concurrent GC verification (FR-338, FR-339)
     #:*gc-worker-count*
     #:rt-gc-detect-worker-count
     #:rt-gc-parallel-root-scan #:rt-gc-parallel-mark #:rt-gc-parallel-sweep
     #:%rt-gc-work-stealing-drain #:rt-gc-verify-tri-color-invariant
    ;; Heap snapshot (FR-368)
     #:rt-gc-heap-snapshot
     ;; Runtime image snapshot/restore (FR-350, FR-563)
     #:rt-image-register-global #:rt-save-image #:rt-load-image
     #:rt-save-core #:rt-load-core #:*saved-core-pathname*
     #:rt-capture-image-state #:rt-restore-image-state
     ;; DOT graph (FR-415)
     #:rt-gc-heap-dump-dot
  ;; Heap walk (FR-445)
  #:rt-gc-map-heap-objects
   ;; ── Sync primitives (sync.lisp) ──────────────────────────────────
   #:rt-make-mutex #:rt-mutex-lock #:rt-mutex-unlock
   #:rt-make-condition-variable
   #:rt-make-semaphore #:rt-semaphore-wait #:rt-semaphore-signal
   #:rt-make-barrier #:rt-barrier-wait
   #:rt-make-once #:rt-once-call
   ;; ── Green-thread scheduler (scheduler.lisp) ───────────────────────
    #:rt-make-scheduler #:rt-scheduler-init #:rt-scheduler-run
    #:rt-spawn #:rt-yield #:rt-current-thread-id #:rt-sleep-task
    #:rt-green-thread #:rt-green-thread-p #:rt-green-thread-status
    #:rt-green-thread-result #:rt-green-thread-error
    #:rt-work-deque #:rt-work-deque-p #:rt-work-deque-push-front
    #:rt-work-deque-pop-front #:rt-work-deque-steal-back #:rt-work-deque-count
    #:rt-worker #:rt-worker-p #:rt-worker-run-once
    #:rt-work-stealing-scheduler #:rt-work-stealing-scheduler-p
    #:rt-make-work-stealing-scheduler #:rt-work-stealing-init
    #:rt-work-stealing-submit #:rt-work-stealing-run #:rt-work-stealing-maybe-grow
    #:rt-scheduler-steal #:*rt-work-stealing-scheduler*
    ;; ── Futures / promises (future.lisp) ─────────────────────────────
     #:rt-make-future #:rt-future-resolve #:rt-future-await #:rt-future-done-p
     #:rt-future-then
     ;; ── Async / async generators (async.lisp, async-generators.lisp) ──
     #:rt-async #:rt-await #:rt-await* #:rt-async-submit
     #:rt-async-lambda #:rt-async-defun
     #:rt-async-cps-transform #:rt-async-cps
     #:rt-async-channel #:rt-async-send #:rt-async-recv
    #:rt-aiter #:rt-aiter-p #:make-rt-aiter
    #:rt-aiter-next-fn #:rt-aiter-next
    #:rt-aiter-from-list #:rt-aiter-map #:rt-aiter-filter #:rt-aiter-take #:rt-aiter-collect
    #:rt-async-generator-state #:rt-async-generator-state-p #:make-rt-async-generator-state
    #:rt-async-generator-state-items #:rt-async-generator-state-done-p
    #:rt-async-generator-state-error #:rt-async-generator-state-waiters
    #:rt-make-async-generator #:rt-async-yield #:rt-async-generator-next
    #:rt-async-generator-close #:rt-async-generator-fail #:rt-async-for
    ;; ── Algebraic effects (effects.lisp) ─────────────────────────────
    #:rt-effect #:rt-effect-p #:make-rt-effect #:rt-effect-name #:rt-effect-payload
    #:rt-perform #:rt-handle #:rt-handler-state #:rt-handler-state-p
    #:make-rt-handler-state #:rt-handler-state-handlers #:rt-with-handler
    #:rt-resume #:rt-current-handler #:rt-effect-state #:rt-effect-error
    #:rt-effect-read #:rt-effect-write #:*rt-handler-stack*
    ;; ── CSP channels (channel.lisp) ──────────────────────────────────
   #:rt-make-channel #:rt-channel-send #:rt-channel-recv #:rt-channel-close
   ;; ── Actor model (actor.lisp) ─────────────────────────────────────
   #:rt-make-actor #:rt-actor-send #:rt-actor-receive
   ;; ── STM (stm.lisp) ───────────────────────────────────────────────
    #:rt-tvar #:rt-tvar-p #:rt-make-tvar #:rt-read-tvar #:rt-write-tvar
    #:rt-atomically #:atomic #:rt-retry #:rt-tvar-value-unsafe #:rt-tvar-version-unsafe
    #:rt-stm-transaction #:rt-stm-conflict #:rt-stm-retry #:opt-pass-stm
    ;; ── Fibers / green threads (fiber.lisp) ───────────────────────────
    #:rt-fiber #:rt-fiber-p #:rt-make-fiber #:rt-fiber-spawn #:rt-fiber-schedule
    #:rt-fiber-resume #:rt-fiber-yield #:rt-fiber-block #:rt-fiber-await
    #:rt-fiber-done-p #:rt-fiber-status-failed-p #:rt-run-fibers
    #:rt-fiber-local #:rt-fiber-async #:rt-green-thread-spawn #:rt-run-green-threads
   ;; ── Lock-free data structures (lockfree.lisp) ────────────────────
   #:rt-make-lfstack #:rt-lfstack-push #:rt-lfstack-pop
   #:rt-make-lfqueue #:rt-make-lfhash-map
   ;; ── Epoch-based reclamation (ebr.lisp) ───────────────────────────
   #:rt-ebr-init #:rt-ebr-register-thread #:rt-ebr-enter #:rt-ebr-leave
   ;; ── Hazard pointers (hazard.lisp) ────────────────────────────────
   #:rt-hp-register-thread #:rt-hp-protect #:rt-hp-retire
   ;; ── RCU (rcu.lisp) ───────────────────────────────────────────────
   #:rt-rcu-read-lock #:rt-rcu-synchronize
   ;; ── QSBR (qsbr.lisp) ─────────────────────────────────────────────
   #:rt-qsbr-init #:rt-qsbr-register-thread #:rt-qsbr-synchronize
    ;; ── OS abstraction (os.lisp) ─────────────────────────────────────
    #:rt-open #:rt-close #:rt-getenv #:rt-argv #:rt-exit
    #:rt-run-program #:rt-process #:rt-process-p #:rt-process-pid
    #:rt-process-status #:rt-process-exit-code #:rt-process-input
    #:rt-process-output #:rt-process-error #:rt-process-wait
    #:rt-process-kill #:rt-process-alive-p #:rt-shell
    #:rt-fork #:rt-exec #:rt-waitpid
    ;; ── Native threads (scheduler.lisp) ───────────────────────────────
    #:rt-native-thread #:rt-native-thread-p #:rt-make-thread
    #:rt-thread-join #:rt-thread-name #:rt-current-thread
    #:rt-thread-alive-p #:rt-all-threads #:rt-interrupt-thread
    #:rt-destroy-thread #:rt-thread-yield #:*rt-native-thread-registry*
    ;; ── Signals (signals.lisp) ───────────────────────────────────────
    #:+rt-sighup+ #:+rt-sigint+ #:+rt-sigterm+ #:+rt-sigusr1+
    #:+rt-sigusr2+ #:+rt-sigwinch+ #:rt-set-signal-handler
    #:rt-get-signal-handler #:rt-signal-mask #:rt-unblock-signal
    #:rt-with-signal-handler #:rt-process-pending-signals
    #:*pending-signals* #:*shutdown-hooks* #:keyboard-interrupt
     ;; ── Network (net.lisp) ───────────────────────────────────────────
    #:+rt-af-inet+ #:+rt-af-inet6+ #:+rt-sock-stream+ #:+rt-sock-dgram+
    #:rt-socket-address #:make-rt-socket-address
    #:rt-socket-address-host #:rt-socket-address-port #:rt-socket-address-family
    #:rt-socket-entry #:rt-socket-entry-p
    #:rt-socket #:rt-bind #:rt-listen #:rt-connect #:rt-accept
    #:rt-socket-send #:rt-socket-recv #:rt-close-socket
    #:rt-make-socket-stream #:rt-socket-name #:rt-resolve-address
    ;; ── mmap files (mmap.lisp) ───────────────────────────────────────
    #:rt-mmap-region #:rt-mmap-region-p #:rt-mmap-region-length
    #:rt-mmap-region-address #:rt-mmap-region-released-p
    #:mmap-file #:mmap-array #:mmap-close #:with-mmap #:mmap-sync #:mmap-advice
   ;; ── GC safe-region depth table (gc-data.lisp) ────────────────────
   #:*rt-gc-safe-region-depths*
   ;; ── Context propagation (context.lisp) ───────────────────────────
   #:rt-context-cancel #:rt-context-cancelled-p
   ;; ── SPSC ring buffer (spsc.lisp) ─────────────────────────────────
   #:rt-make-spsc-queue #:rt-spsc-try-push #:rt-spsc-try-pop
    ;; ── Performance counters (perf.lisp) ─────────────────────────────
    #:rt-perf-init #:rt-perf-enable-counter #:rt-with-perf-counters
    #:rdtsc #:rdtscp #:perf-counters-unsupported
    ;; ── Metrics (metrics.lisp) ────────────────────────────────────────
    #:rt-make-counter #:rt-counter-increment! #:rt-make-histogram
    #:rt-histogram-observe! #:rt-make-gauge #:rt-gauge-set!
    #:rt-register-metric #:rt-metrics-format-prometheus
    #:prometheus-text-format
     ;; ── OpenTelemetry (otel.lisp) ────────────────────────────────────
     #:rt-otel-start-span #:rt-otel-end-span
     ;; ── Continuous profiling (continuous-profile.lisp) ───────────────
      #:rt-continuous-profile-session #:make-rt-continuous-profile-session
      #:rt-profile-frame #:make-rt-profile-frame
      #:rt-profile-frame-function #:rt-profile-frame-source-file
      #:rt-profile-frame-source-line #:rt-profile-frame-address
      #:rt-profile-frame-perf-symbol
      #:rt-profile-sample #:make-rt-profile-sample
      #:rt-profile-sample-timestamp-nanos #:rt-profile-sample-thread-id
      #:rt-profile-sample-trace-id #:rt-profile-sample-span-id
      #:rt-profile-sample-stack #:rt-profile-sample-count
      #:rt-continuous-profile-session-name
      #:rt-continuous-profile-session-samples
      #:rt-continuous-profile-session-sample-log
      #:rt-continuous-profile-session-sample-rate-hz
      #:rt-continuous-profile-session-output
      #:rt-continuous-profile-session-endpoint
      #:rt-continuous-profile-session-trace-id
      #:rt-continuous-profile-session-span-id
      #:rt-continuous-profile-session-running-p
      #:*rt-continuous-profile-session*
      #:rt-start-continuous-profile #:rt-stop-continuous-profile
      #:rt-record-profile-sample #:rt-continuous-profile->otel-span
      #:rt-continuous-profile-to-otel-json
      #:rt-continuous-profile-to-pprof-json
      #:rt-export-continuous-profile
    ;; ── Deadlock detector (deadlock.lisp) ────────────────────────────
    #:*rt-dl-enabled* #:*rt-dl-thread-locks* #:*rt-dl-thread-waits*
    #:rt-deadlock-before-lock #:rt-deadlock-after-lock
    #:rt-deadlock-after-unlock #:rt-deadlock-detect #:rt-deadlock-init
    ;; ── Consensus / Raft (consensus.lisp) ────────────────────────────
    #:rt-make-raft-node #:rt-make-raft-cluster #:rt-raft-propose
   #:rt-raft-cluster-nodes
   ;; ── CRDTs (crdt.lisp) ────────────────────────────────────────────
   #:rt-make-gcounter #:rt-gcounter-increment #:rt-gcounter-value
   #:rt-make-pncounter #:rt-make-lwwregister
     ;; ── Parallel algorithms (parallel-algo.lisp) ─────────────────────
     #:rt-parallel-algo-init
     ;; ── CPU / NUMA topology (topology.lisp) ───────────────────────────
     #:detect-cpu-cores #:detect-numa-topology
     #:get-cpu-affinity-mask #:set-cpu-affinity-mask
     #:memory-tier-info
     #:rt-cpu-topology
     #:rt-thread-set-affinity #:rt-thread-get-affinity
     ;; ── GPU compute (gpu.lisp) ────────────────────────────────────────
     #:rt-gpu-buffer #:rt-gpu-kernel
    #:rt-gpu-buffer-alloc #:rt-gpu-buffer-copy #:rt-gpu-buffer-free
    #:rt-gpu-kernel-compile #:rt-gpu-launch-kernel #:rt-gpu-launch-async
    ;; ── Reactive Streams (reactive.lisp) ──────────────────────────────
    #:rt-subscription #:rt-subscriber
    #:rt-subscribe #:rt-on-subscribe #:rt-on-next #:rt-on-error #:rt-on-complete
    #:rt-request #:rt-cancel
    #:rt-publisher-from-list #:rt-publisher-map #:rt-publisher-filter
    #:rt-publisher-merge #:rt-publisher-zip
    #:rt-make-subscriber #:rt-subscriber-collect
    ;; ── Async Generators (async-generators.lisp) ─────────────────────
    #:rt-aiter #:rt-aiter-next #:rt-aiter-from-list
    #:rt-async-generator-state #:rt-make-async-generator
    #:rt-async-yield #:rt-async-generator-close #:rt-async-generator-fail
    #:rt-async-generator-next
    #:rt-aiter-map #:rt-aiter-filter #:rt-aiter-take #:rt-aiter-collect
    ;; ── Effect system (effects.lisp) ───────────────────────────────────
    #:rt-effect #:rt-handler-state
    #:rt-current-handler #:rt-resume #:rt-perform #:rt-handle
    #:rt-effect-state #:rt-effect-error #:rt-effect-read #:rt-effect-write
    ;; ── Structured logging (log.lisp) ───────────────────────────────────
    #:+log-level-trace+ #:+log-level-debug+ #:+log-level-info+
    #:+log-level-warn+ #:+log-level-error+
    #:*log-level* #:*log-output* #:*log-json-output* #:*log-context*
    #:with-log-context
    #:log-trace #:log-debug #:log-info #:log-warn #:log-error
    ;; ── Arena allocator (allocator.lisp) ────────────────────────────────
    #:rt-arena #:make-arena #:with-arena
    #:arena-alloc #:arena-reset
    #:rt-arena-cursor
    #:rt-arena-block #:rt-arena-block-offset
    ;; ── Object pool (allocator.lisp) ─────────────────────────────────────
    #:make-object-pool #:pool-acquire #:pool-release
    ;; ── GC nursery config (heap-data.lisp) ───────────────────────────────
    #:*gc-nursery-size*
    ;; ── Pinned arrays for FFI (FR-417) ──────────────────────────────────
    #:rt-pin-unboxed-array #:rt-pinned-unboxed-array-buffer-p
    #:rt-pinned-unboxed-array-buffer-array
    #:rt-pinned-unboxed-array-buffer-length
    #:rt-pinned-array-data-pointer #:rt-release-pinned-array
    #:rt-pinned-unboxed-array-buffer-released-p
    ;; ── Self-host portability facades (portable.lisp) ──
    #:rt-unsupported-operation #:rt-unsupported-operation-name
    #:rt-unsupported
    #:rt-current-thread-token
    #:rt-make-lock #:rt-with-lock #:rt-lock #:rt-unlock #:rt-try-lock
    #:rt-thread-yield
    #:rt-atomic-compare-and-swap-symbol
    #:rt-getenv #:rt-with-timeout))

(in-package :cl-cc/runtime)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; These specials are defined later in the ASDF source order, but multiple
  ;; earlier runtime files refer to them during compilation.
  (declaim (special *gc-major-threshold*
                    *gc-max-pause-ms*
                    *gc-worker-count*
                    *rt-current-context*
                    *rt-current-gc-heap*
                    *rt-weak-hash-table-registry*)))

(defun make-hash-table (&rest args)
  "Bootstrap wrapper for CL:MAKE-HASH-TABLE; hash-weak.lisp adds :WEAKNESS."
  (apply #'cl:make-hash-table args))
