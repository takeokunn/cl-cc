;;;; packages/runtime/src/package.lisp - CL-CC Runtime Library Package

(defpackage :cl-cc/runtime
  (:use :cl)
  (:shadow #:compute-applicable-methods)
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
    #:rt-find-package #:rt-make-package #:rt-export
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
    #:make-header #:header-size #:header-tag #:header-age
   #:header-marked-p #:header-gray-p #:header-forwarding-p
   #:header-set-mark #:header-clear-mark #:header-set-gray #:header-clear-gray
   #:header-make-forwarding-ptr #:header-forwarding-ptr #:header-increment-age
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
    #:rt-gc-stats
    #:rt-gc-configure-concurrent-mode
    #:rt-gc-concurrent-assist
     #:*rt-concurrent-gc-enabled-p*
     #:*rt-concurrent-gc-write-barrier-mode*
     #:*rt-concurrent-gc-stw-phases*
      #:*rt-concurrent-gc-mutator-assist-p*
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
    ;; Lifecycle hooks (FR-447)
    #:*rt-gc-alloc-hooks* #:*rt-gc-death-hooks*
    #:rt-gc-register-alloc-hook #:rt-gc-register-death-hook
    ;; GC policy (FR-424)
    #:rt-gc-select-policy
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
     #:rt-register-finalizer #:rt-unregister-finalizer #:rt-register-stream-finalizer
     #:rt-finalize #:rt-cancel-finalization
     #:header-finalized-p #:header-set-finalized #:header-clear-finalized
     #:*rt-finalizer-registry* #:*rt-finalization-queue*
    ;; Precise roots (FR-332)
     #:rt-gc-add-root-typed #:*gc-threads*
     #:rt-gc-enter-safe-region #:rt-gc-leave-safe-region #:*gc-inhibit-during-signals*
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
    ;; DOT graph (FR-415)
    #:rt-gc-heap-dump-dot))
