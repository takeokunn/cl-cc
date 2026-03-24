;;;; src/runtime/package.lisp - CL-CC Runtime Library Package

(defpackage :cl-cc/runtime
  (:use :cl)
  (:export
   ;; Tagged pointer constants
   #:+tag-fixnum+ #:+tag-cons+ #:+tag-symbol+ #:+tag-function+
   #:+tag-character+ #:+tag-array+ #:+tag-string+ #:+tag-other+
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
   ;; Multiple values
   #:rt-values-clear #:rt-values-push #:rt-values-count
   #:rt-values-ref #:rt-values-to-list #:rt-spread-values #:rt-ensure-values
   ;; Global bindings
   #:rt-get-global #:rt-set-global
   ;; Strings
   #:rt-make-string #:rt-string-length #:rt-string-ref #:rt-string-set
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
   #:rt-keywordp #:rt-parse-integer
   ;; Hash tables
   #:rt-make-hash-table #:rt-gethash #:rt-sethash #:rt-remhash #:rt-clrhash
   #:rt-hash-count #:rt-hash-test #:rt-maphash #:rt-hash-keys #:rt-hash-values
   #:rt-hash-table-p
   ;; CLOS
   #:rt-defclass #:rt-make-instance #:rt-slot-value #:rt-slot-set
   #:rt-slot-boundp #:rt-slot-makunbound #:rt-slot-exists-p
   #:rt-class-of #:rt-find-class #:rt-register-method #:rt-call-generic
   #:rt-defclass-from-reg #:rt-make-instance-0
   ;; Conditions
   #:rt-signal-error #:rt-signal #:rt-warn-fn #:rt-cerror
   #:rt-establish-handler #:rt-remove-handler
   #:rt-push-handler #:rt-pop-handler #:rt-bind-restart #:rt-invoke-restart
   ;; Misc
   #:rt-boundp #:rt-fboundp #:rt-makunbound #:rt-fmakunbound
   #:rt-random #:rt-make-random-state
   #:rt-get-universal-time #:rt-get-internal-real-time #:rt-get-internal-run-time
   #:rt-eval #:rt-read-from-string #:rt-read-sexp
   #:rt-coerce
   ;; I/O
   #:rt-print #:rt-princ #:rt-prin1 #:rt-terpri #:rt-fresh-line
   #:rt-write-char #:rt-write-string #:rt-write-line #:rt-write-byte
   #:rt-format #:rt-read-char #:rt-read-line #:rt-read-byte
   #:rt-peek-char #:rt-unread-char
   #:rt-open-file #:rt-close-file
   #:rt-make-string-stream #:rt-get-string-from-stream
   #:rt-make-string-output-stream #:rt-get-output-stream-string #:rt-stream-write-string
   #:rt-finish-output #:rt-force-output #:rt-clear-output
   #:rt-input-stream-p #:rt-output-stream-p #:rt-open-stream-p #:rt-interactive-stream-p
   #:rt-stream-element-type
   #:rt-make-broadcast-stream #:rt-make-two-way-stream
   #:rt-make-echo-stream #:rt-make-concatenated-stream
   #:rt-probe-file #:rt-truename #:rt-rename-file #:rt-delete-file #:rt-directory
   #:rt-make-pathname #:rt-namestring #:rt-pathname-component
   #:rt-merge-pathnames #:rt-enough-namestring
   #:rt-write-to-string
   ;; GC configuration
   #:*gc-young-size-words* #:*gc-old-size-words* #:*gc-tenuring-threshold*
   #:+gc-card-size-words+
   ;; Object header helpers
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
   #:rt-gc-stats))
