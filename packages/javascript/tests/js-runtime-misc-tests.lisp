;;;; packages/javascript/tests/js-runtime-misc-tests.lisp
;;;;
;;;; Unit tests for remaining uncovered runtime functions:
;;;; Promise.race/allSettled/finally/withResolvers/try,
;;;; global isNaN/isFinite, structuredClone, timer stubs,
;;;; Iterator.from, Map.groupBy, Set-from-iterable, Proxy,
;;;; Math.sumPrecise, Error.isError, AggregateError, AbortController,
;;;; URL stub, TypedArray constructor factory, Object.defineProperties.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr, %jr-list)

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Promise.race ────────────────────────────────────────────────────────────

(deftest js-rt-promise-race-returns-first
  "Promise.race returns the first element of the promise array unchanged."
  (let* ((p1  (cl-cc/javascript::%js-promise-resolve 1))
         (p2  (cl-cc/javascript::%js-promise-resolve 2))
         (arr (%jr-arr p1 p2))
         (winner (cl-cc/javascript::%js-promise-race arr)))
    (assert-eq p1 winner)))

(deftest js-rt-promise-race-empty-pending
  "Promise.race with empty array returns an unsettled promise."
  (let ((result (cl-cc/javascript::%js-promise-race (%jr-arr))))
    (assert-true (cl-cc/javascript::js-promise-p result))
    (assert-false (cl-cc/javascript::js-promise-settled-p result))))

;;; ─── Promise.allSettled ──────────────────────────────────────────────────────

(deftest js-rt-promise-all-settled-mixed
  "Promise.allSettled returns status objects for both fulfilled and rejected."
  (let* ((p1  (cl-cc/javascript::%js-promise-resolve "ok"))
         (p2  (cl-cc/javascript::%js-promise-reject  "err"))
         (arr (%jr-arr p1 p2))
         (settled (cl-cc/javascript::%js-await
                   (cl-cc/javascript::%js-promise-all-settled arr)))
         (r1  (aref settled 0))
         (r2  (aref settled 1)))
    (assert-string= "fulfilled" (gethash "status" r1))
    (assert-string= "ok"        (gethash "value"  r1))
    (assert-string= "rejected"  (gethash "status" r2))
    (assert-string= "err"       (gethash "reason" r2))))

;;; ─── Promise.finally ─────────────────────────────────────────────────────────

(deftest js-rt-promise-finally-calls-cleanup
  "Promise.finally invokes on-finally and returns the original promise."
  (let* ((called (list nil))
         (p      (cl-cc/javascript::%js-promise-resolve 42))
         (result (cl-cc/javascript::%js-promise-finally
                  p (lambda () (setf (car called) t)))))
    (assert-true (car called))
    (assert-eq p result)))

;;; ─── Promise.withResolvers ───────────────────────────────────────────────────

(deftest js-rt-promise-with-resolvers
  "Promise.withResolvers returns an object with promise, resolve, reject."
  (let* ((obj     (cl-cc/javascript::%js-promise-with-resolvers))
         (promise  (gethash "promise" obj))
         (resolve  (gethash "resolve" obj)))
    (assert-true (cl-cc/javascript::js-promise-p promise))
    (assert-true (functionp resolve))
    (funcall resolve 99)
    (assert-= 99 (cl-cc/javascript::%js-await promise))))

;;; ─── Promise.try ─────────────────────────────────────────────────────────────

(deftest js-rt-promise-try-success
  "Promise.try wraps a synchronous result in a fulfilled promise."
  (let* ((result (cl-cc/javascript::%js-promise-try (lambda () 7))))
    (assert-= 7 (cl-cc/javascript::%js-await result))))

;;; ─── Global isNaN / isFinite (with coercion) ─────────────────────────────────

(deftest-each js-rt-global-is-nan
  "Global isNaN coerces its argument before checking (unlike Number.isNaN)."
  :cases (("number-nan"  cl-cc/javascript::*js-nan-float*  t)
          ("string-nan"  "NaN"                              t)
          ("string-num"  "42"                               nil)
          ("integer"     5                                  nil))
  (val expected)
  (assert-equal expected (cl-cc/javascript::%js-is-nan val)))

(deftest-each js-rt-global-is-finite
  "Global isFinite coerces its argument (unlike Number.isFinite)."
  :cases (("integer"   42      t)
          ("string-n"  "3.14"  t)
          ("nan"       cl-cc/javascript::*js-nan-float*  nil)
          ("inf"       cl-cc/javascript::*js-inf-float*  nil))
  (val expected)
  (assert-equal expected (cl-cc/javascript::%js-is-finite val)))

;;; ─── structuredClone ─────────────────────────────────────────────────────────

(deftest js-rt-structured-clone
  "%js-structured-clone deep-clones an object (delegates to %js-deep-clone)."
  (let* ((orig  (cl-cc/javascript::%js-make-object "x" 10))
         (clone (cl-cc/javascript::%js-structured-clone orig)))
    (assert-false (eq orig clone))
    (assert-= 10 (gethash "x" clone))))

;;; ─── Timer stubs ─────────────────────────────────────────────────────────────

(deftest js-rt-set-timeout-runs-fn
  "%js-set-timeout runs its callback synchronously and returns +js-undefined+."
  (let ((called (list nil)))
    (let ((ret (cl-cc/javascript::%js-set-timeout (lambda () (setf (car called) t)))))
      (assert-true (car called))
      (assert-eq cl-cc/javascript::+js-undefined+ ret))))

(deftest-each js-rt-timer-no-op-stubs
  "%js-set-interval and %js-clear-timer are no-ops returning +js-undefined+."
  :cases (("set-interval" (cl-cc/javascript::%js-set-interval))
          ("clear-timer"  (cl-cc/javascript::%js-clear-timer 0))
          ("queue-micro"  (cl-cc/javascript::%js-queue-microtask (lambda ()))))
  (result)
  (assert-eq cl-cc/javascript::+js-undefined+ result))

;;; ─── Iterator.from ───────────────────────────────────────────────────────────

(deftest js-rt-iterator-from-array
  "%js-iterator-from-iterable wraps an array in a generator iterator."
  (let* ((arr  (%jr-arr 1 2 3))
         (iter (cl-cc/javascript::%js-iterator-from-iterable arr))
         (acc  nil))
    (cl-cc/javascript::%js-for-of iter (lambda (v) (push v acc)))
    (assert-equal '(3 2 1) acc)))

;;; ─── Map.groupBy ─────────────────────────────────────────────────────────────

(deftest js-rt-map-group-by
  "%js-map-group-by groups elements into a Map by key-fn result."
  (let* ((items  (%jr-arr 1 2 3 4))
         (result (cl-cc/javascript::%js-map-group-by
                  items (lambda (x) (if (evenp x) "even" "odd"))))
         (evens  (cl-cc/javascript::%js-map-get result "even"))
         (odds   (cl-cc/javascript::%js-map-get result "odd")))
    (assert-= 2 (length evens))
    (assert-= 2 (length odds))))

;;; ─── Set-from-iterable ───────────────────────────────────────────────────────

(deftest js-rt-make-set-from-iterable
  "%js-make-set-from-iterable seeds a new Set from an array."
  (let* ((arr (cl-cc/javascript::%js-make-array 1 2 3 2))
         (s   (cl-cc/javascript::%js-make-set-from-iterable arr)))
    (assert-true  (cl-cc/javascript::%js-set-has s 1))
    (assert-true  (cl-cc/javascript::%js-set-has s 3))
    (assert-false (cl-cc/javascript::%js-set-has s 9))))

;;; ─── Proxy stub ──────────────────────────────────────────────────────────────

(deftest js-rt-make-proxy-object
  "%js-make-proxy-object stores target and handler in __proxy-*__ slots."
  (let* ((target  (cl-cc/javascript::%js-make-object "x" 1))
         (handler (cl-cc/javascript::%js-make-object))
         (proxy   (cl-cc/javascript::%js-make-proxy-object target handler)))
    (assert-eq target  (gethash "__proxy-target__"  proxy))
    (assert-eq handler (gethash "__proxy-handler__" proxy))))

;;; ─── Math.sumPrecise (ES2026) ────────────────────────────────────────────────

(deftest js-rt-math-sum-precise
  "%js-math-sum-precise sums a numeric iterable to double-float."
  (let ((result (cl-cc/javascript::%js-math-sum-precise (%jr-arr 1 2 3 4))))
    (assert-= 10.0d0 result)))

;;; ─── Error.isError (ES2026) ──────────────────────────────────────────────────

(deftest js-rt-error-is-error
  "%js-error-is-error returns truthy for Error-like objects, nil for non-errors."
  (let ((with-message (cl-cc/javascript::%js-make-object "message" "oops"))
        (plain-obj    (cl-cc/javascript::%js-make-object "x" 1)))
    (assert-true  (cl-cc/javascript::%js-error-is-error with-message))
    (assert-false (cl-cc/javascript::%js-error-is-error plain-obj))
    (assert-false (cl-cc/javascript::%js-error-is-error "error"))))

;;; ─── AggregateError ──────────────────────────────────────────────────────────

(deftest js-rt-make-aggregate-error
  "%js-make-aggregate-error returns an object with name/message/errors fields."
  (let* ((errors (%jr-arr "e1" "e2"))
         (agg    (cl-cc/javascript::%js-make-aggregate-error errors "multiple")))
    (assert-string= "AggregateError" (gethash "name"    agg))
    (assert-string= "multiple"       (gethash "message" agg))
    (assert-eq errors                (gethash "errors"  agg))))

;;; ─── AbortController ─────────────────────────────────────────────────────────

(deftest js-rt-abort-controller
  "%js-make-abort-controller produces a signal/abort pair."
  (let* ((ctrl   (cl-cc/javascript::%js-make-abort-controller))
         (sig    (gethash "signal" ctrl))
         (abort  (gethash "abort"  ctrl)))
    (assert-false (gethash "aborted" sig))
    (funcall abort "reason")
    (assert-true  (gethash "aborted" sig))
    (assert-string= "reason" (gethash "reason" sig))))

;;; ─── URL stub ────────────────────────────────────────────────────────────────

(deftest js-rt-make-url-stub
  "%js-make-url returns an object with href, origin, pathname etc."
  (let ((url (cl-cc/javascript::%js-make-url "https://example.com/path")))
    (assert-string= "https://example.com/path" (gethash "href" url))
    (assert-string= "" (gethash "pathname" url))))

;;; ─── TypedArray constructor factory ─────────────────────────────────────────

(deftest js-rt-typed-array-ctor-factory
  "%js-make-typed-array-ctor returns a lambda that creates a TypedArray."
  (let* ((ctor (cl-cc/javascript::%js-make-typed-array-ctor "Int32Array"))
         (ta   (funcall ctor 3)))
    (assert-true (cl-cc/javascript::js-typed-array-p ta))
    (assert-= 3 (cl-cc/javascript::js-ta-length ta))))

;;; ─── Object.defineProperties ─────────────────────────────────────────────────

(deftest js-rt-object-define-properties
  "%js-object-define-properties sets multiple keys from a descriptor map."
  (let* ((obj   (cl-cc/javascript::%js-make-object))
         (descs (cl-cc/javascript::%js-make-object
                 "a" (cl-cc/javascript::%js-make-object "value" 1)
                 "b" (cl-cc/javascript::%js-make-object "value" 2))))
    (cl-cc/javascript::%js-object-define-properties obj descs)
    (assert-= 1 (gethash "a" obj))
    (assert-= 2 (gethash "b" obj))))

;;; ─── Reflect.defineProperty / Object.defineProperty ─────────────────────────

(deftest js-rt-reflect-define-property
  "%js-reflect-define-property writes a value descriptor and returns t."
  (let* ((obj  (cl-cc/javascript::%js-make-object))
         (desc (cl-cc/javascript::%js-make-object "value" 42))
         (ret  (cl-cc/javascript::%js-reflect-define-property obj "x" desc)))
    (assert-true ret)
    (assert-= 42 (gethash "x" obj))))

(deftest js-rt-object-define-property-value
  "%js-object-define-property with a value descriptor stores the value."
  (let* ((obj  (cl-cc/javascript::%js-make-object))
         (desc (cl-cc/javascript::%js-make-object "value" 99)))
    (let ((ret (cl-cc/javascript::%js-object-define-property obj "n" desc)))
      (assert-eq obj ret)
      (assert-= 99 (gethash "n" obj)))))

(deftest js-rt-object-define-property-getter
  "%js-object-define-property with a get descriptor stores under __get_KEY slot."
  (let* ((obj  (cl-cc/javascript::%js-make-object))
         (getter (lambda () 7))
         (desc (cl-cc/javascript::%js-make-object "get" getter)))
    (cl-cc/javascript::%js-object-define-property obj "prop" desc)
    (assert-eq getter (gethash "__get_prop" obj))))

;;; ─── URLSearchParams stub ────────────────────────────────────────────────────

(deftest js-rt-url-search-params-stub
  "%js-make-url-search-params returns a stub with get/has/set/toString."
  (let* ((params (cl-cc/javascript::%js-make-url-search-params "a=1")))
    (assert-eq cl-cc/javascript::+js-null+
               (funcall (gethash "get" params) "a"))
    (assert-string= "" (funcall (gethash "toString" params)))))

;;; ─── crypto stub ─────────────────────────────────────────────────────────────

(deftest js-rt-crypto-random-uuid
  "%js-make-crypto randomUUID returns a string matching UUID format."
  (let* ((crypto (cl-cc/javascript::%js-make-crypto))
         (uuid   (funcall (gethash "randomUUID" crypto))))
    (assert-true (stringp uuid))
    (assert-= 36 (length uuid))
    (assert-equal #\- (char uuid 8))
    (assert-equal #\- (char uuid 13))))

(deftest js-rt-crypto-get-random-values
  "%js-make-crypto getRandomValues returns the same array it was given."
  (let* ((crypto (cl-cc/javascript::%js-make-crypto))
         (arr    (make-array 4 :initial-element 0))
         (ret    (funcall (gethash "getRandomValues" crypto) arr)))
    (assert-eq arr ret)
    (assert-= 4 (length ret))))
