;;;; packages/javascript/src/runtime-builtins.lisp — JS built-in dispatch table
;;;;
;;;; *js-builtin-map* maps built-in name strings to CL functions.
;;;; This file must be loaded LAST so all referenced functions are defined.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Reflect / Object property-descriptor helpers (for runtime-builtins-table.lisp)
;;; -----------------------------------------------------------------------

(defun %js-reflect-get (target key &optional _receiver)
  (declare (ignore _receiver))
  (%js-get-prop target key))

(defun %js-reflect-set (target key value &optional _receiver)
  (declare (ignore _receiver))
  (%js-set-prop target key value)
  t)

(defun %js-reflect-has (target key)
  (when (%js-ht-p target)
    (nth-value 1 (gethash (%js-to-string key) target))))

(defun %js-reflect-delete-property (target key)
  (when (%js-ht-p target)
    (remhash (%js-to-string key) target))
  t)

(defun %js-reflect-apply (fn this-arg args)
  (apply #'%js-funcall fn this-arg (coerce args 'list)))

(defun %js-reflect-construct (target args &optional new-target)
  (declare (ignore new-target))
  (%js-new target (coerce args 'list)))

(defun %js-reflect-define-property (target key descriptor)
  (when (and (%js-ht-p target) (%js-ht-p descriptor))
    (let ((val (gethash "value" descriptor +js-undefined+)))
      (unless (eq val +js-undefined+)
        (setf (gethash (%js-to-string key) target) val))))
  t)

(defun %js-reflect-get-own-property-descriptor (target key)
  (%js-object-get-own-property-descriptor target key))

(defun %js-object-define-property (obj key descriptor)
  (when (and (%js-ht-p obj) (%js-ht-p descriptor))
    (let ((val (gethash "value" descriptor +js-undefined+))
          (get (gethash "get" descriptor +js-undefined+))
          (set (gethash "set" descriptor +js-undefined+)))
      (unless (eq val +js-undefined+)
        (setf (gethash (%js-to-string key) obj) val))
      (unless (eq get +js-undefined+)
        (setf (gethash (concatenate 'string "__get_" (%js-to-string key)) obj) get))
      (unless (eq set +js-undefined+)
        (setf (gethash (concatenate 'string "__set_" (%js-to-string key)) obj) set))))
  obj)

(defun %js-object-define-properties (obj props)
  (when (and (%js-ht-p obj) (%js-ht-p props))
    (maphash (lambda (k v)
               (when (%js-ht-p v)
                 (let ((val (gethash "value" v +js-undefined+)))
                   (unless (eq val +js-undefined+)
                     (setf (gethash k obj) val)))))
             props))
  obj)

(defun %js-object-get-own-property-descriptor (obj key)
  (if (%js-ht-p obj)
      (multiple-value-bind (val found) (gethash (%js-to-string key) obj)
        (if found
            (%js-make-object "value" val "writable" t "enumerable" t "configurable" t)
            +js-undefined+))
      +js-undefined+))

(defun %js-object-get-own-property-descriptors (obj)
  (let ((result (%js-make-ht)))
    (when (%js-ht-p obj)
      (maphash (lambda (k v)
                 (unless (%js-internal-key-p k)
                   (setf (gethash k result)
                         (%js-make-object "value" v "writable" t "enumerable" t "configurable" t))))
               obj))
    result))

(defun %js-make-typed-array-ctor (type-name)
  "Return a constructor lambda for the named TypedArray TYPE-NAME."
  (lambda (&optional arg) (%js-make-typed-array type-name arg)))

(defun %js-make-set-from-iterable (&optional (iter +js-undefined+))
  "Build a new JS Set, optionally seeded from ITER."
  (let ((s (%js-make-set)))
    (when (and (not (eq iter +js-undefined+))
               (not (eq iter +js-null+)))
      (%js-for-of iter (lambda (v) (%js-set-add s v))))
    s))

(defun %js-make-proxy-object (target handler)
  "Simplified Proxy constructor: wraps target+handler in a hash table."
  (let ((ht (%js-make-ht)))
    (setf (gethash "__proxy-target__" ht) target
          (gethash "__proxy-handler__" ht) handler)
    ht))

(defun %js-promise-try (fn &rest args)
  "ES2025 Promise.try: call FN with ARGS, wrapping synchronous throws."
  (handler-case
      (%js-promise-resolve (apply #'%js-funcall fn args))
    (js-exception (c)
      (%js-promise-reject (js-exception-value c)))))

(defun %js-math-sum-precise (iterable)
  "ES2026 Math.sumPrecise: precise sum of a numeric iterable."
  (let ((sum 0.0d0))
    (%js-for-of iterable (lambda (v)
      (incf sum (coerce (%js-to-number v) 'double-float))))
    sum))

(defun %js-error-is-error (val)
  "ES2026 Error.isError: true when VAL looks like an Error object."
  (and (%js-ht-p val)
       (or (gethash "message" val)
           (gethash "stack" val))))

(defun %js-make-finalization-registry (cleanup-fn)
  "ES2021 FinalizationRegistry stub (synchronous model — cleanup not observable)."
  (declare (ignore cleanup-fn))
  (let ((registry (%js-make-ht)))
    (setf (gethash "register" registry)
          (lambda (target value &optional _token)
            (declare (ignore target value _token))
            +js-undefined+)
          (gethash "unregister" registry)
          (lambda (_token) (declare (ignore _token)) nil))
    registry))

(defun %js-regexp-escape (str)
  "ES2024 RegExp.escape: escape all regex-special characters in STR."
  (with-output-to-string (out)
    (loop for ch across (%js-to-string str)
          do (when (member ch '(#\\ #\^ #\$ #\. #\| #\? #\* #\+ #\( #\) #\[ #\] #\{ #\} #\/ #\-))
               (write-char #\\ out))
             (write-char ch out))))

(defun %js-make-aggregate-error (errors message &optional _opts)
  "AggregateError constructor stub."
  (declare (ignore _opts))
  (%js-make-object "message" (%js-to-string (or message ""))
                   "errors"  (or errors (%js-make-array))
                   "name"    "AggregateError"))

(defun %js-make-abort-controller ()
  "AbortController constructor stub."
  (let ((sig (%js-make-object "aborted" nil "reason" +js-undefined+)))
    (%js-make-object "signal" sig
                     "abort"  (lambda (&optional reason)
                                (setf (gethash "aborted" sig) t
                                      (gethash "reason"  sig) (or reason +js-undefined+))))))

(defun %js-make-url (url &optional base)
  "URL constructor stub: minimal href/origin/pathname/search/hash."
  (declare (ignore base))
  (let ((href (%js-to-string url)))
    (%js-make-object "href" href "origin" "" "pathname" "" "search" "" "hash" ""
                     "toString" (lambda () href))))

(defun %js-make-url-search-params (&optional init)
  "URLSearchParams constructor stub."
  (declare (ignore init))
  (%js-make-object "get"      (lambda (_) (declare (ignore _)) +js-null+)
                   "set"      (lambda (_k _v) (declare (ignore _k _v)) +js-undefined+)
                   "has"      (lambda (_) (declare (ignore _)) nil)
                   "toString" (lambda () "")))

(defun %js-make-crypto ()
  "crypto global stub (getRandomValues + randomUUID)."
  (%js-make-object
   "getRandomValues" (lambda (arr)
                       (when (%js-vec-p arr)
                         (loop for i below (length arr)
                               do (setf (aref arr i) (random 256))))
                       arr)
   "randomUUID" (lambda ()
                  (format nil "~8,'0x-~4,'0x-4~3,'0x-~4,'0x-~12,'0x"
                          (random #xffffffff) (random #xffff) (random #xfff)
                          (logior #x8000 (random #x3fff)) (random #xffffffffffff)))))
