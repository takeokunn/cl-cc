;;;; packages/javascript/src/runtime-builtins.lisp — JS built-in dispatch table
;;;;
;;;; *js-builtin-map* maps built-in name strings to CL functions.
;;;; This file must be loaded LAST so all referenced functions are defined.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Reflect / Object property-descriptor helpers (for runtime-builtins-table.lisp)
;;; -----------------------------------------------------------------------

(defun %js-reflect-get (target key &optional receiver)
  (if (%js-proxy-object-p target)
      (%js-proxy-get target key (or receiver target))
      (%js-get-prop target key)))

(defun %js-reflect-set (target key value &optional receiver)
  (if (%js-proxy-object-p target)
      (%js-proxy-set target key value (or receiver target))
      (progn
        (%js-set-prop target key value)
        t)))

(defun %js-reflect-has (target key)
  (%js-in key target))

(defun %js-reflect-delete-property (target key)
  (%js-delete target key))

(defun %js-reflect-apply (fn this-arg args)
  (%js-call-with-this this-arg fn (coerce args 'list)))

(defun %js-reflect-construct (target args &optional new-target)
  (declare (ignore new-target))
  (%js-new target (coerce args 'list)))

(defun %js-object-descriptor-slot-key (prefix key)
  (concatenate 'string prefix (%js-to-string key)))

(defun %js-object-own-property-or-accessor-present-p (obj key)
  (let* ((k (%js-to-property-key key))
         (getter-key (%js-object-descriptor-slot-key "__get_" key))
         (setter-key (%js-object-descriptor-slot-key "__set_" key)))
    (or (nth-value 1 (gethash k obj))
        (nth-value 1 (gethash getter-key obj))
        (nth-value 1 (gethash setter-key obj)))))

(defun %js-object-define-property-allowed-p (obj key descriptor)
  (and (%js-ht-p obj)
       (%js-ht-p descriptor)
       (not (%js-object-frozen-p obj))
       (or (%js-object-own-property-or-accessor-present-p obj key)
           (%js-object-extensible-p obj))))

(defun %js-object-apply-property-descriptor (obj key descriptor)
  (when (%js-object-define-property-allowed-p obj key descriptor)
    (let* ((k (%js-to-property-key key))
           (getter-key (%js-object-descriptor-slot-key "__get_" key))
           (setter-key (%js-object-descriptor-slot-key "__set_" key)))
      (multiple-value-bind (val has-value-p) (gethash "value" descriptor)
        (when has-value-p
          (setf (gethash k obj) val)))
      (multiple-value-bind (getter has-getter-p) (gethash "get" descriptor)
        (when has-getter-p
          (setf (gethash getter-key obj) getter)))
      (multiple-value-bind (setter has-setter-p) (gethash "set" descriptor)
        (when has-setter-p
          (setf (gethash setter-key obj) setter))))
    t))

(defun %js-object-data-property-descriptor (obj value)
  (%js-make-object "value" value
                   "writable" (not (%js-object-frozen-p obj))
                   "enumerable" t
                   "configurable" (not (%js-object-sealed-p obj))))

(defun %js-object-accessor-property-descriptor (obj key)
  (let* ((descriptor (%js-make-object "enumerable" t
                                      "configurable" (not (%js-object-sealed-p obj))))
         (getter-key (%js-object-descriptor-slot-key "__get_" key))
         (setter-key (%js-object-descriptor-slot-key "__set_" key)))
    (multiple-value-bind (getter has-getter-p) (gethash getter-key obj)
      (when has-getter-p
        (setf (gethash "get" descriptor) getter)))
    (multiple-value-bind (setter has-setter-p) (gethash setter-key obj)
      (when has-setter-p
        (setf (gethash "set" descriptor) setter)))
    descriptor))

(defun %js-reflect-define-property (target key descriptor)
  (cond
    ((%js-proxy-object-p target)
     (%js-proxy-define-property target key descriptor))
    (t
     (%js-object-apply-property-descriptor target key descriptor))))

(defun %js-reflect-get-own-property-descriptor (target key)
  (%js-object-get-own-property-descriptor target key))

(defun %js-reflect-set-prototype-of (target proto)
  (if (%js-ht-p target)
      (let ((before (%js-object-get-prototype-of target)))
        (%js-object-set-prototype-of target proto)
        (or (eq before proto)
            (eq (%js-object-get-prototype-of target) proto)))
      nil))

(defun %js-reflect-prevent-extensions (target)
  (if (%js-ht-p target)
      (progn
        (%js-object-prevent-extensions target)
        t)
      nil))

(defun %js-object-define-property (obj key descriptor)
  (unless (%js-reflect-define-property obj key descriptor)
    (error "JS TypeError: Cannot define property ~A" (%js-to-string key)))
  obj)

(defun %js-object-define-properties (obj props)
  (when (and (%js-ht-p obj) (%js-ht-p props))
    (maphash (lambda (k v)
               (when (%js-ht-p v)
                 (%js-object-define-property obj k v)))
             props))
  obj)

(defun %js-object-get-own-property-descriptor (obj key)
  (cond
    ((%js-proxy-object-p obj)
     (%js-proxy-get-own-property-descriptor obj key))
    ((%js-ht-p obj)
     (let ((k (%js-to-property-key key)))
       (multiple-value-bind (val found) (gethash k obj)
         (cond
           (found
            (%js-object-data-property-descriptor obj val))
           ((%js-object-own-property-or-accessor-present-p obj k)
            (%js-object-accessor-property-descriptor obj k))
           (t +js-undefined+)))))
    (t +js-undefined+)))

(defun %js-object-get-own-property-descriptors (obj)
  (let ((result (%js-make-ht)))
    (cond
      ((%js-proxy-object-p obj)
       (dolist (key (%js-proxy-key-list (%js-proxy-own-keys obj)))
         (let ((descriptor (%js-object-get-own-property-descriptor obj key)))
             (unless (eq descriptor +js-undefined+)
               (setf (gethash (%js-to-string key) result) descriptor)))))
      ((%js-ht-p obj)
       (maphash (lambda (k v)
                  (unless (%js-internal-key-p k)
                    (setf (gethash k result)
                          (%js-object-data-property-descriptor obj v))))
                obj)
       (maphash (lambda (k v)
                  (declare (ignore v))
                  (let ((property-name (%js-object-accessor-property-name k)))
                    (when (and property-name
                               (not (nth-value 1 (gethash property-name result))))
                      (setf (gethash property-name result)
                            (%js-object-accessor-property-descriptor obj property-name)))))
                obj)))
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

(defun %js-proxy-object-p (obj)
  "True when OBJ is a runtime Proxy wrapper."
  (and (%js-ht-p obj)
       (nth-value 1 (gethash "__proxy-target__" obj))
       (nth-value 1 (gethash "__proxy-handler__" obj))))

(defun %js-proxy-target (proxy)
  (gethash "__proxy-target__" proxy))

(defun %js-proxy-handler (proxy)
  (gethash "__proxy-handler__" proxy))

(defun %js-proxy-trap (proxy name)
  (let ((handler (%js-proxy-handler proxy)))
    (when (%js-ht-p handler)
      (multiple-value-bind (trap found) (gethash name handler)
        (when (and found (funcall *js-callable-p* trap))
          trap)))))

(defun %js-proxy-call-trap (proxy name &rest args)
  (let ((trap (%js-proxy-trap proxy name)))
    (if trap
        (values (apply #'%js-funcall trap args) t)
        (values +js-undefined+ nil))))

(defun %js-proxy-key-list (keys)
  (cond
    ((%js-vec-p keys) (coerce keys 'list))
    ((listp keys) keys)
    (t nil)))

(defun %js-proxy-get (proxy key &optional receiver)
  (let ((target (%js-proxy-target proxy)))
    (multiple-value-bind (result trapped)
        (%js-proxy-call-trap proxy "get" target (%js-to-string key) (or receiver proxy))
      (if trapped
          result
          (%js-get-prop target key)))))

(defun %js-proxy-set (proxy key value &optional receiver)
  (let ((target (%js-proxy-target proxy)))
    (multiple-value-bind (result trapped)
        (%js-proxy-call-trap proxy "set" target (%js-to-string key) value (or receiver proxy))
      (if trapped
          (%js-truthy result)
          (progn
            (%js-set-prop target key value)
            t)))))

(defun %js-proxy-has (proxy key)
  (let ((target (%js-proxy-target proxy)))
    (multiple-value-bind (result trapped)
        (%js-proxy-call-trap proxy "has" target (%js-to-string key))
      (if trapped
          (%js-truthy result)
          (%js-in key target)))))

(defun %js-proxy-delete-property (proxy key)
  (let ((target (%js-proxy-target proxy)))
    (multiple-value-bind (result trapped)
        (%js-proxy-call-trap proxy "deleteProperty" target (%js-to-string key))
      (if trapped
          (%js-truthy result)
          (%js-delete target key)))))

(defun %js-proxy-own-keys (proxy)
  (let ((target (%js-proxy-target proxy)))
    (multiple-value-bind (result trapped)
        (%js-proxy-call-trap proxy "ownKeys" target)
      (if trapped
          result
          (%js-object-own-keys target)))))

(defun %js-proxy-define-property (proxy key descriptor)
  (let ((target (%js-proxy-target proxy)))
    (multiple-value-bind (result trapped)
        (%js-proxy-call-trap proxy "defineProperty" target (%js-to-string key) descriptor)
      (if trapped
          (%js-truthy result)
          (%js-reflect-define-property target key descriptor)))))

(defun %js-proxy-get-own-property-descriptor (proxy key)
  (let ((target (%js-proxy-target proxy)))
    (multiple-value-bind (result trapped)
        (%js-proxy-call-trap proxy "getOwnPropertyDescriptor" target (%js-to-string key))
      (if trapped
          result
          (%js-object-get-own-property-descriptor target key)))))

(defun %js-make-proxy-object (target handler)
  "Proxy constructor: stores target+handler and routes common traps at runtime."
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
  "ES2026 Error.isError: true when VAL is an Error object."
  (and (%js-ht-p val)
       (%js-instanceof val *js-error-class*)))

(defun %js-regexp-ascii-alnum-p (ch)
  (or (and (char>= ch #\0) (char<= ch #\9))
      (and (char>= ch #\A) (char<= ch #\Z))
      (and (char>= ch #\a) (char<= ch #\z))))

(defun %js-regexp-hex2 (code out)
  (format out "\\x~2,'0X" code))

(defun %js-regexp-unicode-escape (code out)
  (format out "\\u~4,'0X" code))

(defun %js-regexp-escape (str)
  "ES2025 RegExp.escape: escape a string for literal use in a RegExp pattern."
  (with-output-to-string (out)
    (loop for ch across (%js-to-string str)
          for first = t then nil
          for code = (char-code ch)
          do (cond
               ((and first (%js-regexp-ascii-alnum-p ch))
                (%js-regexp-hex2 code out))
               ((member ch '(#\^ #\$ #\\ #\. #\* #\+ #\? #\( #\) #\[ #\] #\{ #\} #\| #\/)
                        :test #'char=)
                (write-char #\\ out)
                (write-char ch out))
               ((member ch '(#\, #\- #\= #\< #\> #\# #\& #\! #\% #\: #\; #\@ #\~ #\' #\` #\")
                        :test #'char=)
                (%js-regexp-hex2 code out))
               ((char= ch #\Newline)
                (write-string "\\n" out))
               ((char= ch #\Return)
                (write-string "\\r" out))
               ((char= ch #\Tab)
                (write-string "\\t" out))
               ((char= ch #\Page)
                (write-string "\\f" out))
               ((char= ch #\Space)
                (write-string "\\x20" out))
               ((or (< code #x20)
                    (= code #x2028)
                    (= code #x2029))
                (if (< code #x100)
                    (%js-regexp-hex2 code out)
                    (%js-regexp-unicode-escape code out)))
               (t
                (write-char ch out))))))

(defun %js-make-aggregate-error (errors message &optional opts)
  "Create an AggregateError instance with errors, message, and optional cause."
  (%js-new *js-aggregate-error-class* (list errors message opts)))

(defun %js-abort-reason-object (name message)
  "Create a small DOMException-like abort reason object."
  (%js-make-object "name" name
                   "message" message
                   "toString" (lambda ()
                                (concatenate 'string name ": " message))))

(defun %js-abort-default-reason ()
  (%js-abort-reason-object "AbortError" "This operation was aborted"))

(defun %js-abort-timeout-reason ()
  (%js-abort-reason-object "TimeoutError" "The operation timed out"))

(defun %js-abort-event (target)
  (%js-make-object "type" "abort"
                   "target" target
                   "currentTarget" target
                   "defaultPrevented" nil))

(defun %js-abort-signal-listeners (signal)
  (or (gethash "__abort_listeners__" signal)
      (setf (gethash "__abort_listeners__" signal) nil)))

(defun %js-abort-call-listener (listener event signal)
  (cond
    ((or (funcall *js-callable-p* listener)
         (and (%js-ht-p listener) (gethash "__call__" listener)))
     (%js-call-with-this signal listener (list event)))
    ((and (%js-ht-p listener) (gethash "handleEvent" listener))
     (%js-call-with-this listener (gethash "handleEvent" listener) (list event)))))

(defun %js-abort-signal-dispatch-abort (signal)
  (let ((event (%js-abort-event signal))
        (handler (gethash "onabort" signal)))
    (when (and handler (not (eq handler +js-undefined+)))
      (%js-abort-call-listener handler event signal))
    (dolist (listener (copy-list (%js-abort-signal-listeners signal)))
      (%js-abort-call-listener listener event signal))
    t))

(defun %js-abort-signal-abort (signal reason)
  "Abort SIGNAL once with REASON, dispatching abort listeners synchronously."
  (unless (gethash "aborted" signal)
    (setf (gethash "aborted" signal) t
          (gethash "reason" signal) reason)
    (%js-abort-signal-dispatch-abort signal))
  +js-undefined+)

(defun %js-make-abort-signal (&optional (aborted nil) (reason +js-undefined+))
  "Create an AbortSignal-like EventTarget object."
  (let ((signal (%js-make-object "aborted" aborted
                                 "reason" reason
                                 "onabort" +js-undefined+)))
    (setf (gethash "__abort_listeners__" signal) nil)
    (setf (gethash "addEventListener" signal)
          (lambda (type listener &rest _)
            (declare (ignore _))
            (when (and (string= (%js-to-string type) "abort")
                       listener
                       (not (eq listener +js-undefined+))
                       (not (member listener (%js-abort-signal-listeners signal) :test #'eq)))
              (setf (gethash "__abort_listeners__" signal)
                    (append (%js-abort-signal-listeners signal) (list listener))))
            +js-undefined+))
    (setf (gethash "removeEventListener" signal)
          (lambda (type listener &rest _)
            (declare (ignore _))
            (when (string= (%js-to-string type) "abort")
              (setf (gethash "__abort_listeners__" signal)
                    (remove listener (%js-abort-signal-listeners signal) :test #'eq)))
            +js-undefined+))
    (setf (gethash "dispatchEvent" signal)
          (lambda (event)
            (when (and (%js-ht-p event)
                       (string= (%js-to-string (gethash "type" event)) "abort"))
              (%js-abort-signal-dispatch-abort signal))
            t))
    (setf (gethash "throwIfAborted" signal)
          (lambda ()
            (when (gethash "aborted" signal)
              (%js-throw (gethash "reason" signal)))
            +js-undefined+))
    signal))

(defun %js-make-abort-controller ()
  "Create an AbortController with a real AbortSignal-like signal."
  (let ((signal (%js-make-abort-signal)))
    (%js-make-object "signal" signal
                     "abort" (lambda (&optional (reason +js-undefined+ reason-p))
                               (%js-abort-signal-abort
                                signal
                                (if reason-p reason (%js-abort-default-reason)))))))

(defun %js-make-abort-controller-constructor ()
  "Callable/newable AbortController global."
  (%js-make-object
   "__new__" (lambda (&rest _)
               (declare (ignore _))
               (%js-make-abort-controller))
   "__call__" (lambda (&rest _)
                (declare (ignore _))
                (%js-make-abort-controller))))

(defun %js-abort-signal-aborted (&optional (reason +js-undefined+ reason-p))
  "AbortSignal.abort(reason): return an already-aborted signal."
  (%js-make-abort-signal t (if reason-p reason (%js-abort-default-reason))))

(defun %js-abort-signal-timeout (&optional _)
  "AbortSignal.timeout(ms): synchronous runtime model returns an aborted signal."
  (declare (ignore _))
  (%js-make-abort-signal t (%js-abort-timeout-reason)))

(defun %js-abort-signal-any (signals)
  "AbortSignal.any(iterable): abort when the first input signal aborts."
  (let ((combined (%js-make-abort-signal)))
    (labels ((abort-from (signal)
               (%js-abort-signal-abort
                combined
                (gethash "reason" signal (%js-abort-default-reason)))))
      (%js-for-of signals
        (lambda (signal)
          (when (%js-ht-p signal)
            (if (gethash "aborted" signal)
                (abort-from signal)
                (let ((add-listener (gethash "addEventListener" signal)))
                  (when (functionp add-listener)
                    (%js-call-with-this
                     signal add-listener
                     (list "abort" (lambda (&rest _) (declare (ignore _)) (abort-from signal)))))))))))
    combined))

(defun %js-make-abort-signal-constructor ()
  "AbortSignal global with standard static helpers."
  (%js-make-object
   "abort" #'%js-abort-signal-aborted
   "timeout" #'%js-abort-signal-timeout
   "any" #'%js-abort-signal-any
   "__call__" (lambda (&rest _)
                (declare (ignore _))
                (%js-make-abort-signal))))

(defun %js-split-string-on-char (string char)
  (let ((start 0)
        (parts nil))
    (loop for pos = (position char string :start start)
          do (push (subseq string start pos) parts)
          if pos
            do (setf start (1+ pos))
          else
            return (nreverse parts))))

(defun %js-url-decode-component (string)
  (with-output-to-string (out)
    (loop for i from 0 below (length string)
          for ch = (char string i)
          do (cond
               ((char= ch #\+)
                (write-char #\Space out))
               ((and (char= ch #\%)
                     (< (+ i 2) (length string))
                     (digit-char-p (char string (1+ i)) 16)
                     (digit-char-p (char string (+ i 2)) 16))
                (write-char
                 (code-char (+ (* 16 (digit-char-p (char string (1+ i)) 16))
                               (digit-char-p (char string (+ i 2)) 16)))
                 out)
                (incf i 2))
               (t
                (write-char ch out))))))

(defun %js-url-encode-component (value)
  (let ((string (%js-to-string value)))
    (with-output-to-string (out)
      (loop for ch across string
            for code = (char-code ch)
            do (cond
                 ((or (and (>= code (char-code #\a)) (<= code (char-code #\z)))
                      (and (>= code (char-code #\A)) (<= code (char-code #\Z)))
                      (and (>= code (char-code #\0)) (<= code (char-code #\9)))
                      (member ch '(#\- #\_ #\. #\*) :test #'char=))
                  (write-char ch out))
                 ((char= ch #\Space)
                  (write-char #\+ out))
                 (t
                  (format out "%~2,'0X" code)))))))

(defun %js-url-search-params-pairs (init)
  (cond
    ((or (null init) (eq init +js-undefined+))
     nil)
    ((stringp init)
     (let ((query (if (and (plusp (length init)) (char= (char init 0) #\?))
                      (subseq init 1)
                      init)))
       (loop for part in (%js-split-string-on-char query #\&)
             unless (string= part "")
               collect (let* ((eq-pos (position #\= part))
                              (key (if eq-pos (subseq part 0 eq-pos) part))
                              (value (if eq-pos (subseq part (1+ eq-pos)) "")))
                         (cons (%js-url-decode-component key)
                               (%js-url-decode-component value))))))
    ((%js-ht-p init)
     (let ((pairs nil))
       (maphash (lambda (key value)
                  (push (cons (%js-to-string key) (%js-to-string value)) pairs))
                init)
       (nreverse pairs)))
    ((%js-vec-p init)
     (loop for pair across init
           when (and (%js-vec-p pair) (>= (length pair) 2))
             collect (cons (%js-to-string (aref pair 0))
                           (%js-to-string (aref pair 1)))))
    (t nil)))

(defun %js-url-search-params-string (pairs)
  (format nil "~{~A~^&~}"
          (mapcar (lambda (pair)
                    (format nil "~A=~A"
                            (%js-url-encode-component (car pair))
                            (%js-url-encode-component (cdr pair))))
                  pairs)))

(defun %js-url-normalize-path (path)
  "Normalize URL path dot segments while preserving absolute/trailing slashes."
  (let ((absolute-p (and (plusp (length path)) (char= (char path 0) #\/)))
        (trailing-slash-p (and (> (length path) 1)
                               (char= (char path (1- (length path))) #\/)))
        (segments nil))
    (dolist (segment (%js-split-string-on-char path #\/))
      (cond
        ((or (string= segment "") (string= segment "."))
         nil)
        ((string= segment "..")
         (cond
           (segments (pop segments))
           ((not absolute-p) (push segment segments))))
        (t
         (push segment segments))))
    (let* ((body (format nil "~{~A~^/~}" (nreverse segments)))
           (with-leading (if absolute-p
                             (concatenate 'string "/" body)
                             body)))
      (cond
        ((and absolute-p (string= body "")) "/")
        ((and trailing-slash-p (not (string= with-leading "")))
         (concatenate 'string with-leading "/"))
        (t with-leading)))))

(defun %js-make-url-search-params (&optional init on-change)
  "URLSearchParams: ordered key/value query storage with common mutators."
  (let ((pairs (%js-url-search-params-pairs init)))
    (labels ((changed ()
               (when on-change
                 (funcall on-change (%js-url-search-params-string pairs)))
               +js-undefined+)
             (key= (pair key)
               (string= (car pair) (%js-to-string key)))
             (values-for (key)
               (loop for pair in pairs
                     when (key= pair key)
                       collect (cdr pair)))
             (serialize ()
               (%js-url-search-params-string pairs)))
      (%js-make-object
       "append" (lambda (key value)
                  (setf pairs (append pairs (list (cons (%js-to-string key)
                                                        (%js-to-string value)))))
                  (changed))
       "delete" (lambda (key)
                  (setf pairs (remove-if (lambda (pair) (key= pair key)) pairs))
                  (changed))
       "get" (lambda (key)
               (let ((values (values-for key)))
                 (if values (first values) +js-null+)))
       "getAll" (lambda (key)
                  (apply #'%js-make-array (values-for key)))
       "has" (lambda (key)
               (not (null (values-for key))))
       "set" (lambda (key value)
               (let ((string-key (%js-to-string key))
                     (string-value (%js-to-string value))
                     (written nil)
                     (result nil))
                 (dolist (pair pairs)
                   (cond
                     ((string= (car pair) string-key)
                      (unless written
                        (push (cons string-key string-value) result)
                        (setf written t)))
                     (t
                      (push pair result))))
                 (unless written
                   (push (cons string-key string-value) result))
                 (setf pairs (nreverse result))
                 (changed)))
       "sort" (lambda ()
                (setf pairs
                      (stable-sort pairs
                                   (lambda (a b)
                                     (string< (car a) (car b)))))
                (changed))
       "toString" #'serialize))))

(defun %js-url-build-href (protocol host pathname search hash)
  (concatenate 'string
               protocol
               (if (and (plusp (length protocol)) (plusp (length host))) "//" "")
               host
               pathname
               search
               hash))

(defun %js-parse-url-record (url &optional base)
  (let* ((input (%js-to-string url))
         (base-record (and base
                           (not (or (null base) (eq base +js-undefined+)))
                           (%js-parse-url-record base)))
         (absolute-p (search "://" input))
         (resolved (cond
                     (absolute-p input)
                     (base-record
                      (let* ((base-origin (getf base-record :origin))
                             (base-path (getf base-record :pathname))
                             (base-search (getf base-record :search))
                             (slash (position #\/ base-path :from-end t))
                             (base-dir (if slash (subseq base-path 0 (1+ slash)) "/")))
                        (cond
                          ((and (plusp (length input)) (char= (char input 0) #\/))
                           (concatenate 'string base-origin input))
                          ((and (plusp (length input)) (char= (char input 0) #\?))
                           (concatenate 'string base-origin base-path input))
                          ((and (plusp (length input)) (char= (char input 0) #\#))
                           (concatenate 'string base-origin base-path base-search input))
                          (t
                           (concatenate 'string base-origin base-dir input)))))
                     (t input)))
         (hash-pos (position #\# resolved))
         (before-hash (if hash-pos (subseq resolved 0 hash-pos) resolved))
         (hash (if hash-pos (subseq resolved hash-pos) ""))
         (search-pos (position #\? before-hash))
         (before-search (if search-pos (subseq before-hash 0 search-pos) before-hash))
         (search (if search-pos (subseq before-hash search-pos) ""))
         (scheme-pos (search "://" before-search))
         (protocol (if scheme-pos (subseq before-search 0 (1+ scheme-pos)) ""))
         (after-scheme (if scheme-pos (subseq before-search (+ scheme-pos 3)) before-search))
         (path-pos (position #\/ after-scheme))
         (host (if scheme-pos
                   (if path-pos (subseq after-scheme 0 path-pos) after-scheme)
                   ""))
         (pathname (cond
                     (path-pos (subseq after-scheme path-pos))
                     (scheme-pos "/")
                     ((and (plusp (length before-search))
                           (char= (char before-search 0) #\/))
                      before-search)
                     (t before-search)))
         (normalized-pathname (%js-url-normalize-path pathname))
         (origin (if (and (plusp (length protocol)) (plusp (length host)))
                     (concatenate 'string protocol "//" host)
                     "")))
    (list :protocol protocol
          :host host
          :hostname (let ((colon (position #\: host :from-end t)))
                      (if colon (subseq host 0 colon) host))
          :port (let ((colon (position #\: host :from-end t)))
                  (if colon (subseq host (1+ colon)) ""))
          :origin origin
          :pathname (if (and scheme-pos (string= normalized-pathname ""))
                        "/"
                        normalized-pathname)
          :search search
          :hash hash)))

(defun %js-make-url (url &optional base)
  "URL constructor: parse common absolute and base-relative URL fields."
  (let* ((record (%js-parse-url-record url base))
         (protocol (getf record :protocol))
         (host (getf record :host))
         (origin (getf record :origin))
         (pathname (getf record :pathname))
         (search (getf record :search))
         (hash (getf record :hash))
         (href (%js-url-build-href protocol host pathname search hash))
         object)
    (setf object
          (%js-make-object
           "href" href
           "protocol" protocol
           "host" host
           "hostname" (getf record :hostname)
           "port" (getf record :port)
           "origin" origin
           "pathname" pathname
           "search" search
           "hash" hash
           "toString" (lambda () (gethash "href" object))))
    (setf (gethash "searchParams" object)
          (%js-make-url-search-params
           search
           (lambda (query)
             (let ((next-search (if (string= query "")
                                    ""
                                    (concatenate 'string "?" query))))
               (setf (gethash "search" object) next-search
                     (gethash "href" object)
                     (%js-url-build-href
                      (gethash "protocol" object)
                      (gethash "host" object)
                      (gethash "pathname" object)
                      next-search
                      (gethash "hash" object)))))))
    object))

(defun %js-crypto-random-bytes (count)
  "Return COUNT bytes from the host CSPRNG, with a non-crypto fallback."
  (let ((bytes (make-array count :element-type '(unsigned-byte 8))))
    (handler-case
        (with-open-file (in "/dev/urandom" :direction :input
                                          :element-type '(unsigned-byte 8))
          (unless (= count (read-sequence bytes in))
            (error "short read from host CSPRNG")))
      (error ()
        (dotimes (i count)
          (setf (aref bytes i) (random #x100)))))
    bytes))

(defun %js-crypto-random-raw-integer (type-name)
  "Return random integer bits sized for TYPE-NAME."
  (let ((size (case (%js-ta-type-tag type-name)
                ((:int8 :uint8 :uint8c) 1)
                ((:int16 :uint16)       2)
                ((:int32 :uint32)       4)
                ((:bigint64 :biguint64) 8)
                ((:float32 :float64)
                 (error "JS TypeError: crypto.getRandomValues requires an integer TypedArray"))
                (t 1))))
    (loop with value = 0
          for byte across (%js-crypto-random-bytes size)
          do (setf value (logior (ash value 8) byte))
          finally (return value))))

(defun %js-crypto-random-uuid ()
  "Return a Web Crypto style RFC 4122 version 4 UUID string."
  (let ((bytes (%js-crypto-random-bytes 16)))
    (setf (aref bytes 6) (logior #x40 (logand (aref bytes 6) #x0f))
          (aref bytes 8) (logior #x80 (logand (aref bytes 8) #x3f)))
    (string-downcase
     (format nil "~2,'0x~2,'0x~2,'0x~2,'0x-~2,'0x~2,'0x-~2,'0x~2,'0x-~2,'0x~2,'0x-~2,'0x~2,'0x~2,'0x~2,'0x~2,'0x~2,'0x"
             (aref bytes 0) (aref bytes 1) (aref bytes 2) (aref bytes 3)
             (aref bytes 4) (aref bytes 5)
             (aref bytes 6) (aref bytes 7)
             (aref bytes 8) (aref bytes 9)
             (aref bytes 10) (aref bytes 11) (aref bytes 12)
             (aref bytes 13) (aref bytes 14) (aref bytes 15)))))

(defun %js-crypto-integer-typed-array-p (arr)
  "Return true when ARR is an integer TypedArray accepted by getRandomValues."
  (and (js-typed-array-p arr)
       (not (member (%js-ta-type-tag (js-ta-type-name arr))
                    '(:float32 :float64)))))

(defun %js-crypto-get-random-values (arr)
  "Fill integer TypedArray ARR with random values and return ARR."
  (unless (%js-crypto-integer-typed-array-p arr)
    (error "JS TypeError: crypto.getRandomValues requires an integer TypedArray"))
  (let ((byte-length (* (js-ta-length arr) (js-ta-element-size arr))))
    (when (> byte-length 65536)
      (error "JS QuotaExceededError: crypto.getRandomValues byteLength exceeds 65536")))
  (let ((type-name (js-ta-type-name arr))
        (buffer (js-ta-buffer arr)))
    (dotimes (i (js-ta-length arr))
      (setf (aref buffer i)
            (%js-ta-coerce-element
             type-name
             (%js-crypto-random-raw-integer type-name)))))
  arr)

(defun %js-make-crypto ()
  "crypto global object (getRandomValues + randomUUID)."
  (%js-make-object
   "getRandomValues" #'%js-crypto-get-random-values
   "randomUUID" #'%js-crypto-random-uuid))
