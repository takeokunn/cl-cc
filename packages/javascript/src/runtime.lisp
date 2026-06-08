;;;; packages/javascript/src/runtime.lisp — JavaScript runtime builtins
;;;;
;;;; Implements JS semantics in Common Lisp.
;;;; JS objects  => CL hash tables (:test #'equal), key is string
;;;; JS arrays   => CL adjustable vectors (element-type t)
;;;; JS null     => :js-null
;;;; JS undefined=> :js-undefined
;;;; JS NaN      => :js-nan
;;;; JS Infinity => :js-infinity / :js-neg-infinity
;;;;
;;;; Private class fields  => stored in a nested HT under key "__private__"

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Type constants
;;; -----------------------------------------------------------------------

(defconstant +js-undefined+ :js-undefined)
(defconstant +js-null+      :js-null)
(defconstant +js-nan+       :js-nan)
(defconstant +js-infinity+  :js-infinity)
(defconstant +js-neg-infinity+ :js-neg-infinity)

;;; Real IEEE-754 double specials, built from raw bit patterns so SBCL never
;;; constant-folds a literal (/ 0.0d0 0.0d0) — which would trap at compile time
;;; with FLOATING-POINT-INVALID-OPERATION. make-double-float does no FP math.
(declaim (type double-float *js-nan-float* *js-inf-float* *js-neg-inf-float*))
(defparameter *js-nan-float*     (sb-kernel:make-double-float #x7FF80000 0))   ; quiet NaN
(defparameter *js-inf-float*     (sb-kernel:make-double-float #x7FF00000 0))   ; +Infinity
(defparameter *js-neg-inf-float* (sb-kernel:make-double-float -1048576 0))     ; -Infinity (#xFFF00000)

;;; -----------------------------------------------------------------------
;;;  Internal helpers
;;; -----------------------------------------------------------------------

(defun %js-make-ht (&optional (size 8))
  (make-hash-table :test #'equal :size size))

(defun %js-make-vec (&optional (size 0))
  (make-array size :element-type t :adjustable t :fill-pointer size))

(defun %js-vec-p (x)
  (and (vectorp x) (not (stringp x))))

(defun %js-ht-p (x)
  (hash-table-p x))

(defun %js-float-nan-p (x)
  "Portable NaN test: NaN is the only float not numerically equal to itself."
  (and (floatp x) (/= x x)))

(defun %js-float-infinity-p (x)
  "Portable infinity test against the largest finite double magnitudes."
  (and (floatp x)
       (or (> x most-positive-double-float)
           (< x most-negative-double-float))))

(defun %js-nan-p (x)
  (or (eq x :js-nan)
      (%js-float-nan-p x)))

;;; -----------------------------------------------------------------------
;;;  Type system
;;; -----------------------------------------------------------------------

;;; ─── BigInt struct (declared early so typeof/to-string can reference it) ─────

(defstruct (js-bigint (:constructor %make-js-bigint (value)))
  (value 0 :type integer))

(defun %js-bigint-p (x) (js-bigint-p x))

(defun %js-typeof (x)
  "Return JS typeof string for X."
  (cond
    ((eq x +js-undefined+)   "undefined")
    ((eq x +js-null+)        "object")       ; historic quirk
    ((eq x t)                "boolean")
    ((eq x nil)              "boolean")
    ((stringp x)             "string")
    ((numberp x)             "number")
    ((eq x :js-nan)          "number")
    ((eq x :js-infinity)     "number")
    ((eq x :js-neg-infinity) "number")
    ((%js-vec-p x)           "object")
    ((%js-ht-p x)
     (let ((callable (gethash "__call__" x)))
       (if callable "function" "object")))
    ((functionp x)           "function")
    ;; Symbol — must come before the default "object" case
    ;; js-symbol-p is defined in runtime-symbol.lisp (loaded later), so use typep
    ((typep x 'js-symbol)    "symbol")
    ;; BigInt — defined in runtime-ops.lisp (loaded later), so use typep
    ((typep x 'js-bigint)    "bigint")
    (t                       "object")))

(defun %js-truthy (x)
  "JS truthiness: false, 0, NaN, \"\", null, undefined are falsy."
  (not (or (eq x nil)
           (eq x +js-undefined+)
           (eq x +js-null+)
           (eq x :js-nan)
           (eql x 0)
           (eql x 0.0d0)
           (eql x -0.0d0)
           (equal x ""))))

(defun %js-not-nullish (x)
  "True if X is not null or undefined."
  (not (or (eq x +js-null+) (eq x +js-undefined+))))

(defun %js-to-number (x)
  "JS ToNumber coercion."
  (cond
    ((numberp x)             (coerce x 'double-float))
    ((eq x +js-undefined+)  *js-nan-float*)
    ((eq x +js-null+)       0.0d0)
    ((eq x t)               1.0d0)
    ((eq x nil)             0.0d0)
    ((eq x :js-nan)         *js-nan-float*)
    ((eq x :js-infinity)    *js-inf-float*)
    ((eq x :js-neg-infinity)*js-neg-inf-float*)
    ((stringp x)
     (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) x))
            (val (if (string= trimmed "")
                     0.0d0
                     ;; SECURITY: bind *read-eval* to nil so a string such as
                     ;; "#.(...)" cannot trigger read-time code execution, and
                     ;; require the parsed datum to be a real number (read can
                     ;; otherwise yield symbols/lists) — JS ToNumber must only
                     ;; ever produce a number or NaN.
                     (handler-case
                         (let* ((*read-eval* nil)
                                (datum (read-from-string trimmed)))
                           (if (realp datum)
                               (coerce datum 'double-float)
                               *js-nan-float*))
                       (error () *js-nan-float*)))))
       val))
    (t *js-nan-float*)))

(defun %js-loose-eq (a b)
  "JS == with type coercion."
  (cond
    ;; NaN never equals anything — must be checked BEFORE (equal a b)
    ;; because (equal :js-nan :js-nan) is t for keyword sentinels.
    ((%js-nan-p a) nil)
    ((%js-nan-p b) nil)
    ;; strict identity first
    ((equal a b) t)
    ;; null == undefined
    ((and (eq a +js-null+) (eq b +js-undefined+)) t)
    ((and (eq a +js-undefined+) (eq b +js-null+)) t)
    ;; number == string  => coerce string to number
    ((and (numberp a) (stringp b))
     (%js-loose-eq a (%js-to-number b)))
    ((and (stringp a) (numberp b))
     (%js-loose-eq (%js-to-number a) b))
    ;; boolean => number
    ((eq a t)  (%js-loose-eq 1.0d0 b))
    ((eq a nil) (%js-loose-eq 0.0d0 b))
    ((eq b t)  (%js-loose-eq a 1.0d0))
    ((eq b nil) (%js-loose-eq a 0.0d0))
    ;; numeric comparison
    ((and (numberp a) (numberp b)) (= a b))
    (t nil)))

(defun %js-strict-eq (a b)
  "JS === strict equality, no coercion."
  (cond
    ((and (%js-nan-p a) (%js-nan-p b)) nil)  ; NaN !== NaN
    ((and (numberp a) (numberp b)) (= a b))
    (t (equal a b))))

(defun %js-instanceof (obj constructor)
  "JS instanceof. constructor must be a hash-table with __prototype__."
  (when (%js-ht-p obj)
    (let ((proto (and (%js-ht-p constructor)
                      (gethash "__prototype__" constructor))))
      (when proto
        (let ((obj-proto (gethash "__proto__" obj)))
          (loop while (%js-ht-p obj-proto)
                when (eq obj-proto proto) return t
                do (setf obj-proto (gethash "__proto__" obj-proto))
                finally (return nil)))))))

;;; -----------------------------------------------------------------------
;;;  Callback invocation + method resolution hooks
;;; -----------------------------------------------------------------------

(defvar %js-this +js-undefined+
  "Dynamically bound to the receiver when a JS method is called.
JS source `this.x' compiles to (%js-get-prop %js-this \"x\").
Methods are called via %js-funcall-with-this which establishes this binding.")

(defvar *js-apply-fn* (lambda (fn args) (apply fn args))
  "Invoker used to call a JS callback value (e.g. the FN passed to Array.map /
filter / reduce / sort). Defaults to host APPLY so plain host lambdas work in
unit tests; the pipeline rebinds it to a VM-closure-aware invoker so a callback
that is a compiled-JS closure (vm-closure-object) is dispatched through the VM.")

(defun %js-funcall (fn &rest args)
  "Call JS callback FN with ARGS through the installed *js-apply-fn* invoker.
Array higher-order methods use this instead of CL:FUNCALL so the same code path
works whether FN is a host function (tests) or a compiled-JS closure (runtime)."
  (funcall *js-apply-fn* fn args))

(defvar *js-method-resolver* nil
  "When set, a function (receiver method-name-string) -> a bound method closure,
or +js-undefined+ when the name is not a method. Installed by a late runtime
file once every %js-array-*/%js-string-* method is defined, so %js-get-prop can
resolve obj.method without a load-order cycle. This is what lets `arr.push(x)',
`nums.map(f)' and `s.toUpperCase()' resolve to a callable value the VM can
invoke — mirroring how console.log resolves to a function value.")

(defun %js-method-ref (obj key)
  "Resolve OBJ.KEY to a bound method via *js-method-resolver*, else +js-undefined+."
  (if *js-method-resolver*
      (funcall *js-method-resolver* obj key)
      +js-undefined+))

(defvar *js-callable-p* #'functionp
  "Predicate: is X a callable JS value? Defaults to host FUNCTIONP; the pipeline
extends it to also recognize a compiled-JS closure (vm-closure-object), so
prototype-chain method lookup can tell a method from an inherited data value.")

(defun %js-proto-method-lookup (obj k)
  "Walk OBJ's __proto__ chain for key K (JS prototype method resolution). A
callable found on the chain is a METHOD: return a closure that binds %js-this
to OBJ and then calls the method. A non-callable inherited value is returned
as-is; a miss yields undefined.
The dynamic binding of %js-this is what makes `this.x' in method bodies work."
  (loop with proto = (gethash "__proto__" obj)
        while (%js-ht-p proto)
        do (multiple-value-bind (val found) (gethash k proto)
             (when found
               (return (if (funcall *js-callable-p* val)
                           ;; Bind %js-this dynamically so `this' in the method
                           ;; body resolves to OBJ, then dispatch through
                           ;; %js-funcall for VM-closure compatibility.
                           (let ((method val) (receiver obj))
                             (lambda (&rest args)
                               (let ((%js-this receiver))
                                 (funcall *js-apply-fn* method args))))
                           val)))
             (setf proto (gethash "__proto__" proto)))
        finally (return +js-undefined+)))

;;; -----------------------------------------------------------------------
;;;  Property access
;;; -----------------------------------------------------------------------

(defun %js-get-prop (obj key)
  "Get property KEY from JS object/array/string."
  (let ((k (%js-to-string key)))
    (cond
      ((%js-vec-p obj)
       (cond
         ((string= k "length") (length obj))
         ((every #'digit-char-p k)
          (let ((idx (parse-integer k)))
            (if (< idx (length obj))
                (aref obj idx)
                +js-undefined+)))
         (t (%js-method-ref obj k))))
      ((stringp obj)
       (cond
         ((string= k "length") (length obj))
         ((every #'digit-char-p k)
          (let ((idx (parse-integer k)))
            (if (< idx (length obj))
                (string (char obj idx))
                +js-undefined+)))
         (t (%js-method-ref obj k))))
      ((%js-ht-p obj)
       (multiple-value-bind (val found) (gethash k obj)
         (if found val (%js-proto-method-lookup obj k))))
      ((eq obj +js-null+) (error "JS TypeError: Cannot read properties of null"))
      ((eq obj +js-undefined+) (error "JS TypeError: Cannot read properties of undefined"))
      (t +js-undefined+))))

(defun %js-set-prop (obj key value)
  "Set property KEY on JS object/array."
  (let ((k (%js-to-string key)))
    (cond
      ((%js-vec-p obj)
       (cond
         ((string= k "length")
          (let ((new-len (truncate value)))
            (adjust-array obj new-len :fill-pointer new-len)))
         ((every #'digit-char-p k)
          (let ((idx (parse-integer k)))
            (when (>= idx (length obj))
              (adjust-array obj (1+ idx) :fill-pointer (1+ idx)
                            :initial-element +js-undefined+))
            (setf (aref obj idx) value)))
         (t nil)))
      ((%js-ht-p obj)
       (setf (gethash k obj) value))
      (t nil)))
  value)

(defun %js-delete (obj key)
  "JS delete operator."
  (let ((k (%js-to-string key)))
    (when (%js-ht-p obj)
      (remhash k obj)))
  t)

(defun %js-in (key obj)
  "JS 'key in obj'."
  (let ((k (%js-to-string key)))
    (cond
      ((%js-ht-p obj) (nth-value 1 (gethash k obj)))
      ((%js-vec-p obj)
       (or (string= k "length")
           (and (every #'digit-char-p k)
                (< (parse-integer k) (length obj)))))
      (t nil))))

(defun %js-optional-chain (obj key)
  "Return nil if OBJ is null/undefined, else %js-get-prop."
  (if (%js-not-nullish obj)
      (%js-get-prop obj key)
      +js-undefined+))

(defun %js-optional-call (func &rest args)
  "Call FUNC with ARGS unless FUNC is null/undefined."
  (if (%js-not-nullish func)
      (apply func args)
      +js-undefined+))

;;; -----------------------------------------------------------------------
;;;  String / Template
;;; -----------------------------------------------------------------------

(defun %js-add (a b)
  "JS `+' operator: string concatenation when either operand is a string,
otherwise numeric addition. Models the common ECMAScript number|string cases of
ToPrimitive + — `1 + 2' => 3, `\"a\" + \"b\"' => \"ab\", `\"n=\" + 5' => \"n=5\".
JS `+' is polymorphic, so it cannot lower to the numeric-only make-vm-add; it
routes here via *js-binop-runtime-helpers*."
  (if (or (stringp a) (stringp b))
      (concatenate 'string (%js-to-string a) (%js-to-string b))
      (+ a b)))

(defun %js-mod (a b)
  "JS `%' remainder operator. Like CL REM, the result takes the sign of the
dividend (5 % 3 => 2, -5 % 3 => -2), matching ECMAScript; division by zero => NaN."
  (if (and (numberp b) (zerop b))
      :js-nan
      (rem a b)))

(defun %js-pow (a b)
  "JS `**' exponentiation operator (Math.pow / a ** b)."
  (expt a b))

(defun %js-to-string (x)
  "JS ToString coercion."
  (cond
    ((stringp x)             x)
    ((eq x +js-undefined+)  "undefined")
    ((eq x +js-null+)       "null")
    ((eq x t)               "true")
    ((eq x nil)             "false")
    ((eq x :js-nan)         "NaN")
    ((eq x :js-infinity)    "Infinity")
    ((eq x :js-neg-infinity) "-Infinity")
    ((integerp x)           (format nil "~D" x))
    ((floatp x)
     (if (%js-float-nan-p x)
         "NaN"
         (if (%js-float-infinity-p x)
             (if (> x 0) "Infinity" "-Infinity")
             (let ((s (format nil "~F" x)))
               ;; Remove trailing zeros after decimal, JS style
               (string-right-trim "0" (string-right-trim "." s))))))
    ((numberp x)            (format nil "~A" x))
    ((%js-ht-p x)           "[object Object]")
    ((%js-vec-p x)
     (let ((parts (loop for i below (length x)
                        collect (%js-to-string (aref x i)))))
       (format nil "~{~A~^,~}" parts)))
    ;; BigInt — defined in runtime-ops.lisp (loaded later)
    ((typep x 'js-bigint)   (format nil "~D" (js-bigint-value x)))
    (t (format nil "~A" x))))

(defun %js-template-string (parts)
  "Concatenate template literal parts (already evaluated)."
  (apply #'concatenate 'string
         (mapcar #'%js-to-string parts)))

(defun %js-concat (a b)
  "JS + operator with string coercion."
  (if (or (stringp a) (stringp b))
      (concatenate 'string (%js-to-string a) (%js-to-string b))
      (let ((na (%js-to-number a))
            (nb (%js-to-number b)))
        (+ na nb))))

;;; -----------------------------------------------------------------------
;;;  Control flow / exceptions
;;; -----------------------------------------------------------------------

(define-condition js-exception ()
  ((value :initarg :value :reader js-exception-value)))

(defun %js-throw (value)
  (error 'js-exception :value value))

(defun %js-try-catch-finally (try-thunk catch-thunk finally-thunk)
  "Execute TRY-THUNK; on JS exception call CATCH-THUNK with the value.
   FINALLY-THUNK is always called. Returns value of try or catch."
  (let ((result +js-undefined+))
    (unwind-protect
         (handler-case
             (setf result (funcall try-thunk))
           (js-exception (c)
             (when catch-thunk
               (setf result (funcall catch-thunk (js-exception-value c))))))
      (when finally-thunk
        (funcall finally-thunk)))
    result))

(defun %js-for-in (obj body-fn)
  "Execute BODY-FN for each enumerable string key in OBJ.
Skips internal double-underscore keys (__proto__, __class__, etc.)."
  (when (%js-ht-p obj)
    (maphash (lambda (k v)
               (declare (ignore v))
               ;; Skip internal/prototype-chain keys (double-underscore prefix+suffix)
               ;; e.g. __proto__, __class__, __constructor__, __super__, __new__
               (unless (let ((n (length k)))
                         (and (> n 4)
                              (string= k "__" :end1 2)
                              (string= k "__" :start1 (- n 2))))
                 (%js-funcall body-fn k)))
             obj))
  +js-undefined+)

(defun %js-advance-iterator (iter body-fn)
  "Drain a JS iterator object (with a \"next\" method), calling BODY-FN per value."
  (let ((next-fn (gethash "next" iter)))
    (when next-fn
      (loop
        (let ((result (%js-funcall next-fn)))
          (when (or (not (%js-ht-p result))
                    (%js-truthy (gethash "done" result)))
            (return))
          (%js-funcall body-fn (gethash "value" result)))))))

(defun %js-advance-cl-iterator (iter body-fn)
  "Drain a CL-iterator (closure returning (:value v :done p) or :done)."
  (loop
    (multiple-value-bind (val done) (%js-iter-next iter)
      (when done (return))
      (%js-funcall body-fn val))))

(defun %js-for-of (iterable body-fn)
  "Execute BODY-FN for each element of ITERABLE.
Supports: arrays, strings, Map (entries), Set (values),
CL-iterator closures, JS iterator objects, generator objects."
  (cond
    ;; Plain CL vector (JS Array)
    ((%js-vec-p iterable)
     (loop for i below (length iterable)
           do (%js-funcall body-fn (aref iterable i))))
    ;; String — iterate characters
    ((stringp iterable)
     (loop for ch across iterable
           do (%js-funcall body-fn (string ch))))
    ;; JS Map → iterate [key, value] pairs in insertion order
    ((typep iterable 'js-map)
     (dolist (k (js-map-order iterable))
       (let ((v (gethash k (js-map-ht iterable) +js-undefined+)))
         (%js-funcall body-fn (%js-make-array k v)))))
    ;; CL closure — treat as an iterator (used by %js-make-cl-iterator)
    ((functionp iterable)
     (%js-advance-cl-iterator iterable body-fn))
    ;; JS object: check for iterator protocol (@@iterator or "next" method)
    ((%js-ht-p iterable)
     (let ((next-fn (gethash "next" iterable))
           (iter-fn (gethash "@@iterator" iterable)))
       (cond
         ;; Already an iterator (has .next)
         (next-fn (%js-advance-iterator iterable body-fn))
         ;; Iterable (has @@iterator factory)
         (iter-fn
          (let ((iter (%js-funcall iter-fn)))
            (%js-for-of iter body-fn)))
         ;; Set-like hash-table (all values are t — Set representation)
         ;; For now: if hash-table is not an Object (no string-key convention),
         ;; iterate keys. We use the presence of +php-null+ sentinel to detect
         ;; PHP arrays and skip those.
         (t
          ;; Iterate hash-table keys as a Set (best effort)
          (maphash (lambda (k v)
                     (declare (ignore v))
                     (unless (and (stringp k) (%js-internal-key-p k))
                       (%js-funcall body-fn k)))
                   iterable)))))
    (t nil))
  +js-undefined+)

(defun %js-iter-values (iterable)
  "Collect ITERABLE's values into a fresh CL list, in order. This is the 1-arg
companion to the 2-arg %js-for-of (which calls a body-fn): the for-of loop
LOWERING walks the resulting list with car/cdr so that break/continue and the
loop body's own scope work as ordinary statements. Reuses %js-for-of so every
iterable kind (array, string, iterator-protocol object) is handled in one place."
  (let ((acc nil))
    (%js-for-of iterable (lambda (el) (push el acc)))
    (nreverse acc)))

(defun %js-iter-keys (obj)
  "Collect OBJ's enumerable string keys into a fresh CL list — the 1-arg
companion to %js-for-in, used by the for-in loop lowering (see %js-iter-values)."
  (let ((acc nil))
    (%js-for-in obj (lambda (k) (push k acc)))
    (nreverse acc)))

