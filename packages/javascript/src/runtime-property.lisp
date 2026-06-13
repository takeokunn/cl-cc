;;;; packages/javascript/src/runtime-property.lisp — JS property access + string coercion
;;;;
;;;; %js-get-prop / %js-set-prop / %js-delete / %js-in / optional-chain ops,
;;;; plus %js-add / %js-mod / %js-divide / %js-pow / %js-to-string /
;;;; %js-template-string / %js-concat (arithmetic + coercion used by the above).
;;;;
;;;; Load order: after runtime.lisp (needs type predicates, method-resolver vars,
;;;;             proto-lookup helpers defined there).

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Property access
;;; -----------------------------------------------------------------------

(defun %js-accessor-descriptor-p (v)
  "True when V is a get/set accessor descriptor produced by %js-accessor
(a hash-table tagged __accessor__ carrying KIND and FN)."
  (and (%js-ht-p v) (gethash "__accessor__" v)))

(defun %js-object-put-entry (ht k v)
  "Store K -> V in object HT.  A get/set accessor descriptor is routed to the
internal __get_K / __set_K slot (so a getter and setter on the same key both
survive and are dispatched by %js-get-prop / %js-set-prop); anything else is a
plain own property."
  (if (%js-accessor-descriptor-p v)
      (let ((kind (gethash "kind" v))
            (fn   (gethash "fn"   v)))
        (setf (gethash (concatenate 'string (if (equal kind "set") "__set_" "__get_") k) ht)
              fn))
      (setf (gethash k ht) v))
  v)

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
       (let ((getter (gethash (concatenate 'string "__get_" k) obj)))
         (if getter
             ;; Accessor property (get v(){...} / Object.defineProperty getter):
             ;; invoke the getter with `this' = OBJ and return its result.
             (%js-call-with-this obj getter nil)
             (multiple-value-bind (val found) (gethash k obj)
               (cond
                 ;; An own callable property is a method: return it bound to OBJ so
                 ;; `this' inside an object-literal method (let o={m(){return this.x}})
                 ;; resolves to the receiver, matching o.m() semantics.
                 ((and found (funcall *js-callable-p* val))
                  (let ((method val) (receiver obj))
                    (lambda (&rest args) (%js-call-with-this receiver method args))))
                 (found val)
                 (t (%js-proto-method-lookup obj k)))))))
      ((eq obj +js-null+) (error "JS TypeError: Cannot read properties of null"))
      ((eq obj +js-undefined+) (error "JS TypeError: Cannot read properties of undefined"))
      ;; All remaining types (Number/Symbol/Promise/Map/WeakMap/WeakSet/etc.)
      ;; delegate to the method resolver.  Struct types like js-promise, js-map,
      ;; js-weak-map returned +js-undefined+ before this fix — so promise.then /
      ;; map.get / weakmap.set all failed at the call site.
      (t (%js-method-ref obj k)))))

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
       (cond
         ;; Defining an accessor (object-literal get/set, or assignment of an
         ;; accessor descriptor): route the fn to __get_K/__set_K so a getter and
         ;; setter on the same key coexist instead of overwriting each other.
         ((%js-accessor-descriptor-p value)
          (%js-object-put-entry obj k value))
         ;; Plain write to a key that has a setter (own, or inherited from the
         ;; class prototype): invoke it with `this' = OBJ.
         (t
          (let ((setter (or (gethash (concatenate 'string "__set_" k) obj)
                            (%js-proto-accessor-lookup obj (concatenate 'string "__set_" k)))))
            (if setter
                (%js-call-with-this obj setter (list value))
                (setf (gethash k obj) value))))))
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

(defun %js-optional-method-call (obj key &rest args)
  "obj?.method(args) — short-circuit to undefined when OBJ is null/undefined."
  (if (%js-not-nullish obj)
      (let ((method (%js-get-prop obj key)))
        (apply #'%js-funcall method args))
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

(defun %js-divide (a b)
  "JS `/' operator. JS numbers are IEEE doubles, so division always yields a float
(5/2 => 2.5, 6/2 => 3.0 which prints as `3'); the CL `/' alone would return the
rational 5/2. Division by zero gives +/-Infinity (or NaN for 0/0), per ECMAScript."
  (let ((na (%js-to-number a))
        (nb (%js-to-number b)))
    (cond
      ((or (not (numberp na)) (not (numberp nb))) *js-nan-float*)
      ((zerop nb)
       (cond ((zerop na)  *js-nan-float*)
             ((plusp na)  *js-inf-float*)
             (t           *js-neg-inf-float*)))
      (t (/ (coerce na 'double-float) (coerce nb 'double-float))))))

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
               ;; JS prints an integer-valued float without a decimal: 7.0 -> "7".
               ;; Trim trailing zeros FIRST, then a now-trailing dot. (The reverse
               ;; order left "7." because "7.0" does not end in ".".)
               (string-right-trim "." (string-right-trim "0" s))))))
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
;;;  Control flow / exceptions and iteration protocol
;;; -----------------------------------------------------------------------
;;; js-exception, %js-throw, %js-try-catch-finally, %js-for-in,
;;; %js-advance-iterator, %js-advance-cl-iterator, %js-for-of,
;;; %js-iter-values, and %js-iter-keys are in runtime-control.lisp.

