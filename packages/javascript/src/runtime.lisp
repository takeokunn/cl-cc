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

(defun %js-vm-closure-p (x)
  "Package-safe predicate for a compiled-JS closure (a cl-cc/vm:vm-closure-object).
The VM package is NOT an ASDF compile-time dependency of the JS frontend (closures
are invoked through the runtime *js-apply-fn* pointer), so the vm-closure-object
type symbol cannot be named at read time — resolve it by name at runtime."
  (let ((pkg (find-package "CL-CC/VM")))
    (and pkg
         (let ((sym (find-symbol "VM-CLOSURE-OBJECT" pkg)))
           (and sym (find-class sym nil) (typep x sym))))))

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
    ;; Compiled JS functions are vm-closure-objects (not CL functions and not
    ;; hash-tables with __call__), so without this they fell through to the
    ;; default "object" — breaking the ubiquitous `typeof f === "function"`
    ;; feature-detection idiom.
    ((%js-vm-closure-p x)    "function")
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
           ;; %js-nan-p, not (eq x :js-nan): the NaN literal is a float NaN
           ;; (*js-nan-float*), not the keyword sentinel, so a bare (eq x :js-nan)
           ;; missed it and `NaN ? a : b' / `if (NaN)' tested truthy.
           (%js-nan-p x)
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
                         ;; *read-default-float-format* double: "3.14" must read
                         ;; as 3.14d0, not a single-float (which loses precision:
                         ;; 3.14 -> 3.1400001…).
                         (let* ((*read-eval* nil)
                                (*read-default-float-format* 'double-float)
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

(defun %js-relational (a b num-op str-op)
  "JS Abstract Relational Comparison core, returning a JS boolean (t/nil).
When both operands are strings they compare lexicographically by code unit
(STR-OP); otherwise both coerce to number (ToNumber) and compare with NUM-OP,
except that a NaN operand makes every relational comparison false. This is why
JS relational operators must NOT lower to the VM's CL <,>,<=,>= directly: those
return 1/0 and mishandle strings/NaN/coercion."
  (if (and (stringp a) (stringp b))
      (and (funcall str-op a b) t)
      (let ((na (%js-to-number a))
            (nb (%js-to-number b)))
        (if (or (%js-nan-p na) (%js-nan-p nb))
            nil
            (and (funcall num-op na nb) t)))))

(defmacro define-js-relational-op (name num-op str-op doc)
  "Emit a JS relational comparison that delegates to %js-relational."
  `(defun ,name (a b) ,doc (%js-relational a b #',num-op #',str-op)))

(define-js-relational-op %js-lt <  string<  "JS a < b.")
(define-js-relational-op %js-gt >  string>  "JS a > b.")
(define-js-relational-op %js-le <= string<= "JS a <= b.")
(define-js-relational-op %js-ge >= string>= "JS a >= b.")

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

(defvar *js-apply-fn*
  (lambda (fn args)
    ;; A callable JS object (e.g. `super', Intl/Symbol stubs) carries its
    ;; implementation under __call__; otherwise host APPLY.
    (if (and (hash-table-p fn) (gethash "__call__" fn))
        (apply (gethash "__call__" fn) args)
        (apply fn args)))
  "Invoker used to call a JS callback value (e.g. the FN passed to Array.map /
filter / reduce / sort). Defaults to host APPLY (plus __call__ objects) so plain
host lambdas work in unit tests; the pipeline rebinds it to a VM-closure-aware
invoker so a callback that is a compiled-JS closure is dispatched through the VM.")

(defun %js-funcall (fn &rest args)
  "Call JS callback FN with ARGS through the installed *js-apply-fn* invoker.
Array higher-order methods use this instead of CL:FUNCALL so the same code path
works whether FN is a host function (tests) or a compiled-JS closure (runtime)."
  (funcall *js-apply-fn* fn args))

(defvar *js-apply-with-this-fn*
  (lambda (this fn args)
    (let ((%js-this this))
      (funcall *js-apply-fn* fn args)))
  "Invoke a method/constructor FN with `this' = THIS. The default binds only the
host special %js-this (enough for host-function methods in unit tests). The
pipeline installs a version that ALSO sets the VM-global %js-this, because a
compiled-JS method body reads `this' via vm-get-global and cannot see the host
dynamic binding.")

(defun %js-call-with-this (this fn args)
  "Call method/constructor FN with `this' bound to THIS for both host and
compiled-VM method bodies."
  (funcall *js-apply-with-this-fn* this fn args))

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

(defun %js-proto-accessor-lookup (obj accessor-key)
  "Walk OBJ's __proto__ chain for ACCESSOR-KEY (e.g. \"__get_x\" / \"__set_x\")
and return the accessor function, or NIL.  Used for class getters/setters, which
live on the prototype."
  (loop with proto = (gethash "__proto__" obj)
        while (%js-ht-p proto)
        do (multiple-value-bind (val found) (gethash accessor-key proto)
             (when found (return val)))
           (setf proto (gethash "__proto__" proto))
        finally (return nil)))

(defun %js-proto-method-lookup (obj k)
  "Walk OBJ's __proto__ chain for key K (JS prototype method resolution). A
getter (__get_K on the chain) is invoked with `this' = OBJ and its result
returned. A callable found under K is a METHOD: return a closure that binds
%js-this to OBJ and then calls the method. A non-callable inherited value is
returned as-is; a miss yields undefined.
The dynamic binding of %js-this is what makes `this.x' in method bodies work."
  (let ((getter-key (concatenate 'string "__get_" k)))
    (loop with proto = (gethash "__proto__" obj)
          while (%js-ht-p proto)
          do ;; an inherited getter takes precedence and is invoked immediately
             (multiple-value-bind (gfn gfound) (gethash getter-key proto)
               (when gfound
                 (return-from %js-proto-method-lookup (%js-call-with-this obj gfn nil))))
             (multiple-value-bind (val found) (gethash k proto)
               (when found
                 (return-from %js-proto-method-lookup
                   (if (funcall *js-callable-p* val)
                       ;; Bind %js-this dynamically so `this' in the method body
                       ;; resolves to the receiver, then dispatch through %js-funcall
                       ;; for VM-closure compatibility.  For a `super' object
                       ;; (carrying __super_this__) bind to the REAL instance, not the
                       ;; super object, so super.method() sees the correct `this'.
                       (let ((method val)
                             (receiver (multiple-value-bind (st found)
                                           (gethash "__super_this__" obj)
                                         (if found st obj))))
                         (lambda (&rest args)
                           (%js-call-with-this receiver method args)))
                       val))))
             (setf proto (gethash "__proto__" proto))
          finally (return +js-undefined+))))


;;; Property access and string/arithmetic ops
;;; -> see runtime-property.lisp
