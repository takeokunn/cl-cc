;;;; packages/javascript/src/runtime-class.lisp — JS Class/OOP, nullish coalesce, misc
;;;;
;;;; Class instantiation, private fields, and miscellaneous JS operators.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Class / OOP
;;; -----------------------------------------------------------------------

(defun %js-make-class (&rest args)
  "Build a JS class object. ARGS = (SUPER CTOR name1 fn1 name2 fn2 …): SUPER is
the parent class object (or nil/undefined), CTOR the constructor fn, and the rest
alternating method-name strings and method fns. Methods live on __prototype__;
%js-new links each instance's __proto__ to it for prototype-chain lookup.
An \"@@static\" marker in the tail separates instance methods from static ones.
Called at runtime via the :metaclass slot of the ast-defclass node emitted by
%js-lower-class-to-ast in parser-class-lower.lisp. NOTE: pure &rest shape (not
`super ctor &rest`) — required args before &rest marshal incorrectly through the
VM host bridge for vm-closure values."
  (let* ((super (first args))
         (ctor  (second args))
         (rest  (cddr args))
         ;; A "@@static" marker (if present) separates instance method pairs from
         ;; static method pairs. Statics are set directly on the class object;
         ;; instance methods go on the prototype.
         (split (position "@@static" rest :test #'equal))
         (instance-pairs (if split (subseq rest 0 split) rest))
         (static-pairs   (if split (subseq rest (1+ split)) nil))
         (super* (and (%js-ht-p super) super))
         (klass  (%js-make-ht))
         (proto  (%js-make-ht)))
    (when super*
      (let ((super-proto (gethash "__prototype__" super*)))
        (when super-proto (setf (gethash "__proto__" proto) super-proto))))
    (loop for (name fn) on instance-pairs by #'cddr
          do (setf (gethash name proto) fn))
    (loop for (name fn) on static-pairs by #'cddr
          do (setf (gethash name klass) fn))
    (setf (gethash "__prototype__" klass)   proto
          (gethash "__constructor__" klass) (and ctor (not (eq ctor +js-undefined+)) ctor)
          (gethash "__super__" klass)        super*)
    klass))

(defun %js-run-constructor (klass obj args)
  "Run KLASS's constructor on OBJ (as `this') with ARGS.
Binds %js-this to OBJ so constructors can use `this.x = ...'.
When KLASS defines no constructor, forward to __super__."
  (let ((ctor (and (%js-ht-p klass) (gethash "__constructor__" klass))))
    (cond
      (ctor
       ;; Bind %js-this to the new instance (host + VM global) so the constructor
       ;; can access it via `this.x = ...'.
       (%js-call-with-this obj ctor args))
      ((and (%js-ht-p klass) (gethash "__super__" klass))
       (%js-run-constructor (gethash "__super__" klass) obj args)))))

(defun %js-make-super-binding (super-class this)
  "Return the value of `super' inside a subclass method/constructor.  It is a JS
object that is BOTH:
- CALLABLE — super(args) runs SUPER-CLASS's constructor on THIS (constructor use);
  invoked via its __call__ by *js-apply-fn*.
- a MEMBER target — super.method() resolves METHOD on SUPER-CLASS's prototype but
  binds `this' to THIS (the real instance), not the super object, via the
  __super_this__ marker that %js-proto-method-lookup honors.
Lexical SUPER-CLASS is captured here (the instance's class would loop on
multi-level inheritance)."
  (let ((super-obj (%js-make-ht)))
    (when (%js-ht-p super-class)
      (let ((super-proto (gethash "__prototype__" super-class)))
        (when super-proto
          (setf (gethash "__proto__" super-obj) super-proto))))
    (setf (gethash "__super_this__" super-obj) this)
    (setf (gethash "__call__" super-obj)
          (lambda (&rest args)
            (when (%js-ht-p super-class)
              (%js-run-constructor super-class this args))
            +js-undefined+))
    super-obj))

(defun %js-new (constructor &optional (args nil))
  "Instantiate a JS class. CONSTRUCTOR is a class object from %js-make-class
(with __prototype__/__constructor__/__super__), a HT carrying a host __new__, or
a plain host function. Builds the instance, links its __proto__ to the class
prototype so method lookup walks the chain, then runs the constructor with
this = the new instance."
  ;; ARGS arrives as a JS array (vector) from the `new X(a,b)' lowering; APPLY
  ;; needs a list, so normalize once.
  (let ((arglist (if (listp args) args (coerce args 'list))))
    (cond
      ((and (%js-ht-p constructor) (gethash "__new__" constructor))
       (apply #'%js-funcall (gethash "__new__" constructor) arglist))
      ((functionp constructor)
       (apply constructor arglist))
      (t
       (let ((obj (%js-make-ht)))
         (when (%js-ht-p constructor)
           (let ((proto (gethash "__prototype__" constructor)))
             (when proto (setf (gethash "__proto__" obj) proto)))
           (setf (gethash "__class__" obj) constructor)
           (%js-run-constructor constructor obj arglist))
         obj)))))

;;; Return the __private__ sub-table for OBJ, or NIL when absent/invalid.
(defun %js-private-ht (obj)
  (let ((privates (and (%js-ht-p obj) (gethash "__private__" obj))))
    (and privates (%js-ht-p privates) privates)))

(defun %js-class-private-field-get (obj field-name)
  "Read a private field from OBJ."
  (let ((privates (%js-private-ht obj)))
    (if privates
        (multiple-value-bind (v f) (gethash field-name privates)
          (if f v +js-undefined+))
        +js-undefined+)))

(defun %js-class-private-field-set (obj field-name value)
  "Write a private field on OBJ."
  (when (%js-ht-p obj)
    (let ((privates (or (%js-private-ht obj)
                        (let ((ht (%js-make-ht)))
                          (setf (gethash "__private__" obj) ht)
                          ht))))
      (setf (gethash field-name privates) value)))
  value)

(defun %js-has-private-field (obj field-name)
  "True if OBJ has the named private field."
  (let ((privates (%js-private-ht obj)))
    (and privates (nth-value 1 (gethash field-name privates)))))

;;; -----------------------------------------------------------------------
;;;  Error class hierarchy (ES2015+)
;;; -----------------------------------------------------------------------
;;;
;;; JS Error instances are hash-tables with __proto__ pointing to the
;;; class's __prototype__ hash-table. This makes instanceof work correctly.

(defun %js-make-error-class (name parent-class)
  "Create an Error class with NAME inheriting from PARENT-CLASS (nil = root base).
Covers both the base Error and all subclasses."
  (let ((klass (%js-make-ht))
        (proto (%js-make-ht)))
    (when (and (%js-ht-p parent-class) (gethash "__prototype__" parent-class))
      (setf (gethash "__proto__" proto) (gethash "__prototype__" parent-class)))
    (setf (gethash "name"    proto) name
          (gethash "message" proto) ""
          (gethash "stack"   proto) ""
          (gethash "toString" proto)
          (lambda (&rest _) (declare (ignore _))
            (let* ((this %js-this)
                   (n (if (%js-ht-p this) (gethash "name"    this name) name))
                   (m (if (%js-ht-p this) (gethash "message" this "")   "")))
              (if (string= m "") n (format nil "~A: ~A" n m)))))
    (setf (gethash "__prototype__"   klass) proto
          (gethash "__constructor__" klass)
          (let ((class-name name))
            (lambda (&rest args)
              (let ((msg (if args (%js-to-string (first args)) "")))
                (when (%js-ht-p %js-this)
                  (setf (gethash "message" %js-this) msg
                        (gethash "name"    %js-this) class-name
                        (gethash "stack"   %js-this) (format nil "~A: ~A" class-name msg))))))
          (gethash "__super__" klass) parent-class
          (gethash "name"      klass) name)
    klass))

(defparameter *js-error-class* (%js-make-error-class "Error" nil)
  "The root JS Error class object.")

(defmacro define-js-error-subclasses (&rest name-var-pairs)
  "Define Error subclasses from (js-name var-base) pairs, all extending *js-error-class*.
Example: (define-js-error-subclasses (\"TypeError\" type-error)  ...)
→ (defparameter *js-type-error-class* (%js-make-error-class \"TypeError\" *js-error-class*))"
  `(progn
     ,@(mapcar (lambda (pair)
                 (let* ((js-name (first pair))
                        (base    (second pair))
                        (var     (intern (format nil "*JS-~A-CLASS*" (string-upcase base))
                                         :cl-cc/javascript)))
                   `(defparameter ,var (%js-make-error-class ,js-name *js-error-class*))))
               name-var-pairs)))

(define-js-error-subclasses
  ("TypeError"      type-error)
  ("RangeError"     range-error)
  ("ReferenceError" reference-error)
  ("SyntaxError"    syntax-error)
  ("EvalError"      eval-error)
  ("URIError"       uri-error))

(defun %js-make-error-instance (class msg)
  "Create an Error instance with the given CLASS and message MSG."
  (%js-new class (list msg)))

;;; -----------------------------------------------------------------------
;;;  Nullish coalesce
;;; -----------------------------------------------------------------------

(defun %js-nullish-coalesce (a b)
  "JS ?? operator."
  (if (%js-not-nullish a) a b))

;;; -----------------------------------------------------------------------
;;;  Misc
;;; -----------------------------------------------------------------------

(defun %js-void (x)
  "JS void operator."
  (declare (ignore x))
  +js-undefined+)

(defun %js-debugger ()
  "JS debugger statement — no-op."
  +js-undefined+)

(defun %js-import (module-name &optional with-opts)
  "Dynamic import (stub — returns empty namespace object)."
  (declare (ignore with-opts))
  (%js-promise-resolve
   (%js-make-object "default" +js-undefined+ "__moduleName__" module-name)))

(defun %js-export (kind value)
  "Mark a value as exported (stub — returns value)."
  (declare (ignore kind))
  value)

(defun %js-spread (iterable)
  "Expand iterable into a CL list (for use with apply)."
  (%js-iter-values iterable))
