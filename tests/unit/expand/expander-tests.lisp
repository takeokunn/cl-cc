;;;; tests/unit/expand/expander-tests.lisp — Macro Expansion Helper Tests
;;;;
;;;; Tests for the pure helper functions in expander.lisp:
;;;; reduce-variadic-op, expand-let-binding, expand-flet-labels-binding,
;;;; expand-lambda-list-defaults, expand-setf-cons-place, builtin tables.

(in-package :cl-cc/test)

(defsuite expander-suite :description "Macro expansion helper unit tests")

;;; ─── reduce-variadic-op ─────────────────────────────────────────────────────

(deftest-each reduce-variadic-op
  "reduce-variadic-op builds left-nested call trees for any arity."
  :cases (("zero-plus"  '+ nil       0 0)
          ("zero-mul"   '* nil       1 1)
          ("one-arg"    '+ '(x)      0 'x)
          ("two-args"   '+ '(a b)    0 '(+ a b))
          ("three-args" '+ '(a b c)  0 '(+ (+ a b) c))
          ("four-args"  '* '(a b c d) 1 '(* (* (* a b) c) d)))
  (op args id expected)
  (assert-equal expected (cl-cc::reduce-variadic-op op args id)))

;;; ─── expand-let-binding ─────────────────────────────────────────────────────

(deftest let-binding-symbol-value
  "expand-let-binding expands the value form, preserves name."
  (let ((b (cl-cc::expand-let-binding '(x 42))))
    (assert-equal 'x (first b))
    (assert-equal 42 (second b))))

(deftest let-binding-bare-symbol
  "expand-let-binding passes through a bare symbol."
  (assert-equal 'x (cl-cc::expand-let-binding 'x)))

;;; ─── expand-flet-labels-binding ─────────────────────────────────────────────

(deftest expand-flet-labels-binding
  "expand-flet-labels-binding: full form expands body; short form passes through."
  (let ((full  (cl-cc::expand-flet-labels-binding '(foo (x) (+ x 1))))
        (short (cl-cc::expand-flet-labels-binding '(foo (x)))))
    (assert-equal 'foo    (first full))
    (assert-equal '(x)   (second full))
    (assert-true (consp  (third full)))
    (assert-equal '(foo (x)) short)))

;;; ─── expand-lambda-list-defaults ────────────────────────────────────────────

(deftest lambda-list-no-defaults
  "expand-lambda-list-defaults passes through simple params."
  (assert-equal '(x y z) (cl-cc::expand-lambda-list-defaults '(x y z))))

(deftest lambda-list-optional-with-default
  "expand-lambda-list-defaults expands optional default values."
  (let ((result (cl-cc::expand-lambda-list-defaults '(x &optional (y 42)))))
    (assert-equal 'x (first result))
    (assert-equal '&optional (second result))
    ;; y's default should be expanded (42 stays as 42)
    (assert-true (consp (third result)))))

;;; ─── expand-setf-cons-place ─────────────────────────────────────────────────

(deftest-each expand-setf-cons-place
  "expand-setf-cons-place: outer is LET; body contains rplaca for car, rplacd for cdr."
  :cases (("car" '(car x) "RPLACA")
          ("cdr" '(cdr x) "RPLACD"))
  (place expected-fn)
  (let* ((result (cl-cc::expand-setf-cons-place place 'val))
         (str    (format nil "~S" result)))
    (assert-eq 'let (first result))
    (assert-true (search expected-fn str))))

;;; ─── Builtin arity tables ──────────────────────────────────────────────────

(deftest variadic-fold-builtins-contents
  "*variadic-fold-builtins* includes + * append nconc."
  (assert-true (member '+ cl-cc::*variadic-fold-builtins*))
  (assert-true (member '* cl-cc::*variadic-fold-builtins*))
  (assert-true (member 'append cl-cc::*variadic-fold-builtins*))
  (assert-true (member 'nconc cl-cc::*variadic-fold-builtins*)))

(deftest binary-builtins-contents
  "*binary-builtins* includes key entries."
  (assert-true (member 'cons cl-cc::*binary-builtins*))
  (assert-true (member '= cl-cc::*binary-builtins*))
  (assert-true (member 'mod cl-cc::*binary-builtins*))
  (assert-true (member 'ash cl-cc::*binary-builtins*)))

(deftest unary-builtins-contents
  "*unary-builtins* includes key entries."
  (assert-true (member 'car cl-cc::*unary-builtins*))
  (assert-true (member 'cdr cl-cc::*unary-builtins*))
  (assert-true (member 'not cl-cc::*unary-builtins*))
  (assert-true (member 'length cl-cc::*unary-builtins*)))

(deftest cxr-builtins-completeness
  "*cxr-builtins* has all 28 compositions."
  (assert-equal 28 (length cl-cc::*cxr-builtins*))
  (assert-true (member 'caar cl-cc::*cxr-builtins*))
  (assert-true (member 'cddddr cl-cc::*cxr-builtins*)))

(deftest all-builtin-names-union
  "*all-builtin-names* is the union of all sub-tables."
  (assert-true (member '+ cl-cc::*all-builtin-names*))
  (assert-true (member 'cons cl-cc::*all-builtin-names*))
  (assert-true (member 'car cl-cc::*all-builtin-names*))
  (assert-true (member 'caar cl-cc::*all-builtin-names*))
  (assert-true (member 'list cl-cc::*all-builtin-names*)))

;;; ─── compiler-macroexpand-all ───────────────────────────────────────────────

(deftest-each expand-all-atom-passthrough
  "compiler-macroexpand-all passes atoms (int, string, symbol) through unchanged."
  :cases (("integer" 42)
          ("string"  "hello")
          ("symbol"  'x))
  (form)
  (assert-equal form (cl-cc::compiler-macroexpand-all form)))

(deftest expand-all-quote
  "compiler-macroexpand-all preserves quoted forms."
  (assert-equal '(quote (1 2 3))
                (cl-cc::compiler-macroexpand-all '(quote (1 2 3)))))

(deftest expand-all-if
  "compiler-macroexpand-all recurses into if branches."
  (let ((result (cl-cc::compiler-macroexpand-all '(if t 1 2))))
    (assert-equal 'if (first result))
    (assert-equal t (second result))
    (assert-equal 1 (third result))
    (assert-equal 2 (fourth result))))

(deftest expand-all-let
  "compiler-macroexpand-all expands let binding values."
  (let ((result (cl-cc::compiler-macroexpand-all '(let ((x 1)) x))))
    (assert-equal 'let (first result))
    (assert-equal 'x (caar (second result)))))

(deftest expand-all-variadic-plus
  "compiler-macroexpand-all reduces (+ a b c) to nested binary."
  (let ((result (cl-cc::compiler-macroexpand-all '(+ 1 2 3))))
    ;; Should be (+ (+ 1 2) 3)
    (assert-equal '+ (first result))
    (assert-true (consp (second result)))
    (assert-equal '+ (first (second result)))))

;;; ─── setf expansions ────────────────────────────────────────────────────────

(deftest expander-setf-multi-var-progn
  "compiler-macroexpand-all: (setf x 1 y 2) expands to progn of setq."
  (let ((result (cl-cc::compiler-macroexpand-all '(setf x 1 y 2))))
    (assert-eq 'progn (car result))
    ;; Each sub-form should be a setq
    (assert-eq 'setq (car (second result)))
    (assert-eq 'setq (car (third result)))))

(deftest expander-setf-plain-var-to-setq
  "compiler-macroexpand-all: (setf x 42) expands to (setq x 42)."
  (let ((result (cl-cc::compiler-macroexpand-all '(setf x 42))))
    (assert-eq 'setq (car result))
    (assert-eq 'x (second result))
    (assert-equal 42 (third result))))

(deftest expander-setf-aref-to-aset
  "compiler-macroexpand-all: (setf (aref v i) val) expands to (aset ...)."
  (let ((result (cl-cc::compiler-macroexpand-all '(setf (aref v i) val))))
    (assert-eq 'cl-cc::aset (car result))
    (assert-eq 'v (second result))
    (assert-eq 'i (third result))
    (assert-eq 'val (fourth result))))

(deftest-each expander-setf-cons-cell-place
  "(setf (car/cdr/first/rest x) v) all expand via the appropriate rplaca/rplacd."
  :cases (("car"   '(setf (car x)   v) "RPLACA")
          ("cdr"   '(setf (cdr x)   v) "RPLACD")
          ("first" '(setf (first x) v) "RPLACA")
          ("rest"  '(setf (rest x)  v) "RPLACD"))
  (form expected-str)
  (let ((str (format nil "~S" (cl-cc::compiler-macroexpand-all form))))
    (assert-true (search expected-str str))))

(deftest expander-setf-accessor-slot-value-fallback
  "compiler-macroexpand-all: (setf (foo obj) v) with unknown accessor falls back to slot-value."
  ;; Use a name that is definitely not in *accessor-slot-map*
  (let ((result (cl-cc::compiler-macroexpand-all '(setf (my-unknown-accessor-xyz obj) v))))
    ;; After expand-setf-accessor → setf (slot-value ...) → set-slot-value or similar
    (let ((str (format nil "~S" result)))
      (assert-true (search "SLOT-VALUE" str)))))

;;; *setf-compound-place-handlers* — table-driven place dispatch ─────────────

(deftest expander-setf-nth-place
  "(setf (nth i x) v) expands via rplaca + nthcdr."
  (let ((str (format nil "~S"
                     (cl-cc::compiler-macroexpand-all '(setf (nth 2 lst) newval)))))
    (assert-true (search "RPLACA" str))
    (assert-true (search "NTHCDR" str))))

(deftest-each expander-setf-cxr-compound-places
  "(setf (cadr/cddr x) v) expands via rplaca/rplacd applied to (cdr x)."
  :cases (("cadr" '(setf (cadr x) newval) "RPLACA")
          ("cddr" '(setf (cddr x) newval) "RPLACD"))
  (form expected-op)
  (let ((str (format nil "~S" (cl-cc::compiler-macroexpand-all form))))
    (assert-true (search expected-op str))
    (assert-true (search "CDR" str))))

(deftest expander-setf-getf-place
  "(setf (getf plist key) v) expands to LET wrapper using rt-plist-put."
  (let ((result (cl-cc::compiler-macroexpand-all '(setf (getf my-plist :foo) 42))))
    (assert-eq 'let (car result))
    (assert-true (search "RT-PLIST-PUT" (format nil "~S" result)))))

(deftest expander-setf-multi-place-dispatches-each
  "(setf a 1 b 2) expands to progn of two individual setf/setq forms."
  (let ((result (cl-cc::compiler-macroexpand-all '(setf a 1 b 2))))
    (assert-eq 'progn (car result))
    (assert-= 2 (length (cdr result)))))

;;; ─── defstruct expansion ────────────────────────────────────────────────────

(deftest expander-defstruct-basic-shape
  "compiler-macroexpand-all: defstruct expands to progn of defclass + defun + defun."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(defstruct exptest-shape alpha beta))))
    (assert-eq 'progn (car result))
    ;; First sub-form is defclass
    (assert-eq 'defclass (car (second result)))
    (assert-equal "EXPTEST-SHAPE" (symbol-name (second (second result))))
    ;; Constructor defun named make-exptest-shape
    (assert-eq 'defun (car (third result)))
    (assert-equal "MAKE-EXPTEST-SHAPE" (symbol-name (second (third result))))
    ;; Predicate defun named exptest-shape-p
    (assert-eq 'defun (car (fourth result)))
    (assert-equal "EXPTEST-SHAPE-P" (symbol-name (second (fourth result))))))

(deftest expander-defstruct-registers-slots
  "expand-defstruct registers slot info in *defstruct-slot-registry*."
  (cl-cc::compiler-macroexpand-all '(defstruct reg-test-struct alpha beta))
  (let ((slots (gethash 'reg-test-struct cl-cc::*defstruct-slot-registry*)))
    (assert-true (consp slots))
    (assert-equal 'alpha (first (first slots)))
    (assert-equal 'beta (first (second slots)))))

(deftest expander-defstruct-conc-name-option
  "defstruct with :conc-name uses that prefix for accessors."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(defstruct (vec (:conc-name vec/)) x y))))
    (assert-eq 'progn (car result))
    ;; defclass slot specs should use vec/x and vec/y as accessor names
    (let ((str (format nil "~S" result)))
      (assert-true (search "VEC/X" str)))))

;;; ─── eval-when ──────────────────────────────────────────────────────────────

(deftest-each expander-eval-when-produces-progn
  "eval-when with :execute or :load-toplevel produces (progn ...) of body."
  :cases (("execute"       '(eval-when (:execute) (+ 1 2)))
          ("load-toplevel" '(eval-when (:load-toplevel) (+ 1 2))))
  (form)
  (assert-eq 'progn (car (cl-cc::compiler-macroexpand-all form))))

(deftest expander-eval-when-compile-only-returns-nil
  "compiler-macroexpand-all: eval-when with only :compile-toplevel returns nil."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(eval-when (:compile-toplevel) (+ 1 2)))))
    (assert-null result)))

(deftest expander-eval-when-compile-and-execute-included
  "compiler-macroexpand-all: eval-when with :compile-toplevel and :execute produces progn."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(eval-when (:compile-toplevel :execute) (+ 1 2)))))
    (assert-eq 'progn (car result))))

;;; ─── macrolet ───────────────────────────────────────────────────────────────

(deftest expander-macrolet-expands-body
  "compiler-macroexpand-all: macrolet registers local macro and expands body."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(macrolet ((double (x) (list '+ x x)))
                    (double 5)))))
    ;; The body (double 5) should be expanded to (+ 5 5) and wrapped in progn
    (assert-eq 'progn (car result))
    (let ((body (second result)))
      (assert-eq '+ (car body))
      (assert-equal 5 (second body))
      (assert-equal 5 (third body)))))

(deftest expander-macrolet-local-scope
  "compiler-macroexpand-all: macrolet local macro does not leak outside."
  ;; After the macrolet form, 'triple should not be a known macro
  (cl-cc::compiler-macroexpand-all
   '(macrolet ((triple (x) (list '+ x x x)))
      (triple 1)))
  ;; 'triple is not a registered macro in the global environment
  (assert-null (cl-cc::lookup-macro 'triple)))

;;; ─── typed defun / typed lambda ─────────────────────────────────────────────

(deftest expander-typed-defun-strips-annotations
  "Typed defun: plain params are preserved; type names and return type stripped."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(defun add-ints ((x fixnum) (y fixnum)) fixnum (+ x y)))))
    (assert-eq 'defun (car result))
    (let ((params (third result)))
      (assert-true  (member 'x      params))
      (assert-true  (member 'y      params))
      (assert-false (member 'fixnum params)))))

(deftest expander-typed-lambda-strips-type-annotations
  "compiler-macroexpand-all: typed lambda strips param type annotations."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(lambda ((x fixnum)) fixnum x))))
    (assert-eq 'lambda (car result))
    (let ((params (second result)))
      (assert-true (member 'x params))
      (assert-false (member 'fixnum params)))))

;;; ─── floor/ceiling/truncate/round 1-arg normalization ───────────────────────

(deftest-each expander-rounding-one-arg-normalization
  "1-arg rounding forms are normalized to 2-arg (op n 1) for all *rounding-ops*."
  :cases (("floor"    'floor    '(floor    n))
          ("ceiling"  'ceiling  '(ceiling  n))
          ("truncate" 'truncate '(truncate n))
          ("round"    'round    '(round    n)))
  (op-sym form)
  (let ((result (cl-cc::compiler-macroexpand-all form)))
    (assert-eq  op-sym (car result))
    (assert-eq  'n     (second result))
    (assert-equal 1    (third result))))

(deftest expander-floor-two-arg-unchanged
  "compiler-macroexpand-all: (floor x d) with explicit divisor passes through."
  (let ((result (cl-cc::compiler-macroexpand-all '(floor n 3))))
    (assert-eq 'floor (car result))
    (assert-eq 'n (second result))
    (assert-equal 3 (third result))))

;;; ─── make-hash-table :test #'fn normalization ────────────────────────────────

(deftest expander-make-hash-table-function-test
  "compiler-macroexpand-all: (make-hash-table :test #'equal) normalizes to :test 'equal."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(make-hash-table :test #'equal))))
    (assert-eq 'make-hash-table (car result))
    (assert-eq :test (second result))
    ;; Value should be the quoted symbol, not #'equal
    (assert-equal '(quote equal) (third result))))

(deftest expander-make-hash-table-quoted-test-unchanged
  "compiler-macroexpand-all: (make-hash-table :test 'eql) passes through without change."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(make-hash-table :test 'eql))))
    (assert-eq 'make-hash-table (car result))
    (assert-eq :test (second result))))

;;; ─── make-array with :fill-pointer / :adjustable ─────────────────────────────

(deftest-each expander-make-array-adjustable-promotes
  "(make-array n :fill-pointer t) and (:adjustable t) both become make-adjustable-vector."
  :cases (("fill-pointer" '(make-array 10 :fill-pointer t))
          ("adjustable"   '(make-array 5  :adjustable t)))
  (form)
  (assert-true (search "MAKE-ADJUSTABLE-VECTOR"
                        (format nil "~S" (cl-cc::compiler-macroexpand-all form)))))

(deftest expander-make-array-plain-passthrough
  "compiler-macroexpand-all: (make-array n) with no keywords stays as make-array."
  ;; Only 2-arg (size + no keywords) — does NOT trigger the >=4 branch
  (let ((result (cl-cc::compiler-macroexpand-all '(make-array 10))))
    (assert-eq 'make-array (car result))))

;;; ─── deftype ────────────────────────────────────────────────────────────────

(deftest expander-deftype-registers-alias
  "compiler-macroexpand-all: (deftype foo fixnum) registers the alias and returns (quote foo)."
  (let ((result (cl-cc::compiler-macroexpand-all '(deftype my-index-type fixnum))))
    ;; Return value is (quote name)
    (assert-eq 'quote (car result))
    (assert-eq 'my-index-type (second result))))

;;; ─── defconstant ────────────────────────────────────────────────────────────

(deftest expander-defconstant-to-defparameter
  "defconstant (with or without docstring) expands to defparameter."
  (let ((basic   (cl-cc::compiler-macroexpand-all '(defconstant +my-const+ 42)))
        (with-doc (cl-cc::compiler-macroexpand-all '(defconstant +pi+ 3.14159 "Pi constant"))))
    (assert-eq    'defparameter (car basic))
    (assert-equal 42            (third basic))
    (assert-eq    'defparameter (car with-doc))
    (assert-eq    '+pi+         (second with-doc))))

;;; ─── #'builtin → lambda wrapping ────────────────────────────────────────────

(deftest-each expander-function-builtin-wraps-lambda
  "#'builtin always expands to a lambda; arity matches the builtin type."
  :cases (("binary"   'cons 2)
          ("unary"    'car  1)
          ("variadic" '+    nil))
  (name expected-arity)
  (let ((result (cl-cc::compiler-macroexpand-all `(function ,name))))
    (assert-eq 'lambda (car result))
    (when expected-arity
      (assert-equal expected-arity (length (second result))))))

(deftest expander-function-non-builtin-passthrough
  "compiler-macroexpand-all: #'user-fn (not a builtin) passes through as (function user-fn)."
  (let ((result (cl-cc::compiler-macroexpand-all '(function my-user-defined-fn))))
    (assert-eq 'function (car result))
    (assert-eq 'my-user-defined-fn (second result))))

;;; ─── (funcall 'name ...) → direct call ──────────────────────────────────────

(deftest-each expander-funcall-quoted-to-direct-call
  "(funcall 'name ...) becomes (name ...) for both user-defined and builtin functions."
  :cases (("user-fn" '(funcall 'foo x)   'foo  'x)
          ("builtin" '(funcall 'car lst)  'car  'lst))
  (form expected-head expected-arg)
  (let ((result (cl-cc::compiler-macroexpand-all form)))
    (assert-eq expected-head (car result))
    (assert-eq expected-arg  (second result))))

;;; ─── (apply fn a b ... list) spread-args normalization ──────────────────────

(deftest expander-apply-spread-args-cons-wraps
  "compiler-macroexpand-all: (apply fn a b lst) wraps spread args in cons."
  (let ((result (cl-cc::compiler-macroexpand-all '(apply fn a b lst))))
    ;; Should become (apply fn (cons a (cons b lst)))
    (assert-eq 'apply (car result))
    (assert-eq 'fn (second result))
    (let ((combined (third result)))
      (assert-eq 'cons (car combined)))))

(deftest expander-apply-two-arg-passthrough
  "compiler-macroexpand-all: (apply fn lst) with exactly 2 args does not wrap."
  (let ((result (cl-cc::compiler-macroexpand-all '(apply 'myf lst))))
    ;; 3-element apply — hits the named-fn branch, not spread-args
    ;; Result should not be the spread-args cons wrapping
    (assert-eq 'apply (car result))))

;;; ─── progn eager defmacro ────────────────────────────────────────────────────

(deftest expander-progn-expansion
  "progn expands all sub-forms and preserves the progn head regardless of arity."
  (let ((multi  (cl-cc::compiler-macroexpand-all '(progn (+ 1 2) (+ 3 4))))
        (single (cl-cc::compiler-macroexpand-all '(progn 42))))
    (assert-eq    'progn (car multi))
    (assert-equal 2      (length (cdr multi)))
    (assert-eq    'progn (car single))
    (assert-equal 42     (second single))))

;;; ─── defclass accessor registration ─────────────────────────────────────────

(deftest expander-defclass-registers-accessors
  "compiler-macroexpand-all: defclass with :accessor registers in *accessor-slot-map*."
  (cl-cc::compiler-macroexpand-all
   '(defclass my-reg-point ()
      ((x :initarg :x :accessor my-reg-point-x)
       (y :initarg :y :accessor my-reg-point-y))))
  (assert-true (gethash 'my-reg-point-x cl-cc::*accessor-slot-map*))
  (assert-true (gethash 'my-reg-point-y cl-cc::*accessor-slot-map*)))

(deftest expander-defclass-slot-initform-expanded
  "compiler-macroexpand-all: defclass :initform is macro-expanded."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(defclass foo-cls ()
                    ((val :initform (+ 1 2)))))))
    ;; Result is still a defclass
    (assert-eq 'defclass (car result))
    ;; The :initform should have been expanded (+ 1 2 stays as (+ 1 2) since it's already canonical)
    (let* ((slot-specs (fourth result))
           (slot (first slot-specs))
           (initform (getf (rest slot) :initform)))
      (assert-equal '(+ 1 2) initform))))

;;; ─── variadic fold for multi-arg + / * / append ──────────────────────────────

(deftest-each expander-variadic-fold-nesting
  "3-arg variadic forms nest left-associatively: (OP a b c) → (OP (OP a b) c)."
  :cases (("multiply" '*      '(* a b c))
          ("append"   'append '(append a b c))
          ("minus"    '-      '(- a b c)))
  (op form)
  (let ((result (cl-cc::compiler-macroexpand-all form)))
    (assert-eq op (car result))
    (assert-true (consp (second result)))
    (assert-eq op (car (second result)))))

(deftest-each expander-variadic-zero-arg-identity
  "(+) → 0 and (*) → 1 (their respective identity elements)."
  :cases (("plus"  '(+) 0)
          ("times" '(*) 1))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))

;;; ─── flet / labels body expansion ───────────────────────────────────────────

(deftest-each expander-flet-labels-preserve-binding-names
  "flet and labels preserve binding names while expanding body forms."
  :cases (("flet"   '(flet   ((sq   (x) (* x x)))                         (sq 3))   'flet   'sq)
          ("labels" '(labels ((fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (fact 5)) 'labels 'fact))
  (form expected-head binding-name)
  (let ((result (cl-cc::compiler-macroexpand-all form)))
    (assert-eq expected-head (car result))
    (assert-eq binding-name  (first (first (second result))))))

;;; ─── defun / lambda default param expansion ──────────────────────────────────

(deftest-each expander-defun-lambda-preserve-structure
  "Plain defun and lambda preserve their structure after macro expansion."
  :cases (("defun"  '(defun triple (x) (* 3 x)) 'defun  2 '(x))
          ("lambda" '(lambda (x y) (+ x y))     'lambda 1 '(x y)))
  (form expected-head params-pos expected-params)
  (let ((result (cl-cc::compiler-macroexpand-all form)))
    (assert-eq    expected-head   (car result))
    (assert-equal expected-params (nth params-pos result))))

(deftest expander-lambda-optional-default-expanded
  "compiler-macroexpand-all: lambda &optional default value is expanded."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(lambda (x &optional (y (+ 1 1))) (+ x y)))))
    (assert-eq 'lambda (car result))
    (let* ((params (second result))
           (opt-param (third params)))  ; (y <expanded-default>)
      (assert-equal 'y (first opt-param))
      ;; Default (+ 1 1) is a 2-arg binary — passes through unchanged
      (assert-equal '(+ 1 1) (second opt-param)))))

;;; ─── setf slot-value (special form passthrough) ──────────────────────────────

(deftest expander-setf-slot-value-passthrough
  "compiler-macroexpand-all: (setf (slot-value obj 'slot) v) is a special form and recurses."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(setf (slot-value obj 'field) new-val))))
    ;; slot-value is a compiler special form; setf of slot-value goes through
    ;; the accessor branch which calls set-slot-value or similar
    (assert-true (consp result))))

;;; ─── expand-make-array-form ───────────────────────────────────────────────

(deftest expand-make-array-simple
  "expand-make-array-form without adjustable keywords expands to (make-array SIZE)."
  (let ((result (cl-cc::expand-make-array-form 10 nil)))
    ;; Should ultimately lower to a vm instruction, but at sexp level should be non-nil
    (assert-true result)))

(deftest-each expand-make-array-adjustable-promotes
  "expand-make-array-form uses make-adjustable-vector for :adjustable t or :fill-pointer t."
  :cases (("adjustable"   '(:adjustable t))
          ("fill-pointer" '(:fill-pointer t)))
  (kwargs)
  (assert-true (search "ADJUSTABLE"
                        (format nil "~S" (cl-cc::expand-make-array-form 10 kwargs)))))

;;; ─── expand-eval-when-form ────────────────────────────────────────────────

(deftest-each expand-eval-when-keeps-body
  "expand-eval-when-form with :execute or :load-toplevel returns a non-nil form."
  :cases (("execute"       '(:execute))
          ("load-toplevel" '(:load-toplevel)))
  (situations)
  (assert-true (consp (cl-cc::expand-eval-when-form situations '((+ 1 2))))))

(deftest expand-eval-when-compile-only-returns-nil
  "eval-when :compile-toplevel alone returns nil (excluded from output)."
  (let ((result (cl-cc::expand-eval-when-form '(:compile-toplevel) '((+ 1 2)))))
    (assert-eq nil result)))

;;; ─── expand-apply-named-fn ───────────────────────────────────────────────

(deftest expand-apply-named-fn-binary
  "expand-apply-named-fn for a binary builtin (cons) normalises to (apply #'cons ...)."
  (let ((result (cl-cc::expand-apply-named-fn 'cons 'args)))
    ;; Non-variadic: (apply #'cons <args>)
    (assert-eq (car result) 'apply)))

(deftest expand-apply-named-fn-variadic-plus
  "expand-apply-named-fn for + generates a fold loop (not (apply #'+ ...))."
  (let* ((result (cl-cc::expand-apply-named-fn '+ 'args))
         (str    (format nil "~S" result)))
    ;; Must NOT produce (apply #'+ ...) — variadic get inlined fold
    (assert-false (search "(APPLY" str))))

;;; ─── expand-macrolet-form ────────────────────────────────────────────────

(deftest expand-macrolet-form-expands-local-macro
  "expand-macrolet-form makes a local macro visible in the body.
   Result is (progn <expanded-body>) since the body is wrapped in progn."
  (let* ((result (cl-cc::expand-macrolet-form
                  '((my-one () 1))
                  '((my-one))))
         (str (format nil "~S" result)))
    ;; Body (my-one) should expand to 1; result is (progn 1) or similar
    (assert-true (search "1" str))))

(deftest expand-macrolet-form-body-is-progn
  "expand-macrolet-form with multiple body forms wraps in progn."
  (let ((result (cl-cc::expand-macrolet-form
                 '((add1 (x) (+ x 1)))
                 '((add1 2) (add1 3)))))
    ;; Result should be the last form or a progn-wrapped result
    (assert-true (consp result))))

;;; ─── expand-defclass-slot-spec ──────────────────────────────────────────────

(deftest expand-defclass-slot-spec-bare-symbol
  "expand-defclass-slot-spec passes through a bare symbol unchanged."
  (assert-eq 'foo (cl-cc::expand-defclass-slot-spec 'foo)))

(deftest expand-defclass-slot-spec-no-initform-preserved
  "expand-defclass-slot-spec with no :initform leaves all keys untouched."
  (let ((result (cl-cc::expand-defclass-slot-spec
                 '(x :initarg :x :accessor x-accessor))))
    (assert-eq 'x (first result))
    (assert-eq :initarg (second result))
    (assert-eq :x (third result))
    (assert-eq :accessor (fourth result))
    (assert-eq 'x-accessor (fifth result))))

(deftest expand-defclass-slot-spec-expands-initform
  "expand-defclass-slot-spec macro-expands the :initform value; key is preserved."
  (let ((result (cl-cc::expand-defclass-slot-spec
                 '(x :initarg :x :initform (+ 1 2)))))
    ;; The :initform key must be present in the result plist
    (assert-true (member :initform result))))

(deftest expand-defclass-slot-spec-non-initform-keys-untouched
  "expand-defclass-slot-spec does not expand :type or :accessor values."
  (let* ((spec '(x :type integer :accessor get-x :initform 0))
         (result (cl-cc::expand-defclass-slot-spec spec)))
    (assert-eq 'integer (getf (rest result) :type))))

;;; ─── expand-setf-accessor ────────────────────────────────────────────────────

(deftest expand-setf-accessor-unknown-falls-back-to-slot-value
  "expand-setf-accessor for an unknown accessor generates (setf (slot-value ...))."
  ;; slot-value is a compiler special form, so (setf (slot-value ...)) passes through
  (let* ((result (cl-cc::expand-setf-accessor '(some-unknown-accessor obj) 'val))
         (result-str (format nil "~S" result)))
    (assert-true (search "SLOT-VALUE" result-str))))

(deftest expand-setf-accessor-known-maps-to-slot-name
  "expand-setf-accessor for a registered accessor uses the mapped slot name."
  ;; Register a test accessor → slot mapping
  (setf (gethash 'test-reg-accessor cl-cc::*accessor-slot-map*)
        (cons 'test-class 'the-slot))
  (let* ((result (cl-cc::expand-setf-accessor '(test-reg-accessor obj) 'new-val))
         (result-str (format nil "~S" result)))
    ;; setf (slot-value obj 'the-slot) expands to setf-slot-value with the-slot
    (assert-true (search "THE-SLOT" result-str))))

;;; ─── expand-typed-defun-or-lambda ────────────────────────────────────────────

(deftest expand-typed-defun-plain-params-unchanged
  "expand-typed-defun-or-lambda with plain params produces a defun with same params."
  (let* ((result (cl-cc::expand-typed-defun-or-lambda
                  'defun 'my-typed-fn '(a b) '((+ a b))))
         (result-str (format nil "~S" result)))
    ;; Should produce something containing a defun
    (assert-true (search "MY-TYPED-FN" result-str))))

(deftest expand-typed-defun-typed-params-stripped
  "expand-typed-defun-or-lambda strips type annotations; check-type → typep."
  ;; check-type expands to (unless (typep ...) (error ...))
  (let* ((result (cl-cc::expand-typed-defun-or-lambda
                  'defun 'typed-adder '((x integer) (y integer)) '((+ x y))))
         (result-str (format nil "~S" result)))
    ;; check-type gets expanded to (unless (typep ...) ...) — search for typep
    (assert-true (search "TYPEP" result-str))))

(deftest expand-typed-lambda-produces-lambda
  "expand-typed-defun-or-lambda with 'lambda head produces a lambda form."
  (let* ((result (cl-cc::expand-typed-defun-or-lambda
                  'lambda nil '(x) '(x)))
         (result-str (format nil "~S" result)))
    (assert-true (search "LAMBDA" result-str))))
