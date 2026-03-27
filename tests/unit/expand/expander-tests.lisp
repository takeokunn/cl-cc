;;;; tests/unit/expand/expander-tests.lisp — Macro Expansion Helper Tests
;;;;
;;;; Tests for the pure helper functions in expander.lisp:
;;;; reduce-variadic-op, expand-let-binding, expand-flet-labels-binding,
;;;; expand-lambda-list-defaults, expand-setf-cons-place, builtin tables.

(in-package :cl-cc/test)

(defsuite expander-suite :description "Macro expansion helper unit tests")

;;; ─── reduce-variadic-op ─────────────────────────────────────────────────────

(deftest variadic-zero-args
  "reduce-variadic-op with 0 args returns identity."
  (assert-equal 0 (cl-cc::reduce-variadic-op '+ nil 0))
  (assert-equal 1 (cl-cc::reduce-variadic-op '* nil 1)))

(deftest variadic-one-arg
  "reduce-variadic-op with 1 arg returns the arg itself."
  (assert-equal 'x (cl-cc::reduce-variadic-op '+ '(x) 0)))

(deftest variadic-two-args
  "reduce-variadic-op with 2 args returns (OP a b)."
  (assert-equal '(+ a b) (cl-cc::reduce-variadic-op '+ '(a b) 0)))

(deftest variadic-three-args
  "reduce-variadic-op with 3 args returns (OP (OP a b) c)."
  (assert-equal '(+ (+ a b) c)
                (cl-cc::reduce-variadic-op '+ '(a b c) 0)))

(deftest variadic-four-args
  "reduce-variadic-op with 4 args nests left-associatively."
  (assert-equal '(* (* (* a b) c) d)
                (cl-cc::reduce-variadic-op '* '(a b c d) 1)))

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

(deftest flet-binding-expands-body
  "expand-flet-labels-binding preserves name+params, expands body."
  (let ((b (cl-cc::expand-flet-labels-binding '(foo (x) (+ x 1)))))
    (assert-equal 'foo (first b))
    (assert-equal '(x) (second b))
    ;; Body was processed by compiler-macroexpand-all
    (assert-true (consp (third b)))))

(deftest flet-binding-short-form
  "expand-flet-labels-binding passes through short binding (<3 elements)."
  (let ((b (cl-cc::expand-flet-labels-binding '(foo (x)))))
    (assert-equal '(foo (x)) b)))

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

(deftest setf-car-expands-to-rplaca
  "expand-setf-cons-place for (car x) generates rplaca."
  (let ((result (cl-cc::expand-setf-cons-place '(car x) 'val)))
    (assert-true (consp result))
    ;; Should produce (let ((gensym val)) (rplaca x gensym) gensym)
    (assert-equal 'let (first result))
    ;; The body should contain rplaca
    (let ((body-str (format nil "~S" result)))
      (assert-true (search "RPLACA" body-str)))))

(deftest setf-cdr-expands-to-rplacd
  "expand-setf-cons-place for (cdr x) generates rplacd."
  (let ((result (cl-cc::expand-setf-cons-place '(cdr x) 'val)))
    (let ((body-str (format nil "~S" result)))
      (assert-true (search "RPLACD" body-str)))))

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

(deftest expand-all-integer-literal
  "compiler-macroexpand-all passes through integer literals."
  (assert-equal 42 (cl-cc::compiler-macroexpand-all 42)))

(deftest expand-all-string-literal
  "compiler-macroexpand-all passes through string literals."
  (assert-equal "hello" (cl-cc::compiler-macroexpand-all "hello")))

(deftest expand-all-symbol
  "compiler-macroexpand-all passes through symbols (non-macro)."
  (assert-equal 'x (cl-cc::compiler-macroexpand-all 'x)))

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

(deftest expander-setf-car-place
  "compiler-macroexpand-all: (setf (car x) v) expands via rplaca."
  (let ((result (cl-cc::compiler-macroexpand-all '(setf (car x) v))))
    ;; After expansion the top form is a let wrapping rplaca
    (let ((str (format nil "~S" result)))
      (assert-true (search "RPLACA" str)))))

(deftest expander-setf-cdr-place
  "compiler-macroexpand-all: (setf (cdr x) v) expands via rplacd."
  (let ((result (cl-cc::compiler-macroexpand-all '(setf (cdr x) v))))
    (let ((str (format nil "~S" result)))
      (assert-true (search "RPLACD" str)))))

(deftest expander-setf-accessor-slot-value-fallback
  "compiler-macroexpand-all: (setf (foo obj) v) with unknown accessor falls back to slot-value."
  ;; Use a name that is definitely not in *accessor-slot-map*
  (let ((result (cl-cc::compiler-macroexpand-all '(setf (my-unknown-accessor-xyz obj) v))))
    ;; After expand-setf-accessor → setf (slot-value ...) → set-slot-value or similar
    (let ((str (format nil "~S" result)))
      (assert-true (search "SLOT-VALUE" str)))))

;;; *setf-compound-place-handlers* — table-driven place dispatch ─────────────

(deftest expander-setf-first-place
  "(setf (first x) v) expands via rplaca (synonym for car)."
  (let ((str (format nil "~S"
                     (cl-cc::compiler-macroexpand-all '(setf (first lst) newval)))))
    (assert-true (search "RPLACA" str))))

(deftest expander-setf-rest-place
  "(setf (rest x) v) expands via rplacd (synonym for cdr)."
  (let ((str (format nil "~S"
                     (cl-cc::compiler-macroexpand-all '(setf (rest lst) newval)))))
    (assert-true (search "RPLACD" str))))

(deftest expander-setf-nth-place
  "(setf (nth i x) v) expands via rplaca + nthcdr."
  (let ((str (format nil "~S"
                     (cl-cc::compiler-macroexpand-all '(setf (nth 2 lst) newval)))))
    (assert-true (search "RPLACA" str))
    (assert-true (search "NTHCDR" str))))

(deftest expander-setf-cadr-place
  "(setf (cadr x) v) expands via rplaca on cdr."
  (let ((str (format nil "~S"
                     (cl-cc::compiler-macroexpand-all '(setf (cadr x) newval)))))
    (assert-true (search "RPLACA" str))
    (assert-true (search "CDR" str))))

(deftest expander-setf-cddr-place
  "(setf (cddr x) v) expands via rplacd on cdr."
  (let ((str (format nil "~S"
                     (cl-cc::compiler-macroexpand-all '(setf (cddr x) newval)))))
    (assert-true (search "RPLACD" str))
    (assert-true (search "CDR" str))))

(deftest expander-setf-getf-place
  "(setf (getf plist key) v) expands via rt-plist-put."
  (let ((str (format nil "~S"
                     (cl-cc::compiler-macroexpand-all '(setf (getf my-plist :foo) 42)))))
    (assert-true (search "RT-PLIST-PUT" str))))

(deftest expander-setf-getf-returns-value
  "(setf (getf ...) v) expansion introduces a temp variable for the value."
  (let ((result (cl-cc::compiler-macroexpand-all '(setf (getf plist :k) val))))
    ;; Top form is LET with a temp binding
    (assert-eq 'let (car result))
    ;; The let body contains setq and the temp var
    (let ((str (format nil "~S" result)))
      (assert-true (search "RT-PLIST-PUT" str)))))

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

(deftest expander-eval-when-execute-included
  "compiler-macroexpand-all: eval-when with :execute produces progn of body."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(eval-when (:execute) (+ 1 2)))))
    (assert-eq 'progn (car result))))

(deftest expander-eval-when-load-toplevel-included
  "compiler-macroexpand-all: eval-when with :load-toplevel produces progn of body."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(eval-when (:load-toplevel) (+ 1 2)))))
    (assert-eq 'progn (car result))))

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

(deftest expander-typed-defun-strips-type-annotations
  "compiler-macroexpand-all: typed defun strips param type annotations."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(defun add-ints ((x fixnum) (y fixnum)) fixnum
                    (+ x y)))))
    ;; Should produce a defun with plain params x y
    (assert-eq 'defun (car result))
    (assert-eq 'add-ints (second result))
    (let ((params (third result)))
      (assert-true (member 'x params))
      (assert-true (member 'y params)))))

(deftest expander-typed-defun-return-type-stripped
  "compiler-macroexpand-all: typed defun return type does not appear in param list."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(defun id-int ((x fixnum)) fixnum x))))
    (assert-eq 'defun (car result))
    ;; fixnum should not appear in the plain parameter list
    (assert-false (member 'fixnum (third result)))))

(deftest expander-typed-lambda-strips-type-annotations
  "compiler-macroexpand-all: typed lambda strips param type annotations."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(lambda ((x fixnum)) fixnum x))))
    (assert-eq 'lambda (car result))
    (let ((params (second result)))
      (assert-true (member 'x params))
      (assert-false (member 'fixnum params)))))

;;; ─── floor/ceiling/truncate/round 1-arg normalization ───────────────────────

(deftest expander-floor-one-arg-normalizes
  "compiler-macroexpand-all: (floor x) normalizes to (floor x 1)."
  (let ((result (cl-cc::compiler-macroexpand-all '(floor n))))
    (assert-eq 'floor (car result))
    (assert-eq 'n (second result))
    (assert-equal 1 (third result))))

(deftest expander-ceiling-one-arg-normalizes
  "compiler-macroexpand-all: (ceiling x) normalizes to (ceiling x 1)."
  (let ((result (cl-cc::compiler-macroexpand-all '(ceiling n))))
    (assert-eq 'ceiling (car result))
    (assert-equal 1 (third result))))

(deftest expander-truncate-one-arg-normalizes
  "compiler-macroexpand-all: (truncate x) normalizes to (truncate x 1)."
  (let ((result (cl-cc::compiler-macroexpand-all '(truncate n))))
    (assert-eq 'truncate (car result))
    (assert-equal 1 (third result))))

(deftest expander-round-one-arg-normalizes
  "compiler-macroexpand-all: (round x) normalizes to (round x 1)."
  (let ((result (cl-cc::compiler-macroexpand-all '(round n))))
    (assert-eq 'round (car result))
    (assert-equal 1 (third result))))

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

(deftest expander-make-array-fill-pointer-promotes
  "compiler-macroexpand-all: (make-array n :fill-pointer t) becomes make-adjustable-vector."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(make-array 10 :fill-pointer t))))
    (let ((str (format nil "~S" result)))
      (assert-true (search "MAKE-ADJUSTABLE-VECTOR" str)))))

(deftest expander-make-array-adjustable-promotes
  "compiler-macroexpand-all: (make-array n :adjustable t) becomes make-adjustable-vector."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(make-array 5 :adjustable t))))
    (let ((str (format nil "~S" result)))
      (assert-true (search "MAKE-ADJUSTABLE-VECTOR" str)))))

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
  "compiler-macroexpand-all: (defconstant +k+ 42) expands to (defparameter +k+ 42)."
  (let ((result (cl-cc::compiler-macroexpand-all '(defconstant +my-const+ 42))))
    (assert-eq 'defparameter (car result))
    (assert-eq '+my-const+ (second result))
    (assert-equal 42 (third result))))

(deftest expander-defconstant-with-docstring
  "compiler-macroexpand-all: defconstant with doc string still expands to defparameter."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(defconstant +pi+ 3.14159 "Pi constant"))))
    (assert-eq 'defparameter (car result))
    (assert-eq '+pi+ (second result))))

;;; ─── #'builtin → lambda wrapping ────────────────────────────────────────────

(deftest expander-function-binary-builtin-wraps-lambda
  "compiler-macroexpand-all: #'cons expands to a lambda (a b) (cons a b)."
  (let ((result (cl-cc::compiler-macroexpand-all '(function cons))))
    (assert-eq 'lambda (car result))
    (assert-equal 2 (length (second result)))))

(deftest expander-function-unary-builtin-wraps-lambda
  "compiler-macroexpand-all: #'car expands to a lambda (x) (car x)."
  (let ((result (cl-cc::compiler-macroexpand-all '(function car))))
    (assert-eq 'lambda (car result))
    (assert-equal 1 (length (second result)))))

(deftest expander-function-variadic-builtin-wraps-lambda
  "compiler-macroexpand-all: #'+ expands to a variadic lambda with dolist fold."
  (let ((result (cl-cc::compiler-macroexpand-all '(function +))))
    ;; expand-function-builtin for variadic returns a compiled lambda
    (assert-eq 'lambda (car result))))

(deftest expander-function-non-builtin-passthrough
  "compiler-macroexpand-all: #'user-fn (not a builtin) passes through as (function user-fn)."
  (let ((result (cl-cc::compiler-macroexpand-all '(function my-user-defined-fn))))
    (assert-eq 'function (car result))
    (assert-eq 'my-user-defined-fn (second result))))

;;; ─── (funcall 'name ...) → direct call ──────────────────────────────────────

(deftest expander-funcall-quoted-symbol-to-direct-call
  "compiler-macroexpand-all: (funcall 'foo x) becomes (foo x)."
  (let ((result (cl-cc::compiler-macroexpand-all '(funcall 'foo x))))
    (assert-eq 'foo (car result))
    (assert-eq 'x (second result))))

(deftest expander-funcall-quoted-builtin-direct-call
  "compiler-macroexpand-all: (funcall 'car lst) becomes (car lst)."
  (let ((result (cl-cc::compiler-macroexpand-all '(funcall 'car lst))))
    (assert-eq 'car (car result))
    (assert-eq 'lst (second result))))

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

(deftest expander-progn-passthrough-forms
  "compiler-macroexpand-all: progn expands each sub-form."
  (let ((result (cl-cc::compiler-macroexpand-all '(progn (+ 1 2) (+ 3 4)))))
    (assert-eq 'progn (car result))
    (assert-equal 2 (length (cdr result)))))

(deftest expander-progn-single-form
  "compiler-macroexpand-all: progn with one form expands that form."
  (let ((result (cl-cc::compiler-macroexpand-all '(progn 42))))
    (assert-eq 'progn (car result))
    (assert-equal 42 (second result))))

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

(deftest expander-variadic-multiply-three
  "compiler-macroexpand-all: (* a b c) nests left-associatively."
  (let ((result (cl-cc::compiler-macroexpand-all '(* a b c))))
    (assert-eq '* (car result))
    (assert-true (consp (second result)))
    (assert-eq '* (car (second result)))))

(deftest expander-variadic-append-three
  "compiler-macroexpand-all: (append a b c) nests left-associatively."
  (let ((result (cl-cc::compiler-macroexpand-all '(append a b c))))
    (assert-eq 'append (car result))
    (assert-true (consp (second result)))
    (assert-eq 'append (car (second result)))))

(deftest expander-variadic-minus-three
  "compiler-macroexpand-all: (- a b c) nests left-associatively."
  (let ((result (cl-cc::compiler-macroexpand-all '(- a b c))))
    (assert-eq '- (car result))
    (assert-true (consp (second result)))
    (assert-eq '- (car (second result)))))

(deftest expander-variadic-plus-zero-args
  "compiler-macroexpand-all: (+) with no args returns identity 0."
  (let ((result (cl-cc::compiler-macroexpand-all '(+))))
    (assert-equal 0 result)))

(deftest expander-variadic-multiply-zero-args
  "compiler-macroexpand-all: (*) with no args returns identity 1."
  (let ((result (cl-cc::compiler-macroexpand-all '(*))))
    (assert-equal 1 result)))

;;; ─── flet / labels body expansion ───────────────────────────────────────────

(deftest expander-flet-expands-body-forms
  "compiler-macroexpand-all: flet expands body forms but not function names."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(flet ((sq (x) (* x x)))
                    (sq 3)))))
    (assert-eq 'flet (car result))
    ;; Binding preserved
    (assert-eq 'sq (first (first (second result))))))

(deftest expander-labels-expands-body-forms
  "compiler-macroexpand-all: labels expands body forms but not binding names."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(labels ((fact (n) (if (= n 0) 1 (* n (fact (- n 1))))))
                    (fact 5)))))
    (assert-eq 'labels (car result))
    (assert-eq 'fact (first (first (second result))))))

;;; ─── defun / lambda default param expansion ──────────────────────────────────

(deftest expander-defun-expands-body
  "compiler-macroexpand-all: plain defun body is recursively expanded."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(defun triple (x) (* 3 x)))))
    (assert-eq 'defun (car result))
    (assert-eq 'triple (second result))
    (assert-equal '(x) (third result))))

(deftest expander-lambda-expands-body
  "compiler-macroexpand-all: plain lambda body is recursively expanded."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(lambda (x y) (+ x y)))))
    (assert-eq 'lambda (car result))
    (assert-equal '(x y) (second result))))

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

(deftest expand-make-array-adjustable-promotes
  "expand-make-array-form with :adjustable t uses make-adjustable-vector."
  (let ((result (format nil "~S" (cl-cc::expand-make-array-form 10 '(:adjustable t)))))
    (assert-true (search "ADJUSTABLE" result))))

(deftest expand-make-array-fill-pointer-promotes
  "expand-make-array-form with :fill-pointer t uses make-adjustable-vector."
  (let ((result (format nil "~S" (cl-cc::expand-make-array-form 10 '(:fill-pointer t)))))
    (assert-true (search "ADJUSTABLE" result))))

;;; ─── expand-eval-when-form ────────────────────────────────────────────────

(deftest expand-eval-when-execute-keeps-body
  "eval-when :execute includes the body as a progn."
  (let ((result (cl-cc::expand-eval-when-form '(:execute) '((+ 1 2)))))
    (assert-true (consp result))))

(deftest expand-eval-when-load-toplevel-keeps-body
  "eval-when :load-toplevel includes the body."
  (let ((result (cl-cc::expand-eval-when-form '(:load-toplevel) '((+ 1 2)))))
    (assert-true (consp result))))

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
