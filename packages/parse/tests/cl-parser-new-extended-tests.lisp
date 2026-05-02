;;;; tests/unit/parse/cl-parser-new-extended-tests.lisp — Extended parser tests: roundtrip, slot-spec, lambda edge cases, pipeline
;;;;
;;;; Tests: ast-to-sexp roundtrip additional forms, parse-slot-spec,
;;;; parse-compiler-lambda-list edge cases, full parse-then-lower pipeline.
;;;; Requires helpers (parse-one, parse-many, lower, ast-roundtrip) from earlier files (loaded first).

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── NEW: ast-to-sexp roundtrip additional forms ──────────────────────────

(deftest-each ast-roundtrip-head-preserved
  "ast-to-sexp roundtrip: each form round-trips with its head symbol intact."
  :cases (("return-from"   '(return-from blk 42)          'return-from)
          ("go"            '(go my-tag)                   'go)
          ("catch"         '(catch 'tag 1 2)              'catch)
          ("throw"         '(throw 'tag 99)               'throw)
          ("the"           '(the fixnum x)                'the)
          ("values"        '(values 1 2 3)                'values)
          ("print"         '(print 42)                    'print)
          ("make-instance" '(make-instance 'point :x 1 :y 2) 'make-instance))
  (form expected-head)
  (assert-eq expected-head (first (ast-roundtrip form))))

(deftest-each ast-roundtrip-local-fn-forms
  "ast-to-sexp roundtrip: flet/labels preserve structure with one binding."
  :cases (("flet"   'flet   '(flet ((f (x) x)) (f 1)))
          ("labels" 'labels '(labels ((f (n) (if (= n 0) 1 (f (- n 1))))) (f 5))))
  (expected-head form)
  (let ((result (ast-roundtrip form)))
    (assert-eq expected-head (first result))
    (assert-= 1 (length (second result)))))

(deftest-each ast-roundtrip-condition-control
  "ast-to-sexp roundtrip: handler-case and unwind-protect preserve head and structure length."
  :cases (("handler-case"   '(handler-case (risky) (error (e) (print e)))       'handler-case   3)
          ("unwind-protect" '(unwind-protect (risky) (cleanup1) (cleanup2))     'unwind-protect 4))
  (form expected-head expected-min-len)
  (let ((result (ast-roundtrip form)))
    (assert-eq expected-head (first result))
    (assert-true (>= (length result) expected-min-len))))

(deftest-each ast-roundtrip-definition-forms
  "ast-to-sexp roundtrip: defvar-no-value, defclass, and defmethod preserve their head and key fields."
  :cases (("defvar-no-value" '(defvar *x*)
           'defvar '*x* nil)
          ("defclass"        '(defclass point (shape) (x y))
           'defclass 'point '(shape))
          ("defmethod"       '(defmethod area ((s circle)) (* 3 (slot-value s 'r)))
           'defmethod 'area nil))
  (form expected-head expected-second expected-third)
  (let ((result (ast-roundtrip form)))
    (assert-eq expected-head (first result))
    (assert-eq expected-second (second result))
    (when expected-third
      (assert-equal expected-third (third result)))))

;;; ─── NEW: parse-slot-spec ──────────────────────────────────────────────────

(deftest parse-slot-spec-bare-symbol
  "parse-slot-spec: bare symbol produces slot with nil initarg and reader."
  (let ((slot (cl-cc/parse:parse-slot-spec 'x)))
    (assert-eq 'x (cl-cc::ast-slot-name slot))
    (assert-null (cl-cc::ast-slot-initarg slot))
    (assert-null (cl-cc::ast-slot-reader slot))))

(deftest parse-slot-spec-full-options
  "parse-slot-spec: list form with all options stores each slot attribute."
  (let ((slot (cl-cc/parse:parse-slot-spec
               '(x :initarg :x :reader get-x :writer set-x :accessor x-acc :type integer))))
    (assert-eq 'x       (cl-cc::ast-slot-name     slot))
    (assert-eq :x       (cl-cc::ast-slot-initarg   slot))
    (assert-eq 'get-x   (cl-cc::ast-slot-reader    slot))
    (assert-eq 'set-x   (cl-cc::ast-slot-writer    slot))
    (assert-eq 'x-acc   (cl-cc::ast-slot-accessor  slot))
    (assert-eq 'integer (cl-cc::ast-slot-type      slot))))

(deftest parse-slot-spec-initform
  "parse-slot-spec: :initform is parsed as an AST integer node."
  (let ((slot (cl-cc/parse:parse-slot-spec '(count :initform 0))))
    (assert-eq 'count (cl-cc::ast-slot-name slot))
    (assert-true (cl-cc/ast:ast-int-p (cl-cc::ast-slot-initform slot)))
    (assert-= 0 (cl-cc/ast:ast-int-value (cl-cc::ast-slot-initform slot)))))

;;; ─── NEW: parse-compiler-lambda-list edge cases ────────────────────────────

(deftest parser-lambda-list-rest-and-key
  "parse-compiler-lambda-list: &rest followed by &key stores both rest-param and key-params."
  (multiple-value-bind (required optional rest-param key-params)
      (cl-cc/parse:parse-compiler-lambda-list '(x &rest args &key verbose))
    (assert-equal '(x) required)
    (assert-null optional)
    (assert-eq 'args rest-param)
    (assert-= 1 (length key-params))
    (assert-eq 'verbose (first (first key-params)))))

(deftest parser-lambda-list-allow-other-keys
  "parse-compiler-lambda-list: &allow-other-keys is accepted without error."
  (multiple-value-bind (required optional rest-param key-params)
      (cl-cc/parse:parse-compiler-lambda-list '(&key x &allow-other-keys))
    (assert-null required)
    (assert-null optional)
    (assert-null rest-param)
    (assert-= 1 (length key-params))))

(deftest parser-lambda-list-body-treated-as-rest
  "parse-compiler-lambda-list: &body stores its parameter as rest-param."
  (multiple-value-bind (required optional rest-param key-params)
      (cl-cc/parse:parse-compiler-lambda-list '(x &body forms))
    (declare (ignore optional))
    (assert-equal '(x) required)
    (assert-eq 'forms rest-param)
    (assert-null key-params)))

(deftest parser-lambda-list-bare-optional
  "parse-compiler-lambda-list: bare &optional symbol stores a (name nil) pair."
  (multiple-value-bind (required optional rest-param key-params)
      (cl-cc/parse:parse-compiler-lambda-list '(&optional x))
    (declare (ignore rest-param key-params))
    (assert-null required)
    (assert-= 1 (length optional))
    (assert-eq 'x (first (first optional)))
    (assert-null (second (first optional)))))

;;; ─── NEW: full parse-then-lower pipeline ───────────────────────────────────

(deftest-each parse-lower-pipeline
  "parse-source then lower-sexp-to-ast: end-to-end for various forms"
  :cases (("integer"     "42"                    #'cl-cc/ast:ast-int-p)
          ("string"      "\"hi\""                #'cl-cc/ast:ast-quote-p)
          ("nil"         "nil"                    #'cl-cc/ast:ast-quote-p)
          ("t"           "t"                      #'cl-cc/ast:ast-quote-p)
          ("symbol"      "foo"                    #'cl-cc/ast:ast-var-p)
          ("hole"        "_"                      #'cl-cc/ast:ast-hole-p)
          ("if"          "(if x 1 2)"             #'cl-cc/ast:ast-if-p)
          ("let"         "(let ((x 1)) x)"        #'cl-cc/ast:ast-let-p)
          ("lambda"      "(lambda (x) x)"         #'cl-cc/ast:ast-lambda-p)
          ("defun"       "(defun f (x) x)"        #'cl-cc/ast:ast-defun-p)
          ("quote"       "'hello"                  #'cl-cc/ast:ast-quote-p)
          ("progn"       "(progn 1 2)"             #'cl-cc/ast:ast-progn-p)
          ("setq"        "(setq x 1)"              #'cl-cc/ast:ast-setq-p)
          ("block"       "(block b 1)"             #'cl-cc/ast:ast-block-p)
          ("call"        "(foo 1 2)"               #'cl-cc/ast:ast-call-p))
  (source pred)
  (let* ((sexp (parse-one source))
         (node (lower sexp)))
    (assert-true (funcall pred node))))
