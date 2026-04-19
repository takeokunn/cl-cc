;;;; clos-dispatch-tests.lisp — CLOS :default-initargs, :allocation :class, EQL specializers, and method qualifiers
(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

;;; ── :default-initargs ──────────────────────────────────────────────────────

(deftest clos-parse-default-initargs
  "defclass with :default-initargs parses into AST."
  (let ((ast (lower-sexp-to-ast '(defclass point ()
                                    ((x :initarg :x :initform 0))
                                    (:default-initargs :x 42)))))
    (assert-type ast-defclass ast)
    (let ((di (cl-cc/ast::ast-defclass-default-initargs ast)))
      (assert-= 1 (length di))
      (assert-eq :x (car (first di))))))

(deftest-each clos-default-initargs-runtime
  ":default-initargs provides defaults; explicit initargs override them."
  :cases (("default-applies"    42 "(progn (defclass da-point () ((x :initarg :x :initform 0)) (:default-initargs :x 42)) (slot-value (make-instance 'da-point) 'x))")
          ("explicit-overrides" 99 "(progn (defclass da-point2 () ((x :initarg :x :initform 0)) (:default-initargs :x 42)) (slot-value (make-instance 'da-point2 :x 99) 'x))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; ── :allocation :class Tests ────────────────────────────────────────────────

(deftest-each clos-allocation-class
  "defclass :allocation :class slots are shared; :instance slots are independent."
  :cases
  (("class-slot-shared-read"     42
    "(defclass counter ()
       ((count :initarg :count :allocation :class)))
     (let ((c1 (make-instance 'counter :count 42))
           (c2 (make-instance 'counter)))
       (slot-value c2 'count))")
   ("class-slot-mutation-reads"  99
    "(defclass shared-box ()
       ((val :initarg :val :allocation :class)))
     (let ((a (make-instance 'shared-box :val 10))
           (b (make-instance 'shared-box)))
       (setf (slot-value a 'val) 99)
       (slot-value b 'val))")
   ("mixed-shared-from-second"  100
    "(defclass mixed ()
       ((shared :initarg :shared :allocation :class)
        (own    :initarg :own)))
     (let ((a (make-instance 'mixed :shared 100 :own 1))
           (b (make-instance 'mixed :own 2)))
       (slot-value b 'shared))")
   ("mixed-instance-independent"  1
    "(defclass mixed2 ()
       ((shared :initarg :shared :allocation :class)
        (own    :initarg :own)))
     (let ((a (make-instance 'mixed2 :shared 100 :own 1))
           (b (make-instance 'mixed2 :own 2)))
       (slot-value a 'own))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; ── EQL Specializer Tests ──────────────────────────────────────────────────

(deftest-each clos-eql-specializer
  "EQL specializers dispatch on specific values, fall back to class, and match symbols."
  :cases (("penny"        1   "(defgeneric coin-value (c))
                               (defmethod coin-value ((c (eql :penny))) 1)
                               (defmethod coin-value ((c (eql :nickel))) 5)
                               (coin-value :penny)")
          ("nickel"       5   "(defgeneric coin-val2 (c))
                               (defmethod coin-val2 ((c (eql :penny))) 1)
                               (defmethod coin-val2 ((c (eql :nickel))) 5)
                               (coin-val2 :nickel)")
          ("eql-match"    42  "(defgeneric describe-it (x))
                               (defmethod describe-it ((x (eql 42))) 42)
                               (defmethod describe-it ((x integer)) 0)
                               (describe-it 42)")
          ("class-fallbk" 0   "(defgeneric describe-it2 (x))
                               (defmethod describe-it2 ((x (eql 42))) 42)
                               (defmethod describe-it2 ((x integer)) 0)
                               (describe-it2 99)")
          ("sym-match"    100 "(defgeneric sym-val (s))
                               (defmethod sym-val ((s (eql 'foo))) 100)
                               (defmethod sym-val ((s symbol)) 0)
                               (sym-val 'foo)")
          ("sym-fallbk"   0   "(defgeneric sym-val2 (s))
                               (defmethod sym-val2 ((s (eql 'foo))) 100)
                               (defmethod sym-val2 ((s symbol)) 0)
                               (sym-val2 'bar)"))
  (expected form)
  (assert-= expected (run-string form)))

;;; ── Method Qualifier Tests (:before/:after/:around) ──────────────────────

(deftest-each clos-defmethod-qualifier-run
  "Method qualifier execution: :before/:around/:around+before+after run in the correct order."
  :timeout 15
  :cases (("before"
           "before:primary"
           "(defvar *bq-log* \"\")
            (defgeneric greet-q (x))
            (defmethod greet-q ((x integer))
              (setf *bq-log* (concatenate 'string *bq-log* \"primary\"))
              *bq-log*)
            (defmethod greet-q :before ((x integer))
              (setf *bq-log* (concatenate 'string *bq-log* \"before:\")))
            (greet-q 1)")
          ("before-and-after"
           "B:P:A"
           "(defvar *ba-log* \"\")
            (defgeneric ba-test (x))
            (defmethod ba-test ((x integer))
              (setf *ba-log* (concatenate 'string *ba-log* \"P\"))
              *ba-log*)
            (defmethod ba-test :before ((x integer))
              (setf *ba-log* (concatenate 'string *ba-log* \"B:\")))
            (defmethod ba-test :after ((x integer))
              (setf *ba-log* (concatenate 'string *ba-log* \":A\")))
            (ba-test 1)
            *ba-log*")
          ("around"
           "WRAPPED:42"
           "(defgeneric around-test (x))
            (defmethod around-test ((x integer))
              42)
            (defmethod around-test :around ((x integer))
              (let ((result (call-next-method)))
                (concatenate 'string \"WRAPPED:\" (write-to-string result))))
            (around-test 1)")
          ("around-with-before-after"
           "B:P:A"
           "(defvar *aba-log* \"\")
            (defgeneric aba-test (x))
            (defmethod aba-test ((x integer))
              (setf *aba-log* (concatenate 'string *aba-log* \"P\"))
              *aba-log*)
            (defmethod aba-test :before ((x integer))
              (setf *aba-log* (concatenate 'string *aba-log* \"B:\")))
            (defmethod aba-test :after ((x integer))
              (setf *aba-log* (concatenate 'string *aba-log* \":A\")))
            (defmethod aba-test :around ((x integer))
              (call-next-method)
              *aba-log*)
            (aba-test 1)"))
  (expected form)
  (assert-equal expected (run-string form)))

(deftest clos-defmethod-after-qualifier
  "defmethod :after runs after the primary method; primary value is returned."
  :timeout 15
  (assert-= 42
    (run-string
     "(defgeneric aft-test (x))
      (defmethod aft-test ((x integer))
        42)
      (defmethod aft-test :after ((x integer))
        99)
      (aft-test 1)")))

(deftest-each clos-defmethod-qualifier-parse
  "defmethod with :before and :around qualifiers parse correctly."
  :cases (("before" '(defmethod foo :before ((x integer)) (print x)) :before)
          ("around" '(defmethod foo :around ((x integer)) (print x)) :around))
  (form expected-qualifier)
  (let ((ast (lower-sexp-to-ast form)))
    (assert-type ast-defmethod ast)
    (assert-eq expected-qualifier (cl-cc/ast::ast-defmethod-qualifier ast))))

(deftest clos-defmethod-around-without-cnm
  "defmethod :around without call-next-method returns around's value.
KNOWN COMPILER BUG: self-hosted CLOS dispatch recurses infinitely when an
:around method omits call-next-method. ANSI semantics: the :around body
should run to completion and its value be returned WITHOUT ever entering
the primary methods. Tagged :slow so focused REPL runs can still filter it;
the canonical `nix run .#test` plan runs it with a 5-second hard timeout so the hang is
bounded. Remove the :slow tag once the CLOS dispatch bug is fixed."
  :tags '(:slow)
  :timeout 5
  (assert-equal "AROUND-ONLY"
    (run-string
     "(defgeneric around-only-test (x))
      (defmethod around-only-test ((x integer))
        \"PRIMARY\")
      (defmethod around-only-test :around ((x integer))
        \"AROUND-ONLY\")
      (around-only-test 1)")))
