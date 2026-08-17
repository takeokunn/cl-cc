;;;; tests/pbt/prolog-pbt-tests.lisp - Property-Based Tests for Prolog Engine
;;;
;;; Property-based tests for the external cl-prolog-kit unification engine's
;;; UNIFY / LOGIC-SUBSTITUTE / LOGIC-VAR-P (imported in pbt/package.lisp).
;;; UNIFY returns (VALUES ENV OK) rather than a single :UNIFY-FAIL sentinel.
;;;
;;; These use cl-weave's NATIVE property API (cl-weave:describe / it-property /
;;; gen-integer / gen-symbol) directly, rather than the home-grown cl-cc/pbt
;;; defproperty DSL.
;;;
;;; Every body asserts through CL-WEAVE:EXPECT. IT-PROPERTY decides pass/fail
;;; from a *signaled* condition — RUN-PROPERTY wraps the body in
;;; PROPERTY-FAILURE-CONDITION, which only catches ERROR — and discards the
;;; body's return value, so a body that merely evaluates to a boolean reports
;;; PASS even when the property is false.

(in-package :cl-cc/pbt)


(cl-weave:describe "cl-prolog-kit unification properties"
  (cl-weave:it-property "unify binds a logic variable to an integer"
      ((n (cl-weave:gen-integer :min -1000 :max 1000)))
    (multiple-value-bind (env ok) (unify '?x n)
      (cl-weave:expect ok :to-be-truthy)
      (cl-weave:expect (logic-substitute '?x env) :to-equal n)))

  (cl-weave:it-property "unify binds a logic variable to a symbol"
      ((s (cl-weave:gen-symbol)))
    (multiple-value-bind (env ok) (unify '?x s)
      (cl-weave:expect ok :to-be-truthy)
      (cl-weave:expect (logic-substitute '?x env) :to-be s)))

  (cl-weave:it-property "unify chains two variables, then binds through"
      ((n (cl-weave:gen-integer :min -1000 :max 1000)))
    (multiple-value-bind (env1 ok1) (unify '?x '?y)
      (cl-weave:expect ok1 :to-be-truthy)
      (multiple-value-bind (env2 ok2) (unify '?x n env1)
        (cl-weave:expect ok2 :to-be-truthy)
        (cl-weave:expect (logic-substitute '?y env2) :to-equal n))))

  (cl-weave:it-property "unify fails on two distinct integers"
      ((a (cl-weave:gen-integer :min 0 :max 500))
       (b (cl-weave:gen-integer :min 501 :max 1000)))
    (multiple-value-bind (env ok) (unify a b)
      (declare (ignore env))
      (cl-weave:expect ok :to-be-falsy)))

  (cl-weave:it-property "logic-substitute preserves non-variables"
      ((n (cl-weave:gen-integer :min -1000 :max 1000)))
    (cl-weave:expect (logic-substitute n nil) :to-be n))

  (cl-weave:it-property "unify binds a logic variable to a list"
      ((a (cl-weave:gen-integer :min -100 :max 100))
       (b (cl-weave:gen-integer :min -100 :max 100)))
    (multiple-value-bind (env ok) (unify '?x (list a b))
      (cl-weave:expect ok :to-be-truthy)
      (cl-weave:expect (logic-substitute '?x env) :to-equal (list a b))))

  (cl-weave:it-property "logic-var-p recognizes ?-prefixed symbols only"
      ((n (cl-weave:gen-integer :min -1000 :max 1000)))
    (cl-weave:expect (logic-var-p '?x) :to-be-truthy)
    (cl-weave:expect (logic-var-p '?foo) :to-be-truthy)
    (cl-weave:expect (logic-var-p 'x) :to-be-falsy)
    (cl-weave:expect (logic-var-p n) :to-be-falsy)))
