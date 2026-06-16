(cl:in-package :cl-user)

;;;; packages/prolog/src/package.lisp - CL-CC Prolog Package
;;;;
;;;; Phase 1.1 of the package-by-feature monorepo migration. This package owns
;;;; the Prolog engine, DCG transformer, and peephole rule database. The facade
;;;; :cl-cc package uses :cl-cc/prolog so compiler modules continue to see
;;;; unify/solve-goal/query-* unqualified.
;;;;
;;;; Bootstrap symbols (binop, const, var, cmp, integer-type, boolean-type,
;;;; env-lookup, make-cst-token, lexer-token-p/type/value, our-eval) are
;;;; provided by :cl-cc/bootstrap (cl-cc-bootstrap.asd), which loads before
;;;; this package.

(defpackage :cl-cc/prolog
  (:use :cl :cl-cc/bootstrap)
  (:export
   ;; Logic variables & unification
   #:logic-var-p #:unify #:unify-failed-p
   #:logic-substitute
   ;; Database
    #:*prolog-rules* #:clear-prolog-database #:add-rule
   ;; Fact/rule macros
    #:def-fact #:def-rule
   ;; Cut
    #:prolog-cut
   ;; Data tables & builtins
    #:*builtin-predicate-specs* #:*builtin-predicates*
    #:*peephole-rules* #:*enable-prolog-peephole*
    ;; Solver
    #:solve-goal
    #:query-all #:query-one #:query-first-n
    ;; DCG
    #:def-dcg-rule
   #:phrase #:phrase-all))
