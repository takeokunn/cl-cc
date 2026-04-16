(cl:in-package :cl-user)

;;;; src/prolog/package.lisp - CL-CC Prolog Package
;;;;
;;;; Phase 1.1 of the package-by-feature monorepo migration. This package owns
;;;; the Prolog engine, DCG transformer, and peephole rule database. The facade
;;;; :cl-cc package uses :cl-cc/prolog so compiler modules continue to see
;;;; unify/solve-goal/query-* unqualified.
;;;;
;;;; Bootstrap symbols (binop, const, var, cmp, integer-type, boolean-type,
;;;; env-lookup, make-cst-token, lexer-token-p/type/value, our-eval) are
;;;; provided by :cl-cc/bootstrap (cl-cc-bootstrap.asd), which loads before
;;;; this package. The previous compile-time intern-into-:cl-cc hack in this
;;;; file has been removed.

(defpackage :cl-cc/prolog
  (:use :cl :cl-cc/bootstrap)
  (:export
   ;; Logic variables & unification
   #:logic-var-p #:occurs-check #:unify #:unify-failed-p
   #:logic-substitute #:substitute-variables
   ;; Goal/rule structs
   #:prolog-goal #:make-prolog-goal #:prolog-goal-p
   #:goal-predicate #:goal-args
   #:prolog-rule #:make-prolog-rule #:prolog-rule-p
   #:rule-head #:rule-body
   ;; Database
   #:*prolog-rules* #:clear-prolog-database #:add-rule #:rename-variables
   ;; Fact/rule macros
   #:def-fact #:def-rule
   ;; Cut
   #:prolog-cut
   ;; Data tables & builtins
   #:*builtin-predicate-specs* #:*builtin-predicates*
   #:*peephole-rules* #:*enable-prolog-peephole*
   #:prolog-cut-handler #:prolog-and-handler #:prolog-or-handler
   #:prolog-unify-handler #:prolog-not-unify-handler #:prolog-when-handler
   ;; Solver
   #:solve-goal #:solve-conjunction
   #:subst-for-eval #:eval-lisp-condition
   #:query-all #:query-one #:query-first-n
   #:apply-prolog-peephole
   ;; DCG
   #:*dcg-sync-tokens* #:*dcg-counter*
   #:lexer-tokens-to-dcg-input #:dcg-token-to-cst
   #:dcg-parse #:dcg-parse-all
   #:dcg-fresh-var #:dcg-reset-counter
   #:dcg-transform-body-element #:dcg-transform-body
   #:def-dcg-rule
   #:phrase #:phrase-rest #:phrase-all))
