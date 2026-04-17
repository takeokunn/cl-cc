;;;; packages/frontend/expand/src/package.lisp --- cl-cc/expand package definition
;;;;
;;;; Macro expansion subsystem: macro environment, defmacro machinery,
;;;; macroexpansion (our-macroexpand-1/-all), lambda-list parsing and
;;;; destructuring, and the LOOP/DO/CASE/TYPECASE control-flow macros.
;;;;
;;;; Extracted as a Phase 2 sibling system (:cl-cc-expand). The package
;;;; facade is loaded first (no dependencies); the umbrella :cl-cc system
;;;; then sets up (use-package :cl-cc :cl-cc/expand) so the source files
;;;; can access VM instruction types and accessors unqualified.

(defpackage :cl-cc/expand
  (:use :cl)
  (:export
   ;; --- expander-data.lisp --- constant table + accessor/struct maps -----
   #:*constant-table*
   #:*accessor-slot-map*
   #:*defstruct-slot-registry*

   ;; --- expander-core.lisp --- macro expander construction -------------
   #:make-macro-expander

   ;; --- macro.lisp --- macro environment + expansion engine -----------
   #:macro-env
   #:macro-env-table
   #:our-defmacro
   #:our-macroexpand-1
   #:our-macroexpand
   #:our-macroexpand-all
   #:register-macro
   #:lookup-macro
   #:*macro-environment*
   #:*macro-eval-fn*

   ;; --- macro-lambda-list.lisp --- lambda-list helpers ----------------
   #:parse-lambda-list
   #:destructure-lambda-list

   ;; --- expander.lisp --- compiler macro expansion --------------------
   #:compiler-macroexpand-all

   ;; --- control-flow macros ------------------------------------------
   #:dolist
   #:dotimes
   #:do
   #:do*
   #:case
   #:typecase
   #:loop

   ;; --- macros used via run-string (need umbrella re-export) ---------
   #:defun/c
   #:compiler-let
   #:copy-hash-table
   #:copy-structure))
