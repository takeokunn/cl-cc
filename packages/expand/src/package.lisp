;;;; packages/expand/src/package.lisp --- cl-cc/expand package definition
;;;;
;;;; Macro expansion subsystem: macro environment, defmacro machinery,
;;;; macroexpansion (our-macroexpand-1/-all), lambda-list parsing and
;;;; destructuring, and the LOOP/DO/CASE/TYPECASE control-flow macros.
;;;;
;;;; Uses cl-cc/bootstrap so source files can reference our-eval unqualified.
;;;; cl-cc/type is accessed qualified (cl-cc/type:...) so not in :use.

(defpackage :cl-cc/expand
  (:use :cl :cl-cc/bootstrap)
  (:shadow #:compiler-macro-function)
  (:export
   ;; --- expander-data.lisp --- constant table + accessor/struct maps -----
   #:*constant-table*
    #:*accessor-slot-map*
     #:*defstruct-read-only-accessor-map*
     #:*defstruct-slot-registry*
     #:*defstruct-type-registry*
     #:*setf-compound-place-handlers*
     #:*declaim-inline-registry*
    #:*declaim-optimize-registry*
    #:with-fresh-defstruct-registries
    #:declaration-optimize-quality

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
   #:*symbol-macro-table*
    #:*compiler-macro-table*
    #:*deftransform-table*
    #:*macroexpand-step-cache*
    #:*macroexpand-all-cache*
    #:register-deftransform
    #:lookup-deftransform
    #:deftransform
    #:define-deftransform

    ;; --- macro-lambda-list.lisp --- lambda-list helpers ----------------
   #:parse-lambda-list
   #:destructure-lambda-list
   #:generate-lambda-bindings

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

   ;; --- expander-typed-params.lisp --- typed lambda-list helpers ----------
   #:*function-type-registry*
   #:register-function-type
   #:lambda-list-has-typed-p
   #:strip-typed-params
   #:lambda-list-info-environment

   ;; --- macros used via run-string (need umbrella re-export) ---------
   #:defun/c
   #:copy-hash-table
   #:copy-structure
   #:make-iterator
   #:iterator-next
   #:doiterator
   #:with-iterator
   ;; --- hygienic macros (FR-804/805) ---------------------------------
   #:define-syntax
   #:syntax-rules
   #:with-gensyms
   #:once-only
   ;; --- lazy evaluation (FR-856/857) ---------------------------------
   #:promise
   #:promisep
   #:%make-promise
   #:force
   #:delay
   #:memoize
   #:memoize-stats
   #:memoize-clear
   #:*memoize-registry*))
