;;;; cli/src/package.lisp — CL-CC CLI Package Definition

(defpackage :cl-cc/cli
  (:use :cl)
  (:import-from :cl-cc
                :run-string
                :run-string-typed
                :compile-string
                :compile-file-to-native
                :compilation-result-program
                :run-compiled)
  (:import-from :cl-cc/type
                :type-to-string)
  (:export :main
           :parse-args
           :arg-parse-error
           :arg-parse-error-message
           ;; parsed-args struct and accessors (exported for testing)
           :parsed-args
           :make-parsed-args
           :parsed-args-command
           :parsed-args-positional
           :parsed-args-flags
           ;; Flag accessor helpers
           :flag
           :flag-or))
