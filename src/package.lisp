(defpackage :cl-cc
  (:use :cl :cl-cc/bootstrap :cl-cc/ast :cl-cc/prolog :cl-cc/parse :cl-cc/optimize :cl-cc/emit :cl-cc/expand :cl-cc/compile :cl-cc/vm))

(in-package :cl-cc)

;; Allow optimize source files (in-package :cl-cc/optimize) to access
;; VM instruction types/accessors defined later in the umbrella.
;; use-package establishes persistent inheritance; symbols exported from
;; :cl-cc after this point become accessible in :cl-cc/optimize.
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Re-export bootstrap symbols (our-eval, binop, make-cst-token, etc.)
  ;; so cl-cc:our-eval etc. continue to work for downstream consumers.
  (do-external-symbols (s :cl-cc/bootstrap)
    (export s :cl-cc))
  ;; Re-export AST symbols from :cl-cc so cl-cc:SYMBOL syntax works
  ;; in downstream packages (e.g. :cl-cc/cli, tests, compile/).
  (do-external-symbols (s :cl-cc/ast)
    (export s :cl-cc))
  ;; Re-export prolog symbols from :cl-cc so cl-cc:SYMBOL syntax works.
  (do-external-symbols (s :cl-cc/prolog)
    (export s :cl-cc))
  ;; Re-export optimize symbols BEFORE use-package bridges, so child
  ;; packages can see optimize exports through the umbrella bridge.
  (do-external-symbols (s :cl-cc/optimize)
    (export s :cl-cc))
  ;; Re-export emit symbols from :cl-cc.
  (do-external-symbols (s :cl-cc/emit)
    (export s :cl-cc))
  ;; Re-export parse symbols from :cl-cc.
  (do-external-symbols (s :cl-cc/parse)
    (export s :cl-cc))
  ;; Re-export expand symbols from :cl-cc.
  (do-external-symbols (s :cl-cc/expand)
    (export s :cl-cc))
  ;; Re-export compile symbols from :cl-cc.
  (do-external-symbols (s :cl-cc/compile)
    (export s :cl-cc))
  ;; Wire VM source files into the umbrella package
  (use-package :cl-cc :cl-cc/vm)
  ;; Re-export VM symbols from :cl-cc.
  (do-external-symbols (s :cl-cc/vm)
    (export s :cl-cc))
  ;; NOW establish use-package bridges so child packages can see
  ;; ALL re-exported symbols from the umbrella (including cross-package deps).
  (use-package :cl-cc :cl-cc/parse)
  (use-package :cl-cc :cl-cc/optimize)
  (use-package :cl-cc :cl-cc/emit)
  (use-package :cl-cc :cl-cc/expand)
  (use-package :cl-cc :cl-cc/compile))
