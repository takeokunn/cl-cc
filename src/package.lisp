(defpackage :cl-cc
  (:use :cl :cl-cc/bootstrap :cl-cc/ast :cl-cc/prolog :cl-cc/parse :cl-cc/optimize :cl-cc/emit :cl-cc/expand :cl-cc/compile :cl-cc/vm))

(in-package :cl-cc)

;; Re-export all child-package symbols from :cl-cc umbrella, then wire
;; use-package bridges so expand/compile source files can see each other's
;; exports without qualification (Phase 3c/3d: now real ASDF systems).
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
  ;; Before bridging expand, resolve symbols that expand defines via our-defmacro
  ;; but vm also owns as accessors/variables. We need to:
  ;;  1. Move macro registrations to the vm symbol (so lookups find the macro)
  ;;  2. Unintern the expand-specific symbol so use-package doesn't conflict.
  ;; The conflicting names are known (discovered at Phase 3c migration).
  (let ((conflict-names '("GENERIC-FUNCTION-METHOD-COMBINATION" "GENERIC-FUNCTION-METHODS"
                          "%PACKAGE-EXTERNAL-SYMBOLS" "*DOCUMENTATION-TABLE*"
                          "HASH-TABLE-VALUES" "BSWAP" "%CLASS-SLOT-DEFINITIONS"
                          "%PACKAGE-SYMBOLS" "%ALL-SYMBOLS")))
    (dolist (name conflict-names)
      (let* ((vm-sym (find-symbol name :cl-cc/vm))
             (exp-sym (find-symbol name :cl-cc/expand)))
        (when (and vm-sym exp-sym (not (eq vm-sym exp-sym)))
          ;; Transfer macro registration from exp-sym to vm-sym if present
          (let ((macro-fn (cl-cc/expand:lookup-macro exp-sym)))
            (when macro-fn
              (cl-cc/expand:register-macro vm-sym macro-fn)))
          ;; Unintern exp-sym so use-package won't conflict
          (unintern exp-sym :cl-cc/expand)))))
  ;; NOW establish use-package bridges so child packages can see
  ;; ALL re-exported symbols from the umbrella (including cross-package deps).
  (use-package :cl-cc :cl-cc/parse)
  (use-package :cl-cc :cl-cc/expand)
  (use-package :cl-cc :cl-cc/compile))
