;;;; selfhost-test-support.lisp — Shared helpers and data for self-hosting E2E suites

(in-package :cl-cc/test)

(defun %muffle-redefinition-warning (condition)
  "Muffle implementation warnings for benign iterative DEFUN redefinitions in selfhost tests."
  (when (typep condition 'warning)
    (muffle-warning condition)))

(defmacro %with-fresh-host-package (&body body)
  "Run BODY in a fresh temporary host package using CL.
This prevents selfhost helper defs from colliding with symbols interned in the
long-lived CL-CC/TEST package during canonical full-suite runs."
  `(let* ((pkg-name (format nil "CL-CC/SELFHOST/~36R" (gensym)))
          (pkg (make-package pkg-name :use '(:cl))))
     (unwind-protect
          (let ((*package* pkg))
            ,@body)
       (ignore-errors (delete-package pkg)))))

(defun run-repl-forms (&rest forms)
  "Run FORMS in a fresh REPL context, return the last result."
  (%with-fresh-host-package
    (cl-cc:with-fresh-repl-state
      (let ((result nil))
        (dolist (form forms result)
          (handler-bind ((warning #'%muffle-redefinition-warning))
            (setf result (run-string-repl form))))))))

(defun run-load-and-eval (file-path &rest forms)
  "Load FILE-PATH through our-load, then evaluate FORMS in the same REPL state."
  (%with-fresh-host-package
    (cl-cc:with-fresh-repl-state
      (handler-bind ((warning #'%muffle-redefinition-warning))
        (cl-cc::our-load file-path))
      (let ((result nil))
        (dolist (form forms result)
          (handler-bind ((warning #'%muffle-redefinition-warning))
            (setf result (run-string-repl form))))))))

(defmacro %with-tmpfile ((var prefix content) &body body)
  "Write CONTENT to a unique temp file, bind VAR to its path, then cleanup."
  `(let ((,var (format nil "/tmp/cl-cc-~A-~A.lisp" ,prefix (get-universal-time))))
     (unwind-protect
         (progn
           (with-open-file (s ,var :direction :output :if-exists :supersede)
             (write-string ,content s))
           ,@body)
       (ignore-errors (delete-file ,var)))))

(defun %asdf-source-files (sys-name project-prefix)
  "Return project-relative CL source file paths for SYS-NAME, in ASDF load order."
  (let ((sys (asdf:find-system sys-name nil)))
    (when sys
      (let ((sys-root (asdf:system-source-directory sys))
            (result nil))
        (labels ((walk (comp)
                   (typecase comp
                     (asdf:cl-source-file
                      (let ((rel (enough-namestring (asdf:component-pathname comp) sys-root)))
                        (push (concatenate 'string project-prefix rel) result)))
                     (asdf:module
                      (dolist (child (asdf:component-children comp))
                        (walk child))))))
          (dolist (comp (asdf:component-children sys))
            (walk comp)))
        (nreverse result)))))

(defun selfhost-all-source-files ()
  "All source files in dependency order for Phase 4 self-hosting verification.
Computed at call time so the result is path-independent across compile/load contexts."
  (append
   (%asdf-source-files :cl-cc-bootstrap  "packages/foundation/bootstrap/")
   (%asdf-source-files :cl-cc-ast        "packages/foundation/ast/")
   (%asdf-source-files :cl-cc-prolog     "packages/foundation/prolog/")
   (%asdf-source-files :cl-cc-ir         "packages/foundation/ir/")
   (%asdf-source-files :cl-cc-mir        "packages/foundation/mir/")
   (%asdf-source-files :cl-cc-binary     "packages/backend/binary/")
   (%asdf-source-files :cl-cc-runtime    "packages/backend/runtime/")
   (%asdf-source-files :cl-cc-bytecode   "packages/backend/bytecode/")
   (%asdf-source-files :cl-cc-parse      "packages/frontend/parse/")
   (%asdf-source-files :cl-cc-type       "packages/foundation/type/")
   (%asdf-source-files :cl-cc-vm         "packages/engine/vm/")
   (%asdf-source-files :cl-cc-optimize   "packages/engine/optimize/")
   (%asdf-source-files :cl-cc-emit       "packages/backend/emit/")
   (%asdf-source-files :cl-cc-expand     "packages/frontend/expand/")
   (%asdf-source-files :cl-cc-compile    "packages/engine/compile/")
   (list "packages/umbrella/src/package.lisp"
         "packages/umbrella/pipeline/src/stdlib-source.lisp"
         "packages/umbrella/pipeline/src/stdlib-source-ext.lisp"
         "packages/umbrella/pipeline/src/pipeline-stdlib.lisp"
         "packages/umbrella/pipeline/src/pipeline.lisp"
         "packages/umbrella/pipeline/src/pipeline-native.lisp"
         "packages/umbrella/pipeline/src/pipeline-repl-load.lisp")))

(defparameter *selfhost-representative-files*
  '("packages/frontend/parse/src/cst.lisp"
    "packages/foundation/prolog/src/prolog-data.lisp"
    "packages/foundation/prolog/src/prolog.lisp"
    "packages/frontend/parse/src/lexer.lisp"
    "packages/engine/compile/src/cps.lisp"
    "packages/engine/optimize/src/optimizer.lisp"
    "packages/foundation/type/src/package.lisp"
    "packages/foundation/type/src/kind.lisp"
    "packages/foundation/type/src/multiplicity.lisp"
    "packages/foundation/type/src/types-core.lisp"
    "packages/foundation/type/src/types-extended.lisp"
    "packages/foundation/type/src/types-env.lisp"
    "packages/foundation/type/src/parser.lisp"
    "packages/foundation/type/src/typeclass.lisp"
    "packages/foundation/type/src/solver.lisp"
    "packages/foundation/type/src/inference.lisp"
    "packages/foundation/type/src/checker.lisp"
    "packages/foundation/type/src/printer.lisp"
    "packages/foundation/mir/src/mir.lisp"
    "packages/engine/vm/src/vm.lisp"
    "packages/backend/runtime/src/gc.lisp")
  "Representative subset of source files covering all major modules.")
