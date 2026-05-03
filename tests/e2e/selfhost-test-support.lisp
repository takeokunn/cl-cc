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
   (%asdf-source-files :cl-cc-bootstrap  "packages/bootstrap/")
   (%asdf-source-files :cl-cc-ast        "packages/ast/")
   (%asdf-source-files :cl-cc-prolog     "packages/prolog/")
   (%asdf-source-files :cl-cc-ir         "packages/ir/")
   (%asdf-source-files :cl-cc-mir        "packages/mir/")
   (%asdf-source-files :cl-cc-binary     "packages/binary/")
   (%asdf-source-files :cl-cc-runtime    "packages/runtime/")
   (%asdf-source-files :cl-cc-bytecode   "packages/bytecode/")
   (%asdf-source-files :cl-cc-parse      "packages/parse/")
   (%asdf-source-files :cl-cc-type       "packages/type/")
   (%asdf-source-files :cl-cc-vm         "packages/vm/")
   (%asdf-source-files :cl-cc-optimize   "packages/optimize/")
   (%asdf-source-files :cl-cc-emit       "packages/emit/")
   (%asdf-source-files :cl-cc-expand     "packages/expand/")
    (%asdf-source-files :cl-cc-compile    "packages/compile/")
    (%asdf-source-files :cl-cc-stdlib    "packages/stdlib/")
    (list "packages/umbrella-src/package.lisp")
    (%asdf-source-files :cl-cc-pipeline   "packages/pipeline/")))

(defparameter *selfhost-representative-files*
  '("packages/parse/src/cst.lisp"
    "packages/prolog/src/prolog-data.lisp"
    "packages/prolog/src/prolog.lisp"
    "packages/parse/src/lexer.lisp"
    "packages/cps/src/cps.lisp"
    "packages/optimize/src/optimizer.lisp"
    "packages/type/src/package.lisp"
    "packages/type/src/kind.lisp"
    "packages/type/src/multiplicity.lisp"
    "packages/type/src/types-core.lisp"
    "packages/type/src/types-extended.lisp"
    "packages/type/src/types-env.lisp"
    "packages/type/src/parser.lisp"
    "packages/type/src/typeclass.lisp"
    "packages/type/src/solver.lisp"
    "packages/type/src/inference.lisp"
    "packages/type/src/checker.lisp"
    "packages/type/src/printer.lisp"
    "packages/mir/src/mir.lisp"
    "packages/vm/src/vm.lisp"
    "packages/runtime/src/gc.lisp")
  "Representative subset of source files covering all major modules.")
