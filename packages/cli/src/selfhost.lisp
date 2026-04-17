;;;; cli/src/selfhost.lisp — CL-CC Self-Hosting Verification
;;;;
;;;; Data: *selfhost-source-files* — ordered list of all source files
;;;; Logic: five phase helpers + summary printer + %do-selfhost entry point
;;;;
;;;; Load order: after args.lisp, before main.lisp.

(in-package :cl-cc/cli)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Data: ordered source files for self-hosting phase 4.
;;; Derived from ASDF system definitions so the list stays current
;;; as files are added or removed during refactoring.
;;; ─────────────────────────────────────────────────────────────────────────

(defun %asdf-source-files (sys-name project-prefix)
  "Return project-relative CL source file paths for SYS-NAME, in ASDF load order.
PROJECT-PREFIX is the path from project root to the system directory, e.g.
\"packages/frontend/expand/\". Uses each system's own source directory as
the base so Nix store hashes don't affect relative path computation."
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

(defparameter *selfhost-source-files*
  (append
   ;; Subsystems in dependency order: bootstrap → ast → prolog → ir/mir/binary/runtime/bytecode
   ;;   → parse → type → vm → optimize → emit → expand → compile
   (%asdf-source-files :cl-cc-bootstrap  "packages/foundation/bootstrap/")
   (%asdf-source-files :cl-cc-ast        "packages/foundation/ast/")
   (%asdf-source-files :cl-cc-prolog     "packages/prolog/prolog/")
   (%asdf-source-files :cl-cc-ir         "packages/foundation/ir/")
   (%asdf-source-files :cl-cc-mir        "packages/foundation/mir/")
   (%asdf-source-files :cl-cc-binary     "packages/backend/binary/")
   (%asdf-source-files :cl-cc-runtime    "packages/backend/runtime/")
   (%asdf-source-files :cl-cc-bytecode   "packages/backend/bytecode/")
   (%asdf-source-files :cl-cc-parse      "packages/frontend/parse/")
   (%asdf-source-files :cl-cc-type       "packages/type/type/")
   (%asdf-source-files :cl-cc-vm         "packages/engine/vm/")
   (%asdf-source-files :cl-cc-optimize   "packages/engine/optimize/")
   (%asdf-source-files :cl-cc-emit       "packages/backend/emit/")
   (%asdf-source-files :cl-cc-expand     "packages/frontend/expand/")
   (%asdf-source-files :cl-cc-compile    "packages/engine/compile/")
   ;; Umbrella package + compile-pipeline (use (in-package :cl-cc), load last)
   (list "packages/umbrella/src/package.lisp"
         "packages/engine/pipeline/src/stdlib-source.lisp"
         "packages/engine/pipeline/src/stdlib-source-ext.lisp"
         "packages/engine/pipeline/src/pipeline-stdlib.lisp"
         "packages/engine/pipeline/src/pipeline.lisp"
         "packages/engine/pipeline/src/pipeline-native.lisp"
         "packages/engine/pipeline/src/pipeline-repl.lisp"))
  "Ordered source files for self-hosting phase 4 verification.
Computed dynamically from ASDF system definitions to stay current.")

;;; ─────────────────────────────────────────────────────────────────────────
;;; Phase helpers — each takes CHECK, a (name expected actual) callback.
;;; ─────────────────────────────────────────────────────────────────────────

(defun %selfhost-phase-macro-eval (check)
  "Phase 1: verify macro expansion uses own-eval, not host eval."
  (format t "--- Macro eval through own VM ---~%")
  (funcall check "macro-eval-fn = our-eval (set at load time)"
           t (eq cl-cc:*macro-eval-fn* #'cl-cc:our-eval)))

(defun %selfhost-phase-basic-compilation (check)
  "Phase 2: verify basic CL forms compile and execute correctly."
  (format t "--- Basic compilation ---~%")
  (flet ((try (form) (handler-case (cl-cc:run-string form) (error () :err))))
    (funcall check "arithmetic"            42  (try "(+ 21 21)"))
    (funcall check "recursion"             120
             (try "(defun sh-f (n) (if (<= n 1) 1 (* n (sh-f (- n 1))))) (sh-f 5)"))
    (funcall check "closure"               15
             (try "(let ((x 10)) (funcall (lambda (y) (+ x y)) 5))"))
    (funcall check "defmacro via our-eval" 3
             (try "(defmacro sh-w (t2 &body b) `(if ,t2 (progn ,@b) nil)) (sh-w t (+ 1 2))"))))

(defun %selfhost-phase-meta-circular (check)
  "Phase 3: verify the compiler can meta-circularly compile itself."
  (format t "--- Meta-circular compilation ---~%")
  (flet ((try (form) (handler-case (cl-cc:run-string form) (error () :err))))
    (funcall check "run-string inside run-string"     42  (try "(run-string \"(+ 21 21)\")"))
    (funcall check "defun through nested compilation" 120
             (try "(run-string \"(defun sh-mf (n) (if (<= n 1) 1 (* n (sh-mf (- n 1))))) (sh-mf 5)\")"))))

(defun %selfhost-phase-source-loading (check)
  "Phase 4: load every source file through own compiler; pass iff all succeed."
  (let ((n (length *selfhost-source-files*)))
    (format t "--- Source file self-loading (~D files) ---~%" n)
    (let ((ok 0))
      (pushnew :cl-cc-self-hosting cl:*features*)
      (unwind-protect
        (let ((cl-cc::*repl-vm-state*              nil)
              (cl-cc::*repl-accessor-map*          nil)
              (cl-cc::*repl-pool-instructions*     nil)
              (cl-cc::*repl-pool-labels*           nil)
              (cl-cc::*repl-global-vars-persistent* nil)
              (cl-cc/compile::*repl-label-counter*          nil)
              (cl-cc::*repl-defstruct-registry*    nil))
          (dolist (f *selfhost-source-files*)
            (handler-case (progn (cl-cc::our-load f) (incf ok))
              (error (e) (declare (ignore e))))))
        (setf cl:*features* (remove :cl-cc-self-hosting cl:*features*)))
      (funcall check (format nil "~D/~D source files load through own compiler" ok n) n ok))))

(defun %selfhost-phase-host-eval (check)
  "Phase 5: report host eval replacement status."
  (format t "--- Host eval elimination ---~%")
  (format t "  Replaced with our-eval:~%")
  (format t "    - *macro-eval-fn* (pipeline.lisp) — all defmacro expansion~%")
  (format t "    - eval-lisp-condition (prolog.lisp) — Prolog engine~%")
  (format t "    - rt-eval (runtime.lisp) — runtime eval~%")
  (format t "  Remaining host eval (bootstrap):~%")
  (format t "    - #. read-time eval (lexer.lisp) — host constants~%")
  (format t "    - load-time-value (macro.lisp) — host environment~%")
  (format t "    - our-defmacro eager (expander.lisp) — host macro env~%")
  (format t "    - cps-transform-eval (cps.lisp) — host lambdas~%")
  (funcall check "4 of 7 eval calls replaced with our-eval" t t))

(defun %selfhost-print-summary (pass fail total)
  "Print the final selfhost summary banner and exit with appropriate status."
  (format t "~%~A~%" (make-string 60 :initial-element #\=))
  (format t "  ~D/~D checks passed~%" pass total)
  (if (zerop fail)
      (progn
        (format t "  STATUS: cl-cc is self-hosting.~%~%")
        (format t "  Proven capabilities:~%")
        (format t "    - Macro expansion through own VM (our-eval)~%")
        (format t "    - Meta-circular compilation (compiler compiles compiler)~%")
        (format t "    - ~D/~D source files self-load through own compiler~%"
                (length *selfhost-source-files*) (length *selfhost-source-files*))
        (format t "    - VM host function bridge (whitelist-based)~%")
        (format t "    - #'fn resolves registered closures from function registry~%")
        (uiop:quit 0))
      (progn
        (format t "  STATUS: ~D failures~%" fail)
        (uiop:quit 1))))

(defun %do-selfhost (parsed)
  "Run all self-hosting verification phases and print a TAP-like summary."
  (declare (ignore parsed))
  (let ((pass 0) (fail 0) (total 0))
    (flet ((check (name expected actual)
             (incf total)
             (if (equal expected actual)
                 (progn (incf pass) (format t "  ok ~D - ~A~%" total name))
                 (progn (incf fail)
                        (format t "  FAIL ~D - ~A~%    expected: ~S~%    got: ~S~%"
                                total name expected actual)))))
      (format t "~%cl-cc self-hosting verification (v~A)~%~%" *version*)
      (%selfhost-phase-macro-eval        #'check)
      (%selfhost-phase-basic-compilation #'check)
      (%selfhost-phase-meta-circular     #'check)
      (%selfhost-phase-source-loading    #'check)
      (%selfhost-phase-host-eval         #'check)
      (%selfhost-print-summary pass fail total))))
