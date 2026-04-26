;;;; selfhost-meta-tests.lisp — Meta-circular compilation, our-load, and all-source-files self-hosting tests
(in-package :cl-cc/test)

(in-suite selfhost-suite)

;;; ─── Self-Hosting: Meta-Circular Compilation ─────────────────────────────

(deftest selfhost-meta-circular-eval
  "cl-cc compiles nested expressions through host-level double compilation."
  :timeout 30
  (assert-eql 42
    (run-string (format nil "~A" (run-string "(+ 21 21)")))))

(deftest selfhost-meta-circular-defun
  "cl-cc compiles recursive factorial through REPL state."
  :timeout 30
  (assert-eql 120
    (run-repl-forms
     "(defun sh-meta-f (n) (if (<= n 1) 1 (* n (sh-meta-f (- n 1)))))"
     "(sh-meta-f 5)")))

(deftest selfhost-meta-circular-closure
  "cl-cc compiles a closure that captures a let-binding."
  :timeout 30
  (assert-eql 15
    (run-string "(let ((x 10)) (funcall (lambda (y) (+ x y)) 5))")))

;;; ─── Self-Hosting: True Meta-Circular (VM calls run-string) ─────────────
;;; These tests prove that code running in cl-cc's VM can invoke cl-cc's own
;;; compiler (run-string) via the host function bridge.  This is the key
;;; demonstration of meta-circular compilation: the compiler compiles code
;;; that invokes the compiler.

(deftest selfhost-meta-circular-compilation
  "VM code invokes cl-cc's own compiler via host bridge (true meta-circular compilation)."
  :timeout 30
  (assert-eql 42
    (run-string "(run-string \"(+ 21 21)\")"))
  (assert-eql 120
    (run-string
     "(run-string \"(defun sh-meta-fact (n) (if (<= n 1) 1 (* n (sh-meta-fact (- n 1))))) (sh-meta-fact 5)\")"))
  (assert-eql 15
    (run-string
     "(run-string \"(let ((x 10)) (funcall (lambda (y) (+ x y)) 5))\")")))

;;; ─── Self-Hosting: Load File + Use Definitions ──────────────────────────

(defun run-load-and-eval (file-path &rest forms)
  "Load FILE-PATH through our-load, then evaluate FORMS in the same REPL state.
  Returns the result of the last form."
  (cl-cc:with-fresh-repl-state
    (cl-cc::our-load file-path)
    (let ((result nil))
      (dolist (form forms result)
        (setf result (run-string-repl form))))))

(defmacro %with-tmpfile ((var prefix content) &body body)
  "Write CONTENT to a unique temp file, bind VAR to its path, then cleanup."
  `(let ((,var (format nil "/tmp/cl-cc-~A-~A.lisp" ,prefix (get-universal-time))))
     (unwind-protect
         (progn
           (with-open-file (s ,var :direction :output :if-exists :supersede)
             (write-string ,content s))
           ,@body)
       (ignore-errors (delete-file ,var)))))

(deftest selfhost-load-and-use-defs
  "Load a file with defvar+defun through our-load, then use its definitions."
  :timeout 30
  (%with-tmpfile (tmpfile "sh-defs"
      "(defvar *sh-greeting* \"hello\")
(defun sh-greet (name) (list *sh-greeting* name))")
    (assert-equal '("hello" "world")
      (run-load-and-eval tmpfile "(sh-greet \"world\")"))))

(in-suite selfhost-slow-suite)

(deftest selfhost-load-and-use-recursion
  "Load a file with recursive functions through our-load, then call them."
  :timeout 30
  (%with-tmpfile (tmpfile "sh-rec"
      "(defun sh-fib (n)
  (if (<= n 1) n
      (+ (sh-fib (- n 1)) (sh-fib (- n 2)))))
(defun sh-ack (m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (sh-ack (- m 1) 1))
        (t (sh-ack (- m 1) (sh-ack m (- n 1))))))")
    (assert-eql 13 (run-load-and-eval tmpfile "(sh-fib 7)"))
    (assert-eql 7  (run-load-and-eval tmpfile "(sh-ack 2 2)"))))

(in-suite selfhost-suite)

(deftest selfhost-load-chain
  "Load two files sequentially, second file uses first file's definitions."
  :timeout 30
  (%with-tmpfile (file1 "sh-chain1"
      "(defvar *sh-base* 1000)
(defun sh-offset (n) (+ *sh-base* n))")
    (%with-tmpfile (file2 "sh-chain2"
        "(defun sh-combined (a b)
  (+ (sh-offset a) (sh-offset b)))")
      (cl-cc:with-fresh-repl-state
        (cl-cc::our-load file1)
        (cl-cc::our-load file2)
        (assert-eql 2003
          (run-string-repl "(sh-combined 1 2)"))))))

;;; ─── All Source Files for Phase 4 ─────────────────────────────────────────

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

(defparameter *selfhost-all-source-files*
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
"packages/umbrella/pipeline/src/pipeline-repl-load.lisp"))
  "All source files in dependency order for Phase 4 self-hosting verification.")

;;; ─── Self-Hosting: Source File Loading ────────────────────────────────────

(defvar *selfhost-representative-files*
  '("packages/frontend/parse/src/cst.lisp"
    "packages/foundation/prolog/src/prolog-data.lisp"  ; *builtin-predicate-specs* used by prolog.lisp
    "packages/foundation/prolog/src/prolog.lisp"
    "packages/frontend/parse/src/lexer.lisp"
    "packages/engine/compile/src/cps.lisp"
    "packages/engine/optimize/src/optimizer.lisp"
    "packages/foundation/type/src/package.lisp"
    "packages/foundation/type/src/kind.lisp"
    "packages/foundation/type/src/multiplicity.lisp"   ; prerequisites for types-core
    "packages/foundation/type/src/types-core.lisp"     ; base type structs
    "packages/foundation/type/src/types-extended.lisp" ; type-null/type-int/+pure-effect-row+ etc.
    "packages/foundation/type/src/types-env.lisp"      ; cl-cc/type::+type-unknown+
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

(in-suite selfhost-slow-suite)

(deftest selfhost-load-own-source
  "cl-cc can load a representative subset of its own source files."
  :timeout 30
  (let ((ok 0))
    (cl-cc:with-fresh-repl-state
      (dolist (f *selfhost-representative-files*)
        (handler-case
          (progn (cl-cc::our-load f) (incf ok))
          (error (e)
            (declare (ignore e))))))
    (assert-eql (length *selfhost-representative-files*) ok)))

(in-suite selfhost-suite)

;;; ─── Phase 1: Macro Eval Through Own VM ───────────────────────────────────

(deftest selfhost-macro-eval-fn
  "Macro expansion routes through own VM (our-eval), not host eval."
  :timeout 10
  (assert-true (functionp cl-cc:*macro-eval-fn*))
  (assert-false (eq cl-cc:*macro-eval-fn* #'eval))
  (assert-eql 3 (funcall cl-cc:*macro-eval-fn* '(+ 1 2))))

(in-suite selfhost-slow-suite)

;;; ─── Phase 4: All Source Files Self-Load ──────────────────────────────────

(deftest selfhost-all-source-files
  "cl-cc loads all its own source files through own compiler."
  :timeout 300
  (let ((n (length *selfhost-all-source-files*))
        (ok 0))
    (pushnew :cl-cc-self-hosting cl:*features*)
    (unwind-protect
        (cl-cc:with-fresh-repl-state
          (let ((cl-cc:*skip-optimizer-passes* t))
            (dolist (f *selfhost-all-source-files*)
              (handler-case (progn (cl-cc::our-load f) (incf ok))
                (error (e) (declare (ignore e)))))))
      (setf cl:*features* (remove :cl-cc-self-hosting cl:*features*)))
    (assert-eql n ok)))
