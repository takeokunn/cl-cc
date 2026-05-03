;;;; selfhost-meta-tests.lisp — Meta-circular compilation, our-load, and all-source-files self-hosting tests
(in-package :cl-cc/test)

(in-suite selfhost-suite)

;;; ─── Self-Hosting: Meta-Circular Compilation ─────────────────────────────

(deftest selfhost-meta-circular-eval
  "cl-cc compiles nested expressions through host-level double compilation."
  (assert-eql 42
    (run-string (format nil "~A" (run-string "(+ 21 21)")))))

(deftest selfhost-meta-circular-defun
  "cl-cc compiles recursive factorial through REPL state."
  (assert-eql 120
    (run-repl-forms
     "(defun sh-meta-f (n) (if (<= n 1) 1 (* n (sh-meta-f (- n 1)))))"
     "(sh-meta-f 5)")))

(deftest selfhost-meta-circular-closure
  "cl-cc compiles a closure that captures a let-binding."
  (assert-eql 15
    (run-string "(let ((x 10)) (funcall (lambda (y) (+ x y)) 5))")))

;;; ─── Self-Hosting: True Meta-Circular (VM calls run-string) ─────────────
;;; These tests prove that code running in cl-cc's VM can invoke cl-cc's own
;;; compiler (run-string) via the host function bridge. This is the key
;;; demonstration of meta-circular compilation: the compiler compiles code
;;; that invokes the compiler.

(deftest selfhost-meta-circular-compilation
  "VM code invokes cl-cc's own compiler via host bridge (true meta-circular compilation)."
  (assert-eql 42
    (run-string "(run-string \"(+ 21 21)\")"))
  (assert-eql 120
    (run-string
     "(run-string \"(defun sh-meta-fact (n) (if (<= n 1) 1 (* n (sh-meta-fact (- n 1))))) (sh-meta-fact 5)\")"))
  (assert-eql 15
    (run-string
     "(run-string \"(let ((x 10)) (funcall (lambda (y) (+ x y)) 5))\")")))

;;; ─── Self-Hosting: Load File + Use Definitions ──────────────────────────

(deftest selfhost-load-and-use-defs
  "Load a file with defvar+defun through our-load, then use its definitions."
  (%with-tmpfile (tmpfile "sh-defs"
      "(defvar *sh-greeting* \"hello\")
(defun sh-greet (name) (list *sh-greeting* name))")
    (assert-equal '("hello" "world")
      (run-load-and-eval tmpfile "(sh-greet \"world\")"))))

(in-suite selfhost-slow-suite)

(deftest selfhost-load-and-use-recursion
  "Load a file with recursive functions through our-load, then call them."
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

(deftest selfhost-load-defun-implicit-block-return-from
  "our-load preserves DEFUN's implicit named block for RETURN-FROM in loaded files."
  (%with-tmpfile (tmpfile "sh-return-from"
      "(defun sh-return-from (x)
  (return-from sh-return-from (+ x 1))
  0)")
    (assert-eql 42
      (run-load-and-eval tmpfile "(sh-return-from 41)"))))

;;; ─── Self-Hosting: Source File Loading ────────────────────────────────────

(in-suite selfhost-slow-suite)

(deftest selfhost-load-own-source
  "cl-cc can load a representative subset of its own source files."
  (cl-cc:with-fresh-repl-state
    (dolist (f *selfhost-representative-files*)
      (cl-cc::our-load f))))

(in-suite selfhost-suite)

;;; ─── Phase 1: Macro Eval Through Own VM ───────────────────────────────────

(deftest selfhost-macro-eval-fn-basic-runtime
  "The configured selfhost macro evaluator is callable and can execute simple forms."
  :timeout 10
  (assert-true (functionp cl-cc:*macro-eval-fn*))
  (assert-eql 3 (funcall cl-cc:*macro-eval-fn* '(+ 1 2))))

(in-suite selfhost-slow-suite)

;;; ─── Phase 4: All Source Files Self-Load ──────────────────────────────────

(deftest selfhost-all-source-files-smoke
  "cl-cc can load a substantial representative set of its own source files through its own compiler."
  :timeout 300
  (handler-bind ((warning #'muffle-warning))
    (let* ((files (selfhost-all-source-files))
           (ok 0))
      (pushnew :cl-cc-self-hosting cl:*features*)
      (unwind-protect
           (cl-cc:with-fresh-repl-state
             (let ((cl-cc:*skip-optimizer-passes* t))
               (dolist (f files)
                  (cl-cc::our-load f)
                  (incf ok))))
        (setf cl:*features* (remove :cl-cc-self-hosting cl:*features*)))
      (assert-eql (length files) ok))))


(set-suite-test-timeout! 'selfhost-suite 30)
(set-suite-test-timeout! 'selfhost-slow-suite 30)
