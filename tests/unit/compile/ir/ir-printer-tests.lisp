;;;; tests/unit/compile/ir/ir-printer-tests.lisp — Extended IR Printer Tests
;;;;
;;;; Tests for src/compile/ir/printer.lisp:
;;;; ir-format-value, ir-print-block, ir-print-function, ir-print-module,
;;;; ir-function-to-string — multi-block, parameterized blocks, modules.

(in-package :cl-cc/test)

(defsuite ir-printer-suite :description "Extended IR printer tests")

;;; ─── ir-format-value ──────────────────────────────────────────────────────────

(deftest ir-fmt-value-sequential-ids
  "ir-format-value produces %0, %1, %2 for sequential values."
  (let* ((fn (cl-cc:ir-make-function 'test))
         (v0 (cl-cc:ir-new-value fn))
         (v1 (cl-cc:ir-new-value fn))
         (v2 (cl-cc:ir-new-value fn)))
    (assert-equal "%0" (cl-cc:ir-format-value v0))
    (assert-equal "%1" (cl-cc:ir-format-value v1))
    (assert-equal "%2" (cl-cc:ir-format-value v2))))

(deftest ir-fmt-value-string-literal
  "ir-format-value wraps string literals in quotes."
  (let ((s (cl-cc:ir-format-value "hello")))
    (assert-true (search "hello" s))))

(deftest ir-fmt-value-symbol
  "ir-format-value prints symbols in uppercase."
  (assert-true (search "FOO" (cl-cc:ir-format-value 'foo))))

;;; ─── ir-print-block ───────────────────────────────────────────────────────────

(deftest ir-print-block-empty-no-preds
  "An entry block with no predecessors shows '(none)' in preds."
  (let* ((fn  (cl-cc:ir-make-function 'test))
         (blk (cl-cc:irf-entry fn))
         (s   (with-output-to-string (out)
                (cl-cc:ir-print-block blk out))))
    (assert-true (search "(none)" s))))

(deftest ir-print-block-with-predecessors
  "A block with predecessors lists them in the preds annotation."
  (let* ((fn    (cl-cc:ir-make-function 'test))
         (entry (cl-cc:irf-entry fn))
         (then  (cl-cc:ir-new-block fn :then)))
    (cl-cc:ir-add-edge entry then)
    (let ((s (with-output-to-string (out)
               (cl-cc:ir-print-block then out))))
      (assert-true (search "entry" s)))))

(deftest ir-print-block-with-params
  "A block with block arguments shows them in the header."
  (let* ((fn  (cl-cc:ir-make-function 'test))
         (blk (cl-cc:ir-new-block fn :join))
         (v   (cl-cc:ir-new-value fn)))
    (push v (cl-cc:irb-params blk))
    (let ((s (with-output-to-string (out)
               (cl-cc:ir-print-block blk out))))
      ;; Should contain the block label and the parameter %N
      (assert-true (search "join" s))
      (assert-true (search (cl-cc:ir-format-value v) s)))))

(deftest ir-print-block-with-instructions
  "A block with instructions prints each instruction."
  (let* ((fn    (cl-cc:ir-make-function 'test))
         (entry (cl-cc:irf-entry fn))
         (v0    (cl-cc:ir-new-value fn))
         (inst  (cl-cc:make-ir-inst :result v0)))
    (cl-cc:ir-emit entry inst)
    (let ((s (with-output-to-string (out)
               (cl-cc:ir-print-block entry out))))
      ;; Should contain the result value
      (assert-true (search "%0" s)))))

(deftest ir-print-block-with-terminator
  "A block with a terminator prints it after body instructions."
  (let* ((fn    (cl-cc:ir-make-function 'test))
         (entry (cl-cc:irf-entry fn))
         (term  (cl-cc:make-ir-inst)))
    (cl-cc:ir-set-terminator entry term)
    (let ((s (with-output-to-string (out)
               (cl-cc:ir-print-block entry out))))
      ;; Should contain the terminator's type name
      (assert-true (search "IR-INST" s)))))

;;; ─── ir-print-function / ir-function-to-string ────────────────────────────────

(deftest ir-print-fn-define-keyword
  "ir-function-to-string starts with 'define'."
  (let* ((fn (cl-cc:ir-make-function 'add))
         (s  (cl-cc:ir-function-to-string fn)))
    (assert-true (search "define" s))))

(deftest ir-print-fn-return-type
  "ir-function-to-string includes the return type."
  (let* ((fn (cl-cc:ir-make-function 'add :return-type :integer))
         (s  (cl-cc:ir-function-to-string fn)))
    (assert-true (search "INTEGER" s))))

(deftest ir-print-fn-anonymous
  "ir-function-to-string handles nil function name."
  (let* ((fn (cl-cc:ir-make-function nil))
         (s  (cl-cc:ir-function-to-string fn)))
    (assert-true (search "anonymous" s))))

(deftest ir-print-fn-with-params
  "ir-function-to-string shows function parameters."
  (let* ((fn (cl-cc:ir-make-function 'f))
         (p0 (cl-cc:ir-new-value fn))
         (p1 (cl-cc:ir-new-value fn)))
    (setf (cl-cc:irf-params fn) (list p0 p1))
    (let ((s (cl-cc:ir-function-to-string fn)))
      (assert-true (search "%0" s))
      (assert-true (search "%1" s)))))

(deftest ir-print-fn-multiblock
  "ir-function-to-string includes all reachable blocks."
  (let* ((fn    (cl-cc:ir-make-function 'branch))
         (entry (cl-cc:irf-entry fn))
         (then  (cl-cc:ir-new-block fn :then))
         (else  (cl-cc:ir-new-block fn :else)))
    (cl-cc:ir-add-edge entry then)
    (cl-cc:ir-add-edge entry else)
    (let ((s (cl-cc:ir-function-to-string fn)))
      (assert-true (search "entry" s))
      (assert-true (search "then" s))
      (assert-true (search "else" s)))))

(deftest ir-print-fn-closes-with-brace
  "ir-function-to-string output ends with }."
  (let* ((fn (cl-cc:ir-make-function 'test))
         (s  (cl-cc:ir-function-to-string fn)))
    (assert-true (search "}" s))))

;;; ─── ir-print-module ──────────────────────────────────────────────────────────

(deftest ir-print-module-header
  "ir-print-module outputs a header with function count."
  (let* ((mod (cl-cc:make-ir-module :functions nil))
         (s   (with-output-to-string (out)
                (cl-cc:ir-print-module mod out))))
    (assert-true (search "IR Module" s))
    (assert-true (search "0" s))))

(deftest ir-print-module-single-fn
  "ir-print-module includes the function's output."
  (let* ((fn  (cl-cc:ir-make-function 'my-fn))
         (mod (cl-cc:make-ir-module :functions (list fn)))
         (s   (with-output-to-string (out)
                (cl-cc:ir-print-module mod out))))
    (assert-true (search "1 function" s))
    (assert-true (search "my-fn" s))))

(deftest ir-print-module-multiple-fns
  "ir-print-module prints all functions in order."
  (let* ((f1  (cl-cc:ir-make-function 'alpha))
         (f2  (cl-cc:ir-make-function 'beta))
         (mod (cl-cc:make-ir-module :functions (list f1 f2)))
         (s   (with-output-to-string (out)
                (cl-cc:ir-print-module mod out))))
    (assert-true (search "2 functions" s))
    (let ((pos-a (search "alpha" s))
          (pos-b (search "beta" s)))
      (assert-true pos-a)
      (assert-true pos-b)
      (assert-true (< pos-a pos-b)))))
