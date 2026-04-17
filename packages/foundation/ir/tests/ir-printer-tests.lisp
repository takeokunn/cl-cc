;;;; tests/unit/compile/ir/ir-printer-tests.lisp — Extended IR Printer Tests
;;;;
;;;; Tests for src/compile/ir/printer.lisp:
;;;; ir-format-value, ir-print-block, ir-print-function, ir-print-module,
;;;; ir-function-to-string — multi-block, parameterized blocks, modules.

(in-package :cl-cc/test)

(defsuite ir-printer-suite :description "Extended IR printer tests"
  :parent cl-cc-unit-suite)


(in-suite ir-printer-suite)
;;; ─── ir-format-value ──────────────────────────────────────────────────────────

(deftest ir-fmt-value-cases
  "ir-format-value: sequential %N ids, string literals, and symbol case."
  ;; sequential ids
  (let* ((fn (cl-cc/ir:ir-make-function 'test))
         (v0 (cl-cc/ir:ir-new-value fn))
         (v1 (cl-cc/ir:ir-new-value fn))
         (v2 (cl-cc/ir:ir-new-value fn)))
    (assert-equal "%0" (cl-cc/ir:ir-format-value v0))
    (assert-equal "%1" (cl-cc/ir:ir-format-value v1))
    (assert-equal "%2" (cl-cc/ir:ir-format-value v2)))
  ;; string literal contains original text
  (assert-true (search "hello" (cl-cc/ir:ir-format-value "hello")))
  ;; symbol printed in uppercase
  (assert-true (search "FOO" (cl-cc/ir:ir-format-value 'foo))))

;;; ─── ir-print-block ───────────────────────────────────────────────────────────

(deftest ir-print-block-variations
  "ir-print-block: no-preds annotation, predecessor listing, params, instructions, terminator."
  (let* ((fn    (cl-cc/ir:ir-make-function 'test))
         (entry (cl-cc/ir:irf-entry fn))
         (then  (cl-cc/ir:ir-new-block fn :then))
         (join  (cl-cc/ir:ir-new-block fn :join))
         (v     (cl-cc/ir:ir-new-value fn))
         (v0    (cl-cc/ir:ir-new-value fn))
         (inst  (cl-cc/ir:make-ir-inst :result v0))
         (term  (cl-cc/ir:make-ir-inst)))
    ;; no predecessors shows (none)
    (let ((s (with-output-to-string (out) (cl-cc/ir:ir-print-block entry out))))
      (assert-true (search "(none)" s)))
    ;; block with predecessor lists it
    (cl-cc/ir:ir-add-edge entry then)
    (let ((s (with-output-to-string (out) (cl-cc/ir:ir-print-block then out))))
      (assert-true (search "entry" s)))
    ;; block with params shows label and param
    (push v (cl-cc/ir:irb-params join))
    (let ((s (with-output-to-string (out) (cl-cc/ir:ir-print-block join out))))
      (assert-true (search "join" s))
      (assert-true (search (cl-cc/ir:ir-format-value v) s)))
    ;; block with instruction shows result value
    (cl-cc/ir:ir-emit entry inst)
    (let ((s (with-output-to-string (out) (cl-cc/ir:ir-print-block entry out))))
      (assert-true (search "%1" s)))
    ;; block with terminator shows its type name
    (cl-cc/ir:ir-set-terminator entry term)
    (let ((s (with-output-to-string (out) (cl-cc/ir:ir-print-block entry out))))
      (assert-true (search "IR-INST" s)))))

;;; ─── ir-print-function / ir-function-to-string ────────────────────────────────

(deftest ir-print-function-aspects
  "ir-function-to-string: define keyword, return type, anonymous, params, multiblock, closing brace."
  ;; starts with define
  (let* ((fn (cl-cc/ir:ir-make-function 'add))
         (s  (cl-cc/ir:ir-function-to-string fn)))
    (assert-true (search "define" s))
    (assert-true (search "}" s)))
  ;; includes return type
  (let* ((fn (cl-cc/ir:ir-make-function 'add :return-type :integer))
         (s  (cl-cc/ir:ir-function-to-string fn)))
    (assert-true (search "INTEGER" s)))
  ;; nil name → anonymous
  (let* ((fn (cl-cc/ir:ir-make-function nil))
         (s  (cl-cc/ir:ir-function-to-string fn)))
    (assert-true (search "anonymous" s)))
  ;; function parameters
  (let* ((fn (cl-cc/ir:ir-make-function 'f))
         (p0 (cl-cc/ir:ir-new-value fn))
         (p1 (cl-cc/ir:ir-new-value fn)))
    (setf (cl-cc/ir:irf-params fn) (list p0 p1))
    (let ((s (cl-cc/ir:ir-function-to-string fn)))
      (assert-true (search "%0" s))
      (assert-true (search "%1" s))))
  ;; all reachable blocks present
  (let* ((fn    (cl-cc/ir:ir-make-function 'branch))
         (entry (cl-cc/ir:irf-entry fn))
         (then  (cl-cc/ir:ir-new-block fn :then))
         (else  (cl-cc/ir:ir-new-block fn :else)))
    (cl-cc/ir:ir-add-edge entry then)
    (cl-cc/ir:ir-add-edge entry else)
    (let ((s (cl-cc/ir:ir-function-to-string fn)))
      (assert-true (search "entry" s))
      (assert-true (search "then" s))
      (assert-true (search "else" s)))))

;;; ─── ir-print-module ──────────────────────────────────────────────────────────

(deftest ir-print-module-cases
  "ir-print-module: empty header, single function, multiple functions in order."
  ;; empty module shows header with 0
  (let* ((mod (cl-cc/ir:make-ir-module :functions nil))
         (s   (with-output-to-string (out) (cl-cc/ir:ir-print-module mod out))))
    (assert-true (search "IR Module" s))
    (assert-true (search "0" s)))
  ;; single function included
  (let* ((fn  (cl-cc/ir:ir-make-function 'my-fn))
         (mod (cl-cc/ir:make-ir-module :functions (list fn)))
         (s   (with-output-to-string (out) (cl-cc/ir:ir-print-module mod out))))
    (assert-true (search "1 function" s))
    (assert-true (search "my-fn" s)))
  ;; multiple functions printed in order
  (let* ((f1  (cl-cc/ir:ir-make-function 'alpha))
         (f2  (cl-cc/ir:ir-make-function 'beta))
         (mod (cl-cc/ir:make-ir-module :functions (list f1 f2)))
         (s   (with-output-to-string (out) (cl-cc/ir:ir-print-module mod out))))
    (assert-true (search "2 functions" s))
    (let ((pos-a (search "alpha" s))
          (pos-b (search "beta" s)))
      (assert-true pos-a)
      (assert-true pos-b)
      (assert-true (< pos-a pos-b)))))
