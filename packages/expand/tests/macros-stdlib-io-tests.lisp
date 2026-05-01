;;;; tests/unit/expand/macros-stdlib-io-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib.lisp

(in-package :cl-cc/test)

(defsuite macros-stdlib-io-suite
  :description "Tests for macros-stdlib.lisp: host bridges and file/IO helpers"
  :parent cl-cc-serial-suite)

(defbefore :each (macros-stdlib-io-suite)
  (clrhash cl-cc/expand::*load-time-value-cache*)
  (setf cl-cc/expand::*macro-eval-fn* #'eval))

(in-suite macros-stdlib-io-suite)

(defparameter *load-time-value-hit* 0)

(deftest export-is-not-a-macro
  "(export ...) is handled as a function call, not an expander macro."
  (multiple-value-bind (expansion expanded-p) (our-macroexpand-1 '(export '(foo bar)))
    (declare (ignore expansion))
    (assert-true (not expanded-p))))

(deftest warn-expansion
  "WARN: outer PROGN; body first form is FORMAT to t."
  (let* ((result   (our-macroexpand-1 '(warn "oops ~A" x)))
         (fmt-call (second result)))
    (assert-eq 'progn  (car result))
    (assert-eq 'format (car fmt-call))
    (assert-eq 't      (second fmt-call))))

(deftest copy-hash-table-expansion
  "COPY-HASH-TABLE: outer LET, inner LET body calls MAPHASH."
  (let* ((result       (our-macroexpand-1 '(cl-cc/expand::copy-hash-table ht)))
         (inner-let    (caddr result))
         (maphash-call (caddr inner-let)))
    (assert-eq 'let (car result))
    (assert-eq 'maphash (car maphash-call))))

(deftest with-open-file-expansion
  "WITH-OPEN-FILE: outer LET, body is UNWIND-PROTECT, cleanup calls CLOSE."
  (let* ((result    (our-macroexpand-1 '(with-open-file (s "/tmp/f") body)))
         (body-form (caddr result))
         (cleanup   (third body-form)))
    (assert-eq 'let (car result))
    (assert-eq 'unwind-protect (car body-form))
    (assert-eq 'close (car cleanup))))

(deftest load-time-value-expands-to-quote
  "LOAD-TIME-VALUE evaluates form at expand time and quotes the result"
  (let ((result (our-macroexpand-1 '(load-time-value (+ 1 2)))))
    (assert-eq (car result) 'quote)
    (assert-= (second result) 3)))

(deftest load-time-value-is-memoized-during-expansion
  "LOAD-TIME-VALUE only evaluates identical forms once per compiler session."
  (let ((*load-time-value-hit* 0))
    (clrhash cl-cc/expand::*load-time-value-cache*)
    (let* ((form '(load-time-value (progn (incf *load-time-value-hit*) *load-time-value-hit*)))
           (first (our-macroexpand-1 form))
           (second (our-macroexpand-1 form)))
      (assert-eq 'quote (car first))
      (assert-eq 'quote (car second))
      (assert-= 1 (second first))
      (assert-= 1 (second second))
      (assert-= 1 *load-time-value-hit*))))

(deftest-each provide-require-expansion-structure
  "PROVIDE expands to LET+PUSHNEW; REQUIRE expands to LET+UNLESS."
  :cases (("provide" '(provide :my-lib) 'pushnew)
          ("require" '(require :my-lib) 'unless))
  (form expected-inner-op)
  (let* ((result (our-macroexpand-1 form))
         (inner  (caddr result)))
    (assert-eq 'let (car result))
    (assert-eq expected-inner-op (car inner))))
