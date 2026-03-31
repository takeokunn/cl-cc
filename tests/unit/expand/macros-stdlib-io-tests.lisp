;;;; tests/unit/expand/macros-stdlib-io-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib.lisp

(in-package :cl-cc/test)

(defsuite macros-stdlib-io-suite
  :description "Tests for macros-stdlib.lisp: host bridges and file/IO helpers"
  :parent cl-cc-suite)

(in-suite macros-stdlib-io-suite)

(deftest export-is-not-a-macro
  "(export ...) is now a host-bridged function, not a macro"
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
  (let* ((result       (our-macroexpand-1 '(cl-cc::copy-hash-table ht)))
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

(deftest provide-expansion
  "PROVIDE: outer LET, body calls PUSHNEW to register the module."
  (let* ((result       (our-macroexpand-1 '(provide :my-lib)))
         (pushnew-form (caddr result)))
    (assert-eq 'let (car result))
    (assert-eq 'pushnew (car pushnew-form))))

(deftest require-expansion
  "REQUIRE: outer LET, body is UNLESS+MEMBER guard."
  (let* ((result      (our-macroexpand-1 '(require :my-lib)))
         (unless-form (caddr result)))
    (assert-eq 'let (car result))
    (assert-eq 'unless (car unless-form))))
