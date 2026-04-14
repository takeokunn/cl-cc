;;;; tests/unit/expand/macros-filesystem-tests.lisp
;;;; Coverage tests for src/expand/macros-filesystem.lisp

(in-package :cl-cc/test)

(defsuite macros-filesystem-suite
  :description "Tests for macros-filesystem.lisp"
  :parent cl-cc-unit-suite)

(in-suite macros-filesystem-suite)

(deftest with-compilation-unit-expansion
  "WITH-COMPILATION-UNIT is a PROGN stub."
  (let ((result (our-macroexpand-1 '(with-compilation-unit () form1 form2))))
    (assert-eq 'progn (car result))
    (assert-eq 'form1 (second result))
    (assert-eq 'form2 (third result))))

(deftest time-expansion
  "TIME expands to a LET* that measures elapsed universal time."
  (let ((result (our-macroexpand-1 '(time (+ 1 2)))))
    (assert-eq 'let* (car result))
    (assert-eq 'get-universal-time (car (second (first (second result)))))
    (assert-eq 'format (car (third result)))))

(deftest break-expansion
  "BREAK expands to PROGN with a formatted message."
  (let ((result (our-macroexpand-1 '(break "oops ~A" x))))
    (assert-eq 'progn (car result))
    (assert-eq 'format (car (second result)))))

(deftest file-string-length-expansion
  "FILE-STRING-LENGTH uses a PROGN wrapper and character dispatch."
  (let ((result (our-macroexpand-1 '(file-string-length stream object))))
    (assert-eq 'progn (car result))
    (assert-eq 'let (car (third result)))
    (assert-eq 'if (car (third (third result))))))

(deftest stream-external-format-expansion
  "STREAM-EXTERNAL-FORMAT returns :DEFAULT through PROGN."
  (let ((result (our-macroexpand-1 '(stream-external-format stream))))
    (assert-eq 'progn (car result))
    (assert-eq :default (third result))))

(deftest compile-file-expansion
  "COMPILE-FILE loads the pathname and returns probe-file results."
  (let ((result (our-macroexpand-1 '(compile-file file))))
    (assert-eq 'let (car result))
    (assert-eq (find-symbol "OUR-LOAD" :cl-cc) (car (third result)))
    (assert-eq 'values (car (fourth result)))))

(deftest readtablep-stub-expansion
  "READTABLEP is a simple PROGN stub returning NIL."
  (let ((result (our-macroexpand-1 '(readtablep rt))))
    (assert-eq 'progn (car result))
    (assert-eq nil (third result))))

(deftest set-macro-character-stub-expansion
  "SET-MACRO-CHARACTER stub returns T as its sixth form element."
  (let ((result (our-macroexpand-1 '(set-macro-character #\a fn))))
    (assert-eq t (sixth result))))
