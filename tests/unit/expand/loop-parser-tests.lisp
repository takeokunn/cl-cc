;;;; tests/unit/expand/loop-parser-tests.lisp — LOOP parser layer tests

(in-package :cl-cc/test)

(defsuite loop-parser-suite
  :description "LOOP parser helpers and clause dispatch"
  :parent cl-cc-unit-suite)

(in-suite loop-parser-suite)

(deftest loop-kw-p-is-case-insensitive
  "loop-kw-p matches symbols case-insensitively by name."
  (assert-true (cl-cc::loop-kw-p 'for "FOR"))
  (assert-true (cl-cc::loop-kw-p 'FoR "for"))
  (assert-false (cl-cc::loop-kw-p 'collect "FOR")))

(deftest loop-kw-member-p-recognizes-boundaries
  "loop-kw-member-p recognizes LOOP boundary keywords."
  (assert-true (cl-cc::loop-kw-member-p 'collect))
  (assert-true (cl-cc::loop-kw-member-p 'finally))
  (assert-false (cl-cc::loop-kw-member-p 'foo)))

(deftest loop-finalize-state-normalizes-reversed-lists
  "finalize-loop-state restores accumulated fields to source order."
  (let ((state (cl-cc::make-loop-parse-state)))
    (setf (cl-cc::lps-iterations state) '(b a)
          (cl-cc::lps-body-forms state) '((body-2) (body-1))
          (cl-cc::lps-accumulations state) '((:collect 2 nil nil) (:collect 1 nil nil))
          (cl-cc::lps-conditions state) '((:while b) (:while a))
          (cl-cc::lps-initially state) '((init-2) (init-1))
          (cl-cc::lps-finally state) '((fin-2) (fin-1))
          (cl-cc::lps-loop-name state) 'demo)
    (let ((result (cl-cc::finalize-loop-state state)))
      (assert-equal '(a b) (getf result :iterations))
      (assert-equal '((body-2) (body-1)) (getf result :body))
      (assert-equal '((:collect 1 nil nil) (:collect 2 nil nil))
                    (getf result :accumulations))
      (assert-equal '((:while a) (:while b)) (getf result :conditions))
      (assert-equal '((init-1) (init-2)) (getf result :initially))
      (assert-equal '((fin-1) (fin-2)) (getf result :finally))
      (assert-eq 'demo (getf result :loop-name)))))

(deftest loop-parse-clauses-dispatches-core-clauses
  "parse-loop-clauses builds a plist from representative LOOP clauses."
  (let ((result (cl-cc::parse-loop-clauses
                 '(named demo
                   do (print :hello)
                   collect x into xs
                   finally (list :done)))))
    (assert-eq 'demo (getf result :loop-name))
    (assert-equal '((:collect x xs nil)) (getf result :accumulations))
    (assert-equal '((print :hello)) (getf result :body))
    (assert-equal '((list :done)) (getf result :finally))))
