;;;; tests/unit/expand/macros-setops-tests.lisp — Set operations macro tests

(in-package :cl-cc/test)

(defsuite macros-setops-suite
  :description "Set operation macro tests"
  :parent cl-cc-unit-suite)

(in-suite macros-setops-suite)

(deftest-each macros-setops-expand-to-let
  "All set-operation macros expand to a LET at the top level."
  :cases (("remove"            '(remove x xs))
          ("member"            '(member x xs))
          ("remove-duplicates" '(remove-duplicates xs))
          ("union"             '(union xs ys))
          ("set-difference"    '(set-difference xs ys))
          ("intersection"      '(intersection xs ys))
          ("subsetp"           '(subsetp xs ys))
          ("adjoin"            '(adjoin x xs))
          ("rassoc"            '(rassoc x alist))
          ("pairlis"           '(pairlis keys data)))
  (form)
  (assert-eq 'let (car (our-macroexpand-1 form))))

;;; ── %keyword-test-args ───────────────────────────────────────────────────

(deftest-each keyword-test-args-cases
  "%keyword-test-args returns :test, :test-not, or nil for three arg patterns."
  :cases (("test-given"     '#'eq nil     '(:test #'eq))
          ("test-not-given" nil '#'equal  '(:test-not #'equal))
          ("neither"        nil nil        nil))
  (test test-not expected)
  (assert-equal expected (cl-cc/expand::%keyword-test-args test test-not)))

;;; ── %keyword-test-key-args ───────────────────────────────────────────────

(deftest-each keyword-test-key-args-cases
  "%keyword-test-key-args returns the correct :test/:test-not/:key argument list."
  :cases (("test-and-key"     '#'eq   nil      '#'car  '(:test #'eq :key #'car))
          ("test-not-and-key" nil     '#'equal '#'cdr  '(:test-not #'equal :key #'cdr))
          ("no-test-no-key"   nil     nil      nil     nil)
          ("test-only"        '#'eql  nil      nil     '(:test #'eql)))
  (test test-not key expected)
  (assert-equal expected (cl-cc/expand::%keyword-test-key-args test test-not key)))

;;; ── %test-predicate-form ─────────────────────────────────────────────────

(deftest-each test-predicate-form-cases
  "%test-predicate-form returns complement for test-not, test itself, or #'eql default."
  :cases (("test-not"  nil '#'equal '(complement #'equal))
          ("test"      '#'eq nil    '#'eq)
          ("default"   nil nil      '#'eql))
  (test test-not expected)
  (assert-equal expected (cl-cc/expand::%test-predicate-form test test-not)))

;;; ── macro runtime behaviour ──────────────────────────────────────────────

(deftest macros-setops-member-finds-element
  "member finds an element in a list using EQL (default)."
  (assert-true (run-string "(member 2 '(1 2 3))")))

(deftest macros-setops-remove-filters-element
  "remove removes all matching elements."
  (assert-equal '(1 3) (run-string "(remove 2 '(1 2 3))")))

(deftest macros-setops-union-no-duplicates
  "union merges two lists without duplicates."
  (let ((result (run-string "(sort (union '(1 2) '(2 3)) #'<)")))
    (assert-equal '(1 2 3) result)))

(deftest macros-setops-intersection-common-elements
  "intersection returns common elements."
  (let ((result (run-string "(sort (intersection '(1 2 3) '(2 3 4)) #'<)")))
    (assert-equal '(2 3) result)))

(deftest macros-setops-set-difference-removes-second-set
  "set-difference removes elements of second list from first."
  (let ((result (run-string "(sort (set-difference '(1 2 3) '(2)) #'<)")))
    (assert-equal '(1 3) result)))
