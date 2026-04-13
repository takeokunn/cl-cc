;;;; tests/unit/expand/predicate-tests.lisp — Predicate tests

(in-package :cl-cc/test)

(defsuite predicate-suite
  :description "Predicate macro expansion tests"
  :parent cl-cc-integration-suite)

(in-suite predicate-suite)

(deftest-each predicate-not-delegates-via-complement
  "Each -NOT predicate expands to the base form with (complement pred) as the first arg."
  :cases (("find-if-not"     'find-if     '(find-if-not pred lst))
          ("position-if-not" 'position-if '(position-if-not pred lst))
          ("count-if-not"    'count-if    '(count-if-not pred lst))
          ("rassoc-if-not"   'rassoc-if   '(rassoc-if-not pred alist))
          ("assoc-if-not"    'assoc-if    '(assoc-if-not pred alist)))
  (base-op form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq base-op (car result))
    (assert-eq 'complement (caadr result))))

(deftest-each find-if-not-runtime
  "FIND-IF-NOT returns first element not satisfying predicate; nil when all satisfy."
  :cases (("found"     "(find-if-not #'oddp '(1 3 4 5 6))"  4)
          ("not-found" "(find-if-not #'numberp '(1 2 3))"   nil))
  (form expected)
  (assert-equal expected (run-string form)))

(deftest position-if-expansion-structure
  "POSITION-IF expands to LET; body is BLOCK NIL for early RETURN."
  (let* ((result (our-macroexpand-1 '(position-if pred lst)))
         (body   (caddr result)))
    (assert-eq 'let   (car result))
    (assert-eq 'block (car body))
    (assert-eq nil    (second body))))

(deftest-each position-if-runtime
  "POSITION-IF returns the 0-based index of first matching element; nil when not found."
  :cases (("found"     "(position-if #'evenp '(1 3 4 7 8))"  2)
          ("not-found" "(position-if #'evenp '(1 3 5))"       nil))
  (form expected)
  (assert-equal expected (run-string form)))


(deftest-each position-if-not-runtime
  "POSITION-IF-NOT returns index of first element not satisfying predicate; nil when all satisfy."
  :cases (("found"     "(position-if-not #'oddp '(1 3 4 5))"  2)
          ("not-found" "(position-if-not #'oddp '(1 3 5))"    nil))
  (form expected)
  (assert-equal expected (run-string form)))


(deftest-each count-if-not-runtime
  "COUNT-IF-NOT counts elements not satisfying predicate."
  :cases (("some"  "(count-if-not #'oddp '(1 2 3 4 5))"   2)
          ("empty" "(count-if-not #'numberp '())"           0))
  (form expected)
  (assert-= expected (run-string form)))

(deftest-each remove-if-key-expansion
  "REMOVE-IF and REMOVE-IF-NOT with :key both expand to a multi-binding LET form."
  :cases (("remove-if"     '(remove-if #'oddp lst :key #'car))
          ("remove-if-not" '(remove-if-not #'evenp lst :key #'car)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq 'let (car result))
    (assert-true (> (length (cadr result)) 1))))

(deftest find-if-not-with-key
  "FIND-IF-NOT with :key delegates to FIND-IF with complement."
  (let ((result (our-macroexpand-1 '(find-if-not #'oddp lst :key #'car))))
    (assert-eq (car result) 'find-if)))

(deftest position-if-with-key
  "POSITION-IF with :key applies key before predicate."
  (let ((result (our-macroexpand-1 '(position-if #'oddp lst :key #'car))))
    (assert-eq (car result) 'let)
    (assert-true (> (length (cadr result)) 2))))

(deftest count-if-not-with-key
  "COUNT-IF-NOT with :key delegates to COUNT-IF with complement."
  (let ((result (our-macroexpand-1 '(count-if-not #'oddp lst :key #'car))))
    (assert-eq (car result) 'count-if)))

(deftest-each predicate-if-outer-is-let
  "RASSOC-IF and ASSOC-IF both expand to LET forms binding the predicate."
  :cases (("rassoc-if" '(rassoc-if pred alist))
          ("assoc-if"  '(assoc-if pred alist)))
  (form)
  (assert-eq 'let (car (our-macroexpand-1 form))))

(deftest rassoc-if-body-checks-cdr
  "RASSOC-IF body DOLIST applies predicate to (cdr pair)."
  (let* ((result (our-macroexpand-1 '(rassoc-if pred alist)))
         (dolist-form (caddr result))
         (when-form (second (cdr dolist-form)))
         (and-form (second when-form))
         (funcall-form (third and-form))
         (cdr-arg (caddr funcall-form)))
    (assert-eq (car dolist-form) 'dolist)
    (assert-eq (car funcall-form) 'funcall)
    (assert-eq (car cdr-arg) 'cdr)))


(deftest-each member-if-runtime
  "MEMBER-IF returns the tail starting at first satisfying element; nil when not found."
  :cases (("found"     "(member-if #'evenp '(1 3 4 5 6))"  '(4 5 6))
          ("not-found" "(member-if #'evenp '(1 3 5))"       nil))
  (form expected)
  (assert-equal expected (run-string form)))

(deftest-each member-if-not-runtime
  "MEMBER-IF-NOT returns tail starting at first non-matching element; nil when all satisfy."
  :cases (("found"     "(member-if-not #'oddp '(1 3 4 5))"  '(4 5))
          ("not-found" "(member-if-not #'oddp '(1 3 5))"    nil))
  (form expected)
  (assert-equal expected (run-string form)))


(deftest assoc-if-body-is-dolist
  "ASSOC-IF body is a DOLIST (linear scan)."
  (let* ((result (our-macroexpand-1 '(assoc-if pred alist)))
         (body   (caddr result)))
    (assert-eq (car body) 'dolist)))


(deftest-each assoc-if-runtime
  "ASSOC-IF returns first pair whose car satisfies predicate; nil when not found."
  :cases (("found"     "(car (assoc-if #'evenp '((1 . 10) (2 . 20) (3 . 30))))"  2)
          ("not-found" "(assoc-if #'evenp '((1 . 10) (3 . 30)))"                  nil))
  (form expected)
  (assert-equal expected (run-string form)))

(deftest-each assoc-if-not-runtime
  "ASSOC-IF-NOT returns first pair whose car does NOT satisfy predicate; nil when all satisfy."
  :cases (("found"     "(car (assoc-if-not #'evenp '((2 . 20) (3 . 30))))"  3)
          ("not-found" "(assoc-if-not #'evenp '((2 . 20) (4 . 40)))"         nil))
  (form expected)
  (assert-equal expected (run-string form)))

(deftest complement-expansion-structure
  "COMPLEMENT expands to LET+lambda; the lambda body applies NOT to the pred via APPLY."
  (let* ((result      (our-macroexpand-1 '(complement pred)))
         (lambda-form (caddr result))
         (lambda-body (caddr lambda-form))
         (not-form    lambda-body))
    (assert-eq 'let    (car result))
    (assert-eq 'lambda (car lambda-form))
    (assert-eq 'not    (car not-form))
    (assert-eq 'apply  (caadr not-form))))
