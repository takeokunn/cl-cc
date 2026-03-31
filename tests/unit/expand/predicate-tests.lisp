;;;; tests/unit/expand/predicate-tests.lisp — Predicate tests

(in-package :cl-cc/test)

(defsuite predicate-suite
  :description "Predicate macro expansion tests"
  :parent cl-cc-suite)

(in-suite predicate-suite)

(deftest find-if-not-delegates-to-find-if-complement
  "FIND-IF-NOT expands to (find-if (complement pred) list)."
  (let ((result (our-macroexpand-1 '(find-if-not pred lst))))
    (assert-eq (car result) 'find-if)
    (assert-eq (caadr result) 'complement)))

(deftest find-if-not-runtime
  "FIND-IF-NOT returns first element not satisfying predicate."
  (assert-equal (run-string "(find-if-not #'oddp '(1 3 4 5 6))") 4)
  (assert-eq    (run-string "(find-if-not #'numberp '(1 2 3))") nil))

(deftest position-if-outer-is-let
  "POSITION-IF expands to a LET binding the predicate."
  (assert-eq (car (our-macroexpand-1 '(position-if pred lst))) 'let))

(deftest position-if-body-is-block
  "POSITION-IF body is a BLOCK NIL for early RETURN."
  (let* ((result (our-macroexpand-1 '(position-if pred lst)))
         (body   (caddr result)))
    (assert-eq (car body) 'block)
    (assert-eq (second body) nil)))

(deftest position-if-runtime
  "POSITION-IF returns the 0-based index of first matching element."
  (assert-= (run-string "(position-if #'evenp '(1 3 4 7 8))") 2)
  (assert-eq (run-string "(position-if #'evenp '(1 3 5))") nil))

(deftest position-if-not-delegates-to-position-if-complement
  "POSITION-IF-NOT expands to (position-if (complement pred) list)."
  (let ((result (our-macroexpand-1 '(position-if-not pred lst))))
    (assert-eq (car result) 'position-if)
    (assert-eq (caadr result) 'complement)))

(deftest position-if-not-runtime
  "POSITION-IF-NOT returns index of first element not satisfying predicate."
  (assert-= (run-string "(position-if-not #'oddp '(1 3 4 5))") 2)
  (assert-eq (run-string "(position-if-not #'oddp '(1 3 5))") nil))

(deftest count-if-not-delegates-to-count-if-complement
  "COUNT-IF-NOT expands to (count-if (complement pred) list)."
  (let ((result (our-macroexpand-1 '(count-if-not pred lst))))
    (assert-eq (car result) 'count-if)
    (assert-eq (caadr result) 'complement)))

(deftest count-if-not-runtime
  "COUNT-IF-NOT counts elements not satisfying predicate."
  (assert-= (run-string "(count-if-not #'oddp '(1 2 3 4 5))") 2)
  (assert-= (run-string "(count-if-not #'numberp '())") 0))

(deftest remove-if-with-key
  "REMOVE-IF with :key applies key before predicate."
  (let ((result (our-macroexpand-1 '(remove-if #'oddp lst :key #'car))))
    (assert-eq (car result) 'let)
    (assert-true (> (length (cadr result)) 1))))

(deftest remove-if-not-with-key
  "REMOVE-IF-NOT with :key applies key before predicate."
  (let ((result (our-macroexpand-1 '(remove-if-not #'evenp lst :key #'car))))
    (assert-eq (car result) 'let)
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

(deftest rassoc-if-outer-is-let
  "RASSOC-IF expands to a LET binding the predicate."
  (assert-eq (car (our-macroexpand-1 '(rassoc-if pred alist))) 'let))

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

(deftest rassoc-if-not-delegates-to-rassoc-if-complement
  "RASSOC-IF-NOT expands to (rassoc-if (complement pred) alist)."
  (let ((result (our-macroexpand-1 '(rassoc-if-not pred alist))))
    (assert-eq (car result) 'rassoc-if)
    (assert-eq (caadr result) 'complement)))

(deftest member-if-runtime
  "MEMBER-IF returns the tail starting at first satisfying element."
  (assert-equal (run-string "(member-if #'evenp '(1 3 4 5 6))") '(4 5 6))
  (assert-eq (run-string "(member-if #'evenp '(1 3 5))") nil))

(deftest member-if-not-runtime
  "MEMBER-IF-NOT returns tail starting at first non-matching element."
  (assert-equal (run-string "(member-if-not #'oddp '(1 3 4 5))") '(4 5))
  (assert-eq (run-string "(member-if-not #'oddp '(1 3 5))") nil))

(deftest assoc-if-outer-is-let
  "ASSOC-IF expands to a LET binding the predicate."
  (assert-eq (car (our-macroexpand-1 '(assoc-if pred alist))) 'let))

(deftest assoc-if-body-is-dolist
  "ASSOC-IF body is a DOLIST (linear scan)."
  (let* ((result (our-macroexpand-1 '(assoc-if pred alist)))
         (body   (caddr result)))
    (assert-eq (car body) 'dolist)))

(deftest assoc-if-not-delegates-to-assoc-if-complement
  "ASSOC-IF-NOT expands to (assoc-if (complement pred) alist)."
  (let ((result (our-macroexpand-1 '(assoc-if-not pred alist))))
    (assert-eq (car result) 'assoc-if)
    (assert-eq (caadr result) 'complement)))

(deftest assoc-if-runtime
  "ASSOC-IF returns first pair whose car satisfies predicate."
  (assert-= (run-string "(car (assoc-if #'evenp '((1 . 10) (2 . 20) (3 . 30))))") 2)
  (assert-eq (run-string "(assoc-if #'evenp '((1 . 10) (3 . 30)))") nil))

(deftest assoc-if-not-runtime
  "ASSOC-IF-NOT returns first pair whose car does NOT satisfy predicate."
  (assert-= (run-string "(car (assoc-if-not #'evenp '((2 . 20) (3 . 30))))") 3)
  (assert-eq (run-string "(assoc-if-not #'evenp '((2 . 20) (4 . 40)))") nil))

(deftest complement-outer-is-let
  "COMPLEMENT expands to a LET binding the predicate, returning a lambda."
  (let* ((result (our-macroexpand-1 '(complement pred)))
         (body (caddr result)))
    (assert-eq (car result) 'let)
    (assert-eq (car body) 'lambda)))

(deftest complement-lambda-applies-not
  "COMPLEMENT lambda body applies NOT to the result of applying pred."
  (let* ((result (our-macroexpand-1 '(complement pred)))
         (lambda-body (caddr (caddr result)))
         (not-form lambda-body))
    (assert-eq (car not-form) 'not)
    (assert-eq (caadr not-form) 'apply)))
