;;;; tests/unit/expand/macros-control-flow-loop-tests.lisp — Loop/do/case/typecase control-flow tests

(in-package :cl-cc/test)

(in-suite macros-control-flow-suite)

(deftest-each dolist-expansion-is-block
  "DOLIST always expands to a (block ...) containing tagbody, regardless of arity."
  :cases (("basic"          '(dolist (item list) body))
          ("with-result"    '(dolist (item list result) body))
          ("multi-body"     '(dolist (item list) body1 body2 body3)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'block)
    (assert-true (search "tagbody" (string-downcase (format nil "~S" result))))))

(deftest-each dotimes-expansion-is-block
  "DOTIMES always expands to a (block ...) containing tagbody, regardless of arity."
  :cases (("basic"        '(dotimes (i 10) body))
          ("with-result"  '(dotimes (i 10 'done) body))
          ("zero-count"   '(dotimes (i 0) body)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'block)
    (assert-true (search "tagbody" (string-downcase (format nil "~S" result))))))

(deftest-each do-expansion-is-block
  "DO always expands to a (block ...) containing let+tagbody, in all forms."
  :cases (("basic"        '(do ((i 0 (1+ i))) ((>= i 10) result) body))
          ("multi-vars"   '(do ((i 0 (1+ i)) (j 10 (1- j))) ((= i j) i) body))
          ("no-step"      '(do ((x init)) (test result) body))
          ("multi-body"   '(do ((i 0)) (test) body1 body2 body3)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'block)
    (assert-true (search "tagbody" (string-downcase (format nil "~S" result))))))

(deftest-each do*-expansion-uses-let*
  "DO* uses LET* (sequential binding) and expands to (block ...) with tagbody."
  :cases (("basic"       '(do* ((i 0 (1+ i)) (j i (1+ j))) ((>= i 10) j) body))
          ("dep-binding" '(do* ((x 1) (y (+ x 1))) (t y) body)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'block)
    (assert-true (search "let*" (string-downcase (format nil "~S" result))))))

(deftest-each case-expansion-is-let
  "CASE expands to (let ...) with eql dispatch; body forms wrapped in progn."
  :cases (("basic"         '(case key (a body-a) (b body-b)))
          ("list-of-keys"  '(case key ((a b c) body-abc) (d body-d)))
          ("multi-body"    '(case key (a body1 body2 body3))))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'let)))

(deftest-each case-default-clause
  "CASE with otherwise/t clauses both produce a let including the default body"
  :cases (("otherwise" '(case key (a body-a) (otherwise default-body)))
          ("t-clause"  '(case key (a body-a) (t default-body))))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'let)
    (assert-true (search "default-body" (string-downcase (format nil "~S" result))))))

(deftest case-expands-sparse-integer-keys-into-binary-search
  "CASE with sparse integer keys lowers to a binary-search-style decision tree."
  (let ((result (our-macroexpand-1
                 '(case key
                    (1 body-1)
                    (8 body-8)
                    (16 body-16)
                    (32 body-32)
                    (otherwise default-body)))))
    (let ((printed (string-downcase (format nil "~S" result))))
      (assert-true (search "integerp" printed))
      (assert-true (search "<" printed))
      (assert-true (search "default-body" printed)))))

(deftest case-expands-dense-integer-keys-into-table-dispatch
  "CASE with dense integer keys lowers to a table dispatch."
  (let ((result (our-macroexpand-1
                 '(case key
                    (1 body-1)
                    (2 body-2)
                    (3 body-3)
                    (4 body-4)
                    (otherwise default-body)))))
    (let ((printed (string-downcase (format nil "~S" result))))
      (assert-true (search "svref" printed))
      (assert-true (search "vector" printed))
      (assert-true (search "(if (integerp" printed))
      (assert-true (search "default-body" printed)))))

(deftest-each typecase-expansion-is-let
  "TYPECASE expands to (let ...) with typep dispatch; body forms wrapped in progn."
  :cases (("basic"      '(typecase val (string body-string) (integer body-int)))
          ("multi-body" '(typecase val (string body1 body2))))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'let)))

(deftest-each typecase-default-clause
  "TYPECASE with otherwise/t clauses both produce a let including the default body"
  :cases (("otherwise" '(typecase val (string body-string) (otherwise default-body)))
          ("t-clause"  '(typecase val (string body-string) (t default-body))))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result) 'let)
    (assert-true (search "default-body" (string-downcase (format nil "~S" result))))))

(deftest typecase-prunes-subsumed-later-clause
  "TYPECASE drops later clauses already covered by an earlier supertype."
  (let* ((result (our-macroexpand-1 '(typecase v
                                      (number body-number)
                                      (fixnum body-fixnum)
                                      (otherwise default-body))))
         (printed (string-downcase (format nil "~S" result))))
    (assert-true (search "body-number" printed))
    (assert-false (search "body-fixnum" printed))
    (assert-true (search "default-body" printed))))

(deftest typecase-expands-disjoint-arms-into-decision-tree
  "TYPECASE with pairwise-disjoint arms uses a guarded decision tree."
  (let* ((result (our-macroexpand-1
                  '(typecase v
                     (string body-string)
                     (symbol body-symbol)
                     (integer body-int)
                     (otherwise body-default))))
         (printed (string-downcase (format nil "~S" result))))
    (assert-true (search "(or (typep" printed))
    (assert-true (search "body-default" printed))))

(deftest typecase-overlapping-arms-use-ordered-decision-tree
  "TYPECASE with overlapping arms still uses ordered decision-tree dispatch."
  (let* ((result (our-macroexpand-1
                  '(typecase v
                      (number body-number)
                      (integer body-int)
                      (string body-string)
                      (symbol body-symbol)
                      (otherwise body-default))))
         (printed (string-downcase (format nil "~S" result))))
    (assert-true (search "(or (typep" printed))
    (assert-true (search "body-number" printed))
    (assert-true (search "body-default" printed))))

;;; ─── Direct helper unit tests ──────────────────────────────────────────────

(deftest case-build-eql-chain-empty
  "%case-build-eql-chain with no cases returns the default form."
  (assert-equal 'default-body
                (cl-cc/expand::%case-build-eql-chain '() 'key 'default-body)))

(deftest-each case-build-eql-chain-single-key
  "%case-build-eql-chain single-arm: atom and list keys both produce (if (eql ...) ...)."
  :cases (("atom-key"  '((a body-a)) '(if (eql key 'a) (progn body-a) nil))
          ("list-keys" '(((a b) body-ab)) '(if (or (eql key 'a) (eql key 'b)) (progn body-ab) nil)))
  (cases expected)
  (assert-equal expected
                (cl-cc/expand::%case-build-eql-chain cases 'key nil)))

(deftest case-build-eql-chain-otherwise-terminates
  "%case-build-eql-chain stops at 'otherwise and emits (progn body)."
  (let ((result (cl-cc/expand::%case-build-eql-chain
                 '((otherwise fallback)) 'key 'default)))
    (assert-equal '(progn fallback) result)))

(deftest case-collect-integer-pairs-extracts-default
  "%case-collect-integer-pairs separates default-form from integer pairs."
  (multiple-value-bind (default pairs integer-only-p)
      (cl-cc/expand::%case-collect-integer-pairs
       '((1 body-1) (2 body-2) (otherwise fallback)))
    (assert-equal '(progn fallback) default)
    (assert-= 2 (length pairs))
    (assert-true integer-only-p)))

(deftest case-collect-integer-pairs-non-integer-flag
  "%case-collect-integer-pairs clears integer-only-p when a non-integer key appears."
  (multiple-value-bind (default pairs integer-only-p)
      (cl-cc/expand::%case-collect-integer-pairs
       '((1 body-1) (foo body-foo)))
    (declare (ignore default pairs))
    (assert-false integer-only-p)))

(deftest typecase-build-typep-chain-empty
  "%typecase-build-typep-chain with no cases returns nil."
  (assert-null (cl-cc/expand::%typecase-build-typep-chain '() 'val)))

(deftest typecase-build-typep-chain-single
  "%typecase-build-typep-chain single arm produces (if (typep ...) body nil)."
  (assert-equal '(if (typep val 'string) (progn body-s) nil)
                (cl-cc/expand::%typecase-build-typep-chain
                 '((string body-s)) 'val)))

(deftest typecase-build-typep-chain-otherwise
  "%typecase-build-typep-chain otherwise arm terminates chain with (progn body)."
  (assert-equal '(progn fallback)
                (cl-cc/expand::%typecase-build-typep-chain
                 '((otherwise fallback)) 'val)))

(deftest typecase-related-types-p-detects-subtype-links
  "%typecase-related-types-p returns true for known subtype/supertype pairs."
  (assert-true (cl-cc/expand::%typecase-related-types-p 'integer 'number))
  (assert-true (cl-cc/expand::%typecase-related-types-p 'number 'integer))
  (assert-false (cl-cc/expand::%typecase-related-types-p 'string 'symbol)))

(deftest typecase-choose-split-index-prefers-low-cross-overlap
  "%typecase-choose-split-index picks boundary minimizing hierarchy cross-links."
  (let* ((cases '((integer body-int)
                  (number body-num)
                  (string body-str)
                  (symbol body-sym)))
         (idx (cl-cc/expand::%typecase-choose-split-index cases)))
    (assert-= 2 idx)))
