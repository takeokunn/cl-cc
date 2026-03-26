;;;; tests/unit/expand/macros-sequence-tests.lisp
;;;; Coverage tests for src/expand/macros-sequence.lisp
;;;;
;;;; Macros tested: getf, remf, reduce, substitute, substitute-if,
;;;; delete, copy-seq, fill, replace, mismatch, declare, locally,
;;;; in-package, defpackage, coerce, and nsubstitute variants
;;;;
(in-package :cl-cc/test)

(defsuite macros-sequence-suite
  :description "Tests for macros-sequence.lisp"
  :parent cl-cc-suite)

;;; ── GETF ─────────────────────────────────────────────────────────────────────

(deftest getf-outer-is-let
  "GETF wraps plist and indicator in a LET to avoid double evaluation"
  (let ((result (our-macroexpand-1 '(getf plist :key))))
    (assert-eq (car result) 'let)))

(deftest getf-inner-let-uses-member
  "GETF inner let uses MEMBER to locate the indicator"
  (let* ((result     (our-macroexpand-1 '(getf plist :key)))
         (inner-let  (caddr result))
         (found-init (cadar (cadr inner-let))))
    (assert-eq (car found-init) 'member)))

(deftest getf-body-is-if-found
  "GETF body is (if found (cadr found) default)"
  (let* ((result    (our-macroexpand-1 '(getf plist :key)))
         (inner-let (caddr result))
         (if-form   (caddr inner-let)))
    (assert-eq (car if-form) 'if)))

(deftest getf-with-default
  "GETF with :default arg passes it through the IF fallback"
  (let* ((result    (our-macroexpand-1 '(getf plist :key :missing)))
         (inner-let (caddr result))
         (if-form   (caddr inner-let)))
    ;; fourth element of the if is the default
    (assert-equal (cadddr if-form) :missing)))

;;; ── REMF ─────────────────────────────────────────────────────────────────────

(deftest remf-outer-is-let
  "REMF wraps indicator, prev, cur, found in a LET with a TAGBODY"
  (let ((result (our-macroexpand-1 '(remf plist :key))))
    (assert-eq (car result) 'let)))

(deftest remf-body-contains-tagbody
  "REMF body contains a TAGBODY for the loop"
  (let* ((result (our-macroexpand-1 '(remf plist :key)))
         (body   (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'tagbody))) body))))

;;; ── REDUCE ───────────────────────────────────────────────────────────────────

(deftest reduce-without-initial-value-outer-is-let
  "REDUCE without :initial-value starts from first element"
  (let ((result (our-macroexpand-1 '(reduce #'+ lst))))
    (assert-eq (car result) 'let)))

(deftest reduce-without-initial-value-has-tagbody
  "REDUCE without :initial-value has an inner TAGBODY loop"
  (let* ((result (our-macroexpand-1 '(reduce #'+ lst)))
         ;; outer let body: (let ((acc (car cur))) ... (tagbody ...) acc)
         (inner  (caddr result)))
    (assert-eq (car inner) 'let)))

(deftest reduce-with-initial-value-outer-is-let
  "REDUCE with :initial-value binds fn, acc, cur in the outer LET"
  (let ((result (our-macroexpand-1 '(reduce #'+ lst :initial-value 0))))
    (assert-eq (car result) 'let)))

(deftest reduce-with-initial-value-has-tagbody
  "REDUCE with :initial-value has a TAGBODY loop in the body"
  (let* ((result (our-macroexpand-1 '(reduce #'+ lst :initial-value 0)))
         (body   (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'tagbody))) body))))

;;; ── SUBSTITUTE ───────────────────────────────────────────────────────────────

(deftest substitute-outer-is-let
  "SUBSTITUTE binds new/old and an accumulator in a LET"
  (let ((result (our-macroexpand-1 '(substitute new old seq))))
    (assert-eq (car result) 'let)))

(deftest substitute-body-contains-dolist
  "SUBSTITUTE iterates with DOLIST, choosing new/old per element"
  (let* ((result    (our-macroexpand-1 '(substitute new old seq)))
         (dolist-f  (caddr result)))
    (assert-eq (car dolist-f) 'dolist)))

(deftest substitute-if-outer-is-let
  "SUBSTITUTE-IF binds pred and accumulator in a LET"
  (let ((result (our-macroexpand-1 '(substitute-if new pred seq))))
    (assert-eq (car result) 'let)))

(deftest substitute-if-not-outer-is-let
  "SUBSTITUTE-IF-NOT binds pred and accumulator in a LET"
  (let ((result (our-macroexpand-1 '(substitute-if-not new pred seq))))
    (assert-eq (car result) 'let)))

;;; nsubstitute variants delegate to substitute

(deftest nsubstitute-delegates-to-substitute
  "(nsubstitute new old seq) → (substitute new old seq)"
  (assert-equal (our-macroexpand-1 '(nsubstitute new old seq))
                '(substitute new old seq)))

(deftest nsubstitute-if-delegates-to-substitute-if
  "(nsubstitute-if new pred seq) → (substitute-if new pred seq)"
  (assert-equal (our-macroexpand-1 '(nsubstitute-if new pred seq))
                '(substitute-if new pred seq)))

(deftest nsubstitute-if-not-delegates-to-substitute-if-not
  "(nsubstitute-if-not new pred seq) → (substitute-if-not new pred seq)"
  (assert-equal (our-macroexpand-1 '(nsubstitute-if-not new pred seq))
                '(substitute-if-not new pred seq)))

;;; ── DELETE ───────────────────────────────────────────────────────────────────

(deftest delete-outer-is-let
  "DELETE (same as remove in this impl) accumulates into a LET"
  (let ((result (our-macroexpand-1 '(delete item seq))))
    (assert-eq (car result) 'let)))

(deftest delete-if-outer-is-let
  "DELETE-IF accumulates into a LET"
  (let ((result (our-macroexpand-1 '(delete-if pred seq))))
    (assert-eq (car result) 'let)))

(deftest delete-duplicates-delegates-to-remove-duplicates
  "(delete-duplicates seq) → (remove-duplicates seq)"
  (assert-equal (our-macroexpand-1 '(delete-duplicates seq))
                '(remove-duplicates seq)))

;;; ── COPY-SEQ ─────────────────────────────────────────────────────────────────

(deftest copy-seq-delegates-to-copy-list
  "(copy-seq seq) → (copy-list seq) — thin alias"
  (assert-equal (our-macroexpand-1 '(copy-seq seq)) '(copy-list seq)))

;;; ── FILL ─────────────────────────────────────────────────────────────────────

(deftest fill-outer-is-let*
  "FILL binds seq and a pointer in LET* then loops via TAGBODY"
  (let ((result (our-macroexpand-1 '(fill seq item))))
    (assert-eq (car result) 'let*)))

(deftest fill-body-contains-tagbody
  "FILL body contains a TAGBODY for the mutation loop"
  (let* ((result (our-macroexpand-1 '(fill seq item)))
         (body   (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'tagbody))) body))))

;;; ── MISMATCH ─────────────────────────────────────────────────────────────────

(deftest mismatch-outer-is-block
  "MISMATCH wraps in a BLOCK for early RETURN"
  (let ((result (our-macroexpand-1 '(mismatch seq1 seq2))))
    (assert-eq (car result) 'block)))

(deftest mismatch-body-has-let-with-tagbody
  "MISMATCH body has a LET with index counter and a TAGBODY"
  ;; expansion: (block nil (let (...) (tagbody ...)))
  ;; cadr is the block name (nil); caddr is the let form
  (let* ((result (our-macroexpand-1 '(mismatch seq1 seq2)))
         (let-f  (caddr result)))
    (assert-eq (car let-f) 'let)))

;;; ── DECLARE / LOCALLY / IN-PACKAGE ──────────────────────────────────────────

(deftest declare-expands-to-nil
  "(declare (ignore x)) expands to nil — declarations silently dropped"
  (assert-equal (our-macroexpand-1 '(declare (ignore x))) nil))

(deftest locally-strips-declare-forms
  "LOCALLY removes DECLARE forms and wraps remaining body in PROGN"
  (let ((result (our-macroexpand-1 '(locally (declare (ignore x)) expr1 expr2))))
    (assert-eq (car result) 'progn)
    (assert-false (member 'declare result))
    (assert-equal (cadr result) 'expr1)
    (assert-equal (caddr result) 'expr2)))

(deftest in-package-quotes-name
  "(in-package :cl-cc) → (quote :cl-cc)"
  (assert-equal (our-macroexpand-1 '(in-package :cl-cc)) '(quote :cl-cc)))

(deftest defpackage-quotes-name
  "(defpackage :foo ...) → (quote :foo) — no-op at runtime"
  (assert-equal (our-macroexpand-1 '(defpackage :foo (:use :cl))) '(quote :foo)))

;;; ── COERCE ───────────────────────────────────────────────────────────────────

;; coerce-to-* are CL-CC:: symbols; check by symbol-name to avoid
;; CL-CC/TEST:: vs CL-CC:: package mismatch in expected values.
(deftest-each coerce-quoted-type-expansions
  "COERCE with quoted type dispatches to the right coerce-to-* primitive"
  :cases (("to-string"        '(coerce v 'string)        "COERCE-TO-STRING")
          ("to-simple-string" '(coerce v 'simple-string) "COERCE-TO-STRING")
          ("to-list"          '(coerce v 'list)          "COERCE-TO-LIST")
          ("to-vector"        '(coerce v 'vector)        "COERCE-TO-VECTOR"))
  (form expected-name)
  (let ((result (our-macroexpand-1 form)))
    (assert-equal (symbol-name (car result)) expected-name)
    (assert-equal (cadr result) 'v)))

(deftest coerce-unquoted-type-fallback
  "COERCE with a non-literal type defaults to coerce-to-string"
  (let ((result (our-macroexpand-1 '(coerce v type-var))))
    (assert-equal (symbol-name (car result)) "COERCE-TO-STRING")
    (assert-equal (cadr result) 'v)))

;;; ── MAP (delegates to mapcar + coerce) ──────────────────────────────────────

(deftest map-delegates-to-mapcar-coerce
  "(map result-type fn seq) → (coerce (mapcar fn (coerce seq 'list)) result-type)"
  (assert-equal (our-macroexpand-1 '(map 'list fn seq))
                '(coerce (mapcar fn (coerce seq 'list)) 'list)))
