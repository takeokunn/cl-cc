;;;; tests/unit/expand/macros-sequence-tests.lisp
;;;; Coverage tests for src/expand/macros-sequence.lisp

(in-package :cl-cc/test)

(defsuite macros-sequence-suite
  :description "Tests for macros-sequence.lisp"
  :parent cl-cc-unit-suite)

(in-suite macros-sequence-suite)

(deftest-each reduce-outer-is-let
  "REDUCE wraps its work in an outer LET regardless of :initial-value."
  :cases (("without-initial-value" '(reduce #'+ lst))
          ("with-initial-value"    '(reduce #'+ lst :initial-value 0)))
  (form)
  (assert-eq (car (our-macroexpand-1 form)) 'let))

(deftest reduce-without-initial-value-has-inner-let
  "REDUCE without :initial-value wraps the scan body in an inner LET."
  (let* ((result (our-macroexpand-1 '(reduce #'+ lst)))
         (inner  (caddr result)))
    (assert-eq (car inner) 'let)))

(deftest reduce-with-initial-value-has-loop
  "REDUCE with :initial-value expands to a LOOP accumulation form."
  (let* ((result (our-macroexpand-1 '(reduce #'+ lst :initial-value 0)))
         (body   (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'loop))) body))))

(deftest substitute-expansion
  "SUBSTITUTE: outer LET containing a DOLIST loop."
  (let* ((result   (our-macroexpand-1 '(substitute new old seq)))
         (dolist-f (caddr result)))
    (assert-eq 'let (car result))
    (assert-eq 'dolist (car dolist-f))))

(deftest-each predicate-sequence-outer-is-let
  "Predicate-based sequence macros all wrap their work in a LET."
  :cases (("substitute-if"     '(substitute-if new pred seq))
          ("substitute-if-not" '(substitute-if-not new pred seq))
          ("delete-if"         '(delete-if pred seq))
          ("delete-if-not"     '(delete-if-not pred seq)))
  (form)
  (assert-eq (car (our-macroexpand-1 form)) 'let))

(deftest-each nsubstitute-delegates-to-substitute-variant
  "nsubstitute* forms delegate to their substitute* counterparts."
  :cases (("nsubstitute"        '(nsubstitute new old seq)         '(substitute new old seq))
          ("nsubstitute-if"     '(nsubstitute-if new pred seq)     '(substitute-if new pred seq))
          ("nsubstitute-if-not" '(nsubstitute-if-not new pred seq) '(substitute-if-not new pred seq)))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest delete-delegates-to-remove
  "DELETE expands to REMOVE (non-destructive delegation)."
  (let ((result (our-macroexpand-1 '(delete item seq))))
    (assert-eq (car result) 'remove)))

(deftest delete-duplicates-delegates-to-remove-duplicates
  "delete-duplicates expands to remove-duplicates."
  (assert-equal (our-macroexpand-1 '(delete-duplicates seq))
                '(remove-duplicates seq)))

(deftest copy-seq-delegates-to-copy-list
  "(copy-seq seq) expands to a form containing copy-list (for lists) and subseq (for vectors)"
  (let ((result (our-macroexpand-1 '(copy-seq seq))))
    (assert-eq 'let (car result))))

(deftest fill-expansion
  "FILL: outer LET* with runtime list/vector dispatch containing a TAGBODY mutation loop."
  (let* ((result (our-macroexpand-1 '(fill seq item)))
         (body   (cddr result)))
    (assert-eq 'let* (car result))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'if))) body))))

(deftest mismatch-expansion
  "MISMATCH: outer BLOCK for early RETURN; body is LET with index counter."
  (let* ((result (our-macroexpand-1 '(mismatch seq1 seq2)))
         (let-f  (caddr result)))
    (assert-eq 'block (car result))
    (assert-eq 'let   (car let-f))))


(deftest last-expands-to-let*-nthcdr
  "LAST expands to LET* containing an NTHCDR call for tail access."
  (let ((result (our-macroexpand-1 '(last xs 2))))
    (assert-eq 'let* (car result))
    (assert-eq 'nthcdr (car (caddr result)))))

(deftest butlast-expands-to-let*-when
  "BUTLAST expands to LET* with a WHEN guard around the copy."
  (let ((result (our-macroexpand-1 '(butlast xs 2))))
    (assert-eq 'let* (car result))
    (assert-eq 'when (car (caddr result)))))

(deftest nbutlast-delegates-to-butlast
  "NBUTLAST is a direct alias for BUTLAST (non-destructive delegation)."
  (assert-equal (our-macroexpand-1 '(nbutlast xs 2)) '(butlast xs 2)))

(deftest search-expansion
  "SEARCH expands to a LET* with a BLOCK/DO scan loop."
  (let ((result (our-macroexpand-1 '(search '(1 2) xs))))
    (assert-eq 'let* (car result))
    (assert-true (search "BLOCK" (string-upcase (write-to-string result))))))

(deftest progv-expansion
  "PROGV: outer LET* with 3 bindings, body uses UNWIND-PROTECT."
  (let* ((result   (our-macroexpand-1 '(progv syms vals (do-stuff))))
         (bindings (second result))
         (body     (caddr result)))
    (assert-eq 'let* (car result))
    (assert-= 3 (length bindings))
    (assert-eq 'unwind-protect (car body))))

(deftest sort-with-key-structure
  "SORT with :key: 2 bindings (pred + keyfn); labels has 3 helpers; mmerge accepts 2 params."
  (let* ((result      (our-macroexpand-1 '(sort lst pred :key #'car)))
         (bindings    (second result))
         (labels-form (caddr result))
         (fns         (cadr labels-form))
         (mmerge-def  (second fns))
         (params      (second mmerge-def)))
    (assert-eq (car result)      'let)
    (assert-=  (length bindings) 2)
    (assert-eq (car labels-form) 'labels)
    (assert-=  (length fns)      3)
    (assert-=  (length params)   2)))

(deftest stable-sort-with-key-delegates-to-sort-with-key
  "(stable-sort lst pred :key fn) → (sort lst pred :key fn)"
  (let ((result (our-macroexpand-1 '(stable-sort lst pred :key #'car))))
    (assert-eq (car result) 'sort)
    (assert-equal (cddr result) '(pred :key #'car))))

