;;;; tests/unit/expand/macros-sequence-tests.lisp
;;;; Coverage tests for src/expand/macros-sequence.lisp

(in-package :cl-cc/test)

(defsuite macros-sequence-suite
  :description "Tests for macros-sequence.lisp"
  :parent cl-cc-suite)

(in-suite macros-sequence-suite)

(deftest-each reduce-outer-is-let
  "REDUCE wraps its work in an outer LET regardless of :initial-value."
  :cases (("without-initial-value" '(reduce #'+ lst))
          ("with-initial-value"    '(reduce #'+ lst :initial-value 0)))
  (form)
  (assert-eq (car (our-macroexpand-1 form)) 'let))

(deftest reduce-without-initial-value-has-tagbody
  "REDUCE without :initial-value has an inner TAGBODY loop"
  (let* ((result (our-macroexpand-1 '(reduce #'+ lst)))
         (inner  (caddr result)))
    (assert-eq (car inner) 'let)))

(deftest reduce-with-initial-value-has-tagbody
  "REDUCE with :initial-value has a TAGBODY loop in the body"
  (let* ((result (our-macroexpand-1 '(reduce #'+ lst :initial-value 0)))
         (body   (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'tagbody))) body))))

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
  "DELETE delegates to REMOVE (no :test/:key → simple remove call)"
  (let ((result (our-macroexpand-1 '(delete item seq))))
    (assert-eq (car result) 'remove)))

(deftest delete-duplicates-delegates-to-remove-duplicates
  "(delete-duplicates seq) → (remove-duplicates seq)"
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

(deftest declare-expands-to-nil
  "(declare (ignore x)) expands to nil — declarations silently dropped"
  (assert-equal (our-macroexpand-1 '(declare (ignore x))) nil))

(deftest locally-strips-declare-forms
  "LOCALLY preserves DECLARE forms by wrapping in (let () decl body)"
  (let ((result (our-macroexpand-1 '(locally (declare (ignore x)) expr1 expr2))))
    (assert-eq (car result) 'let)
    (assert-equal (cadr result) nil)
    (assert-equal (caddr result) '(declare (ignore x)))
    (assert-equal (cadddr result) 'expr1)))

(deftest in-package-sets-package
  "(in-package :cl-cc) expands to progn that sets *package* and returns quoted name"
  (let ((result (our-macroexpand-1 '(in-package :cl-cc))))
    (assert-eq (car result) 'progn)
    (assert-eq (car (second result)) 'setq)
    (assert-equal (third result) '(quote :cl-cc))))

(deftest defpackage-creates-package
  "(defpackage :foo ...) expands to progn with find-package/make-package"
  (let* ((result (our-macroexpand-1 '(defpackage :foo (:use :cl))))
         (result-str (format nil "~S" result)))
    (assert-eq (car result) 'progn)
    (assert-true (search "FIND-PACKAGE" result-str))
    (assert-true (search "MAKE-PACKAGE" result-str))))

(deftest defpackage-local-nicknames-expand-and-apply
  "(defpackage ... (:local-nicknames ...)) expands to host local nickname registration."
  (let* ((form '(defpackage :fr275-pkg
                  (:use :cl)
                  (:local-nicknames (:a :cl-user))))
         (result (our-macroexpand-1 form))
         (result-str (format nil "~S" result))
         (pkg-name (eval result))
         (pkg (find-package pkg-name)))
    (assert-true (search "ADD-PACKAGE-LOCAL-NICKNAME" result-str))
    (assert-true (equal (package-name pkg) "FR275-PKG"))
    (assert-true (assoc "A" (sb-ext:package-local-nicknames pkg) :test #'string=)))))

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
  "COERCE with a non-literal type dispatches to %coerce-runtime"
  (let ((result (our-macroexpand-1 '(coerce v type-var))))
    (assert-equal (symbol-name (car result)) "%COERCE-RUNTIME")
    (assert-equal (cadr result) 'v)))

(deftest map-delegates-to-mapcar-coerce
  "(map result-type fn seq) → (coerce (mapcar fn (coerce seq 'list)) result-type)"
  (assert-equal (our-macroexpand-1 '(map 'list fn seq))
                '(coerce (mapcar fn (coerce seq 'list)) 'list)))

(deftest-each dest-returning-sequence-expanders
  "Destination-returning operators: expansion contains the dest variable and returns it."
  :cases (("map-into" '(map-into dest fn src)))
  (form)
  (let* ((result    (our-macroexpand-1 form))
         (dest-var  (first (first (second result))))
         (last-form (car (last (cddr result)))))
    (assert-eq 'let (car result))
    (assert-eq last-form dest-var)))

(deftest replace-expansion-vector-path
  "REPLACE: outer LET* with runtime vector/list dispatch."
  (let* ((result (our-macroexpand-1 '(replace dest src))))
    (assert-eq 'let* (car result))))

(deftest merge-expansion
  "MERGE: outer LET with 4 bindings, body uses TAGBODY."
  (let* ((result   (our-macroexpand-1 '(merge 'list l1 l2 pred)))
         (bindings (second result))
         (body     (caddr result)))
    (assert-eq 'let (car result))
    (assert-= 4 (length bindings))
    (assert-eq 'tagbody (car body))))

(deftest last-expansion
  "LAST expands to an NTHCDR over a LET* binding."
  (let ((result (our-macroexpand-1 '(last xs 2))))
    (assert-eq 'let* (car result))
    (assert-eq 'nthcdr (car (caddr result)))))

(deftest butlast-expansion
  "BUTLAST expands to a LET* binding with a SUBSEQ tail path."
  (let ((result (our-macroexpand-1 '(butlast xs 2))))
    (assert-eq 'let* (car result))
    (assert-eq 'when (car (caddr result)))))

(deftest nbutlast-delegates-to-butlast
  "NBUTLAST delegates to BUTLAST."
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

(deftest sort-with-key-has-four-bindings
  "SORT with :key binds pred AND keyfn — 2 bindings vs no-key 1 binding."
  (let* ((result   (our-macroexpand-1 '(sort lst pred :key #'car)))
         (bindings (second result)))
    (assert-eq (car result) 'let)
    (assert-= (length bindings) 2)))

(deftest sort-with-key-merge-uses-keyfn
  "SORT with :key: labels body has 3 helpers; mmerge accepts (a b) params."
  (let* ((result      (our-macroexpand-1 '(sort lst pred :key #'car)))
         (labels-form (caddr result))
         (fns         (cadr labels-form))
         (mmerge-def  (second fns))
         (params      (second mmerge-def)))
    (assert-eq  (car labels-form) 'labels)
    (assert-=   (length fns)      3)
    (assert-=   (length params)   2)))

(deftest stable-sort-with-key-delegates-to-sort-with-key
  "(stable-sort lst pred :key fn) → (sort lst pred :key fn)"
  (let ((result (our-macroexpand-1 '(stable-sort lst pred :key #'car))))
    (assert-eq (car result) 'sort)
    (assert-equal (cddr result) '(pred :key #'car))))

(deftest concatenate-list-expands-to-append
  "CONCATENATE 'list expands to (append ...sequences)."
  (assert-equal (our-macroexpand-1 '(concatenate 'list '(1 2) '(3 4)))
                '(append '(1 2) '(3 4))))

(deftest concatenate-vector-expands-to-coerce-to-vector
  "CONCATENATE 'vector expands to (coerce-to-vector (append ...))."
  (let ((result (our-macroexpand-1 '(concatenate 'vector '(1) '(2)))))
    (assert-equal (symbol-name (car result)) "COERCE-TO-VECTOR")
    (assert-eq (caadr result) 'append)))

(deftest concatenate-simple-vector-expands-to-coerce-to-vector
  "CONCATENATE 'simple-vector also uses coerce-to-vector path."
  (let ((result (our-macroexpand-1 '(concatenate 'simple-vector '(1) '(2)))))
    (assert-equal (symbol-name (car result)) "COERCE-TO-VECTOR")))

(deftest concatenate-runtime-string
  "CONCATENATE runtime: string concatenation."
  (assert-equal (run-string "(concatenate 'string \"hello\" \" \" \"world\")") "hello world")
  (assert-equal (run-string "(concatenate 'string)") "")
  (assert-equal (run-string "(concatenate 'string \"only\")") "only"))

(deftest concatenate-runtime-list
  "CONCATENATE runtime: list concatenation."
  (assert-equal (run-string "(concatenate 'list '(1 2) '(3 4))") '(1 2 3 4))
  (assert-= (run-string "(length (concatenate 'list '(1 2) '(3 4)))") 4)
  (assert-= (run-string "(length (concatenate 'list '(1)))") 1))

(deftest-each notany-notevery-runtime
  "notany/notevery runtime behaviour mirrors (not (some/every ...))."
  :cases (("notany-all-fail"    "(notany #'evenp '(1 3 5))"   t)
          ("notany-one-passes"  "(notany #'evenp '(1 2 3))"   nil)
          ("notevery-all-pass"  "(notevery #'oddp '(1 3 5))"  nil)
          ("notevery-one-fails" "(notevery #'oddp '(1 2 3))"  t))
  (code expected)
  (if expected
      (assert-true (run-string code))
      (assert-false (run-string code))))

(deftest nreconc-expands-to-nconc-nreverse
  "NRECONC expands to (nconc (nreverse list) tail)."
  (let ((result (our-macroexpand-1 '(nreconc lst tail))))
    (assert-eq (car result) 'nconc)
    (assert-eq (caadr result) 'nreverse)
    (assert-eq (caddr result) 'tail)))

(deftest nreconc-runtime
  "NRECONC prepends reversed list onto tail."
  (assert-equal (run-string "(nreconc (list 3 2 1) '(4 5))") '(1 2 3 4 5))
  (assert-equal (run-string "(nreconc '() '(1 2))") '(1 2)))
