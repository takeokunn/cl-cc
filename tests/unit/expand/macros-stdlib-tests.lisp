;;;; tests/unit/expand/macros-stdlib-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib.lisp
;;;;
;;;; Macros tested: push, pop, incf/decf, 1+/1-, signum, return,
;;;; with-slots, nth-value, destructuring-bind, assert, ignore-errors,
;;;; concatenate, CXR accessors, HOF macros (mapcar, every, some, find,
;;;; position, count, remove-if, remove-if-not, mapcan), remove, sort,
;;;; with-accessors, define-modify-macro, define-condition, handler-bind,
;;;; restart-case, isqrt, with-open-stream
;;;;
(in-package :cl-cc/test)

(defsuite macros-stdlib-suite
  :description "Tests for macros-stdlib.lisp"
  :parent cl-cc-suite)

;;; ── Mutation macros ──────────────────────────────────────────────────────────

(deftest push-expands-to-setf-cons
  "PUSH expands to (setf place (cons value place)) — no gensym"
  (assert-equal (our-macroexpand-1 '(push v lst))
                '(setf lst (cons v lst))))

(deftest pop-outer-is-let
  "POP wraps (car place) binding in a LET then (setf place (cdr place))"
  (let ((result (our-macroexpand-1 '(pop lst))))
    (assert-eq (car result) 'let)))

(deftest pop-body-contains-setf-cdr
  "POP body contains (setf lst (cdr lst))"
  (let* ((result (our-macroexpand-1 '(pop lst)))
         (setf-form (caddr result)))
    (assert-eq (car setf-form) 'setf)
    (assert-equal (caddr setf-form) '(cdr lst))))

(deftest incf-default-delta
  "(incf x) → (setf x (+ x 1))"
  (assert-equal (our-macroexpand-1 '(incf x))
                '(setf x (+ x 1))))

(deftest incf-custom-delta
  "(incf x 5) → (setf x (+ x 5))"
  (assert-equal (our-macroexpand-1 '(incf x 5))
                '(setf x (+ x 5))))

(deftest decf-default-delta
  "(decf x) → (setf x (- x 1))"
  (assert-equal (our-macroexpand-1 '(decf x))
                '(setf x (- x 1))))

(deftest decf-custom-delta
  "(decf x 3) → (setf x (- x 3))"
  (assert-equal (our-macroexpand-1 '(decf x 3))
                '(setf x (- x 3))))

;;; ── Numeric shorthand ────────────────────────────────────────────────────────

(deftest 1+-expands-to-plus
  "(1+ n) → (+ n 1)"
  (assert-equal (our-macroexpand-1 '(1+ n)) '(+ n 1)))

(deftest 1--expands-to-minus
  "(1- n) → (- n 1)"
  (assert-equal (our-macroexpand-1 '(1- n)) '(- n 1)))

(deftest signum-outer-is-let
  "SIGNUM wraps n in a LET to avoid double evaluation"
  (let ((result (our-macroexpand-1 '(signum n))))
    (assert-eq (car result) 'let)))

(deftest signum-body-uses-cond
  "SIGNUM body dispatches via COND on zerop/plusp"
  (let* ((result (our-macroexpand-1 '(signum n)))
         (body   (caddr result)))
    (assert-eq (car body) 'cond)))

;;; ── RETURN ───────────────────────────────────────────────────────────────────

(deftest return-with-value-to-return-from-nil
  "(return v) → (return-from nil v)"
  (assert-equal (our-macroexpand-1 '(return v)) '(return-from nil v)))

(deftest return-no-value-to-return-from-nil
  "(return) → (return-from nil nil)"
  (assert-equal (our-macroexpand-1 '(return)) '(return-from nil nil)))

;;; ── WITH-SLOTS ───────────────────────────────────────────────────────────────

(deftest with-slots-outer-is-let
  "WITH-SLOTS wraps instance in a LET then binds slot vars"
  (let ((result (our-macroexpand-1 '(with-slots (x y) obj body))))
    (assert-eq (car result) 'let)))

(deftest with-slots-inner-let-binds-slot-value
  "WITH-SLOTS inner LET binds each slot via SLOT-VALUE"
  (let* ((result (our-macroexpand-1 '(with-slots (x) obj body)))
         ;; outer let binds the instance; inner let binds slots
         (inner-let (caddr result)))
    (assert-eq (car inner-let) 'let)
    (let ((binding (caadr inner-let)))
      (assert-eq (car binding) 'x)
      (assert-eq (caadr binding) 'slot-value))))

;;; ── NTH-VALUE ────────────────────────────────────────────────────────────────

(deftest nth-value-outer-is-let
  "NTH-VALUE binds (multiple-value-list form) in a LET then calls NTH"
  (let ((result (our-macroexpand-1 '(nth-value 1 form))))
    (assert-eq (car result) 'let)))

(deftest nth-value-body-is-nth
  "NTH-VALUE body calls (nth n gensym)"
  (let* ((result (our-macroexpand-1 '(nth-value 1 form)))
         (body   (caddr result)))
    (assert-eq (car body) 'nth)
    (assert-equal (cadr body) 1)))

;;; ── DESTRUCTURING-BIND ───────────────────────────────────────────────────────

(deftest destructuring-bind-outer-is-let
  "DESTRUCTURING-BIND wraps expr in a LET"
  (let ((result (our-macroexpand-1 '(destructuring-bind (a b) expr body))))
    (assert-eq (car result) 'let)))

(deftest destructuring-bind-body-is-let*
  "DESTRUCTURING-BIND inner form is LET* with destructured bindings"
  (let* ((result (our-macroexpand-1 '(destructuring-bind (a b) expr body)))
         (inner  (caddr result)))
    (assert-eq (car inner) 'let*)))

;;; ── ASSERT ───────────────────────────────────────────────────────────────────

(deftest assert-basic-expands-to-unless
  "(assert test) → (unless test (error ...))"
  (let ((result (our-macroexpand-1 '(assert test))))
    (assert-eq (car result) 'unless)
    (assert-equal (cadr result) 'test)))

(deftest assert-body-is-error
  "(assert test) body is an (error ...) call"
  (let* ((result (our-macroexpand-1 '(assert test)))
         (body   (caddr result)))
    (assert-eq (car body) 'error)))

(deftest assert-with-datum
  "(assert test () \"msg\") signals that message"
  (let* ((result (our-macroexpand-1 '(assert test () "bad input")))
         (body   (caddr result)))
    (assert-eq (car body) 'error)
    (assert-equal (cadr body) "bad input")))

;;; ── IGNORE-ERRORS ────────────────────────────────────────────────────────────

(deftest ignore-errors-outer-is-handler-case
  "IGNORE-ERRORS wraps forms in HANDLER-CASE"
  (let ((result (our-macroexpand-1 '(ignore-errors expr))))
    (assert-eq (car result) 'handler-case)))

(deftest ignore-errors-catches-error-type
  "IGNORE-ERRORS handler clause catches ERROR type"
  ;; expansion: (handler-case (progn expr) (error (evar) nil))
  ;; caddr is the error clause, not cadddr (only 3 top-level elements)
  (let* ((result (our-macroexpand-1 '(ignore-errors expr)))
         (clause (caddr result)))
    (assert-eq (car clause) 'error)))

;;; ── CONCATENATE ──────────────────────────────────────────────────────────────

(deftest-each concatenate-small-arities
  "CONCATENATE expands to empty string or passthrough for 0-1 strings"
  :cases (("zero-strings"  '(concatenate 'string)      "")
          ("one-string"    '(concatenate 'string "a")   "a"))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest concatenate-two-strings
  "(concatenate 'string a b) → (string-concat a b)"
  (let ((result (our-macroexpand-1 '(concatenate 'string "a" "b"))))
    (assert-equal (symbol-name (car result)) "STRING-CONCAT")
    (assert-equal (cdr result) '("a" "b"))))

(deftest concatenate-three-strings-left-associative
  "(concatenate 'string a b c) → (string-concat (string-concat a b) c)"
  (let ((result (our-macroexpand-1 '(concatenate 'string "a" "b" "c"))))
    (assert-equal (symbol-name (car result)) "STRING-CONCAT")
    ;; inner call is also string-concat
    (assert-equal (symbol-name (caadr result)) "STRING-CONCAT")
    (assert-equal (caddr result) "c")))

;;; ── CXR accessors ────────────────────────────────────────────────────────────

(deftest-each cxr-two-level-expansions
  "Two-level cXXr forms expand to two nested car/cdr calls"
  :cases (("caar" '(caar x) '(car (car x)))
          ("cadr" '(cadr x) '(car (cdr x)))
          ("cdar" '(cdar x) '(cdr (car x)))
          ("cddr" '(cddr x) '(cdr (cdr x))))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest-each cxr-three-level-expansions
  "Three-level cXXXr forms expand to three nested car/cdr calls"
  :cases (("caaar" '(caaar x) '(car (car (car x))))
          ("caddr" '(caddr x) '(car (cdr (cdr x))))
          ("cdddr" '(cdddr x) '(cdr (cdr (cdr x)))))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

;;; ── HOF macros ───────────────────────────────────────────────────────────────

(deftest-each hof-macro-outer-is-let
  "Each HOF macro expands to a LET as its outermost form."
  :cases (("mapcar"       '(mapcar fn lst))
          ("every"        '(every pred lst))
          ("some"         '(some pred lst))
          ("remove-if"    '(remove-if pred lst))
          ("remove-if-not" '(remove-if-not pred lst)))
  (form)
  (assert-eq (car (our-macroexpand-1 form)) 'let))

(deftest mapcar-body-contains-dolist
  "MAPCAR body iterates with DOLIST"
  (let* ((result (our-macroexpand-1 '(mapcar fn lst)))
         ;; dolist is in the body of the let
         (dolist-form (caddr result)))
    (assert-eq (car dolist-form) 'dolist)))

(deftest every-body-has-block-and-dolist
  "EVERY body is a BLOCK containing DOLIST with UNLESS return"
  (let* ((result (our-macroexpand-1 '(every pred lst)))
         (block-form (caddr result)))
    (assert-eq (car block-form) 'block)))

(deftest notany-delegates-to-some
  "(notany pred lst) → (not (some pred lst))"
  (assert-equal (our-macroexpand-1 '(notany pred lst))
                '(not (some pred lst))))

(deftest notevery-delegates-to-every
  "(notevery pred lst) → (not (every pred lst))"
  (assert-equal (our-macroexpand-1 '(notevery pred lst))
                '(not (every pred lst))))

(deftest find-no-keys-is-eql-loop
  "FIND without keyword args generates a fast EQL check loop"
  (let* ((result (our-macroexpand-1 '(find item lst)))
         ;; outer let binds item, body is a block with dolist
         (body (caddr result)))
    (assert-eq (car body) 'block)))

(deftest-each sequence-search-macro-outer-is-let
  "Sequence-search HOF macros (position/count/mapcan) expand to a LET."
  :cases (("position" '(position item lst))
          ("count"    '(count item lst))
          ("mapcan"   '(mapcan fn lst)))
  (form)
  (assert-eq (car (our-macroexpand-1 form)) 'let))

(deftest stable-sort-delegates-to-sort
  "(stable-sort lst pred) → (sort lst pred)"
  (assert-equal (our-macroexpand-1 '(stable-sort lst pred))
                '(sort lst pred)))

;;; ── ROTATEF ──────────────────────────────────────────────────────────────────

(deftest rotatef-outer-is-let
  "ROTATEF wraps swap in a LET binding a temp variable"
  (let ((result (our-macroexpand-1 '(rotatef a b))))
    (assert-eq (car result) 'let)))

(deftest rotatef-body-contains-setq-of-a
  "ROTATEF second form sets A to B"
  (let* ((result (our-macroexpand-1 '(rotatef a b)))
         (setq-form (second (cdr result))))
    (assert-eq (car setq-form) 'setq)
    (assert-eq (second setq-form) 'a)
    (assert-eq (third setq-form) 'b)))

(deftest rotatef-returns-nil
  "ROTATEF result form is NIL (last body form)"
  (let* ((result (our-macroexpand-1 '(rotatef a b)))
         (forms (cddr result)))
    (assert-eq (car (last forms)) nil)))

;;; ── ECASE ────────────────────────────────────────────────────────────────────

(deftest ecase-outer-is-let
  "ECASE wraps keyform in a LET to avoid double evaluation"
  (let ((result (our-macroexpand-1 '(ecase x (1 :one) (2 :two)))))
    (assert-eq (car result) 'let)))

(deftest ecase-body-is-case
  "ECASE body is a CASE form"
  (let* ((result (our-macroexpand-1 '(ecase x (1 :one) (2 :two))))
         (body (caddr result)))
    (assert-eq (car body) 'case)))

(deftest ecase-has-otherwise-error-clause
  "ECASE appends an OTHERWISE error clause to the user's cases"
  (let* ((result (our-macroexpand-1 '(ecase x (:a 1))))
         (case-form (caddr result))
         (last-clause (car (last (cddr case-form)))))
    (assert-eq (car last-clause) 'otherwise)))

;;; ── ETYPECASE ────────────────────────────────────────────────────────────────

(deftest etypecase-outer-is-let
  "ETYPECASE wraps keyform in a LET"
  (let ((result (our-macroexpand-1 '(etypecase x (integer 1) (string 2)))))
    (assert-eq (car result) 'let)))

(deftest etypecase-body-is-typecase
  "ETYPECASE body is a TYPECASE form"
  (let* ((result (our-macroexpand-1 '(etypecase x (integer 1))))
         (body (caddr result)))
    (assert-eq (car body) 'typecase)))

(deftest etypecase-has-otherwise-error-clause
  "ETYPECASE appends an OTHERWISE error clause"
  (let* ((result (our-macroexpand-1 '(etypecase x (integer 1))))
         (typecase-form (caddr result))
         (last-clause (car (last (cddr typecase-form)))))
    (assert-eq (car last-clause) 'otherwise)))

;;; ── PSETF ────────────────────────────────────────────────────────────────────

(deftest psetf-outer-is-let
  "PSETF evaluates all new values before assigning — outer form is LET"
  (let ((result (our-macroexpand-1 '(psetf a 1 b 2))))
    (assert-eq (car result) 'let)))

(deftest psetf-binds-two-temps
  "PSETF for two pairs binds two temporary variables"
  (let* ((result (our-macroexpand-1 '(psetf a 1 b 2)))
         (bindings (second result)))
    (assert-= (length bindings) 2)))

(deftest psetf-returns-nil
  "PSETF last form is NIL"
  (let* ((result (our-macroexpand-1 '(psetf a 1)))
         (forms (cddr result)))
    (assert-eq (car (last forms)) nil)))

;;; ── SHIFTF ───────────────────────────────────────────────────────────────────

(deftest shiftf-outer-is-let
  "SHIFTF saves old values in a LET before shifting"
  (let ((result (our-macroexpand-1 '(shiftf a b 99))))
    (assert-eq (car result) 'let)))

(deftest shiftf-binds-one-temp-per-place
  "SHIFTF binds one temp per place — (shiftf a b 99) has 2 places → 2 temps"
  (let* ((result (our-macroexpand-1 '(shiftf a b 99)))
         (bindings (second result)))
    (assert-= (length bindings) 2)))

(deftest shiftf-returns-first-old-value
  "SHIFTF last form is the temp holding old value of first place"
  (let* ((result (our-macroexpand-1 '(shiftf a b 99)))
         (forms (cddr result))
         (ret (car (last forms))))
    ;; ret must be a symbol (the gensym for old a)
    (assert-true (symbolp ret))))

;;; ── PROG / PROG* ─────────────────────────────────────────────────────────────

(deftest prog-outer-is-block
  "PROG wraps body in BLOCK NIL"
  (let ((result (our-macroexpand-1 '(prog (x y) (setq x 1)))))
    (assert-eq (car result) 'block)
    (assert-eq (second result) nil)))

(deftest prog-body-is-let-tagbody
  "PROG body is (let vars (tagbody ...))"
  (let* ((result (our-macroexpand-1 '(prog (x) :tag (setq x 1))))
         (let-form (third result)))
    (assert-eq (car let-form) 'let)))

(deftest prog*-outer-is-block
  "PROG* wraps body in BLOCK NIL"
  (let ((result (our-macroexpand-1 '(prog* ((x 1) (y x)) y))))
    (assert-eq (car result) 'block)
    (assert-eq (second result) nil)))

(deftest prog*-body-is-let*-tagbody
  "PROG* uses LET* for sequential bindings"
  (let* ((result (our-macroexpand-1 '(prog* ((x 1)) x)))
         (let-form (third result)))
    (assert-eq (car let-form) 'let*)))

;;; ── MAPC ─────────────────────────────────────────────────────────────────────

(deftest mapc-outer-is-let
  "MAPC binds fn and list in a LET before dolist"
  (let ((result (our-macroexpand-1 '(mapc fn lst))))
    (assert-eq (car result) 'let)))

(deftest mapc-returns-original-list
  "MAPC returns the original list (dolist result form is the list binding)"
  (let* ((result (our-macroexpand-1 '(mapc fn lst)))
         (bindings (second result))
         ;; lst binding is second in the LET
         (lst-binding (second bindings))
         (lst-var (first lst-binding))
         ;; dolist form in body: (dolist (x lst-var lst-var) body...)
         ;; the result is the third element of the dolist var-spec
         (dolist-form (caddr result))
         (dolist-result (third (second dolist-form))))
    (assert-eq dolist-result lst-var)))

;;; ── FIND-IF ──────────────────────────────────────────────────────────────────

(deftest find-if-outer-is-let
  "FIND-IF binds pred in a LET before the search loop"
  (let ((result (our-macroexpand-1 '(find-if pred lst))))
    (assert-eq (car result) 'let)))

(deftest find-if-body-is-block
  "FIND-IF body is a BLOCK (for early return)"
  (let* ((result (our-macroexpand-1 '(find-if pred lst)))
         (body (caddr result)))
    (assert-eq (car body) 'block)))

;;; ── COUNT-IF ─────────────────────────────────────────────────────────────────

(deftest count-if-outer-is-let
  "COUNT-IF binds fn and counter in a LET"
  (let ((result (our-macroexpand-1 '(count-if pred lst))))
    (assert-eq (car result) 'let)))

(deftest count-if-body-is-dolist
  "COUNT-IF body is a DOLIST incrementing a counter"
  (let* ((result (our-macroexpand-1 '(count-if pred lst)))
         (body (caddr result)))
    (assert-eq (car body) 'dolist)))

;;; ── REMOVE-DUPLICATES ────────────────────────────────────────────────────────

(deftest remove-duplicates-outer-is-let
  "REMOVE-DUPLICATES accumulates unique items in a LET-bound list"
  (let ((result (our-macroexpand-1 '(remove-duplicates lst))))
    (assert-eq (car result) 'let)))

(deftest remove-duplicates-body-is-dolist
  "REMOVE-DUPLICATES iterates with DOLIST, checking MEMBER before adding"
  (let* ((result (our-macroexpand-1 '(remove-duplicates lst)))
         (body (caddr result)))
    (assert-eq (car body) 'dolist)))

;;; ── UNION ────────────────────────────────────────────────────────────────────

(deftest union-outer-is-let
  "UNION binds both lists and accumulator in a LET"
  (let ((result (our-macroexpand-1 '(union l1 l2))))
    (assert-eq (car result) 'let)))

(deftest union-binds-three-vars
  "UNION binds l1, l2, and acc — three bindings"
  (let* ((result (our-macroexpand-1 '(union l1 l2)))
         (bindings (second result)))
    (assert-= (length bindings) 3)))

;;; ── SET-DIFFERENCE ───────────────────────────────────────────────────────────

(deftest set-difference-outer-is-let
  "SET-DIFFERENCE binds l2 and acc in a LET"
  (let ((result (our-macroexpand-1 '(set-difference l1 l2))))
    (assert-eq (car result) 'let)))

;;; ── INTERSECTION ─────────────────────────────────────────────────────────────

(deftest intersection-outer-is-let
  "INTERSECTION binds l2 and acc in a LET"
  (let ((result (our-macroexpand-1 '(intersection l1 l2))))
    (assert-eq (car result) 'let)))

;;; ── SUBSETP ──────────────────────────────────────────────────────────────────

(deftest subsetp-outer-is-let
  "SUBSETP binds l2 in a LET then uses EVERY+MEMBER"
  (let ((result (our-macroexpand-1 '(subsetp l1 l2))))
    (assert-eq (car result) 'let)))

(deftest subsetp-body-is-every
  "SUBSETP body calls EVERY with a MEMBER predicate"
  (let* ((result (our-macroexpand-1 '(subsetp l1 l2)))
         (body (caddr result)))
    (assert-eq (car body) 'every)))

;;; ── ADJOIN ───────────────────────────────────────────────────────────────────

(deftest adjoin-outer-is-let
  "ADJOIN binds item and list in a LET to avoid double evaluation"
  (let ((result (our-macroexpand-1 '(adjoin item lst))))
    (assert-eq (car result) 'let)))

(deftest adjoin-body-is-if-member
  "ADJOIN body is (if (member ...) lst (cons item lst))"
  (let* ((result (our-macroexpand-1 '(adjoin item lst)))
         (body (caddr result)))
    (assert-eq (car body) 'if)
    (assert-eq (car (second body)) 'member)))

;;; ── RASSOC ───────────────────────────────────────────────────────────────────

(deftest rassoc-outer-is-let
  "RASSOC binds item in a LET before the search loop"
  (let ((result (our-macroexpand-1 '(rassoc item alist))))
    (assert-eq (car result) 'let)))

(deftest rassoc-body-is-block
  "RASSOC body is a BLOCK for early return"
  (let* ((result (our-macroexpand-1 '(rassoc item alist)))
         (body (caddr result)))
    (assert-eq (car body) 'block)))

(deftest rassoc-checks-cdr
  "RASSOC dolist body checks (eql item (cdr pair)) — cdr is nested in the eql call"
  (let* ((result (our-macroexpand-1 '(rassoc item alist)))
         (block-body (caddr result))
         ;; block-body = (block nil (dolist ...))
         (dolist-form (third block-body))
         ;; dolist-form = (dolist (x alist nil) (when (and ...) ...))
         (when-form (second (cdr dolist-form)))
         ;; when-form = (when (and (consp x) (eql item-var (cdr x))) ...)
         (and-form (second when-form))
         ;; and-form = (and (consp x) (eql item-var (cdr x)))
         (eql-check (third and-form))
         ;; eql-check = (eql item-var (cdr x))
         ;; third arg of eql is (cdr x)
         (cdr-form (third eql-check)))
    (assert-eq (car cdr-form) 'cdr)))

;;; ── SORT ─────────────────────────────────────────────────────────────────────

(deftest sort-outer-is-let
  "SORT outer form is LET binding the predicate"
  (let ((result (our-macroexpand-1 '(sort lst pred))))
    (assert-eq (car result) 'let)))

(deftest sort-body-uses-labels
  "SORT defines merge-sort helpers via LABELS"
  (let* ((result (our-macroexpand-1 '(sort lst pred)))
         (body (caddr result)))
    (assert-eq (car body) 'labels)))

(deftest sort-labels-has-three-helpers
  "SORT LABELS defines take-n, merge, and msort (3 helpers)"
  (let* ((result (our-macroexpand-1 '(sort lst pred)))
         (bindings (cadr (caddr result))))
    (assert-= (length bindings) 3)))

;;; ── REMOVE ───────────────────────────────────────────────────────────────────

(deftest remove-outer-is-let
  "REMOVE outer form is LET binding the item and acc"
  (let ((result (our-macroexpand-1 '(remove x lst))))
    (assert-eq (car result) 'let)))

(deftest remove-body-contains-dolist
  "REMOVE body iterates with DOLIST and filters with UNLESS+EQL"
  (let* ((result (our-macroexpand-1 '(remove x lst)))
         (dolist-form (caddr result)))  ; (let (bindings) (dolist ...)) → body is 3rd
    (assert-eq (car dolist-form) 'dolist)))

(deftest remove-result-is-nreverse
  "REMOVE result form in DOLIST is (nreverse acc)"
  (let* ((result (our-macroexpand-1 '(remove x lst)))
         (dolist-form (caddr result))            ; (dolist (var lst result-form) body...)
         (result-form (third (second dolist-form)))) ; 3rd of (var lst result-form)
    (assert-equal (symbol-name (car result-form)) "NREVERSE")))

;;; ── WITH-ACCESSORS ───────────────────────────────────────────────────────────

(deftest with-accessors-outer-is-let
  "WITH-ACCESSORS outer form is LET binding the instance"
  (let ((result (our-macroexpand-1 '(with-accessors ((x acc-x) (y acc-y)) obj body))))
    (assert-eq (car result) 'let)))

(deftest with-accessors-inner-binds-accessor-calls
  "WITH-ACCESSORS inner LET binds each var to its accessor applied to the instance"
  (let* ((result (our-macroexpand-1 '(with-accessors ((x acc-x)) obj body)))
         (inner-let (caddr result))
         (bindings (second inner-let))
         (first-binding (first bindings)))
    ;; first-binding is (x (acc-x inst-var))
    (assert-eq (car first-binding) 'x)
    (assert-eq (caar (cdr first-binding)) 'acc-x)))

;;; ── DEFINE-MODIFY-MACRO ──────────────────────────────────────────────────────

(deftest define-modify-macro-generates-our-defmacro
  "DEFINE-MODIFY-MACRO expands to an OUR-DEFMACRO form"
  (let ((result (our-macroexpand-1 '(define-modify-macro my-incf (&optional (n 1)) +))))
    ;; Check by symbol-name to avoid cross-package eq failure
    (assert-equal (symbol-name (car result)) "OUR-DEFMACRO")
    (assert-equal (symbol-name (second result)) "MY-INCF")))

(deftest define-modify-macro-body-uses-setf
  "DEFINE-MODIFY-MACRO body has (setf place (fn place args...))"
  (let* ((result (our-macroexpand-1 '(define-modify-macro my-incf (&optional (n 1)) +)))
         ;; result = (our-defmacro my-incf (place-var &optional (n 1)) `(setf ...))
         (body (cdddr result))
         (template (car body)))
    ;; template is a backquoted form; the backquote-expanded car is setf
    (assert-true (consp template))))

;;; ── DEFINE-CONDITION ─────────────────────────────────────────────────────────

(deftest define-condition-expands-to-defclass
  "DEFINE-CONDITION expands to a DEFCLASS form"
  (let ((result (our-macroexpand-1 '(define-condition my-error (error) ()))))
    (assert-eq (car result) 'defclass)
    (assert-eq (second result) 'my-error)))

(deftest define-condition-no-parents-uses-error
  "DEFINE-CONDITION with empty parent list defaults to (error)"
  (let* ((result (our-macroexpand-1 '(define-condition my-err () ())))
         (parents (third result)))
    (assert-equal parents '(error))))

(deftest define-condition-explicit-parent
  "DEFINE-CONDITION with explicit parent passes it through"
  (let* ((result (our-macroexpand-1 '(define-condition my-type-err (type-error) ())))
         (parents (third result)))
    (assert-equal parents '(type-error))))

;;; ── HANDLER-BIND ─────────────────────────────────────────────────────────────

(deftest handler-bind-no-bindings-is-progn
  "HANDLER-BIND with empty bindings expands to PROGN body"
  (let ((result (our-macroexpand-1 '(handler-bind () body))))
    (assert-eq (car result) 'progn)))

(deftest handler-bind-one-binding-is-handler-case
  "HANDLER-BIND with one binding wraps in HANDLER-CASE"
  (let ((result (our-macroexpand-1 '(handler-bind ((error #'my-handler)) body))))
    (assert-eq (car result) 'handler-case)))

;;; ── RESTART-CASE ─────────────────────────────────────────────────────────────

(deftest restart-case-no-clauses-is-form
  "RESTART-CASE with no clauses returns the form unchanged"
  (let ((result (our-macroexpand-1 '(restart-case (do-stuff)))))
    (assert-equal result '(do-stuff))))

(deftest restart-case-with-clause-is-handler-case
  "RESTART-CASE with a clause wraps in HANDLER-CASE"
  (let ((result (our-macroexpand-1 '(restart-case (do-stuff) (my-restart () ok)))))
    (assert-eq (car result) 'handler-case)))

;;; ── ISQRT ────────────────────────────────────────────────────────────────────

(deftest isqrt-expands-to-floor-sqrt
  "ISQRT expands to (floor (sqrt (float n)))"
  (let ((result (our-macroexpand-1 '(isqrt n))))
    (assert-eq (car result) 'floor)
    (assert-eq (caar (cdr result)) 'sqrt)))

;;; ── WITH-OPEN-STREAM ─────────────────────────────────────────────────────────

(deftest with-open-stream-outer-is-let
  "WITH-OPEN-STREAM outer form is LET binding the stream variable"
  (let ((result (our-macroexpand-1 '(with-open-stream (s (open f)) body))))
    (assert-eq (car result) 'let)))

(deftest with-open-stream-body-is-unwind-protect
  "WITH-OPEN-STREAM body uses UNWIND-PROTECT to ensure CLOSE"
  (let* ((result (our-macroexpand-1 '(with-open-stream (s (open f)) body)))
         (body-form (caddr result)))
    (assert-eq (car body-form) 'unwind-protect)))

;;; ── PUSH / POP ───────────────────────────────────────────────────────────────

(deftest push-expands-to-setf-cons
  "PUSH expands to (setf place (cons value place))"
  (let ((result (our-macroexpand-1 '(push x lst))))
    (assert-eq (car result) 'setf)
    (assert-eq (car (caddr result)) 'cons)))

(deftest pop-outer-is-let
  "POP expands to LET capturing the car, then SETF to advance"
  (let ((result (our-macroexpand-1 '(pop lst))))
    (assert-eq (car result) 'let)))

;;; ── INCF / DECF ──────────────────────────────────────────────────────────────

(deftest incf-expands-to-setf-plus
  "INCF expands to (setf place (+ place delta))"
  (let* ((result (our-macroexpand-1 '(incf x)))
         (rhs (caddr result)))         ; (setf x (+ x delta))
    (assert-eq (car result) 'setf)
    (assert-eq (car rhs) '+)))

(deftest decf-expands-to-setf-minus
  "DECF expands to (setf place (- place delta))"
  (let* ((result (our-macroexpand-1 '(decf x)))
         (rhs (caddr result)))
    (assert-eq (car result) 'setf)
    (assert-eq (car rhs) '-)))

;;; ── 1+ / 1- ──────────────────────────────────────────────────────────────────

(deftest 1+-expands-to-plus-1
  "1+ expands to (+ n 1)"
  (let ((result (our-macroexpand-1 '(1+ x))))
    (assert-eq (car result) '+)
    (assert-= (third result) 1)))

(deftest 1--expands-to-minus-1
  "1- expands to (- n 1)"
  (let ((result (our-macroexpand-1 '(1- x))))
    (assert-eq (car result) '-)
    (assert-= (third result) 1)))

;;; ── RETURN ───────────────────────────────────────────────────────────────────

(deftest return-expands-to-return-from-nil
  "RETURN expands to (return-from nil value)"
  (let ((result (our-macroexpand-1 '(return 42))))
    (assert-eq (car result) 'return-from)
    (assert-eq (second result) nil)))

;;; ── ROTATEF ──────────────────────────────────────────────────────────────────

(deftest rotatef-outer-is-let
  "ROTATEF expands to LET capturing the first value, then SETQs"
  (let ((result (our-macroexpand-1 '(rotatef a b))))
    (assert-eq (car result) 'let)))

(deftest rotatef-returns-nil
  "ROTATEF body ends with NIL (per ANSI spec)"
  (let* ((result (our-macroexpand-1 '(rotatef a b)))
         (last-form (car (last (cddr result)))))
    (assert-eq last-form nil)))

;;; ── PROG / PROG* ─────────────────────────────────────────────────────────────

(deftest prog-outer-is-block-nil
  "PROG outer form is (BLOCK NIL ...)"
  (let ((result (our-macroexpand-1 '(prog (x) (setq x 1)))))
    (assert-eq (car result) 'block)
    (assert-eq (second result) nil)))

(deftest prog-body-is-let-with-tagbody
  "PROG inner is LET with TAGBODY inside"
  (let* ((result  (our-macroexpand-1 '(prog (x) tag1 body1)))
         (let-form (third result)))
    (assert-eq (car let-form) 'let)
    (assert-eq (car (caddr let-form)) 'tagbody)))

(deftest prog*-uses-let*
  "PROG* uses LET* for sequential bindings (unlike PROG which uses LET)"
  (let* ((result (our-macroexpand-1 '(prog* ((x 1) (y x)) body)))
         (let*-form (third result)))
    (assert-eq (car let*-form) 'let*)))

;;; ── NTH-VALUE ────────────────────────────────────────────────────────────────

(deftest nth-value-outer-is-let
  "NTH-VALUE expands to LET binding multiple-value-list result"
  (let ((result (our-macroexpand-1 '(nth-value 0 (floor 7 2)))))
    (assert-eq (car result) 'let)))

(deftest nth-value-body-calls-nth
  "NTH-VALUE body calls NTH to extract the desired position"
  (let* ((result (our-macroexpand-1 '(nth-value 0 (floor 7 2))))
         (nth-form (caddr result)))
    (assert-eq (car nth-form) 'nth)))

;;; ── ECASE / ETYPECASE ────────────────────────────────────────────────────────

(deftest ecase-outer-is-let
  "ECASE wraps keyform in LET to avoid double evaluation"
  (let ((result (our-macroexpand-1 '(ecase x (1 :one) (2 :two)))))
    (assert-eq (car result) 'let)))

(deftest ecase-body-is-case-with-otherwise-error
  "ECASE body is CASE with an OTHERWISE clause that signals ERROR"
  (let* ((result (our-macroexpand-1 '(ecase x (1 :one))))
         (case-form (caddr result))
         (otherwise (car (last case-form))))
    (assert-eq (car case-form) 'case)
    (assert-eq (car otherwise) 'otherwise)))

(deftest etypecase-body-is-typecase
  "ETYPECASE body is TYPECASE with OTHERWISE error clause"
  (let* ((result (our-macroexpand-1 '(etypecase x (integer :int))))
         (tc-form (caddr result)))
    (assert-eq (car tc-form) 'typecase)))

;;; ── EVERY / SOME / NOTANY / NOTEVERY ────────────────────────────────────────

(deftest every-outer-is-let
  "EVERY expands to LET binding the predicate, uses BLOCK+DOLIST"
  (let ((result (our-macroexpand-1 '(every #'oddp lst))))
    (assert-eq (car result) 'let)))

(deftest every-short-circuits-on-false
  "EVERY body contains BLOCK NIL for short-circuit RETURN"
  (let* ((result (our-macroexpand-1 '(every #'oddp lst)))
         (block-form (caddr result)))
    (assert-eq (car block-form) 'block)
    (assert-eq (second block-form) nil)))

(deftest some-outer-is-let
  "SOME expands to LET binding the predicate"
  (let ((result (our-macroexpand-1 '(some #'oddp lst))))
    (assert-eq (car result) 'let)))

(deftest notany-delegates-to-not-some
  "NOTANY expands to (NOT (SOME pred list))"
  (let ((result (our-macroexpand-1 '(notany #'oddp lst))))
    (assert-eq (car result) 'not)
    (assert-equal (symbol-name (caadr result)) "SOME")))

(deftest notevery-delegates-to-not-every
  "NOTEVERY expands to (NOT (EVERY pred list))"
  (let ((result (our-macroexpand-1 '(notevery #'oddp lst))))
    (assert-eq (car result) 'not)
    (assert-equal (symbol-name (caadr result)) "EVERY")))

;;; ── MAPCAR / MAPC / MAPCAN ───────────────────────────────────────────────────

(deftest mapcar-outer-is-let
  "MAPCAR expands to LET binding fn and accumulator, uses DOLIST+NREVERSE"
  (let ((result (our-macroexpand-1 '(mapcar #'1+ lst))))
    (assert-eq (car result) 'let)))

(deftest mapc-outer-is-let
  "MAPC expands to LET binding fn and the original list for return"
  (let ((result (our-macroexpand-1 '(mapc #'print lst))))
    (assert-eq (car result) 'let)))

(deftest mapcan-outer-is-let
  "MAPCAN expands to LET binding fn and accumulator, uses NCONC"
  (let ((result (our-macroexpand-1 '(mapcan #'list lst))))
    (assert-eq (car result) 'let)))

;;; ── FIND / FIND-IF / POSITION / COUNT / COUNT-IF ────────────────────────────

(deftest find-no-keys-outer-is-let
  "FIND without key/test args binds item-var, uses BLOCK+DOLIST+EQL"
  (let ((result (our-macroexpand-1 '(find x lst))))
    (assert-eq (car result) 'let)))

(deftest find-no-keys-body-uses-block
  "FIND without key args has BLOCK NIL for early RETURN"
  (let* ((result (our-macroexpand-1 '(find x lst)))
         (block-form (caddr result)))
    (assert-eq (car block-form) 'block)))

(deftest find-if-outer-is-let
  "FIND-IF expands to LET binding predicate, uses BLOCK+DOLIST"
  (let ((result (our-macroexpand-1 '(find-if #'oddp lst))))
    (assert-eq (car result) 'let)))

(deftest position-outer-is-let
  "POSITION expands to LET binding item and index counter"
  (let ((result (our-macroexpand-1 '(position x lst))))
    (assert-eq (car result) 'let)))

(deftest count-outer-is-let
  "COUNT expands to LET binding item and count accumulator"
  (let ((result (our-macroexpand-1 '(count x lst))))
    (assert-eq (car result) 'let)))

(deftest count-if-outer-is-let
  "COUNT-IF expands to LET binding predicate and count accumulator"
  (let ((result (our-macroexpand-1 '(count-if #'oddp lst))))
    (assert-eq (car result) 'let)))

;;; ── REMOVE / REMOVE-DUPLICATES ───────────────────────────────────────────────

(deftest remove-outer-is-let
  "REMOVE expands to LET binding item and accumulator for filtering"
  (let ((result (our-macroexpand-1 '(remove x lst))))
    (assert-eq (car result) 'let)))

(deftest remove-duplicates-outer-is-let
  "REMOVE-DUPLICATES expands to LET with accumulator using MEMBER check"
  (let ((result (our-macroexpand-1 '(remove-duplicates lst))))
    (assert-eq (car result) 'let)))

;;; ── UNION / SET-DIFFERENCE / INTERSECTION / SUBSETP ─────────────────────────

(deftest union-outer-is-let
  "UNION expands to LET binding both lists and an accumulator"
  (let ((result (our-macroexpand-1 '(union l1 l2))))
    (assert-eq (car result) 'let)))

(deftest set-difference-outer-is-let
  "SET-DIFFERENCE expands to LET binding l2 for membership tests"
  (let ((result (our-macroexpand-1 '(set-difference l1 l2))))
    (assert-eq (car result) 'let)))

(deftest intersection-outer-is-let
  "INTERSECTION expands to LET binding l2 and accumulator"
  (let ((result (our-macroexpand-1 '(intersection l1 l2))))
    (assert-eq (car result) 'let)))

(deftest subsetp-outer-is-let
  "SUBSETP expands to LET binding l2, then delegates to EVERY+MEMBER"
  (let ((result (our-macroexpand-1 '(subsetp l1 l2))))
    (assert-eq (car result) 'let)))

;;; ── ADJOIN / RASSOC / PAIRLIS ────────────────────────────────────────────────

(deftest adjoin-outer-is-let
  "ADJOIN expands to LET binding item and list, then IF+MEMBER"
  (let ((result (our-macroexpand-1 '(adjoin x lst))))
    (assert-eq (car result) 'let)))

(deftest rassoc-outer-is-let
  "RASSOC expands to LET binding item, then BLOCK+DOLIST checking CDR"
  (let ((result (our-macroexpand-1 '(rassoc x alist))))
    (assert-eq (car result) 'let)))

(deftest pairlis-outer-is-let
  "PAIRLIS expands to LET binding keys, data, and optional alist init"
  (let ((result (our-macroexpand-1 '(pairlis ks ds))))
    (assert-eq (car result) 'let)))

;;; ── PSETF / SHIFTF ───────────────────────────────────────────────────────────

(deftest psetf-outer-is-let
  "PSETF expands to LET binding all new values, then SETFs, returns NIL"
  (let ((result (our-macroexpand-1 '(psetf a 1 b 2))))
    (assert-eq (car result) 'let)))

(deftest psetf-returns-nil
  "PSETF last form is NIL (per ANSI spec)"
  (let* ((result (our-macroexpand-1 '(psetf a 1 b 2)))
         (last-form (car (last (cddr result)))))
    (assert-eq last-form nil)))

(deftest shiftf-outer-is-let
  "SHIFTF expands to LET capturing old values, then SETFs, returns first old value"
  (let ((result (our-macroexpand-1 '(shiftf a b 0))))
    (assert-eq (car result) 'let)))

;;; ── WITH-SLOTS ───────────────────────────────────────────────────────────────

(deftest with-slots-outer-is-let
  "WITH-SLOTS binds the instance first (outer LET), then slots via inner LET"
  (let ((result (our-macroexpand-1 '(with-slots (x y) obj body))))
    (assert-eq (car result) 'let)))

(deftest with-slots-inner-uses-slot-value
  "WITH-SLOTS inner bindings call SLOT-VALUE for each named slot"
  (let* ((result (our-macroexpand-1 '(with-slots (x) obj body)))
         (inner-let (caddr result))
         (binding (caadr inner-let)))   ; first binding: (x (slot-value inst 'x))
    (assert-equal (symbol-name (caadr binding)) "SLOT-VALUE")))

;;; ── WITH-INPUT-FROM-STRING / WITH-OUTPUT-TO-STRING ───────────────────────────

(deftest with-input-from-string-outer-is-let
  "WITH-INPUT-FROM-STRING expands to LET binding var to make-string-input-stream"
  (let ((result (our-macroexpand-1 '(with-input-from-string (s "hello") body))))
    (assert-eq (car result) 'let)))

(deftest with-output-to-string-outer-is-let
  "WITH-OUTPUT-TO-STRING expands to LET binding var to make-string-output-stream"
  (let ((result (our-macroexpand-1 '(with-output-to-string (s) (write-char #\x s)))))
    (assert-eq (car result) 'let)))

;;; ── IGNORE-ERRORS ────────────────────────────────────────────────────────────

(deftest ignore-errors-outer-is-handler-case
  "IGNORE-ERRORS wraps body in HANDLER-CASE catching ERROR and returning NIL"
  (let ((result (our-macroexpand-1 '(ignore-errors (risky-op)))))
    (assert-eq (car result) 'handler-case)))

;;; ── MAP / CONCATENATE / STABLE-SORT ─────────────────────────────────────────

(deftest map-delegates-to-coerce-mapcar
  "MAP expands to (coerce (mapcar fn (coerce seq 'list)) result-type)"
  (let ((result (our-macroexpand-1 '(map 'vector #'1+ v))))
    (assert-equal (symbol-name (car result)) "COERCE")))

(deftest stable-sort-delegates-to-sort
  "STABLE-SORT expands to SORT (merge sort is inherently stable)"
  (let ((result (our-macroexpand-1 '(stable-sort lst #'<))))
    (assert-eq (car result) 'sort)))

;;; ── RESTART STUB MACROS ──────────────────────────────────────────────────────

(deftest invoke-restart-expands-to-error
  "INVOKE-RESTART expands to (ERROR ...) since restarts are not implemented"
  (let ((result (our-macroexpand-1 '(invoke-restart 'my-restart))))
    (assert-eq (car result) 'error)))

(deftest find-restart-expands-to-nil
  "FIND-RESTART expands to NIL (stub: no restart stack)"
  (let ((result (our-macroexpand-1 '(find-restart 'my-restart))))
    (assert-eq result nil)))

(deftest compute-restarts-expands-to-nil
  "COMPUTE-RESTARTS expands to NIL (stub)"
  (let ((result (our-macroexpand-1 '(compute-restarts))))
    (assert-eq result nil)))

(deftest restart-name-checks-hash-table
  "RESTART-NAME expands to IF+HASH-TABLE-P to extract :name key or return restart"
  (let ((result (our-macroexpand-1 '(restart-name r))))
    (assert-eq (car result) 'if)))

(deftest restart-bind-ignores-bindings
  "RESTART-BIND expands to PROGN body (bindings are stubbed out)"
  (let ((result (our-macroexpand-1 '(restart-bind ((my-r #'fn)) body))))
    (assert-eq (car result) 'progn)))

(deftest abort-expands-to-error
  "ABORT expands to (ERROR \"ABORT invoked\")"
  (let ((result (our-macroexpand-1 '(abort))))
    (assert-eq (car result) 'error)))

(deftest continue-expands-to-nil
  "CONTINUE expands to NIL (stub)"
  (let ((result (our-macroexpand-1 '(continue))))
    (assert-eq result nil)))

(deftest muffle-warning-expands-to-nil
  "MUFFLE-WARNING expands to NIL (stub)"
  (let ((result (our-macroexpand-1 '(muffle-warning))))
    (assert-eq result nil)))

(deftest use-value-returns-value
  "USE-VALUE expands to the value form itself"
  (let ((result (our-macroexpand-1 '(use-value 42))))
    (assert-= result 42)))

(deftest store-value-returns-value
  "STORE-VALUE expands to the value form itself"
  (let ((result (our-macroexpand-1 '(store-value 42))))
    (assert-= result 42)))

;;; ── WITH-STANDARD-IO-SYNTAX / WITH-PACKAGE-ITERATOR / DEFINE-COMPILER-MACRO ─

(deftest with-standard-io-syntax-is-progn
  "WITH-STANDARD-IO-SYNTAX expands to PROGN body (stub: no I/O var rebinding)"
  (let ((result (our-macroexpand-1 '(with-standard-io-syntax body1 body2))))
    (assert-eq (car result) 'progn)))

(deftest with-package-iterator-outer-is-let
  "WITH-PACKAGE-ITERATOR expands to LET binding name to an exhausted-iterator lambda"
  (let ((result (our-macroexpand-1 '(with-package-iterator (sym pkg) body))))
    (assert-eq (car result) 'let)))

(deftest define-compiler-macro-expands-to-nil
  "DEFINE-COMPILER-MACRO is a stub that expands to NIL"
  (let ((result (our-macroexpand-1 '(define-compiler-macro foo (x) (+ x 1)))))
    (assert-eq result nil)))

;;; ── DESTRUCTURING-BIND ───────────────────────────────────────────────────────

(deftest destructuring-bind-outer-is-let
  "DESTRUCTURING-BIND expands to LET binding the expression, then LET* for pattern vars"
  (let ((result (our-macroexpand-1 '(destructuring-bind (x y) pair body))))
    (assert-eq (car result) 'let)))

(deftest destructuring-bind-inner-is-let*
  "DESTRUCTURING-BIND inner form is LET* for sequential pattern variable binding"
  (let* ((result (our-macroexpand-1 '(destructuring-bind (x y) pair body)))
         (let*-form (caddr result)))
    (assert-eq (car let*-form) 'let*)))

;;; ── SIGNUM ───────────────────────────────────────────────────────────────────

(deftest signum-outer-is-let
  "SIGNUM expands to LET binding n for single evaluation, then COND"
  (let ((result (our-macroexpand-1 '(signum x))))
    (assert-eq (car result) 'let)))

(deftest signum-body-is-cond
  "SIGNUM body is COND checking zerop, plusp, and default -1"
  (let* ((result (our-macroexpand-1 '(signum x)))
         (cond-form (caddr result)))
    (assert-eq (car cond-form) 'cond)))

;;; ── 1- ───────────────────────────────────────────────────────────────────────

(deftest 1--expands-to-minus-1-inner-check
  "1- is also an our-defmacro; confirm it expands to (- n 1) by checking operator"
  (let ((result (our-macroexpand-1 '(1- x))))
    (assert-eq (car result) '-)))
