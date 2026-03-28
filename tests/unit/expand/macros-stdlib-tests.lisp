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

(deftest pop-expansion
  "POP expands to (let ((tmp place)) (setf place (cdr tmp)) (car tmp)) — reads place once."
  (let* ((result      (our-macroexpand-1 '(pop lst)))
         (bindings    (cadr result))       ; ((#:TMP lst))
         (setf-form   (caddr result))      ; (setf lst (cdr #:TMP))
         (tmp-sym     (caar bindings)))    ; the gensym bound to lst
    (assert-eq   (car result)               'let)
    ;; Binding binds tmp gensym to lst
    (assert-equal (cadar bindings)          'lst)
    ;; Setf form structure: (setf lst (cdr tmp))
    (assert-eq   (car setf-form)            'setf)
    (assert-eq   (cadr setf-form)           'lst)
    ;; Value arg to setf is (cdr tmp)
    (assert-eq   (car (caddr setf-form))    'cdr)
    (assert-eq   (cadr (caddr setf-form))   tmp-sym)))

(deftest-each incf-decf-expansion
  "incf/decf expand to (setq x (OP x delta)) for simple symbol places."
  :cases (("incf-default" '(incf x)   '(setq x (+ x 1)))
          ("incf-custom"  '(incf x 5) '(setq x (+ x 5)))
          ("decf-default" '(decf x)   '(setq x (- x 1)))
          ("decf-custom"  '(decf x 3) '(setq x (- x 3))))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

;;; ── Numeric shorthand ────────────────────────────────────────────────────────

(deftest-each 1+-1--expansion
  "1+ and 1- are shorthand for (+ n 1) and (- n 1)."
  :cases (("1+" '(1+ n) '(+ n 1))
          ("1-" '(1- n) '(- n 1)))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest signum-expansion
  "SIGNUM wraps n in a LET (avoids double evaluation) and dispatches via COND."
  (let* ((result (our-macroexpand-1 '(signum n)))
         (body   (caddr result)))
    (assert-eq (car result) 'let)
    (assert-eq (car body)   'cond)))

;;; ── RETURN ───────────────────────────────────────────────────────────────────

(deftest-each return-expansion
  "return expands to (return-from nil ...) with optional value."
  :cases (("with-value" '(return v)  '(return-from nil v))
          ("no-value"   '(return)    '(return-from nil nil)))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

;;; ── WITH-SLOTS ───────────────────────────────────────────────────────────────

(deftest with-slots-expansion
  "WITH-SLOTS: outer LET binds instance; inner SYMBOL-MACROLET binds each slot."
  (let* ((result     (our-macroexpand-1 '(with-slots (x) obj body)))
         (inner-form (caddr result))
         (binding    (caadr inner-form)))
    (assert-eq (car result)     'let)
    (assert-eq (car inner-form) 'symbol-macrolet)
    (assert-eq (car binding)    'x)
    (assert-eq (caadr binding)  'slot-value)))

;;; ── NTH-VALUE ────────────────────────────────────────────────────────────────

(deftest nth-value-expansion
  "NTH-VALUE with constant N expands to MULTIPLE-VALUE-BIND selecting the Nth var."
  (let* ((result (our-macroexpand-1 '(nth-value 1 form))))
    (assert-eq (car result) 'multiple-value-bind)
    ;; Should bind 2 vars (v0 v1), return v1
    (assert-= (length (second result)) 2)))

;;; ── DESTRUCTURING-BIND ───────────────────────────────────────────────────────

(deftest destructuring-bind-expansion
  "DESTRUCTURING-BIND outer form is LET; inner form is LET* with destructured bindings."
  (let* ((result (our-macroexpand-1 '(destructuring-bind (a b) expr body)))
         (inner  (caddr result)))
    (assert-eq (car result) 'let)
    (assert-eq (car inner)  'let*)))

;;; ── ASSERT ───────────────────────────────────────────────────────────────────

(deftest assert-expansion
  "ASSERT: outer is UNLESS, body is CERROR (continuable); custom datum is passed through."
  (let* ((basic  (our-macroexpand-1 '(assert test)))
         (custom (our-macroexpand-1 '(assert test () "bad input"))))
    (assert-eq    (car  basic)          'unless)
    (assert-equal (cadr basic)          'test)
    (assert-eq    (car (caddr basic))   'cerror)
    (assert-eq    (car (caddr custom))  'cerror)
    (assert-equal (caddr (caddr custom)) "bad input")))

;;; ── IGNORE-ERRORS ────────────────────────────────────────────────────────────

(deftest ignore-errors-expansion
  "IGNORE-ERRORS wraps in HANDLER-CASE with an ERROR handler clause."
  (let* ((result (our-macroexpand-1 '(ignore-errors expr)))
         (clause (caddr result)))
    (assert-eq (car result) 'handler-case)
    (assert-eq (car clause) 'error)))

;;; ── CONCATENATE ──────────────────────────────────────────────────────────────

(deftest-each concatenate-small-arities
  "CONCATENATE expands to empty string or passthrough for 0-1 strings"
  :cases (("zero-strings"  '(concatenate 'string)      "")
          ("one-string"    '(concatenate 'string "a")   "a"))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest concatenate-expansion
  "CONCATENATE: 2-arg → (string-concat a b); 3-arg → left-associative nesting."
  (let ((result (our-macroexpand-1 '(concatenate 'string "a" "b"))))
    (assert-equal "STRING-CONCAT" (symbol-name (car result)))
    (assert-equal '("a" "b") (cdr result)))
  (let ((result (our-macroexpand-1 '(concatenate 'string "a" "b" "c"))))
    (assert-equal "STRING-CONCAT" (symbol-name (car result)))
    (assert-equal "STRING-CONCAT" (symbol-name (caadr result)))
    (assert-equal "c" (caddr result))))

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

(deftest every-short-circuits-on-false
  "EVERY body is a BLOCK NIL (for short-circuit RETURN); contains DOLIST"
  (let* ((result     (our-macroexpand-1 '(every pred lst)))
         (block-form (caddr result)))
    (assert-eq (car  block-form) 'block)
    (assert-eq (second block-form) nil)))

(deftest-each notany-notevery-negation
  "notany/notevery are simple (not ...) wrappers around some/every."
  :cases (("notany"   '(notany   pred lst) '(not (some  pred lst)))
          ("notevery" '(notevery pred lst) '(not (every pred lst))))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

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

(deftest rotatef-expansion
  "ROTATEF: outer LET binds a temp; body sets A to B via SETQ; returns NIL."
  (let* ((result    (our-macroexpand-1 '(rotatef a b)))
         (setq-form (second (cdr result)))
         (forms     (cddr result)))
    (assert-eq  (car result)            'let)
    (assert-eq  (car setq-form)         'setq)
    (assert-eq  (second setq-form)      'a)
    (assert-eq  (third setq-form)       'b)
    (assert-eq  (car (last forms))      nil)))

;;; ── ECASE / ETYPECASE ────────────────────────────────────────────────────────

(deftest-each exhaustive-case-outer-is-let
  "ECASE and ETYPECASE wrap keyform in a LET to avoid double evaluation."
  :cases (("ecase"     '(ecase x (1 :one) (2 :two)))
          ("etypecase" '(etypecase x (integer 1) (string 2))))
  (form)
  (assert-eq (car (our-macroexpand-1 form)) 'let))

(deftest-each exhaustive-case-inner-form
  "ECASE body is CASE; ETYPECASE body is TYPECASE."
  :cases (("ecase"     '(ecase x (1 :one) (2 :two))     'case)
          ("etypecase" '(etypecase x (integer 1))        'typecase))
  (form inner-head)
  (let* ((result (our-macroexpand-1 form))
         (body   (caddr result)))
    (assert-eq (car body) inner-head)))

(deftest-each exhaustive-case-has-otherwise-clause
  "ECASE and ETYPECASE append an OTHERWISE error clause."
  :cases (("ecase"     '(ecase x (:a 1))         'case)
          ("etypecase" '(etypecase x (integer 1)) 'typecase))
  (form inner-head)
  (let* ((result      (our-macroexpand-1 form))
         (inner-form  (caddr result))
         (last-clause (car (last (cddr inner-form)))))
    (assert-eq (car inner-form) inner-head)
    (assert-eq (car last-clause) 'otherwise)))

;;; ── PSETF ────────────────────────────────────────────────────────────────────

(deftest psetf-expansion
  "PSETF: outer LET evaluates all new values before assigning; returns NIL."
  (let* ((result2   (our-macroexpand-1 '(psetf a 1 b 2)))
         (result1   (our-macroexpand-1 '(psetf a 1)))
         (bindings  (second result2))
         (forms     (cddr result1)))
    (assert-eq (car result2)          'let)
    (assert-=  (length bindings)      2)
    (assert-eq (car (last forms))     nil)))

;;; ── SHIFTF ───────────────────────────────────────────────────────────────────

(deftest shiftf-expansion
  "SHIFTF: outer LET saves old values; one temp per place; returns first old value."
  (let* ((result   (our-macroexpand-1 '(shiftf a b 99)))
         (bindings (second result))
         (forms    (cddr result))
         (ret      (car (last forms))))
    (assert-eq (car result)     'let)
    (assert-=  (length bindings) 2)
    (assert-true (symbolp ret))))

;;; ── PROG / PROG* ─────────────────────────────────────────────────────────────

(deftest-each prog-prog*-outer-is-block
  "PROG and PROG* both wrap their body in BLOCK NIL."
  :cases (("prog"  '(prog  (x y)       (setq x 1)))
          ("prog*" '(prog* ((x 1) (y x)) y)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq (car result)    'block)
    (assert-eq (second result) nil)))

(deftest-each prog-inner-binding-form
  "PROG inner form is LET (parallel); PROG* inner form is LET* (sequential)."
  :cases (("prog"  '(prog  (x) :tag (setq x 1)) 'let)
          ("prog*" '(prog* ((x 1)) x)           'let*))
  (form let-head)
  (let* ((result   (our-macroexpand-1 form))
         (let-form (third result)))
    (assert-eq (car let-form) let-head)))

;;; ── MAPC ─────────────────────────────────────────────────────────────────────

(deftest mapc-expansion
  "MAPC binds fn and list in a LET; DOLIST result form is the original list."
  (let* ((result      (our-macroexpand-1 '(mapc fn lst)))
         (bindings    (second result))
         (lst-var     (first (second bindings)))
         (dolist-form (caddr result))
         (dolist-ret  (third (second dolist-form))))
    (assert-eq (car result)  'let)
    (assert-eq dolist-ret    lst-var)))

;;; ── FIND-IF / COUNT-IF / REMOVE-DUPLICATES ───────────────────────────────────

(deftest-each list-scan-macro-outer-is-let
  "Predicate-based list-scan macros all expand to a LET."
  :cases (("find-if"           '(find-if pred lst))
          ("count-if"          '(count-if pred lst))
          ("remove-duplicates" '(remove-duplicates lst)))
  (form)
  (assert-eq (car (our-macroexpand-1 form)) 'let))

(deftest-each list-scan-macro-body-type
  "Predicate-based list-scan macros have their expected inner control form."
  :cases (("find-if"           '(find-if pred lst)      'block)
          ("count-if"          '(count-if pred lst)     'dolist)
          ("remove-duplicates" '(remove-duplicates lst) 'dolist))
  (form body-type)
  (assert-eq body-type (car (caddr (our-macroexpand-1 form)))))

;;; ── Set operations ───────────────────────────────────────────────────────────

(deftest-each set-op-outer-is-let
  "Set-operation macros all wrap their work in a LET."
  :cases (("union"          '(union l1 l2))
          ("set-difference" '(set-difference l1 l2))
          ("intersection"   '(intersection l1 l2))
          ("subsetp"        '(subsetp l1 l2))
          ("adjoin"         '(adjoin item lst))
          ("pairlis"        '(pairlis ks ds)))
  (form)
  (assert-eq (car (our-macroexpand-1 form)) 'let))

(deftest union-binds-three-vars
  "UNION binds l1, l2, and acc — three bindings"
  (let* ((result (our-macroexpand-1 '(union l1 l2)))
         (bindings (second result)))
    (assert-= (length bindings) 3)))

(deftest subsetp-body-is-every
  "SUBSETP body calls EVERY with a MEMBER predicate"
  (let* ((result (our-macroexpand-1 '(subsetp l1 l2)))
         (body (caddr result)))
    (assert-eq (car body) 'every)))

;;; ── ADJOIN ───────────────────────────────────────────────────────────────────

(deftest adjoin-body-is-if-member
  "ADJOIN body is (if (member ...) lst (cons item lst))"
  (let* ((result (our-macroexpand-1 '(adjoin item lst)))
         (body (caddr result)))
    (assert-eq (car body) 'if)
    (assert-eq (car (second body)) 'member)))

;;; ── RASSOC ───────────────────────────────────────────────────────────────────

(deftest rassoc-expansion
  "RASSOC: outer LET + BLOCK; dolist body checks (eql item (cdr pair))."
  (let* ((result      (our-macroexpand-1 '(rassoc item alist)))
         (block-body  (caddr result))
         (dolist-form (third block-body))
         (when-form   (second (cdr dolist-form)))
         (and-form    (second when-form))
         (eql-check   (third and-form))
         (cdr-form    (third eql-check)))
    (assert-eq 'let (car result))
    (assert-eq 'block (car block-body))
    (assert-eq 'cdr (car cdr-form))))

;;; ── SORT ─────────────────────────────────────────────────────────────────────

(deftest sort-expansion
  "SORT outer LET binds the predicate; body is LABELS defining 3 merge-sort helpers."
  (let* ((result   (our-macroexpand-1 '(sort lst pred)))
         (body     (caddr result))
         (bindings (cadr body)))
    (assert-eq (car result) 'let)
    (assert-eq (car body)   'labels)
    (assert-=  (length bindings) 3)))

;;; ── REMOVE ───────────────────────────────────────────────────────────────────

(deftest remove-expansion
  "REMOVE: outer LET binds item and acc; body is DOLIST; result is (nreverse acc)."
  (let* ((result      (our-macroexpand-1 '(remove x lst)))
         (dolist-form (caddr result))
         (result-form (third (second dolist-form))))
    (assert-eq    (car result)      'let)
    (assert-eq    (car dolist-form) 'dolist)
    (assert-equal (symbol-name (car result-form)) "NREVERSE")))

;;; ── WITH-ACCESSORS ───────────────────────────────────────────────────────────

(deftest with-accessors-outer-is-let
  "WITH-ACCESSORS outer form is LET; inner is SYMBOL-MACROLET binding accessor calls."
  (let* ((result (our-macroexpand-1 '(with-accessors ((x acc-x)) obj body)))
         (inner-form (caddr result)))
    (assert-eq (car result) 'let)
    (assert-eq (car inner-form) 'symbol-macrolet)))

;;; ── DEFINE-MODIFY-MACRO ──────────────────────────────────────────────────────

(deftest define-modify-macro-expansion
  "DEFINE-MODIFY-MACRO: expands to OUR-DEFMACRO with correct name and a cons body template."
  (let* ((result   (our-macroexpand-1 '(define-modify-macro my-incf (&optional (n 1)) +)))
         (body     (cdddr result))
         (template (car body)))
    (assert-equal "OUR-DEFMACRO" (symbol-name (car result)))
    (assert-equal "MY-INCF" (symbol-name (second result)))
    (assert-true (consp template))))

;;; ── DEFINE-CONDITION ─────────────────────────────────────────────────────────

(deftest define-condition-expands-to-defclass
  "DEFINE-CONDITION expands to a DEFCLASS form with the given name."
  (let ((result (our-macroexpand-1 '(define-condition my-error (error) ()))))
    (assert-eq    (car result)    'defclass)
    (assert-eq    (second result) 'my-error)))

(deftest-each define-condition-parent-list
  "DEFINE-CONDITION: empty parents default to (error); explicit parents pass through."
  :cases (("default-parent"   '(define-condition my-err () ())           '(error))
          ("explicit-parent"  '(define-condition my-type-err (type-error) ()) '(type-error)))
  (form expected-parents)
  (let ((parents (third (our-macroexpand-1 form))))
    (assert-equal parents expected-parents)))

;;; ── HANDLER-BIND ─────────────────────────────────────────────────────────────

(deftest-each handler-bind-expansion
  "HANDLER-BIND: empty bindings → PROGN; one binding → LET (dynamic handler registry)."
  :cases (("no-bindings"   '(handler-bind () body)                      'progn)
          ("one-binding"   '(handler-bind ((error #'my-handler)) body)  'let))
  (form expected-head)
  (assert-eq (car (our-macroexpand-1 form)) expected-head))

;;; ── RESTART-CASE ─────────────────────────────────────────────────────────────

(deftest-each restart-case-basic-expansion
  "RESTART-CASE: no clauses → form unchanged; with clause → LET (restart binding)."
  :cases (("no-clauses"    '(restart-case (do-stuff))                       '(do-stuff))
          ("with-clause"   '(restart-case (do-stuff) (my-restart () ok))    nil))
  (form expected-or-nil)
  (let ((result (our-macroexpand-1 form)))
    (if expected-or-nil
        (assert-equal result expected-or-nil)
        (assert-eq (car result) 'let))))

;;; ── ISQRT ────────────────────────────────────────────────────────────────────

(deftest isqrt-expands-to-floor-sqrt
  "ISQRT expands to let* with Newton correction loop"
  (let ((result (our-macroexpand-1 '(isqrt n))))
    ;; FR-683: isqrt now uses integer Newton's method for precision
    (assert-eq (car result) 'let*)))

;;; ── WITH-OPEN-STREAM ─────────────────────────────────────────────────────────

(deftest with-open-stream-expansion
  "WITH-OPEN-STREAM: outer LET binds the stream; body is UNWIND-PROTECT to ensure CLOSE."
  (let* ((result    (our-macroexpand-1 '(with-open-stream (s (open f)) body)))
         (body-form (caddr result)))
    (assert-eq (car result)    'let)
    (assert-eq (car body-form) 'unwind-protect)))

;;; ── MAP ───────────────────────────────────────────────────────────────────────

(deftest map-delegates-to-coerce-mapcar
  "MAP expands to (coerce (mapcar fn (coerce seq 'list)) result-type)"
  (let ((result (our-macroexpand-1 '(map 'vector #'1+ v))))
    (assert-equal (symbol-name (car result)) "COERCE")))

;;; ── PAIRLIS ──────────────────────────────────────────────────────────────────

(deftest pairlis-expansion
  "PAIRLIS: outer LET binds ks, ds, and acc (3 vars); body is TAGBODY loop."
  (let* ((result   (our-macroexpand-1 '(pairlis ks ds)))
         (bindings (second result))
         (body     (caddr result)))
    (assert-eq (car result)      'let)
    (assert-=  (length bindings) 3)
    (assert-eq (car body)        'tagbody)))

;;; ── FIND with keyword args ───────────────────────────────────────────────────

(deftest find-with-key-test-args
  "FIND with :key/:test keyword args expands to a LET with 3 bindings (item, key, test)."
  (let* ((result   (our-macroexpand-1 '(find item lst :key #'car :test #'equal)))
         (bindings (second result))
         (body     (caddr result)))
    (assert-eq (car result)      'let)
    (assert-=  (length bindings) 3)
    (assert-eq (car body)        'block)))

;;; ── Stream macros ────────────────────────────────────────────────────────────

(deftest-each stream-macro-outer-is-let
  "Stream-binding macros expand to a LET or LET*."
  :cases (("with-input-from-string" '(with-input-from-string (s "hello") body))
          ("with-output-to-string"  '(with-output-to-string (s) (write-char #\x s)))
          ("with-package-iterator"  '(with-package-iterator (sym pkg) body)))
  (form)
  (assert-true (member (car (our-macroexpand-1 form)) '(let let*))))

(deftest with-output-to-string-ends-with-get-output-stream-string
  "WITH-OUTPUT-TO-STRING last form in LET body is (get-output-stream-string var)."
  (let* ((result    (our-macroexpand-1 '(with-output-to-string (s) body)))
         (let-body  (cddr result))
         (last-form (car (last let-body))))
    (assert-eq    (car result)  'let)
    (assert-equal (symbol-name (car last-form)) "GET-OUTPUT-STREAM-STRING")))

;;; ── Restart protocol ─────────────────────────────────────────────────────────

(deftest restart-find-restart-expansion
  "FIND-RESTART expands to LET with ASSOC lookup on *%active-restarts*."
  (let ((result (our-macroexpand-1 '(find-restart 'my-restart))))
    (assert-eq (car result) 'let)))

(deftest restart-compute-restarts-expansion
  "COMPUTE-RESTARTS returns the active restart list."
  (let ((result (our-macroexpand-1 '(compute-restarts))))
    (assert-eq result 'cl-cc::*%active-restarts*)))

(deftest restart-invoke-restart-expansion
  "INVOKE-RESTART expands to LET* with ASSOC + THROW."
  (let ((result (our-macroexpand-1 '(invoke-restart 'my-restart))))
    (assert-eq (car result) 'let*)))

(deftest restart-abort-expansion
  "ABORT expands to LET with FIND-RESTART + INVOKE-RESTART fallback."
  (let ((result (our-macroexpand-1 '(abort))))
    (assert-eq (car result) 'let)))

(deftest restart-continue-expansion
  "CONTINUE expands to LET with FIND-RESTART + conditional INVOKE-RESTART."
  (let ((result (our-macroexpand-1 '(continue))))
    (assert-eq (car result) 'let)))

(deftest restart-use-value-expansion
  "USE-VALUE expands to LET with FIND-RESTART + INVOKE-RESTART or passthrough."
  (let ((result (our-macroexpand-1 '(use-value 42))))
    (assert-eq (car result) 'let)))

(deftest restart-store-value-expansion
  "STORE-VALUE expands to LET with FIND-RESTART + INVOKE-RESTART or passthrough."
  (let ((result (our-macroexpand-1 '(store-value 42))))
    (assert-eq (car result) 'let)))

(deftest restart-name-checks-cons
  "RESTART-NAME expands to IF+CONSP to extract name from restart entry."
  (let ((result (our-macroexpand-1 '(restart-name r))))
    (assert-eq (car result) 'if)))

(deftest restart-case-expansion
  "RESTART-CASE wraps form in nested catches with restart bindings."
  (let ((result (our-macroexpand-1 '(restart-case (error "bad")
                                      (retry () 42)))))
    (assert-eq (car result) 'let)))

;;; ── Misc stubs ───────────────────────────────────────────────────────────────

(deftest restart-bind-expansion
  "RESTART-BIND binds restarts via LET and appends to *%active-restarts*."
  (let ((result (our-macroexpand-1 '(restart-bind ((my-r #'fn)) body))))
    (assert-eq 'let (car result))))

(deftest with-standard-io-syntax-expansion
  "WITH-STANDARD-IO-SYNTAX expands into a LET binding standard I/O variables."
  (let ((result (our-macroexpand-1 '(with-standard-io-syntax body1 body2))))
    ;; Expands to (let ((*print-escape* t) ...) body1 body2)
    (assert-eq 'let (car result))
    ;; Body forms are preserved
    (assert-equal '(body1 body2) (cddr result))))

(deftest define-compiler-macro-returns-name
  "DEFINE-COMPILER-MACRO returns the macro name (no compile-time expansion)."
  (let ((result (our-macroexpand-1 '(define-compiler-macro foo (x) (+ x 1)))))
    (assert-equal result '(quote foo))))

;;; ── FIND-IF-NOT ──────────────────────────────────────────────────────────────

(deftest find-if-not-delegates-to-find-if-complement
  "FIND-IF-NOT expands to (find-if (complement pred) list)."
  (let ((result (our-macroexpand-1 '(find-if-not pred lst))))
    (assert-eq (car result) 'find-if)
    (assert-eq (caadr result) 'complement)))

(deftest find-if-not-runtime
  "FIND-IF-NOT returns first element not satisfying predicate."
  (assert-equal (run-string "(find-if-not #'oddp '(1 3 4 5 6))") 4)
  (assert-eq    (run-string "(find-if-not #'numberp '(1 2 3))") nil))

;;; ── POSITION-IF ──────────────────────────────────────────────────────────────

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

;;; ── POSITION-IF-NOT ──────────────────────────────────────────────────────────

(deftest position-if-not-delegates-to-position-if-complement
  "POSITION-IF-NOT expands to (position-if (complement pred) list)."
  (let ((result (our-macroexpand-1 '(position-if-not pred lst))))
    (assert-eq (car result) 'position-if)
    (assert-eq (caadr result) 'complement)))

(deftest position-if-not-runtime
  "POSITION-IF-NOT returns index of first element not satisfying predicate."
  (assert-= (run-string "(position-if-not #'oddp '(1 3 4 5))") 2)
  (assert-eq (run-string "(position-if-not #'oddp '(1 3 5))") nil))

;;; ── COUNT-IF-NOT ─────────────────────────────────────────────────────────────

(deftest count-if-not-delegates-to-count-if-complement
  "COUNT-IF-NOT expands to (count-if (complement pred) list)."
  (let ((result (our-macroexpand-1 '(count-if-not pred lst))))
    (assert-eq (car result) 'count-if)
    (assert-eq (caadr result) 'complement)))

(deftest count-if-not-runtime
  "COUNT-IF-NOT counts elements not satisfying predicate."
  (assert-= (run-string "(count-if-not #'oddp '(1 2 3 4 5))") 2)
  (assert-= (run-string "(count-if-not #'numberp '())") 0))

;;; ── SORT with :KEY ───────────────────────────────────────────────────────────

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
         ;; labels bindings: ((take-n ...) (mmerge ...) (msort ...))
         (fns         (cadr labels-form))
         ;; mmerge definition: (mmerge-name (a b) (cond ...))
         (mmerge-def  (second fns))
         ;; the parameter list is (a b) — 2 params because key-aware merge
         (params      (second mmerge-def)))
    (assert-eq  (car labels-form) 'labels)
    (assert-=   (length fns)      3)
    ;; mmerge takes exactly 2 params
    (assert-=   (length params)   2)))

;;; ── STABLE-SORT with :KEY ────────────────────────────────────────────────────

(deftest stable-sort-with-key-delegates-to-sort-with-key
  "(stable-sort lst pred :key fn) → (sort lst pred :key fn)"
  (let ((result (our-macroexpand-1 '(stable-sort lst pred :key #'car))))
    (assert-eq (car result) 'sort)
    (assert-equal (cddr result) '(pred :key #'car))))

;;; ── CONCATENATE list/vector result types ─────────────────────────────────────

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

;;; ── NOTANY / NOTEVERY runtime ────────────────────────────────────────────────

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

;;; ── NRECONC ──────────────────────────────────────────────────────────────────

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

;;; ── ASSOC-IF / ASSOC-IF-NOT ──────────────────────────────────────────────────

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

;;; ── PUSHNEW with :TEST ───────────────────────────────────────────────────────

(deftest pushnew-default-expansion
  "PUSHNEW with no :test uses default #'eql in MEMBER call."
  (let* ((result    (our-macroexpand-1 '(pushnew item place)))
         (unless-form (caddr result))
         (member-call (second unless-form)))
    (assert-eq (car result)      'let)
    (assert-eq (car unless-form) 'unless)
    (assert-eq (car member-call) 'member)))

(deftest pushnew-with-test-passes-test-to-member
  "PUSHNEW :test keyword is forwarded to the MEMBER call."
  (let* ((result      (our-macroexpand-1 '(pushnew item place :test #'equal)))
         (unless-form (caddr result))
         (member-call (second unless-form))
         ;; member-call = (member item-var place :test #'equal)
         ;;   length = 5: member + item-var + place + :test + #'equal
         (last-arg    (car (last member-call))))
    ;; the test function #'equal is the last arg in the member call
    (assert-= (length member-call) 5)
    (assert-equal last-arg '#'equal)))

(deftest pushnew-runtime-adds-missing
  "PUSHNEW adds item when not present in place."
  (assert-= (run-string "(let ((lst (list 1 2 3))) (pushnew 4 lst) (length lst))") 4))

(deftest pushnew-runtime-no-duplicate
  "PUSHNEW does not add item already present."
  (assert-= (run-string "(let ((lst (list 1 2 3))) (pushnew 2 lst) (length lst))") 3))

;;; ── RASSOC-IF / RASSOC-IF-NOT ────────────────────────────────────────────────

(deftest rassoc-if-outer-is-let
  "RASSOC-IF expands to a LET binding the predicate."
  (assert-eq (car (our-macroexpand-1 '(rassoc-if pred alist))) 'let))

(deftest rassoc-if-body-checks-cdr
  "RASSOC-IF body DOLIST applies predicate to (cdr pair)."
  (let* ((result      (our-macroexpand-1 '(rassoc-if pred alist)))
         (dolist-form (caddr result))
         (when-form   (second (cdr dolist-form)))
         (and-form    (second when-form))
         ;; and-form is (and pair (funcall fn (cdr pair)))
         (funcall-form (third and-form))
         ;; funcall-form is (funcall fn-var (cdr pair))
         (cdr-arg      (caddr funcall-form)))
    (assert-eq (car dolist-form)   'dolist)
    (assert-eq (car funcall-form)  'funcall)
    ;; the argument to funcall is the form (cdr pair), so its car is 'cdr
    (assert-eq (car cdr-arg)       'cdr)))

(deftest rassoc-if-not-delegates-to-rassoc-if-complement
  "RASSOC-IF-NOT expands to (rassoc-if (complement pred) alist)."
  (let ((result (our-macroexpand-1 '(rassoc-if-not pred alist))))
    (assert-eq (car result) 'rassoc-if)
    (assert-eq (caadr result) 'complement)))

;;; ── MEMBER-IF / MEMBER-IF-NOT ────────────────────────────────────────────────

(deftest member-if-outer-is-let
  "MEMBER-IF expands to a LET binding the predicate."
  (assert-eq (car (our-macroexpand-1 '(member-if pred lst))) 'let))

(deftest member-if-body-is-do-loop
  "MEMBER-IF uses a DO loop walking cdr-by-cdr."
  (let* ((result (our-macroexpand-1 '(member-if pred lst)))
         (body   (caddr result)))
    (assert-eq (car body) 'do)))

(deftest member-if-not-delegates-to-member-if-complement
  "MEMBER-IF-NOT expands to (member-if (complement pred) list)."
  (let ((result (our-macroexpand-1 '(member-if-not pred lst))))
    (assert-eq (car result) 'member-if)
    (assert-eq (caadr result) 'complement)))

(deftest member-if-runtime
  "MEMBER-IF returns the tail starting at first satisfying element."
  (assert-equal (run-string "(member-if #'evenp '(1 3 4 5 6))") '(4 5 6))
  (assert-eq    (run-string "(member-if #'evenp '(1 3 5))") nil))

(deftest member-if-not-runtime
  "MEMBER-IF-NOT returns tail starting at first non-matching element."
  (assert-equal (run-string "(member-if-not #'oddp '(1 3 4 5))") '(4 5))
  (assert-eq    (run-string "(member-if-not #'oddp '(1 3 5))") nil))

;;; ── COMPLEMENT ───────────────────────────────────────────────────────────────

(deftest complement-outer-is-let
  "COMPLEMENT expands to a LET binding the predicate, returning a lambda."
  (let* ((result (our-macroexpand-1 '(complement pred)))
         (body   (caddr result)))
    (assert-eq (car result) 'let)
    (assert-eq (car body)   'lambda)))

(deftest complement-lambda-applies-not
  "COMPLEMENT lambda body applies NOT to the result of applying pred."
  (let* ((result  (our-macroexpand-1 '(complement pred)))
         (lambda-body (caddr (caddr result)))
         ;; lambda body is (not (apply fn args))
         (not-form lambda-body))
    (assert-eq (car not-form) 'not)
    (assert-eq (caadr not-form) 'apply)))

;;; ── :key support for -if/-if-not variants (session 19) ──────────────────────

(deftest remove-if-with-key
  "REMOVE-IF with :key applies key before predicate."
  (let ((result (our-macroexpand-1 '(remove-if #'oddp lst :key #'car))))
    (assert-eq (car result) 'let)
    ;; should have kfn binding
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
    (assert-true (> (length (cadr result)) 2)))) ; fn, kfn, idx bindings

(deftest count-if-not-with-key
  "COUNT-IF-NOT with :key delegates to COUNT-IF with complement."
  (let ((result (our-macroexpand-1 '(count-if-not #'oddp lst :key #'car))))
    (assert-eq (car result) 'count-if)))

(deftest member-if-with-key
  "MEMBER-IF with :key applies key before predicate."
  (let ((result (our-macroexpand-1 '(member-if #'oddp lst :key #'car))))
    (assert-eq (car result) 'let)
    (assert-true (> (length (cadr result)) 1))))
