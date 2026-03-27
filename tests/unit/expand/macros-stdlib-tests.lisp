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
  "POP wraps (car place) in a LET, then (setf place (cdr place)) in the body."
  (let* ((result     (our-macroexpand-1 '(pop lst)))
         (setf-form  (caddr result)))
    (assert-eq  (car result)          'let)
    (assert-eq  (car setf-form)       'setf)
    (assert-equal (caddr setf-form)   '(cdr lst))))

(deftest-each incf-decf-expansion
  "incf/decf expand to (setf x (OP x delta))."
  :cases (("incf-default" '(incf x)   '(setf x (+ x 1)))
          ("incf-custom"  '(incf x 5) '(setf x (+ x 5)))
          ("decf-default" '(decf x)   '(setf x (- x 1)))
          ("decf-custom"  '(decf x 3) '(setf x (- x 3))))
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
  "WITH-SLOTS: outer LET binds the instance; inner LET binds each slot via SLOT-VALUE."
  (let* ((result     (our-macroexpand-1 '(with-slots (x) obj body)))
         (inner-let  (caddr result))
         (binding    (caadr inner-let)))
    (assert-eq (car result)    'let)
    (assert-eq (car inner-let) 'let)
    (assert-eq (car binding)   'x)
    (assert-eq (caadr binding) 'slot-value)))

;;; ── NTH-VALUE ────────────────────────────────────────────────────────────────

(deftest nth-value-expansion
  "NTH-VALUE binds (multiple-value-list form) in a LET then calls (nth n gensym)."
  (let* ((result (our-macroexpand-1 '(nth-value 1 form)))
         (body   (caddr result)))
    (assert-eq    (car result)  'let)
    (assert-eq    (car body)    'nth)
    (assert-equal (cadr body)   1)))

;;; ── DESTRUCTURING-BIND ───────────────────────────────────────────────────────

(deftest destructuring-bind-expansion
  "DESTRUCTURING-BIND outer form is LET; inner form is LET* with destructured bindings."
  (let* ((result (our-macroexpand-1 '(destructuring-bind (a b) expr body)))
         (inner  (caddr result)))
    (assert-eq (car result) 'let)
    (assert-eq (car inner)  'let*)))

;;; ── ASSERT ───────────────────────────────────────────────────────────────────

(deftest assert-expansion
  "ASSERT: outer is UNLESS, body is ERROR; custom datum is passed through."
  (let* ((basic  (our-macroexpand-1 '(assert test)))
         (custom (our-macroexpand-1 '(assert test () "bad input"))))
    (assert-eq    (car  basic)          'unless)
    (assert-equal (cadr basic)          'test)
    (assert-eq    (car (caddr basic))   'error)
    (assert-eq    (car (caddr custom))  'error)
    (assert-equal (cadr (caddr custom)) "bad input")))

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
  "WITH-ACCESSORS outer form is LET binding the instance"
  (let ((result (our-macroexpand-1 '(with-accessors ((x acc-x)) obj body))))
    (assert-eq (car result) 'let)))

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
  "HANDLER-BIND: empty bindings → PROGN; one binding → HANDLER-CASE."
  :cases (("no-bindings"   '(handler-bind () body)                      'progn)
          ("one-binding"   '(handler-bind ((error #'my-handler)) body)  'handler-case))
  (form expected-head)
  (assert-eq (car (our-macroexpand-1 form)) expected-head))

;;; ── RESTART-CASE ─────────────────────────────────────────────────────────────

(deftest-each restart-case-expansion
  "RESTART-CASE: no clauses → form unchanged; with clause → HANDLER-CASE."
  :cases (("no-clauses"    '(restart-case (do-stuff))                       '(do-stuff))
          ("with-clause"   '(restart-case (do-stuff) (my-restart () ok))    nil))
  (form expected-or-nil)
  (let ((result (our-macroexpand-1 form)))
    (if expected-or-nil
        (assert-equal result expected-or-nil)
        (assert-eq (car result) 'handler-case))))

;;; ── ISQRT ────────────────────────────────────────────────────────────────────

(deftest isqrt-expands-to-floor-sqrt
  "ISQRT expands to (floor (sqrt (float n)))"
  (let ((result (our-macroexpand-1 '(isqrt n))))
    (assert-eq (car result)      'floor)
    (assert-eq (caar (cdr result)) 'sqrt)))

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

;;; ── Stream macros ────────────────────────────────────────────────────────────

(deftest-each stream-macro-outer-is-let
  "Stream-binding macros expand to a LET."
  :cases (("with-input-from-string" '(with-input-from-string (s "hello") body))
          ("with-output-to-string"  '(with-output-to-string (s) (write-char #\x s)))
          ("with-package-iterator"  '(with-package-iterator (sym pkg) body)))
  (form)
  (assert-eq (car (our-macroexpand-1 form)) 'let))

;;; ── Restart stubs ────────────────────────────────────────────────────────────

(deftest-each restart-stub-returns-nil
  "Unimplemented restart stubs expand to NIL."
  :cases (("find-restart"     '(find-restart 'my-restart))
          ("compute-restarts" '(compute-restarts))
          ("continue"         '(continue))
          ("muffle-warning"   '(muffle-warning)))
  (form)
  (assert-eq (our-macroexpand-1 form) nil))

(deftest-each restart-stub-value-passthrough
  "USE-VALUE and STORE-VALUE pass through the value form."
  :cases (("use-value"   '(use-value 42))
          ("store-value" '(store-value 42)))
  (form)
  (assert-= (our-macroexpand-1 form) 42))

(deftest-each restart-stubs-expand-to-error
  "Unimplemented restart operations expand to (ERROR ...) stubs."
  :cases (("invoke-restart" '(invoke-restart 'my-restart))
          ("abort"          '(abort)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq 'error (car result))))

(deftest restart-name-checks-hash-table
  "RESTART-NAME expands to IF+HASH-TABLE-P to extract :name key or return restart"
  (let ((result (our-macroexpand-1 '(restart-name r))))
    (assert-eq (car result) 'if)))

;;; ── Misc stubs ───────────────────────────────────────────────────────────────

(deftest-each stub-macros-expand-to-progn
  "Stub macros that expand body forms into a PROGN."
  :cases (("restart-bind"           '(restart-bind ((my-r #'fn)) body))
          ("with-standard-io-syntax" '(with-standard-io-syntax body1 body2)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq 'progn (car result))))

(deftest define-compiler-macro-expands-to-nil
  "DEFINE-COMPILER-MACRO is a stub that expands to NIL"
  (let ((result (our-macroexpand-1 '(define-compiler-macro foo (x) (+ x 1)))))
    (assert-eq result nil)))
