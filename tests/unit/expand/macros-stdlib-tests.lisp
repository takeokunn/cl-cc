;;;; tests/unit/expand/macros-stdlib-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib.lisp
;;;;
;;;; Macros tested: push, pop, incf/decf, 1+/1-, signum, return,
;;;; with-slots, nth-value, destructuring-bind, assert, ignore-errors,
;;;; concatenate, CXR accessors, and the HOF macros (mapcar, every,
;;;; some, find, position, count, remove-if, remove-if-not, mapcan)
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

(deftest mapcar-outer-is-let
  "MAPCAR wraps fn and accumulator in a LET"
  (let ((result (our-macroexpand-1 '(mapcar fn lst))))
    (assert-eq (car result) 'let)))

(deftest mapcar-body-contains-dolist
  "MAPCAR body iterates with DOLIST"
  (let* ((result (our-macroexpand-1 '(mapcar fn lst)))
         ;; dolist is in the body of the let
         (dolist-form (caddr result)))
    (assert-eq (car dolist-form) 'dolist)))

(deftest every-outer-is-let
  "EVERY wraps pred in a LET"
  (let ((result (our-macroexpand-1 '(every pred lst))))
    (assert-eq (car result) 'let)))

(deftest every-body-has-block-and-dolist
  "EVERY body is a BLOCK containing DOLIST with UNLESS return"
  (let* ((result (our-macroexpand-1 '(every pred lst)))
         (block-form (caddr result)))
    (assert-eq (car block-form) 'block)))

(deftest some-outer-is-let
  "SOME wraps pred in a LET"
  (let ((result (our-macroexpand-1 '(some pred lst))))
    (assert-eq (car result) 'let)))

(deftest notany-delegates-to-some
  "(notany pred lst) → (not (some pred lst))"
  (assert-equal (our-macroexpand-1 '(notany pred lst))
                '(not (some pred lst))))

(deftest notevery-delegates-to-every
  "(notevery pred lst) → (not (every pred lst))"
  (assert-equal (our-macroexpand-1 '(notevery pred lst))
                '(not (every pred lst))))

(deftest remove-if-outer-is-let
  "REMOVE-IF accumulates via DOLIST into a reversed list"
  (let ((result (our-macroexpand-1 '(remove-if pred lst))))
    (assert-eq (car result) 'let)))

(deftest remove-if-not-outer-is-let
  "REMOVE-IF-NOT accumulates via DOLIST into a reversed list"
  (let ((result (our-macroexpand-1 '(remove-if-not pred lst))))
    (assert-eq (car result) 'let)))

(deftest find-no-keys-is-eql-loop
  "FIND without keyword args generates a fast EQL check loop"
  (let* ((result (our-macroexpand-1 '(find item lst)))
         ;; outer let binds item, body is a block with dolist
         (body (caddr result)))
    (assert-eq (car body) 'block)))

(deftest position-outer-is-let
  "POSITION tracks an index counter alongside dolist iteration"
  (let ((result (our-macroexpand-1 '(position item lst))))
    (assert-eq (car result) 'let)))

(deftest count-outer-is-let
  "COUNT accumulates a counter via dolist"
  (let ((result (our-macroexpand-1 '(count item lst))))
    (assert-eq (car result) 'let)))

(deftest mapcan-outer-is-let
  "MAPCAN accumulates nconc'd results in a LET"
  (let ((result (our-macroexpand-1 '(mapcan fn lst))))
    (assert-eq (car result) 'let)))

(deftest stable-sort-delegates-to-sort
  "(stable-sort lst pred) → (sort lst pred)"
  (assert-equal (our-macroexpand-1 '(stable-sort lst pred))
                '(sort lst pred)))
