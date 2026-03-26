;;;; tests/unit/expand/macros-basic-tests.lisp
;;;; Coverage tests for src/expand/macros-basic.lisp
;;;;
;;;; Macros tested: check-type, list, setf (all place forms)
;;;;
(in-package :cl-cc/test)

(defsuite macros-basic-suite
  :description "Tests for macros-basic.lisp: check-type, list, setf places"
  :parent cl-cc-suite)

;;; ── CHECK-TYPE ───────────────────────────────────────────────────────────────

(deftest check-type-outer-is-unless
  "CHECK-TYPE wraps typep test in UNLESS"
  (let ((result (our-macroexpand-1 '(check-type x integer))))
    (assert-eq (car result) 'unless)
    (assert-equal (cadr result) '(typep x 'integer))))

(deftest check-type-body-is-error
  "CHECK-TYPE body is an (error ...) call"
  (let ((result (our-macroexpand-1 '(check-type x integer))))
    (assert-eq (car (caddr result)) 'error)))

(deftest check-type-default-message-is-string
  "CHECK-TYPE without type-string generates a string message"
  (let* ((result (our-macroexpand-1 '(check-type x integer)))
         (msg (cadr (caddr result))))
    (assert-true (stringp msg))))

(deftest check-type-with-type-string
  "CHECK-TYPE with type-string still produces (error string)"
  (let* ((result (our-macroexpand-1 '(check-type x integer "a number")))
         (msg (cadr (caddr result))))
    (assert-true (stringp msg))))

;;; ── LIST macro ───────────────────────────────────────────────────────────────

(deftest list-empty-is-nil
  "(list) expands to nil — no cons allocation at all"
  (assert-equal (our-macroexpand-1 '(list)) nil))

(deftest list-one-element-is-cons-nil
  "(list x) → (cons x nil)"
  (assert-equal (our-macroexpand-1 '(list x)) '(cons x nil)))

(deftest list-two-elements-nested-cons
  "(list a b) → (cons a (cons b nil))"
  (assert-equal (our-macroexpand-1 '(list a b)) '(cons a (cons b nil))))

(deftest list-three-elements-nested-cons
  "(list a b c) → (cons a (cons b (cons c nil)))"
  (assert-equal (our-macroexpand-1 '(list a b c))
                '(cons a (cons b (cons c nil)))))

(deftest list-literal-values
  "(list 1 2 3) preserves literal values in cons chain"
  (assert-equal (our-macroexpand-1 '(list 1 2 3))
                '(cons 1 (cons 2 (cons 3 nil)))))

;;; ── SETF places ─────────────────────────────────────────────────────────────

(deftest setf-plain-symbol-to-setq
  "(setf x v) → (setq x v)"
  (assert-equal (our-macroexpand-1 '(setf x v)) '(setq x v)))

(deftest setf-gethash-to-setf-gethash
  "(setf (gethash k ht) v) → (setf-gethash k ht v) — no gensym"
  ;; setf-gethash lives in CL-CC:: so check symbol-name; args come from the
  ;; test form so they are CL-CC/TEST:: and match '(k ht v) directly.
  (let ((result (our-macroexpand-1 '(setf (gethash k ht) v))))
    (assert-equal (symbol-name (car result)) "SETF-GETHASH")
    (assert-equal (cdr result) '(k ht v))))

(deftest setf-slot-value-to-setf-slot-value
  "(setf (slot-value obj s) v) → (setf-slot-value obj s v) — no gensym"
  (let ((result (our-macroexpand-1 '(setf (slot-value obj 'slot) v))))
    (assert-equal (symbol-name (car result)) "SETF-SLOT-VALUE")
    (assert-equal (cdr result) '(obj 'slot v))))

(deftest setf-car-outer-is-let
  "(setf (car x) v) wraps rplaca in a LET binding for the new value"
  (let ((result (our-macroexpand-1 '(setf (car x) v))))
    (assert-eq (car result) 'let)))

(deftest setf-car-body-is-rplaca
  "(setf (car x) v) body calls RPLACA on the cons"
  (let* ((result (our-macroexpand-1 '(setf (car x) v)))
         (body   (cddr result)))
    (assert-eq (caar body) 'rplaca)
    (assert-equal (cadar body) 'x)))

(deftest setf-cdr-body-is-rplacd
  "(setf (cdr x) v) body calls RPLACD"
  (let* ((result (our-macroexpand-1 '(setf (cdr x) v)))
         (body   (cddr result)))
    (assert-eq (caar body) 'rplacd)
    (assert-equal (cadar body) 'x)))

(deftest setf-first-same-as-car
  "(setf (first x) v) is handled as CAR place"
  (let* ((result (our-macroexpand-1 '(setf (first x) v)))
         (body   (cddr result)))
    (assert-eq (caar body) 'rplaca)))

(deftest setf-rest-same-as-cdr
  "(setf (rest x) v) is handled as CDR place"
  (let* ((result (our-macroexpand-1 '(setf (rest x) v)))
         (body   (cddr result)))
    (assert-eq (caar body) 'rplacd)))

(deftest setf-nth-outer-is-let
  "(setf (nth n lst) v) wraps rplaca(nthcdr) in a LET"
  (let ((result (our-macroexpand-1 '(setf (nth 2 lst) v))))
    (assert-eq (car result) 'let)))

(deftest setf-nth-body-uses-rplaca-nthcdr
  "(setf (nth n lst) v) body has (rplaca (nthcdr n lst) v)"
  (let* ((result (our-macroexpand-1 '(setf (nth 2 lst) v)))
         (body   (cddr result)))
    (assert-eq (caar body) 'rplaca)
    (assert-equal (caadar body) 'nthcdr)))

(deftest setf-getf-outer-is-let
  "(setf (getf plist :k) v) wraps in a LET for the new value"
  (let ((result (our-macroexpand-1 '(setf (getf plist :k) v))))
    (assert-eq (car result) 'let)))

(deftest setf-getf-body-uses-rt-plist-put
  "(setf (getf plist :k) v) body uses rt-plist-put (CL-CC-specific symbol)"
  (let* ((result    (our-macroexpand-1 '(setf (getf plist :k) v)))
         (setq-form (caddr result)))
    (assert-eq (car setq-form) 'setq)
    (assert-equal (symbol-name (car (caddr setq-form))) "RT-PLIST-PUT")))
