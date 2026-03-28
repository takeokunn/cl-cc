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

(deftest check-type-expansion
  "CHECK-TYPE: UNLESS with typep test, body signals type-error via make-condition."
  (let* ((result (our-macroexpand-1 '(check-type x integer)))
         (error-form (caddr result))
         (make-cond (cadr error-form)))
    (assert-eq 'unless (car result))
    (assert-equal '(typep x 'integer) (cadr result))
    (assert-eq 'error (car error-form))
    ;; Now signals (error (make-condition 'type-error :datum x :expected-type 'integer))
    (assert-eq 'make-condition (car make-cond))
    (assert-equal '(quote type-error) (second make-cond))))

;;; ── LIST macro ───────────────────────────────────────────────────────────────

(deftest list-empty-is-nil
  "(list) expands to nil — no cons allocation at all"
  (assert-equal (our-macroexpand-1 '(list)) nil))

(deftest-each list-expands-to-nested-cons
  "LIST expands to right-associative nested cons cells terminating in nil."
  :cases (("one-element"    '(list x)     '(cons x nil))
          ("two-elements"   '(list a b)   '(cons a (cons b nil)))
          ("three-elements" '(list a b c) '(cons a (cons b (cons c nil))))
          ("literals"       '(list 1 2 3) '(cons 1 (cons 2 (cons 3 nil)))))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

;;; ── SETF places ─────────────────────────────────────────────────────────────

(deftest setf-plain-symbol-to-setq
  "(setf x v) → (setq x v)"
  (assert-equal (our-macroexpand-1 '(setf x v)) '(setq x v)))

(deftest-each setf-passthrough-expansions
  "(setf (accessor ...)) expands without gensym: (accessor* args val)"
  :cases (("gethash"    '(setf (gethash k ht) v)      "SETF-GETHASH"    '(k ht v))
          ("slot-value" '(setf (slot-value obj 'slot) v) "SETF-SLOT-VALUE" '(obj 'slot v)))
  (form expected-name expected-args)
  (let ((result (our-macroexpand-1 form)))
    (assert-equal (symbol-name (car result)) expected-name)
    (assert-equal (cdr result) expected-args)))

(deftest-each place-macro-outer-is-let
  "Compound-place macros (setf car/nth/getf, case, typecase) wrap in LET."
  :cases (("setf-car"   '(setf (car x) v))
          ("setf-nth"   '(setf (nth 2 lst) v))
          ("setf-getf"  '(setf (getf plist :k) v))
          ("case"       '(case x (1 :one) (2 :two)))
          ("typecase"   '(typecase x (integer :int) (string :str))))
  (form)
  (assert-eq 'let (car (our-macroexpand-1 form))))

(deftest-each setf-cons-cell-synonyms
  "car/first both expand to rplaca; cdr/rest both expand to rplacd."
  :cases (("car"   '(setf (car x)   v) 'rplaca)
          ("first" '(setf (first x) v) 'rplaca)
          ("cdr"   '(setf (cdr x)   v) 'rplacd)
          ("rest"  '(setf (rest x)  v) 'rplacd))
  (form expected-fn)
  (let* ((result (our-macroexpand-1 form))
         (body   (cddr result)))
    (assert-eq (caar body) expected-fn)))

(deftest setf-nth-body-uses-rplaca-nthcdr
  "(setf (nth n lst) v) body has (rplaca (nthcdr n lst) v)"
  (let* ((result (our-macroexpand-1 '(setf (nth 2 lst) v)))
         (body   (cddr result)))
    (assert-eq (caar body) 'rplaca)
    (assert-equal (caadar body) 'nthcdr)))

(deftest setf-getf-body-uses-rt-plist-put
  "(setf (getf plist :k) v) body uses rt-plist-put (CL-CC-specific symbol)"
  (let* ((result    (our-macroexpand-1 '(setf (getf plist :k) v)))
         (setq-form (caddr result)))
    (assert-eq (car setq-form) 'setq)
    (assert-equal (symbol-name (car (caddr setq-form))) "RT-PLIST-PUT")))

;;; ── DOLIST ───────────────────────────────────────────────────────────────────

(deftest-each loop-macro-outer-is-block-nil
  "Loop macros (dolist/dotimes/do) expand to (block nil ...) to support RETURN."
  :cases (("dolist"  '(dolist (x lst) body))
          ("dotimes" '(dotimes (i 10) body))
          ("do"      '(do ((i 0 (1+ i))) ((= i 10) i) body)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq 'block (car result))
    (assert-eq nil (second result))))

(deftest dolist-expansion
  "DOLIST: inner LET with TAGBODY body; optional result-form propagated."
  (let* ((result   (our-macroexpand-1 '(dolist (x lst) body)))
         (let-form (third result)))
    (assert-eq 'let (car let-form))
    (assert-eq 'tagbody (car (caddr let-form))))
  (let* ((result   (our-macroexpand-1 '(dolist (x lst :done) body)))
         (let-form (third result)))
    (assert-eq :done (car (last let-form)))))

;;; ── DOTIMES ──────────────────────────────────────────────────────────────────

(deftest dotimes-inner-binds-counter-and-limit
  "DOTIMES inner LET binds the loop variable to 0 and a count-var to the limit"
  (let* ((result (our-macroexpand-1 '(dotimes (i 10) body)))
         (let-form (third result))
         (bindings (second let-form)))
    ;; bindings: ((i 0) (count-var 10))
    (assert-= (length bindings) 2)
    (assert-= (second (first bindings)) 0)))

;;; ── DO / DO* ─────────────────────────────────────────────────────────────────

(deftest-each do-do*-binding-forms
  "DO uses LET (parallel); DO* uses LET* (sequential) for initial bindings."
  :cases (("do"  '(do  ((i 0 (1+ i))) ((= i 10)) body) 'let)
          ("do*" '(do* ((i 0 (1+ i))) ((= i 10)) body) 'let*))
  (form expected-head)
  (let* ((result   (our-macroexpand-1 form))
         (let-form (third result)))
    (assert-eq expected-head (car let-form))))

(deftest do-uses-psetq-for-parallel-steps
  "DO updates vars with PSETQ (simultaneous) while DO* uses sequential SETQs"
  (let* ((result (our-macroexpand-1 '(do ((i 0 (1+ i)) (j 1 (1+ j))) ((= i 5)))))
         (let-form (third result))
         (tagbody-form (caddr let-form))
         ;; PSETQ appears inside tagbody before (go start)
         (tagbody-body (cddr tagbody-form)))
    ;; Find PSETQ form among tagbody body items
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'psetq)))
                       tagbody-body))))

;;; ── CASE ─────────────────────────────────────────────────────────────────────

(deftest case-single-key-uses-eql
  "CASE single key checks with EQL"
  (let* ((result (our-macroexpand-1 '(case x (1 :one))))
         (body   (caddr result)))   ; (let ((kv x)) body)
    ;; body is (if (eql kv '1) (progn :one) nil)
    (assert-eq (car body) 'if)
    (assert-equal (symbol-name (caar (cdr body))) "EQL")))

(deftest-each case-typecase-otherwise-is-progn
  "CASE and TYPECASE OTHERWISE clauses expand to plain PROGN (no condition test)."
  :cases (("case"     '(case     x (otherwise :default)))
          ("typecase" '(typecase x (otherwise :default))))
  (form)
  (assert-eq 'progn (car (caddr (our-macroexpand-1 form)))))

;;; ── TYPECASE ─────────────────────────────────────────────────────────────────

(deftest typecase-uses-typep
  "TYPECASE clause check uses TYPEP for type dispatch"
  (let* ((result (our-macroexpand-1 '(typecase x (integer :int))))
         (body   (caddr result)))
    ;; body is (if (typep kv 'integer) (progn :int) nil)
    (assert-eq (car body) 'if)
    (assert-equal (symbol-name (caar (cdr body))) "TYPEP")))

