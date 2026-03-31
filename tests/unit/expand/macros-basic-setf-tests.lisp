;;;; tests/unit/expand/macros-basic-setf-tests.lisp
;;;; Coverage tests for src/expand/macros-basic.lisp

(in-package :cl-cc/test)

(defsuite macros-basic-setf-suite
  :description "Tests for macros-basic.lisp: setf places"
  :parent cl-cc-suite)

(in-suite macros-basic-setf-suite)

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
