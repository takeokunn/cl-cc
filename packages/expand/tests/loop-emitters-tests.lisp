;;;; tests/unit/expand/loop-emitters-tests.lisp — LOOP emitter layer unit tests

(in-package :cl-cc/test)

(defsuite loop-emitters-suite
  :description "LOOP emitter layer unit tests"
  :parent cl-cc-unit-suite)

(in-suite loop-emitters-suite)

(deftest-each loop-emitters-register-canonical-dispatch-functions
  "Emitter dispatch tables contain the canonical LOOP emitter registrations."
  :cases (("iter-from" :iter :from)
          ("iter-hash-values" :iter :hash-values)
          ("acc-sum" :acc :sum)
          ("acc-collect" :acc :collect)
          ("condition-while" :condition :while)
          ("condition-thereis" :condition :thereis))
  (table-type key)
  (assert-true
   (functionp
    (gethash key
             (ecase table-type
               (:iter cl-cc/expand::*loop-iter-emitters*)
               (:acc cl-cc/expand::*loop-acc-emitters*)
               (:condition cl-cc/expand::*loop-condition-emitters*))))))

(deftest loop-iter-emitter-from-produces-boundary-tests
  "The :FROM emitter produces the expected bindings, exit tests, and step form."
  (multiple-value-bind (bindings end-tests pre-body step-forms)
      (funcall (gethash :from cl-cc/expand::*loop-iter-emitters*)
               'i
               '(:from 1 :to 5 :by 2))
    (assert-equal '((i 1)) bindings)
    (assert-equal '((> i 5)) end-tests)
    (assert-null pre-body)
    (assert-equal '((setq i (+ i 2))) step-forms)))

(deftest loop-acc-emitter-sum-accumulates-numerically
  "The :SUM emitter initializes and updates a numeric accumulator."
  (multiple-value-bind (body bindings result-form)
      (funcall (gethash :sum cl-cc/expand::*loop-acc-emitters*)
               'total
               'item
               nil
               nil
               nil)
    (assert-equal '(setq total (+ total item)) body)
    (assert-equal '((total 0)) bindings)
    (assert-null result-form)))

(deftest loop-acc-emitter-collect-adds-implicit-nreverse-only-without-into
  "The :COLLECT emitter only synthesizes an implicit NREVERSE result when INTO is absent."
  (multiple-value-bind (body bindings result-form)
      (funcall (gethash :collect cl-cc/expand::*loop-acc-emitters*)
               'items
               'item
               nil
               nil
               nil)
    (assert-equal '(setq items (cons item items)) body)
    (assert-equal '((items nil)) bindings)
    (assert-equal '((nreverse items)) result-form))
  (multiple-value-bind (body bindings result-form)
      (funcall (gethash :collect cl-cc/expand::*loop-acc-emitters*)
               'items
               'item
               nil
               nil
               'external-items)
    (assert-equal '(setq items (cons item items)) body)
    (assert-equal '((items nil)) bindings)
    (assert-null result-form)))

(deftest loop-iter-emitter-in-with-destructuring-and-by-function
  "The :IN emitter expands destructuring bindings and advances with the BY function."
  (multiple-value-bind (bindings end-tests pre-body step-forms)
      (funcall (gethash :in cl-cc/expand::*loop-iter-emitters*)
               '(a . rest)
               '(:in xs :by next-cell))
    (assert-= 4 (length bindings))
    (assert-true (member '(a nil) bindings :test #'equal))
    (assert-true (member '(rest nil) bindings :test #'equal))
    (let* ((list-binding (find-if (lambda (binding)
                                    (equal 'xs (second binding)))
                                  bindings))
           (real-binding (find-if (lambda (binding)
                                    (and (consp (second binding))
                                         (eq 'car (first (second binding)))))
                                  bindings))
           (list-var (first list-binding))
           (real-var (first real-binding)))
      (assert-true list-binding)
      (assert-true real-binding)
      (assert-equal `(,real-var (car ,list-var)) real-binding)
      (assert-equal `((null ,list-var)) end-tests)
      (assert-= 2 (length pre-body))
      (assert-true (member `(setq a (car ,real-var)) pre-body :test #'equal))
      (assert-true (member `(setq rest (cdr ,real-var)) pre-body :test #'equal))
      (assert-= 4 (length step-forms))
      (assert-true (member `(setq ,list-var (funcall next-cell ,list-var))
                           step-forms :test #'equal))
      (assert-true (member `(setq ,real-var (car ,list-var))
                           step-forms :test #'equal))
      (assert-true (member `(setq a (car ,real-var))
                           step-forms :test #'equal))
      (assert-true (member `(setq rest (cdr ,real-var))
                           step-forms :test #'equal)))))

(deftest-each loop-condition-emitters-produce-expected-forms
  "Condition emitters expand to the expected tagbody fragments."
  :cases (("until" :until 'stop-now 'done '(when stop-now (go done)))
          ("always" :always 'ok 'done '(unless ok (return nil)))
          ("never" :never 'bad 'done '(when bad (return nil))))
  (type form end-tag expected)
  (assert-equal expected
                (funcall (gethash type cl-cc/expand::*loop-condition-emitters*)
                         form
                         end-tag)))

(deftest loop-condition-emitter-thereis-wraps-result-in-a-single-binding
  "The :THEREIS emitter captures the probe result once and returns it when truthy."
  (let ((form (funcall (gethash :thereis cl-cc/expand::*loop-condition-emitters*)
                       'probe
                       'done)))
    (assert-eq 'let (car form))
    (assert-= 1 (length (second form)))
    (assert-equal '(probe) (mapcar #'second (second form)))
    (let ((temp-var (caar (second form))))
      (assert-true (symbolp temp-var))
      (assert-equal `(when ,temp-var (return ,temp-var))
                    (third form)))))

(deftest loop-condition-emitter-while-emits-goto
  "The :WHILE emitter terminates the loop by jumping to END-TAG."
  (assert-equal '(unless keep-going (go done))
                (funcall (gethash :while cl-cc/expand::*loop-condition-emitters*)
                         'keep-going
                         'done)))
