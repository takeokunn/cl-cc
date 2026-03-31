;;;; tests/unit/expand/expander-setf-places-tests.lisp — Setf-place expander tests

(in-package :cl-cc/test)

(defsuite expander-setf-places-suite :description "Setf-place expander unit tests")

(deftest expander-setf-slot-value-passthrough
  "compiler-macroexpand-all: (setf (slot-value obj 'slot) v) is a special form and recurses."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(setf (slot-value obj 'field) new-val))))
    (assert-true (consp result))))

(deftest expand-setf-accessor-unknown-falls-back-to-slot-value
  "expand-setf-accessor for an unknown accessor generates (setf (slot-value ...))."
  (let* ((result (cl-cc::expand-setf-accessor '(some-unknown-accessor obj) 'val))
         (result-str (format nil "~S" result)))
    (assert-true (search "SLOT-VALUE" result-str))))

(deftest expand-setf-accessor-known-maps-to-slot-name
  "expand-setf-accessor for a registered accessor uses the mapped slot name."
  (setf (gethash 'test-reg-accessor cl-cc::*accessor-slot-map*)
        (cons 'test-class 'the-slot))
  (let* ((result (cl-cc::expand-setf-accessor '(test-reg-accessor obj) 'new-val))
         (result-str (format nil "~S" result)))
    (assert-true (search "THE-SLOT" result-str))))

(deftest-each expand-setf-cons-place
  "expand-setf-cons-place: outer is LET; body contains rplaca for car, rplacd for cdr."
  :cases (("car" '(car x) "RPLACA")
          ("cdr" '(cdr x) "RPLACD"))
  (place expected-fn)
  (let* ((result (cl-cc::expand-setf-cons-place place 'val))
         (str    (format nil "~S" result)))
    (assert-eq 'let (first result))
    (assert-true (search expected-fn str))))

(deftest expander-setf-accessor-slot-value-fallback
  "compiler-macroexpand-all: (setf (foo obj) v) with unknown accessor falls back to slot-value."
  (let ((result (cl-cc::compiler-macroexpand-all '(setf (my-unknown-accessor-xyz obj) v))))
    (let ((str (format nil "~S" result)))
      (assert-true (search "SLOT-VALUE" str)))))

(deftest expander-setf-nth-place
  "(setf (nth i x) v) expands via rplaca + nthcdr."
  (let ((str (format nil "~S"
                     (cl-cc::compiler-macroexpand-all '(setf (nth 2 lst) newval)))))
    (assert-true (search "RPLACA" str))
    (assert-true (search "NTHCDR" str))))

(deftest-each expander-setf-cxr-compound-places
  "(setf (cadr/cddr x) v) expands via rplaca/rplacd applied to (cdr x)."
  :cases (("cadr" '(setf (cadr x) newval) "RPLACA")
          ("cddr" '(setf (cddr x) newval) "RPLACD"))
  (form expected-op)
  (let ((str (format nil "~S" (cl-cc::compiler-macroexpand-all form))))
    (assert-true (search expected-op str))
    (assert-true (search "CDR" str))))

(deftest expander-setf-getf-place
  "(setf (getf plist key) v) expands to LET wrapper using rt-plist-put."
  (let ((result (cl-cc::compiler-macroexpand-all '(setf (getf my-plist :foo) 42))))
    (assert-eq 'let (car result))
    (assert-true (search "RT-PLIST-PUT" (format nil "~S" result)))))
