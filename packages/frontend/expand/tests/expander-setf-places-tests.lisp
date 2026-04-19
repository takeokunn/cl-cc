;;;; tests/unit/expand/expander-setf-places-tests.lisp — Setf-place expander tests

(in-package :cl-cc/test)

(defsuite expander-setf-places-suite :description "Setf-place expander unit tests"
  :parent cl-cc-unit-suite)


(in-suite expander-setf-places-suite)
(deftest expander-setf-slot-value-passthrough
  "compiler-macroexpand-all: (setf (slot-value obj 'slot) v) is a special form and recurses."
  (let ((result (cl-cc/expand::compiler-macroexpand-all
                 '(setf (slot-value obj 'field) new-val))))
    (assert-true (consp result))))

(deftest expand-setf-accessor-unknown-falls-back-to-slot-value
  "expand-setf-accessor for an unknown accessor generates (setf (slot-value ...))."
  (let* ((result (cl-cc/expand::expand-setf-accessor '(some-unknown-accessor obj) 'val))
         (result-str (format nil "~S" result)))
    (assert-true (search "SLOT-VALUE" result-str))))

(deftest expand-setf-accessor-known-maps-to-slot-name
  "expand-setf-accessor for a registered accessor uses the mapped slot name."
  (setf (gethash 'test-reg-accessor cl-cc/expand::*accessor-slot-map*)
        (cons 'test-class 'the-slot))
  (let* ((result (cl-cc/expand::expand-setf-accessor '(test-reg-accessor obj) 'new-val))
         (result-str (format nil "~S" result)))
    (assert-true (search "THE-SLOT" result-str))))

(deftest-each expand-setf-cons-place
  "expand-setf-cons-place: outer is LET; body contains rplaca for car, rplacd for cdr."
  :cases (("car" '(car x) "RPLACA")
          ("cdr" '(cdr x) "RPLACD"))
  (place expected-fn)
  (let* ((result (cl-cc/expand::expand-setf-cons-place place 'val))
         (str    (format nil "~S" result)))
    (assert-eq 'let (first result))
    (assert-true (search expected-fn str))))

(deftest expander-setf-accessor-slot-value-fallback
  "compiler-macroexpand-all: (setf (foo obj) v) with unknown accessor falls back to slot-value."
  (let ((result (cl-cc/expand::compiler-macroexpand-all '(setf (my-unknown-accessor-xyz obj) v))))
    (let ((str (format nil "~S" result)))
      (assert-true (search "SLOT-VALUE" str)))))

(deftest expander-setf-nth-place
  "(setf (nth i x) v) expands via rplaca + nthcdr."
  (let ((str (format nil "~S"
                     (cl-cc/expand::compiler-macroexpand-all '(setf (nth 2 lst) newval)))))
    (assert-true (search "RPLACA" str))
    (assert-true (search "NTHCDR" str))))

(deftest-each expander-setf-cxr-compound-places
  "(setf (cadr/cddr x) v) expands via rplaca/rplacd applied to (cdr x).
Binds *print-circle* so format can print shared gensym vars compactly —
the expansion shares the temp var multiple times, and without circle
detection format can run the test-level 30s timeout."
  :cases (("cadr" '(setf (cadr x) newval) "RPLACA")
          ("cddr" '(setf (cddr x) newval) "RPLACD"))
  (form expected-op)
  (let* ((*print-circle* t)
         (str (format nil "~S" (cl-cc/expand::compiler-macroexpand-all form))))
    (assert-true (search expected-op str))
    (assert-true (search "CDR" str))))

(deftest expander-setf-getf-place
  "(setf (getf plist key) v) expands to LET wrapper using rt-plist-put."
  (let ((result (cl-cc/expand::compiler-macroexpand-all '(setf (getf my-plist :foo) 42))))
    (assert-eq 'let (car result))
    (assert-true (search "RT-PLIST-PUT" (format nil "~S" result)))))

(deftest expander-setf-get-place
  "(setf (get sym indicator) v) expands via symbol-plist update and returns the value."
  (let ((result (cl-cc/expand::compiler-macroexpand-all '(setf (get my-sym :foo) 42))))
    (assert-eq 'let (car result))
    (let ((str (format nil "~S" result)))
      (assert-true (search "%SET-SYMBOL-PLIST" str))
      (assert-true (search "SYMBOL-PLIST" str))
      (assert-true (search "RT-PLIST-PUT" str)))))

(deftest expander-setf-symbol-value-place
  "(setf (symbol-value sym) v) expands via the runtime symbol-value setter."
  (let ((result (cl-cc/expand::compiler-macroexpand-all '(setf (symbol-value my-sym) 42))))
    (assert-eq 'let (car result))
    (let ((str (format nil "~S" result)))
      (assert-true (search "RT-SET-SYMBOL-VALUE" str))
      (assert-true (search "FIND-SYMBOL" str))
      (assert-true (search "MY-SYM" str)))))

(deftest-each expander-setf-simple-runtime-bridges
  "Simple runtime-backed setf places lower to their dedicated bridge forms."
  :cases (("find-class"      '(setf (find-class sample-class) klass) "%SET-FIND-CLASS")
          ("macro-function"  '(setf (macro-function sample-macro) new-fn) "%SET-MACRO-FUNCTION")
          ("symbol-function" '(setf (symbol-function sample-fn) new-fn) "SET-FDEFINITION")
          ("fdefinition"     '(setf (fdefinition sample-fn) new-fn) "SET-FDEFINITION")
          ("svref"           '(setf (svref vec 1) 42) "%SVSET")
          ("row-major-aref"  '(setf (row-major-aref arr 2) 42) "ASET"))
  (form expected-op)
  (let ((str (format nil "~S" (cl-cc/expand::compiler-macroexpand-all form))))
    (assert-true (search expected-op str))))

(deftest-each expander-setf-shared-string-and-bit-places
  "Shared string and bit place handlers lower through their runtime helpers."
  :cases (("char"  '(setf (char s 0) #\A)  "RT-STRING-SET")
          ("schar" '(setf (schar s 1) #\B) "RT-STRING-SET")
          ("bit"   '(setf (bit bv 2) 1)     "RT-BIT-SET")
          ("sbit"  '(setf (sbit bv 3) 0)    "RT-BIT-SET"))
  (form expected-op)
  (let ((str (format nil "~S" (cl-cc/expand::compiler-macroexpand-all form))))
    (assert-true (search expected-op str))))

(deftest expander-setf-subseq-place
  "(setf (subseq seq start end) new) lowers through replace with start/end keywords."
  (let ((str (format nil "~S"
                     (cl-cc/expand::compiler-macroexpand-all
                      '(setf (subseq seq 1 4) replacement)))))
    (assert-true (search "ASET" str))
    (assert-true (search "RPLACA" str))
    (assert-true (search "NTHCDR" str))))

(deftest expander-setf-subseq-place-without-end
  "(setf (subseq seq start) new) still lowers through the sequence replacement path."
  (let ((str (format nil "~S"
                     (cl-cc/expand::compiler-macroexpand-all
                      '(setf (subseq seq 1) replacement)))))
    (assert-true (search "ASET" str))
    (assert-true (search "RPLACA" str))))

(deftest expander-setf-values-place
  "(setf (values a b) expr) lowers to multiple-value-bind followed by setq chain."
  (let ((result (cl-cc/expand::compiler-macroexpand-all '(setf (values a b) (floor 5 2)))))
    (assert-eq 'multiple-value-bind (car result))
    (assert-eq 'setq (car (fourth result)))
    (assert-eq 'a (second (fourth result)))
    (assert-eq 'setq (car (fifth result)))
    (assert-eq 'b (second (fifth result)))
    (assert-equal '(values a b) (sixth result))))
