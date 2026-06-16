;;;; tests/unit/expand/expander-setf-places-tests.lisp — Setf-place expander tests

(in-package :cl-cc/test)

(defsuite expander-setf-places-suite :description "Setf-place expander unit tests"
  :parent cl-cc-unit-suite)


(in-suite expander-setf-places-suite)
(deftest expander-setf-slot-value-passthrough
  "compiler-macroexpand-all: (setf (slot-value obj 'slot) v) stays parseable and never emits SETF-SLOT-VALUE."
  (let ((result (cl-cc/expand:compiler-macroexpand-all
                  '(setf (slot-value obj 'field) new-val))))
    (assert-eq 'setf (first result))
    (assert-form-string-not-contains result "SETF-SLOT-VALUE")))

(deftest expand-setf-accessor-unknown-falls-back-to-slot-value
  "expand-setf-accessor for an unknown accessor lowers through RT-SLOT-SET."
  (let ((result (cl-cc/expand::expand-setf-accessor '(some-unknown-accessor obj) 'val)))
    (assert-form-string-contains result "RT-SLOT-SET")))

(deftest expand-setf-accessor-known-maps-to-slot-name
  "expand-setf-accessor for a registered accessor uses the mapped slot name."
  (setf (gethash 'test-reg-accessor cl-cc/expand:*accessor-slot-map*)
        (cons 'test-class 'the-slot))
  (let ((result (cl-cc/expand::expand-setf-accessor '(test-reg-accessor obj) 'new-val)))
    (assert-form-string-contains result "THE-SLOT")))

(deftest-each expand-setf-cons-place
  "expand-setf-cons-place: outer is LET; body contains rplaca for car, rplacd for cdr."
  :cases (("car" '(car x) "RPLACA")
          ("cdr" '(cdr x) "RPLACD"))
  (place expected-fn)
  (let ((result (cl-cc/expand::expand-setf-cons-place place 'val)))
    (assert-eq 'let (first result))
    (assert-form-string-contains result expected-fn)))

(deftest expander-setf-accessor-slot-value-fallback
  "compiler-macroexpand-all: (setf (foo obj) v) with unknown accessor lowers through RT-SLOT-SET."
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(setf (my-unknown-accessor-xyz obj) v))))
    (assert-form-string-contains result "RT-SLOT-SET")))

(deftest expander-setf-nth-place
  "(setf (nth i x) v) expands via rplaca + nthcdr."
  (assert-expanded-string-contains '(setf (nth 2 lst) newval) "RPLACA")
  (assert-expanded-string-contains '(setf (nth 2 lst) newval) "NTHCDR"))

(deftest-each expander-setf-list-accessor-places
  "(setf (second/tenth x) v) expands as a list place, not a slot write."
  :cases (("second" '(setf (second x) newval) "RPLACA" "CDR")
          ("tenth"  '(setf (tenth x) newval)  "RPLACA" "CDR"))
  (form expected-op expected-traversal)
  (assert-expanded-string-contains form expected-op)
  (assert-expanded-string-contains form expected-traversal)
  (assert-expanded-string-not-contains form "RT-SLOT-SET"))

(deftest-each expander-setf-cxr-compound-places
  "(setf (cadr/cddr x) v) expands via rplaca/rplacd applied to (cdr x).
Binds *print-circle* so format can print shared gensym vars compactly —
the expansion shares the temp var multiple times, and without circle
detection format can run the test-level 30s timeout."
  :cases (("cadr" '(setf (cadr x) newval) "RPLACA")
          ("cddr" '(setf (cddr x) newval) "RPLACD"))
  (form expected-op)
  (let ((*print-circle* t))
    (assert-expanded-string-contains form expected-op)
    (assert-expanded-string-contains form "CDR")))

(deftest expander-setf-getf-place
  "(setf (getf plist key) v) expands to LET wrapper using rt-plist-put."
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(setf (getf my-plist :foo) 42))))
    (assert-eq 'let (car result))
    (assert-form-string-contains result "RT-PLIST-PUT")))

(deftest expander-setf-getf-compound-cxr-place
  "(setf (getf (cdddr entry) key) v) writes the rebuilt plist back through the CXR place."
  (let ((result (cl-cc/expand:compiler-macroexpand-all
                  '(setf (getf (cdddr method-entry) :phase) :primary))))
    (assert-eq 'let (car result))
    (assert-form-string-contains result "RT-PLIST-PUT")
    (assert-form-string-contains result "RPLACD")
    (assert-form-string-not-contains result "SETQ (CDDDR")))

(deftest expander-setf-fill-pointer-place
  "(setf (fill-pointer vector) value) expands through the VM fill-pointer setter."
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(setf (fill-pointer vec) 3))))
    (assert-string= "%SET-FILL-POINTER" (symbol-name (car result)))
    (assert-eq 'vec (second result))
    (assert-= 3 (third result))))

(deftest expander-setf-get-place
  "(setf (get sym indicator) v) expands via symbol-plist update and returns the value."
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(setf (get my-sym :foo) 42))))
    (assert-eq 'let (car result))
    (assert-form-string-contains result "%SET-SYMBOL-PLIST")
    (assert-form-string-contains result "SYMBOL-PLIST")
    (assert-form-string-contains result "RT-PLIST-PUT")))

(deftest expander-setf-symbol-value-place
  "(setf (symbol-value sym) v) expands via the runtime symbol-value setter."
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(setf (symbol-value my-sym) 42))))
    (assert-eq 'let (car result))
    (assert-form-string-contains result "*RUNTIME-SET-SYMBOL-VALUE-FN*")
    (assert-form-string-contains result "MY-SYM")))

(deftest-each expander-setf-simple-runtime-bridges
  "Simple runtime-backed setf places lower to their dedicated bridge forms."
  :cases (("find-class"      '(setf (find-class sample-class) klass) "%SET-FIND-CLASS")
          ("macro-function"  '(setf (macro-function sample-macro) new-fn) "%SET-MACRO-FUNCTION")
          ("symbol-function" '(setf (symbol-function sample-fn) new-fn) "SET-FDEFINITION")
          ("fdefinition"     '(setf (fdefinition sample-fn) new-fn) "SET-FDEFINITION")
          ("svref"           '(setf (svref vec 1) 42) "%SVSET")
          ("row-major-aref"  '(setf (row-major-aref arr 2) 42) "ASET"))
  (form expected-op)
  (assert-expanded-string-contains form expected-op))

(deftest-each expander-setf-shared-string-and-bit-places
  "Shared string and bit place handlers lower through their runtime helpers."
  :cases (("char"  '(setf (char s 0) #\A)  "RT-STRING-SET")
          ("schar" '(setf (schar s 1) #\B) "RT-STRING-SET")
          ("bit"   '(setf (bit bv 2) 1)     "RT-BIT-SET")
          ("sbit"  '(setf (sbit bv 3) 0)    "RT-BIT-SET"))
  (form expected-op)
  (assert-expanded-string-contains form expected-op))

(deftest expander-setf-subseq-place
  "(setf (subseq seq start end) new) lowers through replace with start/end keywords."
  (assert-expanded-string-contains '(setf (subseq seq 1 4) replacement) "ASET")
  (assert-expanded-string-contains '(setf (subseq seq 1 4) replacement) "RPLACA")
  (assert-expanded-string-contains '(setf (subseq seq 1 4) replacement) "NTHCDR"))

(deftest expander-setf-subseq-place-without-end
  "(setf (subseq seq start) new) still lowers through the sequence replacement path."
  (assert-expanded-string-contains '(setf (subseq seq 1) replacement) "ASET")
  (assert-expanded-string-contains '(setf (subseq seq 1) replacement) "RPLACA"))

(deftest expander-setf-values-place
  "(setf (values a b) expr) lowers to LET + MULTIPLE-VALUE-LIST + SETQ chain."
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(setf (values a b) (floor 5 2)))))
    (assert-eq 'let (car result))
    (assert-form-string-contains result "%VALUES-TO-LIST")
    (assert-eq 'setq (car (third result)))
    (assert-eq 'a (second (third result)))
    (assert-eq 'setq (car (fourth result)))
    (assert-eq 'b (second (fourth result)))
    (assert-eq 'car (car (fifth result)))))
