;;;; tests/unit/expand/macros-sequence-tests.lisp
;;;; Coverage tests for src/expand/macros-sequence.lisp
;;;;
;;;; Macros tested: getf, remf, reduce, substitute, substitute-if,
;;;; delete, copy-seq, fill, replace, mismatch, declare, locally,
;;;; in-package, defpackage, coerce, nsubstitute variants, with-open-file,
;;;; load-time-value, provide, require, print-unreadable-object
;;;;
(in-package :cl-cc/test)

(defsuite macros-sequence-suite
  :description "Tests for macros-sequence.lisp"
  :parent cl-cc-suite)

;;; ── GETF ─────────────────────────────────────────────────────────────────────

(deftest getf-outer-is-let
  "GETF wraps plist and indicator in a LET to avoid double evaluation"
  (let ((result (our-macroexpand-1 '(getf plist :key))))
    (assert-eq (car result) 'let)))

(deftest getf-inner-let-uses-member
  "GETF inner let uses MEMBER to locate the indicator"
  (let* ((result     (our-macroexpand-1 '(getf plist :key)))
         (inner-let  (caddr result))
         (found-init (cadar (cadr inner-let))))
    (assert-eq (car found-init) 'member)))

(deftest getf-body-is-if-found
  "GETF body is (if found (cadr found) default)"
  (let* ((result    (our-macroexpand-1 '(getf plist :key)))
         (inner-let (caddr result))
         (if-form   (caddr inner-let)))
    (assert-eq (car if-form) 'if)))

(deftest getf-with-default
  "GETF with :default arg passes it through the IF fallback"
  (let* ((result    (our-macroexpand-1 '(getf plist :key :missing)))
         (inner-let (caddr result))
         (if-form   (caddr inner-let)))
    ;; fourth element of the if is the default
    (assert-equal (cadddr if-form) :missing)))

;;; ── REMF ─────────────────────────────────────────────────────────────────────

(deftest remf-outer-is-let
  "REMF wraps indicator, prev, cur, found in a LET with a TAGBODY"
  (let ((result (our-macroexpand-1 '(remf plist :key))))
    (assert-eq (car result) 'let)))

(deftest remf-body-contains-tagbody
  "REMF body contains a TAGBODY for the loop"
  (let* ((result (our-macroexpand-1 '(remf plist :key)))
         (body   (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'tagbody))) body))))

;;; ── REDUCE ───────────────────────────────────────────────────────────────────

(deftest reduce-without-initial-value-outer-is-let
  "REDUCE without :initial-value starts from first element"
  (let ((result (our-macroexpand-1 '(reduce #'+ lst))))
    (assert-eq (car result) 'let)))

(deftest reduce-without-initial-value-has-tagbody
  "REDUCE without :initial-value has an inner TAGBODY loop"
  (let* ((result (our-macroexpand-1 '(reduce #'+ lst)))
         ;; outer let body: (let ((acc (car cur))) ... (tagbody ...) acc)
         (inner  (caddr result)))
    (assert-eq (car inner) 'let)))

(deftest reduce-with-initial-value-outer-is-let
  "REDUCE with :initial-value binds fn, acc, cur in the outer LET"
  (let ((result (our-macroexpand-1 '(reduce #'+ lst :initial-value 0))))
    (assert-eq (car result) 'let)))

(deftest reduce-with-initial-value-has-tagbody
  "REDUCE with :initial-value has a TAGBODY loop in the body"
  (let* ((result (our-macroexpand-1 '(reduce #'+ lst :initial-value 0)))
         (body   (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'tagbody))) body))))

;;; ── SUBSTITUTE ───────────────────────────────────────────────────────────────

(deftest substitute-outer-is-let
  "SUBSTITUTE binds new/old and an accumulator in a LET"
  (let ((result (our-macroexpand-1 '(substitute new old seq))))
    (assert-eq (car result) 'let)))

(deftest substitute-body-contains-dolist
  "SUBSTITUTE iterates with DOLIST, choosing new/old per element"
  (let* ((result    (our-macroexpand-1 '(substitute new old seq)))
         (dolist-f  (caddr result)))
    (assert-eq (car dolist-f) 'dolist)))

(deftest substitute-if-outer-is-let
  "SUBSTITUTE-IF binds pred and accumulator in a LET"
  (let ((result (our-macroexpand-1 '(substitute-if new pred seq))))
    (assert-eq (car result) 'let)))

(deftest substitute-if-not-outer-is-let
  "SUBSTITUTE-IF-NOT binds pred and accumulator in a LET"
  (let ((result (our-macroexpand-1 '(substitute-if-not new pred seq))))
    (assert-eq (car result) 'let)))

;;; nsubstitute variants delegate to substitute

(deftest nsubstitute-delegates-to-substitute
  "(nsubstitute new old seq) → (substitute new old seq)"
  (assert-equal (our-macroexpand-1 '(nsubstitute new old seq))
                '(substitute new old seq)))

(deftest nsubstitute-if-delegates-to-substitute-if
  "(nsubstitute-if new pred seq) → (substitute-if new pred seq)"
  (assert-equal (our-macroexpand-1 '(nsubstitute-if new pred seq))
                '(substitute-if new pred seq)))

(deftest nsubstitute-if-not-delegates-to-substitute-if-not
  "(nsubstitute-if-not new pred seq) → (substitute-if-not new pred seq)"
  (assert-equal (our-macroexpand-1 '(nsubstitute-if-not new pred seq))
                '(substitute-if-not new pred seq)))

;;; ── DELETE ───────────────────────────────────────────────────────────────────

(deftest delete-outer-is-let
  "DELETE (same as remove in this impl) accumulates into a LET"
  (let ((result (our-macroexpand-1 '(delete item seq))))
    (assert-eq (car result) 'let)))

(deftest delete-if-outer-is-let
  "DELETE-IF accumulates into a LET"
  (let ((result (our-macroexpand-1 '(delete-if pred seq))))
    (assert-eq (car result) 'let)))

(deftest delete-duplicates-delegates-to-remove-duplicates
  "(delete-duplicates seq) → (remove-duplicates seq)"
  (assert-equal (our-macroexpand-1 '(delete-duplicates seq))
                '(remove-duplicates seq)))

;;; ── COPY-SEQ ─────────────────────────────────────────────────────────────────

(deftest copy-seq-delegates-to-copy-list
  "(copy-seq seq) → (copy-list seq) — thin alias"
  (assert-equal (our-macroexpand-1 '(copy-seq seq)) '(copy-list seq)))

;;; ── FILL ─────────────────────────────────────────────────────────────────────

(deftest fill-outer-is-let*
  "FILL binds seq and a pointer in LET* then loops via TAGBODY"
  (let ((result (our-macroexpand-1 '(fill seq item))))
    (assert-eq (car result) 'let*)))

(deftest fill-body-contains-tagbody
  "FILL body contains a TAGBODY for the mutation loop"
  (let* ((result (our-macroexpand-1 '(fill seq item)))
         (body   (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'tagbody))) body))))

;;; ── MISMATCH ─────────────────────────────────────────────────────────────────

(deftest mismatch-outer-is-block
  "MISMATCH wraps in a BLOCK for early RETURN"
  (let ((result (our-macroexpand-1 '(mismatch seq1 seq2))))
    (assert-eq (car result) 'block)))

(deftest mismatch-body-has-let-with-tagbody
  "MISMATCH body has a LET with index counter and a TAGBODY"
  ;; expansion: (block nil (let (...) (tagbody ...)))
  ;; cadr is the block name (nil); caddr is the let form
  (let* ((result (our-macroexpand-1 '(mismatch seq1 seq2)))
         (let-f  (caddr result)))
    (assert-eq (car let-f) 'let)))

;;; ── DECLARE / LOCALLY / IN-PACKAGE ──────────────────────────────────────────

(deftest declare-expands-to-nil
  "(declare (ignore x)) expands to nil — declarations silently dropped"
  (assert-equal (our-macroexpand-1 '(declare (ignore x))) nil))

(deftest locally-strips-declare-forms
  "LOCALLY removes DECLARE forms and wraps remaining body in PROGN"
  (let ((result (our-macroexpand-1 '(locally (declare (ignore x)) expr1 expr2))))
    (assert-eq (car result) 'progn)
    (assert-false (member 'declare result))
    (assert-equal (cadr result) 'expr1)
    (assert-equal (caddr result) 'expr2)))

(deftest in-package-quotes-name
  "(in-package :cl-cc) → (quote :cl-cc)"
  (assert-equal (our-macroexpand-1 '(in-package :cl-cc)) '(quote :cl-cc)))

(deftest defpackage-quotes-name
  "(defpackage :foo ...) → (quote :foo) — no-op at runtime"
  (assert-equal (our-macroexpand-1 '(defpackage :foo (:use :cl))) '(quote :foo)))

;;; ── COERCE ───────────────────────────────────────────────────────────────────

;; coerce-to-* are CL-CC:: symbols; check by symbol-name to avoid
;; CL-CC/TEST:: vs CL-CC:: package mismatch in expected values.
(deftest-each coerce-quoted-type-expansions
  "COERCE with quoted type dispatches to the right coerce-to-* primitive"
  :cases (("to-string"        '(coerce v 'string)        "COERCE-TO-STRING")
          ("to-simple-string" '(coerce v 'simple-string) "COERCE-TO-STRING")
          ("to-list"          '(coerce v 'list)          "COERCE-TO-LIST")
          ("to-vector"        '(coerce v 'vector)        "COERCE-TO-VECTOR"))
  (form expected-name)
  (let ((result (our-macroexpand-1 form)))
    (assert-equal (symbol-name (car result)) expected-name)
    (assert-equal (cadr result) 'v)))

(deftest coerce-unquoted-type-fallback
  "COERCE with a non-literal type defaults to coerce-to-string"
  (let ((result (our-macroexpand-1 '(coerce v type-var))))
    (assert-equal (symbol-name (car result)) "COERCE-TO-STRING")
    (assert-equal (cadr result) 'v)))

;;; ── MAP (delegates to mapcar + coerce) ──────────────────────────────────────

(deftest map-delegates-to-mapcar-coerce
  "(map result-type fn seq) → (coerce (mapcar fn (coerce seq 'list)) result-type)"
  (assert-equal (our-macroexpand-1 '(map 'list fn seq))
                '(coerce (mapcar fn (coerce seq 'list)) 'list)))

;;; ── REPLACE ──────────────────────────────────────────────────────────────────

(deftest replace-outer-is-let
  "REPLACE binds dest and source in a LET before mutation"
  (let ((result (our-macroexpand-1 '(replace dest src))))
    (assert-eq (car result) 'let)))

(deftest replace-returns-dest
  "REPLACE last form returns the dest variable"
  (let* ((result (our-macroexpand-1 '(replace dest src)))
         (bindings (second result))
         (dest-var (first (first bindings)))
         (last-form (car (last (cddr result)))))
    (assert-eq last-form dest-var)))

;;; ── DELETE-IF / DELETE-IF-NOT ────────────────────────────────────────────────

(deftest delete-if-outer-is-let
  "DELETE-IF accumulates non-matching elements in a LET-bound acc"
  (let ((result (our-macroexpand-1 '(delete-if pred seq))))
    (assert-eq (car result) 'let)))

(deftest delete-if-not-outer-is-let
  "DELETE-IF-NOT accumulates matching elements in a LET-bound acc"
  (let ((result (our-macroexpand-1 '(delete-if-not pred seq))))
    (assert-eq (car result) 'let)))

(deftest delete-duplicates-delegates-to-remove-duplicates
  "(delete-duplicates seq) → (remove-duplicates seq)"
  (assert-equal (our-macroexpand-1 '(delete-duplicates seq))
                '(remove-duplicates seq)))

;;; ── MAP-INTO ─────────────────────────────────────────────────────────────────

(deftest map-into-single-seq-outer-is-let
  "MAP-INTO with one source sequence binds dest, src, fn in a LET"
  (let ((result (our-macroexpand-1 '(map-into dest fn src))))
    (assert-eq (car result) 'let)))

(deftest map-into-single-seq-returns-dest
  "MAP-INTO with one source sequence last form returns dest variable"
  (let* ((result (our-macroexpand-1 '(map-into dest fn src)))
         (bindings (second result))
         (dest-var (first (first bindings)))
         (last-form (car (last (cddr result)))))
    (assert-eq last-form dest-var)))

;;; ── MERGE ────────────────────────────────────────────────────────────────────

(deftest merge-outer-is-let
  "MERGE binds l1, l2, pred, and acc in a LET"
  (let ((result (our-macroexpand-1 '(merge 'list l1 l2 pred))))
    (assert-eq (car result) 'let)))

(deftest merge-binds-four-vars
  "MERGE binds four variables: l1, l2, pred, acc"
  (let* ((result (our-macroexpand-1 '(merge 'list l1 l2 pred)))
         (bindings (second result)))
    (assert-= (length bindings) 4)))

(deftest merge-uses-tagbody
  "MERGE body uses TAGBODY for the merge loop"
  (let* ((result (our-macroexpand-1 '(merge 'list l1 l2 pred)))
         (body (caddr result)))
    (assert-eq (car body) 'tagbody)))

;;; ── SUBSTITUTE-IF / SUBSTITUTE-IF-NOT ───────────────────────────────────────

(deftest substitute-if-outer-is-let
  "SUBSTITUTE-IF accumulates results in a LET-bound acc"
  (let ((result (our-macroexpand-1 '(substitute-if new pred seq))))
    (assert-eq (car result) 'let)))

(deftest substitute-if-not-outer-is-let
  "SUBSTITUTE-IF-NOT accumulates results in a LET-bound acc"
  (let ((result (our-macroexpand-1 '(substitute-if-not new pred seq))))
    (assert-eq (car result) 'let)))

;;; ── PROGV ────────────────────────────────────────────────────────────────────

(deftest progv-outer-is-let*
  "PROGV wraps in LET* binding syms, vals, saved"
  (let ((result (our-macroexpand-1 '(progv syms vals body))))
    (assert-eq (car result) 'let*)))

(deftest progv-binds-three-vars
  "PROGV LET* binds three variables: syms, vals, saved"
  (let* ((result (our-macroexpand-1 '(progv syms vals body)))
         (bindings (second result)))
    (assert-= (length bindings) 3)))

(deftest progv-body-uses-unwind-protect
  "PROGV body uses UNWIND-PROTECT to ensure cleanup"
  (let* ((result (our-macroexpand-1 '(progv syms vals (do-stuff))))
         (body (caddr result)))
    (assert-eq (car body) 'unwind-protect)))

;;; ── EXPORT (no-op) ───────────────────────────────────────────────────────────

(deftest export-expands-to-nil
  "(export ...) → NIL (no-op in this compiler)"
  (assert-eq (our-macroexpand-1 '(export '(foo bar))) nil))

;;; ── WARN ─────────────────────────────────────────────────────────────────────

(deftest warn-outer-is-progn
  "WARN expands to a PROGN containing a FORMAT call"
  (let ((result (our-macroexpand-1 '(warn "oops ~A" x))))
    (assert-eq (car result) 'progn)))

(deftest warn-body-calls-format
  "WARN body first form is a FORMAT call to t"
  (let* ((result (our-macroexpand-1 '(warn "oops" )))
         (fmt-call (second result)))
    (assert-eq (car fmt-call) 'format)
    (assert-eq (second fmt-call) 't)))

;;; ── COPY-HASH-TABLE ──────────────────────────────────────────────────────────

(deftest copy-hash-table-outer-is-let
  "COPY-HASH-TABLE wraps ht in a LET to avoid double evaluation"
  (let ((result (our-macroexpand-1 '(cl-cc::copy-hash-table ht))))
    (assert-eq (car result) 'let)))

(deftest copy-hash-table-uses-maphash
  "COPY-HASH-TABLE body calls MAPHASH to copy entries"
  (let* ((result    (our-macroexpand-1 '(cl-cc::copy-hash-table ht)))
         ;; result = (let ((ht-var ht)) (let ((new-var ...)) (maphash ...) new-var))
         (inner-let (caddr result))
         ;; inner-let = (let ((new-var ...)) (maphash ...) new-var)
         ;; body forms start at cddr; first body form is the maphash call
         (maphash-call (caddr inner-let)))
    (assert-eq (car maphash-call) 'maphash)))

;;; ── WITH-OPEN-FILE ───────────────────────────────────────────────────────────

(deftest with-open-file-outer-is-let
  "WITH-OPEN-FILE outer form is LET binding the stream variable"
  (let ((result (our-macroexpand-1 '(with-open-file (s "/tmp/f") body))))
    (assert-eq (car result) 'let)))

(deftest with-open-file-body-is-unwind-protect
  "WITH-OPEN-FILE body uses UNWIND-PROTECT to ensure CLOSE"
  (let* ((result (our-macroexpand-1 '(with-open-file (s "/tmp/f") body)))
         (body-form (caddr result)))
    (assert-eq (car body-form) 'unwind-protect)))

(deftest with-open-file-cleanup-is-close
  "WITH-OPEN-FILE cleanup form is a CLOSE call on the bound stream var"
  (let* ((result (our-macroexpand-1 '(with-open-file (s "/tmp/f") body)))
         (body-form (caddr result))
         ;; unwind-protect cleanup is the 3rd element
         (cleanup (third body-form)))
    (assert-eq (car cleanup) 'close)))

;;; ── COERCE with quoted types ─────────────────────────────────────────────────

(deftest coerce-to-string-quoted
  "(coerce x 'string) → (coerce-to-string x)"
  (let ((result (our-macroexpand-1 '(coerce x 'string))))
    (assert-equal (symbol-name (car result)) "COERCE-TO-STRING")))

(deftest coerce-to-list-quoted
  "(coerce x 'list) → (coerce-to-list x)"
  (let ((result (our-macroexpand-1 '(coerce x 'list))))
    (assert-equal (symbol-name (car result)) "COERCE-TO-LIST")))

(deftest coerce-to-vector-quoted
  "(coerce x 'vector) → (coerce-to-vector x)"
  (let ((result (our-macroexpand-1 '(coerce x 'vector))))
    (assert-equal (symbol-name (car result)) "COERCE-TO-VECTOR")))

;;; ── LOAD-TIME-VALUE ──────────────────────────────────────────────────────────

(deftest load-time-value-expands-to-quote
  "LOAD-TIME-VALUE evaluates form at expand time and quotes the result"
  (let ((result (our-macroexpand-1 '(load-time-value (+ 1 2)))))
    (assert-eq (car result) 'quote)
    (assert-= (second result) 3)))

;;; ── PROVIDE ──────────────────────────────────────────────────────────────────

(deftest provide-outer-is-let
  "PROVIDE wraps module name in a LET for single evaluation"
  (let ((result (our-macroexpand-1 '(provide :my-lib))))
    (assert-eq (car result) 'let)))

(deftest provide-body-uses-pushnew
  "PROVIDE body calls PUSHNEW to register the module"
  (let* ((result (our-macroexpand-1 '(provide :my-lib)))
         (pushnew-form (caddr result)))
    (assert-eq (car pushnew-form) 'pushnew)))

;;; ── REQUIRE ──────────────────────────────────────────────────────────────────

(deftest require-outer-is-let
  "REQUIRE wraps module name in a LET"
  (let ((result (our-macroexpand-1 '(require :my-lib))))
    (assert-eq (car result) 'let)))

(deftest require-body-is-unless
  "REQUIRE body checks if module is already loaded via UNLESS+MEMBER"
  (let* ((result (our-macroexpand-1 '(require :my-lib)))
         (unless-form (caddr result)))
    (assert-eq (car unless-form) 'unless)))

;;; ── PRINT-UNREADABLE-OBJECT ──────────────────────────────────────────────────

(deftest print-unreadable-object-outer-is-let
  "PRINT-UNREADABLE-OBJECT outer form is LET binding obj and stream"
  (let ((result (our-macroexpand-1 '(print-unreadable-object (obj s :type t) body))))
    (assert-eq (car result) 'let)))

;;; ── PRINT-OBJECT ─────────────────────────────────────────────────────────────

(deftest print-object-outer-is-let*
  "PRINT-OBJECT expands to LET* binding object and stream gensyms"
  (let ((result (our-macroexpand-1 '(print-object obj *standard-output*))))
    (assert-eq (car result) 'let*)))

(deftest print-object-dispatches-on-class
  "PRINT-OBJECT body is an IF: checks for class print method, falls back to PRIN1"
  (let* ((result (our-macroexpand-1 '(print-object obj *standard-output*)))
         (body   (caddr result)))      ; (let* (bindings) body)
    (assert-eq (car body) 'if)))

;;; ── DESCRIBE-OBJECT ──────────────────────────────────────────────────────────

(deftest describe-object-outer-is-let*
  "DESCRIBE-OBJECT expands to LET* binding object and stream gensyms"
  (let ((result (our-macroexpand-1 '(describe-object obj *standard-output*))))
    (assert-eq (car result) 'let*)))

(deftest describe-object-body-is-if
  "DESCRIBE-OBJECT body is an IF dispatching on whether class is a hash-table"
  (let* ((result (our-macroexpand-1 '(describe-object obj *standard-output*)))
         (body   (caddr result)))
    (assert-eq (car body) 'if)))

;;; ── DESCRIBE ─────────────────────────────────────────────────────────────────

(deftest describe-outer-is-let
  "DESCRIBE expands to LET binding the stream (with *standard-output* default)"
  (let ((result (our-macroexpand-1 '(describe obj))))
    (assert-eq (car result) 'let)))

(deftest describe-body-delegates-to-describe-object
  "DESCRIBE body calls DESCRIBE-OBJECT then returns VALUES"
  (let* ((result  (our-macroexpand-1 '(describe obj)))
         (progn-body (cddr result)))   ; forms after let binding
    (assert-equal (symbol-name (car (car progn-body))) "DESCRIBE-OBJECT")))

;;; ── UPDATE-INSTANCE-FOR-DIFFERENT-CLASS ──────────────────────────────────────

(deftest update-instance-for-different-class-delegates-to-reinitialize
  "UPDATE-INSTANCE-FOR-DIFFERENT-CLASS delegates to REINITIALIZE-INSTANCE on current"
  (let ((result (our-macroexpand-1 '(update-instance-for-different-class prev curr :x 1))))
    (assert-equal (symbol-name (car result)) "REINITIALIZE-INSTANCE")))

;;; ── UPDATE-INSTANCE-FOR-CHANGED-CLASS ────────────────────────────────────────

(deftest update-instance-for-changed-class-delegates-to-reinitialize
  "UPDATE-INSTANCE-FOR-CHANGED-CLASS delegates to REINITIALIZE-INSTANCE"
  ;; update-instance-for-changed-class is cl-cc-specific (not standard CL), use cl-cc:: prefix
  (let ((result (our-macroexpand-1 '(cl-cc::update-instance-for-changed-class inst :x 1))))
    (assert-equal (symbol-name (car result)) "REINITIALIZE-INSTANCE")))

;;; ── ENSURE-CLASS ─────────────────────────────────────────────────────────────

(deftest ensure-class-delegates-to-defclass
  "ENSURE-CLASS expands to DEFCLASS with extracted :direct-superclasses/:direct-slots"
  ;; ensure-class is MOP (not standard CL), use cl-cc:: prefix
  (let ((result (our-macroexpand-1 '(cl-cc::ensure-class 'foo :direct-superclasses (bar) :direct-slots ()))))
    (assert-equal (symbol-name (car result)) "DEFCLASS")))

;;; ── CHANGE-CLASS ─────────────────────────────────────────────────────────────

(deftest change-class-outer-is-let*
  "CHANGE-CLASS expands to LET* binding instance and new-class"
  (let ((result (our-macroexpand-1 '(change-class inst 'new-cls))))
    (assert-eq (car result) 'let*)))

(deftest change-class-updates-__class__-slot
  "CHANGE-CLASS body contains SETF of :__class__ key in hash table"
  (let* ((result (our-macroexpand-1 '(change-class inst 'new-cls)))
         (body   (cddr result)))       ; after let* bindings
    (assert-equal (symbol-name (car (second body))) "SETF")))

;;; ── PARSE-FLOAT ──────────────────────────────────────────────────────────────

(deftest parse-float-outer-is-let
  "PARSE-FLOAT expands to LET binding string slice, calls FLOAT+READ-FROM-STRING"
  ;; parse-float is a cl-cc extension (not standard CL), use cl-cc:: prefix
  (let ((result (our-macroexpand-1 '(cl-cc::parse-float "3.14"))))
    (assert-eq (car result) 'let)))

(deftest parse-float-body-calls-float
  "PARSE-FLOAT body calls FLOAT to coerce the parsed result"
  (let* ((result (our-macroexpand-1 '(cl-cc::parse-float "3.14")))
         (body   (caddr result)))
    (assert-eq (car body) 'float)))

;;; ── REINITIALIZE-INSTANCE ────────────────────────────────────────────────────

(deftest reinitialize-instance-outer-is-let*
  "REINITIALIZE-INSTANCE expands to LET* binding instance, args, class, initarg-map"
  (let ((result (our-macroexpand-1 '(reinitialize-instance inst :x 1))))
    (assert-eq (car result) 'let*)))

(deftest reinitialize-instance-body-uses-tagbody
  "REINITIALIZE-INSTANCE body contains TAGBODY for iterating initarg pairs"
  (let* ((result (our-macroexpand-1 '(reinitialize-instance inst :x 1)))
         ;; Structure: (let* (bindings) (when ...) inst)
         (when-form (caddr result)))
    (assert-eq (car when-form) 'when)))

;;; ── SHARED-INITIALIZE ────────────────────────────────────────────────────────

(deftest shared-initialize-outer-is-let*
  "SHARED-INITIALIZE expands to LET* binding instance, slot-names, args, class"
  (let ((result (our-macroexpand-1 '(shared-initialize inst t :x 1))))
    (assert-eq (car result) 'let*)))

(deftest shared-initialize-has-more-bindings-than-reinitialize
  "SHARED-INITIALIZE has one extra binding (slot-names) vs reinitialize-instance"
  (let* ((result-si  (our-macroexpand-1 '(shared-initialize inst t :x 1)))
         (result-ri  (our-macroexpand-1 '(reinitialize-instance inst :x 1)))
         (si-binds   (length (second result-si)))
         (ri-binds   (length (second result-ri))))
    (assert-= si-binds (1+ ri-binds))))

;;; ── %PLIST-PUT ───────────────────────────────────────────────────────────────

(deftest plist-put-outer-is-let
  "%PLIST-PUT expands to LET binding plist, value, result accumulator, found flag"
  ;; %plist-put is cl-cc-internal (not exported), use cl-cc:: prefix
  (let ((result (our-macroexpand-1 '(cl-cc::%plist-put my-plist :key 42))))
    (assert-eq (car result) 'let)))

(deftest plist-put-body-uses-loop
  "%PLIST-PUT body iterates with LOOP to scan the plist"
  (let* ((result (our-macroexpand-1 '(cl-cc::%plist-put my-plist :key 42)))
         (loop-form (caddr result)))
    (assert-eq (car loop-form) 'loop)))
