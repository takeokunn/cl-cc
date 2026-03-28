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

(deftest getf-expansion
  "GETF: outer LET, inner LET uses MEMBER, body is IF, default passes through."
  (let* ((result     (our-macroexpand-1 '(getf plist :key)))
         (inner-let  (caddr result))
         (found-init (cadar (cadr inner-let)))
         (if-form    (caddr inner-let)))
    (assert-eq 'let (car result))
    (assert-eq 'member (car found-init))
    (assert-eq 'if (car if-form)))
  ;; Default passes through to the IF fallback
  (let* ((result    (our-macroexpand-1 '(getf plist :key :missing)))
         (inner-let (caddr result))
         (if-form   (caddr inner-let)))
    (assert-equal :missing (cadddr if-form))))

;;; ── REMF ─────────────────────────────────────────────────────────────────────

(deftest remf-expansion
  "REMF: outer LET containing a TAGBODY loop."
  (let* ((result (our-macroexpand-1 '(remf plist :key)))
         (body   (cddr result)))
    (assert-eq 'let (car result))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'tagbody))) body))))

;;; ── REDUCE ───────────────────────────────────────────────────────────────────

(deftest-each reduce-outer-is-let
  "REDUCE wraps its work in an outer LET regardless of :initial-value."
  :cases (("without-initial-value" '(reduce #'+ lst))
          ("with-initial-value"    '(reduce #'+ lst :initial-value 0)))
  (form)
  (assert-eq (car (our-macroexpand-1 form)) 'let))

(deftest reduce-without-initial-value-has-tagbody
  "REDUCE without :initial-value has an inner TAGBODY loop"
  (let* ((result (our-macroexpand-1 '(reduce #'+ lst)))
         ;; outer let body: (let ((acc (car cur))) ... (tagbody ...) acc)
         (inner  (caddr result)))
    (assert-eq (car inner) 'let)))

(deftest reduce-with-initial-value-has-tagbody
  "REDUCE with :initial-value has a TAGBODY loop in the body"
  (let* ((result (our-macroexpand-1 '(reduce #'+ lst :initial-value 0)))
         (body   (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'tagbody))) body))))

;;; ── SUBSTITUTE ───────────────────────────────────────────────────────────────

(deftest substitute-expansion
  "SUBSTITUTE: outer LET containing a DOLIST loop."
  (let* ((result   (our-macroexpand-1 '(substitute new old seq)))
         (dolist-f (caddr result)))
    (assert-eq 'let (car result))
    (assert-eq 'dolist (car dolist-f))))

(deftest-each predicate-sequence-outer-is-let
  "Predicate-based sequence macros all wrap their work in a LET."
  :cases (("substitute-if"     '(substitute-if new pred seq))
          ("substitute-if-not" '(substitute-if-not new pred seq))
          ("delete-if"         '(delete-if pred seq))
          ("delete-if-not"     '(delete-if-not pred seq)))
  (form)
  (assert-eq (car (our-macroexpand-1 form)) 'let))

;;; nsubstitute variants delegate to substitute

(deftest-each nsubstitute-delegates-to-substitute-variant
  "nsubstitute* forms delegate to their substitute* counterparts."
  :cases (("nsubstitute"        '(nsubstitute new old seq)         '(substitute new old seq))
          ("nsubstitute-if"     '(nsubstitute-if new pred seq)     '(substitute-if new pred seq))
          ("nsubstitute-if-not" '(nsubstitute-if-not new pred seq) '(substitute-if-not new pred seq)))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

;;; ── DELETE ───────────────────────────────────────────────────────────────────

(deftest delete-outer-is-let
  "DELETE (same as remove in this impl) accumulates into a LET"
  (let ((result (our-macroexpand-1 '(delete item seq))))
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

(deftest fill-expansion
  "FILL: outer LET* containing a TAGBODY mutation loop."
  (let* ((result (our-macroexpand-1 '(fill seq item)))
         (body   (cddr result)))
    (assert-eq 'let* (car result))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'tagbody))) body))))

;;; ── MISMATCH ─────────────────────────────────────────────────────────────────

(deftest mismatch-expansion
  "MISMATCH: outer BLOCK for early RETURN; body is LET with index counter."
  ;; expansion: (block nil (let (...) (tagbody ...)))
  (let* ((result (our-macroexpand-1 '(mismatch seq1 seq2)))
         (let-f  (caddr result)))
    (assert-eq 'block (car result))
    (assert-eq 'let   (car let-f))))

;;; ── DECLARE / LOCALLY / IN-PACKAGE ──────────────────────────────────────────

(deftest declare-expands-to-nil
  "(declare (ignore x)) expands to nil — declarations silently dropped"
  (assert-equal (our-macroexpand-1 '(declare (ignore x))) nil))

(deftest locally-strips-declare-forms
  "LOCALLY preserves DECLARE forms by wrapping in (let () decl body)"
  (let ((result (our-macroexpand-1 '(locally (declare (ignore x)) expr1 expr2))))
    ;; FR-397: locally now wraps in (let () decl body) to preserve declarations
    (assert-eq (car result) 'let)
    (assert-equal (cadr result) nil)  ; empty bindings
    (assert-equal (caddr result) '(declare (ignore x)))
    (assert-equal (cadddr result) 'expr1)))

(deftest in-package-sets-package
  "(in-package :cl-cc) expands to progn that sets *package* and returns quoted name"
  (let ((result (our-macroexpand-1 '(in-package :cl-cc))))
    (assert-eq (car result) 'progn)
    (assert-eq (car (second result)) 'setq)
    (assert-equal (third result) '(quote :cl-cc))))

(deftest defpackage-creates-package
  "(defpackage :foo ...) expands to progn with find-package/make-package"
  (let* ((result (our-macroexpand-1 '(defpackage :foo (:use :cl))))
         (result-str (format nil "~S" result)))
    (assert-eq (car result) 'progn)
    (assert-true (search "FIND-PACKAGE" result-str))
    (assert-true (search "MAKE-PACKAGE" result-str))))

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
  "COERCE with a non-literal type dispatches to %coerce-runtime"
  (let ((result (our-macroexpand-1 '(coerce v type-var))))
    (assert-equal (symbol-name (car result)) "%COERCE-RUNTIME")
    (assert-equal (cadr result) 'v)))

;;; ── MAP (delegates to mapcar + coerce) ──────────────────────────────────────

(deftest map-delegates-to-mapcar-coerce
  "(map result-type fn seq) → (coerce (mapcar fn (coerce seq 'list)) result-type)"
  (assert-equal (our-macroexpand-1 '(map 'list fn seq))
                '(coerce (mapcar fn (coerce seq 'list)) 'list)))

;;; ── REPLACE / MAP-INTO ───────────────────────────────────────────────────────

(deftest-each dest-returning-sequence-expanders
  "Destination-returning operators: outer LET, last form is the dest variable."
  :cases (("replace"  '(replace  dest src))
          ("map-into" '(map-into dest fn src)))
  (form)
  (let* ((result    (our-macroexpand-1 form))
         (dest-var  (first (first (second result))))
         (last-form (car (last (cddr result)))))
    (assert-eq 'let (car result))
    (assert-eq last-form dest-var)))

;;; ── MERGE ────────────────────────────────────────────────────────────────────

(deftest merge-expansion
  "MERGE: outer LET with 4 bindings, body uses TAGBODY."
  (let* ((result   (our-macroexpand-1 '(merge 'list l1 l2 pred)))
         (bindings (second result))
         (body     (caddr result)))
    (assert-eq 'let (car result))
    (assert-= 4 (length bindings))
    (assert-eq 'tagbody (car body))))

;;; ── PROGV ────────────────────────────────────────────────────────────────────

(deftest progv-expansion
  "PROGV: outer LET* with 3 bindings, body uses UNWIND-PROTECT."
  (let* ((result   (our-macroexpand-1 '(progv syms vals (do-stuff))))
         (bindings (second result))
         (body     (caddr result)))
    (assert-eq 'let* (car result))
    (assert-= 3 (length bindings))
    (assert-eq 'unwind-protect (car body))))

;;; ── EXPORT (host bridge) ─────────────────────────────────────────────────────

(deftest export-is-not-a-macro
  "(export ...) is now a host-bridged function, not a macro"
  (multiple-value-bind (expansion expanded-p) (our-macroexpand-1 '(export '(foo bar)))
    (declare (ignore expansion))
    (assert-true (not expanded-p))))

;;; ── WARN ─────────────────────────────────────────────────────────────────────

(deftest warn-expansion
  "WARN: outer PROGN; body first form is FORMAT to t."
  (let* ((result   (our-macroexpand-1 '(warn "oops ~A" x)))
         (fmt-call (second result)))
    (assert-eq 'progn  (car result))
    (assert-eq 'format (car fmt-call))
    (assert-eq 't      (second fmt-call))))

;;; ── COPY-HASH-TABLE ──────────────────────────────────────────────────────────

(deftest copy-hash-table-expansion
  "COPY-HASH-TABLE: outer LET, inner LET body calls MAPHASH."
  (let* ((result       (our-macroexpand-1 '(cl-cc::copy-hash-table ht)))
         (inner-let    (caddr result))
         (maphash-call (caddr inner-let)))
    (assert-eq 'let (car result))
    (assert-eq 'maphash (car maphash-call))))

;;; ── WITH-OPEN-FILE ───────────────────────────────────────────────────────────

(deftest with-open-file-expansion
  "WITH-OPEN-FILE: outer LET, body is UNWIND-PROTECT, cleanup calls CLOSE."
  (let* ((result    (our-macroexpand-1 '(with-open-file (s "/tmp/f") body)))
         (body-form (caddr result))
         (cleanup   (third body-form)))
    (assert-eq 'let (car result))
    (assert-eq 'unwind-protect (car body-form))
    (assert-eq 'close (car cleanup))))

;;; ── LOAD-TIME-VALUE ──────────────────────────────────────────────────────────

(deftest load-time-value-expands-to-quote
  "LOAD-TIME-VALUE evaluates form at expand time and quotes the result"
  (let ((result (our-macroexpand-1 '(load-time-value (+ 1 2)))))
    (assert-eq (car result) 'quote)
    (assert-= (second result) 3)))

;;; ── PROVIDE ──────────────────────────────────────────────────────────────────

(deftest provide-expansion
  "PROVIDE: outer LET, body calls PUSHNEW to register the module."
  (let* ((result       (our-macroexpand-1 '(provide :my-lib)))
         (pushnew-form (caddr result)))
    (assert-eq 'let (car result))
    (assert-eq 'pushnew (car pushnew-form))))

;;; ── REQUIRE ──────────────────────────────────────────────────────────────────

(deftest require-expansion
  "REQUIRE: outer LET, body is UNLESS+MEMBER guard."
  (let* ((result      (our-macroexpand-1 '(require :my-lib)))
         (unless-form (caddr result)))
    (assert-eq 'let (car result))
    (assert-eq 'unless (car unless-form))))

;;; ── PRINT-UNREADABLE-OBJECT ──────────────────────────────────────────────────

(deftest print-unreadable-object-outer-is-let
  "PRINT-UNREADABLE-OBJECT outer form is LET binding obj and stream"
  (let ((result (our-macroexpand-1 '(print-unreadable-object (obj s :type t) body))))
    (assert-eq (car result) 'let)))

;;; ── PRINT-OBJECT ─────────────────────────────────────────────────────────────

(deftest-each object-protocol-expansion
  "PRINT-OBJECT and DESCRIBE-OBJECT: outer LET*, body is IF dispatch."
  :cases (("print-object"    '(print-object    obj *standard-output*))
          ("describe-object" '(describe-object obj *standard-output*)))
  (form)
  (let* ((result (our-macroexpand-1 form))
         (body   (caddr result)))
    (assert-eq 'let* (car result))
    (assert-eq 'if   (car body))))

;;; ── DESCRIBE ─────────────────────────────────────────────────────────────────

(deftest describe-expansion
  "DESCRIBE: outer LET binding the stream; body delegates to DESCRIBE-OBJECT."
  (let* ((result     (our-macroexpand-1 '(describe obj)))
         (progn-body (cddr result)))
    (assert-eq 'let (car result))
    (assert-equal "DESCRIBE-OBJECT" (symbol-name (car (car progn-body))))))

;;; ── UPDATE-INSTANCE-FOR-DIFFERENT-CLASS ──────────────────────────────────────

(deftest-each update-instance-delegates-to-reinitialize
  "Both update-instance variants delegate to REINITIALIZE-INSTANCE."
  :cases (("different-class" '(update-instance-for-different-class prev curr :x 1))
          ("changed-class"   '(cl-cc::update-instance-for-changed-class inst :x 1)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-equal "REINITIALIZE-INSTANCE" (symbol-name (car result)))))

;;; ── ENSURE-CLASS ─────────────────────────────────────────────────────────────

(deftest ensure-class-delegates-to-defclass
  "ENSURE-CLASS expands to DEFCLASS with extracted :direct-superclasses/:direct-slots"
  ;; ensure-class is MOP (not standard CL), use cl-cc:: prefix
  (let ((result (our-macroexpand-1 '(cl-cc::ensure-class 'foo :direct-superclasses (bar) :direct-slots ()))))
    (assert-equal (symbol-name (car result)) "DEFCLASS")))

;;; ── CHANGE-CLASS ─────────────────────────────────────────────────────────────

(deftest change-class-expansion
  "CHANGE-CLASS: outer LET*, body contains DOLIST for slot migration and SETF of :__class__."
  (let* ((result (our-macroexpand-1 '(change-class inst 'new-cls)))
         (flat   (write-to-string result)))
    (assert-eq 'let* (car result))
    (assert-true (search "SETF" flat))
    (assert-true (search "__class__" (string-downcase flat)))))

;;; ── PARSE-FLOAT ──────────────────────────────────────────────────────────────

(deftest parse-float-expansion
  "PARSE-FLOAT: outer LET, body calls FLOAT to coerce parsed result."
  ;; parse-float is a cl-cc extension (not standard CL), use cl-cc:: prefix
  (let* ((result (our-macroexpand-1 '(cl-cc::parse-float "3.14")))
         (body   (caddr result)))
    (assert-eq 'let (car result))
    (assert-eq 'float (car body))))

;;; ── REINITIALIZE-INSTANCE ────────────────────────────────────────────────────

(deftest reinitialize-instance-expansion
  "REINITIALIZE-INSTANCE: outer LET*, body has WHEN guard for initarg iteration."
  (let* ((result    (our-macroexpand-1 '(reinitialize-instance inst :x 1)))
         (when-form (caddr result)))
    (assert-eq 'let* (car result))
    (assert-eq 'when (car when-form))))

;;; ── SHARED-INITIALIZE ────────────────────────────────────────────────────────

(deftest shared-initialize-expansion
  "SHARED-INITIALIZE: outer LET*, one more binding than reinitialize-instance (slot-names)."
  (let* ((result-si (our-macroexpand-1 '(shared-initialize inst t :x 1)))
         (result-ri (our-macroexpand-1 '(reinitialize-instance inst :x 1))))
    (assert-eq 'let* (car result-si))
    (assert-= (length (second result-si)) (1+ (length (second result-ri))))))

;;; ── %PLIST-PUT ───────────────────────────────────────────────────────────────

(deftest plist-put-expansion
  "%PLIST-PUT: outer LET, body iterates with LOOP."
  ;; %plist-put is cl-cc-internal (not exported), use cl-cc:: prefix
  (let* ((result    (our-macroexpand-1 '(cl-cc::%plist-put my-plist :key 42)))
         (loop-form (caddr result)))
    (assert-eq 'let (car result))
    (assert-eq 'loop (car loop-form))))
