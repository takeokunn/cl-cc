;;;; tests/unit/expand/macros-stdlib-ansi-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib-ansi.lisp
;;;;
;;;; Covers: with-accessors, assert, define-condition,
;;;; with-input-from-string, with-output-to-string,
;;;; with-standard-io-syntax, with-package-iterator, define-compiler-macro.
;;;;
;;;; Note: psetf, shiftf, and define-modify-macro are already covered in
;;;; macro-psetf-tests.lisp, macro-shiftf-tests.lisp,
;;;; and macro-define-modify-macro-tests.lisp.

(in-package :cl-cc/test)

(defsuite macros-stdlib-ansi-suite
  :description "Tests for macros-stdlib-ansi.lisp: ANSI CL Phase 1 macros"
  :parent cl-cc-suite)

(in-suite macros-stdlib-ansi-suite)

;;; ─── with-accessors ───────────────────────────────────────────────────────

(deftest with-accessors-expands-to-let
  "WITH-ACCESSORS wraps the body in a LET that binds the instance."
  (let ((result (our-macroexpand-1 '(with-accessors ((x x-val)) inst body))))
    (assert-eq 'let (car result))))

(deftest with-accessors-body-has-symbol-macrolet
  "WITH-ACCESSORS inner form is SYMBOL-MACROLET binding local vars to accessor calls."
  (let* ((result       (our-macroexpand-1 '(with-accessors ((v slot-v)) inst body)))
         (inner        (caddr result)))
    (assert-eq 'symbol-macrolet (car inner))))

(deftest with-accessors-accessor-call-in-binding
  "WITH-ACCESSORS symbol-macrolet entry maps each var to (accessor instance)."
  (let* ((result   (our-macroexpand-1 '(with-accessors ((v get-v)) obj body)))
         (sm-form  (caddr result))        ; symbol-macrolet
         (bindings (second sm-form))
         (entry    (first bindings)))
    ;; entry is (V (GET-V <gensym>))
    (assert-eq 'v (car entry))
    (assert-eq 'get-v (car (second entry)))))

;;; ─── assert ───────────────────────────────────────────────────────────────

(deftest assert-passing-test-expands-to-unless
  "ASSERT expands to an UNLESS that guards a CERROR signal."
  (let ((result (our-macroexpand-1 '(assert (= x 1)))))
    (assert-eq 'unless (car result))
    (assert-equal '(= x 1) (second result))))

(deftest assert-cerror-on-failure
  "ASSERT body on failure calls CERROR."
  (let* ((result (our-macroexpand-1 '(assert (zerop n))))
         (body   (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'cerror))) body))))

(deftest assert-with-datum-uses-datum
  "ASSERT with a datum passes it to CERROR."
  (let* ((result (our-macroexpand-1 '(assert test nil "msg ~A" x)))
         (body   (cddr result))
         (cerror (find 'cerror body :key #'car)))
    (assert-true cerror)
    (assert-equal "msg ~A" (third cerror))))

;;; ─── define-condition ─────────────────────────────────────────────────────

(deftest define-condition-without-report-is-defclass
  "DEFINE-CONDITION without :report expands to a plain DEFCLASS."
  (let ((result (our-macroexpand-1 '(define-condition my-err (error) ()))))
    (assert-eq 'defclass (car result))
    (assert-eq 'my-err (second result))))

(deftest define-condition-parent-list-propagated
  "DEFINE-CONDITION parent list is passed through to DEFCLASS."
  (let* ((result (our-macroexpand-1 '(define-condition my-err (simple-error) ())))
         (parents (third result)))
    (assert-true (member 'simple-error parents))))

(deftest define-condition-with-string-report-wraps-in-progn
  "DEFINE-CONDITION with a string :report wraps defclass + defmethod in PROGN."
  (let ((result (our-macroexpand-1
                 '(define-condition my-err (error) ()
                    (:report "something went wrong")))))
    (assert-eq 'progn (car result))
    (assert-eq 'defclass (car (second result)))
    (assert-eq 'defmethod (car (third result)))))

(deftest define-condition-with-lambda-report-wraps-in-progn
  "DEFINE-CONDITION with a lambda :report also wraps in PROGN."
  (let ((result (our-macroexpand-1
                 '(define-condition my-err (error) ()
                    (:report (lambda (c s) (format s "err: ~A" c)))))))
    (assert-eq 'progn (car result))
    (assert-eq 'defmethod (car (third result)))))

;;; ─── with-input-from-string ───────────────────────────────────────────────

(deftest with-input-from-string-expands-to-let
  "WITH-INPUT-FROM-STRING expands to a LET* binding the stream var."
  (let ((result (our-macroexpand-1 '(with-input-from-string (s "hello") body))))
    (assert-eq 'let* (car result))))

(deftest with-input-from-string-stream-is-make-string-input-stream
  "WITH-INPUT-FROM-STRING inner binding calls MAKE-STRING-INPUT-STREAM."
  (let* ((result   (our-macroexpand-1 '(with-input-from-string (s "hello") body)))
         (bindings (second result))
         ;; second binding is (s (make-string-input-stream ...))
         (stream-binding (second bindings)))
    (assert-eq 'make-string-input-stream (car (second stream-binding)))))

(deftest with-input-from-string-start-end-uses-subseq
  "WITH-INPUT-FROM-STRING with :start/:end wraps string in SUBSEQ."
  (let* ((result   (our-macroexpand-1
                    '(with-input-from-string (s "hello" :start 1 :end 3) body)))
         (bindings (second result))
         (str-binding (first bindings)))
    (assert-eq 'subseq (car (second str-binding)))))

;;; ─── with-output-to-string ────────────────────────────────────────────────

(deftest with-output-to-string-expands-to-let
  "WITH-OUTPUT-TO-STRING expands to a LET binding a string-output-stream."
  (let ((result (our-macroexpand-1 '(with-output-to-string (s) body))))
    (assert-eq 'let (car result))))

(deftest with-output-to-string-makes-string-output-stream
  "WITH-OUTPUT-TO-STRING binding calls MAKE-STRING-OUTPUT-STREAM."
  (let* ((result   (our-macroexpand-1 '(with-output-to-string (s) body)))
         (bindings (second result))
         (binding  (first bindings)))
    (assert-eq 'make-string-output-stream (car (second binding)))))

(deftest with-output-to-string-ends-with-get-output-stream-string
  "WITH-OUTPUT-TO-STRING last form calls GET-OUTPUT-STREAM-STRING."
  (let* ((result    (our-macroexpand-1 '(with-output-to-string (s) body)))
         (last-form (car (last result))))
    (assert-eq 'get-output-stream-string (car last-form))))

(deftest with-output-to-string-initial-string-writes-first
  "WITH-OUTPUT-TO-STRING with an initial string emits a WRITE-STRING call."
  (let* ((result (our-macroexpand-1 '(with-output-to-string (s "prefix") body)))
         (body   (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'write-string))) body))))

;;; ─── with-standard-io-syntax ──────────────────────────────────────────────

(deftest with-standard-io-syntax-expands-to-let
  "WITH-STANDARD-IO-SYNTAX expands to a LET with print/read variable bindings."
  (let ((result (our-macroexpand-1 '(with-standard-io-syntax body))))
    (assert-eq 'let (car result))))

(deftest with-standard-io-syntax-binds-print-base
  "WITH-STANDARD-IO-SYNTAX bindings include *PRINT-BASE* set to 10."
  (let* ((result   (our-macroexpand-1 '(with-standard-io-syntax body)))
         (bindings (second result))
         (pb       (assoc '*print-base* bindings)))
    (assert-true pb)
    (assert-= 10 (second pb))))

(deftest with-standard-io-syntax-binds-read-base
  "WITH-STANDARD-IO-SYNTAX bindings include *READ-BASE* set to 10."
  (let* ((result   (our-macroexpand-1 '(with-standard-io-syntax body)))
         (bindings (second result))
         (rb       (assoc '*read-base* bindings)))
    (assert-true rb)
    (assert-= 10 (second rb))))

(deftest with-standard-io-syntax-body-included
  "WITH-STANDARD-IO-SYNTAX passes the body through into the LET."
  (let* ((result (our-macroexpand-1 '(with-standard-io-syntax body)))
         (body   (cddr result)))
    (assert-true (member 'body body))))

;;; ─── define-compiler-macro ────────────────────────────────────────────────

(deftest define-compiler-macro-returns-quoted-name
  "DEFINE-COMPILER-MACRO expands to (QUOTE name), registering the macro as a side effect."
  (let ((result (our-macroexpand-1
                 '(define-compiler-macro my-fn (x) (1+ x)))))
    (assert-eq 'quote (car result))
    (assert-eq 'my-fn (second result))))
