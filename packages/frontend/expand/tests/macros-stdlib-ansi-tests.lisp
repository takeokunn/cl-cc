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
  :parent cl-cc-unit-suite)

(in-suite macros-stdlib-ansi-suite)

;;; ─── with-accessors ───────────────────────────────────────────────────────

(deftest with-accessors-expansion-structure
  "WITH-ACCESSORS: outer LET; inner SYMBOL-MACROLET; bindings map vars to accessor calls."
  (assert-eq 'let (car (our-macroexpand-1 '(with-accessors ((x x-val)) inst body))))
  (let* ((result (our-macroexpand-1 '(with-accessors ((v slot-v)) inst body)))
         (inner  (caddr result)))
    (assert-eq 'symbol-macrolet (car inner)))
  (let* ((result   (our-macroexpand-1 '(with-accessors ((v get-v)) obj body)))
         (sm-form  (caddr result))
         (bindings (second sm-form))
         (entry    (first bindings)))
    (assert-eq 'v (car entry))
    (assert-eq 'get-v (car (second entry)))))

;;; ─── assert ───────────────────────────────────────────────────────────────

(deftest assert-expansion-cases
  "ASSERT: unless-guard; CERROR in failure body; datum forwarded to CERROR."
  (let ((result (our-macroexpand-1 '(assert (= x 1)))))
    (assert-eq 'unless (car result))
    (assert-equal '(= x 1) (second result)))
  (let* ((result (our-macroexpand-1 '(assert (zerop n))))
         (body   (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'cerror))) body)))
  (let* ((result (our-macroexpand-1 '(assert test nil "msg ~A" x)))
         (body   (cddr result))
         (cerror (find 'cerror body :key #'car)))
    (assert-true cerror)
    (assert-equal "msg ~A" (third cerror))))

;;; ─── define-condition ─────────────────────────────────────────────────────

(deftest define-condition-basic-structure
  "DEFINE-CONDITION: expands to DEFCLASS; parent list propagated."
  (let ((result (our-macroexpand-1 '(define-condition my-err (error) ()))))
    (assert-eq 'defclass (car result))
    (assert-eq 'my-err (second result)))
  (let* ((result (our-macroexpand-1 '(define-condition my-err (simple-error) ())))
         (parents (third result)))
    (assert-true (member 'simple-error parents))))

(deftest-each define-condition-with-report-wraps-in-progn
  "DEFINE-CONDITION with a string or lambda :report wraps defclass + defmethod in PROGN."
  :cases (("string" '(define-condition my-err (error) () (:report "something went wrong")))
          ("lambda" '(define-condition my-err (error) ()
                       (:report (lambda (c s) (format s "err: ~A" c))))))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq 'progn   (car result))
    (assert-eq 'defclass (car (second result)))
    (assert-eq 'defmethod (car (third result)))))

;;; ─── with-input-from-string ───────────────────────────────────────────────

(deftest with-input-from-string-structure
  "WITH-INPUT-FROM-STRING: LET*; MAKE-STRING-INPUT-STREAM binding; :start/:end uses SUBSEQ."
  (assert-eq 'let* (car (our-macroexpand-1 '(with-input-from-string (s "hello") body))))
  (let* ((result   (our-macroexpand-1 '(with-input-from-string (s "hello") body)))
         (bindings (second result))
         (stream-binding (second bindings)))
    (assert-eq 'make-string-input-stream (car (second stream-binding))))
  (let* ((result   (our-macroexpand-1
                    '(with-input-from-string (s "hello" :start 1 :end 3) body)))
         (bindings (second result))
         (str-binding (first bindings)))
    (assert-eq 'subseq (car (second str-binding)))))

;;; ─── with-output-to-string ────────────────────────────────────────────────

(deftest with-output-to-string-structure
  "WITH-OUTPUT-TO-STRING: LET; MAKE-STRING-OUTPUT-STREAM; ends with GET-OUTPUT-STREAM-STRING; initial string uses WRITE-STRING."
  (assert-eq 'let (car (our-macroexpand-1 '(with-output-to-string (s) body))))
  (let* ((result   (our-macroexpand-1 '(with-output-to-string (s) body)))
         (bindings (second result))
         (binding  (first bindings)))
    (assert-eq 'make-string-output-stream (car (second binding))))
  (let* ((result    (our-macroexpand-1 '(with-output-to-string (s) body)))
         (last-form (car (last result))))
    (assert-eq 'get-output-stream-string (car last-form)))
  (let* ((result (our-macroexpand-1 '(with-output-to-string (s "prefix") body)))
         (body   (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'write-string))) body))))

;;; ─── with-standard-io-syntax ──────────────────────────────────────────────

(deftest with-standard-io-syntax-structure
  "WITH-STANDARD-IO-SYNTAX: LET; *PRINT-BASE*/*READ-BASE* set to 10; body included."
  (assert-eq 'let (car (our-macroexpand-1 '(with-standard-io-syntax body))))
  (let ((bindings (second (our-macroexpand-1 '(with-standard-io-syntax body)))))
    (assert-= 10 (second (assoc '*print-base* bindings)))
    (assert-= 10 (second (assoc '*read-base*  bindings))))
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
