;;;; tests/unit/parse/cl/grammar-tokenstream-tests.lisp
;;;; Unit tests for src/parse/cl/grammar.lisp — Token Stream helpers
;;;;
;;;; Covers: token-stream struct (make-token-stream, predicates),
;;;;   ts-peek, ts-advance, ts-at-end-p, ts-peek-type, ts-token-value,
;;;;   ts-expect (success, EOF, type mismatch),
;;;;   %tok-to-cst, %make-list-cst,
;;;;   parse-cl-source (simple integer, symbol, list).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Token construction helpers ──────────────────────────────────────────

(defun make-test-token (type value &optional (start 0) (end 1))
  "Build a lexer-token for testing."
  (cl-cc/parse::make-lexer-token :type type :value value
                            :start-byte start :end-byte end :trivia nil))

(defun make-test-ts (token-list &optional (source ""))
  "Build a token-stream from a list of lexer-tokens."
  (cl-cc/parse::make-token-stream :tokens token-list :source source))

;;; ─── ts-peek ─────────────────────────────────────────────────────────────

(deftest ts-peek-returns-first-token-without-consuming
  "ts-peek returns the first token and does not advance the stream."
  (let* ((tok (make-test-token :T-INT 42))
         (ts  (make-test-ts (list tok))))
    (assert-eq tok (cl-cc/parse::ts-peek ts))
    ;; Peeking again returns the same token (not consumed)
    (assert-eq tok (cl-cc/parse::ts-peek ts))))

(deftest ts-peek-returns-nil-on-empty-stream
  "ts-peek returns nil when the stream is empty."
  (let ((ts (make-test-ts nil)))
    (assert-null (cl-cc/parse::ts-peek ts))))

;;; ─── ts-advance ──────────────────────────────────────────────────────────

(deftest ts-advance-consumes-and-returns-first-token
  "ts-advance returns the first token and removes it from the stream."
  (let* ((t1  (make-test-token :T-INT 1))
         (t2  (make-test-token :T-INT 2))
         (ts  (make-test-ts (list t1 t2))))
    (let ((consumed (cl-cc/parse::ts-advance ts)))
      (assert-eq t1 consumed)
      ;; Stream now has only t2
      (assert-eq t2 (cl-cc/parse::ts-peek ts)))))

(deftest ts-advance-on-empty-returns-nil
  "ts-advance on an empty stream returns nil."
  (let ((ts (make-test-ts nil)))
    (assert-null (cl-cc/parse::ts-advance ts))))

;;; ─── ts-peek-type ────────────────────────────────────────────────────────

(deftest ts-peek-type-returns-type-of-current-token
  "ts-peek-type returns the :type field of the head token."
  (let* ((tok (make-test-token :T-IDENT 'foo))
         (ts  (make-test-ts (list tok))))
    (assert-eq :T-IDENT (cl-cc/parse::ts-peek-type ts))))

(deftest ts-peek-type-returns-nil-on-empty
  "ts-peek-type returns nil when the stream is empty."
  (let ((ts (make-test-ts nil)))
    (assert-null (cl-cc/parse::ts-peek-type ts))))

;;; ─── ts-at-end-p ─────────────────────────────────────────────────────────

(deftest ts-at-end-p-true-for-empty-stream
  "ts-at-end-p returns T when the token list is empty."
  (let ((ts (make-test-ts nil)))
    (assert-true (cl-cc/parse::ts-at-end-p ts))))

(deftest ts-at-end-p-false-when-tokens-remain
  "ts-at-end-p returns NIL when there is at least one token."
  (let* ((tok (make-test-token :T-INT 1))
         (ts  (make-test-ts (list tok))))
    (assert-false (cl-cc/parse::ts-at-end-p ts))))

(deftest ts-at-end-p-true-at-eof-token
  "ts-at-end-p returns T when the head token has type :T-EOF."
  (let* ((eof (make-test-token :T-EOF nil))
         (ts  (make-test-ts (list eof))))
    (assert-true (cl-cc/parse::ts-at-end-p ts))))

;;; ─── ts-token-value ──────────────────────────────────────────────────────

(deftest ts-token-value-returns-value-of-head-token
  "ts-token-value returns the :value field of the current token."
  (let* ((tok (make-test-token :T-INT 99))
         (ts  (make-test-ts (list tok))))
    (assert-= 99 (cl-cc/parse::ts-token-value ts))))

(deftest ts-token-value-nil-when-empty
  "ts-token-value returns nil on an empty stream."
  (let ((ts (make-test-ts nil)))
    (assert-null (cl-cc/parse::ts-token-value ts))))

;;; ─── ts-expect ───────────────────────────────────────────────────────────

(deftest ts-expect-succeeds-on-matching-type
  "ts-expect returns the consumed token when the type matches."
  (let* ((tok (make-test-token :T-INT 5))
         (ts  (make-test-ts (list tok))))
    (let ((result (cl-cc/parse::ts-expect ts :T-INT)))
      (assert-eq tok result)
      ;; Stream is now empty
      (assert-true (cl-cc/parse::ts-at-end-p ts)))))

(deftest ts-expect-adds-diagnostic-on-type-mismatch
  "ts-expect records a parse-error diagnostic when the type doesn't match."
  (let* ((tok (make-test-token :T-IDENT 'x))
         (ts  (make-test-ts (list tok))))
    (cl-cc/parse::ts-expect ts :T-INT "ctx")
    (assert-true (> (length (cl-cc/parse::token-stream-diagnostics ts)) 0))))

(deftest ts-expect-adds-diagnostic-on-empty-stream
  "ts-expect records a diagnostic when the stream is empty."
  (let ((ts (make-test-ts nil)))
    (cl-cc/parse::ts-expect ts :T-INT "end")
    (assert-true (> (length (cl-cc/parse::token-stream-diagnostics ts)) 0))))

;;; ─── %tok-to-cst ─────────────────────────────────────────────────────────

(deftest tok-to-cst-returns-cst-token
  "%tok-to-cst converts a lexer-token to a cst-token."
  (let* ((tok (make-test-token :T-INT 7 0 3))
         (cst (cl-cc/parse::%tok-to-cst tok)))
    (assert-true (cl-cc/parse::cst-token-p cst))
    (assert-eq :T-INT (cl-cc/parse::cst-node-kind cst))
    (assert-= 7 (cl-cc/parse::cst-token-value cst))
    (assert-= 0 (cl-cc/parse::cst-node-start-byte cst))
    (assert-= 3 (cl-cc/parse::cst-node-end-byte cst))))

(deftest tok-to-cst-returns-nil-for-nil-input
  "%tok-to-cst returns nil when token is nil."
  (assert-null (cl-cc/parse::%tok-to-cst nil)))

;;; ─── %make-list-cst ──────────────────────────────────────────────────────

(deftest make-list-cst-creates-cst-interior
  "%make-list-cst creates a cst-interior node with the given kind."
  (let ((node (cl-cc/parse::%make-list-cst :defun '() 0 10)))
    (assert-true (cl-cc/parse::cst-interior-p node))
    (assert-eq :defun (cl-cc/parse::cst-node-kind node))
    (assert-= 0  (cl-cc/parse::cst-node-start-byte node))
    (assert-= 10 (cl-cc/parse::cst-node-end-byte node))))

(deftest make-list-cst-stores-children
  "%make-list-cst stores the children list."
  (let* ((child (cl-cc/parse::%tok-to-cst (make-test-token :T-INT 1)))
         (node  (cl-cc/parse::%make-list-cst :call (list child) 0 5)))
    (assert-= 1 (length (cl-cc/parse::cst-interior-children node)))
    (assert-eq child (first (cl-cc/parse::cst-interior-children node)))))

;;; ─── parse-cl-source ─────────────────────────────────────────────────────

(deftest parse-cl-source-integer-literal
  "parse-cl-source parses an integer literal and returns a CST tree."
  (let ((result (cl-cc/parse::parse-cl-source "42")))
    ;; Result should be a list of CST nodes
    (assert-true (listp result))
    (assert-true (> (length result) 0))))

(deftest parse-cl-source-symbol
  "parse-cl-source parses a bare symbol name."
  (let ((result (cl-cc/parse::parse-cl-source "foo")))
    (assert-true (listp result))
    (assert-true (> (length result) 0))))

(deftest parse-cl-source-empty-string
  "parse-cl-source on empty input returns empty list."
  (let ((result (cl-cc/parse::parse-cl-source "")))
    (assert-true (listp result))))

(deftest parse-cl-source-simple-list
  "parse-cl-source parses a simple parenthesized list."
  (let ((result (cl-cc/parse::parse-cl-source "(+ 1 2)")))
    (assert-true (listp result))
    (assert-true (> (length result) 0))))
