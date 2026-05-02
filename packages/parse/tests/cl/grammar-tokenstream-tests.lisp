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

(deftest ts-peek-behavior
  "ts-peek returns first token without consuming it; returns nil on empty stream."
  (let* ((tok (make-test-token :T-INT 42))
         (ts  (make-test-ts (list tok))))
    (assert-eq tok (cl-cc/parse::ts-peek ts))
    (assert-eq tok (cl-cc/parse::ts-peek ts)))
  (assert-null (cl-cc/parse::ts-peek (make-test-ts nil))))

;;; ─── ts-advance ──────────────────────────────────────────────────────────

(deftest ts-advance-behavior
  "ts-advance: consumes and returns first token, advancing stream; nil on empty."
  (let* ((t1  (make-test-token :T-INT 1))
         (t2  (make-test-token :T-INT 2))
         (ts  (make-test-ts (list t1 t2))))
    (let ((consumed (cl-cc/parse::ts-advance ts)))
      (assert-eq t1 consumed)
      (assert-eq t2 (cl-cc/parse::ts-peek ts))))
  (assert-null (cl-cc/parse::ts-advance (make-test-ts nil))))

;;; ─── ts-peek-type ────────────────────────────────────────────────────────

(deftest ts-peek-type-behavior
  "ts-peek-type returns type of head token; returns nil on empty stream."
  (let* ((tok (make-test-token :T-IDENT 'foo))
         (ts  (make-test-ts (list tok))))
    (assert-eq :T-IDENT (cl-cc/parse::ts-peek-type ts)))
  (assert-null (cl-cc/parse::ts-peek-type (make-test-ts nil))))

;;; ─── ts-at-end-p ─────────────────────────────────────────────────────────

(deftest ts-at-end-p-behavior
  "ts-at-end-p: T for empty stream or :T-EOF head; NIL when real tokens remain."
  (assert-true  (cl-cc/parse::ts-at-end-p (make-test-ts nil)))
  (assert-false (cl-cc/parse::ts-at-end-p (make-test-ts (list (make-test-token :T-INT 1)))))
  (assert-true  (cl-cc/parse::ts-at-end-p (make-test-ts (list (make-test-token :T-EOF nil))))))

;;; ─── ts-token-value ──────────────────────────────────────────────────────

(deftest ts-token-value-behavior
  "ts-token-value returns :value of head token; returns nil on empty stream."
  (let* ((tok (make-test-token :T-INT 99))
         (ts  (make-test-ts (list tok))))
    (assert-= 99 (cl-cc/parse::ts-token-value ts)))
  (assert-null (cl-cc/parse::ts-token-value (make-test-ts nil))))

;;; ─── ts-expect ───────────────────────────────────────────────────────────

(deftest ts-expect-behavior
  "ts-expect: returns token on match; records diagnostic on type-mismatch; diagnostic on empty."
  (let* ((tok (make-test-token :T-INT 5))
         (ts  (make-test-ts (list tok))))
    (let ((result (cl-cc/parse::ts-expect ts :T-INT)))
      (assert-eq tok result)
      (assert-true (cl-cc/parse::ts-at-end-p ts))))
  (let* ((tok (make-test-token :T-IDENT 'x))
         (ts  (make-test-ts (list tok))))
    (cl-cc/parse::ts-expect ts :T-INT "ctx")
    (assert-true (> (length (cl-cc/parse::token-stream-diagnostics ts)) 0)))
  (let ((ts (make-test-ts nil)))
    (cl-cc/parse::ts-expect ts :T-INT "end")
    (assert-true (> (length (cl-cc/parse::token-stream-diagnostics ts)) 0))))

;;; ─── %tok-to-cst ─────────────────────────────────────────────────────────

(deftest tok-to-cst-converts-lexer-token
  "%tok-to-cst converts a lexer token to a cst-token preserving kind, value, and byte span."
  (let* ((tok (make-test-token :T-INT 7 0 3))
         (cst (cl-cc/parse::%tok-to-cst tok)))
    (assert-true (cl-cc/parse:cst-token-p cst))
    (assert-eq :T-INT (cl-cc/parse::cst-node-kind cst))
    (assert-= 7 (cl-cc/parse:cst-token-value cst))
    (assert-= 0 (cl-cc/parse:cst-node-start-byte cst))
    (assert-= 3 (cl-cc/parse:cst-node-end-byte cst))))

(deftest tok-to-cst-returns-nil-for-nil-input
  "%tok-to-cst returns nil when given nil (no token)."
  (assert-null (cl-cc/parse::%tok-to-cst nil)))

;;; ─── %make-list-cst ──────────────────────────────────────────────────────

(deftest make-list-cst-creates-interior-with-kind-and-span
  "%make-list-cst creates a cst-interior node with the given kind and byte span."
  (let ((node (cl-cc/parse::%make-list-cst :defun '() 0 10)))
    (assert-true (cl-cc/parse:cst-interior-p node))
    (assert-eq :defun (cl-cc/parse::cst-node-kind node))
    (assert-= 0  (cl-cc/parse:cst-node-start-byte node))
    (assert-= 10 (cl-cc/parse:cst-node-end-byte node))))

(deftest make-list-cst-stores-children
  "%make-list-cst stores and retrieves child nodes by identity."
  (let* ((child (cl-cc/parse::%tok-to-cst (make-test-token :T-INT 1)))
         (node  (cl-cc/parse::%make-list-cst :call (list child) 0 5)))
    (assert-= 1 (length (cl-cc/parse:cst-interior-children node)))
    (assert-eq child (first (cl-cc/parse:cst-interior-children node)))))

;;; ─── parse-cl-source ─────────────────────────────────────────────────────

(deftest parse-cl-source-non-empty-for-valid-forms
  "parse-cl-source: integer, symbol, and list all produce non-empty result lists."
  (dolist (src '("42" "foo" "(+ 1 2)"))
    (let ((result (cl-cc/parse:parse-cl-source src)))
      (assert-true (listp result))
      (assert-true (> (length result) 0)))))

(deftest parse-cl-source-empty-string-returns-list
  "parse-cl-source: empty string returns a list (possibly empty, not an error)."
  (assert-true (listp (cl-cc/parse:parse-cl-source ""))))
