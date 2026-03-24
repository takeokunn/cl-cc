;;;; tests/unit/parse/cst-tests.lisp — CST node unit tests
;;;;
;;;; Tests: node creation, predicates, cst-walk, cst-to-sexp, sexp-to-cst
;;;; roundtrip, and sexp-head-to-kind.

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; ─── Node Creation ──────────────────────────────────────────────────────────

(deftest cst-token-creation
  "cst-token: create and read back fields"
  (let ((tok (cl-cc:make-cst-token :kind :int :value 42 :start-byte 0 :end-byte 2)))
    (assert-eq :int (cl-cc:cst-node-kind tok))
    (assert-= 42 (cl-cc:cst-token-value tok))
    (assert-= 0 (cl-cc:cst-node-start-byte tok))
    (assert-= 2 (cl-cc:cst-node-end-byte tok))))

(deftest cst-interior-creation
  "cst-interior: create with children list"
  (let* ((child1 (cl-cc:make-cst-token :kind :int :value 1))
         (child2 (cl-cc:make-cst-token :kind :int :value 2))
         (node (cl-cc:make-cst-interior :kind :list :children (list child1 child2))))
    (assert-eq :list (cl-cc:cst-node-kind node))
    (assert-= 2 (length (cl-cc:cst-children node)))))

(deftest cst-error-node-creation
  "cst-error-node: create with message"
  (let ((err (cl-cc:make-cst-error-node :message "bad syntax" :start-byte 5 :end-byte 10)))
    (assert-string= "bad syntax" (cl-cc:cst-error-node-message err))
    (assert-= 5 (cl-cc:cst-node-start-byte err))))

;;; ─── Predicates ─────────────────────────────────────────────────────────────

(deftest cst-token-p-true
  "cst-token-p returns true for tokens"
  (assert-true (cl-cc:cst-token-p (cl-cc:make-cst-token :kind :int :value 0))))

(deftest cst-token-p-false
  "cst-token-p returns false for interior nodes"
  (assert-false (cl-cc:cst-token-p (cl-cc:make-cst-interior :kind :list))))

(deftest cst-interior-p-true
  "cst-interior-p returns true for interior nodes"
  (assert-true (cl-cc:cst-interior-p (cl-cc:make-cst-interior :kind :list))))

(deftest cst-error-p-true
  "cst-error-p returns true for error nodes"
  (assert-true (cl-cc:cst-error-p (cl-cc:make-cst-error-node :message "err"))))

;;; ─── cst-child ──────────────────────────────────────────────────────────────

(deftest cst-child-access
  "cst-child returns the Nth child"
  (let* ((c0 (cl-cc:make-cst-token :kind :int :value 10))
         (c1 (cl-cc:make-cst-token :kind :int :value 20))
         (node (cl-cc:make-cst-interior :kind :list :children (list c0 c1))))
    (assert-= 10 (cl-cc:cst-token-value (cl-cc:cst-child node 0)))
    (assert-= 20 (cl-cc:cst-token-value (cl-cc:cst-child node 1)))))

;;; ─── cst-walk ───────────────────────────────────────────────────────────────

(deftest cst-walk-visits-all
  "cst-walk visits all nodes in pre-order"
  (let* ((leaf1 (cl-cc:make-cst-token :kind :int :value 1))
         (leaf2 (cl-cc:make-cst-token :kind :int :value 2))
         (inner (cl-cc:make-cst-interior :kind :list :children (list leaf1 leaf2)))
         (visited nil))
    (cl-cc:cst-walk inner (lambda (n) (push n visited)))
    (assert-= 3 (length visited))
    ;; Pre-order: inner first
    (assert-true (cl-cc:cst-interior-p (car (last visited))))))

(deftest cst-walk-leaf-only
  "cst-walk on a leaf calls fn exactly once"
  (let ((count 0)
        (leaf (cl-cc:make-cst-token :kind :int :value 42)))
    (cl-cc:cst-walk leaf (lambda (n) (declare (ignore n)) (incf count)))
    (assert-= 1 count)))

;;; ─── cst-collect-errors ─────────────────────────────────────────────────────

(deftest cst-collect-errors-finds-errors
  "cst-collect-errors returns error nodes from a tree"
  (let* ((good (cl-cc:make-cst-token :kind :int :value 1))
         (bad (cl-cc:make-cst-error-node :message "oops"))
         (tree (cl-cc:make-cst-interior :kind :list :children (list good bad))))
    (assert-= 1 (length (cl-cc:cst-collect-errors tree)))))

;;; ─── cst-to-sexp ────────────────────────────────────────────────────────────

(deftest cst-to-sexp-token
  "cst-to-sexp on a token returns its value"
  (assert-= 42 (cl-cc:cst-to-sexp (cl-cc:make-cst-token :kind :int :value 42))))

(deftest cst-to-sexp-list
  "cst-to-sexp on an interior :list node returns a list"
  (let* ((c1 (cl-cc:make-cst-token :kind :int :value 1))
         (c2 (cl-cc:make-cst-token :kind :int :value 2))
         (node (cl-cc:make-cst-interior :kind :list :children (list c1 c2))))
    (assert-equal '(1 2) (cl-cc:cst-to-sexp node))))

(deftest cst-to-sexp-quote
  "cst-to-sexp on a :quote node returns (quote x)"
  (let* ((inner (cl-cc:make-cst-token :kind :var :value 'foo))
         (node (cl-cc:make-cst-interior :kind :quote :children (list inner))))
    (assert-equal '(quote foo) (cl-cc:cst-to-sexp node))))

(deftest cst-to-sexp-vector
  "cst-to-sexp on a :vector node returns a CL vector"
  (let* ((c1 (cl-cc:make-cst-token :kind :int :value 1))
         (c2 (cl-cc:make-cst-token :kind :int :value 2))
         (node (cl-cc:make-cst-interior :kind :vector :children (list c1 c2))))
    (let ((result (cl-cc:cst-to-sexp node)))
      (assert-true (vectorp result))
      (assert-= 2 (length result)))))

;;; ─── sexp-to-cst ────────────────────────────────────────────────────────────

(deftest sexp-to-cst-integer
  "sexp-to-cst on an integer produces :int token"
  (let ((node (cl-cc:sexp-to-cst 42)))
    (assert-true (cl-cc:cst-token-p node))
    (assert-eq :int (cl-cc:cst-node-kind node))
    (assert-= 42 (cl-cc:cst-token-value node))))

(deftest sexp-to-cst-string
  "sexp-to-cst on a string produces :string token"
  (let ((node (cl-cc:sexp-to-cst "hello")))
    (assert-true (cl-cc:cst-token-p node))
    (assert-eq :string (cl-cc:cst-node-kind node))))

(deftest sexp-to-cst-nil
  "sexp-to-cst on nil produces :nil token"
  (let ((node (cl-cc:sexp-to-cst nil)))
    (assert-true (cl-cc:cst-token-p node))
    (assert-eq :nil (cl-cc:cst-node-kind node))))

(deftest sexp-to-cst-keyword
  "sexp-to-cst on a keyword produces :keyword token"
  (let ((node (cl-cc:sexp-to-cst :foo)))
    (assert-true (cl-cc:cst-token-p node))
    (assert-eq :keyword (cl-cc:cst-node-kind node))))

;;; ─── Roundtrip ──────────────────────────────────────────────────────────────

(deftest-each cst-roundtrip
  "sexp-to-cst -> cst-to-sexp roundtrips correctly"
  :cases (("integer"  42)
          ("string"   "hello")
          ("symbol"   'foo)
          ("keyword"  :bar)
          ("t"        t)
          ("list"     '(+ 1 2))
          ("nested"   '(if (> x 0) x (- x)))
          ("quote"    '(quote a)))
  (form)
  (assert-equal form (cl-cc:cst-to-sexp (cl-cc:sexp-to-cst form))))

;;; ─── sexp-head-to-kind ──────────────────────────────────────────────────────

(deftest-each cst-head-to-kind
  "sexp-head-to-kind maps head symbols to CST kinds"
  :cases (("defun"   'defun    :defun)
          ("let"     'let      :let)
          ("if"      'if       :if)
          ("lambda"  'lambda   :lambda)
          ("progn"   'progn    :progn)
          ("unknown" 'my-func  :call))
  (sym expected-kind)
  (assert-eq expected-kind (cl-cc:sexp-head-to-kind sym)))
