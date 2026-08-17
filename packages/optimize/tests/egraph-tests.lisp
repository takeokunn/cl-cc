;;;; tests/unit/optimize/egraph-tests.lisp — E-Graph Equality Saturation Tests
;;;
;;; Tests for Phase 2: e-graph data structures, union-find, saturation,
;;; extraction, and rewrite rules.

(in-package :cl-cc/test)




;;; ─── Helpers ─────────────────────────────────────────────────────────────

(defun make-test-egraph ()
  "Create a fresh empty e-graph."
  (cl-cc/optimize:make-e-graph))

(defun egraph-find-dst-inst (instructions dst)
  "Return the first instruction in INSTRUCTIONS whose :dst register equals DST."
  (find dst instructions :key #'cl-cc/vm::vm-dst))

;;; ─── Union-Find ──────────────────────────────────────────────────────────

(it-sequential "egraph-find-self"
  (let* ((eg  (make-test-egraph))
         (id  (cl-cc/optimize:egraph-add eg 'const)))
    (expect (= id (cl-cc/optimize:egraph-find eg id)) :to-be-truthy)))

(it-sequential "egraph-add-id-identity deduplicates"
  (destructuring-bind (op1 op2 expect-equal) (list 'const 'const t)
    (let* ((eg  (make-test-egraph))
         (id0 (cl-cc/optimize:egraph-add eg op1))
         (id1 (cl-cc/optimize:egraph-add eg op2)))
    (if expect-equal
        (expect (= id0 id1) :to-be-truthy)
        (expect (/= id0 id1) :to-be-truthy)))))

(it-sequential "egraph-add-id-identity different"
  (destructuring-bind (op1 op2 expect-equal) (list 'const 'var nil)
    (let* ((eg  (make-test-egraph))
         (id0 (cl-cc/optimize:egraph-add eg op1))
         (id1 (cl-cc/optimize:egraph-add eg op2)))
    (if expect-equal
        (expect (= id0 id1) :to-be-truthy)
        (expect (/= id0 id1) :to-be-truthy)))))

;;; ─── Merge ───────────────────────────────────────────────────────────────

(it-sequential "egraph-merge-same-class"
  (let* ((eg (make-test-egraph))
         (id (cl-cc/optimize:egraph-add eg 'const)))
    (cl-cc/optimize:egraph-merge eg id id)
    (expect (= id (cl-cc/optimize:egraph-find eg id)) :to-be-truthy)))

(it-sequential "egraph-merge-two-classes"
  (let* ((eg  (make-test-egraph))
         (id0 (cl-cc/optimize:egraph-add eg 'const))
         (id1 (cl-cc/optimize:egraph-add eg 'var)))
    (cl-cc/optimize:egraph-merge eg id0 id1)
    (cl-cc/optimize:egraph-rebuild eg)
    (expect (= (cl-cc/optimize:egraph-find eg id0) (cl-cc/optimize:egraph-find eg id1)) :to-be-truthy)))

;;; ─── E-Graph Statistics ──────────────────────────────────────────────────

(it-sequential "egraph-stats-class-count empty"
  (destructuring-bind (node-op expected-classes) (list nil 0)
    (let ((eg (make-test-egraph)))
    (when node-op (cl-cc/optimize:egraph-add eg node-op))
    (expect (= expected-classes (getf (cl-cc/optimize:egraph-stats eg) :classes)) :to-be-truthy))))

(it-sequential "egraph-stats-class-count one-node"
  (destructuring-bind (node-op expected-classes) (list 'const 1)
    (let ((eg (make-test-egraph)))
    (when node-op (cl-cc/optimize:egraph-add eg node-op))
    (expect (= expected-classes (getf (cl-cc/optimize:egraph-stats eg) :classes)) :to-be-truthy))))

;;; ─── Pattern Matching ────────────────────────────────────────────────────

(it-sequential "egraph-pattern-var-p var-x"
  (destructuring-bind (expected sym) (list t '?x)
    (if expected
      (expect (cl-cc/optimize:egraph-pattern-var-p sym) :to-be-truthy)
      (expect (cl-cc/optimize:egraph-pattern-var-p sym) :to-be-falsy))))

(it-sequential "egraph-pattern-var-p var-foo"
  (destructuring-bind (expected sym) (list t '?foo)
    (if expected
      (expect (cl-cc/optimize:egraph-pattern-var-p sym) :to-be-truthy)
      (expect (cl-cc/optimize:egraph-pattern-var-p sym) :to-be-falsy))))

(it-sequential "egraph-pattern-var-p no-prefix"
  (destructuring-bind (expected sym) (list nil 'x)
    (if expected
      (expect (cl-cc/optimize:egraph-pattern-var-p sym) :to-be-truthy)
      (expect (cl-cc/optimize:egraph-pattern-var-p sym) :to-be-falsy))))

(it-sequential "egraph-pattern-var-p number"
  (destructuring-bind (expected sym) (list nil 42)
    (if expected
      (expect (cl-cc/optimize:egraph-pattern-var-p sym) :to-be-truthy)
      (expect (cl-cc/optimize:egraph-pattern-var-p sym) :to-be-falsy))))

(it-sequential "egraph-match-pattern-variable"
  (let* ((eg  (make-test-egraph))
         (id  (cl-cc/optimize:egraph-add eg 'const))
         (matches (cl-cc/optimize:egraph-match-pattern eg '?x id)))
    (expect (= 1 (length matches)) :to-be-truthy)
    (expect (= id (cdr (assoc '?x (car matches)))) :to-be-truthy)))

(it-sequential "egraph-match-pattern-consistent-binding"
  (let* ((eg  (make-test-egraph))
         (id0 (cl-cc/optimize:egraph-add eg 'const))
         (id1 (cl-cc/optimize:egraph-add eg 'var))
         ;; Bind ?x to id0, then try to match ?x against id1 (inconsistent)
         (bindings (list (cons '?x id0)))
         (matches (cl-cc/optimize:egraph-match-pattern eg '?x id1 bindings)))
    ;; Should fail — ?x is already bound to id0, not id1
    (expect matches :to-be-null)))

;;; ─── Rule Application ────────────────────────────────────────────────────

(it-sequential "egraph-rule-registered add-zero-r"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::add-zero-r)
    (let ((rules (cl-cc/optimize:egraph-builtin-rules)))
    (expect (find rule-name rules
                        :key (lambda (r) (getf r :name))) :to-be-truthy))))

(it-sequential "egraph-rule-registered fold-add"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::fold-add)
    (let ((rules (cl-cc/optimize:egraph-builtin-rules)))
    (expect (find rule-name rules
                        :key (lambda (r) (getf r :name))) :to-be-truthy))))

(it-sequential "egraph-rule-registered mul-pow2"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::mul-pow2)
    (let ((rules (cl-cc/optimize:egraph-builtin-rules)))
    (expect (find rule-name rules
                        :key (lambda (r) (getf r :name))) :to-be-truthy))))


(it-sequential "egraph-builtin-rules-consults-prolog-facts"
  (let ((called nil))
    (with-replaced-function (cl-prolog-kit:query-prolog
                             (lambda (rulebase goal)
                               (declare (ignore rulebase))
                               (setf called goal)
                               (list (list (cons 'cl-cc/optimize::?name 'cl-cc/optimize::fold-add)
                                           (cons 'cl-cc/optimize::?lhs '(add (const ?a) (const ?b)))
                                           (cons 'cl-cc/optimize::?rhs '(const))))))
      (let ((rules (cl-cc/optimize:egraph-builtin-rules)))
        (expect called :to-be-truthy)
        (let ((rule (find 'cl-cc/optimize::fold-add rules
                          :key (lambda (r) (getf r :name)))))
          (expect rule :to-be-truthy)
          (expect (getf rule :lhs) :to-equal '(add (const ?a) (const ?b)))
          (expect (getf rule :rhs) :to-equal '(const)))))))
