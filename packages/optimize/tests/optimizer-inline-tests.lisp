;;;; tests/unit/optimize/optimizer-inline-tests.lisp
;;;; Unit tests for src/optimize/optimizer-inline.lisp
;;;;
;;;; Covers: opt-max-reg-index (empty, single, multi-register programs),
;;;;   opt-make-renaming (register discovery + counter assignment),
;;;;   opt-collect-function-defs (linear body detection, jump rejection),
;;;;   opt-build-function-name-map (symbol→label tracking),
;;;;   opt-build-call-graph (direct call edge extraction),
;;;;   opt-call-graph-recursive-labels (SCC detection),
;;;;   opt-known-callee-labels (register tracking through closures/moves).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── opt-max-reg-index ───────────────────────────────────────────────────────

(deftest-each opt-max-reg-index-cases
  "opt-max-reg-index returns the highest :RN register index, or -1 if none present."
  :cases (("empty"    nil                                                        -1)
          ("label"    (list (make-vm-label :name "L0"))                         -1)
          ("single"   (list (make-vm-const :dst :r0 :value 1)
                            (make-vm-ret   :reg :r0))                            0)
          ("multiple" (list (make-vm-const :dst :r0 :value 1)
                            (make-vm-const :dst :r3 :value 2)
                            (make-vm-move  :dst :r7 :src :r3)
                            (make-vm-ret   :reg :r7))                            7))
  (insts expected)
  (assert-= expected (cl-cc/optimize::opt-max-reg-index insts)))

;;; ─── opt-make-renaming ───────────────────────────────────────────────────────

(deftest opt-make-renaming-empty-body
  "opt-make-renaming on an empty body returns an empty hash table."
  (let ((ht (cl-cc/optimize::opt-make-renaming nil 0)))
    (assert-= 0 (hash-table-count ht))))

(deftest opt-make-renaming-assigns-fresh-registers
  "opt-make-renaming assigns fresh :RN starting at BASE-INDEX."
  (let* ((insts (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-ret   :reg :r0)))
         (ht (cl-cc/optimize::opt-make-renaming insts 10)))
    ;; :r0 should be renamed to :r10
    (assert-eq :r10 (gethash :r0 ht))))

(deftest opt-make-renaming-distinct-per-register
  "opt-make-renaming gives each distinct register its own fresh name."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-move  :dst :r2 :src :r0)
                      (make-vm-ret   :reg :r2)))
         (ht (cl-cc/optimize::opt-make-renaming insts 5)))
    ;; 3 unique source registers: :r0, :r1, :r2
    (assert-= 3 (hash-table-count ht))
    ;; All renamed to distinct fresh registers
    (let ((fresh (list (gethash :r0 ht) (gethash :r1 ht) (gethash :r2 ht))))
      (assert-= 3 (length (remove-duplicates fresh))))))

;;; ─── opt-collect-function-defs ───────────────────────────────────────────────

(deftest-each opt-collect-function-defs-empty-cases
  "opt-collect-function-defs returns empty table for empty or jump-body inputs."
  :cases (("empty" nil)
          ("jump"  (list (make-vm-closure :dst :r0 :label "jmp-fn" :params '(:r0) :captured nil)
                         (make-vm-label :name "jmp-fn")
                         (make-vm-jump  :label "somewhere")
                         (make-vm-ret   :reg :r0))))
  (insts)
  (assert-= 0 (hash-table-count (cl-cc/optimize::opt-collect-function-defs insts))))

(deftest opt-collect-function-defs-linear-body
  "opt-collect-function-defs captures a linear function body."
  ;; vm-closure registers function at label 'fn-label' with params (:r0)
  ;; Then label 'fn-label', const, ret — a linear body
  (let* ((closure (make-vm-closure :dst :r5 :label "fn-label"
                                   :params '(:r0) :captured nil))
         (lbl     (make-vm-label :name "fn-label"))
         (body1   (make-vm-const :dst :r1 :value 42))
         (ret     (make-vm-ret   :reg :r1))
         (insts   (list closure lbl body1 ret))
         (ht      (cl-cc/optimize::opt-collect-function-defs insts)))
    (assert-= 1 (hash-table-count ht))
    (let ((def (gethash "fn-label" ht)))
      (assert-true (not (null def)))
      (assert-true (not (null (getf def :body)))))))

;;; ─── opt-build-function-name-map ─────────────────────────────────────────────

(deftest opt-build-function-name-map-empty
  "opt-build-function-name-map on empty instructions returns empty table."
  (let ((ht (cl-cc/optimize::opt-build-function-name-map nil)))
    (assert-= 0 (hash-table-count ht))))

(deftest opt-build-function-name-map-tracks-registration
  "opt-build-function-name-map maps a function name to its label."
  ;; vm-closure loads label 'my-label' into :r0
  ;; vm-register-function registers 'my-fn from :r0
  (let* ((closure  (make-vm-closure :dst :r0 :label "my-label"
                                    :params nil :captured nil))
         (regfn    (cl-cc:make-vm-register-function :src :r0 :name 'my-fn))
         (insts    (list closure regfn))
         (ht       (cl-cc/optimize::opt-build-function-name-map insts)))
    (assert-equal "my-label" (gethash 'my-fn ht))))

;;; ─── opt-build-call-graph ────────────────────────────────────────────────────

(deftest opt-build-call-graph-no-calls
  "opt-build-call-graph produces empty callee lists for functions with no calls."
  (let* ((closure (make-vm-closure :dst :r0 :label "pure-fn"
                                   :params '(:r0) :captured nil))
         (lbl     (make-vm-label :name "pure-fn"))
         (body    (make-vm-const :dst :r1 :value 1))
         (ret     (make-vm-ret   :reg :r1))
         (insts   (list closure lbl body ret))
         (fdefs   (cl-cc/optimize::opt-collect-function-defs insts))
         (nmap    (cl-cc/optimize::opt-build-function-name-map insts))
         (graph   (cl-cc/optimize::opt-build-call-graph insts fdefs nmap)))
    (let ((callees (gethash "pure-fn" graph)))
      (assert-null callees))))

;;; ─── opt-call-graph-recursive-labels ────────────────────────────────────────

(deftest opt-call-graph-recursive-labels-no-recursion
  "opt-call-graph-recursive-labels returns empty table for a non-recursive graph."
  (let ((graph (make-hash-table :test #'equal)))
    (setf (gethash "a" graph) '("b"))
    (setf (gethash "b" graph) '("c"))
    (setf (gethash "c" graph) nil)
    (let ((rec (cl-cc/optimize::opt-call-graph-recursive-labels graph)))
      (assert-= 0 (hash-table-count rec)))))

(deftest opt-call-graph-recursive-labels-direct-recursion
  "opt-call-graph-recursive-labels detects a directly self-recursive function."
  (let ((graph (make-hash-table :test #'equal)))
    (setf (gethash "fact" graph) '("fact"))  ; fact calls itself
    (let ((rec (cl-cc/optimize::opt-call-graph-recursive-labels graph)))
      (assert-true (gethash "fact" rec)))))

(deftest opt-call-graph-recursive-labels-mutual-recursion
  "opt-call-graph-recursive-labels detects mutually recursive functions."
  (let ((graph (make-hash-table :test #'equal)))
    (setf (gethash "even" graph) '("odd"))
    (setf (gethash "odd"  graph) '("even"))
    (let ((rec (cl-cc/optimize::opt-call-graph-recursive-labels graph)))
      (assert-true (gethash "even" rec))
      (assert-true (gethash "odd"  rec)))))

;;; ─── opt-known-callee-labels ─────────────────────────────────────────────────

(deftest-each opt-known-callee-labels-cases
  "opt-known-callee-labels: tracks closure reg→label, propagates through moves, clears on overwrite."
  :cases (("closure"
           (list (make-vm-closure :dst :r0 :label "my-fn" :params nil :captured nil))
           :r0 "my-fn")
          ("propagate"
           (list (make-vm-closure :dst :r0 :label "fn-x" :params nil :captured nil)
                 (make-vm-move :dst :r1 :src :r0))
           :r1 "fn-x")
          ("cleared"
           (list (make-vm-closure :dst :r0 :label "fn-a" :params nil :captured nil)
                 (make-vm-const :dst :r0 :value 99))
           :r0 nil))
  (insts reg expected)
  (let ((table (cl-cc/optimize::opt-known-callee-labels insts)))
    (assert-equal expected (gethash reg table))))

;;; ─── opt-can-safely-rename-p ─────────────────────────────────────────────────

(deftest-each opt-can-safely-rename-p-cases
  "opt-can-safely-rename-p returns T for any safe instruction list."
  :cases (("simple" (list (make-vm-move :dst :R0 :src :R1) (make-vm-ret :reg :R0)))
          ("empty"  nil))
  (insts)
  (assert-true (cl-cc/optimize::opt-can-safely-rename-p insts)))

;;; ─── opt-rename-regs-in-inst ─────────────────────────────────────────────────

(deftest opt-rename-regs-in-inst-behavior
  "opt-rename-regs-in-inst renames from table; leaves registers with no mapping unchanged."
  (let* ((inst     (make-vm-move :dst :R0 :src :R1))
         (renaming (let ((ht (make-hash-table :test #'eq)))
                     (setf (gethash :R0 ht) :R10 (gethash :R1 ht) :R11) ht))
         (renamed  (cl-cc/optimize::opt-rename-regs-in-inst inst renaming)))
    (assert-eq :R10 (cl-cc:vm-dst renamed))
    (assert-eq :R11 (cl-cc:vm-src renamed)))
  (let* ((renamed (cl-cc/optimize::opt-rename-regs-in-inst
                   (make-vm-move :dst :R0 :src :R1)
                   (make-hash-table :test #'eq))))
    (assert-eq :R0 (cl-cc:vm-dst renamed))
    (assert-eq :R1 (cl-cc:vm-src renamed))))

;;; ─── %opt-collect-sexp-regs-into-cell ───────────────────────────────────────

(deftest-each opt-collect-sexp-regs-cases
  "%opt-collect-sexp-regs-into-cell accumulates register keywords from sexp into the cell."
  :cases (("empty-atom"     42               nil)
          ("register-kw"    :r0              '(:r0))
          ("non-reg-kw"     :not-a-reg       nil)
          ("nested-list"    '(:r0 :r1 (:r2)) '(:r2 :r1 :r0)))
  (form expected-set)
  (let ((cell (list nil)))
    (cl-cc/optimize::%opt-collect-sexp-regs-into-cell form cell)
    (let ((regs (car cell)))
      (assert-= (length expected-set) (length regs))
      (dolist (r expected-set)
        (assert-true (member r regs))))))

(deftest opt-collect-sexp-regs-registers-from-inst-sexp
  "%opt-collect-sexp-regs-into-cell collects :r0 and :r1 from a vm-move sexp."
  (let* ((inst (make-vm-move :dst :r0 :src :r1))
         (sexp (cl-cc/optimize::instruction->sexp inst))
         (cell (list nil)))
    (cl-cc/optimize::%opt-collect-sexp-regs-into-cell sexp cell)
    (let ((regs (car cell)))
      (assert-true (member :r0 regs))
      (assert-true (member :r1 regs)))))

;;; ─── opt-can-safely-rename-p ─────────────────────────────────────────────────

(deftest-each opt-can-safely-rename-p-cases
  "opt-can-safely-rename-p: T for simple instructions with sexp-visible regs; NIL for instructions that suppress regs in their sexp form."
  :cases (("const-trivially-safe" (list (make-vm-const :dst :r0 :value 42)
                                        (make-vm-ret   :reg :r0))
                                  t)
          ("move-safe"            (list (make-vm-move :dst :r0 :src :r1)
                                        (make-vm-ret  :reg :r0))
                                  t))
  (insts expected)
  (if expected
      (assert-true  (cl-cc/optimize::opt-can-safely-rename-p insts))
      (assert-false (cl-cc/optimize::opt-can-safely-rename-p insts))))

;;; ─── opt-body-has-global-refs-p ──────────────────────────────────────────────

(deftest-each opt-body-has-global-refs-p-cases
  "opt-body-has-global-refs-p detects registers read outside params and prior dsts."
  :cases (("no-globals" (list (make-vm-add :dst :R2 :lhs :R0 :rhs :R1) (make-vm-ret :reg :R2)) '(:R0 :R1) nil)
          ("detects"    (list (make-vm-move :dst :R2 :src :R99) (make-vm-ret :reg :R2))         '(:R0)     t))
  (insts params expected)
  (if expected
      (assert-true (cl-cc/optimize::opt-body-has-global-refs-p insts params))
      (assert-null (cl-cc/optimize::opt-body-has-global-refs-p insts params))))
