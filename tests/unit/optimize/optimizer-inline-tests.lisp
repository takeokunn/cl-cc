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

(deftest opt-max-reg-index-empty-program
  "opt-max-reg-index returns -1 for an empty instruction list."
  (assert-= -1 (cl-cc::opt-max-reg-index nil)))

(deftest opt-max-reg-index-single-register
  "opt-max-reg-index finds the max N in a program using only :R0."
  (let ((insts (list (make-vm-const :dst :r0 :value 1)
                     (make-vm-ret   :reg :r0))))
    (assert-= 0 (cl-cc::opt-max-reg-index insts))))

(deftest opt-max-reg-index-multiple-registers
  "opt-max-reg-index finds the highest index across all instructions."
  (let ((insts (list (make-vm-const :dst :r0 :value 1)
                     (make-vm-const :dst :r3 :value 2)
                     (make-vm-move  :dst :r7 :src :r3)
                     (make-vm-ret   :reg :r7))))
    (assert-= 7 (cl-cc::opt-max-reg-index insts))))

(deftest opt-max-reg-index-no-register-insts
  "opt-max-reg-index returns -1 when no instructions use :RN registers."
  (let ((insts (list (make-vm-label :name "L0"))))
    (assert-= -1 (cl-cc::opt-max-reg-index insts))))

;;; ─── opt-make-renaming ───────────────────────────────────────────────────────

(deftest opt-make-renaming-empty-body
  "opt-make-renaming on an empty body returns an empty hash table."
  (let ((ht (cl-cc::opt-make-renaming nil 0)))
    (assert-= 0 (hash-table-count ht))))

(deftest opt-make-renaming-assigns-fresh-registers
  "opt-make-renaming assigns fresh :RN starting at BASE-INDEX."
  (let* ((insts (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-ret   :reg :r0)))
         (ht (cl-cc::opt-make-renaming insts 10)))
    ;; :r0 should be renamed to :r10
    (assert-eq :r10 (gethash :r0 ht))))

(deftest opt-make-renaming-distinct-per-register
  "opt-make-renaming gives each distinct register its own fresh name."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-move  :dst :r2 :src :r0)
                      (make-vm-ret   :reg :r2)))
         (ht (cl-cc::opt-make-renaming insts 5)))
    ;; 3 unique source registers: :r0, :r1, :r2
    (assert-= 3 (hash-table-count ht))
    ;; All renamed to distinct fresh registers
    (let ((fresh (list (gethash :r0 ht) (gethash :r1 ht) (gethash :r2 ht))))
      (assert-= 3 (length (remove-duplicates fresh))))))

;;; ─── opt-collect-function-defs ───────────────────────────────────────────────

(deftest opt-collect-function-defs-empty
  "opt-collect-function-defs on empty instructions returns empty table."
  (let ((ht (cl-cc::opt-collect-function-defs nil)))
    (assert-= 0 (hash-table-count ht))))

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
         (ht      (cl-cc::opt-collect-function-defs insts)))
    (assert-= 1 (hash-table-count ht))
    (let ((def (gethash "fn-label" ht)))
      (assert-true (not (null def)))
      (assert-true (not (null (getf def :body)))))))

(deftest opt-collect-function-defs-rejects-jump-body
  "opt-collect-function-defs excludes functions whose bodies contain jumps."
  (let* ((closure (make-vm-closure :dst :r0 :label "jmp-fn"
                                   :params '(:r0) :captured nil))
         (lbl     (make-vm-label :name "jmp-fn"))
         (jmp     (make-vm-jump  :label "somewhere"))
         (ret     (make-vm-ret   :reg :r0))
         (insts   (list closure lbl jmp ret))
         (ht      (cl-cc::opt-collect-function-defs insts)))
    ;; Jump inside body → not collected
    (assert-= 0 (hash-table-count ht))))

;;; ─── opt-build-function-name-map ─────────────────────────────────────────────

(deftest opt-build-function-name-map-empty
  "opt-build-function-name-map on empty instructions returns empty table."
  (let ((ht (cl-cc::opt-build-function-name-map nil)))
    (assert-= 0 (hash-table-count ht))))

(deftest opt-build-function-name-map-tracks-registration
  "opt-build-function-name-map maps a function name to its label."
  ;; vm-closure loads label 'my-label' into :r0
  ;; vm-register-function registers 'my-fn from :r0
  (let* ((closure  (make-vm-closure :dst :r0 :label "my-label"
                                    :params nil :captured nil))
         (regfn    (cl-cc::make-vm-register-function :src :r0 :name 'my-fn))
         (insts    (list closure regfn))
         (ht       (cl-cc::opt-build-function-name-map insts)))
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
         (fdefs   (cl-cc::opt-collect-function-defs insts))
         (nmap    (cl-cc::opt-build-function-name-map insts))
         (graph   (cl-cc::opt-build-call-graph insts fdefs nmap)))
    (let ((callees (gethash "pure-fn" graph)))
      (assert-null callees))))

;;; ─── opt-call-graph-recursive-labels ────────────────────────────────────────

(deftest opt-call-graph-recursive-labels-no-recursion
  "opt-call-graph-recursive-labels returns empty table for a non-recursive graph."
  (let ((graph (make-hash-table :test #'equal)))
    (setf (gethash "a" graph) '("b"))
    (setf (gethash "b" graph) '("c"))
    (setf (gethash "c" graph) nil)
    (let ((rec (cl-cc::opt-call-graph-recursive-labels graph)))
      (assert-= 0 (hash-table-count rec)))))

(deftest opt-call-graph-recursive-labels-direct-recursion
  "opt-call-graph-recursive-labels detects a directly self-recursive function."
  (let ((graph (make-hash-table :test #'equal)))
    (setf (gethash "fact" graph) '("fact"))  ; fact calls itself
    (let ((rec (cl-cc::opt-call-graph-recursive-labels graph)))
      (assert-true (gethash "fact" rec)))))

(deftest opt-call-graph-recursive-labels-mutual-recursion
  "opt-call-graph-recursive-labels detects mutually recursive functions."
  (let ((graph (make-hash-table :test #'equal)))
    (setf (gethash "even" graph) '("odd"))
    (setf (gethash "odd"  graph) '("even"))
    (let ((rec (cl-cc::opt-call-graph-recursive-labels graph)))
      (assert-true (gethash "even" rec))
      (assert-true (gethash "odd"  rec)))))

;;; ─── opt-known-callee-labels ─────────────────────────────────────────────────

(deftest opt-known-callee-labels-tracks-closure
  "opt-known-callee-labels records reg→label for vm-closure instructions."
  (let* ((closure (make-vm-closure :dst :r0 :label "my-fn"
                                   :params nil :captured nil))
         (insts   (list closure))
         (table   (cl-cc::opt-known-callee-labels insts)))
    (assert-equal "my-fn" (gethash :r0 table))))

(deftest opt-known-callee-labels-propagates-through-move
  "opt-known-callee-labels propagates label tracking through vm-move."
  (let* ((closure (make-vm-closure :dst :r0 :label "fn-x"
                                   :params nil :captured nil))
         (mv      (make-vm-move :dst :r1 :src :r0))
         (insts   (list closure mv))
         (table   (cl-cc::opt-known-callee-labels insts)))
    (assert-equal "fn-x" (gethash :r1 table))))

(deftest opt-known-callee-labels-clears-on-overwrite
  "opt-known-callee-labels clears a register when it gets a new definition."
  (let* ((closure (make-vm-closure :dst :r0 :label "fn-a"
                                   :params nil :captured nil))
         (overwrite (make-vm-const :dst :r0 :value 99))
         (insts   (list closure overwrite))
         (table   (cl-cc::opt-known-callee-labels insts)))
    ;; After being overwritten by vm-const, :r0 should no longer track "fn-a"
    (assert-null (gethash :r0 table))))
