(in-package :cl-cc/regalloc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Tests for regalloc-color.lisp — Graph Coloring Allocation (FR-061)
;;;                                  and Spill Slot Sharing (FR-199)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── helpers ──────────────────────────────────────────────────────────────

(defun make-test-interval (vreg start end &key use-positions crosses-call-p return-value-p)
  "Construct a live-interval for use in tests."
  (make-live-interval :vreg vreg
                      :start start
                      :end end
                      :use-positions (or use-positions (list start end))
                      :crosses-call-p crosses-call-p
                      :return-value-p return-value-p))

;;; ─── %intervals-overlap-p ─────────────────────────────────────────────────

(deftest-each "%intervals-overlap-p overlap"
  (vreg-a start-a end-a vreg-b start-b end-b expected-p)
  :cases
  ((:exact-overlap  :a 0 10 :b 0 10 t)
   (:partial-left   :a 0  8 :b 5 12 t)
   (:partial-right  :a 5 12 :b 0  8 t)
   (:contained      :a 0 20 :b 5 10 t)
   (:touching-end   :a 0 10 :b 10 20 t)
   (:touching-start :a 10 20 :b 0 10 t)
   (:disjoint-ab    :a 0  5 :b 6 10 nil)
   (:disjoint-ba    :a 6 10 :b 0  5 nil))
  (let ((a (make-test-interval vreg-a start-a end-a))
        (b (make-test-interval vreg-b start-b end-b)))
    (is (eq expected-p (%intervals-overlap-p a b)))))

;;; ─── %color-build-interference-graph ─────────────────────────────────────

(deftest "color-build-interference-graph-overlapping"
  (let* ((a (make-test-interval :a 0 10))
         (b (make-test-interval :b 5 15))
         (graph (%color-build-interference-graph (list a b))))
    (is (member :b (gethash :a graph) :test #'eq))
    (is (member :a (gethash :b graph) :test #'eq))))

(deftest "color-build-interference-graph-disjoint"
  (let* ((a (make-test-interval :a 0 5))
         (b (make-test-interval :b 6 10))
         (graph (%color-build-interference-graph (list a b))))
    (is (null (gethash :a graph)))
    (is (null (gethash :b graph)))))

(deftest "color-build-interference-graph-no-vreg-skipped"
  ;; Intervals without a vreg must be ignored.
  (let* ((a (make-test-interval nil 0 10))
         (b (make-test-interval :b 0 10))
         (graph (%color-build-interference-graph (list a b))))
    (is (= 1 (hash-table-count graph)))
    (is (null (gethash nil graph)))))

;;; ─── %color-simplify ──────────────────────────────────────────────────────

(deftest "color-simplify-stack-ordering"
  ;; With two overlapping intervals and k=2 (enough registers), neither node
  ;; should be marked as a potential spill (cdr entry = NIL).
  (let* ((a (make-test-interval :a 0 10))
         (b (make-test-interval :b 5 15))
         (graph (%color-build-interference-graph (list a b)))
         (imap (%interval-map (list a b)))
         (stack (%color-simplify graph imap 2)))
    (is (= 2 (length stack)))
    ;; All entries should have spill-p = NIL (degree < k=2 after simplification).
    (is (every (lambda (entry) (not (cdr entry))) stack))))

(deftest "color-simplify-spill-candidate-when-pressure"
  ;; Three mutually interfering intervals but only k=2 colors forces a spill.
  (let* ((a (make-test-interval :a 0 20 :use-positions '(0 5 10 15)))
         (b (make-test-interval :b 0 20 :use-positions '(1 6 11 16)))
         (c (make-test-interval :c 0 20 :use-positions '(2)))
         (graph (%color-build-interference-graph (list a b c)))
         (imap (%interval-map (list a b c)))
         (stack (%color-simplify graph imap 2)))
    (is (= 3 (length stack)))
    ;; At least one node on the stack should be a potential spill (cdr = T).
    (is (some #'cdr stack))))

;;; ─── color-allocate ───────────────────────────────────────────────────────

(deftest "color-allocate-assigns-registers"
  (let* ((a (make-test-interval :a 0 10))
         (b (make-test-interval :b 12 20))
         (available '(:r0 :r1)))
    (multiple-value-bind (assignment spill-map spill-count)
        (color-allocate (list a b) available)
      (is (gethash :a assignment))
      (is (gethash :b assignment))
      (is (= 0 (hash-table-count spill-map)))
      (is (= 0 spill-count)))))

(deftest "color-allocate-spills-when-k-exceeded"
  ;; Three mutually interfering intervals, only 2 physical registers.
  (let* ((a (make-test-interval :a 0 20))
         (b (make-test-interval :b 0 20))
         (c (make-test-interval :c 0 20))
         (available '(:r0 :r1)))
    (multiple-value-bind (assignment spill-map spill-count)
        (color-allocate (list a b c) available)
      (declare (ignore assignment))
      (is (= 1 (hash-table-count spill-map)))
      (is (>= spill-count 1)))))

(deftest "color-allocate-empty-intervals"
  (multiple-value-bind (assignment spill-map spill-count)
      (color-allocate '() '(:r0 :r1))
    (is (= 0 (hash-table-count assignment)))
    (is (= 0 (hash-table-count spill-map)))
    (is (= 0 spill-count))))

;;; ─── color-spill-slots ────────────────────────────────────────────────────

(deftest "color-spill-slots-non-overlapping-share-slot"
  ;; Two non-overlapping spilled intervals should share the same stack slot.
  (let* ((a (make-test-interval :a 0 5))
         (b (make-test-interval :b 6 10))
         (color-map (color-spill-slots (list a b) 0)))
    (is (= (gethash :a color-map) (gethash :b color-map)))))

(deftest "color-spill-slots-overlapping-use-distinct-slots"
  ;; Two overlapping spilled intervals must get distinct slots.
  (let* ((a (make-test-interval :a 0 10))
         (b (make-test-interval :b 5 15))
         (color-map (color-spill-slots (list a b) 0)))
    (is (/= (gethash :a color-map) (gethash :b color-map)))))

(deftest "color-spill-slots-offset-respected"
  ;; Slot offset of 3 means minimum slot number is 4 (1-origin within the offset).
  (let* ((a (make-test-interval :a 0 5))
         (color-map (color-spill-slots (list a) 3)))
    (is (>= (gethash :a color-map) 4))))

;;; ─── regalloc-color-spill-slots ───────────────────────────────────────────

(deftest "regalloc-color-spill-slots-passthrough-when-empty"
  ;; No spilled intervals: original spill-map returned unchanged.
  (let* ((a (make-test-interval :a 0 10))
         (original-map (make-hash-table :test #'eq))
         (result (regalloc-color-spill-slots (list a) original-map 0)))
    (is (eq original-map result))))

(deftest "regalloc-color-spill-slots-applies-coloring"
  ;; Two spilled non-overlapping intervals: coloring should collapse them to
  ;; a single shared slot, so the resulting map has both vregs pointing to
  ;; the same slot number.
  (let* ((a (make-test-interval :a 0 5))
         (b (make-test-interval :b 6 10))
         (spill-map (let ((ht (make-hash-table :test #'eq)))
                      (setf (gethash :a ht) 1
                            (gethash :b ht) 2)
                      ht))
         (result (regalloc-color-spill-slots (list a b) spill-map 0)))
    (is (= (gethash :a result) (gethash :b result)))))

;;; ─── %color-spill-priority ────────────────────────────────────────────────

(deftest-each "%color-spill-priority ml-disabled"
  (start end use-positions crosses-call return-value expected)
  :cases
  ((:uses-length-weighted-formula  0 10 '(1 2 3 4 5) nil nil 1/2)
   (:adds-call-cross-bonus          0 10 '(1)         t   nil 11/10)
   (:adds-return-value-bonus        0 10 '(1)         nil t   11/10)
   (:length-clamp-at-one            5  5 '(5)         nil nil 1))
  ;; With *ml-regalloc-enabled* nil the result is uses/length + call/return bonuses.
  (let* ((*ml-regalloc-enabled* nil)
         (interval (make-test-interval :v start end
                                       :use-positions use-positions
                                       :crosses-call-p crosses-call
                                       :return-value-p return-value))
         (score (%color-spill-priority interval)))
    (is (= score expected))))

;;; ─── %color-spill-candidate ───────────────────────────────────────────────

(deftest "%color-spill-candidate picks lowest-priority interval"
  ;; :low has more uses/length (higher score) so :high should be chosen as
  ;; the spill candidate (lower priority = cheaper to spill).
  (let* ((*ml-regalloc-enabled* nil)
         ;; :low-cost: 1 use over 20 positions → score ≈ 0.05
         (low  (make-test-interval :low-cost  0 20 :use-positions '(1)))
         ;; :high-cost: 10 uses over 10 positions → score = 1.0
         (high (make-test-interval :high-cost 0 10 :use-positions '(0 1 2 3 4 5 6 7 8 9 10)))
         (graph (%color-build-interference-graph (list low high)))
         (imap  (%interval-map (list low high)))
         (candidate (%color-spill-candidate graph imap)))
    (is (eq :low-cost candidate))))

(deftest "%color-spill-candidate tie-breaks by name order"
  ;; Equal priority: :aaa should come before :zzz alphabetically.
  (let* ((*ml-regalloc-enabled* nil)
         (a (make-test-interval :aaa 0 10 :use-positions '(0)))
         (z (make-test-interval :zzz 0 10 :use-positions '(0)))
         (graph (%color-build-interference-graph (list a z)))
         (imap  (%interval-map (list a z)))
         (candidate (%color-spill-candidate graph imap)))
    (is (eq :aaa candidate))))

;;; ─── %color-select-register ───────────────────────────────────────────────

(deftest "%color-select-register picks first non-conflicting register"
  (let* ((a      (make-test-interval :a 0 10))
         (b      (make-test-interval :b 0 10))
         (graph  (%color-build-interference-graph (list a b)))
         (assign (make-hash-table :test #'eq)))
    ;; Color :b with :r0; :a should receive :r1.
    (setf (gethash :b assign) :r0)
    (let ((result (%color-select-register :a graph assign '(:r0 :r1))))
      (is (eq :r1 result)))))

(deftest "%color-select-register returns nil when all registers used by neighbors"
  (let* ((a      (make-test-interval :a 0 10))
         (b      (make-test-interval :b 0 10))
         (graph  (%color-build-interference-graph (list a b)))
         (assign (make-hash-table :test #'eq)))
    ;; Only one register, already given to :b.
    (setf (gethash :b assign) :r0)
    (let ((result (%color-select-register :a graph assign '(:r0))))
      (is (null result)))))

;;; ─── %color-ordered-registers-for-interval ────────────────────────────────

(deftest "%color-ordered-registers-for-interval no-cc returns list unchanged"
  (let* ((interval (make-test-interval :v 0 10))
         (regs     '(:r0 :r1 :r2))
         (result   (%color-ordered-registers-for-interval interval nil regs)))
    ;; cc=nil means no preference can be computed.
    (is (equal regs result))))

;;; ─── %color-assign-spill-slots ────────────────────────────────────────────

(deftest "%color-assign-spill-slots assigns increasing slots for overlapping intervals"
  ;; Two overlapping intervals must not share a slot; both must receive
  ;; a slot >= offset+1, and the max returned must equal the highest slot.
  (let* ((a (make-test-interval :a 0 10))
         (b (make-test-interval :b 5 15)))
    (multiple-value-bind (spill-map max-slot)
        (%color-assign-spill-slots (list a b) 0)
      (is (/= (gethash :a spill-map) (gethash :b spill-map)))
      (is (>= (gethash :a spill-map) 1))
      (is (>= (gethash :b spill-map) 1))
      (is (= max-slot (max (gethash :a spill-map) (gethash :b spill-map)))))))

(deftest "%color-assign-spill-slots non-overlapping intervals share a slot"
  ;; Non-overlapping intervals should collapse to one shared slot.
  (let* ((a (make-test-interval :a 0 5))
         (b (make-test-interval :b 6 10)))
    (multiple-value-bind (spill-map _)
        (%color-assign-spill-slots (list a b) 0)
      (declare (ignore _))
      (is (= (gethash :a spill-map) (gethash :b spill-map))))))

(deftest "%color-assign-spill-slots respects spill-slot-offset"
  (let* ((a (make-test-interval :a 0 5)))
    (multiple-value-bind (spill-map max-slot)
        (%color-assign-spill-slots (list a) 4)
      (is (>= (gethash :a spill-map) 5))
      (is (= max-slot (gethash :a spill-map))))))

;;; ─── %interval-map ────────────────────────────────────────────────────────

(deftest "%interval-map builds vreg->interval hash correctly"
  (let* ((a   (make-test-interval :a 0 10))
         (b   (make-test-interval :b 5 15))
         (imap (%interval-map (list a b))))
    (is (eq a (gethash :a imap)))
    (is (eq b (gethash :b imap)))
    (is (= 2 (hash-table-count imap)))))

(deftest "%interval-map skips nil-vreg intervals"
  (let* ((no-vreg (make-test-interval nil 0 10))
         (named   (make-test-interval :v  0 10))
         (imap    (%interval-map (list no-vreg named))))
    (is (= 1 (hash-table-count imap)))
    (is (eq named (gethash :v imap)))))

;;; ─── %copy-hash-into ──────────────────────────────────────────────────────

(deftest "%copy-hash-into copies all entries from source to destination"
  (let ((from (make-hash-table :test #'eq))
        (to   (make-hash-table :test #'eq)))
    (setf (gethash :a from) 1
          (gethash :b from) 2)
    (%copy-hash-into from to)
    (is (= 1 (gethash :a to)))
    (is (= 2 (gethash :b to)))))

(deftest "%copy-hash-into returns the destination table"
  (let ((from (make-hash-table :test #'eq))
        (to   (make-hash-table :test #'eq)))
    (setf (gethash :x from) :y)
    (let ((result (%copy-hash-into from to)))
      (is (eq to result)))))

(deftest "%copy-hash-into does not mutate source"
  (let ((from (make-hash-table :test #'eq))
        (to   (make-hash-table :test #'eq)))
    (setf (gethash :k from) 99)
    (%copy-hash-into from to)
    (is (= 1 (hash-table-count from)))))

;;; ─── %spill-weight ────────────────────────────────────────────────────────

(deftest-each "%spill-weight scoring"
  (vreg start end use-positions crosses-call return-value expected)
  :cases
  ((:plain        0 10 '(1 2)   nil nil 2)
   (:call-cross   0 10 '(1)     t   nil 2)
   (:return-val   0 10 '(1)     nil t   2)
   (:call+return  0 10 '(1)     t   t   3)
   (:no-uses      0 10 '()      nil nil 0))
  (let ((interval (make-test-interval vreg start end
                                      :use-positions use-positions
                                      :crosses-call-p crosses-call
                                      :return-value-p return-value)))
    (is (= expected (%spill-weight interval)))))

;;; ─── %build-spill-interference-matrix ────────────────────────────────────

(deftest "%build-spill-interference-matrix overlapping intervals interfere"
  (let* ((a (make-test-interval :a 0 10))
         (b (make-test-interval :b 5 15))
         (matrix (%build-spill-interference-matrix (list a b))))
    (is (member :b (gethash :a matrix) :test #'eq))
    (is (member :a (gethash :b matrix) :test #'eq))))

(deftest "%build-spill-interference-matrix non-overlapping intervals do not interfere"
  (let* ((a (make-test-interval :a 0 5))
         (b (make-test-interval :b 6 10))
         (matrix (%build-spill-interference-matrix (list a b))))
    (is (null (gethash :a matrix)))
    (is (null (gethash :b matrix)))))

(deftest "%build-spill-interference-matrix symmetric edges"
  ;; The matrix must be undirected: if a→b then b→a.
  (let* ((a (make-test-interval :a 0 10))
         (b (make-test-interval :b 3  8))
         (c (make-test-interval :c 0 10))
         (matrix (%build-spill-interference-matrix (list a b c))))
    (dolist (vreg '(:a :b :c))
      (dolist (neighbor (gethash vreg matrix))
        (is (member vreg (gethash neighbor matrix) :test #'eq))))))

;;; ─── color-allocate-for-target (GPR+FP class split) ──────────────────────

(deftest "color-allocate-for-target separates gpr and fp classes"
  ;; One GPR and one FP interval.  With enough registers for each class both
  ;; should be assigned a physical register with no spills.
  (let* ((gpr-int (make-live-interval :vreg :gpr :start 0 :end 10
                                      :use-positions '(0 10)
                                      :crosses-call-p nil
                                      :return-value-p nil
                                      :fp-p nil))
         (fp-int  (make-live-interval :vreg :fpr :start 0 :end 10
                                      :use-positions '(0 10)
                                      :crosses-call-p nil
                                      :return-value-p nil
                                      :fp-p t))
         ;; Use an x86-64 target so regalloc-target-fp-registers returns the
         ;; standard xmm pool and target-allocatable-regs derives from gpr-names.
         (cc      (make-target-desc
                    :name        :x86-64
                    :gpr-names   #(:rdi :rsi :rdx :rcx :r8 :r9 :rbx :r12)
                    :arg-regs    '(:rdi :rsi :rdx :rcx :r8 :r9)
                    :ret-reg     :rax
                    :fp-arg-regs '(:xmm0 :xmm1 :xmm2 :xmm3)
                    :fp-ret-reg  :xmm0
                    :callee-saved '(:rbx :r12)
                    :scratch-regs nil)))
    (multiple-value-bind (assignment spill-map spill-count)
        (color-allocate-for-target (list gpr-int fp-int) cc)
      (is (gethash :gpr assignment))
      (is (gethash :fpr assignment))
      (is (= 0 (hash-table-count spill-map)))
      (is (= 0 spill-count)))))
