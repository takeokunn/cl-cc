(in-package :cl-cc/regalloc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Tests for regalloc-spill.lisp — Live-Range Splitting and Spill Insertion
;;;
;;; Covers: split-live-ranges (no-split, single-split, multi-split),
;;;         %collect-live-range-splits, %assign-live-range-split-slots,
;;;         %boundaries-by-position, %finalize-split-spill-registers,
;;;         regalloc-ml-spill-cost, regalloc-loop-depths
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── helpers ──────────────────────────────────────────────────────────────

(defun make-spill-test-interval (vreg start end &key use-positions fp-p)
  "Construct a live-interval for spill tests."
  (make-live-interval :vreg vreg
                      :start start
                      :end end
                      :use-positions (or use-positions (list start end))
                      :fp-p fp-p))

;;; ─── %collect-live-range-splits ───────────────────────────────────────────

(deftest "%collect-live-range-splits no splits when hole is smaller than minimum"
  "%collect-live-range-splits produces no boundaries when the gap between uses is smaller than the minimum hole size."
  ;; Gap of 5 between uses 2 and 7 is below minimum hole size of 8.
  (let* ((interval (make-spill-test-interval :v 0 10 :use-positions '(2 7)))
         (minimum-hole-size 8))
    (multiple-value-bind (child-groups boundaries)
        (%collect-live-range-splits (list interval) minimum-hole-size)
      ;; One group per original interval; no boundaries when no split occurs.
      (is (= 1 (length child-groups)))
      (is (null boundaries)))))

(deftest "%collect-live-range-splits produces boundary when hole exceeds minimum"
  "%collect-live-range-splits emits a split boundary when the use gap exceeds the minimum hole size."
  ;; Gap of 15 between uses 2 and 17 exceeds minimum of 8.
  (let* ((interval (make-spill-test-interval :v 0 20 :use-positions '(2 17)))
         (minimum-hole-size 8))
    (multiple-value-bind (child-groups boundaries)
        (%collect-live-range-splits (list interval) minimum-hole-size)
      (is (= 1 (length child-groups)))
      ;; Exactly one boundary should be emitted for the single split.
      (is (= 1 (length boundaries)))
      (is (= 2 (split-boundary-after-position (first boundaries))))
      (is (= 17 (split-boundary-before-position (first boundaries)))))))

(deftest "%collect-live-range-splits multiple intervals produce separate groups"
  "%collect-live-range-splits produces one child group per input interval."
  (let* ((int-a (make-spill-test-interval :a 0 10 :use-positions '(1 2)))
         (int-b (make-spill-test-interval :b 5 20 :use-positions '(5 6)))
         (minimum-hole-size 8))
    (multiple-value-bind (child-groups _)
        (%collect-live-range-splits (list int-a int-b) minimum-hole-size)
      (declare (ignore _))
      (is (= 2 (length child-groups))))))

;;; ─── %assign-live-range-split-slots ──────────────────────────────────────

(deftest "%assign-live-range-split-slots assigns sequential 1-indexed slots"
  "%assign-live-range-split-slots assigns sequential 1-indexed slot numbers to split boundaries."
  (let* ((interval (make-spill-test-interval :v 0 30 :use-positions '(2 20)))
         (minimum-hole-size 8))
    (multiple-value-bind (child-groups boundaries)
        (%collect-live-range-splits (list interval) minimum-hole-size)
      (declare (ignore child-groups))
      (let ((slot-count (%assign-live-range-split-slots boundaries)))
        ;; One boundary → one slot assigned; count == 1.
        (when (plusp (length boundaries))
          (is (= 1 (split-boundary-slot (first boundaries))))
          (is (= 1 slot-count)))))))

(deftest "%assign-live-range-split-slots returns zero when no boundaries"
  "%assign-live-range-split-slots returns zero when the boundary list is empty."
  (let ((slot-count (%assign-live-range-split-slots '())))
    (is (= 0 slot-count))))

;;; ─── %boundaries-by-position ──────────────────────────────────────────────

(deftest "%boundaries-by-position indexes boundaries by keyfn result"
  "%boundaries-by-position builds a hash table indexing each boundary by the value of the key function."
  (let* ((b1 (make-live-range-split-boundary :after-position 3 :before-position 10
                                              :from-vreg :a :to-vreg :a/split1
                                              :slot 1))
         (b2 (make-live-range-split-boundary :after-position 5 :before-position 12
                                              :from-vreg :b :to-vreg :b/split1
                                              :slot 2))
         (table (%boundaries-by-position (list b1 b2) #'split-boundary-after-position)))
    (is (member b1 (gethash 3 table) :test #'eq))
    (is (member b2 (gethash 5 table) :test #'eq))
    (is (null (gethash 99 table)))))

(deftest "%boundaries-by-position groups multiple boundaries at same position"
  "%boundaries-by-position collects all boundaries that share the same key value into a single list."
  (let* ((b1 (make-live-range-split-boundary :after-position 5 :before-position 10
                                              :from-vreg :a :to-vreg :a/s1 :slot 1))
         (b2 (make-live-range-split-boundary :after-position 5 :before-position 15
                                              :from-vreg :b :to-vreg :b/s1 :slot 2))
         (table (%boundaries-by-position (list b1 b2) #'split-boundary-after-position)))
    (is (= 2 (length (gethash 5 table))))))

;;; ─── split-live-ranges ────────────────────────────────────────────────────

(deftest "split-live-ranges no-split path returns originals unchanged"
  "split-live-ranges returns the original instructions and intervals unchanged when no holes are large enough to split."
  ;; All uses are adjacent; no holes exceed minimum.
  (let* ((interval (make-spill-test-interval :v 0 5 :use-positions '(1 2 3 4)))
         (instructions (list (make-vm-const :dst :v :value 42)))
         (minimum-hole-size 8))
    (multiple-value-bind (new-instructions new-intervals split-count new-float)
        (split-live-ranges instructions (list interval) nil minimum-hole-size)
      (is (equal instructions new-instructions))
      (is (= 1 (length new-intervals)))
      (is (= 0 split-count))
      (is (null new-float)))))

(deftest "split-live-ranges single-split path inserts spill load and store"
  "split-live-ranges inserts vm-spill-load and vm-spill-store instructions when a live range is split."
  ;; Uses at 0 and 20 with a 20-unit gap trigger a split.
  ;; The split-live-ranges function should insert vm-spill-load before
  ;; position 20 and vm-spill-store after position 0.
  (let* ((interval (make-spill-test-interval :v 0 25
                     :use-positions '(0 20)))
         ;; Build an instruction stream with 25 no-op const instructions
         ;; at positions 0..24.
         (instructions (loop for i from 0 to 24
                             collect (make-vm-const :dst (intern (format nil "X~D" i) :keyword)
                                                    :value i)))
         (minimum-hole-size 8))
    (multiple-value-bind (new-instructions _intervals split-count _float)
        (split-live-ranges instructions (list interval) nil minimum-hole-size)
      (declare (ignore _intervals _float))
      ;; At least one spill load and one spill store should be inserted.
      (is (plusp split-count))
      (is (some (lambda (inst) (typep inst 'vm-spill-load)) new-instructions))
      (is (some (lambda (inst) (typep inst 'vm-spill-store)) new-instructions)))))

;;; ─── %finalize-split-spill-registers ─────────────────────────────────────

(deftest-each %finalize-split-spill-registers-cases
  "%finalize-split-spill-registers rewrites virtual regs to physical or passes non-spill through."
  :cases (("store-rewrite"    :store :v0 :rax 1)
          ("load-rewrite"     :load  :v1 :rbx 2)
          ("passthrough"      :const nil nil   nil)
          ("unassigned-store" :store :unassigned nil 3))
  (kind vreg phys slot)
  (let* ((assignment (make-hash-table :test #'eq))
         (inst (cond
                 ((eq kind :store) (make-vm-spill-store :src-reg vreg :slot slot))
                 ((eq kind :load)  (make-vm-spill-load  :dst-reg vreg :slot slot))
                 (t                (make-vm-const :dst :r0 :value 99)))))
    (when phys (setf (gethash vreg assignment) phys))
    (let ((result (%finalize-split-spill-registers (list inst) assignment)))
      (assert-equal 1 (length result))
      (cond
        ((and (eq kind :store) phys)
         (assert-true (eq phys  (vm-spill-src (first result))))
         (assert-equal slot (vm-spill-slot (first result))))
        ((and (eq kind :load) phys)
         (assert-true (eq phys  (vm-spill-dst (first result))))
         (assert-equal slot (vm-spill-slot (first result))))
        (t
         (assert-true (eq inst (first result))))))))

;;; ─── regalloc-loop-depths ─────────────────────────────────────────────────

(deftest "regalloc-loop-depths returns empty table for straight-line code"
  "regalloc-loop-depths returns an empty hash table when the instruction stream contains no backward branches."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                              (make-vm-const :dst :r1 :value 2)))
         (depths (regalloc-loop-depths instructions)))
    (is (= 0 (hash-table-count depths)))))

(deftest "regalloc-loop-depths increments depth for backward branch targets"
  "regalloc-loop-depths assigns a loop depth of at least 1 to positions covered by a backward branch."
  ;; A backward branch from position 3 to label at position 1 should mark
  ;; positions 1, 2, 3 with depth >= 1.
  (let* ((instructions (list (make-vm-const :dst :r0 :value 0)         ; 0
                              (make-vm-label :name "loop-head")         ; 1
                              (make-vm-const :dst :r1 :value 1)         ; 2
                              (make-vm-jump :label-name "loop-head")))  ; 3
         (depths (regalloc-loop-depths instructions)))
    (is (>= (gethash 1 depths 0) 1))
    (is (>= (gethash 2 depths 0) 1))
    (is (>= (gethash 3 depths 0) 1))))

;;; ─── regalloc-ml-spill-cost ───────────────────────────────────────────────

(deftest "regalloc-ml-spill-cost returns positive score for non-trivial interval"
  "regalloc-ml-spill-cost returns a positive score for an interval with multiple use positions."
  (let* ((*ml-regalloc-enabled* t)
         (interval (make-spill-test-interval :v 0 10 :use-positions '(1 3 7))))
    (is (> (regalloc-ml-spill-cost interval nil) 0))))

(deftest "regalloc-ml-spill-cost adds bonus for call-crossing interval"
  "regalloc-ml-spill-cost produces a higher cost for an interval that crosses a call than one that does not."
  (let* ((no-call  (make-live-interval :vreg :nc :start 0 :end 10
                                       :use-positions '(2) :crosses-call-p nil
                                       :return-value-p nil))
         (crosses  (make-live-interval :vreg :cc :start 0 :end 10
                                       :use-positions '(2) :crosses-call-p t
                                       :return-value-p nil)))
    (is (> (regalloc-ml-spill-cost crosses nil)
           (regalloc-ml-spill-cost no-call nil)))))

(deftest "regalloc-ml-spill-cost adds bonus for return-value interval"
  "regalloc-ml-spill-cost produces a higher cost for a return-value interval than a plain interval."
  (let* ((plain  (make-live-interval :vreg :p :start 0 :end 10
                                     :use-positions '(2) :crosses-call-p nil
                                     :return-value-p nil))
         (retval (make-live-interval :vreg :r :start 0 :end 10
                                     :use-positions '(2) :crosses-call-p nil
                                     :return-value-p t)))
    (is (> (regalloc-ml-spill-cost retval nil)
           (regalloc-ml-spill-cost plain nil)))))

(deftest "regalloc-ml-spill-cost reduces cost for rematerializable const interval"
  "regalloc-ml-spill-cost produces a lower cost for an interval with a remat-const than a plain interval."
  (let* ((plain (make-live-interval :vreg :p :start 0 :end 10
                                    :use-positions '(2) :remat-const nil))
         (remat (make-live-interval :vreg :r :start 0 :end 10
                                    :use-positions '(2) :remat-const 42)))
    (is (< (regalloc-ml-spill-cost remat nil)
           (regalloc-ml-spill-cost plain nil)))))

(deftest "regalloc-ml-spill-cost uses loop-depths to weight loop-body uses"
  "regalloc-ml-spill-cost produces a higher cost for uses inside a loop than uses at depth 0."
  (let* ((interval (make-spill-test-interval :v 0 10 :use-positions '(5)))
         (depths (let ((ht (make-hash-table :test #'eql)))
                   (setf (gethash 5 ht) 2)
                   ht))
         (shallow-cost (regalloc-ml-spill-cost interval nil))
         (loop-cost    (regalloc-ml-spill-cost interval depths)))
    ;; Use at depth 2 should yield a higher score than use at depth 0.
    (is (> loop-cost shallow-cost))))
