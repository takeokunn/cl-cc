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
  ;; Gap of 5 between uses 2 and 7 is below minimum hole size of 8.
  (let* ((interval (make-spill-test-interval :v 0 10 :use-positions '(2 7)))
         (minimum-hole-size 8))
    (multiple-value-bind (child-groups boundaries)
        (%collect-live-range-splits (list interval) minimum-hole-size)
      ;; One group per original interval; no boundaries when no split occurs.
      (is (= 1 (length child-groups)))
      (is (null boundaries)))))

(deftest "%collect-live-range-splits produces boundary when hole exceeds minimum"
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
  (let* ((int-a (make-spill-test-interval :a 0 10 :use-positions '(1 2)))
         (int-b (make-spill-test-interval :b 5 20 :use-positions '(5 6)))
         (minimum-hole-size 8))
    (multiple-value-bind (child-groups _)
        (%collect-live-range-splits (list int-a int-b) minimum-hole-size)
      (declare (ignore _))
      (is (= 2 (length child-groups))))))

;;; ─── %assign-live-range-split-slots ──────────────────────────────────────

(deftest "%assign-live-range-split-slots assigns sequential 1-indexed slots"
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
  (let ((slot-count (%assign-live-range-split-slots '())))
    (is (= 0 slot-count))))

;;; ─── %boundaries-by-position ──────────────────────────────────────────────

(deftest "%boundaries-by-position indexes boundaries by keyfn result"
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
  (let* ((b1 (make-live-range-split-boundary :after-position 5 :before-position 10
                                              :from-vreg :a :to-vreg :a/s1 :slot 1))
         (b2 (make-live-range-split-boundary :after-position 5 :before-position 15
                                              :from-vreg :b :to-vreg :b/s1 :slot 2))
         (table (%boundaries-by-position (list b1 b2) #'split-boundary-after-position)))
    (is (= 2 (length (gethash 5 table))))))

;;; ─── split-live-ranges ────────────────────────────────────────────────────

(deftest "split-live-ranges no-split path returns originals unchanged"
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

(deftest "%finalize-split-spill-registers rewrites vm-spill-store src to physical"
  (let* ((assignment (make-hash-table :test #'eq))
         (store (make-vm-spill-store :src-reg :v0 :slot 1)))
    (setf (gethash :v0 assignment) :rax)
    (let ((result (%finalize-split-spill-registers (list store) assignment)))
      (is (= 1 (length result)))
      (is (eq :rax (vm-spill-src (first result))))
      (is (= 1 (vm-spill-slot (first result)))))))

(deftest "%finalize-split-spill-registers rewrites vm-spill-load dst to physical"
  (let* ((assignment (make-hash-table :test #'eq))
         (load (make-vm-spill-load :dst-reg :v1 :slot 2)))
    (setf (gethash :v1 assignment) :rbx)
    (let ((result (%finalize-split-spill-registers (list load) assignment)))
      (is (= 1 (length result)))
      (is (eq :rbx (vm-spill-dst (first result))))
      (is (= 2 (vm-spill-slot (first result)))))))

(deftest "%finalize-split-spill-registers passes through non-spill instructions"
  (let* ((assignment (make-hash-table :test #'eq))
         (const-inst (make-vm-const :dst :r0 :value 99)))
    (let ((result (%finalize-split-spill-registers (list const-inst) assignment)))
      (is (= 1 (length result)))
      (is (eq const-inst (first result))))))

(deftest "%finalize-split-spill-registers leaves instruction unchanged when vreg not assigned"
  ;; When a spill instruction's vreg has no physical assignment, it is returned as-is.
  (let* ((assignment (make-hash-table :test #'eq))
         (store (make-vm-spill-store :src-reg :unassigned :slot 3)))
    (let ((result (%finalize-split-spill-registers (list store) assignment)))
      (is (eq store (first result))))))

;;; ─── regalloc-loop-depths ─────────────────────────────────────────────────

(deftest "regalloc-loop-depths returns empty table for straight-line code"
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                              (make-vm-const :dst :r1 :value 2)))
         (depths (regalloc-loop-depths instructions)))
    (is (= 0 (hash-table-count depths)))))

(deftest "regalloc-loop-depths increments depth for backward branch targets"
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
  (let* ((*ml-regalloc-enabled* t)
         (interval (make-spill-test-interval :v 0 10 :use-positions '(1 3 7))))
    (is (> (regalloc-ml-spill-cost interval nil) 0))))

(deftest "regalloc-ml-spill-cost adds bonus for call-crossing interval"
  (let* ((no-call  (make-live-interval :vreg :nc :start 0 :end 10
                                       :use-positions '(2) :crosses-call-p nil
                                       :return-value-p nil))
         (crosses  (make-live-interval :vreg :cc :start 0 :end 10
                                       :use-positions '(2) :crosses-call-p t
                                       :return-value-p nil)))
    (is (> (regalloc-ml-spill-cost crosses nil)
           (regalloc-ml-spill-cost no-call nil)))))

(deftest "regalloc-ml-spill-cost adds bonus for return-value interval"
  (let* ((plain  (make-live-interval :vreg :p :start 0 :end 10
                                     :use-positions '(2) :crosses-call-p nil
                                     :return-value-p nil))
         (retval (make-live-interval :vreg :r :start 0 :end 10
                                     :use-positions '(2) :crosses-call-p nil
                                     :return-value-p t)))
    (is (> (regalloc-ml-spill-cost retval nil)
           (regalloc-ml-spill-cost plain nil)))))

(deftest "regalloc-ml-spill-cost reduces cost for rematerializable const interval"
  (let* ((plain (make-live-interval :vreg :p :start 0 :end 10
                                    :use-positions '(2) :remat-const nil))
         (remat (make-live-interval :vreg :r :start 0 :end 10
                                    :use-positions '(2) :remat-const 42)))
    (is (< (regalloc-ml-spill-cost remat nil)
           (regalloc-ml-spill-cost plain nil)))))

(deftest "regalloc-ml-spill-cost uses loop-depths to weight loop-body uses"
  (let* ((interval (make-spill-test-interval :v 0 10 :use-positions '(5)))
         (depths (let ((ht (make-hash-table :test #'eql)))
                   (setf (gethash 5 ht) 2)
                   ht))
         (shallow-cost (regalloc-ml-spill-cost interval nil))
         (loop-cost    (regalloc-ml-spill-cost interval depths)))
    ;; Use at depth 2 should yield a higher score than use at depth 0.
    (is (> loop-cost shallow-cost))))
