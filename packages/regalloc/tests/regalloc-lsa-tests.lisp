(in-package :cl-cc/regalloc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Tests for regalloc-allocate.lisp — Linear Scan Allocator Internals
;;;
;;; Covers: %lsa-try-coalesce, %lsa-assign, %lsa-allocate-from-pool,
;;;         %lsa-evict-and-assign
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── helpers ──────────────────────────────────────────────────────────────

(defun make-lsa-test-interval (vreg start end &key fp-p coalesce-with
                                               use-positions crosses-call-p)
  "Construct a live-interval for linear-scan allocator tests."
  (make-live-interval :vreg vreg
                      :start start
                      :end end
                      :fp-p fp-p
                      :coalesce-with coalesce-with
                      :use-positions (or use-positions (list start end))
                      :crosses-call-p crosses-call-p))

(defun make-test-lsa-state (&key (free-regs '(:r0 :r1 :r2))
                                  (free-fp-regs '(:xmm0 :xmm1))
                                  active)
  "Construct a minimal lsa-state for testing."
  (make-lsa-state :free-regs free-regs
                  :free-fp-regs free-fp-regs
                  :active (or active nil)))

;;; ─── %lsa-assign ──────────────────────────────────────────────────────────

(deftest "%lsa-assign records assignment and inserts into active list"
  "%lsa-assign sets the interval's physical register and adds it to the active list."
  (let* ((state (make-test-lsa-state))
         (interval (make-lsa-test-interval :v 0 10)))
    (%lsa-assign state interval :r0)
    (is (eq :r0 (interval-phys-reg interval)))
    (is (eq :r0 (gethash :v (lsa-assignment state))))
    (is (member interval (lsa-active state) :test #'eq))))

(deftest "%lsa-assign keeps active list sorted by interval end"
  "%lsa-assign maintains the active list in ascending order of interval end positions."
  (let* ((state (make-test-lsa-state))
         (int-a (make-lsa-test-interval :a 0 20))
         (int-b (make-lsa-test-interval :b 0  5))
         (int-c (make-lsa-test-interval :c 0 12)))
    (%lsa-assign state int-a :r0)
    (%lsa-assign state int-b :r1)
    (%lsa-assign state int-c :r2)
    ;; After insertion the active list should be ordered by interval-end ascending.
    (let ((ends (mapcar #'interval-end (lsa-active state))))
      (is (equal ends (sort (copy-list ends) #'<))))))

;;; ─── %lsa-try-coalesce ────────────────────────────────────────────────────

(deftest "%lsa-try-coalesce succeeds when source ends exactly at current start"
  "%lsa-try-coalesce assigns the source's register to the new interval when the source ends exactly at the new interval's start."
  ;; Source interval ends at 5; new interval starts at 5 → coalesce allowed.
  (let* ((state (make-test-lsa-state :free-regs '(:r0 :r1)))
         (src-int (make-lsa-test-interval :src 0 5))
         (new-int (make-lsa-test-interval :new 5 10 :coalesce-with :src)))
    ;; Manually assign :r0 to the source and mark it active.
    (setf (interval-phys-reg src-int) :r0)
    (setf (gethash :src (lsa-interval-map state)) src-int)
    (setf (lsa-active state) (list src-int))
    (let ((result (%lsa-try-coalesce state new-int)))
      (is (eq t result))
      (is (eq :r0 (interval-phys-reg new-int)))
      ;; Source should have been removed from active and replaced by new-int.
      (is (not (member src-int (lsa-active state) :test #'eq)))
      (is (member new-int (lsa-active state) :test #'eq)))))

(deftest-each "%lsa-try-coalesce fails (returns nil)"
  (src-end new-start new-fp-p coalesce-with setup-src-p)
  ((:source-still-live   10 5  nil :src t)
   (:no-coalesce-hint     5 5  nil nil  nil)
   (:fp-class-mismatch    5 5  t   :src t))
  (let* ((state   (make-test-lsa-state))
         (src-int (make-lsa-test-interval :src 0 src-end :fp-p nil))
         (new-int (make-lsa-test-interval :new new-start 15
                                          :coalesce-with coalesce-with
                                          :fp-p new-fp-p)))
    (when setup-src-p
      (setf (interval-phys-reg src-int) :r0)
      (setf (gethash :src (lsa-interval-map state)) src-int)
      (setf (lsa-active state) (list src-int)))
    (is (null (%lsa-try-coalesce state new-int)))))

;;; ─── %lsa-allocate-from-pool ──────────────────────────────────────────────

(deftest "%lsa-allocate-from-pool assigns register and removes it from pool"
  "%lsa-allocate-from-pool assigns a physical register to the interval and removes it from the free register pool."
  (let* ((state    (make-test-lsa-state :free-regs '(:r0 :r1 :r2)))
         (interval (make-lsa-test-interval :v 0 10))
         (cc       (make-target-desc
                     :name        :x86-64
                     :gpr-names   #(:rdi :rsi :rdx :rcx)
                     :arg-regs    '(:rdi :rsi)
                     :ret-reg     :rax
                     :fp-arg-regs '(:xmm0)
                     :fp-ret-reg  :xmm0
                     :callee-saved '()
                     :scratch-regs nil)))
    (%lsa-allocate-from-pool state interval cc '(:r0 :r1 :r2))
    (is (not (null (interval-phys-reg interval))))
    (is (member (interval-phys-reg interval) '(:r0 :r1 :r2) :test #'eq))
    (is (not (member (interval-phys-reg interval)
                     (lsa-free-regs state) :test #'eq)))))

;;; ─── %lsa-evict-and-assign ────────────────────────────────────────────────

(deftest "%lsa-evict-and-assign spills current when it is worst candidate"
  "%lsa-evict-and-assign spills the current interval when it is the worst eviction candidate."
  ;; When *ml-regalloc-enabled* is nil and interval has no active candidates
  ;; with a farther next use, the current interval itself is spilled.
  (let* ((state (make-test-lsa-state :free-regs nil))
         (interval (make-lsa-test-interval :v 0 10 :use-positions '(2))))
    (setf (lsa-active state) nil)
    (let ((*ml-regalloc-enabled* nil))
      (%lsa-evict-and-assign state interval))
    ;; The interval should be spilled (spill-slot set, count bumped).
    (is (not (null (interval-spill-slot interval))))
    (is (> (lsa-spill-count state) 0))))

(deftest "%lsa-evict-and-assign frees candidate register and assigns to interval"
  "%lsa-evict-and-assign evicts the active candidate and assigns its freed register to the new interval."
  ;; Active candidate has a nearer next use than the new interval.
  ;; The new interval should evict it and steal its register.
  (let* ((state (make-test-lsa-state :free-regs nil))
         ;; Candidate: next use at position 3 (near).
         (candidate (make-lsa-test-interval :cand 0 20 :use-positions '(3)))
         ;; New interval: next use at position 15 (far from current start 5).
         (interval  (make-lsa-test-interval :new  5 25 :use-positions '(15))))
    (setf (interval-phys-reg candidate) :r0)
    (setf (gethash :cand (lsa-assignment state)) :r0)
    (setf (lsa-active state) (list candidate))
    (let ((*ml-regalloc-enabled* nil))
      (%lsa-evict-and-assign state interval))
    ;; Candidate should now be spilled.
    (is (not (null (interval-spill-slot candidate))))
    (is (null (interval-phys-reg candidate)))
    ;; New interval should have been assigned the freed register.
    (is (eq :r0 (interval-phys-reg interval)))))

;;; ─── %lsa-expire-old ──────────────────────────────────────────────────────

(deftest "%lsa-expire-old removes intervals that end before current start"
  "%lsa-expire-old removes from the active list any interval whose end is before the current interval's start."
  ;; Two intervals in active: one ends at 3 (before current start 5), one ends at 10.
  (let* ((expired  (make-lsa-test-interval :expired  0  3))
         (alive    (make-lsa-test-interval :alive    0 10))
         (current  (make-lsa-test-interval :curr     5 15))
         (state    (make-test-lsa-state :free-regs '())))
    (setf (interval-phys-reg expired) :r0)
    (setf (interval-phys-reg alive)   :r1)
    (setf (lsa-active state) (list expired alive))
    (%lsa-expire-old state current)
    ;; expired should have been removed from active
    (is (not (member expired (lsa-active state) :test #'eq)))
    ;; alive should still be in active
    (is (member alive (lsa-active state) :test #'eq))))

(deftest "%lsa-expire-old returns register of expired interval to pool"
  "%lsa-expire-old returns the physical register of an expired interval to the free register pool."
  ;; After expiry the freed physical register must be back in free-regs.
  (let* ((expired (make-lsa-test-interval :e 0 3))
         (current (make-lsa-test-interval :c 5 15))
         (state   (make-test-lsa-state :free-regs nil)))
    (setf (interval-phys-reg expired) :r7)
    (setf (lsa-active state) (list expired))
    (%lsa-expire-old state current)
    (is (member :r7 (lsa-free-regs state) :test #'eq))))

(deftest "%lsa-expire-old does not expire intervals ending at exactly current start"
  "%lsa-expire-old does not remove an interval whose end equals the current interval's start position."
  ;; Linear scan rule: an interval ending at I is still live at I (not expired).
  (let* ((touching (make-lsa-test-interval :t 0 5))
         (current  (make-lsa-test-interval :c 5 10))
         (state    (make-test-lsa-state :free-regs nil)))
    (setf (interval-phys-reg touching) :r0)
    (setf (lsa-active state) (list touching))
    (%lsa-expire-old state current)
    ;; The interval ends at 5; current starts at 5.  5 < 5 is false so it
    ;; should NOT be expired.
    (is (member touching (lsa-active state) :test #'eq))))

(deftest "%lsa-expire-old returns fp register to fp pool for fp intervals"
  "%lsa-expire-old returns an expired FP interval's register to the FP free pool, not the GPR pool."
  ;; FP intervals must be returned to free-fp-regs, not free-regs.
  (let* ((fp-expired (make-lsa-test-interval :fpe 0 2 :fp-p t))
         (current    (make-lsa-test-interval :c   5 10))
         (state      (make-test-lsa-state :free-regs nil :free-fp-regs nil)))
    (setf (interval-phys-reg fp-expired) :xmm5)
    (setf (lsa-active state) (list fp-expired))
    (%lsa-expire-old state current)
    (is (member :xmm5 (lsa-free-fp-regs state) :test #'eq))
    (is (not (member :xmm5 (lsa-free-regs state) :test #'eq)))))

;;; ─── %lsa-spill-current ───────────────────────────────────────────────────

(deftest "%lsa-spill-current sets spill-slot and increments count"
  "%lsa-spill-current assigns a spill slot to the interval and increments the spill count."
  (let* ((state    (make-test-lsa-state))
         (interval (make-lsa-test-interval :v 0 10))
         (before   (lsa-spill-count state)))
    (%lsa-spill-current state interval)
    (is (not (null (interval-spill-slot interval))))
    (is (> (lsa-spill-count state) before))))

(deftest "%lsa-spill-current records vreg in spill-map"
  "%lsa-spill-current registers the spilled interval's vreg in the lsa-state spill map."
  (let* ((state    (make-test-lsa-state))
         (interval (make-lsa-test-interval :myvreg 0 10)))
    (%lsa-spill-current state interval)
    (is (gethash :myvreg (lsa-spill-map state)))))

(deftest "%lsa-spill-current assigns distinct slots for two intervals"
  "%lsa-spill-current assigns a different spill slot to each successive spilled interval."
  (let* ((state (make-test-lsa-state))
         (int-a (make-lsa-test-interval :a 0 10))
         (int-b (make-lsa-test-interval :b 5 15)))
    (%lsa-spill-current state int-a)
    (%lsa-spill-current state int-b)
    (is (/= (interval-spill-slot int-a) (interval-spill-slot int-b)))))

;;; ─── %lsa-best-spill-candidate ────────────────────────────────────────────

(deftest "%lsa-best-spill-candidate ml-disabled: returns interval with farthest next use"
  "%lsa-best-spill-candidate selects the active interval with the farthest next use as the eviction target."
  ;; Candidate has next use at 20 (farther than interval's next use at 8).
  ;; So the candidate should win (it should be evicted, not the interval).
  (let* ((candidate (make-lsa-test-interval :cand 0 30 :use-positions '(20)))
         (interval  (make-lsa-test-interval :new  5 25 :use-positions '(8)))
         (state     (make-test-lsa-state :free-regs nil)))
    (setf (lsa-active state) (list candidate))
    (let* ((*ml-regalloc-enabled* nil)
           (best (%lsa-best-spill-candidate state interval)))
      (is (eq candidate best)))))

(deftest "%lsa-best-spill-candidate ml-disabled: returns self when no active with farther use"
  "%lsa-best-spill-candidate returns the current interval itself when no active interval has a farther next use."
  ;; Active candidate has next use at 6 (closer than interval at 20).
  ;; interval itself should be chosen as spill victim.
  (let* ((candidate (make-lsa-test-interval :cand 0 30 :use-positions '(6)))
         (interval  (make-lsa-test-interval :new  5 25 :use-positions '(20)))
         (state     (make-test-lsa-state :free-regs nil)))
    (setf (lsa-active state) (list candidate))
    (let* ((*ml-regalloc-enabled* nil)
           (best (%lsa-best-spill-candidate state interval)))
      (is (eq interval best)))))

(deftest "%lsa-best-spill-candidate ml-enabled: returns lowest-cost interval"
  "%lsa-best-spill-candidate in ML mode selects the interval with the lowest ML spill cost."
  ;; ML mode uses regalloc-ml-spill-cost.  The interval with a remat-const
  ;; has negative cost adjustment (-6), making it cheaper to spill.
  (let* ((no-remat  (make-lsa-test-interval :nr 0 10 :use-positions '(1 2 3)))
         (with-remat (make-lsa-test-interval :wr 0 10 :use-positions '(1 2 3)))
         (interval   (make-lsa-test-interval :new 5 15 :use-positions '(6)))
         (state      (make-test-lsa-state :free-regs nil)))
    (setf (interval-remat-const with-remat) 42)
    (setf (lsa-active state) (list no-remat with-remat))
    (let* ((*ml-regalloc-enabled* t)
           (best (%lsa-best-spill-candidate state interval)))
      ;; with-remat has lower ML cost (remat-const bonus = -6) so it should
      ;; be the preferred spill candidate.
      (is (eq with-remat best)))))

(deftest "%lsa-best-spill-candidate ignores cross-class active intervals"
  "%lsa-best-spill-candidate only considers active intervals of the same register class as the new interval."
  ;; Only GP intervals should compete with a GP new interval.
  (let* ((fp-cand (make-lsa-test-interval :fp 0 30 :fp-p t :use-positions '(20)))
         (interval (make-lsa-test-interval :new 5 25 :fp-p nil :use-positions '(8)))
         (state    (make-test-lsa-state :free-regs nil)))
    (setf (lsa-active state) (list fp-cand))
    (let* ((*ml-regalloc-enabled* nil)
           (best (%lsa-best-spill-candidate state interval)))
      ;; fp-cand is filtered out; interval is the only candidate → returns itself.
      (is (eq interval best)))))

;;; ─── %interval-next-use-after ─────────────────────────────────────────────

(deftest-each "%interval-next-use-after"
  (use-positions position expected)
  ((:finds-first-after   '(2 5 8 12)  4   5)
   (:returns-nil-none    '(1 2 3)     5   nil)
   (:exact-boundary      '(5 10)      4   5)
   (:nil-on-equal-pos    '(5 10)      5   10)
   (:empty-list          '()          0   nil))
  (let ((interval (make-lsa-test-interval :v 0 20 :use-positions use-positions)))
    (is (equal expected (%interval-next-use-after interval position)))))

;;; ─── %lsa-interval-pool and %lsa-set-interval-pool ───────────────────────

(deftest-each "%lsa-interval-pool"
  (fp-p free-regs free-fp-regs expected)
  ((:gpr-returns-free-regs    nil '(:r0 :r1) '(:xmm0)       '(:r0 :r1))
   (:fp-returns-free-fp-regs  t   '(:r0)     '(:xmm0 :xmm1) '(:xmm0 :xmm1)))
  (let* ((state    (make-test-lsa-state :free-regs free-regs :free-fp-regs free-fp-regs))
         (interval (make-lsa-test-interval :v 0 10 :fp-p fp-p)))
    (is (equal expected (%lsa-interval-pool state interval)))))

(deftest-each "%lsa-set-interval-pool"
  (fp-p new-pool expected-free-regs expected-free-fp-regs)
  ((:gpr-updates-free-regs    nil '(:r0 :r1 :r2) '(:r0 :r1 :r2) '(:xmm0))
   (:fp-updates-free-fp-regs  t   '(:xmm0 :xmm1) '(:r0)         '(:xmm0 :xmm1)))
  (let* ((state    (make-test-lsa-state :free-regs '(:r0) :free-fp-regs '(:xmm0)))
         (interval (make-lsa-test-interval :v 0 10 :fp-p fp-p)))
    (%lsa-set-interval-pool state interval new-pool)
    (is (equal expected-free-regs    (lsa-free-regs state)))
    (is (equal expected-free-fp-regs (lsa-free-fp-regs state)))))

;;; ─── preferred-register strategy helpers ─────────────────────────────────

(defun make-minimal-cc (&key (name :x86-64)
                              (arg-regs '(:rdi :rsi :rdx))
                              (ret-reg :rax)
                              (fp-arg-regs '(:xmm0 :xmm1))
                              (fp-ret-reg :xmm0)
                              (callee-saved '(:rbx :r12))
                              (caller-saved '(:rax :rcx :rdx :rsi :rdi)))
  "Build a minimal target-desc fixture for preferred-register strategy tests."
  (make-target-desc
    :name         name
    :gpr-names    (coerce (append arg-regs callee-saved caller-saved) 'vector)
    :arg-regs     arg-regs
    :ret-reg      ret-reg
    :fp-arg-regs  fp-arg-regs
    :fp-ret-reg   fp-ret-reg
    :callee-saved callee-saved
    :scratch-regs nil))

(deftest-each "%return-value-preferred-reg"
  (fp-p return-value-p ret-reg fp-ret-reg free-regs expected)
  ((:gpr-rv-in-pool      nil t   :rax :xmm0 '(:rax :rbx :rcx) :rax)
   (:not-return-value    nil nil :rax :xmm0 '(:rax :rbx)       nil)
   (:ret-reg-not-in-pool nil t   :rax :xmm0 '(:rbx :rcx)       nil)
   (:fp-rv-in-pool        t  t   :rax :xmm0 '(:xmm0 :xmm1)     :xmm0))
  (let* ((cc       (make-minimal-cc :ret-reg ret-reg :fp-ret-reg fp-ret-reg))
         (interval (make-lsa-test-interval :v 0 10 :fp-p fp-p)))
    (setf (interval-return-value-p interval) return-value-p)
    (is (eq expected (%return-value-preferred-reg interval cc free-regs)))))

(deftest "%call-crossing-preferred-reg prefers callee-saved for call-crossing gpr interval"
  "%call-crossing-preferred-reg returns a callee-saved register for a GPR interval that crosses a call."
  (let* ((cc       (make-minimal-cc :callee-saved '(:rbx :r12)))
         (interval (make-lsa-test-interval :v 0 10 :crosses-call-p t))
         (free-regs '(:rdi :rbx :r12)))
    (let ((result (%call-crossing-preferred-reg interval cc free-regs)))
      (is (member result '(:rbx :r12) :test #'eq)))))

(deftest "%call-crossing-preferred-reg returns nil for non-call-crossing interval"
  "%call-crossing-preferred-reg returns nil when the interval does not cross a call."
  (let* ((cc       (make-minimal-cc))
         (interval (make-lsa-test-interval :v 0 10 :crosses-call-p nil))
         (free-regs '(:rdi :rbx)))
    (is (null (%call-crossing-preferred-reg interval cc free-regs)))))

(deftest "%call-crossing-preferred-reg returns nil for fp interval"
  "%call-crossing-preferred-reg returns nil for FP intervals regardless of call-crossing status."
  (let* ((cc       (make-minimal-cc))
         (interval (make-lsa-test-interval :v 0 10 :crosses-call-p t :fp-p t))
         (free-regs '(:xmm0)))
    (is (null (%call-crossing-preferred-reg interval cc free-regs)))))

(deftest "%param-preferred-reg returns arg-reg matching parameter-index"
  "%param-preferred-reg returns the argument register corresponding to the interval's parameter index."
  (let* ((cc       (make-minimal-cc :arg-regs '(:rdi :rsi :rdx)))
         (interval (make-lsa-test-interval :v 0 10))
         (free-regs '(:rdi :rsi :rdx)))
    (setf (interval-parameter-index interval) 1)
    (is (eq :rsi (%param-preferred-reg interval cc free-regs)))))

(deftest "%param-preferred-reg returns nil when param-index is out of range"
  "%param-preferred-reg returns nil when the parameter index exceeds the available argument registers."
  (let* ((cc       (make-minimal-cc :arg-regs '(:rdi :rsi)))
         (interval (make-lsa-test-interval :v 0 10))
         (free-regs '(:rdi :rsi)))
    (setf (interval-parameter-index interval) 5)
    (is (null (%param-preferred-reg interval cc free-regs)))))

(deftest "%param-preferred-reg returns nil when preferred reg not in free pool"
  "%param-preferred-reg returns nil when the matching argument register is not in the free register pool."
  (let* ((cc       (make-minimal-cc :arg-regs '(:rdi :rsi :rdx)))
         (interval (make-lsa-test-interval :v 0 10))
         (free-regs '(:rsi :rdx)))  ; :rdi not available
    (setf (interval-parameter-index interval) 0)
    (is (null (%param-preferred-reg interval cc free-regs)))))

(deftest "%hint-policy-preferred-reg returns caller-saved when policy prefers it"
  "%hint-policy-preferred-reg returns a caller-saved register when the allocation policy prefers caller-saved."
  (let* ((cc       (make-minimal-cc :callee-saved '(:rbx) :caller-saved '(:rax :rcx)))
         (interval (make-lsa-test-interval :v 0 10 :crosses-call-p nil))
         (free-regs '(:rax :rbx :rcx))
         (*current-allocation-policy* '(:prefer-caller-saved-p t)))
    (let ((result (%hint-policy-preferred-reg interval cc free-regs)))
      (is (not (null result)))
      ;; The result must be a caller-saved register.
      (is (member result (target-caller-saved cc) :test #'eq)))))

(deftest "%hint-policy-preferred-reg returns nil when policy does not prefer caller-saved"
  "%hint-policy-preferred-reg returns nil when the allocation policy does not prefer caller-saved registers."
  (let* ((cc       (make-minimal-cc))
         (interval (make-lsa-test-interval :v 0 10))
         (free-regs '(:rax :rbx))
         (*current-allocation-policy* '(:prefer-caller-saved-p nil)))
    (is (null (%hint-policy-preferred-reg interval cc free-regs)))))

(deftest "%hint-policy-preferred-reg returns nil for call-crossing intervals even with policy"
  "%hint-policy-preferred-reg returns nil for call-crossing intervals even when the policy prefers caller-saved."
  ;; Safety guard: do not force caller-saved for call-crossing intervals.
  (let* ((cc       (make-minimal-cc))
         (interval (make-lsa-test-interval :v 0 10 :crosses-call-p t))
         (free-regs '(:rax :rbx))
         (*current-allocation-policy* '(:prefer-caller-saved-p t)))
    (is (null (%hint-policy-preferred-reg interval cc free-regs)))))

;;; ─── regalloc-target-fp-registers ────────────────────────────────────────

(deftest "regalloc-target-fp-registers x86-64 returns 16 xmm registers"
  "regalloc-target-fp-registers returns all 16 XMM registers for the x86-64 target."
  (let* ((cc (make-minimal-cc :name :x86-64))
         (fp-regs (regalloc-target-fp-registers cc)))
    (is (= 16 (length fp-regs)))
    (is (every (lambda (r) (member r fp-regs :test #'eq))
               '(:xmm0 :xmm7 :xmm15)))))

(deftest "regalloc-target-fp-registers aarch64 returns 32 v registers"
  "regalloc-target-fp-registers returns all 32 V registers for the AArch64 target."
  (let* ((cc (make-minimal-cc :name :aarch64))
         (fp-regs (regalloc-target-fp-registers cc)))
    (is (= 32 (length fp-regs)))
    (is (every (lambda (r) (member r fp-regs :test #'eq))
               '(:v0 :v15 :v31)))))

(deftest "regalloc-target-fp-registers unknown target falls back to fp-arg-regs union fp-ret-reg"
  "regalloc-target-fp-registers falls back to the union of fp-arg-regs and fp-ret-reg for unknown targets."
  ;; Unknown target name: result should be union of fp-arg-regs and fp-ret-reg
  ;; with duplicates removed.
  (let* ((cc (make-target-desc
               :name        :unknown-arch
               :gpr-names   #(:r0)
               :arg-regs    '(:r0)
               :ret-reg     :r0
               :fp-arg-regs '(:f0 :f1)
               :fp-ret-reg  :f0
               :callee-saved '()
               :scratch-regs nil))
         (fp-regs (regalloc-target-fp-registers cc)))
    ;; Duplicates (:f0 in both fp-arg-regs and fp-ret-reg) must be removed.
    (is (= 2 (length fp-regs)))
    (is (member :f0 fp-regs :test #'eq))
    (is (member :f1 fp-regs :test #'eq))))

;;; ─── %preferred-register-for-interval strategy priority order ────────────

(deftest "%preferred-register-for-interval: hint-policy wins over all others"
  "%preferred-register-for-interval applies hint-policy strategy before all other strategies."
  ;; Return-value, param-index, and call-crossing are all true but hint-policy
  ;; picks a caller-saved reg that differs from ret-reg.
  ;; hint-policy is first in *preferred-register-strategies* so it wins.
  (let* ((cc (make-target-desc
               :name        :x86-64
               :gpr-names   #(:rdi :rax :rbx)
               :arg-regs    '(:rdi)
               :ret-reg     :rax
               :fp-arg-regs '()
               :fp-ret-reg  nil
               :callee-saved '(:rbx)
               :scratch-regs nil))
         (interval (make-lsa-test-interval :v 0 10))
         (free-regs '(:rdi :rax :rbx)))
    ;; Make interval eligible for multiple strategies.
    (setf (interval-return-value-p interval) t)
    (setf (interval-parameter-index interval) 0)
    (let ((*current-allocation-policy* '(:prefer-caller-saved-p t)))
      ;; hint-policy prefers caller-saved (:rdi, :rax); those appear before :rbx.
      ;; return-value prefers :rax.  Both are in free pool.
      ;; hint-policy must win (first in strategy list).
      (let ((result (%preferred-register-for-interval interval cc free-regs)))
        (is (not (null result)))
        ;; The winning strategy is hint-policy which returns a caller-saved reg.
        (is (member result (target-caller-saved cc) :test #'eq))))))

(deftest "%preferred-register-for-interval: return-value wins when hint-policy returns nil"
  "%preferred-register-for-interval applies the return-value strategy when hint-policy produces no result."
  ;; No hint-policy preference → return-value strategy should fire.
  (let* ((cc (make-minimal-cc :ret-reg :rax))
         (interval (make-lsa-test-interval :v 0 10))
         (free-regs '(:rax :rbx)))
    (setf (interval-return-value-p interval) t)
    (let ((*current-allocation-policy* '(:prefer-caller-saved-p nil)))
      (is (eq :rax (%preferred-register-for-interval interval cc free-regs))))))

(deftest "%preferred-register-for-interval: param wins when neither hint nor return-value fires"
  "%preferred-register-for-interval applies the parameter strategy when hint-policy and return-value both yield nil."
  (let* ((cc (make-minimal-cc :arg-regs '(:rdi :rsi)))
         (interval (make-lsa-test-interval :v 0 10))
         (free-regs '(:rdi :rsi :rbx)))
    (setf (interval-parameter-index interval) 0)
    (let ((*current-allocation-policy* nil))
      (is (eq :rdi (%preferred-register-for-interval interval cc free-regs))))))

(deftest "%preferred-register-for-interval: returns nil when no strategy fires"
  "%preferred-register-for-interval returns nil when no preferred-register strategy produces a result."
  (let* ((cc (make-minimal-cc))
         (interval (make-lsa-test-interval :v 0 10))
         (free-regs '(:rbx)))
    ;; No return-value, no param-index, no call-crossing, no policy hint.
    (let ((*current-allocation-policy* nil))
      (is (null (%preferred-register-for-interval interval cc free-regs))))))

;;; ─── %allocation-strategy ────────────────────────────────────────────────

(deftest-each "%allocation-strategy dispatch"
  (policy expected-strategy)
  ((:color-via-allocator   '(:allocator :color)            :color)
   (:lscan-via-allocator   '(:allocator :linear-scan)      :linear-scan)
   (:color-via-alt-key     '(:register-allocator :color)   :color)
   (:nil-policy-default    nil                              :linear-scan))
  (let ((*regalloc-allocation-strategy* :linear-scan))
    (is (eq expected-strategy (%allocation-strategy policy)))))

(deftest "%allocation-strategy falls back to *regalloc-allocation-strategy* when policy is empty"
  "%allocation-strategy returns the value of *regalloc-allocation-strategy* when the policy list is empty."
  (let ((*regalloc-allocation-strategy* :color))
    (is (eq :color (%allocation-strategy '())))))

;;; ─── %derive-single-function-policy ─────────────────────────────────────

(deftest "%derive-single-function-policy returns non-nil policy for single-function stream"
  "%derive-single-function-policy returns a non-nil policy plist for an instruction stream with exactly one function."
  ;; A stream with exactly one vm-label should yield a non-nil policy.
  (let* ((instructions
           (list (make-vm-label :name "entry")
                 (make-vm-const :dst :r0 :value 42)
                 (make-vm-ret  :reg :r0))))
    (let ((policy (%derive-single-function-policy instructions)))
      ;; Single function → policy plist should be non-nil.
      (is (not (null policy))))))

(deftest "%derive-single-function-policy returns nil for multi-function stream"
  "%derive-single-function-policy returns nil when the instruction stream contains more than one function."
  ;; Two vm-labels → more than one function → policy derivation returns NIL.
  (let* ((instructions
           (list (make-vm-label :name "f1")
                 (make-vm-const :dst :r0 :value 1)
                 (make-vm-ret  :reg :r0)
                 (make-vm-label :name "f2")
                 (make-vm-const :dst :r0 :value 2)
                 (make-vm-ret  :reg :r0))))
    (let ((policy (%derive-single-function-policy instructions)))
      (is (null policy)))))

;;; ─── linear-scan-allocate integration test ───────────────────────────────

(deftest "linear-scan-allocate returns assignment-ht spill-ht spill-count for hand-crafted intervals"
  "linear-scan-allocate returns correct assignment and spill hash tables for non-overlapping intervals."
  ;; Two non-overlapping intervals with 2 available registers: both should be
  ;; assigned without spilling.
  (let* ((int-a (make-lsa-test-interval :a 0  5 :use-positions '(0 5)))
         (int-b (make-lsa-test-interval :b 6 10 :use-positions '(6 10)))
         (cc    (make-target-desc
                  :name        :x86-64
                  :gpr-names   #(:r0 :r1)
                  :arg-regs    '(:r0)
                  :ret-reg     :r0
                  :fp-arg-regs '()
                  :fp-ret-reg  nil
                  :callee-saved '()
                  :scratch-regs nil))
         (intervals (list int-a int-b)))
    (multiple-value-bind (assignment spill-map spill-count)
        (linear-scan-allocate intervals cc)
      (is (hash-table-p assignment))
      (is (hash-table-p spill-map))
      (is (integerp spill-count))
      ;; Both intervals should have been assigned a physical register.
      (is (gethash :a assignment))
      (is (gethash :b assignment))
      ;; No spills expected when two registers are available for two non-overlapping intervals.
      (is (= 0 (hash-table-count spill-map))))))

(deftest "linear-scan-allocate spills when register pressure exceeds pool"
  "linear-scan-allocate spills at least one interval when mutually overlapping intervals exceed the register pool."
  ;; Three mutually overlapping intervals with only 2 registers: one must spill.
  (let* ((int-a (make-lsa-test-interval :a 0 20 :use-positions '(0 5 10)))
         (int-b (make-lsa-test-interval :b 0 20 :use-positions '(1 6 11)))
         (int-c (make-lsa-test-interval :c 0 20 :use-positions '(2 7 12)))
         (cc    (make-target-desc
                  :name        :x86-64
                  :gpr-names   #(:r0 :r1)
                  :arg-regs    '(:r0)
                  :ret-reg     :r0
                  :fp-arg-regs '()
                  :fp-ret-reg  nil
                  :callee-saved '()
                  :scratch-regs nil))
         (intervals (list int-a int-b int-c)))
    (multiple-value-bind (assignment spill-map spill-count)
        (linear-scan-allocate intervals cc)
      (declare (ignore assignment))
      ;; At least one vreg must have been spilled.
      (is (>= (hash-table-count spill-map) 1))
      (is (>= spill-count 1)))))
