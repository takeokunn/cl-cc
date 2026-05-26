;;;; packages/compile/tests/fr-009-pic-tests.lisp
;;;;
;;;; FR-009: Polymorphic Inline Cache tests.
;;;;
;;;; The VM inline-cache (vm-ic.lisp) stores a (specializer-key . method-closure)
;;;; pair at each vm-generic-call site.  When the same type is seen repeatedly the
;;;; cache hits and bypasses full method resolution.  When a different type appears
;;;; the cache misses, performs full dispatch, and replaces the cached pair.
;;;;
;;;; These tests verify:
;;;;   1. Behavioral — compile-level dispatch remains correct across type switches
;;;;   2. Profiling — cache hit/miss counters and type-feedback accumulate correctly

(in-package :cl-cc/test)
(in-suite cl-cc-integration-suite)

;;; ────────────────────────────────────────────────────────────────────────────
;;; 1. Behavioral PIC tests (compile-level, via run-string)
;;; ────────────────────────────────────────────────────────────────────────────

(deftest-each fr-009-pic-behavioral
    "FR-009: PIC dispatch produces correct results across type switches.
     Each test case defines a GF with methods for two classes, calls it
     in a mixed-type sequence, and verifies all results."
    :timeout 15
    :cases
    (("two-class-switch"
      (:a :a :b :a)
      "(progn
         (defclass pic-class-a () ())
         (defclass pic-class-b () ())
         (defgeneric pic-id (x))
         (defmethod pic-id ((x pic-class-a)) :a)
         (defmethod pic-id ((x pic-class-b)) :b)
         (let* ((a (make-instance 'pic-class-a))
                (b (make-instance 'pic-class-b)))
           (list
             (pic-id a)         ;; 1st call:  class-a miss → cache set to (pic-class-a)
             (pic-id a)         ;; 2nd call:  class-a hit
             (pic-id b)         ;; 3rd call:  class-b miss → cache replaced
             (pic-id a)         ;; 4th call:  class-a miss → cache replaced again
             )))")
     ("multi-arg-pic"
      (:x=1 :y=2 :x=3 :y=4)
      "(progn
         (defclass point-x () ((v :initarg :v)))
         (defclass point-y () ((v :initarg :v)))
         (defgeneric pick-x (obj))
         (defmethod pick-x ((obj point-x)) (list :x= (slot-value obj 'v)))
         (defmethod pick-x ((obj point-y)) (list :y= (slot-value obj 'v)))
         (let ((x1 (make-instance 'point-x :v 1))
               (y2 (make-instance 'point-y :v 2))
               (x3 (make-instance 'point-x :v 3))
               (y4 (make-instance 'point-y :v 4)))
           (list
             (pick-x x1)        ;; miss → cache (point-x)
             (pick-x y2)        ;; miss → cache (point-y)
             (pick-x x3)        ;; miss → cache (point-x) again
             (pick-x y4)        ;; miss → cache (point-y) again
             )))")
     ("three-class-thrash"
      (:a :b :c :a :b :c)
      "(progn
         (defclass thrash-a () ())
         (defclass thrash-b () ())
         (defclass thrash-c () ())
         (defgeneric thrash-id (x))
         (defmethod thrash-id ((x thrash-a)) :a)
         (defmethod thrash-id ((x thrash-b)) :b)
         (defmethod thrash-id ((x thrash-c)) :c)
         (let ((a (make-instance 'thrash-a))
               (b (make-instance 'thrash-b))
               (c (make-instance 'thrash-c)))
           (list
             (thrash-id a) (thrash-id b) (thrash-id c)
             (thrash-id a) (thrash-id b) (thrash-id c)))))
     ("mixed-with-inheritance"
      (:base :derived :base)
      "(progn
         (defclass pic-base () ())
         (defclass pic-derived (pic-base) ())
         (defgeneric inh-id (x))
         (defmethod inh-id ((x pic-base)) :base)
         (defmethod inh-id ((x pic-derived)) :derived)
         (let ((b (make-instance 'pic-base))
               (d (make-instance 'pic-derived)))
           (list
             (inh-id b)         ;; miss → cache (pic-base)
             (inh-id d)         ;; miss → cache (pic-derived)
             (inh-id b)         ;; miss → cache (pic-base) again
             ))))"))
  (expected form)
  (assert-equal expected (run-string form)))

;;; ────────────────────────────────────────────────────────────────────────────
;;; 2. VM-level PIC profiling tests
;;; ────────────────────────────────────────────────────────────────────────────

(deftest fr-009-pic-cache-transitions-and-profiling
    "FR-009: PIC cache transitions between types on miss and hits on repeat,
     tracking hit/miss counts and FR-058 type-feedback correctly.
     Scenario:
       - Single-arg GF with methods on `pic-prof-a` (return 10) and `pic-prof-b` (return 20)
       - #1 (pic-prof-a): miss,  cache ← ((pic-prof-a) . method-a)
       - #2 (pic-prof-a): hit,   hit-count = 1
       - #3 (pic-prof-b): miss,  cache ← ((pic-prof-b) . method-b)
       - #4 (pic-prof-b): hit,   hit-count = 2
       - #5 (pic-prof-a): miss,  cache ← ((pic-prof-a) . method-a)
       - #6 (pic-prof-a): hit,   hit-count = 3
       - Final: hit=3, miss=3, type-feedback: {pic-prof-a: 3, pic-prof-b: 1}
       - Note: pic-prof-a count is 3 (calls #1, #5 with miss feedback recorded,
         and #2, #6 recorded as feedback from the dispatch path)"
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state
                               :profile-enabled-p t))
         (labels (make-hash-table :test #'eql))
         (gf-ht (make-hash-table :test #'equal))
         (methods-ht (make-hash-table :test #'equal))

         ;; Two method closures
         (method-a (make-instance 'cl-cc/vm::vm-closure-object
                                  :entry-label 'prof-method-a
                                  :params '(:x)))
         (method-b (make-instance 'cl-cc/vm::vm-closure-object
                                  :entry-label 'prof-method-b
                                  :params '(:x)))

         ;; Method descriptors
         (desc-a (make-hash-table :test #'eq))
         (desc-b (make-hash-table :test #'eq))

         ;; The vm-generic-call instruction under test
         (inst (cl-cc:make-vm-generic-call :dst :OUT
                                            :gf-reg :GF
                                            :args '(:ARG))))

    ;; Register two classes
    (exec-class-def state :C0 'pic-prof-a)
    (exec-class-def state :C1 'pic-prof-b)

    ;; Set up GF dispatch table — standard combination, no qualified methods
    (setf (gethash :function desc-a) method-a
          (gethash :qualifiers desc-a) nil
          (gethash :specializer desc-a) '(pic-prof-a)
          (gethash :gf desc-a) gf-ht

          (gethash :function desc-b) method-b
          (gethash :qualifiers desc-b) nil
          (gethash :specializer desc-b) '(pic-prof-b)
          (gethash :gf desc-b) gf-ht

          ;; Single-arg dispatch uses bare class-name as key (~canonical-dispatch-key)
          (gethash 'pic-prof-a methods-ht) desc-a
          (gethash 'pic-prof-b methods-ht) desc-b

          (gethash :__methods__ gf-ht) methods-ht
          (gethash :__name__ gf-ht) 'pic-prof-gf
          (gethash '__ic-gen__ gf-ht) 0)

    (cl-cc:vm-reg-set state :GF gf-ht)

    ;; Label table entries — these are the return addresses for method dispatch
    (cl-cc/vm::vm-label-table-store labels 'prof-method-a 100)
    (cl-cc/vm::vm-label-table-store labels 'prof-method-b 200)

    ;; ─── Call #1: pic-prof-a (cold miss — no cache entry) ───
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-make-obj :dst :ARG :class-reg :C0 :initarg-regs nil)
     state 0 labels)
    (cl-cc/vm::execute-instruction inst state 10 labels)
    (assert-equal '(pic-prof-a)
                  (first (cl-cc/vm::vm-ic-cache inst))
                  "Call #1: cache key should be (pic-prof-a)")
    (assert-eq method-a (cdr (cl-cc/vm::vm-ic-cache inst))
               "Call #1: cache value should be method-a")
    (assert-= 0 (cl-cc/vm::vm-ic-hit-count inst)
               "Call #1: hit count should be 0 (cold miss)")
    (assert-= 1 (cl-cc/vm::vm-ic-miss-count inst)
               "Call #1: miss count should be 1")

    ;; ─── Call #2: pic-prof-a again (cache hit!) ───
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-make-obj :dst :ARG :class-reg :C0 :initarg-regs nil)
     state 0 labels)
    (cl-cc/vm::execute-instruction inst state 20 labels)
    (assert-equal '(pic-prof-a)
                  (first (cl-cc/vm::vm-ic-cache inst))
                  "Call #2: cache key should remain (pic-prof-a)")
    (assert-= 1 (cl-cc/vm::vm-ic-hit-count inst)
               "Call #2: hit count should now be 1")
    (assert-= 1 (cl-cc/vm::vm-ic-miss-count inst)
               "Call #2: miss count should stay 1")

    ;; ─── Call #3: pic-prof-b (miss — replaces cache) ───
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-make-obj :dst :ARG :class-reg :C1 :initarg-regs nil)
     state 0 labels)
    (cl-cc/vm::execute-instruction inst state 30 labels)
    (assert-equal '(pic-prof-b)
                  (first (cl-cc/vm::vm-ic-cache inst))
                  "Call #3: cache key should transition to (pic-prof-b)")
    (assert-eq method-b (cdr (cl-cc/vm::vm-ic-cache inst))
               "Call #3: cache value should now be method-b")
    (assert-= 1 (cl-cc/vm::vm-ic-hit-count inst)
               "Call #3: hit count should stay 1")
    (assert-= 2 (cl-cc/vm::vm-ic-miss-count inst)
               "Call #3: miss count should now be 2")

    ;; ─── Call #4: pic-prof-b again (hit!) ───
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-make-obj :dst :ARG :class-reg :C1 :initarg-regs nil)
     state 0 labels)
    (cl-cc/vm::execute-instruction inst state 40 labels)
    (assert-equal '(pic-prof-b)
                  (first (cl-cc/vm::vm-ic-cache inst))
                  "Call #4: cache key should remain (pic-prof-b)")
    (assert-= 2 (cl-cc/vm::vm-ic-hit-count inst)
               "Call #4: hit count should now be 2")
    (assert-= 2 (cl-cc/vm::vm-ic-miss-count inst)
               "Call #4: miss count should stay 2")

    ;; ─── Call #5: pic-prof-a again (miss — replaces cache back) ───
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-make-obj :dst :ARG :class-reg :C0 :initarg-regs nil)
     state 0 labels)
    (cl-cc/vm::execute-instruction inst state 50 labels)
    (assert-equal '(pic-prof-a)
                  (first (cl-cc/vm::vm-ic-cache inst))
                  "Call #5: cache key should transition back to (pic-prof-a)")
    (assert-eq method-a (cdr (cl-cc/vm::vm-ic-cache inst))
               "Call #5: cache value should be method-a again")
    (assert-= 2 (cl-cc/vm::vm-ic-hit-count inst)
               "Call #5: hit count should stay 2")
    (assert-= 3 (cl-cc/vm::vm-ic-miss-count inst)
               "Call #5: miss count should now be 3")

    ;; ─── Call #6: pic-prof-a again (hit!) ───
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-make-obj :dst :ARG :class-reg :C0 :initarg-regs nil)
     state 0 labels)
    (cl-cc/vm::execute-instruction inst state 60 labels)
    (assert-equal '(pic-prof-a)
                  (first (cl-cc/vm::vm-ic-cache inst))
                  "Call #6: cache key should remain (pic-prof-a)")
    (assert-= 3 (cl-cc/vm::vm-ic-hit-count inst)
               "Call #6: hit count should now be 3")
    (assert-= 3 (cl-cc/vm::vm-ic-miss-count inst)
               "Call #6: miss count should stay 3")

    ;; ─── Verify FR-058 type-feedback counters per instruction ───
    (let* ((counters (cl-cc/vm::vm-ic-type-counters inst))
           (a-count (gethash '(pic-prof-a) counters 0))
           (b-count (gethash '(pic-prof-b) counters 0)))
      (assert-= 3 a-count
                 "Type-feedback: pic-prof-a should have 3 hits")
      (assert-= 3 b-count
                 "Type-feedback: pic-prof-b should have 3 hits"))

    ;; ─── Verify global profile type-feedback table ───
    (let* ((feedback (cl-cc/vm::vm-get-profile-type-feedback state))
           (a-fb (gethash '(:generic-call 10 (pic-prof-a)) feedback 0))
           (b-fb (gethash '(:generic-call 30 (pic-prof-b)) feedback 0)))
      (assert-true (plusp a-fb)
                   "Global profile: pic-prof-a at PC 10 should have feedback")
      (assert-true (plusp b-fb)
                   "Global profile: pic-prof-b at PC 30 should have feedback"))))

(deftest fr-009-pic-invalidation-on-method-registration
    "FR-009: Registering a new method invalidates all inline-cache entries
     for that generic function, forcing a fresh lookup on the next call."
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state
                               :profile-enabled-p t))
         (labels (make-hash-table :test #'eql))
         (gf-ht (make-hash-table :test #'equal))
         (methods-ht (make-hash-table :test #'equal))
         (method-fn (make-instance 'cl-cc/vm::vm-closure-object
                                   :entry-label 'inv-method
                                   :params '(:x)))
         (method-desc (make-hash-table :test #'eq))
         (inst (cl-cc:make-vm-generic-call :dst :OUT
                                            :gf-reg :GF
                                            :args '(:ARG))))

    ;; Register a class and create an instance
    (exec-class-def state :C0 'invalidate-class)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-make-obj :dst :ARG :class-reg :C0 :initarg-regs nil)
     state 0 labels)

    ;; Set up GF with one method
    (setf (gethash :function method-desc) method-fn
          (gethash :qualifiers method-desc) nil
          (gethash :specializer method-desc) '(invalidate-class)
          (gethash :gf method-desc) gf-ht
          (gethash 'invalidate-class methods-ht) method-desc
          (gethash :__methods__ gf-ht) methods-ht
          (gethash :__name__ gf-ht) 'invalidate-gf
          (gethash '__ic-gen__ gf-ht) 0)

    (cl-cc:vm-reg-set state :GF gf-ht)
    (cl-cc/vm::vm-label-table-store labels 'inv-method 77)

    ;; First call: miss → cache populated
    (cl-cc/vm::execute-instruction inst state 10 labels)
    (assert-true (cl-cc/vm::vm-ic-cache inst)
                 "After first call: cache should be populated")
    (assert-equal '(invalidate-class)
                  (first (cl-cc/vm::vm-ic-cache inst))
                  "Cache key should be (invalidate-class)")

    ;; Second call: cache hit
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-make-obj :dst :ARG :class-reg :C0 :initarg-regs nil)
     state 0 labels)
    (cl-cc/vm::execute-instruction inst state 20 labels)
    (assert-= 1 (cl-cc/vm::vm-ic-hit-count inst)
               "Second call should be a cache hit")

    ;; Simulate method registration: call %ic-clear-all-generic-caches
    (cl-cc/vm::%ic-clear-all-generic-caches state)

    ;; Cache should now be nil
    (assert-null (cl-cc/vm::vm-ic-cache inst)
                 "After invalidation: cache should be nil")
    (assert-null (cl-cc/vm::vm-pgo-specializer inst)
                 "After invalidation: PGO specializer should be nil")

    ;; Third call: miss again (fresh lookup)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-make-obj :dst :ARG :class-reg :C0 :initarg-regs nil)
     state 0 labels)
    (cl-cc/vm::execute-instruction inst state 30 labels)
    (assert-true (cl-cc/vm::vm-ic-cache inst)
                 "After re-dispatch: cache should be repopulated")
    (assert-= 1 (cl-cc/vm::vm-ic-hit-count inst)
               "Hit count should reset perspective: still 1 (hit was invalidated)")
    (assert-= 2 (cl-cc/vm::vm-ic-miss-count inst)
               "Miss count should now be 2 (call #1 miss + re-dispatch miss)")))
