;;;; packages/optimize/tests/native-advanced-optimizer-tests.lisp

(in-package :cl-cc/test)

(defsuite native-advanced-optimizer-suite
  :description "Native advanced optimizer FR tests"
  :parent cl-cc-unit-suite)

(in-suite native-advanced-optimizer-suite)

(deftest fr-662-path-profiling-and-block-versioning-plan
  "FR-662: Ball-Larus path counters feed hot-path superblock plans."
  (let* ((instructions (list (cl-cc:make-vm-label :name "entry")
                             (cl-cc:make-vm-const :dst :C :value 1)
                             (cl-cc:make-vm-jump-zero :reg :C :label "taken")
                             (cl-cc:make-vm-const :dst :R0 :value 11)
                             (cl-cc:make-vm-halt :reg :R0)
                             (cl-cc:make-vm-label :name "taken")
                             (cl-cc:make-vm-const :dst :R0 :value 22)
                             (cl-cc:make-vm-halt :reg :R0)))
         (profile (cl-cc/optimize:opt-build-ball-larus-profile instructions :function-id :test-fn))
         (counts (make-hash-table :test #'equal)))
    (setf (gethash (list :test-fn 0) counts) 9)
    (let ((plan (cl-cc/optimize:opt-build-block-version-plan profile
                                                             :counts counts
                                                             :hot-threshold 5)))
      (assert-true (cl-cc/optimize:opt-ball-larus-profile-edges profile))
      (assert-true (find 0 (cl-cc/optimize:opt-ball-larus-profile-paths profile)
                         :key (lambda (path) (getf path :sum))))
      (assert-true (cl-cc/optimize:opt-block-version-plan-versions plan)))))

(deftest fr-662-path-profiling-pass-instruments-edges-and-exits
  "FR-662: path profiling pass emits VM-CONST/VM-ADD and path-record instructions."
  (let* ((instructions (list (cl-cc:make-vm-label :name "entry")
                              (cl-cc:make-vm-const :dst :R0 :value 42)
                              (cl-cc:make-vm-halt :reg :R0)))
          (profile (cl-cc/optimize:opt-compute-path-profile instructions))
          (after-pass (cl-cc/optimize:opt-pass-path-profiling instructions)))
    (assert-true (> (length after-pass) (length instructions)))
    (assert-true (some #'cl-cc/vm:vm-add-p after-pass))
    (assert-true (some #'cl-cc/optimize::opt-vm-path-profile-record-p after-pass))
    (assert-true profile)
    (assert-true (plusp (cl-cc/optimize:opt-path-profile-block-path-id (first profile))))
    (assert-true (integerp (cl-cc/optimize:opt-path-profile-block-successor-count
                            (first profile))))))

(deftest fr-662-hot-path-superblock-clones-blocks
  "FR-662: hot Ball-Larus path sums are duplicated into superblock labels."
  (let* ((instructions (list (cl-cc:make-vm-label :name "entry")
                             (cl-cc:make-vm-const :dst :R0 :value 42)
                             (cl-cc:make-vm-halt :reg :R0)))
         (counts (make-hash-table :test #'equal)))
    (setf (gethash (list :test-fn 0) counts) 10)
    (let ((versioned (cl-cc/optimize:opt-duplicate-hot-paths
                      instructions counts :function-id :test-fn :hot-threshold 5)))
      (assert-true (some (lambda (inst)
                           (and (cl-cc/vm:vm-label-p inst)
                                (search "BL_SUPER_0" (cl-cc/vm::vm-name inst))))
                         versioned)))))

(deftest fr-582-autotune-simd-produces-valid-tile-sizes-and-retunes-lanes
  "FR-582: auto-tuning derives bounded tile sizes and writes a valid SIMD lane count."
  (let* ((cache-info '(:l1 32768 :l2 262144 :l3 8388608 :source :test))
         (simd (cl-cc:make-vm-simd-vector-op :op :+
                                             :dst-array :D
                                             :lhs-array :A
                                             :rhs-array :B
                                             :index-reg :I
                                             :lanes 4
                                             :element-type :i32))
         (cl-cc/optimize:*autotune-simd-enabled* t)
         (cl-cc/optimize::*autotune-simd-cache-info* cache-info))
    (multiple-value-bind (l1 l2 l3)
        (cl-cc/optimize:autotune-simd-tile-sizes cache-info)
      (assert-true (member l1 '(16 32)))
      (assert-true (member l2 '(128 256)))
      (assert-true (member l3 '(256 512)))
      (let ((retuned (first (cl-cc/optimize:opt-pass-autotune-simd (list simd)))))
        (assert-= l1 (cl-cc/vm:vm-simd-vector-op-lanes retuned))))))

(deftest fr-703-compiler-self-profiling-build-analytics
  "FR-703: compiler self-profiling exposes build analytics capability summary."
  (let ((summary (cl-cc/optimize:build-analytics-summary
                  :pass-count 3 :instruction-count 9 :elapsed-us 42 :changed-count 1)))
    (assert-equal :fr-703 (getf summary :fr-id))
    (assert-equal 3 (getf summary :pass-count))
    (assert-true (getf (getf summary :capabilities) :build-analytics))))

(deftest fr-723-load-widening-replaces-adjacent-byte-loads
  "FR-723: adjacent byte loads become one naturally-aligned load plus extraction."
  (let* ((first-load (cl-cc:make-vm-aref :dst :r1 :array-reg :a :index-reg 0))
         (second-load (cl-cc:make-vm-aref :dst :r2 :array-reg :a :index-reg 1))
         (result (cl-cc/optimize:opt-pass-load-widening-store-coalescing
                  (list first-load second-load))))
    (assert-= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-aref)) result))
    (assert-= 2 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-logand)) result))
    (assert-true (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-logand)
                              (eq :r1 (cl-cc/vm::vm-dst i))))
                       result))
    (assert-true (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-logand)
                              (eq :r2 (cl-cc/vm::vm-dst i))))
                       result))))

(deftest fr-723-store-coalescing-packs-adjacent-byte-stores
  "FR-723: adjacent byte stores are packed and emitted as one wider store."
  (let* ((first-store (cl-cc:make-vm-aset :array-reg :a :index-reg 0 :val-reg :v1))
         (second-store (cl-cc:make-vm-aset :array-reg :a :index-reg 1 :val-reg :v2))
         (result (cl-cc/optimize:opt-pass-load-widening-store-coalescing
                  (list first-store second-store))))
    (assert-= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-aset)) result))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-logior)) result))
    (let ((store (find-if (lambda (i) (typep i 'cl-cc/vm::vm-aset)) result)))
      (assert-eq :a (cl-cc/vm::vm-array-reg store))
      (assert-= 0 (cl-cc/vm::vm-index-reg store)))))

(deftest fr-723-load-store-coalescing-respects-natural-alignment
  "FR-723: unaligned adjacent byte accesses are preserved until unaligned markers exist."
  (let* ((first-load (cl-cc:make-vm-aref :dst :r1 :array-reg :a :index-reg 1))
         (second-load (cl-cc:make-vm-aref :dst :r2 :array-reg :a :index-reg 2))
         (result (cl-cc/optimize:opt-pass-load-widening-store-coalescing
                  (list first-load second-load))))
    (assert-equal (list first-load second-load) result)))
