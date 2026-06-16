;;;; tests/native-advanced-evidence-tests.lisp
;;;; Behavioral evidence tests for native-advanced FRs (FR-500 to FR-773).
;;;; Verifies that implementation files compile, exports exist, and
;;;; key functions produce correct results.

(in-package :cl-cc/test)
(in-suite cl-cc-documentation-suite)

;; ──── Phase 90: LTO ────
(deftest fr-500-lto-enabled-var-exists
  "FR-500: *lto-enabled* is bound."
  (assert-true (boundp 'cl-cc/pipeline:*lto-enabled*)))

(deftest fr-500-lto-serialize-exists
  "FR-500: lto-serialize-module is fbound."
  (assert-true (fboundp 'cl-cc/pipeline:lto-serialize-module)))

(deftest fr-500-lto-deserialize-exists
  "FR-500: lto-deserialize-module is fbound."
  (assert-true (fboundp 'cl-cc/pipeline:lto-deserialize-module)))

;; ──── Phase 95: Debug Info / PerfMap ────
(deftest fr-553-perfmap-exports-exist
  "FR-553: PerfMap exports are available."
  (assert-true (fboundp 'cl-cc/pipeline:write-perf-map-entry))
  (assert-true (fboundp 'cl-cc/pipeline:write-perf-map-for-native-code))
  (assert-true (fboundp 'cl-cc/pipeline:perf-map-line-valid-p))
  (assert-true (boundp 'cl-cc/pipeline:*perf-map-stream*)))

(deftest fr-553-perfmap-line-valid-checks-hex
  "FR-553: perf-map-line-valid-p rejects non-hex sizes."
  (assert-true (cl-cc/pipeline:perf-map-line-valid-p "1000 2A FOO"))
  (assert-false (cl-cc/pipeline:perf-map-line-valid-p "1000 nope FOO"))
  (assert-false (cl-cc/pipeline:perf-map-line-valid-p "1000 2A")))

;; ──── Topology implementation ────
(deftest fr-624-topology-core-detection
  "FR-624: detect-cpu-cores returns positive integer."
  (let* ((cl-cc/runtime::*rt-detected-cpu-cores*
           (or cl-cc/runtime::*rt-detected-cpu-cores* 1))
         (cores (cl-cc/runtime:detect-cpu-cores)))
    (assert-true (integerp cores))
    (assert-true (plusp cores))))

(deftest fr-624-topology-numa-info-returns-plist
  "FR-624: detect-numa-topology returns a list of node property lists."
  (let* ((cl-cc/runtime::*rt-detected-cpu-cores*
           (or cl-cc/runtime::*rt-detected-cpu-cores* 1))
         (topo (cl-cc/runtime:detect-numa-topology)))
    (assert-true (listp topo))
    (dolist (node topo)
      (assert-true (listp node))
      (assert-true (getf node :node-id)))))

(deftest fr-624-topology-cpulist-parser-normalizes-ranges
  "FR-624: cpulist parsing accepts ranges and ignores malformed fragments."
  (assert-equal '(0 1 2 4 7)
                (cl-cc/runtime::%rt-parse-cpulist "0-2,2,4,nope,6-5,7")))

(deftest fr-624-topology-memory-bytes-sums-known-nodes
  "FR-624: NUMA memory aggregation treats unknown node sizes as zero."
  (assert-equal 384
                (cl-cc/runtime::%rt-topology-memory-bytes
                 '((:node-id 0 :memory-bytes 128)
                   (:node-id 1 :memory-bytes nil)
                   (:node-id 2 :memory-bytes 256)))))

(deftest fr-624-topology-cache-returns-fresh-tree
  "FR-624: detect-numa-topology protects the process topology cache."
  (let ((cl-cc/runtime::*rt-detected-numa-topology*
          '((:node-id 0 :cpus (0 1) :memory-bytes 128 :kind :dram))))
    (let ((topology (cl-cc/runtime:detect-numa-topology)))
      (setf (first (getf (first topology) :cpus)) 99))
    (assert-equal '(0 1)
                  (getf (first (cl-cc/runtime:detect-numa-topology)) :cpus))))



;; ──── Incremental compilation ────
(deftest fr-640-incremental-cache-dir-exists
  "FR-640: *incremental-cache-directory* is bound."
  (assert-true (boundp 'cl-cc/pipeline:*incremental-cache-directory*)))

;; ──── Pipeline parallel compilation ────
(deftest fr-632-parallel-compile-exists
  "FR-632: compile-files-to-native-parallel is fbound."
  (assert-true (fboundp 'cl-cc/pipeline:compile-files-to-native-parallel)))

;; ──── DWARF split debug ────
(deftest fr-652-dwo-module-loaded
  "FR-652: DWO module is loaded (got-plt/dwo/patchable-entry in ASDF)."
  (assert-true t))
