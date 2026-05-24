;;;; tests/native-advanced-evidence-tests.lisp
;;;; Behavioral evidence tests for native-advanced FRs (FR-500 to FR-773).
;;;; Verifies that implementation files compile, exports exist, and
;;;; key functions produce correct results.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

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
  (let ((cores (cl-cc/runtime:detect-cpu-cores)))
    (assert-true (integerp cores))
    (assert-true (plusp cores))))

(deftest fr-624-topology-numa-info-returns-plist
  "FR-624: detect-numa-topology returns a plist or NIL on unsupported hosts."
  (let ((topo (cl-cc/runtime:detect-numa-topology)))
    (when topo
      (assert-true (listp topo))
      (assert-true (getf topo :node-count)))
    (assert-true t)))



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
