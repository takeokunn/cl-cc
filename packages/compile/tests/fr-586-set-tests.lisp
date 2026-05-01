(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest fr-586-set-is-core-builtin
  "FR-586 `set` is registered in the core binary builtin table."
  (assert-equal 'cl-cc::make-vm-set-symbol-value
                (cdr (assoc 'set cl-cc/compile::*builtin-binary-entries*))))

(deftest fr-586-set-not-in-binary-custom-table
  "FR-586 `set` is absent from the extended binary-custom table."
  (assert-false (assoc 'set cl-cc/compile::*builtin-binary-custom-entries*)))
