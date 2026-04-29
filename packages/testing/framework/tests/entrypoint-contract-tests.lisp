;;;; entrypoint-contract-tests.lisp — flake entrypoint contract checks

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(defun %flake-text ()
  (uiop:read-file-string (merge-pathnames #P"nix/apps.nix" (uiop:getcwd))))

(deftest flake-test-app-uses-fast-runner
  "The public fast app maps to run-fast-tests and exports CLCC timeout vars."
  (let ((text (%flake-text)))
    (assert-true (search "test = mkSbclScript {" text))
    (assert-true (search "CLCC_TEST_TIMEOUT" text))
    (assert-true (search "CLCC_SUITE_TIMEOUT" text))
    (assert-true (search "run-fast-tests" text))))

(deftest flake-test-full-loads-slow-system
  "The full test app dispatches the canonical run-tests entrypoint."
  (let ((text (%flake-text)))
    (assert-true (search "test-full = mkSbclScript {" text))
    (assert-true (search "starting full canonical test plan" text))))

(deftest flake-coverage-loads-slow-system
  "The coverage app loads the base test system and runs the coverage driver."
  (let ((text (%flake-text)))
    (assert-true (search "coverage = mkSbclScript {" text))
    (assert-true (search "cl-user::load-system-soft :cl-cc-test" text))
    (assert-true (search "coverageDriver" text))))
