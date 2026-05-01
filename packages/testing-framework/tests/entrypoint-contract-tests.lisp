;;;; entrypoint-contract-tests.lisp — flake entrypoint contract checks

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(defun %flake-text ()
  (uiop:read-file-string (merge-pathnames #P"nix/apps.nix" (uiop:getcwd))))

(defun %checks-text ()
  (uiop:read-file-string (merge-pathnames #P"nix/checks.nix" (uiop:getcwd))))

(deftest flake-test-app-runs-canonical-suite
  "The single test app dispatches run-tests (canonical full plan) and exports CLCC timeout vars."
  (let ((text (%flake-text)))
    (assert-true (search "test = mkSbclScript {" text))
    (assert-true (search "CLCC_TEST_TIMEOUT" text))
    (assert-true (search "CLCC_SUITE_TIMEOUT" text))
    (assert-true (search "run-tests" text))
    (assert-true (search "starting unified test plan" text))))

(deftest flake-deprecated-apps-removed
  "test-full / coverage / perf-smoke / stability-smoke entrypoints must be removed."
  (let ((text (%flake-text)))
    (assert-false (search "test-full = mkSbclScript" text))
    (assert-false (search "coverage = mkSbclScript" text))
    (assert-false (search "perf-smoke =" text))
    (assert-false (search "stability-smoke =" text))
    (assert-false (search "run-fast-tests" text))))

(deftest flake-checks-tests-mirrors-test-app
  "checks.tests must delegate to the test app, ensuring CI matches `nix run .#test`."
  (let ((text (%checks-text)))
    (assert-true (search "apps.test.program" text))
    (assert-false (search "run-fast-tests" text))
    (assert-false (search "cl-cc-test/clos" text))))
