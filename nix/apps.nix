{
  pkgs,
  lib,
  sbclWithCLCC,
  sbclWithTests,
  sbclBootstrap,
  dispatchSemFix ? null,
}:
let
  sbclFlags = "--dynamic-space-size 4096";

  cwdGuard = ''
    if [ ! -f ./cl-cc.asd ]; then
      echo "cl-cc: run from the project root (cl-cc.asd not found in $PWD)" >&2
      exit 1
    fi
  '';

  pbtSanitize = ''
    if [ -z "''${CLCC_PBT_COUNT:-}" ] || ! printf '%s' "$CLCC_PBT_COUNT" | grep -Eq '^[0-9]+$'; then
      CLCC_PBT_COUNT=3
    fi
    export CLCC_PBT_COUNT
  '';

  # Replaces the perl-based cleaner. The "*$PWD/*" pattern (with trailing slash)
  # mirrors the original perl `index($p, $pwd . "/")` boundary so we don't match
  # cached projects whose path happens to contain $PWD as a substring. ASDF's
  # cache mirrors absolute source paths under ~/.cache/common-lisp/<sbcl-ver>/,
  # so the substring search itself is required (anchoring on $HOME would miss
  # the version-prefixed segment).
  faslCacheCleaner = ''
    find "$HOME/.cache/common-lisp" -path "*$PWD/*" -name "*.fasl" -delete 2>/dev/null || true
    find . -name "*.fasl" -delete 2>/dev/null || true
  '';

  joinEvals = forms: lib.concatMapStringsSep " " (f: "--eval ${lib.escapeShellArg f}") forms;

  mkSbclScript =
    {
      name,
      description ? "cl-cc ${name} app (run via `nix run .#${name}`)",
      sbclVariant ? "production",
      sbclPkgOverride ? null,
      extraEnv ? "",
      lispPreLoadEvalForms ? [ ],
      lispPostLoadEvalForms ? [ ],
      loadAsdSystems ? [ ],
      forceReload ? false,
      disableOutputTranslations ? false,
      needsRlwrap ? false,
      enableDispatchSemFix ? false,
      enablePbtSanitize ? false,
      enableFaslCacheCleaner ? false,
      enableCwdGuard ? true,
      extraTimeoutSeconds ? 120,
      extraSbclFlags ? [ ],
      trailingScript ? "",
    }:
    let
      sbclPkg = if sbclPkgOverride != null then sbclPkgOverride else if sbclVariant == "tests" then sbclWithTests else sbclWithCLCC;
      sbclBin = "${sbclPkg}/bin/sbcl";
      rlwrapPrefix = lib.optionalString needsRlwrap "${lib.getExe pkgs.rlwrap} ";
      forceFlag = lib.optionalString forceReload " :force t";
      loadSystemEvals = lib.concatMapStringsSep " " (
        sys: "--eval ${lib.escapeShellArg "(asdf:load-system ${sys}${forceFlag})"}"
      ) loadAsdSystems;
      disableTranslationsFlag = lib.optionalString disableOutputTranslations
        "--eval '(asdf:disable-output-translations)'";
      dispatchExport =
        lib.optionalString (enableDispatchSemFix && pkgs.stdenv.isDarwin && dispatchSemFix != null)
          ''export DYLD_INSERT_LIBRARIES="${dispatchSemFix}/lib/libdispatch_sem_fix.dylib"'';
      shellSrc = ''
        set -euo pipefail
        ${lib.optionalString enableCwdGuard cwdGuard}
        ${lib.optionalString enablePbtSanitize pbtSanitize}
        ${lib.optionalString enableFaslCacheCleaner faslCacheCleaner}
        ${dispatchExport}
        ${extraEnv}
        export CLCC_TEST_TIMEOUT="''${CLCC_TEST_TIMEOUT:-180}"
        if [ -z "''${CLCC_HEARTBEAT_SECONDS:-}" ] || ! printf '%s' "$CLCC_HEARTBEAT_SECONDS" | grep -Eq '^[0-9]+$'; then
          CLCC_HEARTBEAT_SECONDS=30
        fi
        export CLCC_HEARTBEAT_SECONDS
        if [ -z "''${CLCC_SUITE_TIMEOUT:-}" ] || ! printf '%s' "$CLCC_SUITE_TIMEOUT" | grep -Eq '^[0-9]+$'; then
          CLCC_SUITE_TIMEOUT=600
        fi
        export CLCC_SUITE_TIMEOUT
        shell_timeout=$((CLCC_SUITE_TIMEOUT + ${toString extraTimeoutSeconds}))
        ${pkgs.coreutils}/bin/timeout "$shell_timeout" ${rlwrapPrefix}${sbclBin} ${sbclFlags} ${lib.concatStringsSep " " extraSbclFlags} \
          --non-interactive \
          ${joinEvals lispPreLoadEvalForms} \
          ${lib.removeSuffix "\n" sbclBootstrap} \
          ${disableTranslationsFlag} \
          --load cl-cc.asd \
          ${loadSystemEvals} \
          ${joinEvals lispPostLoadEvalForms}
        ${trailingScript}
      '';
    in
    {
      type = "app";
      meta.description = description;
      program = "${pkgs.writeShellScript "cl-cc-${name}" shellSrc}";
    };

  # Phase loop preserved verbatim from the original flake.nix:452-476.
  coverageDriver = ''(let ((failed nil))
                       (format t "# Coverage phase 1/3: unit~%") (finish-output)
                       (setf failed (or (cl-cc/test:run-suite (quote cl-cc/test::cl-cc-unit-suite)
                                                              :parallel nil :random nil :warm-stdlib nil :coverage t
                                                              :exclude-suites (list (quote cl-cc/test::cl-cc-coverage-unstable-unit-suite)
                                                                                    (quote cl-cc/test::vm-run-suite))
                                                              :quit-p nil)
                                        failed))
                       (format t "# Coverage phase 1/3 complete~%") (finish-output)
                       (format t "# Coverage phase 2/3: integration~%") (finish-output)
                       (setf failed (or (cl-cc/test:run-suite (quote cl-cc/test::cl-cc-integration-suite)
                                                              :parallel nil :random nil :warm-stdlib nil
                                                              :exclude-suites (list (quote cl-cc/test::cl-cc-integration-serial-suite)
                                                                                    (quote cl-cc/test::cl-cc-prolog-integration-suite))
                                                              :quit-p nil)
                                        failed))
                       (format t "# Coverage phase 2/3 complete~%") (finish-output)
                       (format t "# Coverage phase 3/3: pipeline-repl-serial~%") (finish-output)
                       (setf failed (or (cl-cc/test:run-suite (quote cl-cc/test::cl-cc-pipeline-repl-serial-suite)
                                                              :parallel nil :random nil :warm-stdlib nil :quit-p nil)
                                        failed))
                       (format t "# Coverage phase 3/3 complete~%") (finish-output)
                       (cl-cc/test::%print-coverage-report nil)
                       (when failed
                         (format t "# Coverage note: some instrumented test phases failed; final coverage gate is decided by the coverage threshold script.~%")
                         (finish-output))
                       (uiop:quit 0))'';

  apps = rec {
    default = repl;

    # `test` is the fast-feedback entry point: unit + integration tests, but
    # NOT the slow self-hosting suite (selfhost-slow-suite). The full canonical
    # run is exposed separately as `.#test-full`, which now completes
    # successfully and remains the repo’s broader verification path.
    test = mkSbclScript {
      name = "test";
      sbclVariant = "tests";
      enableDispatchSemFix = true;
      enablePbtSanitize = true;
      enableFaslCacheCleaner = true;
      lispPostLoadEvalForms = [
        ''(format t "# loading :cl-cc-test~%")''
        ''(handler-case (asdf:load-system :cl-cc-test) (error (e) (format *error-output* "~&FATAL: ~A~%" e) (uiop:quit 1)))''
        ''(format t "# starting fast test plan (unit + integration; selfhost-slow excluded; warm-stdlib enabled)~%")''
        ''(handler-case (uiop:symbol-call :cl-cc/test (quote run-fast-tests) :warm-stdlib t) (error (e) (format t "~&not ok - run-fast-tests fatal error: ~A~%" e) (format *error-output* "~&FATAL: ~A~%" e) (uiop:quit 1)))''
      ];
    };

    # `test-full` is the canonical full-suite entry point including the slow
    # self-hosting suite. This path is intentionally bounded by
    # CLCC_SUITE_TIMEOUT + extraTimeoutSeconds via mkSbclScript's shell timeout
    # wrapper, so it fails fast instead of hanging indefinitely.
    # Use `.#test` for normal feedback runs.
    test-full = mkSbclScript {
      name = "test-full";
      sbclVariant = "tests";
      enableDispatchSemFix = true;
      enablePbtSanitize = true;
      enableFaslCacheCleaner = true;
      lispPostLoadEvalForms = [
        ''(format t "# loading :cl-cc-test~%")''
        ''(handler-case (asdf:load-system :cl-cc-test) (error (e) (format *error-output* "~&FATAL: ~A~%" e) (uiop:quit 1)))''
        # Targeted output-translation: ONLY tests/e2e/ FASL writes go to
        # user-cache (read-only store path otherwise). Production systems and
        # integration tests still load from precompiled store FASLs.
        ''(let* ((cl-cc-test-dir (asdf:system-source-directory :cl-cc-test)) (e2e-dir (merge-pathnames "tests/e2e/" cl-cc-test-dir))) (asdf:initialize-output-translations `(:output-translations ((,e2e-dir :**/ :*.*.*) (:user-cache :implementation :**/ :*.*.*)) :inherit-configuration)))''
        ''(load (merge-pathnames "tests/e2e/selfhost-test-support.lisp" *default-pathname-defaults*))''
        ''(load (merge-pathnames "tests/e2e/selfhost-meta-tests.lisp" *default-pathname-defaults*))''
        ''(load (merge-pathnames "tests/e2e/selfhost-tests.lisp" *default-pathname-defaults*))''
        ''(format t "# starting full canonical test plan (fast + slow + e2e)~%")''
        ''(handler-case (uiop:symbol-call :cl-cc/test (quote run-tests)) (error (e) (format t "~&not ok - run-tests fatal error: ~A~%" e) (format *error-output* "~&FATAL: ~A~%" e) (uiop:quit 1)))''
      ];
    };

    # Coverage uses sbclVariant = "production" (NOT "tests") on purpose: sb-cover
    # only instruments code compiled AFTER the (declaim ...) form runs, so any
    # FASLs already baked into sbclWithTests would load uninstrumented. By using
    # the production-only closure and then loading test systems from source via
    # disable-output-translations, every test file gets compiled fresh with
    # instrumentation active and shows up in the HTML coverage report.
    coverage = mkSbclScript {
      name = "coverage";
      sbclPkgOverride = pkgs.sbcl;
      enableFaslCacheCleaner = true;
      extraEnv = ''
        export CLCC_PBT_COUNT="''${CLCC_PBT_COUNT:-0}"
        rm -rf /tmp/cl-cc-coverage
        find . -maxdepth 6 -name "*.fasl" -delete
      '';
      # MUST run BEFORE (require :asdf): sb-cover instrumentation has to be
      # active before any FASL load triggered by ASDF.
      lispPreLoadEvalForms = [
        "(require :sb-cover)"
        "(declaim (optimize (sb-cover:store-coverage-data 3)))"
        "(ignore-errors (setf sb-ext:*on-package-variance* nil))"
      ];
      disableOutputTranslations = true;
      lispPostLoadEvalForms = [
        ''(defun cl-user::load-source-system (name)
             (handler-bind ((warning #'muffle-warning)
                            #+sbcl
                            (sb-ext:defconstant-uneql
                             (lambda (c)
                               (declare (ignore c))
                               (let ((r (find-restart 'continue)))
                                 (when r (invoke-restart r))))))
               (asdf:operate 'asdf:load-source-op name)))''
        ''(defun cl-user::load-system-soft (name)
             (handler-bind ((warning #'muffle-warning)
                            #+sbcl
                            (sb-ext:defconstant-uneql
                             (lambda (c)
                               (declare (ignore c))
                               (let ((r (find-restart 'continue)))
                                 (when r (invoke-restart r))))))
               (asdf:load-system name)))''
        "(ignore-errors (asdf:clear-system :cl-cc-bootstrap))"
        "(ignore-errors (asdf:clear-system :cl-cc-ast))"
        "(ignore-errors (asdf:clear-system :cl-cc-binary))"
        "(ignore-errors (asdf:clear-system :cl-cc-runtime))"
        "(ignore-errors (asdf:clear-system :cl-cc-bytecode))"
        "(ignore-errors (asdf:clear-system :cl-cc-ir))"
        "(ignore-errors (asdf:clear-system :cl-cc-mir))"
        "(ignore-errors (asdf:clear-system :cl-cc-prolog))"
        "(ignore-errors (asdf:clear-system :cl-cc-type))"
        "(ignore-errors (asdf:clear-system :cl-cc-parse))"
        "(ignore-errors (asdf:clear-system :cl-cc-vm))"
        "(ignore-errors (asdf:clear-system :cl-cc-optimize))"
        "(ignore-errors (asdf:clear-system :cl-cc-emit))"
        "(ignore-errors (asdf:clear-system :cl-cc-expand))"
        "(ignore-errors (asdf:clear-system :cl-cc-compile))"
        "(ignore-errors (asdf:clear-system :cl-cc))"
        "(ignore-errors (asdf:clear-system :cl-cc-cli))"
        "(ignore-errors (asdf:clear-system :cl-cc-testing-framework))"
        "(ignore-errors (asdf:clear-system :cl-cc-test))"
        "(ignore-errors (asdf:clear-system :cl-cc-test/slow))"
        "(ignore-errors (asdf:clear-system :cl-cc-test/clos))"
        "(load (merge-pathnames \"packages/foundation/bootstrap/cl-cc-bootstrap.asd\" *default-pathname-defaults*))"
        "(load (merge-pathnames \"packages/foundation/ast/cl-cc-ast.asd\" *default-pathname-defaults*))"
        "(load (merge-pathnames \"packages/backend/binary/cl-cc-binary.asd\" *default-pathname-defaults*))"
        "(load (merge-pathnames \"packages/backend/runtime/cl-cc-runtime.asd\" *default-pathname-defaults*))"
        "(load (merge-pathnames \"packages/backend/bytecode/cl-cc-bytecode.asd\" *default-pathname-defaults*))"
        "(load (merge-pathnames \"packages/foundation/ir/cl-cc-ir.asd\" *default-pathname-defaults*))"
        "(load (merge-pathnames \"packages/foundation/mir/cl-cc-mir.asd\" *default-pathname-defaults*))"
        "(load (merge-pathnames \"packages/foundation/prolog/cl-cc-prolog.asd\" *default-pathname-defaults*))"
        "(load (merge-pathnames \"packages/foundation/type/cl-cc-type.asd\" *default-pathname-defaults*))"
        "(load (merge-pathnames \"packages/frontend/parse/cl-cc-parse.asd\" *default-pathname-defaults*))"
        "(load (merge-pathnames \"packages/engine/vm/cl-cc-vm.asd\" *default-pathname-defaults*))"
        "(load (merge-pathnames \"packages/engine/optimize/cl-cc-optimize.asd\" *default-pathname-defaults*))"
        "(load (merge-pathnames \"packages/backend/emit/cl-cc-emit.asd\" *default-pathname-defaults*))"
        "(load (merge-pathnames \"packages/frontend/expand/cl-cc-expand.asd\" *default-pathname-defaults*))"
        "(load (merge-pathnames \"packages/engine/compile/cl-cc-compile.asd\" *default-pathname-defaults*))"
        "(load (merge-pathnames \"packages/cli/cl-cc-cli.asd\" *default-pathname-defaults*))"
        "(load (merge-pathnames \"packages/testing/framework/cl-cc-testing-framework.asd\" *default-pathname-defaults*))"
        "(load (merge-pathnames \"cl-cc.asd\" *default-pathname-defaults*))"
        "(load (merge-pathnames \"cl-cc-test.asd\" *default-pathname-defaults*))"
        ''(let* ((root (truename *default-pathname-defaults*))
                 (nix-store-root #P"/nix/store/"))
             (asdf:initialize-output-translations
              `(:output-translations
                ((,root :**/ :*.*.*) (:user-cache :implementation :**/ :*.*.*))
                ((,nix-store-root :**/ :*.*.*) (:user-cache :implementation :**/ :*.*.*))
                :inherit-configuration)))''
        "(cl-user::load-source-system :cl-cc-bootstrap)"
        "(cl-user::load-source-system :cl-cc-ast)"
        "(cl-user::load-source-system :cl-cc-binary)"
        "(cl-user::load-source-system :cl-cc-runtime)"
        "(cl-user::load-source-system :cl-cc-bytecode)"
        "(cl-user::load-source-system :cl-cc-ir)"
        "(cl-user::load-source-system :cl-cc-mir)"
        "(cl-user::load-source-system :cl-cc-prolog)"
        "(cl-user::load-source-system :cl-cc-type)"
        "(cl-user::load-source-system :cl-cc-parse)"
        "(cl-user::load-source-system :cl-cc-vm)"
        "(cl-user::load-source-system :cl-cc-optimize)"
        "(cl-user::load-source-system :cl-cc-emit)"
        "(cl-user::load-source-system :cl-cc-expand)"
        "(cl-user::load-source-system :cl-cc-compile)"
        "(cl-user::load-source-system :cl-cc)"
        "(cl-user::load-system-soft :cl-cc-test)"
        coverageDriver
      ];
      trailingScript = "${lib.getExe pkgs.perl} ${./coverage-report.pl}";
    };

    perf-smoke = let
      script = pkgs.writeShellScript "cl-cc-perf-smoke" ''
        set -euo pipefail
        if [ ! -f ./cl-cc.asd ]; then
          echo "cl-cc: run from the project root (cl-cc.asd not found in $PWD)" >&2
          exit 1
        fi
        out=''${CLCC_PERF_SMOKE_OUT:-/tmp/cl-cc-perf-smoke.txt}
        fast_budget=''${CLCC_PERF_FAST_MAX_SECONDS:-600}
        full_budget=''${CLCC_PERF_FULL_MAX_SECONDS:-1200}
        : > "$out"
        run_measure() {
          label="$1"
          budget="$2"
          shift
          shift
          echo "## $label" | tee -a "$out"
          timing=$(${pkgs.time}/bin/time -p "$@" 2>&1 | tee -a "$out")
          real=$(printf '%s\n' "$timing" | ${pkgs.gnugrep}/bin/grep '^real ' | ${pkgs.gawk}/bin/awk '{print $2}')
          if [ -n "$real" ] && ${pkgs.gawk}/bin/awk -v real="$real" -v budget="$budget" 'BEGIN { exit !(real > budget) }'; then
            echo "Performance budget exceeded for $label: ''${real}s > ''${budget}s" | tee -a "$out" >&2
            exit 1
          fi
          echo | tee -a "$out"
        }
        run_measure fast "$fast_budget" ${pkgs.nix}/bin/nix run .#test
        run_measure full "$full_budget" ${pkgs.nix}/bin/nix run .#test-full
        echo "Performance smoke report written to $out"
      '';
    in {
      type = "app";
      meta.description = "Measure fast/full gate runtimes against budgets";
      program = "${script}";
    };

    stability-smoke = let
      script = pkgs.writeShellScript "cl-cc-stability-smoke" ''
        set -euo pipefail
        if [ ! -f ./cl-cc.asd ]; then
          echo "cl-cc: run from the project root (cl-cc.asd not found in $PWD)" >&2
          exit 1
        fi
        out=''${CLCC_STABILITY_SMOKE_OUT:-/tmp/cl-cc-stability-smoke.txt}
        rounds=''${CLCC_STABILITY_ROUNDS:-2}
        : > "$out"
        i=1
        while [ "$i" -le "$rounds" ]; do
          echo "## fast round $i" | tee -a "$out"
          ${pkgs.nix}/bin/nix run .#test 2>&1 | tee -a "$out"
          echo | tee -a "$out"
          i=$((i+1))
        done
        echo "Stability smoke report written to $out"
      '';
    in {
      type = "app";
      meta.description = "Repeat the fast gate to spot flake regressions";
      program = "${script}";
    };

    load = mkSbclScript {
      name = "load";
      sbclVariant = "production";
      disableOutputTranslations = true;
      loadAsdSystems = [ ":cl-cc" ];
    };

    repl = mkSbclScript {
      name = "repl";
      sbclVariant = "production";
      needsRlwrap = true;
      disableOutputTranslations = true;
      loadAsdSystems = [ ":cl-cc" ];
    };
  };
in
{
  inherit mkSbclScript apps;
}
