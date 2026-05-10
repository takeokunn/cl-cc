{
  pkgs,
  lib,
  sbclWithCLCC,
  sbclWithTests,
  sbclBootstrap,
  dispatchSemFix ? null,
  testImage,
}:
let
  sbclFlags = "--dynamic-space-size 8192";

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

  faslCacheCleaner = ''
    rm -rf "$HOME/.cache/common-lisp"
    mkdir -p "$HOME/.cache/common-lisp"
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
      loadProjectAsd ? true,
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
      sbclPkg =
        if sbclPkgOverride != null then
          sbclPkgOverride
        else if sbclVariant == "tests" then
          sbclWithTests
        else
          sbclWithCLCC;
      sbclBin = "${sbclPkg}/bin/sbcl";
      rlwrapPrefix = lib.optionalString needsRlwrap "${lib.getExe pkgs.rlwrap} ";
      forceFlag = lib.optionalString forceReload " :force t";
      loadSystemEvals = lib.concatMapStringsSep " " (
        sys: "--eval ${lib.escapeShellArg "(asdf:load-system ${sys}${forceFlag})"}"
      ) loadAsdSystems;
      disableTranslationsFlag = lib.optionalString disableOutputTranslations "--eval '(asdf:disable-output-translations)'";
      dispatchExport = lib.optionalString (
        enableDispatchSemFix && pkgs.stdenv.isDarwin && dispatchSemFix != null
      ) ''export DYLD_INSERT_LIBRARIES="${dispatchSemFix}/lib/libdispatch_sem_fix.dylib"'';
      shellSrc = ''
        set -euo pipefail
        ${lib.optionalString enableCwdGuard cwdGuard}
        ${lib.optionalString enablePbtSanitize pbtSanitize}
        ${lib.optionalString enableFaslCacheCleaner faslCacheCleaner}
        ${dispatchExport}
        ${extraEnv}
        export CLCC_TEST_TIMEOUT="''${CLCC_TEST_TIMEOUT:-10}"
        export CLCC_SUITE_TIMEOUT="''${CLCC_SUITE_TIMEOUT:-1500}"
        if ! printf '%s' "$CLCC_TEST_TIMEOUT"  | grep -Eq '^[0-9]+$'; then CLCC_TEST_TIMEOUT=10;   fi
        if ! printf '%s' "$CLCC_SUITE_TIMEOUT" | grep -Eq '^[0-9]+$'; then CLCC_SUITE_TIMEOUT=1500; fi
        shell_timeout=$((CLCC_SUITE_TIMEOUT + ${toString extraTimeoutSeconds}))
        ${pkgs.coreutils}/bin/timeout --kill-after=30 "$shell_timeout" ${rlwrapPrefix}${sbclBin} ${sbclFlags} ${lib.concatStringsSep " " extraSbclFlags} \
          --non-interactive \
          ${joinEvals lispPreLoadEvalForms} \
          ${lib.removeSuffix "\n" sbclBootstrap} \
          ${disableTranslationsFlag} \
          ${lib.optionalString loadProjectAsd "--load cl-cc.asd"} \
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

  apps = rec {
    default = repl;

    # `test` runs the canonical fast unit plan via `cl-cc/test:run-tests`.
    # `nix flake check` invokes this same program through `checks.tests`.
    # Warm-cache reuse: the FASL cleaner is disabled by default so repeat
    # invocations stay fast. Manual cleanup: `rm -rf ~/.cache/common-lisp/`.
    test = mkSbclScript {
      name = "test";
      description = "Run the canonical fast unit test plan";
      sbclVariant = "tests";
      loadProjectAsd = false;
      enableDispatchSemFix = true;
      enablePbtSanitize = true;
      enableFaslCacheCleaner = false;
      # Load the pre-compiled core image (save-lisp-and-die snapshot).
      # The core has :cl-cc, :cl-cc-cli, :cl-cc-testing-framework pre-loaded and
      # warm-stdlib-cache pre-initialized, so the heavy ASDF loading is skipped.
      # :cl-cc-test is NOT in the core (test-file top-level forms must not bake
      # Nix sandbox paths into globals), so it is loaded fresh after CWD reset.
      # Cap worker count to 4: ≥8 workers trigger GC safepoint contention on macOS 26 ARM64
      # (SBCL 2.6.1). Concurrent SBCL compiler calls (from compiler-macro eval) are now
      # serialised via *macro-eval-fn* mutex, so 4 workers are safe. Users may override
      # upward via CL_CC_TEST_WORKERS=N nix run .#test.
      extraSbclFlags = [
        "--core"
        "${testImage}/cl-cc-test.core"
      ];
      extraEnv = ''export CL_CC_TEST_WORKERS="''${CL_CC_TEST_WORKERS:-4}"'';
      lispPostLoadEvalForms = [
        # *default-pathname-defaults* and uiop:*temporary-directory* are baked
        # into the core at build-sandbox time; reset both to real runtime values.
        # uiop:*temporary-directory* must be set via (uiop:temporary-directory) — NOT nil.
        # Setting it to nil and then passing it as :defaults to make-pathname hangs SBCL
        # 2.6.1 on macOS ARM64; (uiop:temporary-directory) reads TMPDIR from the runtime
        # environment and caches the result in *temporary-directory*.
        ''(setf *default-pathname-defaults* (uiop:getcwd))''
        ''(setf uiop:*temporary-directory* (uiop:temporary-directory))''
        # Load :cl-cc-test FASLs (pre-compiled via sbclWithTests) after CWD reset
        # so any top-level path computations in test files see the correct CWD.
        ''(format t "# loading :cl-cc-test~%")''
        ''(handler-case (asdf:load-system :cl-cc-test) (error (e) (format *error-output* "~&FATAL: ~A~%" e) (uiop:quit 1)))''
        # Reset *macro-eval-fn* to a mutex-wrapped #'eval so test bodies run under host CL,
        # not the cl-cc VM.  pipeline-selfhost.lisp sets it to #'our-eval at
        # load time; leaving it as our-eval causes sb-ext:with-timeout interrupts
        # to be swallowed inside the VM loop, hanging test workers indefinitely.
        # The mutex serialises concurrent SBCL compiler invocations: on macOS 26 ARM64
        # SBCL 2.6.1, four or more parallel workers calling eval simultaneously
        # (via compiler-macro expansion in invoke-registered-expander) deadlock on
        # GC safepoints while the SBCL compiler holds internal locks.  The lock is
        # captured by the closure; %with-isolated-macro-environment identity-rebinds
        # *macro-eval-fn* so all threads share the same mutex.
        ''(let ((lock (sb-thread:make-mutex :name "macro-eval-lock"))) (setf cl-cc/expand:*macro-eval-fn* (lambda (form) (sb-thread:with-mutex (lock) (eval form)))))''
        # Pre-warm BOTH stdlib caches in the main thread (single-threaded, safe).
        # The core bakes *stdlib-expanded-cache-eval-fn* = #'our-eval and a
        # *stdlib-vm-snapshot* compiled under our-eval.  After the *macro-eval-fn*
        # reset above, both caches are stale.  Without pre-warming, all 4 parallel
        # workers simultaneously see cache misses and race to rebuild unprotected
        # globals (*stdlib-expanded-cache*, *stdlib-vm-snapshot*, etc.).  On macOS
        # ARM64 SBCL, concurrent large allocations (857-line stdlib) during GC
        # safepoint windows can deadlock threads waiting on watchdog-lock.
        # warm-stdlib-cache rebuilds both caches under *macro-eval-fn* = #'eval
        # so workers see cache HITs and bypass both rebuild paths entirely.
        ''(handler-case (progn (format t "# warming stdlib cache~%") (cl-cc:warm-stdlib-cache) (format t "# stdlib cache ready~%")) (error (e) (format *error-output* "~&FATAL: stdlib cache warm failed: ~A~%" e) (uiop:quit 1)))''
        ''(format t "# starting fast test plan (unit)~%")''
        ''(handler-case (uiop:symbol-call :cl-cc/test (quote run-tests)) (error (e) (format t "~&not ok - run-tests fatal error: ~A~%" e) (format *error-output* "~&FATAL: ~A~%" e) (uiop:quit 1)))''
      ];
    };

    coverage = mkSbclScript {
      name = "coverage";
      description = "Run the canonical test plan with sb-cover instrumentation";
      sbclVariant = "tests";
      enableDispatchSemFix = true;
      enablePbtSanitize = true;
      enableFaslCacheCleaner = true;
      forceReload = true;
      extraTimeoutSeconds = 600;
      lispPreLoadEvalForms = [
        "(require :sb-cover)"
        "(declaim (optimize sb-cover:store-coverage-data))"
        "(require :asdf)"
        "(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))"
        ''(asdf:initialize-output-translations (quote (:output-translations (t (:home ".cache" "common-lisp" :implementation)) :ignore-inherited-configuration)))''
      ];
      lispPostLoadEvalForms = [
        ''(load (merge-pathnames "cl-cc-test.asd" *default-pathname-defaults*))''
        "(asdf:load-system :cl-cc-test :force t)"
        ''(format t "# starting coverage test plan (sb-cover + unit)~%")''
        ''(handler-case (let ((failed (cl-cc/test:run-suite (quote cl-cc/test:cl-cc-suite) :parallel nil :random nil :coverage t :exclude-suites (quote (cl-cc/test:cl-cc-integration-suite cl-cc/test:cl-cc-e2e-suite)) :quit-p nil))) (declaim (optimize (sb-cover:store-coverage-data 0))) (uiop:quit (if failed 1 0))) (error (e) (format t "~&not ok - coverage fatal error: ~A~%" e) (format *error-output* "~&FATAL: ~A~%" e) (uiop:quit 1)))''
      ];
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
