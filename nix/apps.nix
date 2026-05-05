{
  pkgs,
  lib,
  sbclWithCLCC,
  sbclWithTests,
  sbclBootstrap,
  dispatchSemFix ? null,
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
        if [ -z "''${CLCC_HEARTBEAT_SECONDS:-}" ] || ! printf '%s' "$CLCC_HEARTBEAT_SECONDS" | grep -Eq '^[0-9]+$'; then
          CLCC_HEARTBEAT_SECONDS=1
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
      lispPostLoadEvalForms = [
        ''(format t "# loading :cl-cc-test~%")''
        ''(handler-case (asdf:load-system :cl-cc-test) (error (e) (format *error-output* "~&FATAL: ~A~%" e) (uiop:quit 1)))''
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
