{
  pkgs,
  lib,
  sbclWithTests,
  sbclBootstrap,
  dispatchSemFix ? null,
  packagesDefault,
}:
{
  checks = {
    tests = pkgs.stdenvNoCC.mkDerivation {
      pname = "cl-cc-tests";
      version = "0.1.0";
      src = lib.fileset.toSource {
        root = ../.;
        fileset = lib.fileset.unions [
          ../packages
          ../tests
          ../cl-cc.asd
          ../cl-cc-test.asd
        ];
      };
      nativeBuildInputs = [ sbclWithTests ];
      buildPhase = ''
        export HOME="$TMPDIR"
        export CLCC_PBT_COUNT="''${CLCC_PBT_COUNT:-3}"
        export CLCC_TEST_TIMEOUT="''${CLCC_TEST_TIMEOUT:-180}"
        if [ -z "''${CLCC_HEARTBEAT_SECONDS:-}" ] || ! printf '%s' "$CLCC_HEARTBEAT_SECONDS" | grep -Eq '^[0-9]+$'; then
          CLCC_HEARTBEAT_SECONDS=30
        fi
        export CLCC_HEARTBEAT_SECONDS
        if [ -z "''${CLCC_SUITE_TIMEOUT:-}" ] || ! printf '%s' "$CLCC_SUITE_TIMEOUT" | grep -Eq '^[0-9]+$'; then
          CLCC_SUITE_TIMEOUT=600
        fi
        export CLCC_SUITE_TIMEOUT
        shell_timeout=$((CLCC_SUITE_TIMEOUT + 120))
        ${lib.optionalString (pkgs.stdenv.isDarwin && dispatchSemFix != null) ''
          export DYLD_INSERT_LIBRARIES="${dispatchSemFix}/lib/libdispatch_sem_fix.dylib"
        ''}
        ${pkgs.coreutils}/bin/timeout "$shell_timeout" sbcl --dynamic-space-size 4096 \
          --non-interactive \
          ${sbclBootstrap} \
          --load "cl-cc.asd" \
          --eval '(asdf:load-system :cl-cc-test :force t)' \
          --eval '(asdf:load-system :cl-cc-test/clos :force t)' \
          --eval '(uiop:symbol-call :cl-cc/test (quote run-fast-tests))'
      '';
      installPhase = "mkdir -p $out && touch $out/passed";
      meta.description = "cl-cc unit + integration test suite (checks.tests)";
    };

    build = packagesDefault;
  };
}
