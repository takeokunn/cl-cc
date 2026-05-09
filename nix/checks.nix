{
  pkgs,
  lib,
  sbclWithTests,
  apps,
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
          ../docs
          ../nix
          ../cl-cc.asd
          ../cl-cc-test.asd
        ];
      };
      nativeBuildInputs = [ sbclWithTests ];
      buildPhase = ''
        export HOME="$TMPDIR"
        ${apps.test.program}
      '';
      installPhase = "mkdir -p $out && touch $out/passed";
      meta.description = "cl-cc canonical fast unit test suite";
    };

    build = packagesDefault;
  };
}
