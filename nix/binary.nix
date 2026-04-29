{ pkgs, lib, sbclWithCLCC }:
{
  # Standalone cl-cc binary (./result/bin/cl-cc). Built from packages/ alone —
  # tests/ and cl-cc-test.asd are NOT bundled so the default build stays lean.
  default = pkgs.stdenvNoCC.mkDerivation {
    pname = "cl-cc";
    version = "0.1.0";
    src = lib.fileset.toSource {
      root = ../.;
      fileset = lib.fileset.unions [
        ../packages
        ../cl-cc.asd
      ];
    };
    strictDeps = true;
    enableParallelBuilding = true;
    nativeBuildInputs = [ sbclWithCLCC ];
    dontConfigure = true;
    dontPatch = true;
    dontStrip = true;
    dontPatchELF = true;
    dontPatchShebangs = true;
    buildPhase = ''
      runHook preBuild
      export HOME="$TMPDIR"
      sbcl --dynamic-space-size 4096 --non-interactive --disable-debugger \
        --eval '(require :asdf)' --load cl-cc.asd \
        --eval '(asdf:load-system :cl-cc-cli)' \
        --load packages/cli/scripts/build-cli.lisp
      runHook postBuild
    '';
    installPhase = ''
      runHook preInstall
      install -Dm755 cl-cc $out/bin/cl-cc
      runHook postInstall
    '';
    meta = {
      description = "CL-CC self-hosting Common Lisp compiler (standalone binary)";
      mainProgram = "cl-cc";
      platforms = lib.platforms.unix;
    };
  };
}
