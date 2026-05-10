{ pkgs, lib }:
lib.optionalAttrs pkgs.stdenv.isDarwin {
  dispatchSemFix = pkgs.stdenvNoCC.mkDerivation {
    pname = "dispatch-sem-fix";
    version = "5";
    src = lib.fileset.toSource {
      root = ./dispatch-sem-fix;
      fileset = ./dispatch-sem-fix/dispatch_sem_fix.c;
    };
    nativeBuildInputs = [ pkgs.clang ];
    buildPhase = ''
      runHook preBuild
      clang -dynamiclib -o libdispatch_sem_fix.dylib dispatch_sem_fix.c
      runHook postBuild
    '';
    installPhase = ''
      runHook preInstall
      mkdir -p $out/lib
      cp libdispatch_sem_fix.dylib $out/lib/
      runHook postInstall
    '';
  };
}
