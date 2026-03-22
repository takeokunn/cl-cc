{
  description = "CL-CC bootstrap compiler (zero dependency core)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        sbclTestEnv = pkgs.sbcl.withPackages (ps: [ ps.fiveam ]);
      in {
        devShells.default = pkgs.mkShell {
          packages = [ sbclTestEnv pkgs.rlwrap ];
        };

        packages.default = pkgs.stdenvNoCC.mkDerivation {
          pname = "cl-cc";
          version = "0.1.0";
          src = ./.;
          nativeBuildInputs = [ sbclTestEnv ];
          buildPhase = ''
            export HOME="$TMPDIR"
            pwd
            ls -la
            sbcl --non-interactive \
              --eval '(require :asdf)' \
              --load "cl-cc.asd" \
              --eval '(asdf:load-system :cl-cc)'
          '';
          installPhase = ''
            mkdir -p $out/share/cl-cc
            cp -r src cl-cc.asd run-tests.lisp README.md $out/share/cl-cc/
          '';
        };

        checks.tests = pkgs.stdenvNoCC.mkDerivation {
          pname = "cl-cc-tests";
          version = "0.1.0";
          src = ./.;
          nativeBuildInputs = [ sbclTestEnv ];
          buildPhase = ''
            export HOME="$TMPDIR"
            sbcl --non-interactive \
              --eval '(require :asdf)' \
              --load "cl-cc.asd" \
              --eval '(asdf:test-system :cl-cc/test)'
          '';
          installPhase = ''
            mkdir -p $out
            touch $out/passed
          '';
        };
      });
}
