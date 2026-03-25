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
          shellHook = ''
            echo ""
            echo "  CL-CC Development Shell"
            echo "  ------------------------"
            echo ""
            echo "  Build & Test:"
            echo "    make load              Load cl-cc system into SBCL"
            echo "    make test              Run all tests (~2000+)"
            echo "    make build             Build standalone binary ./cl-cc"
            echo "    make clean             Remove FASL caches"
            echo ""
            echo "  CLI Commands (after 'make build'):"
            echo "    ./cl-cc run example/hello.lisp            Hello world"
            echo "    ./cl-cc run example/arithmetic.lisp       Arithmetic & let bindings"
            echo "    ./cl-cc run example/factorial.lisp         Recursive functions"
            echo "    ./cl-cc run example/fibonacci.lisp         Fibonacci"
            echo "    ./cl-cc run example/closure.lisp           Closures & HOFs"
            echo "    ./cl-cc run example/list.lisp --stdlib     List ops with stdlib"
            echo "    ./cl-cc run example/clos.lisp              CLOS classes & methods"
            echo "    ./cl-cc run example/control-flow.lisp      Block, tagbody, handler-case"
            echo "    ./cl-cc run example/hello.php              PHP frontend"
            echo "    ./cl-cc eval \"(+ 1 2)\"                     Evaluate an expression"
            echo "    ./cl-cc check example/typecheck.lisp       Type inference"
            echo "    ./cl-cc compile example/hello.lisp -o hello  Compile to binary"
            echo "    ./cl-cc repl                                Interactive REPL"
            echo "    ./cl-cc selfhost                            Verify self-hosting"
            echo "    ./cl-cc help                                Show all commands"
            echo ""
          '';
        };

        packages.default = pkgs.stdenvNoCC.mkDerivation {
          pname = "cl-cc";
          version = "0.1.0";
          src = ./.;
          nativeBuildInputs = [ sbclTestEnv ];
          buildPhase = ''
            export HOME="$TMPDIR"
            sbcl --non-interactive \
              --eval '(require :asdf)' \
              --load "cl-cc.asd" \
              --eval '(asdf:load-system :cl-cc/bin)' \
              --load scripts/build-cli.lisp
          '';
          installPhase = ''
            mkdir -p $out/bin
            cp cl-cc $out/bin/cl-cc
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

        checks.selfhost = pkgs.stdenvNoCC.mkDerivation {
          pname = "cl-cc-selfhost";
          version = "0.1.0";
          src = ./.;
          nativeBuildInputs = [ sbclTestEnv ];
          buildPhase = ''
            export HOME="$TMPDIR"
            sbcl --non-interactive \
              --eval '(require :asdf)' \
              --load "cl-cc.asd" \
              --eval '(asdf:load-system :cl-cc/bin)' \
              --load scripts/build-cli.lisp
            ./cl-cc selfhost
          '';
          installPhase = ''
            mkdir -p $out
            touch $out/selfhost-verified
          '';
        };
      });
}
