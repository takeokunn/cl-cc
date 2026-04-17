{
  description = "CL-CC — self-hosting Common Lisp compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";

    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];

      imports = [ inputs.treefmt-nix.flakeModule ];

      flake.overlays.default = final: _prev: {
        cl-cc = self.packages.${final.system}.default;
      };

      perSystem =
        { pkgs, self', ... }:
        let
          inherit (pkgs) sbcl lib;

          # ── Common Lisp packages ─────────────────────────────────────────
          # Each ASDF system is its own derivation so FASL caches are
          # independent: touching `packages/engine/vm` never invalidates
          # `packages/foundation/bootstrap`.
          pkgSrc =
            subdir:
            lib.fileset.toSource {
              root = ./.;
              fileset = ./. + "/${subdir}";
            };

          systemSpec = {
            cl-cc-bootstrap = {
              subdir = "packages/foundation/bootstrap";
              deps = [ ];
            };
            cl-cc-ast = {
              subdir = "packages/foundation/ast";
              deps = [ ];
            };
            cl-cc-binary = {
              subdir = "packages/backend/binary";
              deps = [ ];
            };
            cl-cc-runtime = {
              subdir = "packages/backend/runtime";
              deps = [ ];
            };
            cl-cc-bytecode = {
              subdir = "packages/backend/bytecode";
              deps = [ ];
            };
            cl-cc-ir = {
              subdir = "packages/foundation/ir";
              deps = [ ];
            };
            cl-cc-mir = {
              subdir = "packages/foundation/mir";
              deps = [ ];
            };
            cl-cc-prolog = {
              subdir = "packages/prolog/prolog";
              deps = [ "cl-cc-bootstrap" ];
            };
            cl-cc-type = {
              subdir = "packages/type/type";
              deps = [ "cl-cc-ast" ];
            };
            cl-cc-parse = {
              subdir = "packages/frontend/parse";
              deps = [
                "cl-cc-ast"
                "cl-cc-bootstrap"
              ];
            };
            cl-cc-vm = {
              subdir = "packages/engine/vm";
              deps = [ "cl-cc-bootstrap" ];
            };
            cl-cc-optimize = {
              subdir = "packages/engine/optimize";
              deps = [
                "cl-cc-vm"
                "cl-cc-prolog"
                "cl-cc-type"
              ];
            };
            cl-cc-emit = {
              subdir = "packages/backend/emit";
              deps = [
                "cl-cc-vm"
                "cl-cc-mir"
                "cl-cc-optimize"
              ];
            };
            cl-cc-expand = {
              subdir = "packages/frontend/expand";
              deps = [
                "cl-cc-bootstrap"
                "cl-cc-ast"
                "cl-cc-prolog"
                "cl-cc-parse"
                "cl-cc-type"
              ];
            };
            cl-cc-compile = {
              subdir = "packages/engine/compile";
              deps = [
                "cl-cc-bootstrap"
                "cl-cc-ast"
                "cl-cc-prolog"
                "cl-cc-parse"
                "cl-cc-type"
                "cl-cc-optimize"
                "cl-cc-vm"
                "cl-cc-emit"
                "cl-cc-expand"
              ];
            };
          };

          asdfSystems = lib.fix (
            sys:
            lib.mapAttrs (
              name:
              { subdir, deps }:
              sbcl.buildASDFSystem {
                pname = name;
                version = "0.1.0";
                src = pkgSrc subdir;
                systems = [ name ];
                lispLibs = map (n: sys.${n}) deps;
              }
            ) systemSpec
          );

          sbclWithCLCC = sbcl.withPackages (_: lib.attrValues asdfSystems);

          # ── App scaffolding ──────────────────────────────────────────────
          sbclBin = "${sbclWithCLCC}/bin/sbcl";
          sbclFlags = "--dynamic-space-size 4096";
          sbclBootstrap = "--eval '(require :asdf)' --load cl-cc.asd";
          sbclCmd = "${sbclBin} ${sbclFlags} --non-interactive ${sbclBootstrap}";

          cwdGuard = ''
            if [ ! -f ./cl-cc.asd ]; then
              echo "cl-cc: run from the project root (cl-cc.asd not found in $PWD)" >&2
              exit 1
            fi
          '';

          faslCacheCleaner = ''
            ${lib.getExe pkgs.perl} -e '
              use strict;
              use warnings;
              use File::Path qw(remove_tree);
              use File::Find;
              my $pwd = $ENV{"PWD"} // "";
              exit 0 unless length $pwd;
              my $home = $ENV{"HOME"} // "";
              exit 0 unless length $home;
              my $root = $home . "/.cache/common-lisp";
              exit 0 unless -d $root;
              my $needle = $pwd . "/";
              my @targets;
              find(sub {
                return unless -d $_;
                my $p = $File::Find::name;
                push @targets, $p if index($p, $needle) >= 0;
              }, $root);
              eval { remove_tree(@targets) if @targets; 1 } or warn $@;
            ' || true
          '';

          pbtSanitize = ''
            if [ -z "''${CLCC_PBT_COUNT:-}" ] || ! printf '%s' "$CLCC_PBT_COUNT" | grep -Eq '^[0-9]+$'; then
              CLCC_PBT_COUNT=3
            fi
            export CLCC_PBT_COUNT
          '';

          mkApp = name: text: {
            type = "app";
            meta.description = "cl-cc ${name} app (run via `nix run .#${name}`)";
            program = "${pkgs.writeShellScript "cl-cc-${name}" text}";
          };
        in
        {
          # ── Packages ─────────────────────────────────────────────────────
          packages = asdfSystems // {
            default = pkgs.stdenvNoCC.mkDerivation {
              pname = "cl-cc";
              version = "0.1.0";
              src = lib.fileset.toSource {
                root = ./.;
                fileset = lib.fileset.unions [
                  ./packages
                  ./cl-cc.asd
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
                sbcl ${sbclFlags} \
                  --non-interactive \
                  --disable-debugger \
                  --eval '(require :asdf)' \
                  --load cl-cc.asd \
                  --eval '(asdf:load-system :cl-cc/bin)' \
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
          };

          # ── Apps ─────────────────────────────────────────────────────────
          apps = rec {
            default = repl;

            test = mkApp "test" ''
              set -euo pipefail
              ${cwdGuard}
              ${pbtSanitize}
              exec ${sbclCmd} \
                --load cl-cc-test.asd \
                --eval '(asdf:disable-output-translations)' \
                --eval '(asdf:load-system :cl-cc/test :force t)' \
                --eval '(uiop:symbol-call :cl-cc/test (quote run-tests))'
            '';

            coverage = mkApp "coverage" ''
              set -euo pipefail
              ${cwdGuard}
              rm -rf /tmp/cl-cc-coverage
              ${faslCacheCleaner}
              find . -maxdepth 6 -name "*.fasl" -delete
              exec ${sbclBin} ${sbclFlags} \
                --non-interactive \
                --eval '(require :asdf)' \
                --eval '(require :sb-cover)' \
                --eval '(declaim (optimize (sb-cover:store-coverage-data 3)))' \
                --load cl-cc.asd \
                --load cl-cc-test.asd \
                --eval '(asdf:load-system :cl-cc/test :force t)' \
                --eval '(cl-cc/test:run-suite (quote cl-cc/test::cl-cc-suite) :parallel nil :random nil :warm-stdlib t :coverage t)'
            '';

            selfhost = mkApp "selfhost" ''
              set -euo pipefail
              ${cwdGuard}
              exec ${self'.packages.default}/bin/cl-cc selfhost
            '';

            load = mkApp "load" ''
              set -euo pipefail
              ${cwdGuard}
              exec ${sbclBin} ${sbclFlags} ${sbclBootstrap} \
                --eval '(asdf:disable-output-translations)' \
                --eval '(asdf:load-system :cl-cc)'
            '';

            repl = mkApp "repl" ''
              set -euo pipefail
              ${cwdGuard}
              exec ${lib.getExe pkgs.rlwrap} ${sbclBin} ${sbclFlags} \
                --eval '(require :asdf)' \
                --load cl-cc.asd \
                --eval '(asdf:disable-output-translations)' \
                --eval '(asdf:load-system :cl-cc)'
            '';
          };

          # ── Checks ───────────────────────────────────────────────────────
          checks = {
            tests = pkgs.stdenvNoCC.mkDerivation {
              pname = "cl-cc-tests";
              version = "0.1.0";
              src = lib.fileset.toSource {
                root = ./.;
                fileset = lib.fileset.unions [
                  ./packages
                  ./tests
                  ./cl-cc.asd
                  ./cl-cc-test.asd
                ];
              };
              nativeBuildInputs = [ sbclWithCLCC ];
              buildPhase = ''
                export HOME="$TMPDIR"
                export CLCC_PBT_COUNT="''${CLCC_PBT_COUNT:-3}"
                sbcl ${sbclFlags} \
                  --non-interactive \
                  --eval '(require :asdf)' \
                  --load "cl-cc.asd" \
                  --load "cl-cc-test.asd" \
                  --eval '(asdf:disable-output-translations)' \
                  --eval '(asdf:load-system :cl-cc/test :force t)' \
                  --eval '(asdf:load-system :cl-cc/test-clos :force t)' \
                  --eval '(uiop:symbol-call :cl-cc/test (quote run-tests))'
              '';
              installPhase = "mkdir -p $out && touch $out/passed";
              meta.description = "cl-cc unit + integration test suite (checks.tests)";
            };

            selfhost = pkgs.stdenvNoCC.mkDerivation {
              pname = "cl-cc-selfhost";
              version = "0.1.0";
              src = lib.fileset.toSource {
                root = ./.;
                fileset = lib.fileset.unions [
                  ./packages
                  ./cl-cc.asd
                ];
              };
              nativeBuildInputs = [ self'.packages.default ];
              buildPhase = "cl-cc selfhost";
              installPhase = "mkdir -p $out && touch $out/selfhost-verified";
              meta.description = "cl-cc self-hosting verification (checks.selfhost)";
            };

            build = self'.packages.default;
          };

          # ── Formatter ────────────────────────────────────────────────────
          treefmt = {
            projectRootFile = "flake.nix";
            programs = {
              nixfmt.enable = true;
              deadnix.enable = true;
              statix.enable = true;
              prettier.enable = true;
            };
            settings = {
              formatter.prettier.includes = [
                "*.md"
                "*.yml"
                "*.yaml"
                "*.json"
              ];
              global.excludes = [
                "cl-cc"
                "flake.lock"
                ".direnv/**"
                "*.fasl"
                "LICENSE*"
              ];
            };
          };

          # ── Dev shell ────────────────────────────────────────────────────
          devShells.default = pkgs.mkShell {
            packages = [
              sbclWithCLCC
              pkgs.rlwrap
            ];
            shellHook = ''
              cat <<'EOF'

                CL-CC Development Shell
                ------------------------

                Apps:
                  nix run .#test      Run the canonical test plan
                  nix run .#coverage  Run tests with sb-cover instrumentation
                  nix run .#selfhost  Build + verify self-hosting
                  nix run .#load      Load :cl-cc non-interactively
                  nix run .#repl      rlwrap'd SBCL with :cl-cc loaded  (nix run default)

                Nix:
                  nix flake check    Run checks.tests + .selfhost + .build + .treefmt
                  nix build          Build the standalone binary at ./result/bin/cl-cc
                  nix fmt            Format repo (nixfmt + deadnix + statix + prettier)
                  nix flake show     List all flake outputs

                CLI (after `nix build`, binary at ./result/bin/cl-cc):
                  cl-cc run example/hello.lisp
                  cl-cc eval "(+ 1 2)"
                  cl-cc compile example/hello.lisp -o hello
                  cl-cc selfhost
                  cl-cc help

              EOF
            '';
          };
        };
    };
}
