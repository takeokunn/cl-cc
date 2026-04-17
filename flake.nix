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

        # ─── Shared bootstrap strings ───────────────────────────────────────
        # Kept on a single line so shell line-continuation in app scripts
        # concatenates cleanly. SBCL rule: C runtime flags (--dynamic-space-size)
        # MUST precede Lisp flags (--non-interactive, --eval, --load).
        sbclBin = "${sbclTestEnv}/bin/sbcl";
        sbclRuntimeFlags = "--dynamic-space-size 4096";
        sbclLispBootstrap = "--eval '(require :asdf)' --load cl-cc.asd";
        sbclCmd = "${sbclBin} ${sbclRuntimeFlags} --non-interactive ${sbclLispBootstrap}";
        sbclCmdInteractive = "${sbclBin} ${sbclRuntimeFlags} ${sbclLispBootstrap}";

        # Guard snippet: refuse to run outside the project root so destructive
        # app scripts (clean) and loader scripts (everything using cl-cc.asd)
        # can't traverse a user's $HOME.
        cwdGuard = ''
          if [ ! -f ./cl-cc.asd ]; then
            echo "cl-cc: run from the project root (cl-cc.asd not found in $PWD)" >&2
            exit 1
          fi
        '';

        # Slash-anchored FASL cache cleaner. Bare `index($p, $PWD)` was unsafe:
        #  - empty $PWD matches everything (wiped the entire cache).
        #  - $PWD=/work/cl falsely matches /work/cl-sibling.
        # Fix: require $PWD non-empty, $HOME non-empty, cache dir extant, and
        # match "$PWD/" (trailing slash) so /work/cl can't match /work/cl-foo.
        faslCacheCleaner = ''
          ${pkgs.perl}/bin/perl -e '
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

        # Bash-only CLCC_PBT_COUNT sanitizer: accept a non-negative integer,
        # otherwise reset to the default.
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

        # ─── App scripts ────────────────────────────────────────────────────
        testScript = ''
          set -euo pipefail
          ${cwdGuard}
          ${pbtSanitize}
          exec ${sbclCmd} \
            --eval '(asdf:disable-output-translations)' \
            --eval '(asdf:load-system :cl-cc/test :force t)' \
            --eval '(uiop:symbol-call :cl-cc/test (quote run-tests))'
        '';

        coverageScript = ''
          set -euo pipefail
          ${cwdGuard}
          rm -rf /tmp/cl-cc-coverage
          ${faslCacheCleaner}
          find . -maxdepth 6 -name "*.fasl" -delete
          exec ${sbclTestEnv}/bin/sbcl \
            --dynamic-space-size 4096 \
            --non-interactive \
            --eval '(require :asdf)' \
            --eval '(require :sb-cover)' \
            --eval '(declaim (optimize (sb-cover:store-coverage-data 3)))' \
            --load cl-cc.asd \
            --eval '(asdf:load-system :cl-cc/test :force t)' \
            --eval '(cl-cc/test:run-suite (quote cl-cc/test::cl-cc-suite) :parallel nil :random nil :warm-stdlib t :coverage t)'
        '';

        buildScript = ''
          set -euo pipefail
          ${cwdGuard}
          exec ${sbclCmd} \
            --eval '(asdf:disable-output-translations)' \
            --eval '(asdf:load-system :cl-cc/bin)' \
            --load scripts/build-cli.lisp
        '';

        loadScript = ''
          set -euo pipefail
          ${cwdGuard}
          exec ${sbclCmdInteractive} \
            --eval '(asdf:disable-output-translations)' \
            --eval '(asdf:load-system :cl-cc)'
        '';

        replScript = ''
          set -euo pipefail
          ${cwdGuard}
          exec ${pkgs.rlwrap}/bin/rlwrap ${sbclTestEnv}/bin/sbcl \
            --dynamic-space-size 4096 \
            --eval '(require :asdf)' \
            --load cl-cc.asd \
            --eval '(asdf:disable-output-translations)' \
            --eval '(asdf:load-system :cl-cc)'
        '';

        cleanScript = ''
          set -euo pipefail
          ${cwdGuard}
          rm -f ./*.fasl ./*.lib ./*.dex
          find . -maxdepth 6 -name "*.fasl" -delete
          ${faslCacheCleaner}
          echo "cl-cc: cleaned FASL caches"
        '';

        # ─── Sandbox-side build helper (packages.default + checks.selfhost) ─
        # `packages.default.buildPhase` and `checks.selfhost.buildPhase` are
        # otherwise byte-identical. Shared helper keeps them in lockstep.
        sandboxedBuildCli = ''
          export HOME="$TMPDIR"
          sbcl --dynamic-space-size 4096 \
            --non-interactive \
            --eval '(require :asdf)' \
            --load "cl-cc.asd" \
            --eval '(asdf:load-system :cl-cc/bin)' \
            --load scripts/build-cli.lisp
        '';

      in {
        # ─── Apps ───────────────────────────────────────────────────────────
        # Invoke via `nix run .#<name>` from the project root.
        apps = {
          test     = mkApp "test"     testScript;
          coverage = mkApp "coverage" coverageScript;
          build    = mkApp "build"    buildScript;
          load     = mkApp "load"     loadScript;
          repl     = mkApp "repl"     replScript;
          clean    = mkApp "clean"    cleanScript;
        };

        # ─── Dev shell ──────────────────────────────────────────────────────
        devShells.default = pkgs.mkShell {
          packages = [ sbclTestEnv pkgs.rlwrap ];
          shellHook = ''
            echo ""
            echo "  CL-CC Development Shell"
            echo "  ------------------------"
            echo ""
            echo "  Build & Test (run via 'nix run .#<app>'):"
            echo "    nix run .#test        Run the canonical test plan"
            echo "    nix run .#build       Build standalone binary ./cl-cc"
            echo "    nix run .#coverage    Run tests with sb-cover instrumentation"
            echo "    nix run .#load        Load :cl-cc non-interactively"
            echo "    nix run .#repl        rlwrap'd SBCL with :cl-cc loaded"
            echo "    nix run .#clean       Remove FASL caches"
            echo ""
            echo "  Nix workflows:"
            echo "    nix flake check       Run checks.tests + checks.selfhost"
            echo "    nix build             Build ./result/bin/cl-cc via packages.default"
            echo "    nix flake show        List all flake outputs"
            echo ""
            echo "  CLI Commands (after 'nix run .#build'):"
            echo "    ./cl-cc run example/hello.lisp            Hello world"
            echo "    ./cl-cc run example/arithmetic.lisp       Arithmetic & let bindings"
            echo "    ./cl-cc run example/factorial.lisp        Recursive functions"
            echo "    ./cl-cc run example/fibonacci.lisp        Fibonacci"
            echo "    ./cl-cc run example/closure.lisp          Closures & HOFs"
            echo "    ./cl-cc run example/list.lisp --stdlib    List ops with stdlib"
            echo "    ./cl-cc run example/clos.lisp             CLOS classes & methods"
            echo "    ./cl-cc run example/control-flow.lisp     Block, tagbody, handler-case"
            echo "    ./cl-cc run example/hello.php             PHP frontend"
            echo "    ./cl-cc eval \"(+ 1 2)\"                    Evaluate an expression"
            echo "    ./cl-cc check example/typecheck.lisp      Type inference"
            echo "    ./cl-cc compile example/hello.lisp -o hello  Compile to binary"
            echo "    ./cl-cc repl                              Interactive REPL"
            echo "    ./cl-cc selfhost                          Verify self-hosting"
            echo "    ./cl-cc help                              Show all commands"
            echo ""
          '';
        };

        # ─── Package (standalone binary) ────────────────────────────────────
        packages.default = pkgs.stdenvNoCC.mkDerivation {
          pname = "cl-cc";
          version = "0.1.0";
          src = ./.;
          nativeBuildInputs = [ sbclTestEnv ];
          buildPhase = sandboxedBuildCli;
          installPhase = ''
            mkdir -p $out/bin
            cp cl-cc $out/bin/cl-cc
          '';
          meta = {
            description = "CL-CC self-hosting Common Lisp compiler (standalone binary)";
            mainProgram = "cl-cc";
          };
        };

        # ─── Checks (runnable via `nix flake check`) ────────────────────────
        # `apps.test` (force reload + explicit run-tests) and `checks.tests`
        # must stay in lockstep — stale FASL cannot silently green-light CI.
        checks.tests = pkgs.stdenvNoCC.mkDerivation {
          pname = "cl-cc-tests";
          version = "0.1.0";
          src = ./.;
          nativeBuildInputs = [ sbclTestEnv ];
          buildPhase = ''
            export HOME="$TMPDIR"
            export CLCC_PBT_COUNT="''${CLCC_PBT_COUNT:-3}"
            sbcl --dynamic-space-size 4096 \
              --non-interactive \
              --eval '(require :asdf)' \
              --load "cl-cc.asd" \
              --eval '(asdf:disable-output-translations)' \
              --eval '(asdf:load-system :cl-cc/test :force t)' \
              --eval '(uiop:symbol-call :cl-cc/test (quote run-tests))'
          '';
          installPhase = ''
            mkdir -p $out
            touch $out/passed
          '';
          meta.description = "cl-cc unit + integration test suite (checks.tests)";
        };

        checks.selfhost = pkgs.stdenvNoCC.mkDerivation {
          pname = "cl-cc-selfhost";
          version = "0.1.0";
          src = ./.;
          nativeBuildInputs = [ sbclTestEnv ];
          buildPhase = ''
            ${sandboxedBuildCli}
            ./cl-cc selfhost
          '';
          installPhase = ''
            mkdir -p $out
            touch $out/selfhost-verified
          '';
          meta.description = "cl-cc self-hosting verification (checks.selfhost)";
        };
      });
}
