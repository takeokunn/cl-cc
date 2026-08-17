{
  description = "Self-hosting Common Lisp compiler targeting bytecode, native code, PHP, and JavaScript";

  inputs = {
    # nixos-unstable, not nixpkgs-unstable: it advances only after the NixOS
    # release tests pass, so it is less likely to land a broken build.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Every sibling is pinned to an immutable ref — a release tag where one
    # exists, a commit otherwise. A bare `github:nerima-lisp/cl-weave` follows
    # that repository's default branch, so an upstream push to main would break
    # this repository's CI with no change here and no warning.
    #
    # All of them are taken as PLAIN SOURCE TREES (`flake = false`) and built
    # with cl-cc's own `sbcl.buildASDFSystem` below, exactly the way cl-cc
    # builds its own packages/ subsystems. Two reasons, both still current:
    # several of these flakes declare Linux-only `systems`, so consuming their
    # `packages.<system>` output would break aarch64-darwin here; and evaluating
    # them as flakes would drag in their transitive inputs (paredit-cli and so
    # on) to obtain nothing but their source.
    #
    # PACKAGE_STANDARD.md requires `inputs.nixpkgs.follows` on every input. A
    # `flake = false` input HAS no inputs, and Nix rejects an override for one,
    # so the attribute is omitted here. The rule exists to stop each input from
    # locking its own nixpkgs and inflating flake.lock; taking source trees
    # achieves that outcome more completely, since no second nixpkgs enters the
    # lock at all. treefmt-nix, the one real flake input, carries `follows`.
    cl-prolog-kit = {
      url = "github:nerima-lisp/cl-prolog-kit/v1.5.0";
      flake = false;
    };
    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.0.0";
      flake = false;
    };
    # cl-parser-kit backs packages/parse's tokenizer/combinator/Pratt layer,
    # cl-dataflow-kit backs the packages/pipeline pass-graph orchestration,
    # cl-boundary-kit provides the testable I/O boundaries used by
    # packages/cli + packages/repl, cl-cli is the declarative argument parser
    # behind packages/cli, and cl-tty-kit provides the ANSI/screen/input
    # primitives for the interactive REPL.
    cl-parser-kit = {
      url = "github:nerima-lisp/cl-parser-kit/v1.0.0";
      flake = false;
    };
    cl-dataflow-kit = {
      url = "github:nerima-lisp/cl-dataflow-kit/v1.2.0";
      flake = false;
    };
    cl-boundary-kit = {
      url = "github:nerima-lisp/cl-boundary-kit/v0.6.0";
      flake = false;
    };
    # cl-log-kit backs cl-boundary-kit's logging boundary (split out of
    # cl-boundary-kit itself); pulled in the same way as the toolkits above.
    cl-log-kit = {
      url = "github:nerima-lisp/cl-log-kit/v1.0.0";
      flake = false;
    };
    cl-cli = {
      url = "github:nerima-lisp/cl-cli/v1.0.1";
      flake = false;
    };
    cl-tty-kit = {
      url = "github:nerima-lisp/cl-tty-kit/v1.0.0";
      flake = false;
    };
    cl-regex-kit = {
      url = "github:nerima-lisp/cl-regex-kit/v0.2.0";
      flake = false;
    };
    cl-host-kit = {
      url = "github:nerima-lisp/cl-host-kit/v0.2.1";
      flake = false;
    };
    # cl-cc-ast and cl-cc-type are the first subsystems split out of this
    # repository. Injected into the internal system graph under their original
    # names by nix/asdf-systems.nix, so every `deps = [ "cl-cc-ast" ... ]`
    # resolves to the external derivation. cl-cc-type depends on cl-cc-ast.
    #
    # Pinned by commit, not tag: neither repository has cut a release yet.
    # A commit is a stronger pin than a tag (a tag can be moved), so this
    # satisfies the invariant the tag rule exists to protect. Swap in `/v0.1.0`
    # once those releases are tagged.
    cl-cc-ast = {
      url = "github:nerima-lisp/cl-cc-ast/5bb81120a1acf3a0c125dac655549582f3eb137e";
      flake = false;
    };
    cl-cc-type = {
      url = "github:nerima-lisp/cl-cc-type/324b1c06b86a382f6561019d087d845667cf4606";
      flake = false;
    };
    cl-cc-binary = {
      url = "github:nerima-lisp/cl-cc-binary/3c81059218b0d99838d04dff7dbbb3dd8821a8ad";
      flake = false;
    };
    cl-cc-bootstrap = {
      url = "github:nerima-lisp/cl-cc-bootstrap/b1ff1defbba014ba7b312c882ee8a5e7cabb5cc3";
      flake = false;
    };
    cl-cc-vm = {
      url = "github:nerima-lisp/cl-cc-vm/670376ada1b5b1aa40e4f798764ac34c5cc1959b";
      flake = false;
    };
    cl-cc-mir = {
      url = "github:nerima-lisp/cl-cc-mir/9679e3a471df1bea446df9b22cb26a32e85c42c8";
      flake = false;
    };
    cl-cc-cps = {
      url = "github:nerima-lisp/cl-cc-cps/b09fdc12f8a181946319ac7a4cd2794466617db4";
      flake = false;
    };
    cl-cc-expand = {
      url = "github:nerima-lisp/cl-cc-expand/2628bd758f76df1513456641a358c63812e70aac";
      flake = false;
    };
    cl-cc-optimize = {
      url = "github:nerima-lisp/cl-cc-optimize/5414f773e8fb54855e12f9639b9cb79109a2dbbb";
      flake = false;
    };
    cl-cc-codegen-native = {
      url = "github:nerima-lisp/cl-cc-codegen-native/e58718ee08dd916054ca62c782d0c9435e22583e";
      flake = false;
    };
    cl-cc-parse = {
      url = "github:nerima-lisp/cl-cc-parse/7316dd82137290082fa1fe03300d3aa388d84117";
      flake = false;
    };
    cl-cc-php = {
      url = "github:nerima-lisp/cl-cc-php/c36a40dcf8ddff72c7eaeb3baee5154c9b27d190";
      flake = false;
    };
    cl-cc-javascript = {
      url = "github:nerima-lisp/cl-cc-javascript/777d5450681601860af20d5858c297e7d1af842b";
      flake = false;
    };
    cl-cc-runtime = {
      url = "github:nerima-lisp/cl-cc-runtime/d8d636b";
      flake = false;
    };
    # Pulled in by the standalone cl-cc-runtime, which took dependencies the
    # in-tree copy did not have. cl-process-kit needs cl-boundary-kit and
    # cl-log-kit, both already above; cl-json-kit is self-contained.
    cl-process-kit = {
      url = "github:nerima-lisp/cl-process-kit/v1.0.1";
      flake = false;
    };
    cl-json-kit = {
      url = "github:nerima-lisp/cl-json-kit/v1.0.0";
      flake = false;
    };
    cl-date-kit = {
      url = "github:nerima-lisp/cl-date-kit/040f6bf936c0dd946a0049eaa715e4c1c6a40eb5";
      flake = false;
    };
    cl-concurrent-kit = {
      url = "github:nerima-lisp/cl-concurrent-kit/c129a0b4be03ef2b1671ab208f9b390573eb0412";
      flake = false;
    };
  };

  # Plain `outputs` with `forAllSystems`, not flake-parts. The whole flake is
  # visible in one file this way: `nix flake show` output maps directly onto the
  # attribute names written below, with no module system deciding what appears.
  outputs =
    inputs@{
      self,
      nixpkgs,
      treefmt-nix,
      ...
    }:
    let
      # x86_64-linux is verified by CI; aarch64-darwin is verified by the
      # development machine on every local `nix flake check`. aarch64-linux and
      # x86_64-darwin are not declared because nothing runs them (ADR-0078).
      # `ci.yml` deliberately omits --all-systems: the runner is x86_64-linux
      # and would fail evaluating the darwin derivations.
      # Only what is verified, and CI verifies exactly one platform:
      # x86_64-linux. aarch64-darwin was dropped in the 2026-08-01 revision of
      # PACKAGE_STANDARD.md -- its only gate was the maintainer remembering to
      # run `nix flake check` locally, and a gate nobody can observe being
      # skipped is not a gate.
      #
      # Consequence, accepted deliberately: every per-system output --
      # packages, checks, apps AND devShells -- is generated from this one
      # list, so `nix develop` and `nix build` do not work on macOS.
      # Development happens on Linux.
      systems = [
        "x86_64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;

      # Single source of truth for a package's version: the `:version` form in
      # its .asd. Before this, the flake said 0.8.0, cl-cc.asd said 0.1.0 and
      # the newest tag was v0.1.0 — three answers to one question, and nothing
      # to say which was right. A release now edits one line.
      #
      # Nix regexes are whole-string anchored and `.` never spans newlines, so
      # the version is extracted line-by-line rather than with a single
      # multi-line match.
      asdVersion =
        asd:
        let
          lines = nixpkgs.lib.splitString "\n" (builtins.readFile asd);
          versionLine = builtins.head (
            builtins.filter (line: builtins.match "[[:space:]]*:version \"[^\"]*\"" line != null) lines
          );
        in
        builtins.head (builtins.match "[[:space:]]*:version \"([^\"]*)\"" versionLine);

      version = asdVersion ./cl-cc.asd;

      # The same treatment for every sibling: read the version out of the source
      # tree actually being built, rather than restating it here where it can
      # (and did) fall behind the pinned ref.
      siblingVersion = name: asdVersion "${inputs.${name}}/${name}.asd";

      # treefmt drives `nix fmt` and the `checks.<system>.formatting` gate.
      # Scope is Nix only. Prettier over *.md/*.yml used to be enabled here; it
      # is not any more, because a YAML formatter rewrites the GitHub Actions
      # `on:` key into the boolean `true`, and reformatting Markdown would churn
      # every page under docs/ for no review value.
      treefmtEval = forAllSystems (
        system:
        treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} {
          projectRootFile = "flake.nix";
          programs.nixfmt.enable = true;
          settings.global.excludes = [
            "flake.lock"
            "LICENSE*"
          ];
        }
      );

      # Everything system-dependent is assembled once per system here, and the
      # top-level outputs below only project attributes out of it. Assembling it
      # separately inside `packages`, `checks`, `apps` and `devShells` would
      # re-evaluate the entire ASDF system graph four times.
      perSystem = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          inherit (pkgs) lib;

          dispatchModule = import ./nix/dispatch-sem-fix.nix { inherit pkgs lib; };
          dispatchSemFix = dispatchModule.dispatchSemFix or null;
          sbclModule = import ./nix/sbcl.nix { inherit pkgs; };
          inherit (sbclModule) sbcl sbclBootstrap;

          # Sibling toolkits, built from the pinned source trees. The `version`
          # of each mirrors the tag or commit its input is pinned to.
          clPrologKit = sbcl.buildASDFSystem {
            pname = "cl-prolog-kit";
            version = siblingVersion "cl-prolog-kit";
            src = inputs.cl-prolog-kit;
            systems = [
              "cl-prolog-kit"
              "cl-prolog-kit/callgraph"
            ];
          };
          clWeave = sbcl.buildASDFSystem {
            pname = "cl-weave";
            version = siblingVersion "cl-weave";
            src = inputs.cl-weave;
            systems = [ "cl-weave" ];
          };
          # cl-parser-kit is self-contained. cl-boundary-kit pulls cl-log-kit.
          # cl-dataflow-kit, cl-cli and cl-tty-kit all depend on the external
          # cl-prolog-kit engine in production, so clPrologKit is threaded into their
          # lispLibs and thereby reaches any cl-cc package consuming them.
          clParserKit = sbcl.buildASDFSystem {
            pname = "cl-parser-kit";
            version = siblingVersion "cl-parser-kit";
            src = inputs.cl-parser-kit;
            systems = [ "cl-parser-kit" ];
          };
          clDataflowKit = sbcl.buildASDFSystem {
            pname = "cl-dataflow-kit";
            version = siblingVersion "cl-dataflow-kit";
            src = inputs.cl-dataflow-kit;
            systems = [ "cl-dataflow-kit" ];
            lispLibs = [
              clPrologKit
              clConcurrentKit
            ];
          };
          clLogKit = sbcl.buildASDFSystem {
            pname = "cl-log-kit";
            version = siblingVersion "cl-log-kit";
            src = inputs.cl-log-kit;
            systems = [ "cl-log-kit" ];
          };
          clBoundaryKit = sbcl.buildASDFSystem {
            pname = "cl-boundary-kit";
            version = siblingVersion "cl-boundary-kit";
            src = inputs.cl-boundary-kit;
            systems = [ "cl-boundary-kit" ];
            lispLibs = [ clLogKit ];
          };
          clCli = sbcl.buildASDFSystem {
            pname = "cl-cli";
            version = siblingVersion "cl-cli";
            src = inputs.cl-cli;
            systems = [ "cl-cli" ];
            lispLibs = [ clPrologKit ];
          };
          clTtyKit = sbcl.buildASDFSystem {
            pname = "cl-tty-kit";
            version = siblingVersion "cl-tty-kit";
            src = inputs.cl-tty-kit;
            systems = [ "cl-tty-kit" ];
            lispLibs = [ clPrologKit ];
          };
          clRegexKit = sbcl.buildASDFSystem {
            pname = "cl-regex-kit";
            version = siblingVersion "cl-regex-kit";
            src = inputs.cl-regex-kit;
            systems = [ "cl-regex-kit" ];
            lispLibs = [ clParserKit ];
          };
          clHostKit = sbcl.buildASDFSystem {
            pname = "cl-host-kit";
            version = siblingVersion "cl-host-kit";
            src = inputs.cl-host-kit;
            systems = [ "cl-host-kit" ];
          };
          # Subsystems split out of this repository. clCcAst is a
          # dependency-free leaf; clCcType depends on it.
          #
          # These are now the only definitions of those two systems. packages/
          # {ast,type} used to define them as well, and because ASDF resolved
          # whichever the registry reached first -- always the in-tree copy,
          # since this tree is scanned ahead of the injected derivations -- the
          # repositories were never actually compiled and the two sides drifted.
          # The in-tree copies have been removed; packages/{ast,type}/tests
          # remain and now exercise these derivations.
          clCcAst = sbcl.buildASDFSystem {
            pname = "cl-cc-ast";
            version = siblingVersion "cl-cc-ast";
            src = inputs.cl-cc-ast;
            systems = [ "cl-cc-ast" ];
          };
          clCcType = sbcl.buildASDFSystem {
            pname = "cl-cc-type";
            version = siblingVersion "cl-cc-type";
            src = inputs.cl-cc-type;
            systems = [ "cl-cc-type" ];
            lispLibs = [ clCcAst ];
          };
          # The standalone cl-cc-binary took a dependency on cl-log-kit that the
          # in-tree copy did not have; clLogKit is already an input for
          # cl-boundary-kit, so it is threaded in here rather than added.
          clProcessKit = sbcl.buildASDFSystem {
            pname = "cl-process-kit";
            version = siblingVersion "cl-process-kit";
            src = inputs.cl-process-kit;
            systems = [ "cl-process-kit" ];
            lispLibs = [
              clBoundaryKit
              clLogKit
            ];
          };
          clJsonKit = sbcl.buildASDFSystem {
            pname = "cl-json-kit";
            version = siblingVersion "cl-json-kit";
            src = inputs.cl-json-kit;
            systems = [ "cl-json-kit" ];
          };
          clDateKit = sbcl.buildASDFSystem {
            pname = "cl-date-kit";
            version = siblingVersion "cl-date-kit";
            src = inputs.cl-date-kit;
            systems = [ "cl-date-kit" ];
          };
          clConcurrentKit = sbcl.buildASDFSystem {
            pname = "cl-concurrent-kit";
            version = siblingVersion "cl-concurrent-kit";
            src = inputs.cl-concurrent-kit;
            systems = [ "cl-concurrent-kit" ];
          };
          clCcRuntime = sbcl.buildASDFSystem {
            pname = "cl-cc-runtime";
            version = siblingVersion "cl-cc-runtime";
            src = inputs.cl-cc-runtime;
            systems = [ "cl-cc-runtime" ];
            lispLibs = [
              clLogKit
              clProcessKit
              clJsonKit
            ];
          };
          clCcBootstrap = sbcl.buildASDFSystem {
            pname = "cl-cc-bootstrap";
            version = siblingVersion "cl-cc-bootstrap";
            src = inputs.cl-cc-bootstrap;
            systems = [ "cl-cc-bootstrap" ];
          };
          clCcVm = sbcl.buildASDFSystem {
            pname = "cl-cc-vm";
            version = siblingVersion "cl-cc-vm";
            src = inputs.cl-cc-vm;
            systems = [ "cl-cc-vm" ];
            lispLibs = [
              clCcBootstrap
              clCcRuntime
              clRegexKit
              clTtyKit
              clHostKit
            ];
          };
          clCcMir = sbcl.buildASDFSystem {
            pname = "cl-cc-mir";
            version = siblingVersion "cl-cc-mir";
            src = inputs.cl-cc-mir;
            systems = [
              "cl-cc-mir"
              "cl-cc-target"
            ];
          };
          clCcTarget = clCcMir;
          clCcCps = sbcl.buildASDFSystem {
            pname = "cl-cc-cps";
            version = siblingVersion "cl-cc-cps";
            src = inputs.cl-cc-cps;
            systems = [ "cl-cc-cps" ];
            lispLibs = [
              clCcBootstrap
              clCcAst
            ];
          };
          clCcExpand = sbcl.buildASDFSystem {
            pname = "cl-cc-expand";
            version = siblingVersion "cl-cc-expand";
            src = inputs.cl-cc-expand;
            systems = [ "cl-cc-expand" ];
            lispLibs = [
              clCcBootstrap
              clCcType
              clCcVm
            ];
          };
          clCcOptimize = sbcl.buildASDFSystem {
            pname = "cl-cc-optimize";
            version = siblingVersion "cl-cc-optimize";
            src = inputs.cl-cc-optimize;
            systems = [ "cl-cc-optimize" ];
            lispLibs = [
              clCcVm
              clCcType
              clCcAst
              clPrologKit
              clParserKit
            ];
          };
          # One source tree, three systems -- the repository holds them
          # together because the boundaries between them move. Each is built as
          # its own derivation so dependents keep naming the system they use.
          clCcRegalloc = sbcl.buildASDFSystem {
            pname = "cl-cc-regalloc";
            version = siblingVersion "cl-cc-codegen-native";
            src = inputs.cl-cc-codegen-native;
            systems = [ "cl-cc-regalloc" ];
            lispLibs = [
              clCcVm
              clCcMir
              clCcTarget
              clCcOptimize
            ];
          };
          clCcCodegen = sbcl.buildASDFSystem {
            pname = "cl-cc-codegen";
            version = siblingVersion "cl-cc-codegen-native";
            src = inputs.cl-cc-codegen-native;
            systems = [ "cl-cc-codegen" ];
            lispLibs = [
              clCcBootstrap
              clCcVm
              clCcMir
              clCcTarget
              clCcBinary
              clCcOptimize
              clCcRegalloc
            ];
          };
          clCcEmit = sbcl.buildASDFSystem {
            pname = "cl-cc-emit";
            version = siblingVersion "cl-cc-codegen-native";
            src = inputs.cl-cc-codegen-native;
            systems = [ "cl-cc-emit" ];
            lispLibs = [
              clCcVm
              clCcAst
              clCcMir
              clCcOptimize
              clCcCodegen
            ];
          };
          clCcParse = sbcl.buildASDFSystem {
            pname = "cl-cc-parse";
            version = siblingVersion "cl-cc-parse";
            src = inputs.cl-cc-parse;
            systems = [ "cl-cc-parse" ];
            lispLibs = [
              clCcAst
              clCcBootstrap
            ];
          };
          clCcPhp = sbcl.buildASDFSystem {
            pname = "cl-cc-php";
            version = siblingVersion "cl-cc-php";
            src = inputs.cl-cc-php;
            systems = [ "cl-cc-php" ];
            lispLibs = [
              clCcAst
              clCcBootstrap
              clCcParse
              clCcVm
            ];
          };
          clCcJavascript = sbcl.buildASDFSystem {
            pname = "cl-cc-javascript";
            version = siblingVersion "cl-cc-javascript";
            src = inputs.cl-cc-javascript;
            systems = [ "cl-cc-javascript" ];
            lispLibs = [
              clCcAst
              clCcBootstrap
              clCcParse
              clCcVm
              clDateKit
              clJsonKit
              clConcurrentKit
              clHostKit
            ];
          };
          clCcBinary = sbcl.buildASDFSystem {
            pname = "cl-cc-binary";
            version = siblingVersion "cl-cc-binary";
            src = inputs.cl-cc-binary;
            systems = [ "cl-cc-binary" ];
            lispLibs = [ clLogKit ];
          };

          asdf = import ./nix/asdf-systems.nix {
            inherit
              pkgs
              lib
              sbcl
              clPrologKit
              clWeave
              clParserKit
              clDataflowKit
              clBoundaryKit
              clCli
              clTtyKit
              clRegexKit
              clCcAst
              clCcType
              clCcBinary
              clCcRuntime
              clCcBootstrap
              clCcVm
              clCcMir
              clCcTarget
              clCcCps
              clCcExpand
              clCcOptimize
              clCcRegalloc
              clCcCodegen
              clCcEmit
              clCcParse
              clCcPhp
              clCcJavascript
              ;
          };
          inherit (asdf)
            productionAsdfSystems
            testAsdfSystems
            sbclWithCLCC
            sbclWithJavascriptTests
            sbclWithJitTests
            sbclWithTests
            cl-cc-prolog-tools
            cl-cc-prolog-tools-test
            ;

          binaryModule = import ./nix/binary.nix {
            inherit
              pkgs
              lib
              sbclWithCLCC
              version
              ;
          };
          testImage = import ./nix/sbcl-image.nix {
            inherit
              pkgs
              lib
              sbclWithTests
              dispatchSemFix
              ;
          };
          appsModule = import ./nix/apps.nix {
            inherit
              pkgs
              lib
              sbclWithCLCC
              sbclWithJavascriptTests
              sbclWithTests
              sbclBootstrap
              dispatchSemFix
              testImage
              ;
          };

          # Rendered documentation site (Material for MkDocs). Builds fully
          # offline: Material bundles its assets, so the Nix sandbox needs no
          # network. --strict promotes a broken link or a page missing from the
          # nav into a build failure.
          docs = pkgs.stdenvNoCC.mkDerivation {
            pname = "cl-cc-docs";
            inherit version;
            src = lib.fileset.toSource {
              root = ./docs;
              fileset = lib.fileset.unions [
                ./docs/mkdocs.yml
                ./docs/src
              ];
            };
            nativeBuildInputs = [ pkgs.python3Packages.mkdocs-material ];
            buildPhase = ''
              runHook preBuild
              mkdocs build --strict --config-file mkdocs.yml --site-dir "$out"
              runHook postBuild
            '';
            dontInstall = true;
            meta = {
              description = "Rendered MkDocs (Material) documentation for cl-cc";
              homepage = "https://github.com/nerima-lisp/cl-cc";
              license = lib.licenses.mit;
            };
          };

          checksModule = import ./nix/checks.nix {
            inherit
              pkgs
              lib
              version
              sbclWithJitTests
              sbclWithTests
              clPrologKit
              clWeave
              clCcAst
              ;
            inherit (appsModule) apps;
          };
          devshellModule = import ./nix/devshell.nix { inherit pkgs lib sbclWithCLCC; };
        in
        {
          packages =
            productionAsdfSystems
            // testAsdfSystems
            // {
              inherit (binaryModule) default;
              inherit cl-cc-prolog-tools cl-cc-prolog-tools-test docs;
              test-image = testImage;
            };
          inherit (appsModule) apps;
          inherit (checksModule) testSuite jitTests prologToolsTests;
          inherit (devshellModule) devShells;
        }
      );
    in
    {
      packages = forAllSystems (system: perSystem.${system}.packages);

      # Granularity lives HERE, not in extra GitHub Actions jobs: `nix flake
      # check` evaluates each attribute below as its own derivation, in
      # parallel, with build caching. Want the test suite, the formatter and the
      # docs reported separately? That is what these three names are for. Adding
      # a second job to ci.yml instead would re-implement the same scheduling
      # and lose cache sharing between the pieces.
      checks = forAllSystems (system: {
        # The SBCL test suite, run through ./run-tests.lisp.
        default = perSystem.${system}.testSuite;

        # Fails `nix flake check` when any tracked Nix file is unformatted,
        # which is what makes the formatter a gate rather than a suggestion.
        formatting = treefmtEval.${system}.config.build.check self;

        # `packages.docs` runs `mkdocs build --strict`, so a broken link or a
        # page missing from the nav fails right here. Without this the docs are
        # only ever built by docs.yml, which runs after the merge to main —
        # a break would surface as a failed deploy rather than a failed pull
        # request.
        docs = perSystem.${system}.packages.docs;

        # The standalone binary has to keep linking, not just the library.
        build = perSystem.${system}.packages.default;

        # packages/prolog-tools has its own cl-weave suite, driven by
        # `asdf:test-system` rather than by cl-cc's runner.
        cl-cc-prolog-tools-tests = perSystem.${system}.prologToolsTests;

        # The JIT write-barrier suite is a secondary ASDF system and must be
        # exercised explicitly; building its FASLs alone does not run tests.
        cl-cc-jit-tests = perSystem.${system}.jitTests;
      });

      apps = forAllSystems (system: perSystem.${system}.apps);

      devShells = forAllSystems (system: perSystem.${system}.devShells);

      # `nix fmt` entry point.
      formatter = forAllSystems (system: treefmtEval.${system}.config.build.wrapper);

      overlays.default = final: _prev: { cl-cc = self.packages.${final.system}.default; };
    };
}
