{
  lib,
  sbcl,
  clPrologKit,
  clWeave,
  clParserKit,
  clDataflowKit,
  clBoundaryKit,
  clCli,
  clTtyKit,
  clRegexKit,
  # cl-cc-ast / cl-cc-type were split into their own repos. They are supplied
  # here as prebuilt external derivations and injected into the internal
  # system graph under their original names (see externalCcSystems below), so
  # every `deps = [ "cl-cc-ast" ... ]` keeps resolving without change.
  clCcAst,
  clCcType,
  clCcBinary,
  clCcRuntime,
  clCcBootstrap,
  clCcVm,
  clCcMir,
  clCcTarget,
  clCcOptimize,
  clCcParse,
  clCcCps,
  clCcExpand,
  clCcPhp,
  clCcJavascript,
  clCcRegalloc,
  clCcCodegen,
  clCcEmit,
  ...
}:
let
  projectRoot = ../.;

  # pkgSrc accepts either a `subdir` string (relative to project root) OR a
  # `filesets` list of paths. We use lib.fileset.toSource so each derivation's
  # FASL cache stays independent — touching packages/vm never
  # invalidates packages/bootstrap.
  pkgSrc =
    arg:
    let
      filesets = if builtins.isString arg then [ (projectRoot + "/${arg}") ] else arg;
    in
    lib.fileset.toSource {
      root = projectRoot;
      fileset = lib.fileset.unions filesets;
    };

  # Build an ASDF system via sbcl.buildASDFSystem with shared boilerplate.
  # extraLispLibs threads in external (non cl-cc-*) derivations -- e.g. the
  # external cl-prolog-kit engine -- alongside deps, which only resolves names
  # against allSystems (internal cl-cc-* systems).
  mkAsdfSystem =
    {
      name,
      src,
      deps,
      allSystems,
      extraLispLibs ? [ ],
    }:
    sbcl.buildASDFSystem {
      pname = name;
      version = "0.1.0";
      src = pkgSrc src;
      systems = [ name ];
      lispLibs = (map (n: allSystems.${n}) deps) ++ extraLispLibs;
    };

  # Leaf systems built from packages/. cl-cc-ast and cl-cc-type used to live
  # here but are now external (externalCcSystems, overlaid into the fixpoint).
  leafSpec = {
    cl-cc-docgen = {
      src = "packages/docgen";
      deps = [ ];
    };
    cl-cc-optimize = {
      src = "packages/optimize";
      deps = [
        "cl-cc-vm"
        "cl-cc-type"
      ];
      # clPrologKit backs the peephole/e-graph rewrite rules; clParserKit tokenizes
      # and parses the `--pass-pipeline` spec string.
      extraLispLibs = [
        clPrologKit
        clParserKit
      ];
    };
    cl-cc-compile = {
      src = "packages/compile";
      deps = [
        "cl-cc-bootstrap"
        "cl-cc-ast"
        "cl-cc-parse"
        "cl-cc-type"
        "cl-cc-optimize"
        "cl-cc-parse"
        "cl-cc-vm"
        "cl-cc-expand"
        "cl-cc-cps"
        "cl-cc-codegen"
        "cl-cc-target"
        "cl-cc-regalloc"
      ];
      extraLispLibs = [ clPrologKit ];
    };
    cl-cc-stdlib = {
      src = "packages/stdlib";
      deps = [ "cl-cc-bootstrap" ];
    };
    cl-cc-pipeline = {
      src = "packages/pipeline";
      deps = [
        "cl-cc-bootstrap"
        "cl-cc-ast"
        "cl-cc-parse"
        "cl-cc-php"
        "cl-cc-javascript"
        "cl-cc-javascript"
        "cl-cc-type"
        "cl-cc-optimize"
        "cl-cc-vm"
        "cl-cc-expand"
        "cl-cc-emit"
        "cl-cc-stdlib"
        "cl-cc-binary"
        "cl-cc-compile"
      ];
    };
    cl-cc-selfhost = {
      src = "packages/selfhost";
      deps = [
        "cl-cc-bootstrap"
        "cl-cc-pipeline"
        "cl-cc-expand"
        "cl-cc-vm"
        "cl-cc-runtime"
        "cl-cc-compile"
        "cl-cc-ast"
        "cl-cc-parse"
        "cl-cc-optimize"
        "cl-cc-emit"
        "cl-cc-stdlib"
      ];
    };
    cl-cc-repl = {
      src = "packages/repl";
      deps = [
        "cl-cc-bootstrap"
        "cl-cc-pipeline"
        "cl-cc-selfhost"
        "cl-cc-expand"
        "cl-cc-vm"
        "cl-cc-parse"
        "cl-cc-compile"
        "cl-cc-runtime"
        "cl-cc-ast"
        "cl-cc-optimize"
        "cl-cc-emit"
        "cl-cc-stdlib"
      ];
      # cl-tty-kit provides ANSI/style/screen + input-decoder primitives for
      # the interactive REPL front-end; cl-boundary-kit models its console/
      # system-exit I/O as swappable, testable boundaries.
      extraLispLibs = [
        clTtyKit
        clBoundaryKit
      ];
    };
  };

  leafNames = builtins.attrNames leafSpec;

  # Umbrella + helpers. cl-cc bundles the umbrella package + compile-pipeline.
  # cl-cc-cli depends on :cl-cc. cl-cc-testing-framework also depends on :cl-cc.
  derivedSpec = {
    cl-cc-jit = {
      src = "src/jit";
      deps = [
        "cl-cc-runtime"
        "cl-cc-vm"
        "cl-cc-codegen"
        "cl-cc-compile"
      ];
    };
    cl-cc = {
      # t/ and run-tests.lisp are shipped even though `systems = [ "cl-cc" ]`
      # never compiles them. cl-cc.asd defines "cl-cc/test" as well as "cl-cc",
      # and this derivation's $out puts that file on the source registry. If the
      # tree it sits in lacks t/, anything that resolves "cl-cc/test" through
      # THIS copy fails on a missing component instead of falling through to the
      # test derivation. Shipping the sources costs a few hundred kilobytes and
      # makes both copies of cl-cc.asd interchangeable.
      src = [
        (projectRoot + "/packages")
        (projectRoot + "/src")
        (projectRoot + "/t")
        (projectRoot + "/run-tests.lisp")
        (projectRoot + "/cl-cc.asd")
      ];
      # leafNames covers only what is still built from packages/. Every system
      # that has moved to its own repository is listed here instead, so the
      # umbrella closure still pulls it. Most would arrive transitively through
      # a dependent that stayed, but not all, so this list cannot simply be
      # inferred from leafSpec's own deps.
      #
      deps = leafNames ++ [
        "cl-cc-ast"
        "cl-cc-type"
        "cl-cc-bootstrap"
        "cl-cc-vm"
        "cl-cc-binary"
        "cl-cc-runtime"
        "cl-cc-mir"
        "cl-cc-target"
        "cl-cc-optimize"
        "cl-cc-regalloc"
        "cl-cc-codegen"
        "cl-cc-emit"
        "cl-cc-jit"
      ];
    };
    cl-cc-cli = {
      src = "packages/cli";
      deps = [
        "cl-cc"
        "cl-cc-docgen"
      ];
      # cl-cli is the declarative argument parser behind the cl-cc command
      # tree; cl-boundary-kit models console/args/system-exit as testable I/O
      # boundaries at the CLI edge; cl-tty-kit provides the ANSI/style helpers
      # for the interactive REPL and colored IR dumps; cl-dataflow-kit backs the
      # `dep-graph` command's graph model + DOT/Mermaid/topological export.
      extraLispLibs = [
        clCli
        clBoundaryKit
        clTtyKit
        clDataflowKit
      ];
    };
    cl-cc-testing-framework = {
      src = "packages/testing-framework";
      deps = [
        "cl-cc"
        "cl-cc-php"
      ];
      extraLispLibs = [ clWeave ];
    };
    cl-cc-tools = {
      src = "packages/tools";
      deps = [ "cl-cc" ];
    };
    cl-cc-formatter = {
      src = "packages/formatter";
      deps = [ ];
    };
  };

  # External subsystems, keyed by the names the internal graph depends on.
  # Overlaid onto the fixpoint below so `deps` lookups resolve to them.
  externalCcSystems = {
    cl-cc-ast = clCcAst;
    cl-cc-type = clCcType;
    cl-cc-binary = clCcBinary;
    cl-cc-runtime = clCcRuntime;
    cl-cc-bootstrap = clCcBootstrap;
    cl-cc-vm = clCcVm;
    cl-cc-mir = clCcMir;
    cl-cc-target = clCcTarget;
    cl-cc-optimize = clCcOptimize;
    cl-cc-parse = clCcParse;
    cl-cc-cps = clCcCps;
    cl-cc-expand = clCcExpand;
    cl-cc-php = clCcPhp;
    cl-cc-javascript = clCcJavascript;
    cl-cc-regalloc = clCcRegalloc;
    cl-cc-codegen = clCcCodegen;
    cl-cc-emit = clCcEmit;
  };

  productionAsdfSystems = lib.fix (
    sys:
    (lib.mapAttrs (
      name:
      {
        src,
        deps,
        extraLispLibs ? [ ],
      }:
      mkAsdfSystem {
        inherit
          name
          src
          deps
          extraLispLibs
          ;
        allSystems = sys;
      }
    ) (leafSpec // derivedSpec))
    // externalCcSystems
  );

  # Prolog-based call-graph analysis tools (packages/prolog-tools), built on
  # the external cl-prolog-kit engine — a standalone leaf package registered via
  # `maybe-load-asd` in cl-cc.asd rather than folded into the `:cl-cc`
  # dependency closure, so it is exposed here rather than through
  # `mkAsdfSystem`'s internal-only `deps` lookup (which only resolves names
  # against `productionAsdfSystems`, not external flake inputs).
  cl-cc-prolog-tools = sbcl.buildASDFSystem {
    pname = "cl-cc-prolog-tools";
    version = "0.1.0";
    src = pkgSrc "packages/prolog-tools";
    systems = [ "cl-cc-prolog-tools" ];
    lispLibs = [
      productionAsdfSystems.cl-cc-ast
      clPrologKit
    ];
  };

  # Its cl-weave test suite is a separate system/derivation so the
  # cl-weave dependency stays test-only, never reaching the production
  # cl-cc-prolog-tools closure above.
  cl-cc-prolog-tools-test = sbcl.buildASDFSystem {
    pname = "cl-cc-prolog-tools-test";
    version = "0.1.0";
    src = pkgSrc "packages/prolog-tools";
    systems = [ "cl-cc-prolog-tools/tests" ];
    lispLibs = [
      productionAsdfSystems.cl-cc-ast
      clPrologKit
      clWeave
    ];
  };

  # Test systems live in a separate attrset so Nix consumers can opt into
  # the heavier test FASLs only when needed. lispLibs flow from
  # productionAsdfSystems, never recursively from testAsdfSystems.
  # cl-cc.asd carries the "cl-cc/test" and "cl-cc/test/e2e" secondary systems
  # as well as "cl-cc" itself, so there is no second .asd to ship here.
  testSrc = [
    (projectRoot + "/packages")
    (projectRoot + "/t")
    (projectRoot + "/src")
    (projectRoot + "/run-tests.lisp")
    (projectRoot + "/cl-cc.asd")
  ];

  # Keep self-hosting E2E tests available as an explicit system; the canonical
  # fast test app does not auto-load it.
  testAsdfSystems = {
    "cl-cc-jit/tests" = sbcl.buildASDFSystem {
      pname = "cl-cc-jit-tests";
      version = "0.1.0";
      src = pkgSrc testSrc;
      systems = [ "cl-cc-jit/tests" ];

      # cl-cc-jit/tests is a secondary system in cl-cc-jit.asd. Resolve it
      # from this writable test source before the production JIT derivation.
      preBuild = ''
        export CL_SOURCE_REGISTRY="$src//''${CL_SOURCE_REGISTRY:+:$CL_SOURCE_REGISTRY}"
      '';

      postInstall = ''
        find . -name '*.asd' -exec install -Dm444 {} "$out/{}" \;
      '';

      lispLibs = with productionAsdfSystems; [
        cl-cc-jit
        cl-cc-testing-framework
      ];
    };
    "cl-cc/test" = sbcl.buildASDFSystem {
      # pname stays hyphenated: it becomes a store path component, which cannot
      # contain a slash. Only `systems` names the ASDF system.
      pname = "cl-cc-test";
      version = "0.1.0";
      src = pkgSrc testSrc;
      systems = [ "cl-cc/test" ];

      # nix-cl's stock buildPhase appends `$src//` to the END of
      # CL_SOURCE_REGISTRY, so the lispLibs win every name lookup. That was
      # correct while the test system lived in its own cl-cc-test.asd. It is not
      # any more: "cl-cc/test" is a SECONDARY system of "cl-cc", so ASDF looks
      # for it in cl-cc.asd — and finds the read-only copy inside the cl-cc
      # lispLib first. Its output translations map ${storeDir} to itself, so the
      # build then dies with "Permission denied" writing a .fasl into that store
      # path. Putting $src first makes every cl-cc-family system resolve out of
      # this derivation's own tree, where the translations point at a writable
      # build directory.
      preBuild = ''
        export CL_SOURCE_REGISTRY="$src//''${CL_SOURCE_REGISTRY:+:$CL_SOURCE_REGISTRY}"
      '';

      # nix-cl's installPhase deletes every .asd whose basename is not in
      # `systems`. "cl-cc/test" has no cl-cc/test.asd to match, so the rule
      # removes cl-cc.asd and leaves an output that defines nothing. Put the
      # .asd files back.
      postInstall = ''
        find . -name '*.asd' -exec install -Dm444 {} "$out/{}" \;
      '';

      lispLibs = with productionAsdfSystems; [
        clRegexKit
        cl-cc
        cl-cc-cli
        cl-cc-testing-framework
        cl-cc-tools
        cl-cc-formatter
      ];
    };
    "cl-cc-javascript-test" = sbcl.buildASDFSystem {
      pname = "cl-cc-javascript-test";
      version = "0.1.0";
      src = pkgSrc testSrc;
      systems = [ "cl-cc-javascript-test" ];
      lispLibs = with productionAsdfSystems; [
        cl-cc
        cl-cc-cli
        cl-cc-testing-framework
        cl-cc-tools
        cl-cc-php
        cl-cc-javascript
      ];
    };
    "cl-cc/test/e2e" = sbcl.buildASDFSystem {
      pname = "cl-cc-test-e2e";
      version = "0.1.0";
      src = pkgSrc testSrc;
      systems = [ "cl-cc/test/e2e" ];
      lispLibs = with productionAsdfSystems; [
        cl-cc
        cl-cc-cli
        cl-cc-testing-framework
        cl-cc-tools
      ];
    };
  };
in
{
  inherit productionAsdfSystems testAsdfSystems;
  inherit cl-cc-prolog-tools cl-cc-prolog-tools-test;
  sbclWithCLCC = sbcl.withPackages (_: lib.attrValues productionAsdfSystems);
  # The test derivation goes FIRST: both it and productionAsdfSystems.cl-cc ship
  # a cl-cc.asd, and only the test one has pre-built FASLs for the test
  # components. ASDF takes the first match on the registry, so ordering here is
  # the difference between loading those FASLs and recompiling the whole suite
  # on every run.
  sbclWithTests = sbcl.withPackages (
    _: [ testAsdfSystems."cl-cc/test" ] ++ (lib.attrValues productionAsdfSystems)
  );
  sbclWithJitTests = sbcl.withPackages (
    _: [ testAsdfSystems."cl-cc-jit/tests" ] ++ (lib.attrValues productionAsdfSystems)
  );
  sbclWithJavascriptTests = sbcl.withPackages (
    _: (lib.attrValues productionAsdfSystems) ++ [ testAsdfSystems."cl-cc-javascript-test" ]
  );
}
