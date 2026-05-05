{ lib, sbcl, ... }:
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
  mkAsdfSystem =
    {
      name,
      src,
      deps,
      allSystems,
    }:
    sbcl.buildASDFSystem {
      pname = name;
      version = "0.1.0";
      src = pkgSrc src;
      systems = [ name ];
      lispLibs = map (n: allSystems.${n}) deps;
    };

  # 14 leaf systems — preserved verbatim from the original flake.nix.
  leafSpec = {
    cl-cc-bootstrap = {
      src = "packages/bootstrap";
      deps = [ ];
    };
    cl-cc-ast = {
      src = "packages/ast";
      deps = [ ];
    };
    cl-cc-binary = {
      src = "packages/binary";
      deps = [ ];
    };
    cl-cc-runtime = {
      src = "packages/runtime";
      deps = [ ];
    };
    cl-cc-bytecode = {
      src = "packages/bytecode";
      deps = [ ];
    };
    cl-cc-ir = {
      src = "packages/ir";
      deps = [ ];
    };
    cl-cc-mir = {
      src = "packages/mir";
      deps = [ ];
    };
    cl-cc-prolog = {
      src = "packages/prolog";
      deps = [ "cl-cc-bootstrap" ];
    };
    cl-cc-type = {
      src = "packages/type";
      deps = [ "cl-cc-ast" ];
    };
    cl-cc-parse = {
      src = "packages/parse";
      deps = [
        "cl-cc-ast"
        "cl-cc-bootstrap"
      ];
    };
    cl-cc-vm = {
      src = "packages/vm";
      deps = [ "cl-cc-bootstrap" ];
    };
    cl-cc-vm-interp = {
      src = "packages/vm-interp";
      deps = [ ];
    };
    cl-cc-vm-isa = {
      src = "packages/vm-isa";
      deps = [ ];
    };
    cl-cc-php = {
      src = "packages/php";
      deps = [
        "cl-cc-ast"
        "cl-cc-bootstrap"
        "cl-cc-parse"
      ];
    };
    cl-cc-optimize = {
      src = "packages/optimize";
      deps = [
        "cl-cc-vm"
        "cl-cc-prolog"
        "cl-cc-type"
      ];
    };
    cl-cc-target = {
      src = "packages/target";
      deps = [ ];
    };
    cl-cc-regalloc = {
      src = "packages/regalloc";
      deps = [
        "cl-cc-vm"
        "cl-cc-mir"
        "cl-cc-target"
        "cl-cc-optimize"
      ];
    };
    cl-cc-expand = {
      src = "packages/expand";
      deps = [
        "cl-cc-bootstrap"
        "cl-cc-type"
      ];
    };
    cl-cc-cps = {
      src = "packages/cps";
      deps = [
        "cl-cc-bootstrap"
        "cl-cc-ast"
      ];
    };
    cl-cc-codegen = {
      src = "packages/codegen";
      deps = [
        "cl-cc-bootstrap"
        "cl-cc-vm"
        "cl-cc-mir"
        "cl-cc-target"
        "cl-cc-optimize"
        "cl-cc-regalloc"
      ];
    };
    cl-cc-emit = {
      src = "packages/emit";
      deps = [
        "cl-cc-vm"
        "cl-cc-mir"
        "cl-cc-optimize"
        "cl-cc-codegen"
      ];
    };
    cl-cc-compile = {
      src = "packages/compile";
      deps = [
        "cl-cc-bootstrap"
        "cl-cc-ast"
        "cl-cc-prolog"
        "cl-cc-parse"
        "cl-cc-type"
        "cl-cc-optimize"
        "cl-cc-vm"
        "cl-cc-expand"
        "cl-cc-cps"
        "cl-cc-codegen"
        "cl-cc-target"
        "cl-cc-regalloc"
      ];
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
        "cl-cc-prolog"
        "cl-cc-parse"
        "cl-cc-php"
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
        "cl-cc-prolog"
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
    };
  };

  leafNames = builtins.attrNames leafSpec;

  # Umbrella + helpers. cl-cc bundles the umbrella package + compile-pipeline.
  # cl-cc-cli depends on :cl-cc. cl-cc-testing-framework also depends on :cl-cc.
  derivedSpec = {
    cl-cc = {
      src = [
        (projectRoot + "/packages")
        (projectRoot + "/cl-cc.asd")
        (projectRoot + "/cl-cc-test.asd")
      ];
      deps = leafNames;
    };
    cl-cc-cli = {
      src = "packages/cli";
      deps = [ "cl-cc" ];
    };
    cl-cc-testing-framework = {
      src = "packages/testing-framework";
      deps = [
        "cl-cc"
        "cl-cc-php"
      ];
    };
  };

  productionAsdfSystems = lib.fix (
    sys:
    lib.mapAttrs (
      name:
      { src, deps }:
      mkAsdfSystem {
        inherit name src deps;
        allSystems = sys;
      }
    ) (leafSpec // derivedSpec)
  );

  # Test systems live in a separate attrset so Nix consumers can opt into
  # the heavier test FASLs only when needed. lispLibs flow from
  # productionAsdfSystems, never recursively from testAsdfSystems.
  testSrc = [
    (projectRoot + "/packages")
    (projectRoot + "/tests")
    (projectRoot + "/cl-cc.asd")
    (projectRoot + "/cl-cc-test.asd")
  ];

  # Keep self-hosting E2E tests available as an explicit system; the canonical
  # fast test app does not auto-load it.
  testAsdfSystems = {
    "cl-cc-test" = sbcl.buildASDFSystem {
      pname = "cl-cc-test";
      version = "0.1.0";
      src = pkgSrc testSrc;
      systems = [ "cl-cc-test" ];
      lispLibs = with productionAsdfSystems; [
        cl-cc
        cl-cc-cli
        cl-cc-testing-framework
      ];
    };
    "cl-cc-test/e2e" = sbcl.buildASDFSystem {
      pname = "cl-cc-test-e2e";
      version = "0.1.0";
      src = pkgSrc testSrc;
      systems = [ "cl-cc-test/e2e" ];
      lispLibs = with productionAsdfSystems; [
        cl-cc
        cl-cc-cli
        cl-cc-testing-framework
      ];
    };
  };
in
{
  inherit productionAsdfSystems testAsdfSystems;
  sbclWithCLCC = sbcl.withPackages (_: lib.attrValues productionAsdfSystems);
  sbclWithTests = sbcl.withPackages (
    _: (lib.attrValues productionAsdfSystems) ++ [ testAsdfSystems."cl-cc-test" ]
  );
}
