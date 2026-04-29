{ lib, sbcl, ... }:
let
  projectRoot = ../.;

  # pkgSrc accepts either a `subdir` string (relative to project root) OR a
  # `filesets` list of paths. We use lib.fileset.toSource so each derivation's
  # FASL cache stays independent — touching packages/engine/vm never
  # invalidates packages/foundation/bootstrap.
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
    { name, src, deps, allSystems }:
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
      src = "packages/foundation/bootstrap";
      deps = [ ];
    };
    cl-cc-ast = {
      src = "packages/foundation/ast";
      deps = [ ];
    };
    cl-cc-binary = {
      src = "packages/backend/binary";
      deps = [ ];
    };
    cl-cc-runtime = {
      src = "packages/backend/runtime";
      deps = [ ];
    };
    cl-cc-bytecode = {
      src = "packages/backend/bytecode";
      deps = [ ];
    };
    cl-cc-ir = {
      src = "packages/foundation/ir";
      deps = [ ];
    };
    cl-cc-mir = {
      src = "packages/foundation/mir";
      deps = [ ];
    };
    cl-cc-prolog = {
      src = "packages/foundation/prolog";
      deps = [ "cl-cc-bootstrap" ];
    };
    cl-cc-type = {
      src = "packages/foundation/type";
      deps = [ "cl-cc-ast" ];
    };
    cl-cc-parse = {
      src = "packages/frontend/parse";
      deps = [
        "cl-cc-ast"
        "cl-cc-bootstrap"
      ];
    };
    cl-cc-vm = {
      src = "packages/engine/vm";
      deps = [ "cl-cc-bootstrap" ];
    };
    cl-cc-optimize = {
      src = "packages/engine/optimize";
      deps = [
        "cl-cc-vm"
        "cl-cc-prolog"
        "cl-cc-type"
      ];
    };
    cl-cc-emit = {
      src = "packages/backend/emit";
      deps = [
        "cl-cc-vm"
        "cl-cc-mir"
        "cl-cc-optimize"
      ];
    };
    cl-cc-expand = {
      src = "packages/frontend/expand";
      deps = [
        "cl-cc-bootstrap"
        "cl-cc-ast"
        "cl-cc-prolog"
        "cl-cc-parse"
        "cl-cc-type"
      ];
    };
    cl-cc-compile = {
      src = "packages/engine/compile";
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
      src = "packages/testing/framework";
      deps = [ "cl-cc" ];
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

  # cl-cc-test/slow is precompiled here so that the canonical `nix run .#test`
  # does not pay a cold-FASL compile cost on first invocation. Path-baking is
  # avoided because selfhost-test-support.lisp now exposes selfhost-all-source-files
  # as a function (resolved at call time against the live ASDF state) rather
  # than a load-time defparameter.
  # cl-cc-test/slow is intentionally NOT precompiled here. Loading the
  # precompiled FASL hangs in unsymbolicated SBCL JIT code (root cause not
  # tracked down). The runtime auto-load path through `(asdf:load-system
  # :cl-cc-test/slow)` from inside `run-tests` works correctly with on-the-fly
  # compilation into ~/.cache/common-lisp/.
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
    "cl-cc-test/clos" = sbcl.buildASDFSystem {
      pname = "cl-cc-test-clos";
      version = "0.1.0";
      src = pkgSrc testSrc;
      systems = [ "cl-cc-test/clos" ];
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
  sbclWithTests = sbcl.withPackages (_: lib.attrValues (productionAsdfSystems // testAsdfSystems));
}
