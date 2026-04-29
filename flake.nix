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
      flake.overlays.default = final: _prev: { cl-cc = self.packages.${final.system}.default; };
      perSystem =
        { pkgs, ... }:
        let
          inherit (pkgs) lib;
          dispatchModule = import ./nix/dispatch-sem-fix.nix { inherit pkgs lib; };
          dispatchSemFix = dispatchModule.dispatchSemFix or null;
          sbclModule = import ./nix/sbcl.nix { inherit pkgs; };
          inherit (sbclModule) sbcl sbclBootstrap;
          asdf = import ./nix/asdf-systems.nix { inherit pkgs lib sbcl; };
          inherit (asdf) productionAsdfSystems testAsdfSystems sbclWithCLCC sbclWithTests;
          binaryModule = import ./nix/binary.nix { inherit pkgs lib sbclWithCLCC; };
          appsModule = import ./nix/apps.nix {
            inherit pkgs lib sbclWithCLCC sbclWithTests sbclBootstrap dispatchSemFix;
          };
          checksModule = import ./nix/checks.nix {
            inherit pkgs lib sbclWithTests sbclBootstrap dispatchSemFix;
            packagesDefault = binaryModule.default;
          };
          devshellModule = import ./nix/devshell.nix { inherit pkgs lib sbclWithCLCC; };
        in
        {
          packages = productionAsdfSystems // testAsdfSystems // {
            inherit (binaryModule) default;
          };
          inherit (appsModule) apps;
          inherit (checksModule) checks;
          inherit (devshellModule) devShells treefmt;
        };
    };
}
