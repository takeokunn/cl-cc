{ pkgs, sbclWithCLCC, ... }:
{
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
          nix run .#test      Run the canonical fast unit test plan
          nix run .#load      Load :cl-cc non-interactively
          nix run .#repl      rlwrap'd SBCL with :cl-cc loaded  (nix run default)

        Nix:
          nix flake check    Run checks.tests + .build + .treefmt
          nix build          Build the standalone binary at ./result/bin/cl-cc
          nix fmt            Format repo (nixfmt + deadnix + statix + prettier)
          nix flake show     List all flake outputs

        CLI (after `nix build`, binary at ./result/bin/cl-cc):
          cl-cc run example/hello.lisp
          cl-cc eval "(+ 1 2)"
          cl-cc compile example/hello.lisp -o hello
          cl-cc help

      EOF
    '';
  };

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
}
