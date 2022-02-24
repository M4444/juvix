{ ghcVersion, system ? builtins.currentSystem }:
let
  flake = builtins.getFlake "git+file://${toString ./.}";
in
  flake.devShells.${system}.stack-nix-shell.override { inherit ghcVersion; }
