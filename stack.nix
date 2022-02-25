{ ghcVersion, system ? builtins.currentSystem }:
let
  flake = builtins.getFlake "git+file://${toString ./.}";
in
  flake.devShells.${system}.stack-nix-shell.passthru.withArgs { inherit ghcVersion; }
