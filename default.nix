#
# TODO file subject to be moved almost entirely to project.nix
#
{ compiler-nix-name ? "ghc8107" # Should match the one in stack.yaml
, doHaddock ? doHoogle # hoogle requires haddock
, doHoogle ? false
, materialized ? if builtins.pathExists nix/materialized then nix/materialized else null
, checkMaterialization ? false
, doCheck ? false
, exactDeps ? true
, ...
}:
let
  overlays = [
    # So that haskell.nix knows to find the numa library (when -fnuma)
    (self: super: { numa = self.numactl; })
  ];

  # Loads the stack project (stack.yaml) using the haskell.nix infrastructure.
  project' = { lib, stdenv, haskell-nix, buildPackages }:
    haskell-nix.stackProject' {
      # The "cleanGit" filters out untracked files. You don't need to
      # stage changes, it will use the files in the worktree, but you do
      # need to `git add` new files so that they become tracked.
      src = haskell-nix.haskellLib.cleanGit {
        name = "juvix";
        src = ./.;
      };
      inherit compiler-nix-name;
      modules = [ {
        inherit doHaddock doHoogle doCheck exactDeps;
        # Strip the executables and disable shared to avoid pulling gcc into the closure.
        packages.juvix.components.exes.juvix = { dontStrip = false; enableShared = false; };
        packages.http.components.exes.juvix-server = { dontStrip = false; enableShared = false; };
      } ];
      # For git sources in stack.yaml
      sha256map = import ./nix/stack-sha256map.nix;
      # Materializing speeds up the evaluation of nix expressions.
      inherit materialized checkMaterialization;
    };

  sources = import ./nix/sources.nix { };
  haskellNix = import sources.haskellNix { };
  # Import nixpkgs with customizations specified by haskell.Nix + our own overlays.
  nixpkgsSrc = haskellNix.sources.nixpkgs-unstable;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
  nixpkgsOverlays = nixpkgsArgs.overlays ++ overlays;
  pkgs = import nixpkgsSrc (nixpkgsArgs // { overlays = nixpkgsOverlays; });
  lib = pkgs.lib;
  haskellLib = pkgs.haskell-nix.haskellLib;

  project = pkgs.callPackage project' {
    inherit (pkgs) buildPackages;
  };
in

rec {
  # The project generated from the stack.yaml via haskell.nix
  inherit project;
  # All local Haskell packages.
  local = haskellLib.selectLocalPackages project.hsPkgs;
  # The same project except everything is linked statically (musl).
  static.project = project.projectCross.musl64;
  # All local Haskell packages linked statically.
  static.local = haskellLib.selectLocalPackages static.project.hsPkgs;
  # some convenience exports
  inherit (project) pkgs;
}
  # Add shorthands for local exes, tests and benchmarks
  // lib.genAttrs [ "exes" "tests" "benchmarks" "library" ] (component: haskellLib.collectComponents component haskellLib.isLocalPackage project.hsPkgs)
