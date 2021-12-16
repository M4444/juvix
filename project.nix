{ compiler-nix-name ? "ghc8107" # Should match the one in stack.yaml
, doHaddock ? doHoogle # hoogle requires haddock
, doHoogle ? false
, doCheck ? false
      # For git sources in stack.yaml
, sha256map ? import ./nix/stack-sha256map.nix
, stack-sha256 ? null
, materialized ? if builtins.pathExists nix/materialized then toString nix/materialized else null
, checkMaterialization ? false
      # cross platforms to enable
, crossPlatforms ? (p: [p.musl64 ])
, ...
}:

final: prev: {
  # Stack project turned into nix derivations
  project =
    final.haskell-nix.stackProject' {
      src = final.haskell-nix.cleanSourceHaskell {
        name = "juvix-src";
        src = ./.;
      };
      inherit compiler-nix-name sha256map stack-sha256 materialized checkMaterialization;
      # This is used by `nix develop .` to open a shell for use with
      # `cabal`, `hlint` and `haskell-language-server`
      shell.tools = {
        cabal = { };
        ghcid = { };
        haskell-language-server = { };
        hlint = { };
        ormolu = { };
      };
      modules = [{
        inherit doHaddock doHoogle doCheck;
        # Strip the executables and disable shared to avoid pulling gcc into the closure.
        packages.juvix.components.exes.juvix = { dontStrip = false; enableShared = false; };
        packages.http.components.exes.juvix-server = { dontStrip = false; enableShared = false; };
      }];
    };

  # Flake attributes for the project
  flake =
    let
      project = final.project;
      flake = project.flake { inherit crossPlatforms; };
      # Add paths in apps for the cross packages
      crossApps = drvs: final.lib.mapAttrs'
        (name: _:
          let v = project.projectCross.musl64.getComponent name; in
          final.lib.nameValuePair (v.stdenv.hostPlatform.config + "/" + name) ({ type = "app"; program = v.exePath; }))
        drvs;
    in
    flake // { apps = flake.apps // crossApps flake.apps; };

  # System library tweaks
  numa = final.numactl;
}
