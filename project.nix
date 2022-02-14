{ pkgs
, lib ? pkgs.lib
, haskell-nix ? pkgs.haskell-nix
  # Should match the one in stack.yaml, usually inferred automatically "ghc8107"
, compiler-nix-name ? null
, doHaddock ? doHoogle # hoogle requires haddock
, doHoogle ? false
, doCheck ? false
  # For git sources in stack.yaml
, sha256map ? import ./nix/stack-sha256map.nix
  # From calculateMaterializedSha
, stack-sha256 ? if builtins.pathExists nix/stack-sha256 then lib.fileContents nix/stack-sha256 else null
  # Updated by generateMaterialized
, materialized ? if builtins.pathExists nix/materialized then nix/materialized else null
, checkMaterialization ? false
, ...
}:

# Stack project turned into nix derivations
haskell-nix.stackProject' {
  src = haskell-nix.cleanSourceHaskell {
    name = "juvix-src";
    src = ./.;
  };
  inherit compiler-nix-name;
  inherit sha256map;
  inherit checkMaterialization;
  stack-sha256 = if materialized != null then stack-sha256 else null;
  materialized = if stack-sha256 != null then materialized else null;
  # This is used by `nix develop .` to open a shell for use with
  # `cabal`, `hlint` and `haskell-language-server`
  shell.tools = { };
  modules = [
    ({
      inherit doHaddock doHoogle doCheck;
      # Strip the executables and disable shared to avoid pulling gcc into the closure.
      packages.juvix.components.exes.juvix = { dontStrip = false; enableShared = false; };
      packages.http.components.exes.juvix-server = { dontStrip = false; enableShared = false; };
    })
    # Fix llvm-hs on musl
    ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isMusl {
      packages.llvm-hs.flags.shared-llvm = false;
      packages.llvm-hs.components.library.libs = with pkgs; [
        (libxml2.override (_: { enableShared = false; }))
        ncurses5 # for libtinfo
        zlib
      ];
    })
  ];
}
