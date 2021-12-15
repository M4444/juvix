{ crossPlatforms }:

final: prev:

{
  # So that haskell.nix knows to find the numa library (when -fnuma)
  numa = final.numactl;

  # This overlay adds our project to pkgs
  juvixProject =
    final.haskell-nix.stackProject' {
      src = final.haskell-nix.haskellLib.cleanGit {
        name = "juvix";
        src = ../.;
      };

      # Should match the one in stack.yaml
      compiler-nix-name = "ghc8107";

      # For git sources in stack.yaml
      sha256map = import ./stack-sha256map.nix;
      modules = [{
        doHaddock = false;
        doHoogle = false;
        doCheck = false;
        # Strip the executables and disable shared to avoid pulling gcc into the closure.
        packages.juvix.components.exes.juvix = { dontStrip = false; enableShared = false; };
        packages.http.components.exes.juvix-server = { dontStrip = false; enableShared = false; };
      }];
      # This is used by `nix develop .` to open a shell for use with
      # `cabal`, `hlint` and `haskell-language-server`
      shell.tools = {
        cabal = { };
        hlint = { };
        haskell-language-server = { };
      };
      # Non-Haskell shell tools go here
      #shell.buildInputs = with pkgs; [
      #  nixpkgs-fmt
      #];
      # This adds `js-unknown-ghcjs-cabal` to the shell.
      shell.crossPlatform = crossPlatforms;
    };

  juvixFlake = final.juvixProject.flake {
    # This adds support for `nix build .#musl:hello:exe:hello`
    crossPlatforms = crossPlatforms;
  };
}
