#
# TODO could be replaced with a flake-compat shim. (`nix fevelop` doesn't use this file)
#
{...}@args:
let
  # Build haddock and hoogle indices, so that e.g. hoogle works.
  defaultArgs = { doHaddock = true; doHoogle = true; };
  project = (import ./. (defaultArgs // args)).project;
  hsLib = project.pkgs.haskell-nix.haskellLib;
in
  project.shellFor {
    # There are various settings you could add here. See:
    # https://input-output-hk.github.io/haskell.nix/reference/library.html#shellfor

    # Workaround for a bug in haskell.nix:
    # https://github.com/input-output-hk/haskell.nix/issues/590#issuecomment-712702992
    additional = ps: builtins.attrValues (hsLib.selectLocalPackages ps);

    # Instruct cabal to not deviate from the package set provided by nix.
    exactDeps = true;
  }
