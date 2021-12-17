# Utility functions for working with nix-bundle (for generating self-extracting executables)
#
# Usage: nix run .#bundle http:exe:juvix-server
#
{ self, system, nix-bundle }:

pkgs: _: {

  nix-bundle.app = {
    type = "app";
    program = builtins.toString (pkgs.writeShellScript "nix-bundle" ''
      if [ -z "$1" ]; then
        echo "Usage: $0 program"
        echo "  program: $(nix-instantiate --eval -E "let x = $getFlake; in builtins.attrNames x.apps.$system")"
        exit 1
      else
        set -e -x
        attr=$1
        system=${system}
        getFlake="builtins.getFlake \"${toString self}\""
        bundlerAttrPath=legacyPackages.$system.nix-bundle.mkBundle
        target=$(nix-store --realize "$(nix-instantiate -E "($getFlake).packages.$system.\"$attr\"")")
        exeName=$(basename $target/bin/*)
        out=$(nix-build -E "with $getFlake; $bundlerAttrPath { target = \"$target\"; run = \"/bin/$exeName\"; }")
        echo $out
        ln -nsf $out "$exeName"
      fi
    '');
  };
  nix-bundle.mkBundle = { target, run }:
    let
      nix-bundle' = import nix-bundle { nixpkgs = pkgs; };
    in
    nix-bundle'.nix-bootstrap-nix {
      inherit target run;
    };
}
