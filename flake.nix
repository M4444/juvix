{
  description = "Juvix";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nix-bundle.url = "github:matthewbauer/nix-bundle";
  inputs.nix-bundle.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, flake-utils, haskellNix, nix-bundle }:
    let
      projectOverlay = import ./nix/overlay.nix {
        crossPlatforms = p: [ p.musl64 ];
      };
      supportedSystems = [ "x86_64-linux" ]; # "x86_64-darwin" "aarch64-darwin"
    in
    {
      # TODO figure out why enabling this overlay here results in infinite recursion.
      # overlay = projectOverlay;
    }
    //

    flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay projectOverlay ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        haskellLib = pkgs.haskell-nix.haskellLib;
        project = pkgs.juvixProject;
        flake = pkgs.juvixFlake;
      in
      flake
      //
      {
        # Pinned nixpkgs
        legacyPackages = pkgs;

        # Misc library outputs
        lib = { inherit haskellLib project; };

        #checks = {
        #  build-all = haskellLib.selectLocalPackages project.hsPkgs;
        #  test
        #};

        # Built by `nix build .`
        defaultPackage = flake.packages."juvix:exe:juvix";

        # nix-bundle hack
        myBundler = { target, run }:
          let
            nix-bundle' = import nix-bundle { nixpkgs = pkgs; };
          in
          nix-bundle'.nix-bootstrap-nix {
            inherit target run;
          };

        apps = flake.apps
        //
        pkgs.lib.mapAttrs' (name: _: let
          v = project.projectCross.musl64.getComponent name;
          in pkgs.lib.nameValuePair
              (v.stdenv.hostPlatform.config + ":" + name)
              ({ type = "app"; program = v.exePath; })
        ) (flake.apps)
        //
        {
          # nix run .#bundle http:exe:juvix-server
          bundle = { type = "app"; program = builtins.toString (pkgs.writeShellScript "nix-bundle" ''
              if [ -z "$1" ]; then
                echo "Usage: $0 program"
                echo "  program: $(nix-instantiate --eval -E 'let x = builtins.getFlake "${toString self}"; in builtins.attrNames x.apps.${system}')"
                exit 1
              else
                target=$(nix-store --realize "$(nix-instantiate -E '(builtins.getFlake "${toString self}").packages.${system}."'"$1"'"')")
                exeName=$(basename $target/bin/*)
                out=$(nix-build -E 'with builtins.getFlake "${toString self}"; myBundler.${system} { target = "'$target'"; run = "'"/bin/$exeName"'"; }')
                echo $out
                ln -nsf $out "$exeName"
              fi
            '');
          };
        };
      }
    );
}
