{
  description = "Juvix";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nix-bundle.url = "github:matthewbauer/nix-bundle";
  inputs.nix-bundle.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, flake-utils, haskellNix, nix-bundle }:
    # others not tested, but should work: "x86_64-darwin" "aarch64-darwin"
    let
      supportedSystems = [ "x86_64-linux" ];
    in

    flake-utils.lib.eachSystem supportedSystems (system:
      let

        # allows to override options passed to project.nix
        haskellNixCfg = { };

        # This affects the flake attribute  only
        defaultPackage = "juvix:exe:juvix";

        overlays = [
          haskellNix.overlay
          (import ./project.nix haskellNixCfg)
          (import ./nix/bundle.nix { inherit self system nix-bundle; })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.flake;
        #haskellLib = pkgs.haskell-nix.haskellLib;
      in

      pkgs.flake // {
        defaultPackage = flake.packages.${defaultPackage};

        apps = flake.apps // {
          bundle = pkgs.nix-bundle.app;
        }
        # export utilities to manage stack-nix materialized state as apps
        // pkgs.lib.genAttrs [ "calculateMaterializedSha" "generateMaterialized" "updateMaterialized" ] (app: {
          type = "app";
          program = builtins.toString (pkgs.writeShellScript app "${pkgs.project.stack-nix.passthru.${app}}");
        });

        # Pinned nixpkgs
        legacyPackages = pkgs;
      }
    );
}
