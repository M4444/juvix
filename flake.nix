{
  description = "Juvix";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.haskellNix.inputs.nixpkgs.follows = "/nixpkgs";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  inputs.devshell.url = "github:numtide/devshell";

  outputs = { self, nixpkgs, flake-utils, haskellNix, devshell, ... }:
    let
      # NOTE: `nix flake show` requires `--impure` because some
      # packages rely on `builtins.currentSystem`.
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      # cross platforms to enable
      crossPlatforms = p: [ p.musl64 ];

      overlay = final: prev: {
        # System library tweaks
        numa = final.numactl; # So that haskell.nix knows to find the numa library (when -fnuma)
        llvm-config = final.llvm_9; # So that llvm-hs finds llvm-config
      };

      overlayHs = final: prev: {
        project = import ./project.nix { pkgs = final; };
        flake = final.project.flake { inherit crossPlatforms; };

        projectPure = with final;
        project.projectFunction haskell-nix (project.args // { modules = []; });
      };
    in

    flake-utils.lib.eachSystem supportedSystems (system:
      let
        # Fixes for packages when building with macOS
        darwinHacks = final: prev: {
          ldb = prev.ldb.overrideAttrs (oldAttrs: {
              wafConfigureFlags = ["--bundled-libraries=ALL" "--builtin-libraries=replace" "--without-ldb-lmdb"];
          });
        };

        overlays = [ devshell.overlay (import nix/docs.nix) overlay ] ++ (if (system == "x86_64-darwin") then [darwinHacks] else []);

        pkgs = import nixpkgs { inherit system overlays; };

        # Includes IOHK's many patches that cause cache misses,
        # so we only include them when building Haskell derivations.
        pkgsHs = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = [ haskellNix.overlay overlayHs ] ++ overlays;
        };

        mkAppsCross = project:
        pkgs.lib.mapAttrs' (name: _:
          let v = project.projectCross.musl64.getComponent name; in
          pkgs.lib.nameValuePair (v.stdenv.hostPlatform.config + "/" + name) {
            type = "app";
            program = v.exePath;
          });
      in
      with { inherit (pkgsHs) project flake; };

      {
        defaultPackage = flake.packages."juvix:exe:juvix";

        packages = flake.packages // {
          inherit (pkgs) juvix-docs;

					# Gathers all executables from project packages into one derivation
					executables = pkgsHs.buildEnv {
						name = "executables";
						paths = builtins.attrValues (
							flake-utils.lib.flattenTree (
								(pkgsHs.haskell-nix.haskellLib.collectComponents "exes" (p: p.isProject) project.hsPkgs)));
					};
        };

        checks = flake.checks;

        apps = flake.apps //
        # Add paths in apps for the cross packages
        mkAppsCross project flake.apps // {
          update-materialized = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "update-materialized";
              text = ''
                set -x
                ${project.stack-nix.passthru.calculateMaterializedSha} | tee "$'' + ''{1-nix/stack-sha256}"
                ${project.stack-nix.passthru.generateMaterialized} "$'' + ''{2-nix/materialized}"
              '';
            };
          };
        };

        devShell = pkgs.devshell.mkShell {
          name = "juvix-nix";
          commands = [
            {
              name = "stack-nix-shell";
              category = "haskell";
              command = ''exec nix develop .#stack-nix-shell "$@"'';
              help = "For use with 'stack'. Shell where you can use 'stack' to build with system dependencies provided via Nix.";
            }
            {
              name = "haskell-nix-shell";
              category = "haskell";
              command = ''exec nix develop .#haskell-nix-shell "$@"'';
              help = "Default haskell.nix project shell. Includes GHC with all non-local dependencies (packages) installed. (Build via Nix).";
            }
            {
              name = "hoogle-server";
              category = "haskell";
              command = ''exec nix develop .#hoogleShell -c hoogle server --local'';
              help = "Generate Hoogle index for a set of packages and launch the web interface.";
            }
            {
              name = "update-materialized";
              category = "nix";
              command = ''exec nix run .#update-materialized'';
              help = "Update (generate) `nix/stack-sha256` and `nix/materialized/`";
            }
            {
              name = "docs-server";
              category = "docs";
              command = ''
								[ $# -gt 0 ] || set -- -c env -C docs make html
								exec nix develop .#juvixDocsShell "$@"
							'';
              help = "Launch the Juvix docs preview server locally. Watches for changes and recompiles instantly.";
            }
          ];
        };

        lib.project = project;

        devShells = {
          # nix develop -c stack-nix-shell -c stack ghci
          stack-nix-shell = pkgsHs.callPackage nix/packages/stack-nix-shell.nix { ghcVersion = project.pkg-set.config.compiler.nix-name; };
          haskell-nix-shell = flake.devShell;
          juvixDocsShell = pkgs.juvix-docs;

          hoogleShell = pkgsHs.projectPure.shellFor {
            additional = ps: builtins.attrValues (pkgsHs.projectPure.pkgs.haskell-nix.haskellLib.selectProjectPackages ps);
            withHoogle = true;
          };
        };
      }
    ) // { inherit overlay; };
}
