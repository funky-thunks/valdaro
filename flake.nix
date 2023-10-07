{
  description = "Valdaro - a modern haskell+purescript framework for SaaS projects";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs";
    nixpkgs-22_11.url = "github:nixos/nixpkgs/release-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    node2nix-hs.url = "github:ptitfred/node2nix-hs/v0.0.1.0";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix";
  };

  outputs = { nixpkgs, nixpkgs-unstable, nixpkgs-22_11, flake-utils, node2nix-hs, gitignore, easy-purescript-nix, ... }:
    let globals = {
          lib.valdaro = import ./lib.nix;

          overlays.valdaro = nixpkgs.lib.fixedPoints.composeManyExtensions [ node2nix-hs.overlays.default otherPackages frontendOverlay haskellOverlay lintersOverlay ];

          nixosModules.postgresql = import nixos/postgresql.nix;
        };

        otherPackages = import pkgs/overlay.nix;

        frontendOverlay = _: prev: {
          valdaro.frontend = {
            inherit (gitignore.lib) gitignoreSource;
            easy-ps = easy-purescript-nix.packages.${prev.system};
            inherit (nixpkgs-unstable.legacyPackages.${prev.system}) esbuild;
          };
        };

        haskellOverlay = globals.lib.valdaro.mkHaskellOverlay {
          extraDepsRoot = pkgs/haskell;
          deps = import server/extra-deps.nix;
          localPackages.valdaro-server = ./server;
        };

        lintersOverlay = _: prev: {
          valdaro = prev.valdaro // {
            lint-nix = (import nixpkgs-22_11 { inherit (prev) system; }).callPackage ./lint.nix {
              inherit (nixpkgs-unstable) lib;
            };

            mkChecks =
              let mkCheck = name: script:
                    prev.runCommand name {} ''
                      mkdir -p $out
                      ${script}
                    '';
               in prev.lib.attrsets.mapAttrs mkCheck;
          };
        };

     in globals // flake-utils.lib.eachDefaultSystem (system:
          let pkgs = import nixpkgs {
                inherit system;
                overlays = [ globals.overlays.valdaro ];
              };

              server = pkgs.haskellPackages.valdaro-server;

              tooling = pkgs.callPackage ./shell.nix { pname = "valdaro"; version = "0.0.1.0"; };

           in {
                packages.server  = server;
                packages.default = server;

                devShells.server = server.env;
                devShells.tooling = tooling;
              }
        );
}
