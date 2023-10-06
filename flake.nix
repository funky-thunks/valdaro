{
  description = "Valdaro - a modern haskell+purescript framework for SaaS projects";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.05";
    flake-utils.url = "github:numtide/flake-utils";
    node2nix-hs.url = "github:ptitfred/node2nix-hs/v0.0.1.0";
  };

  outputs = { nixpkgs, flake-utils, node2nix-hs, ... }:
    let globals = {
          lib.valdaro = import ./lib.nix;

          overlays.valdaro = nixpkgs.lib.fixedPoints.composeManyExtensions [ node2nix-hs.overlays.default frontendOverlay haskellOverlay ];

          nixosModules.postgresql = import nixos/postgresql.nix;
        };

        frontendOverlay = import pkgs/overlay.nix;

        haskellOverlay = globals.lib.valdaro.mkHaskellOverlay {
          extraDepsRoot = pkgs/haskell;
          deps = import server/extra-deps.nix;
          localPackages.valdaro-server = ./server;
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
