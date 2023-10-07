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

          overlays.valdaro = nixpkgs.lib.fixedPoints.composeManyExtensions [ node2nix-hs.overlays.default otherPackages frontendOverlay haskellOverlay lintersOverlay packagingOverlay ];

          nixosModules.postgresql = import nixos/postgresql.nix;
        };

        otherPackages = import pkgs/overlay.nix;

        frontendOverlay = final: prev: {
          valdaro.frontend = {
            inherit (gitignore.lib) gitignoreSource;
            easy-ps = easy-purescript-nix.packages.${prev.system};
            inherit (nixpkgs-unstable.legacyPackages.${prev.system}) esbuild;
            mkShell = final.callPackage shells/frontend.nix {};
          } // final.callPackages frontend/package.nix {};
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

        packagingOverlay = final: prev:
          let callService = { frontend, backend, pname, version, nix-lint-source, extra-checks ? {} }:
                serviceOutputs (prepareService { inherit frontend backend pname version nix-lint-source; }) extra-checks;

              prepareService = { frontend, backend, pname, version, nix-lint-source }:
                rec
                  {
                    frontendPackage = final.valdaro.frontend.callPackage frontend { inherit pname version; };
                    backendPackage = final.callPackage backend {};
                    servicePackage = final.symlinkJoin {
                      name = "${pname}-${version}";
                      inherit version;
                      paths = [ backendPackage.package frontendPackage.package ];
                    };
                    checks = {
                      lint-haskell = "${final.hlint}/bin/hlint ${backend}";
                      lint-nix = final.valdaro.lint-nix nix-lint-source;
                    };
                  };

              tooling = prev.callPackage shells/tooling.nix { pname = "valdaro"; version = "0.0.1.0"; };

              serviceOutputs = svc@{ frontendPackage, backendPackage, servicePackage, ... }: checks:
                {
                  packages  = { frontend = frontendPackage.package; backend = backendPackage.package; default = servicePackage; };
                  devShells = { frontend = frontendPackage.shell;   backend = backendPackage.shell;   default = tooling; };
                  inherit (backendPackage) entrypoint;
                  app = { default = { type = "app"; program = backendPackage.entrypoint; }; };
                  checks = final.valdaro.mkChecks svc.checks // checks;
                };
            in { valdaro = prev.valdaro // { inherit callService; }; };

     in globals // flake-utils.lib.eachDefaultSystem (system:
          let pkgs = import nixpkgs {
                inherit system;
                overlays = [ globals.overlays.valdaro ];
              };

              server = pkgs.haskellPackages.valdaro-server;
           in {
                packages.server  = server;
                packages.default = server;

                devShells.server = server.env;
              }
        );
}
