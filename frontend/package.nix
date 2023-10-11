{ callPackage
, writeShellApplication
, valdaro
}:

{
  copy-css-modules = writeShellApplication {
    name = "copy-css-modules";
    runtimeInputs = [ valdaro.frontend.easy-ps.spago ];
    text = builtins.readFile ./copy-css-modules.sh;
  };

  bundle = callPackage ./bundle.nix {};

  bundler = callPackage ./bundler.nix {};

  callPackage =
    src:
    { pname
    , version
    , spagoPackages    ? "${src}/spago-packages.nix"
    , nodeDependencies ? "${src}/node-dependencies.nix"
    , tree-shaking     ? true
    }:

    {
      package = valdaro.frontend.bundle {
        inherit pname version spagoPackages nodeDependencies src tree-shaking;
      };

      shell = valdaro.frontend.mkShell {
        inherit pname version;
      };
    };

  mkShell = callPackage ./shell.nix {};
}
