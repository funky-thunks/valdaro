{ callPackage
, stdenv
, zephyr
, nodejs-18_x
, valdaro
}:

{ pname
, version
, src
, spagoPackages    ? "${src}/spago-packages.nix"
, nodeDependencies ? "${src}/node-dependencies.nix"
}:

let spagoPkgs = callPackage spagoPackages {};
    nodeDeps = (callPackage nodeDependencies {}).nodeDependencies;
in stdenv.mkDerivation {
    pname = "${pname}-frontend";
    inherit version;

    buildInputs = [
      spagoPkgs.installSpagoStyle
      spagoPkgs.buildFromNixStore
      valdaro.frontend.easy-ps.purs
      valdaro.frontend.esbuild
      valdaro.frontend.copy-css-modules
      zephyr
      nodejs-18_x
    ];

    ENVIRONMENT = "production";
    NODE_PATH = "${nodeDeps}/lib/node_modules";

    src = valdaro.frontend.gitignoreSource src;

    unpackPhase = ''
      cp -r $src/src .

      install-spago-style
    '';

    buildPhase = ''
      distDirectory="dist"
      mkdir -p $distDirectory
      cp -r $src/assets/. "$distDirectory"

      build-from-store --codegen corefn,js "./src/**/*.purs"

      zephyr --codegen corefn,js --dce-foreign Main.main

      copy-css-modules "dce-output" "$src/src"
      cp ${./boot.js} dce-output/boot.js

      esbuild --platform=browser --format=esm --loader:.css=local-css --bundle --minify --outfile="$distDirectory/bundle.js" dce-output/boot.js
    '';

    installPhase = ''
      target="$out/share/assets"
      mkdir -p $target
      cp -r dist/. $target
    '';
  }
