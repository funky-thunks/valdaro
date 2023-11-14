{ callPackage
, stdenv
, lib
, zephyr
, nodejs-18_x
, valdaro
}:

{ pname
, version
, src
, spagoPackages    ? "${src}/spago-packages.nix"
, nodeDependencies ? "${src}/node-dependencies.nix"
, tree-shaking     ? true
}:

let spagoPkgs = callPackage spagoPackages {};
    nodeDeps = (callPackage nodeDependencies {}).nodeDependencies;
    finalOutput = if tree-shaking then "dce-output" else "output";
in stdenv.mkDerivation {
    pname = "${pname}-frontend";
    inherit version;

    buildInputs = [
      spagoPkgs.installSpagoStyle
      spagoPkgs.buildFromNixStore
      valdaro.frontend.easy-ps.purs
      valdaro.frontend.esbuild
      valdaro.frontend.copy-css-modules
      nodejs-18_x
    ] ++ lib.lists.optional tree-shaking zephyr;

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

      ${lib.strings.optionalString tree-shaking ''
        echo "Tree shaking with zephyr"
        zephyr --codegen corefn,js --dce-foreign Main.main
      ''}

      copy-css-modules "${finalOutput}" "$src/src"
      cp ${./boot.js} ${finalOutput}/boot.js

      esbuild --platform=browser --format=esm --loader:.js=jsx --bundle --minify --outfile="$distDirectory/bundle.js" ${finalOutput}/boot.js
    '';

    installPhase = ''
      target="$out/share/assets"
      mkdir -p $target
      cp -r dist/. $target
    '';
  }
