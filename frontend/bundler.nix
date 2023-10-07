{ valdaro
, zephyr
, writeShellApplication
}:

writeShellApplication {
  name = "valdaro-bundler";

  runtimeInputs = [
    valdaro.frontend.easy-ps.purs
    valdaro.frontend.easy-ps.spago
    valdaro.frontend.esbuild
    valdaro.frontend.copy-css-modules
    zephyr
  ];

  text = ''
    #!/usr/bin/env bash

    set -e

    distDirectory="dist"

    function bundle {
      esbuild --platform=browser --format=esm --loader:.css=local-css --bundle "$@"
    }

    if [ "''${1-}" == "dev-server" ]
    then
      mkdir -p output
      cp ${./live-reload.js} output/live-reload.js

      bundle --watch --servedir="./assets" --outfile="assets/bundle.js" output/live-reload.js

    else
      mkdir -p dce-output
      cp ${./boot.js} dce-output/boot.js

      cp -r assets/. "$distDirectory"

      spago build --purs-args '--codegen corefn,js'

      zephyr -f Main.main

      copy-css-modules "dce-output"

      bundle --minify --outfile="$distDirectory/bundle.js" dce-output/boot.js
    fi
  '';
}
