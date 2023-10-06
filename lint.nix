{ nix-linter
, writeShellScript
, lib
}:

{ src
, exceptions ? []
}:

let exclusions = lib.strings.concatMapStrings toExclusion exceptions;
    toExclusion = exception: "! -path ${src}/${lib.strings.removePrefix "./" (lib.path.removePrefix src exception)} ";
 in

writeShellScript "nix-linter" ''
  set -e

  find "${src}" -type f -name "*.nix" ${exclusions} -exec ${nix-linter}/bin/nix-linter {} + && echo "Everything is fine!"
''
