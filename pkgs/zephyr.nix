{ stdenv
, fetchurl
, lib
, fixDarwinDylibNames
, gmp
, zlib
, ncurses6
}:

stdenv.mkDerivation rec {
  pname = "zephyr";

  version = "0.5.2";

  src =
    if stdenv.hostPlatform.system == "x86_64-linux" then
      (fetchurl {
        url = "https://github.com/MaybeJustJames/zephyr/releases/download/v${version}/Linux.tar.gz";
        sha256 = "sha256-kHOaSqI5mS4OhfjbudQDtbtfsORrqDIf1SP/3rGL7pU=";
      })
    else if stdenv.hostPlatform.system == "x86_64-darwin" then
      (fetchurl {
        url = "https://github.com/MaybeJustJames/zephyr/releases/download/v${version}/macOS.tar.gz";
        sha256 = "sha256-PXxBttMaQBn7x6aHzR7ONIDJMWdEiWyyzw/xkoUWyBs=";
      })
    else
      throw "Architecture not supported";

  nativeBuildInputs = [ ]
    ++ lib.optional stdenv.isDarwin fixDarwinDylibNames;

  buildInputs = [
    stdenv.cc.cc.lib
    gmp
    zlib
    ncurses6
  ];

  libPath = lib.makeLibraryPath buildInputs;

  dontStrip = true;

  unpackPhase = ''
    mkdir -p $out/bin
    tar xf $src --strip 1 -C $out

    ZEPHYR=$out/bin/zephyr
    install -D -m555 -T $out/zephyr $ZEPHYR

    chmod u+w $ZEPHYR
  '' + lib.optionalString (!stdenv.isDarwin) ''
    patchelf --interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" --set-rpath ${libPath} $ZEPHYR
  '' + ''
    chmod u-w $ZEPHYR

    mkdir -p $out/etc/bash_completion.d/
    $ZEPHYR --bash-completion-script $ZEPHYR > $out/etc/bash_completion.d/zephyr-completion.bash
  '';

  dontInstall = true;
}
