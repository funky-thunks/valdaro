# cabal2nix https://github.com/haskell-cryptography/libsodium-bindings --subpath sel --jailbreak
{ mkDerivation, base, base16, bytestring, fetchgit, hedgehog, lib
, libsodium-bindings, tasty, tasty-hunit, text, text-display
}:
mkDerivation {
  pname = "sel";
  version = "0.0.1.0";
  src = fetchgit {
    url = "https://github.com/haskell-cryptography/libsodium-bindings";
    sha256 = "19yhsdir1zncww21a7nh4gz88zg0chsak12bsi5ci2wkhbkid0ip";
    rev = "06ea49cb4ded582b318b88d623816b090ca9679e";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/sel; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base base16 bytestring libsodium-bindings text text-display
  ];
  testHaskellDepends = [
    base base16 bytestring hedgehog libsodium-bindings tasty
    tasty-hunit text
  ];
  jailbreak = true;
  homepage = "https://github.com/haskell-cryptography/libsodium-bindings";
  description = "Cryptography for the casual user";
  license = lib.licenses.bsd3;
}
