# cabal2nix https://github.com/haskell-cryptography/libsodium-bindings --subpath sel --jailbreak > sel.nix
{ mkDerivation, base, base16, bytestring, fetchgit, hedgehog, lib
, libsodium-bindings, tasty, tasty-hunit, text, text-display
, transformers
}:
mkDerivation {
  pname = "sel";
  version = "0.0.3.0";
  src = fetchgit {
    url = "https://github.com/haskell-cryptography/libsodium-bindings";
    sha256 = "1w9x6yfl2714xn4qks8mqqh8z24iirbs6bhdjc5p98w2bkv8mghn";
    rev = "c6273e178fea77ff8e45960b2f58f5e6f0632702";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/sel; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base base16 bytestring libsodium-bindings text text-display
    transformers
  ];
  testHaskellDepends = [
    base base16 bytestring hedgehog libsodium-bindings tasty
    tasty-hunit text text-display
  ];
  jailbreak = true;
  homepage = "https://github.com/haskell-cryptography/libsodium-bindings";
  description = "Cryptography for the casual user";
  license = lib.licenses.bsd3;
}
