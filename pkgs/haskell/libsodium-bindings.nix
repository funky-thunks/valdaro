# cabal2nix https://github.com/haskell-cryptography/libsodium-bindings --subpath libsodium-bindings > libsodium-bindings.nix
{ mkDerivation, base, fetchgit, lib, libsodium }:
mkDerivation {
  pname = "libsodium-bindings";
  version = "0.0.3.0";
  src = fetchgit {
    url = "https://github.com/haskell-cryptography/libsodium-bindings";
    sha256 = "1w9x6yfl2714xn4qks8mqqh8z24iirbs6bhdjc5p98w2bkv8mghn";
    rev = "c6273e178fea77ff8e45960b2f58f5e6f0632702";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/libsodium-bindings; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base ];
  librarySystemDepends = [ libsodium ];
  homepage = "https://github.com/haskell-cryptography/libsodium-bindings";
  description = "FFI bindings to libsodium";
  license = lib.licenses.bsd3;
}
