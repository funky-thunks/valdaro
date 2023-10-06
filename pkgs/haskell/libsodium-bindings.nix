# cabal2nix https://github.com/haskell-cryptography/libsodium-bindings --subpath libsodium-bindings
{ mkDerivation, base, fetchgit, lib, libsodium }:
mkDerivation {
  pname = "libsodium-bindings";
  version = "0.0.1.0";
  src = fetchgit {
    url = "https://github.com/haskell-cryptography/libsodium-bindings";
    sha256 = "19yhsdir1zncww21a7nh4gz88zg0chsak12bsi5ci2wkhbkid0ip";
    rev = "06ea49cb4ded582b318b88d623816b090ca9679e";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/libsodium-bindings; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base ];
  librarySystemDepends = [ libsodium ];
  homepage = "https://github.com/haskell-cryptography/libsodium-bindings";
  description = "Static FFI bindings to libsodium";
  license = lib.licenses.bsd3;
}
