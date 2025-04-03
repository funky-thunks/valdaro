{
  mkHaskellOverlay =
    { deps ? []
    , extraDepsRoot ? ./.
    , localPackages ? {}
    , overrides ? (_: _: {})
    , compiler ? "ghc966"
    }:

    pkgsNew: pkgsOld:
      let extraDep = definition:
            if builtins.isString definition
            then extraDepWithoutNativeDeps definition
            else extraDepWithNativeDeps    definition;
          extraDepWithoutNativeDeps = name: callHaskellPackage name {};
          extraDepWithNativeDeps = { name, nativeDeps }: callHaskellPackage name (nativeDeps pkgsOld);
          callHaskellPackage = name: attrs:
            { "${name}" = pkgsNew.haskell.packages."${compiler}".callPackage (extraDepsRoot + "/${name}.nix") attrs; };
          extraDeps = names: pkgsNew.lib.lists.foldr (a: b: a // b) {} (map extraDep names);
          local = old: pkgsNew.lib.attrsets.mapAttrs (name: path: old.callCabal2nix name path {});
       in {
            haskell = pkgsOld.haskell // {
              packages = pkgsOld.haskell.packages // {
                "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (_: {
                  overrides = final: old: overrides final old // extraDeps deps // local old localPackages;
                });
              };
            };
          };
}
