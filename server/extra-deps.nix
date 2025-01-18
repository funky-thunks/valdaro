[ "pg-transact" "biscuit-haskell" "biscuit-servant" "text-display" "sel" "text-builder-linear"
  { name = "libsodium-bindings"; nativeDeps = pkgs: { inherit (pkgs) libsodium; }; }
]
