{ mkShell
, nodejs-18_x
, dhall
, zephyr
, valdaro
, node2nix-hs
}:

{ buildInputs ? []
, shellHook ? ""
, pname
, version
}:

mkShell {
  name = "${pname}-frontend-shell-${version}";
  buildInputs = [
    valdaro.frontend.easy-ps.purs
    valdaro.frontend.easy-ps.pulp
    valdaro.frontend.easy-ps.psc-package
    valdaro.frontend.easy-ps.purp
    valdaro.frontend.easy-ps.spago
    valdaro.frontend.easy-ps.psa
    valdaro.frontend.easy-ps.spago2nix
    valdaro.frontend.easy-ps.pscid
    valdaro.frontend.easy-ps.purescript-language-server
    valdaro.frontend.easy-ps.purs-tidy
    valdaro.frontend.easy-ps.purty
    valdaro.frontend.easy-ps.purs-backend-es
    valdaro.frontend.esbuild
    zephyr
    nodejs-18_x
    dhall
    node2nix-hs
  ] ++ buildInputs;
  shellHook = ''
    source <(spago --bash-completion-script `which spago`)
    source <(node --completion-bash)
    function debug_ps() {
      printf "%-25s" "  purs $(purs --version)"
      printf "%-25s" "  $(pulp --version | head -1)"
      printf "%-25s" "  psc-package $(psc-package --version)"
      echo ""
      printf "%-25s" "  spago $(spago --version)"
      printf "%-25s" "  psa $(psa --version)"
      printf "%-25s" "  pscid $(pscid --version)"
      echo ""
      printf "%-25s" "  purs-tidy $(purs-tidy --version)"
      printf "%-25s" "  purs-backend-es $(purs-backend-es --version 2>&1)"
      printf "%-25s" "  $(purty version)"
      echo ""
      printf "%-25s" "  node $(node --version)"
      printf "%-25s" "  esbuild $(esbuild --version)"
      printf "%-25s" "  zephyr $(zephyr --version)"
      echo ""
      printf "%-25s" "  node2nix-hs $(node2nix-hs --version)"
      printf "%-50s" "  purescript-language-server $(purescript-language-server --version)"
      echo ""
    }
    echo "Use 'debug_ps' to check versions."
    ${shellHook}
  '';
}
