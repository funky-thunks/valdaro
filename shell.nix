{ mkShell, biscuit-cli, pname, version }:

mkShell {
  name = "${pname}-${version}-tooling";
  buildInputs = [ biscuit-cli ];
}
