outputDirectory="output"
src="./src"

if [ -n "${1-}" ]
then
  outputDirectory="$1"
else
  outputDirectory="$(spago path output)"
fi

if [ -n "${2-}" ]
then
  src="$2"
fi

function listModules {
  find "$outputDirectory" -mindepth 1 -type d | sed -e "s/${outputDirectory}\///"
}

function copyCssModule {
  local module="$1"
  cssModule="$(echo "$module" | sed -e 's-\.-/-' -e 's/$/.css/')"
  if [ -r "$src/$cssModule" ]
  then
    echo Copying css module for "$module"
    cp "$src/$cssModule" "$outputDirectory/$module/$module.css"
  fi
}

function copyCssModules {
  while read -r module; do copyCssModule "$module"; done
}

function proceed {
  listModules | copyCssModules
}

proceed
