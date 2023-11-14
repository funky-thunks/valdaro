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

function copyCssFile {
  local description="$1"
  local suffix="$2"
  local module="$3"

  cssFile="$(echo "$module" | sed -e 's-\.-/-' -e "s/$/${suffix}/")"
  if [ -r "$src/$cssFile" ]
  then
    echo "Copying $description for $module"
    cp "$src/$cssFile" "$outputDirectory/$module/$module$suffix"
  fi
}

function copyCssFiles {
  while read -r module
  do
    copyCssFile "CSS module" ".module.css" "$module"
    copyCssFile "Global CSS" ".css"        "$module"
  done
}

function proceed {
  listModules | copyCssFiles
}

proceed
