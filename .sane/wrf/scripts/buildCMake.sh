#!/bin/sh
help()
{
  echo "./buildCMake.sh [options]"
  echo "  -c                        Configuration build type, piped directly into configure"
  echo "  -b                        Build command passed into compile"
  echo "  -r                        Clean command passed into cleanCmake"
  echo "  -f                        Fresh build (clean old build if it exists)"
  echo "  -h                  Print this message"
  echo ""
}


echo "Input arguments:"
echo "$*"

freshBuild="true"
while getopts c:b:r:f:h opt; do
  case $opt in
    c)
      configuration="$OPTARG"
    ;;
    b)
      buildCommand="$OPTARG"
    ;;
    r)
      cleanCommand="$OPTARG"
    ;;
    f)
      freshBuild=$( echo "$OPTARG" | tr '[:upper:]' '[:lower:]' )
    ;;
    h)  help; exit 0 ;;
    *)  help; exit 1 ;;
    :)  help; exit 1 ;;
    \?) help; exit 1 ;;
  esac
done

if [ "$freshBuild" = "true" ]; then
  echo "./cleanCMake.sh -a $cleanCommand"
  ./cleanCMake.sh -a $cleanCommand
  echo "Clean done"
fi

echo "./configure_new $configuration"
./configure_new $configuration
result=$?
echo "Configure done"

if [ $result -ne 0 ]; then
  echo  "Failed to configure, command returned non-zero exit status"
  exit 1
fi

echo "./compile_new $buildCommand"
./compile_new $buildCommand
result=$?
echo "Compile done"

if [ $result -ne 0 ]; then
  echo "Failed to compile, command returned non-zero exit status"
  exit 1
fi
