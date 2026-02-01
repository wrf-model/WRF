#!/bin/sh
help()
{
  echo "./buildCMake.sh [options]"
  echo "  -c                        Configuration build type, piped directly into configure"
  echo "  -b                        Build command passed into compile"
  echo "  -r                        Clean command passed into cleanCmake"
  echo "  -h                  Print this message"
  echo ""
}


echo "Input arguments:"
echo "$*"

while getopts c:b:r:h opt; do
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
    h)  help; exit 0 ;;
    *)  help; exit 1 ;;
    :)  help; exit 1 ;;
    \?) help; exit 1 ;;
  esac
done

# Now evaluate env vars in case it pulls from hostenv.sh
if [ ! -z "$envVars" ]; then
  setenvStr "$envVars"
fi

echo "./cleanCMake.sh -a $cleanCommand"
./cleanCMake.sh -a $cleanCommand
echo "Clean done"

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
