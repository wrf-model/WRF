#!/bin/sh
help()
{
  echo "./build.sh [options]"
  echo "  -c                        Configuration build type, piped directly into configure"
  echo "  -n                        Configuration nesting type, piped directly into configure"
  echo "  -o                        Configuration optstring passed into configure"
  echo "  -b                        Build command passed into compile"
  echo "  -d                        Build directory, default is './'"
  echo "  -h                  Print this message"
  echo ""
}

echo "Input arguments:"
echo "$*"

buildDirectory="./"
while getopts c:n:o:b:d:h opt; do
  case $opt in
    c)
      configuration="$OPTARG"
    ;;
    n)
      nesting="$OPTARG"
    ;;
    o)
      configOpt="$OPTARG"
    ;;
    b)
      buildCommand="$OPTARG"
    ;;
    d)
      buildDirectory="$OPTARG"
    ;;
    h)  help; exit 0 ;;
    *)  help; exit 1 ;;
    :)  help; exit 1 ;;
    \?) help; exit 1 ;;
  esac
done

if [ "$buildDirectory" != "./" ]; then
  if [ -d "$buildDirectory" ]; then
    echo "Removing $buildDirectory"
    rm -rf $buildDirectory
  fi
  echo "Copying WRF to $buildDirectory"
  mkdir -p $buildDirectory
  cp arch chem clean cleanCMake.sh cmake CMakeLists.txt \
     compile compile_new confcheck \
     configure configure_new doc \
     dyn_em external frame hydro \
     inc LICENSE.txt main Makefile phys \
     README README.md Registry run share \
     test tools var wrftladj \
     $buildDirectory -ar
  cd $buildDirectory
fi

./clean -a

echo "Compiling with option $configuration nesting=$nesting and additional flags '$configOpt'"
./configure $configOpt << EOF
$configuration
$nesting
EOF

if [ ! -f configure.wrf ]; then
  echo  "Failed to configure"
  exit 1
fi

echo "./compile $buildCommand"
./compile $buildCommand

result=$?

if [ $result -ne 0 ]; then
  echo "Failed to compile"
  exit 1
fi

# And a *very* special check because WRF compiles the WRF way and force-ignores all make errors
# putting the onus on US to check for things
if [ ! -x ./main/wrf.exe ]; then # There's a bunch of other execs but this is the most important and 
                                 # doing more checks to accomodate just reinforces this bad design
  echo "Failed to compile"
  exit 1
fi
