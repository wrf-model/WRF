#!/bin/sh
buildDirectory=_build
installDirectory=install

help()
{
  echo "./cleanCMake.sh [options]"
  echo "  -c            [Default if no options] Basic cmake clean functionality [make -j 1 clean]"
  echo "  -b            Remove cmake binary installs [xargs rm < ${buildDirectory}/install_manifest.txt]"
  echo "  -f            Remove build & install folders (WRF) [ rm ${buildDirectory} -r; rm ${installDirectory}/ -r ]"
  echo "  -a            Remove all (WRF), equivalent to -c -b -f (more specifically -c then -b then-f)"
  echo "Specific builds/installs"
  echo "  -d directory  Specify operating on particular build directory"
  echo "  -i directory  Specify operating on particular install directory"
}

cleanBasicBuild=FALSE
cleanBasicInstall=FALSE
cleanLinks=FALSE
cleanFolders=FALSE
cleanAll=FALSE

while getopts "hcbfad:i:" opt; do
  case ${opt} in
    c)
      cleanBasicBuild=TRUE
    ;;
    b)
      cleanBasicInstall=TRUE
    ;;
    f)
      cleanFolders=TRUE
    ;;
    a)
      cleanAll=TRUE
    ;;
    d)
      buildDirectory=$OPTARG
    ;;
    i)
      installDirectory=$OPTARG
    ;;
    h)  help; exit 0 ;;
    *)  help; exit 1 ;;
    :)  help; exit 1 ;;
    \?) help; exit 1 ;;
  esac
done

if [ $OPTIND -eq 1 ]; then
  # Do basic clean I guess
  cleanBasicBuild=TRUE
fi

if [ "${cleanBasicBuild}" = "TRUE" ] || [ "${cleanAll}" = "TRUE" ]; then
  echo "Doing cmake make clean"
  OLD_DIR=$PWD
  cd ${buildDirectory} && make -j 1 clean > /dev/null 2>&1; cd $OLD_DIR
fi

if [ "${cleanBasicInstall}" = "TRUE" ] || [ "${cleanAll}" = "TRUE" ]; then
  echo "Removing binary installs"
  xargs rm < ${buildDirectory}/install_manifest.txt > /dev/null 2>&1
fi

if [ "${cleanFolders}" = "TRUE" ] || [ "${cleanAll}" = "TRUE" ]; then
  echo "Deleting ${buildDirectory} & ${installDirectory}/"
  rm ${buildDirectory} -r; rm ${installDirectory}/ -r > /dev/null 2>&1
fi
