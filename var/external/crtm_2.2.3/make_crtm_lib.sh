#!/bin/sh
###############################################################################
#
# $Id: make_crtm_lib.sh 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $
#
# Script to configure, build, and install the CRTM library.
#
# * This script is a wrapper for the basic steps detailed in the README file.
#
# * Default installation structure is "NCO style" ala /nwprod/lib.
#
# * The build configuration setup is specified via files in the config-setup/ 
#   subdirectory that are sourced within this script.
#
# * The installation directory is ${PWD}
#
###############################################################################

usage()
{
  echo
  echo " Usage: make_crtm_lib.sh [-g|-h] setup-file"
  echo
  echo "   Script to configure, build, and install the CRTM library."
  echo
  echo '   The installation directory is ${PWD}'
  echo
  echo " Options:"
  echo "   -g          Perform a Gnu-style install into include/ and lib/ directories"
  echo "               The default is an NCO-style install to reflect the structure"
  echo "               of /nwprod/lib"
  echo
  echo "   -h          Print this message and exit"
  echo
  echo " Arguments:"
  echo '   setup-file  File, in the "config-setup/" subdirectory, that contains'
  echo "               the available build configuration setup (compiler and compiler"
  echo "               switches) that are sourced within this script."
  echo
  echo "               Currently available setup files are:"
  for file in `ls --hide=*.csh ./config-setup/`; do
    echo "     `basename ${file}`" >&2
  done
  echo
}


# Setup
# ...Definitions
SCRIPT_NAME=$(basename $0)
SUCCESS=0
FAILURE=1
# ...Defaults
INSTALL_TYPE="nco"


# Parse the command line options
while getopts :gh OPTVAL; do
  # Exit if option argument looks like another option
  case ${OPTARG} in
    -*) break;;
  esac
  # Parse the valid options
  case ${OPTVAL} in
    g) INSTALL_TYPE="gnu";;
    h)  usage
        exit ${SUCCESS};;
    :|\?) OPTVAL=${OPTARG}
          break;;
  esac
done
# ...Remove the options processed
shift $(expr ${OPTIND} - 1)
# ...Output invalidities based on OPTVAL
case ${OPTVAL} in
  # If OPTVAL contains nothing, then all options
  # have been successfully parsed and all that
  # remains are the arguments
  \?) if [ $# -lt 1 ]; then
        echo
        usage
        echo "${SCRIPT_NAME}: ERROR - Missing build setup-file argument"; echo
        exit ${FAILURE}
      fi;;
  # Invalid option
  ?) usage
     echo "${SCRIPT_NAME}: ERROR - Invalid option '-${OPTARG}'"; echo
     exit ${FAILURE};;
esac


# Source the build setup
SETUP_FILE="./config-setup/$1"
if [ ! -f ${SETUP_FILE} ]; then
  echo "${SCRIPT_NAME}: ERROR - Cannot find specified setup file ${SETUP_FILE}" >&2
  exit ${FAILURE}
fi
. ${SETUP_FILE}


# Check that the necessary dependent library environment variables have been defined
echo; echo
echo "==============================================================="
echo "${SCRIPT_NAME}: Checking for required environment variables"
echo "==============================================================="
echo
ENVAR_LIST="FC FCFLAGS"
for ENVAR_NAME in ${ENVAR_LIST}; do
  printf "  Checking %s..." "${ENVAR_NAME}"
  eval ENVAR=\$${ENVAR_NAME}
  if [ -z "${ENVAR}" ]; then
    echo "not defined"
    echo "${SCRIPT_NAME}: ERROR - Required environment variable ${ENVAR_NAME} not specified" >&2
    exit ${FAILURE}
  fi
  echo "defined as ${ENVAR}"
done


# Generate the makefiles
echo; echo; echo; echo
echo "==============================================================="
echo "==============================================================="
echo "${SCRIPT_NAME}: Configuring for build"
echo "==============================================================="
echo "==============================================================="
echo
./configure --prefix=${PWD}
if [ $? -ne 0 ]; then
  echo "${SCRIPT_NAME}: Error configuring for build" >&2
  exit ${FAILURE}
fi


# Build the current configuration
echo; echo
echo "==============================================================="
echo "${SCRIPT_NAME}: Starting build"
echo "==============================================================="
echo
make clean
make
if [ $? -ne 0 ]; then
  echo "${SCRIPT_NAME}: Error building" >&2
  exit ${FAILURE}
fi


# Install the current build...
if [ "${INSTALL_TYPE}" = "nco" ]; then
  echo; echo
  echo "==============================================================="
  echo "${SCRIPT_NAME}: Performing NCO-type install"
  echo "==============================================================="
  echo
  make nco_install
  if [ $? -ne 0 ]; then
    echo "${SCRIPT_NAME}: ERROR in NCO-style installation" >&2
    exit ${FAILURE}
  fi
else
  echo; echo
  echo "==============================================================="
  echo "Performing GNU-type install"
  echo "==============================================================="
  echo
  make install
  if [ $? -ne 0 ]; then
    echo "${SCRIPT_NAME}: ERROR in Gnu-style installation" >&2
    exit ${FAILURE}
  fi
fi


echo; echo
echo "==============================================================="
echo "${SCRIPT_NAME}: Running build check"
echo "==============================================================="
echo
make check
if [ $? -ne 0 ]; then
  echo "${SCRIPT_NAME}: Error running build check" >&2
  exit ${FAILURE}
fi


# Clean up
echo; echo
echo "==============================================================="
echo "${SCRIPT_NAME}: Cleaning up build products"
echo "==============================================================="
echo
make distclean
if [ $? -ne 0 ]; then
  echo "${SCRIPT_NAME}: Error cleaning up build products" >&2
  exit ${FAILURE}
fi
