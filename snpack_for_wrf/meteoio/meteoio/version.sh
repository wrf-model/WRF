# This script retrives the SVN version number of the tree on disk and displays
# a warning if it is unsuitable for an operational version (ie: composite version).
# cf http://subversion.tigris.org/faq.html#version-value-in-source
# Mathias Bavay - SLF - 02/2008

if [ $# -eq 1 ]; then
	FLAG="$1_"
else
	FLAG=""
fi

VERSION=$(svnversion -n ./)

NON_NUM=$(echo "${VERSION}" | grep "[^0-9]")
DATE=$(date +"%Y%m%d")

VER=$(printf "${FLAG}${DATE}.%s" "${VERSION}")
printf "***** Compiling Version ${VER}\n" > "/dev/stderr"

printf "${VER}"

