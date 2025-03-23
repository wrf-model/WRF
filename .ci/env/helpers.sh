#!/bin/sh

# Useful string manipulation functions, leaving in for posterity
# https://stackoverflow.com/a/8811800
# contains(string, substring)
#
# Returns 0 if the specified string contains the specified substring,
# otherwise returns 1.
contains()
{
  string="$1"
  substring="$2"
  
  if [ "${string#*"$substring"}" != "$string" ]; then
    echo 0    # $substring is in $string
  else
    echo 1    # $substring is not in $string
  fi
}

setenvStr()
{
  # Changing IFS produces the most consistent results
  tmpIFS=$IFS
  IFS=","
  string="$1"
  for s in $string; do
    if [ ! -z $s ]; then 
      eval "echo export \"$s\""
      eval "export \"$s\""
    fi
  done
  IFS=$tmpIFS
}

banner()
{
  lengthBanner=$1
  shift
  # https://www.shellscript.sh/examples/banner/
  printf "#%${lengthBanner}s#\n" | tr " " "="
  printf "# %-$(( ${lengthBanner} - 2 ))s #\n" "`date`"
  printf "# %-$(( ${lengthBanner} - 2 ))s #\n" " "
  printf "# %-$(( ${lengthBanner} - 2 ))s #\n" "$*"
  printf "#%${lengthBanner}s#\n" | tr " " "="
}
