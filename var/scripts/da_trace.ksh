#!/bin/ksh

#  Purpose: Trace script usage

typeset NAME=$1
typeset DIR=$2

if [[ ! -z $DIR ]]; then
   shift 2
   typeset REST="$@"
   if [[ -d $DIR ]]; then
      # Make the link a relative directory within the suite
      echo $(date +'%D %T') "<A HREF=\"${DIR##${SUITE_DIR}/}/index.html\">$NAME</a> $REST"
   else
      echo $(date +'%D %T') "$NAME"
   fi
else
   shift 1
   typeset REST="$@"
   echo $(date +'%D %T') "$INIT $NAME" $REST
fi
