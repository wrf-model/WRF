#!/bin/ksh

# da_list_refs.ksh

USAGE="da_list_refs.ksh [directory]"

DIR=${1:-$PWD}

cd $DIR

TEMP1=`mktemp`
ls | grep -v -E -e '^xrefs' > $TEMP1

TEMP2=`mktemp`

TEMP3=`mktemp`

TEMP4=`mktemp`

TEMP5=`mktemp`

TEMP6=`mktemp`

while IFS=" #" read FILE; do
  grep -i -s -E -e 'href' junk $FILE >> $TEMP2
done <$TEMP1

echo "href list produced"

while IFS=" #" read FILE; do
  OUT=xrefs.$FILE
  OUT=${OUT%%.html}.html

  # Print header:
  echo '<!DOCTYPE HTML PUBLIC "-//W3C/DTD HTML 4.01//EN">'        > $OUT
  echo '<html><head><title>References to '$FILE'</title></head>' >> $OUT 
  echo '<body>'                                                  >> $OUT
  echo '<h1>References to '$FILE'</h1>'                          >> $OUT
  echo 'Produced on '$(date)'<p>'                                >> $OUT
  echo '<a href="'$FILE'">Original</a><p>'                       >> $OUT

  grep -i $FILE $TEMP2 > $TEMP3

  # Numbered lines
  #---------------

  grep -i '.html:<A NAME="' $TEMP3 > $TEMP4

  sed s/'.html:<A NAME="'/'.html '/ $TEMP4 > $TEMP5

  # Remove references on dependency statements:
  grep -i -v ' INCLUDE ' $TEMP5 > $TEMP4
  grep -i -v ' Calls: '  $TEMP4 > $TEMP5

  # Remove self references:
  cut -f 1 -d \" $TEMP5 | grep -v $FILE > $TEMP6

  if (( $? == 0 )); then
    NUMBERED_REFS='true'
  else
    NUMBERED_REFS='false'
  fi

  if $NUMBERED_REFS; then
    awk '{print "<a href=\""$1"#"$2"\">"$1"</a> - line "$2"<br>"}' $TEMP6 >> $OUT
  fi

  # Un-numbered lines
  #------------------

  grep -i -v '.html:<A NAME="' $TEMP3 > $TEMP4

  # Remove self references
  cut -f 1 -d : $TEMP4 | grep -v $FILE > $TEMP5

  if (( $? == 0 )); then
    UNNUMBERED_REFS='true'
  else
    UNNUMBERED_REFS='false'
  fi

  if $UNNUMBERED_REFS; then
    awk -F'[:]' '{print "<a href=\""$1"\">"$1"</a><br>"}' $TEMP5 >> $OUT
  fi

  if [[ $NUMBERED_REFS = 'false' && $UNNUMBERED_REFS = 'false' ]]; then
    echo '<b><font color="maroon">No references</font></b>' >> $OUT
  fi

  echo '</body></html>' >> $OUT

done <$TEMP1

rm -f $TEMP1 $TEMP2 $TEMP3 $TEMP4 $TEMP5 $TEMP6

echo "da_list_refs.ksh finished"

exit 0

# EOF
