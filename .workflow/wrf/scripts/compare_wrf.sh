#!/bin/sh
diff_exec=$1
original=$2
shift
shift
folders=$*
echo "Comparing outputs stored in $original to $folders"

errorMsg=""
for folder in $folders; do
  echo ""
  echo ""
  echo ""
  echo "Checking $folder..."
  for io_type in input out; do
    echo "Checking $io_type..."
    for dom in d01 d02 d03; do
      if [ -e $original/wrf${io_type}_${dom}* ]; then
        echo ""
        echo "Checking $dom..."
        if [ ! -e $folder/wrf${io_type}_${dom}* ]; then
          # Domain does not exist in our current run but in the provided folder, that should not happen -FAIL!
          currentErrorMsg="Domain $( ls $original/wrf${io_type}_${dom}* ) file $dom exists in $original but not in $folder, cannot compare results"
          echo "$currentErrorMsg"
          errorMsg="$errorMsg\n$currentErrorMsg"
          continue
        fi

        $diff_exec $original/wrf${io_type}_${dom}* $folder/wrf${io_type}_${dom}*
        result=$?

        if [ $result -ne 0 ]; then
          currentErrorMsg="$diff_exec failed"
          echo "$currentErrorMsg"
          errorMsg="$errorMsg\n$currentErrorMsg"
          continue
        fi

        if [ -e fort.98 ] || [ -e fort.88 ]; then
          currentErrorMsg="$( ls $original/wrf${io_type}_${dom}* ) and $( ls $folder/wrf${io_type}_${dom}* ) differ"
          # if [ -n "$statDiff" ] && [ $statDiff -eq 0 ]; then
          #   currentErrorMsg="$currentErrorMsg - but are statistically equivalent based on checksum"
          # fi
          echo "$currentErrorMsg"
          cat fort.98 fort.88 2>/dev/null
          errorMsg="$errorMsg\n$currentErrorMsg"
          continue
        fi
      elif [ -e $folder/wrf${io_type}_${dom}* ]; then
        currentErrorMsg="Domain file $( ls $folder/wrf${io_type}_${dom}* ) exists but not in $original/, is this an error?"
        echo "$currentErrorMsg"
        errorMsg="$errorMsg\n$currentErrorMsg"
        continue
      # else 
        # neither has it, skip
        # echo "Domain file wrf${io_type}_${dom}* not generated for namelist $namelist, skipping check"
      fi
    done
  done
done

if [ -z "$errorMsg" ]; then
  echo "All comparisons equal"
else
  printf "%b" "$errorMsg"
  exit 1
fi
