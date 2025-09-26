#!/bin/bash
#This script extracts a given column out of a met or pro file

if [ $# -lt 1 ]; then
	printf "Extract parameters out of .pro or .met files (even if they are compressed with bzip2).\n"
	printf "  $0 <filename> \t\t to get the list of available parameters with their number\n"
	printf "  $0 <filename> <param_number>\t to extract a given parameter\n"
	exit
fi

#determine if we are dealing with a pro or met file
ext=`echo ${1##*.}`
if [  "${ext}" = "bz2"  ]; then
	is_bz_compressed=1
	base=`basename $1 .${ext}`
	ext=`echo ${base##*.}`
fi

cat_head() {
	if [ "${is_bz_compressed}" ]; then
		bzcat $1 | head -50
	else
		head -50 $1
	fi
}
 
list_met() {
	char_width=`tput cols`
	cat_head $1 | grep -E "^ID," | tr "," "\n" | nl | pr -2 -t -w ${char_width}
}

cat_all() {
	if [ "${is_bz_compressed}" ]; then
		bzcat $1
	else
		cat $1
	fi
}

extract_met() {
	station=`basename $1 .met`
	cat_all $1 | awk -F, '
		BEGIN {
			param="'"$2"'"
		}
		/^ID/ {
			printf("#'${station}'\n")
			printf("#Date %s\n", $(param))
		}
		/^\[DATA\]/ {
			in_data=1
			next
		}
		in_data==1 {
			date=$2
			gsub(" ",".",date)
			gsub(":",".",date)
			split(date,d,".")
			printf("%04d-%02d-%02dT%02d:%02d %g\n", d[3], d[2], d[1], d[4], d[5], $(param))
		}
		END {
			printf("\n")
		}
	'
}

list_pro() {
	cat_head $1 | awk -F, '
		BEGIN {
			OFS=" "
		}
		/^\[HEADER\]/ {
			in_header=1
			next
		}
		/^\[DATA\]/ {
			exit
		}
		/^#/ {
			next
		}
		in_header==1 {
			if($2=="Date") printf("0500  Date\n")
			else {
				$2=""
				print $0
			}
		}
	'
}

extract_pro() {
	station=`basename $1 .pro`
	cat_all $1 | awk -F, '
		BEGIN {
			param="'"$2"'"
		}
		/^#/ {
			next
		}
		/^\[HEADER\]/ {
			in_header=1
			next
		}
		in_header==1 && $1==param {
			$1=""
			$2=""
			printf("#'${station}'\n")
			printf("#Date %s\n", $0)
			in_header=0
			next
		}
		/^\[DATA\]/ {
			in_header=0
			in_data=1
			next
		}
		in_data==1 && $1=="0500" {
			date=$2
			gsub(" ",".",date)
			gsub(":",".",date)
			split(date,d,".")
			date=sprintf("%04d-%02d-%02dT%02d:%02d", d[3], d[2], d[1], d[4], d[5])
		}
		in_data==1 && $1==param {
			$1=""
			$2=""
			printf("%s %s\n", date, $0)
		}
		END {
			printf("\n")
		}
	'
}

################################################

if [ $# -eq 1 ]; then
	if [ "$ext" = "met" ]; then
		list_met $1
	fi
	if [ "$ext" = "pro" ]; then
		list_pro $1
	fi
fi
if [ $# -eq 2 ]; then
	if [ "$ext" = "met" ]; then
		extract_met $1 $2
	fi
	if [ "$ext" = "pro" ]; then
		extract_pro $1 $2
	fi
fi

