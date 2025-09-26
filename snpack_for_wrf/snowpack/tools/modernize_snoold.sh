#!/bin/bash
#This script reads old sno files and converts them to the newest standard, adding what is necessary

if [ $# -lt 1 ]; then
	me=`basename $0`
	printf "Usage: \n"
	printf "\t[$me {input_directory}] convert all snow files to the newest standard and put them in the current directory\n"
	exit 0
fi

INPUT_DIR=$1
files=`ls ${INPUT_DIR}/*.* | grep ".*\.sno"`

#generate the stations metadata for the map
convert2SMET() {
	awk '
	NR==1 {
		n=split(FILENAME, str, "/")
		if (n>1) filename=str[n]
		else filename=FILENAME
		n=split(filename, str, ".")
		if (n>2) 
			for(ii=1; ii<=n; ii++) filename=sprintf("%s.%s", filename, str[ii])
		else filename=str[1]
		
		n=split(filename, str, "_")
		station_name=filename
		if (n>1) station_id=str[n]
		else station_id="id"
	}
	/SNOWPACK_INITIALIZATION/ {
		printf("SMET 1.1 ASCII\n[HEADER]\n")
		next
	}
	/StationName/ {
		split($0, str, "=")
		station_name=str[2]
		next
	}
	/ProfileDate/ {
		printf("station_id = %s\nstation_name = %s\n", station_id, station_name)
		split($0, str, /=| /)
		prof_date=sprintf("%s-%s-%sT%s:%s", str[2], $2, $3, $4, $5)
		next
	}
	/HS_Last/ {
		gsub("=", " = ", $0)
		HS_Last=$0
		next
	}
	/Latitude/ {
		split($0, str, "=")
		lat=str[2]
		next
	}
	/Longitude/ {
		split($0, str, "=")
		lon=str[2]
		next
	}
	/Altitude/ {
		split($0, str, "=")
		alt=str[2]
		printf("longitude = %s\n", lon)
		printf("latitude = %s\n", lat)
		printf("altitude = %s\n", alt)
		printf("nodata = -999\ntz = 1\n")
		printf("ProfileDate = %s\n", prof_date)
		print HS_Last
		next
	}
	/WindScalingFactor/ {
		gsub("=", " = ", $0)
		WindScalingFactor=$0
		next
	}
	/ErosionLevel/ {
		gsub("=", " = ", $0)
		ErosionLevel=$0
		next
	}
	/TimeCountDeltaHS/ {
		gsub("=", " = ", $0)
		TimeCountDeltaHS=$0
		next
	}
	/CanopyDirectThroughfall/ {
		gsub("=", " = ", $0)
		print $0
		if (WindScalingFactor=="") printf("WindScalingFactor = 0\n")
		else print WindScalingFactor
		if (ErosionLevel=="") printf("ErosionLevel = 0\n")
		else print ErosionLevel
		if (TimeCountDeltaHS=="") printf("TimeCountDeltaHS = 0\n")
		else print TimeCountDeltaHS
		next
	}
	/=/{ #remaining header keys
		gsub("=", " = ", $0)
		print $0
		next
	}
	/^YYYY/ {
		printf("fields = timestamp")
		for(ii=6; ii<=NF; ii++) printf(" %s", $(ii))
		if (NF==21) printf(" CDot metamo")
		printf("\n[DATA]\n")
		next
	}
	/^[0-9][0-9][0-9][0-9] / {
		printf("%s-%s-%sT%s:%s",$1, $2, $3, $4, $5)
		for(ii=6; ii<=NF; ii++) printf(" %s", $(ii))
		if (NF==21) printf(" 0 0")
		printf("\n")
		next
	}
	' $1 > $2
}

#convert all the files in the given directory
for snofile in ${files}; do
	filename=$(basename "${snofile}")
	extension="${filename##*.}"
	filename="${filename%.*}"
	dest=`basename "${snofile}" ".${extension}" | xargs -i echo {}.sno`
	convert2SMET "${snofile}" "${dest}"
done