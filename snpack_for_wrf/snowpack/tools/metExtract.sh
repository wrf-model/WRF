#!/bin/sh

# metExtract.sh
#   by C. Fierz 2012-08-29

# Initialize
# N.B.: 0 = true, 1 = false

usage="Usage: metExtract.sh [-h | --help] [dflt | ant | cal | opera] [OPTIONS]"
variant="dflt"
dateFormat=""
fileFormat=""
f=1
filename="*.met"
p=1
startDate=""
endDate=""
period=""

# Define user functions
exitus () {
	printf "\n%s\n" "	${usage}"
	printf "\t%s\n\n" "Extract fields from SNOWPACK *.met output files"
	printf "%s\n" "	Option(s):"
	printf "%s\n" "		-h | --help display help"
	printf "%s\n" "		-f file to extract from, for example \*tst\*.met (escape sequence necessary!)"
	printf "%s\n" "		     If none is given, all *.met files in the current directory will be considered"
	printf "%s\n" "		-p list of parameters given as a field list for cut, e.g. 3,6-8,12-"
	printf "%s\n" "		     If not given, the user will get a list to choose from"
	printf "%s\n" "		-b set startDate of extract, e.g. 11.08.2007 10:00"
	printf "%s\n" "		-e set endDate of extract, e.g. non-ISO 01.09.2008 09:00"
	printf "%s\n" "		--dateISO use ISO-format for dates in output"
	printf "%s\n" "		--fileR output one header line only (R-file)"
	printf "\n"
	exit
}

################################################################################

# if [ $# -lt 1 ]; then
# 	echo "You need at least to choose a variant from (dflt | ant | cal | opera)"
# 	exitus
# fi

# Read and set options
while [ $# -gt 0 ]; do
	par=$1
	shift
	case ${par} in
		-h|--help)
			exitus;;
		dflt|ant|cal|opera) variant=${par};;
		-f) filename=`echo "$1" | awk -F. '{print $1}'`
			filename="${filename}.met"
			shift;;
		-p) p=0
			fields=$1
			shift;;
		-b) startDate=`echo "$1 $2"`
			shift;shift;;
		-e) endDate=`echo $1 $2`
			shift;shift;;
		--dateISO) dateFormat="ISO";;
		--fileR) fileFormat="R";;
		*)  printf "\t%s\n" "No option ${par} available!"
			exitus;;
	esac
done

printf "\n\t%s\n" "Running variant '${variant}' of either (dflt | ant | cal | opera)"

headerFile=./metFields.${variant}.txt
if [ ${p} -eq 1 ]; then
	head -21 ${headerFile}
	printf "\t%s\n"  "Enter your choice as a field list for cut, e.g. 3,6-8,12-"
	read fields
fi

fieldNames=`tail -1 ${headerFile} | cut -d',' -f${fields}`
period=`echo ${startDate}`
if [ "${endDate}" != "" ]; then
	if [ "${period}" != "" ]; then
		period="from ${period} to ${endDate}"
	else
		period="from the beginning to ${endDate}"
	fi
elif [ "${startDate}" != "" ]; then
	period="from ${startDate} to the end"
else
	period="from the beginning to the end"
fi

printf "\t%s\n"  "Extract field(s) ${fieldNames}"
if [ "${period}" != "" ]; then
	printf "\t%s\n" "${period}"
fi


for file in `/bin/ls ${filename}`; do
	printf "\t%s\n"  "Working on ${file}"

	# Deal with date
	tail -n +17 ${file} > tmpD
	# Extract time period
	if [ "${startDate}" = "" ]; then
		startLine=1
	else
		startLine=`grep -n "${startDate}" tmpD | cut -d':' -f1`
	fi
	tail -n +${startLine} tmpD > tmpDa
	if [ "${endDate}" = "" ]; then
		endLine=`wc -l tmpDa | cut -d' ' -f1`
	else
		endLine=`grep -n "${endDate}" tmpDa | cut -d':' -f1`
	fi
	head -${endLine} tmpDa > tmpDat
	# transform date ?
	cut -d',' -f2 tmpDat > tmpDate
	if [ "${dateFormat}" = "ISO" ]; then
		cut -d' ' -f1 tmpDate | cut -d'.' -f1 > tmpDay
		cut -d' ' -f1 tmpDate | cut -d'.' -f2 > tmpMonth
		cut -d' ' -f1 tmpDate | cut -d'.' -f3 > tmpYear
		cut -d' ' -f2 tmpDate > tmpTime
		paste -d'-' tmpYear tmpMonth tmpDay > tmpDate
		paste -d' ' tmpDate tmpTime > tmpDatime
	else
		mv tmpDate tmpDatime
	fi
	# Extract data fields
	cut -d',' -f3- tmpDat > tmp2
	cut -d',' -f${fields} tmp2 | paste -d',' tmpDatime - > tmpData

	# Output
	headerLine=`grep -n HEADER ${file} | cut -d':' -f1`
	if [ "${fileFormat}" = "R" ]; then
		echo ${fieldNames} > header
	else
		echo "Extracted from ${file}" > header
		headerLine=$((headerLine+2))
		tail -n +${headerLine} ${file} | head -3 | cut -d',' -f3- | cut -d',' -f${fields} >> header
	fi
	fileout=`echo ${file} | cut -d'.' -f1`
	cat header tmpData > ${fileout}.txt

	rm -f tmp* header
done
