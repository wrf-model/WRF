#!/bin/sh
#
# Run SNOWPACK
#   by C. Fierz 2013-06-09

ulimit -c unlimited	# To create core files in case of SNOWPACK crash

usage="Usage: `basename $0` [-h | --help] <mode> [--cfg <cfgfile>] [--eD <ISO-date>]  [--bin [<binPath>] [<binName>]] [--snoold] [suboutdir] [--nosnohaz] [--noslopes] [-rtvon]"

# Define user functions
exitus () {
	printf "\n\t%s\n" "Run SNOWPACK C++"
	printf "\t%s\n" "${usage}"
	printf "\t%s\n" "	use  nohup sn_run > fout  to redirect output to named file"
	printf "\t%s\n" "Parameter(s):"
	printf "\t\t%s\n" "mode		Choose from res (research) or oper (operational)"
	printf "\t%s\n" "Option(s):"
	printf "\t\t%s\n" "-h | --help => display help"
	printf "\t\t%s\n" "--cfg		choose configuration file for examples"
	printf "\t\t%s\n" "			optional arg: <cfgfile> or <all> to choose from all possibilities"
	printf "\t\t%s\n" "--eD		endDate, default NOW"
	printf "\t\t%s\n" "			optional arg: <ISO-date> in ISO-8061-format: yyyy[-mm[-dd[THH[:MM]]]]"
	printf "\t\t%s\n" "--bin		binPath: default ../../bin; release for ${HOME}/usr/bin or path of binary to use"
	printf "\t\t%s\n" "		binName: default snowpack"
	printf "\t\t%s\n" "--snoold	use sno-file format of rev 913 (*.snoold)"
	printf "\t\t%s\n" "--suboutdir	subdirectory for output ('"/research"' is default in research mode)"
	printf "\t\t%s\n" "--nosnohaz	do not save *.sno and *.haz files"
	printf "\t\t%s\n" "--noslopes	do not save slope results"
	printf "\t\t%s\n" "-r [F]		dump stdout and stderr [*.rec]"
	printf "\t\t%s\n" "-t [F]		timing statistics"
	printf "\t\t%s\n" "-v [F]		use valgrind (may need editing VGRIND in sn_run.sh)"
	printf "\t\t%s\n" "-o [F]		suppress oracle errors (use valgrind w/o rec-file)"
	printf "\t\t%s\n" "-n [F]		check correct setting w/o running"
	exit
}

################################################################################

# Check for arguments
if [ "$#" -lt 1 ]; then
	exitus
fi

# Initialize
# Set tools
TOOL=""
#VGRIND="valgrind --tool=callgrind --simulate-cache=yes --dump-instr=yes --trace-jumps=yes"
VGRIND="/software/bin/valgrind --tool=memcheck --leak-check=full --show-reachable=yes --track-origins=yes"

# Set parameters
mode="research"

# Set default for options
endDate="NOW"
ext="sno"
suboutdir=""
outsnohaz=0
outslopes=0
choosecfgfile=1
cfgfile=""
record=1
valgrind=1
oraerr=""
norun=1

# Set default for variables
binPath="../../bin"
#binPath="${HOME}/src/svn/snowpack/bin"
binName="snowpack"
#sdbo=1
sdbo=0 # to be used in Davos only

# Set directories
WORK_DIR="."
OP_TOOLS_DIR="${HOME}/alpine3d/snowpack-opera/snowpack-opera/snowpack"
#OP_TOOLS_DIR="${HOME}/src/svn/snowpack-opera/snowpack-opera/snowpack"
if [ ! -d "${OP_TOOLS_DIR}" ]; then printf "\n\t%s\n" "[E]: ${OP_TOOLS_DIR} does not exist"; exit; fi
LOG_DIR="${WORK_DIR}/logs"
CORE_DIR="${WORK_DIR}/cores"

# Read parameters and options
while [ "$#" -gt 0 ]; do
	par=${1}
	shift
	case ${par} in
		--*) option=$(echo ${par} | awk '{ print (substr($1, 3)) }')
			case ${option} in
				help)	exitus;;
				cfg)	choosecfgfile=0
						if [ "$(echo "'$1'" | awk '{ print(substr($1, 2, 1)) }')" != "-" ]; then
							cfgfile=$1
							shift
						fi;;
				eD)		if [ "$#" -gt 0 ]; then
							fchar=$(echo ${1} | awk '{ print (substr($1, 1, 1)) }')
							# check whether $1 starts with 1yyy, 2yyy, or NOW
							if [ "${fchar}" = "1" -o "${fchar}" = "2" -o "${1}" = "NOW" ]; then
								endDate=`${OP_TOOLS_DIR}/completeISODate.sh ${1}`
								shift
							fi
						fi;;
				bin)	i=0
						while [ "$#" -gt 0 -a ${i} -lt 2 ]; do
							if [ "$(echo "'$1'" | awk '{ print(substr($1, 2, 1)) }')" != "-" ]; then
								optPar=$1
								shift
								i=$((i+1))
							else
								break
							fi
							case ${optPar} in
								release) binPath=${HOME}/usr/bin;;
								.*)		 binPath=${optPar};;
								*)		 binName=${optPar};;
							esac
						done;;
				snoold)	ext="snoold"
						optRedate="os"
						printf "\t%s\n" "use SNOWPACK format *.snoold";;
				suboutdir)	suboutdir=${1}; shift;;
				nosnohaz)	outsnohaz=1;;
				noslopes)	outslopes=1;;
				*)	printf "\n\t%s%s%s\n" "[E]: No option '" "--${option}" "' available"; exitus;;
			esac;;
		-*) lenPar=`expr length ${par}`
			pos=3
			while [ "${pos}" -le $((lenPar+1)) ]; do
				option=$(echo "-${par}" | awk -v pos="$pos" '{ print (substr($1, pos, 1)) }')
				case ${option} in
					h)	exitus;;
					r)	record=0;;
					t)	TOOL="time";;
					v)	valgrind=0;;
					o)	oraerr="--suppressions=/home/fierz/alpine3d/snowpack/tools/valgrindOCCI.supp";;
					n)	norun=0;;
					?)	printf "\n\t%s%s%s\n" "[E]: No option '" "-${option}" "' available"; exitus;;
				esac
				pos=$((pos+1))
			done;;
		res)	mode="research";;
		oper)	mode="operational";;
		*) printf "\n\t%s%s%s\n" "[E]: '" "${par}" "' not expected or not a correct parameter!"; exitus;;
	esac
done

# choose cfgfile
if [ "${choosecfgfile}" -eq 0 -a "${cfgfile}" != "" ]; then
	if [ ! -s cfgfiles/${cfgfile} ]; then
		echo "Choose cfg file from list below or return to use io.ini:"
		ls -l cfgfiles/*.ini  | cut -d'/' -f2 | cut -d' ' -f1
		echo "Enter filename: "
		read dummy
	else
		dummy=${cfgfile}
	fi
elif [ "${cfgfile}" = "" ]; then
	echo "Choose cfg file from list below or return to use io.ini:"
	printf "\t%s\n" "antarctic"
	printf "\t%s\n" "res1exp"
	printf "\t%s\n" "res5exp"
	printf "\t%s\n" "operMST96"
	if [ "${sdbo}" -eq 0 ]; then
		printf "\t%s\n" "resDAV2"
	fi
	echo "Enter your choice: "
	read dummy
fi
if [ "${dummy}" = "" ]; then dummy="io.ini"; fi
if [ "${dummy}" != "io.ini" ]; then
	nf=`echo ${dummy} | awk 'BEGIN{FS="_"}{if (NF==1){print "1"}}'`
	if [ "${nf}" = "" ]; then
		cfgfile="cfgfiles/${dummy}"
	else
		cfgfile="cfgfiles/io_${dummy}.ini"
	fi
else
	cfgfile="io.ini"
fi

#Create directories if they don't exist
if [ ! -d "${LOG_DIR}" ]; then mkdir ${LOG_DIR}; fi
if [ ! -d "${CORE_DIR}" ]; then mkdir ${CORE_DIR}; fi

#Check if directories exist and could be created
if [ ! -d "${WORK_DIR}" ]; then printf "\n\t%s\n" "[E]: ${WORK_DIR} does not exist"; exit; fi
if [ ! -d "${LOG_DIR}" ]; then printf "\n\t%s\n" "[E]: ${LOG_DIR} could not be created..."; exit; fi
if [ ! -d "${CORE_DIR}" ]; then printf "\n\t%s\n" "[E]: ${CORE_DIR} could not be created..."; exit; fi

# set tools and log settings
run=`echo ${cfgfile} | cut -d'.' -f1 | cut -d'_' -f2`
if [ "${run}" = "" ]; then
	run="sn_run"
fi
exp=`grep EXPERIMENT ${cfgfile} | grep -v ";" | awk '{ print $3 }'`	# Determine experiment
if [ "${exp}" != "" ]; then
	run="${run}_${exp}"
fi
corefile=`echo ${WORK_DIR}/cores/${run}.core`	# Determine core file name
echo "Running ${binPath}/${binName} in ${mode} mode" > tmp.log
echo " 	       run: ${run}" >> tmp.log
echo " 	 arguments: -c ${cfgfile} -e ${endDate}" >> tmp.log
${binPath}/${binName} | head -3 >> tmp.log
if [ "${valgrind}" -eq 0 ]; then
	echo " 	     tools: ${TOOL} ${VGRIND} ${oraerr}" >> tmp.log
elif [ "${TOOL}" != "" ]; then
	echo " 	     tools: ${TOOL}" >> tmp.log
fi
echo "" >> tmp.log
cat tmp.log
if [ "${record}" -eq 0 ]; then
	logfile=`echo ${WORK_DIR}/logs/${run}.log`	# Determine log file name
	mv tmp.log ${logfile}
fi
if [ "${norun}" -eq 0 ]; then
	echo "nf=${nf} dummy=${dummy} endDate=${endDate}"
	echo "${mode}-mode: cfgfile=${cfgfile} binP=${binPath} binN=${binName}"
	exit
fi

# run SNOWPACK
snp="${binPath}/${binName} -c ${cfgfile} -m ${mode} -e ${endDate}"
if [ "${valgrind}" -eq 0 ]; then
	if [ "${record}" -eq 0 ]; then
		${TOOL} ${VGRIND} ${oraerr} --log-fd=2 ${snp} >> ${logfile} 2>&1
	else
		${TOOL} ${VGRIND} ${oraerr} ${snp}
	fi
else
	if [ "${record}" -eq 0 ]; then
		${TOOL} ${snp} >> ${logfile} 2>&1
	else
		${TOOL} ${snp}
	fi
fi

echo "Done running SNOWPACK !"
