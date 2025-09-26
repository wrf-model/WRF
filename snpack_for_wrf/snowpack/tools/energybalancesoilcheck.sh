#!/bin/bash
# Provide met file as first argument.
# Example for quickly plotting in gnuplot the energy balance, provided the output file is called output.txt:
# pl "<(cat ./output.txt | awk '{sum+=$16; print sum}')" u 1 w l title 'energy in', "<(cat ./output.txt | awk '{sum+=$17; print -1.0*sum}')" w l title 'energy out', "<(cat ./output.txt | awk '{sum+=$15; print 1.0*sum}')" w l title 'storage'
if [ $# -lt 1 ]; then
        echo "This script reads a met-file (provided as first argument) and writes the energy balance on the stdout and statistics to stderr." > /dev/stderr
	echo "Invoke with: ./energybalancesoilcheck.sh <met file> [firstdate=YYYYMMDD] [lastdate=YYYYMMDD]" > /dev/stderr
	echo "" > /dev/stderr
	echo "Note: 1) the energy balance represents only the snow cover energy balance!" > /dev/stderr
	echo "      2) the energy balance can only be properly checked when the output resolution of the met file is the" > /dev/stderr
	echo "         same as the snowpack calculation step length." > /dev/stderr
	echo "      3) using options firstdate and lastdate, one can define a period over which the mass balance should be" > /dev/stderr
	echo "         determined. Default is full period in met-file. No spaces in command line options are allowed!" > /dev/stderr
	echo "" > /dev/stderr
	echo "How to interpret the results?" > /dev/stderr
	echo "  The script distinghuishes between two situations:" > /dev/stderr
	echo "  1) If a snow cover is present, the energy change of the soil is the sum of the bottom heat flux and the soil-snow heat flux." > /dev/stderr
	echo "     These fluxes are shown in the output and accordingly, all other fluxes (e.g., turbulent fluxes) are set to 0.0." > /dev/stderr
	echo "     This is not because they are 0.0, but because they are not directly contributing to the energy change of the soil." > /dev/stderr
	echo "  1) If a snow cover is not present, the energy change of the soil is the sum of the bottom heat flux and the sum of" > /dev/stderr
	echo "     turbulent and radiative fluxes at the surface. These fluxes are shown in the output." > /dev/stderr
	echo "" > /dev/stderr
	echo "Examples:" > /dev/stderr
	echo " ./energybalancesoilcheck.sh WFJ2_flat.met > output.txt	Writes energy balance in output.txt and shows overall energy balance statistics on screen." > /dev/stderr
	echo " ./energybalancesoilcheck.sh WFJ2_flat.met > /dev/null	Just shows overall energy balance statistics on screen." > /dev/stderr
	echo " ./energybalancesoilcheck.sh WFJ2_flat.met | less		View the energy balance in less." > /dev/stderr
	echo " ./energybalancesoilcheck.sh WFJ2_flat.met firstdate=20071001 lastdate=20080323" > /dev/stderr
	echo "								  Determines energy balance between 1st of October 2007" > /dev/stderr
	echo "								     up to and including 23rd of March 2008." > /dev/stderr
        exit
fi


# Initial settings
firstdate=0
lastdate=99999999


# Get met file name from first argument
met_file=$1


# Read command line parameters
if [ $# -gt 1 ]; then
	for i in `seq 2 $#`
	do
		eval "let \$$i"
	done
fi


# Check if file exists
if [ ! -e "${met_file}" ]; then
	echo "energybalancesoilcheck.sh: ERROR: file ${met_file} does not exist or cannot be opened!" > /dev/stderr
	exit
fi

# Check if file is not empty
if [ ! -s "${met_file}" ]; then
	echo "energybalancesoilcheck.sh: ERROR: file ${met_file} is empty!" > /dev/stderr
	exit
fi

# Read header from met file
header=`cat ${met_file} | grep -m 1 ^ID`
if [ -z "${header}" ]; then
	echo "energybalancesoilcheck.sh: ERROR: no header found." > /dev/stderr
	exit
fi


# Determine column mapping
#  -- date and time
coldatetime=`echo ${header} | sed 's/,/\n/g' | grep -nx "Date" | awk -F: '{print $1}'`

#  -- snow height
colhsmeasured=`echo ${header} | sed 's/,/\n/g' | grep -nx "Measured snow depth HS" | awk -F: '{print $1}'`
colhsmodel=`echo ${header} | sed 's/,/\n/g' | grep -nx "Modelled snow depth (vertical)" | awk -F: '{print $1}'`

#  -- energy balance terms
colSHF=`echo ${header} | sed 's/,/\n/g' | grep -nx "Sensible heat" | awk -F: '{print $1}'`
colLHF=`echo ${header} | sed 's/,/\n/g' | grep -nx "Latent heat" | awk -F: '{print $1}'`
colOLWR=`echo ${header} | sed 's/,/\n/g' | grep -nx "Outgoing longwave radiation" | awk -F: '{print $1}'`
colILWR=`echo ${header} | sed 's/,/\n/g' | grep -nx "Incoming longwave radiation" | awk -F: '{print $1}'`
colNetLWR=`echo ${header} | sed 's/,/\n/g' | grep -nx "Net absorbed longwave radiation" | awk -F: '{print $1}'`
colRSWR=`echo ${header} | sed 's/,/\n/g' | grep -nx "Reflected shortwave radiation" | awk -F: '{print $1}'`
colISWR=`echo ${header} | sed 's/,/\n/g' | grep -nx "Incoming shortwave radiation" | awk -F: '{print $1}'`
colsoilheat=`echo ${header} | sed 's/,/\n/g' | grep -nx "Heat flux at ground surface" | awk -F: '{print $1}'`
colbottomheat=`echo ${header} | sed 's/,/\n/g' | grep -nx "Heat flux at bottom of snow or soil pack" | awk -F: '{print $1}'`
colRainNRG=`echo ${header} | sed 's/,/\n/g' | grep -nx "Heat advected to the surface by liquid precipitation" | awk -F: '{print $1}'`
colIntNRG=`echo ${header} | sed 's/,/\n/g' | grep -nx "Internal energy change soil" | awk -F: '{print $1}'`
colPhchEnergy=`echo ${header} | sed 's/,/\n/g' | grep -nx "Melt freeze part of internal energy change soil" | awk -F: '{print $1}'`

error=0
if [ -z "${coldatetime}" ]; then
	echo "energybalancesoilcheck.sh: ERROR: date/time not found in one of the columns." > /dev/stderr
	error=1
fi
if [ -z "${colhsmeasured}" ]; then
	echo "massbalancecheck.sh: ERROR: measured hs not found in one of the columns." > /dev/stderr
	error=1
fi
if [ -z "${colhsmodel}" ]; then
	echo "massbalancecheck.sh: ERROR: modeled hs not found in one of the columns." > /dev/stderr
	error=1
fi
if [ -z "${colSHF}" ]; then
	echo "energybalancesoilcheck.sh: ERROR: sensible heat flux (SHF) not found in one of the columns." > /dev/stderr
	error=1
fi
if [ -z "${colLHF}" ]; then
	echo "energybalancesoilcheck.sh: ERROR: latent heat flux (LHF) not found in one of the columns." > /dev/stderr
	error=1
fi
if [ -z "${colOLWR}" ]; then
	echo "energybalancesoilcheck.sh: ERROR: OLWR not found in one of the columns." > /dev/stderr
	error=1
fi
if [ -z "${colILWR}" ]; then
	echo "energybalancesoilcheck.sh: ERROR: ILWR not found in one of the columns." > /dev/stderr
	error=1
fi
if [ -z "${colNetLWR}" ]; then
	echo "energybalancesoilcheck.sh: ERROR: Net_LWR not found in one of the columns." > /dev/stderr
	error=1
fi
if [ -z "${colRSWR}" ]; then
	echo "energybalancesoilcheck.sh: ERROR: RSWR not found in one of the columns." > /dev/stderr
	error=1
fi
if [ -z "${colISWR}" ]; then
	echo "energybalancesoilcheck.sh: ERROR: ISWR not found in one of the columns." > /dev/stderr
	error=1
fi
if [ -z "${colsoilheat}" ]; then
	echo "energybalancesoilcheck.sh: ERROR: soil heat flux not found in one of the columns." > /dev/stderr
	error=1
fi
if [ -z "${colbottomheat}" ]; then
	echo "energybalancesoilcheck.sh: ERROR: bottom heat flux not found in one of the columns." > /dev/stderr
	error=1
fi
if [ -z "${colRainNRG}" ]; then
	echo "energybalancesoilcheck.sh: ERROR: rain energy not found in one of the columns." > /dev/stderr
	error=1
fi
if [ -z "${colIntNRG}" ]; then
	echo "energybalancesoilcheck.sh: ERROR: internal energy change soil not found in one of the columns." > /dev/stderr
	echo "  Make sure to set OUT_HAZ = FALSE and OUT_SOILEB = TRUE in the ini-file." > /dev/stderr
	error=1
fi
if [ -z "${colPhchEnergy}" ]; then
	echo "energybalancesoilcheck.sh: ERROR: phase change energy soil not found in one of the columns." > /dev/stderr
	echo "  Make sure to set OUT_HAZ = FALSE and OUT_SOILEB = TRUE in the ini-file." > /dev/stderr
	error=1
fi
if [ "${error}" -eq 1 ]; then
	exit
fi

# -- Determine file resolution
nsamplesperday=$(cat ${met_file} | sed '1,/\[DATA\]/d' | awk -F, '{year=substr($2,7,4);month=substr($2,4,2);day=substr($2,1,2);hour=substr($2,12,2);minute=substr($2,15,2);doy1=mktime(sprintf("%04d %02d %02d %02d %02d %02d %d", year, month, day, hour, minute, 0, 0)); print (24*60*60)/(doy1-doy2); doy2=doy1}' | sort | uniq -c | sort -nu | awk '{print $2}' | tail -1)
if [ -z "${nsamplesperday}" ]; then
	echo "energybalancesoilcheck.sh: ERROR: file resolution could not be determined." > /dev/stderr
	exit
fi


# Create header
echo "#Date time measured_HS modelled_HS SHF     LHF    OLWR   ILWR_absorb   RSWR   ISWR   SoilHeatFlux BottomHeatFlux RainEnergy PhaseChangeEnergy deltaIntEnergy EnergyBalance energy_in energy_out"
echo "#--   --   --          --          E+      E+     E+     E+            E+     E+     E+           E+             E+         --                E-             error         totals    totals"
echo "#-    -    cm          cm          W_m-2   W_m-2  W_m-2  W_m-2         W_m-2  W_m-2  W_m-2        W_m-2          W_m-2      W_m-2             W_m-2          W_m-2         W_m-2     W_m-2"


# Process data (note that the lines below are all piped together).
#  -- Cut out data
sed '1,/\[DATA\]/d' ${met_file} | \
#  -- Select all the energybalance terms, make them correct sign and correct units. Also makes sure some terms are only considered when they are a part of the SNOW energy balance (like SHF, which may also originate from soil).
#     Note: some terms need a change of sign, others need to be converted from kJ/m^2 to W/m^2 and one needs an extra term to be added.
awk -F, '{print $'${coldatetime}', $'${colhsmeasured}', $'${colhsmodel}', ($'${colhsmodel}'==0)?($'${colSHF}'):0, ($'${colhsmodel}'==0)?($'${colLHF}'):0, ($'${colhsmodel}'==0)?-1.0*($'${colOLWR}'):0, ($'${colhsmodel}'==0)?($'${colOLWR}'+$'${colNetLWR}'):0, ($'${colhsmodel}'==0)?-1.0*($'${colRSWR}'):0, ($'${colhsmodel}'==0)?($'${colISWR}'):0, ($'${colhsmodel}'>0.0)?(-1.0*$'${colsoilheat}'):0, $'${colbottomheat}', ($'${colhsmodel}'==0)?($'${colRainNRG}'):0, $'${colPhchEnergy}'*(1000.0/((24.0/'${nsamplesperday}')*3600)), ($'${colIntNRG}'!=-999.0)?(1000.0*$'${colIntNRG}'/((24.0/'${nsamplesperday}')*3600)):0}' | \
#  -- Reformat time
sed 's/\./ /'  | sed 's/\./ /' | sed 's/:/ /' | awk '{printf "%04d%02d%02d %02d%02d", $3, $2, $1, $4, $5; for(i=6; i<=NF; i++) {printf " %s", $i}; printf "\n"}' | \
# Now select period
awk '($1>='${firstdate}' && $1<='${lastdate}') {print $0}' | \
#  -- Now do all the other calculations
#     except for the first line (for the first line, we cannot determine the energy balance, as the previous value of modeled HS is unknown, so we don't know whether there was a snowpack).
awk '{ \
	#Determine energy balance error:
	energybalance=$5+$6+$7+$8+$9+$10+$11+$12+$13-$15; \
	#Determine energy input in system (taking the terms only when they are positive)
	energy_in=(($5>0.0)?$5:0)+(($6>0.0)?$6:0)+(($7>0.0)?$7:0)+(($8>0.0)?$8:0)+(($9>0.0)?$9:0)+(($10>0.0)?$10:0)+(($11>0.0)?$11:0)+(($12>0.0)?$12:0)+(($13>0.0)?$13:0); \
	#Determine energy output in system (taking the terms only when they are negative)
	energy_out=(($5<0.0)?$5:0)+(($6<0.0)?$6:0)+(($7<0.0)?$7:0)+(($8<0.0)?$8:0)+(($9<0.0)?$9:0)+(($10<0.0)?$10:0)+(($11<0.0)?$11:0)+(($12<0.0)?$12:0)+(($13<0.0)?$13:0); \
	#Do the statistics (energy balance error sum, min and max values)
	ndatapoints++; energybalancesum+=energybalance; energybalancesum2+=sqrt(energybalance*energybalance); incomingsum+=energy_in; if(energybalance>maxenergybalance){maxenergybalance=energybalance; maxenergybalancedate=$1; maxenergybalancetime=$2}; if(energybalance<minenergybalance){minenergybalance=energybalance; minenergybalancedate=$1; minenergybalancetime=$2}; \
	#Write to stdout
	print $0, energybalance, energy_in, energy_out}; \
#Write out statistics to stderr:
END {printf "Summary of file: '${met_file}'\n-------------------------------------------------------------------------------------\nSum of energy balance error (W_m-2): %.6f (time averaged (W_m-2): %.6f [= %.6f%% from incoming energy])\nSum of absolute energy balance error (W_m-2): %.6f (time averaged (W_m-2): %.6f [= %.6f%% from incoming energy])\nMaximum positive energy balance error (W_m-2): %.6f (= %.0f J_m-2) at %08d, %04d\nMinimum negative energy balance error (W_m-2): %.6f (= %.0f J_m-2) at %08d, %04d\n", energybalancesum, (energybalancesum/ndatapoints), energybalancesum/incomingsum*100.0, energybalancesum2, (energybalancesum2/ndatapoints), energybalancesum2/incomingsum*100.0, maxenergybalance, maxenergybalance*(86400/'${nsamplesperday}'), maxenergybalancedate, maxenergybalancetime, minenergybalance, minenergybalance*(86400/'${nsamplesperday}'), minenergybalancedate, minenergybalancetime > "/dev/stderr"}'

