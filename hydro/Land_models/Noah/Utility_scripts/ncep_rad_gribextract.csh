#!/bin/tcsh -ef

#
#  This script is intended primarily as an example of how to use the gribextract
#  program to extract individual records from a GRIB Edition 1 dataset
#

set initial = 0

while ( ${#argv} > 0 )

   if ( ( "${argv[1]}" == "-help" ) || ( "${argv[1]}" == "--help" ) || ( "${argv[1]}" == "-h" ) ) then
      printf "Usage:  $0 [--initial] <RAD_file>\n"
      exit (1)
   else if ( ( "${argv[1]}" == "-initial" ) || ( "${argv[1]}" == "--initial" ) ) then
      set initial = 1
   else
      set RAD_file = ${1}
   endif
   shift

end

#
# From the ${nam_file}, extract downwelling longwave radiation.  Only take the
# instantaneous field, skipping the fields.
#

#../Utility_programs/gribextract -q -c 205,1,0 -s 1,21,0 ${nam_file} LW.grb
#../Utility_programs/gribextract -q -c 205,1,0 ${nam_file} LW.grb

#if ( -z LW.grb ) rm LW.grb

set edition = `../Utility_programs/gribedition ${RAD_file}`

if ( ${edition} == 1 ) then
   set parameter_code = 205
   set level_code     = 1,0
else if ( ${edition} == 2 ) then
   set parameter_code = 0,5,192
   set level_code     = 1,0
endif
set LWDOWN_file = LWDOWN.grb
../Utility_programs/gribextract -q -c ${parameter_code} -l ${level_code} ${RAD_file} ${LWDOWN_file}

#
# From the ${nam_file}, extract downwelling shortwave radiation.  Only take the
# instantaneous field, skipping the fields.
#

#../Utility_programs/gribextract -q -c 204,1,0 -s 1,21,0 ${nam_file} SW.grb
#../Utility_programs/gribextract -q -c 204,1,0 ${nam_file} SW.grb
#if ( -z SW.grb ) rm SW.grb

if ( ${edition} == 1 ) then
   set parameter_code = 204
   set level_code     = 1,0
else if ( ${edition} == 2 ) then
   set parameter_code = 0,4,192
   set level_code     = 1,0
endif
set SWDOWN_file = SWDOWN.grb
../Utility_programs/gribextract -q -c ${parameter_code} -l ${level_code} ${RAD_file} ${SWDOWN_file}


RenameFiles:




foreach file ( *.grb )

    #
    # This is perhaps getting too fancy here, but use the od (octal dump) command to 
    # pull specific bytes out of the grib record, specifically, the bytes identifying
    # the time of the field:
    #
    set cc = `od -j 32 -N 1 -t u1 ${file}`
    set yy = `od -j 20 -N 1 -t u1 ${file}`
    set mm = `od -j 21 -N 1 -t u1 ${file}`
    set dd = `od -j 22 -N 1 -t u1 ${file}`
    set hh = `od -j 23 -N 1 -t u1 ${file}`

    set p1 = `od -j 26 -N 1 -t u1 ${file}`
    set p2 = `od -j 27 -N 1 -t u1 ${file}`
    set ti = `od -j 28 -N 1 -t u1 ${file}`

    set cc = `printf "%2.2d" ${cc[2]}`
    set yy = `printf "%2.2d" ${yy[2]}`
    set mm = `printf "%2.2d" ${mm[2]}`
    set dd = `printf "%2.2d" ${dd[2]}`
    set hh = `printf "%2.2d" ${hh[2]}`

    if ( ${yy} > 00 ) @ cc --

    echo "p1 = $p1[2];  p2 = $p2[2];  ti = $ti[2]"
    # echo $cc $yy $mm $dd $hh

    if ( ${p1[2]} == 0 ) then
       set validdate = ${cc}${yy}${mm}${dd}${hh}
    else
       set validdate = `../Utility_programs/geth_newdate ${cc}${yy}${mm}${dd}${hh} ${p1[2]}`
    endif

    #
    # Move the data into directories identified by time:
    #

    mkdir -p ${validdate}
    mv ${file} ${validdate}/NCEP.${file:r}.${validdate}.grb
end
