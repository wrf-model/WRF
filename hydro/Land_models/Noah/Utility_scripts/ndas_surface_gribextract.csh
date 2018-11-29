#!/bin/tcsh  -f

#
#  This script is intended primarily as an example of how to use the gribextract
#  program to extract individual records from a GRIB dataset.  Users will likely
#  have to adapt this script to their particular needs.
#

#
# Process command-line arguments:
#

set initial = 0  # Default value

while ( ${#argv} > 0 )

   if ( ( "${argv[1]}" == "-help" ) || ( "${argv[1]}" == "--help" ) || ( "${argv[1]}" == "-h" ) ) then
      printf "Usage:  $0 [--initial] <ndas_sf_file>\n"
      exit (1)
   else if ( ( "${argv[1]}" == "-initial" ) || ( "${argv[1]}" == "--initial" ) ) then
      set initial = 1
   else
      set ndas_sf_file = ${1}
   endif
   shift

end

#
# Attempt to find the Utility_programs directory, assuming it is one up from where the Utility_scripts are found.
#
set scriptexe = $0
set firstchar = `echo ${scriptexe} | cut -b 0-1`
if ( "${firstchar}" == "." || "${firstchar}" == "/") then
   # We have a path explicitly in the command as issued.
   set scriptdir = ${scriptexe:h}
   set progdir = ${scriptdir:h}/Utility_programs
else
   # No path explicitly issued.  Assume Utility_programs is one up from here.
   set progdir = ../Utility_programs
endif

if ( ! -e ${progdir} ) then
   printf "Cannot find directory %s\n" ${progdir}
   exit (1)
endif


#
# Determine if the file ${ndas_sf_file} is GRIB edition 1 or GRIB edition 2.
#

set edition = `${progdir}/gribedition ${ndas_sf_file}`
if ( ( ${edition} != 1) && ( ${edition} != 2 ) ) then
   printf  "\n ***** File not recognized as GRIB Edition 1 or GRIB Edition 2.\n"
   printf  " ***** File:  '%s'\n" ${ndas_sf_file}
   exit (1)
endif

#
# From the file ${ndas_sf_file}, extract the 2-m temperature field 
# and put it in the file ${T2_file}.  The string "-c 11,105,2" means:
#     field code  11 = temperature
#     level code 105 = meters above ground level
#     level value  2 = 2 m AGL
#

if ( ${edition} == 1 ) then
   set parameter_code = 11
   set level_code     = 105,2
else if ( ${edition} == 2 ) then
   set parameter_code = 0,0,0
   set level_code     = 103,2
endif
set T2_file = T2.grb
${progdir}/gribextract -q -c ${parameter_code} -l ${level_code} ${ndas_sf_file} ${T2_file}

#
# From the file ${ndas_sf_file}, extract the 2-m specific humidity field 
# and put it in the file ${Q2_file}.  The string "-c 51,105,2" means:
#     field code  51 = specific humidity
#     level code 105 = meters above ground level
#     level value  2 = 2 m AGL
#

if ( ${edition} == 1 ) then
   set parameter_code = 51
   set level_code     = 105,2
else if ( ${edition} == 2 ) then
   set parameter_code = 0,1,0
   set level_code     = 103,2
endif
set Q2_file = Q2.grb
${progdir}/gribextract -q -c ${parameter_code} -l ${level_code} ${ndas_sf_file} ${Q2_file}

#
# From the file ${ndas_sf_file}, extract the 10-m wind field (U component)
# and put it in the files ${U10_file}.  The strings "-c 33,105,10" 
#     field code  33 = U component of horizontal wind
#     level code 105 = meters above ground level
#     level value 10 = 10 m AGL
#

if ( ${edition} == 1 ) then
   set parameter_code = 33
   set level_code     = 105,10
else if ( ${edition} == 2 ) then
   set parameter_code = 0,2,2
   set level_code     = 103,10
endif
set U10_file = U10.grb
${progdir}/gribextract -q -c ${parameter_code} -l ${level_code} ${ndas_sf_file} ${U10_file}

#
# From the file ${ndas_sf_file}, extract the 10-m wind field (V component)
# and put it in the files ${V10_file}.  The strings "-c 34,105,10" 
#     field code  34 = V component of horizontal wind
#     level code 105 = meters above ground level
#     level value 10 = 10 m AGL
#

if ( ${edition} == 1 ) then
   set parameter_code = 34
   set level_code     = 105,10
else if ( ${edition} == 2 ) then
   set parameter_code = 0,2,3
   set level_code     = 103,10
endif
set V10_file = V10.grb
${progdir}/gribextract -q -c ${parameter_code} -l ${level_code} ${ndas_sf_file} ${V10_file}

#
# From the file ${ndas_sf_file}, extract the surface pressure field 
# and put it in the file ${PSFC_file}.  The string "-c 1,1,0" means:
#     field code  1 = pressure
#     level code  1 = surface
#     level value 0 = surface
#

if ( ${edition} == 1 ) then
   set parameter_code = 1
   set level_code     = 1,0
else if ( ${edition} == 2 ) then
   set parameter_code = 0,3,0
   set level_code     = 1,0
endif
set PSFC_file = PSFC.grb
${progdir}/gribextract -q -c ${parameter_code} -l ${level_code} ${ndas_sf_file} ${PSFC_file}

#
# From the file ${ndas_sf_file}, extract the water equivalent snow depth field,
# and put it in the file ${WEASD_file}.  Here's an example where additional data
# from the GRIB record are needed to uniquely identify a single field to extract
# The string "-c 65,1,0" means:
#     field code  65 = Water equivalent snow depth
#     level code  1 = surface
#     level value 0 = surface
# The string "-s 1,21,0" means "extract only fields for which section 1, byte 21 is 
# equal to zero."  Byte 21 of section 1 indicates whether the field is an analysis 
# field, or an accumulation over a time window.  We want only the analysis field.
#

if ( ${edition} == 1 ) then
   set parameter_code = 65
   set level_code     = 1,0
   set section_code   = 1,21,0
else if ( ${edition} == 2 ) then
   set parameter_code = 0,1,13
   set level_code     = 1,0
   set section_code   = 4,8:9,0
endif
set WEASD_file = WEASD.grb
${progdir}/gribextract -q -c ${parameter_code} -l ${level_code} -s ${section_code} ${ndas_sf_file} ${WEASD_file}

if ( ! ${initial} ) goto RenameFiles

#
# From the file ${ndas_sf_file}, extract the source model terrain elevation field
# and put it in the file ${ELEVATION_file}.  The string "-c 7,1,0" means:
#     field code  7 = Geopotential height
#     level code  1 = surface
#     level value 0 = surface
#

if ( ${edition} == 1 ) then
   set parameter_code = 7
   set level_code     = 1,0
else if ( ${edition} == 2 ) then
   set parameter_code = 0,3,5
   set level_code     = 1,0
endif
set ELEVATION_file = SURFACE_ELEVATION.grb
${progdir}/gribextract -q -c ${parameter_code} -l ${level_code} ${ndas_sf_file} ${ELEVATION_file}

#
# From the file ${ndas_sf_file}, extract the LAND/SEA flag 
# and put it in the file ${LANDSEA_file}.  The string "-c 81,1,0" means:
#     field code  81 = land/sea flag
#     level code  1 = surface
#     level value 0 = surface
#

if ( ${edition} == 1 ) then
   set parameter_code = 81
   set level_code     = 1,0
else if ( ${edition} == 2 ) then
   set parameter_code = 2,0,0
   set level_code     = 1,0
endif
set LANDSEA_file = LANDSEA.grb
${progdir}/gribextract -q -c ${parameter_code} -l ${level_code} ${ndas_sf_file} ${LANDSEA_file}

#
# From the file ${ndas_sf_file}, extract the canopy water flag
# and put it in the file ${CANWAT_file}.  The string "-c 223,1,0" means:
#     field code  223 = land/sea flag
#     level code  1 = surface
#     level value 0 = surface
#

if ( ${edition} == 1 ) then
   set parameter_code = 223
   set level_code     = 1,0
else if ( ${edition} == 2 ) then
   set parameter_code = 2,0,196
   set level_code     = 1,0
endif
set CANWAT_file = CANWAT.grb
${progdir}/gribextract -q -c ${parameter_code} -l ${level_code} ${ndas_sf_file} ${CANWAT_file}

#
# From the file ${ndas_sf_file}, extract the skin temperature field
# and put it in the file ${SKINTEMP_file}.  The string "-c 11,1,0" means:
#     field code  11 = temperature
#     level code  1 = surface (i.e., skin)
#     level value 0 = surface
#

if ( ${edition} == 1 ) then
   set parameter_code = 11
   set level_code     = 1,0
else if ( ${edition} == 2 ) then
   set parameter_code = 0,0,0
   set level_code     = 1,0
endif
set SKINTEMP_file = SKINTEMP.grb
${progdir}/gribextract -q -c ${parameter_code} -l ${level_code} ${ndas_sf_file} ${SKINTEMP_file}

#
# From the file ${ndas_sf_file}, extract the soil temperature fields, for which
# we have a priori knowledge about the soil levels.
#  Field code 85 = soil temperature
#  Level code 112 = depth below the surface in cm.
#  Level values 0,10; 10,40; 40,100; and 100,200 indicate 0 to 10 cm, 10 to 40 cm, etc.
#

if ( ${edition} == 1 ) then
   set parameter_code = 85
else if ( ${edition} == 2 ) then
   set parameter_code = 2,0,2
endif
foreach level ( 1 2 3 4 )
   if ( ${level} == 1 ) then
      set SOILT_file = SOIL_T_000-010.grb
      if ( ${edition} == 1 ) then
         set level_code = 112,0,10
      else if ( ${edition} == 2 ) then
         set level_code = 106,0,0.10
      endif
   else if ( ${level} == 2 ) then
      set SOILT_file = SOIL_T_010-040.grb
      if ( ${edition} == 1 ) then
         set level_code = 112,10,40
      else if ( ${edition} == 2 ) then
         set level_code = 106,0.10,0.40
      endif
   else if ( ${level} == 3 ) then
      set SOILT_file = SOIL_T_040-100.grb
      if ( ${edition} == 1 ) then
         set level_code = 112,40,100
      else if ( ${edition} == 2 ) then
         set level_code = 106,0.40,1.00
      endif
   else if ( ${level} == 4 ) then
      set SOILT_file = SOIL_T_100-200.grb
      if ( ${edition} == 1 ) then
         set level_code = 112,100,200
      else if ( ${edition} == 2 ) then
         set level_code = 106,1.0,2.0
      endif
   endif
   ${progdir}/gribextract -q -c ${parameter_code} -l ${level_code} ${ndas_sf_file} ${SOILT_file}
end

#
# From the file ${ndas_sf_file}, extract the soil moisture fields, for which
# we have a priori knowledge about the soil levels.
#  Field code 144 = volumetric soil temperature
#  Level code 112 = depth below the surface in cm.
#  Level values 0,10; 10,40; 40,100; and 100,200 indicate 0 to 10 cm, 10 to 40 cm, etc.
#

if ( ${edition} == 1 ) then
   set parameter_code = 144
else if ( ${edition} == 2 ) then
   set parameter_code = 2,0,192
endif
foreach level ( 1 2 3 4 )
   if ( ${level} == 1 ) then
      set SOILM_file = SOIL_M_000-010.grb
      if ( ${edition} == 1 ) then
         set level_code = 112,0,10
      else if ( ${edition} == 2 ) then
         set level_code = 106,0,0.10
      endif
   else if ( ${level} == 2 ) then
      set SOILM_file = SOIL_M_010-040.grb
      if ( ${edition} == 1 ) then
         set level_code = 112,10,40
      else if ( ${edition} == 2 ) then
         set level_code = 106,0.10,0.40
      endif
   else if ( ${level} == 3 ) then
      set SOILM_file = SOIL_M_040-100.grb
      if ( ${edition} == 1 ) then
         set level_code = 112,40,100
      else if ( ${edition} == 2 ) then
         set level_code = 106,0.40,1.00
      endif
   else if ( ${level} == 4 ) then
      set SOILM_file = SOIL_M_100-200.grb
      if ( ${edition} == 1 ) then
         set level_code = 112,100,200
      else if ( ${edition} == 2 ) then
         set level_code = 106,1.0,2.0
      endif
   endif
   ${progdir}/gribextract -q -c ${parameter_code} -l ${level_code} ${ndas_sf_file} ${SOILM_file}
end

RenameFiles:


foreach file ( *.grb )

    #
    # This is perhaps getting too fancy here, but use the od (octal dump) command to 
    # pull specific bytes out of the grib record, specifically, the bytes identifying
    # the time of the field:
    #

    if ( ${edition} == 1 ) then
       set cc = `${progdir}/gribbyte -s 1,25 ${file}`
       set yy = `${progdir}/gribbyte -s 1,13 ${file}`
       set mm = `${progdir}/gribbyte -s 1,14 ${file}`
       set dd = `${progdir}/gribbyte -s 1,15 ${file}`
       set hh = `${progdir}/gribbyte -s 1,16 ${file}`

       set tu = `${progdir}/gribbyte -s 1,18 ${file}`
       set p1 = `${progdir}/gribbyte -s 1,19 ${file}`
       set p2 = `${progdir}/gribbyte -s 1,20 ${file}`
       set ti = `${progdir}/gribbyte -s 1,21 ${file}`

       set cc = `printf "%2.2d" ${cc}`
       set yy = `printf "%2.2d" ${yy}`
       set mm = `printf "%2.2d" ${mm}`
       set dd = `printf "%2.2d" ${dd}`
       set hh = `printf "%2.2d" ${hh}`

       set yytest = `dc -e "${yy} p"`  # use dc to pop the value, which returns a value without any leading zeroes.
       if ( ${yytest} > 0 ) @ cc --

       # echo "tu = ${tu};   p1 = $p1;  p2 = $p2;  ti = $ti"

       #
       # Compute a valid date, based on reference time and time offset.
       # Ultimately, this should take into account time units and time
       # range indicator.
       #

       if ( ( ${tu} == 1 ) && ( ${ti} == 0 ) )  then
	  set validdate = `${progdir}/geth_newdate ${cc}${yy}${mm}${dd}${hh} ${p1}`
       else
	  set validdate = 0000000000
       endif
	
       #
       # Move the data into directories identified by time:
       #

       mkdir -p ${cc}${yy}${mm}${dd}${hh}
       mv ${file} ${cc}${yy}${mm}${dd}${hh}/NDAS.${file:r}.${validdate}.grb

   else if ( ${edition} == 2 ) then
       set yyyy = `${progdir}/gribbyte -s 1,13:14 -d ${file}`
       set mm   = `${progdir}/gribbyte -s 1,15 ${file}`
       set dd   = `${progdir}/gribbyte -s 1,16 ${file}`
       set hh   = `${progdir}/gribbyte -s 1,17 ${file}`
       set yyyy = `printf "%4.4d" ${yyyy}`
       set mm = `printf "%2.2d" ${mm}`
       set dd = `printf "%2.2d" ${dd}`
       set hh = `printf "%2.2d" ${hh}`

       # Compute a valid date, based on reference time and time offset.
       set tu = `${progdir}/gribbyte -s 4,18 ${file}`
       set p1 = `${progdir}/gribbyte -s 4,19:22 -d ${file}`
       
       echo "tu = ${tu};  p1 = ${p1}"

       if ( ${tu} == 1 )  then
          set validdate = `${progdir}/geth_newdate ${yyyy}${mm}${dd}${hh} ${p1}`
       else
          set validdate = 0000000000
       endif

       #
       # Move the data into directories identified by time:
       #
       mkdir -p ${yyyy}${mm}${dd}${hh}
       mv ${file} ${yyyy}${mm}${dd}${hh}/NDAS.${file:r}.${validdate}.grb
   endif
end
