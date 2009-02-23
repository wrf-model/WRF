#!/bin/ksh -x
#-----------------------------------------------------------------------
# Script: da_run_ungrib_afwa.ksh
#
# Purpose: Run WPS's ungrib and metgrid in AFWA mode which uses:
#          AGRMET and NAVYSST in addyion to GFS-Grib2 edtion. 

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}

. ${SCRIPTS_DIR}/da_set_defaults.ksh

if test ! -d $REG_DIR; then mkdir $REG_DIR; fi 
if test ! -d $RUN_DIR; then mkdir -p $RUN_DIR; fi 

export DATE_SAVE=$DATE
export FCST_RANGE_SAVE=$FCST_RANGE
export WORK_DIR=$RUN_DIR/working    #.run_ungrib_metgrid_afwa

if test ! -d $WORK_DIR; then mkdir -p $WORK_DIR; cd $WORK_DIR; fi

ln -fs $WPS_DIR/ungrib.exe .

#-----------------------------------------------------------------------
# [1] Run ungrib for AGRMET: Only one file available per run time.
#-----------------------------------------------------------------------

export FG_TYPE=agrmet
typeset -u UPPERCASE_FG_TYPE=$FG_TYPE
export VTABLE_TYPE=$UPPERCASE_FG_TYPE.AFWA
ln -fs $WPS_DIR/ungrib/Variable_Tables/Vtable.$VTABLE_TYPE Vtable
$WPS_DIR/link_grib.csh $DAT_DIR/$FG_TYPE/$DATE/*

export FCST_RANGE=0
${SCRIPTS_DIR}/da_create_wps_namelist.ksh
cp namelist.wps namelist.wps.$FG_TYPE
./ungrib.exe

RC=$?

if [[ $RC != 0 ]]; then
   echo ungrib failed with error $RC
   exit $RC
fi

mv FILE:* AGRMET
rm Vtable GRIBFILE*

#-----------------------------------------------------------------------
# [2] Run ungrib for NAVYSST: Only one file available per run time.
#-----------------------------------------------------------------------

export FG_TYPE=navysst
typeset -u UPPERCASE_FG_TYPE=$FG_TYPE
  
export VTABLE_TYPE=$UPPERCASE_FG_TYPE
ln -fs $WPS_DIR/ungrib/Variable_Tables/Vtable.$VTABLE_TYPE Vtable
$WPS_DIR/link_grib.csh $DAT_DIR/$FG_TYPE/$DATE/navyssts

# Temporarily change date to that in SST file:
export SST_DATE=`$WPS_DIR/util/g1print.exe ${DAT_DIR}/$FG_TYPE/${DATE}/navyssts | grep -ni WTMP | cut -c 34-46`
export YEAR=`echo $SST_DATE | cut -c1-4`
export MONTH=`echo $SST_DATE | cut -c6-7`
export DAY=`echo $SST_DATE | cut -c9-10`
export HOUR=`echo $SST_DATE | cut -c12-13`
export DATE=${YEAR}${MONTH}${DAY}${HOUR}
export FCST_RANGE=0

${SCRIPTS_DIR}/da_create_wps_namelist.ksh
cp namelist.wps namelist.wps.$FG_TYPE

./ungrib.exe
RC=$?
if test $RC != 0; then
   echo ungrib failed with error $RC
  exit $RC
fi
mv FILE:* NAVYSST
rm Vtable GRIBFILE*

#-----------------------------------------------------------------------------------
# [3] Run ungrib for GFS: Mutiple GFS files available per run time, requires a loop.
#-----------------------------------------------------------------------------------

export DATE=$DATE_SAVE # Restore true date.
export FCST_RANGE=0
export FG_TYPE=gfs
typeset -u UPPERCASE_FG_TYPE=$FG_TYPE
export VTABLE_TYPE=$UPPERCASE_FG_TYPE.AFWA
ln -fs $WPS_DIR/ungrib/Variable_Tables/Vtable.$VTABLE_TYPE Vtable

${SCRIPTS_DIR}/da_create_wps_namelist.ksh
cp namelist.wps namelist.wps.$FG_TYPE

mkdir tmp
export FCST_HOUR=0
export HOUR=`echo $DATE | cut -c 9-10`

while test $FCST_HOUR -le $FCST_RANGE_SAVE; do

   	if test $FCST_HOUR -lt 10; then export FCST_HOUR=0$FCST_HOUR; fi
	
	#Link relevant GFS grib files:
	
	# The GFS file name convention is: gfs.t00z.pgrb2f00, change it if neccessary.
		
	ln -sf $GRIB_DIR/${DATE_SAVE}/gfs.t${HOUR}z.pgrb2f${FCST_HOUR}  tmp/.
		
	$WPS_DIR/link_grib.csh tmp/*
	
	./ungrib.exe

	# Need to run ungrib for every 3 hour separately, so re-organize the namelist.

	export DATE=`$BUILD_DIR/da_advance_time.exe $DATE $LBC_FREQ 2>/dev/null`

	${SCRIPTS_DIR}/da_create_wps_namelist.ksh
	cp namelist.wps namelist.wps.$FG_TYPE

	# Delete the previous GFS file before linking a new file for ungrib.

	if test -s tmp/*; then rm -rf tmp/*; fi
	
	# Move to the next forecast hour.
	export FCST_HOUR=`expr $FCST_HOUR + $LBC_FREQ`
done

RC=$?
if [[ $RC != 0 ]]; then
   echo ungrib failed with error $RC
   exit $RC
fi
rm Vtable GRIBFILE*

#-----------------------------------------------------------------------
# [4] Run metgrid:
#-----------------------------------------------------------------------

# Need to re-organize the namelist for metgrid run.

#Create namelist:
export CONSTANTS1=./AGRMET
export CONSTANTS2=./NAVYSST

export DATE=$DATE_SAVE              # Restore true date.
export FCST_RANGE=$FCST_RANGE_SAVE  # Restore true range.

${SCRIPTS_DIR}/da_create_wps_namelist.ksh

ln -fs $WPS_DIR/metgrid/METGRID.TBL.${METGRID_TABLE_TYPE} METGRID.TBL
ln -fs $WPS_DIR/metgrid.exe .
${RUN_CMD} ./metgrid.exe
RC=$? 

mv metgrid.log* $RUN_DIR
if [[ -f $RUN_DIR/metgrid.log.0000 ]]; then
   echo '<A HREF="metgrid.log.0000">metgrid.log.0000</a>'
else
   echo '<A HREF="metgrid.log">metgrid.log</a>'
fi

if [[ $RC != 0 ]]; then
   echo metgrid failed with error $RC
   exit $RC
fi

mv met_em.d${DOMAINS}* $RC_DIR/$DATE
     

cd $OLDPWD

if $CLEAN; then rm -rf $WORK_DIR; fi

date

exit 0

