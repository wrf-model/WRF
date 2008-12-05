#!/bin/ksh -aeux
#-----------------------------------------------------------------------
# Script: da_run_ungrib_afwa.ksh
#
# Purpose: Run WPS's ungrib and metgrid in AFWA mode.

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
export WORK_DIR=$RUN_DIR/working.run_ungrib_metgrid_afwa

#cmd mkdir -p $WORK_DIR; cd $WORK_DIR

if test ! -d $WORK_DIR; then mkdir -p $WORK_DIR; cd $WORK_DIR; fi

ln -fs $WPS_DIR/ungrib.exe .

#-----------------------------------------------------------------------
# [1] Run ungrib for AGRMET:
#-----------------------------------------------------------------------

export FG_TYPE=agrmet
ln -fs $VTABLE_DIR/Vtable.$FG_TYPE Vtable
$WPS_DIR/link_grib.csh $DAT_DIR/$FG_TYPE/$DATE/*

export FCST_RANGE=0
${SCRIPTS_DIR}/da_create_wps_namelist.ksh
cp namelist.wps namelist.wps.$FG_TYPE
${RUN_CMD} ./ungrib.exe

RC=$?
if test $RC != 0; then
   echo ungrib failed with error $RC
   exit $RC
fi
mv FILE:* AGRMET
rm Vtable GRIBFILE*

#-----------------------------------------------------------------------
# [2] Run ungrib for NavySST:
#-----------------------------------------------------------------------

export FG_TYPE=NavySST
ln -fs $VTABLE_DIR/Vtable.$FG_TYPE Vtable
$WPS_DIR/link_grib.csh $DAT_DIR/$FG_TYPE/$DATE/$FG_TYPE

# Temporarily change date to that in SST file:
export SST_DATE=`$WPS_DIR/util/g1print.exe ${DAT_DIR}/$FG_TYPE/${DATE}/$FG_TYPE | grep -ni WTMP | cut -c 34-46`
export YEAR=`echo $SST_DATE | cut -c1-4`
export MONTH=`echo $SST_DATE | cut -c6-7`
export DAY=`echo $SST_DATE | cut -c9-10`
export HOUR=`echo $SST_DATE | cut -c12-13`
export DATE=${YEAR}${MONTH}${DAY}${HOUR}
export FCST_RANGE=0
${SCRIPTS_DIR}/da_create_wps_namelist.ksh
cp namelist.wps namelist.wps.$FG_TYPE

${RUN_CMD} ./ungrib.exe

RC=$?
if test $RC != 0; then
   echo ungrib failed with error $RC
   exit $RC
fi
mv FILE:* NAVYSST
rm Vtable GRIBFILE*

#-----------------------------------------------------------------------
# [3] Run ungrib for GFS:
#-----------------------------------------------------------------------

export DATE=$DATE_SAVE # Restore true date.
export FCST_RANGE=0
export FG_TYPE=GFS
ln -fs $VTABLE_DIR/Vtable.FG_TYPE Vtable

${SCRIPTS_DIR}/da_create_wps_namelist.ksh
cp namelist.wps namelist.wps.$FG_TYPE

mkdir tmp
export FCST_HOUR=0
export HOUR=`echo $DATE | cut -c 9-10`

while test $FCST_HOUR -le $FCST_RANGE_SAVE; do

   	if test $FCST_HOUR -lt 10; then export FCST_HOUR=0$FCST_HOUR; fi
	
	#Link relevant GFS grib files:
   	#ln -sf $DAT_DIR/$FG_TYPE/$DATE_SAVE/MT.avn_CY.${HOUR}_fh.00${FCST_HOUR}_tl.press_gr.0p5deg tmp/.
	#The new GFS file name convention is: gfs.t00z.pgrb2f00	
	#ln -sf $DAT_DIR/$FG_TYPE/$DATE_SAVE/gfs.t${HOUR}z.pgrb2f${FCST_HOUR}  tmp/.
	
        # Use a very basic input file name that is prepared before the run.
	ln -sf $DAT_DIR/$FG_TYPE/$DATE_SAVE/t${FCST_HOUR}  tmp/.
	
	$WPS_DIR/link_grib.csh tmp/*
	
	${RUN_CMD} ./ungrib.exe

	# Need to run ungrib for every 3 hour separately, so re-organize the namelist.

	export DATE=`$BUILD_DIR/da_advance_cymdh.exe $DATE $LBC_FREQ 2>/dev/null`

	${SCRIPTS_DIR}/da_create_wps_namelist.ksh
	cp namelist.wps namelist.wps.$FG_TYPE

	# Delete the previous GFS file before linking a new file for ungrib.
	# Ungrib code tries to de-grib whichever file in the tmp directory, this slow downs runs.
	# And that we hit run time limit (wall clock limit on SCD's supercomputers).

	if test -s tmp/*; then rm -rf tmp/*; fi
	
	# Move to the next forecast hour.
	export FCST_HOUR=`expr $FCST_HOUR + $LBC_FREQ`
done

RC=$?
if test $RC != 0; then
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
if test -f $RUN_DIR/metgrid.log.0000; then
   echo '<A HREF="metgrid.log.0000">metgrid.log.0000</a>'
else
   echo '<A HREF="metgrid.log">metgrid.log</a>'
fi

if test $RC != 0; then
   echo metgrid failed with error $RC
   exit $RC
fi

mv met_em.d${DOMAIN}* $RC_DIR/$DATE

if $CLEAN; then rm -rf $WORK_DIR; fi

exit 0

