#!/bin/ksh -aeux
#-----------------------------------------------------------------------
# Script: da_run_ungrib_metgrid_kma.ksh
#
# Purpose: Run WPS's ungrib and metgrid a la KMA, i.e. using
# KMA's GDAPS global model output as input with supplemental surface
# fields from GFS.
#
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
export WORK_DIR=$RUN_DIR/working.run_ungrib_metgrid_kma

if test ! -d $WORK_DIR; then mkdir -p $WORK_DIR; cd $WORK_DIR; fi

#-----------------------------------------------------------------------
# [2] Run ungrib for GFS surface fields:
#-----------------------------------------------------------------------

ln -fs $WPS_DIR/ungrib.exe .

export DATE=$DATE_SAVE # Restore true date.
export FCST_RANGE=0
export FCST_HOUR=00

${SCRIPTS_DIR}/da_create_wps_namelist.ksh
export FG_TYPE=GFS
cp namelist.wps namelist.wps.$FG_TYPE.$FCST_HOUR

ln -fs $VTABLE_DIR/$Vtable.$FG_TYPE.GDAPS Vtable

rm -rf tmp > /dev/null 2>&1; mkdir tmp
ln -sf $DAT_DIR/$FG_TYPE/$DATE_SAVE/t${FCST_HOUR}  tmp/.

$WPS_DIR/link_grib.csh tmp/*

${RUN_CMD} ./ungrib.exe
RC=$?
if test $RC != 0; then
   echo ungrib failed with error $RC
   exit $RC
fi

mv FILE:* GFS.GDAPS
rm Vtable GRIBFILE*

#-----------------------------------------------------------------------
# [2] Run ungrib for GDAPS:
#-----------------------------------------------------------------------

export FCST_HOUR=0
export FCST_RANGE=0

while test $FCST_HOUR -le $FCST_RANGE_SAVE; do

   if test $FCST_HOUR -lt 10; then export FCST_HOUR=0$FCST_HOUR; fi

   ${SCRIPTS_DIR}/da_create_wps_namelist.ksh
   export FG_TYPE=GDAPS
   cp namelist.wps namelist.wps.$FG_TYPE.$FCST_HOUR

   ln -fs $VTABLE_DIR/Vtable.$FG_TYPE Vtable

   rm -rf tmp > /dev/null 2>&1; mkdir tmp
   ln -sf $DAT_DIR/$FG_TYPE/$DATE_SAVE/kwrf_lc10_prep_gdps_grib.${DATE_SAVE}  tmp/.

   $WPS_DIR/link_grib.csh tmp/*

   ${RUN_CMD} ./ungrib.exe
   RC=$?
   if test $RC != 0; then
      echo ungrib failed with error $RC
      exit $RC
   fi

#  Move to next forecast hour:

   export DATE=`$BUILD_DIR/da_advance_cymdh.exe $DATE $LBC_FREQ 2>/dev/null`
   export FCST_HOUR=`expr $FCST_HOUR + $LBC_FREQ`

done

rm Vtable GRIBFILE*

#-----------------------------------------------------------------------
# [4] Run metgrid:
#-----------------------------------------------------------------------

# Need to re-organize the namelist for metgrid run.

#Create namelist:
export CONSTANTS1=./GFS.GDAPS
#export CONSTANTS2=./NAVYSST

export DATE=$DATE_SAVE              # Restore true date.
export FCST_RANGE=$FCST_RANGE_SAVE  # Restore true range.

${SCRIPTS_DIR}/da_create_wps_namelist.ksh
cp namelist.wps namelist.wps.metgrid

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

