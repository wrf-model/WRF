#!/bin/ksh
#-----------------------------------------------------------------------
# Script: da_run_wps.ksh
#
# Purpose: Run WPS

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/wps}
export WORK_DIR=$RUN_DIR/working

export WPS_INPUT_DIR=${WPS_INPUT_DIR:-$GRIB_DIR}
export WPS_OUTPUT_DIR=${WPS_OUTPUT_DIR:-$RC_DIR}

if [[ ! -d $REG_DIR ]]; then mkdir $REG_DIR; fi 
if [[ ! -d $RUN_DIR ]]; then mkdir -p $RUN_DIR; fi 
if [[ ! -d $WPS_OUTPUT_DIR/$DATE ]]; then mkdir -p $WPS_OUTPUT_DIR/$DATE; fi 
if [[ ! -d $WORK_DIR ]]; then mkdir $WORK_DIR; fi 

cd $WORK_DIR

echo "<HTML><HEAD><TITLE>$EXPT wps</TITLE></HEAD><BODY><H1>$EXPT wps</H1><PRE>"

. $SCRIPTS_DIR/da_get_date_range.ksh

date

echo 'WPS_DIR        <A HREF="'$WPS_DIR'"</a>'$WPS_DIR'</a>'
echo "DATE           $DATE"
echo "END_DATE       $END_DATE"
echo 'WPS_INPUT_DIR  <A HREF="file:'$WPS_INPUT_DIR'"</a>'$WPS_INPUT_DIR'</a>'
echo 'WPS_OUTPUT_DIR <A HREF="file:'$WPS_OUTPUT_DIR'"</a>'$WPS_OUTPUT_DIR'</a>'
echo 'RUN_DIR        <A HREF="file:'$RUN_DIR'"</a>'$RUN_DIR'</a>'
echo 'WORK_DIR       <A HREF="file:'$WORK_DIR'"</a>'$WORK_DIR'</a>'

$SCRIPTS_DIR/da_create_wps_namelist.ksh

cp $WORK_DIR/namelist.wps $RUN_DIR

echo '<A HREF="namelist.wps">namelist.wps</a>'

#-----------------------------------------------------------------------
# [3.0] Run WPS:
#-----------------------------------------------------------------------

if $DUMMY; then
   echo "Dummy wps"
   LOCAL_DATE=$DATE
   while [[ $LOCAL_DATE -le $END_DATE ]]; do
      export L_YEAR=$(echo $LOCAL_DATE | cut -c1-4)
      export L_MONTH=$(echo $LOCAL_DATE | cut -c5-6)
      export L_DAY=$(echo $LOCAL_DATE | cut -c7-8)
      export L_HOUR=$(echo $LOCAL_DATE | cut -c9-10)
      for DOMAIN in $DOMAINS; do
         echo Dummy wps > met_em.d${DOMAIN}.${L_YEAR}-${L_MONTH}-${L_DAY}_${L_HOUR}:00:00.nc
      done
      LOCAL_DATE=$($BUILD_DIR/da_advance_time.exe ${LOCAL_DATE} 1 2>/dev/null)
   done
else
   if $RUN_GEOGRID; then
      # Run geogrid:
      ln -fs $WPS_DIR/geogrid.exe .
      ${RUN_CMD} ./geogrid.exe
      RC=$?

      mv geogrid.log* $RUN_DIR
      if [[ -f $RUN_DIR/geogrid.log.0000 ]]; then
         echo '<A HREF="geogrid.log.0000">geogrid.log.0000</a>'
      else
         echo '<A HREF="geogrid.log">geogrid.log</a>'
      fi

      if [[ $RC != 0 ]]; then
         echo geogrid failed with error $RC
         exit $RC
      fi
   fi

   # Run ungrib:
   ln -fs $VTABLE_DIR/Vtable.${VTABLE_TYPE} Vtable
   LOCAL_DATE=$DATE
   FILES=''
   while [[ $LOCAL_DATE -le $END_DATE ]]; do
      if [[ -e $WPS_INPUT_DIR/$LOCAL_DATE ]]; then
         FILES="$FILES $WPS_INPUT_DIR/$LOCAL_DATE/*"
      fi
      LOCAL_DATE=$($BUILD_DIR/da_advance_time.exe ${LOCAL_DATE} ${LBC_FREQ} 3>/dev/null)
   done
   $WPS_DIR/link_grib.csh $FILES

   ln -fs $WPS_DIR/ungrib.exe .
   ./ungrib.exe > ungrib.log 2>&1
   RC=$?

   mv ungrib.log $RUN_DIR
   echo '<A HREF="ungrib.log">ungrib.log</a>'

   if [[ $RC != 0 ]]; then
      echo ungrib failed with error $RC
      exit $RC
   fi

   # Run metgrid:
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

fi

cd $OLDPWD

if $CLEAN; then rm -rf $WORK_DIR; fi

date

echo "</BODY></HTML>"

exit 0
