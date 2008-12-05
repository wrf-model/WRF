#!/bin/ksh
#-----------------------------------------------------------------------
# Script da_run_obsproc.ksh
#
# Purpose: Creates observation file for input to WRFVAR (ob_format_2).
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/obsproc}
export WORK_DIR=$RUN_DIR/working

mkdir -p $RUN_DIR $OB_DIR/$DATE

echo "<HTML><HEAD><TITLE>$EXPT obsproc</TITLE></HEAD><BODY><H1>$EXPT obsproc</H1><PRE>"

date

echo 'REL_DIR      <A HREF="'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRFVAR_DIR   <A HREF="'$WRFVAR_DIR'">'$WRFVAR_DIR'</a>'
echo 'RTOBS_DIR    <A HREF="'$RTOBS_DIR'">'$RTOBS_DIR'</a>'
echo 'OB_DIR       <A HREF="'$OB_DIR'">'$OB_DIR'</a>'
echo 'RUN_DIR      <A HREF="'$RUN_DIR'">'$RUN_DIR'</a>'
echo 'WORK_DIR     <A HREF="'$WORK_DIR'">'$WORK_DIR'</a>'

mkdir -p $WORK_DIR
cd $WORK_DIR

let NL_DX_KM=$NL_DX/1000

export NL_BASE_PRES=${NL_BASE_PRES:-100000.0}
export NL_BASE_TEMP=${NL_BASE_TEMP:-300.0}
export NL_BASE_LAPSE=${NL_BASE_LAPSE:-50.0}
export NL_BASE_TROPO_PRES=${NL_BASE_TROPO_PRES:-20000.0}
export NL_BASE_STRAT_TEMP=${NL_BASE_STRAT_TEMP:-215.0}

if [[ $MAP_PROJ == lambert ]]; then
   export PROJ=1
elif [[ $MAP_PROJ == polar ]];  then
   export PROJ=2
elif [[ $MAP_PROJ == mercator ]]; then
   export PROJ=3
else
   echo "   Unknown MAP_PROJ = $MAP_PROJ."
   exit 1
fi

export FCST_RANGE_SAVE=$FCST_RANGE
export FCST_RANGE=-$MAX_OB_RANGE
. $SCRIPTS_DIR/da_get_date_range.ksh
export TIME_WINDOW_MIN=${END_YEAR}-${END_MONTH}-${END_DAY}_${END_HOUR}:00:00
export FCST_RANGE=$MAX_OB_RANGE
. $SCRIPTS_DIR/da_get_date_range.ksh
export TIME_WINDOW_MAX=${END_YEAR}-${END_MONTH}-${END_DAY}_${END_HOUR}:00:00
export FCST_RANGE=0
. $SCRIPTS_DIR/da_get_date_range.ksh
export TIME_ANALYSIS=${START_YEAR}-${START_MONTH}-${START_DAY}_${START_HOUR}:00:00
export FCST_RANGE=$FCST_RANGE_SAVE

export OB_FILE=obs.${START_YEAR}${START_MONTH}${START_DAY}${START_HOUR}

#ln -fs $OB_DIR/$DATE/$OB_FILE .

if [[ -f $RTOBS_DIR/$DATE/${OB_FILE}.gz ]]; then
   # If compressed, unpack
   if $DUMMY; then
      touch ${OB_FILE}
   else
      cp $RTOBS_DIR/$DATE/$OB_FILE.gz .
      gunzip -f ${OB_FILE}.gz
   fi
fi

#Namelist notes:
#1. x,y reversed in namelist as MM5 i=y.
#2. Modified namelist 2 in fortran code to be lime -DBKG.

cat > namelist.3dvar_obs << EOF
&record1
 obs_gts_filename = '$OB_FILE',
 fg_format        = 'WRF',
 obs_err_filename = 'obserr.txt',
/

&record2
 time_window_min  = '${TIME_WINDOW_MIN}',
 time_analysis    = '${TIME_ANALYSIS}',
 time_window_max  = '${TIME_WINDOW_MAX}',
/

&record3
 max_number_of_obs        = 70000,
 fatal_if_exceed_max_obs  = .TRUE.,
/

&record4
 qc_test_vert_consistency = .TRUE.,
 qc_test_convective_adj   = .TRUE.,
 qc_test_above_lid        = .TRUE.,
 remove_above_lid         = .TRUE.,
 domain_check_h           = .true.,
 Thining_SATOB            = ${THINING_SATOB},
 Thining_SSMI             = ${THINING_SSMI},
 Thining_QSCAT            = ${THINING_QSCAT},
/

&record5
 print_gts_read           = .TRUE.,
 print_gpspw_read         = .TRUE.,
 print_recoverp           = .TRUE.,
 print_duplicate_loc      = .TRUE.,
 print_duplicate_time     = .TRUE.,
 print_recoverh           = .TRUE.,
 print_qc_vert            = .TRUE.,
 print_qc_conv            = .TRUE.,
 print_qc_lid             = .TRUE.,
 print_uncomplete         = .TRUE.,
/

&record6
 ptop =  ${P_TOP_REQUESTED},
 base_pres       = ${NL_BASE_PRES},
 base_temp       = ${NL_BASE_TEMP},
 base_lapse      = ${NL_BASE_LAPSE},
 base_strat_temp = ${NL_BASE_STRAT_TEMP},
 base_tropo_pres = ${NL_BASE_TROPO_PRES}
/

&record7
 IPROJ = ${PROJ},
 PHIC  = ${REF_LAT},
 XLONC = ${REF_LON},
 TRUELAT1= ${TRUELAT1},
 TRUELAT2= ${TRUELAT2},
 MOAD_CEN_LAT = ${REF_LAT},
 STANDARD_LON = ${STAND_LON},
/

&record8
 IDD    =   1,
 MAXNES =   2,
 NESTIX =  ${NL_E_SN},  200,  136,  181,  211,
 NESTJX =  ${NL_E_WE},  200,  181,  196,  211,
 DIS    =  ${NL_DX_KM},  10.,  3.3,  1.1,  1.1,
 NUMC   =    1,    1,   2,     3,    4,
 NESTI  =    1,   40,  28,    35,   45,
 NESTJ  =    1,   60,  25,    65,   55,
 / 

&record9
 PREPBUFR_OUTPUT_FILENAME = 'prepbufr_output_filename',
 PREPBUFR_TABLE_FILENAME = 'prepbufr_table_filename',
 OUTPUT_OB_FORMAT = 2
 use_for          = '${NL_USE_FOR}',
 num_slots_past   = ${NL_NUM_SLOTS_PAST},
 num_slots_ahead  = ${NL_NUM_SLOTS_AHEAD},
 write_synop = .true., 
 write_ship  = .true.,
 write_metar = .true.,
 write_buoy  = .true., 
 write_pilot = .true.,
 write_sound = .true.,
 write_amdar = .true.,
 write_satem = .true.,
 write_satob = .true.,
 write_airep = .true.,
 write_gpspw = .true.,
 write_gpsztd= .true.,
 write_gpsref= .true.,
 write_gpseph= .true.,
 write_ssmt1 = .true.,
 write_ssmt2 = .true.,
 write_ssmi  = .true.,
 write_tovs  = .true.,
 write_qscat = .true.,
 write_profl = .true.,
 write_bogus = .true.,
 write_airs  = .true.,
 /

EOF

cp namelist.3dvar_obs $RUN_DIR

echo "Converting $WORK_DIR/$OB_FILE to"
echo "$OB_DIR/$DATE/ob.ascii"
echo '<A HREF="namelist.3dvar_obs">Namelist input</a>'
if $DUMMY; then
   echo "Dummy obsproc"
   echo "Dummy obsproc" > obs_gts.3dvar
else
   ln -fs $WRFVAR_DIR/var/obsproc/obserr.txt .
   ln -fs $WRFVAR_DIR/var/obsproc/prepbufr_table_filename .
   $WRFVAR_DIR/var/obsproc/3dvar_obs.exe
   RC=$?
   echo "Ended %$RC"
fi

if [[ $NL_USE_FOR = 3DVAR ]]; then
   mv obs_gts_${TIME_ANALYSIS}.${NL_USE_FOR} $OB_DIR/$DATE/ob.ascii
   echo mv obs_gts_${TIME_ANALYSIS}.${NL_USE_FOR} $OB_DIR/$DATE/ob.ascii
else 
   I=0
   set -A ff `ls obs_gts*.${NL_USE_FOR}`
   NUM_SLOTS=${#ff[*]}
   let NUM_SLOTS=$NUM_SLOTS-1
   echo NUM_SLOTS= 0 to ${NUM_SLOTS}
   while [[ $I -le $NUM_SLOTS ]]; do
     fg_time=`echo ${ff[$I]} | cut -c 9-27`
     fg_time=`$BUILD_DIR/da_advance_time.exe $fg_time 0`
     mkdir -p $OB_DIR/$fg_time
     if [[ $I -eq 0 ]]; then
       mv ${ff[$I]}  $OB_DIR/$fg_time/ob.ascii+
       echo $I  mv ${ff[$I]}  $OB_DIR/$fg_time/ob.ascii+
     elif [[ $I -eq $NUM_SLOTS ]]; then
       mv ${ff[$I]}  $OB_DIR/$fg_time/ob.ascii-
       echo $I  mv ${ff[$I]}  $OB_DIR/$fg_time/ob.ascii-
     else
       mv ${ff[$I]}  $OB_DIR/$fg_time/ob.ascii
       echo $I  mv ${ff[$I]}  $OB_DIR/$fg_time/ob.ascii
     fi
     let I=$I+1
   done   
fi

date

echo '</PRE></BODY></HTML>'

exit $RC

