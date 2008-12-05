#!/bin/ksh
#=======================================================================================
#  Purpose : Main script for processing and display of results for
#            verification against analysis
#
#  Author  :  Syed RH Rizvi,  NCAR/MMM    10/12/2007
#--------------------------------------------------------------------------------------
#=======================================================================================

echo "<PRE>"

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh

export DATA_DIR=${DATA_DIR:-${REG_DIR}}    
export NUM_EXPT=${NUM_EXPT:-2}
export CONTROL_EXP_DIR=${CONTROL_EXP_DIR:-${REG_DIR}/noda}                 
export EXP_DIRS=${EXP_DIRS:-${REG_DIR}/noda ${REG_DIR}/cycling_rad}
export EXP_NAMES=${EXP_NAMES:-"NODA" "CY RAD"}
export VERIFICATION_FILE_STRING=${VERIFICATION_FILE_STRING:-'wrfout'}
export EXP_LEGENDS=${EXP_LEGENDS:-'(/"noda","cy rad"/)'}

export INTERVAL=${INTERVAL:-12}
export VERIFY_HOUR=${VERIFY_HOUR:-12}
export START_DATE=${START_DATE:-2003010100}
export END_DATE=${END_DATE:-2003010100}

export RUN_DIR=${RUN_DIR:-$PWD}
export VERIFY_ITS_OWN_ANALYSIS=${VERIFY_ITS_OWN_ANALYSIS:-true}
export Verify_Date_Range=${Verify_Date_Range:-"$START_DATE - $END_DATE (${INTERVAL} hour Cycle)"}

export DOMAIN=${DOMAIN:-1}
export PLOT_WKS=${PLOT_WKS:-pdf}

#=======================================================================================
export DESIRED_LEVELS=${DESIRED_LEVELS:-850 500 200}
export DESIRED_SCORES=${DESIRED_SCORES:-'(/"RMSE","BIAS", "ABIAS"/)'}
export EXP_LINES_COLORS=${EXP_LINES_COLORS:-'(/"blue","green", "orange"/)'}

export NUM3D=${NUM3D:-4}
export VAR3D=${VAR3D:-'"U", "V", "TK", "QVAPOR"'}
export NUM2D=${NUM2D:-1}
export VAR2D=${VAR2D:-' "SLP"'}
#--------------------------------------------------------------------------------------
#=========================================================
# BELOW THIS LINE NO CHABGES ARE REQUIRED                 
#=========================================================
export WORK_DIR=${RUN_DIR}/working
mkdir -p ${WORK_DIR}
cd ${WORK_DIR}
rm -rf ${WORK_DIR}/*
export VERT_TYPE=${VERT_TYPE:-'p'}
export BAR_LABEL_ANGLE=${BAR_LABEL_ANGLE:-45}
#=========================================================
iexp=0
exp_dirs=''
out_dirs=''
for EXP_DIR in $EXP_DIRS; do
   exp_dirs="$exp_dirs '$EXP_DIR/fc/',"
done
for EXP_NAME in $EXP_NAMES; do
   out_dirs="$out_dirs '$iexp',"
   pdat_dirs[$iexp]="$EXP_NAME/"
   mkdir -p $iexp
   iexp=$((iexp + 1))
done
#--------------------------------------------------
 SYEAR=`echo $START_DATE | cut -c1-4`
 SMONTH=`echo $START_DATE | cut -c5-6`
 SDAY=`echo $START_DATE | cut -c7-8`
 SHOUR=`echo $START_DATE | cut -c9-10`
#-------------
 EYEAR=`echo $END_DATE | cut -c1-4`
 EMONTH=`echo $END_DATE | cut -c5-6`
 EDAY=`echo $END_DATE | cut -c7-8`
 EHOUR=`echo $END_DATE | cut -c9-10`
#--------------------------------------------------
# Making records for namelist 
#--------------------------------------------------
cat > namelist.in << EOF
&control_main
 verify_its_own_analysis     = ${VERIFY_ITS_OWN_ANALYSIS},
 control_exp_dir             = '${CONTROL_EXP_DIR}/fc', 
 num_verifying_experiments   = ${NUM_EXPT},
 verif_dirs                  = ${exp_dirs}
 out_dirs                    = ${out_dirs}
 verify_forecast_hour        = ${VERIFY_HOUR},
 verification_file_string    = '${VERIFICATION_FILE_STRING}',
 domain                      = ${DOMAIN},
 vertical_type    = '${VERT_TYPE}',
/
&control_times
 start_year    = ${SYEAR},
 start_month   = ${SMONTH},
 start_day     = ${SDAY},
 start_hour    = ${SHOUR},
 end_year      = ${EYEAR},
 end_month     = ${EMONTH},
 end_day       = ${EDAY},
 end_hour      = ${EHOUR},
 interval_hour = ${INTERVAL},
/
&control_vars
 num3dvar = ${NUM3D},
 var3d    = ${VAR3D}
 num2dvar = ${NUM2D},
 var2d    = ${VAR2D}
/
EOF
#------------
# Making full namelist file
#--------------------------------------------------
ln -sf $BUILD_DIR/da_verif_anal.exe .
#
./da_verif_anal.exe  
 RC=$?
 if test $RC != 0; then
   echo " da_verif_anal run failed with error $RC"
   exit 1
 fi

#=========================================================
# Now plotting starts
#=========================================================
iexp=0
pdat_dirs=''
for EXP_NAME in $EXP_NAMES; do
pdat_dirs[$iexp]="${WORK_DIR}/$iexp/"
iexp=$((iexp + 1))
done
#--------------------------------------------------
#---- Create control file for NCL script
#-----------------------------------------
DIAG_VAR=time_series_${VERIFY_HOUR}

echo "$Verify_Date_Range" > header_main
echo "ana_verif" >> header_main
#-----------------
#echo "${pdat_dirs[@]:0}" > ../dirnames
iexp=0
echo "${NUM_EXPT}" >> header_main
while [[ $iexp -lt $NUM_EXPT ]]; do
echo ${pdat_dirs[$iexp]} >> header_main
iexp=$((iexp + 1))
done
#-----------------
#declare -a ob_fnames
rm -f $WORK_DIR/tmp_upr 
num_upr=`ls ${pdat_dirs[0]}/*${DIAG_VAR} |wc -l`

OLDPWD=$PWD
cd ${pdat_dirs[0]}
for vn in U V TK QVAPOR; do
  ls ${vn}*${DIAG_VAR} >> $WORK_DIR/tmp_upr
done

cd $OLDPWD
 
#----------------
if [ "$num_upr" -lt 4 ]; then
   echo "All upper-air files are not generated"
   echo "Check your data and selected observation types"
else
   echo "All upper-air files generated successfully.."
   echo "fnames_upr" >> header_main    

   anyfile=`head -1 "$WORK_DIR/tmp_upr"`
   ncol=`head -1 ${pdat_dirs[0]}/$anyfile |wc -w`
   nrow=`cat ${pdat_dirs[0]}/$anyfile |wc -l`
   echo $nrow > $WORK_DIR/fnames_upr     
   echo $ncol >> $WORK_DIR/fnames_upr     
   while read ob_fname
   do
     if [[ "$ob_fname" = "TK_time_series_${VERIFY_HOUR}" ]]; then
        ob_unit='T (~S~o~N~C)'
     elif [[ "$ob_fname" = "U_time_series_${VERIFY_HOUR}" ]]; then
        ob_unit='U (ms~S~-1~N~)' 
     elif [[ "$ob_fname" = "V_time_series_${VERIFY_HOUR}" ]]; then
        ob_unit='V (ms~S~-1~N~)' 
     elif [[ "$ob_fname" = "QVAPOR_time_series_${VERIFY_HOUR}" ]]; then
        ob_unit='Q (gmKg~S~-1~N~)' 
     else
        echo "Unknown upper-air variable:-Don't know what to do??"
     fi
     echo "${ob_fname}" >> $WORK_DIR/fnames_upr
     echo "${ob_unit}" >> $WORK_DIR/fnames_upr
   done < $WORK_DIR/tmp_upr
fi
#ob_fnames = ( `cat "$tmp_file"` )
#rm -f $WORK_DIR/tmp_upr tmp_sfc
#-----------------------------------------------------------------------------------------------------------------------
# Run NCL scripts now 
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
for LEVEL in ${DESIRED_LEVELS} ; do

NCL_COMMAND_LINE="'wksdev=\"${PLOT_WKS}\"' 'run_dir=\"${RUN_DIR}\"' 'exp_legends=${EXP_LEGENDS}' 'exp_line_cols=${EXP_LINES_COLORS}' 'select_levs=${LEVEL}' 'select_scores=${DESIRED_SCORES}'   'bar_label_angle=${BAR_LABEL_ANGLE}' 'verify_hour=${VERIFY_HOUR}'"

echo "ncl ${NCL_COMMAND_LINE} ${WRFVAR_DIR}/graphics/ncl/verif_anal_time_series.ncl" > run1
chmod +x run1
./run1
#-----------------------------------------------------------------------------------------------------------------------
echo "ncl ${NCL_COMMAND_LINE} ${WRFVAR_DIR}/graphics/ncl/verif_anal_time_average.ncl" > run2
chmod +x run2
./run2
#-----------------------------------------------------------------------------------------------------------------------
done
#-----------------------------------------------------------------------------------------------------------------------
echo "ncl ${NCL_COMMAND_LINE} ${WRFVAR_DIR}/graphics/ncl/verif_anal_vert_profile.ncl" > run3
chmod +x run3
./run3

mv $WORK_DIR/*.pdf $RUN_DIR
#-----------------------------------------------------------------------------------------------------------------------
echo "successfully completed..."
#-----------------------------------------------------------------------------------------------------------------------
exit 0
