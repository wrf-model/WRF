#!/bin/ksh
#============================================================;
# Purpose:  Main script to run the verification package
#============================================================;
# Author: Syed RH Rizvi          MMM/NCAR       
# Date  : 08/30/2007
#
#============================================================;

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh

export NUM_EXPT=${NUM_EXPT:-2}
export EXP_DIRS=${EXP_DIRS:-${REG_DIR}/NO_NOISE ${REG_DIR}/NOISE}
export EXP_NAMES=${EXP_NAMES:-"NO_NOISE" "NOISE"}
export EXP_LEGENDS=${EXP_LEGENDS:-(/"no_noise","noise"/)}

export START_DATE=${START_DATE:-2003010100}
export END_DATE=${END_DATE:-2003010100}

export RUN_DIR=${RUN_DIR:-$PWD/verification}
export WORK_DIR=${WORK_DIR:-$RUN_DIR/working}

export INTERVAL=${INTERVAL:-12}
export Verify_Date_Range=${Verify_Date_Range:-"01 - 28 October 2006 (${INTERVAL} hour Cycle)"}

export NUM_OBS_TYPE=${NUM_OBS_TYPES:-4}
export OBS_TYPES=${OBS_TYPES:-synop sound airep geoamv}

export DESIRED_LEVELS=${DESIRED_LEVELS:-'(/"850","500", "200"/)'}
export DESIRED_SCORES=${DESIRED_SCORES:-'(/"RMSE","BIAS", "ABIAS"/)'}
export EXP_LINES_COLORS=${EXP_LINES_COLORS:-'(/"blue","green", "orange"/)'}

export PLOT_WKS=${PLOT_WKS:-x11}
export CLEAN=${CLEAN:-false}
export GET_OMBOMA_PLOTS=${GET_OMBOMA_PLOTS:-false}
export FILE_PATH_STRING=${FILE_PATH_STRING:-'wrfvar/working/gts_omb_oma'}

#=========================================================
#=========================================================
# BELOW THIS DO NOT CHANGE ANYTHING : CHANGE NOT REQUIRED
#=========================================================
#=========================================================
DIAG_VAR1="omb"
DIAG_VAR2="oma"

echo "<HTML><HEAD><TITLE>Verification Plots for $EXP_NAMES<TITLE></HEAD>"
echo "<BODY><H1>Verification Plots for $EXP_NAMES</H1><PRE>"

rm -rf $WORK_DIR; mkdir -p $WORK_DIR; cd $WORK_DIR

iexp=0
exp_dirs=''
out_dirs=''
for EXP_DIR in $EXP_DIRS; do
   exp_dirs="$exp_dirs '$EXP_DIR',"
done
for EXP_NAME in $EXP_NAMES; do
   out_dirs="$out_dirs '$EXP_NAME',"
   pdat_dirs[$iexp]="$EXP_NAME/"
   mkdir -p $EXP_NAME
   iexp=$((iexp + 1))
done

cat > namelist.plot_diag << EOF
&record1
   exp_num   = ${NUM_EXPT} ,
   exp_dirs  = ${exp_dirs}
   out_dirs  = ${out_dirs} /
&record2
   start_date = '${START_DATE}',
   end_date   = '${END_DATE}',
   interval   = ${INTERVAL} /
&record3
   if_plot_rmse = .TRUE.,
   if_plot_bias = .TRUE.,
   if_plot_abias = .TRUE. /
EOF

cat >> namelist.plot_diag << EOF
&record4
EOF


for OBS_TYPE in $OBS_TYPES; do
   cat >> namelist.plot_diag << EOF
   if_plot_${OBS_TYPE} = .TRUE.,
EOF
done
echo '/' >> namelist.plot_diag

cat >> namelist.plot_diag << EOF
&record5
   file_path_string = '${FILE_PATH_STRING}' /
EOF

ln -sf $BUILD_DIR/da_verif_obs.exe .

nfile=$(ls *omb.diag 2>/dev/null | wc -l)
if [[ $nfile -eq 0 ]]; then
   ./da_verif_obs.exe > da_verif.out 2>&1
   RC=$?
   if [[ $RC != 0 ]]; then
      echo "da_verif_obs.exe failed with error $RC"
      exit 1
   fi
else
   echo "*omb.diag Files already exists, skipping..."
   echo "If you want to compute diag again...."
   echo "Just delete files manually and re-run"
fi

# Create control file for NCL script

echo "$Verify_Date_Range" > header_main
echo "$DIAG_VAR1" >> header_main
echo "$DIAG_VAR2" >> header_main

iexp=0
echo "${NUM_EXPT}" >> header_main
while [[ $iexp -lt $NUM_EXPT ]]; do
   echo ${pdat_dirs[$iexp]} >> header_main
   iexp=$((iexp + 1))
done

rm -f tmp_upr tmp_sfc tmp_gupr tmp_gpsref tmp_ggpsref
num_sfc=`ls ${pdat_dirs[0]}/surface*${DIAG_VAR1}.diag |wc -l`
num_upr=`ls ${pdat_dirs[0]}/upr*${DIAG_VAR1}.diag |wc -l`
num_gupr=`ls ${pdat_dirs[0]}/gupr*${DIAG_VAR1}.diag |wc -l`
num_gpsref=`ls ${pdat_dirs[0]}/gps_ref*${DIAG_VAR1}.diag |wc -l`
num_ggpsref=`ls ${pdat_dirs[0]}/ggps_ref*${DIAG_VAR1}.diag |wc -l`

cd ${pdat_dirs[0]}
if [ "$num_sfc" -ne 0 ]; then
for vn in u v t q p; do
   ls sur*${vn}*${DIAG_VAR1}.diag >> ../tmp_sfc 2>/dev/null
   ls sur*${vn}*${DIAG_VAR2}.diag >> ../tmp_sfc 2>/dev/null
done
fi
if [ "$num_upr" -ne 0 ]; then
for vn in u v t q; do
   ls upr*${vn}*${DIAG_VAR1}.diag >> ../tmp_upr 2>/dev/null
   ls upr*${vn}*${DIAG_VAR2}.diag >> ../tmp_upr 2>/dev/null
done
fi
if [ "$num_gupr" -ne 0 ]; then
for vn in u v t q; do
   ls gupr*${vn}*${DIAG_VAR1}.diag >> ../tmp_gupr 2>/dev/null
   ls gupr*${vn}*${DIAG_VAR2}.diag >> ../tmp_gupr 2>/dev/null
done
fi

if [ "$num_gpsref" -ne 0 ]; then
  ls gps_ref*${DIAG_VAR1}.diag >> ../tmp_gpsref
  ls gps_ref*${DIAG_VAR2}.diag >> ../tmp_gpsref
fi
if [ "$num_ggpsref" -ne 0 ]; then
  ls ggps_ref*${DIAG_VAR1}.diag >> ../tmp_ggpsref
  ls ggps_ref*${DIAG_VAR2}.diag >> ../tmp_ggpsref
fi

cd ..
 
if [ "$num_sfc" -lt 5 ]; then
   echo "All surface files are not generated"
   echo "Check your data and selected observation types"
   plotsfc=false
else
   echo "All surface files generated successfully.."
   echo "fnames_sfc" >> header_main
   plotsfc=true   

   anyfile=$(head -1 "tmp_sfc")
   ncol=$(head -1 ${pdat_dirs[0]}/$anyfile |wc -w)
   nrow=$(cat ${pdat_dirs[0]}/$anyfile |wc -l)
   echo $nrow > fnames_sfc
   echo $ncol >> fnames_sfc
   while read ob_fname
   do
      if [[ "$ob_fname" == "surface_p_${DIAG_VAR1}.diag" ]]; then
         ob_unit='"hPa"'
      elif [[ "$ob_fname" == "surface_t_${DIAG_VAR1}.diag" ]]; then
         ob_unit='T (Degree)'
      elif [[ "$ob_fname" == "surface_u_${DIAG_VAR1}.diag" ]]; then
         ob_unit='U (m/s)' 
      elif [[ "$ob_fname" == "surface_v_${DIAG_VAR1}.diag" ]]; then
         ob_unit='V (m/s)' 
      elif [[ "$ob_fname" == "surface_q_${DIAG_VAR1}.diag" ]]; then
         ob_unit='Q (gm/Kg)' 
      else
         echo "Unknown surface variable:-Don't know what to do??"
      fi
      echo "$ob_fname" >> fnames_sfc
      read ob_fname
      echo "$ob_fname" >> fnames_sfc
      echo "$ob_unit" >> fnames_sfc
   done < tmp_sfc
fi

if [[ "$num_upr" -lt 4 ]]; then
   echo "All upper-air files are not generated"
   echo "Check your data and selected observation types"
   plotupr="false"
else
   echo "All upper-air files generated successfully.."
   echo "fnames_upr" >> header_main    
   plotupr="true"  

   anyfile=$(head -1 "tmp_upr")
   ncol=$(head -1 ${pdat_dirs[0]}/$anyfile |wc -w)
   nrow=$(cat ${pdat_dirs[0]}/$anyfile |wc -l)
   echo $nrow > fnames_upr     
   echo $ncol >> fnames_upr     
   while read ob_fname; do
      if [[ "$ob_fname" == "upr_t_${DIAG_VAR1}.diag" ]]; then
         ob_unit='T (Degree)'
      elif [[ "$ob_fname" == "upr_u_${DIAG_VAR1}.diag" ]]; then
         ob_unit='U (m/s)' 
      elif [[ "$ob_fname" == "upr_v_${DIAG_VAR1}.diag" ]]; then
         ob_unit='V (m/s)' 
      elif [[ "$ob_fname" == "upr_q_${DIAG_VAR1}.diag" ]]; then
         ob_unit='Q (gm/Kg)' 
      else
         echo "Unknown upper-air variable:-Don't know what to do??"
      fi
      echo "${ob_fname}" >> fnames_upr
      read ob_fname
      echo "${ob_fname}" >> fnames_upr
      echo "${ob_unit}" >> fnames_upr
   done < tmp_upr
fi

#gupr
if [ "$num_gupr" -lt 4 ]; then
   echo "All upper-air vertical (gupr) files are not generated"
   echo "Check your data and selected observation types"
else
   echo "All upper-air vertical (gupr) files generated successfully.."
   echo "fnames_gupr" >> header_main

   anyfile=`head -1 "tmp_gupr"`
   ncol=`head -1 ${pdat_dirs[0]}/$anyfile |wc -w`
   nrow=`cat ${pdat_dirs[0]}/$anyfile |wc -l`
   echo $nrow > fnames_gupr
   echo $ncol >> fnames_gupr
   while read ob_fname
   do
      if [ "$ob_fname" = "gupr_t_${DIAG_VAR1}.diag" ]; then
         ob_unit='T (Degree)'
      elif [ "$ob_fname" = "gupr_u_${DIAG_VAR1}.diag" ]; then
         ob_unit='U (m/s)'
      elif [ "$ob_fname" = "gupr_v_${DIAG_VAR1}.diag" ]; then
         ob_unit='V (m/s)'
      elif [ "$ob_fname" = "gupr_q_${DIAG_VAR1}.diag" ]; then
         ob_unit='Q (gm/Kg)'
      else
         echo "Unknown upper-air veritcal (gupr) variable:-Don't know what to do??"
      fi
      echo "${ob_fname}" >> fnames_gupr
      read ob_fname
      echo "${ob_fname}" >> fnames_gupr
      echo "${ob_unit}" >> fnames_gupr
   done < tmp_gupr
fi


#----------------
if [ "$num_gpsref" -lt 1 ]; then
   echo "All gpsref files are not generated"
   echo "Check your data and selected observation types"
   plotgpsref="false"
else
   echo "All gpsref files generated successfully.."
   plotgpsref="true"
   echo "fnames_gpsref" >> header_main
   anyfile=`head -1 "tmp_gpsref"`
   ncol=`head -1 ${pdat_dirs[0]}/$anyfile |wc -w`
   nrow=`cat ${pdat_dirs[0]}/$anyfile |wc -l`
   echo $nrow > fnames_gpsref
   echo $ncol >> fnames_gpsref
   while read ob_fname
   do
     if [ "$ob_fname" = "gps_ref_${DIAG_VAR1}.diag" ]; then
        ob_unit='Refractivity' 
     else
        echo "Unknown gpsref variable:-Don't know what to do??"
     fi
     echo "${ob_fname}" >> fnames_gpsref
     read ob_fname
     echo "${ob_fname}" >> fnames_gpsref
     echo "${ob_unit}" >> fnames_gpsref
   done < tmp_gpsref
fi
#----------------
#----------------
if [ "$num_ggpsref" -lt 1 ]; then
   echo "All ggpsref files are not generated"
   echo "Check your data and selected observation types"
else
   echo "All ggpsref files generated successfully.."
   echo "fnames_ggpsref" >> header_main
   anyfile=`head -1 "tmp_ggpsref"`
   ncol=`head -1 ${pdat_dirs[0]}/$anyfile |wc -w`
   nrow=`cat ${pdat_dirs[0]}/$anyfile |wc -l`
   echo $nrow > fnames_ggpsref
   echo $ncol >> fnames_ggpsref
   while read ob_fname
   do
     if [ "$ob_fname" = "ggps_ref_${DIAG_VAR1}.diag" ]; then
        ob_unit='Refractivity' 
     else
        echo "Unknown gpsref variable:-Don't know what to do??"
     fi
     echo "${ob_fname}" >> fnames_ggpsref
     read ob_fname
     echo "${ob_fname}" >> fnames_ggpsref
     echo "${ob_unit}" >> fnames_ggpsref
   done < tmp_ggpsref
fi
#----------------


BAR_LABEL_ANGLE=45

# Run NCL scripts now 

NCL_COMMAND_LINE="'wksdev=\"${PLOT_WKS}\"' 'run_dir=\"${WORK_DIR}\"' \
   'exp_legends=${EXP_LEGENDS}' 'exp_line_cols=${EXP_LINES_COLORS}' \
   'select_levs=${DESIRED_LEVELS}' 'get_omboma_plots=\"${GET_OMBOMA_PLOTS}\"' \
   'select_scores=${DESIRED_SCORES}' 'bar_label_angle=${BAR_LABEL_ANGLE}'"

#----------------
if [ "$plotsfc" = "true" ] || [ "$plotupr" = "true" ]; then
echo "ncl ${NCL_COMMAND_LINE} ${WRFVAR_DIR}/graphics/ncl/verif_obs_time_series.ncl" > run1
chmod +x run1
./run1 > run1.log 2>&1
fi
#----------------
if [ "$plotupr" = "true" ]; then
echo "ncl ${NCL_COMMAND_LINE} ${WRFVAR_DIR}/graphics/ncl/verif_obs_vert_profile.ncl" > run2
chmod +x run2
./run2 > run2.log 2>&1
fi
#----------------
if [ "$plotupr" = "true" ]; then
echo "ncl ${NCL_COMMAND_LINE} ${WRFVAR_DIR}/graphics/ncl/verif_obs_time_average.ncl" > run3
chmod +x run3
./run3 > run3.log 2>&1
fi
#----------------
if [ "$plotgpsref" = "true" ]; then
echo "ncl ${NCL_COMMAND_LINE} ${WRFVAR_DIR}/graphics/ncl/verif_obs_vert_profile_gpsref.ncl" > run4
chmod +x run4
./run4 > run4.log 2>&1
fi
#----------------
cd $RUN_DIR
mv $WORK_DIR/*.pdf $WORK_DIR/*.log .
echo "<BODY><H1>Verification Plots for $EXP_NAMES</H1><UL>"
for FILE in *.pdf *.log; do
   if [[ -f $FILE ]]; then
      echo '<LI><A HREF="'$FILE'">'$FILE'</a>'
   fi
done

echo "</UL>Output logs<UL>"
for FILE in *.log; do
   if [[ -f $FILE ]]; then
      echo '<LI><A HREF="'$FILE'">'$FILE'</a>'
   fi
done

echo "</UL></BODY></HTML>"

if $CLEAN; then
   rm -rf $WORK_DIR
fi


echo "da_verif_obs_plot.ksh successfully completed..."

exit 0
