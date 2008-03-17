#!/bin/ksh
###############################################
#  Bias correction off-line statistics script
#  Author: Zhiquan Liu NCAR/MMM 02/2007
###############################################

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
. ${WRFVAR_DIR}/var/scripts/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/bias_stat}
export WORK_DIR=$RUN_DIR/working

export PLATFORM=noaa
export PLATFORM_ID=1
export SENSOR=$1     # amsua,amsub
export SATELLITE=$2  # 15,16,17,18
export SENSOR_ID=$3   # 3 , 4
export NSCAN=$4      # 30, 90


 echo 'WORKING directory is $WORK_DIR'

 mkdir $WORK_DIR; cd $WORK_DIR
 cp $WRFVAR_DIR/run/mask_asc $WORK_DIR

 CDATE=$START_DATE

 export sensor=${PLATFORM}-${SATELLITE}-${SENSOR}
 \rm -f biasprep_${sensor}

# 1.0 cat the data together
#------------------------------------
 while [[ $CDATE -le $END_DATE ]]; do
   echo $CDATE
   cat ${WORK_DIR}/${CDATE}/wrfvar/working/biasprep_${PLATFORM}-${SATELLITE}-${SENSOR}.* >> biasprep_${sensor}
   CDATE=$($BUILD_DIR/da_advance_time.exe ${CDATE} ${CYCLE_PERIOD})
 done

#--------------------------------------------------
# 2.0 data selection
#--------------------------------------------------
#  ISURF :  1=sea only, 2=land+sea, 3=land only
echo 'Start da_bias_sele'  
cat > nml_sele << EOF
 &INPUTS
  platform_id  = ${PLATFORM_ID},
  satellite_id = ${SATELLITE},
  sensor_id    = ${SENSOR_ID},
  nscan  = ${NSCAN},
  isurf = 2
 /
EOF
 
### &END Linux namelist conversion

         \rm -f fort.*
         ln -fs biasprep_${sensor}  fort.10  # input: fort.10
         $BUILD_DIR/da_bias_sele.exe < nml_sele > da_bias_sele_${sensor}.log
         mv fort.11 biassele_${sensor}      # output fort.11
echo '  End da_bias_sele'

#--------------------------------------------------------------------------------
# 3.0 Compute scan bias 
#--------------------------------------------------------------------------------
echo 'Start da_bias_scan'
cat > nml_scan << EOF
 &INPUTS
  kscan = ${NSCAN},
  fac=2,
  global=.false.,
  sband=10,
  eband=14,
  smoothing=.true.,
 /
EOF

       \rm -f fort.*
       ln -fs biassele_${sensor}  fort.10  # input : fort.10
       $BUILD_DIR/da_bias_scan.exe < nml_scan > da_bias_scan_${sensor}.log
       mv fort.11 scan_core_${sensor}     # output: fort.11, statistics not divided by nobs
       mv fort.12 scan_bias_${sensor}     # scan bias both band/noband 
echo '  End da_bias_scan'

#------------------------------------------------------------------------------
# 4.0 Compute air-mass bias coefs
#------------------------------------------------------------------------------

echo "Start da_bias_airmass"

cat > nml_bias << EOF
 &INPUTS
  global=.false.,
  lscan = .true.,
  kscan = ${NSCAN},
  check_limb = .false.,
  check_mask = .false.,
  FAC=2,
  cdate=${CDATE},
 /
EOF

       \rm -f fort.*
       ln -fs scan_bias_${sensor} fort.12
       ln -fs biassele_${sensor}  fort.10
       $BUILD_DIR/da_bias_airmass.exe < nml_bias > da_bias_airmass_${sensor}.log
       mv bcor.asc ${sensor}.scor

echo "  End da_bias_airmass"

#------------------------------------------------------------------------------
# 5.0 verification
#------------------------------------------------------------------------------

echo "Start da_bias_verif"

cat > nml_verif << EOF
 &INPUTS
  global=.false.,
  lscan = .true.,
  kscanx= ${NSCAN},
  check_limb = .false.,
  check_mask = .false.,
  FAC=2,
 /
EOF

       \rm -f fort.*
       ln -fs biassele_${sensor}    fort.10
       ln -fs ${sensor}.scor    scor.asc
       $BUILD_DIR/da_bias_verif.exe < nml_verif > da_bias_verif_${sensor}.log
       mv fort.11 bias_verif_${sensor}
       mv bcor.asc ${sensor}.bcor

echo "  End da_bias_verif"

  \rm -f fort*
  \rm -f scor.asc 

exit
