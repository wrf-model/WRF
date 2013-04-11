#!/bin/ksh
#-------------------------------------------------------------------------
#Script for running the (E-grid to C-grid) convertor program
#
# Author : Sujata Pattanayak,   CAS, IIT Delhi, India
# Date : 15 August 2008
#
#Note: INITIAL_DATE, FINAL_DATE, CYCLE_PERIOD should be given.
#Define your SRC_DIR, WRF_NMM_RC_DIR, RUN_DIR, ARW_RC_DIR
#--------------------------------------------------------------------------

export INITIAL_DATE=2007111200
export FINAL_DATE=2007111212
export CYCLE_PERIOD=12

export DATE=${INITIAL_DATE}
echo $DATE
pause
export SRC_DIR=/users/xinzhang/work/code/trunk/phoenix_g95_opt/sujata/convertor
export DAT_DIR=/users/xinzhang/work/code/trunk/phoenix_g95_opt/sujata
export REGION=india_27km_nmm
export EXPT=run_india_27km_nmm
export WRF_NMM_RC_DIR=/users/xinzhang/work/code/trunk/phoenix_g95_opt/sujata/nmm/rc
export WRF_NMM_RC_DIR=${WRF_NMM_RC_DIR:-$DAT_DIR/$REGION/$EXPT/rc}
export BUILD_DIR=/users/xinzhang/work/code/trunk/phoenix_g95_opt/sujata/wrfvar-nmm_branches_V3.0_Release/build

export ARW_RC_DIR=$DAT_DIR/arw/rc
export ARW_RC_DIR=${ARW_RC_DIR:-$DAT_DIR/$REGION/rc}
export RUN_DIR=${RUN_DIR:-$DAT_DIR/$REGION}

while [[ $DATE -le $FINAL_DATE ]] ; do

   echo "==========================="
   echo " Doing job for " $DATE
   echo "==========================="

   mkdir -p ${ARW_RC_DIR}/${DATE}

   export YEAR=$(echo ${DATE} | cut -c1-4)
   export MONTH=$(echo ${DATE} | cut -c5-6)
   export DAY=$(echo ${DATE} | cut -c7-8)
   export HOUR=$(echo ${DATE} | cut -c9-10)

   export SIM_START_DATE=${YEAR}-${MONTH}-${DAY}_${HOUR}:00:00
   export START_DATE=${SIM_START_DATE}
   export FCST_TIME=${YEAR}-${MONTH}-${DAY}_${HOUR}:00:00

echo "SIM_START_DATE =" ${SIM_START_DATE}
echo "START_DATE=" ${SIM_START_DATE}
echo "FCST_TIME =" ${FCST_TIME}

echo "Doing job for File :"   ${WRF_NMM_RC_DIR}/${DATE}/wrfinput_d01

cd ${SRC_DIR}
   make
       mkdir -p ${RUN_DIR}; cd ${RUN_DIR}
       ln -sf ${WRF_NMM_RC_DIR}/$DATE/wrfinput_d01  .
       ${SRC_DIR}/convert_e2c.exe ${FCST_TIME} ${START_DATE} ${SIM_START_DATE}   

       mv wrfoutput_d01 $ARW_RC_DIR/${DATE}/wrfinput_d01
 
   export NEXT_DATE=$($BUILD_DIR/da_advance_time.exe ${DATE} ${CYCLE_PERIOD} 2>/dev/null)
   export DATE=$NEXT_DATE

done

exit 0
