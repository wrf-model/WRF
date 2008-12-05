#!/bin/ksh
#########################################################################
# Script: da_run_ensmean.ksh
#
# Purpose: Calculate mean of ensemble of WRF forecasts.
#
# Description:
# WRF forecasts must be in wrfinput format.
#
#########################################################################

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/ensmean}
export WORK_DIR=$RUN_DIR/working

# Ensemble mean parameters:
export NV=${NV:-15}                               # Number of variables to average.
export CV=${CV:-"'U'", "'V'", "'W'", "'PH'", "'T'", "'MU'", "'TSLB'", "'TSK'", \
                "'QCLOUD'", "'QRAIN'", "'QVAPOR'", "'U10'", "'V10'", "'T2'", "'Q2'"} # Variable names

#------------------------------------------------------------------------------------------

mkdir -p $RUN_DIR
cd $RUN_DIR

export NEXT_DATE=$(${BUILD_DIR}/da_advance_time.exe $DATE $FCST_RANGE)
export YYYY=$(echo $NEXT_DATE | cut -c1-4)
export MM=$(echo $NEXT_DATE | cut -c5-6)
export DD=$(echo $NEXT_DATE | cut -c7-8)
export HH=$(echo $NEXT_DATE | cut -c9-10)
export FILE_DATE=${YYYY}-${MM}-${DD}_${HH}:00:00

export DIRECTORY=${FC_DIR}/${DATE}
export FILENAME=${FILE_TYPE}_d01_${FILE_DATE}

#Copy first member as template for mean:
cp ${DIRECTORY}.e001/${FILENAME} ${DIRECTORY}/${FILENAME}
cp ${DIRECTORY}.e001/${FILENAME} ${DIRECTORY}/${FILENAME}.vari

cat > gen_be_ensmean_nl.nl << EOF
  &gen_be_ensmean_nl
    directory = '${DIRECTORY}'
    filename = '${FILENAME}'
    num_members = ${NUM_MEMBERS},
    nv = ${NV},
    cv = ${CV} /
EOF

#Run:
ln -fs ${BUILD_DIR}/gen_be_ensmean.exe .
./gen_be_ensmean.exe > gen_be_ensmean.out 2>&1

exit 0

