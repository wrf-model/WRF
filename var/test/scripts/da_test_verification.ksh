#!/bin/ksh

# da_test_verification.ksh

. da_test_defaults.ksh

. ./setup.ksh
. ${CASE}/setup.ksh

export REG_DIR=$PWD

export EXP1=${1:-~/data/afwa_2.2/$REGION/${ID}_quick_${NUM_PROCS}}/run
export EXP2=${2:-~/data/trunk/$REGION/${ID}_quick_${NUM_PROCS}}/run

NAME1=$(basename $EXP1)
NAME2=$(basename $EXP2)

export NUM_EXPT=2
export EXP_DIRS="$EXP1 $EXP2"
export EXP_NAMES="$NAME1 $NAME2"
export EXP_LEGENDS='(/"afwa_2.2","trunk"/)'
export VERIFY_HOUR=12

export INTERVAL=12
export PLOT_WKS=pdf
export CLEAN=${CLEAN:-false}
export VERIFY_ITS_OWN_ANALYSIS=true

export RUN_DIR=$PWD/${MACHINE}_verification
rm -rf $RUN_DIR
mkdir -p $RUN_DIR
cd $RUN_DIR

echo "<HTML><HEAD><TITLE>Verification tests<TITLE></HEAD>" > index.html
echo "<BODY><H1>Verification tests</H1><UL>" >> index.html

echo '<LI><A HREF="verif_obs.html">Verify Observations</a>' >> index.html
$WRFVAR_DIR/scripts/da_verif_obs_plot.ksh > verif_obs.html 2>&1

# Change directory names as different syntax for anal script

export EXP1=${1:-~/data/afwa_2.2/$REGION/${ID}_quick_${NUM_PROCS}}
export EXP2=${2:-~/data/trunk/$REGION/${ID}_quick_${NUM_PROCS}}
export CONTROL_EXP_DIR=$EXP1

NAME1=$(basename $EXP1)
NAME2=$(basename $EXP2)

export EXP_DIRS="$EXP1 $EXP2"
export EXP_NAMES="$NAME1 $NAME2"

echo '<LI><A HREF="verif_anal.html">Verify Analyses</a>' >> index.html
$WRFVAR_DIR/scripts/da_verif_anal_plot.ksh > verif_anal.html 2>&1

echo "</UL></BODY></HTML>" >> index.html
