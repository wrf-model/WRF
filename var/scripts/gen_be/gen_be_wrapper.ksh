#! /bin/ksh
#-----------------------------------------------------------------------
# Script gen_be_wrapper.ksh
#
# Purpose: Calculates background error statistics for WRF-Var.
#-----------------------------------------------------------------------

#[1] Define job by overriding default environment variables:

export RUN_GEN_BE_STAGE0=true
export RUN_GEN_BE_STAGE1=true
export RUN_GEN_BE_STAGE2=true
export RUN_GEN_BE_STAGE2A=true
export RUN_GEN_BE_STAGE3=true
export RUN_GEN_BE_STAGE4=true
export RUN_GEN_BE_DIAGS=true
export RUN_GEN_BE_DIAGS_READ=true
export RUN_GEN_BE_MULTICOV=true

export WRFVAR_DIR=/kukui/users/xinzhang/code/WRFDA

export START_DATE=2007091500	# the first perturbation valid date
export END_DATE=2007091700	# the last perturbation valid date
export NUM_LEVELS=27		# = bottom_top = e_vert - 1
export BIN_TYPE=5
#export DATA_ON_LEVELS=.true. # "False if fields projected onto modes."

export BE_METHOD=NMC
export FCST_RANGE=12
#Example of changes required for "be_method=ENS":
#export BE_METHOD=ENS
#export NE=2 # 30

export FC_DIR=/kukui/users/xinzhang/code/tmp	# where wrf forecasts are
export RUN_DIR=`pwd`/gen_be${BIN_TYPE}
export DOMAIN=01
export FCST_RANGE1=24
export FCST_RANGE2=12
export INTERVAL=12
export STRIDE=1
export USE_RFi=true		# use recursive filters?
#[2] Run gen_be:
if ${USE_RFi}; then
   ${WRFVAR_DIR}/var/scripts/gen_be/gen_be.ksh
else                          # loop over wavelet filter lengths:
   export DO_NORMALIZE=.false.	# normalize before wavelet transform?
   NEW_SUF=
   export RUN_DIR=${RUN_DIR}.
   for L in 7;do
      export WAVELET_NBAND=$L
      for N in C;do          # possible WAVELET_NAME values: B C D V
         export WAVELET_NAME=$N
         if [[ $WAVELET_NAME == B ]];then
            export ISTRT=18      
            export IINC=1
            export IFIN=$ISTRT
         elif [[ $WAVELET_NAME == C ]];then
            export ISTRT=30
            export IINC=6
            export IFIN=30
         elif [[ $WAVELET_NAME == D ]];then
            export ISTRT=6
            export IINC=2
            export IFIN=20
         elif [[ $WAVELET_NAME == V ]];then
            export ISTRT=24      
            export IINC=1
            export IFIN=$ISTRT
         fi
         for I in `seq ${ISTRT} ${IINC} ${IFIN}`; do
            export WAVELET_FILT_LEN=$I
            OLD_SUF=${NEW_SUF}
            NEW_SUF=${WAVELET_NBAND}${WAVELET_NAME}${WAVELET_FILT_LEN}n
            export RUN_DIR=${RUN_DIR%${OLD_SUF}}${NEW_SUF}
            ${WRFVAR_DIR}/var/scripts/gen_be/gen_be.ksh
         done
      done
   done
fi
