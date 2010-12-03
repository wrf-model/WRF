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

export WRFVAR_DIR=~/research/trunk

export START_DATE=2008020612  # the first perturbation valid date
export END_DATE=2008020700    # the last perturbation valid date
export NUM_LEVELS=40          # e_vert - 1
export BIN_TYPE=5
#export DATA_ON_LEVELS=.true. # "False if fields projected onto modes."

#Example of changes required for "be_method=ENS":
#export BE_METHOD=ENS
export BE_METHOD=NMC
#export NE=30
#export FCST_RANGE=12

# from /mmmtmp/hclin/wrfda_tut_data_200907.tar.gz
export FC_DIR=~/research/wrf-da/wrfda_tut_data_200907/Testcase/fc # where wrf forecasts are
export SDIR=
#export SDIR=/subsam
#export FC_DIR=~/research/wrf-da/var/gen_be/${BE_METHOD}/fc${SDIR} # where wrf forecast ensemble is
export RUN_DIR=~/research/wrf-da/var/gen_be/${BE_METHOD}${SDIR}/gen_be${BIN_TYPE}
export DOMAIN=01
export FCST_RANGE1=24
#export FCST_RANGE1=0
export FCST_RANGE2=12
export INTERVAL=12
export STRIDE=1
export DO_NORMALIZE=.true.	# normalize before wavelet transform?
export USE_RFi=false		# use recursive filters?
#[2] Run gen_be:
if ${USE_RFi}; then
   ${WRFVAR_DIR}/var/scripts/gen_be/gen_be.ksh
else                          # loop over wavelet filter lengths:
   set echo
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
         export RUN_DIR=${RUN_DIR}.
         for I in `seq ${ISTRT} ${IINC} ${IFIN}`; do
#        for I in {${ISTRT}..${IFIN}..${IINC}}; do
            export WAVELET_FILT_LEN=$I
#           ${var.abc/%.*/.xyz} replaces var.abc by var.xyz:
            export RUN_DIR=${RUN_DIR/%.*/.${WAVELET_NBAND}${WAVELET_NAME}${WAVELET_FILT_LEN}}n
            ${WRFVAR_DIR}/var/scripts/gen_be/gen_be.ksh
         done
      done
   done
fi
