#!/bin/ksh
#########################################################################
# Script: da_run_suite_wrapper_qcobs.ksh
#
# Purpose: Provide user-modifiable interface to da_run_suite.ksh script.
#
#########################################################################
#set echo 
export NUM_PROCS=1
export SUBMIT=none 
export WALLCLOCK=15
export PROJECT=64000420
#------------------------------------------------------------------------
#Decide which stages to run (run if true):
#export RUN_RESTORE_DATA_GRIB=true
#export RUN_RESTORE_DATA_RTOBS=true
#export RUN_WPS=true
#export RUN_REAL=true
#export RUN_OBSPROC=true

export RUN_WRFVAR=true
#export RUN_UPDATE_BC=true
#export RUN_WRF=true

#Experiment details:
#export CLEAN=${CLEAN:-true}

#Time info:
export INITIAL_DATE=2007082100
export FINAL_DATE=2007082500

#Directories:
export REL_DIR=/karri/users/guo/CODE
export WRFVAR_DIR=$REL_DIR/VAR/WRFDA
export WPS_DIR=$REL_DIR/V3.0/WPS
export WRF_DIR=$REL_DIR/V3.0/WRFV3

#Data directories:
export DAT_DIR=/karri3/guo/Verif_V3

export RC_DIR=$DAT_DIR
export OB_DIR=$DAT_DIR
export EXP_DIR=$DAT_DIR
export RUN_DIR=${RUN_DIR:-$EXP_DIR}

export CYCLING=${CYCLING:-false}
export CYCLE_PERIOD=06

# Namelist options
export NL_E_WE=222
export NL_E_SN=128
export MAP_PROJ=lambert
export REF_LAT=17.0655
export REF_LON=118.5891
export TRUELAT1=10.0
export TRUELAT2=40.0
export STAND_LON=120.0
export NL_DX=45000
export NL_DY=45000
export NL_E_VERT=45
#
export NL_NUM_FGAT_TIME=1
#
export BE_DIR=${BE_DIR:-/karri1/guo/CWB/BES/200708}
#export NL_CHECK_MAX_IV=.false.
#----------------------------------------------------------------------
# Filtered OBS:
#----------------------------------------------------------------------
export NL_ANALYSIS_TYPE=QC-OBS
export NL_NTMAX=0
export NL_SFC_ASSI_OPTIONS=1
#-----------------------------------------------------------------------
#  Here is the namelist flags to control different observations
#-----------------------------------------------------------------------
#export NL_USE_SYNOPOBS=${NL_USE_SYNOPOBS:-FLASE}
#export NL_USE_SHIPSOBS=${NL_USE_SHIPSOBS:-FLASE}
#export NL_USE_METAROBS=${NL_USE_METAROBS:-FLASE}
#export NL_USE_SOUNDOBS=${NL_USE_SOUNDOBS:-FALSE}
#export NL_USE_PILOTOBS=${NL_USE_PILOTOBS:-FLASE}
#export NL_USE_AIREPOBS=${NL_USE_AIREPOBS:-FLASE}
#export NL_USE_BUOYOBS=${NL_USE_BUOYOBS:-FLASE}
#export NL_USE_PROFILEROBS=${NL_USE_PROFILEROBS:-FLASE}
#-------------------------------------------------------------
#export NL_USE_GEOAMVOBS=${NL_USE_GEOAMVOBS:-FLASE}
#export NL_USE_POLARAMVOBS=${NL_USE_POLARAMVOBS:-FLASE}
#export NL_USE_SATEMOBS=${NL_USE_SATEMOBS:-TRUE}
#export NL_USE_SSMIRETRIEVALOBS=${NL_USE_SSMIRETRIEVALOBS:-FALSE}
#export NL_USE_SSMITBOBS=${NL_USE_SSMITBOBS:-FLASE}
#export NL_USE_SSMT1OBS=${NL_USE_SSMT1OBS:-FLASE}
#export NL_USE_SSMT2OBS=${NL_USE_SSMT2OBS:-FLASE}
#export NL_USE_QSCATOBS=${NL_USE_QSCATOBS:-FLASE}
#-------------------------------------------------------------
#export NL_USE_GPSPWOBS=${NL_USE_GPSPWOBS:-FLASE}
#export NL_USE_GPSREFOBS=${NL_USE_GPSREFOBS:-FLASE}
#-------------------------------------------------------------
#export NL_USE_BOGUSOBS=${NL_USE_BOGUSOBS:-TRUE}
#-------------------------------------------------------------
#export NL_USE_RADAROBS=${NL_USE_RADAROBS:-TRUE}
#export NL_USE_RADAR_RV=${NL_USE_RADAR_RV:-TRUE}
#export NL_USE_RADAR_RF=${NL_USE_RADAR_RF:-TRUE}
#-------------------------------------------------------------
#export NL_USE_HIRS2OBS=${NL_USE_HIRS2OBS:-TRUE}
#export NL_USE_HIRS3OBS=${NL_USE_HIRS3OBS:-TRUE}
#export NL_USE_MSUOBS=${NL_USE_MSUOBS:-TRUE}
#export NL_USE_AMSUAOBS=${NL_USE_AMSUAOBS:-TRUE}
#export NL_USE_AMSUBOBS=${NL_USE_AMSUBOBS:-TRUE}
#export NL_USE_AIRSOBS=${NL_USE_AIRSOBS:-TRUE}
#export NL_USE_AIRESRETOBS=${NL_USE_AIRESRETOBS:-TRUE}
#export NL_USE_EOS_AMSUAOBS=${NL_USE_EOS_AMSUAOBS:-TRUE}
#export NL_USE_EOS_RADOBS=${NL_USE_EOS_RADOBS:-TRUE}
#export NL_USE_HSBOBS=${NL_USE_HSBOBS:-TRUE}
#export NL_RTMINIT_NSENSOR=2
#export NL_RTMINIT_PLATFORM=1,1
#export NL_RTMINIT_SATID=15,16
#export NL_RTMINIT_SENSOR=3,3
#export NL_LQC_RAD=TRUE
#export NL_RTM_OPTION=1
#-----------------------------------------------------------------------
export SCRIPTS_DIR=$WRFVAR_DIR/var/scripts
$WRFVAR_DIR/var/scripts/da_run_suite.ksh

exit 0

