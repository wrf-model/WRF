#!/bin/ksh
#-----------------------------------------------------------------------
# Script da_get_date_range.ksh
#
# Purpose: Set environment variables associated with date range.
#
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# [1] Set environment variables:
#-----------------------------------------------------------------------

export DATE=${DATE:-2003010100}
export FCST_RANGE=${FCST_RANGE:-6}
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export BUILD_DIR=${BUILD_DIR:-$WRFVAR_DIR/var/da}

#-----------------------------------------------------------------------
# [2] Calculate environment variables:
#-----------------------------------------------------------------------

export START_YEAR="$(echo $DATE | cut -c1-4)"
export START_MONTH="$(echo $DATE | cut -c5-6)"
export START_DAY="$(echo $DATE | cut -c7-8)"
export START_HOUR="$(echo $DATE | cut -c9-10)"
export START_MINUTE=${START_MINUTE:-0}
export START_SECOND=${START_SECOND:-0}

export NL_START_YEAR="11*$(echo $DATE | cut -c1-4)"
export NL_START_MONTH="11*$(echo $DATE | cut -c5-6)"
export NL_START_DAY="11*$(echo $DATE | cut -c7-8)"
export NL_START_HOUR="11*$(echo $DATE | cut -c9-10)"
export NL_START_MINUTE=${NL_START_MINUTE:-11*0}
export NL_START_SECOND=${NL_START_SECOND:-11*0}

#export END_DATE=$($BUILD_DIR/da_advance_time.exe $DATE $FCST_RANGE 2>/dev/null)
export END_DATE=$($BUILD_DIR/da_advance_time.exe $DATE $FCST_RANGE)

export END_YEAR="$(echo $END_DATE | cut -c1-4)"
export END_MONTH="$(echo $END_DATE | cut -c5-6)"
export END_DAY="$(echo $END_DATE | cut -c7-8)"
export END_HOUR="$(echo $END_DATE | cut -c9-10)"
export END_MINUTE=${END_MINUTE:-0}
export END_SECOND=${END_SECOND:-0}

export NL_END_YEAR="11*$(echo $END_DATE | cut -c1-4)"
export NL_END_MONTH="11*$(echo $END_DATE | cut -c5-6)"
export NL_END_DAY="11*$(echo $END_DATE | cut -c7-8)"
export NL_END_HOUR="11*$(echo $END_DATE | cut -c9-10)"
export NL_END_MINUTE=${NL_END_MINUTE:-11*0}
export NL_END_SECOND=${NL_END_SECOND:-11*0}
