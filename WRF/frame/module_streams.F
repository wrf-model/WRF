!WRF:DRIVER_LAYER:IO_STREAMS
MODULE module_streams


! registry-generated switch parameters, alarms

#include "switches_and_alarms.inc"

  INTEGER, PARAMETER :: first_history     = history_only
  INTEGER, PARAMETER :: last_history      = history_only+MAX_HISTORY-1
  INTEGER, PARAMETER :: first_auxhist     = auxhist1_only
  INTEGER, PARAMETER :: last_auxhist      = last_history
  INTEGER, PARAMETER :: first_input       = input_only
  INTEGER, PARAMETER :: last_input        = input_only+MAX_HISTORY-1
  INTEGER, PARAMETER :: first_auxinput    = auxinput1_only
  INTEGER, PARAMETER :: last_auxinput     = last_input
  INTEGER, PARAMETER :: first_stream      = first_history
  INTEGER, PARAMETER :: last_stream       = last_input
  INTEGER, PARAMETER :: restart_only      = 2*(MAX_HISTORY)+1
  INTEGER, PARAMETER :: boundary_only     = 2*(MAX_HISTORY)+2

  INTEGER, PARAMETER :: RESTART_ALARM     = restart_only
  INTEGER, PARAMETER :: BOUNDARY_ALARM    = boundary_only

  INTEGER, PARAMETER :: INPUTOUT_ALARM              = 2*(MAX_HISTORY)+3       ! for outputing input (e.g. for 3dvar)
  INTEGER, PARAMETER :: ALARM_SUBTIME               = 2*(MAX_HISTORY)+4
  INTEGER, PARAMETER :: COMPUTE_VORTEX_CENTER_ALARM = 2*(MAX_HISTORY)+5

  INTEGER, PARAMETER :: MAX_WRF_ALARMS    = COMPUTE_VORTEX_CENTER_ALARM  ! WARNING:  MAX_WRF_ALARMS must be
                                                                         ! large enough to include all of
                                                                         ! the alarms declared above.

  CONTAINS
    SUBROUTINE init_module_streams
    END SUBROUTINE init_module_streams

END MODULE module_streams
