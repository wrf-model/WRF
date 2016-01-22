!WRF:DRIVER_LAYER:UTIL
!
SUBROUTINE wrf_shutdown
#ifndef NO_IEEE_MODULE
   USE , INTRINSIC :: IEEE_EXCEPTIONS
#endif

#ifdef DM_PARALLEL
    LOGICAL wrf_dm_on_monitor
    EXTERNAL wrf_dm_on_monitor
    EXTERNAL wrf_dm_shutdown
#endif
#ifdef DM_PARALLEL
    CALL wrf_dm_shutdown
#else
# ifdef NO_IEEE_MODULE
    STOP
# else
    CALL IEEE_SET_FLAG(IEEE_INEXACT,.false.)
    STOP
# endif
#endif
END SUBROUTINE wrf_shutdown

