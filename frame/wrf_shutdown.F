!WRF:DRIVER_LAYER:UTIL
!
SUBROUTINE wrf_shutdown
#ifdef DM_PARALLEL
    LOGICAL wrf_dm_on_monitor
    EXTERNAL wrf_dm_on_monitor
    EXTERNAL wrf_dm_shutdown
#endif
#ifdef DM_PARALLEL
    CALL wrf_dm_shutdown
#else
    STOP
#endif
END SUBROUTINE wrf_shutdown

