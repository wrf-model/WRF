!KMA2NETCDF:DRIVER_LAYER:MAIN
!

PROGRAM kma2netcdf

   use module_configure, only : grid_config_rec_type
   use module_domain, only : domain
   use da_control, only : debug_level

   USE module_machine
   USE module_driver_constants
   USE module_configure

   USE module_timing
   USE module_wrf_error
#ifdef DM_PARALLEL
   USE module_dm
#endif

   USE da_wrfvar_io
   USE module_kma2netcdf_interface
   USE module_kma_wave2grid

   IMPLICIT NONE

   REAL    :: time

   TYPE (domain) , POINTER :: keep_grid, grid_ptr, null_domain
   TYPE (grid_config_rec_type)              :: config_flags

   INTEGER :: domain_id , fid , oid , idum1 , idum2 

#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4*8192
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif

!--------------
 INTERFACE 
SUBROUTINE Setup_Timekeeping ( grid )
   USE module_domain
   TYPE(domain), POINTER :: grid
END SUBROUTINE Setup_Timekeeping 
END INTERFACE 
!--------------

!  Definitions of dummy arguments to solve

!--Define the name of this program (program_name defined in module_domain)

   program_name = "KMA2NETCDF"

!--Get the NAMELIST data for input.

   CALL init_modules(1)
!rizvi's add  start
     ! Initialize utilities (time manager, etc.)
      call wrfu_initialize(defaultCalendar=WRFU_CAL_GREGORIAN)
!rizvi's add over

#ifdef DM_PARALLEL IF ( wrf_dm_on_monitor() ) THEN
   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
     CALL initial_config
   ENDIF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize
#else
   CALL start_timing
   CALL initial_config
#endif

!shc-wei start
!  CALL get_debug_level ( debug_level )
   CALL nl_get_debug_level ( 1,debug_level )
!shc-wei end
   CALL set_wrf_debug_level ( debug_level )

!--allocated and configure the mother domain

   NULLIFY( null_domain )

   CALL       wrf_debug ( 100 , 'wrf: calling alloc_and_configure_domain ' )
   CALL alloc_and_configure_domain ( domain_id  = 1 ,                  &
!                                    local_time = 0 ,                  &   !shc-wei
                                     grid       = head_grid ,          &
                                     parent     = null_domain ,        &
                                     kid        = -1                   )

   CALL       wrf_debug ( 100 , 'wrf: calling model_to_grid_config_rec ' )
   CALL model_to_grid_config_rec ( head_grid%id , model_config_rec , config_flags )
   CALL       wrf_debug ( 100 , 'wrf: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )
   CALL       wrf_debug ( 100 , 'wrf: calling init_wrfio' )
   CALL Setup_Timekeeping ( head_grid )
  if(config_flags%real_data_init_type == 1 .or. & 
     config_flags%real_data_init_type == 3) then
     CALL init_wrfio

#ifdef DM_PARALLEL
     CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
     CALL wrf_dm_bcast_bytes( configbuf, nbytes )
     CALL set_config_as_buffer( configbuf, configbuflen )
#endif
  endif
   call med_add_config_info_to_grid ( head_grid )

!--read  kma-ascii grid data and pack in grid-array

   CALL wrf_debug ( 100 , 'kma2netcdf: calling kma2netcdf_interface ' )

   CALL kma2netcdf_interface ( head_grid, config_flags)   

   CALL wrf_debug ( 100 , 'kma2netcdf: back from kma2netcdf_interface ' )

!--output 3dvar analysis
   CALL da_med_initialdata_output( head_grid , config_flags )  

   CALL wrf_debug ( 100 , 'kma2netcdf: back from med_initialdata_output' )

   CALL med_shutdown_io ( head_grid , config_flags )

#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
      message(1) = 'Time elapsed in kma2netcdf'
      CALL end_timing ( TRIM(message(1)) )
   END IF
#else
   message(1) = 'Time elapsed in kma2netcdf'
   CALL end_timing ( TRIM(message(1)) )
#endif

   CALL wrf_shutdown

   STOP "Stopped normally. in kma2netcdf"

END PROGRAM kma2netcdf

