!IDEAL:DRIVER_LAYER
!
! create an initial data set for the WRF model based on an ideal condition
PROGRAM ideal

   USE module_machine
   USE module_domain
   USE module_initialize
   USE module_driver_constants
   USE module_configure

   USE module_timing
   USE module_wrf_error
#ifdef DM_PARALLEL
   USE module_dm
#endif

   IMPLICIT NONE

   REAL    :: time

   INTEGER :: loop , &
              levels_to_process


   TYPE(domain) , POINTER :: keep_grid, grid_ptr, null_domain, grid
   TYPE(domain)           :: dummy
   TYPE (grid_config_rec_type)              :: config_flags

   INTEGER :: max_dom , domain_id , fid , oid , idum1 , idum2 , ierr
   INTEGER :: debug_level
   LOGICAL :: input_from_file

   INTERFACE
     SUBROUTINE med_initialdata_output ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) , POINTER :: grid
       TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
     END SUBROUTINE med_initialdata_output 
   END INTERFACE


#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* CONFIG_BUF_LEN
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif

   CHARACTER (LEN=80)     :: message

   !  Define the name of this program (program_name defined in module_domain)

   program_name = "IDEAL V2.0 PREPROCESSOR"

   !  Get the NAMELIST data for input.

   CALL init_modules

#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
     CALL initial_config
   ENDIF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize
#else
   CALL initial_config
#endif
   CALL get_debug_level ( debug_level )
   CALL set_wrf_debug_level ( debug_level )

   CALL  wrf_message ( program_name )


   ! allocated and configure the mother domain

   NULLIFY( null_domain )

   CALL alloc_and_configure_domain ( domain_id  = 1 ,                  &
                                     grid       = head_grid ,          &
                                     parent     = null_domain ,        &
                                     kid        = -1                   )

   CALL       wrf_debug ( 100 , 'wrf: calling model_to_grid_config_rec ' )
   CALL model_to_grid_config_rec ( head_grid%id , model_config_rec , config_flags )
   CALL       wrf_debug ( 100 , 'wrf: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )
   CALL       wrf_debug ( 100 , 'wrf: calling init_wrfio' )
   CALL init_wrfio

#ifdef DM_PARALLEL
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
#endif

   CALL med_initialdata_output( head_grid , config_flags )

   CALL       wrf_debug (   0 , 'wrf: SUCCESS COMPLETE IDEAL INIT' )

   CALL wrf_shutdown

END PROGRAM ideal

SUBROUTINE med_initialdata_output ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
   USE module_initialize
  ! Model layer
   USE module_configure

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)  , POINTER                    :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                :: time_step_begin_restart
   INTEGER                :: fid , ierr , id
   CHARACTER (LEN=80)      :: rstname
   CHARACTER (LEN=80)      :: message
   CHARACTER (LEN=80)      :: inpname , bdyname

   !  Initialize the mother domain.

   grid%input_from_file = .false.
   CALL init_domain (  grid )
   CALL calc_current_date ( grid%id, 0.)

   grid%write_metadata = .false.
   CALL construct_filename1 ( inpname , 'wrfinput' , grid%id , 2 )
   CALL open_w_dataset ( id, TRIM(inpname) , grid , config_flags , output_model_input , "DATASET=INPUT", ierr )
   IF ( ierr .NE. 0 ) THEN
     WRITE (wrf_err_message,*)'ideal: error opening wrfinput for writing ',ierr
     CALL wrf_error_fatal( wrf_err_message )
   ENDIF
   grid%write_metadata = .true.
   grid%write_metadata = .true.
   CALL output_model_input ( id, grid , config_flags , ierr )
   CALL close_dataset ( id , config_flags, "DATASET=INPUT" )

   grid%write_metadata = .false.
   CALL construct_filename1 ( bdyname , 'wrfbdy' , grid%id , 2 )
   CALL open_w_dataset ( id, TRIM(bdyname) , grid , config_flags , output_boundary , "DATASET=BOUNDARY", ierr )
   IF ( ierr .NE. 0 ) THEN
     WRITE (wrf_err_message,*)'ideal: error opening wrfbdy for writing ',ierr
     CALL wrf_error_fatal( wrf_err_message )
   ENDIF
   grid%write_metadata = .true.
   CALL output_boundary ( id, grid , config_flags , ierr )
   CALL close_dataset ( id , config_flags , "DATASET=BOUNDARY" )

   RETURN
END SUBROUTINE med_initialdata_output
