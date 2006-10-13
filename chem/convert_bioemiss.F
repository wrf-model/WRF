! This is a program that converts biobenic emissions data 
!    into WRF input data.
!

PROGRAM convert_bioemiss
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   USE module_machine
   USE module_domain
   USE module_initialize
   USE module_integrate
   USE module_driver_constants
   USE module_configure
   USE module_io_wrf
   USE module_io_domain
   USE module_timing
   USE module_utility
   USE module_wrf_error
   USE module_input_chem_bioemiss
#ifdef DM_PARALLEL
   USE module_dm
#endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   IMPLICIT NONE

   INTERFACE
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping
   END INTERFACE

   REAL    :: time 

   INTEGER :: loop , levels_to_process
   INTEGER :: rc

   TYPE(domain) , POINTER      :: keep_grid, grid_ptr, null_domain, grid, ingrid
   TYPE (grid_config_rec_type) :: config_flags, config_flags_in
   INTEGER                     :: number_at_same_level

   INTEGER :: max_dom, domain_id
   INTEGER :: id1 , id , fid, ierr
   INTEGER :: idum1, idum2 , ihour
#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* CONFIG_BUF_LEN
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif

   REAL    :: dt_from_file, tstart_from_file, tend_from_file
   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: i , j , k , idts, ntsd, emi_frame, nemi_frames
   INTEGER :: debug_level = 0

   CHARACTER (LEN=80)     :: message

   CHARACTER(LEN=24) :: previous_date , this_date , next_date
   CHARACTER(LEN=19) :: start_date_char , end_date_char , current_date_char , next_date_char
   CHARACTER(LEN= 4) :: loop_char

   INTEGER :: start_year , start_month , start_day , start_hour , start_minute , start_second
   INTEGER ::   end_year ,   end_month ,   end_day ,   end_hour ,   end_minute ,   end_second
   INTEGER :: interval_seconds , real_data_init_type
   INTEGER :: time_loop_max , time_loop

   REAL :: cen_lat, cen_lon, moad_cen_lat, truelat1, truelat2, gmt, stand_lon, dum1
   INTEGER :: map_proj, julyr, julday, iswater, isice, isurban, isoilwater
   CHARACTER(LEN= 8) :: chlanduse


   CHARACTER (LEN=80) :: inpname , eminame, dum_str, wrfinname

! these are needed on some compilers, eg compaq/alpha, to
! permit pass by reference through the registry generated
! interface to med_read_emissions, below
#ifdef DEREF_KLUDGE
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
#endif

   !  Get the NAMELIST data for input.

   !  Define the name of this program (program_name defined in module_domain)

   program_name = "WRF V2.1.2 BIOGENIC EMISSIONS PREPROCESSOR"

#ifdef DM_PARALLEL
   CALL disable_quilting
#endif

!  CALL init_modules
   CALL       wrf_debug ( 100 , 'convert_emiss: calling init_modules ' )
   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)
   CALL WRFU_Initialize( defaultCalendar=WRFU_CAL_GREGORIAN, rc=rc )
   CALL init_modules(2)   ! Phase 2 resumes after MPI_INIT() (if it is called)


#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
     CALL initial_config
     CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
     CALL wrf_dm_bcast_bytes( configbuf, nbytes )
     CALL set_config_as_buffer( configbuf, configbuflen )
   ENDIF
   CALL wrf_dm_initialize
#else
   CALL initial_config
#endif

   CALL  wrf_message ( program_name )

   CALL init_wrfio

!     !  Get the grid info from the wrfinput file

   write(message,FMT='(A)') ' allocate for wrfinput_d01 '
   CALL  wrf_message ( program_name )
   NULLIFY( null_domain )
   CALL alloc_and_configure_domain ( domain_id  = 1           , &
                                     grid       = head_grid   , &
                                     parent     = null_domain , &
                                     kid        = -1            )
   write(message,FMT='(A)') ' pointer for wrfinput_d01 '
   CALL  wrf_debug ( 100, message )
   grid => head_grid
   write(message,FMT='(A)') ' set scalars for wrfinput_d01 '
   CALL  wrf_debug ( 100, message )
   CALL set_scalar_indices_from_config ( grid%id , idum1, idum2 )

   write(message,FMT='(A)') ' construct filename for wrfinput_d01 '
   CALL  wrf_debug ( 100, message )
   CALL construct_filename1( wrfinname , 'wrfinput' , grid%id , 2 )

   write(message,FMT='(A,A)') ' open file ',TRIM(wrfinname)
   CALL  wrf_message ( message )
   CALL open_r_dataset ( fid, TRIM(wrfinname) , grid , config_flags , "DATASET=INPUT", ierr )


   write(message,FMT='(A)') ' wrfinput open error check '
   CALL  wrf_debug ( 100, message )
   IF ( ierr .NE. 0 ) THEN
      WRITE( wrf_err_message , FMT='(A,A,A,I8)' )  &
          'program convert_emiss: error opening ',TRIM(wrfinname),' for reading ierr=',ierr
      CALL WRF_ERROR_FATAL ( wrf_err_message )
   ENDIF
   write(message,FMT='(A)') ' past opening wrfinput_d01 '
   CALL  wrf_debug ( 100, message )

      CALL wrf_get_dom_ti_integer ( fid , 'MAP_PROJ' ,  map_proj , 1 , idum1 , ierr )
      CALL wrf_get_dom_ti_real    ( fid , 'CEN_LAT' , cen_lat , 1 , idum1 , ierr )
      CALL wrf_get_dom_ti_real    ( fid , 'CEN_LON' , cen_lon , 1 , idum1 , ierr )
      CALL wrf_get_dom_ti_real    ( fid , 'MOAD_CEN_LAT' , moad_cen_lat , 1 , idum1 , ierr )
      CALL wrf_get_dom_ti_real    ( fid , 'STAND_LON' , stand_lon , 1 , idum1 , ierr )
      CALL wrf_get_dom_ti_real    ( fid , 'TRUELAT1' , truelat1 , 1 , idum1 , ierr )
      CALL wrf_get_dom_ti_real    ( fid , 'TRUELAT2' , truelat2 , 1 , idum1 , ierr )
      CALL wrf_get_dom_ti_real    ( fid , 'GMT' , gmt , 1 , idum1 , ierr )
      CALL wrf_get_dom_ti_integer ( fid , 'JULYR' , julyr , 1 , idum1 , ierr )
      CALL wrf_get_dom_ti_integer ( fid , 'JULDAY' , julday , 1 , idum1 , ierr )
      CALL wrf_get_dom_ti_integer ( fid , 'ISWATER' , iswater , 1 , idum1 , ierr )
      CALL wrf_get_dom_ti_integer ( fid , 'ISICE  ' , isice   , 1 , idum1 , ierr )
      CALL wrf_get_dom_ti_integer ( fid , 'ISURBAN' , isurban , 1 , idum1 , ierr )
      CALL wrf_get_dom_ti_integer ( fid , 'ISOILWATER' , isoilwater , 1 , idum1 , ierr )
      CALL wrf_get_dom_ti_char    ( fid , 'MMINLU'  , chlanduse , ierr )
 
      CALL close_dataset      ( fid , config_flags , "DATASET=INPUT" )


   !  An available simple timer from the timing module.

   CALL set_scalar_indices_from_config ( grid%id , idum1, idum2 )

   CALL Setup_Timekeeping ( grid )
   CALL domain_clock_set( grid, &
                          time_step_seconds=model_config_rec%interval_seconds )
   CALL domain_clock_get ( grid, current_timestr=message )
   write(message,FMT='(A,A)') ' current_time ',Trim(message)
   CALL  wrf_debug ( 100, message )

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

!  print *,'start date=',model_config_rec%start_year(grid%id),model_config_rec%start_month(grid%id),&
!  model_config_rec%start_day(grid%id),model_config_rec%start_hour(grid%id)
!  print *,'end   date=',model_config_rec%end_year(grid%id),model_config_rec%end_month(grid%id),&
!  model_config_rec%end_day(grid%id),model_config_rec%end_hour(grid%id)
!  print *,'interval  =',model_config_rec%interval_seconds
!  print *,'init_typ  =',model_config_rec%real_data_init_type

   !  Figure out the starting and ending dates in a character format.

   start_year   = model_config_rec%start_year  (grid%id)
   start_month  = model_config_rec%start_month (grid%id)
   start_day    = model_config_rec%start_day   (grid%id)
   start_hour   = model_config_rec%start_hour  (grid%id)
   start_minute = model_config_rec%start_minute(grid%id)
   start_second = model_config_rec%start_second(grid%id)

   end_year   = model_config_rec%  end_year  (grid%id)
   end_month  = model_config_rec%  end_month (grid%id)
   end_day    = model_config_rec%  end_day   (grid%id)
   end_hour   = model_config_rec%  end_hour  (grid%id)
   end_minute = model_config_rec%  end_minute(grid%id)
   end_second = model_config_rec%  end_second(grid%id)

   interval_seconds    = 3600
   real_data_init_type = model_config_rec%real_data_init_type

   WRITE ( start_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
           start_year,start_month,start_day,start_hour,start_minute,start_second
   WRITE (   end_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
             end_year,  end_month,  end_day,  end_hour,  end_minute,  end_second

! these are needed on some compilers, eg compaq/alpha, to
! permit pass by reference through the registry generated
! interface to med_read_emissions, below
#ifdef DEREF_KLUDGE
   sm31             = grid%sm31
   em31             = grid%em31
   sm32             = grid%sm32
   em32             = grid%em32
   sm33             = grid%sm33
   em33             = grid%em33
#endif

   ihour = start_hour
   write(message,FMT='(A)') ' READ BIOGENIC EMISSIONS '
   CALL  wrf_debug ( 100, message )
   CALL input_ext_chem_beis3_file ( grid )
   write(message,FMT='(A)') ' PAST READ BIOGENIC EMISSIONS '
   CALL  wrf_debug ( 100, message )

   grid%input_from_file = .false.

   write(message,FMT='(A)') ' OPEN BIOGENIC EMISSIONS WRF file'
   CALL  wrf_debug ( 100, message )

   CALL construct_filename1( inpname , 'wrfbiochemi' , grid%id , 2 )
   CALL open_w_dataset ( id1, TRIM(inpname) , grid , config_flags , output_aux_model_input4 , "DATASET=AUXINPUT4", ierr )
   write(message,FMT='(A,A)') ' BIOGENIC EMISSIONS file name: ',TRIM(inpname)
   CALL  wrf_message ( message )
   
   IF ( ierr .NE. 0 ) THEN
     CALL wrf_error_fatal( 'real: error opening wrfchem emissions file for writing' )
   ENDIF

   write(message,FMT='(A)') ' PAST OPEN BIOGENIC EMISSIONS WRF file '
   CALL  wrf_debug ( 100, message )

   CALL calc_current_date ( grid%id , 0. )
   CALL geth_newdate ( current_date_char, current_date, 3600 )
   current_date = current_date_char // '.0000'

      if( stand_lon  == 0. ) then
         stand_lon = cen_lon
      endif
 
      if( moad_cen_lat  == 0. ) then
         moad_cen_lat = cen_lat
      endif

   CALL output_aux_model_input4 ( id1 , grid , config_flags , ierr )

   write(message,FMT='(A)') ' BIOGENIC EMISSIONS: fix global attributes '
   CALL  wrf_debug ( 100, message )

   ! write global atributes into wrf emissions file

    idum1 = 1
    call wrf_put_dom_ti_char    ( id1 , 'START_DATE' ,TRIM(start_date_char) , ierr )
    CALL wrf_put_dom_ti_integer ( id1 , 'MAP_PROJ'        , map_proj    , 1 , ierr )
    CALL wrf_put_dom_ti_real    ( id1 , 'MOAD_CEN_LAT'    , moad_cen_lat, 1 , ierr )
    CALL wrf_put_dom_ti_real    ( id1 , 'CEN_LAT'         , cen_lat     , 1 , ierr )
    CALL wrf_put_dom_ti_real    ( id1 , 'CEN_LON'         , cen_lon     , 1 , ierr )
    CALL wrf_put_dom_ti_real    ( id1 , 'STAND_LON'       , stand_lon   , 1 , ierr )
    CALL wrf_put_dom_ti_real    ( id1 , 'TRUELAT1'        , truelat1    , 1 , ierr )
    CALL wrf_put_dom_ti_real    ( id1 , 'TRUELAT2'        , truelat2    , 1 , ierr )
    CALL wrf_put_dom_ti_real    ( id1 , 'GMT'             , gmt         , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( id1 , 'JULYR'           , julyr       , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( id1 , 'JULDAY'          , julday      , 1 , ierr )
!   CALL wrf_put_dom_ti_integer ( id1 , 'CHEM_OPT'        , chem_opt    , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( id1 , 'ISWATER'         , iswater     , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( id1 , 'ISICE  '         , isice       , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( id1 , 'ISURBAN'         , isurban     , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( id1 , 'ISOILWATER'      , isoilwater  , 1 , ierr )
    CALL wrf_put_dom_ti_char    ( id1 , 'MMINLU'          , TRIM(chlanduse)   , ierr )


   CALL close_dataset ( id1 , config_flags , "DATASET=AUXOUTPUT4" )

   write(message,FMT='(A)') ' BIOGENIC EMISSIONS: end of program '
   CALL  wrf_message ( message )


    CALL wrf_shutdown
    CALL WRFU_Finalize( rc=rc )

!#ifdef DM_PARALLEL
!   CALL wrf_dm_shutdown
!#endif

   STOP

END PROGRAM convert_bioemiss
