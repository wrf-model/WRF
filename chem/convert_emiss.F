! This is a program that converts emissions data into WRF input data.
!

PROGRAM convert_emiss
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   USE module_machine
   USE module_domain
   USE module_io
   USE module_wrf_error
   USE module_io_wrf
   USE module_initialize
   USE module_integrate
   USE module_driver_constants
   USE module_state_description
   USE module_configure
   USE module_timing
   USE module_utility
   USE module_input_chem_data
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

   INTEGER ibuf(1)
   REAL rbuf(1)

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

   program_name = "WRF V2.1.2 EMISSIONS PREPROCESSOR "

#ifdef DM_PARALLEL
   CALL disable_quilting
#endif

   print *,' init modules '
!  CALL init_modules
   CALL       wrf_debug ( 100 , 'convert_emiss: calling init_modules ' )
   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)
   CALL WRFU_Initialize( defaultCalendar=WRFU_CAL_GREGORIAN, rc=rc )
   CALL init_modules(2)   ! Phase 2 resumes after MPI_INIT() (if it is called)
   print *,' past init modules '

#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
     CALL initial_config
     CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
     CALL wrf_dm_bcast_bytes( configbuf, nbytes )
     CALL set_config_as_buffer( configbuf, configbuflen )
   ENDIF
   CALL wrf_dm_initialize
#else
   print *,' initial config '
   CALL initial_config
#endif

   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   CALL  wrf_message ( program_name )

   CALL init_wrfio

!     !  Get the grid info from the wrfinput file

   print *,' allocate for wrfinput_d01 '
   NULLIFY( null_domain )
   CALL alloc_and_configure_domain ( domain_id  = 1           , &
                                     grid       = head_grid   , &
                                     parent     = null_domain , &
                                     kid        = -1            )
   print *,' pointer for wrfinput_d01 '
   grid => head_grid
   print *,' set scalars for wrfinput_d01 '
   CALL set_scalar_indices_from_config ( grid%id , idum1, idum2 )

   print *,' construct filename for wrfinput_d01 '
   CALL construct_filename1( wrfinname , 'wrfinput' , grid%id , 2 )

   print *,' open file ',TRIM(wrfinname)
   CALL open_r_dataset ( fid, TRIM(wrfinname) , grid , config_flags , "DATASET=INPUT", ierr )

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )


   print *,' wrfinput open error check '
   IF ( ierr .NE. 0 ) THEN
      WRITE( wrf_err_message , FMT='(A,A,A,I8)' )  &
          'program convert_emiss: error opening ',TRIM(wrfinname),' for reading ierr=',ierr
      CALL WRF_ERROR_FATAL ( wrf_err_message )
   ENDIF
   print *,' past opening wrfinput_d01 '

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

!  NULLIFY( null_domain )
!  CALL alloc_and_configure_domain ( domain_id  = 1           , &
!                                    grid       = head_grid   , &
!                                    parent     = null_domain , &
!                                    kid        = -1            )

!  grid => head_grid
   CALL set_scalar_indices_from_config ( grid%id , idum1, idum2 )


   CALL Setup_Timekeeping ( grid )
   CALL domain_clockprint ( 150, grid, &
          'DEBUG emissions_convert:  clock after Setup_Timekeeping,' )
   CALL domain_clock_set( grid, &
                          time_step_seconds=model_config_rec%interval_seconds )
   CALL domain_clockprint ( 150, grid, &
          'DEBUG emissions_convert:  clock after timeStep set,' )

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   print *,'start date=',model_config_rec%start_year(grid%id),model_config_rec%start_month(grid%id),&
   model_config_rec%start_day(grid%id),model_config_rec%start_hour(grid%id)
   print *,'end   date=',model_config_rec%end_year(grid%id),model_config_rec%end_month(grid%id),&
   model_config_rec%end_day(grid%id),model_config_rec%end_hour(grid%id)
   print *,'interval  =',model_config_rec%interval_seconds
   print *,'init_typ  =',model_config_rec%real_data_init_type

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


   !  Figure out our loop count for the processing times.

   time_loop = 1
   PRINT '(A,I4,A,A,A)','Time period #',time_loop,' to process = ',start_date_char,'.'
   current_date_char = start_date_char
   loop_count : DO
      CALL geth_newdate ( next_date_char , current_date_char , interval_seconds )
      IF      ( next_date_char .LT. end_date_char ) THEN
         time_loop = time_loop + 1 
         PRINT '(A,I4,A,A,A)','Time period #',time_loop,' to process = ',next_date_char,'.'
         current_date_char = next_date_char
      ELSE IF ( next_date_char .EQ. end_date_char ) THEN
         time_loop = time_loop + 1 
         PRINT '(A,I4,A,A,A)','Time period #',time_loop,' to process = ',next_date_char,'.'
         PRINT '(A,I4,A)','Total analysis times to input = ',time_loop,'.'
         time_loop_max = time_loop
         EXIT loop_count
      ELSE IF ( next_date_char .GT. end_date_char ) THEN
         PRINT '(A,I4,A)','Total analysis times to input = ',time_loop,'.'
         time_loop_max = time_loop
         EXIT loop_count
      END IF
   END DO loop_count
   PRINT '(A,I4,A,I4)','Total number of times to input = ',time_loop,' ',time_loop_max

   !  Here we define the initial time to process, for later use by the code.

   current_date_char = start_date_char
   start_date = start_date_char // '.0000'
   current_date = start_date

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
   if( debug_level >= 100) print *,' READ EMISSIONS 1'
   CALL med_read_bin_chem_emiss ( grid , config_flags, ihour, time_loop )
   if( debug_level >= 100) print *,' PAST READ EMISSIONS 1'

   grid%input_from_file = .false.

   if( debug_level >= 100) print *,' OPEN EMISSIONS WRF file '

   CALL construct_filename1( inpname , 'wrfchemi' , grid%id , 2 )
   CALL open_w_dataset ( id1, TRIM(inpname) , grid , config_flags , output_aux_model_input5 , "DATASET=AUXINPUT5", ierr )
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   print *,' EMISSIONS file name: ',TRIM(inpname)
   IF ( ierr .NE. 0 ) THEN
     CALL wrf_error_fatal( 'real: error opening wrfchem emissions file for writing' )
   ENDIF

   if( debug_level >= 100) print *,' PAST OPEN EMISSIONS WRF file '

   CALL calc_current_date ( grid%id , 0. )
   CALL geth_newdate ( current_date_char, current_date, 3600 )
   current_date = current_date_char // '.0000'

      if( stand_lon  == 0. ) then
         stand_lon = cen_lon
      endif
 
      if( moad_cen_lat  == 0. ) then
         moad_cen_lat = cen_lat
      endif

      print *,map_proj,cen_lat,cen_lon,truelat1,truelat2
      print *,gmt, julyr,julday,iswater


!  CALL output_aux_model_input5 ( id1 , grid , config_flags , ierr )

   ! write global atributes into wrf emissions file

! grid%map_proj = map_proj
! grid%cen_lat = cen_lat
! grid%cen_lon = cen_lon
  config_flags%map_proj = map_proj
  config_flags%cen_lat = cen_lat
  config_flags%cen_lon = cen_lon
  config_flags%stand_lon = stand_lon
  config_flags%truelat1 = truelat1
  config_flags%truelat2 = truelat2
  config_flags%gmt = gmt
  config_flags%julyr = julyr
  config_flags%julday = julday
  config_flags%iswater = iswater
  config_flags%isice = isice
  config_flags%isurban = isurban
  config_flags%isoilwater = isoilwater
  config_flags%moad_cen_lat = moad_cen_lat
! config_flags%mminlu = TRIM(chlanduse) 

  print *,' map_proj ', grid%map_proj, map_proj, config_flags%map_proj
  print *,' cen_lat  ', grid%cen_lat , cen_lat, config_flags%cen_lat
  print *,' cen_lon  ', grid%cen_lon , cen_lon, config_flags%cen_lon

  CALL output_aux_model_input5 ( id1 , grid , config_flags, ierr )


!   idum1 = 1
!   call wrf_put_dom_ti_char    ( id1 , 'START_DATE' ,TRIM(start_date_char) , ierr )

!    ibuf(1) = map_proj
!    CALL wrf_put_dom_ti_integer ( id1 , 'MAP_PROJ'        , ibuf    , 1 , ierr )
!    rbuf(1) = moad_cen_lat
!    CALL wrf_put_dom_ti_real    ( id1 , 'MOAD_CEN_LAT'    , rbuf    , 1 , ierr )
!    rbuf(1) = cen_lat
!    CALL wrf_put_dom_ti_real    ( id1 , 'CEN_LAT'         , rbuf    , 1 , ierr )
!    rbuf(1) = cen_lon
!    CALL wrf_put_dom_ti_real    ( id1 , 'CEN_LON'         , rbuf    , 1 , ierr )
!    rbuf(1) = stand_lon
!    CALL wrf_put_dom_ti_real    ( id1 , 'STAND_LON'       , rbuf    , 1 , ierr )
!    rbuf(1) = truelat1
!    CALL wrf_put_dom_ti_real    ( id1 , 'TRUELAT1'        , rbuf    , 1 , ierr )
!    rbuf(1) = truelat2
!    CALL wrf_put_dom_ti_real    ( id1 , 'TRUELAT2'        , rbuf    , 1 , ierr )
!    rbuf(1) = gmt
!    CALL wrf_put_dom_ti_real    ( id1 , 'GMT'             , rbuf    , 1 , ierr )
!    ibuf(1) = julyr
!    CALL wrf_put_dom_ti_integer ( id1 , 'JULYR'           , ibuf    , 1 , ierr )
!    ibuf(1) = julday
!    CALL wrf_put_dom_ti_integer ( id1 , 'JULDAY'          , ibuf    , 1 , ierr )
! !   ibuf(1) = chem_opt
! !   CALL wrf_put_dom_ti_integer ( id1 , 'CHEM_OPT'        , ibuf    , 1 , ierr )
!    ibuf(1) = iswater
!    CALL wrf_put_dom_ti_integer ( id1 , 'ISWATER'         , ibuf    , 1 , ierr )
!    ibuf(1) = isice
!    CALL wrf_put_dom_ti_integer ( id1 , 'ISICE  '         , ibuf    , 1 , ierr )
!    ibuf(1) = isurban
!    CALL wrf_put_dom_ti_integer ( id1 , 'ISURBAN'         , ibuf    , 1 , ierr )
!    ibuf(1) = isoilwater
!    CALL wrf_put_dom_ti_integer ( id1 , 'ISOILWATER'      , ibuf    , 1 , ierr )
!
!    CALL wrf_put_dom_ti_char    ( id1 , 'MMINLU'          , TRIM(chlanduse)   , ierr )

! print *,' map_proj ', grid%map_proj, map_proj, config_flags_in%map_proj
! print *,' cen_lat  ', grid%cen_lat , cen_lat, config_flags_in%cen_lat
! print *,' cen_lon  ', grid%cen_lon , cen_lon, config_flags_in%cen_lon

    current_date_char = start_date_char
    current_date = current_date_char

   nemi_frames = time_loop
   if( debug_level >= -100) print *,'NEMI_FRAMES ', nemi_frames,time_loop

   DO emi_frame = 2,nemi_frames
!  DO emi_frame = 2,1
     if( debug_level >= 100) print *,'emi_frame ', emi_frame
     CALL domain_clock_get ( grid, current_timestr=message )
     if( debug_level >= 100) print *,' Current time ',Trim(message)

     current_date_char = current_date(1:19)
     CALL geth_newdate ( next_date_char, current_date_char, int(interval_seconds) )
     current_date = next_date_char // '.0000'

     print *,' Date &  time ',Trim(current_date)

     CALL domain_clockadvance( grid )

     if( debug_level >= 100) print *,' Read emissions ',emi_frame
     !CALL med_conv_emissions ( grid, config_flags, emi_frame-1, nemi_frames-1) 
     ihour = mod(ihour + 1,24)
     CALL med_read_bin_chem_emiss ( grid , config_flags, ihour, nemi_frames-1 )

   ! write global atributes into wrf emissions file

     if( debug_level >= 100) print *,' Output emissions '
     CALL output_aux_model_input5 ( id1 , grid , config_flags , ierr )

!   idum1 = 1
!    CALL wrf_put_dom_ti_char    ( id1 , 'START_DATE' ,TRIM(start_date_char) , ierr )
!    CALL wrf_put_dom_ti_integer ( id1 , 'MAP_PROJ'        , map_proj    , 1 , ierr )
!    CALL wrf_put_dom_ti_real    ( id1 , 'MOAD_CEN_LAT'    , moad_cen_lat, 1 , ierr )
!    CALL wrf_put_dom_ti_real    ( id1 , 'CEN_LAT'         , cen_lat     , 1 , ierr )
!    CALL wrf_put_dom_ti_real    ( id1 , 'CEN_LON'         , cen_lon     , 1 , ierr )
!    CALL wrf_put_dom_ti_real    ( id1 , 'STAND_LON'       , stand_lon   , 1 , ierr )
!    CALL wrf_put_dom_ti_real    ( id1 , 'TRUELAT1'        , truelat1    , 1 , ierr )
!    CALL wrf_put_dom_ti_real    ( id1 , 'TRUELAT2'        , truelat2    , 1 , ierr )
!    CALL wrf_put_dom_ti_real    ( id1 , 'GMT'             , gmt         , 1 , ierr )
!    CALL wrf_put_dom_ti_integer ( id1 , 'MAP_PROJ'        , map_proj    , 1 , ierr )
!    CALL wrf_put_dom_ti_integer ( id1 , 'JULYR'           , julyr       , 1 , ierr )
!    CALL wrf_put_dom_ti_integer ( id1 , 'JULDAY'          , julday      , 1 , ierr )
!    CALL wrf_put_dom_ti_integer ( id1 , 'ISWATER'         , iswater     , 1 , ierr )
!    CALL wrf_put_dom_ti_integer ( id1 , 'ISICE  '         , isice       , 1 , ierr )
!    CALL wrf_put_dom_ti_integer ( id1 , 'ISURBAN'         , isurban     , 1 , ierr )
!    CALL wrf_put_dom_ti_integer ( id1 , 'ISOILWATER'      , isoilwater  , 1 , ierr )
!    CALL wrf_put_dom_ti_char    ( id1 , 'MMINLU'          , TRIM(chlanduse)   , ierr )


! grid%map_proj = map_proj
! grid%cen_lat = cen_lat
! grid%cen_lon = cen_lon
  print *,' map_proj ', config_flags%map_proj, map_proj
  print *,' cen_lat  ', config_flags%cen_lat , cen_lat
  print *,' cen_lon  ', config_flags%cen_lon , cen_lon

   END DO

   CALL close_dataset ( id1 , config_flags , "DATASET=AUXOUTPUT5" )

    print *,' EMISSIONS: end of program '

    CALL wrf_shutdown
    CALL WRFU_Finalize( rc=rc )

!#ifdef DM_PARALLEL
!   CALL wrf_dm_shutdown
!#endif
   STOP

END PROGRAM convert_emiss
