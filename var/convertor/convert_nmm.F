! This is a program that converts NMM data into WRF input data.
! No boundary data yet.
!
PROGRAM convert_nmm

   USE module_machine
   USE module_domain
   USE module_io_domain
   USE module_driver_constants
   USE module_bc
   USE module_configure
   USE module_timing
   USE module_utility
#ifdef DM_PARALLEL
   USE module_dm
#endif

   IMPLICIT NONE

   TYPE(WRFU_TimeInterval) :: time_interval

   INTERFACE
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping
   END INTERFACE

   REAL    :: time , bdyfrq

   INTEGER :: loop , levels_to_process
   INTEGER :: rc


   TYPE(domain) , POINTER      :: keep_grid, grid_ptr, null_domain, grid
   TYPE (grid_config_rec_type) :: config_flags
   INTEGER                     :: number_at_same_level

   INTEGER :: max_dom, domain_id
   INTEGER :: id1 , id , ierr
   INTEGER :: idum1, idum2 
#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* CONFIG_BUF_LEN
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif

   REAL    :: dt_from_file, tstart_from_file, tend_from_file
   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: i , j , k , idts, ntsd, bdy_frame, nbdy_frames
   INTEGER :: debug_level

   CHARACTER (LEN=80)     :: message

   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: ubdy3dtemp1 , vbdy3dtemp1 , tbdy3dtemp1 , pbdy3dtemp1 , qbdy3dtemp1
   REAL , DIMENSION(:,:  ) , ALLOCATABLE :: mbdy2dtemp1
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: ubdy3dtemp2 , vbdy3dtemp2 , tbdy3dtemp2 , pbdy3dtemp2 , qbdy3dtemp2
   REAL , DIMENSION(:,:  ) , ALLOCATABLE :: mbdy2dtemp2

   CHARACTER(LEN=24) :: previous_date , this_date , next_date
   CHARACTER(LEN=19) :: start_date_char , end_date_char , current_date_char , next_date_char
   CHARACTER(LEN= 4) :: loop_char

   INTEGER :: start_year , start_month , start_day , start_hour , start_minute , start_second
   INTEGER ::   end_year ,   end_month ,   end_day ,   end_hour ,   end_minute ,   end_second
   INTEGER :: interval_seconds , real_data_init_type
   INTEGER :: time_loop_max , time_loop

   CHARACTER (LEN=80) :: inpname , bdyname

! these are needed on some compilers, eg compaq/alpha, to
! permit pass by reference through the registry generated
! interface to med_read_nmm, below
#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

   !  Get the NAMELIST data for input.

   !  Define the name of this program (program_name defined in module_domain)

   program_name = "REAL_EM V1.2 PREPROCESSOR"

#ifdef DM_PARALLEL
   CALL disable_quilting
#endif

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

   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   CALL  wrf_message ( program_name )

   !  An available simple timer from the timing module.

   NULLIFY( null_domain )
   CALL alloc_and_configure_domain ( domain_id  = 1           , &
                                     grid       = head_grid   , &
                                     parent     = null_domain , &
                                     kid        = -1            )

   grid => head_grid
   CALL set_scalar_indices_from_config ( grid%id , idum1, idum2 )

   CALL Setup_Timekeeping ( grid )
   CALL WRFU_TimeIntervalSet ( time_interval , S=model_config_rec%interval_seconds, rc=rc )
   CALL WRFU_ClockSet ( grid%domain_clock , timeStep=time_interval , rc=rc )
   CALL WRFU_ClockGet ( grid%domain_clock, currTime=grid%current_time , rc=rc )
   CALL wrf_timetoa ( grid%current_time, message )
   write(0,*)Trim(message)

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

   interval_seconds    = model_config_rec%interval_seconds
   real_data_init_type = model_config_rec%real_data_init_type

   WRITE ( start_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
           start_year,start_month,start_day,start_hour,start_minute,start_second
   WRITE (   end_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
             end_year,  end_month,  end_day,  end_hour,  end_minute,  end_second

   !  Override stop time with value computed above.
   CALL wrf_atotime( end_date_char, grid%stop_time )
   CALL WRFU_ClockSet ( grid%domain_clock , StopTime=grid%stop_time, rc=rc )
   CALL wrf_check_error( WRFU_SUCCESS, rc, &
                         'WRFU_ClockSet(grid%domain_clock) FAILED', &
                         __FILE__ , &
                         __LINE__  )

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

   !  Here we define the initial time to process, for later use by the code.

   current_date_char = start_date_char
   start_date = start_date_char // '.0000'
   current_date = start_date
   bdyfrq = interval_seconds
   CALL nl_set_bdyfrq ( grid%id , bdyfrq )
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )


! these are needed on some compilers, eg compaq/alpha, to
! permit pass by reference through the registry generated
! interface to med_read_nmm, below
#include "deref_kludge.h"

   ntsd = 0 
   CALL med_read_nmm ( grid, config_flags, ntsd, dt_from_file, tstart_from_file, tend_from_file, &
!
#include "nmm_actual_args.inc"
!
                     )

   CALL init_wrfio

   grid%input_from_file = .false.

   CALL construct_filename1( inpname , 'wrfinput' , grid%id , 2 )
   CALL open_w_dataset ( id1, TRIM(inpname) , grid , config_flags , output_model_input , "DATASET=INPUT", ierr )
   IF ( ierr .NE. 0 ) THEN
     CALL wrf_error_fatal( 'real: error opening wrfinput for writing' )
   ENDIF

!   CALL calc_current_date ( grid%id , 0. )

   CALL geth_newdate ( current_date_char, current_date, 3600 )
   current_date = current_date_char // '.0000'
   CALL output_model_input ( id1, grid , config_flags , ierr )

   CALL close_dataset ( id1 , config_flags , "DATASET=INPUT" )

   current_date = current_date_char
   current_date = current_date_char // '.0000'

   CALL construct_filename1( bdyname , 'wrfbdy' , grid%id , 2 )
   CALL open_w_dataset ( id, TRIM(bdyname) , grid , config_flags , output_boundary , "DATASET=BOUNDARY", ierr )

   nbdy_frames = NINT(( tend_from_file - tstart_from_file) )
   DO bdy_frame = 2,nbdy_frames

     write(0,*)'BDY FRAME ', BDY_FRAME
     write(*,*)'BDY FRAME ', BDY_FRAME
     CALL wrf_timetoa ( grid%current_time, message )
     write(0,*)Trim(message)
     bdyfrq = interval_seconds
     CALL nl_set_bdyfrq ( grid%id , bdyfrq )

     IF ( ierr .NE. 0 ) THEN
       CALL wrf_error_fatal( 'real: error opening wrfbdy for writing' )
     ENDIF

     CALL WRFU_ClockGet ( grid%domain_clock, currTime=grid%current_time , rc=rc )

     current_date_char = current_date(1:19)
     CALL geth_newdate ( next_date_char, current_date_char, 3600 )
     current_date = next_date_char // '.0000'

     CALL output_boundary ( id, grid , config_flags , ierr )
     IF ( .NOT. WRFU_ClockIsStopTime(grid%domain_clock ,rc=rc) ) THEN
        CALL WRFU_ClockAdvance( grid%domain_clock, rc=rc )
     ENDIF

     ntsd = (bdy_frame-1)*(3600./dt_from_file)
     CALL med_read_nmm_bdy ( grid, config_flags, ntsd, dt_from_file, tstart_from_file, tend_from_file, &
!
#include "nmm_actual_args.inc"
!
                     )
   END DO

   CALL close_dataset ( id , config_flags , "DATASET=BOUNDARY" )



   CALL wrf_shutdown

   CALL WRFU_Finalize( rc=rc )

END PROGRAM convert_nmm
