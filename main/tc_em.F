!  Create an initial data set for the WRF model based on real data.  This
!  program is specifically set up for the Eulerian, mass-based coordinate.
PROGRAM tc_data
   USE module_machine
   USE module_domain, ONLY : domain, alloc_and_configure_domain, &
        domain_clock_set, head_grid, program_name, domain_clockprint, set_current_grid_ptr
   USE module_io_domain
   USE module_initialize_real, ONLY : wrfu_initialize
   
   USE module_driver_constants
   USE module_configure, ONLY : grid_config_rec_type, model_config_rec, &
        initial_config, get_config_as_buffer, set_config_as_buffer
   USE module_timing
   USE module_state_description, ONLY : realonly
   USE module_symbols_util, ONLY: wrfu_cal_gregorian
   USE module_utility, ONLY : WRFU_finalize

   IMPLICIT NONE


   REAL    :: time , bdyfrq

   INTEGER :: loop , levels_to_process , debug_level


   TYPE(domain) , POINTER :: null_domain
   TYPE(domain) , POINTER :: grid , another_grid
   TYPE(domain) , POINTER :: grid_ptr , grid_ptr2
   TYPE (grid_config_rec_type)              :: config_flags
   INTEGER                :: number_at_same_level

   INTEGER :: max_dom, domain_id , grid_id , parent_id , parent_id1 , id
   INTEGER :: e_we , e_sn , i_parent_start , j_parent_start
   INTEGER :: idum1, idum2 
#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* CONFIG_BUF_LEN
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif
   LOGICAL found_the_id

   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: ips , ipe , jps , jpe , kps , kpe
   INTEGER :: ijds , ijde , spec_bdy_width
   INTEGER :: i , j , k , idts, rc
   INTEGER :: sibling_count , parent_id_hold , dom_loop

   CHARACTER (LEN=80)     :: message

   INTEGER :: start_year , start_month , start_day , start_hour , start_minute , start_second
   INTEGER ::   end_year ,   end_month ,   end_day ,   end_hour ,   end_minute ,   end_second
   INTEGER :: interval_seconds , real_data_init_type
   INTEGER :: time_loop_max , time_loop, bogus_id, storm
   real::t1,t2
   real    :: latc_loc(max_bogus),lonc_loc(max_bogus),vmax(max_bogus),rmax(max_bogus)
   INTERFACE
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain, ONLY : domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping
   END INTERFACE

#include "version_decl"

   !  Define the name of this program (program_name defined in module_domain)

   program_name = "TC_EM " // TRIM(release_version) // " PREPROCESSOR"

!  The TC bogus algorithm assumes that the user defines a central point, and then
!  allows the program to remove a typhoon based on a distance in km.  This is
!  implemented on a single processor only.

#ifdef DM_PARALLEL
   IF ( .NOT. wrf_dm_on_monitor() ) THEN
      CALL wrf_error_fatal( 'TC bogus must run with a single processor only, re-run with num procs set to 1' )
   END IF
#endif

#ifdef DM_PARALLEL
   CALL disable_quilting
#endif

   !  Initialize the modules used by the WRF system.  Many of the CALLs made from the
   !  init_modules routine are NO-OPs.  Typical initializations are: the size of a
   !  REAL, setting the file handles to a pre-use value, defining moisture and
   !  chemistry indices, etc.

   CALL       wrf_debug ( 100 , 'real_em: calling init_modules ' )
   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)
   CALL WRFU_Initialize( defaultCalendar=WRFU_CAL_GREGORIAN, rc=rc )
   CALL init_modules(2)   ! Phase 2 resumes after MPI_INIT() (if it is called)

   !  The configuration switches mostly come from the NAMELIST input.

#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
      CALL initial_config
   END IF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
!   CALL wrf_dm_initialize
#else
   CALL initial_config
#endif


   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   CALL  wrf_message ( program_name )

   !  There are variables in the Registry that are only required for the real
   !  program, fields that come from the WPS package.  We define the run-time
   !  flag that says to allocate space for these input-from-WPS-only arrays.

   CALL nl_set_use_wps_input ( 1 , REALONLY )

   !  Allocate the space for the mother of all domains.

   NULLIFY( null_domain )
   CALL       wrf_debug ( 100 , 'real_em: calling alloc_and_configure_domain ' )
   CALL alloc_and_configure_domain ( domain_id  = 1           , &
                                     grid       = head_grid   , &
                                     parent     = null_domain , &
                                     kid        = -1            )

   grid => head_grid
   CALL nl_get_max_dom ( 1 , max_dom )

   IF ( model_config_rec%interval_seconds .LE. 0 ) THEN
     CALL wrf_error_fatal( 'namelist value for interval_seconds must be > 0')
   END IF

   all_domains : DO domain_id = 1 , max_dom

      IF ( ( model_config_rec%input_from_file(domain_id) ) .OR. &
           ( domain_id .EQ. 1 ) ) THEN

         CALL Setup_Timekeeping ( grid )
         CALL set_current_grid_ptr( grid )
         CALL domain_clockprint ( 150, grid, &
                'DEBUG real:  clock after Setup_Timekeeping,' )
         CALL domain_clock_set( grid, &
                                time_step_seconds=model_config_rec%interval_seconds )
         CALL domain_clockprint ( 150, grid, &
                'DEBUG real:  clock after timeStep set,' )


         CALL       wrf_debug ( 100 , 'tc_em: calling set_scalar_indices_from_config ' )
         CALL set_scalar_indices_from_config ( grid%id , idum1, idum2 )

!This is goofy but we need to loop through the number of storms to get 
!the namelist variables for the tc_bogus.  But then we need to 
!call model_to_grid_config_rec with the grid%id = to 1 in order to
!reset to the correct information.
         CALL       wrf_debug ( 100 , 'tc_em: calling model_to_grid_config_rec ' )
         lonc_loc(:) = -999.
         latc_loc(:) = -999.
         vmax(:)     = -999.
         rmax(:)     = -999.
         CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
         lonc_loc(1) = config_flags%lonc_loc
         latc_loc(1) = config_flags%latc_loc
         vmax(1)     = config_flags%vmax_meters_per_second
         rmax(1)     = config_flags%rmax
!         print *,lonc_loc(1),latc_loc(1),vmax(1),rmax(1)
         do storm = 2,config_flags%num_storm
             bogus_id = storm
             CALL model_to_grid_config_rec ( bogus_id , model_config_rec , config_flags )
             lonc_loc(storm) = config_flags%lonc_loc
             latc_loc(storm) = config_flags%latc_loc
             vmax(storm)     = config_flags%vmax_meters_per_second
             rmax(storm)     = config_flags%rmax
!             print *,"in loop ",storm,lonc_loc(storm),latc_loc(storm),vmax(storm),rmax(storm)
         end do
         CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

         !  Initialize the WRF IO: open files, init file handles, etc.

         CALL       wrf_debug ( 100 , 'tc_em: calling init_wrfio' )
         CALL init_wrfio

         !  Some of the configuration values may have been modified from the initial READ
         !  of the NAMELIST, so we re-broadcast the configuration records.

#ifdef DM_PARALLEL
         CALL       wrf_debug ( 100 , 'tc_em: re-broadcast the configuration records' )
         CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
         CALL wrf_dm_bcast_bytes( configbuf, nbytes )
         CALL set_config_as_buffer( configbuf, configbuflen )
#endif

         !   No looping in this layer.  

         CALL       wrf_debug ( 100 , 'calling tc_med_sidata_input' )
         CALL tc_med_sidata_input ( grid , config_flags, latc_loc, lonc_loc,vmax,rmax )
         CALL       wrf_debug ( 100 , 'backfrom tc_med_sidata_input' )

      ELSE 
         CYCLE all_domains
      END IF

   END DO all_domains

   CALL set_current_grid_ptr( head_grid )

   !  We are done.

   CALL       wrf_debug (   0 , 'tc_em: SUCCESS COMPLETE TC BOGUS' )

   CALL wrf_shutdown

   CALL WRFU_Finalize( rc=rc )


END PROGRAM tc_data


!-----------------------------------------------------------------
SUBROUTINE tc_med_sidata_input ( grid , config_flags, latc_loc, lonc_loc,vmax, rmax )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE module_optional_input

   USE module_date_time
   USE module_utility

   IMPLICIT NONE


  ! Interface 
   INTERFACE
     SUBROUTINE start_domain ( grid , allowed_to_read )  ! comes from module_start in appropriate dyn_ directory
       USE module_domain
       TYPE (domain) grid
       LOGICAL, INTENT(IN) :: allowed_to_read
     END SUBROUTINE start_domain
   END INTERFACE

  ! Arguments
   TYPE(domain)                :: grid
   TYPE (grid_config_rec_type) :: config_flags
  ! Local
   INTEGER                :: time_step_begin_restart
   INTEGER                :: idsi , ierr , myproc, internal_time_loop,iflag
   CHARACTER (LEN=80)      :: si_inpname
   CHARACTER (LEN=80)      :: message

   CHARACTER(LEN=19) :: start_date_char , end_date_char , current_date_char , next_date_char
   CHARACTER(LEN=8)  :: flag_name

   INTEGER :: time_loop_max , loop, rc,icnt,itmp
   INTEGER :: julyr , julday
   REAL    :: gmt
   real    :: t1,t2,t3,t4
   real    :: latc_loc(max_bogus), lonc_loc(max_bogus)
   real    :: vmax(max_bogus),rmax(max_bogus)

   grid%input_from_file = .true.
   grid%input_from_file = .false.

   CALL tc_compute_si_start ( model_config_rec%start_year  (grid%id) , &
                                   model_config_rec%start_month (grid%id) , &
                                   model_config_rec%start_day   (grid%id) , &
                                   model_config_rec%start_hour  (grid%id) , &
                                   model_config_rec%start_minute(grid%id) , &
                                   model_config_rec%start_second(grid%id) , &
                                   model_config_rec%interval_seconds      , &
                                   model_config_rec%real_data_init_type   , &
                                   start_date_char)

   end_date_char = start_date_char
   IF ( end_date_char .LT. start_date_char ) THEN
      CALL wrf_error_fatal( 'Ending date in namelist ' // end_date_char // ' prior to beginning date ' // start_date_char )
   END IF
   print *,"the start date char ",start_date_char
   print *,"the end date char ",end_date_char

   time_loop_max = 1
   !  Override stop time with value computed above.  
   CALL domain_clock_set( grid, stop_timestr=end_date_char )

   ! TBH:  for now, turn off stop time and let it run data-driven
   CALL WRFU_ClockStopTimeDisable( grid%domain_clock, rc=rc ) 
   CALL wrf_check_error( WRFU_SUCCESS, rc, &
                         'WRFU_ClockStopTimeDisable(grid%domain_clock) FAILED', &
                         __FILE__ , &
                         __LINE__  )
   CALL domain_clockprint ( 150, grid, &
          'DEBUG med_sidata_input:  clock after stopTime set,' )

   !  Here we define the initial time to process, for later use by the code.
   
   current_date_char = start_date_char
   start_date = start_date_char // '.0000'
   current_date = start_date

   CALL nl_set_bdyfrq ( grid%id , REAL(model_config_rec%interval_seconds) )


   CALL cpu_time ( t1 )
   DO loop = 1 , time_loop_max

      internal_time_loop = loop
      IF ( ( grid%id .GT. 1 ) .AND. ( loop .GT. 1 ) .AND. &
           ( model_config_rec%grid_fdda(grid%id) .EQ. 0 ) .AND. &
           ( model_config_rec%sst_update .EQ. 0 ) ) EXIT

      print *,' '
      print *,'-----------------------------------------------------------------------------'
      print *,' '
      print '(A,I2,A,A,A,I4,A,I4)' , &
      ' Domain ',grid%id,': Current date being processed: ',current_date, ', which is loop #',loop,' out of ',time_loop_max

      !  After current_date has been set, fill in the julgmt stuff.

      CALL geth_julgmt ( config_flags%julyr , config_flags%julday , config_flags%gmt )

        print *,'configflags%julyr, %julday, %gmt:',config_flags%julyr, config_flags%julday, config_flags%gmt
      !  Now that the specific Julian info is available, save these in the model config record.

      CALL nl_set_gmt (grid%id, config_flags%gmt)
      CALL nl_set_julyr (grid%id, config_flags%julyr)
      CALL nl_set_julday (grid%id, config_flags%julday)

      !  Open the input file for tc stuff.  Either the "new" one or the "old" one.  The "new" one could have
      !  a suffix for the type of the data format.  Check to see if either is around.

      CALL cpu_time ( t3 )
      WRITE ( wrf_err_message , FMT='(A,A)' )'med_sidata_input: calling open_r_dataset for ', &
                                             TRIM(config_flags%auxinput1_inname)
      CALL wrf_debug ( 100 , wrf_err_message )
      IF ( config_flags%auxinput1_inname(1:8) .NE. 'wrf_real' ) THEN
         CALL construct_filename4a( si_inpname , config_flags%auxinput1_inname , grid%id , 2 , &
                                    current_date_char , config_flags%io_form_auxinput1 )
      ELSE
         CALL construct_filename2a( si_inpname , config_flags%auxinput1_inname , grid%id , 2 , &
                                    current_date_char )
      END IF
      CALL open_r_dataset ( idsi, TRIM(si_inpname) , grid , config_flags , "DATASET=AUXINPUT1", ierr )
      IF ( ierr .NE. 0 ) THEN
         CALL wrf_error_fatal( 'error opening ' // TRIM(si_inpname) // &
                               ' for input; bad date in namelist or file not in directory' )
      END IF

      !  Input data.

      CALL wrf_debug ( 100 , 'med_sidata_input: calling input_auxinput1' )
      CALL input_auxinput1 ( idsi ,   grid , config_flags , ierr )
      WRITE ( wrf_err_message , FMT='(A,I10,A)' ) 'Timing for input ',NINT(t4-t3) ,' s.'
      CALL wrf_debug( 0, wrf_err_message )

      !  Possible optional SI input.  This sets flags used by init_domain.

      CALL cpu_time ( t3 )
      CALL       wrf_debug ( 100 , 'med_sidata_input: calling init_module_optional_input' )
      CALL init_module_optional_input ( grid , config_flags )
      CALL       wrf_debug ( 100 , 'med_sidata_input: calling optional_input' )
      CALL optional_input ( grid , idsi , config_flags )

!Here we check the flags yet again.  The flags are checked in optional_input but 
!the grid% flags are not set.
      flag_name(1:8) = 'SM000010'
      CALL wrf_get_dom_ti_integer ( idsi, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
          grid%flag_sm000010 = 1
      end if

       flag_name(1:8) = 'SM010040'
       CALL wrf_get_dom_ti_integer ( idsi, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
       IF ( ierr .EQ. 0 ) THEN
          grid%flag_sm010040 = 1
       end if

       flag_name(1:8) = 'SM040100'
       CALL wrf_get_dom_ti_integer ( idsi, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
       IF ( ierr .EQ. 0 ) THEN
            grid%flag_sm040100 = itmp   
       end if


       flag_name(1:8) = 'SM100200'
       CALL wrf_get_dom_ti_integer ( idsi, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
       IF ( ierr .EQ. 0 ) THEN
            grid%flag_sm100200 = itmp  
       end if

!       flag_name(1:8) = 'SM010200'
!       CALL wrf_get_dom_ti_integer ( idsi, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
!       IF ( ierr .EQ. 0 ) THEN
!            config_flags%flag_sm010200 = itmp 
!            print *,"found the flag_sm010200 "         
!       end if

!Now the soil temperature flags
        flag_name(1:8) = 'ST000010'
        CALL wrf_get_dom_ti_integer ( idsi, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
        IF ( ierr .EQ. 0 ) THEN
            grid%flag_st000010 = 1
        END IF


         flag_name(1:8) = 'ST010040'
         CALL wrf_get_dom_ti_integer ( idsi, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            grid%flag_st010040 = 1
         END IF

         flag_name(1:8) = 'ST040100'
         CALL wrf_get_dom_ti_integer ( idsi, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            grid%flag_st040100 = 1
         END IF


         flag_name(1:8) = 'ST100200'
         CALL wrf_get_dom_ti_integer ( idsi, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            grid%flag_st100200 = 1
         END IF



      CALL close_dataset ( idsi , config_flags , "DATASET=AUXINPUT1" )
      CALL cpu_time ( t4 )
      print *,"the out name ",config_flags%auxinput1_outname

      !  Possible optional SI input.  This sets flags used by init_domain.
      !  We need to call the optional input routines to get the flags that 
      !  are in the metgrid output file so they can be put in the tc bogus 
      !  output file for real to read.
      CALL cpu_time ( t3 )
      already_been_here = .FALSE.
      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )


      CALL cpu_time ( t3 )

!      print *,"before assemble_output ",vmax,rmax
      CALL assemble_output ( grid , config_flags , loop , time_loop_max, current_date_char, &
                             latc_loc, lonc_loc, vmax, rmax )
      CALL cpu_time ( t4 )
      WRITE ( wrf_err_message , FMT='(A,I10,A)' ) 'Timing for output ',NINT(t4-t3) ,' s.'
      CALL wrf_debug( 0, wrf_err_message )
      CALL cpu_time ( t2 )
      WRITE ( wrf_err_message , FMT='(A,I4,A,I10,A)' ) 'Timing for loop # ',loop,' = ',NINT(t2-t1) ,' s.'
      CALL wrf_debug( 0, wrf_err_message )

      CALL cpu_time ( t1 )
   END DO

END SUBROUTINE tc_med_sidata_input


!-------------------------------------------------------------------------------------
SUBROUTINE tc_compute_si_start(  &
   start_year , start_month , start_day , start_hour , start_minute , start_second , &
   interval_seconds , real_data_init_type , &
   start_date_char)

   USE module_date_time

   IMPLICIT NONE

   INTEGER :: start_year , start_month , start_day , start_hour , start_minute , start_second
   INTEGER ::   end_year ,   end_month ,   end_day ,   end_hour ,   end_minute ,   end_second
   INTEGER :: interval_seconds , real_data_init_type
   INTEGER :: time_loop_max , time_loop

   CHARACTER(LEN=19) :: current_date_char , start_date_char , end_date_char , next_date_char

#ifdef PLANET
   WRITE ( start_date_char , FMT = '(I4.4,"-",I5.5,"_",I2.2,":",I2.2,":",I2.2)' ) &
           start_year,start_day,start_hour,start_minute,start_second
#else
   WRITE ( start_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
           start_year,start_month,start_day,start_hour,start_minute,start_second
#endif


END SUBROUTINE tc_compute_si_start

!-----------------------------------------------------------------------
SUBROUTINE assemble_output ( grid , config_flags , loop , time_loop_max,current_date_char, &
                             latc_loc, lonc_loc,vmax,rmax)

   USE module_big_step_utilities_em
   USE module_domain
   USE module_io_domain
   USE module_configure
   USE module_date_time
   USE module_bc
   IMPLICIT NONE

   TYPE(domain)                 :: grid
   TYPE (grid_config_rec_type)  :: config_flags

   INTEGER , INTENT(IN)         :: loop , time_loop_max

!These values are in the name list and are avaiable from
!from the config_flags.
   real    :: vmax(max_bogus),vmax_ratio
   real    :: rmax(max_bogus),stand_lon,cen_lat,ptop_in_pa
   real    :: latc_loc(max_bogus),lonc_loc(max_bogus)

   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: ips , ipe , jps , jpe , kps , kpe
   INTEGER :: ijds , ijde , spec_bdy_width
   INTEGER :: i , j , k , idts,map_proj,remove_only,storms

   INTEGER :: id1 , interval_seconds , ierr, rc, sst_update, grid_fdda
   INTEGER , SAVE :: id, id2,  id4 
   CHARACTER (LEN=80) :: tcoutname , bdyname
   CHARACTER(LEN= 4) :: loop_char
   CHARACTER(LEN=19) ::  current_date_char
   
character *19 :: temp19
character *24 :: temp24 , temp24b

real::t1,t2,truelat1,truelat2

   !  Various sizes that we need to be concerned about.

   ids = grid%sd31
   ide = grid%ed31
   kds = grid%sd32
   kde = grid%ed32
   jds = grid%sd33
   jde = grid%ed33

   ims = grid%sm31
   ime = grid%em31
   kms = grid%sm32
   kme = grid%em32
   jms = grid%sm33
   jme = grid%em33

   ips = grid%sp31
   ipe = grid%ep31
   kps = grid%sp32
   kpe = grid%ep32
   jps = grid%sp33
   jpe = grid%ep33

   ijds = MIN ( ids , jds )
   ijde = MAX ( ide , jde )

   !  Boundary width, scalar value.

   spec_bdy_width = model_config_rec%spec_bdy_width
   interval_seconds = model_config_rec%interval_seconds
   sst_update = model_config_rec%sst_update
   grid_fdda = model_config_rec%grid_fdda(grid%id)
   truelat1  = config_flags%truelat1
   truelat2  = config_flags%truelat2

   stand_lon = config_flags%stand_lon
   cen_lat   = config_flags%cen_lat
   map_proj  = config_flags%map_proj

   vmax_ratio = config_flags%vmax_ratio
   ptop_in_pa = config_flags%p_top_requested
   remove_only = 0
   if(config_flags%remove_storm) then
      remove_only = 1
   end if

   storms = config_flags%num_storm
   call tc_bogus(cen_lat,stand_lon,map_proj,truelat1,truelat2, &
                 grid%dx,ipe,jpe,grid%num_metgrid_levels,ptop_in_pa, &
                 latc_loc,lonc_loc,vmax,vmax_ratio,rmax,remove_only,storms,grid)


!Notes jps is the starting y. Usally 1
!      jpe is the ending y position on the staggered grid north south or ny
!      ipe is the ending x postiion on the staggered grid east west of nx

   !  Open the tc bogused output file. cd 
   CALL construct_filename4a( tcoutname , config_flags%auxinput1_outname , grid%id , 2 , &
                                    current_date_char , config_flags%io_form_auxinput1 )

   print *,"outfile name from construct filename ",tcoutname
   CALL open_w_dataset ( id1, TRIM(tcoutname) , grid , config_flags ,output_auxinput1,"DATASET=AUXINPUT1",ierr )
   IF ( ierr .NE. 0 ) THEN
        CALL wrf_error_fatal( 'tc_em: error opening tc bogus file for writing' )
   END IF
   CALL output_auxinput1( id1, grid , config_flags , ierr )
   CALL close_dataset ( id1 , config_flags , "DATASET=AUXINPUT1" )


END SUBROUTINE assemble_output

!----------------------------------------------------------------------------------------------

SUBROUTINE tc_bogus(centerlat,stdlon,nproj,truelat1,truelat2,dsm,ew,ns,nz,ptop_in_pa, &
                    latc_loc,lonc_loc,vmax,vmax_ratio,rmax,remove_only,storms,grid)
      
!center   lat is the center latitude from the global attributes 
!stdlon   is the center longitude from the global attributes 
!nproj    is the map projection from the global attributes
!dsm      is the spacing in meters from the global attributes
!ew       is the west_east_stag from the dimensions. We want the full domain.
!ns       is the south_north_stag from the dimensions. We want the full domain.
!nz       is the number of metgrid levels from the dimensions
!latc_loc is the center latitude of the bogus strorm.  It comes from the namelist entry
!lonc_loc is the center longitude of the bogus strorm.  It comes from the namelist entry
!vmax     is the max vortex it comes from the namelist entry
!vmax_ratio  this comes from the namelist entry
!rmax        this comes from the namelist entry

!module_llxy resides in the share directory.
  USE module_llxy
!This is for the large structure (grid)
  USE module_domain



  IMPLICIT NONE 
  TYPE(domain)                 :: grid
  integer ew,ns,nz
  integer nproj
  integer storms,nstrm
  real :: centerlat,stdlon,conef,truelat1,truelat2,dsm,dx
  real :: latc_loc(max_bogus),lonc_loc(max_bogus),vmax(max_bogus),vmax_ratio,rmax(max_bogus)
  
  real :: press(ns,ew,nz),rhmx(nz), vwgt(nz)
  real, dimension(:,:,:) , allocatable :: u11,v11,t11,rh11,phi11
  real, dimension(:,:,:) , allocatable :: u1 , v1 , t1 , rh1 , phi1
  real, dimension(ns,ew) :: mfx,mfd,lond,terrain,cor,xlat,pslx
  real, dimension(ns-1,ew) :: mfsu
  real, dimension(ns,ew-1) :: mfsv


  CHARACTER*2  jproj
  LOGICAL :: l_tcbogus


  real :: r_search,r_vor,beta,devps,humidity_max
  real :: devpc,const,r_vor2,cnst,alphar,epsilon,vormx , rad , sum_q 
  real :: avg_q ,q_old,ror,q_new,sum_rh,avg_rh,rh_min,rhbkg,rhbog,dph,dphx0
  real :: rh_max,min_RH_value,r_ratio,ps
  integer :: vert_variation
  integer :: i,k,j,jx,ix,kx,remove_only
  integer :: k00,kfrm ,kto ,k85,n_iter,i_mvc,j_mvc,nct,itr
  integer :: strmci(nz), strmcj(nz)
  real :: disx,disy,alpha,degran,pie,rovcp,cp
  REAL :: rho,pprm,phip0,x0,y0,vmx,xico,xjco,xicn,xjcn,p85,xlo,rconst
  real :: ptop_in_pa 
  real :: latinc,loninc
  real :: rtemp,colat0,colat
  REAL :: q1(ns,ew,nz), psi1(ns,ew,nz) 

! This is the entire map projection enchilada.
  TYPE(proj_info) :: proj

  

  REAL :: lat1 , lon1
! These values are read in from the data set. 
   real :: knowni,knownj

!  TC bogus
   REAL utcr(ns,ew,nz),  vtcr(ns,ew,nz)
   REAL utcp(ns,ew,nz),  vtcp(ns,ew,nz)
   REAL psitc(ns,ew,nz), psiv(nz)
   REAL vortc(ns,ew,nz), vorv(nz)
   REAL tptc(ns,ew,nz),  rhtc(ns,ew,nz)
   REAL phiptc(ns,ew,nz)

!  Work arrays
   REAL uuwork(nz), vvwork(nz)
   REAL vort(ns,ew,nz), div(ns,ew,nz)
   REAL vortsv(ns,ew,nz)
   REAL theta(ns,ew,nz), t_reduce(ns,ew,nz)
   REAL ug(ns,ew,nz),   vg(ns,ew,nz),  vorg(ns,ew,nz)
   REAL uchi(ns,ew,nz), vchi(ns,ew,nz)
   REAL delpx(ns,ew)

!subroutines for relaxation
   REAL outold(ns,ew), outnew(ns,ew)
   REAL rd(ns,ew),     ff(ns,ew)
   REAL tmp1(ns,ew),   tmp2(ns,ew) 

!  Background fields.
   REAL , DIMENSION (ns,ew,nz) :: u0, v0, t0, t00, rh0, q0,       &
                                  phi0, psi0, chi


!  Perturbations
   REAL psipos(ns,ew,nz), upos(ns,ew,nz), vpos(ns,ew,nz),     &
        tpos(ns,ew,nz),   psi(ns,ew,nz),                      &
        phipos(ns,ew,nz), phip(ns,ew,nz)
      
!  Final fields.
   REAL u2(ns,ew,nz),  v2(ns,ew,nz),                          &
        t2(ns,ew,nz),                                         &
        z2(ns,ew,nz),  phi2(ns,ew,nz),                        &
        rh2(ns,ew,nz), q2(ns,ew,nz)
      
   print *,"the dimensions: north-south = ",ns," east-west =",ew
   IF (nproj .EQ. 1) THEN
        jproj = 'LC'
        print *,"Lambert Conformal projection"
   ELSE IF (nproj .EQ. 2) THEN
        jproj = 'ST'
   ELSE IF (nproj .EQ. 3) THEN
        jproj = 'ME'
        print *,"A mercator projection"
   END IF


  knowni = 1.
  knownj = 1.
  pie     = 3.141592653589793
  degran = pie/180.
  rconst = 287.04
  min_RH_value = 5.0
  cp = 1004.0
  rovcp = rconst/cp
   
   r_search = 400000.0
   r_vor = 300000.0
   r_vor2 = r_vor * 4
   beta = 0.5
   devpc= 40.0
   vert_variation = 1   
   humidity_max   = 95.0 
   alphar         = 1.8
   latinc        = -999.
   loninc        = -999.

  !  Set up initializations for map projection so that the lat/lon
  !  of the tropical storm can be put into model (i,j) space.  This needs to be done once per 
  !  map projection definition.  Since this is the domain that we are "GOING TO", it is a once
  !  per regridder requirement.  If the user somehow ends up calling this routine for several
  !  time periods, there is no problemos, just a bit of overhead with redundant calls.
   
   dx = dsm
   lat1 = grid%xlat_gc(1,1)
   lon1 = grid%xlong_gc(1,1)
   IF( jproj .EQ. 'ME' )THEN
       IF ( lon1  .LT. -180. ) lon1  = lon1  + 360.
       IF ( lon1  .GT.  180. ) lon1  = lon1  - 360.
       IF ( stdlon .LT. -180. ) stdlon = stdlon + 360.
       IF ( stdlon .GT.  180. ) stdlon = stdlon - 360.
       CALL map_set ( proj_merc, proj, lat1, lon1, lat1, lon1, knowni, knownj, dx, &
                      latinc,loninc,stdlon , truelat1 , truelat2)
       conef = 0.
   ELSE IF ( jproj .EQ. 'LC' ) THEN
        if((truelat1 .eq. 0.0)  .and. (truelat2 .eq. 0.0)) then
            print *,"Truelat1 and Truelat2 are both 0"
            stop
         end if
        CALL map_set (proj_lc,proj, lat1, lon1, lat1, lon1, knowni, knownj, dx, &
                       latinc,loninc,stdlon , truelat1 , truelat2)
       conef = proj%cone
   ELSE IF ( jproj .EQ. 'ST' ) THEN
        conef = 1.
        CALL map_set ( proj_ps,proj,lat1, lon1, lat1, lon1, knowni, knownj, dx, &
                      latinc,loninc,stdlon , truelat1 , truelat2)
   END IF


!  Press(level 1) = surface pressure.  Pressure is a three dimensional array in this case.
!  The second level is usually 1000 mb.  The last level, kx, is defined as ptop. These come in from 
!  the namelist.   
 kx = nz
 do i = 1,ns
    do j = 1,ew
       do k = 1,nz
           press(i,j,k) = grid%p_gc(j,k,i)*0.01
       end do
    end do
 end do


!  Initialize the vertical profiles for humidity and weighting.
!The ptop variable will be read in from the namelist
   IF ( ( ptop_in_pa .EQ. 40000. ) .OR. ( ptop_in_pa .EQ. 60000. ) ) THEN
         PRINT '(A)','Hold on pardner, your value for PTOP is gonna cause problems for the TC bogus option.'
         PRINT '(A)','Make it higher up than 400 mb.'
         STOP 'ptop_woes_for_tc_bogus'
   END IF

 IF ( vert_variation .EQ. 1 ) THEN
    DO k=1,kx
       IF ( press(1,1,k) .GT. 400. ) THEN
               rhmx(k) = humidity_max
       ELSE
               rhmx(k) = humidity_max * MAX( 0.1 , (press(1,1,k) - ptop_in_pa/100.)/(400.-ptop_in_pa/100.) )
       END IF

        IF ( press(1,1,k) .GT. 600. ) THEN
             vwgt(k) = 1.0
        ELSE IF ( press(1,1,k) .LE. 100. ) THEN
             vwgt(k) = 0.0001
        ELSE
             vwgt(k) = MAX ( 0.0001 , (press(1,1,k)-ptop_in_pa/100.)/(600.-ptop_in_pa/100.) )
        END IF
      END DO

 ELSE IF ( vert_variation .EQ. 2 ) THEN
         IF ( kx .eq. 24 ) THEN
            rhmx = (/ 95.,       95., 95., 95., 95., 95., 95., 95.,      &
                      95., 95.,  95., 95., 95., 90., 85., 80., 75.,      &
                      70., 66.,  60., 39., 10., 10., 10./)
            vwgt = (/ 1.0000,         1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 0.9850,      &
                      0.9680, 0.9500, 0.9290, 0.9060, 0.8810, 0.8500, 0.7580, 0.6500, 0.5100,      &
                      0.3500, 0.2120, 0.0500, 0.0270, 0.0001, 0.0001, 0.0001/)
         ELSE
            PRINT '(A)','Number of vertical levels assumed to be 24 for AFWA TC bogus option'
            STOP 'AFWA_TC_BOGUS_LEVEL_ERROR'
         END IF
 END IF

!Remember that ns = the north south staggered.
!              ew = the east west staggered.


!Put the U and V into the new arrays.
!The u and v are read in destaggered and then put on MM5 dot points UGH!!
 allocate(u11  (1:ns, 1:ew, 1:nz))
 allocate(v11  (1:ns, 1:ew, 1:nz))
!reorder the wind fields and then put on mass points.
!first U put on MM5 grid
 do i = 1,ns-1
    do j = 1,ew
       do k = 1,nz
          u11(i,j,k) = grid%u_gc(j,k,i)
       end do
    end do
 end do


!Destagger U to mass points
 do i = 1,ns-1
    do j = 1,ew-1
       do k = 1,nz
          u11(i,j,k) = 0.5 * (u11(i,j,k) + u11(i,j+1,k))
       end do
    end do
 end do
 call xtodot(u11,ns,ew)


!now V
 do i = 1,ns
    do j = 1,ew-1
       do k = 1,nz
          v11(i,j,k) = grid%v_gc(j,k,i)
       end do
    end do
 end do
 

!Destagger V to mass points
 do i = 1,ns-1
    do j = 1,ew-1
       do k = 1,nz
          v11(i,j,k) = 0.5 * (v11(i,j,k) + v11(i+1,j,k))
       end do
    end do
 end do
 call xtodot(v11,ns,ew)
!we now have u and v on MM5 dot points.



!here we need to transpose the level(k) and the ns direction
!So the ordering will be j,i,k - ns,ew,level
!we want MM5 ordering
 allocate(t11  (1:ns, 1:ew, 1:nz))
 allocate(rh11 (1:ns, 1:ew, 1:nz))
 allocate(phi11(1:ns, 1:ew, 1:nz))

 do i = 1,ns
    do j = 1,ew
       do k = 1,nz
           t11(i,j,k)   = grid%t_gc(j,k,i)
           rh11(i,j,k)  = grid%rh_gc(j,k,i)
           phi11(i,j,k) = grid%ght_gc(j,k,i)
       end do
    end do
 end do



!  Save initial fields.
 allocate(u1  (1:ns, 1:ew, 1:nz))
 allocate(v1  (1:ns, 1:ew, 1:nz))
 allocate(t1  (1:ns, 1:ew, 1:nz))
 allocate(rh1 (1:ns, 1:ew, 1:nz))
 allocate(phi1(1:ns, 1:ew, 1:nz))
!for wrf we need this 
   jx = ew
   ix = ns
    u1   = u11
    v1   = v11
    t1   = t11
   rh1   = rh11
  phi1   = phi11 * 9.81

!The two d fields
!The terrain soil height is from ght at level 1
 do i = 1,ns
    do j = 1,ew
       pslx(i,j)    = grid%pslv_gc(j,i)  * 0.01
       cor(i,j)     = grid%f(j,i)  !coreolous
       lond(i,j)    = grid%xlong_gc(j,i)
       terrain(i,j) = grid%ght_gc(j,1,i)
    end do
 end do
!longitude on dot points
 call xtodot(lond,ns,ew)


 DO k=1,kx
     CALL expand ( rh1(1,1,k) , ns-1 , ew-1 , ns , ew )
     CALL expand ( t1 (1,1,k) , ns-1 , ew-1 , ns , ew )
 END DO
  

!  Loop over the number of storms to process.
   
 l_tcbogus = .FALSE.
 all_storms : DO nstrm=1,storms
 k00  = 2
 kfrm = k00
 p85  = 850.

 kto  = kfrm
 DO k=kfrm+1,kx
     IF ( press(1,1,k) .GE. p85 ) THEN
           kto = kto + 1
     END IF
 END DO
 k85 = kto 


!  Parameters for max wind
 rho  = 1.2
 pprm = devpc*100.
 phip0= pprm/rho 


!latc_loc and lonc_loc come in from the namelist. 
 
 CALL latlon_to_ij ( proj , latc_loc(nstrm) , lonc_loc(nstrm) , x0 , y0 )
 IF ( ( x0 .LT. 1. ) .OR. ( x0 .GT. REAL(jx-1) ) .OR. &
              ( y0 .LT. 1. ) .OR. ( y0 .GT. REAL(ix-1) ) ) THEN
         PRINT '(A,I3,A,A,A)','         Storm position is outside the computational domain.'
         PRINT '(A,2F6.2,A)' ,'         Storm postion: (x,y) = ',x0,y0,'.'
         stop
 END IF

 l_tcbogus = .TRUE.
!  Bogus vortex specifications, vmax (m/s); rmax (m);
 vmx = vmax(nstrm)  * vmax_ratio

 IF (  latc_loc(nstrm) .LT. 0.  ) THEN
       vmx = -vmx
 END IF
   
 IF (  vmax(nstrm)  .LE. 0.  ) THEN
       vmx = SQRT( 2.*(1-beta)*ABS(phip0) )  
 END IF

 xico    = y0
 xjco    = x0
 xicn    = xico
 xjcn    = xjco
 n_iter  = 1

!  Start computing.

 PRINT '(/,A,I3,A,A,A)'     ,'---> TC: Processing storm number= ',nstrm
 PRINT '(A,F6.2,A,F7.2,A)'  ,'         Storm center lat= ',latc_loc(nstrm),' lon= ',lonc_loc(nstrm),'.'
 PRINT '(A,2F6.2,A)'        ,'         Storm center grid position (x,y)= ',xjcn,xicn,'.'
 PRINT '(A,F5.2,F9.2,A)'    ,'         Storm max wind (m/s) and max radius (m)= ',vmx,rmax(nstrm),'.'
 PRINT '(A,F5.2,A)'         ,'         Estimated central press dev (mb)= ',devpc,'.'


!  Initialize storm center to (1,1)

  DO k=1,kx
     strmci(k) = 1
     strmcj(k) = 1
  END DO
 
!  Define complete field of bogus storm
!Note dx is spacing in meters
  DO i=1,ns
     DO j=1,ew
        disx = REAL(j) - xjcn
        disy = REAL(i) - xicn
        CALL rankine(disx,disy,dx,kx,vwgt,rmax(nstrm),vmx,uuwork,vvwork,psiv,vorv)
        DO k=1,kx
           utcp(i,j,k)  = uuwork(k)
           vtcp(i,j,k)  = vvwork(k)
           psitc(i,j,k) = psiv(k)
           vortc(i,j,k) = vorv(k)
        END DO
     END DO
  END DO


! Rotate wind to map proj

  DO i=1,ns
     DO j=1,ew
        xlo = stdlon-lond(i,j)
!        print *,xlo
        IF ( xlo .GT. 180.)xlo = xlo-360.
        IF ( xlo .LT.-180.)xlo = xlo+360.
   
        alpha = xlo*conef*degran*SIGN(1.,centerlat)
        DO k=1,kx
           utcr(i,j,k) = vtcp(i,j,k)*SIN(alpha)+utcp(i,j,k)*COS(alpha)
           vtcr(i,j,k) = vtcp(i,j,k)*COS(alpha)-utcp(i,j,k)*SIN(alpha)
           if(vtcr(i,j,k) .gt. 500.) then
              print *,i,j,k,"a very bad value of vtcr"
              stop
           end if
           if(utcr(i,j,k) .gt. 500.) then
              print *,i,j,k,"a very bad value of utcr"
              stop
           end if           
        END DO
     END DO
  END DO
 

   DO k=1,kx
      DO i=1,ns
         utcr(i,ew,k) = utcr(i,ew-1,k)
         vtcr(i,ew,k) = vtcr(i,ew-1,k)
      END DO
      DO j=1,ew
          utcr(ns,j,k) = utcr(ns-1,j,k)
          vtcr(ns,j,k) = vtcr(ns-1,j,k)
      END DO
   
       utcr( 1,ew,k) = utcr(   2,ew-1,k)
       utcr(ns, 1,k) = utcr(ns-1,   2,k)
       utcr(ns,ew,k) = utcr(ns-1,ew-1,k)
   
       vtcr( 1,ew,k) = vtcr(   2,ew-1,k)
       vtcr(ns, 1,k) = vtcr(ns-1,   2,k)
       vtcr(ns,ew,k) = vtcr(ns-1,ew-1,k)
   END DO



!This is the map scale factor on cross points and dot points
    do i = 1,ns-1
      do j = 1,ew-1
         mfx(i,j)  = grid%msft(j,i)
         mfd(i,j)  = grid%msft(j,i)
      end do
   end do

   call xtodot(mfd,ns,ew)
!we now have a map scale factor on dot points.


!  Compute vorticity of FG 
   CALL vor(u1,v1,mfd,mfx,ns,ew,kx,dx,vort)
   

!  Compute divergence of FG
   CALL diverg(u1,v1,mfd,mfx,ns,ew,kx,dx,div)


!  Compute mixing ratio of FG
   CALL mxratprs(rh1,t1,press*100.,ns,ew,kx,q1,min_RH_value)
   q1(:,:,1) = q1(:,:,2)

   DO k=1,kx
      CALL expand ( q1(1,1,k) , ns-1 , ew-1 , ns , ew )
   END DO

!  Compute initial streamfunction - PSI1 
   vortsv = vort
   q0 = q1
   

!  Solve for streamfunction.
   DO k=1,kx 
      DO i=1,ns
         DO j=1,ew
            ff(i,j) = vort(i,j,k)
            tmp1(i,j)= 0.0
         END DO
      END DO
      epsilon = 1.E-2
      CALL relax(tmp1,ff,rd,ns,ew,dx,epsilon,alphar)
      DO i=1,ns
         DO j=1,ew
            psi1(i,j,k) = tmp1(i,j)
         END DO
      END DO
   END DO

   
   DO k=1,kx  !start of the k loop
      IF ( latc_loc(nstrm) .GE. 0. ) THEN
           vormx = -1.e10
      ELSE
           vormx =  1.e10
      END IF
   
      i_mvc = 1
      j_mvc = 1

      DO i=1,ns
         DO j=1,ew
            rad = SQRT((REAL(i)-xico)**2.+(REAL(j)-xjco)**2.)*dx
            IF ( rad .LE. r_search ) THEN
               IF ( latc_loc(nstrm) .GE. 0. ) THEN
                   IF ( vortsv(i,j,k) .GT. vormx ) THEN
                        vormx = vortsv(i,j,k)
                        i_mvc = i
                        j_mvc = j
                    END IF
               ELSE IF (latc_loc(nstrm) .LT. 0. ) THEN
                    IF ( vortsv(i,j,k) .LT. vormx ) THEN
                         vormx = vortsv(i,j,k)
                         i_mvc = i
                         j_mvc = j
                    END IF
               END IF
            END IF
         END DO
      END DO
      strmci(k) = i_mvc 
      strmcj(k) = j_mvc

      DO i=1,ns
         DO j=1,ew
            rad = SQRT(REAL((i-i_mvc)**2.+(j-j_mvc)**2.))*dx
            IF ( rad .GT. r_vor ) THEN
                 vort(i,j,k) = 0.
                 div(i,j,k)  = 0.
            END IF
         END DO
      END DO   

      DO itr=1,n_iter
         sum_q = 0.
         nct = 0
         DO i=1,ns
            DO j=1,ew
               rad = SQRT(REAL(i-i_mvc)**2.+REAL(j-j_mvc)**2.)*dx
               IF ( (rad .LT. r_vor2).AND.(rad .GE. 0.8*r_vor2) ) THEN
                     sum_q = sum_q + q0(i,j,k)
                     nct = nct + 1
               END IF
             END DO
          END DO
          avg_q = sum_q/MAX(REAL(nct),1.)
   
          DO i=1,ns
             DO j=1,ew
                 q_old = q0(i,j,k)
                 rad = SQRT(REAL(i-i_mvc)**2.+REAL(j-j_mvc)**2.)*dx
                 IF ( rad .LT. r_vor2 ) THEN
                      ror = rad/r_vor2
                      q_new = ((1.-ror)*avg_q) + (ror*q_old)
                      q0(i,j,k) = q_new
                 END IF
              END DO
           END DO
     END DO !end of itr loop
 END DO !of the k loop


!  Compute divergent wind
   DO k=1,kx
      DO i=1,ns
         DO j=1,ew
            ff(i,j) = div(i,j,k)
            tmp1(i,j)= 0.0
         END DO
      END DO
      epsilon = 1.e-2
!     epsilon = 1.E-5
      CALL relax(tmp1,ff,rd,ns,ew,dx,epsilon,alphar)
      DO i=1,ns
         DO j=1,ew
            chi(i,j,k) = tmp1(i,j)
         END DO
      END DO
    END DO !of the k loop for divergent winds 

    DO k=1,kx !start of k loop
         DO i=2,ns-1
            DO j=2,ew-1
               uchi(i,j,k) = ((chi(i  ,j  ,k)+chi(i-1,j  ,k))- (chi(i  ,j-1,k)+chi(i-1,j-1,k)))/(2.*dx)
               vchi(i,j,k) = ((chi(i  ,j  ,k)+chi(i  ,j-1,k))- (chi(i-1,j  ,k)+chi(i-1,j-1,k)))/(2.*dx)
            END DO
         END DO
   
         DO i=2,ns-1
            uchi(i,1,k)  = (chi(i   ,   2,k)-chi(i   ,   1,k))/(2.*dx)
            uchi(i,ew,k) = (chi(i   ,ew  ,k)-chi(i   ,ew-1,k))/(2.*dx)
            vchi(i,1,k)  = (chi(i   ,   2,k)-chi(i -1,   2,k))/(2.*dx)
            vchi(i,ew,k) = (chi(i   ,ew-1,k)-chi(i -1,ew-1,k))/(2.*dx)
         END DO
         DO j=2,ew-1
            uchi(1,j,k)  = (chi(   2,j   ,k)-chi(   2,j -1,k))/(2.*dx)
            uchi(ns,j,k) = (chi(ns-1,j   ,k)-chi(ns-1,j -1,k))/(2.*dx)
            vchi(1,j,k)  = (chi(   2,j -1,k)-chi(   1,j -1,k))/(2.*dx)
            vchi(ns,j,k) = (chi(ns  ,j -1,k)-chi(ns-1,j -1,k))/(2.*dx)
         END DO
   
         uchi( 1, 1,k) = uchi(   2,   2,k)
         uchi( 1,ew,k) = uchi(   2,ew-1,k)
         uchi(ns, 1,k) = uchi(ns-1,   2,k)
         uchi(ns,ns,k) = uchi(ns-1,ew-1,k)
   
         vchi( 1, 1,k) = vchi(   2,   2,k)
         vchi( 1,ew,k) = vchi(   2,ew-1,k)
         vchi(ns, 1,k) = vchi(ns-1,   2,k)
         vchi(ns,ew,k) = vchi(ns-1,ew-1,k)
     END DO !end of k loop


!  Compute background streamfunction (PSI0) and perturbation field (PSI)
     DO k=1,kx 
         DO i=1,ns
            DO j=1,ew
               ff(i,j)=vort(i,j,k)
               tmp1(i,j)=0.0
            END DO
         END DO
         epsilon = 1.e-2
!              epsilon = 1.E-5
         CALL relax(tmp1,ff,rd,ns,ew,dx,epsilon,alphar)
         DO i=1,ns
            DO j=1,ew
               psi(i,j,k)=tmp1(i,j)
            END DO
         END DO
     END DO

     DO k=1,kx
        DO i=2,ns-1
           DO j=2,ew-1
              psi0(i,j,k) = psi1(i,j,k)-psi(i,j,k)
           END DO
        END DO
     END DO

     DO k=k00,kx
        DO i=1,ns
           DO j=1,ew
              psipos(i,j,k)=psi(i,j,k)
           END DO
        END DO
     END DO

         
    DO k=1,kx
       DO i=2,ns-1
          DO j=2,ew-1
              upos(i,j,k) = -((psi(i  ,j  ,k)+psi(i  ,j-1,k))-(psi(i-1,j-1,k)+psi(i-1,j  ,k)))/(2.*dx)
              vpos(i,j,k) =  ((psi(i  ,j  ,k)+psi(i-1,j  ,k))-(psi(i-1,j-1,k)+psi(i  ,j-1,k)))/(2.*dx)
          END DO
       END DO
   
       DO i=2,ns-1
          upos(i,1,k)  = -(psi(i   ,   2,k)-psi(i -1,   2,k))/(2.*dx)
          upos(i,ew,k) = -(psi(i   ,ew-1,k)-psi(i -1,ew-1,k))/(2.*dx)
          vpos(i,1,k)  =  (psi(i   ,   2,k)-psi(i   ,   1,k))/(2.*dx)
          vpos(i,ew,k) =  (psi(i   ,ew  ,k)-psi(i   ,ew-1,k))/(2.*dx)
       END DO
       DO j=2,ew-1
           upos(1,j,k)  = -(psi(   2,j   ,k)-psi(   1,j   ,k))/(2.*dx)
           upos(ns,j,k) = -(psi(ns  ,j   ,k)-psi(ns-1,j   ,k))/(2.*dx)
           vpos(1,j,k)  =  (psi(   2,j   ,k)-psi(   2,j -1,k))/(2.*dx)
           vpos(ns,j,k) =  (psi(ns-1,j   ,k)-psi(ns-1,j -1,k))/(2.*dx)
       END DO
   
       upos( 1, 1,k) = upos(   2,   2,k)
       upos( 1,ew,k) = upos(   2,ew-1,k)
       upos(ns, 1,k) = upos(ns-1,   2,k)
       upos(ns,ew,k) = upos(ns-1,ew-1,k)
   
       vpos( 1, 1,k) = vpos(   2,   2,k)
       vpos( 1,ew,k) = vpos(   2,ew-1,k)
       vpos(ns, 1,k) = vpos(ns-1,   2,k)
       vpos(ns,ew,k) = vpos(ns-1,ew-1,k)
   
    END DO

!  Background u, v fields.
!  Subtract the first quess wind field original winds.
    
     DO k=1,kx
        DO i=1,ns
           DO j=1,ew
              u0(i,j,k) = u1(i,j,k)-(upos(i,j,k)+uchi(i,j,k))
              v0(i,j,k) = v1(i,j,k)-(vpos(i,j,k)+vchi(i,j,k))
           END DO
        END DO
     END DO
     

!Here we have the background u and v winds with the vortex
!removed so if we only want to remove the strorm we can
!now remove it from the winds.  So here we take the winds
!which are on MMM5 dot points put them on mass points and
!then restagger them to the C-grid.
    if(remove_only .eq. 1) then
       call dot2crs(u0,ns,ew)
       call dot2crs(v0,ns,ew) 
       call mass2_Ustag(u0,ns-1,ew,nz)
       call mass2_Vstag(v0,ns,ew-1,nz)
       do i = 1,ns-1
          do j = 1,ew
             do k = 1,nz
                grid%u_gc(j,k,i)   = u0(i,j,k)
             end do
         end do
      end do

       do i = 1,ns
          do j = 1,ew-1
             do k = 1,nz
                grid%v_gc(j,k,i)   = v0(i,j,k)
             end do
         end do
      end do

     end if


!  Geostrophic vorticity.
     CALL geowind(phi1,mfx,mfd,cor,ns,ew,kx,dx,ug,vg)
     CALL vor(ug,vg,mfd,mfx,ns,ew,kx,dx,vorg)


     DO k=1,kx
        i_mvc = strmci(k)
        j_mvc = strmcj(k)
   
         DO i=1,ns
           DO j=1,ew
               rad = SQRT(REAL(i-i_mvc)**2.+REAL(j-j_mvc)**2.)*dx
               IF ( rad .GT. r_vor ) THEN
                    vorg(i,j,k) = 0.
               END IF
           END DO
         END DO
     END DO
   
      DO k=k00,kx
         DO i=1,ns
            DO j=1,ew
               ff(i,j) = vorg(i,j,k)
               tmp1(i,j)= 0.0
            END DO
         END DO
         epsilon = 1.e-3
         CALL relax(tmp1,ff,rd,ns,ew,dx,epsilon,alphar)
         DO i=1,ns
            DO j=1,ew
               phip(i,j,k) = tmp1(i,j)
            END DO
         END DO
     END DO


     !  Background geopotential.
     DO k=k00,kx
         DO i=1,ns
            DO j=1,ew
               phi0(i,j,k)   = phi1(i,j,k) - phip(i,j,k)
            END DO
         END DO
     END DO


     !  Background temperature
     DO k=k00,kx 
        DO i=1,ns
           DO j=1,ew
              IF( k .EQ.  2 ) THEN
                  tpos(i,j,k) = (-1./rconst)*(phip(i,j,k+1)-phip(i,j,k  ))/LOG(press(i,j,k+1)/press(i,j,k))
              ELSE IF ( k .EQ. kx ) THEN
                  tpos(i,j,k) = (-1./rconst)*(phip(i,j,k  )-phip(i,j,k-1))/LOG(press(i,j,k)/press(i,j,k-1))
              ELSE
                  tpos(i,j,k) = (-1./rconst)*(phip(i,j,k+1)-phip(i,j,k-1))/LOG(press(i,j,k+1)/press(i,j,k-1))
              END IF
              t0(i,j,k) = t1(i,j,k)-tpos(i,j,k)
              t00(i,j,k) = t0(i,j,k)
              if(t0(i,j,k) .gt. 400) then
                 print *,"interesting temperature ",t0(i,j,k)," at ",i,j,k
                 stop
              end if
           END DO
        END DO
     END DO


     !  Background RH.
     CALL qvtorh (q0,t0,press*100.,k00,ix,jx,kx,rh0,min_RH_value)

     DO k=k00,kx
        CALL expand ( rh0(1,1,k) , ix-1 , jx-1 , ix , jx )
     END DO

!Note: here we test for the remove storm only for the temperature 
!and height fields.  At the surface (k=1) we just assign the 
!terrain field to the height.
    if(remove_only .eq. 1) then
       do i = 1,ns-1
          do j = 1,ew-1
             do k = 1,nz
                if(k .eq. 1) then
                   grid%ght_gc(j,k,i) = terrain(i,j)
                   grid%t_gc(j,k,i)   = t1(i,j,k)
                   grid%rh_gc(j,k,i)  = rh1(i,j,k)
                else
                   grid%ght_gc(j,k,i) = phi0(i,j,k)/9.8
                   grid%t_gc(j,k,i)   = t0(i,j,k)
                   grid%rh_gc(j,k,i)  = rh0(i,j,k)
                end if
             end do
         end do
      end do


!  Now remove the storm from the surface pressure.
      DO i=1,ns
         DO j=1,ew
            dphx0 = phi0(i,j,k00) - phi1(i,j,k00)
            delpx(i,j) = rho*dphx0*0.01
            pslx(i,j) = pslx(i,j) + delpx(i,j) 
            if(abs(grid%ht(j,i)) .lt. 1) then
                  grid%p_gc(j,1,i)  = pslx(i,j) * 100.
                  grid%psfc_gc(j,i) = pslx(i,j) * 100.
                  grid%pslv_gc(j,i) = pslx(i,j) * 100.
            else
                  grid%p_gc(j,1,i)  = grid%p_gc(j,1,i) - (pslx(i,j) * 100. + grid%pslv_gc(j,i))
                  grid%psfc_gc(j,i) = grid%p_gc(j,1,i)
                  grid%pslv_gc(j,i) = grid%pslv_gc(j,i) - (pslx(i,j) * 100. + grid%pslv_gc(j,i))
            end if
         END DO
      END DO

      return
  end if  
!*****************

     DO k=k00,kx
        rh_max= rhmx(k)
        i_mvc = strmci(k)
        j_mvc = strmcj(k)
   
        sum_rh = 0.
        nct = 0
        DO i=1,ns
           DO j=1,ew
              rad = SQRT(REAL(i-i_mvc)**2.+REAL(j-j_mvc)**2.)*dx
              IF ( (rad .LT. r_vor2).AND.(rad .GE. 0.8*r_vor2) ) THEN
                  sum_rh = sum_rh + rh0(i,j,k)
                  nct = nct + 1
              END IF
           END DO
        END DO
        avg_rh = sum_rh/MAX(REAL(nct),1.)
   
        DO i=1,ns
            DO j=1,ew
               rh_min = avg_rh 
               rad = SQRT((REAL(i)-xico)**2.+(REAL(j)-xjco)**2.)*dx
               IF ( rad .LE. rmax(nstrm) ) THEN
                  rhtc(i,j,k) = rh_max
               ELSE
                  rhtc(i,j,k) = (rmax(nstrm)/rad)*rh_max+(1.-(rmax(nstrm)/rad))*rh_min
               END IF
            END DO
         END DO
     END DO
   
     ! adjust T0
     DO k=k00,kx
        DO i=1,ns
           DO j=1,ew
              theta(i,j,k) = t1(i,j,k)*(1000./press(i,j,k))**rovcp
           END DO
        END DO
     END DO

     i_mvc = strmci(k00)
     j_mvc = strmcj(k00)


     DO k=kfrm,kto
        DO i=1,ns
           DO j=1,ew
              rad = SQRT(REAL(i-i_mvc)**2.+REAL(j-j_mvc)**2.)*dx
              IF ( rad .LT. r_vor2 ) THEN
                  t_reduce(i,j,k) = theta(i,j,k85)-0.03*(press(i,j,k)-press(i,j,k85))
                  t0(i,j,k) = t00(i,j,k)*(rad/r_vor2) + (((press(i,j,k)/1000.)**rovcp)*t_reduce(i,j,k))*(1.-(rad/r_vor2))
              END IF
           END DO
        END DO
     END DO

     !  New RH.
     DO k=k00,kx
        DO i=1,ns
           DO j=1,ew
              rhbkg = rh0(i,j,k)
              rhbog = rhtc(i,j,k)
              rad = SQRT((REAL(i)-xico)**2.+(REAL(j)-xjco)**2.)*dx
               IF ( (rad.GT.rmax(nstrm)) .AND. (rad.LE.r_vor2) ) THEN
                    r_ratio = (rad-rmax(nstrm))/(r_vor2-rmax(nstrm))
                    rh2(i,j,k) = ((1.-r_ratio)*rhbog) + (r_ratio*rhbkg)
              ELSE IF (rad .LE. rmax(nstrm) ) THEN
                    rh2(i,j,k) = rhbog
              ELSE
                    rh2(i,j,k) = rhbkg
              END IF
          END DO
        END DO
    END DO
   

    ! New wind field.
    DO k=1,kx
       DO i=1,ns
          DO j=1,ew
             u2(i,j,k) = u0(i,j,k)+utcr(i,j,k)
             v2(i,j,k) = v0(i,j,k)+vtcr(i,j,k)
          END DO
       END DO
    END DO

    !  Geopotential perturbation
    DO k=1,kx
       DO i=1,ns
          DO j=1,ew
              tmp1(i,j)=psitc(i,j,k)
          END DO
       END DO
       CALL balance(cor,tmp1,ns,ew,dx,outold)
       DO i=1,ns
          DO j=1,ew
             ff(i,j)=outold(i,j)
             tmp1(i,j)=0.0
          END DO
       END DO
       epsilon = 1.e-3
       CALL relax (tmp1,ff,rd,ns,ew,dx,epsilon,alphar)
       DO i=1,ns
          DO j=1,ew
             phiptc(i,j,k) = tmp1(i,j)
          END DO
       END DO
    END DO

   !  New geopotential field.
   DO k=1,kx
      DO i=1,ns
         DO j=1,ew
             phi2(i,j,k)  = phi0(i,j,k) + phiptc(i,j,k)
         END DO
      END DO
   END DO

  
   !  New temperature field.
    DO k=k00,kx
       DO i=1,ns
          DO j=1,ew
             IF( k .EQ.  2 ) THEN
                 tptc(i,j,k)=(-1./rconst)*(phiptc(i,j,k+1)-phiptc(i,j,k  ))/LOG(press(i,j,k+1)/press(i,j,k))
             ELSE IF ( k .EQ. kx ) THEN
                 tptc(i,j,k)=(-1./rconst)*(phiptc(i,j,k  )-phiptc(i,j,k-1))/LOG(press(i,j,k)/press(i,j,k-1))
             ELSE
                 tptc(i,j,k)=(-1./rconst)*(phiptc(i,j,k+1)-phiptc(i,j,k-1))/LOG(press(i,j,k+1)/press(i,j,k-1))
             END IF
             t2(i,j,k) = t0(i,j,k) + tptc(i,j,k)
             if(t2(i,j,k) .gt. 400) then
                print *,"interesting temperatrue "
                print *,t2(i,j,k),t0(i,j,k),tptc(i,j,k),i,j,k
                stop
             end if
           END DO
        END DO
    END DO


   !  Surface pressure change.
      DO i=1,ns
         DO j=1,ew
            dph = phi2(i,j,k00)-phi1(i,j,k00)
            delpx(i,j) = rho*dph*0.01
         END DO
      END DO

    !  New SLP.
      DO i=1,ns
         DO j=1,ew
            pslx(i,j) = pslx(i,j)+delpx(i,j) 
         END DO
      END DO


  !  Set new geopotential at surface to terrain elevation.

     DO i=1,ns-1
        DO j=1,ew-1
           z2(i,j,1) = terrain(i,j) 
        END DO
     END DO
     CALL expand(z2(1,1,1),ix-1,jx-1,ix,jx)
   
  !  Geopotential back to height.

     DO k=k00,kx
         DO i=1,ns
            DO j=1,ew
               z2(i,j,k) = phi2(i,j,k)/9.81 
            END DO
         END DO
         CALL expand(z2(1,1,k),ix-1,jx-1,ix,jx)
     END DO
     

     !  New surface temperature, assuming same theta as from 1000 mb.
     DO i=1,ns
        DO j=1,ew
!           ps = pslx(i,j)
!           t2(i,j,1) = t2(i,j,k00)*((ps/1000.)**rovcp)
            t2(i,j,1) = t11(i,j,1)
           if(t2(i,j,1) .gt. 400) then
              print *,"Interesting surface temperature"
              print *,t2(i,j,1),t2(i,j,k00),ps,i,j
              stop
           end if
        END DO
     END DO


     !  Set surface RH to the value from 1000 mb.
     DO i=1,ns
        DO j=1,ew
           rh2(i,j,1) = rh2(i,j,k00)
        END DO
     END DO

    !  Modification of tropical storm complete.
    PRINT '(A,I3,A)'       ,'         Bogus storm number ',nstrm,' completed.'


   DO k=1,kx
      DO i=1,ix
         DO j=1,jx
            u1(i,j,k)   = u2(i,j,k)
            v1(i,j,k)   = v2(i,j,k)
            t1(i,j,k)   = t2(i,j,k)
            rh1(i,j,k)  = rh2(i,j,k)
            phi1(i,j,k) = phi2(i,j,k)
         END DO
      END DO
   END DO

END DO all_storms
 deallocate(u11)
 deallocate(v11)
 deallocate(t11)
 deallocate(rh11)
 deallocate(phi11)
 deallocate(u1)
 deallocate(v1)
 deallocate(t1)
 deallocate(rh1)
 deallocate(phi1)

!Interpolate the U and V from mass points
!back to the staggered grid.
 call dot2crs(u0,ns,ew)
 call dot2crs(v0,ns,ew) 
 call mass2_Ustag(u2,ns-1,ew,nz)
 call mass2_Vstag(v2,ns,ew-1,nz)


 do i = 1,ns-1
    do j = 1,ew
       do k = 1,nz
          grid%u_gc(j,k,i)   = u2(i,j,k)
       end do
    end do
 end do

 do i = 1,ns
    do j = 1,ew-1
       do k = 1,nz
          grid%v_gc(j,k,i)   = v2(i,j,k)
        end do
    end do
 end do


 do i = 1,ns-1
    do j = 1,ew-1
       do k = 1,nz
             grid%t_gc(j,k,i)   = t2(i,j,k)
             grid%rh_gc(j,k,i)  = rh2(i,j,k)
             grid%ght_gc(j,k,i) = z2(i,j,k)
             if(k .eq. 1) then
                if(abs(grid%ht_gc(j,i)) .lt. 1) then
                   grid%p_gc(j,k,i)  = pslx(i,j) * 100.
                   grid%pslv_gc(j,i) = pslx(i,j) * 100.
                else
                   grid%p_gc(j,k,i)  = grid%p_gc(j,k,i)  + (pslx(i,j) * 100. - grid%pslv_gc(j,i))
                   grid%pslv_gc(j,i) = grid%pslv_gc(j,i) + (pslx(i,j) * 100. - grid%pslv_gc(j,i))
                end if
                grid%psfc(j,i)    = grid%p_gc(j,k,i)
            end if
       end do
    end do
 end do

END SUBROUTINE tc_bogus

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE expand(slab,istart,jstart,itot,jtot)

   !  Fill the nearest data to the empty rows or columns of a slab

      IMPLICIT NONE

      INTEGER :: istart, jstart, itot, jtot
      REAL , DIMENSION(itot,jtot) :: slab

      INTEGER :: i1, j1, i, j

      DO j1=jstart,jtot-1
         DO i=1,istart
            slab(i,j1+1)=slab(i,j1)
         END DO
      END DO

      DO i1=istart,itot-1
         DO j=1,jtot
            slab(i1+1,j)=slab(i1,j)
         END DO
      END DO

   END SUBROUTINE expand
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE rankine(dx,dy,ds,nlvl,vwgt,rmax,vmax,uu,vv,psi,vor)

   !  Define analytical bogus vortex

      IMPLICIT NONE

      INTEGER nlvl
      REAL , DIMENSION(nlvl) :: uu, vv, psi, vor
      REAL , DIMENSION(nlvl) :: vwgt
      REAL :: dx,dy,ds,rmax,vmax
 
      REAL , PARAMETER :: alpha1= 1.
      REAL , PARAMETER :: alpha2= -0.75
      real :: pi


      INTEGER :: k
      REAL :: vr , ang , rr , term1 , bb , term2 , alpha


      pi = 3.141592653589793
      !  Wind component

      DO k=1,nlvl
         rr = SQRT(dx**2+dy**2)*ds
         IF ( rr .LT. rmax ) THEN
            alpha = 1.
         ELSE IF ( rr .GE. rmax ) THEN
            alpha = alpha2
         END IF
         vr = vmax * (rr/rmax)**(alpha)
         IF ( dx.GE.0. ) THEN
            ang = (pi/2.) - ATAN2(dy,MAX(dx,1.e-6))
            uu(k) = vwgt(k)*(-vr*COS(ang))
            vv(k) = vwgt(k)*( vr*SIN(ang))
         ELSE IF ( dx.LT.0. ) THEN
            ang = ((3.*pi)/2.) + ATAN2(dy,dx)
            uu(k) = vwgt(k)*(-vr*COS(ang))
            vv(k) = vwgt(k)*(-vr*SIN(ang))
         END IF
      END DO

      !  psi

      DO k=1,nlvl
         rr = SQRT(dx**2+dy**2)*ds
         IF ( rr .LT. rmax ) THEN
            psi(k) = vwgt(k) * (vmax*rr*rr)/(2.*rmax)
         ELSE IF ( rr .GE. rmax ) THEN
            IF (alpha1.EQ.1.0 .AND. alpha2.eq.-1.0) THEN
               psi(k) = vwgt(k) * vmax*rmax*(0.5+LOG(rr/rmax))
            ELSE IF (alpha1.EQ.1.0 .AND. alpha2.NE.-1.0) THEN
               term1 = vmax/(rmax**alpha1)*(rmax**(alpha1+1)/(alpha1+1))
               bb    = (rr**(alpha2+1)/(alpha2+1))-(rmax**(alpha2+1))/(alpha2+1)
               term2 = vmax/(rmax**alpha2)*bb
               psi(k) = vwgt(k) * (term1 + term2)
            END IF
         END IF
      END DO

      ! vort

      DO k=1,nlvl
         rr = SQRT(dx**2+dy**2)*ds
         IF ( rr .LT. rmax ) THEN
            vor(k) = vwgt(k) * (2.*vmax)/rmax
         ELSE IF ( rr .GE. rmax ) THEN
            vor(k) = vwgt(k) * ( (vmax/rmax**alpha2)*(rr**(alpha2-1.))*(1.+alpha2) )
         END IF
      END DO

   END SUBROUTINE rankine
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE vor(u,v,dmf,xmf,i1,j1,k1,ds,vort)

      !  Compute k component of del cross velocity
      !  vort = m*m (dv/dx - du/dy), where u and v are coupled
      !  with map factors (dot point) and m is the map factors
      !  on cross points

      IMPLICIT NONE

      INTEGER :: i1 , j1 , k1

      REAL , DIMENSION(i1,j1,k1) :: u, v, vort
      REAL , DIMENSION(i1,j1   ) :: xmf, dmf

      REAL :: ds

      REAL :: ds2r , u1 , u2 , u3 , u4 , v1 , v2 , v3 , v4
      INTEGER :: i , j , k

      ds2r=1./(2.*ds)

      DO k=1,k1
         DO j=1,j1-1
            DO i=1,i1-1
               u1=u(i  ,j  ,k)/dmf(i  ,j  )
               u2=u(i+1,j  ,k)/dmf(i+1,j  )
               u3=u(i  ,j+1,k)/dmf(i  ,j+1)
               u4=u(i+1,j+1,k)/dmf(i+1,j+1)
               v1=v(i  ,j  ,k)/dmf(i  ,j  )
               v2=v(i+1,j  ,k)/dmf(i+1,j  )
               v3=v(i  ,j+1,k)/dmf(i  ,j+1)
               v4=v(i+1,j+1,k)/dmf(i+1,j+1)
               vort(i,j,k)=xmf(i,j)*xmf(i,j)*ds2r*((v4-v2+v3-v1)-(u2-u1+u4-u3))
            END DO
         END DO
      END DO

      CALL fillit(vort,i1,j1,k1,i1,j1,1,i1-1,1,j1-1)

   END SUBROUTINE vor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE diverg(u,v,dmf,xmf,i1,j1,k1,ds,div)

   !  Computes divergence
   !  div = m*m (du/dx + dv/dy)

      IMPLICIT NONE

      INTEGER :: i1, j1 , k1

      REAL , DIMENSION(i1,j1,k1) :: u, v, div
      REAL , DIMENSION(i1,j1   ) :: xmf, dmf
      REAL :: ds

      REAL :: ds2r , u1 , u2 , u3 , u4 , v1 , v2 , v3 , v4
      INTEGER :: i , j , k

      ds2r = 1./(2.*ds)

      DO k = 1, k1
         DO j = 1, j1-1
            DO i = 1, i1-1
               u1=u(i  ,j  ,k)/dmf(i  ,j  )
               u2=u(i+1,j  ,k)/dmf(i+1,j  )
               u3=u(i  ,j+1,k)/dmf(i  ,j+1)
               u4=u(i+1,j+1,k)/dmf(i+1,j+1)
               v1=v(i  ,j  ,k)/dmf(i  ,j  )
               v2=v(i+1,j  ,k)/dmf(i+1,j  )
               v2=v(i+1,j  ,k)/dmf(i+1,j  )
               v3=v(i  ,j+1,k)/dmf(i  ,j+1)
               v4=v(i+1,j+1,k)/dmf(i+1,j+1)
               div(i,j,k) = xmf(i,j)*xmf(i,j)*ds2r*((u3-u1+u4-u2)+(v2-v1+v4-v3))
            END DO
         END DO
      END DO

   END SUBROUTINE diverg

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE mxratprs (rh, t, ppa, ix, jx, kx, q, min_RH_value)

      
      IMPLICIT NONE

      INTEGER   :: i , ix , j , jx , k , kx 


      REAL      :: min_RH_value
      REAL      :: ppa(ix,jx,kx)
      REAL      :: p( ix,jx,KX )
      REAL      :: q (ix,jx,kx),rh(ix,jx,kx),t(ix,jx,kx)

      REAL      :: es
      REAL      :: qs
      REAL      :: cp              = 1004.0
      REAL      :: svp1,svp2,svp3
      REAL      :: celkel
      REAL      :: eps
      

      !  This function is designed to compute (q) from basic variables
      !  p (mb), t(K) and rh(0-100%) to give (q) in (kg/kg).

      
      p = ppa * 0.01

      DO k = 1, kx
         DO j = 1, jx - 1
            DO i = 1, ix - 1
                  rh(i,j,k) = MIN ( MAX ( rh(i,j,k) ,min_RH_value ) , 100. ) 
            END DO
         END DO
      END DO

      rh(ix,:,:) = rh(ix-1,:,:)
      rh(:,jx,:) = rh(:,jx-1,:)
      t (ix,:,:) = t (ix-1,:,:)
      t (:,jx,:) = t (:,jx-1,:)

      svp3   =  29.65
      svp1   =  0.6112
      svp2   =  17.67
      celkel =  273.15
         eps =  0.622
      DO k = 1, kx
         DO j = 1, jx
            DO i = 1, ix
               es = svp1 * 10. * EXP(svp2 * (t(i,j,k) - celkel ) / (t(i,j,k) - svp3 ))
               qs = eps * es / (p(i,j,k) - es)
               q(i,j,k) = MAX(0.01 * rh(i,j,k) * qs,0.0)
            END DO
         END DO
      END DO

   END SUBROUTINE mxratprs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE fillit (f, ix, jx, kx, imx, jmx, ifirst, ilast, jfirst, jlast)

      IMPLICIT NONE

      INTEGER                     :: i
      INTEGER                     :: ifirst
      INTEGER                     :: ilast
      INTEGER                     :: imx
      INTEGER                     :: ix
      INTEGER                     :: j
      INTEGER                     :: jfirst
      INTEGER                     :: jlast
      INTEGER                     :: jmx
      INTEGER                     :: jx
      INTEGER                     :: k
      INTEGER                     :: kx

      REAL , dimension(ix,jx,kx) :: f

      DO k = 1 , kx
         DO j = jfirst, jlast
            DO i = 1, ifirst - 1
               f(i,j,k) = f(ifirst,j,k)
            END DO
            DO i = ilast + 1, imx
               f(i,j,k) = f(ilast,j,k)
            END DO
         END DO
   
         DO j = 1, jfirst - 1
            f(:,j,k) = f(:,jfirst,k)
         END DO
         DO j = jlast + 1, jmx
            f(:,j,k) = f(:,jlast,k)
         END DO
      END DO

   END SUBROUTINE fillit
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE mass2_Ustag(field,dim1,dim2,dim3)

   IMPLICIT NONE

   INTEGER :: dim1 , dim2 , dim3
   REAL , DIMENSION(dim1,dim2,dim3) :: field,dummy

   dummy = 0.0
   dummy(:,2:dim2-1,:)         = ( field(:,1:dim2-2,:) + &
                                   field(:,2:dim2-1,:) ) * 0.5
   dummy(:,1,:)                = field(:,1,:)
   dummy(:,dim2,:)             = field(:,dim2-1,:)

   field                       =   dummy

END SUBROUTINE mass2_Ustag

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE mass2_Vstag(field,dim1,dim2,dim3)

   IMPLICIT NONE

   INTEGER :: dim1 , dim2 , dim3
   REAL , DIMENSION(dim1,dim2,dim3) :: field,dummy

   dummy = 0.0
   dummy(2:dim1-1,:,:)         = ( field(1:dim1-2,:,:) + &
                                   field(2:dim1-1,:,:) ) * 0.5
   dummy(1,:,:)                = field(1,:,:)
   dummy(dim1,:,:)             = field(dim1-1,:,:)

   field                       =   dummy

END SUBROUTINE mass2_Vstag


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE crs2dot(field,dim1,dim2)
   
      IMPLICIT NONE

      INTEGER :: dim1 , dim2
      REAL , DIMENSION(dim1,dim2) :: field,dummy
      INTEGER :: i , j 
      

!This fills in the middle of the array.
      dummy(2:dim1-1,2:dim2-1)           = ( field(1:dim1-2,1:dim2-2) + &
                                             field(1:dim1-2,2:dim2-1) + &
                                             field(2:dim1-1,1:dim2-2) + &
                                             field(2:dim1-1,2:dim2-1) ) * 0.25
   
!This fills in the bottom and top of the array
      dummy(2:dim1-1,1:dim2:dim2-1)      = ( field(1:dim1-2,1:dim2-1:dim2-2) + &
                                             field(2:dim1-1,1:dim2-1:dim2-2) ) * 0.5
   
!This fills in the left and right side of the array.
      dummy(1:dim1:dim1-1,2:dim2-1)      = ( field(1:dim1-1:dim1-2,1:dim2-2) + &
                                             field(1:dim1-1:dim1-2,2:dim2-1) ) * 0.5
   

!This takes care of the cornors.(  0,0    0,ew,  ns,0  ns,ew)
      dummy(1:dim1:dim1-1,1:dim2:dim2-1) =   field(1:dim1-1:dim1-2,1:dim2-1:dim2-2)
   
      field                              =   dummy
   
   END SUBROUTINE crs2dot
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE relax (chi, ff, rd, imx, jmx, ds, smallres, alpha)

      IMPLICIT NONE

      INTEGER, PARAMETER    :: mm = 20000

      INTEGER               :: i
      INTEGER               :: ie
      INTEGER               :: imx
      INTEGER               :: iter
      INTEGER               :: j
      INTEGER               :: je
      INTEGER               :: jm
      INTEGER               :: jmx
      INTEGER               :: mi

      REAL                  :: alpha
      REAL                  :: alphaov4
      REAL                  :: chi(imx,jmx)
      REAL                  :: chimx( jmx ) 
      REAL                  :: ds
      REAL                  :: epx
      REAL                  :: fac
      REAL                  :: ff(imx,jmx)
      REAL                  :: rd(imx,jmx)
      REAL                  :: rdmax( jmx )
      REAL                  :: smallres

      LOGICAL               :: converged = .FALSE.

      fac = ds * ds
      alphaov4 = alpha * 0.25

      ie=imx-2
      je=jmx-2

      DO j = 1, jmx
         DO i = 1, imx
            ff(i,j) = fac * ff(i,j)
            rd(i,j) = 0.0
         END DO
      END DO

      iter_loop : DO iter = 1, mm
         mi = iter
         chimx = 0.0


         DO j = 2, je
            DO i = 2, ie
               chimx(j) = MAX(ABS(chi(i,j)),chimx(j))
            END DO
         END DO

         epx = MAXVAL(chimx) * SMALLRES * 4.0 / alpha

         DO j = 2, je
            DO i = 2, ie
               rd(i,j) = chi(i,j+1) + chi(i,j-1) + chi(i+1,j) + chi(i-1,j) - 4.0 * chi(i,j) - ff(i,j)
               chi(i,j) = chi(i,j) + rd(i,j) * alphaov4
            END DO
         END DO

         rdmax = 0.0

         DO j = 2, je
            DO i = 2, ie
               rdmax(j) = MAX(ABS(rd(i,j)),rdmax(j))
            END DO
         END DO

         IF (MAXVAL(rdmax) .lt. epx) THEN
            converged = .TRUE.
            EXIT iter_loop
         END IF

      END DO iter_loop

      IF (converged ) THEN
!        PRINT '(A,I5,A)','Relaxation converged in ',mi,' iterations.'
      ELSE
         PRINT '(A,I5,A)','Relaxation did not converge in',mm,' iterations.'
         STOP 'no_converge'
      END IF

   END SUBROUTINE relax
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE geowind(height,xmf,dmf,cor,imx,jmx,kx,ds,ug,vg)

   !  Computes the geostrophic wind components from the height gradient.
   !  There is no Coriolis parameter used - this is the tropics.

      IMPLICIT NONE

      !     input       height   geopotential               cross    3d
      !                 xmf      map factors                cross    2d
      !                 dmf      map factors                dot      2d
      !                 imx      dot point dimension n-s
      !                 jmx      dot point dimension e-w
      !                 kx       number of vertical levels
      !
      !     output      ug       u component of geo wind    cross    3d
      !                 vg       v component of geo wind    cross    3D

      INTEGER :: imx , jmx , kx
      REAL :: ds
      REAL , DIMENSION(imx,jmx,kx) :: height
      REAL , DIMENSION(imx,jmx   ) :: xmf , dmf , cor

      REAL , DIMENSION(imx,jmx,kx) :: ug , vg

      REAL :: ds2r , h1 , h2 , h3 , h4
      INTEGER :: i , j , k

      ds2r=1./(2.*ds)

      DO k=1,kx
         DO j=2,jmx-1
            DO i=2,imx-1
               h1=height(i-1,j-1,k)
               h2=height(i  ,j-1,k)
               h3=height(i-1,j  ,k)
               h4=height(i  ,j  ,k)
!              ug(i,j,k)=-1.*g*dmf(i,j)/cor(i,j)*ds2r*(h4-h3+h2-h1)
!              vg(i,j,k)=    g*dmf(i,j)/cor(i,j)*ds2r*(h4-h2+h3-h1)
               ug(i,j,k)=-1.*dmf(i,j)*ds2r*(h4-h3+h2-h1)
               vg(i,j,k)=    dmf(i,j)*ds2r*(h4-h2+h3-h1)
            END DO
         END DO
      END DO

      CALL fillit(ug,imx,jmx,kx,imx,jmx,2,imx-1,2,jmx-1)
      CALL fillit(vg,imx,jmx,kx,imx,jmx,2,imx-1,2,jmx-1)

   END SUBROUTINE geowind
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE balance (f,psi,ix,jx,ds,out)

   !  Calculates the forcing terms in balance equation

   IMPLICIT NONE

      !  f       coriolis force
      !  psi     stream function
      !  ix, jx  grid points in east west, north south direction, respectively
      !  ds      grid distance
      !  out     output array
  
      INTEGER :: ix , jx
      REAL , DIMENSION(ix,jx) :: f,psi,out
      REAL :: ds

      REAL :: psixx , psiyy , psiy , psixy 
      REAL :: dssq , ds2 , dssq4

      INTEGER :: i , j

      dssq  = ds * ds
      ds2   = ds * 2.
      dssq4 = ds * ds * 4.

!      print *,"in balance",dssq,ds2,dssq4,ds
      DO i=2,ix-2
         DO j=2,jx-2
            psixx = ( psi(i,j+1) + psi(i,j-1) - 2.*psi(i,j) ) / dssq
            psiyy = ( psi(i+1,j) + psi(i-1,j) - 2.*psi(i,j) ) / dssq
            psiy  = ( psi(i+1,j) - psi(i-1,j) ) / ds2
            psixy = ( psi(i+1,j+1)+psi(i-1,j-1)-psi(i-1,j+1)-psi(i+1,j-1)) / dssq4
!            print *,f(i,j),f(i+1,j),f(i+1,j+1),f(i,j+1),psixx,psiyy,psiy,psixy
            out(i,j)=0.25*(f(i,j)+f(i+1,j)+f(i+1,j+1)+f(i,j+1))*(psixx+psiyy)    &
                   +psiy*(f(i+1,j+1)+f(i+1,j)-f(i,j)-f(i,j+1))/ ds2              &
                   -2.*(psixy*psixy-psixx*psiyy)
         END DO
      END DO

      CALL fill(out,ix,jx,ix,jx,2,ix-2,2,jx-2)

   END SUBROUTINE balance
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE fill (f, ix, jx, imx, jmx, ifirst, ilast, jfirst, jlast)

      IMPLICIT NONE

      INTEGER                     :: I
      INTEGER                     :: IFIRST
      INTEGER                     :: ILAST
      INTEGER                     :: IMX
      INTEGER                     :: IX
      INTEGER                     :: J
      INTEGER                     :: JFIRST
      INTEGER                     :: JLAST
      INTEGER                     :: JMX
      INTEGER                     :: JX

      REAL                        :: F(ix,jx)

      DO j = jfirst, jlast
         DO i = 1, ifirst - 1
            f(i,j) = f(ifirst,j)
         END DO
         DO i = ilast + 1, imx
            f(i,j) = f(ilast,j)
         END DO
      END DO

      DO j = 1, jfirst - 1
         f(:,j) = f(:,jfirst)
      END DO
      DO j = jlast + 1, jmx
         f(:,j) = f(:,jlast)
      END DO

   END SUBROUTINE fill
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE qvtorh ( q , t , p , k00, imx , jmx , kxs , rh, min_RH_value   )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: imx , jmx , kxs , k00
      REAL , INTENT(IN) , DIMENSION(imx,jmx,kxs) :: q ,t, p
      REAL , INTENT(OUT) , DIMENSION(imx,jmx,kxs) :: rh

      real    min_RH_value

      !  Local variables.

      INTEGER :: i , j , k
      REAL      :: es
      REAL      :: qs
      REAL      :: cp              = 1004.0
      REAL      :: svp1,svp2,svp3
      REAL      :: celkel
      REAL      :: eps
      svp3   =  29.65
      svp1   =  0.6112
      svp2   =  17.67
      celkel =  273.15
         eps =  0.622

      DO k = k00 , kxs
         DO j = 1 , jmx - 1
            DO i = 1 , imx - 1
               es = svp1 * 10. * EXP(svp2 * (t(i,j,k) - celkel ) / (t(i,j,k) - svp3 ))
               qs = eps*es/(0.01*p(i,j,k) - es)
               rh(i,j,k) = MIN ( 100. , MAX ( 100.*q(i,j,k)/qs , min_RH_value ) )
            END DO
         END DO
      END DO

   END SUBROUTINE qvtorh
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine utodot(slab,maxiy,maxjx)
!
!   This routine converts data that is on the C-grid u-velocity
!   staggered grid to the B-grid velocity staggered grid (known in
!   MM5 lingo as "dot points") 
!
      dimension slab(maxiy,maxjx),bot(2000)
!
!   Extrapolate out to top and bottom edges.
!
      do j=1,maxjx
         bot(j)=(3.*slab(1,j)-slab(2,j))/2.
         slab(maxiy,j)=(3.*slab(maxiy-1,j)-slab(maxiy-2,j))/2.
      enddo
!
!   Interpolate in the interior.
!
      do j=maxjx,1,-1
      do i=maxiy-1,2,-1
         slab(i,j)=.5*(slab(i-1,j)+slab(i,j))
      enddo
      enddo
!
!   Put "bot" values into slab.
!
      do j=1,maxjx
         slab(1,j)=bot(j)
      enddo
!
      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine vtodot(slab,maxiy,maxjx)
!
!   This routine converts data that is on the C-grid v-velocity
!   staggered grid to the B-grid velocity staggered grid (known in
!   MM5 lingo as "dot points") 
!
      dimension slab(maxiy,maxjx),rleft(2000)
!
!   Extrapolate out to left and right edges.
!
      do i=1,maxiy
         rleft(i)=(3.*slab(i,1)-slab(i,2))/2.
         slab(i,maxjx)=(3.*slab(i,maxjx-1)-slab(i,maxjx-2))/2.
      enddo
!
!   Interpolate in the interior.
!
      do j=maxjx-1,2,-1
      do i=maxiy,1,-1
         slab(i,j)=.5*(slab(i,j-1)+slab(i,j))
      enddo
      enddo
!
!   Put "rleft" values into slab.
!
      do i=1,maxiy
         slab(i,1)=rleft(i)
      enddo
!
      return
      end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine fillarray(array,ndim,val)
      dimension array(ndim)
      do 10 i=1,ndim
         array(i)=val
   10 continue

      return
      end
         
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine xtodot(slab,maxiy,maxjx)
!
!     This routine converts data that is on the B-grid mass grid (known
!     in MM5 lingo as "cross points") to the B-grid velocity staggered
!     grid (known in MM5 lingo as "dot points")
!
      dimension slab(maxiy,maxjx),bot(10000),rleft(10000)
!
!   Extrapolate out to top and bottom edges.
!
      do 200 j=2,maxjx-1
         bot(j)=(3.*(slab(1,j-1)+slab(1,j))- (slab(2,j-1)+slab(2,j)))/4.
         slab(maxiy,j)=(3.*(slab(maxiy-1,j-1)+slab(maxiy-1,j))- (slab(maxiy-2,j-1)+slab(maxiy-2,j)))/4.
  200 continue
!
!   Extrapolate out to left and right edges.
!
      do 300 i=2,maxiy-1
         rleft(i)=(3.*(slab(i-1,1)+slab(i,1))-(slab(i-1,2)+slab(i,2)))/4.
         slab(i,maxjx)=(3.*(slab(i-1,maxjx-1)+slab(i,maxjx-1))-(slab(i-1,maxjx-2)+slab(i,maxjx-2)))/4.
  300 continue
!
!   Extrapolate out to corners.
!
      rleft(1)=(3.*slab(1,1)-slab(2,2))/2.
      rleft(maxiy)=(3.*slab(maxiy-1,1)-slab(maxiy-2,2))/2.
      bot(maxjx)=(3.*slab(1,maxjx-1)-slab(2,maxjx-2))/2.
      slab(maxiy,maxjx)=(3.*slab(maxiy-1,maxjx-1)-slab(maxiy-2,maxjx-2))/2.
!
!   Interpolate in the interior.
!
      do 100 j=maxjx-1,2,-1
      do 100 i=maxiy-1,2,-1
         slab(i,j)=.25*(slab(i-1,j-1)+slab(i,j-1)+slab(i-1,j)+slab(i,j))
  100    continue
!
!   Put "bot" and "rleft" values into slab.
!
      do j=2,maxjx
         slab(1,j)=bot(j)
      enddo
      do i=1,maxiy
         slab(i,1)=rleft(i)
      enddo
!
      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE dot2crs(field,dim1,dim2)

      IMPLICIT NONE

      INTEGER :: dim1 , dim2
      REAL , DIMENSION(dim1,dim2) :: field

      INTEGER :: i , j 

      DO j = 1 , dim2 - 1
         DO i = 1 , dim1 - 1
            field(i,j) = ( field(i  ,j  ) + & 
                           field(i+1,j  ) + & 
                           field(i  ,j+1) + & 
                           field(i+1,j+1) ) * 0.25
         END DO
      END DO

   END SUBROUTINE dot2crs
