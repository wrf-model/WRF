!  Create an initial data set for the WRF model based on real data.  This
!  program is specifically set up for the Eulerian, mass-based coordinate.
PROGRAM real_data

   USE module_machine
#ifdef DM_PARALLEL
   USE module_dm, ONLY : wrf_dm_initialize
#endif
   USE module_domain, ONLY : domain, alloc_and_configure_domain, &
        domain_clock_set, head_grid, program_name, domain_clockprint, &
        set_current_grid_ptr
   USE module_initialize_real, ONLY : wrfu_initialize, find_my_parent, find_my_parent2
   USE module_io_domain
   USE module_driver_constants
   USE module_configure, ONLY : grid_config_rec_type, model_config_rec, &
        initial_config, get_config_as_buffer, set_config_as_buffer
   USE module_timing
   USE module_state_description, ONLY : realonly, THOMPSONAERO
#ifdef NO_LEAP_CALENDAR
   USE module_symbols_util, ONLY: wrfu_cal_noleap
#else
   USE module_symbols_util, ONLY: wrfu_cal_gregorian
#endif
   USE module_check_a_mundo
#if ( WRF_CHEM == 1 )
   USE module_input_chem_data
   USE module_input_chem_bioemiss
!  USE module_input_chem_emissopt3
#endif
   USE module_utility, ONLY : WRFU_finalize

   IMPLICIT NONE

#if ( WRF_CHEM == 1 )
  ! interface
   INTERFACE
     ! mediation-supplied 
     SUBROUTINE med_read_wrf_chem_bioemiss ( grid , config_flags)
       USE module_domain
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_read_wrf_chem_bioemiss
   END INTERFACE
#endif

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
   INTEGER :: time_loop_max , time_loop
real::t1,t2
   INTERFACE
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain, ONLY : domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping
   END INTERFACE

   LOGICAL :: ok_so_far

#include "version_decl"

   !  Define the name of this program (program_name defined in module_domain)

   ! NOTE: share/input_wrf.F tests first 7 chars of this name to decide 
   ! whether to read P_TOP as metadata from the SI (yes, if .eq. REAL_EM)

   program_name = "REAL_EM " // TRIM(release_version) // " PREPROCESSOR"

#ifdef DM_PARALLEL
   CALL disable_quilting
#endif

   !  Initialize the modules used by the WRF system.  Many of the CALLs made from the
   !  init_modules routine are NO-OPs.  Typical initializations are: the size of a
   !  REAL, setting the file handles to a pre-use value, defining moisture and
   !  chemistry indices, etc.

   CALL       wrf_debug ( 100 , 'real_em: calling init_modules ' )
   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)
#ifdef NO_LEAP_CALENDAR
   CALL WRFU_Initialize( defaultCalKind=WRFU_CAL_NOLEAP, rc=rc )
#else
   CALL WRFU_Initialize( defaultCalKind=WRFU_CAL_GREGORIAN, rc=rc )
#endif
   CALL init_modules(2)   ! Phase 2 resumes after MPI_INIT() (if it is called)

   !  The configuration switches mostly come from the NAMELIST input.

#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
      CALL initial_config
   END IF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize
#else
   CALL initial_config
#endif

   !  There are variables in the Registry that are only required for the real
   !  program, fields that come from the WPS package.  We define the run-time
   !  flag that says to allocate space for these input-from-WPS-only arrays.

   CALL nl_set_use_wps_input ( 1 , REALONLY )

   CALL check_nml_consistency
   CALL set_physics_rconfigs

   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   CALL  wrf_message ( program_name )

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

         IF ( domain_id .GT. 1 ) THEN

            CALL nl_get_grid_id        ( domain_id, grid_id        )
            CALL nl_get_parent_id      ( domain_id, parent_id      )
            CALL nl_get_e_we           ( domain_id, e_we           )
            CALL nl_get_e_sn           ( domain_id, e_sn           )
            CALL nl_get_i_parent_start ( domain_id, i_parent_start )
            CALL nl_get_j_parent_start ( domain_id, j_parent_start )
            WRITE (message,FMT='(A,2I3,2I4,2I3)') &
            'new allocated  domain: id, par id, dims i/j, start i/j =', &
            grid_id, parent_id, e_we, e_sn, i_parent_start, j_parent_start

            CALL wrf_debug ( 100 , message )
            CALL nl_get_grid_id        ( parent_id, grid_id        )
            CALL nl_get_parent_id      ( parent_id, parent_id1     )
            CALL nl_get_e_we           ( parent_id, e_we           )
            CALL nl_get_e_sn           ( parent_id, e_sn           )
            CALL nl_get_i_parent_start ( parent_id, i_parent_start )
            CALL nl_get_j_parent_start ( parent_id, j_parent_start )
            WRITE (message,FMT='(A,2I3,2I4,2I3)') &
            'parent domain: id, par id, dims i/j, start i/j =', &
            grid_id, parent_id1, e_we, e_sn, i_parent_start, j_parent_start
            CALL wrf_debug ( 100 , message )

            CALL nl_get_grid_id        ( domain_id, grid_id        )
            CALL nl_get_parent_id      ( domain_id, parent_id      )
            CALL nl_get_e_we           ( domain_id, e_we           )
            CALL nl_get_e_sn           ( domain_id, e_sn           )
            CALL nl_get_i_parent_start ( domain_id, i_parent_start )
            CALL nl_get_j_parent_start ( domain_id, j_parent_start )
            grid_ptr2 => head_grid
            found_the_id = .FALSE.
!           CALL find_my_parent ( grid_ptr2 , grid_ptr , domain_id , parent_id , found_the_id )
            CALL find_my_parent2( grid_ptr2 , grid_ptr ,             parent_id , found_the_id )
            IF ( found_the_id ) THEN

               sibling_count = 0
               DO dom_loop = 2 , domain_id
                 CALL nl_get_parent_id ( dom_loop, parent_id_hold )
                 IF ( parent_id_hold .EQ. parent_id ) THEN
                    sibling_count = sibling_count + 1
                 END IF
               END DO
               CALL alloc_and_configure_domain ( domain_id  = domain_id    , &
                                                 grid       = another_grid , &
                                                 parent     = grid_ptr     , &
                                                 kid        = sibling_count )
               grid => another_grid
            ELSE
              CALL wrf_error_fatal( 'real_em.F: Could not find the parent domain')
            END IF
         END IF

         CALL Setup_Timekeeping ( grid )
         CALL set_current_grid_ptr( grid )
         CALL domain_clockprint ( 150, grid, &
                'DEBUG real:  clock after Setup_Timekeeping,' )
         CALL domain_clock_set( grid, &
                                time_step_seconds=model_config_rec%interval_seconds )
         CALL domain_clockprint ( 150, grid, &
                'DEBUG real:  clock after timeStep set,' )


         CALL       wrf_debug ( 100 , 'real_em: calling set_scalar_indices_from_config ' )
         CALL set_scalar_indices_from_config ( grid%id , idum1, idum2 )

         CALL       wrf_debug ( 100 , 'real_em: calling model_to_grid_config_rec ' )
         CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

         !  Some simple checks.
         
         ok_so_far = .TRUE.

         !DJW changed the check below so that instead of always comparing
         !to d01 we now compare vertical levels between a grid and its parent.
         !If 4 domains were used, this allows for vertical nesting to be enabled between grids 1 & 2 and
         !between grids 3 & 4, but allows you to not use vertical grid nesting between grids 2 & 3.
         DO loop = 2 , model_config_rec%max_dom
           IF (( model_config_rec%vert_refine_method(loop) .EQ. 0 ) .AND. ( model_config_rec%vert_refine_fact .EQ. 1 )) THEN
             IF ( model_config_rec%e_vert(loop) .NE.  model_config_rec%e_vert(model_config_rec%parent_id(loop)) ) THEN
               CALL  wrf_message ( 'e_vert must be the same for each domain' )
               ok_so_far = .FALSE.
              END IF
            END IF
         END DO
         IF ( .NOT. ok_so_far ) THEN
            CALL wrf_error_fatal( 'fix namelist.input settings' )
         END IF

         !  Initialize the WRF IO: open files, init file handles, etc.

         CALL       wrf_debug ( 100 , 'real_em: calling init_wrfio' )
         CALL init_wrfio

         !  Some of the configuration values may have been modified from the initial READ
         !  of the NAMELIST, so we re-broadcast the configuration records.

#ifdef DM_PARALLEL
         CALL       wrf_debug ( 100 , 'real_em: re-broadcast the configuration records' )
         CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
         CALL wrf_dm_bcast_bytes( configbuf, nbytes )
         CALL set_config_as_buffer( configbuf, configbuflen )
#endif

         !   No looping in this layer.  

         CALL       wrf_debug ( 100 , 'calling med_sidata_input' )
         CALL med_sidata_input ( grid , config_flags )
         CALL       wrf_debug ( 100 , 'backfrom med_sidata_input' )

      ELSE 
         CYCLE all_domains
      END IF

   END DO all_domains

   CALL set_current_grid_ptr( head_grid )

   !  We are done.

   CALL       wrf_debug (   0 , 'real_em: SUCCESS COMPLETE REAL_EM INIT' )

   CALL wrf_shutdown

   CALL WRFU_Finalize( rc=rc )

END PROGRAM real_data

SUBROUTINE med_sidata_input ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE module_initialize_real
   USE module_optional_input
#if ( WRF_CHEM == 1 )
   USE module_input_chem_data
   USE module_input_chem_bioemiss
!  USE module_input_chem_emissopt3
#endif

   USE module_wps_io_arw
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
   INTEGER                :: idsi , ierr , myproc
   CHARACTER (LEN=256)     :: si_inpname
   CHARACTER (LEN=80)      :: message

   CHARACTER(LEN=19) :: start_date_char , end_date_char , current_date_char , next_date_char

   INTEGER :: time_loop_max , loop, rc
   INTEGER :: julyr , julday 
   INTEGER :: io_form_auxinput1
   INTEGER, EXTERNAL :: use_package
   LOGICAL :: using_binary_wrfsi
   REAL :: gmt
real::t1,t2,t3,t4

   grid%input_from_file = .true.
   grid%input_from_file = .false.

   CALL compute_si_start_and_end ( model_config_rec%start_year  (grid%id) , &
                                   model_config_rec%start_month (grid%id) , &
                                   model_config_rec%start_day   (grid%id) , &
                                   model_config_rec%start_hour  (grid%id) , &
                                   model_config_rec%start_minute(grid%id) , &
                                   model_config_rec%start_second(grid%id) , &
                                   model_config_rec%  end_year  (grid%id) , & 
                                   model_config_rec%  end_month (grid%id) , &
                                   model_config_rec%  end_day   (grid%id) , &
                                   model_config_rec%  end_hour  (grid%id) , &
                                   model_config_rec%  end_minute(grid%id) , &
                                   model_config_rec%  end_second(grid%id) , &
                                   model_config_rec%interval_seconds      , &
                                   model_config_rec%real_data_init_type   , &
                                   start_date_char , end_date_char , time_loop_max )

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

   !!!!!!!  Loop over each time period to process.

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

      !  Open the input file for real.  This is either the "new" one or the "old" one.  The "new" one could have
      !  a suffix for the type of the data format.  Check to see if either is around.

      CALL cpu_time ( t3 )
      WRITE ( wrf_err_message , FMT='(A,A)' )'med_sidata_input: calling open_r_dataset for ', &
                                             TRIM(config_flags%auxinput1_inname)
      CALL wrf_debug ( 100 , wrf_err_message )

      IF (config_flags%auxinput1_inname(1:10) .eq. 'real_input') THEN
         using_binary_wrfsi=.true.
      ENDIF

      CALL nl_get_io_form_auxinput1( 1, io_form_auxinput1 )

      SELECT CASE ( use_package(io_form_auxinput1) )
#if defined(NETCDF) || defined(PNETCDF) || defined(PIO)
      CASE ( IO_NETCDF , IO_PNETCDF , IO_PIO )


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
      CALL cpu_time ( t4 )
      WRITE ( wrf_err_message , FMT='(A,I10,A)' ) 'Timing for input ',NINT(t4-t3) ,' s.'
      CALL wrf_debug( 0, wrf_err_message )

      !  Possible optional SI input.  This sets flags used by init_domain.

      CALL cpu_time ( t3 )
      IF ( loop .EQ. 1 ) THEN
         already_been_here = .FALSE.
         CALL       wrf_debug ( 100 , 'med_sidata_input: calling init_module_optional_input' )
         CALL init_module_optional_input ( grid , config_flags )
      END IF
      CALL       wrf_debug ( 100 , 'med_sidata_input: calling optional_input' )
      CALL optional_input ( grid , idsi , config_flags )

      !  Close this file that is output from the SI and input to this pre-proc.
      CALL       wrf_debug ( 100 , 'med_sidata_input: back from init_domain' )
      CALL close_dataset ( idsi , config_flags , "DATASET=AUXINPUT1" )
#endif
#ifdef INTIO
      CASE ( IO_INTIO )

!      !  Possible optional SI input.  This sets flags used by init_domain.
!
      IF ( loop .EQ. 1 ) THEN
         CALL  wrf_debug (100, 'med_sidata_input: call init_module_optional_input' )
         CALL init_module_optional_input ( grid , config_flags )
      END IF

      IF (using_binary_wrfsi) THEN

        current_date_char(11:11)='_'
!        CALL read_si ( grid, current_date_char )
        CALL wrf_error_fatal("not supporting binary WRFSI in this code")
        current_date_char(11:11)='T'

      ELSE

        write(message,*) 'binary WPS branch'
        CALL wrf_message(message)
        current_date_char(11:11)='_'
        CALL construct_filename4a( si_inpname , config_flags%auxinput1_inname , grid%id , 2 , current_date_char , &
                                     config_flags%io_form_auxinput1 )

        CALL read_wps ( grid, trim(si_inpname), current_date_char, config_flags%num_metgrid_levels )
      ENDIF
#endif
      CASE DEFAULT
        CALL wrf_error_fatal('real: not valid io_form_auxinput1')
      END SELECT

      CALL       wrf_debug ( 100 , 'med_sidata_input: calling init_domain' )
      grid%input_from_file = .true.
      CALL init_domain ( grid )
      CALL cpu_time ( t4 )
      WRITE ( wrf_err_message , FMT='(A,I10,A)' ) 'Timing for processing ',NINT(t4-t3) ,' s.'
      CALL wrf_debug( 0, wrf_err_message )
      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

#if ( WRF_CHEM == 1 )
      IF ( loop == 1 ) THEN
         IF( grid%chem_opt > 0 ) then
           ! Read the chemistry data from a previous wrf forecast (wrfout file)
           IF(grid%chem_in_opt == 1 ) THEN
              message = 'INITIALIZING CHEMISTRY WITH OLD SIMULATION'
              CALL  wrf_message ( message )

              CALL med_read_wrf_chem_input ( grid , config_flags)

              IF(grid%emiss_opt == ECPTEC .or. grid%emiss_opt == GOCART_ECPTEC   &
                                         .or. grid%biomass_burn_opt == BIOMASSB) THEN
                 message = 'READING EMISSIONS DATA OPT 3'
                 CALL  wrf_message ( message )
                 CALL med_read_wrf_chem_emissopt3 ( grid , config_flags)
              END IF

              IF(grid%bio_emiss_opt == 2 ) THEN
                 message = 'READING BEIS3.14 EMISSIONS DATA'
                 CALL  wrf_message ( message )
                 CALL med_read_wrf_chem_bioemiss ( grid , config_flags)
              else IF(grid%bio_emiss_opt == 3 ) THEN !shc
                 message = 'READING MEGAN 2 EMISSIONS DATA'
                 CALL  wrf_message ( message )
                 CALL med_read_wrf_chem_bioemiss ( grid , config_flags)
              END IF

              IF(grid%dust_opt == 1 .or. grid%dmsemis_opt == 1 .or. grid%chem_opt == 300) THEN !shc
                 message = 'READING GOCART BG AND/OR DUST and DMS REF FIELDS'
                 CALL  wrf_message ( message )
                 CALL med_read_wrf_chem_gocart_bg ( grid , config_flags)
              END IF

           ELSEIF(grid%chem_in_opt == 0)then
              ! Generate chemistry data from a idealized vertical profile
              message = 'STARTING WITH BACKGROUND CHEMISTRY '
              CALL  wrf_message ( message )

              CALL input_chem_profile ( grid )

              IF(grid%bio_emiss_opt == 2 ) THEN
                 message = 'READING BEIS3.14 EMISSIONS DATA'
                 CALL  wrf_message ( message )
                 CALL med_read_wrf_chem_bioemiss ( grid , config_flags)
              else IF(grid%bio_emiss_opt == 3 ) THEN !shc
                 message = 'READING MEGAN 2 EMISSIONS DATA'
                 CALL  wrf_message ( message ) 
                 CALL med_read_wrf_chem_bioemiss ( grid , config_flags)
              END IF
              IF(grid%emiss_opt == ECPTEC .or. grid%emiss_opt == GOCART_ECPTEC   &
                                         .or. grid%biomass_burn_opt == BIOMASSB) THEN
                 message = 'READING EMISSIONS DATA OPT 3'
                 CALL  wrf_message ( message )
!                CALL med_read_bin_chem_emissopt3 ( grid , config_flags)
                 CALL med_read_wrf_chem_emissopt3 ( grid , config_flags)
              END IF

              IF(grid%dust_opt == 1 .or. grid%dmsemis_opt == 1 .or. grid%chem_opt == 300) THEN !shc
                 message = 'READING GOCART BG AND/OR DUST and DMS REF FIELDS'
                 CALL  wrf_message ( message )
                 CALL med_read_wrf_chem_gocart_bg ( grid , config_flags)
              END IF

           ELSE
             message = 'RUNNING WITHOUT CHEMISTRY INITIALIZATION'
             CALL  wrf_message ( message )
           END IF
         END IF
      END IF
#endif

      CALL cpu_time ( t3 )
      CALL assemble_output ( grid , config_flags , loop , time_loop_max )
      CALL cpu_time ( t4 )
      WRITE ( wrf_err_message , FMT='(A,I10,A)' ) 'Timing for output ',NINT(t4-t3) ,' s.'
      CALL wrf_debug( 0, wrf_err_message )
      CALL cpu_time ( t2 )
      WRITE ( wrf_err_message , FMT='(A,I4,A,I10,A)' ) 'Timing for loop # ',loop,' = ',NINT(t2-t1) ,' s.'
      CALL wrf_debug( 0, wrf_err_message )

      !  If this is not the last time, we define the next time that we are going to process.

      IF ( loop .NE. time_loop_max ) THEN
         CALL geth_newdate ( current_date_char , start_date_char , loop * model_config_rec%interval_seconds )
         current_date =  current_date_char // '.0000'
         CALL domain_clockprint ( 150, grid, &
                'DEBUG med_sidata_input:  clock before current_date set,' )
         WRITE (wrf_err_message,*) &
           'DEBUG med_sidata_input:  before currTime set, current_date = ',TRIM(current_date)
         CALL wrf_debug ( 150 , wrf_err_message )
         CALL domain_clock_set( grid, current_date(1:19) )
         CALL domain_clockprint ( 150, grid, &
                'DEBUG med_sidata_input:  clock after current_date set,' )
      END IF
      CALL cpu_time ( t1 )
   END DO

END SUBROUTINE med_sidata_input

SUBROUTINE compute_si_start_and_end (  &
   start_year , start_month , start_day , start_hour , start_minute , start_second , &
     end_year ,   end_month ,   end_day ,   end_hour ,   end_minute ,   end_second , &
   interval_seconds , real_data_init_type , &
   start_date_char , end_date_char , time_loop_max )

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
   WRITE (   end_date_char , FMT = '(I4.4,"-",I5.5,"_",I2.2,":",I2.2,":",I2.2)' ) &
             end_year,  end_day,  end_hour,  end_minute,  end_second
#else
   WRITE ( start_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
           start_year,start_month,start_day,start_hour,start_minute,start_second
   WRITE (   end_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
             end_year,  end_month,  end_day,  end_hour,  end_minute,  end_second
#endif

   IF ( end_date_char .LT. start_date_char ) THEN
      CALL wrf_error_fatal( 'Ending date in namelist ' // end_date_char // ' prior to beginning date ' // start_date_char )
   END IF

!  start_date = start_date_char // '.0000'

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
         IF ( ( time_loop_max .EQ. 1 ) .AND. ( start_date_char .NE. end_date_char ) ) THEN
            PRINT *,'You might have set the end time in the namelist.input for the model'         
            PRINT *,'Regional domains require more than one time-period to process, for BC generation'
            CALL wrf_error_fatal ( "Make the end time at least one 'interval_seconds' beyond the start time" )
         END IF
         EXIT loop_count
      END IF
   END DO loop_count
END SUBROUTINE compute_si_start_and_end

SUBROUTINE assemble_output ( grid , config_flags , loop , time_loop_max )

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

   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: ips , ipe , jps , jpe , kps , kpe
   INTEGER :: ijds , ijde , spec_bdy_width
   INTEGER :: i , j , k , idts

   INTEGER :: id1 , interval_seconds , ierr, rc, sst_update, grid_fdda
   INTEGER , SAVE :: id, id2,  id4 
   CHARACTER (LEN=256) :: inpname , bdyname
   CHARACTER(LEN= 4) :: loop_char
   CHARACTER (LEN=256) :: message
character *19 :: temp19
character *24 :: temp24 , temp24b

   REAL , DIMENSION(:,:,:) , ALLOCATABLE , SAVE :: ubdy3dtemp1 , vbdy3dtemp1 , tbdy3dtemp1 , pbdy3dtemp1 , qbdy3dtemp1
   REAL , DIMENSION(:,:,:) , ALLOCATABLE , SAVE :: mbdy2dtemp1
   REAL , DIMENSION(:,:,:) , ALLOCATABLE , SAVE :: ubdy3dtemp2 , vbdy3dtemp2 , tbdy3dtemp2 , pbdy3dtemp2 , qbdy3dtemp2
   REAL , DIMENSION(:,:,:) , ALLOCATABLE , SAVE :: mbdy2dtemp2
   REAL , DIMENSION(:,:,:) , ALLOCATABLE , SAVE :: qn1bdy3dtemp1, qn1bdy3dtemp2, qn2bdy3dtemp1, qn2bdy3dtemp2
real::t1,t2

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

   !  Domain check.  We cannot decompose the domain into pieces that are
   !  too small to manufacture the lateral boundary conditions.

   IF ( ( ipe-ips+2 .LE. spec_bdy_width ) .OR. &
        ( jpe-jps+2 .LE. spec_bdy_width ) ) THEN
     CALL wrf_message( 'The "width" of the lateral boundary conditions must be entirely contained within')
     CALL wrf_message( 'the decomposed patch.                                                    ')
     WRITE(message,fmt='("ips=",i4,", ipe=",i4,", jps=",i4,", jpe=",i4,", spec_bdy_width=",i2)') ips,ipe,jps,jpe,spec_bdy_width
     CALL wrf_message( message )
     CALL wrf_error_fatal( 'Submit the real program again with fewer processors ')
   END IF


   IF ( loop .EQ. 1 ) THEN

      IF ( ( time_loop_max .EQ. 1 )  .OR. ( config_flags%polar ) ) THEN

         !  No need to allocate space since we do not need the lateral boundary data yet
         !  or at all (in case of the polar flag).

      ELSE

         !  This is the space needed to save the current 3d data for use in computing
         !  the lateral boundary tendencies.
   
         IF ( ALLOCATED ( ubdy3dtemp1 ) ) DEALLOCATE ( ubdy3dtemp1 )
         IF ( ALLOCATED ( vbdy3dtemp1 ) ) DEALLOCATE ( vbdy3dtemp1 )
         IF ( ALLOCATED ( tbdy3dtemp1 ) ) DEALLOCATE ( tbdy3dtemp1 )
         IF ( ALLOCATED ( pbdy3dtemp1 ) ) DEALLOCATE ( pbdy3dtemp1 )
         IF ( ALLOCATED ( qbdy3dtemp1 ) ) DEALLOCATE ( qbdy3dtemp1 )
         IF ( ALLOCATED ( mbdy2dtemp1 ) ) DEALLOCATE ( mbdy2dtemp1 )
         IF ( ALLOCATED ( ubdy3dtemp2 ) ) DEALLOCATE ( ubdy3dtemp2 )
         IF ( ALLOCATED ( vbdy3dtemp2 ) ) DEALLOCATE ( vbdy3dtemp2 )
         IF ( ALLOCATED ( tbdy3dtemp2 ) ) DEALLOCATE ( tbdy3dtemp2 )
         IF ( ALLOCATED ( pbdy3dtemp2 ) ) DEALLOCATE ( pbdy3dtemp2 )
         IF ( ALLOCATED ( qbdy3dtemp2 ) ) DEALLOCATE ( qbdy3dtemp2 )
         IF ( ALLOCATED ( mbdy2dtemp2 ) ) DEALLOCATE ( mbdy2dtemp2 )

         ALLOCATE ( ubdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( vbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( tbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( pbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( qbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( mbdy2dtemp1(ims:ime,1:1,    jms:jme) )
         ALLOCATE ( ubdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( vbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( tbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( pbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( qbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( mbdy2dtemp2(ims:ime,1:1,    jms:jme) )

         IF (config_flags%mp_physics.eq.THOMPSONAERO .AND.  config_flags%use_aero_icbc) THEN
            IF ( ALLOCATED ( qn1bdy3dtemp1 ) ) DEALLOCATE ( qn1bdy3dtemp1 )
            IF ( ALLOCATED ( qn2bdy3dtemp1 ) ) DEALLOCATE ( qn2bdy3dtemp1 )
            IF ( ALLOCATED ( qn1bdy3dtemp2 ) ) DEALLOCATE ( qn1bdy3dtemp2 )
            IF ( ALLOCATED ( qn2bdy3dtemp2 ) ) DEALLOCATE ( qn2bdy3dtemp2 )
            ALLOCATE ( qn1bdy3dtemp1(ims:ime,kms:kme,jms:jme) )
            ALLOCATE ( qn2bdy3dtemp1(ims:ime,kms:kme,jms:jme) )
            ALLOCATE ( qn1bdy3dtemp2(ims:ime,kms:kme,jms:jme) )
            ALLOCATE ( qn2bdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         END IF

      END IF

      !  Open the wrfinput file.  From this program, this is an *output* file.

      grid%this_is_an_ideal_run = .FALSE.

      CALL construct_filename1( inpname , 'wrfinput' , grid%id , 2 )
      CALL open_w_dataset ( id1, TRIM(inpname) , grid , config_flags , output_input , "DATASET=INPUT", ierr )
      IF ( ierr .NE. 0 ) THEN
         CALL wrf_error_fatal( 'real: error opening wrfinput for writing' )
      END IF
      CALL output_input ( id1, grid , config_flags , ierr )
      CALL close_dataset ( id1 , config_flags , "DATASET=INPUT" )

      IF ( time_loop_max .NE. 1 ) THEN
         IF(sst_update .EQ. 1)THEN
           CALL construct_filename1( inpname , 'wrflowinp' , grid%id , 2 )
           CALL open_w_dataset ( id4, TRIM(inpname) , grid , config_flags , output_auxinput4 , "DATASET=AUXINPUT4", ierr )
           IF ( ierr .NE. 0 ) THEN
              CALL wrf_error_fatal( 'real: error opening wrflowinp for writing' )
           END IF
           CALL output_auxinput4 ( id4, grid , config_flags , ierr )
         END IF
      END IF

      IF ( ( time_loop_max .EQ. 1 )  .OR. ( config_flags%polar ) ) THEN

         !  No need to couple data since no lateral BCs required.

      ELSE

         !  We need to save the 3d data to compute a difference during the next loop.  Couple the
         !  3d fields with total mu (mub + mu_2) and the stagger-specific map scale factor.
   
         !  u, theta, h, scalars coupled with my; v coupled with mx
         CALL couple ( grid%mu_2 , grid%mub , ubdy3dtemp1 , grid%u_2                 , 'u' , grid%msfuy , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( grid%mu_2 , grid%mub , vbdy3dtemp1 , grid%v_2                 , 'v' , grid%msfvx , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( grid%mu_2 , grid%mub , tbdy3dtemp1 , grid%t_2                 , 't' , grid%msfty , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( grid%mu_2 , grid%mub , pbdy3dtemp1 , grid%ph_2                , 'h' , grid%msfty , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( grid%mu_2 , grid%mub , qbdy3dtemp1 , grid%moist(:,:,:,P_QV)      , 't' , grid%msfty , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
   
         DO j = jps , MIN(jde-1,jpe)
            DO i = ips , MIN(ide-1,ipe)
               mbdy2dtemp1(i,1,j) = grid%mu_2(i,j)
            END DO
         END DO

         IF (config_flags%mp_physics.eq.THOMPSONAERO .AND.  config_flags%use_aero_icbc) THEN
            CALL couple ( grid%mu_2 , grid%mub , qn1bdy3dtemp1 , grid%scalar(:,:,:,P_QNWFA)      , 't' , grid%msfty , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
            CALL couple ( grid%mu_2 , grid%mub , qn2bdy3dtemp1 , grid%scalar(:,:,:,P_QNIFA)      , 't' , grid%msfty , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         END IF

      END IF

      IF(grid_fdda .GE. 1)THEN
         DO j = jps , jpe
            DO k = kps , kpe
               DO i = ips , ipe
                  grid%fdda3d(i,k,j,p_u_ndg_old) = grid%u_2(i,k,j)
                  grid%fdda3d(i,k,j,p_v_ndg_old) = grid%v_2(i,k,j)
                  grid%fdda3d(i,k,j,p_t_ndg_old) = grid%t_2(i,k,j)
                  grid%fdda3d(i,k,j,p_q_ndg_old) = grid%moist(i,k,j,P_QV)
                  grid%fdda3d(i,k,j,p_ph_ndg_old) = grid%ph_2(i,k,j)
               END DO
            END DO
         END DO

         DO j = jps , jpe
            DO i = ips , ipe
               grid%fdda2d(i,1,j,p_mu_ndg_old) = grid%mu_2(i,j)
!              grid%fdda2d(i,1,j,p_t2_ndg_old) = grid%t2(i,j)
!              grid%fdda2d(i,1,j,p_q2_ndg_old) = grid%q2(i,j)
!              grid%fdda2d(i,1,j,p_sn_ndg_old) = grid%snow(i,j)
            END DO
         END DO
      END IF

      IF ( ( time_loop_max .EQ. 1 )  .OR. ( config_flags%polar ) ) THEN

         !  No need to build boundary arrays, since no lateral BCs are being generated.

      ELSE
   
         !  There are 2 components to the lateral boundaries.  First, there is the starting
         !  point of this time period - just the outer few rows and columns.
   
         CALL stuff_bdy     ( ubdy3dtemp1 , grid%u_bxs, grid%u_bxe, grid%u_bys, grid%u_bye, &
                                                              'U' , spec_bdy_width      , &
                                                                    ids , ide , jds , jde , kds , kde , &
                                                                    ims , ime , jms , jme , kms , kme , &
                                                                    ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( vbdy3dtemp1 , grid%v_bxs, grid%v_bxe, grid%v_bys, grid%v_bye, &
                                                              'V' , spec_bdy_width      , &
                                                                    ids , ide , jds , jde , kds , kde , &
                                                                    ims , ime , jms , jme , kms , kme , &
                                                                    ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( tbdy3dtemp1 , grid%t_bxs, grid%t_bxe, grid%t_bys, grid%t_bye, &
                                                              'T' , spec_bdy_width      , &
                                                                    ids , ide , jds , jde , kds , kde , &
                                                                    ims , ime , jms , jme , kms , kme , &
                                                                    ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( pbdy3dtemp1 , grid%ph_bxs, grid%ph_bxe, grid%ph_bys, grid%ph_bye, &
                                                              'W' , spec_bdy_width      , &
                                                                    ids , ide , jds , jde , kds , kde , &
                                                                    ims , ime , jms , jme , kms , kme , &
                                                                    ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( qbdy3dtemp1 , grid%moist_bxs(:,:,:,P_QV), grid%moist_bxe(:,:,:,P_QV),     &
                                            grid%moist_bys(:,:,:,P_QV), grid%moist_bye(:,:,:,P_QV),     &
                                                              'T' , spec_bdy_width      ,               &
                                                                    ids , ide , jds , jde , kds , kde , &
                                                                    ims , ime , jms , jme , kms , kme , &
                                                                    ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( mbdy2dtemp1 , grid%mu_bxs, grid%mu_bxe, grid%mu_bys, grid%mu_bye, &
                                                              'M' , spec_bdy_width      , &
                                                                    ids , ide , jds , jde , 1 , 1 , &
                                                                    ims , ime , jms , jme , 1 , 1 , &
                                                                    ips , ipe , jps , jpe , 1 , 1 )

         IF (config_flags%mp_physics.eq.THOMPSONAERO .AND.  config_flags%use_aero_icbc) THEN
            CALL stuff_bdy     ( qn1bdy3dtemp1 , grid%scalar_bxs(:,:,:,P_QNWFA), grid%scalar_bxe(:,:,:,P_QNWFA),     &
                                            grid%scalar_bys(:,:,:,P_QNWFA), grid%scalar_bye(:,:,:,P_QNWFA),     &
                                                              'T' , spec_bdy_width      ,               &
                                                                    ids , ide , jds , jde , kds , kde , &
                                                                    ims , ime , jms , jme , kms , kme , &
                                                                    ips , ipe , jps , jpe , kps , kpe )
            CALL stuff_bdy     ( qn2bdy3dtemp1 , grid%scalar_bxs(:,:,:,P_QNIFA), grid%scalar_bxe(:,:,:,P_QNIFA),     &
                                            grid%scalar_bys(:,:,:,P_QNIFA), grid%scalar_bye(:,:,:,P_QNIFA),     &
                                                              'T' , spec_bdy_width      ,               &
                                                                    ids , ide , jds , jde , kds , kde , &
                                                                    ims , ime , jms , jme , kms , kme , &
                                                                    ips , ipe , jps , jpe , kps , kpe )
         END IF
      END IF


   ELSE IF ( loop .GT. 1 ) THEN

      IF(sst_update .EQ. 1)THEN
        CALL output_auxinput4 ( id4, grid , config_flags , ierr )
      END IF

      !  Open the boundary and the fdda file.

      IF ( loop .eq. 2 ) THEN
         IF ( (grid%id .eq. 1) .and. ( .NOT. config_flags%polar ) ) THEN
            CALL construct_filename1( bdyname , 'wrfbdy' , grid%id , 2 )
            CALL open_w_dataset ( id, TRIM(bdyname) , grid , config_flags , output_boundary , "DATASET=BOUNDARY", ierr )
            IF ( ierr .NE. 0 ) THEN
               CALL wrf_error_fatal( 'real: error opening wrfbdy for writing' )
            END IF
         END IF
         IF(grid_fdda .GE. 1)THEN
            CALL construct_filename1( inpname , 'wrffdda' , grid%id , 2 )
            CALL open_w_dataset ( id2, TRIM(inpname) , grid , config_flags , output_auxinput10 , "DATASET=AUXINPUT10", ierr )
            IF ( ierr .NE. 0 ) THEN
               CALL wrf_error_fatal( 'real: error opening wrffdda for writing' )
            END IF
         END IF
      ELSE
         IF ( .NOT. domain_clockisstoptime(grid) ) THEN
            CALL domain_clockadvance( grid )
            CALL domain_clockprint ( 150, grid, &
                   'DEBUG assemble_output:  clock after ClockAdvance,' )
         END IF
      END IF

      IF ( config_flags%polar ) THEN

         !  No need to couple fields, since no lateral BCs are required.

      ELSE
   
         !  Couple this time period's data with total mu, and save it in the *bdy3dtemp2 arrays.
   
         !  u, theta, h, scalars coupled with my; v coupled with mx
         CALL couple ( grid%mu_2 , grid%mub , ubdy3dtemp2 , grid%u_2                 , 'u' , grid%msfuy , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( grid%mu_2 , grid%mub , vbdy3dtemp2 , grid%v_2                 , 'v' , grid%msfvx , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( grid%mu_2 , grid%mub , tbdy3dtemp2 , grid%t_2                 , 't' , grid%msfty , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( grid%mu_2 , grid%mub , pbdy3dtemp2 , grid%ph_2                , 'h' , grid%msfty , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( grid%mu_2 , grid%mub , qbdy3dtemp2 , grid%moist(:,:,:,P_QV)      , 't' , grid%msfty , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
   
         DO j = jps , jpe
            DO i = ips , ipe
               mbdy2dtemp2(i,1,j) = grid%mu_2(i,j)
            END DO
         END DO

         IF (config_flags%mp_physics.eq.THOMPSONAERO .AND.  config_flags%use_aero_icbc) THEN
            CALL couple ( grid%mu_2 , grid%mub , qn1bdy3dtemp2 , grid%scalar(:,:,:,P_QNWFA)      , 't' , grid%msfty , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
            CALL couple ( grid%mu_2 , grid%mub , qn2bdy3dtemp2 , grid%scalar(:,:,:,P_QNIFA)      , 't' , grid%msfty , &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         END IF

      END IF

      IF(grid_fdda .GE. 1)THEN
         DO j = jps , jpe
            DO k = kps , kpe
               DO i = ips , ipe
                  grid%fdda3d(i,k,j,p_u_ndg_new) = grid%u_2(i,k,j)
                  grid%fdda3d(i,k,j,p_v_ndg_new) = grid%v_2(i,k,j)
                  grid%fdda3d(i,k,j,p_t_ndg_new) = grid%t_2(i,k,j)
                  grid%fdda3d(i,k,j,p_q_ndg_new) = grid%moist(i,k,j,P_QV)
                  grid%fdda3d(i,k,j,p_ph_ndg_new) = grid%ph_2(i,k,j)
               END DO
            END DO
         END DO

         DO j = jps , jpe
            DO i = ips , ipe
               grid%fdda2d(i,1,j,p_mu_ndg_new) = grid%mu_2(i,j)
!              grid%fdda2d(i,1,j,p_t2_ndg_new) = grid%t2(i,j)
!              grid%fdda2d(i,1,j,p_q2_ndg_new) = grid%q2(i,j)
!              grid%fdda2d(i,1,j,p_sn_ndg_new) = grid%snow(i,j)
            END DO
         END DO
      END IF

      IF ( config_flags%polar ) THEN

         !  No need to build boundary arrays, since no lateral BCs are being generated.

      ELSE

         !  During all of the loops after the first loop, we first compute the boundary
         !  tendencies with the current data values (*bdy3dtemp2 arrays) and the previously 
         !  saved information stored in the *bdy3dtemp1 arrays.
   
         CALL stuff_bdytend ( ubdy3dtemp2 , ubdy3dtemp1 , REAL(interval_seconds) ,                 &
                                                               grid%u_btxs, grid%u_btxe,     &
                                                               grid%u_btys, grid%u_btye,     &
                                                               'U' , &
                                                               spec_bdy_width      , &
                                                               ids , ide , jds , jde , kds , kde , &
                                                               ims , ime , jms , jme , kms , kme , &
                                                               ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( vbdy3dtemp2 , vbdy3dtemp1 , REAL(interval_seconds) ,                 &
                                                               grid%v_btxs, grid%v_btxe,     &
                                                               grid%v_btys, grid%v_btye,     &
                                                               'V' , &
                                                               spec_bdy_width      , &
                                                               ids , ide , jds , jde , kds , kde , &
                                                               ims , ime , jms , jme , kms , kme , &
                                                               ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( tbdy3dtemp2 , tbdy3dtemp1 , REAL(interval_seconds) ,                 &
                                                               grid%t_btxs, grid%t_btxe,     &
                                                               grid%t_btys, grid%t_btye,     &
                                                               'T' , &
                                                               spec_bdy_width      , &
                                                               ids , ide , jds , jde , kds , kde , &
                                                               ims , ime , jms , jme , kms , kme , &
                                                               ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( pbdy3dtemp2 , pbdy3dtemp1 , REAL(interval_seconds) ,                 &
                                                               grid%ph_btxs, grid%ph_btxe,   &
                                                               grid%ph_btys, grid%ph_btye,   &
                                                               'W' , &
                                                               spec_bdy_width      , &
                                                               ids , ide , jds , jde , kds , kde , &
                                                               ims , ime , jms , jme , kms , kme , &
                                                               ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( qbdy3dtemp2 , qbdy3dtemp1 , REAL(interval_seconds) ,                 &
                                                               grid%moist_btxs(:,:,:,P_QV), grid%moist_btxe(:,:,:,P_QV), &
                                                               grid%moist_btys(:,:,:,P_QV), grid%moist_btye(:,:,:,P_QV), &
                                                               'T' , &
                                                               spec_bdy_width      , &
                                                               ids , ide , jds , jde , kds , kde , &
                                                               ims , ime , jms , jme , kms , kme , &
                                                               ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( mbdy2dtemp2 , mbdy2dtemp1 , REAL(interval_seconds) ,                 &
                                                               grid%mu_btxs, grid%mu_btxe,   &
                                                               grid%mu_btys, grid%mu_btye,   &
                                                               'M' , &
                                                               spec_bdy_width      , &
                                                               ids , ide , jds , jde , 1 , 1 , &
                                                               ims , ime , jms , jme , 1 , 1 , &
                                                               ips , ipe , jps , jpe , 1 , 1 )
         IF (config_flags%mp_physics.eq.THOMPSONAERO .AND.  config_flags%use_aero_icbc) THEN
            CALL stuff_bdytend ( qn1bdy3dtemp2 , qn1bdy3dtemp1 , REAL(interval_seconds) ,                 &
                                                               grid%scalar_btxs(:,:,:,P_QNWFA), grid%scalar_btxe(:,:,:,P_QNWFA), &
                                                               grid%scalar_btys(:,:,:,P_QNWFA), grid%scalar_btye(:,:,:,P_QNWFA), &
                                                               'T' , &
                                                               spec_bdy_width      , &
                                                               ids , ide , jds , jde , kds , kde , &
                                                               ims , ime , jms , jme , kms , kme , &
                                                               ips , ipe , jps , jpe , kps , kpe )
            CALL stuff_bdytend ( qn2bdy3dtemp2 , qn2bdy3dtemp1 , REAL(interval_seconds) ,                 &
                                                               grid%scalar_btxs(:,:,:,P_QNIFA), grid%scalar_btxe(:,:,:,P_QNIFA), &
                                                               grid%scalar_btys(:,:,:,P_QNIFA), grid%scalar_btye(:,:,:,P_QNIFA), &
                                                               'T' , &
                                                               spec_bdy_width      , &
                                                               ids , ide , jds , jde , kds , kde , &
                                                               ims , ime , jms , jme , kms , kme , &
                                                               ips , ipe , jps , jpe , kps , kpe )
         END IF
      END IF

      !  Both pieces of the boundary data are now available to be written (initial time and tendency).
      !  This looks ugly, these date shifting things.  What's it for?  We want the "Times" variable
      !  in the lateral BDY file to have the valid times of when the initial fields are written.
      !  That's what the loop-2 thingy is for with the start date.  We increment the start_date so
      !  that the starting time in the attributes is the second time period.  Why you may ask.  I
      !  agree, why indeed.

      CALL domain_clockprint ( 150, grid, &
             'DEBUG assemble_output:  clock before 1st current_date set,' )
      WRITE (wrf_err_message,*) &
        'DEBUG assemble_output:  before 1st currTime set, current_date = ',TRIM(current_date)
      CALL wrf_debug ( 150 , wrf_err_message )
      CALL domain_clock_set( grid, current_date(1:19) )
      CALL domain_clockprint ( 150, grid, &
             'DEBUG assemble_output:  clock after 1st current_date set,' )

      temp24= current_date
      temp24b=start_date
      start_date = current_date
      CALL geth_newdate ( temp19 , temp24b(1:19) , (loop-2) * model_config_rec%interval_seconds )
      current_date = temp19 //  '.0000'
      CALL domain_clockprint ( 150, grid, &
             'DEBUG assemble_output:  clock before 2nd current_date set,' )
      WRITE (wrf_err_message,*) &
        'DEBUG assemble_output:  before 2nd currTime set, current_date = ',TRIM(current_date)
      CALL wrf_debug ( 150 , wrf_err_message )
      CALL domain_clock_set( grid, current_date(1:19) )
      CALL domain_clockprint ( 150, grid, &
             'DEBUG assemble_output:  clock after 2nd current_date set,' )

      IF ( config_flags%polar ) THEN

         !  No need to ouput boundary data for polar cases.

      ELSE

         !  Output boundary file.
   
         IF(grid%id .EQ. 1)THEN
           print *,'LBC valid between these times ',current_date, ' ',start_date
           CALL output_boundary ( id, grid , config_flags , ierr )
         END IF

      END IF

      !  Output gridded/analysis FDDA file.

      IF(grid_fdda .GE. 1) THEN
         CALL output_auxinput10 ( id2, grid , config_flags , ierr )
      END IF

      current_date = temp24
      start_date = temp24b
      CALL domain_clockprint ( 150, grid, &
             'DEBUG assemble_output:  clock before 3rd current_date set,' )
      WRITE (wrf_err_message,*) &
        'DEBUG assemble_output:  before 3rd currTime set, current_date = ',TRIM(current_date)
      CALL wrf_debug ( 150 , wrf_err_message )
      CALL domain_clock_set( grid, current_date(1:19) )
      CALL domain_clockprint ( 150, grid, &
             'DEBUG assemble_output:  clock after 3rd current_date set,' )

      !  OK, for all of the loops, we output the initialzation data, which would allow us to
      !  start the model at any of the available analysis time periods.

      IF ( config_flags%all_ic_times ) THEN
         CALL construct_filename2a ( inpname , 'wrfinput_d<domain>.<date>' , grid%id , 2 , TRIM(current_date) )
         CALL open_w_dataset ( id1, inpname , grid , config_flags , output_input , "DATASET=INPUT", ierr )
         IF ( ierr .NE. 0 ) THEN
            CALL wrf_error_fatal( 'real: error opening' // inpname // ' for writing' )
         END IF
         CALL output_input ( id1, grid , config_flags , ierr )
         CALL close_dataset ( id1 , config_flags , "DATASET=INPUT" )
      END IF

      !  Is this or is this not the last time time?  We can remove some unnecessary
      !  stores if it is not.

      IF     ( loop .LT. time_loop_max ) THEN

         IF ( config_flags%polar ) THEN
  
            !  No need to swap old for new for the boundary data, it is not required.

         ELSE

            !  We need to save the 3d data to compute a difference during the next loop.  Couple the
            !  3d fields with total mu (mub + mu_2) and the stagger-specific map scale factor.
            !  We load up the boundary data again for use in the next loop.
   
            DO j = jps , jpe
               DO k = kps , kpe
                  DO i = ips , ipe
                     ubdy3dtemp1(i,k,j) = ubdy3dtemp2(i,k,j)
                     vbdy3dtemp1(i,k,j) = vbdy3dtemp2(i,k,j)
                     tbdy3dtemp1(i,k,j) = tbdy3dtemp2(i,k,j)
                     pbdy3dtemp1(i,k,j) = pbdy3dtemp2(i,k,j)
                     qbdy3dtemp1(i,k,j) = qbdy3dtemp2(i,k,j)
                  END DO
               END DO
            END DO
   
            DO j = jps , jpe
               DO i = ips , ipe
                  mbdy2dtemp1(i,1,j) = mbdy2dtemp2(i,1,j)
               END DO
            END DO

            IF (config_flags%mp_physics.eq.THOMPSONAERO .AND.  config_flags%use_aero_icbc) THEN
               DO j = jps , jpe
                  DO k = kps , kpe
                     DO i = ips , ipe
                        qn1bdy3dtemp1(i,k,j) = qn1bdy3dtemp2(i,k,j)
                        qn2bdy3dtemp1(i,k,j) = qn2bdy3dtemp2(i,k,j)
                     END DO
                  END DO
               END DO
            END IF

         END IF

         IF(grid_fdda .GE. 1)THEN
            DO j = jps , jpe
               DO k = kps , kpe
                  DO i = ips , ipe
                     grid%fdda3d(i,k,j,p_u_ndg_old) = grid%fdda3d(i,k,j,p_u_ndg_new)
                     grid%fdda3d(i,k,j,p_v_ndg_old) = grid%fdda3d(i,k,j,p_v_ndg_new)
                     grid%fdda3d(i,k,j,p_t_ndg_old) = grid%fdda3d(i,k,j,p_t_ndg_new)
                     grid%fdda3d(i,k,j,p_q_ndg_old) = grid%fdda3d(i,k,j,p_q_ndg_new)
                     grid%fdda3d(i,k,j,p_ph_ndg_old) = grid%fdda3d(i,k,j,p_ph_ndg_new)
                  END DO
               END DO
            END DO

            DO j = jps , jpe
               DO i = ips , ipe
                  grid%fdda2d(i,1,j,p_mu_ndg_old) = grid%fdda2d(i,1,j,p_mu_ndg_new)
!                 grid%fdda2d(i,1,j,p_t2_ndg_old) = grid%fdda2d(i,1,j,p_t2_ndg_new)
!                 grid%fdda2d(i,1,j,p_q2_ndg_old) = grid%fdda2d(i,1,j,p_q2_ndg_new)
!                 grid%fdda2d(i,1,j,p_sn_ndg_old) = grid%fdda2d(i,1,j,p_sn_ndg_new)
               END DO
            END DO
         END IF

         IF ( config_flags%polar ) THEN

            !  No need to build boundary arrays, since no lateral BCs are being generated.

         ELSE

            !  There are 2 components to the lateral boundaries.  First, there is the starting
            !  point of this time period - just the outer few rows and columns.
   
            CALL stuff_bdy     ( ubdy3dtemp1 , grid%u_bxs, grid%u_bxe, grid%u_bys, grid%u_bye, &
                                                                 'U' , spec_bdy_width      , &
                                                                       ids , ide , jds , jde , kds , kde , &
                                                                       ims , ime , jms , jme , kms , kme , &
                                                                       ips , ipe , jps , jpe , kps , kpe )
            CALL stuff_bdy     ( vbdy3dtemp1 , grid%v_bxs, grid%v_bxe, grid%v_bys, grid%v_bye, &
                                                                 'V' , spec_bdy_width      , &
                                                                       ids , ide , jds , jde , kds , kde , &
                                                                       ims , ime , jms , jme , kms , kme , &
                                                                       ips , ipe , jps , jpe , kps , kpe )
            CALL stuff_bdy     ( tbdy3dtemp1 , grid%t_bxs, grid%t_bxe, grid%t_bys, grid%t_bye, &
                                                                 'T' , spec_bdy_width      , &
                                                                       ids , ide , jds , jde , kds , kde , &
                                                                       ims , ime , jms , jme , kms , kme , &
                                                                       ips , ipe , jps , jpe , kps , kpe )
            CALL stuff_bdy     ( pbdy3dtemp1 , grid%ph_bxs, grid%ph_bxe, grid%ph_bys, grid%ph_bye, &
                                                                 'W' , spec_bdy_width      , &
                                                                       ids , ide , jds , jde , kds , kde , &
                                                                       ims , ime , jms , jme , kms , kme , &
                                                                       ips , ipe , jps , jpe , kps , kpe )
            CALL stuff_bdy     ( qbdy3dtemp1 , grid%moist_bxs(:,:,:,P_QV), grid%moist_bxe(:,:,:,P_QV),     &
                                               grid%moist_bys(:,:,:,P_QV), grid%moist_bye(:,:,:,P_QV),     &
                                                                 'T' , spec_bdy_width      ,               &
                                                                       ids , ide , jds , jde , kds , kde , &
                                                                       ims , ime , jms , jme , kms , kme , &
                                                                       ips , ipe , jps , jpe , kps , kpe )
            CALL stuff_bdy     ( mbdy2dtemp1 , grid%mu_bxs, grid%mu_bxe, grid%mu_bys, grid%mu_bye, &
                                                                 'M' , spec_bdy_width      , &
                                                                       ids , ide , jds , jde , 1 , 1 , &
                                                                       ims , ime , jms , jme , 1 , 1 , &
                                                                       ips , ipe , jps , jpe , 1 , 1 )

            IF (config_flags%mp_physics.eq.THOMPSONAERO .AND.  config_flags%use_aero_icbc) THEN
               CALL stuff_bdy     ( qn1bdy3dtemp1 , grid%scalar_bxs(:,:,:,P_QNWFA), grid%scalar_bxe(:,:,:,P_QNWFA),     &
                                                    grid%scalar_bys(:,:,:,P_QNWFA), grid%scalar_bye(:,:,:,P_QNWFA),     &
                                                                 'T' , spec_bdy_width      ,               &
                                                                       ids , ide , jds , jde , kds , kde , &
                                                                       ims , ime , jms , jme , kms , kme , &
                                                                       ips , ipe , jps , jpe , kps , kpe )
               CALL stuff_bdy     ( qn2bdy3dtemp1 , grid%scalar_bxs(:,:,:,P_QNIFA), grid%scalar_bxe(:,:,:,P_QNIFA),     &
                                                    grid%scalar_bys(:,:,:,P_QNIFA), grid%scalar_bye(:,:,:,P_QNIFA),     &
                                                                 'T' , spec_bdy_width      ,               &
                                                                       ids , ide , jds , jde , kds , kde , &
                                                                       ims , ime , jms , jme , kms , kme , &
                                                                       ips , ipe , jps , jpe , kps , kpe )
            END IF
   
         END IF

      ELSE IF ( loop .EQ. time_loop_max ) THEN

         !  If this is the last time through here, we need to close the files.

         IF ( config_flags%polar ) THEN

            !  No need to close the boundary file, it was never used.

         ELSE
            IF(grid%id .EQ. 1) THEN
               CALL close_dataset ( id , config_flags , "DATASET=BOUNDARY" )
            END IF
         END IF

         IF(grid_fdda .GE. 1) THEN
            CALL close_dataset ( id2 , config_flags , "DATASET=AUXINPUT10" )
         END IF

         IF(sst_update .EQ. 1)THEN
            CALL close_dataset ( id4 , config_flags , "DATASET=AUXINPUT4" )
         END IF

      END IF

   END IF

END SUBROUTINE assemble_output
