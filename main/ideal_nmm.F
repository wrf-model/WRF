!  Create an initial data set for the WRF model based on an ideal condition
!  This program is specifically set up for the NMM core.

PROGRAM ideal_nmm

   USE module_machine
   USE module_domain
   USE module_initialize_ideal
   USE module_io_domain
   USE module_driver_constants
   USE module_configure
   USE module_timing
   USE module_check_a_mundo
#if ( WRF_CHEM == 1 )
   USE module_input_chem_data
   USE module_input_chem_bioemiss
#endif
   USE module_utility
#ifdef DM_PARALLEL
   USE module_dm
#endif

   IMPLICIT NONE

   REAL    :: time , bdyfrq

   INTEGER :: loop , levels_to_process , debug_level


   TYPE(domain) , POINTER :: null_domain
   TYPE(domain) , POINTER :: grid
   TYPE (grid_config_rec_type)              :: config_flags
   INTEGER                :: number_at_same_level

   INTEGER :: max_dom, domain_id
   INTEGER :: idum1, idum2 
#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
!   INTEGER, PARAMETER      :: configbuflen = 2*1024
   INTEGER, PARAMETER      :: configbuflen = 4*CONFIG_BUF_LEN
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif

   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: ips , ipe , jps , jpe , kps , kpe
   INTEGER :: ijds , ijde , spec_bdy_width
   INTEGER :: i , j , k , idts

#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

   CHARACTER (LEN=80)     :: message

   INTEGER :: start_year , start_month , start_day 
   INTEGER :: start_hour , start_minute , start_second
   INTEGER :: end_year ,   end_month ,   end_day ,   &
              end_hour ,   end_minute ,   end_second
   INTEGER :: interval_seconds , real_data_init_type
   INTEGER :: time_loop_max , time_loop, rc
   REAL    :: t1,t2

#include "version_decl"

   INTERFACE
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping
   END INTERFACE

   !  Define the name of this program (program_name defined in module_domain)

   program_name = "IDEAL_HWRF " // TRIM(release_version) // " PREPROCESSOR"

#ifdef DM_PARALLEL
   CALL disable_quilting
#endif

!       CALL start()

   !  Initialize the modules used by the WRF system.  
   !  Many of the CALLs made from the
   !  init_modules routine are NO-OPs.  Typical initializations 
   !  are: the size of a
   !  REAL, setting the file handles to a pre-use value, defining moisture and
   !  chemistry indices, etc.

   CALL       wrf_debug ( 100 , 'ideal_hwrf: calling init_modules ' )

!!!!   CALL init_modules
   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)
   CALL WRFU_Initialize( defaultCalKind=WRFU_CAL_GREGORIAN, rc=rc )
   CALL init_modules(2)   ! Phase 2 resumes after MPI_INIT() (if it is called)

   !  The configuration switches mostly come from the NAMELIST input.

#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
      write(message,*) 'call initial_config'
      CALL wrf_message ( message )
      CALL initial_config
   ENDIF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize
#else
   CALL initial_config
#endif

   CALL check_nml_consistency
   CALL set_physics_rconfigs

   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   CALL  wrf_message ( program_name )

   !  Allocate the space for the mother of all domains.

   NULLIFY( null_domain )
   CALL  wrf_debug ( 100 , 'ideal_hwrf: calling alloc_and_configure_domain ' )
   CALL alloc_and_configure_domain ( domain_id  = 1           , &
                                     grid       = head_grid   , &
                                     parent     = null_domain , &
                                     kid        = -1            )

   grid => head_grid

#include "deref_kludge.h"
   CALL Setup_Timekeeping ( grid )
   CALL domain_clock_set( grid, &
                          time_step_seconds=model_config_rec%interval_seconds )
   CALL wrf_debug ( 100 , 'ideal_hwrf: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( grid%id , idum1, idum2 )

   CALL     wrf_debug ( 100 , 'ideal_hwrf: calling model_to_grid_config_rec ' )

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   write(message,*) 'after model_to_grid_config_rec, e_we, e_sn are: ', &
                    config_flags%e_we, config_flags%e_sn
   CALL wrf_message(message)

   !  Initialize the WRF IO: open files, init file handles, etc.

   CALL       wrf_debug ( 100 , 'ideal_hwrf: calling init_wrfio' )
   CALL init_wrfio

!  Some of the configuration values may have been modified from the initial READ
!  of the NAMELIST, so we re-broadcast the configuration records.

#ifdef DM_PARALLEL
   CALL wrf_debug ( 100 , 'ideal_hwrf: re-broadcast the configuration records' )
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
#endif

   !   No looping in this layer.  

   CALL med_sidata_input ( grid , config_flags )

   !  We are done.

   CALL       wrf_debug (   0 , 'ideal_hwrf: SUCCESS COMPLETE IDEAL_HWRF INIT' )

#ifdef DM_PARALLEL
    CALL wrf_dm_shutdown
#endif

   CALL WRFU_Finalize( rc=rc )

END PROGRAM ideal_nmm

SUBROUTINE med_sidata_input ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE module_initialize_ideal
   USE module_optional_input
#if ( WRF_CHEM == 1 )
   USE module_input_chem_data
   USE module_input_chem_bioemiss
#endif

   USE module_si_io_nmm

   USE module_date_time

   IMPLICIT NONE


  ! Interface 
   INTERFACE
     SUBROUTINE start_domain ( grid , allowed_to_read )
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
   CHARACTER (LEN=132)     :: message

   CHARACTER(LEN=19) :: start_date_char , end_date_char , &
                        current_date_char , next_date_char

   INTEGER :: time_loop_max , loop
   INTEGER :: julyr , julday , LEN

   INTEGER :: io_form_auxinput1
   INTEGER, EXTERNAL :: use_package

   LOGICAL :: using_binary_wrfsi

   REAL :: gmt
   REAL :: t1,t2

   INTEGER :: numx_sm_levels_input,numx_st_levels_input
   REAL,DIMENSION(100) :: smx_levels_input,stx_levels_input


#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

#if ( HWRF == 1 )
  ! Sam Says:

  ! The *INIT arrays are used to read init data written out by hwrf_prep_hybrid
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::TINIT,UINIT,VINIT,QINIT,CWMINIT
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::PINIT
  REAL,ALLOCATABLE,DIMENSION(:,:)::PDINIT

  ! The *B arrays are used to read boundary data written out by hwrf_prep_hybrid
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::TB,UB,VB,QB,CWMB
  REAL,ALLOCATABLE,DIMENSION(:,:)::PDB

  INTEGER :: KB, LM, IM, JM, iunit_gfs, N

  integer :: i,j,k
  LOGICAL,EXTERNAL :: WRF_DM_ON_MONITOR
  integer :: ids,ide, jds,jde, kds,kde
  integer :: ims,ime, jms,jme, kms,kme
  integer :: its,ite, jts,jte, kts,kte

  integer :: ioerror
#endif

#include "deref_kludge.h"


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

   !  Here we define the initial time to process, for later use by the code.

   current_date_char = start_date_char
!   start_date = start_date_char // '.0000'
   start_date = start_date_char 
   current_date = start_date

   CALL nl_set_bdyfrq ( grid%id , REAL(model_config_rec%interval_seconds) )

   !  Loop over each time period to process.

   write(message,*) 'time_loop_max: ', time_loop_max
   CALL wrf_message(message)
   DO loop = 1 , time_loop_max

     internal_time_loop=loop
                                                                                                                                              
      write(message,*) 'loop=', loop
      CALL wrf_message(message)
                                                                                                                                              
      write(message,*) '-----------------------------------------------------------'
      CALL wrf_message(message)
                      
      write(message,*) ' '
      CALL wrf_message(message)
      write(message,'(A,A,A,I2,A,I2)') ' Current date being processed: ', &
        current_date, ', which is loop #',loop,' out of ',time_loop_max
      CALL wrf_message(message)

      !  After current_date has been set, fill in the julgmt stuff.

      CALL geth_julgmt ( config_flags%julyr , config_flags%julday , &
                                              config_flags%gmt )

      !  Now that the specific Julian info is available, 
      !  save these in the model config record.

      CALL nl_set_gmt (grid%id, config_flags%gmt)
      CALL nl_set_julyr (grid%id, config_flags%julyr)
      CALL nl_set_julday (grid%id, config_flags%julday)

      CALL nl_get_io_form_auxinput1( 1, io_form_auxinput1 )
      using_binary_wrfsi=.false.
       
       
      write(message,*) 'TRIM(config_flags%auxinput1_inname): ', TRIM(config_flags%auxinput1_inname)
      CALL wrf_message(message)
       
#if ( HWRF == 1 )
     ifph_onlyfirst: if(.not.grid%use_prep_hybrid .or. loop==1) then
#endif
      IF (config_flags%auxinput1_inname(1:10) .eq. 'real_input') THEN
         using_binary_wrfsi=.true.
      ENDIF

      SELECT CASE ( use_package(io_form_auxinput1) )
#if defined(NETCDF) 
      CASE ( IO_NETCDF )

      !  Open the wrfinput file.

        current_date_char(11:11)='_'
 
       WRITE ( wrf_err_message , FMT='(A,A)' )'med_sidata_input: calling open_r_dataset for ',TRIM(config_flags%auxinput1_inname)
       CALL wrf_debug ( 100 , wrf_err_message )
       IF ( config_flags%auxinput1_inname(1:8) .NE. 'wrf_real' ) THEN
          CALL construct_filename4a( si_inpname , config_flags%auxinput1_inname , grid%id , 2 , current_date_char , &
                                     config_flags%io_form_auxinput1 )
       ELSE
          CALL construct_filename2a( si_inpname , config_flags%auxinput1_inname , grid%id , 2 , current_date_char )
       END IF
       CALL open_r_dataset ( idsi, TRIM(si_inpname) , grid , config_flags , "DATASET=AUXINPUT1", ierr )
 
       IF ( ierr .NE. 0 ) THEN
          CALL wrf_error_fatal( 'error opening ' // TRIM(si_inpname) // ' for input; bad date in namelist or file not in directory' )
       ENDIF

      !  Input data.

      CALL wrf_debug (100, 'med_sidata_input: call input_auxinput1_wrf')

      CALL input_auxinput1 ( idsi, grid, config_flags, ierr )

      !  Possible optional SI input.  This sets flags used by init_domain.

      IF ( loop .EQ. 1 ) THEN
         CALL  wrf_debug (100, 'med_sidata_input: call init_module_optional_input' )
         CALL init_module_optional_input ( grid , config_flags )
      CALL wrf_debug ( 100 , 'med_sidata_input: calling optional_input' )
!
      CALL optional_input ( grid , idsi , config_flags )
	write(0,*) 'maxval st_input(1) within ideal_hwrf: ', maxval(st_input(:,1,:))
      END IF
!
      CALL close_dataset ( idsi , config_flags , "DATASET=AUXINPUT1" )

#endif
#ifdef INTIO
      CASE ( IO_INTIO )

      !  Possible optional SI input.  This sets flags used by init_domain.

      IF ( loop .EQ. 1 ) THEN
         CALL  wrf_debug (100, 'med_sidata_input: call init_module_optional_input' )
         CALL init_module_optional_input ( grid , config_flags )
      END IF

      IF (using_binary_wrfsi) THEN

        current_date_char(11:11)='_'
        CALL read_si ( grid, current_date_char )
        current_date_char(11:11)='T'

      ELSE
                                                                                                                                              
        write(message,*) 'binary WPS branch'
        CALL wrf_message(message)
        current_date_char(11:11)='_'
        CALL construct_filename4a( si_inpname , config_flags%auxinput1_inname , grid%id , 2 , current_date_char , &
                                     config_flags%io_form_auxinput1 )
        CALL read_wps ( grid, trim(si_inpname), current_date_char, config_flags%num_metgrid_levels )
!!! bogus set some flags??
      flag_metgrid=1
      flag_soilhgt=1


          ENDIF

#endif
      CASE DEFAULT
        CALL wrf_error_fatal('ideal_hwrf: not valid io_form_auxinput1')
      END SELECT
#if ( HWRF == 1 )
     endif ifph_onlyfirst
#endif

      grid%islope=1
      grid%vegfra=grid%vegfrc
      grid%dfrlg=grid%dfl/9.81

      grid%isurban=1
      grid%isoilwater=14

      !  Initialize the mother domain for this time period with input data.

      CALL wrf_debug ( 100 , 'med_sidata_input: calling init_domain' )
      grid%input_from_file = .true.

      CALL init_domain ( grid )

#if ( HWRF == 1 )
     read_phinit: if(grid%use_prep_hybrid) then
#if defined(DM_PARALLEL)
        if(.not. wrf_dm_on_monitor()) then
           call wrf_error_fatal('ideal_hwrf: in use_prep_hybrid mode, threading and mpi are forbidden.')
        endif
#endif

        ph_loop1: if(loop==1) then

           ! determine kds, ids, jds
           SELECT CASE ( model_data_order )
           CASE ( DATA_ORDER_ZXY )
              kds = grid%sd31 ; kde = grid%ed31 ;
              ids = grid%sd32 ; ide = grid%ed32 ;
              jds = grid%sd33 ; jde = grid%ed33 ;

              kms = grid%sm31 ; kme = grid%em31 ;
              ims = grid%sm32 ; ime = grid%em32 ;
              jms = grid%sm33 ; jme = grid%em33 ;

              kts = grid%sp31 ; kte = grid%ep31 ; ! tile is entire patch
              its = grid%sp32 ; ite = grid%ep32 ; ! tile is entire patch
              jts = grid%sp33 ; jte = grid%ep33 ; ! tile is entire patch

           CASE ( DATA_ORDER_XYZ )
              ids = grid%sd31 ; ide = grid%ed31 ;
              jds = grid%sd32 ; jde = grid%ed32 ;
              kds = grid%sd33 ; kde = grid%ed33 ;

              ims = grid%sm31 ; ime = grid%em31 ;
              jms = grid%sm32 ; jme = grid%em32 ;
              kms = grid%sm33 ; kme = grid%em33 ;

              its = grid%sp31 ; ite = grid%ep31 ; ! tile is entire patch
              jts = grid%sp32 ; jte = grid%ep32 ; ! tile is entire patch
              kts = grid%sp33 ; kte = grid%ep33 ; ! tile is entire patch

           CASE ( DATA_ORDER_XZY )
              ids = grid%sd31 ; ide = grid%ed31 ;
              kds = grid%sd32 ; kde = grid%ed32 ;
              jds = grid%sd33 ; jde = grid%ed33 ;

              ims = grid%sm31 ; ime = grid%em31 ;
              kms = grid%sm32 ; kme = grid%em32 ;
              jms = grid%sm33 ; jme = grid%em33 ;

              its = grid%sp31 ; ite = grid%ep31 ; ! tile is entire patch
              kts = grid%sp32 ; kte = grid%ep32 ; ! tile is entire patch
              jts = grid%sp33 ; jte = grid%ep33 ; ! tile is entire patch

           END SELECT
           ! Allocate 3D initialization arrays:
           call wrf_message('ALLOCATE PREP_HYBRID INIT ARRAYS')
           ALLOCATE ( TINIT  (ids:(ide-1),kds:(kde-1)  ,jds:(jde-1)) )
           ALLOCATE ( PINIT  (ids:(ide-1),kds:kde      ,jds:(jde-1)) )
           ALLOCATE ( UINIT  (ids:(ide-1),kds:(kde-1)  ,jds:(jde-1)) )
           ALLOCATE ( VINIT  (ids:(ide-1),kds:(kde-1)  ,jds:(jde-1)) )
           ALLOCATE ( QINIT  (ids:(ide-1),kds:(kde-1)  ,jds:(jde-1)) )
           ALLOCATE ( CWMINIT(ids:(ide-1),kds:(kde-1)  ,jds:(jde-1)) )
           ALLOCATE ( PDINIT (ids:(ide-1),              jds:(jde-1)) )

           REWIND 900
           READ(900,iostat=ioerror) PDINIT,TINIT,QINIT,CWMINIT,UINIT,VINIT,PINIT
           if(ioerror/=0) then
              call wrf_error_fatal('Unable to read MAKBND output from unit 900.')
           endif
           WRITE(0,*) 'U V T AT 10 10 10 ',UINIT(10,10,10),VINIT(10,10,10),TINIT(10,10,10)
           ! Switch from IKJ to IJK ordering
           DO I = ids,ide-1
              DO J = jds,jde-1
                 grid%pd(I,J) = PDINIT(I,J)
                 DO K = kds,kde-1
                    grid%q2(I,J,K) = 0
                    grid%u(I,J,K) = UINIT(I,K,J)
                    grid%v(I,J,K) = VINIT(I,K,J)
                    grid%t(I,J,K) = TINIT(I,K,J)
                    grid%q(I,J,K) = QINIT(I,K,J)
                    grid%cwm(I,J,K) = CWMINIT(I,K,J)
                 ENDDO
                 !  Was commented out in original V2 HWRF too:
                 !      DO K = kds,kde
                 !         grid%nmm_pint(I,J,K) = pinit(I,K,J)
                 !      ENDDO
              ENDDO
           ENDDO

           call wrf_message('DEALLOCATE PREP_HYBRID INIT ARRAYS')
           deallocate(TINIT,PINIT,UINIT,VINIT,QINIT,CWMINIT,PDINIT)
        end if ph_loop1
     end if read_phinit
#endif

      CALL model_to_grid_config_rec ( grid%id, model_config_rec, config_flags )

      !  Close this file that is output from the SI and input to this pre-proc.

      CALL wrf_debug ( 100 , 'med_sidata_input: back from init_domain' )


!!! not sure about this, but doesnt seem like needs to be called each time
      IF ( loop .EQ. 1 ) THEN
        CALL start_domain ( grid , .TRUE.)
      END IF

#if ( WRF_CHEM == 1 )
      IF ( loop == 1 ) THEN
!        IF ( ( grid%chem_opt .EQ. RADM2     ) .OR. &
!             ( grid%chem_opt .EQ. RADM2SORG ) .OR. &
!             ( grid%chem_opt .EQ. RACM      ) .OR. &
!             ( grid%chem_opt .EQ. RACMSORG  ) ) THEN
         IF( grid%chem_opt > 0 ) then
           ! Read the chemistry data from a previous wrf forecast (wrfout file)
           IF(grid%chem_in_opt == 1 ) THEN
              message = 'INITIALIZING CHEMISTRY WITH OLD SIMULATION'
              CALL  wrf_message ( message )

              CALL input_ext_chem_file( grid )

              IF(grid%bio_emiss_opt == BEIS311 ) THEN
                 message = 'READING BEIS3.11 EMISSIONS DATA'
                 CALL  wrf_message ( message )
                 CALL med_read_wrf_chem_bioemiss ( grid , config_flags)
              else IF(grid%bio_emiss_opt == 3 ) THEN !shc
                 message = 'READING MEGAN 2 EMISSIONS DATA'
                 CALL  wrf_message ( message )
                 CALL med_read_wrf_chem_bioemiss ( grid , config_flags)
              END IF

           ELSEIF(grid%chem_in_opt == 0)then
              ! Generate chemistry data from a idealized vertical profile
              message = 'STARTING WITH BACKGROUND CHEMISTRY '
              CALL  wrf_message ( message )

              write(message,*)' ETA1 '
              CALL  wrf_message ( message )
!             write(message,*) grid%eta1
!             CALL  wrf_message ( message )

              CALL input_chem_profile ( grid )

              IF(grid%bio_emiss_opt == BEIS311 ) THEN
                 message = 'READING BEIS3.11 EMISSIONS DATA'
                 CALL  wrf_message ( message )
                 CALL med_read_wrf_chem_bioemiss ( grid , config_flags)
              else IF(grid%bio_emiss_opt == 3 ) THEN !shc
                 message = 'READING MEGAN 2 EMISSIONS DATA'
                 CALL  wrf_message ( message )
                 CALL med_read_wrf_chem_bioemiss ( grid , config_flags)
              END IF

           ELSE
             message = 'RUNNING WITHOUT CHEMISTRY INITIALIZATION'
             CALL  wrf_message ( message )
           ENDIF
         ENDIF
      ENDIF
#endif

      config_flags%isurban=1
      config_flags%isoilwater=14

      CALL assemble_output ( grid , config_flags , loop , time_loop_max )

      !  Here we define the next time that we are going to process.

      CALL geth_newdate ( current_date_char , start_date_char , &
                          loop * model_config_rec%interval_seconds )
      current_date =  current_date_char // '.0000'

      CALL domain_clock_set( grid, current_date(1:19) )

      write(message,*) 'current_date= ', current_date
      CALL wrf_message(message)

   END DO
END SUBROUTINE med_sidata_input

SUBROUTINE compute_si_start_and_end (  &
          start_year, start_month, start_day, start_hour, &
          start_minute, start_second, &
          end_year ,   end_month ,   end_day ,   end_hour , &
          end_minute ,   end_second , &
          interval_seconds , real_data_init_type , &
          start_date_char , end_date_char , time_loop_max )

   USE module_date_time

   IMPLICIT NONE

   INTEGER :: start_year , start_month , start_day , &
              start_hour , start_minute , start_second
   INTEGER ::   end_year ,   end_month ,   end_day , &
                end_hour ,   end_minute ,   end_second
   INTEGER :: interval_seconds , real_data_init_type
   INTEGER :: time_loop_max , time_loop

   CHARACTER(LEN=132) :: message
   CHARACTER(LEN=19)  :: current_date_char , start_date_char , &
                        end_date_char , next_date_char

!   WRITE ( start_date_char , FMT = &
!         '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
!         start_year,start_month,start_day,start_hour,start_minute,start_second
!   WRITE (   end_date_char , FMT = &
!         '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
!          end_year,  end_month,  end_day,  end_hour,  end_minute,  end_second

   WRITE ( start_date_char , FMT = &
         '(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2)' ) &
         start_year,start_month,start_day,start_hour,start_minute,start_second
   WRITE (   end_date_char , FMT = &
         '(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2)' ) &
          end_year,  end_month,  end_day,  end_hour,  end_minute,  end_second

!  start_date = start_date_char // '.0000'

   !  Figure out our loop count for the processing times.

   time_loop = 1
   PRINT '(A,I4,A,A,A)','Time period #',time_loop, &
                        ' to process = ',start_date_char,'.'
   current_date_char = start_date_char
   loop_count : DO
      CALL geth_newdate (next_date_char, current_date_char, interval_seconds )
      IF      ( next_date_char .LT. end_date_char ) THEN
         time_loop = time_loop + 1
         PRINT '(A,I4,A,A,A)','Time period #',time_loop,&
                              ' to process = ',next_date_char,'.'
         current_date_char = next_date_char
      ELSE IF ( next_date_char .EQ. end_date_char ) THEN
         time_loop = time_loop + 1
         PRINT '(A,I4,A,A,A)','Time period #',time_loop,&
                              ' to process = ',next_date_char,'.'
         PRINT '(A,I4,A)','Total analysis times to input = ',time_loop,'.'
         time_loop_max = time_loop
         EXIT loop_count
      ELSE IF ( next_date_char .GT. end_date_char ) THEN
         PRINT '(A,I4,A)','Total analysis times to input = ',time_loop,'.'
         time_loop_max = time_loop
         EXIT loop_count
      END IF
   END DO loop_count
        write(message,*) 'done in si_start_and_end'
        CALL wrf_message(message)
END SUBROUTINE compute_si_start_and_end

SUBROUTINE assemble_output ( grid , config_flags , loop , time_loop_max )

!!! replace with something?   USE module_big_step_utilities_em

   USE module_domain
   USE module_io_domain
   USE module_configure
   USE module_date_time
   USE module_bc
   IMPLICIT NONE

#if ( HWRF == 1 )
  external get_wrf_debug_level
  integer :: debug
#endif

   TYPE(domain)                 :: grid
   TYPE (grid_config_rec_type)  :: config_flags
   INTEGER , INTENT(IN)         :: loop , time_loop_max

   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: ips , ipe , jps , jpe , kps , kpe
   INTEGER :: ijds , ijde , spec_bdy_width
   INTEGER :: inc_h,inc_v
   INTEGER :: i , j , k , idts

   INTEGER :: id1 , interval_seconds , ierr, rc, sst_update
   INTEGER , SAVE :: id ,id4
   CHARACTER (LEN=80) :: inpname , bdyname
   CHARACTER(LEN= 4) :: loop_char
   CHARACTER(LEN=132) :: message
character *19 :: temp19
character *24 :: temp24 , temp24b

   REAL, DIMENSION(:,:,:), ALLOCATABLE, SAVE :: ubdy3dtemp1 , vbdy3dtemp1 ,&
                                                tbdy3dtemp1 , &
				                cwmbdy3dtemp1 , qbdy3dtemp1,&
                                                q2bdy3dtemp1 , pdbdy2dtemp1
   REAL, DIMENSION(:,:,:), ALLOCATABLE, SAVE :: ubdy3dtemp2 , vbdy3dtemp2 , &
                                                tbdy3dtemp2 , & 
                                                cwmbdy3dtemp2 , qbdy3dtemp2, &
                                                q2bdy3dtemp2, pdbdy2dtemp2
   REAL :: t1,t2

#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

#if ( HWRF == 1 )
  ! Sam says:

  ! The *B arrays are used to read boundary data written out by hwrf_prep_hybrid
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::TB,UB,VB,QB,CWMB
  REAL,ALLOCATABLE,DIMENSION(:,:)::PDB

  ! Dimensions and looping variables:
  INTEGER :: KB, LM, IM, JM, N

  ! Unit number to read boundary data from (changes each time)
  INTEGER :: iunit_gfs

  ! Did we allocate the prep_hybrid input arrays?
  LOGICAL :: alloc_ph_arrays

  integer :: ioerror
#endif

#include "deref_kludge.h"

#if ( HWRF == 1 )
  alloc_ph_arrays=.false.
  call get_wrf_debug_level(debug)
#endif

   !  Various sizes that we need to be concerned about.

   ids = grid%sd31
   ide = grid%ed31-1 ! 030730tst
   jds = grid%sd32
   jde = grid%ed32-1 ! 030730tst
   kds = grid%sd33
   kde = grid%ed33-1 ! 030730tst

   ims = grid%sm31
   ime = grid%em31
   jms = grid%sm32
   jme = grid%em32
   kms = grid%sm33
   kme = grid%em33

   ips = grid%sp31
   ipe = grid%ep31-1 ! 030730tst
   jps = grid%sp32
   jpe = grid%ep32-1 ! 030730tst
   kps = grid%sp33
   kpe = grid%ep33-1 ! 030730tst

        if (IPE .ne. IDE) IPE=IPE+1
        if (JPE .ne. JDE) JPE=JPE+1

        write(message,*) 'assemble output (ids,ide): ', ids,ide
        CALL wrf_message(message)
        write(message,*) 'assemble output (ims,ime): ', ims,ime
        CALL wrf_message(message)
        write(message,*) 'assemble output (ips,ipe): ', ips,ipe
        CALL wrf_message(message)
 
        write(message,*) 'assemble output (jds,jde): ', jds,jde
        CALL wrf_message(message)
        write(message,*) 'assemble output (jms,jme): ', jms,jme
        CALL wrf_message(message)
        write(message,*) 'assemble output (jps,jpe): ', jps,jpe
        CALL wrf_message(message)
 
        write(message,*) 'assemble output (kds,kde): ', kds,kde
        CALL wrf_message(message)
        write(message,*) 'assemble output (kms,kme): ', kms,kme
        CALL wrf_message(message)
        write(message,*) 'assemble output (kps,kpe): ', kps,kpe
        CALL wrf_message(message)

   ijds = MIN ( ids , jds )
!mptest030805   ijde = MAX ( ide , jde )
   ijde = MAX ( ide , jde ) + 1   ! to make stuff_bdy dimensions consistent with alloc

   !  Boundary width, scalar value.

   spec_bdy_width = model_config_rec%spec_bdy_width
   interval_seconds = model_config_rec%interval_seconds
   sst_update = model_config_rec%sst_update

!-----------------------------------------------------------------------
!
   main_loop_test: IF ( loop .EQ. 1 ) THEN
!
!-----------------------------------------------------------------------

      IF ( time_loop_max .NE. 1 ) THEN
         IF(sst_update .EQ. 1)THEN
           CALL construct_filename1( inpname , 'wrflowinp' , grid%id , 2 )
           CALL open_w_dataset ( id4, TRIM(inpname) , grid , config_flags , output_auxinput4 , "DATASET=AUXINPUT4", ierr )
           IF ( ierr .NE. 0 ) THEN
              CALL wrf_error_fatal( 'ideal_hwrf: error opening wrflowinp for writing' )
           END IF
           CALL output_auxinput4 ( id4, grid , config_flags , ierr )
         END IF
      END IF


   !  This is the space needed to save the current 3d data for use in computing
   !  the lateral boundary tendencies.

      ALLOCATE ( ubdy3dtemp1(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( vbdy3dtemp1(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( tbdy3dtemp1(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( qbdy3dtemp1(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( cwmbdy3dtemp1(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( q2bdy3dtemp1(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( pdbdy2dtemp1(ims:ime,jms:jme,1:1) )

	ubdy3dtemp1=0.
	vbdy3dtemp1=0.
	tbdy3dtemp1=0.
	qbdy3dtemp1=0.
	cwmbdy3dtemp1=0.
	q2bdy3dtemp1=0.
	pdbdy2dtemp1=0.

      ALLOCATE ( ubdy3dtemp2(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( vbdy3dtemp2(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( tbdy3dtemp2(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( qbdy3dtemp2(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( cwmbdy3dtemp2(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( q2bdy3dtemp2(ims:ime,jms:jme,kms:kme) )
      ALLOCATE ( pdbdy2dtemp2(ims:ime,jms:jme,1:1) )

	ubdy3dtemp2=0.
	vbdy3dtemp2=0.
	tbdy3dtemp2=0.
	qbdy3dtemp2=0.
	cwmbdy3dtemp2=0.
	q2bdy3dtemp2=0.
	pdbdy2dtemp2=0.

      !  Open the wrfinput file.  From this program, this is an *output* file.

      CALL construct_filename1( inpname , 'wrfinput' , grid%id , 2 )

      CALL open_w_dataset ( id1, TRIM(inpname) , grid , config_flags , &
                            output_input , "DATASET=INPUT", ierr )

      IF ( ierr .NE. 0 ) THEN
      CALL wrf_error_fatal( 'ideal_hwrf: error opening wrfinput for writing' )
      ENDIF

!     CALL calc_current_date ( grid%id , 0. )
!      grid%write_metadata = .true.

        write(message,*) 'making call to output_input'
        CALL wrf_message(message)

        CALL output_input ( id1, grid , config_flags , ierr )

!***
!***  CLOSE THE WRFINPUT DATASET
!***
      CALL close_dataset ( id1 , config_flags , "DATASET=INPUT" )

      !  We need to save the 3d data to compute a 
      !  difference during the next loop. 

!
!-----------------------------------------------------------------------
!***  SOUTHERN BOUNDARY
!-----------------------------------------------------------------------
!

        IF(JPS==JDS)THEN
          J=1
          DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp1(i,j,k) = grid%u(i,j,k)
            vbdy3dtemp1(i,j,k) = grid%v(i,j,k)
            tbdy3dtemp1(i,j,k) = grid%t(i,j,k)
            qbdy3dtemp1(i,j,k) = grid%q(i,j,k)
            cwmbdy3dtemp1(i,j,k) = grid%cwm(i,j,k)
            q2bdy3dtemp1(i,j,k) = grid%q2(i,j,k)
          END DO
          END DO

          DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp1(i,j,1) = grid%pd(i,j)
          END DO
        ENDIF

!
!-----------------------------------------------------------------------
!***  NORTHERN BOUNDARY
!-----------------------------------------------------------------------
!
        IF(JPE==JDE)THEN
          J=MIN(JDE,JPE)
          DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp1(i,j,k) = grid%u(i,j,k)
            vbdy3dtemp1(i,j,k) = grid%v(i,j,k)
            tbdy3dtemp1(i,j,k) = grid%t(i,j,k)
            qbdy3dtemp1(i,j,k) = grid%q(i,j,k)
            cwmbdy3dtemp1(i,j,k) = grid%cwm(i,j,k)
            q2bdy3dtemp1(i,j,k) = grid%q2(i,j,k)
          END DO
          END DO

          DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp1(i,j,1) = grid%pd(i,j)
          END DO
        ENDIF

!
!-----------------------------------------------------------------------
!***  WESTERN BOUNDARY
!-----------------------------------------------------------------------
!
        write(message,*) 'western boundary, store winds over J: ', jps, min(jpe,jde)
        CALL wrf_message(message)

        IF(IPS==IDS)THEN
          I=1
          DO k = kps , MIN(kde,kpe)
          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, min(jde,jpe),2

        if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then
            tbdy3dtemp1(i,j,k) = grid%t(i,j,k)
            qbdy3dtemp1(i,j,k) = grid%q(i,j,k)
            cwmbdy3dtemp1(i,j,k) = grid%cwm(i,j,k)
            q2bdy3dtemp1(i,j,k) = grid%q2(i,j,k)
      if(k==1)then
        write(message,*)' loop=',loop,' i=',i,' j=',j,' tbdy3dtemp1(i,j,k)=',tbdy3dtemp1(i,j,k)
        CALL wrf_debug(10,message)
      endif
	endif
          END DO
          END DO

          DO k = kps , MIN(kde,kpe)
          inc_v=mod(jps,2)
          DO j = jps+inc_v, min(jde,jpe),2
        if (J .ge. 2 .and. J .le. JDE-1 .and. mod(J,2) .eq. 0) then
            ubdy3dtemp1(i,j,k) = grid%u(i,j,k)
            vbdy3dtemp1(i,j,k) = grid%v(i,j,k)
	endif
          END DO
          END DO
!
          inc_h=mod(jps+1,2)
        DO j = jps+inc_h, min(jde,jpe),2
        if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then
            pdbdy2dtemp1(i,j,1) = grid%pd(i,j)
          write(message,*)' loop=',loop,' i=',i,' j=',j,' pdbdy2dtemp1(i,j)=',pdbdy2dtemp1(i,j,1)
          CALL wrf_debug(10,message)
	endif
          END DO
        ENDIF
!
!-----------------------------------------------------------------------
!***  EASTERN BOUNDARY
!-----------------------------------------------------------------------
!
        IF(IPE==IDE)THEN
          I=MIN(IDE,IPE)
!
          DO k = kps , MIN(kde,kpe)
!
!***   Make sure the J loop is on the global boundary
!
          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, min(jde,jpe),2
        if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then
            tbdy3dtemp1(i,j,k) = grid%t(i,j,k)
            qbdy3dtemp1(i,j,k) = grid%q(i,j,k)
            cwmbdy3dtemp1(i,j,k) = grid%cwm(i,j,k)
            q2bdy3dtemp1(i,j,k) = grid%q2(i,j,k)
	endif
          END DO
          END DO

          DO k = kps , MIN(kde,kpe)
          inc_v=mod(jps,2)
          DO j = jps+inc_v, min(jde,jpe),2
        if (J .ge. 2 .and. J .le. JDE-1 .and. mod(J,2) .eq. 0) then
            ubdy3dtemp1(i,j,k) = grid%u(i,j,k)
            vbdy3dtemp1(i,j,k) = grid%v(i,j,k)
        endif
          END DO
          END DO
!
          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, min(jde,jpe),2
        if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then
            pdbdy2dtemp1(i,j,1) = grid%pd(i,j)
	endif
          END DO
        ENDIF


      !  There are 2 components to the lateral boundaries.  
      !  First, there is the starting
      !  point of this time period - just the outer few rows and columns.


 CALL stuff_bdy_ijk (ubdy3dtemp1, grid%u_bxs, grid%u_bxe, &
                                  grid%u_bys, grid%u_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (vbdy3dtemp1, grid%v_bxs, grid%v_bxe, &
                                  grid%v_bys, grid%v_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (tbdy3dtemp1, grid%t_bxs, grid%t_bxe, &
                                  grid%t_bys, grid%t_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (cwmbdy3dtemp1, grid%cwm_bxs, grid%cwm_bxe, &
                                  grid%cwm_bys, grid%cwm_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (qbdy3dtemp1, grid%q_bxs, grid%q_bxe, &
                                  grid%q_bys, grid%q_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (q2bdy3dtemp1, grid%q2_bxs, grid%q2_bxe, &
                                  grid%q2_bys, grid%q2_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )


 CALL stuff_bdy_ijk (pdbdy2dtemp1, grid%pd_bxs, grid%pd_bxe, &
                                   grid%pd_bys, grid%pd_bye, &
                                   'M', spec_bdy_width, &
                                   ids , ide+1 , jds , jde+1 , 1 , 1 , &
                                   ims , ime , jms , jme , 1 , 1 , &
                                   ips , ipe , jps , jpe , 1 , 1 )

!-----------------------------------------------------------------------
!
   ELSE IF ( loop .GT. 1 ) THEN
!
!-----------------------------------------------------------------------

     call wrf_debug(1,'LOOP>1, so start making non-init boundary conditions')
#if ( HWRF == 1 )

     bdytmp_useph: if(grid%use_prep_hybrid) then
        call wrf_debug(1,'ALLOCATE PREP_HYBRID BOUNDARY ARRAYS')
        !! When running in prep_hybrid mode, we must read in the data here.

        ! Allocate boundary arrays:
        KB = 2*IDE + JDE - 3
        LM = KDE
        IM = IDE
        JM = JDE
        ALLOCATE(TB(KB,LM,2))
        ALLOCATE(QB(KB,LM,2))
        ALLOCATE(CWMB(KB,LM,2))
        ALLOCATE(UB(KB,LM,2))
        ALLOCATE(VB(KB,LM,2))
        ALLOCATE(PDB(KB,2))
        alloc_ph_arrays=.true.

        ! Read in the data:
        IUNIT_GFS = 900 + LOOP - 1
        READ(IUNIT_GFS,iostat=ioerror) PDB,TB,QB,CWMB,UB,VB
        if(ioerror/=0) then
           write(message,*) 'Unable to read MAKBND output from unit ',IUNIT_GFS
           call wrf_error_fatal(message)
        endif

        ! Now copy the data into the temporary boundary arrays, and
        ! switch from IKJ to IJK while we do it.

        !!    Southern boundary

        IF(JPS.EQ.JDS)THEN
           J=1

           DO k = kps , MIN(kde,kpe)
              N=1
              DO i = ips , MIN(ide,ipe)
                 tbdy3dtemp2(i,j,k) =   TB(N,k,1)
                 qbdy3dtemp2(i,j,k) =   QB(N,k,1)
                 cwmbdy3dtemp2(i,j,k) = CWMB(N,k,1)
                 q2bdy3dtemp2(i,j,k) =  0.0        !KWON
                 write(message,*)'southtend t',k,i,n,tbdy3dtemp2(i,j,k)
                 call wrf_debug(10,message)
                 write(message,*)'southtend q',k,i,n,qbdy3dtemp2(i,j,k)
                 call wrf_debug(10,message)
                 if (K .eq. 1 ) then
                    write(0,*) 'S boundary values T,Q : ', I,tbdy3dtemp2(i,j,k),  &
                         qbdy3dtemp2(i,j,k)
                 endif
                 N=N+1
              END DO
           END DO

           DO k = kps , MIN(kde,kpe)
              N=1
              DO i = ips , MIN(ide,ipe)
                 ubdy3dtemp2(i,j,k) = UB(N,k,1)
                 vbdy3dtemp2(i,j,k) = VB(N,k,1)
                 N=N+1
              ENDDO
           END DO

           N=1
           DO i = ips , MIN(ide,ipe)
              pdbdy2dtemp2(i,j,1) = PDB(N,1)
              write(message,*)'southtend p',i,n,pdbdy2dtemp1(i,j,1)
              call wrf_debug(10,message)
              N=N+1
           END DO

        ENDIF

        !     Northern boundary

        IF(JPE.EQ.JDE)THEN

           J=MIN(JDE,JPE)
           DO k = kps , MIN(kde,kpe)
              N=IM+1
              DO i = ips , MIN(ide,ipe)
                 tbdy3dtemp2(i,j,k) =   TB(N,k,1)
                 qbdy3dtemp2(i,j,k) =   QB(N,k,1)
                 cwmbdy3dtemp2(i,j,k) = CWMB(N,k,1)
                 q2bdy3dtemp2(i,j,k) =  0.0        !KWON
                    write(message,*)'northtend t',k,i,n,tbdy3dtemp2(i,j,k)
                    call wrf_debug(10,message)
                    write(message,*)'northtend q',k,i,n,qbdy3dtemp2(i,j,k)
                    call wrf_debug(10,message)
                 N=N+1
              END DO
           END DO

           DO k = kps , MIN(kde,kpe)
              N=IM
              DO i = ips , MIN(ide,ipe)
                 ubdy3dtemp2(i,j,k) = UB(N,k,1)
                 vbdy3dtemp2(i,j,k) = VB(N,k,1)
                 N=N+1
              END DO
           END DO

           N=IM+1
           DO i = ips , MIN(ide,ipe)
              pdbdy2dtemp2(i,j,1) = PDB(N,1)
                 write(message,*)'northtend p',i,n,pdbdy2dtemp1(i,j,1)
                 call wrf_debug(10,message)
              N=N+1
           END DO

        ENDIF

        !!     Western boundary

        IF(IPS.EQ.IDS)THEN
           I=1
           DO k = kps , MIN(kde,kpe)
              N=2*IM+1
              inc_h=mod(jps+1,2)
              DO j = jps+inc_h, MIN(jde,jpe),2
                 if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
                    tbdy3dtemp2(i,j,k) =   TB(N,k,1)
                    qbdy3dtemp2(i,j,k) =   QB(N,k,1)
                    cwmbdy3dtemp2(i,j,k) = CWMB(N,k,1)
                    q2bdy3dtemp2(i,j,k) =  0.0        !KWON
                       write(message,*)'westtend t',k,j,n,tbdy3dtemp2(i,j,k)
                       call wrf_debug(10,message)
                       write(message,*)'westtend q',k,j,n,qbdy3dtemp2(i,j,k)
                       call wrf_debug(10,message)
                    N=N+1
                 endif
              END DO
           END DO

           DO k = kps , MIN(kde,kpe)
              N=2*IM-1
              inc_v=mod(jps,2)
              DO j = jps+inc_v, MIN(jde,jpe),2
                 if (J .ge. 2 .and. J .le. jde-1 .and. mod(J,2) .eq. 0) then
                    ubdy3dtemp2(i,j,k) = UB(N,k,1)
                    vbdy3dtemp2(i,j,k) = VB(N,k,1)
                    N=N+1
                 endif
              END DO
           END DO

           N=2*IM+1
           inc_h=mod(jps+1,2)
           DO j = jps+inc_h, MIN(jde,jpe),2
              if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
                 pdbdy2dtemp2(i,j,1) = PDB(N,1)
                    write(message,*)'westtend p',j,n,pdbdy2dtemp1(i,j,1)
                    call wrf_debug(10,message)
                 N=N+1
              endif
           END DO

        ENDIF

        !!     Eastern boundary

        IF(IPE.EQ.IDE)THEN

           I=MIN(IDE,IPE)

           DO k = kps , MIN(kde,kpe)
              N=2*IM+(JM/2)
              inc_h=mod(jps+1,2)
              DO j = jps+inc_h, MIN(jde,jpe),2
                 if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
                    tbdy3dtemp2(i,j,k) =   TB(N,k,1)
                    qbdy3dtemp2(i,j,k) =   QB(N,k,1)
                    cwmbdy3dtemp2(i,j,k) = CWMB(N,k,1)
                    q2bdy3dtemp2(i,j,k) =  0.0        !KWON
                       write(message,*)'easttend t',k,j,n,tbdy3dtemp2(i,j,k)
                       call wrf_debug(10,message)
                       write(message,*)'easttend q',k,j,n,qbdy3dtemp2(i,j,k)
                       call wrf_debug(10,message)
                    N=N+1
                 endif
              END DO
           END DO

           DO k = kps , MIN(kde,kpe)
              N=2*IM+(JM/2)-1
              inc_v=mod(jps,2)
              DO j = jps+inc_v, MIN(jde,jpe),2
                 if (J .ge. 2 .and. J .le. jde-1 .and. mod(J,2) .eq. 0) then
                    ubdy3dtemp2(i,j,k) = UB(N,k,1)
                    vbdy3dtemp2(i,j,k) = VB(N,k,1)
                    N=N+1
                 endif
              END DO
           END DO

           N=2*IM+(JM/2)
           inc_h=mod(jps+1,2)
           DO j = jps+inc_h, MIN(jde,jpe),2
              if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
                 pdbdy2dtemp2(i,j,1) = PDB(N,1)
                    write(message,*)'easttend p',j,n,pdbdy2dtemp1(i,j,1)
                    call wrf_debug(10,message)
                 N=N+1
              endif
           END DO

        ENDIF
     else
#endif

           CALL output_auxinput4 ( id4, grid , config_flags , ierr )

#if ( HWRF == 1 )
     endif bdytmp_useph
#endif

      write(message,*)' assemble_output loop=',loop,' in IF block'
      call wrf_message(message)

      !  Open the boundary file.

      IF ( loop .eq. 2 ) THEN
         CALL construct_filename1( bdyname , 'wrfbdy' , grid%id , 2 )
      CALL open_w_dataset ( id, TRIM(bdyname) , grid , config_flags , &
                          output_boundary , "DATASET=BOUNDARY", ierr )
         IF ( ierr .NE. 0 ) THEN
               CALL wrf_error_fatal( 'ideal_hwrf: error opening wrfbdy for writing' )
         ENDIF
!         grid%write_metadata = .true.
      ELSE
! what's this do?
!         grid%write_metadata = .true.
!         grid%write_metadata = .false.
         CALL domain_clockadvance( grid )
      END IF

#if ( HWRF == 1 )
     bdytmp_notph: if(.not.grid%use_prep_hybrid) then
#endif
!-----------------------------------------------------------------------
!***  SOUTHERN BOUNDARY
!-----------------------------------------------------------------------
!
        IF(JPS==JDS)THEN
          J=1
          DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp2(i,j,k) = grid%u(i,j,k)
            vbdy3dtemp2(i,j,k) = grid%v(i,j,k)
            tbdy3dtemp2(i,j,k) = grid%t(i,j,k)
            qbdy3dtemp2(i,j,k) = grid%q(i,j,k)
            cwmbdy3dtemp2(i,j,k) = grid%cwm(i,j,k)
            q2bdy3dtemp2(i,j,k) = grid%q2(i,j,k)
          END DO
          END DO
!
          DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp2(i,j,1) = grid%pd(i,j)
          END DO
        ENDIF

!
!-----------------------------------------------------------------------
!***  NORTHERN BOUNDARY
!-----------------------------------------------------------------------
!
        IF(JPE==JDE)THEN
          J=MIN(JDE,JPE)
          DO k = kps , MIN(kde,kpe)
          DO i = ips , MIN(ide,ipe)
            ubdy3dtemp2(i,j,k) = grid%u(i,j,k)
            vbdy3dtemp2(i,j,k) = grid%v(i,j,k)
            tbdy3dtemp2(i,j,k) = grid%t(i,j,k)
            qbdy3dtemp2(i,j,k) = grid%q(i,j,k)
            cwmbdy3dtemp2(i,j,k) = grid%cwm(i,j,k)
            q2bdy3dtemp2(i,j,k) = grid%q2(i,j,k)
          END DO
          END DO

          DO i = ips , MIN(ide,ipe)
            pdbdy2dtemp2(i,j,1) = grid%pd(i,j)
          END DO
        ENDIF
!
!-----------------------------------------------------------------------
!***  WESTERN BOUNDARY
!-----------------------------------------------------------------------
!
        IF(IPS==IDS)THEN
          I=1
          DO k = kps , MIN(kde,kpe)
          inc_h=mod(jps+1,2)
      if(k==1)then
        write(message,*)' assemble_ouput loop=',loop,' inc_h=',inc_h,' jps=',jps
        call wrf_debug(10,message)
      endif
          DO j = jps+inc_h, MIN(jde,jpe),2
        if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
            tbdy3dtemp2(i,j,k) = grid%t(i,j,k)
      if(k==1)then
        write(message,*)' loop=',loop,' i=',i,' j=',j,' tbdy3dtemp1(i,j,k)=',tbdy3dtemp1(i,j,k)
        call wrf_debug(10,message)
      endif
            qbdy3dtemp2(i,j,k) = grid%q(i,j,k)
            cwmbdy3dtemp2(i,j,k) = grid%cwm(i,j,k)
            q2bdy3dtemp2(i,j,k) = grid%q2(i,j,k)
	endif
          END DO
          END DO
!
          DO k = kps , MIN(kde,kpe)
          inc_v=mod(jps,2)
          DO j = jps+inc_v, MIN(jde,jpe),2
        if (J .ge. 2 .and. J .le. jde-1 .and. mod(J,2) .eq. 0) then
            ubdy3dtemp2(i,j,k) = grid%u(i,j,k)
            vbdy3dtemp2(i,j,k) = grid%v(i,j,k)
	endif
          END DO
          END DO

          inc_h=mod(jps+1,2)
        DO j = jps+inc_h, MIN(jde,jpe),2
        if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
          pdbdy2dtemp2(i,j,1) = grid%pd(i,j)
          write(message,*)' loop=',loop,' i=',i,' j=',j,' pdbdy2dtemp1(i,j)=',pdbdy2dtemp1(i,j,1)
          CALL wrf_debug(10,message)
	endif
          END DO
        ENDIF
!
!-----------------------------------------------------------------------
!***  EASTERN BOUNDARY
!-----------------------------------------------------------------------
!
        IF(IPE==IDE)THEN
          I=MIN(IDE,IPE)

          DO k = kps , MIN(kde,kpe)
          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, MIN(jde,jpe),2
        if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
            tbdy3dtemp2(i,j,k) = grid%t(i,j,k)
            qbdy3dtemp2(i,j,k) = grid%q(i,j,k)
            cwmbdy3dtemp2(i,j,k) = grid%cwm(i,j,k)
            q2bdy3dtemp2(i,j,k) = grid%q2(i,j,k)
	endif
          END DO
          END DO

          DO k = kps , MIN(kde,kpe)
          inc_v=mod(jps,2)
          DO j = jps+inc_v, MIN(jde,jpe),2
        if (J .ge. 2 .and. J .le. jde-1 .and. mod(J,2) .eq. 0) then
            ubdy3dtemp2(i,j,k) = grid%u(i,j,k)
            vbdy3dtemp2(i,j,k) = grid%v(i,j,k)
	endif
          END DO
          END DO

          inc_h=mod(jps+1,2)
          DO j = jps+inc_h, MIN(jde,jpe),2
        if (J .ge. 3 .and. J .le. jde-2 .and. mod(J,2) .eq. 1) then
            pdbdy2dtemp2(i,j,1) = grid%pd(i,j)
	endif
          END DO
        ENDIF
#if ( HWRF == 1 )
     endif bdytmp_notph
#endif
!-----------------------------------------------------------------------
      !  During all of the loops after the first loop, 
      !  we first compute the boundary
      !  tendencies with the current data values 
      !  (*bdy3dtemp2 arrays) and the previously 
      !  saved information stored in the *bdy3dtemp1 arrays.


      CALL stuff_bdytend_ijk ( ubdy3dtemp2 , ubdy3dtemp1 , REAL(interval_seconds),&
                                   grid%u_btxs, grid%u_btxe, &
                                   grid%u_btys, grid%u_btye, &
                                   'N',  spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

      CALL stuff_bdytend_ijk ( vbdy3dtemp2 , vbdy3dtemp1 , REAL(interval_seconds),&
                                   grid%v_btxs, grid%v_btxe, &
                                   grid%v_btys, grid%v_btye, &
                                   'N',  spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

      CALL stuff_bdytend_ijk ( tbdy3dtemp2 , tbdy3dtemp1 , REAL(interval_seconds),&
                                   grid%t_btxs, grid%t_btxe, &
                                   grid%t_btys, grid%t_btye, &
                                   'N',  spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

      CALL stuff_bdytend_ijk ( cwmbdy3dtemp2 , cwmbdy3dtemp1 , REAL(interval_seconds),&
                                   grid%cwm_btxs, grid%cwm_btxe, &
                                   grid%cwm_btys, grid%cwm_btye, &
                                   'N',  spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

      CALL stuff_bdytend_ijk ( qbdy3dtemp2 , qbdy3dtemp1 , REAL(interval_seconds),&
                                   grid%q_btxs, grid%q_btxe, &
                                   grid%q_btys, grid%q_btye, &
                                   'N',  spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

      CALL stuff_bdytend_ijk ( q2bdy3dtemp2 , q2bdy3dtemp1 , REAL(interval_seconds),&
                                   grid%q2_btxs, grid%q2_btxe, &
                                   grid%q2_btys, grid%q2_btye, &
                                   'N',  spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe+1 )

      CALL stuff_bdytend_ijk( pdbdy2dtemp2 , pdbdy2dtemp1, REAL(interval_seconds),&
                                   grid%pd_btxs, grid%pd_btxe, &
                                   grid%pd_btys, grid%pd_btye, &
                                   'M',  spec_bdy_width      , &
                                   ids , ide+1 , jds , jde+1 , 1 , 1 , &
                                   ims , ime   , jms , jme   , 1 , 1 , &
                                   ips , ipe   , jps , jpe   , 1 , 1 )



      !  Both pieces of the boundary data are now 
      !  available to be written (initial time and tendency).
      !  This looks ugly, these date shifting things.  
      !  What's it for?  We want the "Times" variable
      !  in the lateral BDY file to have the valid times 
      !  of when the initial fields are written.
      !  That's what the loop-2 thingy is for with the start date.  
      !  We increment the start_date so
      !  that the starting time in the attributes is the 
      !  second time period.  Why you may ask.  I
      !  agree, why indeed.

      temp24= current_date
      temp24b=start_date
      start_date = current_date
      CALL geth_newdate ( temp19 , temp24b(1:19) , &
                         (loop-2) * model_config_rec%interval_seconds )
      current_date = temp19 //  '.0000'
       CALL domain_clock_set( grid, current_date(1:19) )
      write(message,*) 'LBC valid between these times ',current_date, ' ',start_date
      CALL wrf_message(message)

      CALL output_boundary ( id, grid , config_flags , ierr )
      current_date = temp24
      start_date = temp24b

      !  OK, for all of the loops, we output the initialzation 
      !  data, which would allow us to
      !  start the model at any of the available analysis time periods.

!  WRITE ( loop_char , FMT = '(I4.4)' ) loop
!  CALL open_w_dataset ( id1, 'wrfinput'//loop_char , grid , config_flags , output_input , "DATASET=INPUT", ierr )
!  IF ( ierr .NE. 0 ) THEN
!    CALL wrf_error_fatal( 'ideal_hwrf: error opening wrfinput'//loop_char//' for writing' )
!  ENDIF
!  grid%write_metadata = .true.

!  CALL calc_current_date ( grid%id , 0. )
!  CALL output_input ( id1, grid , config_flags , ierr )
!  CALL close_dataset ( id1 , config_flags , "DATASET=INPUT" )

  !  Is this or is this not the last time time?  We can remove some unnecessary
  !  stores if it is not.

      IF     ( loop .LT. time_loop_max ) THEN

         !  We need to save the 3d data to compute a 
         !  difference during the next loop.  Couple the
         !  3d fields with total mu (mub + mu_2) and the 
         !  stagger-specific map scale factor.
         !  We load up the boundary data again for use in the next loop.


!mp	change these limits?????????

         DO k = kps , kpe
            DO j = jps , jpe
               DO i = ips , ipe
                  ubdy3dtemp1(i,j,k) = ubdy3dtemp2(i,j,k)
                  vbdy3dtemp1(i,j,k) = vbdy3dtemp2(i,j,k)
                  tbdy3dtemp1(i,j,k) = tbdy3dtemp2(i,j,k)
                  cwmbdy3dtemp1(i,j,k) = cwmbdy3dtemp2(i,j,k)
                  qbdy3dtemp1(i,j,k) = qbdy3dtemp2(i,j,k)
                  q2bdy3dtemp1(i,j,k) = q2bdy3dtemp2(i,j,k)
               END DO
            END DO
         END DO

!mp	change these limits?????????

         DO j = jps , jpe
            DO i = ips , ipe
               pdbdy2dtemp1(i,j,1) = pdbdy2dtemp2(i,j,1)
            END DO
         END DO

  !  There are 2 components to the lateral boundaries.  
  !   First, there is the starting
  !  point of this time period - just the outer few rows and columns.

 CALL stuff_bdy_ijk (ubdy3dtemp1, grid%u_bxs, grid%u_bxe, &
                                  grid%u_bys, grid%u_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (vbdy3dtemp1, grid%v_bxs, grid%v_bxe, &
                                  grid%v_bys, grid%v_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (tbdy3dtemp1, grid%t_bxs, grid%t_bxe, &
                                  grid%t_bys, grid%t_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (cwmbdy3dtemp1, grid%cwm_bxs, grid%cwm_bxe, &
                                  grid%cwm_bys, grid%cwm_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (qbdy3dtemp1, grid%q_bxs, grid%q_bxe, &
                                  grid%q_bys, grid%q_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (q2bdy3dtemp1, grid%q2_bxs, grid%q2_bxe, &
                                  grid%q2_bys, grid%q2_bye, &
                                  'N', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , kds , kde+1 , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe+1 )

 CALL stuff_bdy_ijk (pdbdy2dtemp1,grid%pd_bxs, grid%pd_bxe, &
                                  grid%pd_bys, grid%pd_bye, &
                                  'M', spec_bdy_width  , &
                                  ids , ide+1 , jds , jde+1 , 1 , 1 , &
                                  ims , ime , jms , jme , 1 , 1 , &
                                  ips , ipe , jps , jpe , 1 , 1 )

      ELSE IF ( loop .EQ. time_loop_max ) THEN

    !  If this is the last time through here, we need to close the files.

         CALL close_dataset ( id , config_flags , "DATASET=BOUNDARY" )

      END IF

   END IF main_loop_test

#if ( HWRF == 1 )
  if(alloc_ph_arrays) then
     call wrf_debug(1,'DEALLOCATE PREP_HYBRID BOUNARY ARRAYS')
     deallocate(TB,QB,CWMB,UB,VB,PDB)
  endif
#endif

END SUBROUTINE assemble_output
