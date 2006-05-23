!WRF:DRIVER_LAYER:MAIN
!

!<DESCRIPTION>
! ESMF Application Wrapper for coupling WRF with a "dummy" component 
! that simply reads SSTs from a file, sends to WRF, receives SST from 
! WRF (two-way coupling). and checks that the SSTs match.  
!
! This file contains the main program and associated modules for the 
! SST "dummy" component and a simple coupler.  It creates ESMF Gridded 
! and Coupler Components.  
!
! This source file is only built when ESMF coupling is used.  
!
!</DESCRIPTION>



!<DESCRIPTION>
! Modules module_sst_component_top and module_sst_setservices define the 
! "SST" dummy component.  
!</DESCRIPTION>

MODULE module_sst_component_top
!<DESCRIPTION>
! This module defines sst_component_init1(), sst_component_init2(), 
! sst_component_run1(), sst_component_run2(), and sst_component_finalize() 
! routines that are called when SST is run as an ESMF component.  
!</DESCRIPTION>

   USE ESMF_Mod
   USE module_esmf_extensions
   USE module_metadatautils, ONLY: AttachTimesToState


   IMPLICIT NONE

   ! everything is private by default
   PRIVATE

   ! Public entry points
   PUBLIC sst_component_init1
   PUBLIC sst_component_init2
   PUBLIC sst_component_run1
   PUBLIC sst_component_run2
   PUBLIC sst_component_finalize

   ! private stuff
   TYPE(ESMF_Grid), SAVE :: esmfgrid  ! grid used in fields
   CHARACTER (4096) :: str
   INTEGER, SAVE :: fid               ! file handle
   ! decomposition information
   INTEGER, SAVE :: ids, ide, jds, jde, kds, kde
   INTEGER, SAVE :: ims, ime, jms, jme, kms, kme
   INTEGER, SAVE :: ips, ipe, jps, jpe, kps, kpe
!$$$here...  change names to remove tmp_ ...  
   REAL(ESMF_KIND_R4), POINTER, SAVE :: tmp_data_out_sst(:,:)
   REAL(ESMF_KIND_R4), POINTER, SAVE :: tmp_data_out_landmask(:,:)
   REAL(ESMF_KIND_R4), POINTER, SAVE :: tmp_data_in_sst(:,:)
   REAL(ESMF_KIND_R4), POINTER, SAVE :: tmp_data_in_landmask(:,:)
!$$$DEBUG
   PUBLIC :: ids, ide, jds, jde, kds, kde
   PUBLIC :: ims, ime, jms, jme, kms, kme
   PUBLIC :: ips, ipe, jps, jpe, kps, kpe
!$$$END DEBUG
   INTEGER, SAVE :: domdesc
   LOGICAL, SAVE :: bdy_mask(4)
   ! MPI communicator, if needed
   INTEGER, SAVE :: mpicom
   ! field data
   REAL, POINTER, SAVE :: file_landmask_data(:,:), file_sst_data(:,:)
   ! input data file name
   CHARACTER ( ESMF_MAXSTR ), SAVE :: sstinfilename
   ! field names
   INTEGER, PARAMETER :: datacount = 2
   INTEGER, PARAMETER :: SST_INDX = 1
   INTEGER, PARAMETER :: LANDMASK_INDX = 2
   CHARACTER(LEN=ESMF_MAXSTR), SAVE :: datanames(datacount)
   TYPE real2d
     REAL, POINTER :: r2d(:,:)
   END TYPE real2d
   TYPE(real2d) :: this_data(datacount)


CONTAINS



   ! First-phase "init" reads "SST" data file and returns "time" metadata in 
   ! exportState.  
   SUBROUTINE sst_component_init1( gcomp, importState, exportState, clock, rc )
     USE module_io
     TYPE(ESMF_GridComp), TARGET, INTENT(INOUT) :: gcomp
     TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: importState
     TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: exportState
     TYPE(ESMF_Clock),    TARGET, INTENT(INOUT) :: clock
     INTEGER,                     INTENT(  OUT) :: rc
!<DESCRIPTION>
!     SST component init routine, phase 1.
!
!     The arguments are:
!       gcomp           Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

#ifdef DM_PARALLEL
     INCLUDE 'mpif.h'
#endif

     ! Local variables
     CHARACTER (LEN=19) :: date_string
#ifdef DM_PARALLEL
     TYPE(ESMF_VM) :: vm
     INTEGER :: mpicomtmp
#endif
     TYPE(ESMF_Time) :: startTime, stopTime, currentTime, dataTime
     TYPE(ESMF_TimeInterval) :: timeStep
     INTEGER :: ierr, num_steps, time_loop_max
     INTEGER :: status_next_var

     !$$$ For now, sstinfilename is hard-coded
     !$$$ Upgrade to use a variant of construct_filename() via startTime 
     !$$$ extracted from clock.  
     sstinfilename = 'sstin_d01_000000'

     ! get MPI communicator out of current VM and duplicate (if needed)
#ifdef DM_PARALLEL
     CALL ESMF_VMGetCurrent(vm, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'sst_component_init1:  ESMF_VMGetCurrent failed' )
     ENDIF
     CALL ESMF_VMGet(vm, mpiCommunicator=mpicomtmp, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'sst_component_init1:  ESMF_VMGet failed' )
     ENDIF
     CALL MPI_Comm_dup( mpicomtmp, mpicom, ierr )
#else
     mpicom = 0
#endif
     !  Open the "SST" input data file for reading.
     write(str,'(A,A)') 'Subroutine sst_component_init1: Opening data file ', &
       TRIM(sstinfilename)
     CALL wrf_message ( TRIM(str) )
     CALL wrf_open_for_read ( TRIM(sstinfilename) , &
                              mpicom ,              &
                              mpicom ,              &
                              "DATASET=INPUT" ,     &
                              fid ,                 &
                              ierr )
     IF ( ierr .NE. 0 ) THEN
        WRITE( str , FMT='(A,A,A,I8)' ) &
          'subroutine sst_component_init1: error opening ', &
          TRIM(sstinfilename),' for reading ierr=',ierr
        CALL wrf_error_fatal ( TRIM(str) )
     ENDIF
     WRITE( str , FMT='(A,A,A,I8)' ) &
       'subroutine sst_component_init1: opened file ', &
       TRIM(sstinfilename),' for reading fid=',fid
     CALL wrf_debug ( 100, TRIM(str) )

     ! How many data time levels are in the SST input file?  
     num_steps = -1
     time_loop_max = 0
     CALL wrf_debug    ( 100, 'subroutine sst_component_init1: find time_loop_max' )
     ! compute SST start time, time step, and end time here
     get_the_right_time : DO
        CALL wrf_get_next_time ( fid, date_string, status_next_var )
        write(str,'(A,A)') 'Subroutine sst_component_init1: SST data startTime: ', &
          date_string
        CALL wrf_debug ( 100 , TRIM(str) )
        IF ( status_next_var == 0 ) THEN
           IF ( time_loop_max == 0 ) THEN
             CALL wrf_atotime( date_string, startTime )
           ELSEIF ( time_loop_max == 1 ) THEN
             ! assumes fixed time step!
             CALL wrf_atotime( date_string, dataTime )
             timeStep = dataTime - startTime
           ENDIF
           time_loop_max = time_loop_max + 1
           CALL wrf_atotime( date_string, stopTime )
        ELSE
           EXIT get_the_right_time
        ENDIF
     END DO get_the_right_time
     CALL wrf_timetoa ( stopTime, date_string )
     write(str,'(A,A)') 'Subroutine sst_component_init1: SST data stopTime: ', &
       date_string
     CALL wrf_debug ( 100 , TRIM(str) )
     ! attach times to exportState for use by driver
     CALL AttachTimesToState( exportState, startTime, stopTime, timeStep )

     ! There should be a more elegant way to get to the beginning of the 
     ! file, but this will do.
     CALL wrf_ioclose( fid , ierr )
     IF ( ierr .NE. 0 ) THEN
        CALL wrf_error_fatal ( 'sst_component_init1:  wrf_ioclose failed' )
     ENDIF
     WRITE( str , FMT='(A,I8)' ) &
       'subroutine sst_component_init1: closed file fid=',fid
     CALL wrf_debug ( 100, TRIM(str) )

     ! set up field names
!$$$ use CF conventions for "standard_name" once WRF Registry supports them
!$$$     datanames(SST_INDX) = "sea_surface_temperature"
!$$$     datanames(LANDMASK_INDX) = "land_binary_mask"
     datanames(SST_INDX) = "SST"
     datanames(LANDMASK_INDX) = "LANDMASK"

     rc = ESMF_SUCCESS

   END SUBROUTINE sst_component_init1



   SUBROUTINE read_data( exportState, clock )
     USE module_io
     TYPE(ESMF_State), INTENT(INOUT) :: exportState
     TYPE(ESMF_Clock), INTENT(IN   ) :: clock
!<DESCRIPTION>
!     Reads data from file and stores.  Then 
!     stuffs the file data into the SST exportState.  
!</DESCRIPTION>

     #include <wrf_status_codes.h>
     #include <wrf_io_flags.h>

     ! Local variables
     CHARACTER (LEN=19) :: date_string
     TYPE(ESMF_Time) :: currentTime, dataTime
     REAL(ESMF_KIND_R4), POINTER :: out_sst_ptr(:,:), out_landmask_ptr(:,:)
     TYPE(ESMF_Field) :: out_sst_field, out_landmask_field
     TYPE(ESMF_Field) :: in_sst_field, in_landmask_field
     INTEGER :: i, j
     CHARACTER(LEN=ESMF_MAXSTR) :: fieldname, debugmsg, errormsg, timestr
     INTEGER :: ierr
     INTEGER :: rc

     ! This call to wrf_get_next_time will position the dataset over the next 
     ! time-frame in the file and return the date_string, which is used as an 
     ! argument to the read_field routines in the blocks of code included 
     ! below.  

     CALL wrf_get_next_time( fid, date_string , ierr )
     WRITE(str,'(A,A)') 'Subroutine read_data: SST data time: ', &
       date_string
     CALL wrf_debug ( 100 , TRIM(str) )
     IF ( ierr .NE. 0 .AND. ierr .NE. WRF_WARN_NOTSUPPORTED .AND.  &
          ierr .NE. WRF_WARN_DRYRUN_READ ) THEN
       CALL wrf_error_fatal ( "... May have run out of valid SST data ..." )
     ELSE IF ( ierr .NE. WRF_WARN_NOTSUPPORTED .AND. &
               ierr .NE. WRF_WARN_DRYRUN_READ) THEN
       ! check input time against current time (which will be start time at 
       ! beginning)
       CALL wrf_atotime( date_string, dataTime )
       CALL ESMF_ClockGet( clock, CurrTime=currentTime, rc=rc )
       IF (rc /= ESMF_SUCCESS) THEN
         CALL wrf_error_fatal ( 'read_data:  ESMF_ClockGet() failed' )
       ENDIF
       CALL wrf_clockprint(150, clock, &
              'DEBUG read_data():  get currentTime from clock,')
       IF ( dataTime .NE. currentTime ) THEN
           CALL wrf_timetoa ( dataTime, timestr )
           WRITE( errormsg , * )'Time in file: ',trim( timestr )
           CALL wrf_message ( trim(errormsg) )
           CALL wrf_timetoa ( currentTime, timestr )
           WRITE( errormsg , * )'Time on domain: ',trim( timestr )
           CALL wrf_message ( trim(errormsg) )
           CALL wrf_error_fatal( &
             "**ERROR** Time in input file not equal to time on domain **ERROR**" )
       ENDIF
     ENDIF

     ! doing this in a loop only works if staggering is the same for all fields
     this_data(SST_INDX)%r2d => file_sst_data
     this_data(LANDMASK_INDX)%r2d => file_landmask_data
     DO i=1, datacount
       fieldname = TRIM(datanames(i))
       debugmsg  = 'ext_read_field '//TRIM(fieldname)//' memorder XY'
       errormsg  = 'could not read '//TRIM(fieldname)//' data from file'
       CALL wrf_ext_read_field (    &
              fid                 , &  ! DataHandle
              date_string         , &  ! DateStr
              TRIM(fieldname)     , &  ! Data Name
              this_data(i)%r2d    , &  ! Field
              WRF_REAL            , &  ! FieldType
              mpicom              , &  ! Comm
              mpicom              , &  ! I/O Comm
              domdesc             , &  ! Domain descriptor
              bdy_mask            , &  ! bdy_mask
              'XY'                , &  ! MemoryOrder
              ''                  , &  ! Stagger
              TRIM(debugmsg)      , &  ! Debug message
              ids , (ide-1) , jds , (jde-1) , 1 , 1 , &
              ims , ime , jms , jme , 1 , 1         , &
              ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 , &
              ierr )
       IF (ierr /= 0) THEN
         CALL wrf_error_fatal ( TRIM(errormsg) )
       ENDIF
     ENDDO

     ! stuff fields into exportState
!$$$ change this to Bundles, eventually
     CALL ESMF_StateGetField( exportState, TRIM(datanames(SST_INDX)), &
                              out_sst_field, rc=rc )
     IF (rc /= ESMF_SUCCESS) THEN
       CALL wrf_error_fatal ( &
         'could not find sea_surface_temperature field in exportState' )
     ENDIF
     CALL ESMF_StateGetField( exportState, TRIM(datanames(LANDMASK_INDX)), &
                              out_landmask_field, rc=rc )
     IF (rc /= ESMF_SUCCESS) THEN
       CALL wrf_error_fatal ( &
         'could not find land_binary_mask field in exportState' )
     ENDIF
     CALL ESMF_FieldGetDataPointer( out_sst_field, out_sst_ptr, rc=rc )
     IF (rc /= ESMF_SUCCESS) THEN
       CALL wrf_error_fatal ( &
         'could not find sea_surface_temperature data in sea_surface_temperature field' )
     ENDIF
     CALL ESMF_FieldGetDataPointer( out_landmask_field, out_landmask_ptr, rc=rc )
     IF (rc /= ESMF_SUCCESS) THEN
       CALL wrf_error_fatal ( &
         'could not find land_binary_mask data in land_binary_mask field' )
     ENDIF
     ! staggered starts/ends
     DO j= jps , MIN( (jde-1), jpe )
       DO i= ips , MIN( (ide-1), ipe )
         out_sst_ptr(i,j) = file_sst_data(i,j)
         out_landmask_ptr(i,j) = file_landmask_data(i,j)
       ENDDO
     ENDDO

   END SUBROUTINE read_data




   SUBROUTINE compare_data( importState, clock )
     TYPE(ESMF_State), INTENT(INOUT) :: importState
!$$$ remove clock after debugging is finished
     TYPE(ESMF_Clock), INTENT(INOUT) :: clock
!<DESCRIPTION>
!     Gets data from coupler via importState 
!     and compares with data read from file and 
!     error-exits if they differ.  
!
!     The arguments are:
!       importState     Importstate
!</DESCRIPTION>

     ! Local variables
     TYPE(ESMF_Field) :: in_sst_field, in_landmask_field
     REAL(ESMF_KIND_R4), POINTER :: in_sst_ptr(:,:), in_landmask_ptr(:,:)
     REAL, POINTER :: in_sst_ptr_real(:,:), in_landmask_ptr_real(:,:)
     INTEGER :: i, j
     INTEGER :: rc
     LOGICAL :: landmask_ok, sst_ok
!$$$DEBUG
TYPE(ESMF_Time) :: currentTime
INTEGER, SAVE :: numtimes=0   ! track number of calls
CHARACTER(LEN=256) :: timestamp
!$$$END DEBUG

!$$$DEBUG
! count calls...
CALL ESMF_ClockGet( clock, CurrTime=currentTime, rc=rc )
IF (rc /= ESMF_SUCCESS) THEN
  CALL wrf_error_fatal ( 'compare_data:  ESMF_ClockGet() failed' )
ENDIF
CALL wrf_timetoa ( currentTime, timestamp )
numtimes = numtimes + 1
WRITE(str,*) 'SST compare_data:  begin, numtimes = ',numtimes,' time = ',TRIM(timestamp)
CALL wrf_debug ( 100 , TRIM(str) )
!$$$END DEBUG

     ! extract data from the importState and compare with data from file
!$$$ change this to Bundles, eventually
     CALL ESMF_StateGetField( importState, TRIM(datanames(SST_INDX)), &
                              in_sst_field, rc=rc )
     IF (rc /= ESMF_SUCCESS) THEN
       CALL wrf_error_fatal ( &
         'could not extract sea_surface_temperature field from importState' )
     ENDIF
     CALL ESMF_StateGetField( importState, TRIM(datanames(LANDMASK_INDX)), &
                              in_landmask_field, rc=rc )
     IF (rc /= ESMF_SUCCESS) THEN
       CALL wrf_error_fatal ( &
         'could not extract land_binary_mask field from importState' )
     ENDIF
     CALL ESMF_FieldGetDataPointer( in_sst_field, in_sst_ptr, rc=rc )
     IF (rc /= ESMF_SUCCESS) THEN
       CALL wrf_error_fatal ( &
         'could not extract sea_surface_temperature data from sea_surface_temperature field' )
     ENDIF
     ALLOCATE( in_sst_ptr_real(ims:ime,jms:jme) )
     WRITE( str,* ) 'compare_data, ips:ipe,jps:jpe = ',           &
       ips,':',ipe,',',jps,':',jpe,                               &
       ', in_sst_ptr(BOUNDS) = ',                                 &
       LBOUND(in_sst_ptr,1),':',UBOUND(in_sst_ptr,1),',',         &
       LBOUND(in_sst_ptr,2),':',UBOUND(in_sst_ptr,2)
     CALL wrf_debug ( 100 , TRIM(str) )
     DO j= jms, jme
       DO i= ims, ime
         in_sst_ptr_real(i,j) = -(i*1000.0 + j)/100000.0     ! obvious bad value for debugging
       ENDDO
     ENDDO
     in_sst_ptr_real(ips:MIN((ide-1),ipe),jps:MIN((jde-1),jpe)) = &
          in_sst_ptr(ips:MIN((ide-1),ipe),jps:MIN((jde-1),jpe))
     CALL ESMF_FieldGetDataPointer( in_landmask_field, in_landmask_ptr, rc=rc )
     IF (rc /= ESMF_SUCCESS) THEN
       CALL wrf_error_fatal ( &
         'could not extract land_binary_mask data from land_binary_mask field' )
     ENDIF
     ALLOCATE( in_landmask_ptr_real(ims:ime,jms:jme) )
     WRITE( str,* ) 'compare_data, ips:ipe,jps:jpe = ',             &
       ips,':',ipe,',',jps,':',jpe,                                 &
       ', in_landmask_ptr(BOUNDS) = ',                              &
       LBOUND(in_landmask_ptr,1),':',UBOUND(in_landmask_ptr,1),',', &
       LBOUND(in_landmask_ptr,2),':',UBOUND(in_landmask_ptr,2)
     CALL wrf_debug ( 100 , TRIM(str) )
     DO j= jms, jme
       DO i= ims, ime
         in_landmask_ptr_real(i,j) = -(i*1000.0 + j)/100000.0     ! obvious bad value for debugging
       ENDDO
     ENDDO
     in_landmask_ptr_real(ips:MIN((ide-1),ipe),jps:MIN((jde-1),jpe)) = &
          in_landmask_ptr(ips:MIN((ide-1),ipe),jps:MIN((jde-1),jpe))

     ! compare LANDMASK...  
     landmask_ok = .TRUE.
     ! staggered starts/ends
     LANDMASK_COMPARE : DO j= jps , MIN( (jde-1), jpe )
       DO i= ips , MIN( (ide-1), ipe )
         IF ( file_landmask_data(i,j) /= in_landmask_ptr_real(i,j) ) THEN
           landmask_ok = .FALSE.
           WRITE( str , * ) 'error landmask mismatch at (i,j) = (',i,',',j, &
                            '), values are',file_landmask_data(i,j),' and ',     &
                            in_landmask_ptr_real(i,j) 
           EXIT LANDMASK_COMPARE
         ENDIF
       ENDDO
     ENDDO LANDMASK_COMPARE
!$$$DEBUG
!CALL wrf_debug( 100, 'compare_data:  writing DEBUG1_SSTcmp_FILE_LANDMASK_'//TRIM(timestamp) )
!OPEN( UNIT=985, FILE='DEBUG1_SSTcmp_FILE_LANDMASK_'//TRIM(timestamp), FORM='formatted' )
!WRITE (985,'(a)') 'LANDMASK'
!DO j = jps, MIN( (jde-1), jpe )
!  DO i = ips, MIN( (ide-1), ipe )
!    WRITE (985,*) '(',i,',',j,'):  ',file_landmask_data(i,j)
!  ENDDO
!ENDDO
!CLOSE (985)
!CALL wrf_debug( 100, 'compare_data:  writing DEBUG1_SSTcmp_FROM_WRF_LANDMASK_'//TRIM(timestamp) )
!OPEN( UNIT=985, FILE='DEBUG1_SSTcmp_FROM_WRF_LANDMASK_'//TRIM(timestamp), FORM='formatted' )
!WRITE (985,'(a)') 'LANDMASK'
!DO j = jps, MIN( (jde-1), jpe )
!  DO i = ips, MIN( (ide-1), ipe )
!    WRITE (985,*) '(',i,',',j,'):  ',in_landmask_ptr_real(i,j)
!  ENDDO
!ENDDO
!CLOSE (985)
!$$$END DEBUG
     IF ( landmask_ok ) THEN
       WRITE(str,*) 'compare_data: LANDMASK compares OK'
       CALL wrf_debug ( 100 , TRIM(str) )
     ELSE
       CALL wrf_error_fatal ( TRIM(str) )
     ENDIF

     ! compare SST...  
     sst_ok = .TRUE.
     ! staggered starts/ends
     SST_COMPARE : DO j= jps , MIN( (jde-1), jpe )
       DO i= ips , MIN( (ide-1), ipe )
         IF ( file_sst_data(i,j) /= in_sst_ptr_real(i,j) ) THEN
           sst_ok = .FALSE.
           WRITE( str , * ) 'error sst mismatch at (i,j) = (',i,',',j, &
                            '), values are',file_sst_data(i,j),' and ',     &
                            in_sst_ptr_real(i,j) 
           EXIT SST_COMPARE
         ENDIF
       ENDDO
     ENDDO SST_COMPARE
!$$$DEBUG
!CALL wrf_debug( 100, 'compare_data:  writing DEBUG1_SSTcmp_FILE_SST_'//TRIM(timestamp) )
!OPEN( UNIT=985, FILE='DEBUG1_SSTcmp_FILE_SST_'//TRIM(timestamp), FORM='formatted' )
!WRITE (985,'(a)') 'SST'
!DO j = jps, MIN( (jde-1), jpe )
!  DO i = ips, MIN( (ide-1), ipe )
!    WRITE (985,*) '(',i,',',j,'):  ',file_sst_data(i,j)
!  ENDDO
!ENDDO
!CLOSE (985)
!CALL wrf_debug( 100, 'compare_data:  writing DEBUG1_SSTcmp_FROM_WRF_SST_'//TRIM(timestamp) )
!OPEN( UNIT=985, FILE='DEBUG1_SSTcmp_FROM_WRF_SST_'//TRIM(timestamp), FORM='formatted' )
!WRITE (985,'(a)') 'SST'
!DO j = jps, MIN( (jde-1), jpe )
!  DO i = ips, MIN( (ide-1), ipe )
!    WRITE (985,*) '(',i,',',j,'):  ',in_sst_ptr_real(i,j)
!  ENDDO
!ENDDO
!CLOSE (985)
!$$$END DEBUG
     IF ( sst_ok ) THEN
       WRITE(str,*) 'compare_data: SST compares OK'
       CALL wrf_debug ( 100 , TRIM(str) )
     ELSE
       CALL wrf_error_fatal ( TRIM(str) )
     ENDIF

     DEALLOCATE( in_sst_ptr_real, in_landmask_ptr_real )

!$$$DEBUG
WRITE(str,*) 'compare_data:  end, numtimes = ',numtimes
CALL wrf_debug ( 100 , TRIM(str) )
!$$$END DEBUG

   END SUBROUTINE compare_data




   ! Second-phase "init" gets decomposition information from 
   ! importState.  
   SUBROUTINE sst_component_init2( gcomp, importState, exportState, clock, rc )
     USE module_metadatautils, ONLY: GetDecompFromState
     USE module_io
     TYPE(ESMF_GridComp), TARGET, INTENT(INOUT) :: gcomp
     TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: importState
     TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: exportState
     TYPE(ESMF_Clock),    TARGET, INTENT(INOUT) :: clock
     INTEGER,                     INTENT(  OUT) :: rc
!<DESCRIPTION>
!     SST component init routine, phase 2.
!
!     The arguments are:
!       gcomp           Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

     ! Local variables
     TYPE(ESMF_RelLoc) :: horzRelloc
     TYPE(ESMF_Field) :: out_sst_field, out_landmask_field
     TYPE(ESMF_Field) :: in_sst_field, in_landmask_field
     INTEGER, PARAMETER :: NUMDIMS=2
     INTEGER :: DomainStart(NUMDIMS)
     INTEGER :: DomainEnd(NUMDIMS)
     INTEGER :: MemoryStart(NUMDIMS)
     INTEGER :: MemoryEnd(NUMDIMS)
     INTEGER :: PatchStart(NUMDIMS)
     INTEGER :: PatchEnd(NUMDIMS)
     INTEGER :: rc, i, j
     INTEGER :: ierr

    ! Get decomposition information from importState.  Note that index 
    ! values are for staggered dimensions, following the WRF convention.  
!$$$ TBH:  Note that this will only work for SPMD serial operation.  For 
!$$$ TBH:  concurrent operation (SPMD or MPMD), we will need to create a new 
!$$$ TBH:  "domdesc" suitable for the task layout of the SST component.  For
!$$$ TBH:  MPMD serial operation, we will need to extract serialized domdesc 
!$$$ TBH:  from export state metadata and de-serialize it.  Similar arguments 
!$$$ TBH:  apply to [ij][mp][se] and bdy_mask.  
     write(str,*) 'sst_component_init2: calling GetDecompFromState'
     CALL wrf_debug ( 100 , TRIM(str) )
     CALL GetDecompFromState( importState,                  &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              domdesc, bdy_mask )
     write(str,*) 'sst_component_init2: back from GetDecompFromState'
     CALL wrf_debug ( 100 , TRIM(str) )
     write(str,*) 'sst_component_init2: ids, ide, jds, jde, kds, kde = ', ids, ide, jds, jde, kds, kde
     CALL wrf_debug ( 100 , TRIM(str) )
     write(str,*) 'sst_component_init2: ims, ime, jms, jme, kms, kme = ', ims, ime, jms, jme, kms, kme
     CALL wrf_debug ( 100 , TRIM(str) )
     write(str,*) 'sst_component_init2: ips, ipe, jps, jpe, kps, kpe = ', ips, ipe, jps, jpe, kps, kpe
     CALL wrf_debug ( 100 , TRIM(str) )

     ! allocate space for data read from disk
     ALLOCATE( file_sst_data     (ims:ime,jms:jme) )
     DO j= jms, jme
       DO i= ims, ime
         file_sst_data(i,j) = -(i*1000.0 + j)/100000.0     ! obvious bad value for debugging
       ENDDO
     ENDDO
!$$$  Hmmm...  really need to load these pointers here?  Check...  
     this_data(SST_INDX)%r2d => file_sst_data
     ALLOCATE( file_landmask_data(ims:ime,jms:jme) )
     DO j= jms, jme
       DO i= ims, ime
         file_landmask_data(i,j) = -(i*1000.0 + j)/100000.0     ! obvious bad value for debugging
       ENDDO
     ENDDO
     this_data(LANDMASK_INDX)%r2d => file_landmask_data

     ! Create ESMF_Fields in importState and exportState
     ! Create ESMF_Grid.  Use exactly the same method as WRF so WRFIO will 
     ! work (ugh).  
     DomainStart(1) = ids; DomainEnd(1) = ide;
     DomainStart(2) = jds; DomainEnd(2) = jde;
     MemoryStart(1) = ims; MemoryEnd(1) = ime;
     MemoryStart(2) = jms; MemoryEnd(2) = jme;
     PatchStart(1)  = ips; PatchEnd(1)  = ipe;
     PatchStart(2)  = jps; PatchEnd(2)  = jpe
     CALL wrf_debug ( 5 , 'DEBUG sst_component_init2:  Calling ioesmf_create_grid_int()' )
     CALL ioesmf_create_grid_int( esmfgrid, NUMDIMS,      &
                                  DomainStart, DomainEnd, &
                                  MemoryStart, MemoryEnd, &
                                  PatchStart, PatchEnd )
     CALL wrf_debug ( 5 , 'DEBUG sst_component_init2:  back from ioesmf_create_grid_int()' )
     ! create ESMF_Fields
     !$$$ use CF standard_names later
     !$$$here...  This is a complete HACK!!  Need to communicate horzrelloc 
     !$$$here...  during init sometime...  
     horzrelloc=ESMF_CELL_CENTER
     ! Note use of patch dimension for POINTERs allocated by ESMF.  
     CALL wrf_debug ( 5 , 'DEBUG sst_component_init2:  Calling ESMF_GridValidate(esmfgrid)' )
     CALL ESMF_GridValidate( esmfgrid, rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       WRITE( str,* ) 'Error in ESMF_GridValidate ',   &
                      __FILE__ ,                       &
                      ', line ',                       &
                      __LINE__ ,                       &
                      ', error code = ',rc
! TBH:  debugging error exit here...
       CALL wrf_error_fatal ( TRIM(str) )
     ENDIF
     CALL wrf_debug ( 5 , 'DEBUG sst_component_init2:  back OK from ESMF_GridValidate(esmfgrid)' )
!TBH     ! let ESMF allocate tmp_data_out_sst
!TBH     ! Note that ESMF will deallocate it when the internal ESMF_Array object 
!TBH     ! is explicitly destroyed.  Assuming that we can figure out how to safely 
!TBH     ! destroy it!!!  (And we have gone around and around on this one.  @#$%!)  
!BELAY THAT:  do it ourselves for now...
     ALLOCATE( tmp_data_out_sst(ips:ipe,jps:jpe) )
     write(str,*) 'sst_component_init2: tmp_data_out_sst(', &
       LBOUND(tmp_data_out_sst,1),':',UBOUND(tmp_data_out_sst,1),',',LBOUND(tmp_data_out_sst,2),':',UBOUND(tmp_data_out_sst,2),')'
     CALL wrf_debug ( 100 , TRIM(str) )
     CALL wrf_debug ( 100, 'sst_component_init2: calling ESMF_FieldCreate(out_sst_field)' )
     out_sst_field = ESMF_FieldCreate(                 &
                       esmfgrid, tmp_data_out_sst,     &
                       copyflag=ESMF_DATA_REF,         &
                       horzrelloc=horzrelloc,          &
                       name=TRIM(datanames(SST_INDX)), &
!                       lbounds=(/ips,jps/),            &
!                       ubounds=(/ipe,jpe/),            &
                       rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       WRITE( str,* ) 'ESMF_FieldCreate(out_sst_field) failed ', &
                      __FILE__ ,                                 &
                      ', line ',                                 &
                      __LINE__ ,                                 &
                      ', error code = ',rc
       CALL wrf_error_fatal ( TRIM(str) )
     ENDIF
     CALL wrf_debug ( 100, 'sst_component_init2: back from ESMF_FieldCreate(out_sst_field)' )
     write(str,*) 'sst_component_init2: ips:ipe,jps:jpe = ', &
       ips,':',ipe,',',jps,':',jpe
     CALL wrf_debug ( 100 , TRIM(str) )
     ! validate ESMF allocation
     IF ( ( ips /= LBOUND(tmp_data_out_sst,1) ) .OR. ( ipe /= UBOUND(tmp_data_out_sst,1) ) .OR. &
          ( jps /= LBOUND(tmp_data_out_sst,2) ) .OR. ( jpe /= UBOUND(tmp_data_out_sst,2) ) ) THEN
       WRITE( str,* ) 'ESMF_FieldCreate(out_sst_field) allocation failed ', &
                      __FILE__ ,                                            &
                      ', line ',                                            &
                      __LINE__ ,                                            &
                      ', ips:ipe,jps:jpe = ',ips,':',ipe,',',jps,':',jpe,   &
                      ', tmp_data_out_sst(BOUNDS) = ',LBOUND(tmp_data_out_sst,1),':',UBOUND(tmp_data_out_sst,1),',', &
                                              LBOUND(tmp_data_out_sst,2),':',UBOUND(tmp_data_out_sst,2)
       CALL wrf_error_fatal ( TRIM(str) )
     ENDIF
!TBH     ! let ESMF allocate tmp_data_out_landmask
!TBH     ! Note that ESMF will deallocate it when the internal ESMF_Array object 
!TBH     ! is explicitly destroyed.  Assuming that we can figure out how to safely 
!TBH     ! destroy it!!!  (And we have gone around and around on this one.  @#$%!)  
!BELAY THAT:  do it ourselves for now...
     ALLOCATE( tmp_data_out_landmask(ips:ipe,jps:jpe) )
     write(str,*) 'sst_component_init2: tmp_data_out_landmask(', &
       LBOUND(tmp_data_out_landmask,1),':',UBOUND(tmp_data_out_landmask,1),',',LBOUND(tmp_data_out_landmask,2),':',UBOUND(tmp_data_out_landmask,2),')'
     CALL wrf_debug ( 100 , TRIM(str) )
     CALL wrf_debug ( 100, 'sst_component_init2: calling ESMF_FieldCreate(out_landmask_field)' )
     out_landmask_field = ESMF_FieldCreate(                      &
                            esmfgrid, tmp_data_out_landmask,     &
                            copyflag=ESMF_DATA_REF,              &
                            horzrelloc=horzrelloc,               &
                            name=TRIM(datanames(LANDMASK_INDX)), &
!                            lbounds=(/ips,jps/),                 &
!                            ubounds=(/ipe,jpe/),                 &
                            rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_FieldCreate(out_landmask_field) failed' )
     ENDIF
     CALL wrf_debug ( 100, 'sst_component_init2: back from ESMF_FieldCreate(out_landmask_field)' )
     ! validate ESMF allocation
     IF ( ( ips /= LBOUND(tmp_data_out_landmask,1) ) .OR. ( ipe /= UBOUND(tmp_data_out_landmask,1) ) .OR. &
          ( jps /= LBOUND(tmp_data_out_landmask,2) ) .OR. ( jpe /= UBOUND(tmp_data_out_landmask,2) ) ) THEN
       WRITE( str,* ) 'ESMF_FieldCreate(out_landmask_field) allocation failed ', &
                      __FILE__ ,                                            &
                      ', line ',                                            &
                      __LINE__ ,                                            &
                      ', ips:ipe,jps:jpe = ',ips,':',ipe,',',jps,':',jpe,   &
                      ', tmp_data_out_landmask(BOUNDS) = ',LBOUND(tmp_data_out_landmask,1),':',UBOUND(tmp_data_out_landmask,1),',', &
                                              LBOUND(tmp_data_out_landmask,2),':',UBOUND(tmp_data_out_landmask,2)
       CALL wrf_error_fatal ( TRIM(str) )
     ENDIF
!TBH     ! let ESMF allocate tmp_data_in_sst
!TBH     ! Note that ESMF will deallocate it when the internal ESMF_Array object 
!TBH     ! is explicitly destroyed.  Assuming that we can figure out how to safely 
!TBH     ! destroy it!!!  (And we have gone around and around on this one.  @#$%!)  
!BELAY THAT:  do it ourselves for now...
     ALLOCATE( tmp_data_in_sst(ips:ipe,jps:jpe) )
     write(str,*) 'sst_component_init2: tmp_data_in_sst(', &
       LBOUND(tmp_data_in_sst,1),':',UBOUND(tmp_data_in_sst,1),',',LBOUND(tmp_data_in_sst,2),':',UBOUND(tmp_data_in_sst,2),')'
     CALL wrf_debug ( 100 , TRIM(str) )
     CALL wrf_debug ( 100, 'sst_component_init2: calling ESMF_FieldCreate(in_sst_field)' )
     in_sst_field = ESMF_FieldCreate(                 &
                      esmfgrid, tmp_data_in_sst,      &
                      copyflag=ESMF_DATA_REF,         &
                      horzrelloc=horzrelloc,          &
                      name=TRIM(datanames(SST_INDX)), &
!                      lbounds=(/ips,jps/),            &
!                      ubounds=(/ipe,jpe/),            &
                      rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_FieldCreate(in_sst_field) failed' )
     ENDIF
     CALL wrf_debug ( 100, 'sst_component_init2: back from ESMF_FieldCreate(in_sst_field)' )
     ! validate ESMF allocation
     IF ( ( ips /= LBOUND(tmp_data_in_sst,1) ) .OR. ( ipe /= UBOUND(tmp_data_in_sst,1) ) .OR. &
          ( jps /= LBOUND(tmp_data_in_sst,2) ) .OR. ( jpe /= UBOUND(tmp_data_in_sst,2) ) ) THEN
       WRITE( str,* ) 'ESMF_FieldCreate(in_sst_field) allocation failed ', &
                      __FILE__ ,                                            &
                      ', line ',                                            &
                      __LINE__ ,                                            &
                      ', ips:ipe,jps:jpe = ',ips,':',ipe,',',jps,':',jpe,   &
                      ', tmp_data_in_sst(BOUNDS) = ',LBOUND(tmp_data_in_sst,1),':',UBOUND(tmp_data_in_sst,1),',', &
                                              LBOUND(tmp_data_in_sst,2),':',UBOUND(tmp_data_in_sst,2)
       CALL wrf_error_fatal ( TRIM(str) )
     ENDIF
!TBH     ! let ESMF allocate tmp_data_in_landmask
!TBH     ! Note that ESMF will deallocate it when the internal ESMF_Array object 
!TBH     ! is explicitly destroyed.  Assuming that we can figure out how to safely 
!TBH     ! destroy it!!!  (And we have gone around and around on this one.  @#$%!)  
!BELAY THAT:  do it ourselves for now...
     ALLOCATE( tmp_data_in_landmask(ips:ipe,jps:jpe) )
     write(str,*) 'sst_component_init2: tmp_data_in_landmask(', &
       LBOUND(tmp_data_in_landmask,1),':',UBOUND(tmp_data_in_landmask,1),',',LBOUND(tmp_data_in_landmask,2),':',UBOUND(tmp_data_in_landmask,2),')'
     CALL wrf_debug ( 100 , TRIM(str) )
     CALL wrf_debug ( 100, 'sst_component_init2: calling ESMF_FieldCreate(in_landmask_field)' )
     in_landmask_field = ESMF_FieldCreate(                      &
                           esmfgrid, tmp_data_in_landmask,      &
                           copyflag=ESMF_DATA_REF,              &
                           horzrelloc=horzrelloc,               &
                           name=TRIM(datanames(LANDMASK_INDX)), &
!                           lbounds=(/ips,jps/),                 &
!                           ubounds=(/ipe,jpe/),                 &
                           rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_FieldCreate(in_landmask_field) failed' )
     ENDIF
     CALL wrf_debug ( 100, 'sst_component_init2: back from ESMF_FieldCreate(in_landmask_field)' )
     ! validate ESMF allocation
     IF ( ( ips /= LBOUND(tmp_data_in_landmask,1) ) .OR. ( ipe /= UBOUND(tmp_data_in_landmask,1) ) .OR. &
          ( jps /= LBOUND(tmp_data_in_landmask,2) ) .OR. ( jpe /= UBOUND(tmp_data_in_landmask,2) ) ) THEN
       WRITE( str,* ) 'ESMF_FieldCreate(in_landmask_field) allocation failed ', &
                      __FILE__ ,                                            &
                      ', line ',                                            &
                      __LINE__ ,                                            &
                      ', ips:ipe,jps:jpe = ',ips,':',ipe,',',jps,':',jpe,   &
                      ', tmp_data_in_landmask(BOUNDS) = ',LBOUND(tmp_data_in_landmask,1),':',UBOUND(tmp_data_in_landmask,1),',', &
                                              LBOUND(tmp_data_in_landmask,2),':',UBOUND(tmp_data_in_landmask,2)
       CALL wrf_error_fatal ( TRIM(str) )
     ENDIF

     ! attach ESMF_Field to importState
     CALL ESMF_StateAddField( importState, in_sst_field, rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_StateAddField(in_sst_field) failed' )
     ENDIF
     CALL ESMF_StateAddField( importState, in_landmask_field, rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_StateAddField(in_landmask_field) failed' )
     ENDIF
     ! attach ESMF_Field to exportState
     CALL ESMF_StateAddField( exportState, out_sst_field, rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_StateAddField(out_sst_field) failed' )
     ENDIF
     CALL ESMF_StateAddField( exportState, out_landmask_field, rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_StateAddField(out_landmask_field) failed' )
     ENDIF

     !  Open the "SST" input data file for reading.
     write(str,'(A,A)') 'sst_component_init2: Opening data file ', &
       TRIM(sstinfilename)
     CALL wrf_message ( TRIM(str) )
     CALL wrf_open_for_read ( TRIM(sstinfilename) , &
                              mpicom ,              &
                              mpicom ,              &
                              "DATASET=INPUT" ,     &
                              fid ,                 &
                              ierr )
     IF ( ierr .NE. 0 ) THEN
       WRITE( str , FMT='(A,A,A,I8)' )  &
         'sst_component_init2: error opening ', &
         TRIM(sstinfilename),' for reading ierr=',ierr
       CALL wrf_error_fatal ( TRIM(str) )
     ENDIF
     WRITE( str , FMT='(A,A,A,I8)' ) &
       'subroutine sst_component_init2: opened file ', &
       TRIM(sstinfilename),' for reading fid=',fid
     CALL wrf_debug ( 100, TRIM(str) )

     write(str,'(A)') 'sst_component_init2: returning rc=ESMF_SUCCESS'
     CALL wrf_debug ( 100 , TRIM(str) )

     rc = ESMF_SUCCESS

   END SUBROUTINE sst_component_init2



   SUBROUTINE sst_component_run1( gcomp, importState, exportState, clock, rc )
     TYPE(ESMF_GridComp), TARGET, INTENT(INOUT) :: gcomp
     TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: importState, exportState
     TYPE(ESMF_Clock),    TARGET, INTENT(INOUT) :: clock
     INTEGER,                     INTENT(  OUT) :: rc
!<DESCRIPTION>
!     SST component run routine, phase 1.
!     Read "SST" data from file and stuff into exportState.  
!
!     The arguments are:
!       gcomp           Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

     rc = ESMF_SUCCESS

     ! Get "SST" data from file and stuff it into exportState.  
     CALL read_data( exportState, clock )

   END SUBROUTINE sst_component_run1



   SUBROUTINE sst_component_run2( gcomp, importState, exportState, clock, rc )
     TYPE(ESMF_GridComp), TARGET, INTENT(INOUT) :: gcomp
     TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: importState, exportState
     TYPE(ESMF_Clock),    TARGET, INTENT(INOUT) :: clock
     INTEGER,                     INTENT(  OUT) :: rc
!<DESCRIPTION>
!     SST component run routine, phase 2.
!     Get from importState, compare with file data, and error-exit 
!     if they differ...  If they are the same, then 
!     stuff the file data into the exportState.  
!
!     The arguments are:
!       gcomp           Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

     rc = ESMF_SUCCESS

     ! Get from importState, compare with file data, and error_exit 
     ! if they differ...  
     ! This works because WRF loads its exportState BEFORE integrating.  
     CALL wrf_clockprint ( 50, clock, 'sst_component_run2:  clock before call to compare_data()' )
     CALL compare_data( importState, clock )
     CALL wrf_clockprint ( 50, clock, 'sst_component_run2:  clock after call to compare_data()' )

   END SUBROUTINE sst_component_run2



   SUBROUTINE sst_component_finalize( gcomp, importState, exportState, clock, rc )
     USE module_io
     TYPE(ESMF_GridComp), TARGET, INTENT(INOUT) :: gcomp
     TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: importState, exportState
     TYPE(ESMF_Clock),    TARGET, INTENT(INOUT) :: clock
     INTEGER,                     INTENT(  OUT) :: rc
!<DESCRIPTION>
!     SST component finalize routine.
!
!     The arguments are:
!       gcomp           Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

     ! Local variables
     TYPE(ESMF_Field) :: tmp_field
     INTEGER :: i, ierr

     rc = ESMF_SUCCESS

     ! destroy ESMF_Fields and other "deep" objects created by this component
     ! note that this component relied on ESMF to allocate data pointers during 
     ! calls to ESMF_FieldCreate() so it also expects ESMF to free these pointers 
!$$$here...  remove duplication
     DO i=1, datacount
       ! destroy field in importState
       CALL ESMF_StateGetField( importState, TRIM(datanames(i)), tmp_field, &
                                rc=rc )
       IF (rc /= ESMF_SUCCESS) THEN
         WRITE( str , * )                                               &
           'sst_component_finalize:  ESMF_StateGetField( importState,', &
           TRIM(datanames(i)),') failed'
         CALL wrf_error_fatal ( TRIM(str) )
       ENDIF
       CALL ESMF_FieldDestroy( tmp_field, rc=rc )
       IF (rc /= ESMF_SUCCESS) THEN
         WRITE( str , * )                                              &
           'sst_component_finalize:  ESMF_FieldDestroy( importState,', &
           TRIM(datanames(i)),') failed'
         CALL wrf_error_fatal ( TRIM(str) )
       ENDIF
       ! destroy field in exportState
       CALL ESMF_StateGetField( exportState, TRIM(datanames(i)), tmp_field, &
                                rc=rc )
       IF (rc /= ESMF_SUCCESS) THEN
         WRITE( str , * )                                               &
           'sst_component_finalize:  ESMF_StateGetField( exportState,', &
           TRIM(datanames(i)),') failed'
         CALL wrf_error_fatal ( TRIM(str) )
       ENDIF
       CALL ESMF_FieldDestroy( tmp_field, rc=rc )
       IF (rc /= ESMF_SUCCESS) THEN
         WRITE( str , * )                                              &
           'sst_component_finalize:  ESMF_FieldDestroy( exportState,', &
           TRIM(datanames(i)),') failed'
         CALL wrf_error_fatal ( TRIM(str) )
       ENDIF
     ENDDO

     ! deallocate space for data read from disk
     DEALLOCATE( file_sst_data, file_landmask_data )

     ! close SST data file
     WRITE( str , FMT='(A,I8)' ) &
       'subroutine sst_component_finalize: closing file fid=',fid
     CALL wrf_debug ( 100, TRIM(str) )
     CALL wrf_ioclose( fid , ierr )
     IF ( ierr .NE. 0 ) THEN
       CALL wrf_error_fatal ( 'sst_component_finalize:  wrf_ioclose failed' )
     ENDIF

   END SUBROUTINE sst_component_finalize


END MODULE module_sst_component_top




MODULE module_sst_setservices
!<DESCRIPTION>
! This module defines SST "Set Services" method sst_register() 
! used for ESMF coupling.  
!</DESCRIPTION>

   USE module_sst_component_top, ONLY: sst_component_init1,  &
                                       sst_component_init2,  &
                                       sst_component_run1,   &
                                       sst_component_run2,   &
                                       sst_component_finalize
   USE ESMF_Mod

   IMPLICIT NONE

   ! everything is private by default
   PRIVATE

   ! Public entry point for ESMF_GridCompSetServices()
   PUBLIC SST_register

   ! private stuff
   CHARACTER (ESMF_MAXSTR) :: str

CONTAINS


   SUBROUTINE sst_register(gcomp, rc)
     TYPE(ESMF_GridComp), INTENT(INOUT) :: gcomp
     INTEGER, INTENT(OUT) :: rc
     INTEGER :: finalrc
!
!<DESCRIPTION>
!     SST_register - Externally visible registration routine
!
!     User-supplied SetServices routine.
!     The Register routine sets the subroutines to be called
!     as the init, run, and finalize routines.  Note that these are
!     private to the module.
!
!     The arguments are:
!       gcomp           Component
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

     finalrc = ESMF_SUCCESS
     ! Register the callback routines.
     call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, &
                                     sst_component_init1, 1, rc)
     IF ( rc /= ESMF_SUCCESS) THEN
        WRITE(str,*) 'ESMF_GridCompSetEntryPoint(sst_component_init1) failed with rc = ', rc
        CALL wrf_error_fatal ( TRIM(str) )
     ENDIF
     call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, &
                                     sst_component_init2, 2, rc)
     IF ( rc /= ESMF_SUCCESS) THEN
        WRITE(str,*) 'ESMF_GridCompSetEntryPoint(sst_component_init2) failed with rc = ', rc
        CALL wrf_error_fatal ( TRIM(str) )
     ENDIF
     call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, &
                                     sst_component_run1, 1, rc)
     IF ( rc /= ESMF_SUCCESS) THEN
        WRITE(str,*) 'ESMF_GridCompSetEntryPoint(sst_component_run1) failed with rc = ', rc
        CALL wrf_error_fatal ( TRIM(str) )
     ENDIF
     call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, &
                                     sst_component_run2, 2, rc)
     IF ( rc /= ESMF_SUCCESS) THEN
        WRITE(str,*) 'ESMF_GridCompSetEntryPoint(sst_component_run2) failed with rc = ', rc
        CALL wrf_error_fatal ( TRIM(str) )
     ENDIF
     call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, &
                                     sst_component_finalize, ESMF_SINGLEPHASE, rc)
     IF ( rc /= ESMF_SUCCESS) THEN
        WRITE(str,*) 'ESMF_GridCompSetEntryPoint(sst_component_finalize) failed with rc = ', rc
        CALL wrf_error_fatal ( TRIM(str) )
     ENDIF

     PRINT *,'SST:  Registered Initialize, Run, and Finalize routines'

     rc = finalrc

   END SUBROUTINE sst_register

END MODULE module_sst_setservices



!<DESCRIPTION>
! Module module_wrfsst_coupler defines the 
! "WRF-SST" coupler component.  It provides two-way coupling between 
! the "SST" and "WRF" components.  
! In its run routine it transfers data directly from the 
! SST Component's export state to the WRF Component's import state.  
! It also transfers data directly from the 
! WRF Component's export state to the SST Component's import state.  
!
! This is derived from src/demo/coupled_flow/src/CouplerMod.F90
! created by Nancy Collins and others on the ESMF Core Team.  
!
!</DESCRIPTION>

MODULE module_wrfsst_coupler

    USE ESMF_Mod

    IMPLICIT NONE
    
    ! everything is private by default
    PRIVATE

    ! Public entry point 
    PUBLIC WRFSSTCpl_register

    ! private data members
    ! route handles and flags
    TYPE(ESMF_RouteHandle), SAVE :: fromWRF_rh, fromSST_rh
    LOGICAL, SAVE :: fromWRF_rh_ready = .FALSE.
    LOGICAL, SAVE :: fromSST_rh_ready = .FALSE.
    ! field names
    INTEGER, PARAMETER :: datacount = 2
    INTEGER, PARAMETER :: SST_INDX = 1
    INTEGER, PARAMETER :: LANDMASK_INDX = 2
    CHARACTER(LEN=ESMF_MAXSTR), SAVE :: datanames(datacount)
    CHARACTER(LEN=ESMF_MAXSTR) :: str


CONTAINS


   SUBROUTINE WRFSSTCpl_register(comp, rc)
     TYPE(ESMF_CplComp), INTENT(INOUT) :: comp
     INTEGER, INTENT(OUT) :: rc
!
!<DESCRIPTION>
!     WRFSSTCpl_register - Externally visible registration routine
!
!     User-supplied SetServices routine.
!     The Register routine sets the subroutines to be called
!     as the init, run, and finalize routines.  Note that these are
!     private to the module.
!
!     The arguments are:
!       comp            Component
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

      ! guilty until proven innocent
      rc = ESMF_FAILURE

      ! Register the callback routines.

      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, WRFSSTCpl_init, &
                                     ESMF_SINGLEPHASE, rc)
      IF ( rc /= ESMF_SUCCESS ) THEN
        CALL wrf_error_fatal ( 'ESMF_CplCompSetEntryPoint(WRFSSTCpl_init) failed' )
      ENDIF
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, WRFSSTCpl_run, &
                                     ESMF_SINGLEPHASE, rc)
      IF ( rc /= ESMF_SUCCESS ) THEN
        CALL wrf_error_fatal ( 'ESMF_CplCompSetEntryPoint(WRFSSTCpl_run) failed' )
      ENDIF
      call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, WRFSSTCpl_final, &
                                     ESMF_SINGLEPHASE, rc)
      IF ( rc /= ESMF_SUCCESS ) THEN
        CALL wrf_error_fatal ( 'ESMF_CplCompSetEntryPoint(WRFSSTCpl_final) failed' )
      ENDIF

      print *, "module_wrfsst_coupler: Registered Initialize, Run, and Finalize routines"

    END SUBROUTINE WRFSSTCpl_register


    SUBROUTINE WRFSSTCpl_init(comp, importState, exportState, clock, rc)
      USE module_metadatautils, ONLY: AttachDecompToState, GetDecompFromState
      TYPE(ESMF_CplComp), INTENT(INOUT) :: comp
      TYPE(ESMF_State), INTENT(INOUT) :: importState, exportState
      TYPE(ESMF_Clock), INTENT(INOUT) :: clock
      INTEGER, INTENT(OUT) :: rc
!<DESCRIPTION>
!     WRF-SST coupler component init routine.  This simply passes needed 
!     metadata from WRF to SST.  Initialization of ESMF_RouteHandle objects 
!     is handled later via lazy evaluation.  
!
!     The arguments are:
!       comp            Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

      ! Local variables
      CHARACTER(ESMF_MAXSTR) :: importstatename
      ! decomposition information
      INTEGER :: ids, ide, jds, jde, kds, kde
      INTEGER :: ims, ime, jms, jme, kms, kme
      INTEGER :: ips, ipe, jps, jpe, kps, kpe
      INTEGER :: domdesc
      LOGICAL :: bdy_mask(4)

      PRINT *, "DEBUG:  Coupler Init starting"

      ! guilty until proven innocent
      rc = ESMF_FAILURE

      CALL ESMF_StateGet(importState, name=importstatename, rc=rc)
      IF ( rc /= ESMF_SUCCESS ) THEN
        CALL wrf_error_fatal ( 'WRFSSTCpl_init:  ESMF_StateGet failed' )
      ENDIF

      IF ( TRIM(importstatename) .EQ. "WRF Export State" ) THEN
        ! get metadata from WRF export state
        CALL GetDecompFromState( importState,                  &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                                 domdesc, bdy_mask )
        ! put metadata from in SST import state
        CALL AttachDecompToState( exportState,                  &
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  ips, ipe, jps, jpe, kps, kpe, &
                                  domdesc, bdy_mask )


      ELSE
        CALL wrf_error_fatal ( 'WRFSSTCpl_init:  invalid importState name' )
      ENDIF

      ! set up field names
!$$$ use CF conventions for "standard_name" once WRF Registry supports them
!$$$     datanames(SST_INDX) = "sea_surface_temperature"
!$$$     datanames(LANDMASK_INDX) = "land_binary_mask"
      datanames(SST_INDX) = "SST"
      datanames(LANDMASK_INDX) = "LANDMASK"

      PRINT *, "DEBUG:  Coupler Init returning"
   
    END SUBROUTINE WRFSSTCpl_init



    SUBROUTINE WRFSSTCpl_run(comp, importState, exportState, clock, rc)
!$$$DEBUG
! get ips,ipe, ... from this hack for debugging
      USE module_sst_component_top
!$$$END DEBUG
      TYPE(ESMF_CplComp), INTENT(INOUT) :: comp
      TYPE(ESMF_State), INTENT(INOUT) :: importState, exportState
      TYPE(ESMF_Clock), INTENT(INOUT) :: clock
      INTEGER, INTENT(OUT) :: rc
!<DESCRIPTION>
!     WRF-SST coupler component run routine.
!
!     The arguments are:
!       comp            Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

      ! Local variables
      TYPE(ESMF_Field) :: src_field, dst_field
      TYPE(ESMF_RouteHandle) :: routehandle
      TYPE(ESMF_VM) :: vm
      LOGICAL :: build_fromWRF_rh, build_fromSST_rh, fromWRF
      CHARACTER(LEN=ESMF_MAXSTR) :: importStatename
      CHARACTER(LEN=ESMF_MAXSTR) :: SST_exportStatename, WRF_exportStatename
      INTEGER :: i
!$$$DEBUG
      TYPE(ESMF_Time) :: currentTime
      CHARACTER(LEN=256) :: timestamp, directionString
      INTEGER :: ii, jj
      REAL(ESMF_KIND_R4), POINTER :: tmp_data_ptr(:,:)
!$$$END DEBUG

      WRITE(str,*) 'WRFSSTCpl_run: begin'
      CALL wrf_debug ( 100 , TRIM(str) )

!$$$DEBUG
CALL ESMF_ClockGet( clock, CurrTime=currentTime, rc=rc )
IF (rc /= ESMF_SUCCESS) THEN
  CALL wrf_error_fatal ( 'WRFSSTCpl_run:  ESMF_ClockGet() failed' )
ENDIF
CALL wrf_timetoa ( currentTime, timestamp )
!$$$END DEBUG

      ! guilty until proven innocent
      rc = ESMF_FAILURE

      ! Which way is this coupling going?  
      WRITE(str,*) 'WRFSSTCpl_run: calling ESMF_StateGet(importState,name,...)'
      CALL wrf_debug ( 100 , TRIM(str) )
      CALL ESMF_StateGet( importState, name=importStatename, rc=rc )
      IF ( rc /= ESMF_SUCCESS ) THEN
        CALL wrf_error_fatal ( 'WRFSSTCpl_run:  ESMF_StateGet(importState,name,...) failed' )
      ENDIF
      WRITE(str,*) 'WRFSSTCpl_run: back from ESMF_StateGet, importStatename = <',TRIM(importStatename),'>'
      CALL wrf_debug ( 100 , TRIM(str) )

      ! first time through in each direction:  create route handle and 
      ! associated objects
      WRF_exportStatename = "WRF Export State"
      SST_exportStatename = "SST Export State"
      IF ( TRIM(importStatename) .EQ. TRIM(WRF_exportStatename) ) THEN
        fromWRF = .TRUE.
        directionString = 'WRFtoSST'
      ELSE IF ( TRIM(importStatename) .EQ. TRIM(SST_exportStatename) ) THEN
        fromWRF = .FALSE.
        directionString = 'SSTtoWRF'
      ELSE
        CALL wrf_error_fatal ( 'WRFSSTCpl_run:  invalid importState name' )
      ENDIF
      WRITE(str,*) 'WRFSSTCpl_run: fromWRF = ',fromWRF
      CALL wrf_debug ( 100 , TRIM(str) )
      build_fromWRF_rh =         fromWRF   .AND. ( .NOT. fromWRF_rh_ready )
      build_fromSST_rh = ( .NOT. fromWRF ) .AND. ( .NOT. fromSST_rh_ready )
      WRITE(str,*) 'WRFSSTCpl_run: build_fromWRF_rh = ',build_fromWRF_rh
      CALL wrf_debug ( 100 , TRIM(str) )
      WRITE(str,*) 'WRFSSTCpl_run: build_fromSST_rh = ',build_fromSST_rh
      CALL wrf_debug ( 100 , TRIM(str) )
      IF ( build_fromWRF_rh .OR. build_fromSST_rh ) THEN
        CALL ESMF_CplCompGet( comp, vm=vm, rc=rc )
        IF ( rc /= ESMF_SUCCESS ) THEN
          CALL wrf_error_fatal ( 'WRFSSTCpl_run:  ESMF_CplCompGet failed' )
        ENDIF
        ! The use of literal index "1" here indicates that we don't care which 
        ! ESMF_Field we get so we might as well get the first one.  
!$$$ Right now, staggering of all fields is identical.  Do we need more than one 
!$$$ routeHandle if there is more than one staggering?  
        WRITE(str,*) 'WRFSSTCpl_run: grabbing first field <',TRIM(datanames(1)), &
                     '> from import state'
        CALL wrf_debug ( 100 , TRIM(str) )
        CALL ESMF_StateGetField( importState, TRIM(datanames(1)), src_field, &
                                 rc=rc )
        IF ( rc /= ESMF_SUCCESS ) THEN
          CALL wrf_error_fatal ( 'WRFSSTCpl_run:  ESMF_StateGetField(importState) failed' )
        ENDIF
        WRITE(str,*) 'WRFSSTCpl_run: grabbing first field <',TRIM(datanames(1)), &
                     '> from export state'
        CALL wrf_debug ( 100 , TRIM(str) )
        CALL ESMF_StateGetField( exportState, TRIM(datanames(1)), dst_field, &
                                 rc=rc )
        IF ( rc /= ESMF_SUCCESS ) THEN
          CALL wrf_error_fatal ( 'WRFSSTCpl_run:  ESMF_StateGetField(exportState) failed' )
        ENDIF
        IF ( build_fromWRF_rh ) THEN
          WRITE(str,*) 'WRFSSTCpl_run: creating fromWRF_rh'
          CALL wrf_debug ( 100 , TRIM(str) )
          fromWRF_rh = ESMF_RouteHandleCreate( rc )
          IF ( rc /= ESMF_SUCCESS ) THEN
            CALL wrf_error_fatal ( 'WRFSSTCpl_run:  ESMF_RouteHandleCreate(fromWRF_rh) failed' )
          ENDIF
          WRITE(str,*) 'WRFSSTCpl_run: calling ESMF_FieldRedistStore(fromWRF_rh)'
          CALL wrf_debug ( 100 , TRIM(str) )
          CALL ESMF_FieldRedistStore( src_field, dst_field, vm, &
                                      routehandle=fromWRF_rh, rc=rc )
          IF ( rc /= ESMF_SUCCESS ) THEN
            CALL wrf_error_fatal ( 'WRFSSTCpl_run:  ESMF_FieldRedistStore(fromWRF_rh) failed' )
          ENDIF
          fromWRF_rh_ready = .TRUE.
        ENDIF
        IF ( build_fromSST_rh ) THEN
          WRITE(str,*) 'WRFSSTCpl_run: creating fromSST_rh'
          CALL wrf_debug ( 100 , TRIM(str) )
          fromSST_rh = ESMF_RouteHandleCreate( rc )
          IF ( rc /= ESMF_SUCCESS ) THEN
            CALL wrf_error_fatal ( 'WRFSSTCpl_run:  ESMF_RouteHandleCreate(fromSST_rh) failed' )
          ENDIF
          WRITE(str,*) 'WRFSSTCpl_run: calling ESMF_FieldRedistStore(fromSST_rh)'
          CALL wrf_debug ( 100 , TRIM(str) )
          CALL ESMF_FieldRedistStore( src_field, dst_field, vm, &
                                      routehandle=fromSST_rh, rc=rc )
          IF ( rc /= ESMF_SUCCESS ) THEN
            CALL wrf_error_fatal ( 'WRFSSTCpl_run:  ESMF_FieldRedistStore(fromSST_rh) failed' )
          ENDIF
          fromSST_rh_ready = .TRUE.
        ENDIF
        DO i=1, datacount
          WRITE(str,*) 'WRFSSTCpl_run: calling ESMF_StateSetNeeded(importState, ',TRIM(datanames(i)),')'
          CALL wrf_debug ( 100 , TRIM(str) )
          CALL ESMF_StateSetNeeded( importState, TRIM(datanames(i)), &
                                    ESMF_NEEDED, rc=rc )
          IF ( rc /= ESMF_SUCCESS ) THEN
            WRITE(str,*) 'WRFSSTCpl_run:  ESMF_StateSetNeeded(',TRIM(datanames(i)),') failed'
            CALL wrf_error_fatal ( str )
          ENDIF
        ENDDO
      ENDIF

      ! In this case, the coupling is symmetric - you call redist going
      ! both ways - so we only care about the coupling direction in order 
      ! to get the right routehandle selected.
      IF ( fromWRF ) THEN
        WRITE(str,*) 'WRFSSTCpl_run: routehandle = fromWRF_rh'
        CALL wrf_debug ( 100 , TRIM(str) )
        routehandle = fromWRF_rh 
      ELSE
        WRITE(str,*) 'WRFSSTCpl_run: routehandle = fromSST_rh'
        CALL wrf_debug ( 100 , TRIM(str) )
        routehandle = fromSST_rh 
      ENDIF

      DO i=1, datacount
        WRITE(str,*) 'WRFSSTCpl_run: grabbing field <',TRIM(datanames(i)),'>'
        CALL wrf_debug ( 100 , TRIM(str) )
        ! check isneeded flag here
        IF ( .NOT. ESMF_StateIsNeeded( importState, TRIM(datanames(i)), rc=rc ) ) THEN 
          IF ( rc /= ESMF_SUCCESS ) THEN
            WRITE(str,*) 'WRFSSTCpl_run:  ESMF_StateIsNeeded(',TRIM(datanames(i)),') failed'
            CALL wrf_error_fatal ( str )
          ENDIF
          WRITE(str,*) 'WRFSSTCpl_run: skipping field <',TRIM(datanames(i)),'>'
          CALL wrf_debug ( 100 , TRIM(str) )
          CYCLE
        ENDIF

        WRITE(str,*) 'WRFSSTCpl_run: processing field <',TRIM(datanames(i)),'>'
        CALL wrf_debug ( 100 , TRIM(str) )

!   The following piece of code provides an example of calling the data
!   redistribution routine  between two Fields in the Coupler Component.  
!   Unlike regrid, which translates between
!   different Grids, redist translates between different DELayouts on
!   the same Grid.   The first two lines get the Fields from the 
!   States, each corresponding to a different subcomponent.  One is
!   an Export State and the other is an Import State.
!
        WRITE(str,*) 'WRFSSTCpl_run:  calling ESMF_StateGetField(importState,', &
                     TRIM(datanames(i)),')...'
        CALL wrf_debug ( 100 , TRIM(str) )
        CALL ESMF_StateGetField( importState, TRIM(datanames(i)), src_field, &
                                 rc=rc )
        IF ( rc /= ESMF_SUCCESS ) THEN
          WRITE(str,*) 'WRFSSTCpl_run:  ESMF_StateGetField(importState,', &
                       TRIM(datanames(i)),') failed'
          CALL wrf_error_fatal ( str )
        ENDIF

!$$$$ debugging...
!$$$     CALL ESMF_CplCompRun(compCplWRFSST, exportStateSST, &
!$$$                          importStateWRF, driverClock, rc=rc)
!$$$ Why is LANDMASK not on importStateWRF?  May be moot now due to fix in Registry...  

        WRITE(str,*) 'WRFSSTCpl_run:  calling ESMF_StateGetField(exportState,', &
                     TRIM(datanames(i)),')...'
        CALL wrf_debug ( 100 , TRIM(str) )
        CALL ESMF_StateGetField( exportState, TRIM(datanames(i)), dst_field, &
                                 rc=rc )
        IF ( rc /= ESMF_SUCCESS ) THEN
          WRITE(str,*) 'WRFSSTCpl_run:  ESMF_StateGetField(exportState,', &
                       TRIM(datanames(i)),') failed'
          CALL wrf_error_fatal ( str )
        ENDIF

!   The redist routine uses information contained in the Fields and the
!   Coupler VM object to call the communication routines to move the data.
!   Because many Fields may share the same Grid association, the same
!   routing information may be needed repeatedly.  Route information is 
!   saved so the precomputed information can be retained.  The following 
!   is an example of a Field redist call:
!$$$DEBUG
!CALL wrf_debug ( 100, 'WRFSSTCpl_run '//TRIM(directionString)//':  calling ESMF_FieldPrint( src_field )' )
!CALL ESMF_FieldPrint( src_field, rc=rc )
!CALL wrf_debug ( 100, 'WRFSSTCpl_run '//TRIM(directionString)//':  back from ESMF_FieldPrint( src_field )' )
!CALL ESMF_FieldGetDataPointer( src_field, tmp_data_ptr, rc=rc )
!IF (rc /= ESMF_SUCCESS) THEN
!  WRITE(str,*) 'WRFSSTCpl_run '//TRIM(directionString)//':  ESMF_FieldGetDataPointer( src_field, '//TRIM(datanames(i))//' ) returned rc = ',rc
!  CALL wrf_debug ( 100 , TRIM(str) )
!  CALL wrf_error_fatal ( &
!    'WRFSSTCpl_run '//TRIM(directionString)//':  ESMF_FieldGetDataPointer( src_field, ... ) failed' )
!ENDIF
!CALL wrf_debug( 100, 'WRFSSTCpl_run:  writing DEBUG1_CPLcmp_src_'//TRIM(directionString)//'_'//TRIM(datanames(i))//'_'//TRIM(timestamp) )
!OPEN( UNIT=985, FILE='DEBUG1_CPLcmp_src_'//TRIM(directionString)//'_'//TRIM(datanames(i))//'_'//TRIM(timestamp), FORM='formatted' )
!WRITE (985,'(a)') TRIM(datanames(i))
!DO jj = jps, MIN( (jde-1), jpe )
!  DO ii = ips, MIN( (ide-1), ipe )
!    WRITE (985,*) '(',ii,',',jj,'):  ',tmp_data_ptr(ii,jj)
!  ENDDO
!ENDDO
!CLOSE (985)
!$$$END DEBUG
        WRITE(str,*) 'WRFSSTCpl_run:  calling ESMF_FieldRedist for <', &
                     TRIM(datanames(i)),'>...'
        CALL wrf_debug ( 100 , TRIM(str) )
        CALL ESMF_FieldRedist( src_field, dst_field, routehandle, rc=rc )
        IF ( rc /= ESMF_SUCCESS ) THEN
          CALL wrf_error_fatal ( 'WRFSSTCpl_run:  ESMF_FieldRedist failed' )
        ENDIF
        WRITE(str,*) 'WRFSSTCpl_run:  back from ESMF_FieldRedist for <', &
                     TRIM(datanames(i)),'>...'
        CALL wrf_debug ( 100 , TRIM(str) )
!$$$DEBUG
!CALL wrf_debug ( 100, 'WRFSSTCpl_run '//TRIM(directionString)//':  calling ESMF_FieldPrint( dst_field )' )
!CALL ESMF_FieldPrint( dst_field, rc=rc )
!CALL wrf_debug ( 100, 'WRFSSTCpl_run '//TRIM(directionString)//':  back from ESMF_FieldPrint( dst_field )' )
!CALL ESMF_FieldGetDataPointer( dst_field, tmp_data_ptr, rc=rc )
!IF (rc /= ESMF_SUCCESS) THEN
!  WRITE(str,*) 'WRFSSTCpl_run '//TRIM(directionString)//':  ESMF_FieldGetDataPointer( dst_field, '//TRIM(datanames(i))//' ) returned rc = ',rc
!  CALL wrf_debug ( 100 , TRIM(str) )
!  CALL wrf_error_fatal ( &
!    'WRFSSTCpl_run '//TRIM(directionString)//':  ESMF_FieldGetDataPointer( dst_field, ... ) failed' )
!ENDIF
!CALL wrf_debug( 100, 'WRFSSTCpl_run:  writing DEBUG1_CPLcmp_dst_'//TRIM(directionString)//'_'//TRIM(datanames(i))//'_'//TRIM(timestamp) )
!OPEN( UNIT=985, FILE='DEBUG1_CPLcmp_dst_'//TRIM(directionString)//'_'//TRIM(datanames(i))//'_'//TRIM(timestamp), FORM='formatted' )
!WRITE (985,'(a)') TRIM(datanames(i))
!DO jj = jps, MIN( (jde-1), jpe )
!  DO ii = ips, MIN( (ide-1), ipe )
!    WRITE (985,*) '(',ii,',',jj,'):  ',tmp_data_ptr(ii,jj)
!  ENDDO
!ENDDO
!CLOSE (985)
!$$$END DEBUG

      ENDDO

      WRITE(str,*) 'WRFSSTCpl_run: end'
      CALL wrf_debug ( 100 , TRIM(str) )

    END SUBROUTINE WRFSSTCpl_run



    SUBROUTINE WRFSSTCpl_final(comp, importState, exportState, clock, rc)
      TYPE(ESMF_CplComp) :: comp
      TYPE(ESMF_State), INTENT(INOUT) :: importState, exportState
      TYPE(ESMF_Clock), INTENT(INOUT) :: clock
      INTEGER, INTENT(OUT) :: rc
!<DESCRIPTION>
!     WRF-SST coupler component finalize routine.
!
!     The arguments are:
!       comp            Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

      PRINT *, "DEBUG:  Coupler Final starting"
   
      ! guilty until proven innocent
      rc = ESMF_FAILURE

      ! Only thing to do here is release redist and route handles
      IF ( fromWRF_rh_ready ) THEN
        CALL ESMF_FieldRedistRelease(fromWRF_rh, rc)
        IF ( rc /= ESMF_SUCCESS ) THEN
          CALL wrf_error_fatal ( 'WRFSSTCpl_final:  ESMF_FieldRedistRelease(fromWRF_rh) failed' )
        ENDIF
        CALL ESMF_RouteHandleDestroy(fromWRF_rh, rc)
        IF ( rc /= ESMF_SUCCESS ) THEN
          CALL wrf_error_fatal ( 'WRFSSTCpl_final:  ESMF_RouteHandleDestroy(fromWRF_rh) failed' )
        ENDIF
      ENDIF
      IF ( fromSST_rh_ready ) THEN
        CALL ESMF_FieldRedistRelease(fromSST_rh, rc)
        IF ( rc /= ESMF_SUCCESS ) THEN
          CALL wrf_error_fatal ( 'WRFSSTCpl_final:  ESMF_FieldRedistRelease(fromSST_rh) failed' )
        ENDIF
        CALL ESMF_RouteHandleDestroy(fromSST_rh, rc)
        IF ( rc /= ESMF_SUCCESS ) THEN
          CALL wrf_error_fatal ( 'WRFSSTCpl_final:  ESMF_RouteHandleDestroy(fromSST_rh) failed' )
        ENDIF
      ENDIF

      PRINT *, "DEBUG:  Coupler Final returning"
   
    END SUBROUTINE WRFSSTCpl_final


END MODULE module_wrfsst_coupler
    
    


PROGRAM wrf_SST_ESMF

!$$$AAAA

!$$$TBH:   update this documentation!  

!<DESCRIPTION>
! ESMF Application Wrapper for coupling WRF with a "dummy" component 
! that simply reads SSTs from a file and sends them to WRF (one-way 
! coupling).
!
! Note that, like other WRF coupling methods (MCEL, MCT), ESMF coupling is 
! supported only via auxiliary input and history streams.  
!
! This is the main program that creates the ESMF Gridded and Coupler 
! Component.  
!
! "init" looks like this:  
!   1. Init phase 1 for WRF, sets WRF exportState metadata for "time" 
!      and "domain" information needed by WRF IOAPI (which is called from 
!      the SST component).  It also sets up all WRF and WSF modules.  Note 
!      that this must be called before SST phase-1 init because SST uses 
!      WRF IOAPI.  
!   2. Init phase 1 for SST, sets "time" metadata in SST exportState.  
!   3. Initialize coupler, passing decomposition metadata from WRF exportState 
!      to SST importState.  
!   4. Resolve any "time" metadata inconsistencies and create top-level clock.  
!   5. Init phase 2 for SST, gets "domain" information from importState, 
!      creates an ESMF_Grid based on "domain" information using the exact same 
!      method as WRF (so WRF IOAPI calls will work), and sets up SST 
!      importState and exportState.  
!   6. Init phase 2 for WRF, runs up to the end of the head_grid I/O "training" 
!      phase (done in med_before_solve_io()).  This initializes WRF 
!      importState and exportState prior to the first coupling step during the 
!      "run" loop.  Note that this only works for head_grid at present because 
!      recursion in WRF traversal of subdomains is not dealt with yet and 
!      because the code that populates the WRF importState and exportState is 
!      not yet sophisticated enough to handle creating and destroying nested 
!      domains at any time during the model run.  
!$$$   NOTE:  At the moment, any ESMF auxio that does not begin at the start 
!$$$          of the model run will FAIL due to the way WRF init phases have 
!$$$          been split.  A solution would be to split the WRF run into two 
!$$$          phases instead and run the first part, which will stop after 
!$$$          "training", at the very start of the "run" loop".  The main 
!$$$          implication of this change would be that WRF import and export 
!$$$          states would not be valid until after first-phase run were 
!$$$          called.  A nasty business either way.    TBH
!
!$$$here...  Note that we really need nested states, one for each auxio stream!!
!$$$here...  For now, only support one input and/or one output stream via 
!$$$here...  io_esmf.  This condition is asserted in 
!$$$here...  ext_esmf_open_for_read_begin() and 
!$$$here...  ext_esmf_open_for_write_begin().  
!
! "run" loop looks like this:  
!   1. Run SST phase 1, reads SST from file and writes it to SST exportState 
!      for coupling to WRF.  
!   2. Couple SST exportState -> WRF importState.  First iteration:  set up 
!      SST->WRF routeHandle via lazy evaluation.  
!   3. Run WRF.  First iteration:  head_grid resumes after I/O "training" 
!      phase.  Other iterations and domains:  run normally.  
!      Read WRF importState and write WRF exportState (via med_before_solve_io()).  
!      Note that WRF assigns sst -> tsk for sea points in 
!      share/module_soil_pre.F.  
!$$$here...  However, WRF does NOT assign tsk -> sst.  Do we need to send TSK 
!$$$here...  from WRF too for self-test?  
!$$$here...  eventually couple LANDMASK on first iteration only
!$$$here...  For concurrent coupling, must break wrf_run into two phases, first 
!$$$here...  phase returns after the call to med_before_solve_io(), second phase 
!$$$here...  resumes after the call to med_before_solve_io().  This is 
!$$$here...  *relatively* easy if we limit ESMF coupling to head_grid, but is 
!$$$here...  NOT so easy otherwise due to recursion.  Also, we will need 
!$$$here...  dynamic ESMF_States to couple to WRF nested domains since the 
!$$$here...  nested domains may be created/destroyed at any time during the 
!$$$here...  model run!  Not clear that using ESMF to couple directly to WRF 
!$$$here...  nested domains is a small effort, and not clear that it is needed.  
!
!$$$  Note that moving init phase-2 to a first run phase and then splitting 
!$$$  yet again after med_before_solve_io() would lead to three run phases for 
!$$$  WRF.  One could argue that since the current "everyone calls everything" 
!$$$  ESMF model for "concurrent components" is suboptimal for loosely-coupled 
!$$$  concurrency anyway, we should aviod the split after 
!$$$  med_before_solve_io(), limit ESMF use in WRF to sequential coupling, and 
!$$$  use MCEL/MCT for concurrent coupling.  Food for thought...    TBH
!
!   4. Couple WRF exportState -> SST importState.  First iteration:  set up
!      WRF->SST routeHandle via lazy evaluation.
!   5. Run SST phase 2, compare SST from file with SST from WRF (via 
!      SST importState) and error-exit if they differ.  
!   6. Advance clock and goto step 1
!
! "finalize" is trivial, except for destruction of ESMF objects which is 
! quite non-trivial at the moment.  
!
!</DESCRIPTION>

!$$$ TBH:  Need to eliminate duplication between wrf_ESMFApp.F 
!$$$ TBH:  and wrf_SST_ESMF.F.  

   ! WRF registration routine
   USE module_wrf_setservices, ONLY: WRF_register
   ! SST registration routine
   USE module_sst_setservices, ONLY: SST_register
   ! WRF-SST coupler registration routine
   USE module_wrfsst_coupler, ONLY: WRFSSTCpl_register
   ! ESMF module, defines all ESMF data types and procedures
   USE ESMF_Mod
   ! Not-yet-implemented ESMF features
   USE module_esmf_extensions
   ! Component-independent utilities
   USE module_metadatautils, ONLY: GetTimesFromStates

   IMPLICIT NONE

   ! Local variables

   ! Components
   TYPE(ESMF_GridComp) :: compGriddedWRF   ! WRF
   TYPE(ESMF_GridComp) :: compGriddedSST   ! SST reader
   TYPE(ESMF_CplComp) :: compCplWRFSST     ! WRF-SST coupler

   ! State, Virtual Machine, and DELayout
   TYPE(ESMF_VM) :: vm
   TYPE(ESMF_State) :: importStateWRF, exportStateWRF
   TYPE(ESMF_State) :: importStateSST, exportStateSST

   ! A clock, some times, and a time step
   TYPE(ESMF_Clock) :: driverClock
   TYPE(ESMF_Time) :: startTime
   TYPE(ESMF_Time) :: stopTime
   TYPE(ESMF_TimeInterval) :: couplingInterval

   ! other misc stuff
   TYPE(ESMF_State) :: tmpState
   INTEGER :: timestepdebug

   ! Return codes for error checks
   INTEGER :: rc
   CHARACTER (ESMF_MAXSTR) :: str

   ! debugging
   CHARACTER(LEN=256) :: couplingIntervalString

        
   ! Warn users that this is not yet ready for general use.  
   PRINT *, '                      W A R N I N G                          '
   PRINT *, '  ESMF COUPLING CAPABILITY IS EXPERIMENTAL AND UNSUPPORTED   '
   PRINT *, '               IN THIS VERSION OF WRF-SST                    '
   PRINT *, '          U S E   A T   Y O U R   O W N   R I S K            '

   ! This call includes everything that must be done before ESMF_Initialize()
   ! is called.
   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)

   ! Initialize ESMF, get the default Global VM, and set
   ! the default calendar to be Gregorian.
   CALL ESMF_Initialize( vm=vm, defaultCalendar=ESMF_CAL_GREGORIAN, rc=rc )
   IF ( rc /= ESMF_SUCCESS ) THEN
     PRINT *, 'wrf_SST_ESMF:  ESMF_Initialize failed'
   ENDIF
   ! Note:  wrf_debug and wrf_error_fatal are not initialized yet
   PRINT *, 'DEBUG wrf_SST_ESMF:  returned from ESMF_Initialize'
   CALL ESMF_SetInitialized()   ! eliminate this once ESMF does it internally

   ! Create the WRF Gridded Component, passing in the default VM.
   compGriddedWRF = ESMF_GridCompCreate(vm, "WRF Model", rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     PRINT *, 'wrf_SST_ESMF:  ESMF_GridCompCreate(WRF Model) failed'
   ENDIF

   ! Create the SST Gridded Component, passing in the default VM.
   compGriddedSST = ESMF_GridCompCreate(vm, "SST Dummy Model", rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     PRINT *, 'wrf_SST_ESMF:  ESMF_GridCompCreate(WRF Dummy Model) failed'
   ENDIF

   ! Create the WRF-SST Coupler Component, passing in the default VM.
   compCplWRFSST = ESMF_CplCompCreate(vm, "WRF-SST Coupler", rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     PRINT *, 'wrf_SST_ESMF:  ESMF_CplCompCreate failed'
   ENDIF

   ! Create empty import and export states for WRF
   importStateWRF = ESMF_StateCreate("WRF Import State", statetype=ESMF_STATE_IMPORT, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     PRINT *, 'wrf_SST_ESMF:  ESMF_StateCreate(WRF Import State) failed'
   ENDIF
   exportStateWRF = ESMF_StateCreate("WRF Export State", statetype=ESMF_STATE_EXPORT, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     PRINT *, 'wrf_SST_ESMF:  ESMF_StateCreate(WRF Export State) failed'
   ENDIF

   ! Create empty import and export states for SST
   importStateSST = ESMF_StateCreate("SST Import State", statetype=ESMF_STATE_IMPORT, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     PRINT *, 'wrf_SST_ESMF:  ESMF_StateCreate(SST Import State) failed'
   ENDIF
   exportStateSST = ESMF_StateCreate("SST Export State", statetype=ESMF_STATE_EXPORT, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     PRINT *, 'wrf_SST_ESMF:  ESMF_StateCreate(SST Export State) failed'
   ENDIF

   ! Register the WRF Gridded Component
   CALL ESMF_GridCompSetServices(compGriddedWRF, WRF_register, rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     PRINT *, 'wrf_SST_ESMF:  ESMF_GridCompSetServices(compGriddedWRF) failed'
   ENDIF

   ! Register the SST Gridded Component
   CALL ESMF_GridCompSetServices(compGriddedSST, SST_register, rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     PRINT *, 'wrf_SST_ESMF:  ESMF_GridCompSetServices(compGriddedSST) failed'
   ENDIF

   ! Register the WRF-SST Coupler Component
   CALL ESMF_CplCompSetServices(compCplWRFSST, WRFSSTCpl_register, rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     PRINT *, 'wrf_SST_ESMF:  ESMF_CplCompSetServices failed'
   ENDIF

   ! Create top-level clock.  There is no way to create an "empty" clock, so 
   ! stuff in bogus values for start time, stop time, and time step and fix 
   ! them after gridded component "init" phases return.  
   CALL ESMF_TimeSet(startTime, yy=2000, mm=1, dd=1, &
                     h=0, m=0, s=0, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     PRINT *, 'wrf_SST_ESMF:  ESMF_TimeSet(startTime) failed'
   ENDIF
   CALL ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, &
                     h=12, m=0, s=0, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     PRINT *, 'wrf_SST_ESMF:  ESMF_TimeSet(stopTime) failed'
   ENDIF
   CALL ESMF_TimeIntervalSet(couplingInterval, s=2, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     PRINT *, 'wrf_SST_ESMF:  ESMF_TimeIntervalSet failed'
   ENDIF
   driverClock = ESMF_ClockCreate(timeStep=couplingInterval, &
                                  startTime=startTime,       &
                                  stopTime=stopTime, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     PRINT *, 'wrf_SST_ESMF:  ESMF_ClockCreate failed'
   ENDIF

   ! Init, Run, and Finalize section

   ! Init...
   ! initialize WRF, phase 1
   ! Phase 1 init returns WRF time and decomposition information as
   ! exportState metadata.
   PRINT *, 'DEBUG wrf_SST_ESMF:  calling phase-1 WRF init (wrf_component_init1)'
   CALL ESMF_GridCompInitialize(compGriddedWRF, importStateWRF, &
                                exportStateWRF, driverClock, phase=1, rc=rc)
   ! Note:  wrf_debug and wrf_error_fatal are now initialized
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_GridCompInitialize(WRF phase 1) failed' )
   ENDIF

   ! initialize SST, phase 1
   ! Phase 1 init returns SST time information as
   ! exportState metadata.
   PRINT *, 'DEBUG wrf_SST_ESMF:  calling phase-1 SST init (sst_component_init1)'
   CALL ESMF_GridCompInitialize(compGriddedSST, importStateSST, &
                                exportStateSST, driverClock, phase=1, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_GridCompInitialize(SST phase 1) failed' )
   ENDIF

   ! Reconcile clock settings from WRF and SST components to set up 
   ! top-level clock.  These are passed back from each "init" as attributes 
   ! on exportState*.  
   ! Stuff both States into a single State to pass into GetTimesFromStates() 
   ! which is smart enough to deal with a Composite.  
   PRINT *, 'DEBUG wrf_SST_ESMF:  reconciling clock from WRF and SST components'
   tmpState = ESMF_StateCreate( rc=rc )
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_StateCreate(tmpState) failed' )
   ENDIF
   CALL ESMF_StateAddState( tmpState, exportStateWRF, rc )
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_StateAddState(exportStateWRF) failed' )
   ENDIF
   CALL ESMF_StateAddState( tmpState, exportStateSST, rc )
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_StateAddState(exportStateSST) failed' )
   ENDIF
   CALL GetTimesFromStates( tmpState, startTime, stopTime, couplingInterval )
   CALL ESMF_TimeIntervalGet( couplingInterval, TimeString=couplingIntervalString, &
                              rc=rc )
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_TimeIntervalGet failed' )
   ENDIF
   CALL wrf_debug( 100, 'wrf_SST_ESMF:  couplingInterval = '//TRIM(couplingIntervalString) )
   CALL ESMF_StateDestroy( tmpState, rc )
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_StateDestroy(tmpState) failed' )
   ENDIF
   ! update driver clock
   CALL ESMF_ClockDestroy(driverClock, rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_ClockDestroy failed' )
   ENDIF
   driverClock = ESMF_ClockCreate(timeStep=couplingInterval, &
                                  startTime=startTime,       &
                                  stopTime=stopTime, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_ClockCreate(driverClock) failed' )
   ENDIF
   PRINT *, 'DEBUG wrf_SST_ESMF:  done reconciling clock from WRF and SST components'
   CALL wrf_clockprint(50, driverClock, &
          'DEBUG wrf_SST_ESMF:  driverClock after creation,')

   ! initialize WRF-SST Coupler
   PRINT *, 'DEBUG wrf_SST_ESMF:  calling phase-1 CPL init (WRFSSTCpl_init)'
   CALL ESMF_CplCompInitialize(compCplWRFSST, exportStateWRF, &
                               importStateSST, driverClock, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_CplCompInitialize(WRF -> SST) failed' )
   ENDIF
! TBH:  this bit is not needed
!   CALL ESMF_CplCompInitialize(compCplWRFSST, exportStateSST, &
!                               importStateWRF, driverClock, rc=rc)
!   IF ( rc /= ESMF_SUCCESS ) THEN
!     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_CplCompInitialize(SST -> WRF) failed' )
!   ENDIF

   ! initialize SST, phase 2
   WRITE(str,'(A)') 'PROGRAM wrf_SST_ESMF: Calling phase-2 init for SST (sst_component_init2)'
   CALL wrf_debug ( 100 , TRIM(str) )
   CALL ESMF_GridCompInitialize(compGriddedSST, importStateSST, &
                                exportStateSST, driverClock, phase=2, rc=rc)
   WRITE(str,'(A)') 'PROGRAM wrf_SST_ESMF: back from phase-2 init for SST'
   CALL wrf_debug ( 100 , TRIM(str) )
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_GridCompInitialize(SST phase 2) failed' )
   ENDIF

   ! initialize WRF, phase 2
   ! Phase 2 init sets up WRF importState and exportState.
   WRITE(str,'(A)') 'PROGRAM wrf_SST_ESMF: Calling phase-2 init for WRF (wrf_component_init2)'
   CALL wrf_debug ( 100 , TRIM(str) )
   CALL ESMF_GridCompInitialize(compGriddedWRF, importStateWRF, &
                                exportStateWRF, driverClock, phase=2, rc=rc)
   WRITE(str,'(A)') 'PROGRAM wrf_SST_ESMF: back from phase-2 init for WRF'
   CALL wrf_debug ( 100 , TRIM(str) )
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_GridCompInitialize(WRF phase 2) failed' )
   ENDIF

   CALL wrf_clockprint(50, driverClock, &
          'DEBUG wrf_SST_ESMF:  driverClock before main time-stepping loop,')
   ! Run...
   ! main time-stepping loop
   timestepdebug = 0
   DO WHILE ( .NOT. ESMF_ClockIsStopTime(driverClock, rc) )

     timestepdebug = timestepdebug + 1
     WRITE(str,'(A,I8)') 'PROGRAM wrf_SST_ESMF: Top of time-stepping loop, timestepdebug = ',timestepdebug
     CALL wrf_debug ( 100 , TRIM(str) )
     CALL wrf_clockprint(50, driverClock, &
            'DEBUG wrf_SST_ESMF:  driverClock at top of time-stepping loop,')

     ! Run SST phase 1
     WRITE(str,'(A)') 'PROGRAM wrf_SST_ESMF: Calling phase-1 run for SST (sst_component_run1)'
     CALL wrf_debug ( 100 , TRIM(str) )
     CALL ESMF_GridCompRun(compGriddedSST, importStateSST, exportStateSST, &
                           driverClock, phase=1, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_GridCompRun(SST phase 1) failed' )
     ENDIF
     WRITE(str,'(A)') 'PROGRAM wrf_SST_ESMF: back from phase-1 run for SST (sst_component_run1)'
     CALL wrf_debug ( 100 , TRIM(str) )

     ! couple SST export -> WRF import
     WRITE(str,'(A)') 'PROGRAM wrf_SST_ESMF: Calling run for CPL SST->WRF (WRFSSTCpl_run)'
     CALL wrf_debug ( 100 , TRIM(str) )
     CALL ESMF_CplCompRun(compCplWRFSST, exportStateSST, &
                          importStateWRF, driverClock, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_CplCompRun(SST -> WRF) failed' )
     ENDIF
     WRITE(str,'(A)') 'PROGRAM wrf_SST_ESMF: back from run for CPL SST->WRF (WRFSSTCpl_run)'
     CALL wrf_debug ( 100 , TRIM(str) )

     ! Run WRF
     WRITE(str,'(A)') 'PROGRAM wrf_SST_ESMF: Calling run for WRF (wrf_component_run)'
     CALL wrf_debug ( 100 , TRIM(str) )
     CALL ESMF_GridCompRun(compGriddedWRF, importStateWRF, exportStateWRF, &
                           driverClock, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_GridCompRun(WRF) failed' )
     ENDIF
     WRITE(str,'(A)') 'PROGRAM wrf_SST_ESMF: back from run for WRF (wrf_component_run)'
     CALL wrf_debug ( 100 , TRIM(str) )

     ! couple WRF export -> SST import
     WRITE(str,'(A)') 'PROGRAM wrf_SST_ESMF: Calling run for CPL WRF->SST (WRFSSTCpl_run)'
     CALL wrf_debug ( 100 , TRIM(str) )
     CALL ESMF_CplCompRun(compCplWRFSST, exportStateWRF, &
                          importStateSST, driverClock, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_CplCompRun(WRF -> SST) failed' )
     ENDIF
     WRITE(str,'(A)') 'PROGRAM wrf_SST_ESMF: back from run for CPL WRF->SST (WRFSSTCpl_run)'
     CALL wrf_debug ( 100 , TRIM(str) )

     ! Run SST phase 2
     WRITE(str,'(A)') 'PROGRAM wrf_SST_ESMF: Calling phase-2 run for SST (sst_component_run2)'
     CALL wrf_debug ( 100 , TRIM(str) )
     CALL ESMF_GridCompRun(compGriddedSST, importStateSST, exportStateSST, &
                           driverClock, phase=2, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_GridCompRun(SST phase 2) failed' )
     ENDIF
     WRITE(str,'(A)') 'PROGRAM wrf_SST_ESMF: back from phase-2 run for SST (sst_component_run2)'
     CALL wrf_debug ( 100 , TRIM(str) )

     ! advance clock to next coupling time step
     WRITE(str,'(A)') 'PROGRAM wrf_SST_ESMF: advancing clock'
     CALL wrf_debug ( 100 , TRIM(str) )
     CALL ESMF_ClockAdvance( driverClock, rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_ClockAdvance failed' )
     ENDIF
     WRITE(str,'(A)') 'PROGRAM wrf_SST_ESMF: done advancing clock'
     CALL wrf_debug ( 100 , TRIM(str) )

     CALL wrf_clockprint(50, driverClock, &
            'DEBUG wrf_SST_ESMF:  driverClock at end of time-stepping loop,')

   ENDDO

   WRITE(str,'(A)') 'PROGRAM wrf_SST_ESMF: done with time-stepping loop'
   CALL wrf_debug ( 100 , TRIM(str) )

   ! clean up SST
   CALL ESMF_GridCompFinalize(compGriddedSST, importStateSST, exportStateSST, &
                              driverClock, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_GridCompFinalize(compGriddedSST) failed' )
   ENDIF
 
   ! clean up compCplWRFSST
   CALL ESMF_CplCompFinalize( compCplWRFSST, exportStateWRF, importStateSST, &
                              driverClock, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_CplCompFinalize(compCplWRFSST) failed' )
   ENDIF
 
   ! clean up WRF
   ! must do this AFTER clean up of SST since SST uses WRF IOAPI
   CALL ESMF_GridCompFinalize(compGriddedWRF, importStateWRF, exportStateWRF, &
                              driverClock, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_GridCompFinalize(compGriddedWRF) failed' )
   ENDIF
 
   ! Clean up

   CALL ESMF_GridCompDestroy(compGriddedWRF, rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_GridCompDestroy(compGriddedWRF) failed' )
   ENDIF
   CALL ESMF_StateDestroy(importStateWRF, rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_StateDestroy(importStateWRF) failed' )
   ENDIF
   CALL ESMF_StateDestroy(exportStateWRF, rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_StateDestroy(exportStateWRF) failed' )
   ENDIF
   CALL ESMF_StateDestroy(importStateSST, rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_StateDestroy(importStateSST) failed' )
   ENDIF
   CALL ESMF_StateDestroy(exportStateSST, rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_StateDestroy(exportStateSST) failed' )
   ENDIF
   CALL ESMF_ClockDestroy(driverClock, rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_ClockDestroy(driverClock) failed' )
   ENDIF

   CALL ESMF_Finalize( rc=rc )
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal ( 'wrf_SST_ESMF:  ESMF_Finalize failed' )
   ENDIF

END PROGRAM wrf_SST_ESMF


