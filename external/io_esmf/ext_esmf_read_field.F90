!--- read_field
SUBROUTINE ext_esmf_read_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm, &
                            DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER       ,INTENT(IN)    :: DataHandle 
  CHARACTER*(*) ,intent(inout) :: DateStr
  CHARACTER*(*) ,intent(inout) :: VarName
  integer       ,intent(inout) :: FieldType
  integer       ,intent(inout) :: Comm
  integer       ,intent(inout) :: IOComm
  integer       ,intent(inout) :: DomainDesc
  character*(*) ,intent(inout) :: MemoryOrder
  character*(*) ,intent(inout) :: Stagger
  character*(*) ,intent(inout) :: DimNames(*)
  integer       ,intent(inout) :: DomainStart(*), DomainEnd(*)
  integer       ,intent(inout) :: MemoryStart(*), MemoryEnd(*)
  integer       ,intent(inout) :: PatchStart(*),  PatchEnd(*)
  REAL          ,INTENT(INOUT) :: Field(*)
  integer       ,intent(out)   :: Status
  ! Local declarations
  integer ims,ime,jms,jme,kms,kme

  ims = MemoryStart(1) ; ime = MemoryEnd(1)
  jms = MemoryStart(2) ; jme = MemoryEnd(2)
  kms = MemoryStart(3) ; kme = MemoryEnd(3)

!$$$here...  This layer used to be needed a few years ago.  Is it still needed?  Test!  
  CALL ext_esmf_read_fieldi ( DataHandle, DateStr, VarName, Field, FieldType, Comm, IOComm, &
                              DomainDesc , MemoryOrder , Stagger , DimNames ,               &
                              DomainStart , DomainEnd ,                                     &
                              ims, ime, jms, jme, kms, kme,                                 &
                              PatchStart , PatchEnd ,                                       &
                              Status )

END SUBROUTINE ext_esmf_read_field



SUBROUTINE ext_esmf_read_fieldi ( DataHandle, DateStr, VarName, Field, FieldType, Comm, IOComm, &
                                  DomainDesc , MemoryOrder , Stagger , DimNames ,               &
                                  DomainStart , DomainEnd ,                                     &
                                  ims, ime, jms, jme, kms, kme,                                 &
                                  PatchStart , PatchEnd ,                                       &
                                  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER       ,INTENT(IN)    :: DataHandle 
  CHARACTER*(*) ,intent(inout) :: DateStr
  CHARACTER*(*) ,intent(inout) :: VarName
  integer       ,intent(inout) :: FieldType
  integer       ,intent(inout) :: Comm
  integer       ,intent(inout) :: IOComm
  integer       ,intent(inout) :: DomainDesc
  character*(*) ,intent(inout) :: MemoryOrder
  character*(*) ,intent(inout) :: Stagger
  character*(*) ,intent(inout) :: DimNames(*)
  integer       ,intent(inout) :: DomainStart(*), DomainEnd(*)
  integer       ,intent(inout) :: MemoryStart(*), MemoryEnd(*)
  integer       ,intent(inout) :: PatchStart(*),  PatchEnd(*)
  REAL          ,INTENT(INOUT) :: Field( ims:ime, jms:jme, kms:kme )
  integer       ,intent(out)   :: Status

!local
!$$$here...  clean up
  INTEGER                        :: locDataHandle
  CHARACTER*132                  :: locDateStr
  CHARACTER*132                  :: locVarName
  integer                        :: locFieldType
  integer                        :: locComm
  integer                        :: locIOComm
  integer                        :: locDomainDesc
  character*132                  :: locMemoryOrder
  character*132                  :: locStagger
  character*132 , dimension (3)  :: locDimNames
  integer ,dimension(3)          :: locDomainStart, locDomainEnd
  integer ,dimension(3)          :: locMemoryStart, locMemoryEnd
  integer ,dimension(3)          :: locPatchStart,  locPatchEnd
  real, allocatable, dimension(:,:) :: temp
  doubleprecision, allocatable, dimension(:,:) :: dtemp
  integer gSize(2)
  INTEGER, EXTERNAL :: cast_to_int
integer myproc

  character*256 mess
  integer ips,ipe,jps,jpe
  integer idex,ierr,i,j

  integer ii,jj,kk,myrank,ierr, mcel_type
  real*8 data_time
  character*14 timestr

  INTEGER inttypesize, realtypesize, istat, code

  IF ( .NOT. int_valid_handle( DataHandle ) ) THEN
    CALL wrf_error_fatal("io_esmf.F90: ext_esmf_read_fieldi: invalid data handle" )
  ENDIF
  IF ( .NOT. int_handle_in_use( DataHandle ) ) THEN
    CALL wrf_error_fatal("io_esmf.F90: ext_esmf_read_fieldi: DataHandle not opened" )
  ENDIF

  ips = PatchStart(1) ; ipe = PatchEnd(1) 
  jps = PatchStart(2) ; jpe = PatchEnd(2) 

write(mess,*)'ext_esmf_read_fieldi ',DataHandle, TRIM(DateStr), TRIM(VarName)
call wrf_debug( 300, mess )

  inttypesize = itypesize
  realtypesize = rtypesize
  IF      ( FieldType .EQ. WRF_REAL ) THEN
    typesize = rtypesize
    mcel_type = ESMF_KIND_R4
  ELSE IF ( FieldType .EQ. WRF_DOUBLE ) THEN
    mcel_type = ESMF_KIND_R8
!$$$here...  what to do about typesize?  
  ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
    typesize = itypesize
    mcel_type = ESMF_KIND_I4
  ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
    CALL wrf_error_fatal( 'ext_esmf_write_field, WRF_LOGICAL not yet supported')
  ENDIF

  ! case 1: the file is opened but not commited for read
write(mess,*) ' read_field: okay_to_read: ', DataHandle, okay_to_read(DataHandle)
call wrf_debug( 300, mess )

!$$$here...  need to check for opened_for_read( DataHandle ) ?  

!$$$
!$$$  What's the best way to get all of this @#$% in and out via top-of-model 
!$$$  interfaces?  Specifically, how will ESMF know what importState to pass 
!$$$  in given its ignorance of the WRF Registry?  Looks like we'll need to 
!$$$  have the read "training" bits occur in a phase prior to the first phase 
!$$$  that has coupling.  Then we can "export" an importState.  exportState 
!$$$  should not be an issue since WRF is responsible for its content.  Of 
!$$$  course, if this can be done for importState, then it can be done for 
!$$$  exportState too, if that is convenient.  
!$$$
!$$$  NOTE:  Point to the current importState.  
!$$$         Then copy-in matching VarName's during each non-training call.  
!$$$         DUE TO CICO, it just does not make sense to hand ESMF 
!$$$         a FORTRAN POINTER to Field.  
!$$$         How to handle a missing VarName?  
!$$$

  if ( .not. okay_to_read( DataHandle ) )  then

!$$$here...
write(mess,*)'ext_esmf_read_fieldi tr addSources ', TRIM(VarName), mcel_type
call wrf_debug( 300, mess )
        CALL addSources ( open_file_descriptors(1,DataHandle), MCEL_SERVER,  &
  &       TRIM(VarName),1, mcel_type, ierr )
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_esmf_write_field: addSources")
write(mess,*)'ext_esmf_read_fieldi tr addOutputs ', TRIM(VarName), mcel_type
call wrf_debug( 300, mess )
        CALL addOutputs ( open_file_descriptors(1,DataHandle),   &
  &       TRIM(VarName),1, mcel_type, ierr )
! add this field to the list that we know something about
        ListOfFields(DataHandle) = TRIM(ListOfFields(DataHandle)) // ',' // TRIM(VarName)
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_esmf_write_field: addOutputs")
!$$$here...  ???
      ! sieve the fields coming in and grab the ones we need for geo registration
      !IF ( TRIM(VarName) .EQ. TRIM(LANDMASK_I(DataHandle)) ) THEN
      !  IF ( ALLOCATED(mask) ) THEN
      !    DEALLOCATE(mask)
      !  ENDIF
      !  ALLOCATE(mask(ips:ipe,jps:jpe))
      !  IF ( FieldType .EQ. WRF_INTEGER ) THEN
      !    CALL copy_field_to_cache_int ( Field, mask, ips, ipe, jps, jpe, ims, ime, jms, jme )
      !  ELSE IF ( FieldType .EQ. WRF_REAL ) THEN
      !    ALLOCATE(rmask(ips:ipe,jps:jpe))
      !    CALL copy_field_to_cache_r2r ( Field, rmask, ips, ipe, jps, jpe, ims, ime, jms, jme )
      !    mask = NINT( rmask )
      !    DEALLOCATE(rmask)
      !  ELSE IF (FieldType .EQ. WRF_DOUBLE ) THEN
      !    ALLOCATE(dmask(ips:ipe,jps:jpe))
      !    CALL copy_field_to_cache_d2d ( Field, dmask, ips, ipe, jps, jpe, ims, ime, jms, jme )
      !    mask = NINT( dmask )
      !    DEALLOCATE(dmask)
      !  ENDIF
      !ENDIF

  ! case 2: opened for read and committed
  else if ( okay_to_read( DataHandle ) )  then

write(mess,*)'ext_esmf_read_fieldi ok ', Trim(VarName)
call wrf_debug( 300, mess )
write(mess,*)'ext_esmf_read_fieldi LAT_R ', Trim(LAT_R(DataHandle))
call wrf_debug( 300, mess )
write(mess,*)'ext_esmf_read_fieldi LON_R ', Trim(LON_R(DataHandle))
call wrf_debug( 300, mess )
write(mess,*)'ext_esmf_read_fieldi LANDMASK_I ', Trim(LANDMASK_I(DataHandle))
call wrf_debug( 300, mess )
    IF ( TRIM(VarName) .NE. TRIM(LAT_R(DataHandle)) .AND. TRIM(VarName) .NE. TRIM(LON_R(DataHandle)) .AND. &
         TRIM(VarName) .NE. TRIM(LANDMASK_I(DataHandle)) ) THEN
      IF ( .NOT. mcel_finalized( DataHandle ) ) THEN
        IF ( ALLOCATED( xlat ) .AND. ALLOCATED( xlong ) ) THEN
write(mess,*)'ext_esmf_read_fieldi ok setlocationsXY ', Trim(VarName)
call wrf_debug( 300, mess )
          CALL setLocationsXY( open_file_descriptors(2,DataHandle), xlong, xlat, ierr )
          IF ( ierr .NE. 0 ) CALL wrf_error_fatal( "ext_esmf_open_read_field: setLocationsXY" )
        ELSE
          CALL wrf_error_fatal( "ext_esmf_read_fieldi:noLocationsXY or dx/dy")
        ENDIF
        IF ( ALLOCATED(mask) ) THEN
          CALL setMask ( open_file_descriptors(2,DataHandle) , mask, ierr )
          IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_esmf_read_fieldi: setMask")
        ENDIF
write(mess,*)'ext_esmf_read_fieldi ok setoutputgrid ', Trim(VarName)
call wrf_debug( 300, mess )
        CALL setoutputgrid ( open_file_descriptors(1,DataHandle), open_file_descriptors(2,DataHandle), ierr )
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_esmf_read_fieldi: setoutputgrid")
write(mess,*)'ext_esmf_read_fieldi ok finalizefilters ', Trim(VarName)
call wrf_debug( 300, mess )
        CALL finalizefilters ( open_file_descriptors(1,DataHandle), ierr )
        IF ( ierr .GT. 0 .and. ierr .ne. 3 ) THEN
           write(mess,*)'ext_esmf_open_for_read_field: finalizefilters ierr=',ierr
           CALL wrf_error_fatal( TRIM(mess) )
        ENDIF
        mcel_finalized( DataHandle ) = .TRUE.
      ENDIF

      ! a little string munging, assumes that we're getting an ISO compliant date string
      ! basically removing the delimeters

      timestr = "              "
      timestr(1:4)   = DateStr(1:4)    ! YYYY
      timestr(5:6)   = DateStr(6:7)    ! MM
      timestr(7:8)   = DateStr(9:10)   ! DD
      timestr(9:10)  = DateStr(12:13)  ! HH
      timestr(11:12) = DateStr(15:16)  ! MM
      timestr(13:14) = DateStr(18:19)  ! SS

      CALL YYYYMMDDHHMMSS2SECS( timestr, data_time )

write(mess,*)'TRIM( VarName ) ',TRIM( VarName )
call wrf_debug( 300, mess )
write(mess,*)'TRIM( ListOfFields(DataHandle) ) ',TRIM( ListOfFields(DataHandle) )
call wrf_debug( 300, mess )
write(mess,*)'INDEX( TRIM( ListOfFields(DataHandle) ), TRIM( VarName ) )', INDEX( TRIM( ListOfFields(DataHandle) ), TRIM( VarName ) )
call wrf_debug( 300, mess )

      IF ( INDEX( TRIM( ListOfFields(DataHandle) ), TRIM( VarName ) ) .EQ. 0 ) THEN
        write(mess,*)'ext_esmf_open_for_read_field: ',TRIM( VarName ),' is not a field set up for DataHandle ', DataHandle
        CALL wrf_error_fatal( TRIM(mess) )
      ENDIF

      IF ( FieldType .EQ. WRF_REAL ) THEN
        ALLOCATE(temp(ips:ipe,jps:jpe))
write(mess,*)'ext_esmf_read_fieldi opened_for_update(DataHandle) ',opened_for_update(DataHandle)
call wrf_debug( 300, mess )
        IF ( opened_for_update(DataHandle) ) THEN
          CALL copy_field_to_cache_r2r ( Field, temp, ips, ipe, jps, jpe, ims, ime, jms, jme )
          call getData(open_file_descriptors(1,DataHandle),TRIM(VarName),temp,                 &
            data_time,data_time,MCEL_TIMECENT_POINT,usemask(DataHandle),                       &
            MCEL_FETCHPOLICY_KEEPBLOCK,ierr)
        ELSE
! the difference is there is no KEEP in the FETCHPOLICY
write(mess,*)'ext_esmf_read_fieldi ok getData ', Trim(VarName)
call wrf_debug( 300, mess )
          call getData(open_file_descriptors(1,DataHandle),TRIM(VarName),temp,                 &
            data_time,data_time,MCEL_TIMECENT_POINT,usemask(DataHandle),                      &
            MCEL_FETCHPOLICY_BLOCK,ierr)
write(mess,*)'ext_esmf_read_fieldi ok getData returns ',ierr, Trim(VarName)
call wrf_debug( 300, mess )
        ENDIF
        CALL copy_cache_to_field_r2r ( temp, Field, ips, ipe, jps, jpe, ims, ime, jms, jme )
        DEALLOCATE(temp)
      ELSE IF ( FieldType .EQ. WRF_DOUBLE ) THEN

        ALLOCATE(dtemp(ips:ipe,jps:jpe))
write(mess,*)'ext_esmf_read_fieldi opened_for_update(DataHandle) ',opened_for_update(DataHandle)
call wrf_debug( 300, mess )
        IF ( opened_for_update(DataHandle) ) THEN
          CALL copy_field_to_cache_d2d ( Field, dtemp, ips, ipe, jps, jpe, ims, ime, jms, jme )
write(mess,*)'ext_esmf_read_fieldi ok getData returns ',ierr, Trim(VarName)
call wrf_debug( 300, mess )
          call getData(open_file_descriptors(1,DataHandle),TRIM(VarName),dtemp,                 &
            data_time,data_time,MCEL_TIMECENT_POINT,usemask(DataHandle),                       &
            MCEL_FETCHPOLICY_KEEPBLOCK,ierr)
        ELSE
! the difference is there is no KEEP in the FETCHPOLICY
write(mess,*)'ext_esmf_read_fieldi ok getData ', Trim(VarName)
call wrf_debug( 300, mess )
          call getData(open_file_descriptors(1,DataHandle),TRIM(VarName),dtemp,                 &
            data_time,data_time,MCEL_TIMECENT_POINT,usemask(DataHandle),                      &
            MCEL_FETCHPOLICY_BLOCK,ierr)
write(mess,*)'ext_esmf_read_fieldi ok getData returns ',ierr, Trim(VarName)
call wrf_debug( 300, mess )
        ENDIF
        CALL copy_cache_to_field_d2d ( dtemp, Field, ips, ipe, jps, jpe, ims, ime, jms, jme )

        DEALLOCATE(dtemp)

      ENDIF
    ENDIF
  endif

  Status = 0

  RETURN

END SUBROUTINE ext_esmf_read_fieldi
