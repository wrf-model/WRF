!--- read_field
SUBROUTINE ext_mcel_read_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm, &
                            DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  integer                       ,intent(inout)    :: FieldType
  integer                       ,intent(inout) :: Comm
  integer                       ,intent(inout) :: IOComm
  integer                       ,intent(inout)    :: DomainDesc
  character*(*)                 ,intent(inout)    :: MemoryOrder
  character*(*)                 ,intent(inout)    :: Stagger
  character*(*) , dimension (*) ,intent(inout)    :: DimNames
  integer ,dimension(*)         ,intent(inout)    :: DomainStart, DomainEnd
  integer ,dimension(*)         ,intent(inout)    :: MemoryStart, MemoryEnd
  integer ,dimension(*)         ,intent(inout)    :: PatchStart,  PatchEnd
  integer                       ,intent(out)   :: Status

!local
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

  character*132 mess
  integer ips,ipe,jps,jpe
  integer ims,ime,jms,jme
  integer idex,ierr,i,j

  integer ii,jj,kk,myrank,ierr, mcel_type
  real*8 data_time
  character*14 timestr
  


!  REAL, DIMENSION( MemoryStart(1):MemoryEnd(1), &
!                   MemoryStart(2):MemoryEnd(2), &
!                   MemoryStart(3):MemoryEnd(3) ) :: Field
  REAL, DIMENSION(*)    :: Field

  INTEGER inttypesize, realtypesize, istat, code

  IF ( .NOT. int_valid_handle( DataHandle ) ) THEN
    CALL wrf_error_fatal("external/io_quilt/io_int.F90: ext_mcel_read_field: invalid data handle" )
  ENDIF
  IF ( .NOT. int_handle_in_use( DataHandle ) ) THEN
    CALL wrf_error_fatal("external/io_quilt/io_int.F90: ext_mcel_read_field: DataHandle not opened" )
  ENDIF


  ips = PatchStart(1) ; ipe = PatchEnd(1) 
  jps = PatchStart(2) ; jpe = PatchEnd(2) 
  ims = MemoryStart(1) ; ime = MemoryEnd(1)
  jms = MemoryStart(2) ; jme = MemoryEnd(2)

write(0,*)'ext_mcel_read_field ',DataHandle, TRIM(DateStr), TRIM(VarName)

  inttypesize = itypesize
  realtypesize = rtypesize
  IF      ( FieldType .EQ. WRF_REAL ) THEN
    typesize = rtypesize
    mcel_type = MCEL_DATATYPE_REAL
  ELSE IF ( FieldType .EQ. WRF_DOUBLE ) THEN
    mcel_type = MCEL_DATATYPE_DOUBLE
  ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
    typesize = itypesize
    mcel_type = MCEL_DATATYPE_INT32
  ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
    CALL wrf_error_fatal( 'io_int.F90: ext_mcel_write_field, WRF_LOGICAL not yet supported')
  ENDIF

  ! case 1: the file is opened but not commited for update 
write(0,*)' read_field: okay_to_read: ', DataHandle, okay_to_read(DataHandle)
write(0,*)' read_field: opened_for_update: ', DataHandle, opened_for_update(DataHandle)
  if ( .not. okay_to_read( DataHandle ) )  then
    IF ( opened_for_update( DataHandle) ) THEN
write(0,*)'ext_mcel_read_field tr calling ext_mcel_write_field ', TRIM(DateStr), TRIM(VarName)
      CALL ext_mcel_write_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm, &
                                  DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                  DomainStart , DomainEnd ,                                    &
                                  MemoryStart , MemoryEnd ,                                    &
                                  PatchStart , PatchEnd ,                                      &
                                  ierr )
      IF ( TRIM(VarName) .NE. TRIM(LAT_R(DataHandle)) .AND. TRIM(VarName) .NE. TRIM(LON_R(DataHandle)) .AND. &
           TRIM(VarName) .NE. TRIM(LANDMASK_I(DataHandle)) ) THEN
        ListOfFields(DataHandle) = TRIM(ListOfFields(DataHandle)) // ',' // TRIM(VarName)
      ENDIF
write(0,*)'ext_mcel_read_field tr back from ext_mcel_write_field ', TRIM(DateStr), TRIM(VarName), ierr
    ELSE

! these will have been set in the call to open_for_read_begin from sysdepinfo
      IF ( mcel_npglobal .NE. -1 .AND. mcel_mystart .NE. -1 .AND.  &
           mcel_mnproc   .NE. -1 .AND. mcel_myproc  .NE. -1     ) THEN
write(0,*)'ext_mcel_read_field tr setglobalsize ', TRIM(VarName), mcel_npglobal
        call setglobalsize(open_file_descriptors(2,DataHandle),mcel_npglobal,ierr)
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_read_field: setglobalsize")
write(0,*)'ext_mcel_read_field tr setglobalstart ', TRIM(VarName), mcel_mystart
        call setglobalstart(open_file_descriptors(2,DataHandle),mcel_mystart,ierr)
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_read_field: setglobalstart")
#if 0
        call setprocinfo(open_file_descriptors(1,DataHandle),mcel_mnproc,mcel_myproc,ierr)
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_read_field: setprocinfo")
#endif
      ENDIF
      mcel_npglobal=-1 ; mcel_mystart=-1 ; mcel_mnproc=-1 ; mcel_myproc=-1

      ! sieve the fields coming in and grab the ones we need for geo registration
      IF      ( TRIM(VarName) .EQ. TRIM(LAT_R(DataHandle)) ) THEN
        IF ( ALLOCATED(xlat) ) THEN
          DEALLOCATE(xlat)
        ENDIF
        ALLOCATE(xlat(ips:ipe,jps:jpe))
        IF      ( FieldType .EQ. WRF_REAL ) THEN
          CALL copy_field_to_cache_r2d ( Field, xlat, ips, ipe, jps, jpe, ims, ime, jms, jme )
        ELSE IF ( FieldType .EQ. WRF_DOUBLE ) THEN
          CALL copy_field_to_cache_d2d ( Field, xlat, ips, ipe, jps, jpe, ims, ime, jms, jme )
        ENDIF

      ELSE IF ( TRIM(VarName) .EQ. TRIM(LON_R(DataHandle)) ) THEN
        IF ( ALLOCATED(xlong) ) THEN
          DEALLOCATE(xlong)
        ENDIF
        ALLOCATE(xlong(ips:ipe,jps:jpe))
        IF      ( FieldType .EQ. WRF_REAL ) THEN
          CALL copy_field_to_cache_r2d ( Field, xlong, ips, ipe, jps, jpe, ims, ime, jms, jme )
        ELSE IF ( FieldType .EQ. WRF_DOUBLE ) THEN
          CALL copy_field_to_cache_d2d ( Field, xlong, ips, ipe, jps, jpe, ims, ime, jms, jme )
        ENDIF
      ELSE IF ( TRIM(VarName) .EQ. TRIM(LANDMASK_I(DataHandle)) ) THEN
        IF ( ALLOCATED(mask) ) THEN
          DEALLOCATE(mask)
        ENDIF
        ALLOCATE(mask(ips:ipe,jps:jpe))
        IF ( FieldType .EQ. WRF_INTEGER ) THEN
          CALL copy_field_to_cache_int ( Field, mask, ips, ipe, jps, jpe, ims, ime, jms, jme )
        ELSE IF ( FieldType .EQ. WRF_REAL ) THEN
          ALLOCATE(rmask(ips:ipe,jps:jpe))
          CALL copy_field_to_cache_r2r ( Field, rmask, ips, ipe, jps, jpe, ims, ime, jms, jme )
          mask = NINT( rmask )
          DEALLOCATE(rmask)
        ELSE IF (FieldType .EQ. WRF_DOUBLE ) THEN
          ALLOCATE(dmask(ips:ipe,jps:jpe))
          CALL copy_field_to_cache_d2d ( Field, dmask, ips, ipe, jps, jpe, ims, ime, jms, jme )
          mask = NINT( dmask )
          DEALLOCATE(dmask)
        ENDIF
      ELSE
        IF ( .NOT. mcel_grid_defined( DataHandle ) ) THEN
          mcel_grid_defined( DataHandle ) = .true.
          gSize(1) = ipe-ips+1
          gSize(2) = jpe-jps+1
write(0,*)'ext_mcel_read_field tr setSize ', TRIM(VarName), gSize
          CALL setSize ( open_file_descriptors(2,DataHandle), gSize, ierr )
          IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: setSize")
        ENDIF
write(0,*)'ext_mcel_read_field tr addSources ', TRIM(VarName), mcel_type
        CALL addSources ( open_file_descriptors(1,DataHandle), MCEL_SERVER,  &
  &       TRIM(VarName),1, mcel_type, ierr )
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: addSources")
write(0,*)'ext_mcel_read_field tr addOutputs ', TRIM(VarName), mcel_type
        CALL addOutputs ( open_file_descriptors(1,DataHandle),   &
  &       TRIM(VarName),1, mcel_type, ierr )
! add this field to the list that we know something about
        ListOfFields(DataHandle) = TRIM(ListOfFields(DataHandle)) // ',' // TRIM(VarName)
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: addOutputs")
      ENDIF
    ENDIF

  ! case 2: opened for update and committed
!  else if ( okay_to_write( DataHandle ) .and. opened_for_update( DataHandle) )  then
  else if ( okay_to_read( DataHandle ) )  then

write(0,*)'ext_mcel_read_field ok ', Trim(VarName)
write(0,*)'ext_mcel_read_field LAT_R ', Trim(LAT_R(DataHandle))
write(0,*)'ext_mcel_read_field LON_R ', Trim(LON_R(DataHandle))
write(0,*)'ext_mcel_read_field LANDMASK_I ', Trim(LANDMASK_I(DataHandle))
    IF ( TRIM(VarName) .NE. TRIM(LAT_R(DataHandle)) .AND. TRIM(VarName) .NE. TRIM(LON_R(DataHandle)) .AND. &
         TRIM(VarName) .NE. TRIM(LANDMASK_I(DataHandle)) ) THEN
      IF ( .NOT. mcel_finalized( DataHandle ) ) THEN
        IF ( ALLOCATED( xlat ) .AND. ALLOCATED( xlong ) ) THEN
write(0,*)'ext_mcel_read_field ok setlocationsXY ', Trim(VarName)

!call wrf_get_myproc(myproc)
!write(90+myproc,*)ipe-ips+1,jpe-jps+1,' xlong in read_field before setMask'
!do j=jps,jpe
!do i=ips,ipe
!write(90+myproc,*)xlong(i,j)
!enddo
!enddo
!write(90+myproc,*)ipe-ips+1,jpe-jps+1,' xlat in read_field before setMask'
!do j=jps,jpe
!do i=ips,ipe
!write(90+myproc,*)xlat(i,j)
!enddo
!enddo

          CALL setLocationsXY( open_file_descriptors(2,DataHandle), xlong, xlat, ierr )
          IF ( ierr .NE. 0 ) CALL wrf_error_fatal( "ext_mcel_open_read_field: setLocationsXY" )
        ELSE IF ( deltax .gt. 0. .and. deltay .gt. 0. .and. originx .gt. 0. .and. originy .gt. 0. ) THEN
          dxm(1) = deltax
          dxm(2) = deltay
          call SetDX ( open_file_descriptors(2,DataHandle), dxm, ierr)
          origin(1) = originx
          origin(2) = originy
          call SetOrigin ( open_file_descriptors(2,DataHandle), origin, ierr)
        ELSE
          CALL wrf_error_fatal( "ext_mcel_read_field:noLocationsXY or dx/dy")
        ENDIF
        IF ( ALLOCATED(mask) ) THEN

!write(0,*)'ext_mcel_read_field ok setMask ', Trim(VarName)
!call wrf_get_myproc(myproc)
!write(90+myproc,*)ipe-ips+1,jpe-jps+1,' mask in read_field before setMask'
!do j=jps,jpe
!do i=ips,ipe
!write(90+myproc,*)mask(i,j)
!enddo
!enddo

          CALL setMask ( open_file_descriptors(2,DataHandle) , mask, ierr )
          IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_read_field: setMask")
        ENDIF
write(0,*)'ext_mcel_read_field ok setoutputgrid ', Trim(VarName)
        CALL setoutputgrid ( open_file_descriptors(1,DataHandle), open_file_descriptors(2,DataHandle), ierr )
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_read_field: setoutputgrid")
write(0,*)'ext_mcel_read_field ok finalizefilters ', Trim(VarName)
        CALL finalizefilters ( open_file_descriptors(1,DataHandle), ierr )
        IF ( ierr .GT. 0 .and. ierr .ne. 3 ) THEN
           write(mess,*)'ext_mcel_open_for_read_field: finalizefilters ierr=',ierr
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

write(0,*)'TRIM( VarName ) ',TRIM( VarName )
write(0,*)'TRIM( ListOfFields(DataHandle) ) ',TRIM( ListOfFields(DataHandle) )
write(0,*)'INDEX( TRIM( ListOfFields(DataHandle) ), TRIM( VarName ) )', INDEX( TRIM( ListOfFields(DataHandle) ), TRIM( VarName ) )

      IF ( INDEX( TRIM( ListOfFields(DataHandle) ), TRIM( VarName ) ) .EQ. 0 ) THEN
        write(mess,*)'ext_mcel_open_for_read_field: ',TRIM( VarName ),' is not a field set up for DataHandle ', DataHandle
        CALL wrf_error_fatal( TRIM(mess) )
      ENDIF

      IF ( FieldType .EQ. WRF_REAL ) THEN
        ALLOCATE(temp(ips:ipe,jps:jpe))
write(0,*)'ext_mcel_read_field opened_for_update(DataHandle) ',opened_for_update(DataHandle)
        IF ( opened_for_update(DataHandle) ) THEN
          CALL copy_field_to_cache_r2r ( Field, temp, ips, ipe, jps, jpe, ims, ime, jms, jme )
!call wrf_get_myproc(myproc)
!write(90+myproc,*)ipe-ips+1,jpe-jps+1,' temp in read_field before getData'
!do j=jps,jpe
!do i=ips,ipe
!write(90+myproc,*)temp(i,j)
!enddo
!enddo
          call getData(open_file_descriptors(1,DataHandle),TRIM(VarName),temp,                 &
            data_time,data_time,MCEL_TIMECENT_POINT,usemask(DataHandle),                       &
            MCEL_FETCHPOLICY_KEEPBLOCK,ierr)
!write(90+myproc,*)ipe-ips+1,jpe-jps+1,' temp in read_field after getData'
!do j=jps,jpe
!do i=ips,ipe
!write(90+myproc,*)temp(i,j)
!enddo
!enddo
!write(0,*)'ext_mcel_read_field ok getData returns ',ierr, Trim(VarName)

        ELSE
! the difference is there is no KEEP in the FETCHPOLICY
write(0,*)'ext_mcel_read_field ok getData ', Trim(VarName)
          call getData(open_file_descriptors(1,DataHandle),TRIM(VarName),temp,                 &
            data_time,data_time,MCEL_TIMECENT_POINT,usemask(DataHandle),                      &
            MCEL_FETCHPOLICY_BLOCK,ierr)
write(0,*)'ext_mcel_read_field ok getData returns ',ierr, Trim(VarName)
        ENDIF
        CALL copy_cache_to_field_r2r ( temp, Field, ips, ipe, jps, jpe, ims, ime, jms, jme )
        DEALLOCATE(temp)
      ELSE IF ( FieldType .EQ. WRF_DOUBLE ) THEN

        ALLOCATE(dtemp(ips:ipe,jps:jpe))
write(0,*)'ext_mcel_read_field opened_for_update(DataHandle) ',opened_for_update(DataHandle)
        IF ( opened_for_update(DataHandle) ) THEN
          CALL copy_field_to_cache_d2d ( Field, dtemp, ips, ipe, jps, jpe, ims, ime, jms, jme )
write(0,*)'ext_mcel_read_field ok getData returns ',ierr, Trim(VarName)
          call getData(open_file_descriptors(1,DataHandle),TRIM(VarName),dtemp,                 &
            data_time,data_time,MCEL_TIMECENT_POINT,usemask(DataHandle),                       &
            MCEL_FETCHPOLICY_KEEPBLOCK,ierr)
        ELSE
! the difference is there is no KEEP in the FETCHPOLICY
write(0,*)'ext_mcel_read_field ok getData ', Trim(VarName)
          call getData(open_file_descriptors(1,DataHandle),TRIM(VarName),dtemp,                 &
            data_time,data_time,MCEL_TIMECENT_POINT,usemask(DataHandle),                      &
            MCEL_FETCHPOLICY_BLOCK,ierr)
write(0,*)'ext_mcel_read_field ok getData returns ',ierr, Trim(VarName)
        ENDIF
        CALL copy_cache_to_field_d2d ( dtemp, Field, ips, ipe, jps, jpe, ims, ime, jms, jme )

        DEALLOCATE(dtemp)

      ENDIF
    ENDIF
  endif

  Status = 0

  RETURN

END SUBROUTINE ext_mcel_read_field
