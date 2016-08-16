!--- write_field
SUBROUTINE ext_mcel_write_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm,  &
                             DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd ,                                      &
                             Status )
  USE module_ext_mcel
!  USE module_date_time   ! defined in share
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  integer                       ,intent(in)    :: FieldType
  integer                       ,intent(inout) :: Comm
  integer                       ,intent(inout) :: IOComm
  integer                       ,intent(in)    :: DomainDesc
  character*(*)                 ,intent(in)    :: MemoryOrder
  character*(*)                 ,intent(in)    :: Stagger
  character*(*) , dimension (*) ,intent(in)    :: DimNames
  integer ,dimension(*)         ,intent(in)    :: DomainStart, DomainEnd
  integer ,dimension(*)         ,intent(in)    :: MemoryStart, MemoryEnd
  integer ,dimension(*)         ,intent(in)    :: PatchStart,  PatchEnd
  integer                       ,intent(out)   :: Status

  integer ips,ipe,jps,jpe
  integer ims,ime,jms,jme
  integer idex,ierr,i,j

  integer ii,jj,kk,myrank,mcel_type
  integer gSize(2)
  integer idts
  real*8 data_time
  CHARACTER*256 RollOverDeathDate
  CHARACTER*80 mess, timestr
  INTEGER, EXTERNAL :: cast_to_int

!  REAL, DIMENSION( MemoryStart(1):MemoryEnd(1), &
!                   MemoryStart(2):MemoryEnd(2), &
!                   MemoryStart(3):MemoryEnd(3) ) :: Field

  REAL, DIMENSION(*)    :: Field

  real, allocatable, dimension(:,:) :: temp
  integer, allocatable, dimension(:,:) :: itemp
  doubleprecision, allocatable, dimension(:,:) :: dtemp

  INTEGER inttypesize, realtypesize

write(0,*)"write field : called "
  IF ( .NOT. int_valid_handle( DataHandle ) ) THEN
    CALL wrf_error_fatal("ext_mcel_write_field: invalid data handle" )
  ENDIF
  IF ( .NOT. int_handle_in_use( DataHandle ) ) THEN
    CALL wrf_error_fatal("ext_mcel_write_field: DataHandle not opened" )
  ENDIF

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

  ips = PatchStart(1) ; ipe = PatchEnd(1) 
  jps = PatchStart(2) ; jpe = PatchEnd(2) 
  ims = MemoryStart(1) ; ime = MemoryEnd(1)
  jms = MemoryStart(2) ; jme = MemoryEnd(2)

write(0,*)"write field : okay_to_write ",okay_to_write( DataHandle )

  IF ( okay_to_write( DataHandle ) ) THEN
    IF ( TRIM(VarName) .NE. TRIM(LAT_R(DataHandle)) .AND. TRIM(VarName) .NE. TRIM(LON_R(DataHandle)) .AND. &
         TRIM(VarName) .NE. TRIM(LANDMASK_I(DataHandle)) ) THEN
      IF ( .NOT. mcel_finalized( DataHandle ) ) THEN
        IF ( ALLOCATED( xlat ) .AND. ALLOCATED( xlong ) ) THEN
          CALL setLocationsXY( open_file_descriptors(2,DataHandle), xlong, xlat, ierr )
          IF ( ierr .NE. 0 ) CALL wrf_error_fatal( "ext_mcel_write_field: setLocationsXY" )
        ELSE IF ( deltax .gt. 0. .and. deltay .gt. 0. .and. originx .gt. 0. .and. originy .gt. 0. ) THEN
          dxm(1) = deltax
          dxm(2) = deltay
          call SetDX ( open_file_descriptors(2,DataHandle), dxm, ierr)
          origin(1) = originx
          origin(2) = originy
          call SetOrigin ( open_file_descriptors(2,DataHandle), origin, ierr)
        ELSE
          CALL wrf_error_fatal( "ext_mcel_write_field:noLocationsXY")
        ENDIF
        IF ( ALLOCATED(mask) ) THEN
          CALL setMask ( open_file_descriptors(2,DataHandle) , mask, ierr )
          IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: setMask")
        ENDIF
        CALL setGrid ( open_file_descriptors(1,DataHandle), open_file_descriptors(2,DataHandle), ierr )
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: setGrid")
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: setoutputgrid")
        CALL finalize ( open_file_descriptors(1,DataHandle), ierr )
        IF ( ierr .GT. 0 ) THEN
           write(mess,*)'ext_mcel_write_field: finalize ierr=',ierr
           CALL wrf_error_fatal( TRIM(mess) )
        ENDIF
        mcel_finalized( DataHandle ) = .TRUE.
      ENDIF

      timestr(1:4)   = DateStr(1:4)    ! YYYY
      timestr(5:6)   = DateStr(6:7)    ! MM
      timestr(7:8)   = DateStr(9:10)   ! DD
      timestr(9:10)  = DateStr(12:13)  ! HH
      timestr(11:12) = DateStr(15:16)  ! MM
      timestr(13:14) = DateStr(18:19)  ! SS
      CALL YYYYMMDDHHMMSS2SECS( timestr, data_time )

      IF ( FieldType .EQ. WRF_INTEGER ) THEN
        ALLOCATE(itemp(ips:ipe,jps:jpe))
        DO j = jps, jpe
          DO i = ips, ipe
            idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
            itemp(i,j) = cast_to_int(Field( idex ))
          ENDDO
        ENDDO
        CALL storeData( open_file_descriptors(1,DataHandle), TRIM(Varname), &
                        itemp, &
                        data_time, data_time,  &
                        MCEL_TIMECENT_POINT, ierr )
        DEALLOCATE(itemp)
      ELSE IF ( FieldType .EQ. WRF_DOUBLE ) THEN
        ALLOCATE(dtemp(ips:ipe,jps:jpe))
        CALL copy_field_to_cache_d2d ( Field, dtemp, ips, ipe, jps, jpe, ims, ime, jms, jme )
        CALL storeData( open_file_descriptors(1,DataHandle), TRIM(Varname), &
                        dtemp, &
                        data_time, data_time,  &
                        MCEL_TIMECENT_POINT, ierr )
        DEALLOCATE(dtemp)
      ELSE IF ( FieldType .EQ. WRF_REAL ) THEN
        ALLOCATE(temp(ips:ipe,jps:jpe))
        CALL copy_field_to_cache_r2r ( Field, temp, ips, ipe, jps, jpe, ims, ime, jms, jme )
        CALL storeData( open_file_descriptors(1,DataHandle), TRIM(Varname), &
                        temp, &
                        data_time, data_time,  &
                        MCEL_TIMECENT_POINT, ierr )
        DEALLOCATE(temp)
      ENDIF

      IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: storeData")

    ENDIF
  ELSE   ! opened for training

    ! sieve the fields coming in and grab the ones we need for geo registration
    IF      ( TRIM(VarName) .EQ. TRIM(LAT_R(DataHandle)) ) THEN
      IF ( ALLOCATED(xlat) ) THEN
        DEALLOCATE(xlat)
      ENDIF
      ALLOCATE(xlat(ips:ipe,jps:jpe))
      IF      ( FieldType .EQ. WRF_REAL ) THEN
        CALL copy_field_to_cache_r2d ( Field, xlat, ips, ipe, jps, jpe, ims, ime, jms, jme )
      ELSE IF (FieldType .EQ. WRF_DOUBLE ) THEN
        CALL copy_field_to_cache_d2d ( Field, xlat, ips, ipe, jps, jpe, ims, ime, jms, jme )
      ENDIF
    ELSE IF ( TRIM(VarName) .EQ. TRIM(LON_R(DataHandle)) ) THEN
      IF ( ALLOCATED(xlong) ) THEN
        DEALLOCATE(xlong)
      ENDIF
      ALLOCATE(xlong(ips:ipe,jps:jpe))
      IF      ( FieldType .EQ. WRF_REAL ) THEN
        CALL copy_field_to_cache_r2d ( Field, xlong, ips, ipe, jps, jpe, ims, ime, jms, jme )
      ELSE IF (FieldType .EQ. WRF_DOUBLE ) THEN
        CALL copy_field_to_cache_d2d ( Field, xlong, ips, ipe, jps, jpe, ims, ime, jms, jme )
      ENDIF
    ELSE IF ( TRIM(VarName) .EQ. TRIM(LANDMASK_I(DataHandle)) ) THEN
write(0,*)'write_field: ALLOCATED(mask)', ALLOCATED(mask)
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
        CALL copy_field_to_cache_d2d ( Field, rmask, ips, ipe, jps, jpe, ims, ime, jms, jme )
        mask = NINT( dmask )
        DEALLOCATE(dmask)
      ENDIF
    ELSE
      IF ( .NOT. mcel_grid_defined( DataHandle ) ) THEN
        mcel_grid_defined( DataHandle ) = .true.

        gSize(1) = ipe-ips+1
        gSize(2) = jpe-jps+1
        CALL setSize ( open_file_descriptors(2,DataHandle), gSize, ierr )
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: setSize")

! these will have been set in the call to open_for_write_begin from sysdepinfo
        IF ( mcel_npglobal .NE. -1 .AND. mcel_mystart .NE. -1 .AND.  &
             mcel_mnproc   .NE. -1 .AND. mcel_myproc  .NE. -1     ) THEN
          call setglobalsize(open_file_descriptors(2,DataHandle),mcel_npglobal,ierr)
          call setglobalstart(open_file_descriptors(2,DataHandle),mcel_mystart,ierr)
          call setprocinfo(open_file_descriptors(1,DataHandle),mcel_mnproc,mcel_myproc,ierr)
        ENDIF
        mcel_npglobal=-1 ; mcel_mystart=-1 ; mcel_mnproc=-1 ; mcel_myproc=-1

      ENDIF
      IF ( opened_for_read( DataHandle) ) THEN
        CALL addSources ( open_file_descriptors(1,DataHandle), MCEL_SERVER,  &
  &       TRIM(VarName),1, mcel_type, ierr )
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: addSources")
        CALL addOutputs ( open_file_descriptors(1,DataHandle),   &
  &       TRIM(VarName),1, mcel_type, ierr )
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: addOutputs")
      ELSE
        CALL addVar ( open_file_descriptors(1,DataHandle), TRIM(VarName), mcel_type, ierr )
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: addVar")
      ENDIF
    ENDIF
  ENDIF
  Status = 0
  RETURN
END SUBROUTINE ext_mcel_write_field
