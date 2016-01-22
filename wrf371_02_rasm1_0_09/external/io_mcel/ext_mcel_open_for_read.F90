!--- open_for_read_begin
SUBROUTINE ext_mcel_open_for_read_begin( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                         DataHandle , Status )
  USE module_ext_mcel
  IMPLICIT NONE
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER*(*) :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER i, tasks_in_group, ierr, comm_io_group
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
  REAL dummy
  INTEGER io_form
  CHARACTER*80 read_mode, grid_type, filter_handle, use_mask
  CHARACTER*256   tstr

  CALL int_get_fresh_handle(i)
  okay_to_write(i) = .false.
  okay_to_read(i) = .false.
  opened_for_read(i) = .true.
  opened_for_write(i) = .false.
  mcel_grid_defined(i) = .false.
  mcel_finalized(i) = .false.
  DataHandle = i
  ListOfFields(i) = " "

! recover the names of the strings that contain georeference and mask data, if avail
  lat_r(i) = ""
  lon_r(i) = ""
  landmask_i(i) = ""
  read_mode = ""
  grid_type = "STRUCTURED"
  filter_handle = "Filter"
  use_mask = ""
  CALL get_value( "LAT_R", SysDepInfo, lat_r(i) )
  CALL get_value( "LON_R", SysDepInfo, lon_r(i) )
  CALL get_value( "LANDMASK_I", SysDepInfo, landmask_i(i) )
  CALL get_value( "READ_MODE", SysDepInfo, read_mode )
write(0,*)'open_for_write_begin: SysDepInfo ', TRIM(SysDepInfo)
write(0,*)'open_for_write_begin: read_mode ', read_mode
  CALL get_value( "MCEL_GRIDTYPE", SysDepInfo, grid_type )
  CALL get_value( "FILTER_HANDLE", SysDepInfo, filter_handle )
  CALL get_value( "USE_MASK", SysDepInfo, use_mask )
  IF ( TRIM(read_mode) .EQ. 'UPDATE' ) THEN
    opened_for_update( i ) = .true.
  ELSE
    opened_for_update( i ) = .false.
  ENDIF
write(0,*)'opened_for_update(i) ', opened_for_update(i)

  usemask(i) = MCEL_MASK_ALLVALID
  IF ( trim(use_mask) .NE. "" ) read( use_mask , '(I)' ) usemask(i)
write(0,*)'ofrb use_mask ',trim(use_mask)
write(0,*)'ofrb filter_handle ',trim(filter_handle)
write(0,*)'ofrb usemask(',i,') = ',usemask(i)

  mcel_npglobal=-1 ; mcel_mystart=-1 ; mcel_mnproc=-1 ; mcel_myproc=-1

  CALL get_value( "MCEL_NPGLOBAL", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(I)' ) mcel_npglobal
  CALL get_value( "MCEL_MYSTART", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(I)' ) mcel_mystart
  CALL get_value( "MCEL_MNPROC", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(I)' ) mcel_mnproc
  CALL get_value( "MCEL_MYPROC", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(I)' ) mcel_myproc
  deltax = -1
  CALL get_value( "MCEL_DELTA_X", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(f15.8)' ) deltax
  deltay = -1
  CALL get_value( "MCEL_DELTA_Y", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(f15.8)' ) deltay
  originx = -1
  CALL get_value( "MCEL_ORIGIN_X", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(f15.8)' ) originx
  originy = -1
  CALL get_value( "MCEL_ORIGIN_Y", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(f15.8)' ) originy

  IF ( trim(grid_type) .EQ. 'UNSTRUCTURED' ) THEN
    CALL newGrid( open_file_descriptors(2,i), 2, MCEL_GRIDTYPE_UNSTRUCTURED, &
                  MCEL_GRIDCENT_NODAL, MCEL_GRIDCOORD_LATLONG, &
                  ierr )
  ELSE
    CALL newGrid( open_file_descriptors(2,i), 2, MCEL_GRIDTYPE_STRUCTURED, &
                  MCEL_GRIDCENT_NODAL, MCEL_GRIDCOORD_LATLONG, &
                  ierr )
  ENDIF
  IF ( ierr .NE. 0 ) CALL wrf_error_fatal( "ext_mcel_open_for_read_begin: newGrid" )

!  IF ( opened_for_update( i ) ) THEN
    ! right now the name of the ior file is coming in as the file name; whereis for writing the
    ! file name is the program name.  Needs to be resolved for this call to newfilter. 
    ! Make the FileName the program name and find a different way to bring in the ior file; sysdepinfo? environ?
    CALL newfilter ( open_file_descriptors(1,i), 'relfile:/' // TRIM(FileName), filter_handle, ierr )
    IF ( ierr .NE. 0 ) CALL wrf_error_fatal( "ext_mcel_open_for_read_begin: newfilter" )
!  ELSE
!    CALL newProgram ( open_file_descriptors(1,i), TRIM(FileName), ierr )
!    IF ( ierr .NE. 0 ) CALL wrf_error_fatal( "ext_mcel_open_for_read_begin: newProgram" )
!  ENDIF

  Status = 0
  RETURN  
END SUBROUTINE ext_mcel_open_for_read_begin

!--- open_for_read_commit
SUBROUTINE ext_mcel_open_for_read_commit( DataHandle , Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER*80   FileName,SysDepInfo, mess
  REAL dummy
  INTEGER ierr

  IF ( int_valid_handle ( DataHandle ) ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      if ( opened_for_update( DataHandle ) ) okay_to_write( DataHandle ) = .true.
      okay_to_read( DataHandle ) = .true.
    ENDIF
  ENDIF

! everything else will be handled in lazy fashion by first operation

  Status = 0

  RETURN  
END SUBROUTINE ext_mcel_open_for_read_commit
