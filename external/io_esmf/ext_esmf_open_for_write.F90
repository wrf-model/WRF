!--- open_for_write_begin
SUBROUTINE ext_mcel_open_for_write_begin( FileName , Comm_compute, Comm_io, SysDepInfo, &
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
  CHARACTER*256   fname
  CHARACTER*256   grid_type
  CHARACTER*256   tstr

  fname = FileName

  CALL int_get_fresh_handle(i)
  okay_to_write(i) = .false.
  okay_to_read(i) = .false.
  opened_for_update(i) = .false.
  opened_for_read(i) = .false.
  opened_for_write(i) = .true.
  mcel_grid_defined(i) = .false.
  mcel_finalized(i) = .false.
  DataHandle = i

! right now only 2d -- extend later to 3d, 1d, 0d

  CALL get_value ( 'MCEL_GRIDTYPE', SysDepInfo, grid_type )

  LAT_R(i) = ""
  LON_R(i) = ""
  LANDMASK_I(i) = ""
  CALL get_value( "LAT_R", SysDepInfo, LAT_R(i) )
  CALL get_value( "LON_R", SysDepInfo, LON_R(i) )
  CALL get_value( "LANDMASK_I", SysDepInfo, LANDMASK_I(i) )

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


  IF ( TRIM(grid_type) .EQ. 'UNSTRUCTURED' ) THEN
     CALL newGrid( open_file_descriptors(2,i), 2, MCEL_GRIDTYPE_UNSTRUCTURED, &
                   MCEL_GRIDCENT_NODAL, MCEL_GRIDCOORD_LATLONG, &
                   ierr )
  ELSE

     CALL newGrid( open_file_descriptors(2,i), 2, MCEL_GRIDTYPE_STRUCTURED, &
                   MCEL_GRIDCENT_NODAL, MCEL_GRIDCOORD_LATLONG, &
                   ierr )
  ENDIF

  IF ( ierr .NE. 0 ) CALL wrf_error_fatal( "ext_mcel_open_for_write_begin: newGrid" )

  CALL newProgram ( open_file_descriptors(1,i), TRIM(fname), ierr )

  IF ( ierr .NE. 0 ) CALL wrf_error_fatal( "ext_mcel_open_for_write_begin: newProgram" )

  Status = 0
  RETURN  
END SUBROUTINE ext_mcel_open_for_write_begin

!--- open_for_write_commit
SUBROUTINE ext_mcel_open_for_write_commit( DataHandle , Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER*80   FileName,SysDepInfo, mess
  REAL dummy
  INTEGER ierr

  IF ( int_valid_handle ( DataHandle ) ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      okay_to_write( DataHandle ) = .true.
      okay_to_read( DataHandle ) = .true.
      opened_for_update( DataHandle ) = .false.
    ENDIF
  ENDIF

! rest of this is handled lazy on first write

  Status = 0

  RETURN  
END SUBROUTINE ext_mcel_open_for_write_commit
