
!$$$here...  TBH:  remove duplication between ext_esmf_read_field and 
!$$$here...  TBH:  ext_esmf_write_field

!$$$here...  TBH:  how to deal with time?  (via current ESMF_Clock)
!$$$here...  TBH:  to begin, use it as an error check!  

!$$$here...  TBH:  where is all this destroyed?  "close"?  


!--- write_field
SUBROUTINE ext_esmf_write_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm, &
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
  INTEGER :: ims,ime,jms,jme,kms,kme
  INTEGER :: ips,ipe,jps,jpe,kps,kpe
  TYPE(ESMF_State), POINTER :: exportstate
  TYPE(ESMF_Field) :: tmpField
  TYPE(ESMF_Array) :: tmpArray
  TYPE(ESMF_ArraySpec) :: arrayspec
  TYPE(ESMF_DataKind) :: esmf_kind
  TYPE(ESMF_DataType) :: esmf_type
  TYPE(ESMF_RelLoc) :: horzRelloc
  REAL(ESMF_KIND_R4), POINTER :: data_esmf_real_ptr(:,:)
  INTEGER(ESMF_KIND_I4), POINTER :: data_esmf_int_ptr(:,:)
  INTEGER :: esmf_rank
  ! esmf_counts is redundant.  remove it as soon as ESMF_ArrayCreate no 
  ! longer requires it
  INTEGER :: esmf_counts(3)
  INTEGER :: rc
  LOGICAL, EXTERNAL :: has_char
  character*256 mess

  IF ( .NOT. int_valid_handle( DataHandle ) ) THEN
    CALL wrf_error_fatal("ext_esmf_write_field: invalid data handle" )
  ENDIF
  IF ( .NOT. int_handle_in_use( DataHandle ) ) THEN
    CALL wrf_error_fatal("ext_esmf_write_field: DataHandle not opened" )
  ENDIF
  IF ( .NOT. opened_for_write( DataHandle ) ) THEN
    CALL wrf_error_fatal("ext_esmf_write_field: DataHandle not opened for write" )
  ENDIF

write(mess,*)'ext_esmf_write_field ',DataHandle, TRIM(DateStr), TRIM(VarName)
call wrf_debug( 300, mess )

  IF      ( FieldType .EQ. WRF_REAL ) THEN
    esmf_type = ESMF_DATA_REAL
    esmf_kind = ESMF_R4
  ELSE IF ( FieldType .EQ. WRF_DOUBLE ) THEN
!    esmf_type = ESMF_DATA_REAL
!    esmf_kind = ESMF_R8
    CALL wrf_error_fatal( 'ext_esmf_write_field, WRF_DOUBLE not yet supported')
  ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
    esmf_type = ESMF_DATA_INTEGER
    esmf_kind = ESMF_I4
  ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
    CALL wrf_error_fatal( 'ext_esmf_write_field, WRF_LOGICAL not yet supported')
  ENDIF

  ims = MemoryStart(1) ; ime = MemoryEnd(1)
  jms = MemoryStart(2) ; jme = MemoryEnd(2)
  kms = MemoryStart(3) ; kme = MemoryEnd(3)

  ips = PatchStart(1) ; ipe = PatchEnd(1)
  jps = PatchStart(2) ; jpe = PatchEnd(2)
  kps = PatchStart(3) ; kpe = PatchEnd(3)

  ! For now, treat all arrays as 2D...  
!$$$ Eventually, use ../io_netcdf subroutines Transpose() and reorder() 
!$$$ (and etc.) to handle general array ranks and index orderings.  
!$$$ Some copies of these exist in ../../frame/module_io.F.  
!$$$ Then use ESMF_ArrayDataMap class to handle index mapping.  
  esmf_rank = 2
  IF ( kms /= kme ) THEN
    CALL wrf_error_fatal( 'ext_esmf_write_field:  rank > 2 not yet supported')
  ENDIF

write(mess,*) ' ext_esmf_write_field: okay_to_write: ', DataHandle, okay_to_write(DataHandle)
call wrf_debug( 300, mess )

  ! case 1: the file is opened for write but not committed ("training")
  IF ( .NOT. okay_to_write( DataHandle ) )  THEN

    ! Training:  build the ESMF export state

    ! First, build the ESMF_Grid for this DataHandle, if it does not 
    ! already exist
    CALL create_grid( DataHandle, MemoryOrder, Stagger, &
                      DomainStart(1:3), DomainEnd(1:3), &
                      MemoryStart(1:3), MemoryEnd(1:3), &
                      PatchStart(1:3), PatchEnd(1:3) )
    ! Grab the current exportState and add to it...
    CALL ESMF_ExportStateGetCurrent( exportstate, rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal("ext_esmf_write_field, training:  ESMF_ExportStateGetCurrent failed" )
    ENDIF
    ! Build ESMF objects...  
    ! Build an ESMF_ArraySpec
    CALL ESMF_ArraySpecSet(arrayspec, rank=esmf_rank, type=esmf_type, &
                                      kind=esmf_kind, rc=rc)
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal("ext_esmf_write_field:  ESMF_ArraySpecSet failed" )
    ENDIF
    ! Build an ESMF_Array
    ! Implementation note:  since we do not yet have full control over how 
    ! ESMF chooses to lay out a "patch" within "memory", we must copy by 
    ! hand.  (Reasons include lack of support in ESMF for asymmetric halos, 
    ! addition of "extra" rows/columns to optimize alignment on some machines, 
    ! handling of periodic boundary conditions, etc.)  Thus, there 
    ! is no point in using larger "memory" sizes to build the array -- patch 
    ! is fine.  Also, since we must copy anyway, might as well let ESMF manage 
    ! the memory for simplicity.  
!$$$ Once ESMF can match WRF memory-patch mapping, replace this with a more 
!$$$ efficient solution that does not require a copy.  
    ! esmf_counts is redundant.  Remove it as soon as ESMF_ArrayCreate no 
    ! longer requires it.  
    esmf_counts(1:3) = PatchEnd(1:3) - PatchStart(1:3) + 1
    tmpArray = ESMF_ArrayCreate(arrayspec, counts=esmf_counts, &
                                lbounds=PatchStart(1:3),       &
                                ubounds=PatchEnd(1:3), rc=rc)
    ! Determine grid staggering for this Field
    IF ( has_char( Stagger, 'x' ) .AND. has_char( Stagger, 'y' ) ) THEN
      CALL wrf_error_fatal( &
        "ext_esmf_write_field:  ESMF does not yet support XY staggering for C-grid" )
    ELSE IF ( has_char( Stagger, 'x' ) ) THEN
      horzrelloc=ESMF_CELL_WFACE
    ELSE IF ( has_char( Stagger, 'y' ) ) THEN
      horzrelloc=ESMF_CELL_SFACE
    ELSE
      horzrelloc=ESMF_CELL_CENTER
    ENDIF
    ! Build an ESMF_Field
    ! Note:  though it is counter-intuitive, ESMF uses 
    ! shallow-copy-masquerading-as-reference to implement the 
    ! pseudo-equivalent of POINTER assignment under-the-hood.  What this means 
    ! here is that it is OK to pass deep object tmpArray into 
    ! ESMF_FieldCreate() and then return from this subroutine.  Even though 
    ! tmpArray goes out of scope, it is OK.  However, if tmpArray were to be 
    ! modified after this call, the changes would not be guaranteed to always 
    ! appear in tmpField.  It works that way now, but ESMF Core team has 
    ! plans that may make it break in the future.  Build-it, attach-it, 
    ! flush-it will work.  Build-it, attach-it, modify-it, flush-it may not 
    ! always work.  
    ! "Pie, pie and a fox..."  
    ! Note:  unique Field name is required by ESMF_StateAddField().  
    tmpField = ESMF_FieldCreate( grid( DataHandle )%ptr, tmpArray,          &
                                 copyflag=ESMF_DATA_REF,                    &
                                 horzrelloc=horzrelloc, name=TRIM(VarName), &
                                 rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal("ext_esmf_write_field:  ESMF_FieldCreate failed" )
    ENDIF
    ! Add the Field to the export state...  
!$$$here...  for now, just build ESMF_Fields and stuff them in
!$$$here...  later, use a single ESMF_Bundle
    CALL ESMF_StateAddField( exportstate, tmpField, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal("ext_esmf_write_field:  ESMF_StateAddField failed" )
    ENDIF

  ! case 2: opened for write and committed
  ELSE IF ( okay_to_write( DataHandle ) )  THEN

    ! write:  insert data into the ESMF export state
    ! Grab the current exportState
    CALL ESMF_ExportStateGetCurrent( exportstate, rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal("ext_esmf_write_field:  ESMF_ExportStateGetCurrent failed" )
    ENDIF
    ! grab the Field
    CALL ESMF_StateGetField( exportstate, fieldName=TRIM(VarName), &
                             field=tmpfield, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal("ext_esmf_write_field:  ESMF_StateGetField failed" )
    ENDIF

    ! grab a pointer to the export state data and copy data from Field
    IF      ( FieldType .EQ. WRF_REAL ) THEN
      CALL ESMF_FieldGetDataPointer( tmpField, data_esmf_real_ptr, &
                                     ESMF_DATA_REF, rc=rc )
      IF ( rc /= ESMF_SUCCESS ) THEN
        CALL wrf_error_fatal("ext_esmf_write_field:  ESMF_FieldGetDataPointer(r4) failed" )
      ENDIF
      CALL ext_esmf_insert_data_real( Field, data_esmf_real_ptr,    &
                                      ips, ipe, jps, jpe, kps, kpe, &
                                      ims, ime, jms, jme, kms, kme )
    ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
      CALL ESMF_FieldGetDataPointer( tmpField, data_esmf_int_ptr, &
                                     ESMF_DATA_REF, rc=rc )
      IF ( rc /= ESMF_SUCCESS ) THEN
        CALL wrf_error_fatal("ext_esmf_write_field:  ESMF_FieldGetDataPointer(i4) failed" )
      ENDIF
      CALL ext_esmf_insert_data_int( Field, data_esmf_int_ptr,     &
                                     ips, ipe, jps, jpe, kps, kpe, &
                                     ims, ime, jms, jme, kms, kme )
    ENDIF

  ENDIF

  Status = 0

  RETURN

END SUBROUTINE ext_esmf_write_field

