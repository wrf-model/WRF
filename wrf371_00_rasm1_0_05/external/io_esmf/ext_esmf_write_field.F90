

!TODO:  remove duplication between ext_esmf_read_field and 
!TODO:  ext_esmf_write_field

!TODO:  how to deal with time?  (via current ESMF_Clock)
!TODO:  to begin, use it as an error check...


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
  INTEGER :: ids,ide,jds,jde,kds,kde
  INTEGER :: ims,ime,jms,jme,kms,kme
  INTEGER :: ips,ipe,jps,jpe,kps,kpe
  TYPE(ESMF_State), POINTER :: exportstate
  TYPE(ESMF_Field) :: tmpField
  TYPE(ESMF_Array) :: tmpArray
  TYPE(ESMF_ArraySpec) :: arrayspec
!  TYPE(ESMF_DataKind) :: esmf_kind
  INTEGER :: esmf_kind
  REAL(ESMF_KIND_R4), POINTER :: data_esmf_real_ptr(:,:)
  REAL(ESMF_KIND_R4), POINTER :: tmp_esmf_r4_ptr(:,:)
  INTEGER(ESMF_KIND_I4), POINTER :: data_esmf_int_ptr(:,:)
  INTEGER, PARAMETER :: esmf_rank = 2
  INTEGER :: DomainEndFull(esmf_rank), idefull, jdefull, ict, i, j
  INTEGER :: PatchEndFull(esmf_rank), ipefull, jpefull
  ! esmf_counts is redundant.  remove it as soon as ESMF_ArrayCreate no 
  ! longer requires it
  INTEGER :: esmf_counts(esmf_rank)
  INTEGER :: rc, debug_level
  LOGICAL, EXTERNAL :: has_char
  character*256 mess

  CALL get_wrf_debug_level( debug_level )

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
call wrf_debug( 300, TRIM(mess) )

  IF      ( FieldType .EQ. WRF_REAL ) THEN
    esmf_kind = ESMF_KIND_R4
  ELSE IF ( FieldType .EQ. WRF_DOUBLE ) THEN
    CALL wrf_error_fatal( 'ext_esmf_write_field, WRF_DOUBLE not yet supported')
  ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
    esmf_kind = ESMF_KIND_I4
!TODO:  implement this (below)
    CALL wrf_error_fatal( 'ext_esmf_write_field, WRF_INTEGER not yet implemented')
  ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
    CALL wrf_error_fatal( 'ext_esmf_write_field, WRF_LOGICAL not yet supported')
  ENDIF

  ims = MemoryStart(1) ; ime = MemoryEnd(1)
  jms = MemoryStart(2) ; jme = MemoryEnd(2)
  kms = MemoryStart(3) ; kme = MemoryEnd(3)

  ips = PatchStart(1) ; ipe = PatchEnd(1)
  jps = PatchStart(2) ; jpe = PatchEnd(2)
  kps = PatchStart(3) ; kpe = PatchEnd(3)

  ids = DomainStart(1) ; ide = DomainEnd(1)
  jds = DomainStart(2) ; jde = DomainEnd(2)
  kds = DomainStart(3) ; kde = DomainEnd(3)

  ! For now, treat all arrays as 2D...  
!TODO:  Eventually, use ../io_netcdf subroutines Transpose() and reorder() 
!TODO:  (and etc.) to handle general array ranks and index orderings.  
!TODO:  Some copies of these exist in ../../frame/module_io.F.  
!TODO:  Then use ESMF_ArrayDataMap class to handle index mapping.  
  IF ( kms /= kme ) THEN
    write(mess,*)'ext_esmf_write_field: rank > 2 not yet supported. field = ',TRIM(VarName)
    CALL wrf_error_fatal( mess )
  ENDIF

! The non-staggered variables come in at one-less than
! domain dimensions, but io_esmf is currently hacked to use full
! domain spec, so adjust if not staggered.
!TODO:  Remove EndFull hackery once ESMF can support staggered 
!TODO:  grids in regional models.  (This hack works around the current 
!TODO:  need to use only larger staggered dimensions for ESMF_Arrays.)  
  CALL ioesmf_endfullhack( esmf_rank, DomainEnd, PatchEnd, Stagger, &
                           DomainEndFull, PatchEndFull )
  idefull = DomainEndFull(1)
  jdefull = DomainEndFull(2)
  ipefull = PatchEndFull(1)
  jpefull = PatchEndFull(2)

write(mess,*) ' ext_esmf_write_field: okay_to_write: ', DataHandle, okay_to_write(DataHandle)
call wrf_debug( 300, TRIM(mess) )

  ! case 1: the file is opened for write but not committed ("training")
  IF ( .NOT. okay_to_write( DataHandle ) )  THEN

    ! Training:  build the ESMF export state
write(mess,*) ' ext_esmf_write_field: TRAINING WRITE:  DataHandle = ', DataHandle
call wrf_debug( 300, TRIM(mess) )

    ! First, build the ESMF_Grid for this DataHandle, if it does not 
    ! already exist
write(0,*)__FILE__,__LINE__,'ext_esmf_write_field Stagger',TRIM(Stagger)
write(0,*)__FILE__,__LINE__,'ext_esmf_write_field VarName',TRIM(VarName)
write(0,*)__FILE__,__LINE__,'ext_esmf_write_field DomainEnd ', DomainEnd(1:esmf_rank)
write(0,*)__FILE__,__LINE__,'ext_esmf_write_field PatchEnd  ', PatchEnd(1:esmf_rank)
    CALL ioesmf_create_grid( DataHandle, esmf_rank, MemoryOrder, Stagger,      &
                             DomainStart(1:esmf_rank), DomainEnd(1:esmf_rank), &
                             MemoryStart(1:esmf_rank), MemoryEnd(1:esmf_rank), &
                             PatchStart(1:esmf_rank), PatchEnd(1:esmf_rank) )
    ! Grab the current exportState and add to it...
    CALL ESMF_ExportStateGetCurrent( exportstate, rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal("ext_esmf_write_field, training:  ESMF_ExportStateGetCurrent failed" )
    ENDIF
! BEGIN DOESNOTWORK
! The following code does not work for reasons as-yet unknown.  
! A likely suspect is lbounds and ubounds which fail in other interfaces in 
! ESMF 2.2.0rp1 ...  
    ! Build ESMF objects...  
    ! Build an ESMF_ArraySpec.  The use of ESMF_ArraySpec and ESMF_Array 
    ! objects allows some of the code that follows to be type-kind-independent.  

    ! Build an ESMF_Array
    ! Implementation note:  since we do not yet have full control over how 
    ! ESMF chooses to lay out a "patch" within "memory", we must copy by 
    ! hand.  (Reasons include lack of support in ESMF for asymmetric halos, 
    ! addition of "extra" rows/columns to optimize alignment on some machines, 
    ! handling of periodic boundary conditions, etc.)  Thus, there 
    ! is no point in using larger "memory" sizes to build the array -- patch 
    ! is fine.  Also, since we must copy anyway, might as well let ESMF manage 
    ! the memory for simplicity.  
!TODO:  Once ESMF can match WRF memory-patch mapping, replace this with a more 
!TODO:  efficient solution that does not require a copy.  
!TODO:  esmf_counts is redundant.  Remove it as soon as ESMF_ArrayCreate no 
!TODO:  longer requires it.  
!    esmf_counts(1:esmf_rank) = DomainEndFull(1:esmf_rank) - &
!                               DomainStart(1:esmf_rank) + 1
!    tmpArray = ESMF_ArrayCreate(arrayspec, counts=esmf_counts,      &
!                                lbounds=DomainStart(1:esmf_rank),   &
!                                ubounds=DomainEndFull(1:esmf_rank), &
!                                rc=rc)
!    IF ( rc /= ESMF_SUCCESS ) THEN
!      WRITE(mess,*) ' ext_esmf_write_field: ESMF_ArrayCreate failed, rc = ', rc
!      CALL wrf_error_fatal( TRIM(mess) )
!    ENDIF
    ! Determine grid staggering for this Field
!    IF ( has_char( Stagger, 'x' ) .AND. has_char( Stagger, 'y' ) ) THEN
!      CALL wrf_error_fatal( &
!        "ext_esmf_write_field:  ESMF does not yet support XY staggering for C-grid" )
!    ELSE IF ( has_char( Stagger, 'x' ) ) THEN
!      horzrelloc=ESMF_CELL_WFACE
!    ELSE IF ( has_char( Stagger, 'y' ) ) THEN
!      horzrelloc=ESMF_CELL_SFACE
!    ELSE
!      horzrelloc=ESMF_CELL_CENTER
!    ENDIF
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
    ! Note:  unique Field name is required by ESMF_StateAdd().  
!TODO:  use CF "standard_name" once the WRF Registry supports it
!    tmpField = ESMF_FieldCreate( grid( DataHandle )%ptr, tmpArray,          &
!                                 copyflag=ESMF_DATA_REF,                    &
!                                 horzrelloc=horzrelloc, name=TRIM(VarName), &
!                                 rc=rc )
! END DOESNOTWORK
!TODO:  Compute horzrelloc from Stagger as above once ESMF supports staggering
!    horzrelloc=ESMF_CELL_CENTER
!TODO:  Add code for other data types here...  
!    ALLOCATE( tmp_esmf_r4_ptr(ips:ipefull,jps:jpefull) )
    ALLOCATE( tmp_esmf_r4_ptr(ips:ipe,jps:jpe) )
    CALL wrf_debug ( 100, 'ext_esmf_write_field: calling ESMF_FieldCreate' )
    tmpField = ESMF_FieldCreate(         &
                 grid( DataHandle )%ptr, &
                 tmp_esmf_r4_ptr,        &
                 datacopyflag=ESMF_DATACOPY_REFERENCE, &
                 staggerloc=ESMF_STAGGERLOC_CENTER,    &
                 name=TRIM(VarName),     &
                 rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      WRITE(mess,*) ' ext_esmf_write_field: ESMF_FieldCreate failed, rc = ', rc
      CALL wrf_error_fatal( TRIM(mess) )
    ENDIF
    CALL wrf_debug ( 100, 'ext_esmf_write_field: back from ESMF_FieldCreate' )
    WRITE(mess,*) 'ext_esmf_write_field: tmp_esmf_r4_ptr(',        &
      LBOUND(tmp_esmf_r4_ptr,1),':',UBOUND(tmp_esmf_r4_ptr,1),',', &
      LBOUND(tmp_esmf_r4_ptr,2),':',UBOUND(tmp_esmf_r4_ptr,2),')'
    CALL wrf_debug ( 100 , TRIM(mess) )
    ! Add the Field to the export state...  
!TODO:  for now, just build ESMF_Fields and stuff them in
!TODO:  later, use a single ESMF_Bundle
    CALL ESMF_StateAdd( exportstate, (/tmpField/), rc=rc ) ! 5.2.0r only accepts arrays; use array constructor
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal("ext_esmf_write_field:  ESMF_StateAddfailed" )
    ENDIF
write(mess,*) ' ext_esmf_write_field: END TRAINING WRITE:  DataHandle = ', DataHandle
call wrf_debug( 300, TRIM(mess) )

  ! case 2: opened for write and committed
  ELSE IF ( okay_to_write( DataHandle ) )  THEN

write(mess,*) ' ext_esmf_write_field: ACTUAL WRITE:  DataHandle = ', DataHandle
call wrf_debug( 300, TRIM(mess) )

    ! write:  insert data into the ESMF export state
    ! Grab the current exportState
    CALL ESMF_ExportStateGetCurrent( exportstate, rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal("ext_esmf_write_field:  ESMF_ExportStateGetCurrent failed" )
    ENDIF
    ! grab the Field
    CALL ESMF_StateGet( exportstate, itemName=TRIM(VarName), &
                             field=tmpfield, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal("ext_esmf_write_field:  ESMF_StateGetfailed" )
    ENDIF

CALL wrf_debug ( 100, 'ext_esmf_write_field '//TRIM(VarName)//':  calling ESMF_FieldPrint( tmpField ) 1' )
IF ( 100 .LE. debug_level ) THEN
  CALL ESMF_FieldPrint( tmpField, rc=rc )
ENDIF
CALL wrf_debug ( 100, 'ext_esmf_write_field '//TRIM(VarName)//':  back from ESMF_FieldPrint( tmpField ) 1' )

    ! grab a pointer to the export state data and copy data from Field
    IF      ( FieldType .EQ. WRF_REAL ) THEN
      CALL ESMF_FieldGet( tmpField, 0, data_esmf_real_ptr, rc=rc )
      IF ( rc /= ESMF_SUCCESS ) THEN
        CALL wrf_error_fatal("ext_esmf_write_field:  ESMF_FieldGetDataPointer(r4) failed" )
      ENDIF
      IF ( ( PatchStart(1)   /= LBOUND(data_esmf_real_ptr,1) ) .OR. &
           ( PatchEnd(1) /= UBOUND(data_esmf_real_ptr,1) ) .OR. &
           ( PatchStart(2)   /= LBOUND(data_esmf_real_ptr,2) ) .OR. &
           ( PatchEnd(2) /= UBOUND(data_esmf_real_ptr,2) ) ) THEN
        WRITE( mess,* ) 'ESMF_FieldGetDataPointer bounds mismatch',          &
          __FILE__ ,                                                         &
          ', line ',                                                         &
          __LINE__ ,                                                         &
          ', ips:ipe,jps:jpe = ',PatchStart(1),':',PatchEnd(1),',',      &
                                 PatchStart(2),':',PatchEnd(2),          &
          ', data_esmf_real_ptr(BOUNDS) = ',                                 &
          LBOUND(data_esmf_real_ptr,1),':',UBOUND(data_esmf_real_ptr,1),',', &
          LBOUND(data_esmf_real_ptr,2),':',UBOUND(data_esmf_real_ptr,2)
        CALL wrf_error_fatal ( TRIM(mess) )
      ENDIF

WRITE( mess,* ) 'DEBUG:  ext_esmf_write_field:  ips:ipe,jps:jpe = ', &
  ips,':',ipe,',',jps,':',jpe,                                       &
  ', data_esmf_real_ptr(BOUNDS) = ',                                 &
  LBOUND(data_esmf_real_ptr,1),':',UBOUND(data_esmf_real_ptr,1),',', &
  LBOUND(data_esmf_real_ptr,2),':',UBOUND(data_esmf_real_ptr,2)
CALL wrf_debug( 300, TRIM(mess) )

      CALL ioesmf_insert_data_real( Field, data_esmf_real_ptr,            &
                                    ips, ipe, jps, jpe, kps, kpe, &
                                    ims, ime, jms, jme, kms, kme )
    ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
      CALL ESMF_FieldGet( tmpField, 0, data_esmf_int_ptr, rc=rc )
      IF ( rc /= ESMF_SUCCESS ) THEN
        CALL wrf_error_fatal("ext_esmf_write_field:  ESMF_FieldGetDataPointer(i4) failed" )
      ENDIF
      IF ( ( PatchStart(1)   /= LBOUND(data_esmf_int_ptr,1) ) .OR. &
           ( PatchEnd(1) /= UBOUND(data_esmf_int_ptr,1) ) .OR. &
           ( PatchStart(2)   /= LBOUND(data_esmf_int_ptr,2) ) .OR. &
           ( PatchEnd(2) /= UBOUND(data_esmf_int_ptr,2) ) ) THEN
        WRITE( mess,* ) 'ESMF_FieldGetDataPointer bounds mismatch',        &
          __FILE__ ,                                                       &
          ', line ',                                                       &
          __LINE__ ,                                                       &
          ', ips:ipe,jps:jpe = ',PatchStart(1),':',PatchEnd(1),',',    &
                                 PatchStart(2),':',PatchEnd(2),        &
          ', data_esmf_int_ptr(BOUNDS) = ',                                &
          LBOUND(data_esmf_int_ptr,1),':',UBOUND(data_esmf_int_ptr,1),',', &
          LBOUND(data_esmf_int_ptr,2),':',UBOUND(data_esmf_int_ptr,2)
        CALL wrf_error_fatal ( TRIM(mess) )
      ENDIF
      CALL ioesmf_insert_data_int( Field, data_esmf_int_ptr,             &
                                   ips, ipe, jps, jpe, kps, kpe, &
                                   ims, ime, jms, jme, kms, kme )
    ENDIF
write(mess,*) ' ext_esmf_write_field: END ACTUAL WRITE:  DataHandle = ', DataHandle
call wrf_debug( 300, TRIM(mess) )

  ENDIF

CALL wrf_debug ( 100, 'ext_esmf_write_field '//TRIM(VarName)//':  calling ESMF_FieldPrint( tmpField ) 2' )
IF ( 100 .LE. debug_level ) THEN
  CALL ESMF_FieldPrint( tmpField, rc=rc )
ENDIF
CALL wrf_debug ( 100, 'ext_esmf_write_field '//TRIM(VarName)//':  back from ESMF_FieldPrint( tmpField ) 2' )

  Status = 0

  RETURN

END SUBROUTINE ext_esmf_write_field

