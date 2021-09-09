!WRF:MEDIATION:IO
  SUBROUTINE wrf_ext_write_field_arr(DataHandle,DateStr,Var       &
                                ,Field                            &
                                ,idx4, idx5, idx6, idx7           &
                                ,nx4 , nx5 , nx6                  &
                                ,TypeSizeInBytes                  &
                                ,FieldType,grid                   &
                                ,DomainDesc                       &
                                ,bdy_mask                         &
                                ,dryrun                           &
                                ,MemoryOrder                      &
                                ,Stagger                          &
                                ,Dimname1, Dimname2, Dimname3     &
                                ,Desc, Units                      &
                                ,debug_message                                &
                                ,ds1, de1, ds2, de2, ds3, de3                 &
                                ,ms1, me1, ms2, me2, ms3, me3                 &
                                ,ps1, pe1, ps2, pe2, ps3, pe3, Status          )
    USE module_io
    USE module_wrf_error
    USE module_state_description
    USE module_timing
    USE module_domain

    IMPLICIT NONE

    INTEGER, INTENT(IN)       :: idx4, idx5, idx6, idx7
    INTEGER, INTENT(IN)       :: nx4 , nx5 , nx6
    INTEGER, INTENT(IN)       :: TypeSizeInBytes
    INTEGER               ,INTENT(IN   )         :: DataHandle
    CHARACTER*(*)         ,INTENT(IN   )         :: DateStr
    CHARACTER*(*)         ,INTENT(IN   )         :: Var
    INTEGER               ,INTENT(IN   )         :: Field(*)
    INTEGER               ,INTENT(IN   )         :: FieldType
    TYPE(domain)                                 :: grid
    INTEGER               ,INTENT(IN   )         :: DomainDesc
    LOGICAL               ,INTENT(IN   )         :: dryrun
    CHARACTER*(*)         ,INTENT(IN   )         :: MemoryOrder
    LOGICAL, DIMENSION(4) ,INTENT(IN   )         :: bdy_mask
    CHARACTER*(*)         ,INTENT(IN   )         :: Stagger
    CHARACTER*(*)         ,INTENT(IN   )         :: Dimname1, Dimname2, Dimname3
    CHARACTER*(*)         ,INTENT(IN   )         :: Desc, Units
    CHARACTER*(*)         ,INTENT(IN   )         :: debug_message

    INTEGER ,       INTENT(IN   ) :: ds1, de1, ds2, de2, ds3, de3, &
                                     ms1, me1, ms2, me2, ms3, me3, &
                                     ps1, pe1, ps2, pe2, ps3, pe3
    INTEGER ,       INTENT(INOUT) :: Status
! Local
    INTEGER  tsfac  ! Type size factor
    CHARACTER*256 mess

    tsfac = TypeSizeInBytes / IWORDSIZE

    IF ( tsfac .LE. 0 ) THEN
      CALL wrf_message('wrf_ext_write_field_arr')
      WRITE(mess,*)'Internal error: email this message to wrfhelp@ucar.edu ',TypeSizeInBytes,IWORDSIZE
      CALL wrf_error_fatal(mess)
    ENDIF

    CALL wrf_ext_write_field(    DataHandle,DateStr,Var           &
                                ,Field(1                                                            &
                                      +tsfac*(0                                                     &
                                      +(idx4-1)*(me3-ms3+1)*(me2-ms2+1)*(me1-ms1+1)                 &
                                      +(idx5-1)*nx4*(me3-ms3+1)*(me2-ms2+1)*(me1-ms1+1)             &
                                      +(idx6-1)*nx5*nx4*(me3-ms3+1)*(me2-ms2+1)*(me1-ms1+1)         &
                                      +(idx7-1)*nx6*nx5*nx4*(me3-ms3+1)*(me2-ms2+1)*(me1-ms1+1)))   &
                                ,FieldType,grid                   &
                                ,DomainDesc                       &
                                ,bdy_mask                         &
                                ,dryrun                           &
                                ,MemoryOrder                      &
                                ,Stagger                          &
                                ,Dimname1, Dimname2, Dimname3     &
                                ,Desc, Units                      &
                                ,debug_message                                &
                                ,ds1, de1, ds2, de2, ds3, de3                 &
                                ,ms1, me1, ms2, me2, ms3, me3                 &
                                ,ps1, pe1, ps2, pe2, ps3, pe3, Status          )
    
  END SUBROUTINE wrf_ext_write_field_arr


  SUBROUTINE wrf_ext_write_field(DataHandle,DateStr,Var,Field,FieldType,grid, &
                                 DomainDesc,                      &
                                 bdy_mask   ,                     &
                                 dryrun        ,                  &
                                 MemoryOrder,                     &
                                 Stagger,                         &
                                 Dimname1, Dimname2, Dimname3 ,   &
                                 Desc, Units,                     &
                                 debug_message ,                              &
                                 ds1, de1, ds2, de2, ds3, de3,                &
                                 ms1, me1, ms2, me2, ms3, me3,                &
                                 ps1, pe1, ps2, pe2, ps3, pe3, Status          )
    USE module_io
    USE module_wrf_error
    USE module_state_description
    USE module_timing
    USE module_domain

    IMPLICIT NONE

    INTEGER               ,INTENT(IN   )         :: DataHandle
    CHARACTER*(*)         ,INTENT(IN   )         :: DateStr
    CHARACTER*(*)         ,INTENT(IN   )         :: Var
    INTEGER               ,INTENT(IN   )         :: Field(*)
    INTEGER               ,INTENT(IN   )         :: FieldType
    TYPE(domain)                                 :: grid
    INTEGER               ,INTENT(IN   )         :: DomainDesc
    LOGICAL               ,INTENT(IN   )         :: dryrun
    CHARACTER*(*)         ,INTENT(IN   )         :: MemoryOrder
    LOGICAL, DIMENSION(4) ,INTENT(IN   )         :: bdy_mask
    CHARACTER*(*)         ,INTENT(IN   )         :: Stagger
    CHARACTER*(*)         ,INTENT(IN   )         :: Dimname1, Dimname2, Dimname3
    CHARACTER*(*)         ,INTENT(IN   )         :: Desc, Units
    CHARACTER*(*)         ,INTENT(IN   )         :: debug_message

    INTEGER ,       INTENT(IN   ) :: ds1, de1, ds2, de2, ds3, de3, &
                                     ms1, me1, ms2, me2, ms3, me3, &
                                     ps1, pe1, ps2, pe2, ps3, pe3

! Local
    INTEGER , DIMENSION(3) :: domain_start , domain_end
    INTEGER , DIMENSION(3) :: memory_start , memory_end
    INTEGER , DIMENSION(3) :: patch_start , patch_end
    CHARACTER*80 , DIMENSION(3) :: dimnames

    integer                       ,intent(inout)   :: Status
    LOGICAL for_out, horiz_stagger
    INTEGER io_form
    LOGICAL, EXTERNAL :: has_char
    INTEGER, EXTERNAL :: use_package
    INTEGER Hndl

    IF ( wrf_at_debug_level( 500 ) ) THEN
      call start_timing
    ENDIF
    domain_start(1) = ds1 ; domain_end(1) = de1 ;
    patch_start(1)  = ps1 ; patch_end(1)  = pe1 ;
    memory_start(1) = ms1 ; memory_end(1) = me1 ;
    domain_start(2) = ds2 ; domain_end(2) = de2 ;
    patch_start(2)  = ps2 ; patch_end(2)  = pe2 ;
    memory_start(2) = ms2 ; memory_end(2) = me2 ;
    domain_start(3) = ds3 ; domain_end(3) = de3 ;
    patch_start(3)  = ps3 ; patch_end(3)  = pe3 ;
    memory_start(3) = ms3 ; memory_end(3) = me3 ;

    dimnames(1) = Dimname1
    dimnames(2) = Dimname2
    dimnames(3) = Dimname3

    CALL debug_io_wrf ( debug_message,DateStr,                          &
                        domain_start,domain_end,patch_start,patch_end,  &
                        memory_start,memory_end                          )
#if 0
    Status = 1
    if ( de1 - ds1 < 0 ) return
    if ( de2 - ds2 < 0 ) return
    if ( de3 - ds3 < 0 ) return
    if ( pe1 - ps1 < 0 ) return
    if ( pe2 - ps2 < 0 ) return
    if ( pe3 - ps3 < 0 ) return
    if ( me1 - ms1 < 0 ) return
    if ( me2 - ms2 < 0 ) return
    if ( me3 - ms3 < 0 ) return
#endif
    Status = 0

    CALL wrf_write_field (   &
                       DataHandle                 &  ! DataHandle
                      ,DateStr                    &  ! DateStr
                      ,Var                        &  ! Data Name
                      ,Field                      &  ! Field
                      ,FieldType                  &  ! FieldType
                      ,grid                       &  ! grid
                      ,DomainDesc                 &  ! DomainDesc
                      ,bdy_mask                   &  ! bdy_mask
                      ,MemoryOrder                &  ! MemoryOrder
                      ,Stagger                    &  ! JMMODS 010620
                      ,dimnames                   &  ! JMMODS 001109
                      ,domain_start               &  ! DomainStart
                      ,domain_end                 &  ! DomainEnd
                      ,memory_start               &  ! MemoryStart
                      ,memory_end                 &  ! MemoryEnd
                      ,patch_start                &  ! PatchStart
                      ,patch_end                  &  ! PatchEnd
                      ,Status )

    CALL get_handle ( Hndl, io_form , for_out, DataHandle )

    IF ( ( dryrun .AND. ( use_package(io_form) .EQ. IO_NETCDF .OR. &
                          use_package(io_form) .EQ. IO_NETCDFPAR .OR. &
                          use_package(io_form) .EQ. IO_PIO    .OR. &
                          use_package(io_form) .EQ. IO_PNETCDF ) ) .OR. &
                        ( use_package(io_form) .EQ. IO_PHDF5  )   ) THEN

      CALL wrf_put_var_ti_char( &
                       DataHandle                 &  ! DataHandle
                      ,"description"              &  ! Element
                      ,Var                        &  ! Data Name
                      ,Desc                       &  ! Data
                      ,Status )
      CALL wrf_put_var_ti_char( &
                       DataHandle                 &  ! DataHandle
                      ,"units"                    &  ! Element
                      ,Var                        &  ! Data Name
                      ,Units                      &  ! Data
                      ,Status )
      CALL wrf_put_var_ti_char( &
                       DataHandle                 &  ! DataHandle
                      ,"stagger"                  &  ! Element
                      ,Var                        &  ! Data Name
                      ,Stagger                    &  ! Data
                      ,Status )
#if (EM_CORE == 1)
! TBH:  Added "coordinates" metadata for GIS folks in RAL.  It is a step 
! TBH:  towards CF.  This change was requested by Jennifer Boehnert based 
! TBH:  upon a suggestion from Nawajish Noman.  
! TBH:  TODO:  This code depends upon longitude and latitude arrays being 
! TBH:         named "XLONG", "XLAT", "XLONG_U", "XLAT_U", "XLONG_V", and 
! TBH:         "XLAT_V" for EM_CORE.  We need a more general way to handle 
! TBH:         this, possibly via the Registry.  
! TBH:  TODO:  Use dimnames(*) == south_north || west_east instead of 
! TBH:         MemoryOrder and Stagger?  It would also work for both ARW 
! TBH:         and NMM and be easier to handle via Registry...  

      IF ( ( TRIM(MemoryOrder) == 'XY' ) .AND. &
         ( ( TRIM(Var) == 'XLONG'   ) .OR. &
           ( TRIM(Var) == 'XLAT'    ) .OR. &
           ( TRIM(Var) == 'XLONG_U' ) .OR. &
           ( TRIM(Var) == 'XLAT_U'  ) .OR. &
           ( TRIM(Var) == 'XLONG_V' ) .OR. &
           ( TRIM(Var) == 'XLAT_V'  ) ) ) THEN
        horiz_stagger = .FALSE.
        IF ( LEN_TRIM(Stagger) == 1 ) THEN
          IF ( has_char( Stagger, 'x' ) ) THEN
            horiz_stagger = .TRUE.
            CALL wrf_put_var_ti_char( &
                             DataHandle                 &  ! DataHandle
                            ,"coordinates"              &  ! Element
                            ,Var                        &  ! Data Name
                            ,"XLONG_U XLAT_U"           &  ! Data
                            ,Status )
          ELSE IF ( has_char( Stagger, 'y' ) ) THEN
            horiz_stagger = .TRUE.
            CALL wrf_put_var_ti_char( &
                             DataHandle                 &  ! DataHandle
                            ,"coordinates"              &  ! Element
                            ,Var                        &  ! Data Name
                            ,"XLONG_V XLAT_V"           &  ! Data
                            ,Status )
          ENDIF
        ENDIF
        IF ( .NOT. horiz_stagger ) THEN
          CALL wrf_put_var_ti_char( &
                           DataHandle                 &  ! DataHandle
                          ,"coordinates"              &  ! Element
                          ,Var                        &  ! Data Name
                          ,"XLONG XLAT"               &  ! Data
                          ,Status )
        ENDIF
      ELSE IF ( ( TRIM(MemoryOrder) == 'XY'  ) .OR. &
                ( TRIM(MemoryOrder) == 'XZY' ) .OR. &
                ( TRIM(MemoryOrder) == 'XYZ' ) ) THEN
        horiz_stagger = .FALSE.
        IF ( LEN_TRIM(Stagger) == 1 ) THEN
          IF ( has_char( Stagger, 'x' ) ) THEN
            horiz_stagger = .TRUE.
            CALL wrf_put_var_ti_char( &
                             DataHandle                 &  ! DataHandle
                            ,"coordinates"              &  ! Element
                            ,Var                        &  ! Data Name
                            ,"XLONG_U XLAT_U XTIME"     &  ! Data
                            ,Status )
          ELSE IF ( has_char( Stagger, 'y' ) ) THEN
            horiz_stagger = .TRUE.
            CALL wrf_put_var_ti_char( &
                             DataHandle                 &  ! DataHandle
                            ,"coordinates"              &  ! Element
                            ,Var                        &  ! Data Name
                            ,"XLONG_V XLAT_V XTIME"     &  ! Data
                            ,Status )
          ENDIF
        ENDIF
        IF ( .NOT. horiz_stagger ) THEN
          CALL wrf_put_var_ti_char( &
                           DataHandle                 &  ! DataHandle
                          ,"coordinates"              &  ! Element
                          ,Var                        &  ! Data Name
                          ,"XLONG XLAT XTIME"         &  ! Data
                          ,Status )
        ENDIF
      ENDIF
#endif
    ENDIF

    IF ( wrf_at_debug_level(300) ) THEN
      WRITE(wrf_err_message,*) debug_message,' Status = ',Status
      CALL wrf_message ( TRIM(wrf_err_message) )
    ENDIF

    IF ( wrf_at_debug_level( 500 ) ) THEN
      CALL end_timing('wrf_ext_write_field')
    ENDIF

  END SUBROUTINE wrf_ext_write_field
