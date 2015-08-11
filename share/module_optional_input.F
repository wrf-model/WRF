MODULE module_optional_input

   INTEGER :: flag_metgrid  , flag_tavgsfc  , flag_psfc     , flag_soilhgt  , flag_mf_xy , flag_slp , &
              flag_snow     , flag_snowh    , flag_tsk      , flag_pinterp  , flag_prho

   INTEGER :: flag_qv       , flag_qc       , flag_qr       , flag_qi       , flag_qs       , &
              flag_qg       , flag_qh       , &
              flag_qni      , flag_qnr      , &
              flag_qnwfa    , flag_qnifa    , &
              flag_sh       , flag_speccldl , flag_speccldf

   INTEGER :: flag_soil_levels, flag_soil_layers

   INTEGER :: flag_st000010 , flag_st010040 , flag_st040100 , flag_st100200 , flag_st010200 , &
              flag_sm000010 , flag_sm010040 , flag_sm040100 , flag_sm100200 , flag_sm010200 , &
              flag_sw000010 , flag_sw010040 , flag_sw040100 , flag_sw100200 , flag_sw010200

   INTEGER :: flag_st000007 , flag_st007028 , flag_st028100 , flag_st100255 , &
              flag_sm000007 , flag_sm007028 , flag_sm028100 , flag_sm100255

   INTEGER :: flag_soilt000 , flag_soilt005 , flag_soilt020 , flag_soilt040 , flag_soilt160 , flag_soilt300 , &
              flag_soilm000 , flag_soilm005 , flag_soilm020 , flag_soilm040 , flag_soilm160 , flag_soilm300 , &
              flag_soilw000 , flag_soilw005 , flag_soilw020 , flag_soilw040 , flag_soilw160 , flag_soilw300

   INTEGER :: flag_sst      , flag_toposoil

   INTEGER :: flag_icedepth , flag_icefrac  , flag_albsi , flag_snowsi

   INTEGER :: flag_ptheta

   INTEGER :: flag_lake_depth

   INTEGER :: flag_excluded_middle

   INTEGER :: flag_um_soil
   INTEGER :: flag_icepct

   INTEGER :: flag_hgtmaxw  , flag_pmaxw    , flag_tmaxw    , flag_umaxw    , flag_vmaxw     , &
              flag_hgttrop  , flag_ptrop    , flag_ttrop    , flag_utrop    , flag_vtrop
   INTEGER :: flag_pmaxwnn  , flag_ptropnn
   INTEGER :: flag_extra_levels

   INTEGER                  :: num_soil_levels_input
   INTEGER                  :: num_st_levels_input , num_sm_levels_input , num_sw_levels_input
   INTEGER                  :: num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc
   INTEGER , DIMENSION(100) ::     st_levels_input ,     sm_levels_input ,     sw_levels_input
   REAL , ALLOCATABLE , DIMENSION(:,:,:) :: st_input , sm_input , sw_input

   CHARACTER (LEN=80) , PRIVATE :: flag_name
 
   LOGICAL :: already_been_here

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE init_module_optional_input ( grid , config_flags ) 

      USE module_domain	, ONLY : domain
      USE module_configure , ONLY : grid_config_rec_type

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      TYPE (grid_config_rec_type) :: config_flags

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      !  Get the various indices, assume XYZ & XZY ordering.
#if (NMM_CORE==1)
      ids = grid%sd31 ; ide = grid%ed31 ;
      jds = grid%sd32 ; jde = grid%ed32 ;
      kds = grid%sd33 ; kde = grid%ed33 ;

      ims = grid%sm31 ; ime = grid%em31 ;
      jms = grid%sm32 ; jme = grid%em32 ;
      kms = grid%sm33 ; kme = grid%em33 ;

      its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
      jts = grid%sp32 ; jte = grid%ep32 ;   ! note that tile is entire patch
      kts = grid%sp33 ; kte = grid%ep33 ;   ! note that tile is entire patch
#endif
#if (EM_CORE==1)
      ids = grid%sd31 ; ide = grid%ed31 ;
      kds = grid%sd32 ; kde = grid%ed32 ;
      jds = grid%sd33 ; jde = grid%ed33 ;

      ims = grid%sm31 ; ime = grid%em31 ;
      kms = grid%sm32 ; kme = grid%em32 ;
      jms = grid%sm33 ; jme = grid%em33 ;

      its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
      kts = grid%sp32 ; kte = grid%ep32 ;   ! note that tile is entire patch
      jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch
#endif
      IF ( .NOT. already_been_here ) THEN

         num_st_levels_alloc = config_flags%num_soil_layers * 3 ! used to be 2
         num_sm_levels_alloc = config_flags%num_soil_layers * 3
         num_sw_levels_alloc = config_flags%num_soil_layers * 3

         IF ( ALLOCATED ( st_input ) ) DEALLOCATE ( st_input )
         IF ( ALLOCATED ( sm_input ) ) DEALLOCATE ( sm_input )
         IF ( ALLOCATED ( sw_input ) ) DEALLOCATE ( sw_input )
   
         ALLOCATE ( st_input(ims:ime,num_st_levels_alloc,jms:jme) )
         ALLOCATE ( sm_input(ims:ime,num_sm_levels_alloc,jms:jme) )
         ALLOCATE ( sw_input(ims:ime,num_sw_levels_alloc,jms:jme) )

      END IF

      already_been_here = .TRUE.

   END SUBROUTINE init_module_optional_input

#if (DA_CORE != 1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_input ( grid , fid, config_flags )

      USE module_io_domain
      USE module_configure       , ONLY : grid_config_rec_type
      USE module_domain	, ONLY : domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      TYPE (grid_config_rec_type) :: config_flags
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr, num_layers
      CHARACTER (LEN=132) :: message

      !  Get the various indices, assume XYZ & XZY ordering.
#if (NMM_CORE==1)
      ids = grid%sd31 ; ide = grid%ed31 ;
      jds = grid%sd32 ; jde = grid%ed32 ;
      kds = grid%sd33 ; kde = grid%ed33 ;

      ims = grid%sm31 ; ime = grid%em31 ;
      jms = grid%sm32 ; jme = grid%em32 ;
      kms = grid%sm33 ; kme = grid%em33 ;

      its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
      jts = grid%sp32 ; jte = grid%ep32 ;   ! note that tile is entire patch
      kts = grid%sp33 ; kte = grid%ep33 ;   ! note that tile is entire patch
#endif
#if (EM_CORE==1)
      ids = grid%sd31 ; ide = grid%ed31 ;
      kds = grid%sd32 ; kde = grid%ed32 ;
      jds = grid%sd33 ; jde = grid%ed33 ;

      ims = grid%sm31 ; ime = grid%em31 ;
      kms = grid%sm32 ; kme = grid%em32 ;
      jms = grid%sm33 ; jme = grid%em33 ;

      its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
      kts = grid%sp32 ; kte = grid%ep32 ;   ! note that tile is entire patch
      jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch
#endif

      CALL optional_tsk        ( grid , fid , &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte  )

      CALL optional_tavgsfc    ( grid , fid , &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte  )

      CALL optional_moist      ( grid , fid , &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte  )

      CALL optional_metgrid    ( grid , fid , &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte  )

      CALL optional_sst        ( grid , fid , &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte  )

      CALL optional_snowh      ( grid , fid , &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte  )


      CALL optional_sfc        ( grid , fid , &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte  )

      CALL optional_ice        ( grid , fid , &
                                 config_flags%seaice_albedo_opt , &
                                 config_flags%seaice_snowdepth_opt , &
                                 config_flags%seaice_thickness_opt , &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte  )
      CALL optional_lake       ( grid , fid , &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte  )

      CALL optional_ptheta     ( grid , fid , &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte  )

      CALL optional_excl_middle( grid , fid , &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte  )

      CALL optional_levels     ( grid , fid , &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte  ) 

      flag_soil_levels = 0 
      flag_soil_layers = 0 

      !  How many soil levels have we found?  Well, right now, none.

      num_st_levels_input = 0
      num_sm_levels_input = 0
      num_sw_levels_input = 0

      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_SOIL_LEVELS', itmp, 1, icnt, ierr ) 

      IF ( ierr .EQ. 0 ) THEN
         flag_soil_levels = itmp
         write (message,'(A50,I3)') 'flag_soil_levels read from met_em file is',flag_soil_levels
         CALL wrf_debug(0,message)
      END IF

      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_SOIL_LAYERS', itmp, 1, icnt, ierr ) 

      IF ( ierr .EQ. 0 ) THEN
         flag_soil_layers = itmp
         write (message,'(A50,I3)') 'flag_soil_layers read from met_em file is',flag_soil_layers
         CALL wrf_debug(0,message)
      END IF

#if (EM_CORE == 1)
      IF ( ( flag_soil_levels == 1 ) .OR. ( flag_soil_layers == 1 ) ) THEN

         num_st_levels_input   = config_flags%num_metgrid_soil_levels
         num_sm_levels_input   = config_flags%num_metgrid_soil_levels
         num_sw_levels_input   = config_flags%num_metgrid_soil_levels
         num_soil_levels_input = config_flags%num_metgrid_soil_levels

      END IF
#endif

      IF (  ( model_config_rec%sf_surface_physics(grid%id) .EQ. 1 ) .OR. &
            ( model_config_rec%sf_surface_physics(grid%id) .EQ. 2 ) .OR. &
            ( model_config_rec%sf_surface_physics(grid%id) .EQ. 3 ) .OR. &
            ( model_config_rec%sf_surface_physics(grid%id) .EQ. 4 ) .OR. &
            ( model_config_rec%sf_surface_physics(grid%id) .EQ. 5 ) .OR. &
            ( model_config_rec%sf_surface_physics(grid%id) .EQ. 7 ) .OR. &
            ( model_config_rec%sf_surface_physics(grid%id) .EQ. 8 ) .OR. & !fds 
            ( model_config_rec%sf_surface_physics(grid%id) .EQ. 88 ) ) THEN
   
         CALL optional_lsm_levels ( grid , fid , &
                                    ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    its, ite, jts, jte, kts, kte  )
      END IF
     
   END SUBROUTINE optional_input

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_moist ( grid , fid , &
                               ids, ide, jds, jde, kds, kde, &
                               ims, ime, jms, jme, kms, kme, &
                               its, ite, jts, jte, kts, kte  )

      USE module_io_wrf
      USE module_domain	, ONLY : domain

USE module_configure , ONLY : grid_config_rec_type
USE module_io_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr

      flag_name = '                                                                                '

      flag_qv       = 0
      flag_qc       = 0
      flag_qr       = 0
      flag_qi       = 0
      flag_qs       = 0
      flag_qg       = 0
      flag_qh       = 0
      flag_qni      = 0
      flag_qnr      = 0
      flag_qnwfa    = 0
      flag_qnifa    = 0
      flag_sh       = 0
      flag_speccldl = 0
      flag_speccldf = 0

      flag_name(1:8) = 'QV      '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_qv       = itmp
      END IF
      flag_name(1:8) = 'QC      '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_qc       = itmp
      END IF
      flag_name(1:8) = 'QR      '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_qr       = itmp
      END IF
      flag_name(1:8) = 'QI      '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_qi       = itmp
      END IF
      flag_name(1:8) = 'QS      '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_qs       = itmp
      END IF
      flag_name(1:8) = 'QG      '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_qg       = itmp
      END IF
      flag_name(1:8) = 'QH      '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_qh       = itmp
      END IF
      flag_name(1:8) = 'QNI      '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr )
      IF ( ierr .EQ. 0 ) THEN
         flag_qni       = itmp
      END IF
      flag_name(1:8) = 'QNR      '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr )
      IF ( ierr .EQ. 0 ) THEN
         flag_qnr       = itmp
      END IF
      flag_name(1:8) = 'QNWFA    '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr )
      IF ( ierr .EQ. 0 ) THEN
         flag_qnwfa     = itmp
      END IF
      flag_name(1:8) = 'QNIFA    '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr )
      IF ( ierr .EQ. 0 ) THEN
         flag_qnifa     = itmp
      END IF
      flag_name(1:8) = 'SH      '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_sh       = itmp
      END IF
      flag_name(1:8) = 'SPECCLDL'
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_speccldl = itmp
      END IF
      flag_name(1:8) = 'SPECCLDF'
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_speccldf = itmp
      END IF
    
   END SUBROUTINE optional_moist

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_metgrid ( grid , fid , &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte  )

      USE module_io_wrf
      USE module_domain	, ONLY : domain
USE module_configure , ONLY : grid_config_rec_type
USE module_io_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr

      flag_name = '                                                                                '

      flag_metgrid = 0 

      flag_name(1:8) = 'METGRID '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_metgrid  = itmp
      END IF

      flag_pinterp = 0

      flag_name(1:8) = 'P_INTERP'
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr )
      IF ( ierr .EQ. 0 ) THEN
         flag_pinterp  = itmp
      END IF

      flag_mf_xy = 0 

      flag_name(1:8) = 'MF_XY   '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_mf_xy    = itmp
      END IF
    
      grid%flag_metgrid = flag_metgrid
      grid%flag_mf_xy   = flag_mf_xy
   END SUBROUTINE optional_metgrid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_sst ( grid , fid , &
                             ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             its, ite, jts, jte, kts, kte  )

      USE module_io_wrf
      USE module_domain	, ONLY : domain
USE module_configure , ONLY : grid_config_rec_type
USE module_io_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr

      flag_name = '                                                                                '

      flag_sst      = 0 

      flag_name(1:8) = 'SST     '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_sst      = itmp
      END IF
    
   END SUBROUTINE optional_sst

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_levels ( grid , fid , &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                its, ite, jts, jte, kts, kte  )

      USE module_io_wrf
      USE module_domain	, ONLY : domain
USE module_configure , ONLY : grid_config_rec_type
USE module_io_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr

      flag_name = '                                                                                '

      flag_hgtmaxw  = 0 
      flag_pmaxw    = 0 
      flag_pmaxwnn  = 0 
      flag_tmaxw    = 0 
      flag_umaxw    = 0 
      flag_vmaxw    = 0 
      flag_hgttrop  = 0 
      flag_ptrop    = 0 
      flag_ptropnn  = 0 
      flag_ttrop    = 0 
      flag_utrop    = 0 
      flag_vtrop    = 0 
      flag_extra_levels = 0

      flag_name(1:8) = 'HGTMAXW '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_hgtmaxw  = itmp
      END IF
      flag_name(1:8) = 'PMAXW   '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_pmaxw    = itmp
      END IF
      flag_name(1:8) = 'PMAXWNN '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_pmaxwnn  = itmp
      END IF
      flag_name(1:8) = 'TMAXW   '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_tmaxw    = itmp
      END IF
      flag_name(1:8) = 'UMAXW   '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_umaxw    = itmp
      END IF
      flag_name(1:8) = 'VMAXW   '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_vmaxw    = itmp
      END IF
      flag_name(1:8) = 'HGTTROP '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_hgttrop  = itmp
      END IF
      flag_name(1:8) = 'PTROP   '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_ptrop    = itmp
      END IF
      flag_name(1:8) = 'PTROPNN '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_ptropnn  = itmp
      END IF
      flag_name(1:8) = 'TTROP   '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_ttrop    = itmp
      END IF
      flag_name(1:8) = 'UTROP   '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_utrop    = itmp
      END IF
      flag_name(1:8) = 'VTROP   '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_vtrop    = itmp
      END IF

      !  Newer versions of WPS supply a nearest-neighbor version of pressure and a
      !  pressure that is suitable for interpolation.  If the nearest neighbor is 
      !  missing, all fields are set to unavailable.

      IF ( flag_pmaxwnn .EQ. 0 ) THEN
         flag_hgtmaxw  = 0 
         flag_pmaxw    = 0 
         flag_tmaxw    = 0 
         flag_umaxw    = 0 
         flag_vmaxw    = 0 
      END IF

      IF ( flag_ptropnn .EQ. 0 ) THEN
         flag_hgttrop  = 0 
         flag_ptrop    = 0 
         flag_ttrop    = 0 
         flag_utrop    = 0 
         flag_vtrop    = 0 
      END IF

      flag_extra_levels = flag_hgtmaxw*flag_pmaxw*flag_tmaxw*flag_umaxw*flag_vmaxw* &
                          flag_hgttrop*flag_ptrop*flag_ttrop*flag_utrop*flag_vtrop

   END SUBROUTINE optional_levels

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   SUBROUTINE optional_lake ( grid , fid , &
                             ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             its, ite, jts, jte, kts, kte  )

      USE module_io_wrf
      USE module_domain	, ONLY : domain
USE module_configure , ONLY : grid_config_rec_type
USE module_io_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr

      flag_name = '                                                                                '

      flag_lake_depth      = 0 

      flag_name(1:10) = 'LAKE_DEPTH'
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_lake_depth      = itmp
      END IF
    
   END SUBROUTINE optional_lake

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_tsk     ( grid , fid , &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte  )

      USE module_io_wrf
      USE module_domain	, ONLY : domain
USE module_configure , ONLY : grid_config_rec_type
USE module_io_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr

      flag_name = '                                                                                '

      flag_tsk      = 0 

      flag_name(1:8) = 'TSK     '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_tsk      = itmp
      END IF
    
   END SUBROUTINE optional_tsk

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_tavgsfc ( grid , fid , &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte  )

      USE module_io_wrf
      USE module_domain	, ONLY : domain
USE module_configure , ONLY : grid_config_rec_type
USE module_io_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr

      flag_name = '                                                                                '

      flag_tavgsfc  = 0 

      flag_name(1:8) = 'TAVGSFC '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_tavgsfc  = itmp
      END IF
    
   END SUBROUTINE optional_tavgsfc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_snowh ( grid , fid , &
                               ids, ide, jds, jde, kds, kde, &
                               ims, ime, jms, jme, kms, kme, &
                               its, ite, jts, jte, kts, kte  )

      USE module_io_wrf
      USE module_domain	, ONLY : domain
USE module_configure , ONLY : grid_config_rec_type
USE module_io_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr

      flag_name = '                                                                                '

      flag_snowh    = 0 

      flag_name(1:8) = 'SNOWH   '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_snowh    = itmp
      END IF

      flag_snow     = 0 

      flag_name(1:8) = 'SNOW    '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_snow     = itmp
      END IF
      grid%flag_snow = flag_snow

   END SUBROUTINE optional_snowh

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_sfc ( grid , fid , &
                             ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             its, ite, jts, jte, kts, kte  )

      USE module_io_wrf
      USE module_domain	, ONLY : domain
USE module_configure , ONLY : grid_config_rec_type
USE module_io_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr

      flag_name = '                                                                                '

      flag_psfc     = 0 
      flag_soilhgt  = 0 
      flag_toposoil = 0 
      flag_slp      = 0 

      flag_name(1:8) = 'TOPOSOIL'
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_toposoil = itmp
      END IF

      flag_name(1:8) = 'PSFC    '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_psfc     = itmp
      END IF

      flag_name(1:8) = 'SOILHGT '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_soilhgt  = itmp
      END IF

      flag_name(1:8) = 'SLP     '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_slp      = itmp
      END IF

      flag_name(1:8) = 'UM_SOIL '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr )
      IF ( ierr .EQ. 0 ) THEN
         flag_um_soil  = itmp
      END IF
    
      grid%flag_soilhgt = flag_soilhgt
      grid%flag_slp     = flag_slp 
      grid%flag_psfc    = flag_psfc
   END SUBROUTINE optional_sfc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_ice ( grid , fid , &
                             seaice_albedo_opt , &
                             seaice_snowdepth_opt , &
                             seaice_thickness_opt , &
                             ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             its, ite, jts, jte, kts, kte  )

      USE module_io_wrf
      USE module_domain	, ONLY : domain
      USE module_configure , ONLY : grid_config_rec_type
      USE module_io_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid
      INTEGER , INTENT(IN) :: seaice_albedo_opt
      INTEGER , INTENT(IN) :: seaice_snowdepth_opt
      INTEGER , INTENT(IN) :: seaice_thickness_opt

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr, i, j

      flag_name = '                                                                                '

      flag_icedepth = 0 
      flag_icefrac  = 0 
      flag_albsi    = 0
      flag_snowsi   = 0

      flag_name(1:8) = 'ICEFRAC '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_icefrac  = itmp
      END IF

      flag_name(1:8) = 'ICEPCT  '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr )
      IF ( ierr .EQ. 0 ) THEN
         flag_icepct  = itmp
      END IF

      flag_name(1:8) = 'ICEDEPTH'
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_icedepth = itmp
      END IF

      flag_name(1:8) = 'ALBSI   '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_albsi  = itmp
      END IF
    
      flag_name(1:8) = 'SNOWSI  '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_snowsi  = itmp
      END IF

      !
      !  Check that ICEDEPTH field is available for SEAICE_THICKNESS_OPT == 1
      !
      IF ( flag_icedepth == 0 ) THEN
          IF ( seaice_thickness_opt == 1 ) THEN
             call wrf_error_fatal("Field ICEDEPTH not found in input.  Field ICEDEPTH is required if SEAICE_THICKNESS_OPT=1")
             ! DO j = jts , MIN(jde-1,jte)
             !     DO i = its , MIN(ide-1,ite)
             !         grid%icedepth(i,j) = -1.E8
             !     ENDDO
             ! ENDDO
          ENDIF
      ENDIF
    
      !
      !  Check that ALBSI field is available for SEAICE_ALBEDO_OPT == 2
      !
      IF ( flag_albsi == 0 ) THEN
          IF ( seaice_albedo_opt == 2 ) THEN
             call wrf_error_fatal("Field ALBSI not found in input.  Field ALBSI is required if SEAICE_ALBEDO_OPT=2")
             ! DO j = jts , MIN(jde-1,jte)
             !     DO i = its , MIN(ide-1,ite)
             !         grid%albsi(i,j) = -1.E8
             !     ENDDO
             ! ENDDO
          ENDIF
      ENDIF

      !
      !  Check that SNOWSI field is available for SEAICE_SNOWDEPTH_OPT == 1
      !
      IF ( flag_snowsi == 0 ) THEN
          IF ( seaice_snowdepth_opt == 1 ) THEN
             call wrf_error_fatal("Field SNOWSI not found in input.  Field SNOWSI is required if SEAICE_SNOWDEPTH_OPT=1")
             ! DO j = jts , MIN(jde-1,jte)
             !     DO i = its , MIN(ide-1,ite)
             !         grid%snowsi(i,j) = -1.E8
             !     ENDDO
             ! ENDDO
          ENDIF
      ENDIF

   END SUBROUTINE optional_ice

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_ptheta ( grid , fid , &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                its, ite, jts, jte, kts, kte  )

      USE module_io_wrf
      USE module_domain	, ONLY : domain
      USE module_configure , ONLY : grid_config_rec_type
      USE module_io_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr

      flag_name = '                                                                                '

      flag_ptheta = 0 

      flag_name(1:8) = 'PTHETA  '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_ptheta = itmp
      END IF

      flag_name = '                                                                                '

      flag_prho = 0 

      flag_name(1:8) = 'PRHO    '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_prho = itmp
      END IF

   END SUBROUTINE optional_ptheta

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_excl_middle ( grid , fid , &
                                     ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte  )

      USE module_io_wrf
      USE module_domain	, ONLY : domain
      USE module_configure , ONLY : grid_config_rec_type
      USE module_io_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr

      flag_name = '                                                                                '

      flag_excluded_middle = 0 

      flag_name(1:16) = 'EXCLUDED_MIDDLE '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_excluded_middle = itmp
      END IF

   END SUBROUTINE optional_excl_middle

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_lsm_levels ( grid , fid , &
                                    ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    its, ite, jts, jte, kts, kte  )

      USE module_io_wrf
      USE module_domain	, ONLY : domain
      !USE module_configure , ONLY : grid_config_rec_type
      USE module_io_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr , i , j , k
      INTEGER :: level_above
      CHARACTER (LEN=132) :: message
    
      !  Initialize the soil temp and moisture flags to "field not found".

      flag_name = '                                                                                '

      flag_st000010 = 0 
      flag_st010040 = 0
      flag_st040100 = 0
      flag_st100200 = 0
      flag_st010200 = 0

      flag_sm000010 = 0
      flag_sm010040 = 0
      flag_sm040100 = 0
      flag_sm100200 = 0
      flag_sm010200 = 0

      flag_sw000010 = 0
      flag_sw010040 = 0
      flag_sw040100 = 0
      flag_sw100200 = 0
      flag_sw010200 = 0

      flag_st000007 = 0 
      flag_st007028 = 0
      flag_st028100 = 0
      flag_st100255 = 0

      flag_sm000007 = 0
      flag_sm007028 = 0
      flag_sm028100 = 0
      flag_sm100255 = 0

      flag_soilt000 = 0 
      flag_soilt005 = 0 
      flag_soilt020 = 0 
      flag_soilt040 = 0 
      flag_soilt160 = 0 
      flag_soilt300 = 0 

      flag_soilm000 = 0 
      flag_soilm005 = 0 
      flag_soilm020 = 0 
      flag_soilm040 = 0 
      flag_soilm160 = 0 
      flag_soilm300 = 0 

      flag_soilw000 = 0 
      flag_soilw005 = 0 
      flag_soilw020 = 0 
      flag_soilw040 = 0 
      flag_soilw160 = 0 
      flag_soilw300 = 0 

      st_levels_input = -1
      sm_levels_input = -1
      sw_levels_input = -1

#if (EM_CORE==1)
!-------------------------------------------------------------------------
! NOTE:  We are assuming that soil_layers are the same for each grid point
!-------------------------------------------------------------------------
      IF ( flag_soil_levels == 1 ) THEN

         DO k = 1, num_st_levels_input
            st_levels_input(k) = grid%soil_levels(its,num_st_levels_input + 1 - k,jts)
            sm_levels_input(k) = grid%soil_levels(its,num_st_levels_input + 1 - k,jts)
            sw_levels_input(k) = grid%soil_levels(its,num_st_levels_input + 1 - k,jts)
         END DO

         !----------------------------------------------------------------
         ! Flip the input soil temperature/moisture/water 
         ! profiles upside down to make k=1 closest to the sfc
         !----------------------------------------------------------------
         DO j = jts , MIN(jde-1,jte)
            DO k = 1, num_st_levels_input
               DO i = its , MIN(ide-1,ite)
                  st_input(i,k,j) = grid%soilt(i,num_st_levels_input + 1 - k,j)
                  sm_input(i,k,j) = grid%soilm(i,num_st_levels_input + 1 - k,j)
                  !-------------------------------------------------------------------------
                  ! Initialize sw_input to 0. For 3D RUC soil moisture, there is no sw, 
                  ! but num_sw_levels_input is set to num_metgrid_soil_levels from the
                  ! namelist causing sw_input to be used in init_soil_#_real subroutines
                  !-------------------------------------------------------------------------
                  sw_input(i,k,j) = 0.0
               END DO
            END DO
         END DO

      END IF    ! flag_soil_levels == 1

      IF ( flag_soil_layers == 1 ) THEN
         level_above = 0
         DO k = 1, num_st_levels_input
            !-------------------------------------------------------------
            ! Calculate mid-point of each layer and set to st_levels_input
            ! Flip the input soil depths upside down to make k=1 closest to the sfc
            !-------------------------------------------------------------
            !st_levels_input(k) = (level_above + grid%soil_layers(its,num_st_levels_input + 1 - k,jts))/2
            !sm_levels_input(k) = (level_above + grid%soil_layers(its,num_st_levels_input + 1 - k,jts))/2
            !sw_levels_input(k) = (level_above + grid%soil_layers(its,num_st_levels_input + 1 - k,jts))/2
            !level_above = grid%soil_layers(its,num_st_levels_input + 1 - k,jts)
            !-------------------------------------------------------------
            ! If UM soil input, levels are cumulative (0-10cm, 0-25cm,
            ! etc.) so we simply take the midpoint of each level - GAC
            !-------------------------------------------------------------
            IF ( flag_um_soil == 1 ) THEN
              st_levels_input(k) = (grid%soil_layers(its,num_st_levels_input + 1 - k,jts))/2
              sm_levels_input(k) = (grid%soil_layers(its,num_st_levels_input + 1 - k,jts))/2
              sw_levels_input(k) = (grid%soil_layers(its,num_st_levels_input + 1 - k,jts))/2
            ELSE
              st_levels_input(k) = (level_above + grid%soil_layers(its,num_st_levels_input + 1 - k,jts))/2
              sm_levels_input(k) = (level_above + grid%soil_layers(its,num_st_levels_input + 1 - k,jts))/2
              sw_levels_input(k) = (level_above + grid%soil_layers(its,num_st_levels_input + 1 - k,jts))/2
              level_above = grid%soil_layers(its,num_st_levels_input + 1 - k,jts)
            END IF

         END DO

         !----------------------------------------------------------------
         ! Flip the input soil temperature/moisture/water 
         ! profiles upside down to make k=1 closest to the sfc
         !----------------------------------------------------------------
         DO j = jts , MIN(jde-1,jte)
            DO k = 1, num_st_levels_input
               DO i = its , MIN(ide-1,ite)
                  st_input(i,k+1,j) = grid%st(i,num_st_levels_input + 1 - k,j)
                  sm_input(i,k+1,j) = grid%sm(i,num_st_levels_input + 1 - k,j)
                  sw_input(i,k+1,j) = grid%sw(i,num_st_levels_input + 1 - k,j)
               END DO
            END DO
         END DO

         !----------------------------------------------------------------
         ! UM input is in kg/m2, convert to volumetric soil moisture here
         !----------------------------------------------------------------
         IF ( flag_um_soil == 1 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO k = 1, num_sm_levels_input
                  DO i = its, MIN(ide-1,ite)
                    sm_input(i,k+1,j)=100.*sm_input(i,k+1,j)/(2*sm_levels_input(k)*1000.)
                  END DO
               END DO
            END DO
         END IF

      END IF    ! flag_soil_layers == 1
#endif

      IF ( ( flag_soil_levels == 0 ) .AND. ( flag_soil_layers == 0 ) ) THEN        ! Legacy code

         flag_name(1:8) = 'ST000010'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_st000010 = itmp
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  st_input(i,num_st_levels_input + 1,j) = grid%st000010(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'ST010040'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_st010040 = itmp
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  st_input(i,num_st_levels_input + 1,j) = grid%st010040(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'ST040100'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_st040100 = itmp
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  st_input(i,num_st_levels_input + 1,j) = grid%st040100(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'ST100200'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_st100200 = itmp
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  st_input(i,num_st_levels_input + 1,j) = grid%st100200(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'ST010200'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_st010200 = itmp
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  st_input(i,num_st_levels_input + 1,j) = grid%st010200(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'ST000007'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_st000007 = itmp
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  st_input(i,num_st_levels_input + 1,j) = grid%st000007(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'ST007028'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_st007028 = itmp
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  st_input(i,num_st_levels_input + 1,j) = grid%st007028(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'ST028100'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_st028100 = itmp
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  st_input(i,num_st_levels_input + 1,j) = grid%st028100(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'ST100255'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_st100255 = itmp
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  st_input(i,num_st_levels_input + 1,j) = grid%st100255(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SOILT000'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_soilt000 = itmp
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int1(flag_name(6:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  st_input(i,num_st_levels_input ,j) = grid%soilt000(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SOILT005'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_soilt005 = itmp
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int1(flag_name(6:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  st_input(i,num_st_levels_input ,j) = grid%soilt005(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SOILT020'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_soilt020 = itmp
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int1(flag_name(6:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  st_input(i,num_st_levels_input ,j) = grid%soilt020(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SOILT040'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_soilt040 = itmp
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int1(flag_name(6:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  st_input(i,num_st_levels_input ,j) = grid%soilt040(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SOILT160'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_soilt160 = itmp
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int1(flag_name(6:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  st_input(i,num_st_levels_input ,j) = grid%soilt160(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SOILT300'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_soilt300 = itmp
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int1(flag_name(6:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  st_input(i,num_st_levels_input ,j) = grid%soilt300(i,j)
               END DO
            END DO
         END IF

         flag_name(1:8) = 'SM000010'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_sm000010 = itmp
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sm_input(i,num_sm_levels_input + 1,j) = grid%sm000010(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SM010040'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_sm010040 = itmp
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sm_input(i,num_sm_levels_input + 1,j) = grid%sm010040(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SM040100'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_sm040100 = itmp
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sm_input(i,num_sm_levels_input + 1,j) = grid%sm040100(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SM100200'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_sm100200 = itmp
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sm_input(i,num_sm_levels_input + 1,j) = grid%sm100200(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SM010200'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_sm010200 = itmp
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sm_input(i,num_sm_levels_input + 1,j) = grid%sm010200(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SM000007'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_sm000007 = itmp
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sm_input(i,num_sm_levels_input + 1,j) = grid%sm000007(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SM007028'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_sm007028 = itmp
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sm_input(i,num_sm_levels_input + 1,j) = grid%sm007028(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SM028100'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_sm028100 = itmp
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sm_input(i,num_sm_levels_input + 1,j) = grid%sm028100(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SM100255'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_sm100255 = itmp
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sm_input(i,num_sm_levels_input + 1,j) = grid%sm100255(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SOILM000'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_soilm000 = itmp
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int1(flag_name(6:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sm_input(i,num_sm_levels_input ,j) = grid%soilm000(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SOILM005'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_soilm005 = itmp
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int1(flag_name(6:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sm_input(i,num_sm_levels_input ,j) = grid%soilm005(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SOILM020'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_soilm020 = itmp
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int1(flag_name(6:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sm_input(i,num_sm_levels_input ,j) = grid%soilm020(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SOILM040'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_soilm040 = itmp
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int1(flag_name(6:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sm_input(i,num_sm_levels_input ,j) = grid%soilm040(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SOILM160'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_soilm160 = itmp
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int1(flag_name(6:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sm_input(i,num_sm_levels_input ,j) = grid%soilm160(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SOILM300'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_soilm300 = itmp
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int1(flag_name(6:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sm_input(i,num_sm_levels_input ,j) = grid%soilm300(i,j)
               END DO
            END DO
         END IF

         flag_name(1:8) = 'SW000010'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_sw000010 = itmp
            num_sw_levels_input = num_sw_levels_input + 1
            sw_levels_input(num_sw_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sw_input(i,num_sw_levels_input + 1,j) = grid%sw000010(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SW010040'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_sw010040 = itmp
            num_sw_levels_input = num_sw_levels_input + 1
            sw_levels_input(num_sw_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sw_input(i,num_sw_levels_input + 1,j) = grid%sw010040(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SW040100'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_sw040100 = itmp
            num_sw_levels_input = num_sw_levels_input + 1
            sw_levels_input(num_sw_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sw_input(i,num_sw_levels_input + 1,j) = grid%sw040100(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SW100200'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_sw100200 = itmp
            num_sw_levels_input = num_sw_levels_input + 1
            sw_levels_input(num_sw_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sw_input(i,num_sw_levels_input + 1,j) = grid%sw100200(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SW010200'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_sw010200 = itmp
            num_sw_levels_input = num_sw_levels_input + 1
            sw_levels_input(num_sw_levels_input) = char2int2(flag_name(3:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sw_input(i,num_sw_levels_input + 1,j) = grid%sw010200(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SOILW000'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_soilw000 = itmp
            num_sw_levels_input = num_sw_levels_input + 1
            sw_levels_input(num_sw_levels_input) = char2int1(flag_name(6:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sw_input(i,num_sw_levels_input ,j) = grid%soilw000(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SOILW005'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_soilw005 = itmp
            num_sw_levels_input = num_sw_levels_input + 1
            sw_levels_input(num_sw_levels_input) = char2int1(flag_name(6:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sw_input(i,num_sw_levels_input ,j) = grid%soilw005(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SOILW020'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_soilw020 = itmp
            num_sw_levels_input = num_sw_levels_input + 1
            sw_levels_input(num_sw_levels_input) = char2int1(flag_name(6:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sw_input(i,num_sw_levels_input ,j) = grid%soilw020(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SOILW040'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_soilw040 = itmp
            num_sw_levels_input = num_sw_levels_input + 1
            sw_levels_input(num_sw_levels_input) = char2int1(flag_name(6:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sw_input(i,num_sw_levels_input ,j) = grid%soilw040(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SOILW160'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_soilw160 = itmp
            num_sw_levels_input = num_sw_levels_input + 1
            sw_levels_input(num_sw_levels_input) = char2int1(flag_name(6:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sw_input(i,num_sw_levels_input ,j) = grid%soilw160(i,j)
               END DO
            END DO
         END IF
         flag_name(1:8) = 'SOILW300'
         CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            flag_soilw300 = itmp
            num_sw_levels_input = num_sw_levels_input + 1
            sw_levels_input(num_sw_levels_input) = char2int1(flag_name(6:8))
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sw_input(i,num_sw_levels_input ,j) = grid%soilw300(i,j)
               END DO
            END DO
         END IF

      END IF       ! End of legacy code for temperature and moisture

! The flags flag_st*, flag_sm*, flag_sw*, flag_soilt*, flag_soilm*, flag_soilw* are no longer used.
! If the new flags flag_soil_layers or flag_soil_levels are not set in met_em, and some legacy
! flags are, reset the new flags
    
      IF ( ( flag_soil_levels == 0 ) .AND. ( flag_soil_layers == 0 ) ) THEN
         IF ( flag_st000010 == 1 .OR. flag_st010040 == 1 .OR. flag_st040100 == 1 .OR. &
              flag_st100200 == 1 .OR. flag_st010200 == 1 .OR. &
              flag_sm000010 == 1 .OR. flag_sm010040 == 1 .OR. flag_sm040100 == 1 .OR. &
              flag_sm100200 == 1 .OR. flag_sm010200 == 1 .OR. &
              flag_sw000010 == 1 .OR. flag_sw010040 == 1 .OR. flag_sw040100 == 1 .OR. &
              flag_sw100200 == 1 .OR. flag_sw010200 == 1 .OR. &
              flag_st000007 == 1 .OR. flag_st007028 == 1 .OR. flag_st028100 == 1 .OR. &
              flag_st100255 == 1 .OR. &
              flag_sm000007 == 1 .OR. flag_sm007028 == 1 .OR. flag_sm028100 == 1 .OR. &
              flag_sm100255 == 1 ) THEN
            flag_soil_layers=1
         END IF
         IF ( flag_soilt000 == 1 .OR. flag_soilt005 == 1 .OR. flag_soilt020 == 1 .OR. &
              flag_soilt040 == 1 .OR. flag_soilt160 == 1 .OR. flag_soilt300 == 1 .OR. &
              flag_soilm000 == 1 .OR. flag_soilm005 == 1 .OR. flag_soilm020 == 1 .OR. &
              flag_soilm040 == 1 .OR. flag_soilm160 == 1 .OR. flag_soilm300 == 1 .OR. &
              flag_soilw000 == 1 .OR. flag_soilw005 == 1 .OR. flag_soilw020 == 1 .OR. &
              flag_soilw040 == 1 .OR. flag_soilw160 == 1 .OR. flag_soilw300 == 1 ) THEN
            flag_soil_levels=1
         END IF
      END IF

      write (message,'(A,I3)') 'flag_soil_layers at end of optional_lsm_levels is',flag_soil_layers
      CALL wrf_debug(1,message)
      write (message,'(A,I3)') 'flag_soil_levels at end of optional_lsm_levels is',flag_soil_levels
      CALL wrf_debug(1,message)

      write (message,'(A,10(i3,1x))') 'st_levels_input = ', (st_levels_input(k), k=1,num_st_levels_input)
      CALL wrf_debug(1,message)
      write (message,'(A,10(i3,1x))') 'sm_levels_input = ', (sm_levels_input(k), k=1,num_sm_levels_input)
      CALL wrf_debug(1,message)
      write (message,'(A,10(i3,1x))') 'sw_levels_input = ', (sw_levels_input(k), k=1,num_sw_levels_input)
      CALL wrf_debug(1,message)

      !  OK, let's do a quick sanity check.
 
      IF ( ( num_st_levels_input .GT. num_st_levels_alloc ) .OR. &
           ( num_sm_levels_input .GT. num_sm_levels_alloc ) .OR. &
           ( num_sw_levels_input .GT. num_sw_levels_alloc ) ) THEN
         print *,'pain and woe, the soil level allocation is too small'
         CALL wrf_error_fatal ( 'soil_levels_too_few' )
      END IF

   END SUBROUTINE optional_lsm_levels

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION char2int1( string3 ) RESULT ( int1 )
      CHARACTER (LEN=3) , INTENT(IN) :: string3
      INTEGER :: i1 , int1
      READ(string3,fmt='(I3)') i1
      int1 = i1
   END FUNCTION char2int1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION char2int2( string6 ) RESULT ( int1 )
      CHARACTER (LEN=6) , INTENT(IN) :: string6
      INTEGER :: i2 , i1 , int1
      READ(string6,fmt='(I3,I3)') i1,i2
      int1 = ( i2 + i1 ) / 2
   END FUNCTION char2int2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#endif
END MODULE module_optional_input
