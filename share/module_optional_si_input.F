MODULE module_optional_si_input

   INTEGER :: flag_qc       , flag_qr       , flag_qi       , flag_qs       , flag_qg

   INTEGER :: flag_st000010 , flag_st010040 , flag_st040100 , flag_st100200 , flag_st010200 , &
              flag_sm000010 , flag_sm010040 , flag_sm040100 , flag_sm100200 , flag_sm010200 , &
              flag_sw000010 , flag_sw010040 , flag_sw040100 , flag_sw100200 , flag_sw010200

   INTEGER :: flag_soilt000 , flag_soilt005 , flag_soilt020 , flag_soilt040 , flag_soilt160 , flag_soilt300 , &
              flag_soilm000 , flag_soilm005 , flag_soilm020 , flag_soilm040 , flag_soilm160 , flag_soilm300 , &
              flag_soilw000 , flag_soilw005 , flag_soilw020 , flag_soilw040 , flag_soilw160 , flag_soilw300

   INTEGER :: flag_sst      , flag_toposoil , flag_snowh

   INTEGER                  :: num_st_levels_input , num_sm_levels_input , num_sw_levels_input
   INTEGER                  :: num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc
   INTEGER , DIMENSION(100) ::     st_levels_input ,     sm_levels_input ,     sw_levels_input
   REAL , ALLOCATABLE , DIMENSION(:,:,:) :: st_input , sm_input , sw_input

   CHARACTER (LEN=8) , PRIVATE :: flag_name
 
   LOGICAL , PRIVATE :: already_been_here = .FALSE.

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE init_module_optional_si_input ( grid , config_flags ) 

      USE module_domain
      USE module_configure

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      TYPE (grid_config_rec_type) :: config_flags

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      !  Get the various indices, assume XZY ordering.

      ids = grid%sd31 ; ide = grid%ed31 ;
      kds = grid%sd32 ; kde = grid%ed32 ;
      jds = grid%sd33 ; jde = grid%ed33 ;

      ims = grid%sm31 ; ime = grid%em31 ;
      kms = grid%sm32 ; kme = grid%em32 ;
      jms = grid%sm33 ; jme = grid%em33 ;

      its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
      kts = grid%sp32 ; kte = grid%ep32 ;   ! note that tile is entire patch
      jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch

      IF ( .NOT. already_been_here ) THEN

         num_st_levels_alloc = config_flags%num_soil_layers * 2
         num_sm_levels_alloc = config_flags%num_soil_layers * 2
         num_sw_levels_alloc = config_flags%num_soil_layers * 2
   
         ALLOCATE ( st_input(ims:ime,num_st_levels_alloc,jms:jme) )
         ALLOCATE ( sm_input(ims:ime,num_sm_levels_alloc,jms:jme) )
         ALLOCATE ( sw_input(ims:ime,num_sw_levels_alloc,jms:jme) )

      END IF

      already_been_here = .TRUE.

   END SUBROUTINE init_module_optional_si_input

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_si_input ( grid , fid )

      USE module_configure      
      USE module_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      !  Get the various indices, assume XZY ordering.

      ids = grid%sd31 ; ide = grid%ed31 ;
      kds = grid%sd32 ; kde = grid%ed32 ;
      jds = grid%sd33 ; jde = grid%ed33 ;

      ims = grid%sm31 ; ime = grid%em31 ;
      kms = grid%sm32 ; kme = grid%em32 ;
      jms = grid%sm33 ; jme = grid%em33 ;

      its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
      kts = grid%sp32 ; kte = grid%ep32 ;   ! note that tile is entire patch
      jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch


      CALL optional_moist      ( grid , fid , &
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

      IF (  ( model_config_rec%sf_surface_physics(grid%id) .EQ. 1 ) .OR. &
            ( model_config_rec%sf_surface_physics(grid%id) .EQ. 2 ) .OR. &
            ( model_config_rec%sf_surface_physics(grid%id) .EQ. 3 ) .OR. &
            ( model_config_rec%sf_surface_physics(grid%id) .EQ. 99 ) ) THEN

         CALL optional_lsm        ( grid , fid , &
                                    ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    its, ite, jts, jte, kts, kte  )
   
         CALL optional_lsm_levels ( grid , fid , &
                                    ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    its, ite, jts, jte, kts, kte  )
      END IF
     
   END SUBROUTINE optional_si_input

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_moist ( grid , fid , &
                               ids, ide, jds, jde, kds, kde, &
                               ims, ime, jms, jme, kms, kme, &
                               its, ite, jts, jte, kts, kte  )

      USE module_io_wrf
      USE module_domain

USE module_configure
USE module_io_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr

      flag_qc       = 0
      flag_qr       = 0
      flag_qi       = 0
      flag_qs       = 0
      flag_qg       = 0

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
    
   END SUBROUTINE optional_moist

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_sst ( grid , fid , &
                             ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             its, ite, jts, jte, kts, kte  )

      USE module_io_wrf
      USE module_domain
USE module_configure
USE module_io_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr

      flag_sst      = 0 

      flag_name(1:8) = 'SST     '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_sst      = itmp
      END IF
    
   END SUBROUTINE optional_sst


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_snowh ( grid , fid , &
                               ids, ide, jds, jde, kds, kde, &
                               ims, ime, jms, jme, kms, kme, &
                               its, ite, jts, jte, kts, kte  )

      USE module_io_wrf
      USE module_domain
USE module_configure
USE module_io_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr

      flag_snowh    = 0 

      flag_name(1:8) = 'SNOWH   '
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_snowh    = itmp
      END IF
    
   END SUBROUTINE optional_snowh

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_lsm ( grid , fid , &
                             ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             its, ite, jts, jte, kts, kte  )

      USE module_io_wrf
      USE module_domain
USE module_configure
USE module_io_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr

      flag_toposoil = 0 

      flag_name(1:8) = 'TOPOSOIL'
      CALL wrf_get_dom_ti_integer ( fid, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
         flag_toposoil = itmp
      END IF
    
   END SUBROUTINE optional_lsm

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE optional_lsm_levels ( grid , fid , &
                                    ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    its, ite, jts, jte, kts, kte  )

      USE module_io_wrf
      USE module_domain
USE module_configure
USE module_io_domain

      IMPLICIT NONE 

      TYPE ( domain ) :: grid
      INTEGER , INTENT(IN) :: fid

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte

      INTEGER :: itmp , icnt , ierr , i , j
    
      !  Initialize the soil temp and moisture flags to "field not found".

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

      !  How many soil levels have we found?  Well, right now, none.

      num_st_levels_input = 0
      num_sm_levels_input = 0
      num_sw_levels_input = 0
      st_levels_input = -1
      sm_levels_input = -1
      sw_levels_input = -1

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

END MODULE module_optional_si_input
