!REAL:MODEL_LAYER:INITIALIZATION

!  This MODULE holds the routines which are used to perform various initializations
!  for the individual domains, specifically for the Eulerian, mass-based coordinate.

!-----------------------------------------------------------------------

MODULE module_initialize

   USE module_bc
   USE module_configure
   USE module_domain
   USE module_io_domain
   USE module_model_constants
   USE module_state_description
   USE module_timing
   USE module_soil_pre
#ifdef DM_PARALLEL
   USE module_dm
#endif


CONTAINS

!-------------------------------------------------------------------

   SUBROUTINE init_domain ( grid )

      IMPLICIT NONE

      !  Input space and data.  No gridded meteorological data has been stored, though.

!     TYPE (domain), POINTER :: grid 
      TYPE (domain)          :: grid 

      !  Local data.

      INTEGER :: dyn_opt 
      INTEGER :: idum1, idum2

#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

#include "deref_kludge.h"

      CALL nl_get_dyn_opt ( 1, dyn_opt )
      
      CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

      IF (      dyn_opt .eq. 1 &
           .or. dyn_opt .eq. 2 &
           .or. dyn_opt .eq. 3 &
                                          ) THEN
        CALL init_domain_rk( grid, &
!
#include <em_actual_args.inc>
!
      )

      ELSE
         WRITE(0,*)' init_domain: unknown or unimplemented dyn_opt = ',dyn_opt
         CALL wrf_error_fatal ( 'ERROR-dyn_opt-wrong-in-namelist' )
      ENDIF

   END SUBROUTINE init_domain

!-------------------------------------------------------------------

   SUBROUTINE init_domain_rk ( grid, &
!
# include <em_dummy_args.inc>
!
   )

      USE module_optional_si_input
      IMPLICIT NONE

      !  Input space and data.  No gridded meteorological data has been stored, though.

!     TYPE (domain), POINTER :: grid
      TYPE (domain)          :: grid

# include <em_dummy_decl.inc>

      TYPE (grid_config_rec_type)              :: config_flags

      !  Local domain indices and counters.

      INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat
      INTEGER :: loop , num_seaice_changes

      INTEGER                             ::                       &
                                     ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte, &
                                     ips, ipe, jps, jpe, kps, kpe, &
                                     i, j, k
      INTEGER :: ns

      !  Local data

      INTEGER :: error
      REAL    :: p_surf, p_level
      REAL    :: cof1, cof2
      REAL    :: qvf , qvf1 , qvf2 , pd_surf
      REAL    :: p00 , t00 , a
      REAL    :: hold_znw
      LOGICAL :: were_bad

      LOGICAL :: stretch_grid, dry_sounding, debug

!      INTEGER , PARAMETER :: nl_max = 1000
!      REAL , DIMENSION(nl_max) :: dn


integer::oops1,oops2

#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
      INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
      INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

#include "deref_kludge.h"

#define COPY_IN
#include <em_scalar_derefs.inc>
#ifdef DM_PARALLEL
#    include <em_data_calls.inc>
#endif

      SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_ZXY )
            kds = grid%sd31 ; kde = grid%ed31 ;
            ids = grid%sd32 ; ide = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            kms = grid%sm31 ; kme = grid%em31 ;
            ims = grid%sm32 ; ime = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            kts = grid%sp31 ; kte = grid%ep31 ;   ! note that tile is entire patch
            its = grid%sp32 ; ite = grid%ep32 ;   ! note that tile is entire patch
            jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch

         CASE ( DATA_ORDER_XYZ )
            ids = grid%sd31 ; ide = grid%ed31 ;
            jds = grid%sd32 ; jde = grid%ed32 ;
            kds = grid%sd33 ; kde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            jms = grid%sm32 ; jme = grid%em32 ;
            kms = grid%sm33 ; kme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
            jts = grid%sp32 ; jte = grid%ep32 ;   ! note that tile is entire patch
            kts = grid%sp33 ; kte = grid%ep33 ;   ! note that tile is entire patch

         CASE ( DATA_ORDER_XZY )
            ids = grid%sd31 ; ide = grid%ed31 ;
            kds = grid%sd32 ; kde = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            kms = grid%sm32 ; kme = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
            kts = grid%sp32 ; kte = grid%ep32 ;   ! note that tile is entire patch
            jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch

      END SELECT

      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

      !  Check to see if the boundary conditions are set properly in the namelist file.
      !  This checks for sufficiency and redundancy.

      CALL boundary_condition_check( config_flags, bdyzone, error, grid%id )

      !  Some sort of "this is the first time" initialization.  Who knows.

      step_number = 0
      grid%itimestep=0

      !  Pull in the info in the namelist to compare it to the input data.

      real_data_init_type = model_config_rec%real_data_init_type

      !  Save the TSK field for later use in the sea ice surface temperature
      !  for the Noah LSM scheme.

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
            tsk_save(i,j) = tsk(i,j)
         END DO
      END DO

      !  Take the data from the input file and store it in the variables that
      !  use the WRF naming and ordering conventions.

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
            IF ( snow(i,j) .GE. 10. ) then
               snowc(i,j) = 1.
            ELSE
               snowc(i,j) = 0.0
            END IF
         END DO
      END DO

      !  Set flag integers for presence of snowh and soilw fields

      ifndsnowh = flag_snowh
      IF (num_sw_levels_input .GE. 1) THEN
         ifndsoilw = 1
      ELSE
         ifndsoilw = 0
      END IF

      !  We require input data for the various LSM schemes.

      enough_data : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

         CASE (LSMSCHEME)
            IF ( num_st_levels_input .LT. 2 ) THEN
               CALL wrf_error_fatal ( 'Not enough soil temperature data for Noah LSM scheme.')
            END IF

         CASE (RUCLSMSCHEME)
            IF ( num_st_levels_input .LT. 2 ) THEN
               CALL wrf_error_fatal ( 'Not enough soil temperature data for RUC LSM scheme.')
            END IF

      END SELECT enough_data

      !  For sf_surface_physics = 1, we want to use close to a 30 cm value
      !  for the bottom level of the soil temps.

      fix_bottom_level_for_temp : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

         CASE (SLABSCHEME)
            IF      ( flag_st010040 .EQ. 1 ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     tmn(i,j) = st010040(i,j)
                  END DO
               END DO
            ELSE IF ( flag_st000010 .EQ. 1 ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     tmn(i,j) = st000010(i,j)
                  END DO
               END DO
            ELSE IF ( flag_soilt020 .EQ. 1 ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     tmn(i,j) = soilt020(i,j)
                  END DO
               END DO
            ELSE
               CALL wrf_error_fatal ( 'No 10-40 cm, 0-10 cm, or 20 cm soil temperature data for TMN')
            END IF

         CASE (LSMSCHEME)

         CASE (RUCLSMSCHEME)

      END SELECT fix_bottom_level_for_temp

      !  Adjustments for the seaice field PRIOR to the TSLB computations.  This is
      !  is for the 5-layer scheme.

      num_veg_cat      = SIZE ( landusef , DIM=2 )
      num_soil_top_cat = SIZE ( soilctop , DIM=2 )
      num_soil_bot_cat = SIZE ( soilcbot , DIM=2 )
      CALL nl_get_seaice_threshold ( grid%id , seaice_threshold ) 
      CALL nl_get_isice ( grid%id , isice )
      CALL nl_get_iswater ( grid%id , iswater )
      CALL adjust_for_seaice_pre ( xice , landmask , tsk , ivgtyp , vegcat , lu_index , &
                                   xland , landusef , isltyp , soilcat , soilctop , &
                                   soilcbot , tmn , &
                                   seaice_threshold , &
                                   num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                   iswater , isice , &
                                   model_config_rec%sf_surface_physics(grid%id) , & 
                                   ids , ide , jds , jde , kds , kde , & 
                                   ims , ime , jms , jme , kms , kme , & 
                                   its , ite , jts , jte , kts , kte ) 

      !  surface_input_source=1 => use data from static file (fractional category as input)
      !  surface_input_source=2 => use data from grib file (dominant category as input)
  
      IF ( config_flags%surface_input_source .EQ. 1 ) THEN
         vegcat (its,jts) = 0
         soilcat(its,jts) = 0
      END IF

      !  Generate the vegetation and soil category information from the fractional input
      !  data, or use the existing dominant category fields if they exist.

      IF ( ( soilcat(its,jts) .LT. 0.5 ) .AND. ( vegcat(its,jts) .LT. 0.5 ) ) THEN

         num_veg_cat      = SIZE ( landusef , DIM=2 )
         num_soil_top_cat = SIZE ( soilctop , DIM=2 )
         num_soil_bot_cat = SIZE ( soilcbot , DIM=2 )
   
         CALL process_percent_cat_new ( landmask , &               
                                    landusef , soilctop , soilcbot , &
                                    isltyp , ivgtyp , &
                                    num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                    ids , ide , jds , jde , kds , kde , &
                                    ims , ime , jms , jme , kms , kme , &
                                    its , ite , jts , jte , kts , kte , &
                                    model_config_rec%iswater(grid%id) )

         !  Make all the veg/soil parms the same so as not to confuse the developer.

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               vegcat(i,j)  = ivgtyp(i,j)
               soilcat(i,j) = isltyp(i,j)
            END DO
         END DO

      ELSE

         !  Do we have dominant soil and veg data from the input already?
   
         IF ( soilcat(its,jts) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  isltyp(i,j) = NINT( soilcat(i,j) )
               END DO
            END DO
         END IF
         IF ( vegcat(its,jts) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  ivgtyp(i,j) = NINT( vegcat(i,j) )
               END DO
            END DO
         END IF

      END IF
         
      !  Land use assignment.

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            lu_index(i,j) = ivgtyp(i,j)
            IF ( lu_index(i,j) .NE. model_config_rec%iswater(grid%id) ) THEN
               landmask(i,j) = 1
               xland(i,j)    = 1
            ELSE
               landmask(i,j) = 0
               xland(i,j)    = 2
            END IF
         END DO
      END DO

      !  Adjust the various soil temperature values depending on the difference in
      !  in elevation between the current model's elevation and the incoming data's
      !  orography.
         
      IF ( flag_toposoil .EQ. 1 ) THEN
         adjust_soil : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

            CASE ( SLABSCHEME , LSMSCHEME , RUCLSMSCHEME )
               CALL adjust_soil_temp_new ( tmn , model_config_rec%sf_surface_physics(grid%id) , &
                                           tsk , ht , toposoil , landmask , flag_toposoil , &
                                           st000010 , st010040 , st040100 , st100200 , st010200 , &
                                           flag_st000010 , flag_st010040 , flag_st040100 , flag_st100200 , flag_st010200 , &
                                           soilt000 , soilt005 , soilt020 , soilt040 , soilt160 , soilt300 , &
                                           flag_soilt000 , flag_soilt005 , flag_soilt020 , flag_soilt040 , &
                                           flag_soilt160 , flag_soilt300 , &
                                           ids , ide , jds , jde , kds , kde , &
                                           ims , ime , jms , jme , kms , kme , &
                                           its , ite , jts , jte , kts , kte )

         END SELECT adjust_soil
      END IF

      !  Fix tmn and tsk.

      fix_tsk_tmn : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

         CASE ( SLABSCHEME , LSMSCHEME , RUCLSMSCHEME )
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  IF ( ( landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) ) THEN
                     tmn(i,j) = sst(i,j)
                     tsk(i,j) = sst(i,j)
                  ELSE IF ( landmask(i,j) .LT. 0.5 ) THEN
                     tmn(i,j) = tsk(i,j)
                  END IF
               END DO
            END DO
      END SELECT fix_tsk_tmn
    
      !  Is the TSK reasonable?

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( tsk(i,j) .LT. 170 .or. tsk(i,j) .GT. 400. ) THEN
               print *,'error in the TSK'
               print *,'i,j=',i,j
               print *,'landmask=',landmask(i,j)
               print *,'tsk, sst, tmn=',tsk(i,j),sst(i,j),tmn(i,j)
               if(tmn(i,j).gt.170. .and. tmn(i,j).lt.400.)then
                  tsk(i,j)=tmn(i,j)
               else if(sst(i,j).gt.170. .and. sst(i,j).lt.400.)then
                  tsk(i,j)=sst(i,j)
               else
                  CALL wrf_error_fatal ( 'TSK unreasonable' )
               end if
            END IF
         END DO
      END DO

      !  Is the TMN reasonable?

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( ( tmn(i,j) .LT. 170. ) .OR. ( tmn(i,j) .GT. 400. ) ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN
                  print *,'error in the TMN'
                  print *,'i,j=',i,j
                  print *,'landmask=',landmask(i,j)
                  print *,'tsk, sst, tmn=',tsk(i,j),sst(i,j),tmn(i,j)
               if(tsk(i,j).gt.170. .and. tsk(i,j).lt.400.)then
                  tmn(i,j)=tsk(i,j)
               else if(sst(i,j).gt.170. .and. sst(i,j).lt.400.)then
                  tmn(i,j)=sst(i,j)
               else
                  CALL wrf_error_fatal ( 'TMN unreasonable' )
               endif
            END IF
         END DO
      END DO
   
      interpolate_soil_tmw : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

         CASE ( SLABSCHEME , LSMSCHEME , RUCLSMSCHEME )
            CALL process_soil_real ( tsk , tmn , &
                                  landmask , sst , &
                                  st_input , sm_input , sw_input , st_levels_input , sm_levels_input , sw_levels_input , &
                                  zs , dzs , tslb , smois , sh2o , &
                                  flag_sst , flag_soilt000, flag_soilm000, &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  its , ite , jts , jte , kts , kte , &
                                  model_config_rec%sf_surface_physics(grid%id) , &
                                  model_config_rec%num_soil_layers , &
                                  model_config_rec%real_data_init_type , &
                                  num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                  num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc )

      END SELECT interpolate_soil_tmw

      !  Is the TSLB reasonable?

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( ( tslb(i,1,j) .LT. 170. ) .OR. ( tslb(i,1,j) .GT. 400. ) ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN
                  print *,'error in the TSLB'
                  print *,'i,j=',i,j
                  print *,'landmask=',landmask(i,j)
                  print *,'tsk, sst, tmn=',tsk(i,j),sst(i,j),tmn(i,j)
                  print *,'tslb = ',tslb(i,:,j)
                  print *,'old smois = ',smois(i,:,j)
                  smois(i,1,j) = 0.3
                  smois(i,2,j) = 0.3
                  smois(i,3,j) = 0.3
                  smois(i,4,j) = 0.3

                  IF ( (tsk(i,j).GT.170. .AND. tsk(i,j).LT.400.) .AND. &
                       (tmn(i,j).GT.170. .AND. tmn(i,j).LT.400.) ) THEN
                     fake_soil_temp : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )
                        CASE ( SLABSCHEME )
                           DO ns = 1 , model_config_rec%num_soil_layers
                              tslb(i,ns,j) = ( tsk(i,j)*(3.0 - zs(ns)) + tmn(i,j)*(0.0 - zs(ns)) ) /(3.0 - 0.0)
                           END DO
                        CASE ( LSMSCHEME , RUCLSMSCHEME )
                           CALL wrf_error_fatal ( 'Assigning constant soil moisture, bad idea')
                           DO ns = 1 , model_config_rec%num_soil_layers
                              tslb(i,ns,j) = ( tsk(i,j)*(3.0 - zs(ns)) + tmn(i,j)*(0.0 - zs(ns)) ) /(3.0 - 0.0)
                           END DO
                     END SELECT fake_soil_temp
                  else if(tsk(i,j).gt.170. .and. tsk(i,j).lt.400.)then
                     CALL wrf_error_fatal ( 'TSLB unreasonable 1' )
                     DO ns = 1 , model_config_rec%num_soil_layers
                        tslb(i,ns,j)=tsk(i,j)
                     END DO
                  else if(sst(i,j).gt.170. .and. sst(i,j).lt.400.)then
                     CALL wrf_error_fatal ( 'TSLB unreasonable 2' )
                     DO ns = 1 , model_config_rec%num_soil_layers
                        tslb(i,ns,j)=sst(i,j)
                     END DO
                  else if(tmn(i,j).gt.170. .and. tmn(i,j).lt.400.)then
                     CALL wrf_error_fatal ( 'TSLB unreasonable 3' )
                     DO ns = 1 , model_config_rec%num_soil_layers
                        tslb(i,ns,j)=tmn(i,j)
                     END DO
                  else
                     CALL wrf_error_fatal ( 'TSLB unreasonable 4' )
                  endif
            END IF
         END DO
      END DO

      !  Adjustments for the seaice field AFTER the TSLB computations.  This is
      !  is for the Noah LSM scheme.

      num_veg_cat      = SIZE ( landusef , DIM=2 )
      num_soil_top_cat = SIZE ( soilctop , DIM=2 )
      num_soil_bot_cat = SIZE ( soilcbot , DIM=2 )
      CALL nl_get_seaice_threshold ( grid%id , seaice_threshold ) 
      CALL nl_get_isice ( grid%id , isice )
      CALL nl_get_iswater ( grid%id , iswater )
      CALL adjust_for_seaice_post ( xice , landmask , tsk , tsk_save , ivgtyp , vegcat , lu_index , &
                                    xland , landusef , isltyp , soilcat , soilctop , &
                                    soilcbot , tmn , vegfra , &
                                    tslb , smois , sh2o , &
                                    seaice_threshold , &
                                    num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                    model_config_rec%num_soil_layers , &
                                    iswater , isice , &
                                    model_config_rec%sf_surface_physics(grid%id) , & 
                                    ids , ide , jds , jde , kds , kde , & 
                                    ims , ime , jms , jme , kms , kme , & 
                                    its , ite , jts , jte , kts , kte ) 

      !  Let us make sure (again) that the landmask and the veg/soil categories match.

oops1=0
oops2=0
      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( ( landmask(i,j) .LT. 0.5 ) .AND. ( ivgtyp(i,j) .NE. config_flags%iswater .OR. isltyp(i,j) .NE. 14 ) ) .OR. &
                 ( ( landmask(i,j) .GT. 0.5 ) .AND. ( ivgtyp(i,j) .EQ. config_flags%iswater .OR. isltyp(i,j) .EQ. 14 ) ) ) THEN
               IF ( tslb(i,1,j) .GT. 1. ) THEN
oops1=oops1+1
                  ivgtyp(i,j) = 5
                  isltyp(i,j) = 8
                  landmask(i,j) = 1
                  xland(i,j) = 1
               ELSE IF ( sst(i,j) .GT. 1. ) THEN
oops2=oops2+1
                  ivgtyp(i,j) = config_flags%iswater
                  isltyp(i,j) = 14
                  landmask(i,j) = 0
                  xland(i,j) = 2
               ELSE
                  print *,'the landmask and soil/veg cats do not match'
                  print *,'i,j=',i,j
                  print *,'landmask=',landmask(i,j)
                  print *,'ivgtyp=',ivgtyp(i,j)
                  print *,'isltyp=',isltyp(i,j)
                  print *,'iswater=', config_flags%iswater
                  print *,'tslb=',tslb(i,:,j)
                  print *,'sst=',sst(i,j)
                  CALL wrf_error_fatal ( 'mismatch_landmask_ivgtyp' )
               END IF
            END IF
         END DO
      END DO
if (oops1.gt.0) then
print *,'points artificially set to land : ',oops1
endif
if(oops2.gt.0) then
print *,'points artificially set to water: ',oops2
endif
! fill sst array with tsk if missing in real input (needed for time-varying sst in wrf)
      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
           IF ( flag_sst .NE. 1 ) THEN
             sst(i,j) = tsk(i,j)
           ENDIF
         END DO
      END DO

      !  From the full level data, we can get the half levels, reciprocals, and layer
      !  thicknesses.  These are all defined at half level locations, so one less level.
      !  We allow the vertical coordinate to *accidently* come in upside down.  We want
      !  the first full level to be the ground surface.

      !  Check whether ZNW (full level) data are truly full levels. If not, we need to adjust them
      !  to be full levels.
      !  in this test, we check if znw(1) is neither 0 nor 1 (within a tolerance of 10**-5)

      were_bad = .false.
      IF ( ( (znw(1).LT.(1-1.E-5) ) .OR. ( znw(1).GT.(1+1.E-5) ) ).AND. &
           ( (znw(1).LT.(0-1.E-5) ) .OR. ( znw(1).GT.(0+1.E-5) ) ) ) THEN
         were_bad = .true.
         print *,'Your ZNW input values are probably half-levels. '
         print *,ZNW
         print *,'WRF expects ZNW values to be full levels. '
         print *,'Adjusting now to full levels...'
         !  We want to ignore the first value if it's negative
         IF (znw(1).LT.0) THEN
            znw(1)=0
         END IF
         DO k=2,kde
            znw(k)=2*znw(k)-znw(k-1)
         END DO
      END IF

      !  Let's check our changes

      IF ( ( ( znw(1) .LT. (1-1.E-5) ) .OR. ( znw(1) .GT. (1+1.E-5) ) ).AND. &
           ( ( znw(1) .LT. (0-1.E-5) ) .OR. ( znw(1) .GT. (0+1.E-5) ) ) ) THEN
         print *,'The input ZNW height values were half-levels or erroneous. '
         print *,'Attempts to treat the values as half-levels and change them '
         print *,'to valid full levels failed.'
         CALL wrf_error_fatal("bad ZNW values from input files")
      ELSE IF ( were_bad ) THEN
         print *,'...adjusted. ZNW array now contains full eta level values. '
      ENDIF


      IF ( znw(1) .LT. znw(kde) ) THEN
         DO k=1, kde/2
            hold_znw = znw(k)
            znw(k)=znw(kde+1-k)
            znw(kde+1-k)=hold_znw
         END DO
      END IF

      DO k=1, kde-1
         dnw(k) = znw(k+1) - znw(k)
         rdnw(k) = 1./dnw(k)
         znu(k) = 0.5*(znw(k+1)+znw(k))
      END DO

      !  Now the same sort of computations with the half eta levels, even ANOTHER
      !  level less than the one above.

      DO k=2, kde-1
         dn(k) = 0.5*(dnw(k)+dnw(k-1))
         rdn(k) = 1./dn(k)
         fnp(k) = .5* dnw(k  )/dn(k)
         fnm(k) = .5* dnw(k-1)/dn(k)
      END DO

      !  Scads of vertical coefficients.

      cof1 = (2.*dn(2)+dn(3))/(dn(2)+dn(3))*dnw(1)/dn(2) 
      cof2 =     dn(2)        /(dn(2)+dn(3))*dnw(1)/dn(3) 

      cf1  = fnp(2) + cof1
      cf2  = fnm(2) - cof1 - cof2
      cf3  = cof2       

      cfn  = (.5*dnw(kde-1)+dn(kde-1))/dn(kde-1)
      cfn1 = -.5*dnw(kde-1)/dn(kde-1)

      !  Inverse grid distances.

      rdx = 1./dx
      rdy = 1./dy

      !  Some of the many weird geopotential initializations that we'll see today: ph0 is total, 
      !  and ph_2 is a perturbation from the base state geopotential.  We set the base geopotential 
      !  at the lowest level to terrain elevation * gravity.

      DO j=jts,jte
         DO i=its,ite
            ph0(i,1,j) = ht(i,j) * g
            ph_2(i,1,j) = 0.
         END DO
      END DO

      !  To define the base state, we call a USER MODIFIED routine to set the three
      !  necessary constants:  p00 (sea level pressure, Pa), t00 (sea level temperature, K), 
      !  and A (temperature difference, from 1000 mb to 300 mb, K).

      CALL const_module_initialize ( p00 , t00 , a ) 

      !  Base state potential temperature and inverse density (alpha = 1/rho) from
      !  the half eta levels and the base-profile surface pressure.  Compute 1/rho 
      !  from equation of state.  The potential temperature is a perturbation from t0.

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

            !  Base state pressure is a function of eta level and terrain, only, plus
            !  the hand full of constants: p00 (sea level pressure, Pa), t00 (sea level
            !  temperature, K), and A (temperature difference, from 1000 mb to 300 mb, K).

            p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*ht(i,j)/a/r_d ) **0.5 ) 


            DO k = 1, kte-1
               pb(i,k,j) = znu(k)*(p_surf - p_top) + p_top
               t_init(i,k,j) = (t00 + A*LOG(pb(i,k,j)/p00))*(p00/pb(i,k,j))**(r_d/cp) - t0
               alb(i,k,j) = (r_d/p1000mb)*(t_init(i,k,j)+t0)*(pb(i,k,j)/p1000mb)**cvpm
            END DO
       
            !  Base state mu is defined as base state surface pressure minus p_top

            mub(i,j) = p_surf - p_top
       
            !  Dry surface pressure is defined as the following (this mu is from the input file
            !  computed from the dry pressure).  Here the dry pressure is just reconstituted.

            pd_surf = mu0(i,j) + p_top
       
            !  Compute the perturbation mass and the full mass from the base surface pressure
            !  (a function of elevation) and the dry surface pressure.
       
            mu_2(i,j) = pd_surf-p_surf

            !  Integrate base geopotential, starting at terrain elevation.  This assures that 
            !  the base state is in exact hydrostatic balance with respect to the model equations.
            !  This field is on full levels.

            phb(i,1,j) = ht(i,j) * g
            DO k  = 2,kte
               phb(i,k,j) = phb(i,k-1,j) - dnw(k-1)*mub(i,j)*alb(i,k-1,j)
            END DO
         END DO
      END DO

      !  Fill in the outer rows and columns to allow us to be sloppy.

      IF ( ite .EQ. ide ) THEN
      i = ide
      DO j = jts, MIN(jde-1,jte)
         mub(i,j) = mub(i-1,j)
         mu_2(i,j) = mu_2(i-1,j)
         DO k = 1, kte-1
            pb(i,k,j) = pb(i-1,k,j)
            t_init(i,k,j) = t_init(i-1,k,j)
            alb(i,k,j) = alb(i-1,k,j)
         END DO
         DO k = 1, kte
            phb(i,k,j) = phb(i-1,k,j)
         END DO
      END DO
      END IF

      IF ( jte .EQ. jde ) THEN
      j = jde
      DO i = its, ite
         mub(i,j) = mub(i,j-1)
         mu_2(i,j) = mu_2(i,j-1)
         DO k = 1, kte-1
            pb(i,k,j) = pb(i,k,j-1)
            t_init(i,k,j) = t_init(i,k,j-1)
            alb(i,k,j) = alb(i,k,j-1)
         END DO
         DO k = 1, kte
            phb(i,k,j) = phb(i,k,j-1)
         END DO
      END DO
      END IF

      DO j = jts, min(jde-1,jte)
         DO i = its, min(ide-1,ite)

            !  Assign the potential temperature (perturbation from t0) and qv on all the mass
            !  point locations.

            DO k =  1 , kde-1
               t_2(i,k,j)          = t_2(i,k,j) - t0
            END DO
      
            !  Integrate the hydrostatic equation (from the RHS of the bigstep vertical momentum 
            !  equation) down from the top to get the pressure perturbation.  First get the pressure
            !  perturbation, moisture, and inverse density (total and perturbation) at the top-most level.
      
            k = kte-1
      
            qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k,j,P_QV))
            qvf2 = 1./(1.+qvf1)
            qvf1 = qvf1*qvf2
      
            p(i,k,j) = - 0.5*(mu_2(i,j)+qvf1*mub(i,j))/rdnw(k)/qvf2
            qvf = 1. + rvovrd*moist(i,k,j,P_QV)
            alt(i,k,j) = (r_d/p1000mb)*(t_2(i,k,j)+t0)*qvf*(((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm)
            al(i,k,j) = alt(i,k,j) - alb(i,k,j)
      
            !  Now, integrate down the column to compute the pressure perturbation, and diagnose the two
            !  inverse density fields (total and perturbation).
      
            DO k=kte-2,1,-1
               qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k+1,j,P_QV))
               qvf2 = 1./(1.+qvf1)
               qvf1 = qvf1*qvf2
               p(i,k,j) = p(i,k+1,j) - (mu_2(i,j) + qvf1*mub(i,j))/qvf2/rdn(k+1)
               qvf = 1. + rvovrd*moist(i,k,j,P_QV)
               alt(i,k,j) = (r_d/p1000mb)*(t_2(i,k,j)+t0)*qvf* &
                           (((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm)
               al(i,k,j) = alt(i,k,j) - alb(i,k,j)
            END DO
      
            !  This is the hydrostatic equation used in the model after the small timesteps.  In 
            !  the model, al (inverse density) is computed from the geopotential.
      
            DO k  = 2,kte
               ph_2(i,k,j) = ph_2(i,k-1,j) - &
                             dnw(k-1) * ( (mub(i,j)+mu_2(i,j))*al(i,k-1,j) + mu_2(i,j)*alb(i,k-1,j) )
               ph0(i,k,j) = ph_2(i,k,j) + phb(i,k,j)
            END DO
       
         END DO
      END DO

      DO j = jts, min(jde-1,jte)
         DO i = its, min(ide,ite)
            u10(i,j)=u_2(i,1,j)
         END DO
      END DO

      DO j = jts, min(jde,jte)
         DO i = its, min(ide-1,ite)
            v10(i,j)=v_2(i,1,j)
         END DO
      END DO

      DO j = jts, min(jde-1,jte)
         DO i = its, min(ide-1,ite)
            p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*ht(i,j)/a/r_d ) **0.5 ) 
            psfc(i,j)=p_surf + p(i,1,j)
            q2(i,j)=moist(i,1,j,P_QV)
            th2(i,j)=t_2(i,1,j)+300.
            t2(i,j)=th2(i,j)*(((p(i,1,j)+pb(i,1,j))/p00)**(r_d/cp))
         END DO
      END DO

      ips = its ; ipe = ite ; jps = jts ; jpe = jte ; kps = kts ; kpe = kte
#ifdef DM_PARALLEL
#   include "HALO_EM_INIT_1.inc"
#   include "HALO_EM_INIT_2.inc"
#   include "HALO_EM_INIT_3.inc"
#   include "HALO_EM_INIT_4.inc"
#   include "HALO_EM_INIT_5.inc"
#endif

#define COPY_OUT
#include <em_scalar_derefs.inc>
      RETURN

   END SUBROUTINE init_domain_rk

!---------------------------------------------------------------------

   SUBROUTINE const_module_initialize ( p00 , t00 , a ) 
      USE module_configure
      IMPLICIT NONE
      !  For the real-data-cases only.
      REAL , INTENT(OUT) :: p00 , t00 , a
      CALL nl_get_base_pres  ( 1 , p00 )
      CALL nl_get_base_temp  ( 1 , t00 )
      CALL nl_get_base_lapse ( 1 , a   )
   END SUBROUTINE const_module_initialize

!-------------------------------------------------------------------

   SUBROUTINE rebalance_driver ( grid ) 

      IMPLICIT NONE

      TYPE (domain)          :: grid 

#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
      INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
      INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

#include "deref_kludge.h"

      CALL rebalance( grid, &
!
#include <em_actual_args.inc>
!
      )

   END SUBROUTINE rebalance_driver

!---------------------------------------------------------------------

   SUBROUTINE rebalance ( grid , &
!
#include <em_dummy_args.inc>
!
                        )
      IMPLICIT NONE

      TYPE (domain)          :: grid

#include <em_dummy_decl.inc>

      TYPE (grid_config_rec_type)              :: config_flags

      REAL :: p_surf ,  pd_surf, p_surf_int , pb_int , ht_hold
      REAL :: qvf , qvf1 , qvf2
      REAL :: p00 , t00 , a
      REAL , DIMENSION(:,:,:) , ALLOCATABLE :: t_init_int

      !  Local domain indices and counters.

      INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat

      INTEGER                             ::                       &
                                     ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte, &
                                     ips, ipe, jps, jpe, kps, kpe, &
                                     i, j, k

#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
      INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
      INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

#include "deref_kludge.h"

#define COPY_IN
#include <em_scalar_derefs.inc>
#ifdef DM_PARALLEL
#    include <em_data_calls.inc>
#endif

      SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_ZXY )
            kds = grid%sd31 ; kde = grid%ed31 ;
            ids = grid%sd32 ; ide = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            kms = grid%sm31 ; kme = grid%em31 ;
            ims = grid%sm32 ; ime = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            kts = grid%sp31 ; kte = grid%ep31 ;   ! note that tile is entire patch
            its = grid%sp32 ; ite = grid%ep32 ;   ! note that tile is entire patch
            jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch

         CASE ( DATA_ORDER_XYZ )
            ids = grid%sd31 ; ide = grid%ed31 ;
            jds = grid%sd32 ; jde = grid%ed32 ;
            kds = grid%sd33 ; kde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            jms = grid%sm32 ; jme = grid%em32 ;
            kms = grid%sm33 ; kme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
            jts = grid%sp32 ; jte = grid%ep32 ;   ! note that tile is entire patch
            kts = grid%sp33 ; kte = grid%ep33 ;   ! note that tile is entire patch

         CASE ( DATA_ORDER_XZY )
            ids = grid%sd31 ; ide = grid%ed31 ;
            kds = grid%sd32 ; kde = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            kms = grid%sm32 ; kme = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
            kts = grid%sp32 ; kte = grid%ep32 ;   ! note that tile is entire patch
            jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch

      END SELECT

      ALLOCATE ( t_init_int(ims:ime,kms:kme,jms:jme) )

      !  Some of the many weird geopotential initializations that we'll see today: ph0 is total, 
      !  and ph_2 is a perturbation from the base state geopotential.  We set the base geopotential 
      !  at the lowest level to terrain elevation * gravity.

      DO j=jts,jte
         DO i=its,ite
            ph0(i,1,j) = ht_fine(i,j) * g
            ph_2(i,1,j) = 0.
         END DO
      END DO

      !  To define the base state, we call a USER MODIFIED routine to set the three
      !  necessary constants:  p00 (sea level pressure, Pa), t00 (sea level temperature, K), 
      !  and A (temperature difference, from 1000 mb to 300 mb, K).

      CALL const_module_initialize ( p00 , t00 , a ) 

      !  Base state potential temperature and inverse density (alpha = 1/rho) from
      !  the half eta levels and the base-profile surface pressure.  Compute 1/rho 
      !  from equation of state.  The potential temperature is a perturbation from t0.

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

            !  Base state pressure is a function of eta level and terrain, only, plus
            !  the hand full of constants: p00 (sea level pressure, Pa), t00 (sea level
            !  temperature, K), and A (temperature difference, from 1000 mb to 300 mb, K).
            !  The fine grid terrain is ht_fine, the interpolated is ht.

            p_surf     = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*ht_fine(i,j)/a/r_d ) **0.5 ) 
            p_surf_int = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*ht(i,j)     /a/r_d ) **0.5 ) 

            DO k = 1, kte-1
               pb(i,k,j) = znu(k)*(p_surf     - p_top) + p_top
               pb_int    = znu(k)*(p_surf_int - p_top) + p_top
               t_init(i,k,j)    = (t00 + A*LOG(pb(i,k,j)/p00))*(p00/pb(i,k,j))**(r_d/cp) - t0
               t_init_int(i,k,j)= (t00 + A*LOG(pb_int   /p00))*(p00/pb_int   )**(r_d/cp) - t0
               alb(i,k,j) = (r_d/p1000mb)*(t_init(i,k,j)+t0)*(pb(i,k,j)/p1000mb)**cvpm
            END DO
       
            !  Base state mu is defined as base state surface pressure minus p_top

            mub(i,j) = p_surf - p_top
       
            !  Dry surface pressure is defined as the following (this mu is from the input file
            !  computed from the dry pressure).  Here the dry pressure is just reconstituted.

            pd_surf = ( mub(i,j) + mu_2(i,j) ) + p_top
       
            !  Integrate base geopotential, starting at terrain elevation.  This assures that 
            !  the base state is in exact hydrostatic balance with respect to the model equations.
            !  This field is on full levels.

            phb(i,1,j) = ht_fine(i,j) * g
            DO k  = 2,kte
               phb(i,k,j) = phb(i,k-1,j) - dnw(k-1)*mub(i,j)*alb(i,k-1,j)
            END DO
         END DO
      END DO

      !  Replace interpolated terrain with fine grid values.

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
            ht(i,j) = ht_fine(i,j)
         END DO
      END DO

      !  Perturbation fields.

      DO j = jts, min(jde-1,jte)
         DO i = its, min(ide-1,ite)

            !  The potential temperature is THETAnest = THETAinterp + ( TBARnest - TBARinterp)

            DO k =  1 , kde-1
               t_2(i,k,j) = t_2(i,k,j) + ( t_init(i,k,j) - t_init_int(i,k,j) ) 
            END DO
      
            !  Integrate the hydrostatic equation (from the RHS of the bigstep vertical momentum 
            !  equation) down from the top to get the pressure perturbation.  First get the pressure
            !  perturbation, moisture, and inverse density (total and perturbation) at the top-most level.
      
            k = kte-1
      
            qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k,j,P_QV))
            qvf2 = 1./(1.+qvf1)
            qvf1 = qvf1*qvf2
      
            p(i,k,j) = - 0.5*(mu_2(i,j)+qvf1*mub(i,j))/rdnw(k)/qvf2
            qvf = 1. + rvovrd*moist(i,k,j,P_QV)
            alt(i,k,j) = (r_d/p1000mb)*(t_2(i,k,j)+t0)*qvf*(((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm)
            al(i,k,j) = alt(i,k,j) - alb(i,k,j)
      
            !  Now, integrate down the column to compute the pressure perturbation, and diagnose the two
            !  inverse density fields (total and perturbation).
      
            DO k=kte-2,1,-1
               qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k+1,j,P_QV))
               qvf2 = 1./(1.+qvf1)
               qvf1 = qvf1*qvf2
               p(i,k,j) = p(i,k+1,j) - (mu_2(i,j) + qvf1*mub(i,j))/qvf2/rdn(k+1)
               qvf = 1. + rvovrd*moist(i,k,j,P_QV)
               alt(i,k,j) = (r_d/p1000mb)*(t_2(i,k,j)+t0)*qvf* &
                           (((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm)
               al(i,k,j) = alt(i,k,j) - alb(i,k,j)
            END DO
      
            !  This is the hydrostatic equation used in the model after the small timesteps.  In 
            !  the model, al (inverse density) is computed from the geopotential.
      
            DO k  = 2,kte
               ph_2(i,k,j) = ph_2(i,k-1,j) - &
                             dnw(k-1) * ( (mub(i,j)+mu_2(i,j))*al(i,k-1,j) + mu_2(i,j)*alb(i,k-1,j) )
               ph0(i,k,j) = ph_2(i,k,j) + phb(i,k,j)
            END DO
       
         END DO
      END DO

      DEALLOCATE ( t_init_int ) 

      ips = its ; ipe = ite ; jps = jts ; jpe = jte ; kps = kts ; kpe = kte
#ifdef DM_PARALLEL
#   include "HALO_EM_INIT_1.inc"
#   include "HALO_EM_INIT_2.inc"
#   include "HALO_EM_INIT_3.inc"
#   include "HALO_EM_INIT_4.inc"
#   include "HALO_EM_INIT_5.inc"
#endif
   END SUBROUTINE rebalance

!---------------------------------------------------------------------

   SUBROUTINE init_module_initialize
   END SUBROUTINE init_module_initialize

!---------------------------------------------------------------------

END MODULE module_initialize
