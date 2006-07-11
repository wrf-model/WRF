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
   USE module_date_time
#ifdef DM_PARALLEL
   USE module_dm
#endif

   REAL , SAVE :: p_top_save
   INTEGER :: internal_time_loop

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
      INTEGER IICOUNT

      INTEGER :: num_vert_levels
      REAL , DIMENSION(max_eta) :: eta_levels

!      INTEGER , PARAMETER :: nl_max = 1000
!      REAL , DIMENSION(nl_max) :: dn

integer::oops1,oops2

      INTEGER :: interp_type , lagrange_order
      LOGICAL :: lowest_lev_from_sfc

!-- Carsel and Parrish [1988]
        REAL , DIMENSION(100) :: lqmi

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

      grid%real_data_init_type = model_config_rec%real_data_init_type
   
      !  To define the base state, we call a USER MODIFIED routine to set the three
      !  necessary constants:  p00 (sea level pressure, Pa), t00 (sea level temperature, K), 
      !  and A (temperature difference, from 1000 mb to 300 mb, K).
   
      CALL const_module_initialize ( p00 , t00 , a ) 

#if 0
!KLUDGE, this is for testing only
if ( flag_metgrid .eq. 1 ) then
read (22) ht_gc
else
write (22) ht
endif
#endif
   
      !  Is there any vertical interpolation to do?  The "old" data comes in on the correct
      !  vertical locations already.

      IF ( flag_metgrid .EQ. 1 ) THEN  !   <----- START OF VERTICAL INTERPOLATION PART ---->

!KLUDGE, they should be named the same
         !  Variables that are named differently between old and new SI.

         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)
              landmask(i,j) = landmask_gc(i,j)
              tsk(i,j) = tsk_gc(i,j)
              tmn(i,j) = tmn_gc(i,j)
              xlat(i,j) = xlat_gc(i,j)
              xlong(i,j) = xlong_gc(i,j)
              ht(i,j) = ht_gc(i,j)
           END DO
         END DO
   
         !  The number of vertical levels in the input data.  There is no staggering for
         !  different variables.
   
         num_vert_levels = grid%num_vert_levels

         !  Compute the top pressure, p_top.  For isobaric data, this is just the
         !  top level.  For the generalized vertical coordinate data, we find the
         !  max pressure on the top level.  We have to be careful of two things:
         !  1) the value has to be communicated, 2) the value can not increase
         !  at subsequent times from the initial value.

         IF ( internal_time_loop .EQ. 1 ) THEN
            CALL find_p_top ( p_gc , p_top , &
                              ids , ide , jds , jde , 1   , num_vert_levels , &
                              ims , ime , jms , jme , 1   , num_vert_levels , &
                              its , ite , jts , jte , 1   , num_vert_levels )

#ifdef DM_PARALLEL
            p_top = wrf_dm_max_real ( p_top )
#endif
            p_top_save = p_top
         ELSE
            CALL find_p_top ( p_gc , p_top , &
                              ids , ide , jds , jde , 1   , num_vert_levels , &
                              ims , ime , jms , jme , 1   , num_vert_levels , &
                              its , ite , jts , jte , 1   , num_vert_levels )

#ifdef DM_PARALLEL
            p_top = wrf_dm_max_real ( p_top )
#endif
            IF ( p_top .GT. p_top_save ) THEN
               CALL wrf_error_fatal ( 'p_top > previous value' )
            END IF
            p_top = p_top_save
         ENDIF
   
         !  Get the monthly values interpolated to the current date for the traditional monthly
         !  fields of green-ness fraction and background albedo.
   
         CALL monthly_interp_to_date ( greenfrac , current_date , vegfra , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )
   
         CALL monthly_interp_to_date ( albedo12m , current_date , albbck , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )
   
         !  Get the min/max of each i,j for the monthly green-ness fraction.
   
         CALL monthly_min_max ( greenfrac , shdmin , shdmax , &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte )

         !  The model expects the green-ness values in percent, not fraction.

         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)
              vegfra(i,j) = vegfra(i,j) * 100.
              shdmax(i,j) = shdmax(i,j) * 100.
              shdmin(i,j) = shdmin(i,j) * 100.
           END DO
         END DO

         !  The model expects the albedo fields as a fraction, not a percent.  Set the
         !  water values to 8%.

         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)
              albbck(i,j) = albbck(i,j) / 100.
              snoalb(i,j) = snoalb(i,j) / 100.
              IF ( landmask(i,j) .LT. 0.5 ) THEN
                 albbck(i,j) = 0.08
                 snoalb(i,j) = 0.08
              END IF
           END DO
         END DO
   
         !  Compute the mixing ratio from the input relative humidity.
   
         CALL rh_to_mxrat (rh_gc, t_gc, p_gc, q_gc , .TRUE. , &
                           ids , ide , jds , jde , 1   , num_vert_levels , &
                           ims , ime , jms , jme , 1   , num_vert_levels , &
                           its , ite , jts , jte , 1   , num_vert_levels )
         
         !  Swap low-res geopot surface with the input topography.
   
         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)
              ght_gc(i,1,j) = ht(i,j)
           END DO
         END DO

         !  Compute total surface pressure from input sea level pressure and hi-res topography.

         CALL sfcprs (t_gc, q_gc, ght_gc, pslv_gc, ht, st000010, p_gc, &
                      psfc, 0, .FALSE., &
                      ids , ide , jds , jde , 1   , num_vert_levels , &
                      ims , ime , jms , jme , 1   , num_vert_levels , &
                      its , ite , jts , jte , 1   , num_vert_levels )
         
         !  Swap low-res surface pressure for computed value.
   
         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)
              p_gc(i,1,j) = psfc(i,j)
           END DO
         END DO
   
         !  Integrate the mixing ratio to get the vapor pressure.
   
         CALL integ_moist ( q_gc , p_gc , pd_gc , t_gc , ght_gc , intq_gc , &
                            ids , ide , jds , jde , 1   , num_vert_levels , &
                            ims , ime , jms , jme , 1   , num_vert_levels , &
                            its , ite , jts , jte , 1   , num_vert_levels )
   
         !  Compute the difference between the dry, total surface pressure (input) and the 
         !  dry top pressure (constant).
   
         CALL p_dts ( mu0 , intq_gc , p_gc , p_top , &
                      ids , ide , jds , jde , 1   , num_vert_levels , &
                      ims , ime , jms , jme , 1   , num_vert_levels , &
                      its , ite , jts , jte , 1   , num_vert_levels )
   
         !  Compute the dry, hydrostatic surface pressure.
   
         CALL p_dhs ( pdhs , ht , p00 , t00 , a , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )
   
         !  Compute the eta levels if not defined already.
   
         IF ( znw(1) .LT. 1 ) THEN
   
            eta_levels(1:kde) = model_config_rec%eta_levels(1:kde)
            CALL compute_eta ( znw , &
                               eta_levels , max_eta , &
                               ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte )
         END IF
   
         !  The input field is temperature, we want potential temp.

         CALL t_to_theta ( t_gc , p_gc , p00 , &
                           ids , ide , jds , jde , 1   , num_vert_levels , &
                           ims , ime , jms , jme , 1   , num_vert_levels , &
                           its , ite , jts , jte , 1   , num_vert_levels )
   
         !  On the eta surfaces, compute the dry pressure = mu eta, stored in 
         !  pb, since it is a pressure, and we don't need another kms:kme 3d
         !  array floating around.  The pb array is re-computed as the base pressure
         !  later after the vertical interpolations are complete.
   
         CALL p_dry ( mu0 , znw , p_top , pb , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )
         
         !  All of the vertical interpolations are done in dry-pressure space.  The
         !  input data has had the moisture removed (pd_gc).  The target levels (pb)
         !  had the vapor pressure removed from the surface pressure, then they were
         !  scaled by the eta levels.

         interp_type = grid%interp_type
         lagrange_order = grid%lagrange_order
         lowest_lev_from_sfc = grid%lowest_lev_from_sfc

         CALL vert_interp ( q_gc , pd_gc , moist(:,:,:,P_QV) , pb , &
                            num_vert_levels , 'Q' , &
                            interp_type , lagrange_order , lowest_lev_from_sfc , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )
   
         CALL vert_interp ( t_gc , pd_gc , t_2               , pb , &
                            num_vert_levels , 'T' , &
                            interp_type , lagrange_order , lowest_lev_from_sfc , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )
#if 0
         !  Uncomment the Registry entries to activate these.  This adds
         !  noticeably to the allocated space for the model.

         IF ( flag_qr .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QR ) THEN
                  CALL vert_interp ( qr_gc , pd_gc , moist(:,:,:,P_QR) , pb , &
                                     num_vert_levels , 'Q' , &
                                     interp_type , lagrange_order , lowest_lev_from_sfc , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF
   
         IF ( flag_qc .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QC ) THEN
                  CALL vert_interp ( qc_gc , pd_gc , moist(:,:,:,P_QC) , pb , &
                                     num_vert_levels , 'Q' , &
                                     interp_type , lagrange_order , lowest_lev_from_sfc , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF
   
         IF ( flag_qi .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QI ) THEN
                  CALL vert_interp ( qi_gc , pd_gc , moist(:,:,:,P_QI) , pb , &
                                     num_vert_levels , 'Q' , &
                                     interp_type , lagrange_order , lowest_lev_from_sfc , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF
   
         IF ( flag_qs .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QS ) THEN
                  CALL vert_interp ( qs_gc , pd_gc , moist(:,:,:,P_QS) , pb , &
                                     num_vert_levels , 'Q' , &
                                     interp_type , lagrange_order , lowest_lev_from_sfc , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF
   
         IF ( flag_qg .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QG ) THEN
                  CALL vert_interp ( qg_gc , pd_gc , moist(:,:,:,P_QG) , pb , &
                                     num_vert_levels , 'Q' , &
                                     interp_type , lagrange_order , lowest_lev_from_sfc , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF
#endif
   
#ifdef DM_PARALLEL
         ips = its ; ipe = ite ; jps = jts ; jpe = jte ; kps = kts ; kpe = kte

         !  For the U and V vertical interpolation, we need the pressure defined
         !  at both the locations for the horizontal momentum, which we get by
         !  averaging two pressure values (i and i-1 for U, j and j-1 for V).  The
         !  pressure field on input (pd_gc) and the pressure of the new coordinate
         !  (pb) are both communicated with an 8 stencil.

#   include "HALO_EM_VINTERP_UV_1.inc"
#endif
   
         CALL vert_interp ( u_gc , pd_gc , u_2               , pb , &
                            num_vert_levels , 'U' , &
                            interp_type , lagrange_order , lowest_lev_from_sfc , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )
   
         CALL vert_interp ( v_gc , pd_gc , v_2               , pb , &
                            num_vert_levels , 'V' , &
                            interp_type , lagrange_order , lowest_lev_from_sfc , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

      END IF     !   <----- END OF VERTICAL INTERPOLATION PART ---->

      !  Protect against bad tsk values over water by supplying sst (if it is 
      !  available, and if the sst is reasonable).

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) .AND. &
                 ( sst(i,j) .GT. 200. ) .AND. ( sst(i,j) .LT. 350. ) ) THEN
               tsk(i,j) = sst(i,j)
            ENDIF            
         END DO
      END DO

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
      CALL nl_get_seaice_threshold ( grid%id , grid%seaice_threshold ) 
      CALL nl_get_isice ( grid%id , grid%isice )
      CALL nl_get_iswater ( grid%id , grid%iswater )
      CALL adjust_for_seaice_pre ( xice , landmask , tsk , ivgtyp , vegcat , lu_index , &
                                   xland , landusef , isltyp , soilcat , soilctop , &
                                   soilcbot , tmn , &
                                   grid%seaice_threshold , &
                                   num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                   grid%iswater , grid%isice , &
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

      IF ( internal_time_loop .NE. 1 ) THEN
!KLUDGE this is to set it to something "reasonable", t_2 is theta at this point!
         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               IF ( tsk(i,j) .LT. 170 .or. tsk(i,j) .GT. 400. ) THEN
                  tsk(i,j) = t_2(i,1,j)
               END IF
            END DO
         END DO
      ELSE
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
      END IF

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

      !  Minimum soil values, residual, from RUC LSM scheme.  For input from Noah and using
      !  RUC LSM scheme, this must be subtracted from the input total soil moisture.  For
      !  input RUC data and using the Noah LSM scheme, this value must be added to the soil
      !  moisture input.

      lqmi(1:num_soil_top_cat) = &
      (/0.045, 0.057, 0.065, 0.067, 0.034, 0.078, 0.10,     &
        0.089, 0.095, 0.10,  0.070, 0.068, 0.078, 0.0,      &
        0.004, 0.065 /)
!       0.004, 0.065, 0.020, 0.004, 0.008 /)  !  has extra levels for playa, lava, and white sand

      !  At the initial time we care about values of soil moisture and temperature, other times are
      !  ignored by the model, so we ignore them, too.  

      IF ( domain_ClockIsStartTime(grid) ) THEN
         account_for_zero_soil_moisture : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )
   
            CASE ( LSMSCHEME )
               iicount = 0
               IF      ( FLAG_SM000010 .EQ. 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( (landmask(i,j).gt.0.5) .and. ( tslb(i,1,j) .gt. 200 ) .and. &
                             ( tslb(i,1,j) .lt. 400 ) .and. ( smois(i,1,j) .lt. 0.005 ) ) then
                           print *,'Noah -> Noah: bad soil moisture at i,j = ',i,j,smois(i,:,j)
                           iicount = iicount + 1
                           smois(i,:,j) = 0.005
                        END IF
                     END DO
                  END DO
                  IF ( iicount .GT. 0 ) THEN
                     print *,'Noah -> Noah: total number of small soil moisture locations = ',iicount
                  END IF
               ELSE IF ( FLAG_SOILM000 .EQ. 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        smois(i,:,j) = smois(i,:,j) + lqmi(isltyp(i,j))
                     END DO
                  END DO
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( (landmask(i,j).gt.0.5) .and. ( tslb(i,1,j) .gt. 200 ) .and. &
                             ( tslb(i,1,j) .lt. 400 ) .and. ( smois(i,1,j) .lt. 0.005 ) ) then
                           print *,'RUC -> Noah: bad soil moisture at i,j = ',i,j,smois(i,:,j)
                           iicount = iicount + 1
                           smois(i,:,j) = 0.005
                        END IF
                     END DO
                  END DO
                  IF ( iicount .GT. 0 ) THEN
                     print *,'RUC -> Noah: total number of small soil moisture locations = ',iicount
                  END IF
               END IF
   
            CASE ( RUCLSMSCHEME )
               iicount = 0
               IF      ( FLAG_SM000010 .EQ. 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        smois(i,:,j) = MAX ( smois(i,:,j) - lqmi(isltyp(i,j)) , 0. )
                     END DO
                  END DO
               ELSE IF ( FLAG_SOILM000 .EQ. 1 ) THEN
                  ! no op
               END IF
   
         END SELECT account_for_zero_soil_moisture
      END IF

      !  Is the TSLB reasonable?

      IF ( internal_time_loop .NE. 1 ) THEN
!KLUDGE this is to set it to something "reasonable", t_2 is theta at this point!
         DO j = jts, MIN(jde-1,jte)
            DO ns = 1 , model_config_rec%num_soil_layers
               DO i = its, MIN(ide-1,ite)
                  IF ( tslb(i,ns,j) .LT. 170 .or. tslb(i,ns,j) .GT. 400. ) THEN
                     tslb(i,ns,j) = t_2(i,1,j)
                     smois(i,ns,j) = 0.3
                  END IF
               END DO
            END DO
         END DO
      ELSE
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
      END IF

      !  Adjustments for the seaice field AFTER the TSLB computations.  This is
      !  is for the Noah LSM scheme.

      num_veg_cat      = SIZE ( landusef , DIM=2 )
      num_soil_top_cat = SIZE ( soilctop , DIM=2 )
      num_soil_bot_cat = SIZE ( soilcbot , DIM=2 )
      CALL nl_get_seaice_threshold ( grid%id , grid%seaice_threshold ) 
      CALL nl_get_isice ( grid%id , grid%isice )
      CALL nl_get_iswater ( grid%id , grid%iswater )
      CALL adjust_for_seaice_post ( xice , landmask , tsk , tsk_save , ivgtyp , vegcat , lu_index , &
                                    xland , landusef , isltyp , soilcat , soilctop , &
                                    soilcbot , tmn , vegfra , &
                                    tslb , smois , sh2o , &
                                    grid%seaice_threshold , &
                                    num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                    model_config_rec%num_soil_layers , &
                                    grid%iswater , grid%isice , &
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

      rdx = 1./config_flags%dx
      rdy = 1./config_flags%dy

      !  Some of the many weird geopotential initializations that we'll see today: ph0 is total, 
      !  and ph_2 is a perturbation from the base state geopotential.  We set the base geopotential 
      !  at the lowest level to terrain elevation * gravity.

      DO j=jts,jte
         DO i=its,ite
            ph0(i,1,j) = ht(i,j) * g
            ph_2(i,1,j) = 0.
         END DO
      END DO

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

   RECURSIVE SUBROUTINE find_my_parent ( grid_ptr_in , grid_ptr_out , id_wanted , found_the_id )

      USE module_domain

      TYPE(domain) , POINTER :: grid_ptr_in , grid_ptr_out
      TYPE(domain) , POINTER :: grid_ptr_sibling
      INTEGER :: id_wanted
      LOGICAL :: found_the_id

      found_the_id = .FALSE.
      grid_ptr_sibling => grid_ptr_in
      DO WHILE ( ASSOCIATED ( grid_ptr_sibling ) )

         IF ( grid_ptr_sibling%grid_id .EQ. id_wanted ) THEN
            found_the_id = .TRUE.
            grid_ptr_out => grid_ptr_sibling
            RETURN
         ELSE IF ( grid_ptr_sibling%num_nests .GT. 0 ) THEN
            grid_ptr_sibling => grid_ptr_sibling%nests(1)%ptr
            CALL find_my_parent ( grid_ptr_sibling , grid_ptr_out , id_wanted , found_the_id )
         ELSE
            grid_ptr_sibling => grid_ptr_sibling%sibling
         END IF

      END DO

   END SUBROUTINE find_my_parent

!---------------------------------------------------------------------

   SUBROUTINE vert_interp ( forig , po , fnew , pnu , &
                            generic , var_type , &
                            interp_type , lagrange_order , lowest_lev_from_sfc , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

   !  Vertically interpolate the new field.  The original field on the original
   !  pressure levels is provided, and the new pressure surfaces to interpolate to.
   
      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: interp_type , lagrange_order
      LOGICAL , INTENT(IN)        :: lowest_lev_from_sfc
      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte
      INTEGER , INTENT(IN)        :: generic

      CHARACTER (LEN=1) :: var_type 

      REAL , DIMENSION(ims:ime,generic,jms:jme) , INTENT(IN)     :: forig , po
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: pnu
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: fnew

      REAL , DIMENSION(ims:ime,generic,jms:jme)                  :: porig
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme)                  :: pnew

      !  Local vars

      INTEGER :: i , j , k , ko , kn , k1 , k2 , ko_1 , ko_2
      INTEGER :: istart , iend , jstart , jend , kstart , kend 
      INTEGER , DIMENSION(ims:ime,kms:kme        )               :: k_above , k_below
      INTEGER , DIMENSION(ims:ime                )               :: ks
      INTEGER , DIMENSION(ims:ime                )               :: ko_above_sfc
      INTEGER :: count , zap , kst

      LOGICAL :: any_below_ground

      REAL :: p1 , p2 , pn
      REAL , DIMENSION(1:generic) :: ordered_porig , ordered_forig
      REAL , DIMENSION(kts:kte) :: ordered_pnew , ordered_fnew

      !  Horiontal loop bounds for different variable types.

      IF      ( var_type .EQ. 'U' ) THEN
         istart = its
         iend   = ite
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte-1
         DO j = jstart,jend
            DO k = 1,generic
               DO i = MAX(ids+1,its) , MIN(ide-1,ite)
                  porig(i,k,j) = ( po(i,k,j) + po(i-1,k,j) ) * 0.5
               END DO
            END DO
            IF ( ids .EQ. its ) THEN
               DO k = 1,generic
                  porig(its,k,j) =  po(its,k,j)
               END DO
            END IF
            IF ( ide .EQ. ite ) THEN
               DO k = 1,generic
                  porig(ite,k,j) =  po(ite-1,k,j)
               END DO
            END IF

            DO k = kstart,kend
               DO i = MAX(ids+1,its) , MIN(ide-1,ite)
                  pnew(i,k,j) = ( pnu(i,k,j) + pnu(i-1,k,j) ) * 0.5
               END DO
            END DO
            IF ( ids .EQ. its ) THEN
               DO k = kstart,kend
                  pnew(its,k,j) =  pnu(its,k,j)
               END DO
            END IF
            IF ( ide .EQ. ite ) THEN
               DO k = kstart,kend
                  pnew(ite,k,j) =  pnu(ite-1,k,j)
               END DO
            END IF
         END DO
      ELSE IF ( var_type .EQ. 'V' ) THEN
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = jte
         kstart = kts
         kend   = kte-1
         DO i = istart,iend
            DO k = 1,generic
               DO j = MAX(jds+1,jts) , MIN(jde-1,jte)
                  porig(i,k,j) = ( po(i,k,j) + po(i,k,j-1) ) * 0.5
               END DO
            END DO
            IF ( jds .EQ. jts ) THEN
               DO k = 1,generic
                  porig(i,k,jts) =  po(i,k,jts)
               END DO
            END IF
            IF ( jde .EQ. jte ) THEN
               DO k = 1,generic
                  porig(i,k,jte) =  po(i,k,jte-1)
               END DO
            END IF

            DO k = kstart,kend
               DO j = MAX(jds+1,jts) , MIN(jde-1,jte)
                  pnew(i,k,j) = ( pnu(i,k,j) + pnu(i,k,j-1) ) * 0.5
               END DO
            END DO
            IF ( jds .EQ. jts ) THEN
               DO k = kstart,kend
                  pnew(i,k,jts) =  pnu(i,k,jts)
               END DO
            END IF
            IF ( jde .EQ. jte ) THEN
              DO k = kstart,kend
                  pnew(i,k,jte) =  pnu(i,k,jte-1)
               END DO
            END IF
         END DO
      ELSE IF ( ( var_type .EQ. 'W' ) .OR.  ( var_type .EQ. 'Z' ) ) THEN
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte
         DO j = jstart,jend
            DO k = 1,generic
               DO i = istart,iend
                  porig(i,k,j) = po(i,k,j)
               END DO
            END DO

            DO k = kstart,kend
               DO i = istart,iend
                  pnew(i,k,j) = pnu(i,k,j)
               END DO
            END DO
         END DO
      ELSE IF ( ( var_type .EQ. 'T' ) .OR. ( var_type .EQ. 'Q' ) ) THEN
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte-1
         DO j = jstart,jend
            DO k = 1,generic
               DO i = istart,iend
                  porig(i,k,j) = po(i,k,j)
               END DO
            END DO

            DO k = kstart,kend
               DO i = istart,iend
                  pnew(i,k,j) = pnu(i,k,j)
               END DO
            END DO
         END DO
      ELSE
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte-1
         DO j = jstart,jend
            DO k = 1,generic
               DO i = istart,iend
                  porig(i,k,j) = po(i,k,j)
               END DO
            END DO

            DO k = kstart,kend
               DO i = istart,iend
                  pnew(i,k,j) = pnu(i,k,j)
               END DO
            END DO
         END DO
      END IF

      DO j = jstart , jend
    
         !  Skip all of the levels below ground in the original data based upon the surface pressure.
         !  The ko_above_sfc is the index in the pressure array that is above the surface.  If there
         !  are no levels underground, this is index = 2.  The remaining levels are eligible for use
         !  in the vertical interpolation.
   
         DO i = istart , iend
            ko_above_sfc(i) = -1
         END DO
         DO ko = kstart+1 , kend
            DO i = istart , iend
               IF ( ko_above_sfc(i) .EQ. -1 ) THEN
                  IF ( porig(i,1,j) .GT. porig(i,ko,j) ) THEN
                     ko_above_sfc(i) = ko
                  END IF
               END IF
            END DO
         END DO

         !  Piece together columns of the original input data.  Pass the vertical columns to
         !  the iterpolator.

         DO i = istart , iend

            !  If the surface value is in the middle of the array, three steps: 1) do the
            !  values below the ground, do the surface, then add in the levels that are above
            !  the surface.

            zap = 0
            IF (  ko_above_sfc(i) .GT. 2 ) THEN
               count = 1
               DO ko = 2 , ko_above_sfc(i)-1
                  ordered_porig(count) = porig(i,ko,j)
                  ordered_forig(count) = forig(i,ko,j)
                  count = count + 1
               END DO

               !  Make sure the pressure just below the surface is not "too close", this
               !  will cause havoc with the higher order interpolators.  In case of a "too close"
               !  instance, we toss out the offending level (NOT the surface one) by simply
               !  decrementing the accumulating loop counter.

               IF ( ordered_porig(count-1) - porig(i,1,j) .LT. 500 ) THEN
                  count = count -1
                  zap = 1
               END IF

               !  Add in the surface values.
   
               ordered_porig(count) = porig(i,1,j)
               ordered_forig(count) = forig(i,1,j)
               count = count + 1

               !  One more time, make sure the pressure just above the surface is not "too close", this
               !  will cause havoc with the higher order interpolators.  In case of a "too close"
               !  instance, we toss out the offending level above the surface (NOT the surface one) by simply
               !  incrementing the loop counter.

               IF ( ordered_porig(count-1) - porig(i,ko_above_sfc(i),j) .LT. 500 ) THEN
                  kst =  ko_above_sfc(i)+1
                  zap = zap + 1
               ELSE
                  kst =  ko_above_sfc(i)
               END IF
   
               DO ko = kst , generic
                  ordered_porig(count) = porig(i,ko,j)
                  ordered_forig(count) = forig(i,ko,j)
                  count = count + 1
               END DO

            !  This is easy, the surface is the lowest level, just stick them in, in this order.
            
            ELSE
               zap = 0
               ordered_porig(1) = porig(i,1,j)
               ordered_forig(1) = forig(i,1,j)
               count = 2
               DO ko = 2 , generic
                  IF ( ordered_porig(count-1) - porig(i,ko,j) .LT. 500 ) THEN
                     zap = zap + 1
                     CYCLE
                  END IF
                  ordered_porig(count) = porig(i,ko,j)
                  ordered_forig(count) = forig(i,ko,j)
                  count = count + 1
               END DO

            END IF

            !  Now get the column of the "new" pressure data.

            DO kn = kstart , kend
               ordered_pnew(kn) = pnew(i,kn,j)
            END DO

            !  The polynomials are either in pressure or LOG(pressure).

            IF ( interp_type .EQ. 1 ) THEN
               CALL lagrange_setup (     ordered_porig                 , ordered_forig , generic-zap   , lagrange_order , &
                                         ordered_pnew                  , ordered_fnew  , kend-kstart+1 ,i,j)
            ELSE
               CALL lagrange_setup ( LOG(ordered_porig(1:generic-zap)) , ordered_forig , generic-zap   , lagrange_order , &
                                     LOG(ordered_pnew(kstart:kend))    , ordered_fnew  , kend-kstart+1 ,i,j)
            END IF

            !  Save the computed data.

            DO kn = kstart , kend
               fnew(i,kn,j) = ordered_fnew(kn)
            END DO

            !  There may have been a request to have the surface data from the input field
            !  to be assigned as to the lowest eta level.  This assumes thin layers (usually
            !  the isobaric original field has the surface from 2-m T and RH, and 10-m U and V).

            IF ( lowest_lev_from_sfc ) THEN
               fnew(i,1,j) = forig(i,ko_above_sfc(i),j)
            END IF

         END DO

      END DO

   END SUBROUTINE vert_interp

!---------------------------------------------------------------------

   SUBROUTINE vert_interp_old ( forig , po , fnew , pnu , &
                            generic , var_type , &
                            interp_type , lagrange_order , lowest_lev_from_sfc , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

   !  Vertically interpolate the new field.  The original field on the original
   !  pressure levels is provided, and the new pressure surfaces to interpolate to.
   
      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: interp_type , lagrange_order
      LOGICAL , INTENT(IN)        :: lowest_lev_from_sfc
      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte
      INTEGER , INTENT(IN)        :: generic

      CHARACTER (LEN=1) :: var_type 

      REAL , DIMENSION(ims:ime,generic,jms:jme) , INTENT(IN)     :: forig , po
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: pnu
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: fnew

      REAL , DIMENSION(ims:ime,generic,jms:jme)                  :: porig
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme)                  :: pnew

      !  Local vars

      INTEGER :: i , j , k , ko , kn , k1 , k2 , ko_1 , ko_2
      INTEGER :: istart , iend , jstart , jend , kstart , kend 
      INTEGER , DIMENSION(ims:ime,kms:kme        )               :: k_above , k_below
      INTEGER , DIMENSION(ims:ime                )               :: ks
      INTEGER , DIMENSION(ims:ime                )               :: ko_above_sfc

      LOGICAL :: any_below_ground

      REAL :: p1 , p2 , pn
integer vert_extrap
vert_extrap = 0

      !  Horiontal loop bounds for different variable types.

      IF      ( var_type .EQ. 'U' ) THEN
         istart = its
         iend   = ite
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte-1
         DO j = jstart,jend
            DO k = 1,generic
               DO i = MAX(ids+1,its) , MIN(ide-1,ite)
                  porig(i,k,j) = ( po(i,k,j) + po(i-1,k,j) ) * 0.5
               END DO
            END DO
            IF ( ids .EQ. its ) THEN
               DO k = 1,generic
                  porig(its,k,j) =  po(its,k,j)
               END DO
            END IF
            IF ( ide .EQ. ite ) THEN
               DO k = 1,generic
                  porig(ite,k,j) =  po(ite-1,k,j)
               END DO
            END IF

            DO k = kstart,kend
               DO i = MAX(ids+1,its) , MIN(ide-1,ite)
                  pnew(i,k,j) = ( pnu(i,k,j) + pnu(i-1,k,j) ) * 0.5
               END DO
            END DO
            IF ( ids .EQ. its ) THEN
               DO k = kstart,kend
                  pnew(its,k,j) =  pnu(its,k,j)
               END DO
            END IF
            IF ( ide .EQ. ite ) THEN
               DO k = kstart,kend
                  pnew(ite,k,j) =  pnu(ite-1,k,j)
               END DO
            END IF
         END DO
      ELSE IF ( var_type .EQ. 'V' ) THEN
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = jte
         kstart = kts
         kend   = kte-1
         DO i = istart,iend
            DO k = 1,generic
               DO j = MAX(jds+1,jts) , MIN(jde-1,jte)
                  porig(i,k,j) = ( po(i,k,j) + po(i,k,j-1) ) * 0.5
               END DO
            END DO
            IF ( jds .EQ. jts ) THEN
               DO k = 1,generic
                  porig(i,k,jts) =  po(i,k,jts)
               END DO
            END IF
            IF ( jde .EQ. jte ) THEN
               DO k = 1,generic
                  porig(i,k,jte) =  po(i,k,jte-1)
               END DO
            END IF

            DO k = kstart,kend
               DO j = MAX(jds+1,jts) , MIN(jde-1,jte)
                  pnew(i,k,j) = ( pnu(i,k,j) + pnu(i,k,j-1) ) * 0.5
               END DO
            END DO
            IF ( jds .EQ. jts ) THEN
               DO k = kstart,kend
                  pnew(i,k,jts) =  pnu(i,k,jts)
               END DO
            END IF
            IF ( jde .EQ. jte ) THEN
              DO k = kstart,kend
                  pnew(i,k,jte) =  pnu(i,k,jte-1)
               END DO
            END IF
         END DO
      ELSE IF ( ( var_type .EQ. 'W' ) .OR.  ( var_type .EQ. 'Z' ) ) THEN
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte
         DO j = jstart,jend
            DO k = 1,generic
               DO i = istart,iend
                  porig(i,k,j) = po(i,k,j)
               END DO
            END DO

            DO k = kstart,kend
               DO i = istart,iend
                  pnew(i,k,j) = pnu(i,k,j)
               END DO
            END DO
         END DO
      ELSE IF ( ( var_type .EQ. 'T' ) .OR. ( var_type .EQ. 'Q' ) ) THEN
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte-1
         DO j = jstart,jend
            DO k = 1,generic
               DO i = istart,iend
                  porig(i,k,j) = po(i,k,j)
               END DO
            END DO

            DO k = kstart,kend
               DO i = istart,iend
                  pnew(i,k,j) = pnu(i,k,j)
               END DO
            END DO
         END DO
      ELSE
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte-1
         DO j = jstart,jend
            DO k = 1,generic
               DO i = istart,iend
                  porig(i,k,j) = po(i,k,j)
               END DO
            END DO

            DO k = kstart,kend
               DO i = istart,iend
                  pnew(i,k,j) = pnu(i,k,j)
               END DO
            END DO
         END DO
      END IF

      DO j = jstart , jend
    
         !  Skip all of the levels below ground in the original data based upon the surface pressure.
         !  The ko_above_sfc is the index in the pressure array that is above the surface.  If there
         !  are no levels underground, this is index = 2.  The remaining levels are eligible for use
         !  in the vertical interpolation.
   
         DO i = istart , iend
            ko_above_sfc(i) = -1
         END DO
         DO ko = kstart+1 , kend
            DO i = istart , iend
               IF ( ko_above_sfc(i) .EQ. -1 ) THEN
                  IF ( porig(i,1,j) .GT. porig(i,ko,j) ) THEN
                     ko_above_sfc(i) = ko
                  END IF
               END IF
            END DO
         END DO

         !  Initialize interpolation location.  These are the levels in the original pressure
         !  data that are physically below and above the targeted new pressure level.
   
         DO kn = kts , kte
            DO i = its , ite
               k_above(i,kn) = -1
               k_below(i,kn) = -2
            END DO
         END DO
    
         !  Starting location is no lower than previous found location.  This is for O(n logn)
         !  and not O(n^2), where n is the number of vertical levels to search.
   
         DO i = its , ite
            ks(i) = 1
         END DO

         !  Find trapping layer for interpolation.  The kn index runs through all of the "new"
         !  levels of data.
   
         DO kn = kstart , kend

            DO i = istart , iend

               !  For each "new" level (kn), we search to find the trapping levels in the "orig"
               !  data.  Most of the time, the "new" levels are the eta surfaces, and the "orig"
               !  levels are the input pressure levels.

               found_trap_above : DO ko = ks(i) , generic-1

                  !  Because we can have levels in the interpolation that are not valid,
                  !  let's toss out any candidate orig pressure values that are below ground
                  !  based on the surface pressure.  If the level =1, then this IS the surface
                  !  level, so we HAVE to keep that one, but maybe not the ones above.  If the
                  !  level (ks) is NOT=1, then we have to just CYCLE our loop to find a legit
                  !  below-pressure value.  If we are not below ground, then we choose two
                  !  neighboring levels to test whether they surround the new pressure level.

                  !  The input trapping levels that we are trying is the surface and the first valid
                  !  level above the surface.

                  IF      ( ( ko .LT. ko_above_sfc(i) ) .AND. ( ko .EQ. 1 ) ) THEN
                     ko_1 = ko
                     ko_2 = ko_above_sfc(i)
     
                  !  The "below" level is underground, cycle until we get to a valid pressure
                  !  above ground.
 
                  ELSE IF ( ( ko .LT. ko_above_sfc(i) ) .AND. ( ko .NE. 1 ) ) THEN
                     CYCLE found_trap_above

                  !  The "below" level is above the surface, so we are in the clear to test these
                  !  two levels out.

                  ELSE
                     ko_1 = ko
                     ko_2 = ko+1

                  END IF

                  !  The test of the candidate levels: "below" has to have a larger pressure, and
                  !  "above" has to have a smaller pressure. 

                  !  OK, we found the correct two surrounding levels.  The locations are saved for use in the
                  !  interpolation.

                  IF      ( ( porig(i,ko_1,j) .GE. pnew(i,kn,j) ) .AND. &
                            ( porig(i,ko_2,j) .LT. pnew(i,kn,j) ) ) THEN
                     k_above(i,kn) = ko_2
                     k_below(i,kn) = ko_1
                     ks(i) = ko_1
                     EXIT found_trap_above

                  !  What do we do is we need to extrapolate the data underground?  This happens when the
                  !  lowest pressure that we have is physically "above" the new target pressure.  Our
                  !  actions depend on the type of variable we are interpolating.

                  ELSE IF   ( porig(i,1,j) .LT. pnew(i,kn,j) ) THEN

                     !  For horizontal winds and moisture, we keep a constant value under ground.

                     IF      ( ( var_type .EQ. 'U' ) .OR. &
                               ( var_type .EQ. 'V' ) .OR. &
                               ( var_type .EQ. 'Q' ) ) THEN
                        k_above(i,kn) = 1
                        ks(i) = 1

                     !  For temperature and height, we extrapolate the data.  Hopefully, we are not
                     !  extrapolating too far.  For pressure level input, the eta levels are always
                     !  contained within the surface to p_top levels, so no extrapolation is ever
                     !  required.  

                     ELSE IF ( ( var_type .EQ. 'Z' ) .OR. &
                               ( var_type .EQ. 'T' ) ) THEN
                        k_above(i,kn) = ko_above_sfc(i)
                        k_below(i,kn) = 1
                        ks(i) = 1

                     !  Just a catch all right now.

                     ELSE
                        k_above(i,kn) = 1
                        ks(i) = 1
                     END IF

                     EXIT found_trap_above

                  !  The other extrapolation that might be required is when we are going above the
                  !  top level of the input data.  Usually this means we chose a P_PTOP value that
                  !  was inappropriate, and we should stop and let someone fix this mess.  

                  ELSE IF   ( porig(i,generic,j) .GT. pnew(i,kn,j) ) THEN
                     print *,'data is too high, try a lower p_top'
                     print *,'pnew=',pnew(i,kn,j)
                     print *,'porig=',porig(i,:,j)
                     CALL wrf_error_fatal ('requested p_top is higher than input data, lower p_top')

                  END IF
               END DO found_trap_above
            END DO
         END DO

         !  Linear vertical interpolation.

         DO kn = kstart , kend
            DO i = istart , iend
               IF ( k_above(i,kn) .EQ. 1 ) THEN
                  fnew(i,kn,j) = forig(i,1,j)
               ELSE
                  k2 = MAX ( k_above(i,kn) , 2)
                  k1 = MAX ( k_below(i,kn) , 1)
                  IF ( k1 .EQ. k2 ) THEN
                     CALL wrf_error_fatal ( 'identical values in the interp, bad for divisions' )
                  END IF
                  IF      ( interp_type .EQ. 1 ) THEN
                     p1 = porig(i,k1,j)
                     p2 = porig(i,k2,j)
                     pn = pnew(i,kn,j)  
                  ELSE IF ( interp_type .EQ. 2 ) THEN
                     p1 = ALOG(porig(i,k1,j))
                     p2 = ALOG(porig(i,k2,j))
                     pn = ALOG(pnew(i,kn,j))
                  END IF
                  IF ( ( p1-pn) * (p2-pn) > 0. ) THEN
!                    CALL wrf_error_fatal ( 'both trapping pressures are on the same side of the new pressure' )
!                    CALL wrf_debug ( 0 , 'both trapping pressures are on the same side of the new pressure' )
vert_extrap = vert_extrap + 1
                  END IF
                  fnew(i,kn,j) = ( forig(i,k1,j) * ( p2 - pn )   + &
                                   forig(i,k2,j) * ( pn - p1 ) ) / &
                                   ( p2 - p1 )
               END IF 
            END DO
         END DO

         search_below_ground : DO kn = kstart , kend
            any_below_ground = .FALSE.
            DO i = istart , iend
               IF ( k_above(i,kn) .EQ. 1 ) THEN 
                  fnew(i,kn,j) = forig(i,1,j)
                  any_below_ground = .TRUE.
               END IF
            END DO
            IF ( .NOT. any_below_ground ) THEN
               EXIT search_below_ground
            END IF
         END DO search_below_ground

         !  There may have been a request to have the surface data from the input field
         !  to be assigned as to the lowest eta level.  This assumes thin layers (usually
         !  the isobaric original field has the surface from 2-m T and RH, and 10-m U and V).

         DO i = istart , iend
            IF ( lowest_lev_from_sfc ) THEN
               fnew(i,1,j) = forig(i,ko_above_sfc(i),j)
            END IF
         END DO

      END DO
print *,'VERT EXTRAP = ', vert_extrap

   END SUBROUTINE vert_interp_old

!---------------------------------------------------------------------

   SUBROUTINE p_dry ( mu0 , eta , pdht , pdry , &
                             ids , ide , jds , jde , kds , kde , &
                             ims , ime , jms , jme , kms , kme , &
                             its , ite , jts , jte , kts , kte )

   !  Compute reference pressure and the reference mu.
   
      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , DIMENSION(ims:ime,        jms:jme) , INTENT(IN)     :: mu0
      REAL , DIMENSION(        kms:kme        ) , INTENT(IN)     :: eta
      REAL                                                       :: pdht
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: pdry

      !  Local vars

      INTEGER :: i , j , k 
      REAL , DIMENSION(        kms:kme        )                  :: eta_h

      DO k = kts , kte-1
         eta_h(k) = ( eta(k) + eta(k+1) ) * 0.5
      END DO

      DO j = jts , MIN ( jde-1 , jte )
         DO k = kts , kte-1
            DO i = its , MIN (ide-1 , ite )
                  pdry(i,k,j) = eta_h(k) * mu0(i,j) + pdht
            END DO
         END DO
      END DO

   END SUBROUTINE p_dry

!---------------------------------------------------------------------

   SUBROUTINE p_dts ( pdts , intq , p , p_top , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Compute difference between the dry, total surface pressure and the top pressure.
   
      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , INTENT(IN) :: p_top
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: p
      REAL , DIMENSION(ims:ime,        jms:jme) , INTENT(IN)     :: intq
      REAL , DIMENSION(ims:ime,        jms:jme) , INTENT(OUT)    :: pdts

      !  Local vars

      INTEGER :: i , j , k 

      DO j = jts , MIN ( jde-1 , jte )
         DO i = its , MIN (ide-1 , ite )
               pdts(i,j) = p(i,1,j) - intq(i,j) - p_top
         END DO
      END DO

   END SUBROUTINE p_dts

!---------------------------------------------------------------------

   SUBROUTINE p_dhs ( pdhs , ht , p0 , t0 , a , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Compute dry, hydrostatic surface pressure.
   
      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , DIMENSION(ims:ime,        jms:jme) , INTENT(IN)     :: ht
      REAL , DIMENSION(ims:ime,        jms:jme) , INTENT(OUT)    :: pdhs

      REAL , INTENT(IN) :: p0 , t0 , a

      !  Local vars

      INTEGER :: i , j , k 

      REAL , PARAMETER :: Rd = 287.
      REAL , PARAMETER :: g  =   9.8

      DO j = jts , MIN ( jde-1 , jte )
         DO i = its , MIN (ide-1 , ite )
               pdhs(i,j) = p0 * EXP ( -t0/a + SQRT ( (t0/a)**2 - 2. * g * ht(i,j)/(a * Rd) ) )
         END DO
      END DO

   END SUBROUTINE p_dhs

!---------------------------------------------------------------------

   SUBROUTINE find_p_top ( p , p_top , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte )

   !  Find the largest pressure in the top level.  This is our p_top.  We are
   !  assuming that the top level is the location where the pressure is a minimum
   !  for each column.  In cases where the top surface is not isobaric, a 
   !  communicated value must be shared in the calling routine.  Also in cases
   !  where the top surface is not isobaric, care must be taken that the new
   !  maximum pressure is not greater than the previous value.  This test is
   !  also handled in the calling routine.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL :: p_top
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN) :: p

      !  Local vars

      INTEGER :: i , j , k 

      k = kte
      p_top = p(its,k,jts)
      DO j = jts , MIN ( jde-1 , jte )
         DO i = its , MIN (ide-1 , ite )
            p_top = MAX ( p_top , p(i,k,j) )
         END DO
      END DO

   END SUBROUTINE find_p_top

!---------------------------------------------------------------------

   SUBROUTINE t_to_theta ( t , p , p00 , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Compute dry, hydrostatic surface pressure.
   
      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , INTENT(IN) :: p00
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: p
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT)  :: t

      !  Local vars

      INTEGER :: i , j , k 

      REAL , PARAMETER :: Rd = 287.
      REAL , PARAMETER :: Cp = 1004.

      DO j = jts , MIN ( jde-1 , jte )
         DO k = kts , kte
            DO i = its , MIN (ide-1 , ite )
               t(i,k,j) = t(i,k,j) * ( p00 / p(i,k,j) ) ** (Rd / Cp)
            END DO
         END DO
      END DO

   END SUBROUTINE t_to_theta

!---------------------------------------------------------------------

   SUBROUTINE integ_moist ( q , p , pd , t , ght , intq , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

   !  Integrate the moisture field vertically.  Mostly used to get the total
   !  vapor pressure, which can be subtracted from the total pressure to get
   !  the dry pressure.
   
      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: q , p , t , ght
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: pd
      REAL , DIMENSION(ims:ime,        jms:jme) , INTENT(OUT)    :: intq

      !  Local vars

      INTEGER :: i , j , k 
      INTEGER , DIMENSION(ims:ime) :: level_above_sfc
      REAL , DIMENSION(ims:ime,jms:jme) :: psfc , tsfc , qsfc, zsfc

      REAL :: rhobar , qbar , dz
      REAL :: p1 , p2 , t1 , t2 , q1 , q2 , z1, z2

      REAL , PARAMETER :: Rd = 287.
      REAL , PARAMETER :: g  =   9.8

      !  Get a surface value, always the first level of a 3d field.

      DO j = jts , MIN ( jde-1 , jte )
         DO i = its , MIN (ide-1 , ite )
            psfc(i,j) = p(i,1,j)
            tsfc(i,j) = t(i,1,j)
            qsfc(i,j) = q(i,1,j)
            zsfc(i,j) = ght(i,1,j)
         END DO
      END DO

      DO j = jts , MIN ( jde-1 , jte )

         !  Initialize the integrated quantity of moisture to zero.

         DO i = its , MIN (ide-1 , ite )
            intq(i,j) = 0.
         END DO

         !  Find the first level above the ground.  If all of the levels are above ground, such as
         !  a terrain following lower coordinate, then the first level above ground is index #2.

         DO i = its , MIN (ide-1 , ite )
            level_above_sfc(i) = -1
            IF ( p(i,kts+1,j) .LT. psfc(i,j) ) THEN
               level_above_sfc(i) = kts+1
            ELSE
               find_k : DO k = kts+1,kte-1
                  IF ( ( p(i,k  ,j)-psfc(i,j) .GE. 0. ) .AND. &
                       ( p(i,k+1,j)-psfc(i,j) .LT. 0. ) ) THEN 
                     level_above_sfc(i) = k+1
                     EXIT find_k
                  END IF
               END DO find_k
               IF ( level_above_sfc(i) .EQ. -1 ) THEN
                  CALL wrf_error_fatal ( 'Could not find level above ground')
               END IF
            END IF
         END DO

         DO i = its , MIN (ide-1 , ite )

            !  Account for the moisture above the ground.

            DO k = kte-1,level_above_sfc(i),-1
                  rhobar = ( p(i,k  ,j) / ( Rd * t(i,k  ,j) ) + &
                             p(i,k+1,j) / ( Rd * t(i,k+1,j) ) ) * 0.5
                  qbar   = ( q(i,k  ,j) + q(i,k+1,j) ) * 0.5
                  dz     = ght(i,k+1,j) - ght(i,k,j)
                  intq(i,j) = intq(i,j) + g * qbar * rhobar / (1. + qbar) * dz
                  pd(i,k+1,j) = p(i,k+1,j) - intq(i,j)
            END DO

            !  Account for the moisture between the surface and the first level up.

            IF ( ( p(i,level_above_sfc(i)-1,j)-psfc(i,j) .GE. 0. ) .AND. &
                 ( p(i,level_above_sfc(i)  ,j)-psfc(i,j) .LT. 0. ) .AND. &
                 ( level_above_sfc(i) .GT. kts ) ) THEN
                  p1 = psfc(i,j)
                  p2 = p(i,level_above_sfc(i),j)
                  t1 = tsfc(i,j)
                  t2 = t(i,level_above_sfc(i),j)
                  q1 = qsfc(i,j)
                  q2 = q(i,level_above_sfc(i),j)
                  z1 = zsfc(i,j)
                  z2 = ght(i,level_above_sfc(i),j)
                  rhobar = ( p1 / ( Rd * t1 ) + &
                             p2 / ( Rd * t2 ) ) * 0.5
                  qbar   = ( q1 + q2 ) * 0.5
                  dz     = z2 - z1
                  intq(i,j) = intq(i,j) + g * qbar * rhobar / (1. + qbar) * dz
                  pd(i,level_above_sfc(i),j) = p(i,level_above_sfc(i),j) - intq(i,j)
                  DO k = level_above_sfc(i)-1,1,-1
                     pd(i,k,j) = p(i,k,j) - intq(i,j)
                  END DO
            END IF

         END DO
      END DO

   END SUBROUTINE integ_moist

!---------------------------------------------------------------------

   SUBROUTINE rh_to_mxrat (rh, t, p, q , wrt_liquid , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte )
   
      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      LOGICAL , INTENT(IN)        :: wrt_liquid

      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: p , t
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT)  :: rh
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: q

      !  Local vars

      INTEGER                     :: i , j , k 

      REAL                        :: ew , q1 , t1

      REAL,         PARAMETER     :: T_REF       = 0.0
      REAL,         PARAMETER     :: MW_AIR      = 28.966
      REAL,         PARAMETER     :: MW_VAP      = 18.0152

      REAL,         PARAMETER     :: A0       = 6.107799961
      REAL,         PARAMETER     :: A1       = 4.436518521e-01
      REAL,         PARAMETER     :: A2       = 1.428945805e-02
      REAL,         PARAMETER     :: A3       = 2.650648471e-04
      REAL,         PARAMETER     :: A4       = 3.031240396e-06
      REAL,         PARAMETER     :: A5       = 2.034080948e-08
      REAL,         PARAMETER     :: A6       = 6.136820929e-11

      REAL,         PARAMETER     :: ES0 = 6.1121

      REAL,         PARAMETER     :: C1       = 9.09718
      REAL,         PARAMETER     :: C2       = 3.56654
      REAL,         PARAMETER     :: C3       = 0.876793
      REAL,         PARAMETER     :: EIS      = 6.1071
      REAL                        :: RHS
      REAL,         PARAMETER     :: TF       = 273.16
      REAL                        :: TK

      REAL                        :: ES
      REAL                        :: QS
      REAL,         PARAMETER     :: EPS         = 0.622
      REAL,         PARAMETER     :: SVP1        = 0.6112
      REAL,         PARAMETER     :: SVP2        = 17.67
      REAL,         PARAMETER     :: SVP3        = 29.65
      REAL,         PARAMETER     :: SVPT0       = 273.15

      !  This subroutine computes mixing ratio (q, kg/kg) from basic variables
      !  pressure (p, Pa), temperature (t, K) and relative humidity (rh, 1-100%).
      !  The reference temperature (t_ref, C) is used to describe the temperature 
      !  at which the liquid and ice phase change occurs.

      DO j = jts , MIN ( jde-1 , jte )
         DO k = kts , kte
            DO i = its , MIN (ide-1 , ite )
                  rh(i,k,j) = MIN ( MAX ( rh(i,k,j) ,  1. ) , 100. ) 
            END DO
         END DO
      END DO

      IF ( wrt_liquid ) THEN
         DO j = jts , MIN ( jde-1 , jte )
            DO k = kts , kte
               DO i = its , MIN (ide-1 , ite )
                  es=svp1*10.*EXP(svp2*(t(i,k,j)-svpt0)/(t(i,k,j)-svp3))
                  qs=eps*es/(p(i,k,j)/100.-es)
                  q(i,k,j)=MAX(.01*rh(i,k,j)*qs,0.0)
               END DO
            END DO
         END DO

      ELSE
         DO j = jts , MIN ( jde-1 , jte )
            DO k = kts , kte
               DO i = its , MIN (ide-1 , ite )

                  t1 = t(i,k,j) - 273.16

                  !  Obviously dry.

                  IF ( t1 .lt. -200. ) THEN
                     q(i,k,j) = 0

                  ELSE

                     !  First compute the ambient vapor pressure of water

                     IF ( ( t1 .GE. t_ref ) .AND. ( t1 .GE. -47.) ) THEN    ! liq phase ESLO
                        ew = a0 + t1 * (a1 + t1 * (a2 + t1 * (a3 + t1 * (a4 + t1 * (a5 + t1 * a6)))))

                     ELSE IF ( ( t1 .GE. t_ref ) .AND. ( t1 .LT. -47. ) ) then !liq phas poor ES
                        ew = es0 * exp(17.67 * t1 / ( t1 + 243.5))

                     ELSE
                        tk = t(i,k,j)
                        rhs = -c1 * (tf / tk - 1.) - c2 * alog10(tf / tk) +  &
                               c3 * (1. - tk / tf) +      alog10(eis)
                        ew = 10. ** rhs

                     END IF

                     !  Now sat vap pres obtained compute local vapor pressure
  
                     ew = MAX ( ew , 0. ) * rh(i,k,j) * 0.01

                     !  Now compute the specific humidity using the partial vapor
                     !  pressures of water vapor (ew) and dry air (p-ew).  The
                     !  constants assume that the pressure is in hPa, so we divide
                     !  the pressures by 100.

                     q1 = mw_vap * ew
                     q1 = q1 / (q1 + mw_air * (p(i,k,j)/100. - ew))

                     q(i,k,j) = q1 / (1. - q1 )

                  END IF

               END DO
            END DO
         END DO

      END IF

   END SUBROUTINE rh_to_mxrat

!---------------------------------------------------------------------

   SUBROUTINE compute_eta ( znw , &
                           eta_levels , max_eta , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte )
   
      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte
      REAL , DIMENSION (kts:kte) , INTENT(OUT) :: znw

      INTEGER , INTENT(IN)        :: max_eta
      REAL , DIMENSION (max_eta) , INTENT(IN)  :: eta_levels

      !  Local vars

      INTEGER :: k 

      IF ( ABS(eta_levels(1)+1.) .GT. 0.0000001 ) THEN

         IF ( ( ABS(eta_levels(1  )-1.) .LT. 0.0000001 ) .AND. &
              ( ABS(eta_levels(kde)-0.) .LT. 0.0000001 ) ) THEN
            DO k = kds+1 , kde-1
	       znw(k) = eta_levels(k)
            END DO
            znw(  1) = 1.
            znw(kde) = 0.
         ELSE
            CALL wrf_error_fatal ( 'First eta level should be 1.0 and the last 0.0' )
         END IF

      ELSE
         IF      ( kte .EQ. 28 ) THEN
            znw( 1) = 1.000
            znw( 2) = 0.990
            znw( 3) = 0.978
            znw( 4) = 0.964
            znw( 5) = 0.946
            znw( 6) = 0.922
            znw( 7) = 0.894
            znw( 8) = 0.860
            znw( 9) = 0.817
            znw(10) = 0.766
            znw(11) = 0.707
            znw(12) = 0.644
            znw(13) = 0.576
            znw(14) = 0.507
            znw(15) = 0.444
            znw(16) = 0.380
            znw(17) = 0.324
            znw(18) = 0.273
            znw(19) = 0.228
            znw(20) = 0.188
            znw(21) = 0.152
            znw(22) = 0.121
            znw(23) = 0.093
            znw(24) = 0.069
            znw(25) = 0.048
            znw(26) = 0.029
            znw(27) = 0.014
            znw(28) = 0.000
#if 0
         ELSE IF ( kte .EQ. 35 ) THEN
            znw( 1) = 1.00000
            znw( 2) = 0.99258
            znw( 3) = 0.98275
            znw( 4) = 0.96996
            znw( 5) = 0.95372
            znw( 6) = 0.93357
            znw( 7) = 0.90913
            znw( 8) = 0.87957
            znw( 9) = 0.84531
            znw(10) = 0.80683
            znw(11) = 0.76467
            znw(12) = 0.71940
            znw(13) = 0.67163
            znw(14) = 0.62198
            znw(15) = 0.57108
            znw(16) = 0.51956
            znw(17) = 0.46803
            znw(18) = 0.42030
            znw(19) = 0.37613
            znw(20) = 0.33532
            znw(21) = 0.29764
            znw(22) = 0.26290
            znw(23) = 0.23092
            znw(24) = 0.20152
            znw(25) = 0.17452
            znw(26) = 0.14978
            znw(27) = 0.12714
            znw(28) = 0.10646
            znw(29) = 0.08761
            znw(30) = 0.07045
            znw(31) = 0.05466
            znw(32) = 0.03981
            znw(33) = 0.02580
            znw(34) = 0.01258
            znw(35) = 0.00000
#endif
         ELSE IF ( kte .EQ. 35 ) THEN
            znw( 1) = 1.000
            znw( 2) = 0.993
            znw( 3) = 0.983
            znw( 4) = 0.970
            znw( 5) = 0.954
            znw( 6) = 0.934
            znw( 7) = 0.909
            znw( 8) = 0.880
            znw( 9) = 0.845
            znw(10) = 0.807
            znw(11) = 0.765
            znw(12) = 0.719
            znw(13) = 0.672
            znw(14) = 0.622
            znw(15) = 0.571
            znw(16) = 0.520
            znw(17) = 0.468
            znw(18) = 0.420
            znw(19) = 0.376
            znw(20) = 0.335
            znw(21) = 0.298
            znw(22) = 0.263
            znw(23) = 0.231
            znw(24) = 0.202
            znw(25) = 0.175
            znw(26) = 0.150
            znw(27) = 0.127
            znw(28) = 0.106
            znw(29) = 0.088
            znw(30) = 0.070
            znw(31) = 0.055
            znw(32) = 0.040
            znw(33) = 0.026
            znw(34) = 0.013
            znw(35) = 0.000
         ELSE
            CALL wrf_error_fatal ( 'umm, just use 28 or 35 levels for now' )
         END IF
      END IF

   END SUBROUTINE compute_eta

!---------------------------------------------------------------------

   SUBROUTINE monthly_min_max ( field_in , field_min , field_max , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Plow through each month, find the max, min values for each i,j.
   
      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , DIMENSION(ims:ime,12,jms:jme) , INTENT(IN)  :: field_in
      REAL , DIMENSION(ims:ime,   jms:jme) , INTENT(OUT) :: field_min , field_max

      !  Local vars

      INTEGER :: i , j , l
      REAL :: minner , maxxer

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            minner = field_in(i,1,j)
            maxxer = field_in(i,1,j)
            DO l = 2 , 12
               IF ( field_in(i,l,j) .LT. minner ) THEN
                  minner = field_in(i,l,j)
               END IF
               IF ( field_in(i,l,j) .GT. maxxer ) THEN
                  maxxer = field_in(i,l,j)
               END IF
            END DO
            field_min(i,j) = minner
            field_max(i,j) = maxxer
         END DO
      END DO
   
   END SUBROUTINE monthly_min_max

!---------------------------------------------------------------------

   SUBROUTINE monthly_interp_to_date ( field_in , date_str , field_out , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Linrarly in time interpolate data to a current valid time.  The data is
   !  assumed to come in "monthly", valid at the 15th of every month.
   
      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      CHARACTER (LEN=24) , INTENT(IN) :: date_str
      REAL , DIMENSION(ims:ime,12,jms:jme) , INTENT(IN)  :: field_in
      REAL , DIMENSION(ims:ime,   jms:jme) , INTENT(OUT) :: field_out

      !  Local vars

      INTEGER :: i , j , l
      INTEGER , DIMENSION(0:13) :: middle
      INTEGER :: target_julyr , target_julday , target_date
      INTEGER :: julyr , julday , int_month
      REAL :: gmt
      CHARACTER (LEN=4) :: yr
      CHARACTER (LEN=2) :: mon , day15


      WRITE(day15,FMT='(I2.2)') 15
      DO l = 1 , 12
         WRITE(mon,FMT='(I2.2)') l
         CALL get_julgmt ( date_str(1:4)//'-'//mon//'-'//day15//'_'//'00:00:00.0000' , julyr , julday , gmt )
         middle(l) = julyr*1000 + julday
      END DO

      l = 0
      middle(l) = middle( 1) - 31

      l = 13
      middle(l) = middle(12) + 31

      CALL get_julgmt ( date_str , target_julyr , target_julday , gmt )
      target_date = target_julyr * 1000 + target_julday
      find_month : DO l = 0 , 12
         IF ( ( middle(l) .LT. target_date ) .AND. ( middle(l+1) .GE. target_date ) ) THEN
            DO j = jts , MIN ( jde-1 , jte )
               DO i = its , MIN (ide-1 , ite )
                  int_month = MOD ( l , 12 )
                  IF ( int_month .EQ. 0 ) int_month = 12
                  field_out(i,j) =  ( field_in(i,int_month+1,j) * ( target_date - middle(l)   ) + &
                                      field_in(i,int_month  ,j) * ( middle(l+1) - target_date ) ) / &
                                    ( middle(l+1) - middle(l) )
               END DO
            END DO
            EXIT find_month
         END IF
      END DO find_month

   END SUBROUTINE monthly_interp_to_date

!---------------------------------------------------------------------

   SUBROUTINE sfcprs (t, q, height, pslv, ter, avgsfct, p, &
                      psfc, psfc_method, less_than_24h, &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )


      !  Computes the surface pressure using the input height,
      !  temperature and q (already computed from relative
      !  humidity) on p surfaces.  Sea level pressure is used
      !  to extrapolate a first guess.

      IMPLICIT NONE

      REAL, PARAMETER    :: g         = 9.8
      REAL, PARAMETER    :: gamma     = 6.5E-3
      REAL, PARAMETER    :: pconst    = 10000.0
      REAL, PARAMETER    :: Rd        = 287.
      REAL, PARAMETER    :: TC        = 273.15 + 17.5

      REAL, PARAMETER    :: gammarg   = gamma * Rd / g
      REAL, PARAMETER    :: rov2      = Rd / 2.

      INTEGER , INTENT(IN) ::  ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte 
      INTEGER , INTENT ( IN ) :: psfc_method

      LOGICAL , INTENT ( IN ) :: less_than_24h

      REAL , DIMENSION (ims:ime,kms:kme,jms:jme) , INTENT(IN ):: t, q, height, p
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(IN ):: pslv ,  ter, avgsfct 
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(OUT):: psfc
      
      INTEGER                     :: i
      INTEGER                     :: j
      INTEGER                     :: k
      INTEGER                     :: k500
      INTEGER                     :: k700
      INTEGER                     :: k850

      LOGICAL                     :: l1
      LOGICAL                     :: l2
      LOGICAL                     :: l3

      REAL                        :: gamma78     ( its:ite,jts:jte )
      REAL                        :: gamma57     ( its:ite,jts:jte )
      REAL                        :: ht          ( its:ite,jts:jte )
      REAL                        :: p1          ( its:ite,jts:jte )
      REAL                        :: t1          ( its:ite,jts:jte )
      REAL                        :: t500        ( its:ite,jts:jte )
      REAL                        :: t700        ( its:ite,jts:jte )
      REAL                        :: t850        ( its:ite,jts:jte )
      REAL                        :: tfixed      ( its:ite,jts:jte )
      REAL                        :: tsfc        ( its:ite,jts:jte )
      REAL                        :: tslv        ( its:ite,jts:jte )

      !  We either compute the surface pressure from a time averaged surface temperature
      !  (what we will call the "easy way"), or we try to remove the diurnal impact on the
      !  surface temperature (what we will call the "other way").  Both are essentially 
      !  corrections to a sea level pressure with a high-resolution topography field.

      IF ( ( psfc_method .eq. 1 ) .AND. ( .NOT. less_than_24h ) ) THEN

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               psfc(i,j) = pslv(i,j) * ( 1.0 + gamma * ter(i,j) / avgsfct(i,j) ) ** ( - g / ( Rd * gamma ) )
            END DO
         END DO

      ELSE

         !  Find the locations of the 850, 700 and 500 mb levels.
   
         k850 = 0                              ! find k at: P=850
         k700 = 0                              !            P=700
         k500 = 0                              !            P=500
   
         i = 1
         j = 1
         DO k = kts+1 , kte
            IF      (NINT(p(i,k,j)) .EQ. 85000) THEN
               k850 = k
            ELSE IF (NINT(p(i,k,j)) .EQ. 70000) THEN
               k700 = k
            ELSE IF (NINT(p(i,k,j)) .EQ. 50000) THEN
               k500 = k
            END IF
         END DO
   
         IF ( ( k850 .EQ. 0 ) .OR. ( k700 .EQ. 0 ) .OR. ( k500 .EQ. 0 ) ) THEN
            PRINT '(A)','Error in finding p level for 850, 700 or 500 hPa.'
            DO K = kts+1 , kte
               PRINT '(A,I3,A,F10.2,A)','K = ',k,'  PRESSURE = ',p(i,k,j),' Pa'
            END DO
            PRINT '(A)','Expected 850, 700, and 500 mb values, at least.'
            CALL wrf_error_fatal ( 'wrong pressure levels' )
         END IF
       
         !  The 850 hPa level of geopotential height is called something special.
   
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               ht(i,j) = height(i,k850,j)
            END DO
         END DO
   
         !  The variable ht is now -ter/ht(850 hPa).  The plot thickens.
   
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               ht(i,j) = -ter(i,j) / ht(i,j)
            END DO
         END DO
   
         !  Make an isothermal assumption to get a first guess at the surface
         !  pressure.  This is to tell us which levels to use for the lapse
         !  rates in a bit.
   
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               psfc(i,j) = pslv(i,j) * (pslv(i,j) / 85000.) ** ht(i,j)
            END DO
         END DO
   
         !  Get a pressure more than pconst Pa above the surface - p1.  The
         !  p1 is the top of the level that we will use for our lapse rate
         !  computations.
   
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF      ( ( psfc(i,j) - 95000. ) .GE. 0. ) THEN
                  p1(i,j) = 85000.
               ELSE IF ( ( psfc(i,j) - 70000. ) .GE. 0. ) THEN
                  p1(i,j) = psfc(i,j) - pconst
               ELSE
                  p1(i,j) = 50000.
               END IF
            END DO
         END DO
   
         !  Compute virtual temperatures for k850, k700, and k500 layers.  Now
         !  you see why we wanted Q on pressure levels, it all is beginning   
         !  to make sense.
   
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               t850(i,j) = t(i,k850,j) * (1. + 0.608 * q(i,k850,j))
               t700(i,j) = t(i,k700,j) * (1. + 0.608 * q(i,k700,j))
               t500(i,j) = t(i,k500,j) * (1. + 0.608 * q(i,k500,j))
            END DO
         END DO
   
         !  Compute lapse rates between these three levels.  These are
         !  environmental values for each (i,j).
   
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               gamma78(i,j) = ALOG(t850(i,j) / t700(i,j))  / ALOG (85000. / 70000.)
               gamma57(i,j) = ALOG(t700(i,j) / t500(i,j))  / ALOG (70000. / 50000.)
            END DO
         END DO
   
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF      ( ( psfc(i,j) - 95000. ) .GE. 0. ) THEN
                  t1(i,j) = t850(i,j)
               ELSE IF ( ( psfc(i,j) - 85000. ) .GE. 0. ) THEN
                  t1(i,j) = t700(i,j) * (p1(i,j) / 70000.) ** gamma78(i,j)
               ELSE IF ( ( psfc(i,j) - 70000. ) .GE. 0.) THEN 
                  t1(i,j) = t500(i,j) * (p1(i,j) / 50000.) ** gamma57(i,j)
               ELSE
                  t1(i,j) = t500(i,j)
               ENDIF
            END DO 
         END DO 
   
         !  From our temperature way up in the air, we extrapolate down to
         !  the sea level to get a guess at the sea level temperature.
   
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               tslv(i,j) = t1(i,j) * (pslv(i,j) / p1(i,j)) ** gammarg
            END DO 
         END DO 
   
         !  The new surface temperature is computed from the with new sea level 
         !  temperature, just using the elevation and a lapse rate.  This lapse 
         !  rate is -6.5 K/km.
   
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               tsfc(i,j) = tslv(i,j) - gamma * ter(i,j)
            END DO 
         END DO 
   
         !  A small correction to the sea-level temperature, in case it is too warm.
   
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               tfixed(i,j) = tc - 0.005 * (tsfc(i,j) - tc) ** 2
            END DO 
         END DO 
   
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               l1 = tslv(i,j) .LT. tc
               l2 = tsfc(i,j) .LE. tc
               l3 = .NOT. l1
               IF      ( l2 .AND. l3 ) THEN
                  tslv(i,j) = tc
               ELSE IF ( ( .NOT. l2 ) .AND. l3 ) THEN
                  tslv(i,j) = tfixed(i,j)
               END IF
            END DO
         END DO
   
         !  Finally, we can get to the surface pressure.

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
            p1(i,j) = - ter(i,j) * g / ( rov2 * ( tsfc(i,j) + tslv(i,j) ) )
            psfc(i,j) = pslv(i,j) * EXP ( p1(i,j) )
            END DO
         END DO

      END IF

      !  Surface pressure and sea-level pressure are the same at sea level.

!     DO j = jts , MIN(jde-1,jte)
!        DO i = its , MIN(ide-1,ite)
!           IF ( ABS ( ter(i,j) )  .LT. 0.1 ) THEN
!              psfc(i,j) = pslv(i,j)
!           END IF
!        END DO
!     END DO

   END SUBROUTINE sfcprs

!---------------------------------------------------------------------

   SUBROUTINE lagrange_setup ( all_x , all_y , all_dim , n , target_x , target_y , target_dim ,i,j)

      !  We call a Lagrange polynomial interpolator.  The parallel concerns are put off as this
      !  is initially set up for vertical use.  The purpose is an input column of pressure (all_x),
      !  and the associated pressure level data (all_y).  These are assumed to be sorted (ascending
      !  or descending, no matter).  The locations to be interpolated to are the pressures in
      !  target_x, probably the new vertical coordinate values.  The field that is output is the
      !  target_y, which is defined at the target_x location.  Mostly we expect to be 2nd order
      !  overlapping polynomials, with only a single 2nd order method near the top and bottom.
      !  When n=1, this is linear; when n=2, this is a second order interpolator.

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: all_dim , n , target_dim
      REAL, DIMENSION(all_dim) , INTENT(IN) :: all_x , all_y
      REAL , DIMENSION(target_dim) , INTENT(IN) :: target_x
      REAL , DIMENSION(target_dim) , INTENT(OUT) :: target_y

      !  Brought in for debug purposes.
integer,intent(in) :: i,j

      !  Local vars

      REAL , DIMENSION(n+1) :: x , y 
      REAL :: target_y_1 , target_y_2
      LOGICAL :: found_loc
      INTEGER :: loop , loc_center_left , loc_center_right , ist , iend , target_loop

      IF ( all_dim .LT. n+1 ) THEN
print *,'all_dim = ',all_dim
print *,'order = ',n
print *,'i,j = ',i,j
print *,'p array = ',all_x
print *,'f array = ',all_y
print *,'p target= ',target_x
         CALL wrf_error_fatal ( 'troubles, the interpolating order is too large for this few input values' )
      END IF

      IF ( n .LT. 1 ) THEN
         CALL wrf_error_fatal ( 'pal, linear is about as low as we go' )
      END IF

      !  Loop over the list of target x and y values.

      DO target_loop = 1 , target_dim

         !  Find the two trapping x values, and keep the indices.
   
         found_loc = .FALSE.
         find_trap : DO loop = 1 , all_dim -1
            IF ( ( target_x(target_loop) - all_x(loop) ) * ( target_x(target_loop) - all_x(loop+1) ) .LE. 0.0 ) THEN
               loc_center_left  = loop
               loc_center_right = loop+1
               found_loc = .TRUE.
               EXIT find_trap
            END IF
         END DO find_trap
   
         IF ( .NOT. found_loc ) THEN
            CALL wrf_error_fatal ( 'troubles, could not find trapping x locations' )
         END IF
   
         !  Even or odd order?  We can put the value in the middle if this is
         !  an odd order interpolator.  For the even guys, we'll do it twice
         !  and shift the range one index, then get an average.
   
         IF      ( MOD(n,2) .NE. 0 ) THEN
            IF ( ( loc_center_left -(((n+1)/2)-1) .GE.       1 ) .AND. &
                 ( loc_center_right+(((n+1)/2)-1) .LE. all_dim ) ) THEN
               ist  = loc_center_left -(((n+1)/2)-1)
               iend = iend + n
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , n , target_x(target_loop) , target_y(target_loop) )
            ELSE
               IF ( .NOT. found_loc ) THEN
                  CALL wrf_error_fatal ( 'I doubt this will happen, I will only do 2nd order for now' )
               END IF
            END IF
   
         ELSE IF ( MOD(n,2) .EQ. 0 ) THEN
            IF      ( ( loc_center_left -(((n  )/2)-1) .GE.       1 ) .AND. &
                      ( loc_center_right+(((n  )/2)  ) .LE. all_dim ) .AND. &
                      ( loc_center_left -(((n  )/2)  ) .GE.       1 ) .AND. &
                      ( loc_center_right+(((n  )/2)-1) .LE. all_dim ) ) THEN
               ist  = loc_center_left -(((n  )/2)-1)
               iend = ist + n
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , n , target_x(target_loop) , target_y_1              )
               ist  = loc_center_left -(((n  )/2)  )
               iend = ist + n
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , n , target_x(target_loop) , target_y_2              )
               target_y(target_loop) = ( target_y_1 + target_y_2 ) * 0.5
   
            ELSE IF ( ( loc_center_left -(((n  )/2)-1) .GE.       1 ) .AND. &
                      ( loc_center_right+(((n  )/2)  ) .LE. all_dim ) ) THEN
               ist  = loc_center_left -(((n  )/2)-1)
               iend = ist + n
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , n , target_x(target_loop) , target_y(target_loop)   )
            ELSE IF ( ( loc_center_left -(((n  )/2)  ) .GE.       1 ) .AND. &
                      ( loc_center_right+(((n  )/2)-1) .LE. all_dim ) ) THEN
               ist  = loc_center_left -(((n  )/2)  )
               iend = ist + n
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , n , target_x(target_loop) , target_y(target_loop)   )
            ELSE
               CALL wrf_error_fatal ( 'unauthorized area, you should not be here' )
            END IF
               
         END IF

      END DO

   END SUBROUTINE lagrange_setup 

!---------------------------------------------------------------------

   SUBROUTINE lagrange_interp ( x , y , n , target_x , target_y )

      !  Interpolation using Lagrange polynomials.
      !  P(x) = f(x0)Ln0(x) + ... + f(xn)Lnn(x)
      !  where Lnk(x) = (x -x0)(x -x1)...(x -xk-1)(x -xk+1)...(x -xn)
      !                 ---------------------------------------------
      !                 (xk-x0)(xk-x1)...(xk-xk-1)(xk-xk+1)...(xk-xn)

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: n
      REAL , DIMENSION(0:n) , INTENT(IN) :: x , y
      REAL , INTENT(IN) :: target_x

      REAL , INTENT(OUT) :: target_y

      !  Local vars

      INTEGER :: i , k
      REAL :: numer , denom , Px
      REAL , DIMENSION(0:n) :: Ln

      Px = 0.
      DO i = 0 , n
         numer = 1.         
         denom = 1.         
         DO k = 0 , n
            IF ( k .EQ. i ) CYCLE
            numer = numer * ( target_x  - x(k) )
            denom = denom * ( x(i)  - x(k) )
         END DO
         Ln(i) = y(i) * numer / denom
         Px = Px + Ln(i)
      END DO
      target_y = Px

   END SUBROUTINE lagrange_interp

!---------------------------------------------------------------------

   SUBROUTINE init_module_initialize
   END SUBROUTINE init_module_initialize

!---------------------------------------------------------------------

END MODULE module_initialize
