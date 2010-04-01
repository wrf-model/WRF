!REAL:MODEL_LAYER:INITIALIZATION

#ifndef VERT_UNIT
!  This MODULE holds the routines which are used to perform various initializations
!  for the individual domains, specifically for the Eulerian, mass-based coordinate.

!-----------------------------------------------------------------------

MODULE module_initialize_real

   USE module_bc
   USE module_configure
   USE module_domain
   USE module_io_domain
   USE module_model_constants
   USE module_state_description
   USE module_timing
   USE module_soil_pre
   USE module_date_time
   USE module_llxy
#ifdef DM_PARALLEL
   USE module_dm
   USE module_comm_dm, ONLY : &
                           HALO_EM_INIT_1_sub   &
                          ,HALO_EM_INIT_2_sub   &
                          ,HALO_EM_INIT_3_sub   &
                          ,HALO_EM_INIT_4_sub   &
                          ,HALO_EM_INIT_5_sub   &
                          ,HALO_EM_VINTERP_UV_1_sub
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

      INTEGER :: idum1, idum2

      CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

        CALL init_domain_rk( grid &
!
#include "actual_new_args.inc"
!
      )
   END SUBROUTINE init_domain

!-------------------------------------------------------------------

   SUBROUTINE init_domain_rk ( grid &
!
#include "dummy_new_args.inc"
!
   )

      USE module_optional_input
      IMPLICIT NONE

      !  Input space and data.  No gridded meteorological data has been stored, though.

!     TYPE (domain), POINTER :: grid
      TYPE (domain)          :: grid

#include "dummy_new_decl.inc"

      TYPE (grid_config_rec_type)              :: config_flags

      !  Local domain indices and counters.

      INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat
      INTEGER :: loop , num_seaice_changes

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte, &
                 ips, ipe, jps, jpe, kps, kpe, &
                 i, j, k

      INTEGER :: imsx, imex, jmsx, jmex, kmsx, kmex,    &
                 ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                 imsy, imey, jmsy, jmey, kmsy, kmey,    &
                 ipsy, ipey, jpsy, jpey, kpsy, kpey

      INTEGER :: ns

      !  Local data

      INTEGER :: error
      INTEGER :: im, num_3d_m, num_3d_s
      REAL    :: p_surf, p_level
      REAL    :: cof1, cof2
      REAL    :: qvf , qvf1 , qvf2 , pd_surf
      REAL    :: p00 , t00 , a , tiso
      REAL    :: hold_znw
      LOGICAL :: were_bad

      LOGICAL :: stretch_grid, dry_sounding, debug
      INTEGER IICOUNT

      REAL :: p_top_requested , temp
      INTEGER :: num_metgrid_levels
      REAL , DIMENSION(max_eta) :: eta_levels
      REAL :: max_dz

!      INTEGER , PARAMETER :: nl_max = 1000
!      REAL , DIMENSION(nl_max) :: grid%dn

integer::oops1,oops2

      REAL    :: zap_close_levels
      INTEGER :: force_sfc_in_vinterp
      INTEGER :: interp_type , lagrange_order , extrap_type , t_extrap_type
      LOGICAL :: lowest_lev_from_sfc , use_levels_below_ground , use_surface
      LOGICAL :: we_have_tavgsfc , we_have_tsk

      INTEGER :: lev500 , loop_count
      REAL    :: zl , zu , pl , pu , z500 , dz500 , tvsfc , dpmu

      LOGICAL , PARAMETER :: want_full_levels = .TRUE.
      LOGICAL , PARAMETER :: want_half_levels = .FALSE.

      CHARACTER (LEN=80) :: a_message
      REAL :: max_mf

!-- Carsel and Parrish [1988]
        REAL , DIMENSION(100) :: lqmi

      !  Dimension information stored in grid data structure.

      CALL get_ijk_from_grid (  grid ,                   &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe,    &
                                imsx, imex, jmsx, jmex, kmsx, kmex,    &
                                ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                                imsy, imey, jmsy, jmey, kmsy, kmey,    &
                                ipsy, ipey, jpsy, jpey, kpsy, kpey )
      its = ips ; ite = ipe ; jts = jps ; jte = jpe ; kts = kps ; kte = kpe


      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

      !  Send out a quick message about the time steps based on the map scale factors.

      IF ( ( internal_time_loop .EQ. 1 ) .AND. ( grid%id .EQ. 1 ) .AND. ( .NOT. config_flags%map_proj .EQ. PROJ_CASSINI ) ) THEN
         max_mf = grid%msft(its,jts)
         DO j=jts,MIN(jde-1,jte)
            DO i=its,MIN(ide-1,ite)
               max_mf = MAX ( max_mf , grid%msft(i,j) )
            END DO
         END DO
#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
         max_mf = wrf_dm_max_real ( max_mf )
#endif
         WRITE ( a_message , FMT='(A,F5.2,A)' ) 'Max map factor in domain 1 = ',max_mf,'. Scale the dt in the model accordingly.'
         CALL wrf_message ( a_message ) 
      END IF

      !  Check to see if the boundary conditions are set properly in the namelist file.
      !  This checks for sufficiency and redundancy.

      CALL boundary_condition_check( config_flags, bdyzone, error, grid%id )

      !  Some sort of "this is the first time" initialization.  Who knows.

      grid%step_number = 0
      grid%itimestep=0

      !  Pull in the info in the namelist to compare it to the input data.

      grid%real_data_init_type = model_config_rec%real_data_init_type

      !  To define the base state, we call a USER MODIFIED routine to set the three
      !  necessary constants:  p00 (sea level pressure, Pa), t00 (sea level temperature, K),
      !  and A (temperature difference, from 1000 mb to 300 mb, K).
   
      CALL const_module_initialize ( p00 , t00 , a , tiso ) 

      !  Save these constants to write out in model output file

      grid%t00  = t00
      grid%p00  = p00
      grid%tlp  = a
      grid%tiso = tiso

      !  Fix the snow (water equivalent depth, kg/m^2) and the snowh (physical snow
      !  depth, m) fields.

      IF      ( ( flag_snow .EQ. 0 ) .AND. ( flag_snowh .EQ. 0 ) ) THEN
         DO j=jts,MIN(jde-1,jte)
            DO i=its,MIN(ide-1,ite)
               grid%snow(i,j)  = 0.
               grid%snowh(i,j) = 0.
            END DO
         END DO

      ELSE IF ( ( flag_snow .EQ. 0 ) .AND. ( flag_snowh .EQ. 1 ) ) THEN
         DO j=jts,MIN(jde-1,jte)
            DO i=its,MIN(ide-1,ite)
!              ( m -> kg/m^2 )  & ( reduce to liquid, 5:1 ratio )
               grid%snow(i,j)  = grid%snowh(i,j) * 1000. / 5.
            END DO
         END DO

      ELSE IF ( ( flag_snow .EQ. 1 ) .AND. ( flag_snowh .EQ. 0 ) ) THEN
         DO j=jts,MIN(jde-1,jte)
            DO i=its,MIN(ide-1,ite)
!              ( kg/m^2 -> m)  & ( liquid to snow depth, 5:1 ratio )
               grid%snowh(i,j) = grid%snow(i,j) / 1000. * 5.
            END DO
         END DO

      END IF

      !  For backward compatibility, we might need to assign the map factors from
      !  what they were, to what they are.

      IF      ( ( config_flags%polar ) .AND. ( flag_mf_xy .EQ. 1 ) ) THEN
         DO j=max(jds+1,jts),min(jde-1,jte)
            DO i=its,min(ide-1,ite)
               grid%msfvx_inv(i,j) = 1./grid%msfvx(i,j)
            END DO
         END DO
         IF(jts == jds) THEN
            DO i=its,ite
               grid%msfvx(i,jts) = 0.
               grid%msfvx_inv(i,jts) = 0.
            END DO
         END IF
         IF(jte == jde) THEN
            DO i=its,ite
               grid%msfvx(i,jte) = 0.
               grid%msfvx_inv(i,jte) = 0.
            END DO
         END IF
      ELSE IF ( ( config_flags%map_proj .EQ. PROJ_CASSINI ) .AND. ( flag_mf_xy .EQ. 1 ) ) THEN
         DO j=jts,jte
            DO i=its,min(ide-1,ite)
               grid%msfvx_inv(i,j) = 1./grid%msfvx(i,j)
            END DO
         END DO
      ELSE IF ( ( .NOT. config_flags%map_proj .EQ. PROJ_CASSINI ) .AND. ( flag_mf_xy .NE. 1 ) ) THEN
         DO j=jts,jte
            DO i=its,ite
               grid%msfvx(i,j) = grid%msfv(i,j)
               grid%msfvy(i,j) = grid%msfv(i,j)
               grid%msfux(i,j) = grid%msfu(i,j)
               grid%msfuy(i,j) = grid%msfu(i,j)
               grid%msftx(i,j) = grid%msft(i,j)
               grid%msfty(i,j) = grid%msft(i,j)
            ENDDO
         ENDDO
         DO j=jts,min(jde,jte)
            DO i=its,min(ide-1,ite)
               grid%msfvx_inv(i,j) = 1./grid%msfvx(i,j)
            END DO
         END DO
      ELSE IF ( ( .NOT. config_flags%map_proj .EQ. PROJ_CASSINI ) .AND. ( flag_mf_xy .EQ. 1 ) ) THEN
         IF ( grid%msfvx(its,jts) .EQ. 0 ) THEN
            CALL wrf_error_fatal ( 'Maybe you do not have the new map factors, try re-running geogrid' )
         END IF
         DO j=jts,min(jde,jte)
            DO i=its,min(ide-1,ite)
               grid%msfvx_inv(i,j) = 1./grid%msfvx(i,j)
            END DO
         END DO
      ELSE IF ( ( config_flags%map_proj .EQ. PROJ_CASSINI ) .AND. ( flag_mf_xy .NE. 1 ) ) THEN
         CALL wrf_error_fatal ( 'Neither SI data nor older metgrid data can initialize a global domain' )
      ENDIF

      !  Check to see what available surface temperatures we have.

      IF ( flag_tavgsfc .EQ. 1 ) THEN
         we_have_tavgsfc = .TRUE.
      ELSE
         we_have_tavgsfc = .FALSE.
      END IF

      IF ( flag_tsk     .EQ. 1 ) THEN
         we_have_tsk     = .TRUE.
      ELSE
         we_have_tsk     = .FALSE.
      END IF
   
      IF ( config_flags%use_tavg_for_tsk ) THEN
         IF ( we_have_tsk .OR. we_have_tavgsfc ) THEN
           !  we are OK
         ELSE
            CALL wrf_error_fatal ( 'We either need TSK or TAVGSFC, verify these fields are coming from WPS' )
         END IF
   
         !  Since we require a skin temperature in the model, we can use the average 2-m temperature if provided.
   
         IF ( we_have_tavgsfc ) THEN
            DO j=jts,min(jde,jte)
               DO i=its,min(ide-1,ite)
                  grid%tsk(i,j) = grid%tavgsfc(i,j)
               END DO
            END DO
         END IF
      END IF

      !  Is there any vertical interpolation to do?  The "old" data comes in on the correct
      !  vertical locations already.

      IF ( flag_metgrid .EQ. 1 ) THEN  !   <----- START OF VERTICAL INTERPOLATION PART ---->

         !  Variables that are named differently between SI and WPS.

         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)
              grid%tsk(i,j) = grid%tsk_gc(i,j)
              grid%tmn(i,j) = grid%tmn_gc(i,j)
              grid%xlat(i,j) = grid%xlat_gc(i,j)
              grid%xlong(i,j) = grid%xlong_gc(i,j)
              grid%ht(i,j) = grid%ht_gc(i,j)
           END DO
         END DO

         !  A user could request that the most coarse grid has the
         !  topography along the outer boundary smoothed.  This smoothing
         !  is similar to the coarse/nest interface.  The outer rows and
         !  cols come from the existing large scale topo, and then the
         !  next several rows/cols are a linear ramp of the large scale
         !  model and the hi-res topo from WPS.  We only do this for the
         !  coarse grid since we are going to make the interface consistent
         !  in the model betwixt the CG and FG domains.

         IF ( ( config_flags%smooth_cg_topo ) .AND. &
              ( grid%id .EQ. 1 ) .AND. &
              ( flag_soilhgt .EQ. 1) ) THEN
            CALL blend_terrain ( grid%toposoil  , grid%ht , &
                                 ids , ide , jds , jde , 1   , 1   , &
                                 ims , ime , jms , jme , 1   , 1   , &
                                 ips , ipe , jps , jpe , 1   , 1   )

         END IF

         !  Filter the input topography if this is a polar projection.

         IF ( ( config_flags%polar ) .AND. ( grid%fft_filter_lat .GT. 90 ) ) THEN
            CALL wrf_error_fatal ( 'If the polar boundary condition is used, then fft_filter_lat must be set in namelist.input' )
         END IF

         IF ( config_flags%map_proj .EQ. PROJ_CASSINI ) THEN
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )

            !  We stick the topo and map fac in an unused 3d array. The map scale
            !  factor and computational latitude are passed along for the ride
            !  (part of the transpose process - we only do 3d arrays) to determine
            !  "how many" values are used to compute the mean.  We want a number
            !  that is consistent with the original grid resolution.


            DO j = jts, MIN(jte,jde-1)
              DO k = kts, kte
                 DO i = its, MIN(ite,ide-1)
                    grid%t_init(i,k,j) = 1.
                 END DO
              END DO
              DO i = its, MIN(ite,ide-1)
                 grid%t_init(i,1,j) = grid%ht(i,j)
                 grid%t_init(i,2,j) = grid%msftx(i,j)
                 grid%t_init(i,3,j) = grid%clat(i,j)
              END DO
            END DO

# include "XPOSE_POLAR_FILTER_TOPO_z2x.inc"

            !  Retrieve the 2d arrays for topo, map factors, and the
            !  computational latitude.

            DO j = jpsx, MIN(jpex,jde-1)
              DO i = ipsx, MIN(ipex,ide-1)
                 grid%ht_xxx(i,j)   = grid%t_xxx(i,1,j)
                 grid%mf_xxx(i,j)   = grid%t_xxx(i,2,j)
                 grid%clat_xxx(i,j) = grid%t_xxx(i,3,j)
              END DO
            END DO

            !  Get a mean topo field that is consistent with the grid
            !  distance on each computational latitude loop.

            CALL filter_topo ( grid%ht_xxx , grid%clat_xxx , grid%mf_xxx , &
                               grid%fft_filter_lat , &
                               ids, ide, jds, jde, 1 , 1 , &
                               imsx, imex, jmsx, jmex, 1, 1, &
                               ipsx, ipex, jpsx, jpex, 1, 1 )

            !  Stick the filtered topo back into the dummy 3d array to
            !  transpose it back to "all z on a patch".

            DO j = jpsx, MIN(jpex,jde-1)
              DO i = ipsx, MIN(ipex,ide-1)
                 grid%t_xxx(i,1,j) = grid%ht_xxx(i,j)
              END DO
            END DO

# include "XPOSE_POLAR_FILTER_TOPO_x2z.inc"

            !  Get the un-transposed topo data.

            DO j = jts, MIN(jte,jde-1)
              DO i = its, MIN(ite,ide-1)
                 grid%ht(i,j) = grid%t_init(i,1,j)
              END DO
            END DO
#else
            CALL filter_topo ( grid%ht , grid%clat , grid%msftx , &
                               grid%fft_filter_lat , &
                               ids, ide, jds, jde, 1,1,           &
                               ims, ime, jms, jme, 1,1,           &
                               its, ite, jts, jte, 1,1 )
#endif
         END IF

         !  If we have any input low-res surface pressure, we store it.

         IF ( flag_psfc .EQ. 1 ) THEN
            DO j = jts, MIN(jte,jde-1)
              DO i = its, MIN(ite,ide-1)
                 grid%psfc_gc(i,j) = grid%psfc(i,j)
                 grid%p_gc(i,1,j) = grid%psfc(i,j)
              END DO
            END DO
         END IF

         !  If we have the low-resolution surface elevation, stick that in the
         !  "input" locations of the 3d height.  We still have the "hi-res" topo
         !  stuck in the grid%ht array.  The grid%landmask if test is required as some sources
         !  have ZERO elevation over water (thank you very much).

         IF ( flag_soilhgt .EQ. 1) THEN
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
!                 IF ( grid%landmask(i,j) .GT. 0.5 ) THEN
                     grid%ght_gc(i,1,j) = grid%toposoil(i,j)
                     grid%ht_gc(i,j)= grid%toposoil(i,j)
!                 END IF
               END DO
           END DO
         END IF

         !  The number of vertical levels in the input data.  There is no staggering for
         !  different variables.

         num_metgrid_levels = grid%num_metgrid_levels

         !  Check for and semi-fix missing surface fields.

         IF ( grid%p_gc(its,num_metgrid_levels,jts) .LT. grid%p_gc(its,2,jts) ) THEN
            k = 2
         ELSE
            k = num_metgrid_levels
         END IF

         IF ( grid%t_gc(its,1,jts) .EQ. -1.E30 ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  grid%t_gc(i,1,j) = grid%t_gc(i,k,j)
               END DO
            END DO
            config_flags%use_surface = .FALSE.
            grid%use_surface = .FALSE.
            WRITE ( a_message , * ) 'Missing surface temp, replaced with closest level, use_surface set to false.'
            CALL wrf_message ( a_message ) 
         END IF

         IF ( grid%rh_gc(its,1,jts) .EQ. -1.E30 ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  grid%rh_gc(i,1,j) = grid%rh_gc(i,k,j)
               END DO
            END DO
            config_flags%use_surface = .FALSE.
            grid%use_surface = .FALSE.
            WRITE ( a_message , * ) 'Missing surface RH, replaced with closest level, use_surface set to false.'
            CALL wrf_message ( a_message ) 
         END IF

         IF ( grid%u_gc(its,1,jts) .EQ. -1.E30 ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO i = its, ite
                  grid%u_gc(i,1,j) = grid%u_gc(i,k,j)
               END DO
            END DO
            config_flags%use_surface = .FALSE.
            grid%use_surface = .FALSE.
            WRITE ( a_message , * ) 'Missing surface u wind, replaced with closest level, use_surface set to false.'
            CALL wrf_message ( a_message ) 
         END IF

         IF ( grid%v_gc(its,1,jts) .EQ. -1.E30 ) THEN
            DO j = jts, jte
               DO i = its, MIN(ite,ide-1)
                  grid%v_gc(i,1,j) = grid%v_gc(i,k,j)
               END DO
            END DO
            config_flags%use_surface = .FALSE.
            grid%use_surface = .FALSE.
            WRITE ( a_message , * ) 'Missing surface v wind, replaced with closest level, use_surface set to false.'
            CALL wrf_message ( a_message ) 
         END IF


         !  Compute the mixing ratio from the input relative humidity.

         IF ( ( flag_qv .NE. 1 ) .AND. ( flag_sh .NE. 1 ) ) THEN
            IF ( grid%p_gc(its,num_metgrid_levels,jts) .LT. grid%p_gc(its,2,jts) ) THEN
               k = 2
            ELSE
               k = num_metgrid_levels
            END IF

            CALL rh_to_mxrat (grid%rh_gc, grid%t_gc, grid%p_gc, grid%qv_gc ,         &
                              config_flags%rh2qv_wrt_liquid ,                        &
                              config_flags%qv_max_p_safe ,                           &
                              config_flags%qv_max_flag , config_flags%qv_max_value , &
                              config_flags%qv_min_p_safe ,                           &
                              config_flags%qv_min_flag , config_flags%qv_min_value , &
                              ids , ide , jds , jde , 1   , num_metgrid_levels ,     &
                              ims , ime , jms , jme , 1   , num_metgrid_levels ,     &
                              its , ite , jts , jte , 1   , num_metgrid_levels )

         ELSE IF ( flag_sh .EQ. 1 ) THEN
            IF ( grid%p_gc(its,num_metgrid_levels,jts) .LT. grid%p_gc(its,2,jts) ) THEN
               k = 2
            ELSE
               k = num_metgrid_levels
            END IF
            IF ( grid%sh_gc(its,kts,jts) .LT. 1.e-6 ) THEN
               DO j = jts, MIN(jte,jde-1)
                  DO i = its, MIN(ite,ide-1)
                     grid%sh_gc(i,1,j) = grid%sh_gc(i,k,j)
                  END DO
               END DO
            END IF

         END IF

         !  Some data sets do not provide a 3d geopotential height field.  

         IF ( grid%ght_gc(its,grid%num_metgrid_levels/2,jts) .LT. 1 ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO k = kts+1 , grid%num_metgrid_levels
                  DO i = its, MIN(ite,ide-1)
                     grid%ght_gc(i,k,j) = grid%ght_gc(i,k-1,j) - &
                        R_d / g * 0.5 * ( grid%t_gc(i,k  ,j) * ( 1 + 0.608 * grid%qv_gc(i,k  ,j) ) +   &
                                          grid%t_gc(i,k-1,j) * ( 1 + 0.608 * grid%qv_gc(i,k-1,j) ) ) * &
                        LOG ( grid%p_gc(i,k,j) / grid%p_gc(i,k-1,j) )
                  END DO
               END DO
            END DO
         END IF

         !  Assign surface fields with original input values.  If this is hybrid data,
         !  the values are not exactly representative.  However - this is only for
         !  plotting purposes and such at the 0h of the forecast, so we are not all that
         !  worried.

         DO j = jts, min(jde-1,jte)
            DO i = its, min(ide,ite)
               grid%u10(i,j)=grid%u_gc(i,1,j)
            END DO
         END DO

         DO j = jts, min(jde,jte)
            DO i = its, min(ide-1,ite)
               grid%v10(i,j)=grid%v_gc(i,1,j)
            END DO
         END DO

         DO j = jts, min(jde-1,jte)
            DO i = its, min(ide-1,ite)
               grid%t2(i,j)=grid%t_gc(i,1,j)
            END DO
         END DO

         IF ( flag_qv .EQ. 1 ) THEN
            DO j = jts, min(jde-1,jte)
               DO i = its, min(ide-1,ite)
                  grid%q2(i,j)=grid%qv_gc(i,1,j)
               END DO
            END DO
         END IF

         !  The requested ptop for real data cases.

         p_top_requested = grid%p_top_requested

         !  Compute the top pressure, grid%p_top.  For isobaric data, this is just the
         !  top level.  For the generalized vertical coordinate data, we find the
         !  max pressure on the top level.  We have to be careful of two things:
         !  1) the value has to be communicated, 2) the value can not increase
         !  at subsequent times from the initial value.

         IF ( internal_time_loop .EQ. 1 ) THEN
            CALL find_p_top ( grid%p_gc , grid%p_top , &
                              ids , ide , jds , jde , 1   , num_metgrid_levels , &
                              ims , ime , jms , jme , 1   , num_metgrid_levels , &
                              its , ite , jts , jte , 1   , num_metgrid_levels )

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
            grid%p_top = wrf_dm_max_real ( grid%p_top )
#endif

            !  Compare the requested grid%p_top with the value available from the input data.

            IF ( p_top_requested .LT. grid%p_top ) THEN
               print *,'p_top_requested = ',p_top_requested
               print *,'allowable grid%p_top in data   = ',grid%p_top
               CALL wrf_error_fatal ( 'p_top_requested < grid%p_top possible from data' )
            END IF

            !  The grid%p_top valus is the max of what is available from the data and the
            !  requested value.  We have already compared <, so grid%p_top is directly set to
            !  the value in the namelist.

            grid%p_top = p_top_requested

            !  For subsequent times, we have to remember what the grid%p_top for the first
            !  time was.  Why?  If we have a generalized vert coordinate, the grid%p_top value
            !  could fluctuate.

            p_top_save = grid%p_top

         ELSE
            CALL find_p_top ( grid%p_gc , grid%p_top , &
                              ids , ide , jds , jde , 1   , num_metgrid_levels , &
                              ims , ime , jms , jme , 1   , num_metgrid_levels , &
                              its , ite , jts , jte , 1   , num_metgrid_levels )

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
            grid%p_top = wrf_dm_max_real ( grid%p_top )
#endif
            IF ( grid%p_top .GT. p_top_save ) THEN
               print *,'grid%p_top from last time period = ',p_top_save
               print *,'grid%p_top from this time period = ',grid%p_top
               CALL wrf_error_fatal ( 'grid%p_top > previous value' )
            END IF
            grid%p_top = p_top_save
         ENDIF

         !  Get the monthly values interpolated to the current date for the traditional monthly
         !  fields of green-ness fraction and background albedo.

         CALL monthly_interp_to_date ( grid%greenfrac , current_date , grid%vegfra , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )

         CALL monthly_interp_to_date ( grid%albedo12m , current_date , grid%albbck , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )

         !  Get the min/max of each i,j for the monthly green-ness fraction.

         CALL monthly_min_max ( grid%greenfrac , grid%shdmin , grid%shdmax , &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte )

         !  The model expects the green-ness values in percent, not fraction.

         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)
              grid%vegfra(i,j) = grid%vegfra(i,j) * 100.
              grid%shdmax(i,j) = grid%shdmax(i,j) * 100.
              grid%shdmin(i,j) = grid%shdmin(i,j) * 100.
           END DO
         END DO

         !  The model expects the albedo fields as a fraction, not a percent.  Set the
         !  water values to 8%.

         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)
              grid%albbck(i,j) = grid%albbck(i,j) / 100.
              grid%snoalb(i,j) = grid%snoalb(i,j) / 100.
              IF ( grid%landmask(i,j) .LT. 0.5 ) THEN
                 grid%albbck(i,j) = 0.08
                 grid%snoalb(i,j) = 0.08
              END IF
           END DO
         END DO

         !  If the pressure levels in the middle of the atmosphere are upside down, then
         !  this is hybrid data.  Computing the new surface pressure should use sfcprs2.

         IF ( grid%p_gc(its,num_metgrid_levels/2,jts) .LT. grid%p_gc(its,num_metgrid_levels/2+1,jts) ) THEN
            config_flags%sfcp_to_sfcp = .TRUE.
         END IF

         !  Two ways to get the surface pressure.  1) If we have the low-res input surface
         !  pressure and the low-res topography, then we can do a simple hydrostatic
         !  relation.  2) Otherwise we compute the surface pressure from the sea-level
         !  pressure.
         !  Note that on output, grid%psfc is now hi-res.  The low-res surface pressure and
         !  elevation are grid%psfc_gc and grid%ht_gc (same as grid%ght_gc(k=1)).

         IF      ( ( flag_psfc    .EQ. 1 ) .AND. &
                   ( flag_soilhgt .EQ. 1 ) .AND. &
                   ( flag_slp     .EQ. 1 ) .AND. &
                   ( .NOT. config_flags%sfcp_to_sfcp ) ) THEN
            CALL sfcprs3(grid%ght_gc, grid%p_gc, grid%ht, &
                         grid%pslv_gc, grid%psfc, &
                         ids , ide , jds , jde , 1   , num_metgrid_levels , &
                         ims , ime , jms , jme , 1   , num_metgrid_levels , &
                         its , ite , jts , jte , 1   , num_metgrid_levels )
         ELSE IF ( ( flag_psfc    .EQ. 1 ) .AND. &
                   ( flag_soilhgt .EQ. 1 ) .AND. &
                   ( config_flags%sfcp_to_sfcp ) ) THEN
            CALL sfcprs2(grid%t_gc, grid%qv_gc, grid%ght_gc, grid%psfc_gc, grid%ht, &
                         grid%tavgsfc, grid%p_gc, grid%psfc, we_have_tavgsfc, &
                         ids , ide , jds , jde , 1   , num_metgrid_levels , &
                         ims , ime , jms , jme , 1   , num_metgrid_levels , &
                         its , ite , jts , jte , 1   , num_metgrid_levels )
         ELSE IF ( flag_slp     .EQ. 1 ) THEN
            CALL sfcprs (grid%t_gc, grid%qv_gc, grid%ght_gc, grid%pslv_gc, grid%ht, &
                         grid%tavgsfc, grid%p_gc, grid%psfc, we_have_tavgsfc, &
                         ids , ide , jds , jde , 1   , num_metgrid_levels , &
                         ims , ime , jms , jme , 1   , num_metgrid_levels , &
                         its , ite , jts , jte , 1   , num_metgrid_levels )
         ELSE
            WRITE(a_message,FMT='(3(A,I2),A,L1)') 'ERROR in psfc: flag_psfc = ',flag_psfc, &
                                               ', flag_soilhgt = ',flag_soilhgt , &
                                               ', flag_slp = ',flag_slp , & 
                                               ', sfcp_to_sfcp = ',config_flags%sfcp_to_sfcp 
            CALL wrf_message ( a_message ) 
            CALL wrf_error_fatal ( 'not enough info for a p sfc computation' )
         END IF

         !  If we have no input surface pressure, we'd better stick something in there.

         IF ( flag_psfc .NE. 1 ) THEN
            DO j = jts, MIN(jte,jde-1)
              DO i = its, MIN(ite,ide-1)
                 grid%psfc_gc(i,j) = grid%psfc(i,j)
                 grid%p_gc(i,1,j) = grid%psfc(i,j)
              END DO
            END DO
         END IF

         !  Integrate the mixing ratio to get the vapor pressure.

         CALL integ_moist ( grid%qv_gc , grid%p_gc , grid%pd_gc , grid%t_gc , grid%ght_gc , grid%intq_gc , &
                            ids , ide , jds , jde , 1   , num_metgrid_levels , &
                            ims , ime , jms , jme , 1   , num_metgrid_levels , &
                            its , ite , jts , jte , 1   , num_metgrid_levels )

         !  Compute the difference between the dry, total surface pressure (input) and the
         !  dry top pressure (constant).

         CALL p_dts ( grid%mu0 , grid%intq_gc , grid%psfc , grid%p_top , &
                      ids , ide , jds , jde , 1   , num_metgrid_levels , &
                      ims , ime , jms , jme , 1   , num_metgrid_levels , &
                      its , ite , jts , jte , 1   , num_metgrid_levels )

         !  Compute the dry, hydrostatic surface pressure.

         CALL p_dhs ( grid%pdhs , grid%ht , p00 , t00 , a , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

         !  Compute the eta levels if not defined already.

         IF ( grid%znw(1) .NE. 1.0 ) THEN

            eta_levels(1:kde) = model_config_rec%eta_levels(1:kde)
            max_dz            = model_config_rec%max_dz

            CALL compute_eta ( grid%znw , &
                               eta_levels , max_eta , max_dz , &
                               grid%p_top , g , p00 , cvpm , a , r_d , cp , t00 , p1000mb , t0 , tiso , &
                               ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte )
         END IF

         !  The input field is temperature, we want potential temp.

         CALL t_to_theta ( grid%t_gc , grid%p_gc , p00 , &
                           ids , ide , jds , jde , 1   , num_metgrid_levels , &
                           ims , ime , jms , jme , 1   , num_metgrid_levels , &
                           its , ite , jts , jte , 1   , num_metgrid_levels )

         IF ( flag_slp .EQ. 1 ) THEN

            !  On the eta surfaces, compute the dry pressure = mu eta, stored in
            !  grid%pb, since it is a pressure, and we don't need another kms:kme 3d
            !  array floating around.  The grid%pb array is re-computed as the base pressure
            !  later after the vertical interpolations are complete.

            CALL p_dry ( grid%mu0 , grid%znw , grid%p_top , grid%pb , want_full_levels , &
                         ids , ide , jds , jde , kds , kde , &
                         ims , ime , jms , jme , kms , kme , &
                         its , ite , jts , jte , kts , kte )

            !  All of the vertical interpolations are done in dry-pressure space.  The
            !  input data has had the moisture removed (grid%pd_gc).  The target levels (grid%pb)
            !  had the vapor pressure removed from the surface pressure, then they were
            !  scaled by the eta levels.

            interp_type = 2
            lagrange_order = grid%lagrange_order
            lowest_lev_from_sfc = .FALSE.
            use_levels_below_ground = .TRUE.
            use_surface = .TRUE.
            zap_close_levels = grid%zap_close_levels
            force_sfc_in_vinterp = 0
            t_extrap_type = grid%t_extrap_type
            extrap_type = 1

            !  For the height field, the lowest level pressure is the slp (approximately "dry").  The
            !  lowest level of the input height field (to be associated with slp) then is an array
            !  of zeros.

            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  grid%psfc_gc(i,j) = grid%pd_gc(i,1,j)
                  grid%pd_gc(i,1,j) = grid%pslv_gc(i,j) - ( grid%p_gc(i,1,j) - grid%pd_gc(i,1,j) )
                  grid%ht_gc(i,j) = grid%ght_gc(i,1,j)
                  grid%ght_gc(i,1,j) = 0.
               END DO
            END DO

            CALL vert_interp ( grid%ght_gc , grid%pd_gc , grid%ph0 , grid%pb , &
                               num_metgrid_levels , 'Z' , &
                               interp_type , lagrange_order , extrap_type , &
                               lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                               zap_close_levels , force_sfc_in_vinterp , &
                               ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte )

            !  Put things back to normal.

            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  grid%pd_gc(i,1,j) = grid%psfc_gc(i,j)
                  grid%ght_gc(i,1,j) = grid%ht_gc(i,j)
               END DO
            END DO

         END IF

         !  Now the rest of the variables on half-levels to inteprolate.

         CALL p_dry ( grid%mu0 , grid%znw , grid%p_top , grid%pb , want_half_levels , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

         interp_type = grid%interp_type
         lagrange_order = grid%lagrange_order
         lowest_lev_from_sfc = grid%lowest_lev_from_sfc
         use_levels_below_ground = grid%use_levels_below_ground
         use_surface = grid%use_surface
         zap_close_levels = grid%zap_close_levels
         force_sfc_in_vinterp = grid%force_sfc_in_vinterp
         t_extrap_type = grid%t_extrap_type
         extrap_type = grid%extrap_type

         !  Interpolate RH, diagnose Qv later when have temp and pressure.  Temporarily
         !  store this in the u_1 space, for later diagnosis into Qv and stored into moist.

         CALL vert_interp ( grid%rh_gc , grid%pd_gc , grid%u_1 , grid%pb , &
                            num_metgrid_levels , 'Q' , &
                            interp_type , lagrange_order , extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

         CALL vert_interp ( grid%t_gc , grid%pd_gc , grid%t_2               , grid%pb , &
                            num_metgrid_levels , 'T' , &
                            interp_type , lagrange_order , t_extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )
     
         !  Do not have full pressure on eta levels, get a first guess at Qv by using
         !  dry pressure.  The use of u_1 (rh) and v_1 (temperature) is temporary.
         !  We fix the approximation to Qv after the total pressure is available on
         !  eta surfaces.

         grid%v_1 = grid%t_2
         CALL theta_to_t ( grid%v_1 , grid%pb , p00 , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte )

         CALL rh_to_mxrat (grid%u_1, grid%v_1, grid%pb, moist(:,:,:,P_QV) ,       &
                           config_flags%rh2qv_wrt_liquid ,                        &
                           config_flags%qv_max_p_safe ,                           &
                           config_flags%qv_max_flag , config_flags%qv_max_value , &
                           config_flags%qv_min_p_safe ,                           &
                           config_flags%qv_min_flag , config_flags%qv_min_value , &
                           ids , ide , jds , jde , kds , kde ,                    &
                           ims , ime , jms , jme , kms , kme ,                    &
                           its , ite , jts , jte , kts , kte-1 )

#ifdef RUC_CLOUD
         !  Add -DRUC_CLOUD to ARCHFLAGS in configure.wrf file to activate the following code

         num_3d_m = num_moist
         num_3d_s = num_scalar

         IF ( flag_qr .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QR ) THEN
                  CALL vert_interp ( grid%qr_gc , grid%pd_gc , moist(:,:,:,P_QR) , grid%pb , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , lagrange_order , extrap_type , &
                                     lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF

         IF ( flag_qc .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QC ) THEN
                  CALL vert_interp ( grid%qc_gc , grid%pd_gc , moist(:,:,:,P_QC) , grid%pb , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , lagrange_order , extrap_type , &
                                     lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF

         IF ( flag_qi .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QI ) THEN
                  CALL vert_interp ( grid%qi_gc , grid%pd_gc , moist(:,:,:,P_QI) , grid%pb , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , lagrange_order , extrap_type , &
                                     lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF

         IF ( flag_qs .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QS ) THEN
                  CALL vert_interp ( grid%qs_gc , grid%pd_gc , moist(:,:,:,P_QS) , grid%pb , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , lagrange_order , extrap_type , &
                                     lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF

         IF ( flag_qg .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QG ) THEN
                  CALL vert_interp ( grid%qg_gc , grid%pd_gc , moist(:,:,:,P_QG) , grid%pb , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , lagrange_order , extrap_type , &
                                     lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF

         IF ( flag_qni .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_s
               IF ( im .EQ. P_QNI ) THEN
                  CALL vert_interp ( grid%qni_gc , grid%pd_gc , scalar(:,:,:,P_QNI) , grid%pb , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , lagrange_order , extrap_type , &
                                     lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , &
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
         !  pressure field on input (grid%pd_gc) and the pressure of the new coordinate
         !  (grid%pb) are both communicated with an 8 stencil.

#   include "HALO_EM_VINTERP_UV_1.inc"
#endif

         CALL vert_interp ( grid%u_gc , grid%pd_gc , grid%u_2               , grid%pb , &
                            num_metgrid_levels , 'U' , &
                            interp_type , lagrange_order , extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

         CALL vert_interp ( grid%v_gc , grid%pd_gc , grid%v_2               , grid%pb , &
                            num_metgrid_levels , 'V' , &
                            interp_type , lagrange_order , extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

      END IF     !   <----- END OF VERTICAL INTERPOLATION PART ---->

      ! Set the temperature of the inland lakes to tavgsfc if the temperature is available
      ! and islake is > num_veg_cat

      num_veg_cat      = SIZE ( grid%landusef , DIM=2 )
      CALL nl_get_iswater ( grid%id , grid%iswater )
      CALL nl_get_islake  ( grid%id , grid%islake )

      IF ( grid%islake < 0 ) THEN
         CALL wrf_debug ( 0 , 'Old data, no inland lake information')
      ELSE
         IF ( we_have_tavgsfc ) THEN

            CALL wrf_debug ( 0 , 'Using inland lakes with average surface temperature')
            DO j=jts,MIN(jde-1,jte)
               DO i=its,MIN(ide-1,ite)
                  IF ( grid%landusef(i,grid%islake,j) >= 0.5 ) THEN
                     grid%sst(i,j) = grid%tavgsfc(i,j)
                     grid%tsk(i,j) = grid%tavgsfc(i,j)
                  END IF
               END DO
            END DO

         ELSE     ! We don't have tavgsfc

            CALL wrf_debug ( 0 , 'No average surface temperature for use with inland lakes')

         END IF
         DO j=jts,MIN(jde-1,jte)
            DO i=its,MIN(ide-1,ite)
               grid%landusef(i,grid%iswater,j) = grid%landusef(i,grid%iswater,j) + &
                                                 grid%landusef(i,grid%islake,j)
               grid%landusef(i,grid%islake,j) = 0.
            END DO
         END DO

      END IF

      !  Save the grid%tsk field for later use in the sea ice surface temperature
      !  for the Noah LSM scheme.

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
            grid%tsk_save(i,j) = grid%tsk(i,j)
         END DO
      END DO

      !  Protect against bad grid%tsk values over water by supplying grid%sst (if it is
      !  available, and if the grid%sst is reasonable).

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( grid%landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) .AND. &
                 ( grid%sst(i,j) .GT. 170. ) .AND. ( grid%sst(i,j) .LT. 400. ) ) THEN
               grid%tsk(i,j) = grid%sst(i,j)
            ENDIF
         END DO
      END DO

      !  Take the data from the input file and store it in the variables that
      !  use the WRF naming and ordering conventions.

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
            IF ( grid%snow(i,j) .GE. 10. ) then
               grid%snowc(i,j) = 1.
            ELSE
               grid%snowc(i,j) = 0.0
            END IF
         END DO
      END DO

      !  Set flag integers for presence of snowh and soilw fields

      grid%ifndsnowh = flag_snowh
      IF (num_sw_levels_input .GE. 1) THEN
         grid%ifndsoilw = 1
      ELSE
         grid%ifndsoilw = 0
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

         CASE (PXLSMSCHEME)
            IF ( num_st_levels_input .LT. 2 ) THEN
               CALL wrf_error_fatal ( 'Not enough soil temperature data for P-X LSM scheme.')
            END IF

      END SELECT enough_data

      interpolate_soil_tmw : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

         CASE ( SLABSCHEME , LSMSCHEME , RUCLSMSCHEME, PXLSMSCHEME )
            CALL process_soil_real ( grid%tsk , grid%tmn , grid%tavgsfc,  &
                                  grid%landmask , grid%sst , grid%ht, grid%toposoil, &
                                  st_input , sm_input , sw_input , &
                                  st_levels_input , sm_levels_input , sw_levels_input , &
                                  grid%zs , grid%dzs , grid%tslb , grid%smois , grid%sh2o , &
                                  flag_sst , flag_tavgsfc, &
                                  flag_soilhgt, flag_soil_layers, flag_soil_levels, &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  its , ite , jts , jte , kts , kte , &
                                  model_config_rec%sf_surface_physics(grid%id) , &
                                  model_config_rec%num_soil_layers , &
                                  model_config_rec%real_data_init_type , &
                                  num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                  num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc )

      END SELECT interpolate_soil_tmw

      !  surface_input_source=1 => use data from static file (fractional category as input)
      !  surface_input_source=2 => use data from grib file (dominant category as input)
      !  surface_input_source=3 => use dominant data from static file (dominant category as input)

      IF ( config_flags%surface_input_source .EQ. 1 ) THEN
         grid%vegcat (its,jts) = 0
         grid%soilcat(its,jts) = 0
      END IF

      !  Generate the vegetation and soil category information from the fractional input
      !  data, or use the existing dominant category fields if they exist.

      IF ( ( grid%soilcat(its,jts) .LT. 0.5 ) .AND. ( grid%vegcat(its,jts) .LT. 0.5 ) ) THEN

         num_veg_cat      = SIZE ( grid%landusef , DIM=2 )
         num_soil_top_cat = SIZE ( grid%soilctop , DIM=2 )
         num_soil_bot_cat = SIZE ( grid%soilcbot , DIM=2 )

         CALL process_percent_cat_new ( grid%landmask , &
                                    grid%landusef , grid%soilctop , grid%soilcbot , &
                                    grid%isltyp , grid%ivgtyp , &
                                    num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                    ids , ide , jds , jde , kds , kde , &
                                    ims , ime , jms , jme , kms , kme , &
                                    its , ite , jts , jte , kts , kte , &
                                    model_config_rec%iswater(grid%id) )

         !  Make all the veg/soil parms the same so as not to confuse the developer.

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               grid%vegcat(i,j)  = grid%ivgtyp(i,j)
               grid%soilcat(i,j) = grid%isltyp(i,j)
            END DO
         END DO

      ELSE IF ( config_flags%surface_input_source .EQ. 2 ) THEN

         !  Do we have dominant soil and veg data from the input already?

         IF ( grid%soilcat(its,jts) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  grid%isltyp(i,j) = NINT( grid%soilcat(i,j) )
               END DO
            END DO
         END IF
         IF ( grid%vegcat(its,jts) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  grid%ivgtyp(i,j) = NINT( grid%vegcat(i,j) )
               END DO
            END DO
         END IF

      ELSE IF ( config_flags%surface_input_source .EQ. 3 ) THEN

         !  Do we have dominant soil and veg data from the static input already?

         IF ( grid%sct_dom_gc(its,jts) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  grid%isltyp(i,j) = NINT( grid%sct_dom_gc(i,j) )
                  grid%soilcat(i,j) = grid%isltyp(i,j)
               END DO
            END DO
         ELSE
            WRITE ( a_message , * ) 'You have set surface_input_source = 3, but your geogrid data does not have valid dominant soil data.'
            CALL wrf_error_fatal ( a_message ) 
         END IF
         IF ( grid%lu_index(its,jts) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  grid%ivgtyp(i,j) = NINT( grid%lu_index(i,j) )
                  grid%vegcat(i,j) = grid%ivgtyp(i,j)
               END DO
            END DO
         ELSE
            WRITE ( a_message , * ) 'You have set surface_input_source = 3, but your geogrid data does not have valid dominant land use data.'
            CALL wrf_error_fatal ( a_message ) 
         END IF

      END IF

      !  Adjustments for the seaice field PRIOR to the grid%tslb computations.  This is
      !  is for the 5-layer scheme.

      num_veg_cat      = SIZE ( grid%landusef , DIM=2 )
      num_soil_top_cat = SIZE ( grid%soilctop , DIM=2 )
      num_soil_bot_cat = SIZE ( grid%soilcbot , DIM=2 )
      CALL nl_get_seaice_threshold ( grid%id , grid%seaice_threshold )
      CALL nl_get_isice ( grid%id , grid%isice )
      CALL nl_get_iswater ( grid%id , grid%iswater )
      CALL adjust_for_seaice_pre ( grid%xice , grid%landmask , grid%tsk , grid%ivgtyp , grid%vegcat , grid%lu_index , &
                                   grid%xland , grid%landusef , grid%isltyp , grid%soilcat , grid%soilctop , &
                                   grid%soilcbot , grid%tmn , &
                                   grid%seaice_threshold , &
                                   config_flags%fractional_seaice, &
                                   num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                   grid%iswater , grid%isice , &
                                   model_config_rec%sf_surface_physics(grid%id) , &
                                   ids , ide , jds , jde , kds , kde , &
                                   ims , ime , jms , jme , kms , kme , &
                                   its , ite , jts , jte , kts , kte )

      !  Land use assignment.

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            grid%lu_index(i,j) = grid%ivgtyp(i,j)
            IF ( grid%lu_index(i,j) .NE. model_config_rec%iswater(grid%id) ) THEN
               grid%landmask(i,j) = 1
               grid%xland(i,j)    = 1
            ELSE
               grid%landmask(i,j) = 0
               grid%xland(i,j)    = 2
            END IF
         END DO
      END DO

      !  Fix grid%tmn and grid%tsk.

      fix_tsk_tmn : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

         CASE ( SLABSCHEME , LSMSCHEME , RUCLSMSCHEME, PXLSMSCHEME )
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  IF ( ( grid%landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) .AND. &
                       ( grid%sst(i,j) .GT. 170. ) .AND. ( grid%sst(i,j) .LT. 400. ) ) THEN
                     grid%tmn(i,j) = grid%sst(i,j)
                     grid%tsk(i,j) = grid%sst(i,j)
                  ELSE IF ( grid%landmask(i,j) .LT. 0.5 ) THEN
                     grid%tmn(i,j) = grid%tsk(i,j)
                  END IF
               END DO
            END DO
      END SELECT fix_tsk_tmn

      !  Is the grid%tsk reasonable?

      IF ( internal_time_loop .NE. 1 ) THEN
         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               IF ( grid%tsk(i,j) .LT. 170 .or. grid%tsk(i,j) .GT. 400. ) THEN
                  grid%tsk(i,j) = grid%t_2(i,1,j)
               END IF
            END DO
         END DO
      ELSE
         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               IF ( grid%tsk(i,j) .LT. 170 .or. grid%tsk(i,j) .GT. 400. ) THEN
                  print *,'error in the grid%tsk'
                  print *,'i,j=',i,j
                  print *,'grid%landmask=',grid%landmask(i,j)
                  print *,'grid%tsk, grid%sst, grid%tmn=',grid%tsk(i,j),grid%sst(i,j),grid%tmn(i,j)
                  if(grid%tmn(i,j).gt.170. .and. grid%tmn(i,j).lt.400.)then
                     grid%tsk(i,j)=grid%tmn(i,j)
                  else if(grid%sst(i,j).gt.170. .and. grid%sst(i,j).lt.400.)then
                     grid%tsk(i,j)=grid%sst(i,j)
                  else
                     CALL wrf_error_fatal ( 'grid%tsk unreasonable' )
                  end if
               END IF
            END DO
         END DO
      END IF

      !  Is the grid%tmn reasonable?

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( ( grid%tmn(i,j) .LT. 170. ) .OR. ( grid%tmn(i,j) .GT. 400. ) ) &
               .AND. ( grid%landmask(i,j) .GT. 0.5 ) ) THEN
               IF ( model_config_rec%sf_surface_physics(grid%id) .NE. LSMSCHEME ) THEN
                  print *,'error in the grid%tmn'
                  print *,'i,j=',i,j
                  print *,'grid%landmask=',grid%landmask(i,j)
                  print *,'grid%tsk, grid%sst, grid%tmn=',grid%tsk(i,j),grid%sst(i,j),grid%tmn(i,j)
               END IF

               if(grid%tsk(i,j).gt.170. .and. grid%tsk(i,j).lt.400.)then
                  grid%tmn(i,j)=grid%tsk(i,j)
               else if(grid%sst(i,j).gt.170. .and. grid%sst(i,j).lt.400.)then
                  grid%tmn(i,j)=grid%sst(i,j)
               else
                  CALL wrf_error_fatal ( 'grid%tmn unreasonable' )
               endif
            END IF
         END DO
      END DO
   

      !  Minimum soil values, residual, from RUC LSM scheme.  For input from Noah or EC, and using
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
               IF      ( flag_soil_layers == 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( (grid%landmask(i,j).gt.0.5) .and. ( grid%tslb(i,1,j) .gt. 170 ) .and. &
                             ( grid%tslb(i,1,j) .lt. 400 ) .and. ( grid%smois(i,1,j) .lt. 0.005 ) ) then
                           print *,'Noah -> Noah: bad soil moisture at i,j = ',i,j,grid%smois(i,:,j)
                           iicount = iicount + 1
                           grid%smois(i,:,j) = 0.005
                        END IF
                     END DO
                  END DO
                  IF ( iicount .GT. 0 ) THEN
                     print *,'Noah -> Noah: total number of small soil moisture locations = ',iicount
                  END IF
               ELSE IF ( flag_soil_levels == 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        grid%smois(i,:,j) = MAX ( grid%smois(i,:,j) + lqmi(grid%isltyp(i,j)) , 0.005 )
                     END DO
                  END DO
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( (grid%landmask(i,j).gt.0.5) .and. ( grid%tslb(i,1,j) .gt. 170 ) .and. &
                             ( grid%tslb(i,1,j) .lt. 400 ) .and. ( grid%smois(i,1,j) .lt. 0.005 ) ) then
                           print *,'RUC -> Noah: bad soil moisture at i,j = ',i,j,grid%smois(i,:,j)
                           iicount = iicount + 1
                           grid%smois(i,:,j) = 0.005
                        END IF
                     END DO
                  END DO
                  IF ( iicount .GT. 0 ) THEN
                     print *,'RUC -> Noah: total number of small soil moisture locations = ',iicount
                  END IF
               END IF

            CASE ( RUCLSMSCHEME )
               iicount = 0
               IF      ( flag_soil_layers == 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        grid%smois(i,:,j) = MAX ( grid%smois(i,:,j) - lqmi(grid%isltyp(i,j)) , 0.005 )
                     END DO
                  END DO
               ELSE IF ( flag_soil_levels == 1 ) THEN
                  ! no op
               END IF

             CASE ( PXLSMSCHEME )
               iicount = 0
               IF ( flag_soil_layers == 1 ) THEN
                  ! no op
               ELSE IF ( flag_soil_levels == 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        grid%smois(i,:,j) = MAX ( grid%smois(i,:,j) + lqmi(grid%isltyp(i,j)) , 0.005 )
                     END DO
                  END DO
               END IF

         END SELECT account_for_zero_soil_moisture
      END IF

      !  Is the grid%tslb reasonable?

      IF ( internal_time_loop .NE. 1 ) THEN
         DO j = jts, MIN(jde-1,jte)
            DO ns = 1 , model_config_rec%num_soil_layers
               DO i = its, MIN(ide-1,ite)
                  IF ( grid%tslb(i,ns,j) .LT. 170 .or. grid%tslb(i,ns,j) .GT. 400. ) THEN
                     grid%tslb(i,ns,j) = grid%t_2(i,1,j)
                     grid%smois(i,ns,j) = 0.3
                  END IF
               END DO
            END DO
         END DO
      ELSE
         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               IF ( ( ( grid%tslb(i,1,j) .LT. 170. ) .OR. ( grid%tslb(i,1,j) .GT. 400. ) ) .AND. &
                       ( grid%landmask(i,j) .GT. 0.5 ) ) THEN
                     IF ( ( model_config_rec%sf_surface_physics(grid%id) .NE. LSMSCHEME    ) .AND. &
                          ( model_config_rec%sf_surface_physics(grid%id) .NE. RUCLSMSCHEME ).AND. &
                          ( model_config_rec%sf_surface_physics(grid%id) .NE. PXLSMSCHEME ) ) THEN
                        print *,'error in the grid%tslb'
                        print *,'i,j=',i,j
                        print *,'grid%landmask=',grid%landmask(i,j)
                        print *,'grid%tsk, grid%sst, grid%tmn=',grid%tsk(i,j),grid%sst(i,j),grid%tmn(i,j)
                        print *,'grid%tslb = ',grid%tslb(i,:,j)
                        print *,'old grid%smois = ',grid%smois(i,:,j)
                        grid%smois(i,1,j) = 0.3
                        grid%smois(i,2,j) = 0.3
                        grid%smois(i,3,j) = 0.3
                        grid%smois(i,4,j) = 0.3
                     END IF

                     IF ( (grid%tsk(i,j).GT.170. .AND. grid%tsk(i,j).LT.400.) .AND. &
                          (grid%tmn(i,j).GT.170. .AND. grid%tmn(i,j).LT.400.) ) THEN
                        fake_soil_temp : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )
                           CASE ( SLABSCHEME )
                              DO ns = 1 , model_config_rec%num_soil_layers
                                 grid%tslb(i,ns,j) = ( grid%tsk(i,j)*(3.0 - grid%zs(ns)) + &
                                                       grid%tmn(i,j)*(0.0 - grid%zs(ns)) ) /(3.0 - 0.0)
                              END DO
                           CASE ( LSMSCHEME , RUCLSMSCHEME, PXLSMSCHEME )
                              CALL wrf_error_fatal ( 'Assigned constant soil moisture to 0.3, stopping')
                              DO ns = 1 , model_config_rec%num_soil_layers
                                 grid%tslb(i,ns,j) = ( grid%tsk(i,j)*(3.0 - grid%zs(ns)) + &
                                                       grid%tmn(i,j)*(0.0 - grid%zs(ns)) ) /(3.0 - 0.0)
                              END DO
                        END SELECT fake_soil_temp
                     else if(grid%tsk(i,j).gt.170. .and. grid%tsk(i,j).lt.400.)then
                        CALL wrf_error_fatal ( 'grid%tslb unreasonable 1' )
                        DO ns = 1 , model_config_rec%num_soil_layers
                           grid%tslb(i,ns,j)=grid%tsk(i,j)
                        END DO
                     else if(grid%sst(i,j).gt.170. .and. grid%sst(i,j).lt.400.)then
                        CALL wrf_error_fatal ( 'grid%tslb unreasonable 2' )
                        DO ns = 1 , model_config_rec%num_soil_layers
                           grid%tslb(i,ns,j)=grid%sst(i,j)
                        END DO
                     else if(grid%tmn(i,j).gt.170. .and. grid%tmn(i,j).lt.400.)then
                        CALL wrf_error_fatal ( 'grid%tslb unreasonable 3' )
                        DO ns = 1 , model_config_rec%num_soil_layers
                           grid%tslb(i,ns,j)=grid%tmn(i,j)
                        END DO
                     else
                        CALL wrf_error_fatal ( 'grid%tslb unreasonable 4' )
                     endif
               END IF
            END DO
         END DO
      END IF

      !  Adjustments for the seaice field AFTER the grid%tslb computations.  This is
      !  is for the Noah LSM scheme.

      num_veg_cat      = SIZE ( grid%landusef , DIM=2 )
      num_soil_top_cat = SIZE ( grid%soilctop , DIM=2 )
      num_soil_bot_cat = SIZE ( grid%soilcbot , DIM=2 )
      CALL nl_get_seaice_threshold ( grid%id , grid%seaice_threshold )
      CALL nl_get_isice ( grid%id , grid%isice )
      CALL nl_get_iswater ( grid%id , grid%iswater )
      CALL adjust_for_seaice_post ( grid%xice , grid%landmask , grid%tsk , grid%tsk_save , &
                                    grid%ivgtyp , grid%vegcat , grid%lu_index , &
                                    grid%xland , grid%landusef , grid%isltyp , grid%soilcat ,  &
                                    grid%soilctop , &
                                    grid%soilcbot , grid%tmn , grid%vegfra , &
                                    grid%tslb , grid%smois , grid%sh2o , &
                                    grid%seaice_threshold , &
                                    grid%sst,flag_sst, &
                                    config_flags%fractional_seaice, &
                                    num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                    model_config_rec%num_soil_layers , &
                                    grid%iswater , grid%isice , &
                                    model_config_rec%sf_surface_physics(grid%id) , &
                                    ids , ide , jds , jde , kds , kde , &
                                    ims , ime , jms , jme , kms , kme , &
                                    its , ite , jts , jte , kts , kte )

      !  Let us make sure (again) that the grid%landmask and the veg/soil categories match.

oops1=0
oops2=0
      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( ( grid%landmask(i,j) .LT. 0.5 ) .AND. &
                   ( grid%ivgtyp(i,j) .NE. config_flags%iswater .OR. grid%isltyp(i,j) .NE. 14 ) ) .OR. &
                 ( ( grid%landmask(i,j) .GT. 0.5 ) .AND. &
                   ( grid%ivgtyp(i,j) .EQ. config_flags%iswater .OR. grid%isltyp(i,j) .EQ. 14 ) ) ) THEN
               IF ( grid%tslb(i,1,j) .GT. 1. ) THEN
oops1=oops1+1
                  grid%ivgtyp(i,j) = 5
                  grid%isltyp(i,j) = 8
                  grid%landmask(i,j) = 1
                  grid%xland(i,j) = 1
               ELSE IF ( grid%sst(i,j) .GT. 1. ) THEN
oops2=oops2+1
                  grid%ivgtyp(i,j) = config_flags%iswater
                  grid%isltyp(i,j) = 14
                  grid%landmask(i,j) = 0
                  grid%xland(i,j) = 2
               ELSE
                  print *,'the grid%landmask and soil/veg cats do not match'
                  print *,'i,j=',i,j
                  print *,'grid%landmask=',grid%landmask(i,j)
                  print *,'grid%ivgtyp=',grid%ivgtyp(i,j)
                  print *,'grid%isltyp=',grid%isltyp(i,j)
                  print *,'iswater=', config_flags%iswater
                  print *,'grid%tslb=',grid%tslb(i,:,j)
                  print *,'grid%sst=',grid%sst(i,j)
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
! fill grid%sst array with grid%tsk if missing in real input (needed for time-varying grid%sst in wrf)
      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
           IF ( flag_sst .NE. 1 ) THEN
             grid%sst(i,j) = grid%tsk(i,j)
           ENDIF
         END DO
      END DO
!tgs set snoalb to land value if the water point is covered with ice
      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
           IF ( grid%ivgtyp(i,j) .EQ. config_flags%isice) THEN
             grid%snoalb(i,j) = 0.75
           ENDIF
         END DO
      END DO

      !  From the full level data, we can get the half levels, reciprocals, and layer
      !  thicknesses.  These are all defined at half level locations, so one less level.
      !  We allow the vertical coordinate to *accidently* come in upside down.  We want
      !  the first full level to be the ground surface.

      !  Check whether grid%znw (full level) data are truly full levels. If not, we need to adjust them
      !  to be full levels.
      !  in this test, we check if grid%znw(1) is neither 0 nor 1 (within a tolerance of 10**-5)

      were_bad = .false.
      IF ( ( (grid%znw(1).LT.(1-1.E-5) ) .OR. ( grid%znw(1).GT.(1+1.E-5) ) ).AND. &
           ( (grid%znw(1).LT.(0-1.E-5) ) .OR. ( grid%znw(1).GT.(0+1.E-5) ) ) ) THEN
         were_bad = .true.
         print *,'Your grid%znw input values are probably half-levels. '
         print *,grid%znw
         print *,'WRF expects grid%znw values to be full levels. '
         print *,'Adjusting now to full levels...'
         !  We want to ignore the first value if it's negative
         IF (grid%znw(1).LT.0) THEN
            grid%znw(1)=0
         END IF
         DO k=2,kde
            grid%znw(k)=2*grid%znw(k)-grid%znw(k-1)
         END DO
      END IF

      !  Let's check our changes

      IF ( ( ( grid%znw(1) .LT. (1-1.E-5) ) .OR. ( grid%znw(1) .GT. (1+1.E-5) ) ).AND. &
           ( ( grid%znw(1) .LT. (0-1.E-5) ) .OR. ( grid%znw(1) .GT. (0+1.E-5) ) ) ) THEN
         print *,'The input grid%znw height values were half-levels or erroneous. '
         print *,'Attempts to treat the values as half-levels and change them '
         print *,'to valid full levels failed.'
         CALL wrf_error_fatal("bad grid%znw values from input files")
      ELSE IF ( were_bad ) THEN
         print *,'...adjusted. grid%znw array now contains full eta level values. '
      ENDIF

      IF ( grid%znw(1) .LT. grid%znw(kde) ) THEN
         DO k=1, kde/2
            hold_znw = grid%znw(k)
            grid%znw(k)=grid%znw(kde+1-k)
            grid%znw(kde+1-k)=hold_znw
         END DO
      END IF

      DO k=1, kde-1
         grid%dnw(k) = grid%znw(k+1) - grid%znw(k)
         grid%rdnw(k) = 1./grid%dnw(k)
         grid%znu(k) = 0.5*(grid%znw(k+1)+grid%znw(k))
      END DO

      !  Now the same sort of computations with the half eta levels, even ANOTHER
      !  level less than the one above.

      DO k=2, kde-1
         grid%dn(k) = 0.5*(grid%dnw(k)+grid%dnw(k-1))
         grid%rdn(k) = 1./grid%dn(k)
         grid%fnp(k) = .5* grid%dnw(k  )/grid%dn(k)
         grid%fnm(k) = .5* grid%dnw(k-1)/grid%dn(k)
      END DO

      !  Scads of vertical coefficients.

      cof1 = (2.*grid%dn(2)+grid%dn(3))/(grid%dn(2)+grid%dn(3))*grid%dnw(1)/grid%dn(2)
      cof2 =     grid%dn(2)        /(grid%dn(2)+grid%dn(3))*grid%dnw(1)/grid%dn(3)

      grid%cf1  = grid%fnp(2) + cof1
      grid%cf2  = grid%fnm(2) - cof1 - cof2
      grid%cf3  = cof2

      grid%cfn  = (.5*grid%dnw(kde-1)+grid%dn(kde-1))/grid%dn(kde-1)
      grid%cfn1 = -.5*grid%dnw(kde-1)/grid%dn(kde-1)

      !  Inverse grid distances.

      grid%rdx = 1./config_flags%dx
      grid%rdy = 1./config_flags%dy

      !  Some of the many weird geopotential initializations that we'll see today: grid%ph0 is total,
      !  and grid%ph_2 is a perturbation from the base state geopotential.  We set the base geopotential
      !  at the lowest level to terrain elevation * gravity.

      DO j=jts,jte
         DO i=its,ite
            grid%ph0(i,1,j) = grid%ht(i,j) * g
            grid%ph_2(i,1,j) = 0.
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

            p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht(i,j)/a/r_d ) **0.5 )


            DO k = 1, kte-1
               grid%php(i,k,j) = grid%znw(k)*(p_surf - grid%p_top) + grid%p_top ! temporary, full lev base pressure
               grid%pb(i,k,j) = grid%znu(k)*(p_surf - grid%p_top) + grid%p_top
               temp = MAX ( tiso, t00 + A*LOG(grid%pb(i,k,j)/p00) )
!              temp =             t00 + A*LOG(grid%pb(i,k,j)/p00)
               grid%t_init(i,k,j) = temp*(p00/grid%pb(i,k,j))**(r_d/cp) - t0
               grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
            END DO

            !  Base state mu is defined as base state surface pressure minus grid%p_top

            grid%mub(i,j) = p_surf - grid%p_top

            !  Dry surface pressure is defined as the following (this mu is from the input file
            !  computed from the dry pressure).  Here the dry pressure is just reconstituted.

            pd_surf = grid%mu0(i,j) + grid%p_top

            !  Integrate base geopotential, starting at terrain elevation.  This assures that
            !  the base state is in exact hydrostatic balance with respect to the model equations.
            !  This field is on full levels.

            grid%phb(i,1,j) = grid%ht(i,j) * g
            DO k  = 2,kte
               grid%phb(i,k,j) = grid%phb(i,k-1,j) - grid%dnw(k-1)*grid%mub(i,j)*grid%alb(i,k-1,j)
            END DO
         END DO
      END DO

      !  Fill in the outer rows and columns to allow us to be sloppy.

      IF ( ite .EQ. ide ) THEN
      i = ide
      DO j = jts, MIN(jde-1,jte)
         grid%mub(i,j) = grid%mub(i-1,j)
         grid%mu_2(i,j) = grid%mu_2(i-1,j)
         DO k = 1, kte-1
            grid%pb(i,k,j) = grid%pb(i-1,k,j)
            grid%t_init(i,k,j) = grid%t_init(i-1,k,j)
            grid%alb(i,k,j) = grid%alb(i-1,k,j)
         END DO
         DO k = 1, kte
            grid%phb(i,k,j) = grid%phb(i-1,k,j)
         END DO
      END DO
      END IF

      IF ( jte .EQ. jde ) THEN
      j = jde
      DO i = its, ite
         grid%mub(i,j) = grid%mub(i,j-1)
         grid%mu_2(i,j) = grid%mu_2(i,j-1)
         DO k = 1, kte-1
            grid%pb(i,k,j) = grid%pb(i,k,j-1)
            grid%t_init(i,k,j) = grid%t_init(i,k,j-1)
            grid%alb(i,k,j) = grid%alb(i,k,j-1)
         END DO
         DO k = 1, kte
            grid%phb(i,k,j) = grid%phb(i,k,j-1)
         END DO
      END DO
      END IF

      !  Compute the perturbation dry pressure (grid%mub + grid%mu_2 + ptop = dry grid%psfc).

      DO j = jts, min(jde-1,jte)
         DO i = its, min(ide-1,ite)
            grid%mu_2(i,j) = grid%mu0(i,j) - grid%mub(i,j)
         END DO
      END DO

      !  Fill in the outer rows and columns to allow us to be sloppy.

      IF ( ite .EQ. ide ) THEN
      i = ide
      DO j = jts, MIN(jde-1,jte)
         grid%mu_2(i,j) = grid%mu_2(i-1,j)
      END DO
      END IF

      IF ( jte .EQ. jde ) THEN
      j = jde
      DO i = its, ite
         grid%mu_2(i,j) = grid%mu_2(i,j-1)
      END DO
      END IF

      lev500 = 0
      DO j = jts, min(jde-1,jte)
         DO i = its, min(ide-1,ite)

            !  Assign the potential temperature (perturbation from t0) and qv on all the mass
            !  point locations.

            DO k =  1 , kde-1
               grid%t_2(i,k,j)          = grid%t_2(i,k,j) - t0
            END DO

            dpmu = 10001.
            loop_count = 0

            DO WHILE ( ( ABS(dpmu) .GT. 10. ) .AND. &
                       ( loop_count .LT. 5 ) )

               loop_count = loop_count + 1

               !  Integrate the hydrostatic equation (from the RHS of the bigstep vertical momentum
               !  equation) down from the top to get the pressure perturbation.  First get the pressure
               !  perturbation, moisture, and inverse density (total and perturbation) at the top-most level.

               k = kte-1

               qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k,j,P_QV))
               qvf2 = 1./(1.+qvf1)
               qvf1 = qvf1*qvf2

               grid%p(i,k,j) = - 0.5*(grid%mu_2(i,j)+qvf1*grid%mub(i,j))/grid%rdnw(k)/qvf2
               qvf = 1. + rvovrd*moist(i,k,j,P_QV)
               grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_2(i,k,j)+t0)*qvf&
                                 *(((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
               grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
               grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)

               !  Now, integrate down the column to compute the pressure perturbation, and diagnose the two
               !  inverse density fields (total and perturbation).

               DO k=kte-2,1,-1
                  qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k+1,j,P_QV))
                  qvf2 = 1./(1.+qvf1)
                  qvf1 = qvf1*qvf2
                  grid%p(i,k,j) = grid%p(i,k+1,j) - (grid%mu_2(i,j) + qvf1*grid%mub(i,j))/qvf2/grid%rdn(k+1)
                  qvf = 1. + rvovrd*moist(i,k,j,P_QV)
                  grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_2(i,k,j)+t0)*qvf* &
                              (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
                  grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
                  grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)
               END DO

#if 1
               !  This is the hydrostatic equation used in the model after the small timesteps.  In
               !  the model, grid%al (inverse density) is computed from the geopotential.

               DO k  = 2,kte
                  grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) - &
                                grid%dnw(k-1) * ( (grid%mub(i,j)+grid%mu_2(i,j))*grid%al(i,k-1,j) &
                              + grid%mu_2(i,j)*grid%alb(i,k-1,j) )
                  grid%ph0(i,k,j) = grid%ph_2(i,k,j) + grid%phb(i,k,j)
               END DO
#else
               !  Get the perturbation geopotential from the 3d height array from WPS.

               DO k  = 2,kte
                  grid%ph_2(i,k,j) = grid%ph0(i,k,j)*g - grid%phb(i,k,j)
               END DO
#endif

               !  Adjust the column pressure so that the computed 500 mb height is close to the
               !  input value (of course, not when we are doing hybrid input).

               IF ( ( flag_metgrid .EQ. 1 ) .AND. ( i .EQ. its ) .AND. ( j .EQ. jts ) ) THEN
                  DO k = 1 , num_metgrid_levels
                     IF ( ABS ( grid%p_gc(i,k,j) - 50000. ) .LT. 1. ) THEN
                        lev500 = k
                        EXIT
                     END IF
                  END DO
               END IF

               !  We only do the adjustment of height if we have the input data on pressure
               !  surfaces, and folks have asked to do this option.

               IF ( ( flag_metgrid .EQ. 1 ) .AND. &
                    ( config_flags%adjust_heights ) .AND. &
                    ( lev500 .NE. 0 ) ) THEN

                  DO k = 2 , kte-1

                     !  Get the pressures on the full eta levels (grid%php is defined above as
                     !  the full-lev base pressure, an easy array to use for 3d space).

                     pl = grid%php(i,k  ,j) + &
                          ( grid%p(i,k-1  ,j) * ( grid%znw(k    ) - grid%znu(k  ) ) + &
                            grid%p(i,k    ,j) * ( grid%znu(k-1  ) - grid%znw(k  ) ) ) / &
                          ( grid%znu(k-1  ) - grid%znu(k  ) )
                     pu = grid%php(i,k+1,j) + &
                          ( grid%p(i,k-1+1,j) * ( grid%znw(k  +1) - grid%znu(k+1) ) + &
                            grid%p(i,k  +1,j) * ( grid%znu(k-1+1) - grid%znw(k+1) ) ) / &
                          ( grid%znu(k-1+1) - grid%znu(k+1) )

                     !  If these pressure levels trap 500 mb, use them to interpolate
                     !  to the 500 mb level of the computed height.

                     IF ( ( pl .GE. 50000. ) .AND. ( pu .LT. 50000. ) ) THEN
                        zl = ( grid%ph_2(i,k  ,j) + grid%phb(i,k  ,j) ) / g
                        zu = ( grid%ph_2(i,k+1,j) + grid%phb(i,k+1,j) ) / g

                        z500 = ( zl * ( LOG(50000.) - LOG(pu    ) ) + &
                                 zu * ( LOG(pl    ) - LOG(50000.) ) ) / &
                               ( LOG(pl) - LOG(pu) )
!                       z500 = ( zl * (    (50000.) -    (pu    ) ) + &
!                                zu * (    (pl    ) -    (50000.) ) ) / &
!                              (    (pl) -    (pu) )

                        !  Compute the difference of the 500 mb heights (computed minus input), and
                        !  then the change in grid%mu_2.  The grid%php is still full-levels, base pressure.

                        dz500 = z500 - grid%ght_gc(i,lev500,j)
                        tvsfc = ((grid%t_2(i,1,j)+t0)*((grid%p(i,1,j)+grid%php(i,1,j))/p1000mb)**(r_d/cp)) * &
                                (1.+0.6*moist(i,1,j,P_QV))
                        dpmu = ( grid%php(i,1,j) + grid%p(i,1,j) ) * EXP ( g * dz500 / ( r_d * tvsfc ) )
                        dpmu = dpmu - ( grid%php(i,1,j) + grid%p(i,1,j) )
                        grid%mu_2(i,j) = grid%mu_2(i,j) - dpmu
                        EXIT
                     END IF

                  END DO
               ELSE
                  dpmu = 0.
               END IF

            END DO

         END DO
      END DO
     
      !  Recompute Qv with full pressure on eta levels.  We use the temporary
      !  space of v_1 for total theta, which gets diagnosed into temperature.
      !  The u_1 array is RH on eta levels from the initial interpolation.  The
      !  previous value of moist was computed with dry pressure, not total
      !  pressure.

      grid%v_1 = grid%t_2 + 300
      CALL theta_to_t ( grid%v_1 , grid%pb + grid%p , p00 , &
                        ids , ide , jds , jde , kds , kde , &
                        ims , ime , jms , jme , kms , kme , &
                        its , ite , jts , jte , kts , kte )

      CALL rh_to_mxrat (grid%u_1, grid%v_1, grid%pb + grid%p, moist(:,:,:,P_QV) , &
                        config_flags%rh2qv_wrt_liquid ,                           &
                        config_flags%qv_max_p_safe ,                              &
                        config_flags%qv_max_flag , config_flags%qv_max_value ,    &
                        config_flags%qv_min_p_safe ,                              &
                        config_flags%qv_min_flag , config_flags%qv_min_value ,    &
                        ids , ide , jds , jde , kds , kde ,                       &
                        ims , ime , jms , jme , kms , kme ,                       &
                        its , ite , jts , jte , kts , kte-1 )

      !  If this is data from the SI, then we probably do not have the original
      !  surface data laying around.  Note that these are all the lowest levels
      !  of the respective 3d arrays.  For surface pressure, we assume that the
      !  vertical gradient of grid%p prime is zilch.  This is not all that important.
      !  These are filled in so that the various plotting routines have something
      !  to play with at the initial time for the model.

      IF ( flag_metgrid .NE. 1 ) THEN
         DO j = jts, min(jde-1,jte)
            DO i = its, min(ide,ite)
               grid%u10(i,j)=grid%u_2(i,1,j)
            END DO
         END DO

         DO j = jts, min(jde,jte)
            DO i = its, min(ide-1,ite)
               grid%v10(i,j)=grid%v_2(i,1,j)
            END DO
         END DO

         DO j = jts, min(jde-1,jte)
            DO i = its, min(ide-1,ite)
               p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht(i,j)/a/r_d ) **0.5 )
               grid%psfc(i,j)=p_surf + grid%p(i,1,j)
               grid%q2(i,j)=moist(i,1,j,P_QV)
               grid%th2(i,j)=grid%t_2(i,1,j)+300.
               grid%t2(i,j)=grid%th2(i,j)*(((grid%p(i,1,j)+grid%pb(i,1,j))/p00)**(r_d/cp))
            END DO
         END DO

      !  If this data is from WPS, then we have previously assigned the surface
      !  data for u, v, and t.  If we have an input qv, welp, we assigned that one,
      !  too.  Now we pick up the left overs, and if RH came in - we assign the
      !  mixing ratio.

      ELSE IF ( flag_metgrid .EQ. 1 ) THEN

         DO j = jts, min(jde-1,jte)
            DO i = its, min(ide-1,ite)
               p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht(i,j)/a/r_d ) **0.5 )
               grid%psfc(i,j)=p_surf + grid%p(i,1,j)
               grid%th2(i,j)=grid%t2(i,j)*(p00/(grid%p(i,1,j)+grid%pb(i,1,j)))**(r_d/cp)
            END DO
         END DO
         IF ( flag_qv .NE. 1 ) THEN
            DO j = jts, min(jde-1,jte)
               DO i = its, min(ide-1,ite)
                  grid%q2(i,j)=moist(i,1,j,P_QV)
               END DO
            END DO
         END IF

      END IF

!  Set flag to denote that we are saving original values of HT, MUB, and
!  PHB for 2-way nesting and cycling.

      grid%save_topo_from_real=1

      ips = its ; ipe = ite ; jps = jts ; jpe = jte ; kps = kts ; kpe = kte
#ifdef DM_PARALLEL
#   include "HALO_EM_INIT_1.inc"
#   include "HALO_EM_INIT_2.inc"
#   include "HALO_EM_INIT_3.inc"
#   include "HALO_EM_INIT_4.inc"
#   include "HALO_EM_INIT_5.inc"
#endif

      RETURN

   END SUBROUTINE init_domain_rk

!---------------------------------------------------------------------

   SUBROUTINE const_module_initialize ( p00 , t00 , a , tiso ) 
      USE module_configure
      IMPLICIT NONE
      !  For the real-data-cases only.
      REAL , INTENT(OUT) :: p00 , t00 , a , tiso
      CALL nl_get_base_pres  ( 1 , p00 )
      CALL nl_get_base_temp  ( 1 , t00 )
      CALL nl_get_base_lapse ( 1 , a   )
      CALL nl_get_iso_temp   ( 1 , tiso )
   END SUBROUTINE const_module_initialize

!-------------------------------------------------------------------

   SUBROUTINE rebalance_driver ( grid )

      IMPLICIT NONE

      TYPE (domain)          :: grid

      CALL rebalance( grid &
!
#include "actual_new_args.inc"
!
      )

   END SUBROUTINE rebalance_driver

!---------------------------------------------------------------------

   SUBROUTINE rebalance ( grid  &
!
#include "dummy_new_args.inc"
!
                        )
      IMPLICIT NONE

      TYPE (domain)          :: grid

#include "dummy_new_decl.inc"

      TYPE (grid_config_rec_type)              :: config_flags

      REAL :: p_surf ,  pd_surf, p_surf_int , pb_int , ht_hold
      REAL :: qvf , qvf1 , qvf2
      REAL :: p00 , t00 , a , tiso
      REAL , DIMENSION(:,:,:) , ALLOCATABLE :: t_init_int

      !  Local domain indices and counters.

      INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat

      INTEGER                             ::                       &
                                     ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte, &
                                     ips, ipe, jps, jpe, kps, kpe, &
                                     i, j, k

      REAL    :: temp, temp_int

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

      !  Some of the many weird geopotential initializations that we'll see today: grid%ph0 is total,
      !  and grid%ph_2 is a perturbation from the base state geopotential.  We set the base geopotential
      !  at the lowest level to terrain elevation * gravity.

      DO j=jts,jte
         DO i=its,ite
            grid%ph0(i,1,j) = grid%ht_fine(i,j) * g
            grid%ph_2(i,1,j) = 0.
         END DO
      END DO

      !  To define the base state, we call a USER MODIFIED routine to set the three
      !  necessary constants:  p00 (sea level pressure, Pa), t00 (sea level temperature, K),
      !  and A (temperature difference, from 1000 mb to 300 mb, K).

      CALL const_module_initialize ( p00 , t00 , a , tiso ) 

      !  Base state potential temperature and inverse density (alpha = 1/rho) from
      !  the half eta levels and the base-profile surface pressure.  Compute 1/rho
      !  from equation of state.  The potential temperature is a perturbation from t0.

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

            !  Base state pressure is a function of eta level and terrain, only, plus
            !  the hand full of constants: p00 (sea level pressure, Pa), t00 (sea level
            !  temperature, K), and A (temperature difference, from 1000 mb to 300 mb, K).
            !  The fine grid terrain is ht_fine, the interpolated is grid%ht.

            p_surf     = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht_fine(i,j)/a/r_d ) **0.5 )
            p_surf_int = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht(i,j)     /a/r_d ) **0.5 )

            DO k = 1, kte-1
               grid%pb(i,k,j) = grid%znu(k)*(p_surf     - grid%p_top) + grid%p_top
               pb_int    = grid%znu(k)*(p_surf_int - grid%p_top) + grid%p_top
               temp = MAX ( tiso, t00 + A*LOG(grid%pb(i,k,j)/p00) )
!              temp =             t00 + A*LOG(pb/p00)
               grid%t_init(i,k,j) = temp*(p00/grid%pb(i,k,j))**(r_d/cp) - t0
!              grid%t_init(i,k,j)    = (t00 + A*LOG(grid%pb(i,k,j)/p00))*(p00/grid%pb(i,k,j))**(r_d/cp) - t0
               temp_int = MAX ( tiso, t00 + A*LOG(pb_int   /p00) )
               t_init_int(i,k,j)= temp_int*(p00/pb_int   )**(r_d/cp) - t0
!              t_init_int(i,k,j)= (t00 + A*LOG(pb_int   /p00))*(p00/pb_int   )**(r_d/cp) - t0
               grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
            END DO

            !  Base state mu is defined as base state surface pressure minus grid%p_top

            grid%mub(i,j) = p_surf - grid%p_top

            !  Dry surface pressure is defined as the following (this mu is from the input file
            !  computed from the dry pressure).  Here the dry pressure is just reconstituted.

            pd_surf = ( grid%mub(i,j) + grid%mu_2(i,j) ) + grid%p_top

            !  Integrate base geopotential, starting at terrain elevation.  This assures that
            !  the base state is in exact hydrostatic balance with respect to the model equations.
            !  This field is on full levels.

            grid%phb(i,1,j) = grid%ht_fine(i,j) * g
            DO k  = 2,kte
               grid%phb(i,k,j) = grid%phb(i,k-1,j) - grid%dnw(k-1)*grid%mub(i,j)*grid%alb(i,k-1,j)
            END DO
         END DO
      END DO

      !  Replace interpolated terrain with fine grid values.

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
            grid%ht(i,j) = grid%ht_fine(i,j)
         END DO
      END DO

      !  Perturbation fields.

      DO j = jts, min(jde-1,jte)
         DO i = its, min(ide-1,ite)

            !  The potential temperature is THETAnest = THETAinterp + ( TBARnest - TBARinterp)

            DO k =  1 , kde-1
               grid%t_2(i,k,j) = grid%t_2(i,k,j) + ( grid%t_init(i,k,j) - t_init_int(i,k,j) )
            END DO

            !  Integrate the hydrostatic equation (from the RHS of the bigstep vertical momentum
            !  equation) down from the top to get the pressure perturbation.  First get the pressure
            !  perturbation, moisture, and inverse density (total and perturbation) at the top-most level.

            k = kte-1

            qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k,j,P_QV))
            qvf2 = 1./(1.+qvf1)
            qvf1 = qvf1*qvf2

            grid%p(i,k,j) = - 0.5*(grid%mu_2(i,j)+qvf1*grid%mub(i,j))/grid%rdnw(k)/qvf2
            qvf = 1. + rvovrd*moist(i,k,j,P_QV)
            grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_2(i,k,j)+t0)*qvf* &
                                 (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
            grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)

            !  Now, integrate down the column to compute the pressure perturbation, and diagnose the two
            !  inverse density fields (total and perturbation).

            DO k=kte-2,1,-1
               qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k+1,j,P_QV))
               qvf2 = 1./(1.+qvf1)
               qvf1 = qvf1*qvf2
               grid%p(i,k,j) = grid%p(i,k+1,j) - (grid%mu_2(i,j) + qvf1*grid%mub(i,j))/qvf2/grid%rdn(k+1)
               qvf = 1. + rvovrd*moist(i,k,j,P_QV)
               grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_2(i,k,j)+t0)*qvf* &
                           (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
               grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
            END DO

            !  This is the hydrostatic equation used in the model after the small timesteps.  In
            !  the model, grid%al (inverse density) is computed from the geopotential.

            DO k  = 2,kte
               grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) - &
                             grid%dnw(k-1) * ( (grid%mub(i,j)+grid%mu_2(i,j))*grid%al(i,k-1,j) &
                           + grid%mu_2(i,j)*grid%alb(i,k-1,j) )
               grid%ph0(i,k,j) = grid%ph_2(i,k,j) + grid%phb(i,k,j)
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

   RECURSIVE SUBROUTINE find_my_parent ( grid_ptr_in , grid_ptr_out , id_i_am , id_wanted , found_the_id )

!  RAR - Modified to correct problem in which the parent of a child domain could
!  not be found in the namelist. This condition typically occurs while using the
!  "allow_grid" namelist option when an inactive domain comes before an active
!  domain in the list, i.e., the domain number of the active domain is greater than
!  that of an inactive domain at the same level. 
!      
      USE module_domain

      TYPE(domain) , POINTER :: grid_ptr_in , grid_ptr_out
      TYPE(domain) , POINTER :: grid_ptr_sibling
      INTEGER :: id_wanted , id_i_am
      INTEGER :: nest                    ! RAR
      LOGICAL :: found_the_id

      found_the_id = .FALSE.
      grid_ptr_sibling => grid_ptr_in
      nest = 0                           ! RAR

      DO WHILE ( ASSOCIATED ( grid_ptr_sibling ) )

         IF ( grid_ptr_sibling%grid_id .EQ. id_wanted ) THEN
            found_the_id = .TRUE.
            grid_ptr_out => grid_ptr_sibling
            RETURN
! RAR    ELSE IF ( grid_ptr_sibling%num_nests .GT. 0 ) THEN
         ELSE IF ( grid_ptr_sibling%num_nests .GT. 0 .AND. nest .LT. grid_ptr_sibling%num_nests ) THEN
            nest = nest + 1               ! RAR
            grid_ptr_sibling => grid_ptr_sibling%nests(nest)%ptr ! RAR
            CALL find_my_parent ( grid_ptr_sibling , grid_ptr_out , id_i_am , id_wanted , found_the_id )
            IF (.NOT. found_the_id) grid_ptr_sibling => grid_ptr_sibling%parents(1)%ptr   ! RAR
         ELSE
            grid_ptr_sibling => grid_ptr_sibling%sibling
         END IF

      END DO

   END SUBROUTINE find_my_parent

#endif

!---------------------------------------------------------------------

#ifdef VERT_UNIT

!This is a main program for a small unit test for the vertical interpolation.

program vint

   implicit none

   integer , parameter :: ij = 3
   integer , parameter :: keta = 30
   integer , parameter :: kgen =20

   integer :: ids , ide , jds , jde , kds , kde , &
              ims , ime , jms , jme , kms , kme , &
              its , ite , jts , jte , kts , kte

   integer :: generic

   real , dimension(1:ij,kgen,1:ij) :: fo , po
   real , dimension(1:ij,1:keta,1:ij) :: fn_calc , fn_interp , pn

   integer, parameter :: interp_type             = 1 ! 2
!  integer, parameter :: lagrange_order          = 2 ! 1
   integer            :: lagrange_order
   logical, parameter :: lowest_lev_from_sfc     = .FALSE. ! .TRUE.
   logical, parameter :: use_levels_below_ground = .FALSE. ! .TRUE.
   logical, parameter :: use_surface             = .FALSE. ! .TRUE.
   real   , parameter :: zap_close_levels        = 500. ! 100.
   integer, parameter :: force_sfc_in_vinterp    = 0 ! 6

   integer :: k

   ids = 1 ; ide = ij ; jds = 1 ; jde = ij ; kds = 1 ; kde = keta
   ims = 1 ; ime = ij ; jms = 1 ; jme = ij ; kms = 1 ; kme = keta
   its = 1 ; ite = ij ; jts = 1 ; jte = ij ; kts = 1 ; kte = keta

   generic = kgen

   print *,' '
   print *,'------------------------------------'
   print *,'UNIT TEST FOR VERTICAL INTERPOLATION'
   print *,'------------------------------------'
   print *,' '
   do lagrange_order = 1 , 2
      print *,' '
      print *,'------------------------------------'
      print *,'Lagrange Order = ',lagrange_order
      print *,'------------------------------------'
      print *,' '
      call fillitup ( fo , po , fn_calc , pn , &
                    ids , ide , jds , jde , kds , kde , &
                    ims , ime , jms , jme , kms , kme , &
                    its , ite , jts , jte , kts , kte , &
                    generic , lagrange_order )

      print *,' '
      print *,'Level   Pressure     Field'
      print *,'          (Pa)      (generic)'
      print *,'------------------------------------'
      print *,' '
      do k = 1 , generic
      write (*,fmt='(i2,2x,f12.3,1x,g15.8)' ) &
         k,po(2,k,2),fo(2,k,2)
      end do
      print *,' '

      call vert_interp ( fo , po , fn_interp , pn , &
                         generic , 'T' , &
                         interp_type , lagrange_order , &
                         lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                         zap_close_levels , force_sfc_in_vinterp , &
                         ids , ide , jds , jde , kds , kde , &
                         ims , ime , jms , jme , kms , kme , &
                         its , ite , jts , jte , kts , kte )

      print *,'Multi-Order Interpolator'
      print *,'------------------------------------'
      print *,' '
      print *,'Level  Pressure      Field           Field         Field'
      print *,'         (Pa)        Calc            Interp        Diff'
      print *,'------------------------------------'
      print *,' '
      do k = kts , kte-1
      write (*,fmt='(i2,2x,f12.3,1x,3(g15.7))' ) &
         k,pn(2,k,2),fn_calc(2,k,2),fn_interp(2,k,2),fn_calc(2,k,2)-fn_interp(2,k,2)
      end do

      call vert_interp_old ( fo , po , fn_interp , pn , &
                         generic , 'T' , &
                         interp_type , lagrange_order , &
                         lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                         zap_close_levels , force_sfc_in_vinterp , &
                         ids , ide , jds , jde , kds , kde , &
                         ims , ime , jms , jme , kms , kme , &
                         its , ite , jts , jte , kts , kte )

      print *,'Linear Interpolator'
      print *,'------------------------------------'
      print *,' '
      print *,'Level  Pressure      Field           Field         Field'
      print *,'         (Pa)        Calc            Interp        Diff'
      print *,'------------------------------------'
      print *,' '
      do k = kts , kte-1
      write (*,fmt='(i2,2x,f12.3,1x,3(g15.7))' ) &
         k,pn(2,k,2),fn_calc(2,k,2),fn_interp(2,k,2),fn_calc(2,k,2)-fn_interp(2,k,2)
      end do
   end do

end program vint

subroutine wrf_error_fatal (string)
   character (len=*) :: string
   print *,string
   stop
end subroutine wrf_error_fatal

subroutine fillitup ( fo , po , fn , pn , &
                    ids , ide , jds , jde , kds , kde , &
                    ims , ime , jms , jme , kms , kme , &
                    its , ite , jts , jte , kts , kte , &
                    generic , lagrange_order )

   implicit none

   integer , intent(in) :: ids , ide , jds , jde , kds , kde , &
              ims , ime , jms , jme , kms , kme , &
              its , ite , jts , jte , kts , kte

   integer , intent(in) :: generic , lagrange_order

   real , dimension(ims:ime,generic,jms:jme) , intent(out) :: fo , po
   real , dimension(ims:ime,kms:kme,jms:jme) , intent(out) :: fn , pn

   integer :: i , j , k

   real , parameter :: piov2 = 3.14159265358 / 2.

   k = 1
   do j = jts , jte
   do i = its , ite
      po(i,k,j) = 102000.
   end do
   end do

   do k = 2 , generic
   do j = jts , jte
   do i = its , ite
      po(i,k,j) = ( 5000. * ( 1 - (k-1) ) + 100000. * ( (k-1) - (generic-1) ) ) / (1. - real(generic-1) )
   end do
   end do
   end do

   if ( lagrange_order .eq. 1 ) then
      do k = 1 , generic
      do j = jts , jte
      do i = its , ite
         fo(i,k,j) = po(i,k,j)
!        fo(i,k,j) = sin(po(i,k,j) * piov2 / 102000. )
      end do
      end do
      end do
   else if ( lagrange_order .eq. 2 ) then
      do k = 1 , generic
      do j = jts , jte
      do i = its , ite
         fo(i,k,j) = (((po(i,k,j)-5000.)/102000.)*((102000.-po(i,k,j))/102000.))*102000.
!        fo(i,k,j) = sin(po(i,k,j) * piov2 / 102000. )
      end do
      end do
      end do
   end if

!!!!!!!!!!!!

   do k = kts , kte
   do j = jts , jte
   do i = its , ite
      pn(i,k,j) = ( 5000. * ( 0 - (k-1) ) + 102000. * ( (k-1) - (kte-1) ) ) / (-1. *  real(kte-1) )
   end do
   end do
   end do

   do k = kts , kte-1
   do j = jts , jte
   do i = its , ite
      pn(i,k,j) = ( pn(i,k,j) + pn(i,k+1,j) ) /2.
   end do
   end do
   end do


   if ( lagrange_order .eq. 1 ) then
      do k = kts , kte-1
      do j = jts , jte
      do i = its , ite
         fn(i,k,j) = pn(i,k,j)
!        fn(i,k,j) = sin(pn(i,k,j) * piov2 / 102000. )
      end do
      end do
      end do
   else if ( lagrange_order .eq. 2 ) then
      do k = kts , kte-1
      do j = jts , jte
      do i = its , ite
         fn(i,k,j) = (((pn(i,k,j)-5000.)/102000.)*((102000.-pn(i,k,j))/102000.))*102000.
!        fn(i,k,j) = sin(pn(i,k,j) * piov2 / 102000. )
      end do
      end do
      end do
   end if

end subroutine fillitup

#endif

!---------------------------------------------------------------------

   SUBROUTINE vert_interp ( fo , po , fnew , pnu , &
                            generic , var_type , &
                            interp_type , lagrange_order , extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

   !  Vertically interpolate the new field.  The original field on the original
   !  pressure levels is provided, and the new pressure surfaces to interpolate to.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: interp_type , lagrange_order , extrap_type
      LOGICAL , INTENT(IN)        :: lowest_lev_from_sfc , use_levels_below_ground , use_surface
      REAL    , INTENT(IN)        :: zap_close_levels
      INTEGER , INTENT(IN)        :: force_sfc_in_vinterp
      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte
      INTEGER , INTENT(IN)        :: generic

      CHARACTER (LEN=1) :: var_type

      REAL , DIMENSION(ims:ime,generic,jms:jme) , INTENT(IN)     :: fo , po
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: pnu
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: fnew

      REAL , DIMENSION(ims:ime,generic,jms:jme)                  :: forig , porig
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme)                  :: pnew

      !  Local vars

      INTEGER :: i , j , k , ko , kn , k1 , k2 , ko_1 , ko_2 , knext
      INTEGER :: istart , iend , jstart , jend , kstart , kend
      INTEGER , DIMENSION(ims:ime,kms:kme        )               :: k_above , k_below
      INTEGER , DIMENSION(ims:ime                )               :: ks
      INTEGER , DIMENSION(ims:ime                )               :: ko_above_sfc
      INTEGER :: count , zap , zap_below , zap_above , kst , kcount
      INTEGER :: kinterp_start , kinterp_end , sfc_level

      LOGICAL :: any_below_ground

      REAL :: p1 , p2 , pn, hold
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

         !  The lowest level is the surface.  Levels 2 through "generic" are supposed to
         !  be "bottom-up".  Flip if they are not.  This is based on the input pressure
         !  array.

         IF      ( porig(its,2,j) .LT. porig(its,generic,j) ) THEN
            DO kn = 2 , ( generic + 1 ) / 2
               DO i = istart , iend
                  hold                    = porig(i,kn,j)
                  porig(i,kn,j)           = porig(i,generic+2-kn,j)
                  porig(i,generic+2-kn,j) = hold
                  forig(i,kn,j)           = fo   (i,generic+2-kn,j)
                  forig(i,generic+2-kn,j) = fo   (i,kn,j)
               END DO
            END DO
            DO i = istart , iend
               forig(i,1,j)               = fo   (i,1,j)
            END DO
            IF ( MOD(generic,2) .EQ. 0 ) THEN
               k=generic/2 + 1
               DO i = istart , iend
                  forig(i,k,j)            = fo   (i,k,j)
               END DO
            END IF
         ELSE
            DO kn = 1 , generic
               DO i = istart , iend
                  forig(i,kn,j)           = fo   (i,kn,j)
               END DO
            END DO
         END IF

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
            !  values below the ground (this is just to catch the occasional value that is
            !  inconsistently below the surface based on input data), 2) do the surface level, then
            !  3) add in the levels that are above the surface.  For the levels next to the surface,
            !  we check to remove any levels that are "too close".  When building the column of input
            !  pressures, we also attend to the request for forcing the surface analysis to be used
            !  in a few lower eta-levels.

            !  Fill in the column from up to the level just below the surface with the input
            !  presssure and the input field (orig or old, which ever).  For an isobaric input
            !  file, this data is isobaric.

            !  How many levels have we skipped in the input column.

            zap = 0
            zap_below = 0
            zap_above = 0

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

               IF ( ordered_porig(count-1) - porig(i,1,j) .LT. zap_close_levels ) THEN
                  count = count -1
                  zap = 1
                  zap_below = 1
               END IF

               !  Add in the surface values.

               ordered_porig(count) = porig(i,1,j)
               ordered_forig(count) = forig(i,1,j)
               count = count + 1

               !  A usual way to do the vertical interpolation is to pay more attention to the
               !  surface data.  Why?  Well it has about 20x the density as the upper air, so we
               !  hope the analysis is better there.  We more strongly use this data by artificially
               !  tossing out levels above the surface that are beneath a certain number of prescribed
               !  eta levels at this (i,j).  The "zap" value is how many levels of input we are
               !  removing, which is used to tell the interpolator how many valid values are in
               !  the column.  The "count" value is the increment to the index of levels, and is
               !  only used for assignments.

               IF ( force_sfc_in_vinterp .GT. 0 ) THEN

                  !  Get the pressure at the eta level.  We want to remove all input pressure levels
                  !  between the level above the surface to the pressure at this eta surface.  That
                  !  forces the surface value to be used through the selected eta level.  Keep track
                  !  of two things: the level to use above the eta levels, and how many levels we are
                  !  skipping.

                  knext = ko_above_sfc(i)
                  find_level : DO ko = ko_above_sfc(i) , generic
                     IF ( porig(i,ko,j) .LE. pnew(i,force_sfc_in_vinterp,j) ) THEN
                        knext = ko
                        exit find_level
                     ELSE
                        zap = zap + 1
                        zap_above = zap_above + 1
                     END IF
                  END DO find_level

               !  No request for special interpolation, so we just assign the next level to use
               !  above the surface as, ta da, the first level above the surface.  I know, wow.

               ELSE
                  knext = ko_above_sfc(i)
               END IF

               !  One more time, make sure the pressure just above the surface is not "too close", this
               !  will cause havoc with the higher order interpolators.  In case of a "too close"
               !  instance, we toss out the offending level above the surface (NOT the surface one) by simply
               !  incrementing the loop counter.  Here, count-1 is the surface level and knext is either
               !  the next level up OR it is the level above the prescribed number of eta surfaces.

               IF ( ordered_porig(count-1) - porig(i,knext,j) .LT. zap_close_levels ) THEN
                  kst = knext+1
                  zap = zap + 1
                  zap_above = zap_above + 1
               ELSE
                  kst = knext
               END IF

               DO ko = kst , generic
                  ordered_porig(count) = porig(i,ko,j)
                  ordered_forig(count) = forig(i,ko,j)
                  count = count + 1
               END DO

            !  This is easy, the surface is the lowest level, just stick them in, in this order.  OK,
            !  there are a couple of subtleties.  We have to check for that special interpolation that
            !  skips some input levels so that the surface is used for the lowest few eta levels.  Also,
            !  we must make sure that we still do not have levels that are "too close" together.

            ELSE

               !  Initialize no input levels have yet been removed from consideration.

               zap = 0

               !  The surface is the lowest level, so it gets set right away to location 1.

               ordered_porig(1) = porig(i,1,j)
               ordered_forig(1) = forig(i,1,j)

               !  We start filling in the array at loc 2, as in just above the level we just stored.

               count = 2

               !  Are we forcing the interpolator to skip valid input levels so that the
               !  surface data is used through more levels?  Essentially as above.

               IF ( force_sfc_in_vinterp .GT. 0 ) THEN
                  knext = 2
                  find_level2: DO ko = 2 , generic
                     IF ( porig(i,ko,j) .LE. pnew(i,force_sfc_in_vinterp,j) ) THEN
                        knext = ko
                        exit find_level2
                     ELSE
                        zap = zap + 1
                        zap_above = zap_above + 1
                     END IF
                  END DO find_level2
               ELSE
                  knext = 2
               END IF

               !  Fill in the data above the surface.  The "knext" index is either the one
               !  just above the surface OR it is the index associated with the level that
               !  is just above the pressure at this (i,j) of the top eta level that is to
               !  be directly impacted with the surface level in interpolation.

               DO ko = knext , generic
                  IF ( ( ordered_porig(count-1) - porig(i,ko,j) .LT. zap_close_levels ) .AND. &
                       ( ko .LT. generic ) ) THEN
                     zap = zap + 1
                     zap_above = zap_above + 1
                     CYCLE
                  END IF
                  ordered_porig(count) = porig(i,ko,j)
                  ordered_forig(count) = forig(i,ko,j)
                  count = count + 1
               END DO

            END IF

            !  Now get the column of the "new" pressure data.  So, this one is easy.

            DO kn = kstart , kend
               ordered_pnew(kn) = pnew(i,kn,j)
            END DO

            !  How many levels (count) are we shipping to the Lagrange interpolator.

            IF      ( ( use_levels_below_ground ) .AND. ( use_surface ) ) THEN

               !  Use all levels, including the input surface, and including the pressure
               !  levels below ground.  We know to stop when we have reached the top of
               !  the input pressure data.

               count = 0
               find_how_many_1 : DO ko = 1 , generic
                  IF ( porig(i,generic,j) .EQ. ordered_porig(ko) ) THEN
                     count = count + 1
                     EXIT find_how_many_1
                  ELSE
                     count = count + 1
                  END IF
               END DO find_how_many_1
               kinterp_start = 1
               kinterp_end = kinterp_start + count - 1

            ELSE IF ( ( use_levels_below_ground ) .AND. ( .NOT. use_surface ) ) THEN

               !  Use all levels (excluding the input surface) and including the pressure
               !  levels below ground.  We know to stop when we have reached the top of
               !  the input pressure data.

               count = 0
               find_sfc_2 : DO ko = 1 , generic
                  IF ( porig(i,1,j) .EQ. ordered_porig(ko) ) THEN
                     sfc_level = ko
                     EXIT find_sfc_2
                  END IF
               END DO find_sfc_2

               DO ko = sfc_level , generic-1
                  ordered_porig(ko) = ordered_porig(ko+1)
                  ordered_forig(ko) = ordered_forig(ko+1)
               END DO
               ordered_porig(generic) = 1.E-5
               ordered_forig(generic) = 1.E10

               count = 0
               find_how_many_2 : DO ko = 1 , generic
                  IF ( porig(i,generic,j) .EQ. ordered_porig(ko) ) THEN
                     count = count + 1
                     EXIT find_how_many_2
                  ELSE
                     count = count + 1
                  END IF
               END DO find_how_many_2
               kinterp_start = 1
               kinterp_end = kinterp_start + count - 1

            ELSE IF ( ( .NOT. use_levels_below_ground ) .AND. ( use_surface ) ) THEN

               !  Use all levels above the input surface pressure.

               kcount = ko_above_sfc(i)-1-zap_below
               count = 0
               DO ko = 1 , generic
                  IF ( porig(i,ko,j) .EQ. ordered_porig(kcount) ) THEN
!  write (6,fmt='(f11.3,f11.3,g11.5)') porig(i,ko,j),ordered_porig(kcount),ordered_forig(kcount)
                     kcount = kcount + 1
                     count = count + 1
                  ELSE
!  write (6,fmt='(f11.3            )') porig(i,ko,j)
                  END IF
               END DO
               kinterp_start = ko_above_sfc(i)-1-zap_below
               kinterp_end = kinterp_start + count - 1

            END IF

            !  The polynomials are either in pressure or LOG(pressure).

            IF ( interp_type .EQ. 1 ) THEN
               CALL lagrange_setup ( var_type , &
                    ordered_porig(kinterp_start:kinterp_end) , &
                    ordered_forig(kinterp_start:kinterp_end) , &
                    count , lagrange_order , extrap_type , &
                    ordered_pnew(kstart:kend)  , ordered_fnew  , kend-kstart+1 ,i,j)
            ELSE
               CALL lagrange_setup ( var_type , &
                LOG(ordered_porig(kinterp_start:kinterp_end)) , &
                    ordered_forig(kinterp_start:kinterp_end) , &
                    count , lagrange_order , extrap_type , &
                LOG(ordered_pnew(kstart:kend)) , ordered_fnew  , kend-kstart+1 ,i,j)
            END IF

            !  Save the computed data.

            DO kn = kstart , kend
               fnew(i,kn,j) = ordered_fnew(kn)
            END DO

            !  There may have been a request to have the surface data from the input field
            !  to be assigned as to the lowest eta level.  This assumes thin layers (usually
            !  the isobaric original field has the surface from 2-m T and RH, and 10-m U and V).

            IF ( lowest_lev_from_sfc ) THEN
               fnew(i,1,j) = forig(i,ko_above_sfc(i)-1,j)
            END IF

         END DO

      END DO

   END SUBROUTINE vert_interp

!---------------------------------------------------------------------

   SUBROUTINE vert_interp_old ( forig , po , fnew , pnu , &
                            generic , var_type , &
                            interp_type , lagrange_order , extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

   !  Vertically interpolate the new field.  The original field on the original
   !  pressure levels is provided, and the new pressure surfaces to interpolate to.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: interp_type , lagrange_order , extrap_type
      LOGICAL , INTENT(IN)        :: lowest_lev_from_sfc , use_levels_below_ground , use_surface
      REAL    , INTENT(IN)        :: zap_close_levels
      INTEGER , INTENT(IN)        :: force_sfc_in_vinterp
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

   SUBROUTINE lagrange_setup ( var_type , all_x , all_y , all_dim , n , extrap_type , &
                               target_x , target_y , target_dim ,i,j)

      !  We call a Lagrange polynomial interpolator.  The parallel concerns are put off as this
      !  is initially set up for vertical use.  The purpose is an input column of pressure (all_x),
      !  and the associated pressure level data (all_y).  These are assumed to be sorted (ascending
      !  or descending, no matter).  The locations to be interpolated to are the pressures in
      !  target_x, probably the new vertical coordinate values.  The field that is output is the
      !  target_y, which is defined at the target_x location.  Mostly we expect to be 2nd order
      !  overlapping polynomials, with only a single 2nd order method near the top and bottom.
      !  When n=1, this is linear; when n=2, this is a second order interpolator.

      IMPLICIT NONE

      CHARACTER (LEN=1) :: var_type
      INTEGER , INTENT(IN) :: all_dim , n , extrap_type , target_dim
      REAL, DIMENSION(all_dim) , INTENT(IN) :: all_x , all_y
      REAL , DIMENSION(target_dim) , INTENT(IN) :: target_x
      REAL , DIMENSION(target_dim) , INTENT(OUT) :: target_y

      !  Brought in for debug purposes, all of the computations are in a single column.

      INTEGER , INTENT(IN) :: i,j

      !  Local vars

      REAL , DIMENSION(n+1) :: x , y
      REAL :: a , b
      REAL :: target_y_1 , target_y_2
      LOGICAL :: found_loc
      INTEGER :: loop , loc_center_left , loc_center_right , ist , iend , target_loop
      INTEGER :: vboundb , vboundt

      !  Local vars for the problem of extrapolating theta below ground.

      REAL :: temp_1 , temp_2 , temp_3 , temp_y
      REAL :: depth_of_extrap_in_p , avg_of_extrap_p , temp_extrap_starting_point , dhdp , dh , dt
      REAL , PARAMETER :: RovCp      = rcp
      REAL , PARAMETER :: CRC_const1 = 11880.516      ! m
      REAL , PARAMETER :: CRC_const2 =     0.1902632  !
      REAL , PARAMETER :: CRC_const3 =     0.0065     ! K/km

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

      !  We can pinch in the area of the higher order interpolation with vbound.  If
      !  vbound = 0, no pinching.  If vbound = m, then we make the lower "m" and upper
      !  "m" eta levels use a linear interpolation.

      vboundb = 4
      vboundt = 0

      !  Loop over the list of target x and y values.

      DO target_loop = 1 , target_dim

         !  Find the two trapping x values, and keep the indices.

         found_loc = .FALSE.
         find_trap : DO loop = 1 , all_dim -1
            a = target_x(target_loop) - all_x(loop)
            b = target_x(target_loop) - all_x(loop+1)
            IF ( a*b .LE. 0.0 ) THEN
               loc_center_left  = loop
               loc_center_right = loop+1
               found_loc = .TRUE.
               EXIT find_trap
            END IF
         END DO find_trap

         IF ( ( .NOT. found_loc ) .AND. ( target_x(target_loop) .GT. all_x(1) ) ) THEN

            !  Isothermal extrapolation.

            IF      ( ( extrap_type .EQ. 1 ) .AND. ( var_type .EQ. 'T' ) ) THEN

               temp_1 = all_y(1) * ( all_x(1) / 100000. ) ** RovCp
               target_y(target_loop) = temp_1 * ( 100000. / target_x(target_loop) ) ** RovCp

            !  Standard atmosphere -6.5 K/km lapse rate for the extrapolation.

            ELSE IF ( ( extrap_type .EQ. 2 ) .AND. ( var_type .EQ. 'T' ) ) THEN

               depth_of_extrap_in_p = target_x(target_loop) - all_x(1)
               avg_of_extrap_p = ( target_x(target_loop) + all_x(1) ) * 0.5
               temp_extrap_starting_point = all_y(1) * ( all_x(1) / 100000. ) ** RovCp
               dhdp = CRC_const1 * CRC_const2 * ( avg_of_extrap_p / 100. ) ** ( CRC_const2 - 1. )
               dh = dhdp * ( depth_of_extrap_in_p / 100. )
               dt = dh * CRC_const3
               target_y(target_loop) = ( temp_extrap_starting_point + dt ) * ( 100000. / target_x(target_loop) ) ** RovCp

            !  Adiabatic extrapolation for theta.

            ELSE IF ( ( extrap_type .EQ. 3 ) .AND. ( var_type .EQ. 'T' ) ) THEN

               target_y(target_loop) = all_y(1)


            !  Wild extrapolation for non-temperature vars.

            ELSE IF ( extrap_type .EQ. 1 ) THEN

               target_y(target_loop) = ( all_y(2) * ( target_x(target_loop) - all_x(3) ) + &
                                         all_y(3) * ( all_x(2) - target_x(target_loop) ) ) / &
                                       ( all_x(2) - all_x(3) )

            !  Use a constant value below ground.

            ELSE IF ( extrap_type .EQ. 2 ) THEN

               target_y(target_loop) = all_y(1)

            ELSE IF ( extrap_type .EQ. 3 ) THEN
               CALL wrf_error_fatal ( 'You are not allowed to use extrap_option #3 for any var except for theta.' )

            END IF
            CYCLE
         ELSE IF ( .NOT. found_loc ) THEN
            print *,'i,j = ',i,j
            print *,'target pressure and value = ',target_x(target_loop),target_y(target_loop)
            DO loop = 1 , all_dim
               print *,'column of pressure and value = ',all_x(loop),all_y(loop)
            END DO
            CALL wrf_error_fatal ( 'troubles, could not find trapping x locations' )
         END IF

         !  Even or odd order?  We can put the value in the middle if this is
         !  an odd order interpolator.  For the even guys, we'll do it twice
         !  and shift the range one index, then get an average.

         IF      ( MOD(n,2) .NE. 0 ) THEN
            IF ( ( loc_center_left -(((n+1)/2)-1) .GE.       1 ) .AND. &
                 ( loc_center_right+(((n+1)/2)-1) .LE. all_dim ) ) THEN
               ist  = loc_center_left -(((n+1)/2)-1)
               iend = ist + n
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , n , target_x(target_loop) , target_y(target_loop) )
            ELSE
               IF ( .NOT. found_loc ) THEN
                  CALL wrf_error_fatal ( 'I doubt this will happen, I will only do 2nd order for now' )
               END IF
            END IF

         ELSE IF ( ( MOD(n,2) .EQ. 0 ) .AND. &
                   ( ( target_loop .GE. 1 + vboundb ) .AND. ( target_loop .LE. target_dim - vboundt ) ) ) THEN
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

         ELSE IF ( MOD(n,2) .EQ. 0 ) THEN
               ist  = loc_center_left
               iend = loc_center_right
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , 1 , target_x(target_loop) , target_y(target_loop) )

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

#ifndef VERT_UNIT
!---------------------------------------------------------------------

   SUBROUTINE p_dry ( mu0 , eta , pdht , pdry , full_levs , &
                             ids , ide , jds , jde , kds , kde , &
                             ims , ime , jms , jme , kms , kme , &
                             its , ite , jts , jte , kts , kte )

   !  Compute reference pressure and the reference mu.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      LOGICAL :: full_levs

      REAL , DIMENSION(ims:ime,        jms:jme) , INTENT(IN)     :: mu0
      REAL , DIMENSION(        kms:kme        ) , INTENT(IN)     :: eta
      REAL                                                       :: pdht
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: pdry

      !  Local vars

      INTEGER :: i , j , k
      REAL , DIMENSION(        kms:kme        )                  :: eta_h

      IF ( full_levs ) THEN
         DO j = jts , MIN ( jde-1 , jte )
            DO k = kts , kte
               DO i = its , MIN (ide-1 , ite )
                     pdry(i,k,j) = eta(k) * mu0(i,j) + pdht
               END DO
            END DO
         END DO

      ELSE
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
      END IF

   END SUBROUTINE p_dry

!---------------------------------------------------------------------

   SUBROUTINE p_dts ( pdts , intq , psfc , p_top , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Compute difference between the dry, total surface pressure and the top pressure.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , INTENT(IN) :: p_top
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN)     :: psfc
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN)     :: intq
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(OUT)    :: pdts

      !  Local vars

      INTEGER :: i , j , k

      DO j = jts , MIN ( jde-1 , jte )
         DO i = its , MIN (ide-1 , ite )
               pdts(i,j) = psfc(i,j) - intq(i,j) - p_top
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

      REAL , PARAMETER :: Rd = r_d

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

      INTEGER :: i , j , k, min_lev

      i = its
      j = jts
      p_top = p(i,2,j)
      min_lev = 2
      DO k = 2 , kte
         IF ( p_top .GT. p(i,k,j) ) THEN
            p_top = p(i,k,j)
            min_lev = k
         END IF
      END DO

      k = min_lev
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

   !  Compute potential temperature from temperature and pressure.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , INTENT(IN) :: p00
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: p
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT)  :: t

      !  Local vars

      INTEGER :: i , j , k

      REAL , PARAMETER :: Rd = r_d

      DO j = jts , MIN ( jde-1 , jte )
         DO k = kts , kte
            DO i = its , MIN (ide-1 , ite )
               t(i,k,j) = t(i,k,j) * ( p00 / p(i,k,j) ) ** (Rd / Cp)
            END DO
         END DO
      END DO

   END SUBROUTINE t_to_theta


!---------------------------------------------------------------------

   SUBROUTINE theta_to_t ( t , p , p00 , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Compute temperature from potential temp and pressure.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , INTENT(IN) :: p00
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: p
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT)  :: t

      !  Local vars

      INTEGER :: i , j , k

      REAL , PARAMETER :: Rd = r_d

      DO j = jts , MIN ( jde-1 , jte )
         DO k = kts , kte
            DO i = its , MIN (ide-1 , ite )
               t(i,k,j) = t(i,k,j) / ( ( p00 / p(i,k,j) ) ** (Rd / Cp) )
            END DO
         END DO
      END DO

   END SUBROUTINE theta_to_t

!---------------------------------------------------------------------

   SUBROUTINE integ_moist ( q_in , p_in , pd_out , t_in , ght_in , intq , &
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

      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: q_in , p_in , t_in , ght_in
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: pd_out
      REAL , DIMENSION(ims:ime,        jms:jme) , INTENT(OUT)    :: intq

      !  Local vars

      INTEGER :: i , j , k
      INTEGER , DIMENSION(ims:ime) :: level_above_sfc
      REAL , DIMENSION(ims:ime,jms:jme) :: psfc , tsfc , qsfc, zsfc
      REAL , DIMENSION(ims:ime,kms:kme) :: q , p , t , ght, pd

      REAL :: rhobar , qbar , dz
      REAL :: p1 , p2 , t1 , t2 , q1 , q2 , z1, z2

      LOGICAL :: upside_down

      REAL , PARAMETER :: Rd = r_d

      !  Get a surface value, always the first level of a 3d field.

      DO j = jts , MIN ( jde-1 , jte )
         DO i = its , MIN (ide-1 , ite )
            psfc(i,j) = p_in(i,kts,j)
            tsfc(i,j) = t_in(i,kts,j)
            qsfc(i,j) = q_in(i,kts,j)
            zsfc(i,j) = ght_in(i,kts,j)
         END DO
      END DO

      IF ( p_in(its,kts+1,jts) .LT. p_in(its,kte,jts) ) THEN
         upside_down = .TRUE.
      ELSE
         upside_down = .FALSE.
      END IF

      DO j = jts , MIN ( jde-1 , jte )

         !  Initialize the integrated quantity of moisture to zero.

         DO i = its , MIN (ide-1 , ite )
            intq(i,j) = 0.
         END DO

         IF ( upside_down ) THEN
            DO i = its , MIN (ide-1 , ite )
               p(i,kts) = p_in(i,kts,j)
               t(i,kts) = t_in(i,kts,j)
               q(i,kts) = q_in(i,kts,j)
               ght(i,kts) = ght_in(i,kts,j)
               DO k = kts+1,kte
                  p(i,k) = p_in(i,kte+2-k,j)
                  t(i,k) = t_in(i,kte+2-k,j)
                  q(i,k) = q_in(i,kte+2-k,j)
                  ght(i,k) = ght_in(i,kte+2-k,j)
               END DO
            END DO
         ELSE
            DO i = its , MIN (ide-1 , ite )
               DO k = kts,kte
                  p(i,k) = p_in(i,k      ,j)
                  t(i,k) = t_in(i,k      ,j)
                  q(i,k) = q_in(i,k      ,j)
                  ght(i,k) = ght_in(i,k      ,j)
               END DO
            END DO
         END IF

         !  Find the first level above the ground.  If all of the levels are above ground, such as
         !  a terrain following lower coordinate, then the first level above ground is index #2.

         DO i = its , MIN (ide-1 , ite )
            level_above_sfc(i) = -1
            IF ( p(i,kts+1) .LT. psfc(i,j) ) THEN
               level_above_sfc(i) = kts+1
            ELSE
               find_k : DO k = kts+1,kte-1
                  IF ( ( p(i,k  )-psfc(i,j) .GE. 0. ) .AND. &
                       ( p(i,k+1)-psfc(i,j) .LT. 0. ) ) THEN
                     level_above_sfc(i) = k+1
                     EXIT find_k
                  END IF
               END DO find_k
               IF ( level_above_sfc(i) .EQ. -1 ) THEN
print *,'i,j = ',i,j
print *,'p = ',p(i,:)
print *,'p sfc = ',psfc(i,j)
                  CALL wrf_error_fatal ( 'Could not find level above ground')
               END IF
            END IF
         END DO

         DO i = its , MIN (ide-1 , ite )

            !  Account for the moisture above the ground.

            pd(i,kte) = p(i,kte)
            DO k = kte-1,level_above_sfc(i),-1
                  rhobar = ( p(i,k  ) / ( Rd * t(i,k  ) ) + &
                             p(i,k+1) / ( Rd * t(i,k+1) ) ) * 0.5
                  qbar   = ( q(i,k  ) + q(i,k+1) ) * 0.5
                  dz     = ght(i,k+1) - ght(i,k)
                  intq(i,j) = intq(i,j) + g * qbar * rhobar / (1. + qbar) * dz
                  pd(i,k) = p(i,k) - intq(i,j)
            END DO

            !  Account for the moisture between the surface and the first level up.

            IF ( ( p(i,level_above_sfc(i)-1)-psfc(i,j) .GE. 0. ) .AND. &
                 ( p(i,level_above_sfc(i)  )-psfc(i,j) .LT. 0. ) .AND. &
                 ( level_above_sfc(i) .GT. kts ) ) THEN
               p1 = psfc(i,j)
               p2 = p(i,level_above_sfc(i))
               t1 = tsfc(i,j)
               t2 = t(i,level_above_sfc(i))
               q1 = qsfc(i,j)
               q2 = q(i,level_above_sfc(i))
               z1 = zsfc(i,j)
               z2 = ght(i,level_above_sfc(i))
               rhobar = ( p1 / ( Rd * t1 ) + &
                          p2 / ( Rd * t2 ) ) * 0.5
               qbar   = ( q1 + q2 ) * 0.5
               dz     = z2 - z1
               IF ( dz .GT. 0.1 ) THEN
                  intq(i,j) = intq(i,j) + g * qbar * rhobar / (1. + qbar) * dz
               END IF

               !  Fix the underground values.

               DO k = level_above_sfc(i)-1,kts+1,-1
                  pd(i,k) = p(i,k) - intq(i,j)
               END DO
            END IF
            pd(i,kts) = psfc(i,j) - intq(i,j)

         END DO

         IF ( upside_down ) THEN
            DO i = its , MIN (ide-1 , ite )
               pd_out(i,kts,j) = pd(i,kts)
               DO k = kts+1,kte
                  pd_out(i,kte+2-k,j) = pd(i,k)
               END DO
            END DO
         ELSE
            DO i = its , MIN (ide-1 , ite )
               DO k = kts,kte
                  pd_out(i,k,j) = pd(i,k)
               END DO
            END DO
         END IF

      END DO

   END SUBROUTINE integ_moist

!---------------------------------------------------------------------

   SUBROUTINE rh_to_mxrat (rh, t, p, q , wrt_liquid , &
                           qv_max_p_safe , &
                           qv_max_flag , qv_max_value , &
                           qv_min_p_safe , &
                           qv_min_flag , qv_min_value , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      LOGICAL , INTENT(IN)        :: wrt_liquid

      REAL , INTENT(IN)           :: qv_max_p_safe , qv_max_flag , qv_max_value
      REAL , INTENT(IN)           :: qv_min_p_safe , qv_min_flag , qv_min_value

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
                  rh(i,k,j) = MIN ( MAX ( rh(i,k,j) ,  0. ) , 100. )
            END DO
         END DO
      END DO

      IF ( wrt_liquid ) THEN
         DO j = jts , MIN ( jde-1 , jte )
            DO k = kts , kte
               DO i = its , MIN (ide-1 , ite )

! es is reduced by RH here to avoid problems in low-pressure cases

                  es=.01*rh(i,k,j)*svp1*10.*EXP(svp2*(t(i,k,j)-svpt0)/(t(i,k,j)-svp3))
                  IF (es .ge. p(i,k,j)/100.)THEN
                    q(i,k,j)=0.0
                    print *,'warning: vapor pressure exceeds total pressure '
                    print *,'setting mixing ratio to 0'
                  ELSE
                    q(i,k,j)=eps*es/(p(i,k,j)/100.-es)
                  ENDIF
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

      !  For pressures above a defined level, reasonable Qv values should be
      !  a certain value or smaller.  If they are larger than this, the input data
      !  probably had "missing" RH, and we filled in some values.  This is an
      !  attempt to catch those.  Also, set the minimum value for the entire
      !  domain that is above the selected pressure level.
     
      DO j = jts , MIN ( jde-1 , jte )
         DO k = kts , kte
            DO i = its , MIN (ide-1 , ite )
               IF ( p(i,k,j) .LT. qv_max_p_safe ) THEN
                  IF ( q(i,k,j) .GT. qv_max_flag ) THEN
                     q(i,k,j) = qv_max_value
                  END IF
               END IF
               IF ( p(i,k,j) .LT. qv_min_p_safe ) THEN
                  IF ( q(i,k,j) .LT. qv_min_flag ) THEN
                     q(i,k,j) = qv_min_value
                  END IF
               END IF
            END DO
         END DO
      END DO

   END SUBROUTINE rh_to_mxrat

!---------------------------------------------------------------------

   SUBROUTINE compute_eta ( znw , &
                           eta_levels , max_eta , max_dz , &
                           p_top , g , p00 , cvpm , a , r_d , cp , t00 , p1000mb , t0 , tiso , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte )

      !  Compute eta levels, either using given values from the namelist (hardly
      !  a computation, yep, I know), or assuming a constant dz above the PBL,
      !  knowing p_top and the number of eta levels.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte
      REAL , INTENT(IN)           :: max_dz
      REAL , INTENT(IN)           :: p_top , g , p00 , cvpm , a , r_d , cp , t00 , p1000mb , t0 , tiso
      INTEGER , INTENT(IN)        :: max_eta
      REAL , DIMENSION (max_eta) , INTENT(IN)  :: eta_levels

      REAL , DIMENSION (kts:kte) , INTENT(OUT) :: znw

      !  Local vars

      INTEGER :: k
      REAL :: mub , t_init , p_surf , pb, ztop, ztop_pbl , dz , temp
      REAL , DIMENSION(kts:kte) :: dnw

      INTEGER , PARAMETER :: prac_levels = 17
      INTEGER :: loop , loop1
      REAL , DIMENSION(prac_levels) :: znw_prac , znu_prac , dnw_prac
      REAL , DIMENSION(kts:kte) :: alb , phb

      !  Gee, do the eta levels come in from the namelist?

      IF ( ABS(eta_levels(1)+1.) .GT. 0.0000001 ) THEN

         !  Check to see if the array is oriented OK, we can easily fix an upside down oops.

         IF      ( ( ABS(eta_levels(1  )-1.) .LT. 0.0000001 ) .AND. &
                   ( ABS(eta_levels(kde)-0.) .LT. 0.0000001 ) ) THEN
            DO k = kds+1 , kde-1
	       znw(k) = eta_levels(k)
            END DO
            znw(  1) = 1.
            znw(kde) = 0.
         ELSE IF ( ( ABS(eta_levels(kde)-1.) .LT. 0.0000001 ) .AND. &
                   ( ABS(eta_levels(1  )-0.) .LT. 0.0000001 ) ) THEN
            DO k = kds+1 , kde-1
	       znw(k) = eta_levels(kde+1-k)
            END DO
            znw(  1) = 1.
            znw(kde) = 0.
         ELSE
            CALL wrf_error_fatal ( 'First eta level should be 1.0 and the last 0.0 in namelist' )
         END IF

         !  Check to see if the input full-level eta array is monotonic.

         DO k = kds , kde-1
            IF ( znw(k) .LE. znw(k+1) ) THEN
               PRINT *,'eta on full levels is not monotonic'
               PRINT *,'eta (',k,') = ',znw(k)
               PRINT *,'eta (',k+1,') = ',znw(k+1)
               CALL wrf_error_fatal ( 'Fix non-monotonic "eta_levels" in the namelist.input file' )
            END IF
         END DO

      !  Compute eta levels assuming a constant delta z above the PBL.

      ELSE

         !  Compute top of the atmosphere with some silly levels.  We just want to
         !  integrate to get a reasonable value for ztop.  We use the planned PBL-esque
         !  levels, and then just coarse resolution above that.  We know p_top, and we
         !  have the base state vars.

         p_surf = p00

         znw_prac = (/ 1.000 , 0.993 , 0.983 , 0.970 , 0.954 , 0.934 , 0.909 , &
                       0.88 , 0.8 , 0.7 , 0.6 , 0.5 , 0.4 , 0.3 , 0.2 , 0.1 , 0.0 /)

         DO k = 1 , prac_levels - 1
            znu_prac(k) = ( znw_prac(k) + znw_prac(k+1) ) * 0.5
            dnw_prac(k) = znw_prac(k+1) - znw_prac(k)
         END DO

         DO k = 1, prac_levels-1
            pb = znu_prac(k)*(p_surf - p_top) + p_top
            temp = MAX ( tiso, t00 + A*LOG(pb/p00) )
!           temp =             t00 + A*LOG(pb/p00)
            t_init = temp*(p00/pb)**(r_d/cp) - t0
            alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
         END DO

         !  Base state mu is defined as base state surface pressure minus p_top

         mub = p_surf - p_top

         !  Integrate base geopotential, starting at terrain elevation.

         phb(1) = 0.
         DO k  = 2,prac_levels
               phb(k) = phb(k-1) - dnw_prac(k-1)*mub*alb(k-1)
         END DO

         !  So, now we know the model top in meters.  Get the average depth above the PBL
         !  of each of the remaining levels.  We are going for a constant delta z thickness.

         ztop     = phb(prac_levels) / g
         ztop_pbl = phb(8          ) / g
         dz = ( ztop - ztop_pbl ) / REAL ( kde - 8 )

         !  Standard levels near the surface so no one gets in trouble.

         DO k = 1 , 8
            znw(k) = znw_prac(k)
         END DO

         !  Using d phb(k)/ d eta(k) = -mub * alb(k), eqn 2.9
         !  Skamarock et al, NCAR TN 468.  Use full levels, so
         !  use twice the thickness.

         DO k = 8, kte-1-2
            pb = znw(k) * (p_surf - p_top) + p_top
            temp = MAX ( tiso, t00 + A*LOG(pb/p00) )
!           temp =             t00 + A*LOG(pb/p00)
            t_init = temp*(p00/pb)**(r_d/cp) - t0
            alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
            znw(k+1) = znw(k) - dz*g / ( mub*alb(k) )
         END DO
         znw(kte-2) = 0.000

         !  There is some iteration.  We want the top level, ztop, to be
         !  consistent with the delta z, and we want the half level values
         !  to be consistent with the eta levels.  The inner loop to 10 gets
         !  the eta levels very accurately, but has a residual at the top, due
         !  to dz changing.  We reset dz five times, and then things seem OK.

         DO loop1 = 1 , 5
            DO loop = 1 , 10
               DO k = 8, kte-1-2
                  pb = (znw(k)+znw(k+1))*0.5 * (p_surf - p_top) + p_top
                  temp = MAX ( tiso, t00 + A*LOG(pb/p00) )
!                 temp =             t00 + A*LOG(pb/p00)
                  t_init = temp*(p00/pb)**(r_d/cp) - t0
                  alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
                  znw(k+1) = znw(k) - dz*g / ( mub*alb(k) )
               END DO
               IF ( ( loop1 .EQ. 5 ) .AND. ( loop .EQ. 10 ) ) THEN
                  print *,'Converged znw(kte) should be about 0.0 = ',znw(kte-2)
               END IF
               znw(kte-2) = 0.000
            END DO

            !  Here is where we check the eta levels values we just computed.

            DO k = 1, kde-1-2
               pb = (znw(k)+znw(k+1))*0.5 * (p_surf - p_top) + p_top
               temp = MAX ( tiso, t00 + A*LOG(pb/p00) )
!              temp =             t00 + A*LOG(pb/p00)
               t_init = temp*(p00/pb)**(r_d/cp) - t0
               alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
            END DO

            phb(1) = 0.
            DO k  = 2,kde-2
                  phb(k) = phb(k-1) - (znw(k)-znw(k-1)) * mub*alb(k-1)
            END DO

            !  Reset the model top and the dz, and iterate.

            ztop = phb(kde-2)/g
            ztop_pbl = phb(8)/g
            dz = ( ztop - ztop_pbl ) / REAL ( (kde-2) - 8 )
         END DO

         IF ( dz .GT. max_dz ) THEN
print *,'z (m)            = ',phb(1)/g
do k = 2 ,kte-2
print *,'z (m) and dz (m) = ',phb(k)/g,(phb(k)-phb(k-1))/g
end do
print *,'dz (m) above fixed eta levels = ',dz
print *,'namelist max_dz (m) = ',max_dz
print *,'namelist p_top (Pa) = ',p_top
            CALL wrf_debug ( 0, 'You need one of three things:' )
            CALL wrf_debug ( 0, '1) More eta levels to reduce the dz: e_vert' )
            CALL wrf_debug ( 0, '2) A lower p_top so your total height is reduced: p_top_requested')
            CALL wrf_debug ( 0, '3) Increase the maximum allowable eta thickness: max_dz')
            CALL wrf_debug ( 0, 'All are namelist options')
            CALL wrf_error_fatal ( 'dz above fixed eta levels is too large')
         END IF

         !  Add those 2 levels back into the middle, just above the 8 levels
         !  that semi define a boundary layer.  After we open up the levels,
         !  then we just linearly interpolate in znw.  So now levels 1-8 are
         !  specified as the fixed boundary layer levels given in this routine.
         !  The top levels, 12 through kte are those computed.  The middle
         !  levels 9, 10, and 11 are equi-spaced in znw, and are each 1/2 the
         !  the znw thickness of levels 11 through 12.

         DO k = kte-2 , 9 , -1
            znw(k+2) = znw(k)
         END DO

         znw( 9) = 0.75 * znw( 8) + 0.25 * znw(12)
         znw(10) = 0.50 * znw( 8) + 0.50 * znw(12)
         znw(11) = 0.25 * znw( 8) + 0.75 * znw(12)

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
      INTEGER :: julyr , julday , int_month , month1 , month2
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
                  int_month = l
                  IF ( ( int_month .EQ. 0 ) .OR. ( int_month .EQ. 12 ) ) THEN
                     month1 = 12
                     month2 =  1
                  ELSE
                     month1 = int_month
                     month2 = month1 + 1
                  END IF
                  field_out(i,j) =  ( field_in(i,month2,j) * ( target_date - middle(l)   ) + &
                                      field_in(i,month1,j) * ( middle(l+1) - target_date ) ) / &
                                    ( middle(l+1) - middle(l) )
               END DO
            END DO
            EXIT find_month
         END IF
      END DO find_month

   END SUBROUTINE monthly_interp_to_date

!---------------------------------------------------------------------

   SUBROUTINE sfcprs (t, q, height, pslv, ter, avgsfct, p, &
                      psfc, ez_method, &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )


      !  Computes the surface pressure using the input height,
      !  temperature and q (already computed from relative
      !  humidity) on p surfaces.  Sea level pressure is used
      !  to extrapolate a first guess.

      IMPLICIT NONE

      REAL, PARAMETER    :: gamma     = 6.5E-3
      REAL, PARAMETER    :: pconst    = 10000.0
      REAL, PARAMETER    :: Rd        = r_d
      REAL, PARAMETER    :: TC        = svpt0 + 17.5

      REAL, PARAMETER    :: gammarg   = gamma * Rd / g
      REAL, PARAMETER    :: rov2      = Rd / 2.

      INTEGER , INTENT(IN) ::  ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte
      LOGICAL , INTENT ( IN ) :: ez_method

      REAL , DIMENSION (ims:ime,kms:kme,jms:jme) , INTENT(IN ):: t, q, height, p
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(IN ):: pslv ,  ter, avgsfct
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(OUT):: psfc

      INTEGER                     :: i
      INTEGER                     :: j
      INTEGER                     :: k
      INTEGER , DIMENSION (its:ite,jts:jte) :: k500 , k700 , k850

      LOGICAL                     :: l1
      LOGICAL                     :: l2
      LOGICAL                     :: l3
      LOGICAL                     :: OK

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

      IF ( ez_method ) THEN

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

         i = its
         j = jts
         DO k = kts+1 , kte
            IF      (NINT(p(i,k,j)) .EQ. 85000) THEN
               k850(i,j) = k
            ELSE IF (NINT(p(i,k,j)) .EQ. 70000) THEN
               k700(i,j) = k
            ELSE IF (NINT(p(i,k,j)) .EQ. 50000) THEN
               k500(i,j) = k
            END IF
         END DO

         IF ( ( k850(i,j) .EQ. 0 ) .OR. ( k700(i,j) .EQ. 0 ) .OR. ( k500(i,j) .EQ. 0 ) ) THEN

            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  psfc(i,j) = pslv(i,j) * ( 1.0 + gamma * ter(i,j) / t(i,1,j) ) ** ( - g / ( Rd * gamma ) )
               END DO
            END DO

            RETURN
#if 0

            !  Possibly it is just that we have a generalized vertical coord, so we do not
            !  have the values exactly.  Do a simple assignment to a close vertical level.

            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  DO k = kts+1 , kte-1
                     IF ( ( p(i,k,j) - 85000. )  * ( p(i,k+1,j) - 85000. ) .LE. 0.0 ) THEN
                        k850(i,j) = k
                     END IF
                     IF ( ( p(i,k,j) - 70000. )  * ( p(i,k+1,j) - 70000. ) .LE. 0.0 ) THEN
                        k700(i,j) = k
                     END IF
                     IF ( ( p(i,k,j) - 50000. )  * ( p(i,k+1,j) - 50000. ) .LE. 0.0 ) THEN
                        k500(i,j) = k
                     END IF
                  END DO
               END DO
            END DO

            !  If we *still* do not have the k levels, punt.  I mean, we did try.

            OK = .TRUE.
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( ( k850(i,j) .EQ. 0 ) .OR. ( k700(i,j) .EQ. 0 ) .OR. ( k500(i,j) .EQ. 0 ) ) THEN
                     OK = .FALSE.
                     PRINT '(A)','(i,j) = ',i,j,'  Error in finding p level for 850, 700 or 500 hPa.'
                     DO K = kts+1 , kte
                        PRINT '(A,I3,A,F10.2,A)','K = ',k,'  PRESSURE = ',p(i,k,j),' Pa'
                     END DO
                     PRINT '(A)','Expected 850, 700, and 500 mb values, at least.'
                  END IF
               END DO
            END DO
            IF ( .NOT. OK ) THEN
               CALL wrf_error_fatal ( 'wrong pressure levels' )
            END IF
#endif

         !  We are here if the data is isobaric and we found the levels for 850, 700,
         !  and 500 mb right off the bat.

         ELSE
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  k850(i,j) = k850(its,jts)
                  k700(i,j) = k700(its,jts)
                  k500(i,j) = k500(its,jts)
               END DO
            END DO
         END IF

         !  The 850 hPa level of geopotential height is called something special.

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               ht(i,j) = height(i,k850(i,j),j)
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
               psfc(i,j) = pslv(i,j) * (pslv(i,j) / p(i,k850(i,j),j)) ** ht(i,j)
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
               t850(i,j) = t(i,k850(i,j),j) * (1. + 0.608 * q(i,k850(i,j),j))
               t700(i,j) = t(i,k700(i,j),j) * (1. + 0.608 * q(i,k700(i,j),j))
               t500(i,j) = t(i,k500(i,j),j) * (1. + 0.608 * q(i,k500(i,j),j))
            END DO
         END DO

         !  Compute lapse rates between these three levels.  These are
         !  environmental values for each (i,j).

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               gamma78(i,j) = ALOG(t850(i,j) / t700(i,j))  / ALOG (p(i,k850(i,j),j) / p(i,k700(i,j),j) )
               gamma57(i,j) = ALOG(t700(i,j) / t500(i,j))  / ALOG (p(i,k700(i,j),j) / p(i,k500(i,j),j) )
            END DO
         END DO

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF      ( ( psfc(i,j) - 95000. ) .GE. 0. ) THEN
                  t1(i,j) = t850(i,j)
               ELSE IF ( ( psfc(i,j) - 85000. ) .GE. 0. ) THEN
                  t1(i,j) = t700(i,j) * (p1(i,j) / (p(i,k700(i,j),j))) ** gamma78(i,j)
               ELSE IF ( ( psfc(i,j) - 70000. ) .GE. 0.) THEN
                  t1(i,j) = t500(i,j) * (p1(i,j) / (p(i,k500(i,j),j))) ** gamma57(i,j)
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

   SUBROUTINE sfcprs2(t, q, height, psfc_in, ter, avgsfct, p, &
                      psfc, ez_method, &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )


      !  Computes the surface pressure using the input height,
      !  temperature and q (already computed from relative
      !  humidity) on p surfaces.  Sea level pressure is used
      !  to extrapolate a first guess.

      IMPLICIT NONE

      REAL, PARAMETER    :: Rd        = r_d

      INTEGER , INTENT(IN) ::  ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte
      LOGICAL , INTENT ( IN ) :: ez_method

      REAL , DIMENSION (ims:ime,kms:kme,jms:jme) , INTENT(IN ):: t, q, height, p
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(IN ):: psfc_in ,  ter, avgsfct
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(OUT):: psfc

      INTEGER                     :: i
      INTEGER                     :: j
      INTEGER                     :: k

      REAL :: tv_sfc_avg , tv_sfc , del_z

      !  Compute the new surface pressure from the old surface pressure, and a
      !  known change in elevation at the surface.

      !  del_z = diff in surface topo, lo-res vs hi-res
      !  psfc = psfc_in * exp ( g del_z / (Rd Tv_sfc ) )


      IF ( ez_method ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               tv_sfc_avg = avgsfct(i,j) * (1. + 0.608 * q(i,1,j))
               del_z = height(i,1,j) - ter(i,j)
               psfc(i,j) = psfc_in(i,j) * EXP ( g * del_z / ( Rd * tv_sfc_avg ) )
            END DO
         END DO
      ELSE
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               tv_sfc = t(i,1,j) * (1. + 0.608 * q(i,1,j))
               del_z = height(i,1,j) - ter(i,j)
               psfc(i,j) = psfc_in(i,j) * EXP ( g * del_z / ( Rd * tv_sfc     ) )
            END DO
         END DO
      END IF

   END SUBROUTINE sfcprs2

!---------------------------------------------------------------------

   SUBROUTINE sfcprs3( height , p , ter , slp , psfc , &
                       ids , ide , jds , jde , kds , kde , &
                       ims , ime , jms , jme , kms , kme , &
                       its , ite , jts , jte , kts , kte )

      !  Computes the surface pressure by vertically interpolating
      !  linearly (or log) in z the pressure, to the targeted topography.

      IMPLICIT NONE

      INTEGER , INTENT(IN) ::  ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte

      REAL , DIMENSION (ims:ime,kms:kme,jms:jme) , INTENT(IN ):: height, p
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(IN ):: ter , slp
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(OUT):: psfc

      INTEGER                     :: i
      INTEGER                     :: j
      INTEGER                     :: k

      LOGICAL                     :: found_loc

      REAL :: zl , zu , pl , pu , zm

      !  Loop over each grid point

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)

            !  Special case where near the ocean level.  Assume that the SLP is a good value.

            IF      ( ter(i,j) .LT. 50 ) THEN
               psfc(i,j) = slp(i,j) + ( p(i,2,j)-p(i,3,j) ) / ( height(i,2,j)-height(i,3,j) ) * ter(i,j)
               CYCLE
            END IF

            !  Find the trapping levels

            found_loc = .FALSE.

            !  Normal sort of scenario - the model topography is somewhere between
            !  the height values of 1000 mb and the top of the model.

            found_k_loc : DO k = kts+1 , kte-2
               IF ( ( height(i,k  ,j) .LE. ter(i,j) ) .AND. &
                    ( height(i,k+1,j) .GT. ter(i,j) ) ) THEN
                  zl = height(i,k  ,j)
                  zu = height(i,k+1,j)
                  zm = ter(i,j)
                  pl = p(i,k  ,j)
                  pu = p(i,k+1,j)
                  psfc(i,j) = EXP ( ( LOG(pl) * ( zm - zu ) + LOG(pu) * ( zl - zm ) ) / ( zl - zu ) )
                  found_loc = .TRUE.
                  EXIT found_k_loc
               END IF
            END DO found_k_loc

            !  Interpolate betwixt slp and the first isobaric level above - this is probably the
            !  usual thing over the ocean.

            IF ( .NOT. found_loc ) THEN
               IF ( slp(i,j) .GE. p(i,2,j) ) THEN
                  zl = 0.
                  zu = height(i,3,j)
                  zm = ter(i,j)
                  pl = slp(i,j)
                  pu = p(i,3,j)
                  psfc(i,j) = EXP ( ( LOG(pl) * ( zm - zu ) + LOG(pu) * ( zl - zm ) ) / ( zl - zu ) )
                  found_loc = .TRUE.
               ELSE
                  found_slp_loc : DO k = kts+1 , kte-3
                     IF ( ( slp(i,j) .GE. p(i,k+1,j) ) .AND. &
                          ( slp(i,j) .LT. p(i,k  ,j) ) ) THEN
                        zl = 0.
                        zu = height(i,k+1,j)
                        zm = ter(i,j)
                        pl = slp(i,j)
                        pu = p(i,k+1,j)
                        psfc(i,j) = EXP ( ( LOG(pl) * ( zm - zu ) + LOG(pu) * ( zl - zm ) ) / ( zl - zu ) )
                        found_loc = .TRUE.
                        EXIT found_slp_loc
                     END IF
                  END DO found_slp_loc
               END IF
            END IF

            !  Did we do what we wanted done.

            IF ( .NOT. found_loc ) THEN
               print *,'i,j = ',i,j
               print *,'p column = ',p(i,2:,j)
               print *,'z column = ',height(i,2:,j)
               print *,'model topo = ',ter(i,j)
               CALL wrf_error_fatal ( ' probs with sfc p computation ' )
            END IF

         END DO
      END DO

   END SUBROUTINE sfcprs3
!---------------------------------------------------------------------

   SUBROUTINE filter_topo ( ht_in , xlat , msftx , fft_filter_lat , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) ::  ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte

      REAL , INTENT(IN) :: fft_filter_lat
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: ht_in
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: xlat , msftx


      !  Local vars

      INTEGER :: i , j , j_lat_pos , j_lat_neg
      INTEGER :: i_kicker , ik , i1, i2, i3, i4
      REAL :: length_scale , sum
      REAL , DIMENSION(its:ite,jts:jte) :: ht_out

      !  The filtering is a simple average on a latitude loop.  Possibly a LONG list of
      !  numbers.  We assume that ALL of the 2d arrays have been transposed so that
      !  each patch has the entire domain size of the i-dim local.

      IF ( ( its .NE. ids ) .OR. ( ite .NE. ide ) ) THEN
         CALL wrf_error_fatal ( 'filtering assumes all values on X' )
      END IF

      !  Starting at the south pole, we find where the
      !  grid distance is big enough, then go back a point.  Continuing to the
      !  north pole, we find the first small grid distance.  These are the
      !  computational latitude loops and the associated computational poles.

      j_lat_neg = 0
      j_lat_pos = jde + 1
      loop_neg : DO j = jts , MIN(jde-1,jte)
         IF ( xlat(its,j) .LT. 0.0 ) THEN
            IF ( ABS(xlat(its,j)) .LT. fft_filter_lat ) THEN
               j_lat_neg = j - 1
               EXIT loop_neg
            END IF
         END IF
      END DO loop_neg

      loop_pos : DO j = jts , MIN(jde-1,jte)
         IF ( xlat(its,j) .GT. 0.0 ) THEN
            IF ( xlat(its,j) .GE. fft_filter_lat ) THEN
               j_lat_pos = j
               EXIT loop_pos
            END IF
         END IF
      END DO loop_pos

      !  Set output values to initial input topo values for whole patch.

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            ht_out(i,j) = ht_in(i,j)
         END DO
      END DO

      !  Filter the topo at the negative lats.

      DO j = j_lat_neg , jts , -1
         i_kicker = MIN( MAX ( NINT(msftx(its,j)) , 1 ) , (ide - ids) / 2 )
         print *,'j = ' , j, ', kicker = ',i_kicker
         DO i = its , MIN(ide-1,ite)
            IF      ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
               sum = 0.0
               DO ik = 1 , i_kicker
                  sum = sum + ht_in(i+ik,j) + ht_in(i-ik,j)
               END DO
               ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
            ELSE IF ( ( i - i_kicker .LT. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
               sum = 0.0
               DO ik = 1 , i_kicker
                  sum = sum + ht_in(i+ik,j)
               END DO
               i1 = i - i_kicker + ide -1
               i2 = ide-1
               i3 = ids
               i4 = i-1
               DO ik = i1 , i2
                  sum = sum + ht_in(ik,j)
               END DO
               DO ik = i3 , i4
                  sum = sum + ht_in(ik,j)
               END DO
               ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
            ELSE IF ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .GT. ide-1 ) ) THEN
               sum = 0.0
               DO ik = 1 , i_kicker
                  sum = sum + ht_in(i-ik,j)
               END DO
               i1 = i+1
               i2 = ide-1
               i3 = ids
               i4 = ids + ( i_kicker+i ) - ide
               DO ik = i1 , i2
                  sum = sum + ht_in(ik,j)
               END DO
               DO ik = i3 , i4
                  sum = sum + ht_in(ik,j)
               END DO
               ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
            END IF
         END DO
      END DO

      !  Filter the topo at the positive lats.

      DO j = j_lat_pos , MIN(jde-1,jte)
         i_kicker = MIN( MAX ( NINT(msftx(its,j)) , 1 ) , (ide - ids) / 2 )
         print *,'j = ' , j, ', kicker = ',i_kicker
         DO i = its , MIN(ide-1,ite)
            IF      ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
               sum = 0.0
               DO ik = 1 , i_kicker
                  sum = sum + ht_in(i+ik,j) + ht_in(i-ik,j)
               END DO
               ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
            ELSE IF ( ( i - i_kicker .LT. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
               sum = 0.0
               DO ik = 1 , i_kicker
                  sum = sum + ht_in(i+ik,j)
               END DO
               i1 = i - i_kicker + ide -1
               i2 = ide-1
               i3 = ids
               i4 = i-1
               DO ik = i1 , i2
                  sum = sum + ht_in(ik,j)
               END DO
               DO ik = i3 , i4
                  sum = sum + ht_in(ik,j)
               END DO
               ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
            ELSE IF ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .GT. ide-1 ) ) THEN
               sum = 0.0
               DO ik = 1 , i_kicker
                  sum = sum + ht_in(i-ik,j)
               END DO
               i1 = i+1
               i2 = ide-1
               i3 = ids
               i4 = ids + ( i_kicker+i ) - ide
               DO ik = i1 , i2
                  sum = sum + ht_in(ik,j)
               END DO
               DO ik = i3 , i4
                  sum = sum + ht_in(ik,j)
               END DO
               ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
            END IF
         END DO
      END DO

      !  Set output values to initial input topo values for whole patch.

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            ht_in(i,j) = ht_out(i,j)
         END DO
      END DO

   END SUBROUTINE filter_topo

!---------------------------------------------------------------------

   SUBROUTINE init_module_initialize
   END SUBROUTINE init_module_initialize

!---------------------------------------------------------------------

END MODULE module_initialize_real
#endif
