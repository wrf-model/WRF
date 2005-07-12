!-------------------------------------------------------------------

   SUBROUTINE start_domain_em ( grid, allowed_to_read , &
! Actual arguments generated from Registry
# include <em_dummy_args.inc>
!
)

   USE module_domain
   USE module_dm
!  USE module_io_domain
   USE module_state_description
   USE module_model_constants
   USE module_bc
   USE module_bc_em
!  USE module_timing
   USE module_configure
   USE module_date_time
   USE module_tiles

   USE module_physics_init

#ifdef DM_PARALLEL
   USE module_dm
#endif

!!debug
!USE module_compute_geop

   USE module_model_constants
   IMPLICIT NONE
   !  Input data.
   TYPE (domain)          :: grid

   LOGICAL , INTENT(IN)   :: allowed_to_read

   !  Definitions of dummy arguments to this routine (generated from Registry).
# include <em_dummy_decl.inc>

   !  Structure that contains run-time configuration (namelist) data for domain
   TYPE (grid_config_rec_type)              :: config_flags

   !  Local data
   INTEGER                             ::                       &
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  ips, ipe, jps, jpe, kps, kpe, &
                                  its, ite, jts, jte, kts, kte, &
                                  ij,i,j,k,ii,jj,kk,loop,error,l
                                  
! These are used if -DDEREF_KLUDGE is compiled
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31  , em31  , sm32  , em32  , sm33  , em33
   INTEGER     :: sm31x , em31x , sm32x , em32x , sm33x , em33x
   INTEGER     :: sm31y , em31y , sm32y , em32y , sm33y , em33y

   INTEGER     :: hr, mn, sec, ms, rc

   CHARACTER*80 :: message

   REAL        :: p00, t00, a, p_surf, pd_surf

   REAL :: qvf1, qvf2, qvf
   REAL :: MPDT
   LOGICAL :: first_trip_for_this_domain

   REAL :: lat1 , lat2 , lat3 , lat4
   REAL :: lon1 , lon2 , lon3 , lon4
   INTEGER :: num_points_lat_lon , iloc , jloc
!integer myproc

! Trick problematic compilers into not performing copy-in/copy-out by adding
! indices to array arguments in the CALL statements in this routine.
! It has the effect of passing only the first element of the array, rather 
! than the entire array.  See:  
! http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
#include "deref_kludge.h"

! Limit the number of arguments if compiled with -DLIMIT_ARGS by copying 
! scalar (non-array) arguments out of the grid data structure into locally
! defined copies (defined in em_dummy_decl.inc, above, as they are if they
! are arguments). An equivalent include of em_scalar_derefs.inc appears
! at the end of the routine to copy back any chnaged non-array values.
! The definition of COPY_IN or COPY_OUT before the include defines the
! direction of the copy.  Em_scalar_derefs.inc is generated from Registry
#define COPY_IN
#include <em_scalar_derefs.inc>

! Needed by some comm layers, e.g. RSL. If needed, nmm_data_calls.inc is
! generated from the registry.  The definition of REGISTER_I1 allows
! I1 data to be communicated in this routine if necessary.
#ifdef DM_PARALLEL
#    include <em_data_calls.inc>
#endif
   CALL get_ijk_from_grid (  grid ,                   &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )

   kts = kps ; kte = kpe     ! note that tile is entire patch
   its = ips ; ite = ipe     ! note that tile is entire patch
   jts = jps ; jte = jpe    ! note that tile is entire patch

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

! here we check to see if the boundary conditions are set properly

   CALL boundary_condition_check( config_flags, bdyzone, error, grid%id )

!kludge - need to stop CG from resetting precip and phys tendencies to zero
!         when we are in here due to a nest being spawned, we want to still
!         recompute the base state, but that is about it
   !  This is temporary and will need to be changed when itimestep is removed.

   IF ( itimestep .EQ. 0 ) THEN
      first_trip_for_this_domain = .TRUE.
   ELSE
      first_trip_for_this_domain = .FALSE.
   END IF

   IF ( .not. restart ) THEN 
       itimestep=0
   ENDIF

   IF ( restart .or. moved ) THEN 
       first_trip_for_this_domain = .TRUE.
   ENDIF
   
   IF (config_flags%specified) THEN
!
! Arrays for specified boundary conditions


     DO loop = spec_zone + 1, spec_zone + relax_zone
       fcx(loop) = 0.1 / dt * (spec_zone + relax_zone - loop) / (relax_zone - 1)
       gcx(loop) = 1.0 / dt / 50. * (spec_zone + relax_zone - loop) / (relax_zone - 1)
     ENDDO

   ELSE IF (config_flags%nested) THEN
!
! Arrays for specified boundary conditions

     DO loop = spec_zone + 1, spec_zone + relax_zone
!       fcx(loop) = 0.1 / dt * (spec_zone + relax_zone - loop) / (relax_zone - 1)
!       gcx(loop) = 1.0 / dt / 50. * (spec_zone + relax_zone - loop) / (relax_zone - 1)
       fcx(loop) = 0.
       gcx(loop) = 0.
     ENDDO

     dtbc = 0.

   ENDIF


!call wrf_get_myproc(myproc)

      IF ( grid%id .NE. 1 ) THEN
   
      !  Every time a domain starts or every time a domain moves, this routine is called.  We want
      !  the center (middle) lat/lon of the grid for the metacode.  The lat/lon values are
      !  defined at mass points.  Depending on the even/odd points in the SN and WE directions,
      !  we end up with the middle point as either 1 point or an average of either 2 or 4 points.
      !  Add to this, the need to make sure that we are on the correct patch to retrieve the
      !  value of the lat/lon, AND that the lat/lons (for an average) may not all be on the same
      !  patch.  Once we find the correct value for lat lon, we need to keep it around on all patches, 
      !  which is where the wrf_dm_min_real calls come in.
   
      IF      ( ( MOD(ide,2) .EQ. 0 ) .AND. ( MOD(jde,2) .EQ. 0 ) ) THEN
         num_points_lat_lon = 1
         iloc = ide/2
         jloc = jde/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat1 = xlat (iloc,jloc)
            lon1 = xlong(iloc,jloc)
         ELSE
            lat1 = 99999.
            lon1 = 99999.
         END IF
         lat1 = wrf_dm_min_real ( lat1 )
         lon1 = wrf_dm_min_real ( lon1 )
         CALL nl_set_cen_lat ( grid%id , lat1 )
         CALL nl_set_cen_lon ( grid%id , lon1 )
      ELSE IF ( ( MOD(ide,2) .NE. 0 ) .AND. ( MOD(jde,2) .EQ. 0 ) ) THEN
         num_points_lat_lon = 2
         iloc = (ide-1)/2
         jloc =  jde   /2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat1 = xlat (iloc,jloc)
            lon1 = xlong(iloc,jloc)
         ELSE
            lat1 = 99999.
            lon1 = 99999.
         END IF
         lat1 = wrf_dm_min_real ( lat1 )
         lon1 = wrf_dm_min_real ( lon1 )
   
         iloc = (ide+1)/2
         jloc =  jde   /2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat2 = xlat (iloc,jloc)
            lon2 = xlong(iloc,jloc)
         ELSE
            lat2 = 99999.
            lon2 = 99999.
         END IF
         lat2 = wrf_dm_min_real ( lat2 )
         lon2 = wrf_dm_min_real ( lon2 )
   
         CALL nl_set_cen_lat ( grid%id , ( lat1 + lat2 ) * 0.50 )
         CALL nl_set_cen_lon ( grid%id , ( lon1 + lon2 ) * 0.50 )
      ELSE IF ( ( MOD(ide,2) .EQ. 0 ) .AND. ( MOD(jde,2) .NE. 0 ) ) THEN
         num_points_lat_lon = 2
         iloc =  ide   /2
         jloc = (jde-1)/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat1 = xlat (iloc,jloc)
            lon1 = xlong(iloc,jloc)
         ELSE
            lat1 = 99999.
            lon1 = 99999.
         END IF
         lat1 = wrf_dm_min_real ( lat1 )
         lon1 = wrf_dm_min_real ( lon1 )
   
         iloc =  ide   /2
         jloc = (jde+1)/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat2 = xlat (iloc,jloc)
            lon2 = xlong(iloc,jloc)
         ELSE
            lat2 = 99999.
            lon2 = 99999.
         END IF
         lat2 = wrf_dm_min_real ( lat2 )
         lon2 = wrf_dm_min_real ( lon2 )
   
         CALL nl_set_cen_lat ( grid%id , ( lat1 + lat2 ) * 0.50 )
         CALL nl_set_cen_lon ( grid%id , ( lon1 + lon2 ) * 0.50 )
      ELSE IF ( ( MOD(ide,2) .NE. 0 ) .AND. ( MOD(jde,2) .NE. 0 ) ) THEN
         num_points_lat_lon = 4
         iloc = (ide-1)/2
         jloc = (jde-1)/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat1 = xlat (iloc,jloc)
            lon1 = xlong(iloc,jloc)
         ELSE
            lat1 = 99999.
            lon1 = 99999.
         END IF
         lat1 = wrf_dm_min_real ( lat1 )
         lon1 = wrf_dm_min_real ( lon1 )
   
         iloc = (ide+1)/2
         jloc = (jde-1)/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat2 = xlat (iloc,jloc)
            lon2 = xlong(iloc,jloc)
         ELSE
            lat2 = 99999.
            lon2 = 99999.
         END IF
         lat2 = wrf_dm_min_real ( lat2 )
         lon2 = wrf_dm_min_real ( lon2 )
   
         iloc = (ide-1)/2
         jloc = (jde+1)/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat3 = xlat (iloc,jloc)
            lon3 = xlong(iloc,jloc)
         ELSE
            lat3 = 99999.
            lon3 = 99999.
         END IF
         lat3 = wrf_dm_min_real ( lat3 )
         lon3 = wrf_dm_min_real ( lon3 )
   
         iloc = (ide+1)/2
         jloc = (jde+1)/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat4 = xlat (iloc,jloc)
            lon4 = xlong(iloc,jloc)
         ELSE
            lat4 = 99999.
            lon4 = 99999.
         END IF
         lat4 = wrf_dm_min_real ( lat4 )
         lon4 = wrf_dm_min_real ( lon4 )
   
         CALL nl_set_cen_lat ( grid%id , ( lat1 + lat2 + lat3 + lat4 ) * 0.25 )
         CALL nl_set_cen_lon ( grid%id , ( lon1 + lon2 + lon3 + lon4 ) * 0.25 )
      END IF
   END IF

   

   
   


   IF ( ( config_flags%input_from_hires ) .OR. ( config_flags%input_from_file ) ) THEN

      CALL nl_get_base_pres  ( 1 , p00 )
      CALL nl_get_base_temp  ( 1 , t00 )
      CALL nl_get_base_lapse ( 1 , a   )

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

            !  Integrate base geopotential, starting at terrain elevation.  This assures that
            !  the base state is in exact hydrostatic balance with respect to the model equations.
            !  This field is on full levels.

            phb(i,1,j) = ht(i,j) * g
            DO k  = 2,kte
               phb(i,k,j) = phb(i,k-1,j) - dnw(k-1)*mub(i,j)*alb(i,k-1,j)
            END DO
         END DO
      END DO

   ENDIF

   IF(.not.config_flags%restart)THEN

!  if this is for a nested domain, the defined/interpolated fields are the _2

     IF ( first_trip_for_this_domain ) THEN

! data that is expected to be zero must be explicitly initialized as such
     h_diabatic = 0.

       DO j = jts,min(jte,jde-1)
       DO k = kts,kte-1
       DO i = its, min(ite,ide-1)
           IF ( imask_nostag(i,j) .EQ. 1 ) THEN
             t_1(i,k,j)=t_2(i,k,j)
           ENDIF
       ENDDO
       ENDDO
       ENDDO
  
       DO j = jts,min(jte,jde-1)
       DO i = its, min(ite,ide-1)
           IF ( imask_nostag(i,j) .EQ. 1 ) THEN
             mu_1(i,j)=mu_2(i,j)
           ENDIF
       ENDDO
       ENDDO
     END IF

!  reconstitute base-state fields

     DO j = jts,min(jte,jde-1)
     DO k = kts,kte-1
     DO i = its, min(ite,ide-1)
       IF ( imask_nostag(i,j) .EQ. 1 ) THEN
         pb(i,k,j) = znu(k)*mub(i,j)+p_top
         alb(i,k,j) = -rdnw(k)*(phb(i,k+1,j)-phb(i,k,j))/mub(i,j)
         t_init(i,k,j) = alb(i,k,j)*(p1000mb/r_d)/((pb(i,k,j)/p1000mb)**cvpm) - t0
       ENDIF
     ENDDO
     ENDDO
     ENDDO

     DO j = jts,min(jte,jde-1)

       k = kte-1
       DO i = its, min(ite,ide-1)
         IF ( imask_nostag(i,j) .EQ. 1 ) THEN
           qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k,j,P_QV))
           qvf2 = 1./(1.+qvf1)
           qvf1 = qvf1*qvf2
           p(i,k,j) = - 0.5*(mu_1(i,j)+qvf1*mub(i,j))/rdnw(k)/qvf2
           qvf = 1. + rvovrd*moist(i,k,j,P_QV)
           alt(i,k,j) = (r_d/p1000mb)*(t_1(i,k,j)+t0)*qvf*(((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm)
           al(i,k,j) = alt(i,k,j) - alb(i,k,j)
         ENDIF
       ENDDO

       DO k = kte-2, 1, -1
       DO i = its, min(ite,ide-1)
         IF ( imask_nostag(i,j) .EQ. 1 ) THEN
           qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k+1,j,P_QV))
           qvf2 = 1./(1.+qvf1)
           qvf1 = qvf1*qvf2
           p(i,k,j) = p(i,k+1,j) - (mu_1(i,j) + qvf1*mub(i,j))/qvf2/rdn(k+1)
           qvf = 1. + rvovrd*moist(i,k,j,P_QV)
           alt(i,k,j) = (r_d/p1000mb)*(t_1(i,k,j)+t0)*qvf* &
                        (((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm)
           al(i,k,j) = alt(i,k,j) - alb(i,k,j)
         ENDIF
       ENDDO
       ENDDO

     ENDDO

   ENDIF

   IF ( ( grid%id .NE. 1 ) .AND. &
       ( ( config_flags%input_from_hires ) .OR. ( config_flags%input_from_file ) ) ) THEN
      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
            mu_2(i,j) = mu_2(i,j) + al(i,1,j) / ( alt(i,1,j) * alb(i,1,j) ) * &
                                    g * ( ht(i,j) - ht_fine(i,j) )
         END DO
       END DO
       DO j = jts,min(jte,jde-1)
       DO i = its, min(ite,ide-1)
          mu_1(i,j)=mu_2(i,j)
       ENDDO
       ENDDO

   END IF

   IF ( first_trip_for_this_domain ) THEN

   CALL wrf_debug ( 100 , 'module_start: start_domain_rk: Before call to phy_init' )

! namelist MPDT does not exist yet, so set it here
! MPDT is the call frequency for microphysics in minutes (0 means every step)
   MPDT = 0.

! set GMT outside of phy_init because phy_init may not be called on this
! process if, for example, it is a moving nest and if this part of the domain is not
! being initialized (not the leading edge).
   IF ( .NOT. grid%restart ) THEN
     CALL wrf_clockprint(150, grid%domain_clock, &
                         'DEBUG phy_init():  get currTime from grid%domain_clock,')
     CALL WRF_UTIL_ClockGet( grid%domain_clock, currTime=grid%current_time, rc=rc )
     CALL WRF_UTIL_TimeGet( grid%current_time, YY=julyr, dayOfYear=julday, H=hr, M=mn, S=sec, MS=ms, rc=rc)
     CALL wrf_timetoa ( grid%current_time, message )
     WRITE( wrf_err_message , * ) 'DEBUG start_em():  currTime = [',TRIM( message ),']'
     CALL wrf_debug( 150, TRIM(wrf_err_message) )
     gmt=hr+real(mn)/60.+real(sec)/3600.+real(ms)/(1000*3600)
     WRITE( wrf_err_message , * ) 'DEBUG start_em():  julyr,hr,mn,sec,ms,julday = ', &
                                  julyr,hr,mn,sec,ms,julday
     CALL wrf_debug( 150, TRIM(wrf_err_message) )
     WRITE( wrf_err_message , * ) 'DEBUG start_em():  gmt = ',gmt
     CALL wrf_debug( 150, TRIM(wrf_err_message) )
   ELSE
     CALL nl_get_julyr (grid%id, julyr)
     CALL nl_get_julday (grid%id, julday)
     CALL nl_get_gmt (grid%id, gmt)
   END IF

   CALL set_tiles ( grid , imask_nostag, ims, ime, jms, jme, ips, ipe, jps, jpe )

! Phy_init is not necessarily thread-safe; do not multi-thread this loop.
! The tiling is to handle the fact that we may be masking off part of the computation.
   DO ij = 1, grid%num_tiles

     CALL phy_init (  grid,                                   &  
                      grid%id , config_flags, DT, znw, znu,   &
                      p_top, TSK, RADT,BLDT,CUDT, MPDT,       &
                      RTHCUTEN, RQVCUTEN, RQRCUTEN,           &
                      RQCCUTEN, RQSCUTEN, RQICUTEN,           &
                      RUBLTEN,RVBLTEN,RTHBLTEN,               &
                      RQVBLTEN,RQCBLTEN,RQIBLTEN,             &
                      RTHRATEN,RTHRATENLW,RTHRATENSW,         &
                      STEPBL,STEPRA,STEPCU,                   &
                      W0AVG, RAINNC, RAINC, RAINCV, RAINNCV,  &
                      NCA,                                    &
                      CLDEFI,LOWLYR,                          &
                      MASS_FLUX,                              &
                      RTHFTEN, RQVFTEN,                       &
                      CLDFRA,GLW,GSW,EMISS,LU_INDEX,          &
                      XLAT,XLONG,ALBEDO,ALBBCK,GMT,JULYR,JULDAY,     &
                      TMN,XLAND,ZNT,Z0,UST,MOL,PBLH,TKE_MYJ,     &
                      EXCH_H,THC,SNOWC,MAVAIL,HFX,QFX,RAINBL, &
                      TSLB,ZS,DZS,num_soil_layers,warm_rain,  &
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,      &
                      APR_CAPMA,APR_CAPME,APR_CAPMI,          &
                      XICE,VEGFRA,SNOW,CANWAT,SMSTAV,         &
                      SMSTOT, SFCRUNOFF,UDRUNOFF,GRDFLX,ACSNOW,      &
                      ACSNOM,IVGTYP,ISLTYP, SFCEVP,SMOIS,     &
                      SH2O, SNOWH, SMFR3D,                    &
                      DX,DY,F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY, &
                      mp_restart_state,tbpvs_state,tbpvs0_state,&
                      allowed_to_read, moved ,                &
                      ids, ide, jds, jde, kds, kde,           &
                      ims, ime, jms, jme, kms, kme,           &
                      grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij), kts, kte           )

   ENDDO

!                     SH2O, SNOWH, FNDSOILW, FNDSNOWH,        &  ! correct one

   CALL wrf_debug ( 100 , 'module_start: start_domain_rk: After call to phy_init' )

#ifdef MCELIO
   LU_MASK = 0.
   WHERE ( LU_INDEX .EQ. 16 ) LU_MASK = 1.
#endif

   END IF

!
!

 !  set physical boundary conditions for all initialized variables

!-----------------------------------------------------------------------
!  Stencils for patch communications  (WCS, 29 June 2001)
!  Note:  the size of this halo exchange reflects the 
!         fact that we are carrying the uncoupled variables 
!         as state variables in the mass coordinate model, as
!         opposed to the coupled variables as in the height
!         coordinate model.
!
!                           * * * * *
!         *        * * *    * * * * *
!       * + *      * + *    * * + * * 
!         *        * * *    * * * * *
!                           * * * * *
!
!j  u_1                          x
!j  u_2                          x
!j  v_1                          x
!j  v_2                          x
!j  w_1                          x
!j  w_2                          x
!j  t_1                          x
!j  t_2                          x
!j ph_1                          x
!j ph_2                          x
!
!j  t_init                       x
!
!j  phb   x
!j  ph0   x
!j  php   x
!j   pb   x
!j   al   x
!j  alt   x
!j  alb   x
!
!  the following are 2D (xy) variables
!
!j mu_1                          x
!j mu_2                          x
!j mub   x
!j mu0   x
!j ht    x
!j msft  x
!j msfu  x
!j msfv  x
!j sina  x
!j cosa  x
!j e     x
!j f     x
!
!  4D variables
!
! moist                        x
!  chem                        x
!scalar                        x

!--------------------------------------------------------------


#ifdef DM_PARALLEL
#  include "PERIOD_BDY_EM_INIT.inc"
#  include "PERIOD_BDY_EM_MOIST.inc"
#  include "PERIOD_BDY_EM_CHEM.inc"
#  include "HALO_EM_INIT_1.inc"
#  include "HALO_EM_INIT_2.inc"
#  include "HALO_EM_INIT_3.inc"
#  include "HALO_EM_INIT_4.inc"
#  include "HALO_EM_INIT_5.inc"
#endif

   CALL set_physical_bc3d( u_1 , 'U' , config_flags ,                  &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( u_2 , 'U' , config_flags ,                  &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc3d( v_1 , 'V' , config_flags ,                  &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( v_2 , 'V' , config_flags ,                  &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

! set kinematic condition for w 

   CALL set_physical_bc2d( ht , 'r' , config_flags ,                &
                           ids , ide , jds , jde , &
                           ims , ime , jms , jme , &
                           its , ite , jts , jte , &
                           its , ite , jts , jte   )

   IF ( .not. restart ) THEN 
   CALL set_w_surface(  config_flags,                                      &
                        w_1, ht, u_1, v_1, cf1, cf2, cf3, rdx, rdy, msft,  &
                        ids, ide, jds, jde, kds, kde,                      &
                        ips, ipe, jps, jpe, kps, kpe,                      &
                        its, ite, jts, jte, kts, kte,                      &
                        ims, ime, jms, jme, kms, kme                      )
   CALL set_w_surface(  config_flags,                                      &
                        w_2, ht, u_2, v_2, cf1, cf2, cf3, rdx, rdy, msft,  &
                        ids, ide, jds, jde, kds, kde,                      &
                        ips, ipe, jps, jpe, kps, kpe,                      &
                        its, ite, jts, jte, kts, kte,                      &
                        ims, ime, jms, jme, kms, kme                      )
   END IF

! finished setting kinematic condition for w at the surface

   CALL set_physical_bc3d( w_1 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( w_2 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc3d( ph_1 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc3d( ph_2 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc3d( t_1 , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc3d( t_2 , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc2d( mu_1, 't' , config_flags ,   &
                           ids , ide , jds , jde ,  &
                           ims , ime , jms , jme ,  &
                           its , ite , jts , jte ,  &
                           its , ite , jts , jte   )
   CALL set_physical_bc2d( mu_2, 't' , config_flags ,   &
                           ids , ide , jds , jde ,  &
                           ims , ime , jms , jme ,  &
                           its , ite , jts , jte ,  &
                           its , ite , jts , jte   )
   CALL set_physical_bc2d( mub , 't' , config_flags ,   &
                           ids , ide , jds , jde ,  &
                           ims , ime , jms , jme ,  &
                           its , ite , jts , jte ,  &
                           its , ite , jts , jte   )
   CALL set_physical_bc2d( mu0 , 't' , config_flags ,   &
                           ids , ide , jds , jde ,  &
                           ims , ime , jms , jme ,  &
                           its , ite , jts , jte ,  &
                           its , ite , jts , jte   )


   CALL set_physical_bc3d( phb , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( ph0 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( php , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc3d( pb , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( al , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( alt , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( alb , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d(t_init, 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   IF (num_moist > 0) THEN

! use of (:,:,:,loop) not efficient on DEC, but (ims,kms,jms,loop) not portable to SGI/Cray

      loop_3d_m   : DO loop = 1 , num_moist
         CALL set_physical_bc3d( moist(:,:,:,loop) , 'r' , config_flags ,                 &
                                 ids , ide , jds , jde , kds , kde ,        &
                                 ims , ime , jms , jme , kms , kme ,        &
                                 its , ite , jts , jte , kts , kte ,        &
                                 its , ite , jts , jte , kts , kte )
      END DO loop_3d_m

   ENDIF

   IF (num_chem >= PARAM_FIRST_SCALAR ) THEN
#ifdef WRF_CHEM
         CALL chem_init (grid%id,chem,dt,bioemdt,photdt,chemdt,           &
                         stepbioe,stepphot,stepchem,                        &
                         ph_2,phb,g,aerwrf,config_flags,                    &
                         alt,t_1,pb,p,t0,p1000mb,rcp,                       &
                         aer_dry,aer_water,chem_in_opt,                     &
                         imicrogram,                                        &
                                 ids , ide , jds , jde , kds , kde ,        &
                                 ims , ime , jms , jme , kms , kme ,        &
                                 its , ite , jts , jte , kts , kte          )
!
! calculate initial pm
!
        print *,'calculating initial pm'
        do j=jts,jte
        jj=min(jde-1,j)
        do k=kts,kte
        kk=min(kde-1,k)
        do i=its,ite
        ii=min(ide-1,i)

          pm2_5(i,k,j)=0.
          do l=p_so4aj,p_p25i
             pm2_5(i,k,j)=pm2_5(i,k,j)+chem(ii,kk,jj,l)
          enddo
          pm10(i,k,j)= pm2_5(i,k,j)                      &
                      +chem(ii,kk,jj,p_antha)             &
                      +chem(ii,kk,jj,p_soila)             &
                      +chem(ii,kk,jj,p_seas)
        enddo
        enddo
        enddo
#endif

! use of (:,:,:,loop) not efficient on DEC, but (ims,kms,jms,loop) not portable to SGI/Cray

      loop_3d_c   : DO loop = PARAM_FIRST_SCALAR , num_chem
         CALL set_physical_bc3d( chem(:,:,:,loop) , 'r' , config_flags ,                 &
                                 ids , ide , jds , jde , kds , kde ,        &
                                 ims , ime , jms , jme , kms , kme ,        &
                                 its , ite , jts , jte , kts , kte ,        &
                                 its , ite , jts , jte , kts , kte )
      END DO loop_3d_c

   ENDIF

   CALL set_physical_bc2d( msft , 'r' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( msfu , 'x' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   ) 
   CALL set_physical_bc2d( msfv , 'y' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   ) 
   CALL set_physical_bc2d( sina , 'r' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   ) 
   CALL set_physical_bc2d( cosa , 'r' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   ) 
   CALL set_physical_bc2d( e , 'r' , config_flags ,                 &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   ) 
   CALL set_physical_bc2d( f , 'r' , config_flags ,                 &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   ) 

#ifdef DM_PARALLEL
#   include "HALO_EM_INIT_1.inc"
#   include "HALO_EM_INIT_2.inc"
#   include "HALO_EM_INIT_3.inc"
#   include "HALO_EM_INIT_4.inc"
#   include "HALO_EM_INIT_5.inc"
#endif

     CALL wrf_debug ( 100 , 'module_start: start_domain_rk: Returning' )

#define COPY_OUT
#include <em_scalar_derefs.inc>

     RETURN

   END SUBROUTINE start_domain_em

