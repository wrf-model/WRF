!-------------------------------------------------------------------

   SUBROUTINE start_domain_em ( grid, &
!
# include <em_dummy_args.inc>
!
)

   USE module_domain
!  USE module_io_domain
   USE module_state_description
   USE module_model_constants
   USE module_bc
   USE module_bc_em
!  USE module_timing
   USE module_configure
   USE module_date_time

   USE module_physics_init

#ifdef DM_PARALLEL
   USE module_dm
#endif

   USE module_model_constants
   IMPLICIT NONE

   !  Input data.
   TYPE (domain)          :: grid

# include <em_dummy_decl.inc>

   TYPE (grid_config_rec_type)              :: config_flags

   !  Local data
   INTEGER                             ::                       &
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  ips, ipe, jps, jpe, kps, kpe, &
                                  its, ite, jts, jte, kts, kte, &
                                  i, j, k, loop, error

   REAL :: qvf1, qvf2, qvf
   REAL :: MPDT

#define COPY_IN
#include <em_scalar_derefs.inc>
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

   IF ( .not. restart ) THEN 
       itimestep=0
   ENDIF


   
   IF (config_flags%specified) THEN
!
! Arrays for specified boundary conditions
!

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

   IF(.not.config_flags%restart)THEN

! data that is expected to be zero must be explicitly initialized as such
     h_diabatic = 0.

!  if this is for a nested domain, the defined/interpolated fields are the _2

     IF ( grid%id .NE. 1 ) THEN
       DO j = jts,min(jte,jde-1)
       DO k = kts,kte-1
       DO i = its, min(ite,ide-1)
           moist_1(i,k,j,P_QV)=moist_2(i,k,j,P_QV)
           t_1(i,k,j)=t_2(i,k,j)
       ENDDO
       ENDDO
       ENDDO
  
       DO j = jts,min(jte,jde-1)
       DO i = its, min(ite,ide-1)
           mu_1(i,j)=mu_2(i,j)
       ENDDO
       ENDDO
     END IF

!  reconstitute base-state fields

     DO j = jts,min(jte,jde-1)
     DO k = kts,kte-1
     DO i = its, min(ite,ide-1)
       pb(i,k,j) = znu(k)*mub(i,j)+p_top
       alb(i,k,j) = -rdnw(k)*(phb(i,k+1,j)-phb(i,k,j))/mub(i,j)
       t_init(i,k,j) = alb(i,k,j)*(p1000mb/r_d)/((pb(i,k,j)/p1000mb)**cvpm) - t0
     ENDDO
     ENDDO
     ENDDO
      
     DO j = jts,min(jte,jde-1)

       k = kte-1
       DO i = its, min(ite,ide-1)
         qvf1 = 0.5*(moist_1(i,k,j,P_QV)+moist_1(i,k,j,P_QV))
         qvf2 = 1./(1.+qvf1)
         qvf1 = qvf1*qvf2
         p(i,k,j) = - 0.5*(mu_1(i,j)+qvf1*mub(i,j))/rdnw(k)/qvf2
         qvf = 1. + rvovrd*moist_1(i,k,j,P_QV)
         alt(i,k,j) = (r_d/p1000mb)*(t_1(i,k,j)+t0)*qvf*(((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm)
         al(i,k,j) = alt(i,k,j) - alb(i,k,j)
       ENDDO

       DO k = kte-2, 1, -1
       DO i = its, min(ite,ide-1)
         qvf1 = 0.5*(moist_1(i,k,j,P_QV)+moist_1(i,k+1,j,P_QV))
         qvf2 = 1./(1.+qvf1)
         qvf1 = qvf1*qvf2
         p(i,k,j) = p(i,k+1,j) - (mu_1(i,j) + qvf1*mub(i,j))/qvf2/rdn(k+1)
         qvf = 1. + rvovrd*moist_1(i,k,j,P_QV)
         alt(i,k,j) = (r_d/p1000mb)*(t_1(i,k,j)+t0)*qvf* &
                      (((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm)
         al(i,k,j) = alt(i,k,j) - alb(i,k,j)
       ENDDO
       ENDDO

     ENDDO

   ENDIF


   CALL wrf_debug ( 100 , 'module_start: start_domain_rk: Before call to phy_init' )

! namelist MPDT does not exist yet, so set it here
! MPDT is the call frequency for microphysics in minutes (0 means every step)
   MPDT = 0.

   CALL phy_init   (  grid,                                   &  
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
                      XLAT,XLONG,ALBEDO,ALBBCK,GMT,JULYR,JULDAY,&
                      TMN,XLAND,ZNT,Z0, UST,MOL,PBLH,TKE_MYJ, &
                      THC,SNOWC,MAVAIL,HFX,QFX,RAINBL,        &
                      TSLB,ZS,DZS,num_soil_layers,warm_rain,  &
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,      &
                      APR_CAPMA,APR_CAPME,APR_CAPMI,          &
                      XICE,VEGFRA,SNOW,CANWAT,SMSTAV,         &
                      SMSTOT, SFCRUNOFF,UDRUNOFF,GRDFLX,ACSNOW,      &
                      ACSNOM,IVGTYP,ISLTYP, SFCEVP,SMOIS,     &
                      SH2O, SNOWH, SMFR3D,                    &
                      DX,DY,F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY, &
                      ids, ide, jds, jde, kds, kde,           &
                      ims, ime, jms, jme, kms, kme,           &
                      its, ite, jts, jte, kts, kte           )

!                     SH2O, SNOWH, FNDSOILW, FNDSNOWH,        &  ! correct one

   CALL wrf_debug ( 100 , 'module_start: start_domain_rk: After call to phy_init' )

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
! moist_1                        x
! moist_2                        x
!  chem_1                        x
!  chem_2                        x

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

   IF(.not.config_flags%restart)THEN
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
   ENDIF

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
         CALL set_physical_bc3d( moist_1(:,:,:,loop) , 'r' , config_flags ,                 &
                                 ids , ide , jds , jde , kds , kde ,        &
                                 ims , ime , jms , jme , kms , kme ,        &
                                 its , ite , jts , jte , kts , kte ,        &
                                 its , ite , jts , jte , kts , kte )
         CALL set_physical_bc3d( moist_2(:,:,:,loop) , 'r' , config_flags ,                 &
                                 ids , ide , jds , jde , kds , kde ,        &
                                 ims , ime , jms , jme , kms , kme ,        &
                                 its , ite , jts , jte , kts , kte ,        &
                                 its , ite , jts , jte , kts , kte )
      END DO loop_3d_m

   ENDIF

   IF (num_chem >= PARAM_FIRST_SCALAR ) THEN

! use of (:,:,:,loop) not efficient on DEC, but (ims,kms,jms,loop) not portable to SGI/Cray

      loop_3d_c   : DO loop = PARAM_FIRST_SCALAR , num_chem
         CALL set_physical_bc3d( chem_1(:,:,:,loop) , 'r' , config_flags ,                 &
                                 ids , ide , jds , jde , kds , kde ,        &
                                 ims , ime , jms , jme , kms , kme ,        &
                                 its , ite , jts , jte , kts , kte ,        &
                                 its , ite , jts , jte , kts , kte )
         CALL set_physical_bc3d( chem_2(:,:,:,loop) , 'r' , config_flags ,                 &
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

