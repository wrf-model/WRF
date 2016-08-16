!IDEAL:MODEL_LAYER:INITIALIZATION

!  This MODULE holds the routines which are used to perform various initializations
!  for the individual domains.  

!-----------------------------------------------------------------------

MODULE module_initialize_ideal

   USE module_domain             ! frame/module_domain.F
   USE module_io_domain          ! share
   USE module_state_description  ! frame
   USE module_model_constants    ! share
   USE module_bc                 ! share
   USE module_timing             ! frame
   USE module_configure          ! frame
   USE module_init_utilities     ! dyn_em
#ifdef DM_PARALLEL
   USE module_dm
#endif


CONTAINS


!-------------------------------------------------------------------
! this is a wrapper for the solver-specific init_domain routines.
! Also dereferences the grid variables and passes them down as arguments.
! This is crucial, since the lower level routines may do message passing
! and this will get fouled up on machines that insist on passing down
! copies of assumed-shape arrays (by passing down as arguments, the 
! data are treated as assumed-size -- ie. f77 -- arrays and the copying
! business is avoided).  Fie on the F90 designers.  Fie and a pox.

   SUBROUTINE init_domain ( grid )

   IMPLICIT NONE

   !  Input data.
   TYPE (domain), POINTER :: grid 
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
# include "dummy_new_args.inc"
!
)
   IMPLICIT NONE

   !  Input data.
   TYPE (domain), POINTER :: grid

# include "dummy_decl.inc"

   TYPE (grid_config_rec_type)              :: config_flags

   !  Local data
   INTEGER                             ::                       &
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  its, ite, jts, jte, kts, kte, &
                                  i, j, k

   INTEGER :: nxx, nyy, ig, jg, im, error

   REAL :: dlam, dphi, vlat, tperturb
   REAL :: p_surf, p_level, pd_surf, qvf1, qvf2, qvf
   REAL :: thtmp, ptmp, temp(3), cof1, cof2

   INTEGER :: icm,jcm

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

! here we check to see if the boundary conditions are set properly

   CALL boundary_condition_check( config_flags, bdyzone, error, grid%id )

    grid%itimestep=0
   grid%step_number = 0


#ifdef DM_PARALLEL
   CALL wrf_dm_bcast_bytes( icm , IWORDSIZE )
   CALL wrf_dm_bcast_bytes( jcm , IWORDSIZE )
#endif

   ! Initialize 2D surface arrays

   nxx = ide-ids ! Don't include u-stagger
   nyy = jde-jds ! Don't include v-stagger
   dphi = 180./REAL(nyy)
   dlam = 360./REAL(nxx)

   DO j = jts, jte
   DO i = its, ite
      ! ig is the I index in the global (domain) span of the array.
      ! jg is the J index in the global (domain) span of the array.
      ig = i - ids + 1  ! ids is not necessarily 1
      jg = j - jds + 1  ! jds is not necessarily 1

      grid%xlat(i,j)  = (REAL(jg)-0.5)*dphi-90.
      grid%xlong(i,j) = (REAL(ig)-0.5)*dlam-180.
      vlat       = grid%xlat(i,j) - 0.5*dphi

      grid%clat(i,j) = grid%xlat(i,j)

      grid%msftx(i,j) = 1./COS(grid%xlat(i,j)*degrad)
      grid%msfty(i,j) = 1.
      grid%msfux(i,j) = 1./COS(grid%xlat(i,j)*degrad)
      grid%msfuy(i,j) = 1.
      grid%e(i,j)     = 2*EOMEG*COS(grid%xlat(i,j)*degrad)
      grid%f(i,j)     = 2*EOMEG*SIN(grid%xlat(i,j)*degrad)

      ! The following two are the cosine and sine of the rotation
      ! of projection.  Simple cylindrical is *simple* ... no rotation!
      grid%sina(i,j) = 0.
      grid%cosa(i,j) = 1.

   END DO
   END DO

!   DO j = max(jds+1,jts), min(jde-1,jte)
   DO j = jts, jte
   DO i = its, ite
      vlat       = grid%xlat(i,j) - 0.5*dphi
      grid%msfvx(i,j) = 1./COS(vlat*degrad)
      grid%msfvy(i,j) = 1.
      grid%msfvx_inv(i,j) = 1./grid%msfvx(i,j)
   END DO
   END DO

   IF(jts == jds) THEN
   DO i = its, ite
      grid%msfvx(i,jts) = 00.
      grid%msfvx_inv(i,jts) = 0.
   END DO
   END IF

   IF(jte == jde) THEN
   DO i = its, ite
      grid%msfvx(i,jte) = 00.
      grid%msfvx_inv(i,jte) = 0.
   END DO
   END IF

   DO j=jts,jte
     vlat       = grid%xlat(its,j) - 0.5*dphi
     write(6,*) j,vlat,grid%msfvx(its,j),grid%msfvx_inv(its,j)
   ENDDO

   DO j=jts,jte
   DO i=its,ite
      grid%ht(i,j)       = 0.

      grid%albedo(i,j)   = 0.
      grid%thc(i,j)      = 1000.
      grid%znt(i,j)      = 0.01
      grid%emiss(i,j)    = 1.
      grid%ivgtyp(i,j)   = 1
      grid%lu_index(i,j) = REAL(ivgtyp(i,j))
      grid%xland(i,j)    = 1.
      grid%mavail(i,j)   = 0.
   END DO
   END DO

   grid%dx = dlam*degrad/reradius
   grid%dy = dphi*degrad/reradius
   grid%rdx = 1./grid%dx
   grid%rdy = 1./grid%dy

   !WRITE(*,*) ''
   !WRITE(*,'(A,1PG14.6,A,1PG14.6)') ' For the namelist: dx =',grid%dx,', dy =',grid%dy

   CALL nl_set_mminlu(1,'    ')
   grid%iswater = 0
   grid%cen_lat = 0.
   grid%cen_lon = 0.
   grid%truelat1 = 0.
   grid%truelat2 = 0.
   grid%moad_cen_lat = 0.
   grid%stand_lon    = 0.
   grid%pole_lon = 0.
   grid%pole_lat = 90.
   ! Apparently, map projection 0 is "none" which actually turns out to be
   ! a regular grid of latitudes and longitudes, the simple cylindrical projection
   grid%map_proj = 0

   DO k = kds, kde
      grid%znw(k) = 1. - REAL(k-kds)/REAL(kde-kds)
   END DO

   DO k=1, kde-1
    grid%dnw(k) = grid%znw(k+1) - grid%znw(k)
    grid%rdnw(k) = 1./grid%dnw(k)
    grid%znu(k) = 0.5*(grid%znw(k+1)+grid%znw(k))
   ENDDO
   DO k=2, kde-1
    grid%dn(k) = 0.5*(grid%dnw(k)+grid%dnw(k-1))
    grid%rdn(k) = 1./grid%dn(k)
    grid%fnp(k) = .5* grid%dnw(k  )/grid%dn(k)
    grid%fnm(k) = .5* grid%dnw(k-1)/grid%dn(k)
   ENDDO

   cof1 = (2.*grid%dn(2)+grid%dn(3))/(grid%dn(2)+grid%dn(3))*grid%dnw(1)/grid%dn(2) 
   cof2 =     grid%dn(2)        /(grid%dn(2)+grid%dn(3))*grid%dnw(1)/grid%dn(3) 
   grid%cf1  = grid%fnp(2) + cof1
   grid%cf2  = grid%fnm(2) - cof1 - cof2
   grid%cf3  = cof2       

   grid%cfn  = (.5*grid%dnw(kde-1)+grid%dn(kde-1))/grid%dn(kde-1)
   grid%cfn1 = -.5*grid%dnw(kde-1)/grid%dn(kde-1)


   ! Need to add perturbations to initial profile.  Set up random number
   ! seed here.
   CALL random_seed

   ! General assumption from here after is that the initial temperature
   ! profile is isothermal at a value of T0, and the initial winds are
   ! all 0.

   ! find ptop for the desired ztop (ztop is input from the namelist)
   grid%p_top =  p0 * EXP(-(g*config_flags%ztop)/(r_d*T0))


   ! Values of geopotential (base, perturbation, and at p0) at the surface
   DO j = jts, jte
   DO i = its, ite
      grid%phb(i,1,j) = grid%ht(i,j)*g
      grid%php(i,1,j) = 0.         ! This is perturbation geopotential
                              ! Since this is an initial condition, there
                              ! should be no perturbation!
      grid%ph0(i,1,j) = grid%ht(i,j)*g
   ENDDO
   ENDDO


   DO J = jts, jte
   DO I = its, ite

      p_surf = p0 * EXP(-(g*grid%phb(i,1,j)/g)/(r_d*T0))
      grid%mub(i,j) = p_surf-grid%p_top

      ! given p (coordinate), calculate theta and compute 1/rho from equation
      ! of state

      DO K = kts, kte-1
         p_level = grid%znu(k)*(p_surf - grid%p_top) + grid%p_top
         grid%pb(i,k,j) = p_level

         grid%t_init(i,k,j) = T0*(p0/p_level)**rcp
         grid%t_init(i,k,j) = grid%t_init(i,k,j) - t0

         grid%alb(i,k,j)=(r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
      END DO

      ! calculate hydrostatic balance (alternatively we could interpolate
      ! the geopotential from the sounding, but this assures that the base
      ! state is in exact hydrostatic balance with respect to the model eqns.

      DO k = kts+1, kte
         grid%phb(i,k,j) = grid%phb(i,k-1,j) - grid%dnw(k-1)*grid%mub(i,j)*grid%alb(i,k-1,j)
      ENDDO

   ENDDO
   ENDDO

   DO im = PARAM_FIRST_SCALAR, num_moist
   DO J = jts, jte
   DO K = kts, kte-1
   DO I = its, ite
      grid%moist(i,k,j,im) = 0.
   END DO
   END DO
   END DO
   END DO

   ! Now calculate the full (hydrostatically-balanced) state for each column
   ! We will include moisture
   DO J = jts, jte
   DO I = its, ite

      ! At this point p_top is already set. find the DRY mass in the column
      pd_surf = p0 * EXP(-(g*grid%phb(i,1,j)/g)/(r_d*T0))

      ! compute the perturbation mass (mu/mu_1/mu_2) and the full mass
      grid%mu_1(i,j) = pd_surf-grid%p_top - grid%mub(i,j)
      grid%mu_2(i,j) = grid%mu_1(i,j)
      grid%mu0(i,j)  = grid%mu_1(i,j) + grid%mub(i,j)

      ! given the dry pressure and coordinate system, calculate the
      ! perturbation potential temperature (t/t_1/t_2)

      DO k = kds, kde-1
         p_level = grid%znu(k)*(pd_surf - grid%p_top) + grid%p_top
         grid%t_1(i,k,j) = T0*(p0/p_level)**rcp
         ! Add a small perturbation to initial isothermal profile
         CALL random_number(tperturb)
         grid%t_1(i,k,j)=grid%t_1(i,k,j)*(1.0+0.004*(tperturb-0.5))
         grid%t_1(i,k,j) = grid%t_1(i,k,j)-t0
         grid%t_2(i,k,j) = grid%t_1(i,k,j)
      END DO


      ! integrate the hydrostatic equation (from the RHS of the bigstep
      ! vertical momentum equation) down from the top to get p.
      ! first from the top of the model to the top pressure

      k = kte-1  ! top level

      qvf1 = 0.5*(grid%moist(i,k,j,P_QV)+grid%moist(i,k,j,P_QV))
      qvf2 = 1./(1.+qvf1)
      qvf1 = qvf1*qvf2

      ! grid%p(i,k,j) = - 0.5*grid%mu_1(i,j)/grid%rdnw(k)
      grid%p(i,k,j) = - 0.5*(grid%mu_1(i,j)+qvf1*grid%mub(i,j))/grid%rdnw(k)/qvf2
      qvf = 1. + rvovrd*grid%moist(i,k,j,P_QV)
      grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_1(i,k,j)+t0)*qvf* &
                  (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
      grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)

      !  down the column

      do k=kte-2,kts,-1
         qvf1 = 0.5*(grid%moist(i,k,j,P_QV)+grid%moist(i,k+1,j,P_QV))
         qvf2 = 1./(1.+qvf1)
         qvf1 = qvf1*qvf2
         grid%p(i,k,j) = grid%p(i,k+1,j) - (grid%mu_1(i,j) + qvf1*grid%mub(i,j))/qvf2/grid%rdn(k+1)
         qvf = 1. + rvovrd*grid%moist(i,k,j,P_QV)
         grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_1(i,k,j)+t0)*qvf* &
                     (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
         grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
      enddo

      ! this is the hydrostatic equation used in the model after the
      ! small timesteps.  In the model, al (inverse density)
      ! is computed from the geopotential.

      grid%ph_1(i,1,j) = 0.
      DO k  = kts+1,kte
         grid%ph_1(i,k,j) = grid%ph_1(i,k-1,j) - (grid%dnw(k-1))*(  &
                      (grid%mub(i,j)+grid%mu_1(i,j))*grid%al(i,k-1,j)+  &
                       grid%mu_1(i,j)*grid%alb(i,k-1,j)  )

         grid%ph_2(i,k,j) = grid%ph_1(i,k,j)
         grid%ph0(i,k,j) = grid%ph_1(i,k,j) + grid%phb(i,k,j)
      ENDDO

   END DO
   END DO



   ! Now set U & V
   DO J = jts, jte
   DO K = kts, kte-1
   DO I = its, ite
      grid%u_1(i,k,j) = 0.
      grid%u_2(i,k,j) = 0.
      grid%v_1(i,k,j) = 0.
      grid%v_2(i,k,j) = 0.
   END DO
   END DO
   END DO


   DO j=jts, jte
   DO k=kds, kde
   DO i=its, ite
      grid%ww(i,k,j)  = 0.
      grid%w_1(i,k,j) = 0.
      grid%w_2(i,k,j) = 0.
      grid%h_diabatic(i,k,j) = 0.
   END DO
   END DO
   END DO

  
   DO k=kts,kte
      grid%t_base(k)  = grid%t_init(its,k,jts)
      grid%qv_base(k) = 0.
      grid%u_base(k)  = 0.
      grid%v_base(k)  = 0.
   END DO

   ! One subsurface layer: infinite slab at constant temperature below
   ! the surface.  Surface temperature is an infinitely thin "skin" on
   ! top of a half-infinite slab.  The temperature of both the skin and
   ! the slab are determined from the initial nearest-surface-air-layer
   ! temperature.
   DO J = jts, MIN(jte, jde-1)
   DO I = its, MIN(ite, ide-1)
      thtmp   = grid%t_2(i,1,j)+t0
      ptmp    = grid%p(i,1,j)+grid%pb(i,1,j)
      temp(1) = thtmp * (ptmp/p1000mb)**rcp
      thtmp   = grid%t_2(i,2,j)+t0
      ptmp    = grid%p(i,2,j)+grid%pb(i,2,j)
      temp(2) = thtmp * (ptmp/p1000mb)**rcp
      thtmp   = grid%t_2(i,3,j)+t0
      ptmp    = grid%p(i,3,j)+grid%pb(i,3,j)
      temp(3) = thtmp * (ptmp/p1000mb)**rcp
      grid%tsk(I,J)=cf1*temp(1)+cf2*temp(2)+cf3*temp(3)
      grid%tmn(I,J)=grid%tsk(I,J)-0.5
   END DO
   END DO

   RETURN

 END SUBROUTINE init_domain_rk

!---------------------------------------------------------------------

 SUBROUTINE init_module_initialize
 END SUBROUTINE init_module_initialize

!---------------------------------------------------------------------

END MODULE module_initialize_ideal
