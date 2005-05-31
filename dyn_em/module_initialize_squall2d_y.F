!IDEAL:MODEL_LAYER:INITIALIZATION
!

!  This MODULE holds the routines which are used to perform various initializations
!  for the individual domains.  

!  This MODULE CONTAINS the following routines:

!  initialize_field_test - 1. Set different fields to different constant
!                             values.  This is only a test.  If the correct
!                             domain is not found (based upon the "id")
!                             then a fatal error is issued.               

!-----------------------------------------------------------------------

MODULE module_initialize

   USE module_domain
   USE module_io_domain
   USE module_state_description
   USE module_model_constants
   USE module_bc
   USE module_timing
   USE module_configure
   USE module_init_utilities
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
   INTEGER                :: dyn_opt 
   INTEGER :: idum1, idum2

#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

#include "deref_kludge.h"

   CALL nl_get_dyn_opt( 1, dyn_opt )
   
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
     CALL wrf_error_fatal ( ' init_domain: unknown or unimplemented dyn_opt ' )
   ENDIF

   END SUBROUTINE init_domain

!-------------------------------------------------------------------

   SUBROUTINE init_domain_rk ( grid, &
!
# include <em_dummy_args.inc>
!
)
   IMPLICIT NONE

   !  Input data.
   TYPE (domain), POINTER :: grid

# include <em_dummy_decl.inc>

   TYPE (grid_config_rec_type)              :: config_flags

   !  Local data
   INTEGER                             ::                       &
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  its, ite, jts, jte, kts, kte, &
                                  i, j, k

   ! Local data

   INTEGER, PARAMETER :: nl_max = 1000
   REAL, DIMENSION(nl_max) :: zk, p_in, theta, rho, u, v, qv, pd_in
   INTEGER :: nl_in


   INTEGER :: icm,jcm, ii, im1, jj, jm1, loop, error, fid, nxc, nyc
   REAL    :: u_mean,v_mean, f0, p_surf, p_level, qvf, z_at_v, z_at_u
   REAL    :: z_scale, xrad, yrad, zrad, rad, delt, cof1, cof2
!   REAL, EXTERNAL :: interp_0
   REAL    :: hm
   REAL    :: pi

!  stuff from original initialization that has been dropped from the Registry 
   REAL    :: vnu, xnu, xnus, dinit0, cbh, p0_temp, t0_temp, zd, zt
   REAL    :: qvf1, qvf2, pd_surf
   INTEGER :: it
   real :: thtmp, ptmp, temp(3)

   LOGICAL :: moisture_init
   LOGICAL :: stretch_grid, dry_sounding

   
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


   stretch_grid = .true.
   delt = 3.
!   z_scale = .50
   z_scale = .40
   pi = 2.*asin(1.0)
   write(6,*) ' pi is ',pi
   nxc = (ide-ids)/2
   nyc = (jde-jds)/2

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

! here we check to see if the boundary conditions are set properly

   CALL boundary_condition_check( config_flags, bdyzone, error, grid%id )

   moisture_init = .true.

    grid%itimestep=0

#ifdef DM_PARALLEL
   CALL wrf_dm_bcast_bytes( icm , IWORDSIZE )
   CALL wrf_dm_bcast_bytes( jcm , IWORDSIZE )
#endif

    CALL nl_set_mminlu(1, '    ')
    CALL nl_set_iswater(1,0)
    CALL nl_set_cen_lat(1,40.)
    CALL nl_set_cen_lon(1,-105.)
    CALL nl_set_truelat1(1,0.)
    CALL nl_set_truelat2(1,0.)
    CALL nl_set_moad_cen_lat (1,0.)
    CALL nl_set_stand_lon (1,0.)
    CALL nl_set_map_proj(1,0)


!  here we initialize data we currently is not initialized 
!  in the input data

    DO j = jts, jte
      DO i = its, ite
         msft(i,j)     = 1.
         msfu(i,j)     = 1.
         msfv(i,j)     = 1.
         sina(i,j)     = 0.
         cosa(i,j)     = 1.
         e(i,j)        = 0.
         f(i,j)        = 0.

      END DO
   END DO

    DO j = jts, jte
    DO k = kts, kte
      DO i = its, ite
         ww(i,k,j)     = 0.
      END DO
   END DO
   END DO

   step_number = 0

! set up the grid

   IF (stretch_grid) THEN ! exponential stretch for eta (nearly constant dz)
     DO k=1, kde
      znw(k) = (exp(-(k-1)/float(kde-1)/z_scale) - exp(-1./z_scale))/ &
                                (1.-exp(-1./z_scale))
     ENDDO
   ELSE
     DO k=1, kde
      znw(k) = 1. - float(k-1)/float(kde-1)
     ENDDO
   ENDIF

   DO k=1, kde-1
    dnw(k) = znw(k+1) - znw(k)
    rdnw(k) = 1./dnw(k)
    znu(k) = 0.5*(znw(k+1)+znw(k))
   ENDDO
   DO k=2, kde-1
    dn(k) = 0.5*(dnw(k)+dnw(k-1))
    rdn(k) = 1./dn(k)
    fnp(k) = .5* dnw(k  )/dn(k)
    fnm(k) = .5* dnw(k-1)/dn(k)
   ENDDO

   cof1 = (2.*dn(2)+dn(3))/(dn(2)+dn(3))*dnw(1)/dn(2) 
   cof2 =     dn(2)        /(dn(2)+dn(3))*dnw(1)/dn(3) 
   cf1  = fnp(2) + cof1
   cf2  = fnm(2) - cof1 - cof2
   cf3  = cof2       

   cfn  = (.5*dnw(kde-1)+dn(kde-1))/dn(kde-1)
   cfn1 = -.5*dnw(kde-1)/dn(kde-1)
   rdx = 1./dx
   rdy = 1./dy

!  get the sounding from the ascii sounding file, first get dry sounding and 
!  calculate base state

  write(6,*) ' getting dry sounding for base state '
  dry_sounding = .true.
  CALL get_sounding( zk, p_in, pd_in, theta, rho, u, v, qv, dry_sounding, nl_max, nl_in )

  write(6,*) ' returned from reading sounding, nl_in is ',nl_in

!  find ptop for the desired ztop (ztop is input from the namelist),
!  and find surface pressure

  p_top = interp_0( p_in, zk, ztop, nl_in )

  DO j=jts,jte
  DO i=its,ite  ! flat surface
    phb(i,1,j) = 0.
    php(i,1,j) = 0.
    ph0(i,1,j) = 0.
    ht(i,j) = 0.
  ENDDO
  ENDDO

  DO J = jts, jte
  DO I = its, ite

    p_surf = interp_0( p_in, zk, phb(i,1,j)/g, nl_in )
    mub(i,j) = p_surf-p_top

!  this is dry hydrostatic sounding (base state), so given p (coordinate),
!  interp theta (from interp) and compute 1/rho from eqn. of state

    DO K = 1, kte-1
      p_level = znu(k)*(p_surf - p_top) + p_top
      pb(i,k,j) = p_level
      t_init(i,k,j) = interp_0( theta, p_in, p_level, nl_in ) - t0
      alb(i,k,j) = (r_d/p1000mb)*(t_init(i,k,j)+t0)*(pb(i,k,j)/p1000mb)**cvpm
    ENDDO

!  calc hydrostatic balance (alternatively we could interp the geopotential from the
!  sounding, but this assures that the base state is in exact hydrostatic balance with
!  respect to the model eqns.

    DO k  = 2,kte
      phb(i,k,j) = phb(i,k-1,j) - dnw(k-1)*mub(i,j)*alb(i,k-1,j)
    ENDDO

  ENDDO
  ENDDO

  write(6,*) ' ptop is ',p_top
  write(6,*) ' base state mub(1,1), p_surf is ',mub(1,1),mub(1,1)+p_top

!  calculate full state for each column - this includes moisture.

  write(6,*) ' getting moist sounding for full state '
  dry_sounding = .false.
  CALL get_sounding( zk, p_in, pd_in, theta, rho, u, v, qv, dry_sounding, nl_max, nl_in )

  DO J = jts, min(jde-1,jte)
  DO I = its, min(ide-1,ite)

!  At this point p_top is already set. find the DRY mass in the column 
!  by interpolating the DRY pressure.  

   pd_surf = interp_0( pd_in, zk, phb(i,1,j)/g, nl_in )

!  compute the perturbation mass and the full mass

    mu_1(i,j) = pd_surf-p_top - mub(i,j)
    mu_2(i,j) = mu_1(i,j)
    mu0(i,j) = mu_1(i,j) + mub(i,j)

! given the dry pressure and coordinate system, interp the potential
! temperature and qv

    do k=1,kde-1

      p_level = znu(k)*(pd_surf - p_top) + p_top

      moist(i,k,j,P_QV) = interp_0( qv, pd_in, p_level, nl_in )
      t_1(i,k,j)          = interp_0( theta, pd_in, p_level, nl_in ) - t0
      t_2(i,k,j)          = t_1(i,k,j)
      

    enddo

!  integrate the hydrostatic equation (from the RHS of the bigstep
!  vertical momentum equation) down from the top to get p.
!  first from the top of the model to the top pressure

    k = kte-1  ! top level

    qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k,j,P_QV))
    qvf2 = 1./(1.+qvf1)
    qvf1 = qvf1*qvf2

!    p(i,k,j) = - 0.5*mu_1(i,j)/rdnw(k)
    p(i,k,j) = - 0.5*(mu_1(i,j)+qvf1*mub(i,j))/rdnw(k)/qvf2
    qvf = 1. + rvovrd*moist(i,k,j,P_QV)
    alt(i,k,j) = (r_d/p1000mb)*(t_1(i,k,j)+t0)*qvf* &
                (((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm)
    al(i,k,j) = alt(i,k,j) - alb(i,k,j)

!  down the column

    do k=kte-2,1,-1
      qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k+1,j,P_QV))
      qvf2 = 1./(1.+qvf1)
      qvf1 = qvf1*qvf2
      p(i,k,j) = p(i,k+1,j) - (mu_1(i,j) + qvf1*mub(i,j))/qvf2/rdn(k+1)
      qvf = 1. + rvovrd*moist(i,k,j,P_QV)
      alt(i,k,j) = (r_d/p1000mb)*(t_1(i,k,j)+t0)*qvf* &
                  (((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm)
      al(i,k,j) = alt(i,k,j) - alb(i,k,j)
    enddo

!  this is the hydrostatic equation used in the model after the
!  small timesteps.  In the model, al (inverse density)
!  is computed from the geopotential.


    ph_1(i,1,j) = 0.
    DO k  = 2,kte
      ph_1(i,k,j) = ph_1(i,k-1,j) - (1./rdnw(k-1))*(       &
                   (mub(i,j)+mu_1(i,j))*al(i,k-1,j)+ &
                    mu_1(i,j)*alb(i,k-1,j)  )
                                                   
      ph_2(i,k,j) = ph_1(i,k,j) 
      ph0(i,k,j) = ph_1(i,k,j) + phb(i,k,j)
    ENDDO

    if((i==2) .and. (j==2)) then
     write(6,*) ' ph_1 calc ',ph_1(2,1,2),ph_1(2,2,2),mu_1(2,2)+mub(2,2),mu_1(2,2), &
                              alb(2,1,2),al(1,2,1),rdnw(1)
    endif

  ENDDO
  ENDDO

!#if 0

!  thermal perturbation to kick off convection

  write(6,*) ' nxc, nyc for perturbation ',nxc,nyc
  write(6,*) ' delt for perturbation ',delt

  DO J = jts, min(jde-1,jte)
    yrad = dy*float(j-nyc)/4000.
!     yrad = 0.
    DO I = its, min(ide-1,ite)
!      xrad = dx*float(i-nxc)/4000.
     xrad = 0.
      DO K = 1, kte-1

!  put in preturbation theta (bubble) and recalc density.  note,
!  the mass in the column is not changing, so when theta changes,
!  we recompute density and geopotential

        zrad = 0.5*(ph_1(i,k,j)+ph_1(i,k+1,j)  &
                   +phb(i,k,j)+phb(i,k+1,j))/g
        zrad = (zrad-1500.)/1500.
        RAD=SQRT(xrad*xrad+yrad*yrad+zrad*zrad)
        IF(RAD <= 1.) THEN
           T_1(i,k,j)=T_1(i,k,j)+delt*COS(.5*PI*RAD)**2
           T_2(i,k,j)=T_1(i,k,j)
           qvf = 1. + rvovrd*moist(i,k,j,P_QV)
           alt(i,k,j) = (r_d/p1000mb)*(t_1(i,k,j)+t0)*qvf* &
                        (((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm)
           al(i,k,j) = alt(i,k,j) - alb(i,k,j)
        ENDIF
      ENDDO

!  rebalance hydrostatically

      DO k  = 2,kte
        ph_1(i,k,j) = ph_1(i,k-1,j) - (1./rdnw(k-1))*(       &
                     (mub(i,j)+mu_1(i,j))*al(i,k-1,j)+ &
                      mu_1(i,j)*alb(i,k-1,j)  )
                                                   
        ph_2(i,k,j) = ph_1(i,k,j) 
        ph0(i,k,j) = ph_1(i,k,j) + phb(i,k,j)
      ENDDO

    ENDDO
  ENDDO

!#endif

   write(6,*) ' mu_1 from comp ', mu_1(1,1)
   write(6,*) ' full state sounding from comp, ph, p, al, t_1, qv '
   do k=1,kde-1
     write(6,'(i3,1x,5(1x,1pe10.3))') k, ph_1(1,k,1)+phb(1,k,1), &
                                      p(1,k,1)+pb(1,k,1), alt(1,k,1), &
                                      t_1(1,k,1)+t0, moist(1,k,1,P_QV)
   enddo

   write(6,*) ' pert state sounding from comp, ph_1, pp, alp, t_1, qv '
   do k=1,kde-1
     write(6,'(i3,1x,5(1x,1pe10.3))') k, ph_1(1,k,1), &
                                      p(1,k,1), al(1,k,1), &
                                      t_1(1,k,1), moist(1,k,1,P_QV)
   enddo

! interp v

  DO J = jts, jte
  DO I = its, min(ide-1,ite)

    IF (j == jds) THEN
      z_at_v = phb(i,1,j)/g
    ELSE IF (j == jde) THEN
      z_at_v = phb(i,1,j-1)/g
    ELSE
      z_at_v = 0.5*(phb(i,1,j)+phb(i,1,j-1))/g
    END IF

    p_surf = interp_0( p_in, zk, z_at_v, nl_in )

    DO K = 1, kte
      p_level = znu(k)*(p_surf - p_top) + p_top
      v_1(i,k,j) = interp_0( v, p_in, p_level, nl_in )
      v_2(i,k,j) = v_1(i,k,j)
    ENDDO

  ENDDO
  ENDDO

! interp u

  DO J = jts, min(jde-1,jte)
  DO I = its, ite

    IF (i == ids) THEN
      z_at_u = phb(i,1,j)/g
    ELSE IF (i == ide) THEN
      z_at_u = phb(i-1,1,j)/g
    ELSE
      z_at_u = 0.5*(phb(i,1,j)+phb(i-1,1,j))/g
    END IF

    p_surf = interp_0( p_in, zk, z_at_u, nl_in )

    DO K = 1, kte
      p_level = znu(k)*(p_surf - p_top) + p_top
      u_1(i,k,j) = interp_0( u, p_in, p_level, nl_in )
      u_2(i,k,j) = u_1(i,k,j)
    ENDDO

  ENDDO
  ENDDO

!  set w

  DO J = jts, min(jde-1,jte)
  DO K = kts, kte
  DO I = its, min(ide-1,ite)
    w_1(i,k,j) = 0.
    w_2(i,k,j) = 0.
  ENDDO
  ENDDO
  ENDDO

!  set a few more things

  DO J = jts, min(jde-1,jte)
  DO K = kts, kte-1
  DO I = its, min(ide-1,ite)
    h_diabatic(i,k,j) = 0.
  ENDDO
  ENDDO
  ENDDO

  DO k=1,kte-1
    t_base(k) = t_1(1,k,1)
    qv_base(k) = moist(1,k,1,P_QV)
    u_base(k) = u_1(1,k,1)
    v_base(k) = v_1(1,k,1)
    z_base(k) = 0.5*(phb(1,k,1)+phb(1,k+1,1)+ph_1(1,k,1)+ph_1(1,k+1,1))/g
  ENDDO

  DO J = jts, min(jde-1,jte)
  DO I = its, min(ide-1,ite)
     thtmp   = t_2(i,1,j)+t0
     ptmp    = p(i,1,j)+pb(i,1,j)
     temp(1) = thtmp * (ptmp/p1000mb)**rcp
     thtmp   = t_2(i,2,j)+t0
     ptmp    = p(i,2,j)+pb(i,2,j)
     temp(2) = thtmp * (ptmp/p1000mb)**rcp
     thtmp   = t_2(i,3,j)+t0
     ptmp    = p(i,3,j)+pb(i,3,j)
     temp(3) = thtmp * (ptmp/p1000mb)**rcp

     TSK(I,J)=cf1*temp(1)+cf2*temp(2)+cf3*temp(3)
     TMN(I,J)=TSK(I,J)-0.5
  ENDDO
  ENDDO

#define COPY_OUT
#include <em_scalar_derefs.inc>
  RETURN

 END SUBROUTINE init_domain_rk

   SUBROUTINE init_module_initialize
   END SUBROUTINE init_module_initialize

!---------------------------------------------------------------------

!  test driver for get_sounding
!
!      implicit none
!      integer n
!      parameter(n = 1000)
!      real zk(n),p(n),theta(n),rho(n),u(n),v(n),qv(n),pd(n)
!      logical dry
!      integer nl,k
!
!      dry = .false.
!      dry = .true.
!      call get_sounding( zk, p, pd, theta, rho, u, v, qv, dry, n, nl )
!      write(6,*) ' input levels ',nl
!      write(6,*) ' sounding '
!      write(6,*) '  k  height(m)  press (Pa) pd(Pa) theta (K) den(kg/m^3)  u(m/s)     v(m/s)    qv(g/g) '
!      do k=1,nl
!        write(6,'(1x,i3,8(1x,1pe10.3))') k, zk(k), p(k), pd(k), theta(k), rho(k), u(k), v(k), qv(k)
!      enddo
!      end
!
!---------------------------------------------------------------------------

      subroutine get_sounding( zk, p, p_dry, theta, rho, &
                               u, v, qv, dry, nl_max, nl_in )
      implicit none

      integer nl_max, nl_in
      real zk(nl_max), p(nl_max), theta(nl_max), rho(nl_max), &
           u(nl_max), v(nl_max), qv(nl_max), p_dry(nl_max)
      logical dry

      integer n
      parameter(n=1000)
      logical debug
      parameter( debug = .true.)

! input sounding data

      real p_surf, th_surf, qv_surf
      real pi_surf, pi(n)
      real h_input(n), th_input(n), qv_input(n), u_input(n), v_input(n)

! diagnostics

      real rho_surf, p_input(n), rho_input(n)
      real pm_input(n)  !  this are for full moist sounding

! local data

      real p1000mb,cv,cp,r,cvpm,g
      parameter (p1000mb = 1.e+05, r = 287, cp = 1003., cv = cp-r, cvpm = -cv/cp, g=9.81 )
      integer k, it, nl
      real qvf, qvf1, dz

!  first, read the sounding

      call read_sounding( p_surf, th_surf, qv_surf, &
                          h_input, th_input, qv_input, u_input, v_input,n, nl, debug )

      if(dry) then
       do k=1,nl
         qv_input(k) = 0.
       enddo
      endif

      if(debug) write(6,*) ' number of input levels = ',nl

        nl_in = nl
        if(nl_in .gt. nl_max ) then
          write(6,*) ' too many levels for input arrays ',nl_in,nl_max
          call wrf_error_fatal ( ' too many levels for input arrays ' )
        end if

!  compute diagnostics,
!  first, convert qv(g/kg) to qv(g/g)

      do k=1,nl
        qv_input(k) = 0.001*qv_input(k)
      enddo

      p_surf = 100.*p_surf  ! convert to pascals
      qvf = 1. + rvovrd*qv_input(1) 
      rho_surf = 1./((r/p1000mb)*th_surf*qvf*((p_surf/p1000mb)**cvpm))
      pi_surf = (p_surf/p1000mb)**(r/cp)

      if(debug) then
        write(6,*) ' surface density is ',rho_surf
        write(6,*) ' surface pi is      ',pi_surf
      end if


!  integrate moist sounding hydrostatically, starting from the
!  specified surface pressure
!  -> first, integrate from surface to lowest level

          qvf = 1. + rvovrd*qv_input(1) 
          qvf1 = 1. + qv_input(1)
          rho_input(1) = rho_surf
          dz = h_input(1)
          do it=1,10
            pm_input(1) = p_surf &
                    - 0.5*dz*(rho_surf+rho_input(1))*g*qvf1
            rho_input(1) = 1./((r/p1000mb)*th_input(1)*qvf*((pm_input(1)/p1000mb)**cvpm))
          enddo

! integrate up the column

          do k=2,nl
            rho_input(k) = rho_input(k-1)
            dz = h_input(k)-h_input(k-1)
            qvf1 = 0.5*(2.+(qv_input(k-1)+qv_input(k)))
            qvf = 1. + rvovrd*qv_input(k)   ! qv is in g/kg here
 
            do it=1,10
              pm_input(k) = pm_input(k-1) &
                      - 0.5*dz*(rho_input(k)+rho_input(k-1))*g*qvf1
              rho_input(k) = 1./((r/p1000mb)*th_input(k)*qvf*((pm_input(k)/p1000mb)**cvpm))
            enddo
          enddo

!  we have the moist sounding

!  next, compute the dry sounding using p at the highest level from the
!  moist sounding and integrating down.

        p_input(nl) = pm_input(nl)

          do k=nl-1,1,-1
            dz = h_input(k+1)-h_input(k)
            p_input(k) = p_input(k+1) + 0.5*dz*(rho_input(k)+rho_input(k+1))*g
          enddo


        do k=1,nl

          zk(k) = h_input(k)
          p(k) = pm_input(k)
          p_dry(k) = p_input(k)
          theta(k) = th_input(k)
          rho(k) = rho_input(k)
          u(k) = u_input(k)
          v(k) = v_input(k)
          qv(k) = qv_input(k)

        enddo

     if(debug) then
      write(6,*) ' sounding '
      write(6,*) '  k  height(m)  press (Pa) pd(Pa) theta (K) den(kg/m^3)  u(m/s)     v(m/s)    qv(g/g) '
      do k=1,nl
        write(6,'(1x,i3,8(1x,1pe10.3))') k, zk(k), p(k), p_dry(k), theta(k), rho(k), u(k), v(k), qv(k)
      enddo

     end if

      end subroutine get_sounding

!-------------------------------------------------------

      subroutine read_sounding( ps,ts,qvs,h,th,qv,u,v,n,nl,debug )
      implicit none
      integer n,nl
      real ps,ts,qvs,h(n),th(n),qv(n),u(n),v(n)
      logical end_of_file
      logical debug

      integer k

      open(unit=10,file='input_sounding',form='formatted',status='old')
      rewind(10)
      read(10,*) ps, ts, qvs
      if(debug) then
        write(6,*) ' input sounding surface parameters '
        write(6,*) ' surface pressure (mb) ',ps
        write(6,*) ' surface pot. temp (K) ',ts
        write(6,*) ' surface mixing ratio (g/kg) ',qvs
      end if

      end_of_file = .false.
      k = 0

      do while (.not. end_of_file)

        read(10,*,end=100) h(k+1), th(k+1), qv(k+1), u(k+1), v(k+1)
        k = k+1
        if(debug) write(6,'(1x,i3,5(1x,e10.3))') k, h(k), th(k), qv(k), u(k), v(k)
        go to 110
 100    end_of_file = .true.
 110    continue
      enddo

      nl = k

      close(unit=10,status = 'keep')

      end subroutine read_sounding

END MODULE module_initialize
