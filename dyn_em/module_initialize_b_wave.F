!IDEAL:MODEL_LAYER:INITIALIZATION

!  This MODULE holds the routines which are used to perform various initializations
!  for the individual domains.  

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
     call wrf_error_fatal ( ' init_domain: unknown or unimplemented dyn_opt ' )
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

   LOGICAL :: moisture_init
   LOGICAL :: stretch_grid, dry_sounding, debug

!  kludge space for initial jet

   INTEGER, parameter :: nz_jet=64, ny_jet=80
   REAL, DIMENSION(nz_jet, ny_jet) :: u_jet, rho_jet, th_jet, z_jet

!  perturbation parameters

   REAL, PARAMETER :: htbub=8000., radbub=2000000., radz=8000., tpbub=1.0
   REAL :: piov2, tp
   INTEGER :: icen, jcen
   real :: thtmp, ptmp, temp(3)
#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

#define COPY_IN
#include <em_scalar_derefs.inc>
#include "deref_kludge.h"
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

   piov2 = 2.*atan(1.0)
   icen = ide/4
   jcen = jde/2

   stretch_grid = .true.
   delt = 0.
   z_scale = .50
   pi = 2.*asin(1.0)
   write(6,*) ' pi is ',pi
   nxc = (ide-ids)/4
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

    CALL nl_set_mminlu(1,'    ')
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

         ht(i,j)       = 0.
         msft(i,j)     = 1.
         msfu(i,j)     = 1.
         msfv(i,j)     = 1.
         sina(i,j)     = 0.
         cosa(i,j)     = 1.
         e(i,j)        = 0.
         f(i,j)        = 1.e-04

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

  write(6,*) ' reading input jet sounding '
  call read_input_jet( u_jet, rho_jet, th_jet, z_jet, nz_jet, ny_jet )

  write(6,*) ' getting dry sounding for base state '
  write(6,*) ' using middle column in jet sounding, j = ',ny_jet/2
  dry_sounding = .true.

  dry_sounding   = .true.
  debug = .true.  !  this will produce print of the sounding
  CALL get_sounding( zk, p_in, pd_in, theta, rho, u, v, qv, dry_sounding, &
                      nl_max, nl_in, u_jet, rho_jet, th_jet, z_jet,      &
                      nz_jet, ny_jet, ny_jet/2, debug                   )

  write(6,*) ' returned from reading sounding, nl_in is ',nl_in

!  find ptop for the desired ztop (ztop is input from the namelist),
!  and find surface pressure

!  For the jet, using the middle column for the base state means that
!  we will be extrapolating above the highest height data to the south
!  of the centerline.

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

  dry_sounding = .true.
  IF (config_flags%mp_physics /= 0)  dry_sounding = .false.

  DO J = jts, min(jde-1,jte)

!  get sounding for this point

  debug = .false.  !  this will turn off print of the sounding
  CALL get_sounding( zk, p_in, pd_in, theta, rho, u, v, qv, dry_sounding, &
                      nl_max, nl_in, u_jet, rho_jet, th_jet, z_jet,      &
                      nz_jet, ny_jet, j, debug                          )

  DO I = its, min(ide-1,ite)

!   we could just do the first point in "i" and copy from there, but we'll
!   be lazy and do all the points as if they are all, independent

!   At this point p_top is already set. find the DRY mass in the column 
!   by interpolating the DRY pressure.  

    pd_surf = interp_0( pd_in, zk, phb(i,1,j)/g, nl_in )

!   compute the perturbation mass and the full mass

    mu_1(i,j) = pd_surf-p_top - mub(i,j)
    mu_2(i,j) = mu_1(i,j)
    mu0(i,j) = mu_1(i,j) + mub(i,j)

!   given the dry pressure and coordinate system, interp the potential
!   temperature and qv

    do k=1,kde-1

      p_level = znu(k)*(pd_surf - p_top) + p_top

      moist(i,k,j,P_QV) = interp_0( qv, pd_in, p_level, nl_in )
      t_1(i,k,j)          = interp_0( theta, pd_in, p_level, nl_in ) - t0
      t_2(i,k,j)          = t_1(i,k,j)
      

    enddo

!   integrate the hydrostatic equation (from the RHS of the bigstep
!   vertical momentum equation) down from the top to get p.
!   first from the top of the model to the top pressure

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

! interp u

    DO K = 1, kte
      p_level = znu(k)*(p_surf - p_top) + p_top
      u_1(i,k,j) = interp_0( u, p_in, p_level, nl_in )
      u_2(i,k,j) = u_1(i,k,j)
    ENDDO

  ENDDO
  ENDDO

!  thermal perturbation to kick off convection

  write(6,*) ' nxc, nyc for perturbation ',nxc,nyc
  write(6,*) ' delt for perturbation ',tpbub

  DO J = jts, min(jde-1,jte)
    yrad = dy*float(j-jde/2-1)/radbub
    DO I = its, min(ide-1,ite)
      xrad = float(i-1)/float(ide-ids)

      DO K = 1, kte-1

!  put in preturbation theta (bubble) and recalc density.  note,
!  the mass in the column is not changing, so when theta changes,
!  we recompute density and geopotential

        zrad = 0.5*(ph_1(i,k,j)+ph_1(i,k+1,j)  &
                   +phb(i,k,j)+phb(i,k+1,j))/g
        zrad = (zrad-htbub)/radz
        RAD=SQRT(yrad*yrad+zrad*zrad)
        IF(RAD <= 1.) THEN
           tp = tpbub*cos(rad*piov2)*cos(rad*piov2)*cos(xrad*2*pi+pi)
           T_1(i,k,j)=T_1(i,k,j)+tp
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
   write(6,*) ' pert state sounding from comp, ph_1, pp, al, t_1, qv '
   do k=1,kde-1
     write(6,'(i3,1x,5(1x,1pe10.3))') k, ph_1(1,k,1),p(1,k,1), al(1,k,1), &
                     t_1(1,k,1), moist(1,k,1,P_QV)
   enddo

   write(6,*) ' mu_1 from comp ', mu_1(1,1)
   write(6,*) ' full state sounding from comp, ph, p, al, t_1, qv '
   write(6,*) ' at j = 1 '
   do k=1,kde-1
     write(6,'(i3,1x,5(1x,1pe10.3))') k, ph_1(1,k,1)+phb(1,k,1), &
                     p(1,k,1)+pb(1,k,1), al(1,k,1)+alb(1,k,1), &
                     t_1(1,k,1)+t0, moist(1,k,1,P_QV)
   enddo


   write(6,*) ' full state sounding from comp, ph, p, al, t_1, qv '
   write(6,*) ' at j = jde/2 '
   do k=1,kde-1
     write(6,'(i3,1x,5(1x,1pe10.3))') k, ph_1(1,k,jde/2)+phb(1,k,jde/2), &
                     p(1,k,jde/2)+pb(1,k,jde/2), al(1,k,jde/2)+alb(1,k,jde/2), &
                     t_1(1,k,jde/2)+t0, moist(1,k,jde/2,P_QV)
   enddo

   write(6,*) ' full state sounding from comp, ph, p, al, t_1, qv '
   write(6,*) ' at j = jde-1 '
   do k=1,kde-1
     write(6,'(i3,1x,5(1x,1pe10.3))') k, ph_1(1,k,jde-1)+phb(1,k,jde-1), &
                     p(1,k,jde-1)+pb(1,k,jde-1), al(1,k,jde-1)+alb(1,k,jde-1), &
                     t_1(1,k,jde-1)+t0, moist(1,k,jde-1,P_QV)
   enddo

! set v

  DO J = jts, jte
  DO I = its, min(ide-1,ite)

    DO K = 1, kte
      v_1(i,k,j) = 0.
      v_2(i,k,j) = v_1(i,k,j)
    ENDDO

  ENDDO
  ENDDO

!  fill out last i row for u

  DO J = jts, min(jde-1,jte)
  DO I = ite, ite

    DO K = 1, kte
      u_1(i,k,j) = u_1(its,k,j)
      u_2(i,k,j) = u_2(its,k,j)
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
     if (i .eq. 1) print*,'sfctem',j,temp(1),temp(2),temp(3),TSK(I,J)
     TMN(I,J)=TSK(I,J)-0.5
  ENDDO
  ENDDO

#define COPY_OUT
#include <em_scalar_derefs.inc>
  RETURN

 END SUBROUTINE init_domain_rk

!---------------------------------------------------------------------

 SUBROUTINE init_module_initialize
 END SUBROUTINE init_module_initialize

!---------------------------------------------------------------------
#if 0
! TEST DRIVER FOR "read_input_jet" and "get_sounding"
  implicit none 
  integer, parameter :: nz_jet=64, ny_jet=80
  real, dimension(nz_jet,ny_jet) :: u_jet, rho_jet, &
                                    th_jet, z_jet

  real, dimension(nz_jet,ny_jet) :: zk,p,p_dry,theta,rho,u,v,qv
  logical :: dry, debug
  integer :: j, nl

  call read_input_jet( u_jet, rho_jet, th_jet, z_jet, nz_jet, ny_jet )

  call opngks
  call parray( u_jet, nz_jet, ny_jet)
  call parray( rho_jet, nz_jet, ny_jet)
  call parray( th_jet, nz_jet, ny_jet)
!  call clsgks

!  set up initial jet

  debug = .true.
  dry = .true.
  do j=1,ny_jet

    call get_sounding( zk(:,j),p(:,j),p_dry(:,j),theta(:,j),      &
                       rho(:,j),u(:,j), v(:,j),  qv(:,j),        &
                       dry, nz_jet, nl, u_jet, rho_jet, th_jet,  &
                       z_jet, nz_jet, ny_jet, j, debug          )
    debug = .false.

  enddo

  write(6,*) ' lowest level p, th, and rho, highest level p '

  do j=1,ny_jet
    write(6,*) j, p(1,j),theta(1,j),rho(1,j), p(nz_jet,j)
!    write(6,*) j, p(1,j),theta(1,j)-th_jet(1,j),rho(1,j)-rho_jet(1,j)
  enddo

  call parray( p, nz_jet, ny_jet)
  call parray( p_dry, nz_jet, ny_jet)
  call parray( theta, nz_jet, ny_jet)

  call clsgks

  end

!---------------------------------

      subroutine parray(a,m,n)
      dimension a(m,n)
      dimension b(n,m)

    do i=1,m
    do j=1,n
      b(j,i) = a(i,j)
    enddo
    enddo
      
      write(6,'(''  dimensions m,n  '',2i6)')m,n
        call set(.05,.95,.05,.95,0.,1.,0.,1.,1)
        call perim(4,5,4,5)
        call setusv('LW',2000)
!        CALL CONREC(a,m,m,n,cmax,cmin,cinc,-1,-638,-922)
        CALL CONREC(b,n,n,m,0.,0.,0.,-1,-638,-922)
        call frame
      return
      end

! END TEST DRIVER FOR "read_input_jet" and "get_sounding"
#endif

!------------------------------------------------------------------

    subroutine get_sounding( zk, p, p_dry, theta, rho,       &
                             u, v, qv, dry, nl_max, nl_in,  &
                             u_jet, rho_jet, th_jet, z_jet, &
                             nz_jet, ny_jet, j_point, debug )
    implicit none

    integer nl_max, nl_in
    real zk(nl_max), p(nl_max), theta(nl_max), rho(nl_max), &
         u(nl_max), v(nl_max), qv(nl_max), p_dry(nl_max)
    logical dry

    integer nz_jet, ny_jet, j_point
    real, dimension(nz_jet, ny_jet) :: u_jet, rho_jet, th_jet, z_jet

    integer n
    parameter(n=1000)
    logical debug

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

!    call read_sounding( p_surf, th_surf, qv_surf, &
!                          h_input, th_input, qv_input, u_input, v_input,n, nl, debug )

   call calc_jet_sounding( p_surf, th_surf, qv_surf,                             &
                           h_input, th_input, qv_input, u_input, v_input,        &
                           n, nl, debug, u_jet, rho_jet, th_jet, z_jet, j_point, &
                           nz_jet, ny_jet, dry                                  )

   nl = nz_jet

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
!
!      do k=1,nl
!        qv_input(k) = 0.001*qv_input(k)
!      enddo
!      p_surf = 100.*p_surf  ! convert to pascals

    qvf = 1. + rvovrd*qv_input(1) 
    rho_surf = 1./((r/p1000mb)*th_surf*qvf*((p_surf/p1000mb)**cvpm))
    pi_surf = (p_surf/p1000mb)**(r/cp)

    if(debug) then
      write(6,*) ' surface density is ',rho_surf
      write(6,*) ' surface pi is    ',pi_surf
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
      write(6,*) '  k  height(m)  press (Pa)   pd(Pa)   theta (K)  den(kg/m^3)  u(m/s)     v(m/s)    qv(g/g) '
      do k=1,nl
        write(6,'(1x,i3,8(1x,1pe10.3))') k, zk(k), p(k), p_dry(k), theta(k), rho(k), u(k), v(k), qv(k)
      enddo

     end if

     end subroutine get_sounding

!------------------------------------------------------------------

  subroutine calc_jet_sounding( p_surf, th_surf, qv_surf,      &
                                h, th, qv, u, v, n, nl, debug, &
                                u_jet, rho_jet, th_jet, z_jet, &
                                jp, nz_jet, ny_jet, dry       )
  implicit none
  integer :: n, nl, jp, nz_jet, ny_jet

  real, dimension(nz_jet, ny_jet) :: u_jet, rho_jet, th_jet, z_jet
  real, dimension(n) :: h,th,qv,u,v
  real :: p_surf, th_surf, qv_surf
  logical :: debug, dry

  real, dimension(1:nz_jet) :: rho, rel_hum, p
  integer :: k

!  some local stuff

  real :: tmppi, es, qvs, temperature
  real, parameter :: p1000mb=1.e+05, rcp=287./1004.5, svpt0=273.15, &
                     svp3 = 29.65, ep_2=287./461.6, r_d = 287., &
                     cpovcv = 1004./(1004.-287.),               &
                     svp1 = 0.6112, svp2 = 17.67

!  get sounding from column jp

   do k=1,nz_jet
     h(k)  = z_jet(k,jp)
     th(k) = th_jet(k,jp)
     qv(k) = 0.
     rho(k) = rho_jet(k,jp)
     u(k) = u_jet(k,jp)
     v(k) = 0.
   enddo

   if (.not.dry) then
     DO k=1,nz_jet
       if(h(k) .gt. 8000.) then
         rel_hum(k)=0.1
       else
         rel_hum(k)=(1.-0.90*(h(k)/8000.)**1.25)
       end if
       rel_hum(k) = min(0.7,rel_hum(k))
     ENDDO
   else
     do k=1,nz_jet
       rel_hum(k) = 0.
     enddo
   endif

!  next, compute pressure

   do k=1,nz_jet
     p(k) = p1000mb*(R_d*rho(k)*th(k)/p1000mb)**cpovcv
   enddo

!  here we adjust for fixed moisture profile

     IF (.not.dry)  THEN

!  here we assume the input theta is th_v, so we reset theta accordingly

       DO k=1,nz_jet
         tmppi=(p(k)/p1000mb)**rcp
         temperature = tmppi*th(k)
         if (temperature .gt. svpt0) then
            es  = 1000.*svp1*exp(svp2*(temperature-svpt0)/(temperature-svp3))
            qvs = ep_2*es/(p(k)-es)
         else
            es  = 1000.*svp1*exp( 21.8745584*(temperature-273.16)/(temperature-7.66) )
            qvs = ep_2*es/(p(k)-es)
         endif
         qv(k) = rel_hum(k)*qvs
         th(k) = th(k)/(1.+.61*qv(k))
       ENDDO
 
     ENDIF

!  finally, set the surface data. We'll just do a simple extrapolation

   p_surf = 1.5*p(1) - 0.5*p(2)
   th_surf = 1.5*th(1) - 0.5*th(2)
   qv_surf = 1.5*qv(1) - 0.5*qv(2)

   end subroutine calc_jet_sounding

!---------------------------------------------------------------------

 SUBROUTINE read_input_jet( u, r, t, zk, nz, ny )
 implicit none

 integer, intent(in) :: nz,ny
 real, dimension(nz,ny), intent(out) :: u,r,t,zk
 integer :: ny_in, nz_in, j,k
 real, dimension(ny,nz) :: field_in

! this code assumes it is called on processor 0 only

   OPEN(unit=10, file='input_jet', form='unformatted', status='old' )
   REWIND(10) 
   read(10) ny_in,nz_in
   if((ny_in /= ny ) .or. (nz_in /= nz)) then
     write(0,*) ' error in input jet dimensions '
     write(0,*) ' ny, ny_input, nz, nz_input ', ny, ny_in, nz,nz_in
     write(0,*) ' error exit '
     call wrf_error_fatal ( ' error in input jet dimensions ' )
   end if
   read(10) field_in
   do j=1,ny
   do k=1,nz
     u(k,j) = field_in(j,k)
   enddo
   enddo
   read(10) field_in
   do j=1,ny
   do k=1,nz
     t(k,j) = field_in(j,k)
   enddo
   enddo

   read(10) field_in
   do j=1,ny
   do k=1,nz
     r(k,j) = field_in(j,k)
   enddo
   enddo

   do j=1,ny
   do k=1,nz
     zk(k,j) = 125. + 250.*float(k-1)
   enddo
   enddo

 end subroutine read_input_jet

END MODULE module_initialize
