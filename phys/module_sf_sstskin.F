!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_sf_sstskin

CONTAINS

   SUBROUTINE sst_skin_update(xland,glw,gsw,hfx,qfx,tsk,ust,emiss,  &
                dtw1,sstsk,dt,stbolt,                                &
                ids, ide, jds, jde, kds, kde,                       &
                ims, ime, jms, jme, kms, kme,                       &
                its, ite, jts, jte, kts, kte                       )


   USE module_wrf_error
   IMPLICIT NONE


!---------------------------------------------------------------------
   INTEGER , INTENT(IN)           :: ids, ide, jds, jde, kds, kde,   &
                                     ims, ime, jms, jme, kms, kme,   &
                                     its, ite, jts, jte, kts, kte


   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: xland, glw, gsw
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: hfx, qfx, tsk
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: ust, emiss
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT  ) :: dtw1         ! warm temp difference (C)
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT  ) :: sstsk        ! skin sst (K)
   REAL,                                      INTENT(IN )   ::   DT           ! model time step
   REAL,                                      INTENT(IN )   ::   STBOLT       ! Stefan-Boltzmann constant (W/m^2/K^4)
!---------------------------------------------------------------------
! Local
   REAL :: lw, sw, q, qn, zeta, dep, dtw3, skinmax, skinmin
   REAL :: fs, con1, con2, con3, con4, con5, zlan, q2, ts, phi, qn1
   REAL :: usw, qo, swo, us, tb, dtc, dtw, alw, dtwo, delt, f1
   INTEGER :: i, j, k
!---------------------------------------------------------------------
   INTEGER , PARAMETER :: n=1152
   REAL , PARAMETER :: z1=3.,an=.3,zk=.4,rho=1.2,rhow=1025.,cw=4190.
   REAL , PARAMETER :: g=9.8,znuw=1.e-6,zkw=1.4e-7,sdate=1201.6667
!     parameter(g=9.8,delt=900.,znuw=1.e-6,zkw=1.4e-7)
!
!     Input arguments
!     (all fluxes are positive downwards)
!     real qo      ! LH + SH + LW (W/m^2), + down
!     real swo      ! Net shortwave flux (W/m^2), + down
!     real u       ! Wind speed (m/s)
!     real us      ! Atmospheric friction velocity (m/s)
!     real tb      ! Bulk temperature (deg C)
!     real dtwo    ! Warm layer temp. diff. from previous time (deg C)
!     Local variables
!     real lw
!     real sw
!     real q       ! LH + SH + LW
!     real qn      ! Q + R_s - R(-d)
!     real zeta    ! -z / L
!     real dep     ! Skin layer depth (m)
!     real dtw3
!     Output variables
!     real dtw     ! Warm layer temp. diff. (deg C)
!     real dtc     ! Cool skin temp. diff. (deg C)
!     real ts      ! Skin temperature (deg C)
!      q=lh+sh+lwo
!
      skinmax=-9999.
      skinmin=9999.
      do i=its,ite
      do j=jts,jte
!
      if(xland(i,j).ge.1.5) then
      qo=glw(i,j)-emiss(i,j)*stbolt*(sstsk(i,j)**4)-2.5e6*qfx(i,j)-hfx(i,j)
      swo=gsw(i,j)
      us=MAX(ust(i,j), 0.01)
      tb=tsk(i,j)-273.15
      dtwo=dtw1(i,j)
      delt=dt
!
      q=qo/(rhow*cw)
      sw=swo/(rhow*cw)
! TEMPORARY KLUDGE
!     f1=1.-0.28*exp(-71.5*z1)-0.27*exp(-2.8*z1)-0.45*exp(-0.07*z1)
      f1=1.                   -0.27*exp(-2.8*z1)-0.45*exp(-0.07*z1)
! cool skin
      dtc=0.0
! tb in C
      alw=1.e-5*max(tb,1.)
      con4=16.*g*alw*znuw**3/zkw**2
      usw=sqrt(rho/rhow)*us
      con5=con4/usw**4
! otherwise, iterations would be needed for the computation of fs
! iteration impact is less than 0.03C
      q2=max(1./(rhow*cw),-q)
      zlan=6./(1.+(con5*q2)**0.75)**0.333
      dep=zlan*znuw/usw                    ! skin layer depth (m)
      fs=0.065+11.*dep-(6.6e-5/dep)*(1.-exp(-dep/8.e-4))
      fs=max(fs,0.01)          ! fract. of solar rad. absorbed in sublayer
      dtc=dep*(q+sw*fs)/zkw            ! cool skin temp. diff (deg C)
      dtc=min(dtc,0.)
! warm layer (X. Zeng)
      dtw=0.0
! tb in C
      alw=1.e-5*max(tb,1.)
      con1=sqrt(5.*z1*g*alw/an)
      con2=zk*g*alw
      qn=q+sw*f1
      usw=sqrt(rho/rhow)*us
!  does not change when qn is positive
      if(dtwo.gt.0..and.qn.lt.0.) then
         qn1=sqrt(dtwo)*usw**2/con1
         qn=max(qn,qn1)
      endif
      zeta=z1*con2*qn/usw**3
      if(zeta.gt.0.) then
         phi=1.+5.*zeta
      else
         phi=1./sqrt(1.-16.*zeta)
      endif
      con3=zk*usw/(z1*phi)
! use all SW flux
      dtw=(dtwo+(an+1.)/an*(q+sw*f1)*                             &
                          delt/z1)/(1.+(an+1.)*con3*delt)
      dtw=max(0.,dtw)
      dtwo=dtw
      ts = tb + dtw + dtc
!
      skinmax=amax1(skinmax,ts-tb)
      skinmin=amin1(skinmin,ts-tb)
      sstsk(i,j)=ts+273.15      ! convert ts (in C) to sstsk (in K)
      dtw1(i,j)=dtw              ! dtw always in C
      endif
!
      end do
      end do
!     print *, 'check skin sst skinmax = ', skinmax, '  skinmin = ', skinmin
!
      return

   END SUBROUTINE sst_skin_update


END MODULE module_sf_sstskin
