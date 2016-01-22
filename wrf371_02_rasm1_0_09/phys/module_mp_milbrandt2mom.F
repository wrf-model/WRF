!_______________________________________________________________________________!
!                                                                               !
!  This module contains the microphysics sub-driver for the 2-moment version of !
!  the Milbrandt-Yau (2005, JAS) microphysics scheme, along with all associated !
!  subprograms.  The main subroutine, 'mp_milbrandt2mom_main', is essentially   !
!  directly from the RPN-CMC physics library of the Canadian GEM model.  It is  !
!  called by the wrapper 'mp_milbrandt2mom_driver' which makes the necessary    !
!  adjustments to the calling parameters for the interface to WRF.              !
!                                                                               !
!  For questions, bug reports, etc. pertaining to the scheme, or to request     !
!  updates to the code (before the next offical WRF release) please contact     !
!  Jason Milbrandt (Environment Canada) at jason.milbrandt@ec.gc.ca             !
!                                                                               !
!_______________________________________________________________________________!
!                                                                               !
!  Package version:   2.25.2_beta_04   (internal bookkeeping)                   !
!  Last modified:     2015-02-11                                                !
!_______________________________________________________________________________!

MODULE my2_mod

 private
 public  :: mp_milbrandt2mom_main
 private :: NccnFNC,SxFNC,gamma,gser,gammln,gammp,cfg,gamminc,polysvp,qsat,check_values
 private :: sedi_wrapper_2,sedi_1D,count_columns,des_OF_Ds,Dm_x,iLAMDA_x,N_Cooper,Nos_Thompson

 CONTAINS

!==============================================================================!

 REAL FUNCTION NccnFNC(Win,Tin,Pin,CCNtype)

!---------------------------------------------------------------------------!
! This function returns number concentration (activated aerosols) as a
! function of w,T,p, based on polynomial approximations of detailed
! approach using a hypergeometric function, following Cohard and Pinty (2000a).
!---------------------------------------------------------------------------!

  IMPLICIT NONE

! PASSING PARAMETERS:
  real,    intent(in) :: Win, Tin, Pin
  integer, intent(in) :: CCNtype

! LOCAL PARAMETERS:
  real :: T,p,x,y,a,b,c,d,e,f,g,h,T2,T3,T4,x2,x3,x4,p2

  x= log10(Win*100.);   x2= x*x;  x3= x2*x;  x4= x2*x2
  T= Tin - 273.15;      T2= T*T;  T3= T2*T;  T4= T2*T2
  p= Pin*0.01;          p2= p*p

  if (CCNtype==1) then  !Maritime

     a= 1.47e-9*T4 -6.944e-8*T3 -9.933e-7*T2 +2.7278e-4*T -6.6853e-4
     b=-1.41e-8*T4 +6.662e-7*T3 +4.483e-6*T2 -2.0479e-3*T +4.0823e-2
     c= 5.12e-8*T4 -2.375e-6*T3 +4.268e-6*T2 +3.9681e-3*T -3.2356e-1
     d=-8.25e-8*T4 +3.629e-6*T3 -4.044e-5*T2 +2.1846e-3*T +9.1227e-1
     e= 5.02e-8*T4 -1.973e-6*T3 +3.944e-5*T2 -9.0734e-3*T +1.1256e0
     f= -1.424e-6*p2 +3.631e-3*p -1.986
     g= -0.0212*x4 +0.1765*x3 -0.3770*x2 -0.2200*x +1.0081
     h= 2.47e-6*T3 -3.654e-5*T2 +2.3327e-3*T +0.1938
     y= a*x4 + b*x3 + c*x2 + d*x + e + f*g*h
     NccnFNC= 10.**min(2.,max(0.,y)) *1.e6                ![m-3]

  else if (CCNtype==2) then  !Continental

     a= 0.
     b= 0.
     c=-2.112e-9*T4 +3.9836e-8*T3 +2.3703e-6*T2 -1.4542e-4*T -0.0698
     d=-4.210e-8*T4 +5.5745e-7*T3 +1.8460e-5*T2 +9.6078e-4*T +0.7120
     e= 1.434e-7*T4 -1.6455e-6*T3 -4.3334e-5*T2 -7.6720e-3*T +1.0056
     f= 1.340e-6*p2 -3.5114e-3*p  +1.9453
     g= 4.226e-3*x4 -5.6012e-3*x3 -8.7846e-2*x2 +2.7435e-2*x +0.9932
     h= 5.811e-9*T4 +1.5589e-7*T3 -3.8623e-5*T2 +1.4471e-3*T +0.1496
     y= a*x4 +b*x3 +c*x2 + d*x + e + (f*g*h)
     NccnFNC= 10.**max(0.,y) *1.e6

  else

    print*, '*** STOPPED in MODULE ### NccnFNC  *** '
    print*, '    Parameter CCNtype incorrectly specified'
    stop

  endif

 END FUNCTION NccnFNC
!======================================================================!

   real FUNCTION SxFNC(Win,Tin,Pin,Qsw,Qsi,CCNtype,WRT)

!---------------------------------------------------------------------------!
! This function returns the peak supersaturation achieved during ascent with
! activation of CCN aerosols as a function of w,T,p, based on polynomial
! approximations of detailed approach using a hypergeometric function,
! following Cohard and Pinty (2000a).
!---------------------------------------------------------------------------!

 IMPLICIT NONE

! PASSING PARAMETERS:
  integer, intent(IN) :: WRT
  integer, intent(IN) :: CCNtype
  real,    intent(IN) :: Win, Tin, Pin, Qsw, Qsi

! LOCAL PARAMETERS:
  real   ::  Si,Sw,Qv,T,p,x,a,b,c,d,f,g,h,Pcorr,T2corr,T2,T3,T4,x2,x3,x4,p2
  real, parameter :: TRPL= 273.15

  x= log10(max(Win,1.e-20)*100.);   x2= x*x;  x3= x2*x;  x4= x2*x2
  T= Tin;                           T2= T*T;  T3= T2*T;  T4= T2*T2
  p= Pin*0.01;                      p2= p*p

  if (CCNtype==1) then  !Maritime

     a= -5.109e-7*T4 -3.996e-5*T3 -1.066e-3*T2 -1.273e-2*T +0.0659
     b=  2.014e-6*T4 +1.583e-4*T3 +4.356e-3*T2 +4.943e-2*T -0.1538
     c= -2.037e-6*T4 -1.625e-4*T3 -4.541e-3*T2 -5.118e-2*T +0.1428
     d=  3.812e-7*T4 +3.065e-5*T3 +8.795e-4*T2 +9.440e-3*T +6.14e-3
     f= -2.012e-6*p2 + 4.1913e-3*p    - 1.785e0
     g=  2.832e-1*x3 -5.6990e-1*x2 +5.1105e-1*x -4.1747e-4
     h=  1.173e-6*T3 +3.2174e-5*T2 -6.8832e-4*T +6.7888e-2
     Pcorr= f*g*h
     T2corr= 0.9581-4.449e-3*T-2.016e-4*T2-3.307e-6*T3-1.725e-8*T4

  else if (CCNtype==2) then  !Continental [computed for -35<T<-5C]

     a=  3.80e-5*T2 +1.65e-4*T +9.88e-2
     b= -7.38e-5*T2 -2.53e-3*T -3.23e-1
     c=  8.39e-5*T2 +3.96e-3*T +3.50e-1
     d= -1.88e-6*T2 -1.33e-3*T -3.73e-2
     f= -1.9761e-6*p2 + 4.1473e-3*p - 1.771e0
     g=  0.1539*x4 -0.5575*x3 +0.9262*x2 -0.3498*x -0.1293
     h=-8.035e-9*T4+3.162e-7*T3+1.029e-5*T2-5.931e-4*T+5.62e-2
     Pcorr= f*g*h
     T2corr= 0.98888-5.0525e-4*T-1.7598e-5*T2-8.3308e-8*T3

  else

    print*, '*** STOPPED in MODULE ### SxFNC  *** '
    print*, '    Parameter CCNtype incorrectly specified'
    stop

  endif

  Sw= (a*x3 + b*x2 +c*x + d) + Pcorr
  Sw= 1. + 0.01*Sw
  Qv= Qsw*Sw
  Si= Qv/Qsi
  Si= Si*T2corr
  if (WRT.eq.1) then
     SxFNC= Sw
  else
     SxFNC= Si
  endif
  if (Win.le.0.) SxFNC= 1.

 END function SxFNC
!======================================================================!

 real FUNCTION gamma(xx)

!  Modified from "Numerical Recipes"

  IMPLICIT NONE

! PASSING PARAMETERS:
  real, intent(IN) :: xx

! LOCAL PARAMETERS:
  integer  :: j
  real*8   :: ser,stp,tmp,x,y,cof(6),gammadp


  SAVE cof,stp
  DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,               &
       24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,  &
       -.5395239384953d-5,2.5066282746310005d0/
  x=dble(xx)
  y=x
  tmp=x+5.5d0
  tmp=(x+0.5d0)*log(tmp)-tmp
  ser=1.000000000190015d0
! do j=1,6   !original
  do j=1,4
!!do j=1,3   !gives result to within ~ 3 %
     y=y+1.d0
     ser=ser+cof(j)/y
  enddo
  gammadp=tmp+log(stp*ser/x)
  gammadp= exp(gammadp)

!!GEM:
  gamma  = sngl(gammadp)

 END FUNCTION gamma
!======================================================================!

 SUBROUTINE gser(gamser,a,x,gln)

! USES gammln

!   Returns the incomplete gamma function P(a,x) evaluated by its series
!   representation as gamser.  Also returns GAMMA(a) as gln.

 implicit none

 integer :: itmax
 real    :: a,gamser,gln,x,eps
 parameter (itmax=100, eps=3.e-7)
 integer :: n
 real :: ap,de1,summ

 gln=gammln(a)
 if(x.le.0.)then
    gamser=0.
    return
 endif
 ap=a
 summ=1./a
 de1=summ
 do n=1,itmax
    ap=ap+1.
    de1=de1*x/ap
    summ=summ+de1
    if(abs(de1).lt.abs(summ)*eps) goto 1
 enddo
1 gamser=summ*exp(-x+a*log(x)-gln)
 return

END SUBROUTINE gser
!======================================================================!

 real FUNCTION gammln(xx)

!  Returns value of ln(GAMMA(xx)) for xx>0
!   (modified from "Numerical Recipes")

  IMPLICIT NONE

! PASSING PARAMETERS:
  real, intent(IN) :: xx

! LOCAL PARAMETERS:
  integer  :: j
  real*8   :: ser,stp,tmp,x,y,cof(6)

  SAVE cof,stp
  DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,               &
       24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,  &
       -.5395239384953d-5,2.5066282746310005d0/
  x=dble(xx)
  y=x
  tmp=x+5.5d0
  tmp=(x+0.5d0)*log(tmp)-tmp
  ser=1.000000000190015d0
  do j=1,6   !original
!  do j=1,4
     y=y+1.d0
     ser=ser+cof(j)/y
  enddo

!! GEM:
  gammln= sngl( tmp+log(stp*ser/x)  )

 END FUNCTION gammln
!======================================================================!

 real FUNCTION gammp(a,x)

! USES gcf,gser

! Returns the incomplete gamma function P(a,x)

 implicit none

 real :: a,x,gammcf,gamser,gln

 if(x.lt.a+1.)then
    call gser(gamser,a,x,gln)
    gammp=gamser
 else
    call cfg(gammcf,a,x,gln)
    gammp=1.-gammcf
 endif
 return

 END FUNCTION gammp
!======================================================================!

 SUBROUTINE cfg(gammcf,a,x,gln)

! USES gammln

! Returns the incomplete gamma function (Q(a,x) evaluated by tis continued fraction
! representation as gammcf.  Also returns ln(GAMMA(a)) as gln.  ITMAX is the maximum
! allowed number of iterations; EPS is the relative accuracy; FPMIN is a number near
! the smallest representable floating-point number.

 implicit none

 integer :: i,itmax
 real    :: a,gammcf,gln,x,eps,fpmin
 real    :: an,b,c,d,de1,h
 parameter (itmax=100,eps=3.e-7)

 gln=gammln(a)
 b=x+1.-a
 c=1./fpmin
 d=1./b
 h=d
 do i= 1,itmax
   an=-i*(i-a)
   b=b+2.
   d=an*d+b
   if(abs(d).lt.fpmin)d=fpmin
   c=b+an/c
 if(abs(c).lt.fpmin) c=fpmin
   d=1./d
   de1=d*c
   h=h*de1
   if(abs(de1-1.).lt.eps) goto 1
 enddo
1 gammcf=exp(-x+a*log(x)-gln)*h
 return

END SUBROUTINE cfg
!======================================================================!

 real FUNCTION gamminc(p,xmax)

! USES gammp, gammln
! Returns incomplete gamma function, gamma(p,xmax)= P(p,xmax)*GAMMA(p)
 real :: p,xmax
 gamminc= gammp(p,xmax)*exp(gammln(p))

 end FUNCTION gamminc
!======================================================================!

 real function polysvp(T,TYPE)

!--------------------------------------------------------------
! Taken from 'module_mp_morr_two_moment.F' (WRFV3.4)

!  COMPUTE SATURATION VAPOR PRESSURE

!  POLYSVP RETURNED IN UNITS OF PA.
!  T IS INPUT IN UNITS OF K.
!  TYPE REFERS TO SATURATION WITH RESPECT TO LIQUID (0) OR ICE (1)

! REPLACE GOFF-GRATCH WITH FASTER FORMULATION FROM FLATAU ET AL. 1992,
! TABLE 4 (RIGHT-HAND COLUMN)
!--------------------------------------------------------------

      IMPLICIT NONE

      REAL DUM
      REAL T
      INTEGER TYPE
! ice
      real a0i,a1i,a2i,a3i,a4i,a5i,a6i,a7i,a8i
      data a0i,a1i,a2i,a3i,a4i,a5i,a6i,a7i,a8i /&
	6.11147274, 0.503160820, 0.188439774e-1, &
        0.420895665e-3, 0.615021634e-5,0.602588177e-7, &
        0.385852041e-9, 0.146898966e-11, 0.252751365e-14/

! liquid
      real a0,a1,a2,a3,a4,a5,a6,a7,a8

! V1.7
      data a0,a1,a2,a3,a4,a5,a6,a7,a8 /&
	6.11239921, 0.443987641, 0.142986287e-1, &
        0.264847430e-3, 0.302950461e-5, 0.206739458e-7, &
        0.640689451e-10,-0.952447341e-13,-0.976195544e-15/
      real dt

! ICE

      IF (TYPE.EQ.1) THEN

!         POLYSVP = 10.**(-9.09718*(273.16/T-1.)-3.56654*                &
!          LOG10(273.16/T)+0.876793*(1.-T/273.16)+						&
!          LOG10(6.1071))*100.


      dt = max(-80.,t-273.16)
      polysvp = a0i + dt*(a1i+dt*(a2i+dt*(a3i+dt*(a4i+dt*(a5i+dt*(a6i+dt*(a7i+a8i*dt)))))))
      polysvp = polysvp*100.

      END IF

! LIQUID

      IF (TYPE.EQ.0) THEN

       dt = max(-80.,t-273.16)
       polysvp = a0 + dt*(a1+dt*(a2+dt*(a3+dt*(a4+dt*(a5+dt*(a6+dt*(a7+a8*dt)))))))
       polysvp = polysvp*100.

!         POLYSVP = 10.**(-7.90298*(373.16/T-1.)+                        &
!             5.02808*LOG10(373.16/T)-									&
!             1.3816E-7*(10**(11.344*(1.-T/373.16))-1.)+				&
!             8.1328E-3*(10**(-3.49149*(373.16/T-1.))-1.)+				&
!             LOG10(1013.246))*100.

         END IF

 end function polysvp

!==============================================================================!
 real function qsat(temp,pres,wtype)

!-----------------------------------------------------------------------------
! Returns the saturation mixing ratio [kg kg-1], as a function of temperature
! pressure, with respect to liquid water [wtype=0] or ice [wtype=1], by calling
! function POLYSVP to obtain the saturation vapor pressure.

! 2013-08-06
!-----------------------------------------------------------------------------

  implicit none

 !Calling parameters:
  real, intent(in)    :: temp     !temperature [K]
  real, intent(in)    :: pres     !pressure    [Pa]
  integer, intent(in) :: wtype    !0=liquid water; 1=ice

 !Local variables:
  real :: tmp1

  tmp1 = polysvp(temp,wtype)       !esat [Pa], wrt liquid (Flatau formulation)
  qsat = 0.622*tmp1/(pres-tmp1)

  end function qsat

!==============================================================================!
 subroutine check_values(Qv,T,Qc,Qr,Qi,Qn,Qg,Qh,Nc,Nr,Ny,Nn,Ng,Nh,epsQ,epsN,     &
                         check_consistency,force_abort,source_ind)

!-----------------------------------------------------------------------------
! Checks current values of prognotic variables for reasonable values and
! stops and prints values if they are out of specified allowable ranges.
!
! 'trapAll = 1' means include trap for inconsistency in hydrometeor moments;
! otherwise, only trap for Q, T, and negative Qx, Nx.  This option is here
! to allow for Q<epsQ.and.N>epsN or Q>epsQ.and.N<epsN which can be produced
! at the leading edges due to sedimentation and whose values are accpetable
! since Dmax is imposed in SEDI (so one does not necessarily want to trap
! for inconsistency after sedimentation has been called).
!
! The value 'source_ind' indicates the approximate line number in 'my2_main'
! from where 'check_values' was called before it resulted in a trap.
!
! 2013-06-21
!-----------------------------------------------------------------------------

  implicit none

 !Calling parameters:
  real, dimension(:,:), intent(in) :: Qv,T,Qc,Qr,Qi,Qn,Qg,Qh,Nc,Nr,Ny,Nn,Ng,Nh
  real, intent(in)                 :: epsQ,epsN
  integer,              intent(in) :: source_ind
  logical,              intent(in) :: force_abort         !.TRUE. = forces abort if value violation is detected
  logical,              intent(in) :: check_consistency   !.TRUE. = check for sign consistency between Qx and Nx

 !Local variables:
  real, parameter :: T_low  = 173.
  real, parameter :: T_high = 323.
  real, parameter :: Q_high = 1.e-1
  real, parameter :: N_high = 1.e+20
  real            :: x_low
  integer         :: i,k,ni,nk
  logical         :: trap

  if (source_ind == 100) then
     x_low = -1.e+30          !for call at beginning of main
  else
     x_low = 0.
  endif

  ni = size(Qv,dim=1)
  nk = size(Qv,dim=2)

  trap = .false.
  do k = 1,nk
     do i = 1,ni

        if (.not.(T(i,k)>T_low .and. T(i,k)<T_high)) then
           print*,'** WARNING IN MICRO **'
           print*,'** src,i,k,T: ',source_ind,i,k,T(i,k)
           trap = .true.
        endif
        if (.not.(Qv(i,k)>=0. .and. Qv(i,k)<Q_high)) then
           print*,'** WARNING IN MICRO **'
           print*,'** src,i,k,Qv (HU): ',source_ind,i,k,Qv(i,k)
        endif
        !-- NAN check:
        if (.not.(Qc(i,k)>=x_low .and. Qc(i,k)<Q_high .and.                               &
                  Qr(i,k)>=x_low .and. Qr(i,k)<Q_high .and.                               &
                  Qi(i,k)>=x_low .and. Qi(i,k)<Q_high .and.                               &
                  Qn(i,k)>=x_low .and. Qn(i,k)<Q_high .and.                               &
                  Qg(i,k)>=x_low .and. Qg(i,k)<Q_high .and.                               &
                  Qh(i,k)>=x_low .and. Qh(i,k)<Q_high  )) then
           print*,'** WARNING IN MICRO **'
           print*, '** src,i,k,Qc,Qr,Qi,Qn,Qg,Qh: ',source_ind,i,k,Qc(i,k),Qr(i,k),       &
                   Qi(i,k),Qn(i,k),Qg(i,k),Qh(i,k)  
           trap = .true.
        endif
        if (.not.(Nc(i,k)>=x_low .and. Nc(i,k)<N_high .and.                               &
                  Nr(i,k)>=x_low .and. Nr(i,k)<N_high .and.                               &
                  Ny(i,k)>=x_low .and. Ny(i,k)<N_high .and.                               &
                  Nn(i,k)>=x_low .and. Nn(i,k)<N_high .and.                               &
                  Ng(i,k)>=x_low .and. Ng(i,k)<N_high .and.                               &
                  Nh(i,k)>=x_low .and. Nh(i,k)<N_high  )) then
           print*,'** WARNING IN MICRO **'
           print*, '** src,i,k,Nc,Nr,Ny,Nn,Ng,Nh: ',source_ind,i,k,Nc(i,k),Nr(i,k),       &
                  Ny(i,k),Nn(i,k),Ng(i,k),Nh(i,k)
           trap = .true.
        endif
        !==
        if (check_consistency) then
           if ((Qc(i,k)>epsQ.and.Nc(i,k)<epsN) .or. (Qc(i,k)<epsQ.and.Nc(i,k)>epsN)) then
              print*,'** WARNING IN MICRO **'
              print*, '** src,i,k,Qc,Nc: ',source_ind,i,k,Qc(i,k),Nc(i,k)
              trap = .true.
           endif
           if ((Qr(i,k)>epsQ.and.Nr(i,k)<epsN) .or. (Qr(i,k)<epsQ.and.Nr(i,k)>epsN)) then
              print*,'** WARNING IN MICRO **'
              print*, '** src,i,k,Qr,Nr: ',source_ind,i,k,Qr(i,k),Nr(i,k)
              trap = .true.
           endif
           if ((Qi(i,k)>epsQ.and.Ny(i,k)<epsN) .or. (Qi(i,k)<epsQ.and.Ny(i,k)>epsN)) then
              print*,'** WARNING IN MICRO **'
              print*, '** src,i,k,Qi,Ny: ',source_ind,i,k,Qi(i,k),Ny(i,k)
              trap = .true.
           endif
           if ((Qn(i,k)>epsQ.and.Nn(i,k)<epsN) .or. (Qn(i,k)<epsQ.and.Nn(i,k)>epsN)) then
              print*,'** WARNING IN MICRO **'
              print*, '** src,i,k,Qn,Nn: ',source_ind,i,k,Qn(i,k),Nn(i,k)
              trap = .true.
           endif
           if ((Qg(i,k)>epsQ.and.Ng(i,k)<epsN) .or. (Qg(i,k)<epsQ.and.Ng(i,k)>epsN)) then
              print*,'** WARNING IN MICRO **'
              print*, '** src,i,k,Qg,Ng: ',source_ind,i,k,Qg(i,k),Ng(i,k)
              trap = .true.
           endif
           if ((Qh(i,k)>epsQ.and.Nh(i,k)<epsN) .or. (Qh(i,k)<epsQ.and.Nh(i,k)>epsN)) then
              print*,'** WARNING IN MICRO **'
              print*, '** src,i,k,Qh,Nh: ',source_ind,i,k,Qh(i,k),Nh(i,k)
              trap = .true.
           endif
        endif !if (check_consistency)
     enddo
  enddo

  if (trap .and. force_abort) then
     print*,'** DEBUG TRAP IN MICRO, s/r CHECK_VALUES -- source: ',source_ind
     if (source_ind/=100) stop
  endif

 end subroutine check_values


!=====================================================================================!
  SUBROUTINE sedi_wrapper_2(QX,NX,cat,epsQ,epsQ_sedi,epsN,dmx,ni,VxMax,DxMax,dt,     &
                massFlux_bot,kdir,kbot,ktop_sedi,GRAV,zheight,nk,DE,iDE,iDP,         &
                DZ,iDZ,gamfact,kount,afx_in,bfx_in,cmx_in,ckQx1_in,ckQx2_in,ckQx4_in)

!-------------------------------------------------------------------------------------!
!  Wrapper for s/r SEDI, for computation on all vertical levels.  Called from MY2_MAIN.
!-------------------------------------------------------------------------------------!

  implicit none

! PASSING PARAMETERS:
  real, dimension(:,:), intent(inout) :: QX,NX
  real, dimension(:),   intent(out)   :: massFlux_bot
  real, dimension(:,:), intent(in)    :: zheight, DE,iDE,iDP,DZ,iDZ,gamfact
  real, intent(in)                    :: epsQ,epsQ_sedi,epsN,VxMax,dmx,DxMax,dt,GRAV
  real, intent(in), optional          :: afx_in,bfx_in,cmx_in,ckQx1_in,ckQx2_in,ckQx4_in
  integer, dimension(:), intent(in)   :: ktop_sedi
  integer, intent(in)                 :: ni,cat,kbot,kdir,nk,kount

! LOCAL VARIABLES:
  integer, dimension(size(QX,dim=1))  :: activeColumn,ktop
  integer                             :: counter
  integer                             :: a,i,k


   massFlux_bot = 0.

   ktop = ktop_sedi  !(i-array)  - for complete column, ktop(:)=1 (GEM) or =nk (WRF)
   call count_columns(QX,ni,epsQ_sedi,counter,activeColumn,kdir,kbot,ktop)

   DO a = 1,counter
      i= activeColumn(a)
     !From here, all sedi calcs are done for each column i

      call sedi_1D(QX(i,:),NX(i,:),cat,DE(i,:),iDE(i,:),iDP(i,:),gamfact(i,:),epsQ,epsN,  &
                   dmx,VxMax,DxMax,dt,DZ(i,:),iDZ(i,:),massFlux_bot(i),kdir,kbot,ktop(i), &
                   GRAV,afx_in=afx_in,bfx_in=bfx_in,cmx_in=cmx_in,ckQx1_in=ckQx1_in,      &
                   ckQx2_in=ckQx2_in,ckQx4_in=ckQx4_in)

   ENDDO  !a-loop

 END SUBROUTINE sedi_wrapper_2

!=====================================================================================!
SUBROUTINE sedi_1D(QX1d,NX1d,cat,DE1d,iDE1d,iDP1d,gamfact1d,epsQ,epsN,dmx,VxMax,DxMax,    &
                    dt,DZ1d,iDZ1d,massFlux_bot,kdir,kbot,ktop,GRAV,afx_in,bfx_in,cmx_in,  &
                    ckQx1_in,ckQx2_in,ckQx4_in,BX1d,epsB)

  implicit none

! PASSING PARAMETERS:
  real, dimension(:),  intent(inout), optional :: BX1d
  real, dimension(:),  intent(inout) :: QX1d,NX1d
  real, dimension(:),  intent(in)    :: gamfact1d
  real,                intent(out)   :: massFlux_bot
  real, dimension(:),  intent(in)    :: DE1d,iDE1d,iDP1d,DZ1d,iDZ1d
  real,                intent(in)    :: epsQ,epsN,VxMax,dmx,DxMax,dt,GRAV
  real, optional,      intent(in)    :: afx_in,bfx_in,cmx_in,ckQx1_in,ckQx2_in,          &
                                        ckQx4_in,epsB
  integer,             intent(in)    :: cat,kbot,kdir
  integer,             intent(in)    :: ktop

! LOCAL PARAMETERS:
  integer                            :: npassx
  real, dimension(size(QX1d,dim=1))  :: VVQ,VVN
  real                               :: dzMIN,dtx,VxMaxx
  logical                            :: firstPass,QxPresent,BX_present
  integer                            :: nnn,i,k,l,km1,kp1,idzmin,kk
  real                               :: VqMax,VnMax,iLAMx,iLAMxB0,tmp1,tmp2,tmp3,Dx,     &
                                        iDxMax,icmx,VincFact,ratio_Vn2Vq,zmax_Q,zmax_N,  &
                                        idmx
  real                               :: alpha_x,afx,bfx,cmx,ckQx1,ckQx2,ckQx4

  real, parameter :: thrd    = 1./3.
  real, parameter :: sxth    = 1./6.
! real, parameter :: CoMAX   = 0.5  !0.8
  real, parameter :: CoMAX   = 0.8
  real, parameter :: PIov6   = 3.14159265*sxth
!-------------------------------------------------------------------------------------!

   BX_present = present(BX1d)

  !for rain, ice, snow, hail:
! !    if (.not. (cat==4 .and. BX_present)) then
      afx   = afx_in
      bfx   = bfx_in
      cmx   = cmx_in
      icmx  = 1./cmx
      ckQx1 = ckQx1_in
      ckQx2 = ckQx2_in
      ckQx4 = ckQx4_in
      ratio_Vn2Vq  = ckQx2/ckQx1
! !    endif

   massFlux_bot = 0.
   iDxMax = 1./DxMax
   idmx   = 1./dmx
   VVQ    = 0.
   VVN    = 0.
   VqMax  = 0.
   VnMax  = 0.
   VVQ(:) = 0.

      do k= kbot,ktop,kdir
         QxPresent =  (QX1d(k)>epsQ .and. NX1d(k)>epsN)
         if (QxPresent) VVQ(k)= VV_Q()
      enddo

   Vxmaxx= min( VxMax, maxval(VVQ(:)))
   if (kdir==1) then
      dzMIN = minval(DZ1d)  !WRF (to be tested)
   else
      dzMIN = minval(DZ1d(ktop:kbot+kdir))  !GEM
   endif
   npassx= max(1, nint( dt*Vxmaxx/(CoMAX*dzMIN) ))


   dtx   = dt/float(npassx)

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -!
   DO nnn= 1,npassx

      firstPass = (nnn==1)
      do k= kbot,ktop,kdir
         QxPresent  = (QX1d(k)>epsQ .and. NX1d(k)>epsN)
         if (QxPresent) then
            if (firstPass) then     !to avoid re-computing VVQ on first pass
               VVQ(k)= -VVQ(k)
            else
               VVQ(k)= -VV_Q()
            endif
           !--
           !control excessive size-sorting for hail:
            if (cat==5) then
               tmp1 = (icmx*QX1d(k)/NX1d(k))**thrd   !Dmh
               tmp2 = min(50., 0.1*(1000.*tmp1))     !mu = const*Dmh [mm]
               ratio_Vn2Vq = ((3.+tmp2)*(2.+tmp2)*(1.+tmp2))/((3.+bfx+tmp2)*             &
                              (2.+bfx+tmp2)*(1.+bfx+tmp2))
            endif
           !==
            VVN(k) = VVQ(k)*ratio_Vn2Vq
            VqMax  = max(VxMAX,-VVQ(k))
            VnMax  = max(VxMAX,-VVN(k))
         else
            VVQ(k) = 0.
            VVN(k) = 0.
            VqMax  = 0.
            VnMax  = 0.
         endif
      enddo  !k-loop

      massFlux_bot= massFlux_bot - VVQ(kbot)*DE1d(kbot)*QX1d(kbot)
      do k= kbot,ktop,kdir
         QX1d(k)= QX1d(k) + dtx*iDE1d(k)*(-DE1d(k+kdir)*QX1d(k+kdir)*VVQ(k+kdir) +       &
                            DE1d(k)*QX1d(k)*VVQ(k))*iDZ1d(k+kdir)
         NX1d(k)= NX1d(k) + dtx*(-NX1d(k+kdir)*VVN(k+kdir) + NX1d(k)*VVN(k))*iDZ1d(k+kdir)
         QX1d(k) = max( QX1d(k), 0.)
         NX1d(k) = max( NX1d(k), 0.)
      enddo
     !
      if (BX_present) then
       do k= kbot,ktop,kdir
         BX1d(k)= BX1d(k) + dtx*iDE1d(k)*(-DE1d(k+kdir)*BX1d(k+kdir)*VVQ(k+kdir) +       &
                            DE1d(k)*BX1d(k)*VVQ(k))*iDZ1d(k+kdir)
         BX1d(k) = max( BX1d(k), 0.)
       enddo
      endif
     !--

      do k= kbot,ktop,kdir

        !Prescribe NX if QX>0.and.NX=0:
        if (QX1d(k)>epsQ .and. NX1d(k)<epsN) then
           !determine first level above with NX>epsN
           do kk = k+kdir,ktop,kdir
              !note: the following condition should normally be satisfied immediately;
              !      that is, the next level up should contain NX>epsN
              if (NX1d(kk)>=epsN) exit
           enddo
          !prescribe new NX:
          !  note: if no kk with NX>epsN found [i.e. if kk=ktop at this point] then
          !        epsN is prescribed; this will then be modified via size-limiter below
           NX1d(k) = max(epsN,NX1d(kk))
        endif

        !Impose size-limiter / drop-breakup:
        if (QX1d(k)>epsQ .and. NX1d(k)>epsN) then
           Dx= (DE1d(k)*QX1d(k)/(NX1d(k)*cmx))**idmx
           if (cat==1 .and. Dx>3.e-3) then
              NX1d(k)= NX1d(k)*max((1.+2.e4*(Dx-3.e-3)**2),(Dx*iDxMAX)**3)
           else
              NX1d(k)= NX1d(k)*(max(Dx,DxMAX)*iDxMAX)**dmx   !impose Dx_max
           endif
        endif

      enddo

   ENDDO  !nnn-loop
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -!
  !Compute average mass flux during the full time step: (used to compute the
  !instantaneous sedimentation rate [liq. equiv. volume flux] in the main s/r)
   massFlux_bot= massFlux_bot/float(npassx)

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -!

   CONTAINS

   real function VV_Q()
   !Calculates Q-weighted fall velocity
      iLAMx   = ((QX1d(k)*DE1d(k)/NX1d(k))*ckQx4)**idmx
      iLAMxB0 = iLAMx**bfx
      VV_Q    = gamfact1d(k)*iLAMxB0*ckQx1
   end function VV_Q

   real function VV_Qg()
   !Calculates Q-weighted fall velocity
!     iLAMx is already computed in 'calc_grpl_params' (for graupel only)
      iLAMxB0 = iLAMx**bfx
      VV_Qg   = gamfact1d(k)*iLAMxB0*ckQx1
   end function VV_Qg

 END SUBROUTINE sedi_1D

!=====================================================================================!
 SUBROUTINE count_columns(QX,ni,minQX,counter,activeColumn,kdir,kbot,ktop)

 !--------------------------------------------------------------------------
 ! Searches the hydrometeor array QX(ni,nk) for non-zero (>minQX) values.
 ! Returns the array if i-indices (activeColumn) for the column indices (i) which
 ! contain at least one non-zero value, the number of such columns (counter),
 ! and the k-indices of the maximum level to compute sedimentation.
 !--------------------------------------------------------------------------

  implicit none

!PASSING PARAMETERS:
  real,    dimension(:,:),intent(in)   :: QX            ! mixing ratio
  real,    intent(in)                  :: minQX         ! mixing ratio threshold
  integer, intent(in)                  :: ni            ! total number of columns (input)
  integer, intent(in)                  :: kbot          ! k index of lowest level
  integer, intent(in)                  :: kdir          ! -1 of k=1 is top (GEM); 1 if k=1 is bottom (WRF)
  integer, dimension(:),  intent(inout):: ktop          ! IN: array of highest level to look at; OUT: array of highest level with QX>epsQ
  integer,                intent(out)  :: counter       ! number of columns containing at least one QX>epsQ
  integer, dimension(:),  intent(out)  :: activeColumn  ! array of i-indices with columns containing at least one QX>epsQ

!LOCAL PARAMETERS:
  integer                              :: i
  integer, dimension(size(QX,dim=1))   :: k

   counter       = 0
   activeColumn  = 0

 !Note:  k_top(i) must be at least one level higher than the level with non-zero Qx

   do i=1,ni
      k(i)= ktop(i)
      do
         k(i)=k(i)-kdir               !step 1 level downward (towards lowest-level k)
         if (QX(i,k(i))>minQX) then
            counter=counter+1
            activeColumn(counter)=i
            ktop(i)= k(i)             !set ktop(k) to highest level with QX>minQX
            exit
         else
            if (k(i)==kbot) then
               ktop(i) = kbot
               exit
            endif
         endif
      enddo
   enddo

 END SUBROUTINE count_columns
!=======================================================================================!

!_______________________________________________________________________________________!

 SUBROUTINE mp_milbrandt2mom_main(WZ,T,Q,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH,PS,          &
     sigma,RT_rn1,RT_rn2,RT_fr1,RT_fr2,RT_sn1,RT_sn2,RT_sn3,RT_pe1,RT_pe2,RT_peL,RT_snd,  &
     dt,NI,NK,J,KOUNT,CCNtype,precipDiag_ON,sedi_ON,warmphase_ON,autoconv_ON,icephase_ON, &
     snow_ON,Dm_c,Dm_r,Dm_i,Dm_s,Dm_g,Dm_h,ZET,ZEC,SS,nk_bottom)

  implicit none

!CALLING PARAMETERS:
  integer,               intent(in)    :: NI,NK,J,KOUNT,CCNtype
  real,                  intent(in)    :: dt
  real, dimension(:),    intent(in)    :: PS
  real, dimension(:),    intent(out)   :: RT_rn1,RT_rn2,RT_fr1,RT_fr2,RT_sn1,RT_sn2,     &
                                          RT_sn3,RT_pe1,RT_pe2,RT_peL,ZEC,RT_snd
  real, dimension(:,:),  intent(in)    :: WZ,sigma
  real, dimension(:,:),  intent(inout) :: T,Q,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH
  real, dimension(:,:),  intent(out)   :: ZET,Dm_c,Dm_r,Dm_i,Dm_s,Dm_g,Dm_h
  real, dimension(:,:,:),intent(out)   :: SS
  logical,               intent(in)    :: precipDiag_ON,sedi_ON,icephase_ON,snow_ON,     &
                                          warmphase_ON,autoconv_ON,nk_BOTTOM

!_______________________________________________________________________________________
!                                                                                       !
!                    Milbrandt-Yau Multimoment Bulk Microphysics Scheme                 !
!                              - double-moment version   -                              !
!_______________________________________________________________________________________!
!
!  Author:
!       J. Milbrandt, McGill University (August 2004)
!
!  Major revisions:
!
!  001  J. Milbrandt  (Dec 2006) - Converted the full Milbrandt-Yau (2005) multimoment
!        (RPN)                     scheme to an efficient fixed-dispersion double-moment
!                                  version
!  002  J. Milbrandt  (Mar 2007) - Added options for single-moment/double-moment for
!                                  each hydrometeor category
!  003  J. Milbrandt  (Feb 2008) - Modified single-moment version for use in GEM-LAM-2.5
!  004  J. Milbrandt  (Nov 2008) - Modified double-moment version for use in 2010 Vancouver
!                                  Olympics GEM-LAM configuration
!  005  J. Milbrandt  (Aug 2009) - Modified (dmom) for PHY_v5.0.4, for use in V2010 system:
!                                  + reduced ice/snow capacitance to C=0.25D (from C=0.5D)
!                                  + added diagnostic fields (VIS, levels, etc.)
!                                  + added constraints to snow size distribution (No_s and
!                                    LAMDA_s limits, plus changed m-D parameters
!                                  + modified solid-to-liquid ratio calculation, based on
!                                    volume flux (and other changes)
!                                  + added back sedimentation of ice category
!                                  + modified condition for conversion of graupel to hail
!                                  + corrected bug it diagnostic "ice pellets" vs. "hail"
!                                  + minor bug corrections (uninitialized values, etc.)
!  006  J. Milbrandt  (Jan 2011) - Bug fixes and minor code clean-up from PHY_v5.1.3 version
!                                  + corrected latent heat constants in thermodynamic functions
!                                    (ABi and ABw) for sublimation and evaporation
!                                  + properly initialized variables No_g and No_h
!                                  + changed max ice crystal size (fallspeed) to 5 mm (2 m s-1)
!                                  + imposed maximum ice number concentration of 1.e+7 m-3
!                                  + removed unused supersaturation reduction
!
!  Object:
!          Computes changes to the temperature, water vapor mixing ratio, and the
!          mixing ratios and total number concentrations of six hydrometeor species
!          resulting from cloud microphysical interactions at saturated grid points.
!          Liquid and solid surface precipitation rates from sedimenting hydrometeor
!          categories are also computed.
!
!          This subroutine and the associated modules form the single/double-moment
!          switchable verion of the multimoment bulk microphysics package, the full
!          version of which is described in the references below.
!
!  References:  Milbrandt and Yau, (2005a), J. Atmos. Sci., vol.62, 3051-3064
!               --------- and ---, (2005b), J. Atmos. Sci., vol.62, 3065-3081
!               (and references therein)
!
!  Please report bugs to:  jason.milbrandt@ec.gc.ca
!_______________________________________________________________________________________!
!
! Arguments:         Description:                                         Units:
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -!
!            - Input -
!
! NI                 number of x-dir points (in local subdomain)
! NK                 number of vertical levels
! N                  not used (to be removed)
! J                  y-dir index (local subdomain)
! KOUNT              current model time step number
! dt                 model time step                                      [s]
! CCNtype            switch for airmass type
!                      1 = maritime                   --> N_c =  80 cm-3  (1-moment cloud)
!                      2 = continental 1              --> N_c = 200 cm-3     "       "
!                      3 = continental 2  (polluted)  --> N_c = 500 cm-3     "       "
!                      4 = land-sea-mask-dependent (TBA)
! WZ                 vertical velocity                                    [m s-1]
! sigma              sigma = p/p_sfc
! precipDiag_ON      logical switch, .F. to suppress calc. of sfc precip types
! sedi_ON            logical switch, .F. to suppress sedimentation
! warmphase_ON       logical switch, .F. to suppress warm-phase (Part II)
! autoconv_ON        logical switch, .F. to supppress autoconversion (cld->rn)
! icephase_ON        logical switch, .F. to suppress ice-phase (Part I)
! snow_ON            logical switch, .F. to suppress snow initiation
! nk_BOTTOM          logical switch, .T. for  nk at bottom (GEM, 1dkin); .F. for nk at top (WRF, 2dkin)
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -!
!            - Input/Output -
!
! T                  air temperature at time (t*)                         [K]
! Q                  water vapor mixing ratio at (t*)                     [kg kg-1]
! PS                 surface pressure at time (t*)                        [Pa]
!
!  For x = (C,R,I,N,G,H):  C = cloud
!                          R = rain
!                          I = ice (pristine) [except 'NY', not 'NI']
!                          N = snow
!                          G = graupel
!                          H = hail
!
! Q(x)               mixing ratio for hydrometeor x at (t*)               [kg kg-1]
! N(x)               total number concentration for hydrometeor x  (t*)   [m-3]
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -!
!            - Output -
!
! Dm_(x)             mean-mass diameter for hydrometeor x                 [m]
! RT_rn1             precipitation rate (at sfc) of liquid rain           [m+3 m-2 s-1]
! RT_rn2             precipitation rate (at sfc) of liquid drizzle        [m+3 m-2 s-1]
! RT_fr1             precipitation rate (at sfc) of freezing rain         [m+3 m-2 s-1]
! RT_fr2             precipitation rate (at sfc) of freezing drizzle      [m+3 m-2 s-1]
! RT_sn1             precipitation rate (at sfc) of ice crystals (liq-eq) [m+3 m-2 s-1]
! RT_sn2             precipitation rate (at sfc) of snow    (liq-equiv)   [m+3 m-2 s-1]
! RT_sn3             precipitation rate (at sfc) of graupel (liq-equiv)   [m+3 m-2 s-1]
! RT_snd             precipitation rate (at sfc) of snow    (frozen)      [m+3 m-2 s-1]
! RT_pe1             precipitation rate (at sfc) of ice pellets (liq-eq)  [m+3 m-2 s-1]
! RT_pe2             precipitation rate (at sfc) of hail (total; liq-eq)  [m+3 m-2 s-1]
! RT_peL             precipitation rate (at sfc) of hail (large only)     [m+3 m-2 s-1]
! SS(i,k,n)          array (n) for 3D diagnostic output (e.g. S/S term)
! ZET                total equivalent radar reflectivity                  [dBZ]
! ZEC                composite (column-max) of ZET                        [dBZ]
!_______________________________________________________________________________________!


!LOCAL VARIABLES:

 !Variables to count active grid points:
  logical :: log1,log2,log3,log4,doneK,rainPresent,calcDiag
  logical, dimension(size(QC,dim=1),size(QC,dim=2)) :: activePoint
  integer, dimension(size(QC,dim=1)) :: ktop_sedi
  integer :: i,k,niter,ll,start,kskip_1,ktop,kbot,kdir

  real    :: tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10,                    &
       VDmax,NNUmax,X,D,DEL,QREVP,NuDEPSOR,NuCONTA,NuCONTB,NuCONTC,iMUkin,Ecg,Erg,  &
       NuCONT,GG,Na,Tcc,F1,F2,Kdiff,PSIa,Kn,source,sink,sour,ratio,qvs0,Kstoke,     &
       DELqvs,ft,esi,Si,Simax,Vq,Vn,Vz,LAMr,No_r_DM,No_i,No_s,No_g,No_h,D_sll,      &
       iABi,ABw,VENTr,VENTs,VENTg,VENTi,VENTh,Cdiff,Ka,MUdyn,MUkin,Ng_tail,         &
       gam,ScTHRD,Tc,mi,ff,Ec,Ntr,Dho,DMrain,Ech,DMice,DMsnow,DMgrpl,DMhail,        &
       ssat,Swmax,dey,Esh,Eii,Eis,Ess,Eig,Eih,FRAC,JJ,Dirg,Dirh,Dsrs,Dsrg,Dsrh,     &
       Dgrg,Dgrh,SIGc,L,TAU,DrAUT,DrINIT,Di,Ds,Dg,Dh,qFact,nFact,Ki,Rz,NgCNgh,      &
       vr0,vi0,vs0,vg0,vh0,Dc,Dr,QCLcs,QCLrs,QCLis,QCLcg,QCLrg,QCLig,NhCNgh,        &
       QCLch,QCLrh,QCLsh,QMLir,QMLsr,QMLgr,QMLhr,QCLih,QVDvg,QVDvh,QSHhr,           &
       QFZci,QNUvi,QVDvi,QCNis,QCNis1,QCNis2,QCLir,QCLri,QCNsg,QCLsr,QCNgh,         &
       QCLgr,QHwet,QVDvs,QFZrh,QIMsi,QIMgi,NMLhr,NVDvh,NCLir,NCLri,NCLrh,           &
       NCLch,NCLsr,NCLirg,NCLirh,NrFZrh,NhFZrh,NCLsrs,NCLsrg,NCLsrh,NCLgrg,         &
       NCLgrh,NVDvg,NMLgr,NiCNis,NsCNis,NVDvs,NMLsr,NCLsh,NCLss,NNUvi,NFZci,NVDvi,  &
       NCLis,NCLig,NCLih,NMLir,NCLrs,NCNsg,NCLcs,NCLcg,NIMsi,NIMgi,NCLgr,NCLrg,     &
       NSHhr,RCAUTR,RCACCR,CCACCR,CCSCOC,CCAUTR,CRSCOR,ALFx,des_pmlt,Ecs,des,ides,  &
       LAMx,iLAMx,iLAMxB0,Dx,ffx,iLAMc,iNC,iNR,iNY,iNN,iNG,iLAMs_D3,                &
       iLAMg,iLAMg2,iLAMgB0,iLAMgB1,iLAMgB2,iLAMh,iLAMhB0,iLAMhB1,iLAMhB2,iNH,      &
       iLAMi,iLAMi2,iLAMi3,iLAMi4,iLAMi5,iLAMiB0,iLAMiB1,iLAMiB2,iLAMr6,iLAMh2,     &
       iLAMs,iLAMs2,iLAMsB0,iLAMsB1,iLAMsB2,iLAMr,iLAMr2,iLAMr3,iLAMr4,iLAMr5,      &
       iLAMc2,iLAMc3,iLAMc4,iLAMc5,iLAMc6,iQC,iQR,iQI,iQN,iQG,iQH,iEih,iEsh,        &
       N_c,N_r,N_i,N_s,N_g,N_h,fluxV_i,fluxV_g,fluxV_s,rhos_mlt,fracLiq

 !Variables that only need to be calulated on the first step (and saved):
  real, save :: idt,iMUc,cmr,cmi,cms,cmg,cmh,icmr,icmi,icmg,icms,icmh,idew,idei,    &
       ideh,ideg,GC1,imso,icexc9,cexr1,cexr2,cexr3,No_s_SM,No_r,idms,imgo,icexs2,   &
       cexr4,cexr5,cexr6,cexr9,icexr9,ckQr1,ckQr2,ckQr3,ckQi1,ckQi2,ckQi3,ckQi4,    &
       icexi9,ckQs1,ckQs2,cexs1,cexs2,ckQg1,ckQg2,ckQg4,ckQh1,ckQh2,ckQh4,GR37,dms, &
       LCP,LFP,LSP,ck5,ck6,PI2,PIov4,PIov6,CHLS,iCHLF,cxr,cxi,Gzr,Gzi,Gzs,Gzg,Gzh,  &
       N_c_SM,iGC1,GC2,GC3,GC4,GC5,iGC5,GC6,GC7,GC8,GC11,GC12,GC13,GC14,iGR34,mso,  &
       GC15,GR1,GR3,GR13,GR14,GR15,GR17,GR31,iGR31,GR32,GR33,GR34,GR35,GR36,GI4,    &
       GI6,GI20,GI21,GI22,GI31,GI32,GI33,GI34,GI35,iGI31,GI11,GI36,GI37,GI40,iGG34, &
       GS09,GS11,GS12,GS13,iGS20,GS31,iGS31,GS32,GS33,GS34,GS35,GS36,GS40,iGS40,    &
       GS50,GG09,GG11,GG12,GG13,GG31,iGG31,GG32,GG33,GG34,GG35,GG36,GG40,iGG99,GH09,&
       GH11,GH12,GH13,GH31,GH32,GH33,GH40,GR50,GG50,iGH34,GH50,iGH99,iGH31,iGS34,   &
       iGS20_D3,GS40_D3,cms_D3,eds,fds,rfact_FvFm

!Size distribution parameters:
  real, parameter :: MUc      =  3.    !shape parameter for cloud
  real, parameter :: alpha_c  =  1.    !shape parameter for cloud
  real, parameter :: alpha_r  =  0.    !shape parameter for rain
  real, parameter :: alpha_i  =  0.    !shape parameter for ice
  real, parameter :: alpha_s  =  0.    !shape parameter for snow
  real, parameter :: alpha_g  =  0.    !shape parameter for graupel
  real, parameter :: alpha_h  =  0.    !shape parameter for hail
  real, parameter :: No_s_max =  1.e+8 !max. allowable intercept for snow [m-4]
  real, parameter :: lamdas_min= 500.  !min. allowable LAMDA_s [m-1]

 !For single-moment:
  real, parameter :: No_r_SM  =  1.e+7  !intercept parameter for rain    [m-4]
  real, parameter :: No_g_SM  =  4.e+6  !intercept parameter for graupel [m-4]
  real, parameter :: No_h_SM  =  1.e+5  !intercept parameter for hail    [m-4]
  !note: No_s = f(T), rather than a fixed value
  !------------------------------------!
  ! Symbol convention: (dist. params.) ! MY05: Milbrandt & Yau, 2005a,b (JAS)
  !       MY05    F94       CP00       ! F94:  Ferrier, 1994            (JAS)
  !       ------  --------  ------     ! CP00: Cohard & Pinty, 2000a,b  (QJGR)
  !       ALFx    ALPHAx    MUx-1      !
  !       MUx     (1)       ALPHAx     !
  !       ALFx+1  ALPHAx+1  MUx        !
  !------------------------------------!
  !  Note: The symbols for MU and ALPHA are REVERSED from that of CP2000a,b
  !        Explicit appearance of MUr = 1. has been removed.

  ! Fallspeed parameters:
  real, parameter :: afr=  149.100,  bfr= 0.5000   !Tripoloi and Cotton (1980)
  real, parameter :: afi=   71.340,  bfi= 0.6635   !Ferrier (1994)
  real, parameter :: afs=   11.720,  bfs= 0.4100   !Locatelli and Hobbs (1974)
  real, parameter :: afg=   19.300,  bfg= 0.3700   !Ferrier (1994)
  real, parameter :: afh=  206.890,  bfh= 0.6384   !Ferrier (1994)
 !options:
 !real, parameter :: afs=    8.996,  bfs= 0.4200   !Ferrier (1994)
 !real, parameter :: afg=   6.4800,  bfg= 0.2400   !LH74 (grpl-like snow of lump type)

  real, parameter :: epsQ  = 1.e-14   !kg kg-1, min. allowable mixing ratio
  real, parameter :: epsN  = 1.e-3    !m-3,     min. allowable number concentration
  real, parameter :: epsQ2 = 1.e-6    !kg kg-1, mixing ratio threshold for diagnostics
  real, parameter :: epsVIS= 1.       !m,       min. allowable visibility

  real, parameter :: iLAMmin1= 1.e-6  !min. iLAMx (prevents underflow in Nox and VENTx calcs)
  real, parameter :: iLAMmin2= 1.e-10 !min. iLAMx (prevents underflow in Nox and VENTx calcs)
  real, parameter :: eps   = 1.e-32

  real, parameter :: deg   =  400., mgo= 1.6e-10
  real, parameter :: deh   =  900.
  real, parameter :: dei   =  500., mio=1.e-12, Nti0=1.e3
  real, parameter :: dew   = 1000.
  real, parameter :: desFix=  100.  !used for snowSpherical = .true.
  real, parameter :: desMax=  500.
  real, parameter :: Dso   =  125.e-6  ![m]; embryo snow diameter (mean-volume particle)
  real, parameter :: dmr   = 3., dmi= 3., dmg= 3., dmh= 3.

  ! NOTE: VxMAX below are the max.allowable mass-weighted fallspeeds for sedimentation.
  !       Thus, Vx corresponds to DxMAX (at sea-level) times the max. density factor, GAM.
  !       [GAMmax=sqrt(DEo/DEmin)=sqrt(1.25/0.4)~2.]  e.g. VrMAX = 2.*8.m/s = 16.m/s
  real, parameter :: DrMax=  5.e-3,   VrMax= 16.,   epsQr_sedi= 1.e-8
  real, parameter :: DiMax=  5.e-3,   ViMax=  2.,   epsQi_sedi= 1.e-10
  real, parameter :: DsMax=  5.e-3,   VsMax=  4.,   epsQs_sedi= 1.e-8
  real, parameter :: DgMax=  2.e-3,   VgMax=  6.,   epsQg_sedi= 1.e-8
  real, parameter :: DhMax= 80.e-3,   VhMax= 25.,   epsQh_sedi= 1.e-10

  real, parameter :: CPW     = 4218.              ![J kg-1 K-1] specific heat capacity of water
  real, parameter :: DEo     = 1.225              ![kg m-3] reference air density
  real, parameter :: thrd    = 1./3.
  real, parameter :: sixth   = 0.5*thrd
  real, parameter :: Ers     = 1., Eci= 1.        !collection efficiencies, Exy, between categories x and y
  real, parameter :: Eri     = 1., Erh= 1.
  real, parameter :: Xdisp   = 0.25               !dispersion of the fall velocity of ice
  real, parameter :: aa11    = 9.44e15, aa22= 5.78e3, Rh= 41.e-6
  real, parameter :: Avx     = 0.78, Bvx= 0.30    !ventilation coefficients [F94 (B.36)]
  real, parameter :: Abigg   = 0.66, Bbigg= 100.  !parameters in probabilistic freezing
  real, parameter :: fdielec     = 4.464          !ratio of dielectric factor, |K|w**2/|K|i**2
  real, parameter :: zfact       = 1.e+18         !conversion factor for m-3 to mm2 m-6 for Ze
  real, parameter :: minZET      = -99.           ![dBZ] min threshold for ZET
  real, parameter :: maxVIS      = 99.e+3         ![m] max. allowable VIS (visibility)
  real, parameter :: Drshed      = 0.001          ![m] mean diam. of drop shed during wet growth
  real, parameter :: SIGcTHRS    = 15.e-6         !threshold cld std.dev. before autoconversion
  real, parameter :: KK1         = 3.03e3         !parameter in Long (1974) kernel
  real, parameter :: KK2         = 2.59e15        !parameter in Long (1974) kernel
  real, parameter :: Dhh         = 82.e-6         ![m] diameter that rain hump first appears
  real, parameter :: zMax_sedi   = 20000.         ![m] maximum height to compute sedimentation
  real, parameter :: Dr_large    = 200.e-6        ![m] size threshold to distinguish rain/drizzle for precip rates
  real, parameter :: Ds_large    = 200.e-6        ![m] size threshold to distinguish snow/snow-grains for precip rates
  real, parameter :: Dh_large    = 1.0e-2         ![m] size threshold for "large" hail precipitation rate
  real, parameter :: Dh_min      = 1.0e-3         ![m] size threhsold for below which hail converts to graupel
  real, parameter :: Dr_3cmpThrs = 2.5e-3         ![m] size threshold for hail production from 3-comp freezing
  real, parameter :: w_CNgh      = 3.             ![m s-1] vertical motion  threshold for CNgh
  real, parameter :: Ngh_crit    = 1.e+0          ![m-3] critical graupel concentration for CNgh
  real, parameter :: Tc_FZrh     = -10.           !temp-threshold (C) for FZrh
  real, parameter :: CNsgThres   = 1.0            !threshold for CLcs/VDvs ratio for CNsg
  real, parameter :: capFact_i   = 0.5            !capacitace factor for ice  (C= 0.5*D*capFact_i)
  real, parameter :: capFact_s   = 0.5            !capacitace factor for snow (C= 0.5*D*capFact_s)
  real, parameter :: Fv_Dsmin    = 125.e-6        ![m] min snow size to compute volume flux
  real, parameter :: Fv_Dsmax    = 0.008          ![m] max snow size to compute volume flux
  real, parameter :: Ni_max      = 1.e+7          ![m-3] max ice crystal concentration
  real, parameter :: satw_peak   = 1.01           !assumed max. peak saturation w.r.t. water (for calc of Simax)

!------------------------------------------------------------------------------!
!-- For GEM:
!#include "consphy.cdk"

!-- For WRF + kin_1d + kin_2d:
  real, parameter :: CPI      =.21153e+4          !J kg-1 K-1; specific heat capacity of ice
  real, parameter :: TRPL     =.27316e+3          !K; triple point of water
  real, parameter :: TCDK     =.27315e+3          !conversion from kelvin to celsius
  real, parameter :: RAUW     =.1e+4              !density of liquid H2O
  real, parameter :: EPS2     =.3780199778986     !1 - EPS1
  real, parameter :: TGL      =.27316e+3          !K; ice temperature in the atmosphere
  real, parameter :: CONSOL   =.1367e+4           !W m-2; solar constant
  real, parameter :: RAYT     =.637122e+7         !M; mean radius of the earth
  real, parameter :: STEFAN   =.566948e-7         !J m-2 s-1 K-4; Stefan-Boltzmann constant
  real, parameter :: PI       =.314159265359e+1   !PI constant = ACOS(-1)
  real, parameter :: OMEGA    =.7292e-4           !s-1; angular speed of rotation of the earth
  real, parameter :: KNAMS    =.514791            !conversion from knots to m/s
  real, parameter :: STLO     =.6628486583943e-3  !K s2 m-2; Schuman-Newell Lapse Rate
  real, parameter :: KARMAN   =.35                !Von Karman constant
  real, parameter :: RIC      =.2                 !Critical Richardson number
!-- For kin_1d + kin_2d (exclude from WRF):
  real, parameter :: CHLC     =.2501e+7           !J kg-1; latent heat of condensation
  real, parameter :: CHLF     =.334e+6            !J kg-1; latent heat of fusion
  real, parameter :: CPD      =.100546e+4         !J K-1 kg-1; specific heat of dry air
  real, parameter :: CPV      =.186946e+4         !J K-1 kg-1; specific heat of water vapour
  real, parameter :: RGASD    =.28705e+3          !J K-1 kg-1; gas constant for dry air
  real, parameter :: RGASV    =.46151e+3          !J K-1 kg-1; gas constant for water vapour
  real, parameter :: EPS1     =.62194800221014    !RGASD/RGASV
  real, parameter :: DELTA    =.6077686814144     !1/EPS1 - 1
  real, parameter :: CAPPA    =.28549121795       !RGASD/CPD
  real, parameter :: GRAV     =.980616e+1         !M s-2; gravitational acceleration
!==
!------------------------------------------------------------------------------!

  ! Constants used for contact ice nucleation:
  real, parameter :: LAMa0  = 6.6e-8     ![m] mean free path at T0 and p0 [W95_eqn58]
  real, parameter :: T0     = 293.15     ![K] ref. temp.
  real, parameter :: p0     = 101325.    ![Pa] ref. pres.
  real, parameter :: Ra     = 1.e-6      ![m] aerosol (IN) radius         [M92 p.713; W95_eqn60]
  real, parameter :: kBoltz = 1.381e-23  !Boltzmann's constant
  real, parameter :: KAPa   = 5.39e5     !aerosol thermal conductivity

 !Test switches:
  logical, parameter :: DEBUG_ON      = .false.  !.true. to switch on debugging checks/traps throughout code
  logical, parameter :: DEBUG_abort   = .true.  !.true. will result in forced abort in s/r 'check_values'
  logical, parameter :: iceDep_ON     = .true.  !.false. to suppress depositional growth of ice
  logical, parameter :: grpl_ON       = .true.  !.false. to suppress graupel initiation
  logical, parameter :: hail_ON       = .true.  !.false. to suppress hail initiation
  logical, parameter :: rainAccr_ON   = .true.  ! rain accretion and self-collection ON/OFF
  logical, parameter :: snowSpherical = .false. !.true.: m(D)=(pi/6)*const_des*D^3 | .false.: m(D)= 0.069*D^2
  integer, parameter :: primIceNucl   = 1       !1= Meyers+contact ;  2= Cooper
  real,    parameter :: outfreq       =  60.    !frequency to compute output diagnostics [s]

  real, dimension(size(QC,dim=1),size(QC,dim=2)) :: DE,iDE,iDP,QSW,QSI,DZ,iDZ,zz,VQQ,    &
        gamfact,pres,zheight,QC_in,QR_in,NC_in,NR_in
  real, dimension(size(QC,dim=1))                :: fluxM_r,fluxM_i,fluxM_s,fluxM_g,     &
        fluxM_h,dum
  integer, dimension(size(QC,dim=1))             :: activeColumn

 !-- for use with sedimentation on subset of levels:
 !integer                                        :: k_sub,nk_sub,nk_skip
 !integer                                        :: status  !for allocate/deallocate statements (0 for success)
 !integer, allocatable, dimension(:)             :: kfull,kskip
 !integer, allocatable, dimension(:,:)           :: iint
 !real, dimension(:,:), allocatable              :: DE_sub,iDE_sub,iDP_sub,pres_sub,     &
 !==                                                DZ_sub,zheight_sub,iDZ_sub,gamfact_sub


  !==================================================================================!

  !----------------------------------------------------------------------------------!
  !                      PART 1:   Prelimiary Calculations                           !
  !----------------------------------------------------------------------------------!

  !Switch on here later, once it is certain that calling routine is not supposed to
  !pass negative values of tracers.
  if (DEBUG_ON) call check_values(Q,T,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH,epsQ,epsN,.false.,DEBUG_abort,100)

  if (nk_BOTTOM) then
!    !GEM / kin_1d:
     ktop  = 1          !k of top level
     kbot  = nk         !k of bottom level
     kdir  = -1         !direction of vertical leveling (k: 1=top, nk=bottom)
  else
   !WRF / kin_2d: (assuming no array flipping in wrapper)
     ktop  = nk         !k of top level
     kbot  = 1          !k of bottom level
     kdir  = 1          !direction of vertical leveling (k: 1=bottom, nk=top)
  endif

  do k= kbot,ktop,kdir
     pres(:,k)= PS(:)*sigma(:,k)               !air pressure [Pa]
     do i=1,ni
        QSW(i,k) = qsat(T(i,k),pres(i,k),0)    !wrt. liquid water
        QSI(i,k) = qsat(T(i,k),pres(i,k),1)    !wrt. ice
     enddo
  enddo

 !Air density:
  DE  = pres/(RGASD*T)
  iDE = 1./DE

 !Convert N from #/kg to #/m3:
  NC = NC*DE
  NR = NR*DE
  NY = NY*DE
  NN = NN*DE
  NG = NG*DE
  NH = NH*DE

  ! The SS(i,k,n) array is passed to 'vkuocon6' where it is converted into individual
  ! arrays [a_ss01(i,k)] and then passed to the volatile bus for output as 3-D diagnostic
  ! output variables, for testing purposes.  For example, to output the
  ! instantanous value of the deposition rate, add 'SS(i,k,1) = QVDvi'  in the
  ! appropriate place.  It can then be output as a 3-D physics variable by adding
  ! SS01 to the sortie_p list in 'outcfgs.out'
  SS= 0.

 !Compute diagnostic values only every 'outfreq' minutes:
 !calcDiag= (mod(DT*float(KOUNT),outfreq)==0.)
  calcDiag = .true.  !compute diagnostics every step (for time-series output)

!####  These need only to be computed once per model integration:
!      (note:  These variables must be declared with the SAVE attribute)

! if (KOUNT==0) then
!*** For restarts, these values are not saved.  Therefore, the condition statement
!    must be modified to something like: IF (MOD(Step_rsti,KOUNT).eq.0) THEN
!    in order that these be computed only on the first step of a given restart.
!    (...to be done.  For now, changing condition to IF(TRUE) to compute at each step.)


  if (.TRUE.) then

   PI2    = PI*2.
   PIov4  = 0.25*PI
   PIov6  = PI*sixth
   CHLS   = CHLC+CHLF  !J k-1; latent heat of sublimation
   LCP    = CHLC/CPD
   LFP    = CHLF/CPD
   iCHLF  = 1./CHLF
   LSP    = LCP+LFP
   ck5    = 4098.170*LCP
   ck6    = 5806.485*LSP
   idt    = 1./dt
   imgo   = 1./mgo
   idew   = 1./dew
   idei   = 1./dei
   ideg   = 1./deg
   ideh   = 1./deh

   !Constants based on size distribution parameters:

   ! Mass parameters [ m(D) = cD^d ]
   cmr    = PIov6*dew;  icmr= 1./cmr
   cmi    = 440.;       icmi= 1./cmi
   cmg    = PIov6*deg;  icmg= 1./cmg
   cmh    = PIov6*deh;  icmh= 1./cmh

   cms_D3 = PIov6*desFix !used for snowSpherical = .T. or .F.
   if (snowSpherical) then
      cms = cms_D3
      dms = 3.
   else
!     cms = 0.0690;  dms = 2.000   !Cox, 1988 (QJRMS)
      cms = 0.1597;  dms = 2.078   !Brandes et al., 2007 (JAMC)
   endif
   icms   = 1./cms
   idms   = 1./dms
   mso    = cms*Dso**dms
   imso   = 1./mso
  !bulk density parameters: [rho(D) = eds*D^fds]
  !  These are implied by the mass-diameter parameters, by computing the bulk
  !  density of a sphere with the equaivalent mass.
  !  e.g. m(D) = cD^d = (pi/6)rhoD^3 and solve for rho(D)
   eds    = cms/PIov6
   fds    = dms-3.
   if (fds/=-1. .and..not.snowSpherical) GS50= gamma(1.+fds+alpha_s)

   ! Cloud:
   iMUc   =  1./MUc
   GC1    =  gamma(alpha_c+1.0)
   iGC1   = 1./GC1
   GC2    =  gamma(alpha_c+1.+3.0*iMUc)  !i.e. gamma(alf + 4)
   GC3    =  gamma(alpha_c+1.+6.0*iMUc)  !i.e. gamma(alf + 7)
   GC4    =  gamma(alpha_c+1.+9.0*iMUc)  !i.e. gamma(alf + 10)
   GC11   =  gamma(1.0*iMUc+1.0+alpha_c)
   GC12   =  gamma(2.0*iMUc+1.0+alpha_c)
   GC5    =  gamma(1.0+alpha_c)
   iGC5   = 1./GC5
   GC6    =  gamma(1.0+alpha_c+1.0*iMUc)
   GC7    =  gamma(1.0+alpha_c+2.0*iMUc)
   GC8    =  gamma(1.0+alpha_c+3.0*iMUc)
   GC13   =  gamma(3.0*iMUc+1.0+alpha_c)
   GC14   =  gamma(4.0*iMUc+1.0+alpha_c)
   GC15   =  gamma(5.0*iMUc+1.0+alpha_c)
   icexc9 =  1./(GC2*iGC1*PIov6*dew)
  !specify cloud droplet number concentration [m-3] based on 'CCNtype' (1-moment):
   if     (CCNtype==1) then
      N_c_SM =  0.8e+8          !maritime
   elseif (CCNtype==2) then
      N_c_SM =  2.0e+8          !continental 1
   elseif (CCNtype==3) then
      N_c_SM =  5.0e+8          !continental 2 (polluted)
   else
      N_c_SM =  2.0e+8          !default (cont1), if 'CCNtype' specified incorrectly
   endif

   ! Rain:
   cexr1  = 1.+alpha_r+dmr+bfr
   cexr2  = 1.+alpha_r+dmr
   GR17   = gamma(2.5+alpha_r+0.5*bfr)
   GR31   = gamma(1.+alpha_r)
   iGR31  = 1./GR31
   GR32   = gamma(2.+alpha_r)
   GR33   = gamma(3.+alpha_r)
   GR34   = gamma(4.+alpha_r)
   iGR34  = 1./GR34
   GR35   = gamma(5.+alpha_r)
   GR36   = gamma(6.+alpha_r)
   GR37   = gamma(7.+alpha_r)
   GR50   = (No_r_SM*GR31)**0.75  !for 1-moment or Nr-initialization
   cexr5  = 2.+alpha_r
   cexr6  = 2.5+alpha_r+0.5*bfr
   cexr9  = cmr*GR34*iGR31;    icexr9= 1./cexr9
   cexr3  = 1.+bfr+alpha_r
   cexr4  = 1.+alpha_r
   ckQr1  = afr*gamma(1.+alpha_r+dmr+bfr)/gamma(1.+alpha_r+dmr)
   ckQr2  = afr*gamma(1.+alpha_r+bfr)*GR31
   ckQr3  = afr*gamma(7.+alpha_r+bfr)/GR37

   ! Ice:
   GI4    = gamma(alpha_i+dmi+bfi)
   GI6    = gamma(2.5+bfi*0.5+alpha_i)
   GI11   = gamma(1.+bfi+alpha_i)
   GI20   = gamma(0.+bfi+1.+alpha_i)
   GI21   = gamma(1.+bfi+1.+alpha_i)
   GI22   = gamma(2.+bfi+1.+alpha_i)
   GI31   = gamma(1.+alpha_i)
   iGI31  = 1./GI31
   GI32   = gamma(2.+alpha_i)
   GI33   = gamma(3.+alpha_i)
   GI34   = gamma(4.+alpha_i)
   GI35   = gamma(5.+alpha_i)
   GI36   = gamma(6.+alpha_i)
   GI40   = gamma(1.+alpha_i+dmi)
   icexi9 = 1./(cmi*gamma(1.+alpha_i+dmi)*iGI31)
   ckQi1  = afi*gamma(1.+alpha_i+dmi+bfi)/GI40
   ckQi2  = afi*GI11*iGI31
   ckQi4  = 1./(cmi*GI40*iGI31)

   ! Snow:
   cexs1  = 2.5+0.5*bfs+alpha_s
   cexs2  = 1.+alpha_s+dms
   icexs2 = 1./cexs2
   GS09   = gamma(2.5+bfs*0.5+alpha_s)
   GS11   = gamma(1.+bfs+alpha_s)
   GS12   = gamma(2.+bfs+alpha_s)
   GS13   = gamma(3.+bfs+alpha_s)
   GS31   = gamma(1.+alpha_s)
   iGS31  = 1./GS31
   GS32   = gamma(2.+alpha_s)
   GS33   = gamma(3.+alpha_s)
   GS34   = gamma(4.+alpha_s)
   iGS34  = 1./GS34
   GS35   = gamma(5.+alpha_s)
   GS36   = gamma(6.+alpha_s)
   GS40   = gamma(1.+alpha_s+dms)
   iGS40  = 1./GS40
   iGS20  = 1./(GS40*iGS31*cms)
   ckQs1  = afs*gamma(1.+alpha_s+dms+bfs)*iGS40
   ckQs2  = afs*GS11*iGS31
   GS40_D3 = gamma(1.+alpha_s+3.)
   iGS20_D3= 1./(GS40_D3*iGS31*cms_D3)
   rfact_FvFm= PIov6*icms*gamma(4.+bfs+alpha_s)/gamma(1.+dms+bfs+alpha_s)

   ! Graupel:
   GG09   = gamma(2.5+0.5*bfg+alpha_g)
   GG11   = gamma(1.+bfg+alpha_g)
   GG12   = gamma(2.+bfg+alpha_g)
   GG13   = gamma(3.+bfg+alpha_g)
   GG31   = gamma(1.+alpha_g)
   iGG31  = 1./GG31
   GG32   = gamma(2.+alpha_g)
   GG33   = gamma(3.+alpha_g)
   GG34   = gamma(4.+alpha_g)
   iGG34  = 1./GG34
   GG35   = gamma(5.+alpha_g)
   GG36   = gamma(6.+alpha_g)
   GG40   = gamma(1.+alpha_g+dmg)
   iGG99  = 1./(GG40*iGG31*cmg)
   GG50   = (No_g_SM*GG31)**0.75     !for 1-moment only
   ckQg1  = afg*gamma(1.+alpha_g+dmg+bfg)/GG40
   ckQg2  = afg*GG11*iGG31
   ckQg4  = 1./(cmg*GG40*iGG31)

   ! Hail:
   GH09   = gamma(2.5+bfh*0.5+alpha_h)
   GH11   = gamma(1.+bfh+alpha_h)
   GH12   = gamma(2.+bfh+alpha_h)
   GH13   = gamma(3.+bfh+alpha_h)
   GH31   = gamma(1.+alpha_h)
   iGH31  = 1./GH31
   GH32   = gamma(2.+alpha_h)
   GH33   = gamma(3.+alpha_h)
   iGH34  = 1./gamma(4.+alpha_h)
   GH40   = gamma(1.+alpha_h+dmh)
   iGH99  = 1./(GH40*iGH31*cmh)
   GH50   = (No_h_SM*GH31)**0.75     !for 1-moment only
   ckQh1  = afh*gamma(1.+alpha_h+dmh+bfh)/GH40
   ckQh2  = afh*GH11*iGH31
   ckQh4  = 1./(cmh*GH40*iGH31)

  endif  !if (KOUNT=0)
!####

!=======================================================================================!
!  if (DEBUG_ON) call check_values(Q,T,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH,epsQ,epsN,.false.,DEBUG_abort,200)

!--- Ensure consistency between moments:
  do k= kbot,ktop,kdir
     do i= 1,ni

        tmp1 = QSW(i,k)/max(Q(i,k),1.e-20)   !saturation w.r.t. water
        tmp2 = QSI(i,k)/max(Q(i,k),1.e-20)   !saturation w.r.t. ice
       !NOTE: Hydrometeor (Qx,Nx) is clipped if Qx is tiny or if Qx is small
       !      and is expected to completely evaporate/sublimate in one time step
       !      anyway.  (To avoid creating mass from a parallel universe, only
       !      positive Qx values are added to water vapor upon clipping.)
       !   ** RH thresholds for clipping need to be tuned (especially for graupel
       !      and hail, for which it may be preferable to reduce threshold)


       !cloud:
        if (QC(i,k)>epsQ .and. NC(i,k)<epsN) then
           NC(i,k) = N_c_SM
        elseif (QC(i,k)<=epsQ .or. (QC(i,k)<epsQ2 .and. tmp1<0.90)) then
           tmp3    = max(0., QC(i,k))
           Q(i,k)  = Q(i,k) + tmp3
           T(i,k)  = T(i,k) - LCP*tmp3
           QC(i,k) = 0.
           NC(i,k) = 0.
        endif

       !rain
        if (QR(i,k)>epsQ .and. NR(i,k)<epsN) then
           NR(i,k) = (No_r_SM*GR31)**(3./(4.+alpha_r))*(GR31*iGR34*DE(i,k)*QR(i,k)*      &
                     icmr)**((1.+alpha_r)/(4.+alpha_r))
        elseif (QR(i,k)<=epsQ .or. (QR(i,k)<epsQ2 .and. tmp1<0.90)) then
           tmp3    = max(0., QR(i,k))
           Q(i,k)  = Q(i,k) + tmp3
           T(i,k)  = T(i,k) - LCP*tmp3
           QR(i,k) = 0.
           NR(i,k) = 0.
        endif

        !ice:
        if (QI(i,k)>epsQ .and. NY(i,k)<epsN) then
    !       NY(i,k) = N_Cooper(TRPL,T(i,k))
           NY(i,k) = max(2.*epsN, N_Cooper(TRPL,T(i,k)) )
        elseif (QI(i,k)<=epsQ .or. (QI(i,k)<epsQ2 .and. tmp2<0.80)) then
           tmp3    = max(0., QI(i,k))
           Q(i,k)  = Q(i,k) + tmp3
           T(i,k)  = T(i,k) - LSP*tmp3
           QI(i,k) = 0.
           NY(i,k) = 0.
        endif

       !snow:
        if (QN(i,k)>epsQ .and. NN(i,k)<epsN) then
           No_s    = Nos_Thompson(TRPL,T(i,k))
           NN(i,k) = (No_s*GS31)**(dms*icexs2)*(GS31*iGS40*icms*DE(i,k)*QN(i,k))**       &
                     ((1.+alpha_s)*icexs2)
        elseif (QN(i,k)<=epsQ .or. (QN(i,k)<epsQ2 .and. tmp2<0.80)) then
           tmp3    = max(0., QN(i,k))
           Q(i,k)  = Q(i,k) + tmp3
           T(i,k)  = T(i,k) - LSP*tmp3
           QN(i,k) = 0.
           NN(i,k) = 0.
        endif

       !grpl:
      if (QG(i,k)>epsQ .and. NG(i,k)<epsN) then
           NG(i,k) = (No_g_SM*GG31)**(3./(4.+alpha_g))*(GG31*iGG34*DE(i,k)*QG(i,k)*      &
                     icmg)**((1.+alpha_g)/(4.+alpha_g))
        elseif (QG(i,k)<=epsQ .or. (QG(i,k)<epsQ2 .and. tmp2<0.80)) then
           tmp3    = max(0., QG(i,k))
           Q(i,k)  = Q(i,k) + tmp3
           T(i,k)  = T(i,k) - LSP*tmp3
           QG(i,k) = 0.
           NG(i,k) = 0.
        endif

       !hail:
        if (QH(i,k)>epsQ .and. NH(i,k)<epsN) then
           NH(i,k) = (No_h_SM*GH31)**(3./(4.+alpha_h))*(GH31*iGH34*DE(i,k)*QH(i,k)*      &
                     icmh)**((1.+alpha_h)/(4.+alpha_h))
        elseif (QH(i,k)<=epsQ .or. (QH(i,k)<epsQ2 .and. tmp2<0.80)) then
           tmp3    = max(0., QH(i,k))
           Q(i,k)  = Q(i,k) + tmp3
           T(i,k)  = T(i,k) - LSP*tmp3
           QH(i,k) = 0.
           NH(i,k) = 0.
        endif

     enddo !i-loop
  enddo    !k-loop
!===

  if (DEBUG_ON) call check_values(Q,T,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH,epsQ,epsN,.true.,DEBUG_abort,300)

!Temporarily store arrays at time (t*) in order to compute warm-rain coalescence
! equations (Part 3a):
  QC_in = QC
  QR_in = QR
  NC_in = NC
  NR_in = NR


 !Air-density factor (for fall velocity computations):
  do i= 1,ni
     gamfact(i,:)  = sqrt(DEo/(DE(i,:)))
  enddo

 !Pressure difference between levels (used for sedimentation)
  iDP(:,kbot) = 1./(PS(:)-pres(:,kbot))
  do k = kbot+kdir,ktop,kdir
    iDP(:,k) = 1./(pres(:,k-kdir)-pres(:,k))
  enddo

 !Compute thickness of layers for sedimentation calculation: (optional use if height coordiates)
 ! note - from 'cldoptx4.ftn':  dz(i,k)= dp(i,k)/aird(i,k)*rec_grav
  iDZ = DE*GRAV*iDP
  DZ  = 1./iDZ  

 !Compute height above surface (zheight) and height above lowest prog level (zz):
  zheight(:,kbot)= DZ(:,kbot)
  zz(:,kbot)= 0.       !<-- define height of lowest prognosic level to be 0. (for diagnostics only)
  do k = kbot+kdir,ktop,kdir
     zheight(:,k) = zheight(:,k-kdir) + DZ(:,k)
     zz(:,k)      = zz(:,k-kdir)      + DZ(:,k)
  enddo

 !Determine the upper-most level in each column to which to compute sedimentation:
 !ktop_sedi= ktop+kdir !<Optional (in place of code below) - to compute sedimentation at all levels
  ktop_sedi= 0
  do i=1,ni
     do k= ktop,kbot,-kdir
       ktop_sedi(i)= k
       if (zheight(i,k)<zMax_sedi) exit
     enddo
  enddo

  !----------------------------------------------------------------------------------!

  !----------------------------------------------------------------------------------!
  !                 End of Preliminary Calculation section (Part 1)                  !
  !----------------------------------------------------------------------------------!

  if (DEBUG_ON) call check_values(Q,T,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH,epsQ,epsN,.true.,DEBUG_abort,400)

  !----------------------------------------------------------------------------------!
  !                      PART 2: Cold Microphysics Processes                         !
  !----------------------------------------------------------------------------------!

! Determine the active grid points (i.e. those which scheme should treat):
  activePoint = .false.
  DO k= ktop-kdir,kbot,-kdir
     DO i=1,ni
        log1= ((QI(i,k)+QG(i,k)+QN(i,k)+QH(i,k))<epsQ)     !no solid  (i,g,s,h)
        log2= ((QC(i,k)+QR(i,k))                  <epsQ)   !no liquid (c,r)
        log3= ((T(i,k)>TRPL) .and. log1)                   !T>0C & no i,g,s,h
        log4= log1.and.log2.and.(Q(i,k)<QSI(i,k))          !no sol. or liq.; subsat(i)
        if (.not.( log3 .or. log4 ) .and. icephase_ON) then
          activePoint(i,k)= .true.
        endif
     ENDDO
  ENDDO

    ! Size distribution parameters:
    !  Note: + 'thrd' should actually be '1/dmx'(but dmx=3 for all categories x)
    !        + If Qx=0, LAMx etc. are never be used in any calculations
    !          (If Qc=0, CLcy etc. will never be calculated. iLAMx is set to 0
    !           to avoid possible problems due to bugs.)

  DO k= ktop-kdir,kbot,-kdir
    DO i= 1,ni
      IF (activePoint(i,k)) THEN

       Tc= T(i,k)-TRPL
       if (Tc<-120. .or. Tc>50.) then
          print*, '***WARNING*** -- In MICROPHYSICS --  Ambient Temp.(C),step,i,k:',Tc,kount,i,k
         !stop
       endif
       Cdiff = (2.2157e-5+0.0155e-5*Tc)*1.e5/pres(i,k)
       MUdyn = 1.72e-5*(393./(T(i,k)+120.))*(T(i,k)/TRPL)**1.5 !RYp.102
       MUkin = MUdyn*iDE(i,k)
       iMUkin= 1./MUkin
       ScTHRD= (MUkin/Cdiff)**thrd       ! i.e. Sc^(1/3)
       Ka    = 2.3971e-2 + 0.0078e-2*Tc                                   !therm.cond.(air)
       Kdiff = (9.1018e-11*T(i,k)*T(i,k)+8.8197e-8*T(i,k)-(1.0654e-5)) !therm.diff.(air)
       gam   = gamfact(i,k)

      !Collection efficiencies:
       Eis   = min(0.05*exp(0.1*Tc),1.)     !Ferrier, 1995 (Table 1)
       Eig   = min(0.01*exp(0.1*Tc),1.)     !dry (Eig=1.0 for wet growth)
       Eii   = 0.1*Eis
       Ess   = Eis;   Eih = Eig;   Esh = Eig
       iEih  = 1./Eih
       iEsh  = 1./Esh
       !note:  Eri=Ers=Erh=1. (constant parameters)
       !       - Ecs is computed in CLcs section
       !       - Ech is computed in CLch section
       !       - Ecg is computed in CLcg section
       !       - Erg is computed in CLrg section

       qvs0   = qsat(TRPL,pres(i,k),0)      !sat.mix.ratio at 0C
       DELqvs = qvs0-(Q(i,k))

    ! Cloud:
       if (QC(i,k)>epsQ) then
          iQC   = 1./QC(i,k)
          iNC   = 1./NC(i,k)
          Dc     = Dm_x(DE(i,k),QC(i,k),iNC,icmr,thrd)

          iLAMc  = iLAMDA_x(DE(i,k),QC(i,k),iNC,icexc9,thrd)
          iLAMc2 = iLAMc *iLAMc
          iLAMc3 = iLAMc2*iLAMc
          iLAMc4 = iLAMc2*iLAMc2
          iLAMc5 = iLAMc3*iLAMc2
       else
          Dc     = 0.;   iLAMc3= 0.
          iLAMc  = 0.;   iLAMc4= 0.
          iLAMc2 = 0.;   iLAMc5= 0.
       endif

    ! Rain:
       if (QR(i,k)>epsQ) then
          iQR   = 1./QR(i,k)
          iNR   = 1./NR(i,k)
          Dr     = Dm_x(DE(i,k),QR(i,k),iNR,icmr,thrd)
          iLAMr  = max( iLAMmin1, iLAMDA_x(DE(i,k),QR(i,k),iNR,icexr9,thrd) )
          tmp1   = 1./iLAMr
          iLAMr2 = iLAMr**2
          iLAMr3 = iLAMr**3
          iLAMr4 = iLAMr**4
          iLAMr5 = iLAMr**5
          if (Dr>40.e-6) then
             vr0 = gamfact(i,k)*ckQr1*iLAMr**bfr
          else
             vr0 = 0.
          endif
       else
          iLAMr  = 0.;  Dr    = 0.;  vr0   = 0.
          iLAMr2 = 0.;  iLAMr3= 0.;  iLAMr4= 0.;  iLAMr5 = 0.
       endif

    ! Ice:
       if (QI(i,k)>epsQ) then
          iQI   = 1./QI(i,k)
          iNY   = 1./NY(i,k)
          iLAMi  = max( iLAMmin2, iLAMDA_x(DE(i,k),QI(i,k),iNY,icexi9,thrd) )
          iLAMi2 = iLAMi**2
          iLAMi3 = iLAMi**3
          iLAMi4 = iLAMi**4
          iLAMi5 = iLAMi**5
          iLAMiB0= iLAMi**(bfi)
          iLAMiB1= iLAMi**(bfi+1.)
          iLAMiB2= iLAMi**(bfi+2.)
          vi0    = gamfact(i,k)*ckQi1*iLAMiB0
          Di     = Dm_x(DE(i,k),QI(i,k),iNY,icmi,thrd)
       else
          iLAMi  = 0.;  vi0    = 0.;  Di     = 0.
          iLAMi2 = 0.;  iLAMi3 = 0.;  iLAMi4 = 0.;  iLAMi5= 0.
          iLAMiB0= 0.;  iLAMiB1= 0.;  iLAMiB2= 0.
       endif

    ! Snow:
       if (QN(i,k)>epsQ) then
          iQN   = 1./QN(i,k)
          iNN   = 1./NN(i,k)
          iLAMs  = max( iLAMmin2, iLAMDA_x(DE(i,k),QN(i,k),iNN,iGS20,idms) )
          iLAMs_D3= max(iLAMmin2, iLAMDA_x(DE(i,k),QN(i,k),iNN,iGS20_D3,thrd) )
          iLAMs2 = iLAMs**2
          iLAMsB0= iLAMs**(bfs)
          iLAMsB1= iLAMs**(bfs+1.)
          iLAMsB2= iLAMs**(bfs+2.)
          vs0    = gamfact(i,k)*ckQs1*iLAMsB0
          Ds     = min(DsMax, Dm_x(DE(i,k),QN(i,k),iNN,icms,idms))
          if (snowSpherical) then
             des = desFix
          else
             des = des_OF_Ds(Ds,desMax,eds,fds)
          endif
         !!-- generalized equations (any alpha_s):
         !    No_s  = (NN(i,k))*iGS31/iLAMs**(1.+alpha_s)
         !    VENTs = Avx*GS32*iLAMs**(2.+alpha_s)+Bvx*ScTHRD*sqrt(gam*afs*iMUkin)*      &
         !!--         GS09*iLAMs**(2.5+0.5*bfs+alpha_s)
         !The following equations for No_s and VENTs is based on m(D)=(pi/6)*100.*D**3 for snow.
         !  Strict application of m(D)=c*D**2 would require re-derivation using implied
         !  definition of D as the MAXIMUM DIMENSION of an ellipsoid, rather than a sphere.
         !  For simplicity, the m-D^3 relation is applied -- used for VDvs and MLsr only.
        !No_s= NN(i,k)*iGS31/iLAMs     !optimized for alpha_s=0
         No_s= NN(i,k)*iGS31/iLAMs_D3  !based on m-D^3 (consistent with VENTs, below)
         VENTs= Avx*GS32*iLAMs_D3**2. + Bvx*ScTHRD*sqrt(gamfact(i,k)*afs*iMUkin)*GS09*   &
                iLAMs_D3**cexs1
       else
          iLAMs  = 0.;  vs0    = 0.;  Ds     = 0.;  iLAMs2= 0.
          iLAMsB0= 0.;  iLAMsB1= 0.;  iLAMsB1= 0.
          des    = desFix !used for 3-component freezing if QN=0 (even for snowSpherical=.F.)
       endif
       ides  = 1./des


    ! Graupel:
       if (QG(i,k)>epsQ) then
          iQG    = 1./QG(i,k)
          iNG    = 1./NG(i,k)
          iLAMg  = max( iLAMmin1, iLAMDA_x(DE(i,k),QG(i,k),iNG,iGG99,thrd) )
          iLAMg2 = iLAMg**2
          iLAMgB0= iLAMg**(bfg)
          iLAMgB1= iLAMg**(bfg+1.)
          iLAMgB2= iLAMg**(bfg+2.)
         !No_g = (NG(i,k))*iGG31/iLAMg**(1.+alpha_g)
          No_g= NG(i,k)*iGG31/iLAMg     !optimized for alpha_g=0
          vg0    = gamfact(i,k)*ckQg1*iLAMgB0
          Dg     = Dm_x(DE(i,k),QG(i,k),iNG,icmg,thrd)
       else
          iLAMg  = 0.;  vg0    = 0.;  Dg     = 0.;  No_g   = 0.
          iLAMg2 = 0.;  iLAMgB0= 0.;  iLAMgB1= 0.;  iLAMgB1= 0.
       endif

    ! Hail:
       if (QH(i,k)>epsQ) then
          iQH    = 1./QH(i,k)
          iNH    = 1./NH(i,k)
          iLAMh  = max( iLAMmin1, iLAMDA_x(DE(i,k),QH(i,k),iNH,iGH99,thrd) )
          iLAMh2 = iLAMh**2
          iLAMhB0= iLAMh**(bfh)
          iLAMhB1= iLAMh**(bfh+1.)
          iLAMhB2= iLAMh**(bfh+2.)
          No_h= NH(i,k)*iGH31/iLAMh**(1.+alpha_h)
          vh0    = gamfact(i,k)*ckQh1*iLAMhB0
          Dh     = Dm_x(DE(i,k),QH(i,k),iNH,icmh,thrd)
       else
          iLAMh  = 0.;  vh0    = 0.;  Dh     = 0.;  No_h= 0.
          iLAMhB0= 0.;  iLAMhB1= 0.;  iLAMhB1= 0.
       endif
!------

 !Calculating ice-phase source/sink terms:

 ! Initialize all source terms to zero:
       QNUvi=0.;  QVDvi=0.;  QVDvs=0.;  QVDvg=0.;  QVDvh=0.
       QCLcs=0.;  QCLcg=0.;  QCLch=0.;  QFZci=0.;  QCLri=0.;   QMLsr=0.
       QCLrs=0.;  QCLrg=0.;  QMLgr=0.;  QCLrh=0.;  QMLhr=0.;   QFZrh=0.
       QMLir=0.;  QCLsr=0.;  QCLsh=0.;  QCLgr=0.;  QCNgh=0.
       QCNis=0.;  QCLir=0.;  QCLis=0.;  QCLih=0.
       QIMsi=0.;  QIMgi=0.;  QCNsg=0.;  QHwet=0.

       NCLcs= 0.; NCLcg=0.;  NCLch=0.;  NFZci=0.;  NMLhr=0.;   NhCNgh=0.
       NCLri= 0.; NCLrs=0.;  NCLrg=0.;  NCLrh=0.;  NMLsr=0.;   NMLgr=0.
       NMLir= 0.; NSHhr=0.;  NNUvi=0.;  NVDvi=0.;  NVDvh=0.;   QCLig=0.
       NCLir= 0.; NCLis=0.;  NCLig=0.;  NCLih=0.;  NIMsi=0.;   NIMgi=0.
       NiCNis=0.; NsCNis=0.; NVDvs=0.;  NCNsg=0.;  NCLgr=0.;   NCLsrh=0.
       NCLss= 0.; NCLsr=0.;  NCLsh=0.;  NCLsrs=0.; NCLgrg=0.;  NgCNgh=0.
       NVDvg= 0.; NCLirg=0.; NCLsrg=0.; NCLgrh=0.; NrFZrh=0.;  NhFZrh=0.
       NCLirh=0.

       Dirg=0.; Dirh=0.; Dsrs= 0.; Dsrg= 0.; Dsrh= 0.; Dgrg=0.; Dgrh=0.

   !-------------------------------------------------------------------------------------------!

       Si    = Q(i,k)/QSI(i,k)
       iABi  = 1./( CHLS*CHLS/(Ka*RGASV*T(i,k)**2) + 1./(DE(i,k)*(QSI(i,k))*Cdiff) )

           ! COLLECTION by snow, graupel, hail:
           !  (i.e. wet or dry ice-categories [=> excludes ice crystals])

           ! Collection by SNOW:
       if (QN(i,k)>epsQ) then
          ! cloud:
          if (QC(i,k)>epsQ) then

            !Approximation of Ecs based on Pruppacher & Klett (1997) Fig. 14-11
             Ecs= min(Dc,30.e-6)*3.333e+4*sqrt(min(Ds,1.e-3)*1.e+3)
             QCLcs= dt*gam*afs*cmr*Ecs*PIov4*iDE(i,k)*(NC(i,k)*NN(i,k))*iGC5*iGS31*    &
                    (GC13*GS13*iLAMc3*iLAMsB2+2.*GC14*GS12*iLAMc4*iLAMsB1+GC15*GS11*     &
                    iLAMc5*iLAMsB0)

             NCLcs= dt*gam*afs*PIov4*Ecs*(NC(i,k)*NN(i,k))*iGC5*iGS31*(GC5*GS13*       &
                    iLAMsB2+2.*GC11*GS12*iLAMc*iLAMsB1+GC12*GS11*iLAMc2*iLAMsB0)

            !continuous collection: (alternative; gives values ~0.95 of SCE [above])
            !QCLcs= dt*gam*Ecs*PIov4*afs*QC(i,k)*NN(i,k)*iLAMs**(2.+bfs)*GS13*iGS31
            !NCLcs= QCLcs*NC(i,k)/QC(i,k)

            !Correction factor for non-spherical snow [D = maximum dimension] which
            !changes projected area:   [assumption: A=0.50*D**2 (vs. A=(PI/4)*D**2)]
            ! note: Strictly speaking, this correction should only be applied to
            !       continuous growth approximation for cloud.  [factor = 0.50/(pi/4)]
             if (.not. snowSpherical) then
                tmp1 = 0.6366      !factor = 0.50/(pi/4)
                QCLcs= tmp1*QCLcs
                NCLcs= tmp1*NCLcs
             endif

             QCLcs= min(QCLcs, QC(i,k))
             NCLcs= min(NCLcs, NC(i,k))
          else
             QCLcs= 0.;   NCLcs= 0.
          endif

          ! ice:
          if (QI(i,k)>epsQ) then
             tmp1= vs0-vi0
             tmp3= sqrt(tmp1*tmp1+0.04*vs0*vi0)

             QCLis= dt*cmi*iDE(i,k)*PI*6.*Eis*(NY(i,k)*NN(i,k))*tmp3*iGI31*iGS31*(0.5* &
                    iLAMs2*iLAMi3+2.*iLAMs*iLAMi4+5.*iLAMi5)

             NCLis= dt*PIov4*Eis*(NY(i,k)*NN(i,k))*GI31*GS31*tmp3*(GI33*GS31*iLAMi2+   &
                    2.*GI32*GS32*iLAMi*iLAMs+GI31*GS33*iLAMs2)

             QCLis= min(QCLis, (QI(i,k)))
             NCLis= min(QCLis*(NY(i,k)*iQI), NCLis)
          else
             QCLis= 0.;   NCLis= 0.
          endif

          !snow: (i.e. self-collection [aggregation])
          NCLss= dt*0.93952*Ess*(DE(i,k)*(QN(i,k)))**((2.+bfs)*thrd)*(NN(i,k))**         &
                   ((4.-bfs)*thrd)
            !Note: 0.91226 = I(bfs)*afs*PI^((1-bfs)/3)*des^((-2-bfs)/3); I(bfs=0.41)=1138
            !      0.93952 = I(bfs)*afs*PI^((1-bfs)/3)*des^((-2-bfs)/3); I(bfs=0.42)=1172
            !      [interpolated from 3rd-order polynomial approx. of values given in RRB98;
            !       see eqn(A.35)]
           NCLss= min(NCLss, 0.5*(NN(i,k)))

       else
          QCLcs= 0.;   NCLcs= 0.;   QCLis= 0.;   NCLis= 0.;  NCLss= 0.
       endif

       ! Collection by GRAUPEL:
       if (QG(i,k)>epsQ) then

          ! cloud:
          if (QC(i,k)>epsQ) then

            !(parameterization of Ecg based on Cober and List, 1993 [JAS])
             Kstoke = dew*vg0*Dc*Dc/(9.*MUdyn*Dg)
             Kstoke = max(1.5,min(10.,Kstoke))
             Ecg    = 0.55*log10(2.51*Kstoke)

             QCLcg= dt*gam*afg*cmr*Ecg*PIov4*iDE(i,k)*(NC(i,k)*NG(i,k))*iGC5*iGG31*      &
                    (GC13*GG13*iLAMc3*iLAMgB2+ 2.*GC14*GG12*iLAMc4*iLAMgB1+GC15*GG11*    &
                    iLAMc5*iLAMgB0)

             NCLcg= dt*gam*afg*PIov4*Ecg*(NC(i,k)*NG(i,k))*iGC5*iGG31*(GC5*GG13*         &
                    iLAMgB2+2.*GC11*GG12*iLAMc*iLAMgB1+GC12*GG11*iLAMc2*iLAMgB0)

             QCLcg= min(QCLcg, (QC(i,k)))
             NCLcg= min(NCLcg, (NC(i,k)))
          else
             QCLcg= 0.;   NCLcg= 0.
          endif

          ! ice:
          if (QI(i,k)>epsQ) then
             tmp1= vg0-vi0
             tmp3= sqrt(tmp1*tmp1+0.04*vg0*vi0)

             QCLig= dt*cmi*iDE(i,k)*PI*6.*Eig*(NY(i,k)*NG(i,k))*tmp3*iGI31*iGG31*(0.5*   &
                    iLAMg2*iLAMi3+2.*iLAMg*iLAMi4+5.*iLAMi5)
             NCLig= dt*PIov4*Eig*(NY(i,k)*NG(i,k))*GI31*GG31*tmp3*(GI33*GG31*iLAMi2+     &
                    2.*GI32*GG32*iLAMi*iLAMg+GI31*GG33*iLAMg2)

             QCLig= min(QCLig, (QI(i,k)))
             NCLig= min(QCLig*(NY(i,k)*iQI), NCLig)
          else
             QCLig= 0.;   NCLig= 0.
          endif

         !Deposition/sublimation:
          VENTg= Avx*GG32*iLAMg*iLAMg+Bvx*ScTHRD*sqrt(gam*afg*iMUkin)*GG09*iLAMg**       &
                 (2.5+0.5*bfg+alpha_g)
!         QVDvg = dt*iDE(i,k)*iABi*(PI2*(Si-1.)*No_g*VENTg - CHLS*CHLF/(Ka*RGASV*        &
!                   T(i,k)**2)*QCLcg*idt)
          QVDvg = dt*iDE(i,k)*iABi*(PI2*(Si-1.)*No_g*VENTg)   !neglect accretion term
          ! Prevent overdepletion of vapor:
          VDmax = (Q(i,k)-QSI(i,k))/(1.+ck6*QSI(i,k)/(T(i,k)-7.66)**2)  !KY97_A.33
          if(Si>=1.) then
             QVDvg= min(max(QVDvg,0.),VDmax)
          else
             if (VDmax<0.) QVDvg= max(QVDvg,VDmax)
             !IF prevents subl.(QVDvs<0 at t) changing to dep.(VDmax>0 at t*)
          endif
         !NVDvg = -min(0.,NG(i,k)*iQG*QVDvg)  !assume slope  does not change during sublimation (pos. quantity)
          NVDvg = 0.                            !assume number does not change during sublimation

       else
          QCLcg= 0.;   QCLrg= 0.;   QCLig= 0.
          NCLcg= 0.;   NCLrg= 0.;   NCLig= 0.
       endif

       ! Collection by HAIL:
       if (QH(i,k)>epsQ) then

         ! cloud:
          if (QC(i,k)>epsQ) then
             Ech  = exp(-8.68e-7*Dc**(-1.6)*Dh)    !Ziegler (1985) A24

             QCLch= dt*gam*afh*cmr*Ech*PIov4*iDE(i,k)*(NC(i,k)*NH(i,k))*iGC5*iGH31*      &
                    (GC13*GH13*iLAMc3*iLAMhB2+2.*GC14*GH12*iLAMc4*iLAMhB1+GC15*GH11*     &
                    iLAMc5*iLAMhB0)

             NCLch= dt*gam*afh*PIov4*Ech*(NC(i,k)*NH(i,k))*iGC5*iGH31*(GC5*GH13*         &
                    iLAMhB2+2.*GC11*GH12*iLAMc*iLAMhB1+GC12*GH11*iLAMc2*iLAMhB0)

             QCLch= min(QCLch, QC(i,k))
             NCLch= min(NCLch, NC(i,k))
          else
             QCLch= 0.;   NCLch= 0.
          endif

          ! rain:
          if (QR(i,k)>epsQ) then
             tmp1= vh0-vr0
             tmp3= sqrt(tmp1*tmp1+0.04*vh0*vr0)
             QCLrh= dt*cmr*Erh*PIov4*iDE(i,k)*(NH(i,k)*NR(i,k))*iGR31*iGH31*tmp3*        &
                    (GR36*GH31*iLAMr5+2.*GR35*GH32*iLAMr4*iLAMh+GR34*GH33*iLAMr3*iLAMh2)

             NCLrh= dt*PIov4*Erh*(NH(i,k)*NR(i,k))*iGR31*iGH31*tmp3*(GR33*GH31*          &
                    iLAMr2+2.*GR32*GH32*iLAMr*iLAMh+GR31*GH33*iLAMh2)

             QCLrh= min(QCLrh, QR(i,k))
             NCLrh= min(NCLrh, QCLrh*(NR(i,k)*iQR))
          else
             QCLrh= 0.;   NCLrh= 0.
          endif

          ! ice:
          if (QI(i,k)>epsQ) then
             tmp1 = vh0-vi0
             tmp3 = sqrt(tmp1*tmp1+0.04*vh0*vi0)

             QCLih= dt*cmi*iDE(i,k)*PI*6.*Eih*(NY(i,k)*NH(i,k))*tmp3*iGI31*iGH31*(0.5*   &
                    iLAMh2*iLAMi3+2.*iLAMh*iLAMi4+5.*iLAMi5)

             NCLih= dt*PIov4*Eih*(NY(i,k)*NH(i,k))*GI31*GH31*tmp3*(GI33*GH31*iLAMi2+     &
                    2.*GI32*GH32*iLAMi*iLAMh+GI31*GH33*iLAMh2)

             QCLih= min(QCLih, QI(i,k))
             NCLih= min(QCLih*(NY(i,k)*iQI), NCLih)
          else
             QCLih= 0.;   NCLih= 0.
          endif

          ! snow:
          if (QN(i,k)>epsQ) then
             tmp1 = vh0-vs0
             tmp3 = sqrt(tmp1*tmp1+0.04*vh0*vs0)
             tmp4 = iLAMs2*iLAMs2

             if (snowSpherical) then
               !hardcoded for dms=3:
                QCLsh= dt*cms*iDE(i,k)*PI*6.*Esh*(NN(i,k)*NH(i,k))*tmp3*iGS31*iGH31*     &
                       (0.5*iLAMh2*iLAMs2*iLAMs+2.*iLAMh*tmp4+5.*tmp4*iLAMs)
             else
               !hardcoded for dms=2:
                QCLsh= dt*cms*iDE(i,k)*PI*0.25*Esh*tmp3*NN(i,k)*NH(i,k)*iGS31*iGH31*     &
                       (GH33*GS33*iLAMh**2.*iLAMs**2. + 2.*GH32*GS34*iLAMh*iLAMs**3. +   &
                        GH31*GS35*iLAMs**4.)
             endif

             NCLsh= dt*PIov4*Esh*(NN(i,k)*NH(i,k))*GS31*GH31*tmp3*(GS33*GH31*iLAMs2+     &
                    2.*GS32*GH32*iLAMs*iLAMh+GS31*GH33*iLAMh2)

             QCLsh= min(QCLsh, (QN(i,k)))
             NCLsh= min((NN(i,k)*iQN)*QCLsh, NCLsh, (NN(i,k)))
          else
             QCLsh= 0.;   NCLsh= 0.
          endif

         !wet growth:
          VENTh= Avx*GH32*iLAMh**(2.+alpha_h) + Bvx*ScTHRD*sqrt(gam*afh*iMUkin)*GH09*    &
                 iLAMh**(2.5+0.5*bfh+alpha_h)
          QHwet= max(0., dt*PI2*(DE(i,k)*CHLC*Cdiff*DELqvs-Ka*Tc)*No_h*iDE(i,k)/(CHLF+   &
                 CPW*Tc)*VENTh+(QCLih*iEih+QCLsh*iEsh)*(1.-CPI*Tc/(CHLF+CPW*Tc)) )

         !Deposition/sublimation:
!         QVDvh = dt*iDE(i,k)*iABi*(PI2*(Si-1.)*No_h*VENTh - CHLS*CHLF/(Ka*RGASV*        &
!                   T(i,k)**2)*QCLch*idt)
          QVDvh = dt*iDE(i,k)*iABi*(PI2*(Si-1.)*No_h*VENTh)   !neglect acretion term
          !prevent overdepletion of vapor:
          VDmax = (Q(i,k)-QSI(i,k))/(1.+ck6*(QSI(i,k))/(T(i,k)-7.66)**2)  !KY97_A.33    ** USED BY OTHERS; COULD BE PUT ABOVE
          if(Si>=1.) then
             QVDvh= min(max(QVDvh,0.),VDmax)
          else
             if (VDmax<0.) QVDvh= max(QVDvh,VDmax)  !prevents subl.(QVDvs<0 at t) changing to dep.(VDmax>0 at t*)
          endif
!         NVDvh= -min(0.,NH(i,k)*iQH*QVDvh)  !assume SLOPE does not change during sublimation (pos. quantity)
          NVDvh= 0.                            !assume NUMBER does not change during sublimation

       else
          QCLch= 0.;   QCLrh= 0.;   QCLih= 0.;   QCLsh= 0.;   QHwet= 0.
          NCLch= 0.;   NCLrh= 0.;   NCLsh= 0.;   NCLih= 0.
       endif

       IF (T(i,k)>TRPL .and. warmphase_ON) THEN
          !**********!
          !  T > To  !
          !**********!

          ! MELTING of frozen particles:
          !  ICE:
          QMLir   = QI(i,k)  !all pristine ice melts in one time step
          QI(i,k)= 0.
          NMLir   = NY(i,k)

          !  SNOW:
          if (QN(i,k)>epsQ) then
             QMLsr= dt*(PI2*iDE(i,k)*iCHLF*No_s*VENTs*(Ka*Tc-CHLC*Cdiff*DELqvs) + CPW*   &
                    iCHLF*Tc*(QCLcs+QCLrs)*idt)
             QMLsr= min(max(QMLsr,0.), QN(i,k))
             NMLsr= NN(i,k)*iQN*QMLsr
          else
             QMLsr= 0.;   NMLsr= 0.
          endif

          !  GRAUPEL:
          if (QG(i,k)>epsQ) then
             QMLgr= dt*(PI2*iDE(i,k)*iCHLF*No_g*VENTg*(Ka*Tc-CHLC*Cdiff*DELqvs) + CPW*   &
                    iCHLF*Tc*(QCLcg+QCLrg)*idt)
             QMLgr= min(max(QMLgr,0.), QG(i,k))
             NMLgr= NG(i,k)*iQG*QMLgr
          else
             QMLgr= 0.;   NMLgr= 0.
          endif

          !  HAIL:
          if (QH(i,k)>epsQ.and.Tc>5.) then
             VENTh= Avx*GH32*iLAMh**(2.+alpha_h) + Bvx*ScTHRD*sqrt(gam*afh*iMUkin)*GH09* &
                    iLAMh**(2.5+0.5*bfh+alpha_h)
             QMLhr= dt*(PI2*iDE(i,k)*iCHLF*No_h*VENTh*(Ka*Tc-CHLC*Cdiff*DELqvs) + CPW/   &
                    CHLF*Tc*(QCLch+QCLrh)*idt)
             QMLhr= min(max(QMLhr,0.), QH(i,k))
             NMLhr= NH(i,k)*iQH*QMLhr
             if(QCLrh>0.) NMLhr= NMLhr*0.1   !Prevents problems when hail is ML & CL
          else
             QMLhr= 0.;   NMLhr= 0.
          endif

         ! Cold (sub-zero) source/sink terms:
          QNUvi= 0.;   QFZci= 0.;   QVDvi= 0.;   QVDvs= 0.
          QCLis= 0.;   QCNis1=0.;   QCNis2=0.;   QCLri= 0.
          QCNgh= 0.;   QIMsi= 0.;   QIMgi= 0.;   QCLir= 0.
          QCLrs= 0.;   QCLgr= 0.;   QCLrg= 0.;   QCNis= 0.
          QCNsg= 0.;   QCLsr= 0.

          NNUvi= 0.;   NFZci= 0.;   NCLgr= 0.;   NCLrg= 0.;   NgCNgh= 0.
          NCLis= 0.;   NVDvi= 0.;   NVDvs= 0.;   NCLri= 0.;   NCLsr= 0.
          NCNsg= 0.;   NhCNgh= 0.;  NiCNis=0.;   NsCNis=0.
          NIMsi= 0.;   NIMgi= 0.;   NCLir= 0.;   NCLrs= 0.

       ELSE  !----------!
             !  T < To  !
             !----------!

          ! Warm-air-only source/sink terms:
          QMLir= 0.;   QMLsr= 0.;   QMLgr= 0.;   QMLhr= 0.
          NMLir= 0.;   NMLsr= 0.;   NMLgr= 0.;   NMLhr= 0.

          !Probabilistic freezing (Bigg) of rain:
          if (Tc<Tc_FZrh .and. QR(i,k)>epsQ .and. hail_ON) then
             !note: - (Tc<-10.C) condition is based on Pruppacher-Klett (1997) Fig. 9-41
             !      - Small raindrops will freeze to hail. However, if after all S/S terms
             !        are added Dh<Dh_min, then hail will be converted to graupel. Thus,
             !        probabilistic freezing of small rain is effectively a source of graupel.
             NrFZrh= -dt*Bbigg*(exp(Abigg*Tc)-1.)*DE(i,k)*QR(i,k)*idew
             Rz= 1.  !N and Z (and Q) are conserved for FZrh with triple-moment
           ! The Rz factor serves to conserve reflectivity when a rain distribution
           !  converts to an distribution with a different shape parameter, alpha.
           !  (e.g. when rain freezes to hail)  The factor Rz non-conserves N while
           !  acting to conserve Z for double-moment.  See Ferrier, 1994 App. D)
           ! Rz= (gamma(7.d0+alpha_h)*GH31*GR34*GR34)/(GR36(i,k)*GR31*   &
           !      gamma(4.d0+alpha_h)*gamma(4.d0+alpha_h))
             NhFZrh= Rz*NrFZrh
             QFZrh = NrFZrh*(QR(i,k)*iNR)
          else
             QFZrh= 0.;   NrFZrh= 0.;  NhFZrh= 0.
          endif

          !--------!
          !  ICE:  !
          !--------!
          ! Homogeneous freezing of cloud to ice:
          if (QC(i,k)>epsQ) then
             tmp2  = Tc*Tc; tmp3= tmp2*Tc; tmp4= tmp2*tmp2
             JJ    = (10.**max(-20.,(-606.3952-52.6611*Tc-1.7439*tmp2-0.0265*tmp3-    &
                      1.536e-4*tmp4)))
             tmp1  = 1.e6*(DE(i,k)*(QC(i,k)*iNC)*icmr) !i.e. Dc[cm]**3
             FRAC  = 1.-exp(-JJ*PIov6*tmp1*dt)
             if (Tc>-30.) FRAC= 0.
             if (Tc<-50.) FRAC= 1.
             QFZci= FRAC*QC(i,k)
             NFZci= FRAC*NC(i,k)
          else
             QFZci= 0.;   NFZci= 0.
          endif

          !Primary ice nucleation:
          NNUvi= 0.;   QNUvi= 0.
          if (primIceNucl==1) then

             NuDEPSOR= 0.;   NuCONT= 0.
             if (QSI(i,k)>1.e-20) then
                Simax = min(Si, satw_peak*QSW(i,k)/QSI(i,k))
             else
                Simax = 0.
             endif
             tmp1    = T(i,k)-7.66
             NNUmax  = max(0., DE(i,k)/mio*(Q(i,k)-QSI(i,k))/(1.+ck6*(QSI(i,k)/(tmp1*    &
                       tmp1))))
             !Deposition/sorption nucleation:
             if (Tc<-5. .and. Si>1.) then
                NuDEPSOR= max(0., 1.e3*exp(12.96*(Simax-1.)-0.639)-(NY(i,k))) !Meyers(1992)
             endif
             !Contact nucleation:
             if (QC(i,k)>epsQ .and. Tc<-2. .and. WZ(i,k)>0.001) then
                GG     =  1.*idew/(RGASV*(T(i,k))/((QSW(i,k)*pres(i,k))/EPS1)/          &
                            Cdiff+CHLC/Ka/(T(i,k))*(CHLC/RGASV/(T(i,k))-1.))  !CP00a
                Swmax  =  SxFNC(WZ(i,k),Tc,pres(i,k),QSW(i,k),QSI(i,k),CCNtype,1)
                if (QSW(i,k)>1.e-20) then
                   ssat=  min((Q(i,k)/QSW(i,k)), Swmax) -1.
                else
                   ssat= 0.
                endif
                Tcc    =  Tc + GG*ssat*CHLC/Kdiff                            !C86_eqn64
                Na     =  exp(4.11-0.262*Tcc)                                !W95_eqn60/M92_2.6
                Kn     =  LAMa0*(T(i,k))*p0/(T0*pres(i,k)*Ra)               !W95_eqn59
                PSIa   =  -kBoltz*Tcc/(6.*pi*Ra*MUdyn)*(1.+Kn)               !W95_eqn58
                ft     =  0.4*(1.+1.45*Kn+0.4*Kn*exp(-1./Kn))*(Ka+2.5*Kn*KAPa)/          &
                         (1.+3.*Kn)/(2.*Ka+5.*KAPa*Kn+KAPa)                  !W95_eqn57
                Dc     =  (DE(i,k)*(QC(i,k)*iNC)*icmr)**thrd
                F1     =  PI2*Dc*Na*(NC(i,k))                               !W95_eqn55
                F2     =  Ka/pres(i,k)*(Tc-Tcc)                              !W95_eqn56
                NuCONTA= -F1*F2*RGASV*(T(i,k))/CHLC*iDE(i,k)                !diffusiophoresis
                NuCONTB=  F1*F2*ft*iDE(i,k)                                  !thermeophoresis
                NuCONTC=  F1*PSIa                                            !Brownian diffusion
                NuCONT =  max(0.,(NuCONTA+NuCONTB+NuCONTC)*dt)
             endif
             !Total primary ice nucleation:
             if (icephase_ON) then
                NNUvi= min(NNUmax, NuDEPSOR + NuCONT )
                QNUvi= mio*iDE(i,k)*NNUvi
                QNUvi= min(QNUvi,(Q(i,k)))
             endif

          elseif (primIceNucl==2) then
             if (Tc<-5. .and. Si>1.08) then !following Thompson etal (2006)
                NNUvi= max(N_Cooper(TRPL,T(i,k))-NY(i,k),0.)
                QNUvi= min(mio*iDE(i,k)*NNUvi, Q(i,k))
             endif
         !elseif (primIceNucl==3) then
         !! (for alternative [future] ice nucleation parameterizations)
         !   NNUvi=...
         !   QNUvi=...
          endif !if (primIceNucl==1)



          IF (QI(i,k)>epsQ) THEN

             !Deposition/sublimation:
!            No_i  = NY(i,k)*iGI31/iLAMi**(1.+alpha_i)
!            VENTi= Avx*GI32*iLAMi**(2.+alpha_i)+Bvx*ScTHRD*sqrt(gam*afi*iMUkin)*GI6*    &
!                     iLAMi**(2.5+0.5*bfi+alpha_i)
             No_i  = NY(i,k)*iGI31/iLAMi    !optimized for alpha_i=0
             VENTi= Avx*GI32*iLAMi*iLAMi+Bvx*ScTHRD*sqrt(gam*afi*iMUkin)*GI6*iLAMi**     &
                    (2.5+0.5*bfi+alpha_i)
            !Note: ice crystal capacitance is implicitly C = 0.5*D*capFact_i
!            QVDvi= dt*capFact_i*iABi*(PI2*(Si-1.)*No_i*VENTi)
             QVDvi = dt*iDE(i,k)*capFact_i*iABi*(PI2*(Si-1.)*No_i*VENTi)

             ! Prevent overdepletion of vapor:
             VDmax = (Q(i,k)-QSI(i,k))/(1.+ck6*(QSI(i,k))/(T(i,k)-7.66)**2)
             if(Si>=1.) then
                QVDvi= min(max(QVDvi,0.),VDmax)
             else
                if (VDmax<0.) QVDvi= max(QVDvi,VDmax)
               !IF prevents subl.(QVDvi<0 at t) changing to dep.(VDmax>0 at t*)  2005-06-28
             endif
             if (.not. iceDep_ON) QVDvi= 0. !suppresses depositional growth
             NVDvi= min(0., (NY(i,k)*iQI)*QVDvi) !dNi/dt=0 for deposition

             ! Conversion to snow:
             if (QI(i,k)+QVDvi>epsQ .and. NY(i,k)+NVDvi>epsN) then
                tmp5   = iLAMi !hold value
                tmp6   = No_i  !hold value
               !estimate ice PSD after VDvi (if there were no CNis):
                tmp1   = QI(i,k) + QVDvi
                tmp2   = NY(i,k) + NVDvi
                tmp3   = 1./tmp2
                iLAMi  = max( iLAMmin2, iLAMDA_x(DE(i,k),tmp1,tmp3,icexi9,thrd) )
                No_i   = tmp2*iGI31/iLAMi    !optimized for alpha_i=0
               !compute number and mass of ice converted to snow as the integral from
               ! Dso to INF of Ni(D)dD and m(D)Ni(D)dD, respectively:
                tmp4   = exp(-Dso/iLAMi)
                NiCNis = No_i*iLAMi*tmp4
                NsCNis = NiCNis
                QCNis  = cmi*No_i*tmp4*(Dso**3*iLAMi + 3.*Dso**2*iLAMi**2 + 6.*Dso*      &
                         iLAMi**3 + 6.*iLAMi**4)
                iLAMi  = tmp5  !(restore value)
                No_i   = tmp6  !(restore value)
             endif

             if (.not.(snow_ON)) then  !Suppress SNOW initiation (for testing only)
                QCNis  = 0.
                NiCNis = 0.
                NsCNis = 0.
             endif

             ! 3-component freezing (collisions with rain):
             if (QR(i,k)>epsQ .and. QI(i,k)>epsQ) then
                tmp1 = vr0-vi0
                tmp3 = sqrt(tmp1*tmp1+0.04*vr0*vi0)

                QCLir= dt*cmi*Eri*PIov4*iDE(i,k)*(NR(i,k)*NY(i,k))*iGI31*iGR31*tmp3*     &
                       (GI36*GR31*iLAMi5+2.*GI35*GR32*iLAMi4*iLAMr+GI34*GR33*iLAMi3*     &
                       iLAMr2)

                NCLri= dt*PIov4*Eri*(NR(i,k)*NY(i,k))*iGI31*iGR31*tmp3*(GI33*GR31*       &
                       iLAMi2+2.*GI32*GR32*iLAMi*iLAMr+GI31*GR33*iLAMr2)

                QCLri= dt*cmr*Eri*PIov4*iDE(i,k)*(NY(i,k)*NR(i,k))*iGR31*iGI31*tmp3*     &
                       (GR36*GI31 *iLAMr5+2.*GR35*GI32*iLAMr4*iLAMi+GR34*GI33*iLAMr3*    &
                       iLAMi2)

               !note: For explicit eqns, both NCLri and NCLir are mathematically identical)
                NCLir= min(QCLir*(NY(i,k)*iQI), NCLri)
                QCLri= min(QCLri, (QR(i,k)));  QCLir= min(QCLir, (QI(i,k)))
                NCLri= min(NCLri, (NR(i,k)));  NCLir= min(NCLir, (NY(i,k)))

                !Determine destination of 3-comp.freezing:
                tmp1= max(Di,Dr)
                dey= (dei*Di*Di*Di+dew*Dr*Dr*Dr)/(tmp1*tmp1*tmp1)
                if (dey>0.5*(deg+deh) .and. Dr>Dr_3cmpThrs .and. hail_ON) then
                   Dirg= 0.;  Dirh= 1.
                else
                   Dirg= 1.;  Dirh= 0.
                endif
                if (.not. grpl_ON) Dirg= 0.

             else
                QCLir= 0.;  NCLir= 0.;  QCLri= 0.
                NCLri= 0.;  Dirh = 0.;  Dirg= 0.
             endif

             !  Rime-splintering (ice multiplication):
             ff= 0.
             if(Tc>=-8..and.Tc<=-5.) ff= 3.5e8*(Tc +8.)*thrd
             if(Tc> -5..and.Tc< -3.) ff= 3.5e8*(-3.-Tc)*0.5
             NIMsi= DE(i,k)*ff*QCLcs
             NIMgi= DE(i,k)*ff*QCLcg
             QIMsi= mio*iDE(i,k)*NIMsi
             QIMgi= mio*iDE(i,k)*NIMgi

          ELSE

             QVDvi= 0.;  QCNis= 0.
             QIMsi= 0.;  QIMgi= 0.;   QCLri= 0.;   QCLir= 0.
             NVDvi= 0.;  NCLir= 0.;   NIMsi= 0.
             NiCNis=0.;  NsCNis=0.;   NIMgi= 0.;   NCLri= 0.

          ENDIF
          !---------!
          !  SNOW:  !
          !---------!
          IF (QN(i,k)>epsQ) THEN

            !Deposition/sublimation:
             !note: - snow crystal capacitance is implicitly C = 0.5*D*capFact_s
             !      - No_s and VENTs are computed above
!             QVDvs = dt*capFact_s*iABi*(PI2*(Si-1.)*No_s*VENTs - CHLS*CHLF/(Ka*RGASV*    &
!                     T(i,k)*T(i,k))*QCLcs*idt)
             QVDvs = dt*iDE(i,k)*capFact_s*iABi*(PI2*(Si-1.)*No_s*VENTs - CHLS*CHLF/(Ka* &
                     RGASV*T(i,k)**2)*QCLcs*idt)

             ! Prevent overdepletion of vapor:
             VDmax = (Q(i,k)-QSI(i,k))/(1.+ck6*(QSI(i,k))/(T(i,k)-7.66)**2)
             if(Si>=1.) then
                QVDvs= min(max(QVDvs,0.),VDmax)
             else
                if (VDmax<0.) QVDvs= max(QVDvs,VDmax)
                !IF prevents subl.(QVDvs<0 at t) changing to dep.(VDmax>0 at t*)
             endif
             NVDvs= -min(0.,(NN(i,k)*iQN)*QVDvs)  !pos. quantity

             ! Conversion to graupel:
             if (QCLcs>0. .and. QCLcs>CNsgThres*QVDvs .and. grpl_ON) then
                tmp1 = 100.  !tuning factor: controls amount of mass either added or removed
                             !from snow (QN) during partial conversion to graupel. [If QCLcs/QN > 1/tmp1,
                             !then some snow mass will be converted to graupel (in addition to rime mass).]
                QCNsg = min( QN(i,k)+QCLcs, QCLcs*(tmp1*QCLcs/QN(i,k)) )
               !calculate NCNsg: [explicit logic]
               !mgo   = DE(i,k)*(QN(i,k)+QCLsg)/NN(i,k)  !mean-mass of new graupel
               !NCNsg = DE(i,k)*QCNsg/mgo
               !calculate NCNsg: [optimized; substituting mgo and factoring]
                NCNsg = DE(i,k)*QCNsg/(QN(i,k)+QCLcs)
             else
                QCNsg = 0.
                NCNsg = 0.
             endif

             ! 3-component freezing (collisions with rain):
              if (QR(i,k)>epsQ .and. QN(i,k)>epsQ .and. Tc<-5.) then
                tmp1 = vs0-vr0
                tmp2 = sqrt(tmp1*tmp1+0.04*vs0*vr0)
                tmp6 = iLAMs2*iLAMs2*iLAMs

                QCLrs= dt*cmr*Ers*PIov4*iDE(i,k)*NN(i,k)*NR(i,k)*iGR31*iGS31*tmp2*       &
                       (GR36*GS31*iLAMr5+2.*GR35*GS32*iLAMr4*iLAMs+GR34*GS33*iLAMr3*     &
                       iLAMs2)

                NCLrs= dt*0.25e0*PI*Ers*(NN(i,k)*NR(i,k))*iGR31*iGS31*tmp2*(GR33*        &
                       GS31*iLAMr2+2.*GR32*GS32*iLAMr*iLAMs+GR31*GS33*iLAMs2)

                if (snowSpherical) then
                  !hardcoded for dms=3:
                   QCLsr= dt*cms*Ers*PIov4*iDE(i,k)*(NR(i,k)*NN(i,k))*iGS31*iGR31*       &
                          tmp2*(GS36*GR31*tmp6+2.*GS35*GR32*iLAMs2*iLAMs2*iLAMr+GS34*    &
                          GR33*iLAMs2*iLAMs*iLAMr2)
                else
                  !hardcoded for dms=2:
                   QCLsr= dt*cms*iDE(i,k)*PI*0.25*ERS*tmp2*NN(i,k)*NR(i,k)*iGS31*        &
                          iGR31*(GR33*GS33*iLAMr**2.*iLAMs**2. + 2.*GR32*GS34*iLAMr*     &
                          iLAMs**3. +GR31*GS35*iLAMs**4.)
                endif

               !note: For explicit eqns, NCLsr = NCLrs
                NCLsr= min(QCLsr*(NN(i,k)*iQN), NCLrs)
                QCLrs= min(QCLrs, QR(i,k));  QCLsr= min(QCLsr, QN(i,k))
                NCLrs= min(NCLrs, NR(i,k));  NCLsr= min(NCLsr, NN(i,k))

                ! Determine destination of 3-comp.freezing:
                Dsrs= 0.;   Dsrg= 0.;    Dsrh= 0.
                tmp1= max(Ds,Dr)
                tmp2= tmp1*tmp1*tmp1
                dey = (des*Ds*Ds*Ds + dew*Dr*Dr*Dr)/tmp2
                if (dey<=0.5*(des+deg)                        ) Dsrs= 1.  !snow
                if (dey >0.5*(des+deg) .and. dey<0.5*(deg+deh)) Dsrg= 1.  !graupel
                if (dey>=0.5*(deg+deh)) then
                   Dsrh= 1.                                               !hail
                   if (.not.hail_ON .or. Dr<Dr_3cmpThrs) then
                      Dsrg= 1.;   Dsrh= 0.                                !graupel
                   endif
                endif
                if (.not. grpl_ON) Dsrg=0.

             else
                QCLrs= 0.;   QCLsr= 0.;   NCLrs= 0.;   NCLsr= 0.
             endif

          ELSE

             QVDvs= 0.;  QCLcs= 0.;  QCNsg= 0.;  QCLsr= 0.;  QCLrs= 0.
             NVDvs= 0.;  NCLcs= 0.;  NCLsr= 0.;  NCLrs= 0.;  NCNsg= 0.

          ENDIF
          !------------!
          !  GRAUPEL:  !
          !------------!
          IF (QG(i,k)>epsQ .and. NG(i,k)>epsN) THEN

           !Conversion to hail:    (D_sll given by S-L limit)
             if ( (QCLcg+QCLrg)>0. .and. hail_ON ) then
!               D_sll = 0.01*(exp(min(20.,-Tc/(1.1e4*DE(i,k)*(QC(i,k)+QR(i,k))-1.3e3*DE(i,k)*QI(i,k)+1.)))-1.)
!               D_sll = 2.0*D_sll !correction factor [error Ziegler (1985), as per Young (1993)]
!               D_sll = 2.*0.01*(exp(min(20.,-Tc/(1.1e4*DE(i,k)*(QC(i,k)+QR(i,k)) + 1.)))-1.)
                tmp1  = 1.1e4*DE(i,k)*(QC(i,k)+QR(i,k)) + 1.
                tmp1  = max(1.,tmp1)  !to prevent div-by-zero
                D_sll = 2.*0.01*(exp(min(20.,-Tc/tmp1))-1.)
                D_sll = min(1., max(0.0001,D_sll))    !smallest D_sll=0.1mm; largest=1m
                tmp1  = iLAMg !hold value
                tmp2  = No_g  !hold value
               !estimate PSD after accretion (and before conversion to hail): (assume inverse-exponential)
                tmp3  = QG(i,k) + QCLcg + QCLrg
                iLAMg = exp(thrd*log(DE(i,k)*tmp3/(NG(i,k)*6*cmg)))
                No_g  = NG(i,k)/iLAMg
                tmp4  = exp(-D_sll/iLAMg)
                Ng_tail = No_g*iLAMg*tmp4
                if (Ng_tail > Ngh_crit) then
                   NgCNgh= min(NG(i,k), Ng_tail)
                   QCNgh = min(QG(i,k), cmg*No_g*tmp4*(D_sll**3*iLAMg + 3.*D_sll**2*    &
                                         iLAMg**2 + 6.*D_sll*iLAMg**3 + 6.*iLAMg**4) )
                   Rz= 1.
                   ! The Rz factor (/=1) serves to conserve reflectivity when graupel
                   ! converts to hail with a a different shape parameter, alpha.
                   ! (See Ferrier, 1994 App. D).
                   NhCNgh = Rz*NgCNgh
                else
                   QCNgh  = 0.
                   NgCNgh = 0.
                   NhCNgh = 0.
                endif
                iLAMg = tmp1  !restore value
                No_g  = tmp2  !restore value
             endif

          !3-component freezing (collisions with rain):
!            if (QR(i,k)>epsQ) then
             if (QR(i,k)>epsQ .and. Tc<-5.) then
                tmp1 = vg0-vr0
                tmp2 = sqrt(tmp1*tmp1 + 0.04*vg0*vr0)
                tmp8 = iLAMg2*iLAMg      ! iLAMg**3
                tmp9 = tmp8*iLAMg        ! iLAMg**4
                tmp10= tmp9*iLAMg        ! iLAMg**5

               !(parameterization of Erg based on Cober and List, 1993 [JAS])
                Kstoke = dew*abs(vg0-vr0)*Dr*Dr/(9.*MUdyn*Dg)
                Kstoke = max(1.5,min(10.,Kstoke))
                Erg    = 0.55*log10(2.51*Kstoke)

                QCLrg= dt*cmr*Erg*PIov4*iDE(i,k)*(NG(i,k)*NR(i,k))*iGR31*iGG31*tmp2*     &
                       (GR36*GG31*iLAMr5+2.*GR35*GG32*iLAMr4*iLAMg+GR34*GG33*iLAMr3*     &
                       iLAMg2)

                NCLrg= dt*PIov4*Erg*(NG(i,k)*NR(i,k))*iGR31*iGG31*tmp2*(GR33*GG31*       &
                       iLAMr2+2.*GR32*GG32*iLAMr*iLAMg+GR31*GG33*iLAMg2)

                QCLgr= dt*cmg*Erg*PIov4*iDE(i,k)*(NR(i,k)*NG(i,k))*iGG31*iGR31*tmp2*     &
                       (GG36*GR31*tmp10+2.*GG35*GR32*tmp9*iLAMr+GG34*GR33*tmp8*iLAMr2)

               !(note: For explicit eqns, NCLgr= NCLrg)
                NCLgr= min(NCLrg, QCLgr*(NG(i,k)*iQG))
                QCLrg= min(QCLrg, QR(i,k));  QCLgr= min(QCLgr, QG(i,k))
                NCLrg= min(NCLrg, NR(i,k));  NCLgr= min(NCLgr, NG(i,k))

               ! Determine destination of 3-comp.freezing:
                tmp1= max(Dg,Dr)
                tmp2= tmp1*tmp1*tmp1
                dey = (deg*Dg*Dg*Dg + dew*Dr*Dr*Dr)/tmp2
                if (dey>0.5*(deg+deh) .and. Dr>Dr_3cmpThrs .and. hail_ON) then
                   Dgrg= 0.;  Dgrh= 1.
                else
                   Dgrg= 1.;  Dgrh= 0.
                endif
             else
                QCLgr= 0.;  QCLrg= 0.;  NCLgr= 0.;  NCLrg= 0.
             endif

          ELSE

             QCNgh= 0.;  QCLgr= 0.;  QCLrg= 0.;  NgCNgh= 0.
             NhCNgh= 0.; NCLgr= 0.;  NCLrg= 0.

          ENDIF
          !---------!
          !  HAIL:  !
          !---------!
          IF (QH(i,k)>epsQ) THEN

            !Wet growth:
             if (QHwet<(QCLch+QCLrh+QCLih+QCLsh) .and. Tc>-40.) then
                QCLih= min(QCLih*iEih, QI(i,k))  !change Eih to 1. in CLih
                NCLih= min(NCLih*iEih, NY(i,k))  !  "    "
                QCLsh= min(QCLsh*iEsh, QN(i,k))  !change Esh to 1. in CLsh
                NCLsh= min(NCLsh*iEsh, NN(i,k))  !  "    "
                tmp3 = QCLrh
                QCLrh= QHwet-(QCLch+QCLih+QCLsh)  !actual QCLrh minus QSHhr
                QSHhr= tmp3-QCLrh                 !QSHhr used here only
                NSHhr= DE(i,k)*QSHhr/(cmr*Drshed*Drshed*Drshed)
             else
                NSHhr= 0.
             endif
          ELSE
             NSHhr= 0.
          ENDIF

       ENDIF  ! ( if Tc<0C Block )

     !----- Prevent mass transfer from accretion during melting:  ---!
     ! (only if exepcted NX (X=s,g,h) < 0 after source/sinks added)
     ! The purpose is to prevent mass tranfer from cloud to X and then
     ! to vapor (during "prevent overdepletion" code) if all of X
     ! would otherwise be completely depleted (e.g. due to melting).

      !estimate NN after S/S:
       tmp1= NN(i,k) +NsCNis -NVDvs -NCNsg -NMLsr -NCLss -NCLsr -NCLsh +NCLsrs
       if (tmp1<epsN) then
          QCLcs = 0.
          NCLcs = 0.
          QCLrs = 0.
          NCLrs = 0.
          if (Dsrs==1.) then
             QCLrs = 0.
             QCLsr = 0.
           endif
        endif
      !estimate NG after S/S:
       tmp2= NG(i,k) +NCNsg -NCLgr -NVDvg -NMLgr +NCLirg +NCLsrg +NCLgrg -NgCNgh
       if (tmp2<epsN) then
          QCLcg = 0.
          NCLcg = 0.
          QCLrg = 0.
          NCLrg = 0.
          if (Dirg==1.) then
             QCLri = 0.
             QCLir = 0.
          endif
          if (Dsrg==1.) then
             QCLrs = 0.
             QCLsr = 0.
          endif
       endif
      !estimate NH after S/S:
       tmp3= NH(i,k) +NhFZrh +NhCNgh -NMLhr -NVDvh +NCLirh +NCLsrh +NCLgrh
       if (tmp3<epsN) then
          QCLch = 0.
          NCLch = 0.
          QCLrh = 0.
          NCLrh = 0.
          if (Dirh==1.) then
             QCLri = 0.
             QCLir = 0.
          endif
          if (Dsrh==1.) then
             QCLrs = 0.
             QCLsr = 0.
          endif
       endif
     !=====

     !------------  End of source/sink term calculation  -------------!

     !-- Adjustment of source/sink terms to prevent  overdepletion: --!
       do niter= 1,2

          ! (1) Vapor:
          source= Q(i,k) +dim(-QVDvi,0.)+dim(-QVDvs,0.)+dim(-QVDvg,0.)+dim(-QVDvh,0.)
          sink  = QNUvi+dim(QVDvi,0.)+dim(QVDvs,0.)
          sour  = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             QNUvi= ratio*QNUvi;   NNUvi= ratio*NNUvi
             if(QVDvi>0.) then
               QVDvi= ratio*QVDvi; NVDvi= ratio*NVDvi
             endif
             if(QVDvs>0.) then
               QVDvs=ratio*QVDvs;  NVDvs=ratio*NVDvs
             endif
             QVDvg= ratio*QVDvg;   NVDvg= ratio*NVDvg
             QVDvh= ratio*QVDvh;   NVDvh= ratio*NVDvh
          endif

          ! (2) Cloud:
          source= QC(i,k)
          sink  = QCLcs+QCLcg+QCLch+QFZci
          sour  = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             QFZci= ratio*QFZci;   NFZci= ratio*NFZci
             QCLcs= ratio*QCLcs;   NCLcs= ratio*NCLcs
             QCLcg= ratio*QCLcg;   NCLcg= ratio*NCLcg
             QCLch= ratio*QCLch;   NCLch= ratio*NCLch
          endif

          ! (3) Rain:
          source= QR(i,k)+QMLsr+QMLgr+QMLhr+QMLir
          sink  = QCLri+QCLrs+QCLrg+QCLrh+QFZrh
          sour  = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             QCLrg= ratio*QCLrg;   QCLri= ratio*QCLri;   NCLri= ratio*NCLri
             QCLrs= ratio*QCLrs;   NCLrs= ratio*NCLrs
             NCLrg= ratio*NCLrg;   QCLrh= ratio*QCLrh;   NCLrh= ratio*NCLrh
             QFZrh= ratio*QFZrh;   NrFZrh=ratio*NrFZrh;  NhFZrh=ratio*NhFZrh
             if (ratio==0.) then
                Dirg= 0.; Dirh= 0.; Dgrg= 0.; Dgrh= 0.
                Dsrs= 0.; Dsrg= 0.; Dsrh= 0.
              endif
          endif

          ! (4) Ice:
          source= QI(i,k)+QNUvi+dim(QVDvi,0.)+QFZci
          sink  = QCNis+QCLir+dim(-QVDvi,0.)+QCLis+QCLig+QCLih+QMLir
          sour  = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             QMLir= ratio*QMLir;    NMLir= ratio*NMLir
             if (QVDvi<0.) then
                QVDvi= ratio*QVDvi; NVDvi= ratio*NVDvi
             endif
             QCNis=  ratio*QCNis;   NiCNis= ratio*NiCNis;   NsCNis= ratio*NsCNis
             QCLir=  ratio*QCLir;   NCLir=  ratio*NCLir;    QCLig=  ratio*QCLig
             QCLis=  ratio*QCLis;   NCLis=  ratio*NCLis
             QCLih=  ratio*QCLih;   NCLih=  ratio*NCLih
             if (ratio==0.) then
                Dirg= 0.; Dirh= 0.
             endif
          endif

          ! (5) Snow:
          source= QN(i,k)+QCNis+dim(QVDvs,0.)+QCLis+Dsrs*(QCLrs+QCLsr)+QCLcs
          sink  = dim(-QVDvs,0.)+QCNsg+QMLsr+QCLsr+QCLsh
          sour  = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             if(QVDvs<=0.) then
                QVDvs= ratio*QVDvs;   NVDvs= ratio*NVDvs
             endif
             QCNsg= ratio*QCNsg;   NCNsg= ratio*NCNsg;   QMLsr= ratio*QMLsr
             NMLsr= ratio*NMLsr;   QCLsr= ratio*QCLsr;   NCLsr= ratio*NCLsr
             QCLsh= ratio*QCLsh;   NCLsh= ratio*NCLsh
             if (ratio==0.) then
                Dsrs= 0.; Dsrg= 0.; Dsrh= 0.
             endif
          endif

          !  (6) Graupel:
          source= QG(i,k)+QCNsg+dim(QVDvg,0.)+Dirg*(QCLri+QCLir)+Dgrg*(QCLrg+QCLgr)+     &
                  QCLcg+Dsrg*(QCLrs+QCLsr)+QCLig
          sink  = dim(-QVDvg,0.)+QMLgr+QCNgh+QCLgr
          sour  = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             QVDvg= ratio*QVDvg;   NVDvg= ratio*NVDvg;   QMLgr = ratio*QMLgr
             NMLgr= ratio*NMLgr;   QCNgh= ratio*QCNgh;   NgCNgh= ratio*NgCNgh
             QCLgr= ratio*QCLgr;   NCLgr= ratio*NCLgr;   NhCNgh= ratio*NhCNgh
             if (ratio==0.) then
                Dgrg= 0.; Dgrh= 0.
             endif
          endif

          !  (7) Hail:
          source= QH(i,k)+dim(QVDvh,0.)+QCLch+QCLrh+Dirh*(QCLri+QCLir)+QCLih+QCLsh+      &
                  Dsrh*(QCLrs+QCLsr)+QCNgh+Dgrh*(QCLrg+QCLgr)+QFZrh
          sink  = dim(-QVDvh,0.)+QMLhr
          sour  = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             QVDvh= ratio*QVDvh;   NVDvh= ratio*NVDvh
             QMLhr= ratio*QMLhr;   NMLhr= ratio*NMLhr
          endif

       enddo
       !---------------  End of source/sink term adjustment  ------------------!

      !Compute N-tendencies for destination categories of 3-comp.freezing:
       NCLirg= 0.;  NCLirh= 0.;  NCLsrs= 0.;  NCLsrg= 0.
       NCLsrh= 0.;  NCLgrg= 0.;  NCLgrh= 0.

       if (QCLir+QCLri>0.) then
          tmp1  = max(Dr,Di)
          tmp2  = tmp1*tmp1*tmp1*PIov6
          NCLirg= Dirg*DE(i,k)*(QCLir+QCLri)/(deg*tmp2)
          NCLirh= Dirh*DE(i,k)*(QCLir+QCLri)/(deh*tmp2)
       endif

       if (QCLsr+QCLrs>0.) then
          tmp1  = max(Dr,Ds)
          tmp2  = tmp1*tmp1*tmp1*PIov6
          NCLsrs= Dsrs*DE(i,k)*(QCLsr+QCLrs)/(des*tmp2)
          NCLsrg= Dsrg*DE(i,k)*(QCLsr+QCLrs)/(deg*tmp2)
          NCLsrh= Dsrh*DE(i,k)*(QCLsr+QCLrs)/(deh*tmp2)
       endif

       if (QCLgr+QCLrg>0.) then
          tmp1  = max(Dr,Dg)
          tmp2  = tmp1*tmp1*tmp1*PIov6
          NCLgrg= Dgrg*DE(i,k)*(QCLgr+QCLrg)/(deg*tmp2)
          NCLgrh= Dgrh*DE(i,k)*(QCLgr+QCLrg)/(deh*tmp2)
       endif

       !========================================================================!
       !           Add all source/sink terms to all predicted moments:          !
       !========================================================================!

      !Diagnostic S/S terms:  (to facilitate output of 3D variables for diagnostics)
      !e.g. SS(i,k,1)= QVDvs*idt  (for depositional growth rate of snow, kg kg-1 s-1)

       ! Q-Source/Sink Terms:
       Q(i,k) = Q(i,k)  -QNUvi -QVDvi -QVDvs -QVDvg -QVDvh
       QC(i,k)= QC(i,k) -QCLcs -QCLcg -QCLch -QFZci
       QR(i,k)= QR(i,k) -QCLri +QMLsr -QCLrs -QCLrg +QMLgr -QCLrh +QMLhr -QFZrh +QMLir
       QI(i,k)= QI(i,k) +QNUvi +QVDvi +QFZci -QCNis -QCLir -QCLis -QCLig                 &
                        -QMLir -QCLih +QIMsi +QIMgi
       QG(i,k)= QG(i,k) +QCNsg +QVDvg +QCLcg -QCLgr-QMLgr -QCNgh -QIMgi +QCLig           &
                        +Dirg*(QCLri+QCLir) +Dgrg*(QCLrg+QCLgr) +Dsrg*(QCLrs+QCLsr)
       QN(i,k)= QN(i,k) +QCNis +QVDvs +QCLcs -QCNsg -QMLsr -QIMsi -QCLsr +QCLis -QCLsh   &
                        +Dsrs*(QCLrs+QCLsr)
       QH(i,k)= QH(i,k) +Dirh*(QCLri+QCLir) -QMLhr +QVDvh +QCLch +Dsrh*(QCLrs+QCLsr)     &
                        +QCLih +QCLsh +QFZrh +QCLrh +QCNgh +Dgrh*(QCLrg+QCLgr)

       ! N-Source/Sink Terms:
       NC(i,k)= NC(i,k) -NCLcs -NCLcg -NCLch -NFZci
       NR(i,k)= NR(i,k) -NCLri -NCLrs -NCLrg -NCLrh +NMLsr +NMLgr +NMLhr -NrFZrh +NMLir  &
                        +NSHhr
       NY(i,k)= NY(i,k) +NNUvi +NVDvi +NFZci -NCLir -NCLis -NCLig -NCLih -NMLir +NIMsi   &
                        +NIMgi -NiCNis
       NN(i,k)= NN(i,k) +NsCNis -NVDvs -NCNsg -NMLsr -NCLss -NCLsr -NCLsh +NCLsrs
       NG(i,k)= NG(i,k) +NCNsg -NCLgr -NVDvg -NMLgr +NCLirg +NCLsrg +NCLgrg -NgCNgh
       NH(i,k)= NH(i,k) +NhFZrh +NhCNgh -NMLhr -NVDvh +NCLirh +NCLsrh +NCLgrh

       T(i,k)= T(i,k)   +LFP*(QCLri+QCLcs+QCLrs+QFZci-QMLsr+QCLcg+QCLrg-QMLir-QMLgr      &
                        -QMLhr+QCLch+QCLrh+QFZrh) +LSP*(QNUvi+QVDvi+QVDvs+QVDvg+QVDvh)

!--- Ensure consistency between moments: (prescribe NX (and BG) in case of inconsistency)
         tmp1= pres(i,k)/(RGASD*T(i,k))    !updated air density
         !note: In Part 2a (warm-rain coalescence), air density at initial time (before
         !      modification of T due to ice-phase) is used; hence, DE is not recomputed here.

        !cloud:
         if (QC(i,k)>epsQ .and. NC(i,k)<epsN) then
             NC(i,k)= N_c_SM
         elseif (QC(i,k)<=epsQ) then
            Q(i,k)  = Q(i,k) + QC(i,k)
            T(i,k)  = T(i,k) - LCP*QC(i,k)
            QC(i,k) = 0.
            NC(i,k) = 0.
         endif

        !rain
         if (QR(i,k)>epsQ .and. NR(i,k)<epsN) then
            NR(i,k)= (No_r_SM*GR31)**(3./(4.+alpha_r))*(GR31*iGR34*tmp1*QR(i,k)*         &
                     icmr)**((1.+alpha_r)/(4.+alpha_r))
         elseif (QR(i,k)<=epsQ) then
            Q(i,k)  = Q(i,k) + QR(i,k)
            T(i,k)  = T(i,k) - LCP*QR(i,k)
            QR(i,k) = 0.
            NR(i,k) = 0.
         endif

        !ice:
         if (QI(i,k)>epsQ .and. NY(i,k)<epsN) then
         !   NY(i,k)= N_Cooper(TRPL,T(i,k))
           NY(i,k) = max(2.*epsN, N_Cooper(TRPL,T(i,k)) )
         elseif (QI(i,k)<=epsQ) then
            Q(i,k)  = Q(i,k) + QI(i,k)
            T(i,k)  = T(i,k) - LSP*QI(i,k)
            QI(i,k) = 0.
            NY(i,k) = 0.
         endif

        !snow:
         if (QN(i,k)>epsQ .and. NN(i,k)<epsN) then
            No_s= Nos_Thompson(TRPL,T(i,k))
            NN(i,k)= (No_s*GS31)**(dms*icexs2)*(GS31*iGS40*icms*tmp1*QN(i,k))**((1.+     &
                     alpha_s)*icexs2)
         elseif (QN(i,k)<=epsQ) then
            Q(i,k)  = Q(i,k) + QN(i,k)
            T(i,k)  = T(i,k) - LSP*QN(i,k)
            QN(i,k) = 0.
            NN(i,k) = 0.
         endif

        !grpl:
         if (QG(i,k)>epsQ .and. NG(i,k)<epsN) then
            NG(i,k) = (No_g_SM*GG31)**(3./(4.+alpha_g))*(GG31*iGG34*tmp1*QG(i,k)*        &
                      icmg)**((1.+alpha_g)/(4.+alpha_g))
         elseif (QG(i,k)<=epsQ) then
            Q(i,k)  = Q(i,k) + QG(i,k)
            T(i,k)  = T(i,k) - LSP*QG(i,k)
            QG(i,k) = 0.
            NG(i,k) = 0.
         endif

        !hail:
         if (QH(i,k)>epsQ .and. NH(i,k)<epsN) then
            NH(i,k)= (No_h_SM*GH31)**(3./(4.+alpha_h))*(GH31*iGH34*tmp1*QH(i,k)*         &
                     icmh)**((1.+alpha_h)/(4.+alpha_h))
         elseif (QH(i,k)<=epsQ) then
            Q(i,k)  = Q(i,k) + QH(i,k)
            T(i,k)  = T(i,k) - LSP*QH(i,k)
            QH(i,k) = 0.
            NH(i,k) = 0.
         endif
         if (QH(i,k)>epsQ .and. NH(i,k)>epsN) then
          !transfer small hail to graupel:
            tmp1 = 1./NH(i,k)
            Dh   = Dm_x(DE(i,k),QH(i,k),tmp1,icmh,thrd)
            if (Dh < Dh_min) then
               QG(i,k) = QG(i,k) + QH(i,k)
               NG(i,k) = NG(i,k) + NH(i,k)
               QH(i,k) = 0.
               NH(i,k) = 0.
            endif
         endif
!===
         Q(i,k)= max(Q(i,k),0.)
         NY(i,k)= min(NY(i,k), Ni_max)

!-----
  if ( T(i,k)<173. .or. T(i,k)>323.) then                            !** DEBUG **
     print*, '** STOPPING IN MICROPHYSICS: (Part 2, end) **'         !** DEBUG **
     print*, '** i,k,T [K]: ',i,k,T(i,k)                             !** DEBUG **
     stop                                                            !** DEBUG **
  endif                                                              !** DEBUG **
!=====

      ENDIF  !if (activePoint)
    ENDDO
  ENDDO

  !----------------------------------------------------------------------------------!
  !                    End of ice phase microphysics (Part 2)                        !
  !----------------------------------------------------------------------------------!

  if (DEBUG_ON) call check_values(Q,T,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH,epsQ,epsN,.true.,DEBUG_abort,450)

  !----------------------------------------------------------------------------------!
  !                       PART 3: Warm Microphysics Processes                        !
  !                                                                                  !
  !  Equations for warm-rain coalescence based on Cohard and Pinty (2000a,b; QJRMS)  !
  !  Condensation/evaportaion equations based on Kong and Yau (1997; Atmos-Ocean)    !
  !  Equations for rain reflectivity (ZR) based on Milbrandt and Yau (2005b; JAS)    !
  !----------------------------------------------------------------------------------!

  ! Part 3a - Warm-rain Coallescence:

 IF (warmphase_ON) THEN

  DO k= ktop-kdir,kbot,-kdir
     DO i= 1,ni

        RCAUTR= 0.;  CCACCR= 0.;  Dc= 0.;  iLAMc= 0.;  L  = 0.
        RCACCR= 0.;  CCSCOC= 0.;  Dr= 0.;  iLAMr= 0.;  TAU= 0.
        CCAUTR= 0.;  CRSCOR= 0.;  SIGc= 0.;  DrINIT= 0.
        iLAMc3= 0.;  iLAMc6= 0.;  iLAMr3= 0.;  iLAMr6= 0.

        rainPresent= (QR_in(i,k)>epsQ .and. NR_in(i,k)>epsN)

        if (QC_in(i,k)>epsQ .and. NC_in(i,k)>epsN) then
           iNC = 1./NC_in(i,k)
           iLAMc = iLAMDA_x(DE(i,k),QC_in(i,k),iNC,icexc9,thrd)
           iLAMc3= iLAMc*iLAMc*iLAMc
           iLAMc6= iLAMc3*iLAMc3
           Dc    = iLAMc*(GC2*iGC1)**thrd
           SIGc  = iLAMc*( GC3*iGC1- (GC2*iGC1)*(GC2*iGC1) )**sixth
           L     = 0.027*DE(i,k)*QC_in(i,k)*(6.25e18*SIGc*SIGc*SIGc*Dc-0.4)
           if (SIGc>SIGcTHRS) TAU= 3.7/(DE(i,k)*(QC_in(i,k))*(0.5e6*SIGc-7.5))
        endif

        if (rainPresent) then
           iNR = 1./NR_in(i,k)
           Dr = Dm_x(DE(i,k),QR_in(i,k),iNR,icmr,thrd)

          !Drop-size limiter [prevents initially large drops]
            if (Dr>3.e-3) then
              tmp1 = (Dr-3.e-3)
              tmp2 = (Dr/DrMAX)
              tmp3 = tmp2*tmp2*tmp2
              NR_in(i,k)= NR_in(i,k)*max((1.+2.e4*tmp1*tmp1),tmp3)
              iNR  = 1./NR_in(i,k)
              Dr   = Dm_x(DE(i,k),QR_in(i,k),iNR,icmr,thrd)
           endif
           iLAMr  = iLAMDA_x(DE(i,k),QR_in(i,k),iNR,icexr9,thrd)
           iLAMr3 = iLAMr*iLAMr*iLAMr
           iLAMr6 = iLAMr3*iLAMr3
        endif

        !  Autoconversion:
        if (QC_in(i,k)>epsQ .and. SIGc>SIGcTHRS .and. autoconv_ON) then
           RCAUTR= min( max(L/TAU,0.), QC(i,k)*idt )
           DrINIT= max(83.e-6, 12.6e-4/(0.5e6*SIGc-3.5))  !initiation regime Dr
           DrAUT = max(DrINIT, Dr)                     !init. or feeding DrAUT
           CCAUTR= RCAUTR*DE(i,k)/(cmr*DrAUT*DrAUT*DrAUT)

           ! ---------------------------------------------------------------------------- !
           ! NOTE: The formulation for CCAUTR here (dNr/dt|initiation) does NOT follow
           !       eqn (18) in CP2000a, but rather it comes from the F90 code provided
           !       by J-P Pinty (subroutine: 'rain_c2r2.f90').
           !       (See notes: 2001-10-17; 2001-10-22)
           !
           !       Similarly, the condition for the activation of accretion and self-
           !       collection depends on whether or not autoconversion is in the feeding
           !       regime (see notes 2002-01-07).  This is apparent in the F90 code, but
           !       NOT in CP2000a.
           ! ---------------------------------------------------------------------------- !

           ! cloud self-collection: (dNc/dt_autoconversion)   {CP eqn(25)}
           CCSCOC= min(KK2*NC_in(i,k)*NC_in(i,k)*GC3*iGC1*iLAMc6, NC_in(i,k)*idt)  !{CP00a eqn(25)}
        endif

        ! Accretion, rain self-collection, and collisional breakup:
        if (((QR_in(i,k))>1.2*max(L,0.)*iDE(i,k).or.Dr>max(5.e-6,DrINIT)).and.rainAccr_ON  &
             .and. rainPresent) then

           !  Accretion:                                                      !{CP00a eqn(22)}
           if (QC_in(i,k)>epsQ.and.L>0.) then
              if (Dr.ge.100.e-6) then
                 CCACCR = KK1*(NC_in(i,k)*NR_in(i,k))*(GC2*iGC1*iLAMc3+GR34*iGR31*iLAMr3)
                 RCACCR = cmr*iDE(i,k)*KK1*(NC_in(i,k)*NR_in(i,k))*iLAMc3*(GC3*iGC1*iLAMc3+  &
                          GC2*iGC1*GR34*iGR31*iLAMr3)
              else
                 CCACCR = KK2*(NC_in(i,k)*NR_in(i,k))*(GC3*iGC1*iLAMc6+GR37*iGR31*iLAMr6)

!                  RCACCR= cmr*iDE(i,k)*KK2*(NC(i,k)*NR(i,k))*iLAMc3*                  &
!                          (GC4*iGR31*iLAMc6+GC2*iGC1*GR37*iGR31*iLAMr6)
!++  The following calculation of RCACCR avoids overflow:
                 tmp1   = cmr*iDE(i,k)
                 tmp2   = KK2*(NC_in(i,k)*NR_in(i,k))*iLAMc3
                 RCACCR = tmp1 * tmp2
                 tmp1   = GC4*iGR31
                 tmp1   = (tmp1)*iLAMc6
                 tmp2   = GC2*iGC1
                 tmp2   = tmp2*GR37*iGR31
                 tmp2   = (tmp2)*iLAMr6
                 RCACCR = RCACCR * (tmp1 + tmp2)
!++
              endif
              CCACCR = min(CCACCR,(NC(i,k))*idt)
              RCACCR = min(RCACCR,(QC(i,k))*idt)
            endif

         !Rain self-collection:
           tmp1= NR_in(i,k)*NR_in(i,k)
           if (Dr.ge.100.e-6) then
              CRSCOR= KK1*tmp1*GR34*iGR31*iLAMr3                        !{CP00a eqn(24)}
           else
              CRSCOR= KK2*tmp1*GR37*iGR31*iLAMr6                        !{CP00a eqn(25)}
           endif
         !Raindrop breakup:                                             !{CP00a eqn(26)}
           Ec= 1.
!          if (Dr > 300.e-6) Ec = 2.-exp(2300.*(Dr-300.e-6))
           if (iLAMr > 300.e-6) Ec = 2.-exp(2300.*(iLAMr-300.e-6))  !(assumes alpha_r=0)

           CRSCOR= min(Ec*CRSCOR,(0.5*NR(i,k))*idt) !0.5 prevents depletion of NR

        endif  !accretion/self-collection/breakup

        ! Prevent overdepletion of cloud:
        source= QC(i,k)
        sink  = (RCAUTR+RCACCR)*dt
        if (sink>source) then
           ratio = source/sink
           RCAUTR= ratio*RCAUTR
           RCACCR= ratio*RCACCR
           CCACCR= ratio*CCACCR
        endif

        ! Apply tendencies:
        QC(i,k)= max(0., QC(i,k)+(-RCAUTR-RCACCR)*dt )
        QR(i,k)= max(0., QR(i,k)+( RCAUTR+RCACCR)*dt )
        NC(i,k)= max(0., NC(i,k)+(-CCACCR-CCSCOC)*dt )
        NR(i,k)= max(0., NR(i,k)+( CCAUTR-CRSCOR)*dt )

        if (QR(i,k)>epsQ .and. NR(i,k)>epsN) then
           iNR = 1./NR(i,k)
           Dr  = Dm_x(DE(i,k),QR(i,k),iNR,icmr,thrd)
           if (Dr>3.e-3) then
              tmp1= (Dr-3.e-3);   tmp2= tmp1*tmp1
              tmp3= (Dr/DrMAX);   tmp4= tmp3*tmp3*tmp3
              NR(i,k)= NR(i,k)*(max((1.+2.e4*tmp2),tmp4))
           elseif (Dr<Dhh) then
           !Convert small raindrops to cloud:
              QC(i,k)= QC(i,k) + QR(i,k)
              NC(i,k)= NC(i,k) + NR(i,k)
              QR(i,k)= 0.;   NR(i,k)= 0.
           endif
        else
           QR(i,k)= 0.;   NR(i,k)= 0.
        endif  !(Qr,Nr>eps)

  ! Part 3b - Condensation/Evaporation:

        QSW(i,k) = qsat(T(i,k),pres(i,k),0)              !Flatau formulation
        if (QSW(i,k)>1.e-20) then
           ssat = Q(i,k)/QSW(i,k)-1.                     !supersaturation ratio
        else
           ssat = 0.
        endif
        X       = Q(i,k)-QSW(i,k)                        !saturation excess (deficit)
        !adjustment for latent heating during cond/evap
!       X       = X/(1.+ck5*QSW(i,k)/(T(i,k)-35.86)**2)                                   !orig (KY97)
        X       = X / ( 1.+ ((3.1484e6-2370.*T(i,k))**2 * QSW(i,k))/( (1005.*(1.+       & !morr2mom
                        0.887*Q(i,k))) *461.5*T(i,k)**2 ) )
        X       = max(X, -QC(i,k))                       !ensure no overdepletion of QC
        QC(i,k) = QC(i,k) + X
        Q(i,k)  = Q(i,k)  - X
        T(i,k)  = T(i,k)  + LCP*X

        if (X>0.) then
          !nucleation of cloud droplets:
           if (WZ(i,k)>0.001) then
              !condensation and non-negligible upward motion:
               !note: WZ threshold of 1 mm/s is to overflow problem in NccnFNC, which
               !      uses a polynomial approximation that is invalid for tiny WZ.
              NC(i,k) = max(NC(i,k), NccnFNC(WZ(i,k),T(i,k),pres(i,k),CCNtype))
           else
             !condensation and negible or downward vertical motion:
              NC(i,k) = max(NC(i,k), N_c_SM)
           endif
        else
           if (QC(i,k)>epsQ) then
              !partial evaporation of cloud droplets:
              NC(i,k) = max(0., NC(i,k) + X*NC(i,k)/max(QC(i,k),epsQ) ) !(dNc/dt)|evap
           else
              NC(i,k) = 0.
           endif
        endif
        if (QC(i,k)>epsQ .and. NC(i,k)<epsN) NC(i,k)= N_c_SM !prevents non-zero_Q & zero_N

       !rain evaporation:
       !saturation adjustment:
        QSW(i,k) = qsat(T(i,k),pres(i,k),0)              !Flatau formulation
        if (Q(i,k)<QSW(i,k) .and. QR(i,k)>epsQ .and. NR(i,k)>epsN) then
           ssat    = Q(i,k)/QSW(i,k)-1.
           Tc      = T(i,k)-TRPL
           Cdiff   = max(1.62e-5, (2.2157e-5 + 0.0155e-5*Tc)) *1.e5/pres(i,k)
           MUdyn   = max(1.51e-5, (1.7153e-5 + 0.0050e-5*Tc))
           Ka      = max(2.07e-2, (2.3971e-2 + 0.0078e-2*Tc))
           MUkin   = MUdyn*iDE(i,k)
           iMUkin  = 1./MUkin
           ScTHRD  = (MUkin/Cdiff)**thrd
           X       = QSW(i,k) - Q(i,k)                      !saturation exesss(deficit)
           !adjustment for latent cooling during evaporation:
!          X       = X/(1.+ck5*QSW(i,k)/(T(i,k)-35.86)**2)                                  !orig (KY97)
           X       = X / ( 1.+ ((3.1484e6-2370.*T(i,k))**2 * QSW(i,k))/( (1005.*(1.+     &  !morr2mom
                     0.887*Q(i,k))) *461.5*T(i,k)**2 ) )
           DE(i,k)  = pres(i,k)/(RGASD*T(i,k))        !recompute air density (with updated T)
           iDE(i,k) = 1./DE(i,k)
           gam      = sqrt(DEo*iDE(i,k))
           tmp1     = 1./NR(i,k)
           iLAMr    = iLAMDA_x(DE(i,k),QR(i,k),tmp1,icexr9,thrd)
           LAMr     = 1./iLAMr
          !note: The following coding of 'No_r=...' prevents overflow:
          !No_r     = NR(i,k)*LAMr**(1.+alpha_r))*iGR31
           No_r     = sngl(dble(NR(i,k))*dble(LAMr)**dble(1.+alpha_r))*iGR31
          !note: There is an error in MY05a_eq(8) for VENTx (corrected in code)
           VENTr    = Avx*GR32*iLAMr**cexr5 + Bvx*ScTHRD*sqrt(gam*afr*iMUkin)*GR17*iLAMr**cexr6
           ABw      = CHLC**2/(Ka*RGASV*T(i,k)**2)+1./(DE(i,k)*(QSW(i,k))*Cdiff)
           QREVP    = min( QR(i,k), -dt*(iDE(i,k)*PI2*ssat*No_r*VENTr/ABw) )
           tmp1     = QR(i,k)   !value of QR before update, used in NR update eqn
           T(i,k)   = T(i,k)  - LCP*QREVP
           Q(i,k)   = Q(i,k)  + QREVP
           QR(i,k)  = QR(i,k) - QREVP
           NR(i,k)  = max(0., NR(i,k) - QREVP*NR(i,k)/tmp1)
          !Protect against negative values due to overdepletion:
           if (QR(i,k)<epsQ .or. NR(i,k)<epsN)  then
              Q(i,k)  = Q(i,k) + QR(i,k)
              T(i,k)  = T(i,k) - QR(i,k)*LCP
              QR(i,k) = 0.
              NR(i,k) = 0.
           endif
        endif

       !homogeneous freezing of cloud:
        Tc = T(i,k) - TRPL
        if (QC(i,k)>epsQ .and. Tc<-30. .and. icephase_ON) then

         !-detailed:
!            Tc    = T(i,k) - TRPL
!            JJ    = (10.**max(-20.,(-606.3952-52.6611*Tc-1.7439*Tc**2-0.0265*Tc**3-       &
!                     1.536e-4*Tc**4)))
!            tmp1  = 1.e6*(DE(i,k)*(QC(i,k)/NC(i,k))*icmr) !i.e. Dc[cm]**3
!            FRAC  = 1.-exp(-JJ*PIov6*tmp1*dt)
!            if (Tc>-30.) FRAC= 0.
!            if (Tc<-50.) FRAC= 1.
         !-simplified:
           if (Tc<-35.) then
              FRAC = 1.
           else
              FRAC = 0.
           endif
          !=
           QFZci   = FRAC*QC(i,k)
           NFZci   = FRAC*NC(i,k)
           QC(i,k) = QC(i,k) - QFZci
           NC(i,k) = NC(i,k) - NFZci
           QI(i,k) = QI(i,k) + QFZci
           NY(i,k) = NY(i,k) + NFZci
           T(i,k)  = T(i,k)  + LFP*QFZci

           if (QC(i,k)>epsQ .and. NC(i,k)<epsN) then
               NC(i,k)= N_c_SM
           elseif (QC(i,k)<=epsQ) then
              Q(i,k)  = Q(i,k) + QC(i,k)
              T(i,k)  = T(i,k) - LCP*QC(i,k)
              QC(i,k) = 0.
              NC(i,k) = 0.
           endif

           if (QI(i,k)>epsQ .and. NY(i,k)<epsN) then
           !   NY(i,k)= N_Cooper(TRPL,T(i,k))
           NY(i,k) = max(2.*epsN, N_Cooper(TRPL,T(i,k)) )
           elseif (QI(i,k)<=epsQ) then
              Q(i,k)  = Q(i,k) + QI(i,k)
              T(i,k)  = T(i,k) - LSP*QI(i,k)
              QI(i,k) = 0.
              NY(i,k) = 0.
           endif

        endif

!==

     ENDDO
  ENDDO    !cond/evap [k-loop]

 ENDIF  !if warmphase_ON

  !----------------------------------------------------------------------------------!
  !                    End of warm-phase microphysics (Part 3)                       !
  !----------------------------------------------------------------------------------!

  if (DEBUG_ON) call check_values(Q,T,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH,epsQ,epsN,.true.,DEBUG_abort,500)

  !----------------------------------------------------------------------------------!
  !                            PART 4:  Sedimentation                                !
  !----------------------------------------------------------------------------------!

 IF (sedi_ON) THEN

   fluxM_r= 0.;  fluxM_i= 0.;  fluxM_s= 0.;  fluxM_g= 0.;  fluxM_h= 0.
   RT_rn1 = 0.;  RT_rn2 = 0.;  RT_fr1 = 0.;  RT_fr2 = 0.;  RT_sn1 = 0.
   RT_sn2 = 0.;  RT_sn3 = 0.;  RT_pe1 = 0.;  RT_pe2 = 0.;  RT_peL = 0.

!---- For sedimentation on all levels:
    call sedi_wrapper_2(QR,NR,1,epsQ,epsQr_sedi,epsN,dmr,ni,VrMax,DrMax,dt,fluxM_r,kdir, &
                        kbot,ktop_sedi,GRAV,zheight,nk,DE,iDE,iDP,DZ,iDZ,gamfact,kount,  &
                        afr,bfr,cmr,ckQr1,ckQr2,icexr9)
  if (DEBUG_ON) call check_values(Q,T,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH,epsQ,epsN,.false.,DEBUG_abort,610)

    call sedi_wrapper_2(QI,NY,2,epsQ,epsQi_sedi,epsN,dmi,ni,ViMax,DiMax,dt,fluxM_i,kdir, &
                        kbot,ktop_sedi,GRAV,zheight,nk,DE,iDE,iDP,DZ,iDZ,gamfact,kount,  &
                        afi,bfi,cmi,ckQi1,ckQi2,ckQi4)

  if (DEBUG_ON) call check_values(Q,T,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH,epsQ,epsN,.false.,DEBUG_abort,620)

    call sedi_wrapper_2(QN,NN,3,epsQ,epsQs_sedi,epsN,dms,ni,VsMax,DsMax,dt,fluxM_s,kdir, &
                        kbot,ktop_sedi,GRAV,zheight,nk,DE,iDE,iDP,DZ,iDZ,gamfact,kount,  &
                        afs,bfs,cms,ckQs1,ckQs2,iGS20)

  if (DEBUG_ON) call check_values(Q,T,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH,epsQ,epsN,.false.,DEBUG_abort,630)

    call sedi_wrapper_2(QG,NG,4,epsQ,epsQg_sedi,epsN,dmg,ni,VgMax,DgMax,dt,fluxM_g,kdir, &
                        kbot,ktop_sedi,GRAV,zheight,nk,DE,iDE,iDP,DZ,iDZ,gamfact,kount,  &
                        afg,bfg,cmg,ckQg1,ckQg2,ckQg4)

  if (DEBUG_ON) call check_values(Q,T,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH,epsQ,epsN,.false.,DEBUG_abort,640)

    call sedi_wrapper_2(QH,NH,5,epsQ,epsQh_sedi,epsN,dmh,ni,VhMax,DhMax,dt,fluxM_h,kdir, &
                        kbot,ktop_sedi,GRAV,zheight,nk,DE,iDE,iDP,DZ,iDZ,gamfact,kount,  &
                        afh,bfh,cmh,ckQh1,ckQh2,ckQh4)
!====


  if (DEBUG_ON) call check_values(Q,T,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH,epsQ,epsN,.false.,DEBUG_abort,650)

!---  Impose constraints on size distribution parameters ---!
    do k= ktop,kbot,-kdir
      do i= 1,ni
       !snow:
         if (QN(i,k)>epsQ .and. NN(i,k)>epsN) then
         !Impose No_s max for snow: (assumes alpha_s=0.)
            tmp1   = 1./NN(i,k)
            iLAMs  = max( iLAMmin2, iLAMDA_x(DE(i,k),QN(i,k),tmp1,iGS20,idms) )
            tmp1   = min(NN(i,k)/iLAMs,No_s_max)                 !min. No_s
            NN(i,k)= tmp1**(dms/(1.+dms))*(iGS20*DE(i,k)*QN(i,k))**(1./(1.+dms)) !impose Nos_max
         !Impose LAMDAs_min (by increasing LAMDAs if it is below LAMDAs_min2 [2xLAMDAs_min])
            tmp1   = 1./NN(i,k)
            iLAMs  = max( iLAMmin2, iLAMDA_x(DE(i,k),QN(i,k),tmp1,iGS20,idms) )
            tmp2   = 1./iLAMs                                   !LAMs before adjustment
           !adjust value of lamdas_min to be applied:
           !  This adjusts for multiple corrections (each time step).  The factor 0.6 was obtained by
           !  trial-and-error to ultimately give reasonable LAMs profiles, smooth and with min LAMs~lamdas_min
            tmp4   = 0.6*lamdas_min
            tmp5   = 2.*tmp4
            tmp3   = tmp2 + tmp4*(max(0.,tmp5-tmp2)/tmp5)**2.   !LAMs after adjustment
            tmp3   = max(tmp3, lamdas_min)                      !final correction
            NN(i,k)= NN(i,k)*(tmp3*iLAMs)**dms                  !re-compute NN after LAMs adjustment
         endif
       enddo !i-loop
    enddo !k-loop
!===

  !Compute melted (liquid-equivalent) volume fluxes [m3 (liquid) m-2 (sfc area) s-1]:
  !  (note: For other precipitation schemes in RPN-CMC physics, this is computed in 'vkuocon6.ftn')
  RT_rn1 = fluxM_r *idew
  RT_sn1 = fluxM_i *idew
  RT_sn2 = fluxM_s *idew
  RT_sn3 = fluxM_g *idew
  RT_pe1 = fluxM_h *idew

  if (DEBUG_ON) call check_values(Q,T,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH,epsQ,epsN,.false.,DEBUG_abort,700)

!----
  !Compute sum of solid (unmelted) volume fluxes [m3 (bulk hydrometeor) m-2 (sfc area) s-1]:
  !(this is the precipitation rate for UNmelted total snow [i+s+g])

  !    Note:  In 'calcdiagn.ftn', the total solid precipitation (excluding hail) SN is computed
  !           from the sum of the liq-eq.vol fluxes, tss_sn1 + tss_sn2 + tss_sn3.  With the
  !           accumulation of SND (in 'calcdiag.ftn'), the solid-to-liquid ratio for the total
  !           accumulated "snow" (i+s+g) can be compute as SND/SN.  Likewise, the instantaneous
  !           solid-to-liquid ratio of solid precipitation is computed (in 'calcdiag.ftn') as
  !           RS2L = RSND/RSN.

   do i= 1,ni

      fluxV_i= fluxM_i(i)*idei
      fluxV_g= fluxM_g(i)*ideg
     !Compute unmelted volume flux for snow:
         ! note: This is based on the strict calculation of the volume flux, where vol=(pi/6)D^3,
         !       and remains in the integral calculation Fv = int[V(D)*vol(D)*N(D)*dD].
         !       For a constant density (ice and graupel), vol(D) = m(D)/dex, dex comes out of
         !       integral and Fv_x=Fm_x/dex
         !       Optimized for alpha_s = 0.
      if (QN(i,nk)>epsQ .and. NN(i,nk)>epsN .and. fluxM_s(i)>0.) then
         tmp2    = 1./NN(i,nk)
         tmp1    = 1./iLAMDA_x(DE(i,nk),QN(i,nk),tmp2,iGS20,idms) !LAMDA_s
         fluxV_s = fluxM_s(i)*rfact_FvFm*tmp1**(dms-3.)
      else
         fluxV_s = 0.
      endif

     !total solid unmelted volume flux, before accounting for partial melting:
      tmp1= fluxV_i + fluxV_g + fluxV_s

     !liquid-fraction of partially-melted solid precipitation:
     !  The physical premise is that if QR>0, QN+QI+QG>0, and T>0, then QR
     !  originates from melting and can be used to estimate the liquid portion
     !  of the partially-melted solid hydrometeor.
      tmp2= QR(i,nk) + QI(i,nk) + QN(i,nk) + QG(i,nk)
      if (T(i,nk)>TRPL .and. tmp2>epsQ) then
         fracLiq= QR(i,nk)/tmp2
      else
         fracLiq= 0.
      endif

     !Tend total volume flux towards the liquid-equivalent as the liquid-fraction increases to 1:
      tmp3= RT_sn1(i) + RT_sn2(i) + RT_sn3(i)      !total liquid-equivalent volume flux    (RSN,  Fv_sol)
      RT_snd(i)= (1.-fracLiq)*tmp1 + fracLiq*tmp3  !total volume flux with partial melting (RSND, Fvsle_sol)
      !Note:  Calculation of instantaneous solid-to-liquid ratio [RS2L = RSND/RSN]
      !       is based on the above quantities and is done on 'calcdiag.ftn'.

   enddo  !i-loop
!====

 !++++
 ! Diagnose surface precipitation types:
 !
 ! The following involves diagnostic conditions to determine surface precipitation rates
 ! for various precipitation elements defined in Canadian Meteorological Operational Internship
 ! Program module 3.1 (plus one for large hail) based on the sedimentation rates of the five
 ! sedimenting hydrometeor categories.
 !
 ! With the diagnostics shut off (precipDiag_ON=.false.), 5 rates are just the 5 category
 ! rates, with the other 6 rates just 0.  The model output variables will have:
 !
 !   total liquid = RT_rn1 [RAIN]
 !   total solid  = RT_sn1 [ICE] + RT_sn2 [SNOW] + RT_sn3 [GRAUPEL] + RT_pe1 [HAIL]
 !
 ! With the diagnostics on, the 5 sedimentation rates are partitioned into 9 rates,
 ! with the following model output variable:
 !
 !  total liquid = RT_rn1 [liquid rain] + RT_rn2 [liquid drizzle]
 !  total solid  = RT_fr1 [freezing rain] + RT_fr2 [freezing drizzle] + RT_sn1 [ice crystals] +
 !                 RT_sn2 [snow] + RT_sn3 [graupel] + RT_pe1 [ice pellets] + RT_pe2 [hail]
 !
 ! NOTE:  - The above total liquid/solid rates are computed in 'calcdiag.ftn' (as R2/R4).
 !        - RT_peL [large hail] is a sub-set of RT_pe2 [hail] and is thus not added to the total
 !          solid precipitation.

!   call tmg_start0(98,'mmCalcDIAG')

   IF (precipDiag_ON) THEN
      DO i= 1,ni

         DE(i,kbot)= pres(i,kbot)/(RGASD*T(i,kbot))

      !rain vs. drizzle:
         N_r= NR(i,nk)
         if (QR(i,kbot)>epsQ .and. N_r>epsN) then
            Dm_r(i,kbot)= (DE(i,nk)*icmr*QR(i,kbot)/N_r)**thrd
            if (Dm_r(i,kbot)>Dr_large) then  !Dr_large is rain/drizzle size threshold
               RT_rn2(i)= RT_rn1(i);   RT_rn1(i)= 0.
            endif
         endif

      !liquid vs. freezing rain or drizzle:
         if (T(i,nk)<TRPL) then
            RT_fr1(i)= RT_rn1(i);   RT_rn1(i)= 0.
            RT_fr2(i)= RT_rn2(i);   RT_rn2(i)= 0.
         endif

      !ice pellets vs. hail:
         if (T(i,nk)>(TRPL+5.0)) then
         !note: The condition (T_sfc<5C) for ice pellets is a simply proxy for the presence
         !      of a warm layer aloft, though which falling snow or graupel will melt to rain,
         !      over a sub-freezinglayer, where the rain will freeze into the 'hail' category
            RT_pe2(i)= RT_pe1(i);   RT_pe1(i)= 0.
         endif

      !large hail:
         if (QH(i,kbot)>epsQ) then
            N_h  = NH(i,kbot)
            tmp1 = 1./N_h
            Dm_h(i,kbot)= Dm_x(DE(i,kbot),QH(i,kbot),tmp1,icmh,thrd)
            if (DM_h(i,kbot)>Dh_large) RT_peL(i)= RT_pe2(i)
            !note: large hail (RT_peL) is a subset of the total hail (RT_pe2)
         endif

      ENDDO
   ENDIF  !if (precipDiag_ON)
 !
 !++++

 ENDIF  ! if (sedi_ON)

 where (Q<0.) Q= 0.

 !-----------------------------------------------------------------------------------!
 !                     End of sedimentation calculations (Part 4)                    !
 !-----------------------------------------------------------------------------------!

  if (DEBUG_ON) call check_values(Q,T,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH,epsQ,epsN,.false.,DEBUG_abort,800)

 !===================================================================================!
 !                             End of microphysics scheme                            !
 !===================================================================================!

 !-----------------------------------------------------------------------------------!
 !                    Calculation of diagnostic variables:                           !

 !Compute effective radii for cloud and ice (to be passed to radiation scheme):
 !  - based of definition, r_eff = M_r(3)/M_r(2) = 0.5*M_D(3)/M_D(2),
 !    where the pth moment w.r.t. diameter, M_D(p), is given by MY2005a, eqn (2)
  do k = kbot,ktop,kdir
     do i = 1,ni

      !cloud:
        if (QC(i,k)>epsQ .and. NC(i,k)>epsN) then
          !hardcoded for alpha_c = 1. and mu_c = 3.
           iNC   = 1./NC(i,k)
           iLAMc = iLAMDA_x(DE(i,k),QC(i,k),iNC,icexc9,thrd)
        !   reff_c(i,k) = 0.664639*iLAMc
        else
        !   reff_c(i,k) = 0.
        endif

      !ice:
        if (QI(i,k)>epsQ .and. NY(i,k)>epsN) then
          !hardcoded for alpha_i = 0. and mu_i = 1.
           iNY   = 1./NY(i,k)
           iLAMi = max( iLAMmin2, iLAMDA_x(DE(i,k),QI(i,k),iNY,icexi9,thrd) )
        !   reff_i(i,k) = 1.5*iLAMi
        else
        !   reff_i(i,k) = 0.
        endif

     enddo
  enddo


  IF (calcDiag) THEN

   !For reflectivity calculations:
     ZEC= minZET
     cxr=            icmr*icmr   !for rain
     cxi= 1./fdielec*icmr*icmr   !for all frozen categories
     Gzr= (6.+alpha_r)*(5.+alpha_r)*(4.+alpha_r)/((3.+alpha_r)*(2.+alpha_r)*(1.+alpha_r))
     Gzi= (6.+alpha_i)*(5.+alpha_i)*(4.+alpha_i)/((3.+alpha_i)*(2.+alpha_i)*(1.+alpha_i))
     if (snowSpherical) then  !dms=3
        Gzs= (6.+alpha_s)*(5.+alpha_s)*(4.+alpha_s)/((3.+alpha_s)*(2.+alpha_s)*          &
             (1.+alpha_s))
     else                     !dms=2
        Gzs= (4.+alpha_s)*(3.+alpha_s)/((2.+alpha_s)*(1.+alpha_s))
     endif
     Gzg= (6.+alpha_g)*(5.+alpha_g)*(4.+alpha_g)/((3.+alpha_g)*(2.+alpha_g)*(1.+alpha_g))
     Gzh= (6.+alpha_h)*(5.+alpha_h)*(4.+alpha_h)/((3.+alpha_h)*(2.+alpha_h)*(1.+alpha_h))

     do k= ktop,kbot,-kdir
       do i= 1,ni
           DE(i,k)= pres(i,k)/(RGASD*T(i,k))
           tmp9= DE(i,k)*DE(i,k)

           N_c= NC(i,k)
           N_r= NR(i,k)
           N_i= NY(i,k)
           N_s= NN(i,k)
           N_g= NG(i,k)
           N_h= NH(i,k)

        !Total equivalent reflectivity:     (units of [dBZ])
           tmp1= 0.;  tmp2= 0.;  tmp3= 0.;  tmp4= 0.;  tmp5= 0.
           if (QR(i,k)>epsQ .and. N_r>epsN) tmp1 = cxr*Gzr*tmp9*QR(i,k)*QR(i,k)/N_r
           if (QI(i,k)>epsQ .and. N_i>epsN) tmp2 = cxi*Gzi*tmp9*QI(i,k)*QI(i,k)/N_i
           if (QN(i,k)>epsQ .and. N_s>epsN) tmp3 = cxi*Gzs*tmp9*QN(i,k)*QN(i,k)/N_s
           if (QG(i,k)>epsQ .and. N_g>epsN) tmp4 = cxi*Gzg*tmp9*QG(i,k)*QG(i,k)/N_g
           if (QH(i,k)>epsQ .and. N_h>epsN) tmp5 = cxi*Gzh*tmp9*QH(i,k)*QH(i,k)/N_h
          !Modifiy dielectric constant for melting ice-phase categories:
           if ( T(i,k)>TRPL) then
             tmp2= tmp2*fdielec
             tmp3= tmp3*fdielec
             tmp4= tmp4*fdielec
             tmp5= tmp5*fdielec
           endif
           ZET(i,k) = tmp1 + tmp2 + tmp3 + tmp4 + tmp5   != Zr+Zi+Zs+Zg+Zh
           if (ZET(i,k)>0.) then
              ZET(i,k)= 10.*log10((ZET(i,k)*Zfact))      !convert to dBZ
           else
              ZET(i,k)= minZET
           endif
           ZET(i,k)= max(ZET(i,k),minZET)
           ZEC(i)= max(ZEC(i),ZET(i,k))  !composite (max in column) of ZET

         !Mean-mass diameters:  (units of [m])
           Dm_c(i,k)= 0.;   Dm_r(i,k)= 0.;   Dm_i(i,k)= 0.
           Dm_s(i,k)= 0.;   Dm_g(i,k)= 0.;   Dm_h(i,k)= 0.
           if (QC(i,k)>epsQ.and.N_c>epsN) then
              tmp1      = 1./N_c
              Dm_c(i,k) = Dm_x(DE(i,k),QC(i,k),tmp1,icmr,thrd)
           endif
           if (QR(i,k)>epsQ.and.N_r>epsN) then
              tmp1      = 1./N_r
              Dm_r(i,k) = Dm_x(DE(i,k),QR(i,k),tmp1,icmr,thrd)
           endif
           if (QI(i,k)>epsQ.and.N_i>epsN) then
              tmp1      = 1./N_i
              Dm_i(i,k) = Dm_x(DE(i,k),QI(i,k),tmp1,icmi,thrd)
           endif
           if (QN(i,k)>epsQ.and.N_s>epsN) then
              tmp1      = 1./N_s
              Dm_s(i,k) = Dm_x(DE(i,k),QN(i,k),tmp1,icms,idms)
           endif
           if (QG(i,k)>epsQ.and.N_g>epsN) then
              tmp1      = 1./N_g
              Dm_g(i,k) = Dm_x(DE(i,k),QG(i,k),tmp1,icmg,thrd)
           endif
           if (QH(i,k)>epsQ.and.N_h>epsN) then
              tmp1      = 1./N_h
              Dm_h(i,k) = Dm_x(DE(i,k),QH(i,k),tmp1,icmh,thrd)
           endif

        enddo  !i-loop
     enddo     !k-loop

  ENDIF

!-- Convert N from #/m3 to #/kg:
  iDE = (RGASD*T)/pres
  NC  = NC*iDE
  NR  = NR*iDE
  NY  = NY*iDE
  NN  = NN*iDE
  NG  = NG*iDE
  NH  = NH*iDE

!-- Ensure than no negative final values:
!   QC = max(QC, 0.);   NC = max(NC, 0.)
!   QR = max(QR, 0.);   NR = max(NR, 0.)
!   QI = max(QI, 0.);   NY = max(NY, 0.)
!   QN = max(QN, 0.);   NN = max(NN, 0.)
!   QG = max(QG, 0.);   NG = max(NG, 0.)
!   QH = max(QH, 0.);   NH = max(NH, 0.)

  if (DEBUG_ON) call check_values(Q,T,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH,epsQ,epsN,.false.,DEBUG_abort,900)

 END SUBROUTINE mp_milbrandt2mom_main

!___________________________________________________________________________________!

   real function des_OF_Ds(Ds_local,desMax_local,eds_local,fds_local)
   !Computes density of equivalent-volume snow particle based on (pi/6*des)*Ds^3 = cms*Ds^dms
      real :: Ds_local,desMax_local,eds_local,fds_local
!     des_OF_Ds= min(desMax_local, eds_local*Ds_local**fds_local)
      des_OF_Ds= min(desMax_local, eds_local*exp(fds_local*log(Ds_local)))   !IBM optimization
   end function des_OF_Ds


   real function Dm_x(DE_local,QX_local,iNX_local,icmx_local,idmx_local)
   !Computes mean-mass diameter
      real :: DE_local,QX_local,iNX_local,icmx_local,idmx_local
     !Dm_x = (DE_local*QX_local*iNX_local*icmx_local)**idmx_local
      Dm_x = exp(idmx_local*log(DE_local*QX_local*iNX_local*icmx_local))     !IBM optimization
   end function Dm_x


   real function iLAMDA_x(DE_local,QX_local,iNX_local,icex_local,idmx_local)
   !Computes 1/LAMDA ("slope" parameter):
      real :: DE_local,QX_local,iNX_local,icex_local,idmx_local
     !iLAMDA_x = (DE_local*QX_local*iNX_local*icex_local)**idmx_local
      iLAMDA_x = exp(idmx_local*log(DE_local*QX_local*iNX_local*icex_local)) !IBM optimization
   end function


   real function N_Cooper(TRPL_local,T_local)
   !Computes total number concentration of ice as a function of temperature
   !according to parameterization of Cooper (1986):
      real :: TRPL_local,T_local
      N_Cooper= 5.*exp(0.304*(TRPL_local-max(233.,T_local)))
   end function N_Cooper

   real function Nos_Thompson(TRPL_local,T_local)
   !Computes the snow intercept, No_s, as a function of temperature
   !according to the parameterization of Thompson et al. (2004):
      real :: TRPL_local,T_local
      Nos_Thompson= min(2.e+8, 2.e+6*exp(-0.12*min(-0.001,T_local-TRPL_local)))
   end function Nos_Thompson

!===================================================================================================!

END MODULE my2_mod

!________________________________________________________________________________________!

 MODULE module_mp_milbrandt2mom

      use module_wrf_error
      use my2_mod, ONLY: mp_milbrandt2mom_main

      implicit none

! To be done later.  Currently, parameters are initialized in the main routine
! (at every time step).

      CONTAINS

!----------------------------------------------------------------------------------------!
      SUBROUTINE milbrandt2mom_init

! To be done later.  Currently, parameters are initialized in the main routine (at every time step).

      END SUBROUTINE milbrandt2mom_init

!----------------------------------------------------------------------------------------!

 !+---------------------------------------------------------------------+
 ! This is a wrapper routine designed to transfer values from 3D to 2D. !
 !+---------------------------------------------------------------------+


      SUBROUTINE mp_milbrandt2mom_driver(qv, qc, qr, qi, qs, qg, qh, nc, nr, ni, ns, ng,  &
                              nh, th, pii, p, w, dz, dt_in, itimestep, p8w,               & 
                              RAINNC, RAINNCV, SNOWNC, SNOWNCV, GRPLNC, GRPLNCV,          &
                              HAILNC, HAILNCV, SR, Zet,                                   &
                              ids,ide, jds,jde, kds,kde,                                  &  ! domain dims
                              ims,ime, jms,jme, kms,kme,                                  &  ! memory dims
                              its,ite, jts,jte, kts,kte)                                     ! tile dims

      implicit none

 !Subroutine arguments:
      integer, intent(in):: ids,ide, jds,jde, kds,kde,                                   &
                            ims,ime, jms,jme, kms,kme,                                   &
                            its,ite, jts,jte, kts,kte
      real, dimension(ims:ime, kms:kme, jms:jme), intent(inout)::                        &
                            qv,qc,qr,qi,qs,qg,qh,nc,nr,ni,ns,ng,nh,th,Zet
      real, dimension(ims:ime, kms:kme, jms:jme), intent(in)::                           &
                            pii,p,w,dz
      real, dimension(ims:ime, kms:kme, jms:jme), intent(in):: p8w                                  
      real, dimension(ims:ime, jms:jme), intent(inout)::                                 &
                            RAINNC,RAINNCV,SNOWNC,SNOWNCV,GRPLNC,GRPLNCV,HAILNC,HAILNCV, &
                            SR
      real, intent(in)::    dt_in
      integer, intent(in):: itimestep !, ccntype

 !Local variables:
      real, dimension(1:ite-its+1,1:kte-kts+1) :: t2d,p2d,sigma2d

     !tentatively local; to be passed out as output variables later
      real, dimension(1:ite-its+1,1:kte-kts+1)      :: Dm_c,Dm_r,Dm_i,Dm_s,Dm_g,Dm_h
      real, dimension(1:ite-its+1,1:kte-kts+1,1:20) :: SS
      !note: 20 is temporarily hardcoded; eventually should allocate size with parameter
      !integer, parameter :: numSS = 20    ! number of diagnostic arrays [SS(i,k,:)]

      real, dimension(1:ite-its+1) :: rt_rn1,rt_rn2,rt_fr1,rt_fr2,rt_sn1,rt_sn2,rt_sn3,  &
            rt_pe1,rt_pe2,rt_peL,rt_snd,ZEC,p_sfc
      real, dimension(ims:ime, kms:kme, jms:jme) :: t3d

      real    :: dt,ms2mmstp
      integer :: i,j,k,i2d,k2d,i2d_max,k2d_max
      integer :: i_start, j_start, i_end, j_end, CCNtype
      logical :: precipDiag_ON,sedi_ON,warmphase_ON,autoconv_ON,icephase_ON,snow_ON

      real, parameter    :: ms2mmh    = 3.6e+6   !conversion factor for precipitation rates
      logical, parameter :: nk_BOTTOM = .false.  !.F. for k=1 at bottom level (WRF)

      real, parameter    :: statfreq  = 300.     !frequency (seconds) to output block stats

!+---+
      i2d_max  = ite-its+1
      k2d_max  = kte-kts+1
      dt       = dt_in
      ms2mmstp = 1.e+3*dt    !conversion factor: m/2 to mm/step

   !--- temporary initialization (until variables are put as namelist options:
!     CCNtype       = 1.  !maritime      --> N_c =  80 cm-3 for dblMom_c = .F.
      CCNtype       = 2.  !continental   --> N_c = 200 cm-3 for dblMom_c = .F.

      precipDiag_ON = .true.
      sedi_ON       = .true.
      warmphase_ON  = .true.
      autoconv_ON   = .true.
      icephase_ON   = .true.
      snow_ON       = .true.
   !---

      RAINNCV(its:ite,jts:jte) = 0.
      SNOWNCV(its:ite,jts:jte) = 0.
      GRPLNCV(its:ite,jts:jte) = 0.
      HAILNCV(its:ite,jts:jte) = 0.
      SR(its:ite,jts:jte)      = 0.

      !--run-time stats:
!       if (mod(itimestep*dt,statfreq)==0.) then
!           t3d = th*pii
!           print*, 'Bloc Stats -- BEFORE micro call (my-2.25.1_b11)'
!           write(6,'(a8,7e15.5)') 'Max qx: ',maxval(qc(its:ite,kts:kte,jts:jte)),maxval(qr(its:ite,kts:kte,jts:jte)),maxval(qi(its:ite,kts:kte,jts:jte)), &
!                                             maxval(qs(its:ite,kts:kte,jts:jte)),maxval(qg(its:ite,kts:kte,jts:jte)),maxval(qh(its:ite,kts:kte,jts:jte))
!           write(6,'(a8,6e15.5)') 'Max Nx: ',maxval(nc(its:ite,kts:kte,jts:jte)),maxval(nr(its:ite,kts:kte,jts:jte)),maxval(ni(its:ite,kts:kte,jts:jte)), &
!                                             maxval(ns(its:ite,kts:kte,jts:jte)),maxval(ng(its:ite,kts:kte,jts:jte)),maxval(nh(its:ite,kts:kte,jts:jte))
!           write(6,'(a8,7e15.5)') 'Min qx: ',minval(qc(its:ite,kts:kte,jts:jte)),minval(qr(its:ite,kts:kte,jts:jte)),minval(qi(its:ite,kts:kte,jts:jte)), &
!                                             minval(qs(its:ite,kts:kte,jts:jte)),minval(qg(its:ite,kts:kte,jts:jte)),minval(qh(its:ite,kts:kte,jts:jte))
!           write(6,'(a8,6e15.5)') 'Min Nx: ',minval(nc(its:ite,kts:kte,jts:jte)),minval(nr(its:ite,kts:kte,jts:jte)),minval(ni(its:ite,kts:kte,jts:jte)), &
!                                             minval(ns(its:ite,kts:kte,jts:jte)),minval(ng(its:ite,kts:kte,jts:jte)),minval(nh(its:ite,kts:kte,jts:jte))
!           write(6,'(a8,6e15.5)') 'W,T,qv: ',minval( w(its:ite,kts:kte,jts:jte)),maxval( w(its:ite,kts:kte,jts:jte)),minval(t3d(its:ite,kts:kte,jts:jte)),&
!                                             maxval(t3d(its:ite,kts:kte,jts:jte)),minval(qv(its:ite,kts:kte,jts:jte)),maxval(qv(its:ite,kts:kte,jts:jte))
!       endif
      
      do j = jts, jte

         t2d(:,:) = th(its:ite,kts:kte,j)*pii(its:ite,kts:kte,j)
         p2d(:,:) = p(its:ite,kts:kte,j)
      !   p_sfc(:) = p2d(:,k2d_max)
         p_sfc(:) = p8w(its:ite,kms, j)  

         do i = its, ite
            i2d = i-its+1
            sigma2d(i2d,:) = p2d(i2d,:)/p_sfc(i2d)
         enddo
         call mp_milbrandt2mom_main(w(its:ite,kts:kte,j),t2d,qv(its:ite,kts:kte,j), &
               qc(its:ite,kts:kte,j),qr(its:ite,kts:kte,j),qi(its:ite,kts:kte,j),   &
               qs(its:ite,kts:kte,j),qg(its:ite,kts:kte,j),qh(its:ite,kts:kte,j),   &
               nc(its:ite,kts:kte,j),nr(its:ite,kts:kte,j),ni(its:ite,kts:kte,j),   &
               ns(its:ite,kts:kte,j),ng(its:ite,kts:kte,j),nh(its:ite,kts:kte,j),   &
               p_sfc,sigma2d,rt_rn1,rt_rn2,rt_fr1,rt_fr2,rt_sn1,rt_sn2,rt_sn3,      &
               rt_pe1,rt_pe2,rt_peL,rt_snd,dt,i2d_max,k2d_max,j,itimestep,CCNtype,  &
               precipDiag_ON,sedi_ON,warmphase_ON,autoconv_ON,icephase_ON,snow_ON,  &
               Dm_c,Dm_r,Dm_i,Dm_s,Dm_g,Dm_h,Zet(its:ite,kts:kte,j),ZEC,SS,nk_BOTTOM)

         th(its:ite,kts:kte,j) = t2d(:,:)/pii(its:ite,kts:kte,j)

         !Convert individual precipitation rates (in m/s) to WRF precipitation fields (mm/step):
         RAINNCV(its:ite,j) = ( rt_rn1(:)+rt_rn2(:)+rt_fr1(:)+rt_fr2(:)+rt_sn1(:)+rt_sn2(:)+ &
                                rt_sn3(:)+rt_pe1(:)+rt_pe2(:) )*ms2mmstp
         SNOWNCV(its:ite,j) = (rt_sn1(:) + rt_sn2(:))*ms2mmstp
         HAILNCV(its:ite,j) = (rt_pe1(:) + rt_pe2(:))*ms2mmstp
         GRPLNCV(its:ite,j) = rt_sn3(:)*ms2mmstp

         RAINNC(its:ite,j)  = RAINNC(its:ite,j) + RAINNCV(its:ite,j)
         SNOWNC(its:ite,j)  = SNOWNC(its:ite,j) + SNOWNCV(its:ite,j)
         HAILNC(its:ite,j)  = HAILNC(its:ite,j) + HAILNCV(its:ite,j)
         GRPLNC(its:ite,j)  = GRPLNC(its:ite,j) + GRPLNCV(its:ite,j)
         SR(its:ite,j)      = (SNOWNCV(its:ite,j)+HAILNCV(its:ite,j)+GRPLNCV(its:ite,j))/(RAINNCV(its:ite,j)+1.e-12)


      enddo !j_loop

      !--run-time stats:
!       if (mod(itimestep*dt,statfreq)==0.) then
!          t3d = th*pii
!          print*, 'Bloc Stats -- AFTER micro call; my_2.25.1-b11)'
!          write(6,'(a8,7e15.5)') 'Max qx: ',maxval(qc(its:ite,kts:kte,jts:jte)),maxval(qr(its:ite,kts:kte,jts:jte)),maxval(qi(its:ite,kts:kte,jts:jte)), &
!                                            maxval(qs(its:ite,kts:kte,jts:jte)),maxval(qg(its:ite,kts:kte,jts:jte)),maxval(qh(its:ite,kts:kte,jts:jte))
!          write(6,'(a8,6e15.5)') 'Max Nx: ',maxval(nc(its:ite,kts:kte,jts:jte)),maxval(nr(its:ite,kts:kte,jts:jte)),maxval(ni(its:ite,kts:kte,jts:jte)), &
!                                            maxval(ns(its:ite,kts:kte,jts:jte)),maxval(ng(its:ite,kts:kte,jts:jte)),maxval(nh(its:ite,kts:kte,jts:jte))
!          write(6,'(a8,7e15.5)') 'Min qx: ',minval(qc(its:ite,kts:kte,jts:jte)),minval(qr(its:ite,kts:kte,jts:jte)),minval(qi(its:ite,kts:kte,jts:jte)), &
!                                            minval(qs(its:ite,kts:kte,jts:jte)),minval(qg(its:ite,kts:kte,jts:jte)),minval(qh(its:ite,kts:kte,jts:jte))
!          write(6,'(a8,6e15.5)') 'Min Nx: ',minval(nc(its:ite,kts:kte,jts:jte)),minval(nr(its:ite,kts:kte,jts:jte)),minval(ni(its:ite,kts:kte,jts:jte)), &
!                                            minval(ns(its:ite,kts:kte,jts:jte)),minval(ng(its:ite,kts:kte,jts:jte)),minval(nh(its:ite,kts:kte,jts:jte))
!          write(6,'(a8,6e15.5)') 'W,T,qv: ',minval( w(its:ite,kts:kte,jts:jte)),maxval( w(its:ite,kts:kte,jts:jte)),minval(t3d(its:ite,kts:kte,jts:jte)),&
!                                            maxval(t3d(its:ite,kts:kte,jts:jte)),minval(qv(its:ite,kts:kte,jts:jte)),maxval(qv(its:ite,kts:kte,jts:jte))
!          write(6,'(a8,2e15.5)') 'Zet   : ',maxval(Zet(its:ite,kts:kte,jts:jte))
!       endif

      END SUBROUTINE mp_milbrandt2mom_driver

!+---+-----------------------------------------------------------------+
!________________________________________________________________________________________!

END MODULE module_mp_milbrandt2mom
