#if( BUILD_SBM_FAST != 1)
      MODULE module_mp_fast_sbm
      CONTAINS
      SUBROUTINE SBM_fast
         REAL :: dummy
         dummy = 1
      END SUBROUTINE SBM_fast
      END MODULE module_mp_fast_sbm
#else
! +-----------------------------------------------------------------------------+
! +-----------------------------------------------------------------------------+

! This is the spectral-bin microphysics scheme based on the Hebrew University
! Cloud Model (HUCM), originally formulated and coded by Alexander Khain
! (email: Alexander.Khain@mail.huji.ac.il);
! The WRF bin microphysics scheme (Fast SBM or FSBM) solves equations for four
! size distribution functions: aerosols, drop (including rain drops), snow and
! graupel/hail (from which mass mixing ratio qna, qc, qr, qs, qg/qh and
! their number concentrations are calculated).

! The scheme is generally written in CGS units. In the updated scheme (FSBM-2)
! the users can choose either graupel or hail to describe dense particles
! (see the 'hail_opt' switch). By default, the 'hail_opt = 1' is used.
! Hail particles have larger terminal velocity than graupel per mass bin.
! 'hail_opt' is recommended to be used in simulations of continental cloud
! systems. The Graupel option may lead to better results in simulations of
! maritime convection.

! The aerosol spectrum in FSBM-2 is approximated by 3-lognormal size distribution
! representing smallest aerosols (nucleation mode), intermediate-size
! (accumulation mode) and largest aerosols (coarse mode). The BC/IC for aerosols
! ,as well as aerosols vertical distribution profile -- are set from within the
! FSBM scheme (see the 'DX_BOUND' parameter). The flag to enable the lognormal
! aerosols is (ILogNormal_modes_Aerosol = 1, manadatory flag). The modes parameters
! (concentration, mean radius and model width) are defined inside the routine
! "LogNormal_modes_Aerosol".
!  **NOTE**: In order to set aerosol BC for the outer-most domain, set the threshold 
! 'DX_BOUND' = 'MY_DX_OUTER_DOMAIN'-1. For example, if the outermost domain 
! resolutionis is 4km 'DX_BOUND' = 3999.


! The user can set the liquid water content threshold (LWC) in which rimed snow
! is being transferred to hail/graupel (see 'ALCR' parameter).
! The default value is ALCR = 0.5 [g/m3]. Increasing this value will result
! in an increase of snow mass content, and a decrease in hail/graupel mass
! contents.

! We thank and acknowledge contribution from Jiwen Fan (PNNL), Alexander Rhyzkov
! (CIMMS/NSSL), Jeffery Snyder (CIMMS/NSSL), Jimy Dudhia (NCAR) and Dave Gill
! (NCAR).

! The previous WRF FSBM version  (FSBM-1) was coded by Barry Lynn (email:
! Barry.H.Lynn@gmail.com); This updated WRF SBM version (FSBM-2) was coded and
! is maintained by Jacob Shpund (email: kobby.shpund@mail.huji.ac.il).
! Please feel free to reachout with questions about the scheme.

! Usefull references:
! -------------------
!     Khain, A. P., and I. Sednev, 1996: Simulation of precipitation formation in
! the Eastern Mediterranean coastal zone using a spectral microphysics cloud
! ensemble model. Atmospheric Research, 43: 77-110;
!     Khain, A. P., A. Pokrovsky and M. Pinsky, A. Seifert, and V. Phillips, 2004:
! Effects of atmospheric aerosols on deep convective clouds as seen from
! simulations using a spectral microphysics mixed-phase cumulus cloud model
! Part 1: Model description. J. Atmos. Sci 61, 2963-2982);
!     Khain A. P. and M. Pinsky, 2018: Physical Processes in Clouds and Cloud
! modeling. Cambridge University Press. 642 pp
!     Shpund, J., A. Khain, and D. Rosenfeld, 2019: Effects of Sea Spray on the
! Dynamics and Microphysics of an Idealized Tropical Cyclone. J. Atmos. Sci., 0,
! https://doi.org/10.1175/JAS-D-18-0270.1 (A preliminary description of the
! updated FSBM-2 scheme)

! When using the FSBM-2 version please cite:
! -------------------------------------------
!     Shpund, J., Khain, A., Lynn, B., Fan, J., Han, B., Ryzhkov, A., Snyder, J., 
! Dudhia, J. and Gill, D., 2019. Simulating a Mesoscale Convective System Using WRF 
! With a New Spectral Bin Microphysics: 1: Hail vs Graupel. 
! Journal of Geophysical Research: Atmospheres.

! +---------------------------------------------------------------------------- +
! +-----------------------------------------------------------------------------+
module module_mp_SBM_BreakUp

private
public Spont_Rain_BreakUp,Spontanous_Init,BreakUp_Snow,KR_SNOW_MIN,KR_SNOW_MAX

! Kind paramater
INTEGER, PARAMETER, PRIVATE:: R8SIZE = 8
INTEGER, PARAMETER, PRIVATE:: R4SIZE = 4

! ... Spontanous Rain BreakUp
INTEGER,PARAMETER :: 		JBreak_Spontanous = 28, &
              					I_Break_Method = 1
DOUBLE PRECISION,PARAMETER :: COL = 0.23105
! ... Snow-BreakUp
INTEGER,PARAMETER :: KR_SNOW_MAX = 35
INTEGER,PARAMETER :: KR_SNOW_MIN = 34
! ... Snow breakup probability
DOUBLE PRECISION,PARAMETER :: BREAK_SNOW_KRMAX_0 = 0.02D0
DOUBLE PRECISION,PARAMETER :: BREAK_SNOW_KRMAX_1 = 0.012D0
!DOUBLE PRECISION,PARAMETER :: BREAK_SNOW_KRMAX_2 = 0.08D0
!DOUBLE PRECISION,PARAMETER :: BREAK_SNOW_KRMAX_3 = 0.04D0

contains
	! +--------------------------------------------------------------------------+
  subroutine Spontanous_Init(DTwrf, XL, DROPRADII, Prob, Gain_Var_New, NND, NKR, &
                           ikr_spon_break)

  implicit none

  integer,intent(in):: NKR
  real(kind=r4size),intent(in) :: 	DTwrf,XL(:),DROPRADII(:)
  real(kind=r8size),intent(out) :: Prob(:), Gain_Var_New(:,:), NND(:,:)

! ... Locals
  real(kind=r8size) :: diameter(nkr), ratio_new, q_m, gain_var(nkr,nkr), dtime_spon_break, &
                  		 DROPRADII_dp(nkr),XL_dp(nkr)
  integer :: kr,i,j, ikr_spon_break
  real(kind=r8size),parameter :: gamma = 0.453d0
  character*256 :: wrf_err_message
! ... Locals

!dtime_spon_break = DTwrf
DROPRADII_dp = DROPRADII
XL_dp = XL
! diameter in nm
diameter(:) = DROPRADII_dp(:)*2.0d0*10.0d0

DO KR=1,NKR
	ikr_spon_break=kr
	IF (DROPRADII(kr)>=0.3) exit
END DO

WRITE( wrf_err_message , * ) 'IKR_Spon_Break=',ikr_spon_break
CALL wrf_message ( TRIM ( wrf_err_message ) )

if (i_break_method==1) then
 	DO KR=1,NKR
		 prob(kr)=2.94d-7*dexp(34.0d0*DROPRADII(kr))
	ENDDO
else if  (i_break_method==2) then
 	DO KR=1,NKR
		 prob(kr)=0.155d-3*dexp(1.466d0*10.0d0*DROPRADII(kr))
 	ENDDO
endif

!DO KR=1,NKR
!  prob(kr)=2.94d-7*dexp(34.0d0*DROPRADII_dp(kr))*dtime_spon_break
!  IF (prob(kr)>=1.0d0) exit
!END DO

DO j=ikr_spon_break,nkr
	DO i=1,j-1
		gain_var(j,i)=(145.37d0/xl_dp(i))*(dropradii_dp(i)/dropradii_dp(j))*dexp(-7.0d0*dropradii_dp(i)/dropradii_dp(j))
		!gain_var_new(j,i)=gain_var(j,i)*xl(j)/(gain_var(j,i)*xl(i)**2.0d0)
		nnd(j,i)=gamma*dexp(-gamma*diameter(i))/(1-dexp(-gamma*diameter(j)))
	END DO
END DO
! Calculation the ratio that leads to mass conservation
q_m = 0.0
DO i=1,ikr_spon_break-1
  !nnd_m = nnd_m+nnd(ikr_spon_break,i)*m(i);
   q_m = q_m + gain_var(ikr_spon_break,i)*xl_dp(i)**2;
END DO
ratio_new = q_m/xl_dp(ikr_spon_break)
 ! print*, 'ikr_spon_break,q_m,xl(ikr_spon_break),ratio_new'
 ! print*,  ikr_spon_break,q_m,xl(ikr_spon_break),ratio_new
 DO j=ikr_spon_break,nkr
 	DO i=1,j-1
  	gain_var_new(j,i) = gain_var(j,i)/ratio_new
 	END DO
 END DO

 RETURN
 End Subroutine Spontanous_Init
! +-----------------------------------------------------------------------------+
! i_break_method=1: Spontaneous breakup according to Srivastava1971_JAS -
! Size distribution od raindrops generated by their breakup and coalescence
! i_break_method=2: Spontaneous breakup according to Kamra et al 1991 JGR -
! SPONTANEOUS BREAKUP OF CHARGED AND UNCHARGED WATER DROPS FREELY SUSPENDED IN A WIND TUNNEL
! Eyal's new changes (29/3/15)    (start)
! Description of variables       (start)
! FF1R(KR), 1/g/cm3 - non conservative drop size distribution
! XL(kr), g - Mass of liquid drops
! prob, dimensionless - probability for breakup
! dropconc_bf(kr), cm^-3 - drops concentration before breakup
! dropconc_af(kr), cm^-3 - drops concentration before breakup
! drops_break(kr), cm^-3 - concentration of breaking drops
! Description of variables       (end)

  SUBROUTINE Spont_Rain_BreakUp (DTwrf, FF1R, XL, Prob, Gain_Var_New, NND, NKR, ikr_spon_break)

  implicit none

  integer,intent(in) :: NKR, IKR_Spon_Break
  real(kind=r8size),intent(INOUT) :: FF1R(:)
  real(kind=r8size),intent(IN) ::XL(:),Prob(:),Gain_Var_New(:,:),NND(:,:)
  real(kind=r4size),intent(in) :: DTwrf

! ... Local
  real(kind=r8size) :: dm, deg01, tmp_1, tmp_2, tmp_3
  real(kind=r8size),dimension(nkr) :: dropconc_bf, dropconc_af, drops_break, psi1, dropradii
  integer :: kr,i,imax,j
  real(kind=r4size) :: start_time, end_time, dtime_spon_break
! ... Local

 dtime_spon_break = DTwrf

  DEG01 = 1.0/3.0
  DO KR=1,NKR
     DROPRADII(KR)=(3.*XL(KR)/4./3.141593/1.)**DEG01
  ENDDO

  if(SUM(FF1R) <= nkr*1.D-30) return

  imax=nkr
  do i=nkr,1,-1
     imax=i
     if (FF1R(i) > 0.0D0) exit
  enddo

  if (imax<ikr_spon_break) return

! Initialization        (start)
  psi1(:)=ff1r(:)
  drops_break(:)=0.0d0
  dropconc_bf(:)=0.0d0

! b) Calculation of concentration of raindrops in all bins
  do kr=1,imax
     dm=3.0d0*col*xl(kr)
     dropconc_bf(kr)=dropconc_bf(kr)+dm*psi1(kr)
  enddo
  dropconc_af(:)=dropconc_bf(:)

! c+d) Calculation of number of breaking drops  and the concentration of drops remaining in particular bin

  do kr=imax,ikr_spon_break,-1
    !dropconc_af(kr)=dropconc_bf(kr)/(1+prob(kr)*dtime_spon_break)
		tmp_1 = prob(kr)*dtime_spon_break ! [KS, 18thJan18] >> the time was added here and not in the initialization
		tmp_2 = dexp(-tmp_1)
		tmp_3 = dropconc_bf(kr)
		dropconc_af(kr) = tmp_2*tmp_3
		!dropconc_af(kr) = dexp(-dtime_spon_break*prob(kr))*dropconc_bf(kr)
    drops_break(kr) = dropconc_bf(kr)-dropconc_af(kr)
    !if (dropconc_af(kr)<0.0d0) stop 'Spontaneous breakup'
  enddo

! e) Recalculation of DSD in bin j using new concentration
!        do kr=ikr_spon_break,imax
!           dm=3.0D0*col*xl(kr)
!           psi1(kr)=psi1(kr)-drops_break(kr)/dm
!        enddo

! f+g) Redistributing and calculations drops concentration over smaller (i<j) bins
!
  select case (i_break_method)
  case(1)
  do j=ikr_spon_break,imax
       do i=1,j-1
            dropconc_af(i)=dropconc_af(i)+drops_break(j)*gain_var_new(j,i)*xl(i)
        enddo
  enddo

  case(2)
  do j=ikr_spon_break,imax
       do i=1,j-1
	   		dropconc_af(i)=dropconc_af(i)+drops_break(j)*gain_var_new(j,i)*xl(i)
            !dropconc_af(i)=dropconc_af(i)+drops_break(j)*nnd(j,i)
        enddo
  enddo
end select

! h) recalculation of DSD in bins kr using new concentrations

  do kr=1,imax
	   dm=3.0D0*col*xl(kr)
	   psi1(kr)=dropconc_af(kr)/dm
  enddo

  ff1r(:)=psi1(:)
! 200   FORMAT(1X,I2,2X,5D13.5)
! Eyal's new changes (29/3/15)    (end)

  RETURN
  END SUBROUTINE Spont_Rain_BreakUp
! +-------------------------------------------+
	SUBROUTINE BreakUp_Snow (Tin,F,FL,X,RF,NKR)

	IMPLICIT NONE

	INTEGER,INTENT(in) :: NKR
	real(kind=r8size),INTENT(inout) :: F(:),FL(:),RF(:)
	real(kind=r8size),INTENT(in) :: X(:)
	real(kind=r4size),INTENT(in) :: Tin

! ... Locals
	real(kind=r8size) :: G(NKR),GLW(NKR),GRM(NKR),DEL_GLW(NKR),DEL_GRM(NKR), BREAK_SNOW(NKR), &
						           A,GLW_MAX, FLW_MAX, GRM_MAX, FRM_MAX, GMAX
	INTEGER :: KR,K,KMAX,KMIN
! ... Locals

	DO KR=1,NKR
	   BREAK_SNOW(KR)=0.0D0
	END DO

   if (KR_SNOW_MAX  <=NKR) BREAK_SNOW(KR_SNOW_MAX) = BREAK_SNOW_KRMAX_0
   if (KR_SNOW_MAX-1<=NKR) BREAK_SNOW(KR_SNOW_MAX-1) = BREAK_SNOW_KRMAX_1
  !if (KR_SNOW_MAX-2<=NKR) BREAK_SNOW(KR_SNOW_MAX-2) = BREAK_SNOW_KRMAX_2
  !if (KR_SNOW_MAX-3<=NKR) BREAK_SNOW(KR_SNOW_MAX-3) = BREAK_SNOW_KRMAX_3

	DO K=1,NKR
		G(K)=0.0D0
		GLW(K)=0.0D0
		GRM(K)=0.0D0
		DEL_GLW(K)=0.0D0
		DEL_GRM(K)=0.0D0
	END DO

	KMAX=KR_SNOW_MAX
	KMIN=KR_SNOW_MIN

	A=X(KMAX)*X(KMAX)

	GLW_MAX=0.0D0

	DO K=KMAX+1,NKR
   		GLW_MAX=GLW_MAX+X(K)*X(K)*F(K)*FL(K)
	ENDDO

	GLW_MAX=GLW_MAX+A*F(KMAX)*FL(KMAX)

	FLW_MAX=GLW_MAX/A

	GRM_MAX=0.0D0

	DO K=KMAX+1,NKR
   		GRM_MAX=GRM_MAX+X(K)*X(K)*F(K)*(1.0D0-FL(K))*RF(K)
	ENDDO

	GRM_MAX=GRM_MAX+A*F(KMAX)*(1.0D0-FL(KMAX))*RF(KMAX)

	FRM_MAX=GRM_MAX/A

	GMAX=0.0D0

	DO K=KMAX+1,NKR
   		GMAX=GMAX+X(K)*X(K)*F(K)
	ENDDO

	GMAX=GMAX+A*F(KMAX)

	F(KMAX) = GMAX/A

	!FL(KMAX)=FLW_MAX/F(KMAX)

	IF (F(KMAX) .lt. 1.0E-20)then
	   if(TIN > 273.15)then
        FL(kmax) = 1.0d0
        RF(kmax) = 0.0d0
	   else
        FL(kmax) = 0.0d0
        RF(kmax) = 1.0d0
	   endif
	ELSE
	   if(TIN > 273.15)then
		   RF(KMAX) = 0.0
       FL(KMAX) = FLW_MAX/F(KMAX)
	  else
       FL(KMAX) = 0.0
       RF(KMAX) = FRM_MAX/F(KMAX)/(1.0D0-FL(KMAX))
	   endif
	END IF

	DO K=KMAX+1,NKR
		F(K)=0.0D0
		if(TIN > 273.15)then
		  RF(K) = 0.0D0
		  FL(K) = 1.0D0
		else
		  RF(K) = 1.0D0
		  FL(K) = 0.0D0
		endif
	ENDDO

	G(KMAX)=3.0D0*F(KMAX)*A
	DO K=KMAX-1,KMIN-1,-1
	   G(K)=F(K)*3.0D0*X(K)*X(K)
	   GLW(K)=G(K)*FL(K)
	   GRM(K)=G(K)*(1.0D0-FL(K))*RF(K)
	ENDDO

	DO K=KMAX,KMIN,-1
	   DEL_GLW(K) = G(K)*BREAK_SNOW(K)*FL(K)
	   GLW(K-1) = GLW(K-1)+DEL_GLW(K)
	   DEL_GRM(K) = G(K)*(1.0D0-FL(K))*RF(K)*BREAK_SNOW(K)
	   GRM(K-1) = GRM(K-1)+DEL_GRM(K)
	   G(K-1) = G(K-1)+G(K)*BREAK_SNOW(K)
	   F(K-1) = G(K-1)/3.0D0/X(K-1)/X(K-1)

   		if (G(k-1) < 1.0d-20) then
			if(TIN > 273.15)then
			   	FL(k-1) = 1.0d0
			   	RF(k-1) = 0.0d0
			else
	   			FL(k-1) = 0.0d0
			   	RF(k-1) = 1.0d0
			endif
	   else
			if(TIN > 273.15)then
				FL(k-1) = GLW(k-1)/G(k-1)
				RF(K-1) = 0.0
			else
				FL(K-1) = 0.0
				!print*,'SnowBr',GRM(k-1),G(k-1),FL(k-1)
				RF(k-1) = GRM(k-1)/G(k-1)/(1.0D0-FL(k-1))
			endif
		endif

		 ! FL(K-1)=GLW(K-1)/G(K-1)
	   ! RF(K-1)=GRM(K-1)/G(K-1)/(1.0D0-FL(K-1))

	   G(K) = G(K)*(1.0D0-BREAK_SNOW(K))
	   F(K) = G(K)/3.0D0/X(K)/X(K)
	END DO

	RETURN
	END SUBROUTINE BreakUp_Snow
! +------------------------------+
end module module_mp_SBM_BreakUp
! +-----------------------------------------------------------------------------+
! +-----------------------------------------------------------------------------+
 module module_mp_SBM_Collision

 private
 public coll_xyy_lwf, coll_xyx_lwf, coll_xxx_lwf, &
        coll_xyz_lwf, coll_xxy_lwf, &
        modkrn_KS, coll_breakup_KS, courant_bott_KS

  ! Kind paramater
  INTEGER, PARAMETER, PRIVATE:: R8SIZE = 8
  INTEGER, PARAMETER, PRIVATE:: R4SIZE = 4
  integer,parameter :: kp_flux_max = 44
  real(kind=r8size), parameter :: G_LIM = 1.0D-16 ! [g/cm^3]
  integer,parameter :: kr_sgs_max = 20 ! rg(20)=218.88 mkm

 contains
! +------------------------------------------------+
subroutine coll_xyy_lwf (gx,gy,flx,fly,ckxy,x,y, &
						            c,ima,prdkrn,nkr,indc)
	implicit none

	integer,intent(in) :: nkr
	real(kind=r8size),intent(inout) :: gy(:),gx(:),fly(:),flx(:)
	real(kind=r8size),intent(in) :: ckxy(:,:),x(:),y(:),c(:,:)
	integer,intent(in) :: ima(:,:)
	real(kind=r8size),intent(in) :: prdkrn

! ... Locals
 real(kind=r8size) :: gmin,ckxy_ji,x01,x02,x03,gsi,gsj,gsk,gsi_w,gsj_w,gsk_w,gk,gk_w,&
                      fl_gk,fl_gsk,flux,x1,flux_w,gy_k_w,gy_kp_old,gy_kp_w
 integer :: j,jx0,jx1,i,iy0,iy1,jmin,indc,k,kp
! ... Locals

	  gmin = 1.0d-60

! jx0 - lower limit of integration by j
do j=1,nkr-1
   jx0=j
   if(gx(j).gt.gmin) goto 2000
enddo
2000   continue
if(jx0.eq.nkr-1) return

! jx1 - upper limit of integration by j
do j=nkr-1,jx0,-1
   jx1=j
   if(gx(j).gt.gmin) goto 2010
enddo
2010   continue

! iy0 - lower limit of integration by i
do i=1,nkr-1
   iy0=i
   if(gy(i).gt.gmin) goto 2001
enddo
2001   continue
if(iy0.eq.nkr-1) return

! iy1 - upper limit of integration by i
do i=nkr-1,iy0,-1
   iy1=i
   if(gy(i).gt.gmin) goto 2011
enddo
2011   continue

! collisions :
        do i = iy0,iy1
           if(gy(i).le.gmin) goto 2020
           jmin = i
           if(jmin.eq.nkr-1) return
           if(i.lt.jx0) jmin=jx0-indc
            do j=jmin+indc,jx1
              if(gx(j).le.gmin) goto 2021
              k=ima(i,j)
              kp=k+1
              ckxy_ji=ckxy(j,i)
              x01=ckxy_ji*gy(i)*gx(j)*prdkrn
              x02=dmin1(x01,gy(i)*x(j))
              x03=dmin1(x02,gx(j)*y(i))
              gsi=x03/x(j)
              gsj=x03/y(i)
              gsk=gsi+gsj
              if(gsk.le.gmin) goto 2021
              gsi_w=gsi*fly(i)
              gsj_w=gsj*flx(j)
              gsk_w=gsi_w+gsj_w
              gsk_w=dmin1(gsk_w,gsk)
              gy(i)=gy(i)-gsi
              gy(i)=dmax1(gy(i),0.0d0)
              gx(j)=gx(j)-gsj
              gx(j)=dmax1(gx(j),0.0d0)
              gk=gy(k)+gsk
              if(gk.le.gmin) goto 2021
              gk_w=gy(k)*fly(k)+gsk_w
              gk_w=dmin1(gk_w,gk)

	            fl_gk=gk_w/gk

              fl_gsk=gsk_w/gsk

              flux=0.d0
              x1=dlog(gy(kp)/gk+1.d-15)
              flux=gsk/x1*(dexp(0.5d0*x1)-dexp(x1*(0.5d0-c(i,j))))
              flux=dmin1(flux,gsk)
              flux=dmin1(flux,gk)

              if(kp.gt.kp_flux_max) flux=0.5d0*flux
              flux_w=flux*fl_gsk
              flux_w=dmin1(flux_w,gsk_w)
              flux_w=dmin1(flux_w,gk_w)

                gy(k)=gk-flux
                gy(k)=dmax1(gy(k),gmin)
                gy_k_w=gk*fl_gk-flux_w
                gy_k_w=dmin1(gy_k_w,gy(k))
                gy_k_w=dmax1(gy_k_w,0.0d0)
                fly(k)=gy_k_w/gy(k)
                gy_kp_old=gy(kp)
                gy(kp)=gy(kp)+flux
                gy(kp)=dmax1(gy(kp),gmin)
                gy_kp_w=gy_kp_old*fly(kp)+flux_w
                gy_kp_w=dmin1(gy_kp_w,gy(kp))
                fly(kp)=gy_kp_w/gy(kp)

                if(fly(k).gt.1.0d0.and.fly(k).le.1.0001d0) &
                   fly(k)=1.0d0
                if(fly(kp).gt.1.0d0.and.fly(kp).le.1.0001d0) &
                   fly(kp)=1.0d0
                if(fly(k).gt.1.0001d0.or.fly(kp).gt.1.0001d0 &
                   .or.fly(k).lt.0.0d0.or.fly(kp).lt.0.0d0) then

                print*,    'in subroutine coll_xyy_lwf'

                if(fly(k).gt.1.0001d0)  print*, 'fly(k).gt.1.0001d0'
                if(fly(kp).gt.1.0001d0) print*, 'fly(kp).gt.1.0001d0'

                if(fly(k).lt.0.0d0)  print*, 'fly(k).lt.0.0d0'
                if(fly(kp).lt.0.0d0) print*, 'fly(kp).lt.0.0d0'

                print*,    'i,j,k,kp'
                print*,     i,j,k,kp

                print*,    'jx0,jx1,iy0,iy1'
                print*,     jx0,jx1,iy0,iy1

                print*,   'ckxy(j,i),x01,x02,x03'
                print 204, ckxy(j,i),x01,x02,x03

                print*,   'gsi,gsj,gsk'
                print 203, gsi,gsj,gsk

                print*,   'gsi_w,gsj_w,gsk_w'
                print 203, gsi_w,gsj_w,gsk_w

                print*,   'gk,gk_w'
                print 202, gk,gk_w

                print*,   'fl_gk,fl_gsk'
                print 202, fl_gk,fl_gsk

                print*,   'x1,c(i,j)'
                print 202, x1,c(i,j)

                print*,   'flux'
                print 201, flux

                print*,   'flux_w'
                print 201, flux_w

                print*,   'gy_k_w'
                print 201, gy_k_w

                print*,   'gy_kp_w'
                print 201, gy_kp_w

		            if(fly(k).lt.0.0d0) print*, &
				            'stop 2022: in subroutine coll_xyy_lwf, fly(k) < 0'

                if(fly(kp).lt.0.0d0) print*, &
					           'stop 2022: in subroutine coll_xyy_lwf, fly(kp) < 0'

                if(fly(k).gt.1.0001d0) print*, &
					           'stop 2022: in sub. coll_xyy_lwf, fly(k) > 1.0001'

				        if(fly(kp).gt.1.0001d0) print*, &
					           'stop 2022: in sub. coll_xyy_lwf, fly(kp) > 1.0001'

                     call wrf_error_fatal("in coal_bott coll_xyy_lwf, model stop")
! in case fly(k).gt.1.0001d0.or.fly(kp).gt.1.0001d0
!        .or.fly(k).lt.0.0d0.or.fly(kp).lt.0.0d0
          endif
 2021   continue
       enddo
! cycle by j
 2020   continue
    enddo
! cycle by i

 201    format(1x,d13.5)
 202    format(1x,2d13.5)
 203    format(1x,3d13.5)
 204    format(1x,4d13.5)

  return
  end subroutine coll_xyy_lwf
! +-----------------------------------------------------+
  subroutine coll_xxx_lwf(g,fl,ckxx,x,c,ima,prdkrn,nkr)

    implicit none

    integer,intent(in) :: nkr
    real(kind=r8size),intent(inout) :: g(:),fl(:)
    real(kind=r8size),intent(in) ::	ckxx(:,:),x(:), c(:,:)
    integer,intent(in) :: ima(:,:)
    real(kind=r8size),intent(in) :: prdkrn

! ... Locals
   real(kind=r8size):: gmin,x01,x02,x03,gsi,gsj,gsk,gsi_w,gsj_w,gsk_w,gk, &
                       gk_w,fl_gk,fl_gsk,flux,x1,flux_w,g_k_w,g_kp_old,g_kp_w
   integer :: i,ix0,ix1,j,k,kp
! ... Locals

  gmin=g_lim*1.0d3

! ix0 - lower limit of integration by i

  do i=1,nkr-1
   ix0=i
   if(g(i).gt.gmin) goto 2000
  enddo
  2000   continue
  if(ix0.eq.nkr-1) return

! ix1 - upper limit of integration by i
  do i=nkr-1,1,-1
   ix1=i
   if(g(i).gt.gmin) goto 2010
  enddo
  2010   continue

! ... collisions
      do i=ix0,ix1
         if(g(i).le.gmin) goto 2020
         do j=i,ix1
            if(g(j).le.gmin) goto 2021
            k=ima(i,j)
            kp=k+1
            x01=ckxx(i,j)*g(i)*g(j)*prdkrn
            x02=dmin1(x01,g(i)*x(j))
            if(j.ne.k) x03=dmin1(x02,g(j)*x(i))
            if(j.eq.k) x03=x02
            gsi=x03/x(j)
            gsj=x03/x(i)
            gsk=gsi+gsj
            if(gsk.le.gmin) goto 2021
            gsi_w=gsi*fl(i)
            gsj_w=gsj*fl(j)
            gsk_w=gsi_w+gsj_w
            gsk_w=dmin1(gsk_w,gsk)
            g(i)=g(i)-gsi
            g(i)=dmax1(g(i),0.0d0)
            g(j)=g(j)-gsj
  ! new change of 23.01.11                                      (start)
            if(j.ne.k) g(j)=dmax1(g(j),0.0d0)
  ! new change of 23.01.11                                        (end)
            gk=g(k)+gsk

            if(g(j).lt.0.d0.and.gk.le.gmin) then
              g(j)=0.d0
              g(k)=g(k)+gsi
              goto 2021
          endif

            if(gk.le.gmin) goto 2021

            gk_w=g(k)*fl(k)+gsk_w
            gk_w=dmin1(gk_w,gk)

            fl_gk=gk_w/gk
            fl_gsk=gsk_w/gsk
            flux=0.d0
            x1=dlog(g(kp)/gk+1.d-15)
            flux=gsk/x1*(dexp(0.5d0*x1)-dexp(x1*(0.5d0-c(i,j))))
            flux=dmin1(flux,gsk)
            flux=dmin1(flux,gk)
            if(kp.gt.kp_flux_max) flux=0.5d0*flux
            flux_w=flux*fl_gsk
            flux_w=dmin1(flux_w,gsk_w)
            flux_w=dmin1(flux_w,gk_w)
            g(k)=gk-flux
            g(k)=dmax1(g(k),gmin)
            g_k_w=gk_w-flux_w
            g_k_w=dmin1(g_k_w,g(k))
            g_k_w=dmax1(g_k_w,0.0d0)
            fl(k)=g_k_w/g(k)
            g_kp_old=g(kp)
            g(kp)=g(kp)+flux
            g(kp)=dmax1(g(kp),gmin)
            g_kp_w=g_kp_old*fl(kp)+flux_w
            g_kp_w=dmin1(g_kp_w,g(kp))
            fl(kp)=g_kp_w/g(kp)

            if(fl(k).gt.1.0d0.and.fl(k).le.1.0001d0) &
                fl(k)=1.0d0

            if(fl(kp).gt.1.0d0.and.fl(kp).le.1.0001d0) &
                fl(kp)=1.0d0

            if(fl(k).gt.1.0001d0.or.fl(kp).gt.1.0001d0 &
               .or.fl(k).lt.0.0d0.or.fl(kp).lt.0.0d0) then

              print*,    'in subroutine coll_xxx_lwf'
              print*,    'snow - snow = snow'

              if(fl(k).gt.1.0001d0)  print*, 'fl(k).gt.1.0001d0'
              if(fl(kp).gt.1.0001d0) print*, 'fl(kp).gt.1.0001d0'

              if(fl(k).lt.0.0d0)  print*, 'fl(k).lt.0.0d0'
              if(fl(kp).lt.0.0d0) print*, 'fl(kp).lt.0.0d0'

              print*,    'i,j,k,kp'
              print*,     i,j,k,kp
              print*,    'ix0,ix1'
              print*,     ix0,ix1

              print*,   'ckxx(i,j),x01,x02,x03'
                print 204, ckxx(i,j),x01,x02,x03

              print*,   'gsi,gsj,gsk'
                print 203, gsi,gsj,gsk

              print*,   'gsi_w,gsj_w,gsk_w'
                print 203, gsi_w,gsj_w,gsk_w

              print*,   'gk,gk_w'
                print 202, gk,gk_w

              print*,   'fl_gk,fl_gsk'
                print 202, fl_gk,fl_gsk

              print*,   'x1,c(i,j)'
                print 202, x1,c(i,j)

              print*,   'flux'
                print 201, flux

              print*,   'flux_w'
                print 201, flux_w

              print*,   'g_k_w'
                print 201, g_k_w

                print *,  'g_kp_w'
                print 201, g_kp_w

              if(fl(k).lt.0.0d0) print*, &
                 'stop 2022: in subroutine coll_xxx_lwf, fl(k) < 0'

              if(fl(kp).lt.0.0d0) print*, &
                 'stop 2022: in subroutine coll_xxx_lwf, fl(kp) < 0'

              if(fl(k).gt.1.0001d0) print*, &
                 'stop 2022: in sub. coll_xxx_lwf, fl(k) > 1.0001'

              if(fl(kp).gt.1.0001d0) print*, &
                 'stop 2022: in sub. coll_xxx_lwf, fl(kp) > 1.0001'
                    call wrf_error_fatal("in coal_bott sub. coll_xxx_lwf, model stop")
              endif
2021     continue
       enddo
! cycle by j
2020    continue
   enddo
! cycle by i

201    format(1x,d13.5)
202    format(1x,2d13.5)
203    format(1x,3d13.5)
204    format(1x,4d13.5)

 return
 end subroutine coll_xxx_lwf
! +----------------------------------------------------+
 subroutine coll_xyx_lwf (gx,gy,flx,fly,ckxy,x,y, &
					               c,ima,prdkrn,nkr,indc,dm_rime)
	implicit none

	integer,intent(in) :: nkr
	real(kind=r8size),intent(inout) :: gy(:),gx(:),fly(:),flx(:),dm_rime(:)
	real(kind=r8size),intent(in) :: ckxy(:,:),x(:),y(:),c(:,:),prdkrn
	integer,intent(in) :: ima(:,:)

! ... Locals
	real(kind=r8size) :: gmin,x01,x02,x03,gsi,gsj,gsk,gk,flux,x1,gsi_w,gsj_w,gsk_w, &
    	                gk_w,fl_gk,fl_gsk,flux_w,gx_k_w,gx_kp_old,gx_kp_w,frac_split
	integer :: j, jx0, jx1, i, iy0, iy1, jmin, indc, k, kp
! ... Locals

	gmin=g_lim*1.0d3

! jx0 - lower limit of integration by j
        do j=1,nkr-1
           jx0=j
           if(gx(j).gt.gmin) goto 2000
        end do
 2000   continue
        if(jx0.eq.nkr-1) return
! jx1 - upper limit of integration by j
        do j=nkr-1,jx0,-1
           jx1=j
           if(gx(j).gt.gmin) goto 2010
        end do
 2010   continue
! iy0 - lower limit of integration by i
        do i=1,nkr-1
           iy0=i
           if(gy(i).gt.gmin) goto 2001
        end do
 2001   continue
        if(iy0.eq.nkr-1) return
! iy1 - upper limit of integration by i
        do i=nkr-1,iy0,-1
           iy1=i
           if(gy(i).gt.gmin) goto 2011
        end do
 2011   continue

	 do i = 1,nkr
	   dm_rime(i)=0.0
	 end do

! ... collisions :
        do i=iy0,iy1
           if(gy(i).le.gmin) goto 2020
           jmin=i
           if(jmin.eq.nkr-1) return
           if(i.lt.jx0) jmin=jx0-indc
	   		do j=jmin+indc,jx1
              if(gx(j).le.gmin) goto 2021
              k=ima(i,j)
              kp=k+1
              x01=ckxy(j,i)*gy(i)*gx(j)*prdkrn
              x02=dmin1(x01,gy(i)*x(j))
			! new change of 20.01.11                                      (start)
              if(j.ne.k) x03=dmin1(x02,gx(j)*y(i))
              if(j.eq.k) x03=x02
			! new change of 20.01.11                                        (end)
              gsi=x03/x(j)
              gsj=x03/y(i)
              gsk=gsi+gsj
			        if(gsk.le.gmin) goto 2021
              gsi_w=gsi*fly(i)
              gsj_w=gsj*flx(j)
              gsk_w=gsi_w+gsj_w
			        gsk_w=dmin1(gsk_w,gsk)
              gy(i)=gy(i)-gsi
              gy(i)=dmax1(gy(i),0.0d0)
              gx(j)=gx(j)-gsj
			! new change of 20.01.11                                      (start)
              if(j.ne.k) gx(j)=dmax1(gx(j),0.0d0)
			! new change of 20.01.11                                        (end)
              gk=gx(k)+gsk
              if(gk.le.gmin) goto 2021
              gk_w=gx(k)*flx(k)+gsk_w
			        gk_w=dmin1(gk_w,gk)
	            fl_gk=gk_w/gk
              fl_gsk=gsk_w/gsk
              flux=0.d0
              x1=dlog(gx(kp)/gk+1.d-15)
              flux=gsk/x1*(dexp(0.5d0*x1)-dexp(x1*(0.5d0-c(i,j))))
              flux=dmin1(flux,gsk)
              flux=dmin1(flux,gk)

              if(kp.gt.kp_flux_max) flux=0.5d0*flux
              flux_w=flux*fl_gsk
              flux_w=dmin1(flux_w,gsk_w)
              flux_w=dmin1(flux_w,gk_w)
			        frac_split = flux/gsk
              if(frac_split < 0.) frac_split = 0.
	            if(frac_split > 1.) frac_split = 1.
              dm_rime(k)=dm_rime(k)+gsi*(1.-frac_split)
              dm_rime(kp)=dm_rime(kp)+gsi*frac_split
              gx(k)=gk-flux
	            gx(k)=dmax1(gx(k),gmin)

              gx_k_w=gk_w-flux_w
              gx_k_w=dmin1(gx_k_w,gx(k))
              gx_k_w=dmax1(gx_k_w,0.0d0)
              flx(k)=gx_k_w/gx(k)
              gx_kp_old=gx(kp)
              gx(kp)=gx(kp)+flux
              gx(kp)=dmax1(gx(kp),gmin)

              gx_kp_w=gx_kp_old*flx(kp)+flux_w
              gx_kp_w=dmin1(gx_kp_w,gx(kp))

              flx(kp)=gx_kp_w/gx(kp)

              if(flx(k).gt.1.0d0.and.flx(k).le.1.0001d0) &
              flx(k)=1.0d0

              if(flx(kp).gt.1.0d0.and.flx(kp).le.1.0001d0) &
              	flx(kp)=1.0d0

              if(flx(k).gt.1.0001d0.or.flx(kp).gt.1.0001d0 &
              .or.flx(k).lt.0.0d0.or.flx(kp).lt.0.0d0) then

              print*, 'in subroutine coll_xyx_lwf'

              if(flx(k).gt.1.0001d0) &
              print*, 'flx(k).gt.1.0001d0'

              if(flx(kp).gt.1.0001d0) &
              print*, 'flx(kp).gt.1.0001d0'

              if(flx(k).lt.0.0d0)  print*, 'flx(k).lt.0.0d0'
              if(flx(kp).lt.0.0d0) print*, 'flx(kp).lt.0.0d0'

                print*,   'i,j,k,kp'
                print*,    i,j,k,kp

                print*,   'jx0,jx1,iy0,iy1'
                print*,    jx0,jx1,iy0,iy1

                print*,   'gx_kp_old'
                	print 201, gx_kp_old

                print*,   'ckxy(j,i),x01,x02,x03'
                	print 204, ckxy(j,i),x01,x02,x03

                print*,   'gsi,gsj,gsk'
                	print 203, gsi,gsj,gsk

                print*,   'gsi_w,gsj_w,gsk_w'
                	print 203, gsi_w,gsj_w,gsk_w

                print*,   'gk,gk_w'
                	print 202, gk,gk_w

                print*,   'fl_gk,fl_gsk'
                	print 202, fl_gk,fl_gsk

                print*,   'x1,c(i,j)'
                	print 202, x1,c(i,j)

                print*,   'flux'
                	print 201, flux

                print*,   'flux_w'
                	print 201, flux_w

                print*,   'gx_k_w'
                	print 201, gx_k_w

                print*,   'gx_kp_w'
                	print 201, gx_kp_w

        				if(flx(k).lt.0.0d0) print*, &
        					   'stop 2022: in subroutine coll_xyx_lwf, flx(k) < 0'

        				if(flx(kp).lt.0.0d0) print*, &
        					   'stop 2022: in subroutine coll_xyx_lwf, flx(kp) < 0'

        				if(flx(k).gt.1.0001d0) print*, &
        					   'stop 2022: in sub. coll_xyx_lwf, flx(k) > 1.0001'

        				if(flx(kp).gt.1.0001d0) print*, &
        					   'stop 2022: in sub. coll_xyx_lwf, flx(kp) > 1.0001'
                  call wrf_error_fatal("fatal error in module_mp_fast_sbm in coll_xyx_lwf (stop 2022), model stop")
                  stop 2022
               endif
 2021         continue
           enddo
! cycle by j
 2020      continue
        enddo
! cycle by i

 201    format(1x,d13.5)
 202    format(1x,2d13.5)
 203    format(1x,3d13.5)
 204    format(1x,4d13.5)

 return
 end subroutine coll_xyx_lwf
! -------------------------------------------------------+
 subroutine coll_xyz_lwf(gx,gy,gz,flx,fly,flz,ckxy,x,y, &
                        c,ima,prdkrn,nkr,indc)

 implicit none

 integer,intent(in) :: nkr
 real(kind=r8size),intent(inout) :: gx(:),gy(:),gz(:),flx(:),fly(:),flz(:)
 real(kind=r8size),intent(in) :: ckxy(:,:),x(:),y(:),c(:,:)
 integer,intent(in) :: ima(:,:)
 real(kind=r8size),intent(in) :: prdkrn

! ... Locals
 real(kind=r8size) :: gmin,ckxy_ji,x01,x02,x03,gsi,gsj,gsk,gsi_w,gsj_w,gsk_w,gk, &
                      gk_w,fl_gk,fl_gsk,flux,x1,flux_w,gz_k_w,gz_kp_old,gz_kp_w
integer :: j,jx0,jx1,i,iy0,iy1,jmin,indc,k,kp
! ... Locals

gmin = 1.0d-60

! jx0 - lower limit of integration by j
do j=1,nkr-1
 jx0=j
 if(gx(j).gt.gmin) goto 2000
enddo
2000   continue
if(jx0.eq.nkr-1) return

! jx1 - upper limit of integration by j
do j=nkr-1,jx0,-1
 jx1=j
 if(gx(j).gt.gmin) goto 2010
enddo
2010   continue

! iy0 - lower limit of integration by i
do i=1,nkr-1
 iy0=i
 if(gy(i).gt.gmin) goto 2001
enddo
2001   continue
if(iy0.eq.nkr-1) return

! iy1 - upper limit of integration by i
do i=nkr-1,iy0,-1
 iy1=i
 if(gy(i).gt.gmin) goto 2011
enddo
2011   continue

! ... collisions

      do i=iy0,iy1
         if(gy(i).le.gmin) goto 2020
         jmin=i
         if(jmin.eq.nkr-1) return
         if(i.lt.jx0) jmin=jx0-indc
         do j=jmin+indc,jx1
            if(gx(j).le.gmin) goto 2021
            k=ima(i,j)
            kp=k+1
            ckxy_ji=ckxy(j,i)
            x01=ckxy_ji*gy(i)*gx(j)*prdkrn
            x02=dmin1(x01,gy(i)*x(j))
            x03=dmin1(x02,gx(j)*y(i))
            gsi=x03/x(j)
            gsj=x03/y(i)
            gsk=gsi+gsj
            if(gsk.le.gmin) goto 2021
            gsi_w=gsi*fly(i)
            gsj_w=gsj*flx(j)
            gsk_w=gsi_w+gsj_w
            gsk_w=dmin1(gsk_w,gsk)
            gy(i)=gy(i)-gsi
            gy(i)=dmax1(gy(i),0.0d0)

            gx(j)=gx(j)-gsj
            gx(j)=dmax1(gx(j),0.0d0)

            gk=gz(k)+gsk

            if(gk.le.gmin) goto 2021

            gk_w=gz(k)*flz(k)+gsk_w
            gk_w=dmin1(gk_w,gk)

            fl_gk=gk_w/gk

            fl_gsk=gsk_w/gsk

            flux=0.d0

            x1=dlog(gz(kp)/gk+1.d-15)

            flux=gsk/x1*(dexp(0.5d0*x1)-dexp(x1*(0.5d0-c(i,j))))
            flux=dmin1(flux,gsk)
            flux=dmin1(flux,gk)

            if(kp.gt.kp_flux_max) flux=0.5d0*flux

            flux_w=flux*fl_gsk
            flux_w=dmin1(flux_w,gsk_w)
            flux_w=dmin1(flux_w,gk_w)

            gz(k)=gk-flux
            gz(k)=dmax1(gz(k),gmin)

            gz_k_w=gk*fl_gk-flux_w
            gz_k_w=dmin1(gz_k_w,gz(k))
            gz_k_w=dmax1(gz_k_w,0.0d0)

            flz(k)=gz_k_w/gz(k)

            gz_kp_old=gz(kp)

            gz(kp)=gz(kp)+flux
            gz(kp)=dmax1(gz(kp),gmin)

            gz_kp_w=gz_kp_old*flz(kp)+flux_w
            gz_kp_w=dmin1(gz_kp_w,gz(kp))

            flz(kp)=gz_kp_w/gz(kp)

            if(flz(k).gt.1.0d0.and.flz(k).le.1.0001d0) &
            flz(k)=1.0d0

            if(flz(kp).gt.1.0d0.and.flz(kp).le.1.0001d0) &
            flz(kp)=1.0d0

            if(flz(k).gt.1.0001d0.or.flz(kp).gt.1.0001d0 &
            .or.flz(k).lt.0.0d0.or.flz(kp).lt.0.0d0) then

            print*,    'in subroutine coll_xyz_lwf'

            if(flz(k).gt.1.0001d0)  print*, 'flz(k).gt.1.0001d0'
            if(flz(kp).gt.1.0001d0) print*, 'flz(kp).gt.1.0001d0'

            if(flz(k).lt.0.0d0)  print*, 'flz(k).lt.0.0d0'
            if(flz(kp).lt.0.0d0) print*, 'flz(kp).lt.0.0d0'

            print*,   'i,j,k,kp'
            print*,    i,j,k,kp

            print*,   'jx0,jx1,iy0,iy1'
            print*,    jx0,jx1,iy0,iy1

            print*,   'gz_kp_old'
            print 201, gz_kp_old

            print*,   'x01,x02,x03'
            print 203, x01,x02,x03

            print*,   'gsi,gsj,gsk'
            print 203, gsi,gsj,gsk

            print*,   'gsi_w,gsj_w,gsk_w'
            print 203, gsi_w,gsj_w,gsk_w

            print*,   'gk,gk_w'
            print 202, gk,gk_w

            print*,   'fl_gk,fl_gsk'
            print 202, fl_gk,fl_gsk

            print*,   'x1,c(i,j)'
            print 202, x1,c(i,j)

            print*,   'flux'
            print 201, flux

            print*,   'flux_w'
            print 201, flux_w

            print*,   'gz_k_w'
            print 201, gz_k_w

            print*,   'gz_kp_w'
            print 204, gz_kp_w

            if(flz(k).lt.0.0d0) print*, &
            'stop 2022: in subroutine coll_xyz_lwf, flz(k) < 0'

            if(flz(kp).lt.0.0d0) print*, &
               'stop 2022: in subroutine coll_xyz_lwf, flz(kp) < 0'

            if(flz(k).gt.1.0001d0) print*, &
               'stop 2022: in sub. coll_xyz_lwf, flz(k) > 1.0001'

            if(flz(kp).gt.1.0001d0) print*, &
               'stop 2022: in sub. coll_xyz_lwf, flz(kp) > 1.0001'
              call wrf_error_fatal("fatal error: in sub. coll_xyz_lwf,model stop")
            endif
2021         continue
         enddo
! cycle by j
2020      continue
      enddo
! cycle by i

201    format(1x,d13.5)
202    format(1x,2d13.5)
203    format(1x,3d13.5)
204    format(1x,4d13.5)

 return
 end subroutine coll_xyz_lwf
! -----------------------------------------------+
 subroutine coll_xxy_lwf(gx,gy,flx,fly,ckxx,x, &
                        c,ima,prdkrn,nkr)

  implicit none

  integer,intent(in) :: nkr
  real(kind=r8size),intent(inout):: gx(nkr),gy(nkr),flx(nkr),fly(nkr)
  real(kind=r8size),intent(in) :: x(nkr),ckxx(nkr,nkr),c(nkr,nkr)
  real(kind=r8size),intent(in) :: prdkrn
  integer,intent(in) :: ima(nkr,nkr)

! ... Locals
  real(kind=r8size) :: gmin,ckxx_ij,x01,x02,x03,gsi,gsj,gsk,gsi_w,gsj_w,gsk_w, &
                       gk,gk_w,flux,flux_w,fl_gk,fl_gsk,x1,gy_k_w,gy_kp_w, &
                       gy_kp_old
  integer::i,ix0,ix1,j,k,kp
! ... Locals

!gmin=g_lim*1.0d3
gmin = 1.0d-60

! ix0 - lower limit of integration by i
do i=1,nkr-1
   ix0=i
   if(gx(i).gt.gmin) goto 2000
enddo
2000   continue
if(ix0.eq.nkr-1) return

! ix1 - upper limit of integration by i
do i=nkr-1,ix0,-1
   ix1=i
   if(gx(i).gt.gmin) goto 2010
enddo
2010   continue

! ... collisions
      do i=ix0,ix1
         if(gx(i).le.gmin) goto 2020
         do j=i,ix1
            if(gx(j).le.gmin) goto 2021
            k=ima(i,j)
            kp=k+1
            ckxx_ij = ckxx(i,j)
            x01=ckxx_ij*gx(i)*gx(j)*prdkrn
            x02=dmin1(x01,gx(i)*x(j))
            x03=dmin1(x02,gx(j)*x(i))
            gsi=x03/x(j)
            gsj=x03/x(i)
            gsk=gsi+gsj

            if(gsk.le.gmin) goto 2021

            gsi_w=gsi*flx(i)
            gsj_w=gsj*flx(j)
            gsk_w=gsi_w+gsj_w
            gsk_w=dmin1(gsk_w,gsk)

            gx(i)=gx(i)-gsi
            gx(i)=dmax1(gx(i),0.0d0)

            gx(j)=gx(j)-gsj
            gx(j)=dmax1(gx(j),0.0d0)

            gk=gy(k)+gsk

            if(gk.le.gmin) goto 2021

            gk_w=gy(k)*fly(k)+gsk_w
            gk_w=dmin1(gk_w,gk)
            fl_gk=gk_w/gk
            fl_gsk=gsk_w/gsk

            flux=0.d0

            x1=dlog(gy(kp)/gk+1.d-15)
            !		print *,'nir1',gy(kp),gk,kp,i,j
            flux=gsk/x1*(dexp(0.5d0*x1)-dexp(x1*(0.5d0-c(i,j))))
            flux=dmin1(flux,gsk)
            flux=dmin1(flux,gk)

            if(kp.gt.kp_flux_max) flux=0.5d0*flux

            flux_w=flux*fl_gsk
            flux_w=dmin1(flux_w,gk_w)
            flux_w=dmin1(flux_w,gsk_w)
            flux_w=dmax1(flux_w,0.0d0)

            gy(k)=gk-flux
            gy_k_w=gk*fl_gk-flux_w
            gy_k_w=dmin1(gy_k_w,gy(k))
            gy_k_w=dmax1(gy_k_w,0.0d0)
            !		print *,'nirxxylwf4',k,gy(k),gy_k_w,x1,flux
            if (gy(k)/=0.0) then
              fly(k)=gy_k_w/gy(k)
            else
              fly(k)=0.0d0
            endif
            gy_kp_old=gy(kp)
            gy(kp)=gy(kp)+flux
            gy_kp_w=gy_kp_old*fly(kp)+flux_w
            gy_kp_w=dmin1(gy_kp_w,gy(kp))
            if (gy(kp)/=0.0) then
              fly(kp)=gy_kp_w/gy(kp)
            else
              fly(kp)=0.0d0
            endif
2021  continue

      if(fly(k).gt.1.0d0.and.fly(k).le.1.0001d0) &
          fly(k)=1.0d0

        if(fly(kp).gt.1.0d0.and.fly(kp).le.1.0001d0) &
          fly(kp)=1.0d0

         end do
! cycle by j
2020      continue
      end do
! cycle by i

 return
 end subroutine coll_xxy_lwf
! +-------------------------------------------+
 SUBROUTINE INTERPOL_SE (NH, H_TAB, X_TAB, H, X)

  implicit none
! ### Interface
 integer :: NH
 real(kind=r4size) :: H_TAB(NH), X_TAB(NH)
 real(kind=r8size) :: H, X
! ### Interface
 integer :: I, J

 IF(H > H_TAB(1)) THEN
    X = X_TAB(1)
    RETURN
 ENDIF

 IF(H < H_TAB(NH)) THEN
    X = X_TAB(NH)
    RETURN
 ENDIF

 DO I = 2,NH
    IF(H > H_TAB(I)) THEN
       J = I-1
       X = X_TAB(J)+(X_TAB(I)-X_TAB(J))/ &
           (H_TAB(I)-H_TAB(J))*(H-H_TAB(J))

       RETURN
    ENDIF
 ENDDO

 RETURN
 END SUBROUTINE INTERPOL_SE 
! +-------------------------------------------------------------------------------+
    subroutine modkrn_KS (tt,qq,pp,rho,factor_t,ttcoal,ICase,Icondition, &
                          Iin,Jin,Kin)

    implicit none

    real(kind=r8size),intent(in) :: tt, pp
    real(kind=r8size),intent(inout) :: qq
    real(kind=r4size),intent(in) :: ttcoal, rho
    real(kind=r8size),intent(out) :: factor_t
    integer :: ICase, Iin, Jin, Kin, Icondition

    real(kind=r8size) :: satq2, temp, epsf, tc, ttt1, ttt, qs2, qq1, dele, tc_min, &
                          tc_max, factor_max, factor_min, f, t, a, b, c, p, d
    real(kind=r8size) :: at, bt, ct, dt
    real(kind=r8size) :: AA,BB,CC,DD,Es,Ew,AA1_MY,BB1_MY
    real(kind=r4size) :: tt_r, T_tab(7), SE_tab(7)

    satq2(t,p) = 3.80d3*(10**(9.76421d0-2667.1d0/t))/p
    temp(a,b,c,d,t) = d*t*t*t+c*t*t+b*t+a


    tc = tt - 273.15
    if (tc > 0.0) return  

    SELECT CASE (ICase)

    CASE(1)

        !satq2(t,p) = 3.80d3*(10**(9.76421d0-2667.1d0/t))/p
        !temp(a,b,c,d,t) = d*t*t*t+c*t*t+b*t+a

        data at, bt, ct, dt /0.88333d0,  0.0931878d0,  0.0034793d0,  4.5185186d-05/

        if(qq.le.0.0) qq = 1.0e-15
          epsf = 0.5d0
          tc = tt - 273.15

          ttt1	=temp(at,bt,ct,dt,tc)
          ttt	=ttt1
          qs2	=satq2(tt,pp)
          qq1	=qq*(0.622d0+0.378d0*qs2)/(0.622d0+0.378d0*qq)/qs2
          dele	=ttt*qq1

          if(tc.ge.-6.0d0) then
            factor_t = dele
            if(factor_t.lt.epsf) factor_t = epsf
            if(factor_t.gt.1.0d0) factor_t = 1.0d0
          endif

          if (Icondition == 0) then
            if(tc.ge.-12.5d0 .and. tc.lt.-6.0d0) factor_t = 0.5D0  ! 0.5d0 !### (KS-ICE-SNOW)
            if(tc.ge.-17.0d0 .and. tc.lt.-12.5d0) factor_t = 1.0
            if(tc.ge.-20.0d0 .and. tc.lt.-17.0d0) factor_t = 0.4d0
          else
            if(tc.ge.-12.5d0 .and. tc.lt.-6.0d0) factor_t = 0.3D0  ! 0.5d0 !### (KS-ICE-SNOW)
            if(tc.ge.-17.0d0 .and. tc.lt.-12.5d0) factor_t = 0.1d0
            if(tc.ge.-20.0d0 .and. tc.lt.-17.0d0) factor_t = 0.05d0
          endif

        if(tc.lt.-20.0d0) then
          tc_min = ttcoal-273.15d0
          tc_max = -20.0d0
          if(Icondition == 0)then
            factor_max = 0.4d0
            factor_min = 0.0d0
          else
            factor_max = 0.05d0
            factor_min = 0.0d0
          endif

          f = factor_min + (tc-tc_min)*(factor_max-factor_min)/ &
                          (tc_max-tc_min)
          factor_t = f
        ! in case tc.lt.-20.0d0
        endif

        if(tc.lt.-40.0d0) then
          factor_t = 0.0d0
        endif

        if (factor_t > 1.0) factor_t = 1.0

        if(tc.ge.0.0d0) then
          factor_t = 1.0d0
        endif

    CASE(11)

    ! ... Dashed-dotted (linear)
    T_tab =  [0.0, -0.813, -5.26, -10.13, -14.63, -20.02, -40.0 ]
    SE_tab = [10.0**(-0.693), 10.0**(-0.72), 10.0**(-0.877), 10.0**(-1.050),  10.0**(-1.212),  10.0**(-1.401),  10.0**(-2.082) ]


    CALL INTERPOL_SE (size(SE_tab), T_TAB, SE_TAB, TC, factor_t)

      if(tc < -40.0d0) then
          factor_t = 0.0d0
      endif

      if (factor_t > 1.0) factor_t = 1.0

      if(tc > 0.0d0) then
          factor_t = 1.0d0
      endif

    END SELECT

  return
  end subroutine modkrn_KS
  ! +-----------------------------------------------------------+
  subroutine coll_breakup_KS (gt_mg, xt_mg, jmax, dt, jbreak, &
                              PKIJ, QKJ, NKRinput, NKR)

    implicit none
  ! ... Interface
    integer,intent(in) :: jmax, jbreak, NKRInput, NKR
    real(kind=r8size),intent(in) :: xt_mg(:), dt
    real(kind=r4size),intent(in) :: pkij(:,:,:),qkj(:,:)
    real(kind=r8size),intent(inout) :: gt_mg(:)
  ! ... Interface

  ! ... Locals
  ! ke = jbreak
  integer,parameter :: ia=1, ja=1, ka=1
  integer :: ie, je, ke, nkrdiff, jdiff, k, i, j
  real(kind=r8size),parameter :: eps = 1.0d-20
  real(kind=r8size) :: gt(jmax), xt(jmax+1), ft(jmax), fa(jmax), dg(jmax), df(jmax), dbreak(jbreak) &
                     ,amweight(jbreak), gain, aloss
  ! ... Locals

  ie=jbreak
  je=jbreak
  ke=jbreak

  !input variables

  ! gt_mg : mass distribution function of Bott
  ! xt_mg : mass of bin in mg
  ! jmax  : number of bins
  ! dt    : timestep in s

  !in CGS

  nkrdiff = nkrinput-nkr
  do j=1,jmax
  xt(j)=xt_mg(j)
  gt(j)=gt_mg(j)
  ft(j)=gt(j)/xt(j)/xt(j)
  enddo

  !shift between coagulation and breakup grid
  jdiff=jmax-jbreak

  !initialization
  !shift to breakup grid
  fa = 0.0
  do k=1,ke-nkrdiff
    fa(k)=ft(k+jdiff+nkrdiff)
  enddo

  !breakup: bleck's first order method
  !pkij: gain coefficients
  !qkj : loss coefficients

  xt(jmax+1)=xt(jmax)*2.0d0

  amweight = 0.0
  dbreak = 0.0
  do k=1,ke-nkrdiff
    gain=0.0d0
    do i=1,ie-nkrdiff
      do j=1,i
        gain=gain+fa(i)*fa(j)*pkij(k,i,j)
      enddo
    enddo
    aloss=0.0d0
    do j=1,je-nkrdiff
      aloss=aloss+fa(j)*qkj(k,j)
    enddo
    j=jmax-jbreak+k+nkrdiff
    amweight(k)=2.0/(xt(j+1)**2.0-xt(j)**2.0)
    dbreak(k)=amweight(k)*(gain-fa(k)*aloss)

    if(dbreak(k) .ne. dbreak(k)) then
      print*,dbreak(k),amweight(k),gain,fa(k),aloss
      print*,"-"
      print*,dbreak
      print*,"-"
      print*,amweight
      print*,"-"
      print*,j,jmax,jbreak,k,nkrdiff
      print*,"-"
      print*,fa
      print*,"-"
      print*,xt
      print*,"-"
      print*,gt
      call wrf_error_fatal(" inside coll_breakup, NaN, model stop")
    endif
  enddo

  !shift rate to coagulation grid
  df = 0.0d0
  do j=1,jdiff+nkrdiff
    df(j)=0.0d0
  enddo

  do j=1,ke-nkrdiff
    df(j+jdiff)=dbreak(j)
  enddo

  !transformation to mass distribution function g(ln x)
  do j=1,jmax
    dg(j)=df(j)*xt(j)*xt(j)
  enddo

  !time integration

  do j=1,jmax
    gt(j)=gt(j)+dg(j)*dt
  !	if(gt(j)<0.0) then
    !print*, 'gt(j) < 0'
    !print*, 'j'
    !print*,  j
    !print*, 'dg(j),dt,gt(j)'
    !print*,  dg(j),dt,gt(j)
    !hlp=dmin1(gt(j),hlp)
  !	gt(j) = eps
  !	print*,'kr',j
  !	print*,'gt',gt
  !	print*,'dg',dg
  !	print*,'gt_mg',gt_mg
    !stop "in coll_breakup_ks gt(kr) < 0.0 "
  !	endif
  enddo

   gt_mg = gt

  return
  end subroutine coll_breakup_KS
  ! +----------------------------------------------------+
  subroutine courant_bott_KS(xl, nkr, chucm, ima, scal)

    implicit none

    integer,intent(in) :: nkr
    real,intent(in) :: xl(:)
    real(kind=r8size),intent(inout) :: chucm(:,:)
    integer,intent(inout) :: ima(:,:)
    real(kind=r8size),intent(in) :: scal

    ! ... Locals
    integer :: k, kk, j, i
    real(kind=r8size) :: x0, xl_mg(nkr), dlnr
    ! ... Locals

    ! ima(i,j) - k-category number,
    ! chucm(i,j)   - courant number :
    ! logarithmic grid distance(dlnr) :

      !xl_mg(0)=xl_mg(1)/2
      xl_mg(1:nkr) = xl(1:nkr)*1.0D3

      dlnr=dlog(2.0d0)/(3.0d0*scal)

      do i = 1,nkr
         do j = i,nkr
            x0 = xl_mg(i) + xl_mg(j)
            do k = j,nkr
              !if(k == 1) goto 1000 ! ### (KS)
               kk = k
               if(k == 1) goto 1000
               if(xl_mg(k) >= x0 .and. xl_mg(k-1) < x0) then
                 chucm(i,j) = dlog(x0/xl_mg(k-1))/(3.d0*dlnr)
                 if(chucm(i,j) > 1.0d0-1.d-08) then
                   chucm(i,j) = 0.0d0
                   kk = kk + 1
                 endif
                 ima(i,j) = min(nkr-1,kk-1)
                 !if (ima(i,j) == 0) then
                 !	print*,"ima==0"
                 !endif
                 goto 2000
               endif
               1000 continue
            enddo
            2000  continue
            !if(i.eq.nkr.or.j.eq.nkr) ima(i,j)=nkr
            chucm(j,i) = chucm(i,j)
            ima(j,i) = ima(i,j)
         enddo
      enddo

      return
      end subroutine courant_bott_KS
  ! +----------------------------------+
end module module_mp_SBM_Collision
! +-----------------------------------------------------------------------------+
! +-----------------------------------------------------------------------------+
 module module_mp_SBM_Auxiliary

 private
 public :: POLYSVP, JERRATE_KS, JERTIMESC_KS, JERSUPSAT_KS, &
 		       JERDFUN_KS, JERDFUN_NEW_KS, Relaxation_Time

 ! Kind paramater
 INTEGER, PARAMETER, PRIVATE:: R8SIZE = 8
 INTEGER, PARAMETER, PRIVATE:: R4SIZE = 4

 INTEGER,PARAMETER :: ISIGN_KO_1 = 0, ISIGN_KO_2 = 0,  ISIGN_3POINT = 1,  &
                      IDebug_Print_DebugModule = 1
 DOUBLE PRECISION,PARAMETER::COEFF_REMAPING = 0.0066667D0
 DOUBLE PRECISION,PARAMETER::VENTPL_MAX = 5.0D0

 DOUBLE PRECISION,PARAMETER::RW_PW_MIN = 1.0D-10
 DOUBLE PRECISION,PARAMETER::RI_PI_MIN = 1.0D-10
 DOUBLE PRECISION,PARAMETER::RW_PW_RI_PI_MIN = 1.0D-10
 DOUBLE PRECISION,PARAMETER::RATIO_ICEW_MIN = 1.0D-4

 contains
! +----------------------------------------------------------+
 double precision FUNCTION POLYSVP (TT,ITYPE)

 implicit none

 real,intent(in) :: TT
 integer,intent(in) :: ITYPE

 real(4),parameter :: C1 = -9.09718E0, C2 = -3.56654E0, C3 = 0.876793E0, C4 = 0.78583503E0, &
                      AA1_MY = 2.53E12, BB1_MY = 5.42E3, AA2_MY = 3.41E13, BB2_MY = 6.13E3
 real(4) :: ES1N, ES2N

 method_select: SELECT CASE(ITYPE)

 ! liquid
 Case(0)
   ES1N = AA1_MY*EXP(-BB1_MY/TT)
   POLYSVP = ES1N ! [dyn/cm2] to [mb]

 ! ice  
 Case(1)
   ES2N = AA2_MY*EXP(-BB2_MY/TT)
   POLYSVP = ES2N ! [dyn/cm2] to [mb]

 END SELECT method_select

 return
 end function POLYSVP
! + -------------------------------------------------------- +
      SUBROUTINE JERRATE_KS (xlS, &
        						        TP,PP, &
						                Vxl,RIEC,RO1BL, &
						                B11_MY, &
						                ID,IN,fl1,NKR,ICEMAX)

		IMPLICIT NONE
! ... Interface
		INTEGER,INTENT(IN) :: ID, IN, NKR, ICEMAX
		REAL(KIND=r4size),INTENT(IN) :: RO1BL(NKR,ID),RIEC(NKR,ID),FL1(NKR)
		REAL(KIND=r4size),INTENT(INOUT) :: B11_MY(NKR,ID)
		REAL(KIND=r8size),INTENT(IN) :: PP, TP, xlS(NKR,ID),Vxl(NKR,ID)
! ... Interface
! ... Locals
		INTEGER :: KR, nskin(nkr), ICE
		REAL(KIND=r4size) :: VENTPLM(NKR), FD1(NKR,ICEMAX),FK1(NKR,ICEMAX), xl_MY1(NKR,ICEMAX), &
        		              AL1_MY(2),ESAT1(2), TPreal
		REAL(KIND=r8size) :: PZERO, TZERO, CONST, D_MY, COEFF_VISCOUS, SHMIDT_NUMBER,     &
							           A, B, RVT, SHMIDT_NUMBER03, XLS_KR_ICE, RO1BL_KR_ICE, VXL_KR_ICE, REINOLDS_NUMBER, &
							           RESHM, VENTPL, CONSTL, DETL

		REAL(KIND=r4size) :: deg01,deg03

! A1L_MY - CONSTANTS FOR "MAXWELL": MKS
		REAL(KIND=r8size),parameter:: RV_MY=461.5D4, CF_MY=2.4D3, D_MYIN=0.211D0

! CGS :

! RV_MY, CM*CM/SEC/SEC/KELVIN - INDIVIDUAL GAS CONSTANT
!                               FOR WATER VAPOUR
	!RV_MY=461.5D4

! D_MYIN, CM*CM/SEC - COEFFICIENT OF DIFFUSION OF WATER VAPOUR

	!D_MYIN=0.211D0

! PZERO, DYNES/CM/CM - REFERENCE PRESSURE

	PZERO=1.013D6

! TZERO, KELVIN - REFERENCE TEMPERATURE

	TZERO=273.15D0

do kr=1,nkr
	if (in==2 .and. fl1(kr)==0.0 .or. in==6 .or. in==3 .and. tp<273.15) then
	   nskin(kr) = 2
	else !in==1 or in==6 or lef/=0
	   nskin(kr) = 1
	endif
enddo

! CONSTANTS FOR CLAUSIUS-CLAPEYRON EQUATION :

! A1_MY(1),G/SEC/SEC/CM

!	A1_MY(1)=2.53D12

! A1_MY(2),G/SEC/SEC/CM

!	A1_MY(2)=3.41D13

! BB1_MY(1), KELVIN

!	BB1_MY(1)=5.42D3

! BB1_MY(2), KELVIN

!	BB1_MY(2)=6.13D3

! AL1_MY(1), CM*CM/SEC/SEC - LATENT HEAT OF VAPORIZATION

	AL1_MY(1)=2.5D10

! AL1_MY(2), CM*CM/SEC/SEC - LATENT HEAT OF SUBLIMATION

	AL1_MY(2)=2.834D10

! CF_MY, G*CM/SEC/SEC/SEC/KELVIN - COEFFICIENT OF
!                                  THERMAL CONDUCTIVITY OF AIR
	!CF_MY=2.4D3

  DEG01=1.0/3.0
  DEG03=1.0/3.0

	CONST=12.566372D0

! coefficient of diffusion

	D_MY=D_MYIN*(PZERO/PP)*(TP/TZERO)**1.94D0

! coefficient of viscousity

! COEFF_VISCOUS=0.13 cm*cm/sec

        COEFF_VISCOUS=0.13D0

! Shmidt number

        SHMIDT_NUMBER=COEFF_VISCOUS/D_MY

! Constants used for calculation of Reinolds number

        A=2.0D0*(3.0D0/4.0D0/3.141593D0)**DEG01
        B=A/COEFF_VISCOUS

	RVT=RV_MY*TP
  !	ESAT1(IN)=A1_MY(IN)*DEXP(-BB1_MY(IN)/TP)
  !	if (IN==1) then
  !            ESAT1(IN)=EW(TP)
  !	ELSE
  !            ESAT1(IN)=EI(TP)
  !	endif

		! ... (KS) - update the saturation vapor pressure
		!ESAT1(1)=EW(TP)
    !ESAT1(2)=EI(TP)
		TPreal = TP
		ESAT1(1) = POLYSVP(TPreal,0)
		ESAT1(2) = POLYSVP(TPreal,1)

	   DO KR=1,NKR
	      VENTPLM(KR)=0.0D0
    ENDDO

		SHMIDT_NUMBER03=SHMIDT_NUMBER**DEG03

	   DO ICE=1,ID
	      DO KR=1,NKR

          xlS_KR_ICE=xlS(KR,ICE)
          RO1BL_KR_ICE=RO1BL(KR,ICE)
          Vxl_KR_ICE=Vxl(KR,ICE)
! Reynolds numbers
          REINOLDS_NUMBER= &
              B*Vxl_KR_ICE*(xlS_KR_ICE/RO1BL_KR_ICE)**DEG03
          RESHM=DSQRT(REINOLDS_NUMBER)*SHMIDT_NUMBER03

          IF(REINOLDS_NUMBER<2.5D0) THEN
            VENTPL=1.0D0+0.108D0*RESHM*RESHM
            VENTPLM(KR)=VENTPL
          ELSE
            VENTPL=0.78D0+0.308D0*RESHM
            VENTPLM(KR)=VENTPL
          ENDIF

        ENDDO
! cycle by KR

! VENTPL_MAX is given in MICRO.PRM include file

	     DO KR=1,NKR

        VENTPL=VENTPLM(KR)

        IF(VENTPL>VENTPL_MAX) THEN
          VENTPL=VENTPL_MAX
          VENTPLM(KR)=VENTPL
        ENDIF

        CONSTL=CONST*RIEC(KR,ICE)

        FD1(KR,ICE)=RVT/D_MY/ESAT1(nskin(kr))
        FK1(KR,ICE)=(AL1_MY(nskin(kr))/RVT-1.0D0)*AL1_MY(nskin(kr))/CF_MY/TP

        xl_MY1(KR,ICE)=VENTPL*CONSTL
        ! growth rate
        DETL=FK1(KR,ICE)+FD1(KR,ICE)
        B11_MY(KR,ICE)=xl_MY1(KR,ICE)/DETL

       ENDDO
! cycle by KR

      ENDDO
! cycle by ICE

	RETURN
	END SUBROUTINE JERRATE_KS

! SUBROUTINE JERRATE
! ................................................................................
	SUBROUTINE JERTIMESC_KS (FI1,X1,SFN11, &
        				  	       B11_MY,CF,ID,NKR,ICEMAX,COL)

	IMPLICIT NONE

! ... Interface
	INTEGER,INTENT(IN) :: ID,NKR,ICEMAX
	REAL(KIND=r4size),INTENT(in) :: B11_MY(NKR,ID), FI1(NKR,ID), COL, CF
	REAL(KIND=r8size),INTENT(in) :: X1(NKR,ID)
	REAL(KIND=r4size),INTENT(out) :: SFN11(ID)
! ... Interface

! ... Locals
	INTEGER :: ICE, KR
	REAL(KIND=r4size) :: SFN11S, FK, DELM, FUN, B11
! ... Locals

	DO ICE=1,ID
     SFN11S=0.0D0
 	   SFN11(ICE)=CF*SFN11S
   DO KR=1,NKR
! value of size distribution functions
      	FK=FI1(KR,ICE)
! delta-m
      	DELM=X1(KR,ICE)*3.0D0*COL
! integral's expression
	      	FUN=FK*DELM
! values of integrals
	      	B11=B11_MY(KR,ICE)
		  	SFN11S=SFN11S+FUN*B11
  ENDDO
! cycle by kr
! correction
 	SFN11(ICE)=CF*SFN11S
  ENDDO

! cycle by ice

	RETURN
	END SUBROUTINE JERTIMESC_KS
! +--------------------------------------------------------+
	SUBROUTINE JERSUPSAT_KS (DEL1,DEL2,DEL1N,DEL2N, &
			                     RW,PW,RI,PI, &
          			           DT,DEL1INT,DEL2INT,DYN1,DYN2, &
          			           ISYM1,ISYM2,ISYM3,ISYM4,ISYM5)

		IMPLICIT NONE
! ... Interface
		INTEGER,INTENT(INOUT) :: 		ISYM1, ISYM2(:), ISYM3, ISYM4, ISYM5
		REAL(KIND=r4size),INTENT(IN) ::   DT, DYN1, DYN2
		REAL(KIND=r8size),INTENT(IN) :: 	DEL1, DEL2
		REAL(KIND=r8size),INTENT(INOUT) :: DEL1N,DEL2N,DEL1INT,DEL2INT,RW, PW, RI, PI
! ... Interface
! ... Locals
	   INTEGER :: I, ISYMICE, IRW, IPW, IRI, IPI
	   REAL(KIND=r8size) :: X, EXPM1, DETER, EXPR, EXPP, A, ALFA, BETA, GAMA, G31, G32, G2, EXPB, EXPG, &
	   				  C11, C21, C12, C22, A1DEL1N, A2DEL1N, A3DEL1N, A4DEL1N, A1DEL1INT, A2DEL1INT, &
							A3DEL1INT, A4DEL1INT, A1DEL2N, A2DEL2N, A3DEL2N , A4DEL2N, A1DEL2INT, A2DEL2INT, &
							A3DEL2INT, A4DEL2INT, A5DEL2INT
! ... Locals

		EXPM1(x)=x+x*x/2.0D0+x*x*x/6.0D0+x*x*x*x/24.0D0+ &
                 x*x*x*x*x/120.0D0

  ISYMICE = sum(ISYM2) + ISYM3 + ISYM4 + ISYM5
  IRW = 1
  IPW = 1
  IRI = 1
  IPI = 1

	IF(AMAX1(RW,PW,RI,PI)<=RW_PW_RI_PI_MIN) THEN

    RW = 0.0
    IRW = 0
    PW = 0.0
    IPW = 0
    RI = 0.0
    IRI = 0
    PI = 0.0
    IPI = 0
    ISYM1 = 0
    ISYMICE = 0

	ELSE

    IF(DMAX1(RW,PW)>RW_PW_MIN) THEN

			! ... (KS) - A zero can pass through, assign a minimum value
			IF(RW < RW_PW_MIN*RW_PW_MIN) THEN
        RW = 1.0D-20
        IRW = 0
      ENDIF
      IF(PW < RW_PW_MIN*RW_PW_MIN)THEN
        PW = 1.0D-20
        IPW = 0
      ENDIF

      IF(DMAX1(PI/PW,RI/RW)<=RATIO_ICEW_MIN) THEN
			  ! ... only water
        RI = 0.0
        IRI = 0
        PI = 0.0
        IPI = 0
        ISYMICE = 0
      ENDIF

      IF(DMIN1(PI/PW,RI/RW)>1.0D0/RATIO_ICEW_MIN) THEN
			  ! ... only ice
        RW = 0.0
        IRW = 0
        PW = 0.0
        IPW = 0
        ISYM1 = 0
      ENDIF

    ELSE
			! only ice
			RW = 0.0
      IRW = 0
      PW = 0.0
      IPW = 0
      ISYM1 = 0

	  ENDIF
	 ENDIF

	IF(ISYMICE == 0)THEN
		ISYM2 = 0
		ISYM3 = 0
		ISYM4 = 0
		ISYM5 = 0
	ENDIF

    DETER=RW*PI-PW*RI


    IF(IRW == 0 .AND. IRI == 0) THEN

          DEL1N=DEL1+DYN1*DT
          DEL2N=DEL2+DYN2*DT
          DEL1INT=DEL1*DT+DYN1*DT*DT/2.0D0
          DEL2INT=DEL2*DT+DYN2*DT*DT/2.0D0

          GOTO 100

    ENDIF

! solution of equation for supersaturation with
! different DETER values

    IF(IRI == 0) THEN
! ... only water                                                     (start)

      EXPR=EXP(-RW*DT)
      IF(ABS(RW*DT)>1.0E-6) THEN
        DEL1N=DEL1*EXPR+(DYN1/RW)*(1.0D0-EXPR)
        DEL2N=PW*DEL1*EXPR/RW-PW*DYN1*DT/RW- &
              PW*DYN1*EXPR/(RW*RW)+DYN2*DT+ &
              DEL2-PW*DEL1/RW+PW*DYN1/(RW*RW)
        DEL1INT=-DEL1*EXPR/RW+DYN1*DT/RW+ &
                 DYN1*EXPR/(RW*RW)+DEL1/RW-DYN1/(RW*RW)
        DEL2INT=PW*DEL1*EXPR/(-RW*RW)-PW*DYN1*DT*DT/(2.0D0*RW)+ &
                PW*DYN1*EXPR/(RW*RW*RW)+DYN2*DT*DT/2.0D0+ &
                DEL2*DT-PW*DEL1*DT/RW+PW*DYN1*DT/(RW*RW)+ &
                PW*DEL1/(RW*RW)-PW*DYN1/(RW*RW*RW)
        GOTO 100
! in case DABS(RW*DT)>1.0D-6
	     ELSE

! in case DABS(RW*DT)<=1.0D-6

          EXPR=EXPM1(-RW*DT)
          DEL1N=DEL1+DEL1*EXPR+(DYN1/RW)*(0.0D0-EXPR)
          DEL2N=PW*DEL1*EXPR/RW-PW*DYN1*DT/RW- &
                   PW*DYN1*EXPR/(RW*RW)+DYN2*DT+DEL2
          DEL1INT=-DEL1*EXPR/RW+DYN1*DT/RW+DYN1*EXPR/(RW*RW)
          DEL2INT=PW*DEL1*EXPR/(-RW*RW)-PW*DYN1*DT*DT/(2.0D0*RW)+ &
                     PW*DYN1*EXPR/(RW*RW*RW)+DYN2*DT*DT/2.0D0+ &
                     DEL2*DT-PW*DEL1*DT/RW+PW*DYN1*DT/(RW*RW)
          GOTO 100

	       ENDIF
! ... only water                                                    (end)

! in case RI==0.0D0
    ENDIF

    IF(IRW == 0) THEN
! ... only ice                                                    (start)

      EXPP=EXP(-PI*DT)

      IF(ABS(PI*DT)>1.0E-6) THEN

        DEL2N = DEL2*EXPP+(DYN2/PI)*(1.0D0-EXPP)
        DEL2INT = -DEL2*EXPP/PI+DYN2*DT/PI+ &
                   DYN2*EXPP/(PI*PI)+DEL2/PI-DYN2/(PI*PI)
        DEL1N = +RI*DEL2*EXPP/PI-RI*DYN2*DT/PI- &
                  RI*DYN2*EXPP/(PI*PI)+DYN1*DT+ &
                  DEL1-RI*DEL2/PI+RI*DYN2/(PI*PI)
        DEL1INT = -RI*DEL2*EXPP/(PI*PI)-RI*DYN2*DT*DT/(2.0D0*PI)+ &
                    RI*DYN2*EXPP/(PI*PI*PI)+DYN1*DT*DT/2.0D0+ &
                    DEL1*DT-RI*DEL2*DT/PI+RI*DYN2*DT/(PI*PI)+ &
                    RI*DEL2/(PI*PI)-RI*DYN2/(PI*PI*PI)
        GOTO 100
! in case DABS(PI*DT)>1.0D-6
      ELSE

! in case DABS(PI*DT)<=1.0D-6

          EXPP=EXPM1(-PI*DT)
          DEL2N=DEL2+DEL2*EXPP-EXPP*DYN2/PI
          DEL2INT=-DEL2*EXPP/PI+DYN2*DT/PI+DYN2*EXPP/(PI*PI)
          DEL1N=+RI*DEL2*EXPP/PI-RI*DYN2*DT/PI- &
                    RI*DYN2*EXPP/(PI*PI)+DYN1*DT+DEL1
          DEL1INT=-RI*DEL2*EXPP/(PI*PI)-RI*DYN2*DT*DT/(2.0D0*PI)+ &
                      RI*DYN2*EXPP/(PI*PI*PI)+DYN1*DT*DT/2.0D0+ &
                      DEL1*DT-RI*DEL2*DT/PI+RI*DYN2*DT/(PI*PI)
          GOTO 100

      ENDIF
! ... only ice                                                      (end)

! in case RW==0.0D0
    ENDIF

    IF(IRW == 1 .AND. IRI == 1) THEN

      A=(RW-PI)*(RW-PI)+4.0E0*PW*RI

	     IF(A < 0.0) THEN
	          PRINT*,   'IN SUBROUTINE JERSUPSAT: A < 0'
            PRINT*,   'DETER'
            PRINT 201, DETER
            PRINT*,   'RW,PW,RI,PI'
            PRINT 204, RW,PW,RI,PI
            PRINT*,   'DT,DYN1,DYN2'
            PRINT 203, DT,DYN1,DYN2
            PRINT*,   'DEL1,DEL2'
            PRINT 202, DEL1,DEL2
	          PRINT*,   'STOP 1905:A < 0'
	          call wrf_error_fatal("fatal error: STOP 1905:A < 0, model stop")
       ENDIF
! ... water and ice                                               (start)
       ALFA=DSQRT((RW-PI)*(RW-PI)+4.0D0*PW*RI)

! 5/8/04 Nir, Beta is negative to the simple solution so it will decay

        BETA=0.5D0*(ALFA+RW+PI)
        GAMA=0.5D0*(ALFA-RW-PI)
        G31=PI*DYN1-RI*DYN2
        G32=-PW*DYN1+RW*DYN2
        G2=RW*PI-RI*PW
        IF (G2 < 1.0d-20) G2 = 1.0004d-11*1.0003d-11-1.0002d-11*1.0001e-11 ! ... (KS) - 24th,May,2016
        EXPB=DEXP(-BETA*DT)
        EXPG=DEXP(GAMA*DT)

        IF(DABS(GAMA*DT)>1.0E-6) THEN
          C11=(BETA*DEL1-RW*DEL1-RI*DEL2-BETA*G31/G2+DYN1)/ALFA
          C21=(GAMA*DEL1+RW*DEL1+RI*DEL2-GAMA*G31/G2-DYN1)/ALFA
          C12=(BETA*DEL2-PW*DEL1-PI*DEL2-BETA*G32/G2+DYN2)/ALFA
          C22=(GAMA*DEL2+PW*DEL1+PI*DEL2-GAMA*G32/G2-DYN2)/ALFA
          DEL1N=C11*EXPG+C21*EXPB+G31/G2
          DEL1INT=C11*EXPG/GAMA-C21*EXPB/BETA+(C21/BETA-C11/GAMA) &
                  +G31*DT/G2
          DEL2N=C12*EXPG+C22*EXPB+G32/G2
          DEL2INT=C12*EXPG/GAMA-C22*EXPB/BETA+(C22/BETA-C12/GAMA) &
                  +G32*DT/G2
            GOTO 100
! in case DABS(GAMA*DT)>1.0D-6
	       ELSE
! in case DABS(GAMA*DT)<=1.0D-6
            IF(ABS(RI/RW)>1.0E-12) THEN
              IF(ABS(RW/RI)>1.0E-12) THEN
                ALFA=DSQRT((RW-PI)*(RW-PI)+4.0D0*PW*RI)
                BETA=0.5D0*(ALFA+RW+PI)
                GAMA=0.5D0*(ALFA-RW-PI)
		            IF (GAMA < 0.5*2.0d-10) GAMA=0.5D0*(2.002d-10-2.001d-10) ! ... (KS) - 24th,May,2016
                EXPG=EXPM1(GAMA*DT)
                EXPB=DEXP(-BETA*DT)

! beta/alfa could be very close to 1 that why I transform it
! remember alfa-beta=gama

                C11=(BETA*DEL1-RW*DEL1-RI*DEL2+DYN1)/ALFA
                C21=(GAMA*DEL1+RW*DEL1+RI*DEL2-GAMA*G31/G2-DYN1)/ALFA
                C12=(BETA*DEL2-PW*DEL1-PI*DEL2+DYN2)/ALFA
                C22=(GAMA*DEL2+PW*DEL1+PI*DEL2-GAMA*G32/G2-DYN2)/ALFA

                A1DEL1N=C11
                A2DEL1N=C11*EXPG
                A3DEL1N=C21*EXPB
                A4DEL1N=G31/G2*(GAMA/ALFA+(GAMA/ALFA-1.0D0)*EXPG)

                DEL1N=A1DEL1N+A2DEL1N+A3DEL1N+A4DEL1N

                A1DEL1INT=C11*EXPG/GAMA
                A2DEL1INT=-C21*EXPB/BETA
                A3DEL1INT=C21/BETA
                A4DEL1INT=G31/G2*DT*(GAMA/ALFA)

                DEL1INT=A1DEL1INT+A2DEL1INT+A3DEL1INT+A4DEL1INT

                A1DEL2N=C12
                A2DEL2N=C12*EXPG
                A3DEL2N=C22*EXPB
                A4DEL2N=G32/G2*(GAMA/ALFA+ &
                       (GAMA/ALFA-1.0D0)* &
                       (GAMA*DT+GAMA*GAMA*DT*DT/2.0D0))

                DEL2N=A1DEL2N+A2DEL2N+A3DEL2N+A4DEL2N

                A1DEL2INT=C12*EXPG/GAMA
                A2DEL2INT=-C22*EXPB/BETA
                A3DEL2INT=C22/BETA
                A4DEL2INT=G32/G2*DT*(GAMA/ALFA)
                A5DEL2INT=G32/G2*(GAMA/ALFA-1.0D0)* &
                                 (GAMA*DT*DT/2.0D0)

                DEL2INT=A1DEL2INT+A2DEL2INT+A3DEL2INT+A4DEL2INT+ &
                        A5DEL2INT

! in case DABS(RW/RI)>1D-12
              ELSE

! in case DABS(RW/RI)<=1D-12

                X=-2.0D0*RW*PI+RW*RW+4.0D0*PW*RI

                ALFA=PI*(1+(X/PI)/2.0D0-(X/PI)*(X/PI)/8.0D0)
                BETA=PI+(X/PI)/4.0D0-(X/PI)*(X/PI)/16.0D0+RW/2.0D0
                GAMA=(X/PI)/4.0D0-(X/PI)*(X/PI)/16.0D0-RW/2.0D0

                EXPG=EXPM1(GAMA*DT)
                EXPB=DEXP(-BETA*DT)

    	        	C11=(BETA*DEL1-RW*DEL1-RI*DEL2+DYN1)/ALFA
    	        	C21=(GAMA*DEL1+RW*DEL1+RI*DEL2-GAMA*G31/G2-DYN1)/ALFA
    	        	C12=(BETA*DEL2-PW*DEL1-PI*DEL2+DYN2)/ALFA
    	        	C22=(GAMA*DEL2+PW*DEL1+PI*DEL2-GAMA*G32/G2-DYN2)/ALFA

                DEL1N=C11+C11*EXPG+C21*EXPB+ &
                         G31/G2*(GAMA/ALFA+(GAMA/ALFA-1)*EXPG)
                DEL1INT=C11*EXPG/GAMA-C21*EXPB/BETA+(C21/BETA)+ &
                           G31/G2*DT*(GAMA/ALFA)
                DEL2N=C12+C12*EXPG+C22*EXPB+G32/G2*(GAMA/ALFA+ &
                        (GAMA/ALFA-1.0D0)* &
                        (GAMA*DT+GAMA*GAMA*DT*DT/2.0D0))
	              DEL2INT=C12*EXPG/GAMA-C22*EXPB/BETA+ &
      		       (C22/BETA)+G32/G2*DT*(GAMA/ALFA)+ &
      		        G32/G2*(GAMA/ALFA-1.0D0)*(GAMA*DT*DT/2.0D0)

! in case DABS(RW/RI)<=1D-12
	         ENDIF
! alfa/beta 2
! in case DABS(RI/RW)>1D-12

            ELSE

! in case DABS(RI/RW)<=1D-12

              X=-2.0D0*RW*PI+PI*PI+4.0D0*PW*RI

              ALFA=RW*(1.0D0+(X/RW)/2.0D0-(X/RW)*(X/RW)/8.0D0)
              BETA=RW+(X/RW)/4.0D0-(X/RW)*(X/RW)/16.0D0+PI/2.0D0
              GAMA=(X/RW)/4.0D0-(X/RW)*(X/RW)/16.0D0-PI/2.0D0

              EXPG=EXPM1(GAMA*DT)
              EXPB=DEXP(-BETA*DT)

              C11=(BETA*DEL1-RW*DEL1-RI*DEL2+DYN1)/ALFA
      	      C21=(GAMA*DEL1+RW*DEL1+RI*DEL2-GAMA*G31/G2-DYN1)/ALFA
              C12=(BETA*DEL2-PW*DEL1-PI*DEL2+DYN2)/ALFA
  	          C22=(GAMA*DEL2+PW*DEL1+PI*DEL2-GAMA*G32/G2-DYN2)/ALFA

              DEL1N=C11+C11*EXPG+C21*EXPB+ &
                    G31/G2*(GAMA/ALFA+(GAMA/ALFA-1.0D0)*EXPG)
              DEL1INT=C11*EXPG/GAMA-C21*EXPB/BETA+(C21/BETA)+ &
                      G31/G2*DT*(GAMA/ALFA)
              DEL2N=C12+C12*EXPG+C22*EXPB+G32/G2* &
                    (GAMA/ALFA+ &
                    (GAMA/ALFA-1.0D0)*(GAMA*DT+GAMA*GAMA*DT*DT/2.0D0))
	      	   DEL2INT=C12*EXPG/GAMA-C22*EXPB/BETA+C22/BETA+ &
      		      G32/G2*DT*(GAMA/ALFA)+ &
      		      G32/G2*(GAMA/ALFA-1.0D0)*(GAMA*DT*DT/2.0D0)
! alfa/beta
! in case DABS(RI/RW)<=1D-12
	    ENDIF
! in case DABS(GAMA*DT)<=1D-6
	  ENDIF

! water and ice                                                 (end)

! in case ISYM1/=0.AND.ISYM2/=0

        ENDIF

 100    CONTINUE

  201	FORMAT(1X,D13.5)
  202	FORMAT(1X,2D13.5)
  203	FORMAT(1X,3D13.5)
  204	FORMAT(1X,4D13.5)

        RETURN
        END SUBROUTINE JERSUPSAT_KS

! SUBROUTINE JERSUPSAT
! ....................................................................
	SUBROUTINE JERDFUN_KS (xi,xiN,B21_MY, &
        					       FI2,PSI2,fl2,DEL2N, &
        					       ISYM2,IND,ITYPE,TPN,IDROP, &
        					       FR_LIM,FRH_LIM,ICEMAX,NKR,COL,Ihydro,Iin,Jin,Kin,Itimestep)

	IMPLICIT NONE
! ... Interface
	INTEGER,INTENT(IN) :: ISYM2, IND, ITYPE, NKR, ICEMAX, Ihydro, Iin, Jin ,Kin, Itimestep
	INTEGER,INTENT(INOUT) :: IDROP
	REAL(kind=R4SIZE),INTENT(IN) :: B21_MY(:), FI2(:), FR_LIM(:), FRH_LIM(:), &
					   				DEL2N, COL
	REAL(kind=R8SIZE),INTENT(IN) :: TPN, xi(:)
	REAL(kind=R8SIZE),INTENT(INOUT) :: xiN(:)
	REAL(kind=R4SIZE),INTENT(INOUT) :: PSI2(:), FL2(:)
! ... Interface

! ... Locals
	INTEGER :: ITYP, KR, NR, ICE, K, IDSD_Negative
	REAL(kind=R8SIZE) :: FL2_NEW(NKR), FI2R(NKR), PSI2R(NKR), C, DEGREE1, DEGREE2, DEGREE3, D, RATEXI, &
						           B, A, xiR(NKR),xiNR(NKR), FR_LIM_KR
! ... Locals


	C = 2.0D0/3.0D0

	DEGREE1 = 1.0D0/3.0D0
	DEGREE2 = C
	DEGREE3 = 3.0D0/2.0D0

	IF(IND > 1) THEN
	  ITYP = ITYPE
	ELSE
	  ITYP = 1
	ENDIF

	DO KR=1,NKR
	   PSI2R(KR) = FI2(KR)
	   FI2R(KR) = FI2(KR)
	ENDDO

	NR=NKR

! new size distribution functions                             (start)

	IF(ISYM2 == 1) THEN
	  IF(IND==1 .AND. ITYPE==1) THEN
! drop diffusional growth
	    DO KR=1,NKR
	       D=xi(KR)**DEGREE1
	       RATExi=C*DEL2N*B21_MY(KR)/D
	       B=xi(KR)**DEGREE2
	       A=B+RATExi
	       IF(A<0.0D0) THEN
	         xiN(KR)=1.0D-50
	       ELSE
	         xiN(KR)=A**DEGREE3
	       ENDIF
	    ENDDO
! in case IND==1.AND.ITYPE==1
	  ELSE
! in case IND/=1.OR.ITYPE/=1
	         DO KR=1,NKR
	            RATExi = DEL2N*B21_MY(KR)
	            xiN(KR) = xi(KR) + RATExi
	         ENDDO
	  ENDIF

! recalculation of size distribution functions                (start)

      DO KR=1,NKR
        xiR(KR) = xi(KR)
        xiNR(KR) = xiN(KR)
      	FI2R(KR) = FI2(KR)
      END DO

   		IDSD_Negative = 0
			CALL JERNEWF_KS &
				    (NR,xiR,FI2R,PSI2R,xiNR,ISIGN_3POINT,TPN,IDROP,NKR,COL,IDSD_Negative,Ihydro,Iin,Jin,Kin,Itimestep)
			IF(IDSD_Negative == 1)THEN
				IF(ISIGN_KO_1 == 1) THEN
					! ... (KS) - we do not use Kovatch-Ouland as separate method
					!	CALL JERNEWF_KO_KS &
    			!					(NR,xiR,FI2R,PSI2R,xiNR,NKR,COL)
				ENDIF
			ENDIF

   		DO KR=1,NKR
          IF(ITYPE==5) THEN
		      			FR_LIM_KR=FRH_LIM(KR)
	    		ELSE
		      			FR_LIM_KR=FR_LIM(KR)
					ENDIF
		  		IF(PSI2R(KR)<0.0D0) THEN
					  PRINT*,    'STOP 1506 : PSI2R(KR)<0.0D0, in JERDFUN_KS'
						call wrf_error_fatal("fatal error in PSI2R(KR)<0.0D0, in JERDFUN_KS, model stop")
			  	ENDIF
    	  	PSI2(KR) = PSI2R(KR)
   	 ENDDO
! cycle by ICE
! recalculation of size distribution functions                  (end)
! in case ISYM2/=0
	ENDIF
! new size distribution functions                               (end)

  201	FORMAT(1X,D13.5)
  304   FORMAT(1X,I2,2X,4D13.5)

	RETURN
	END SUBROUTINE JERDFUN_KS
! +----------------------------------------------------------------------------+
		SUBROUTINE JERNEWF_KS &
       				(NRX,RR,FI,PSI,RN,I3POINT,TPN,IDROP,NKR,COL,IDSD_Negative,Ihydro, &
              Iin,Jin,Kin,Itimestep)

        IMPLICIT NONE
! ... Interface
		INTEGER,INTENT(IN) :: NRX, I3POINT, NKR, Ihydro, Iin, Jin, Kin, Itimestep
		INTEGER,INTENT(INOUT) :: IDROP, IDSD_Negative
		real(kind=R8SIZE),INTENT(IN) :: TPN
		real(kind=R4SIZE),INTENT(IN) :: COL
		real(kind=R8SIZE),INTENT(INOUT) :: PSI(:), RN(:), FI(:), RR(:)
! ... Interface

! ... Locals
		INTEGER :: KMAX, KR, I, K , NRXP, ISIGN_DIFFUSIONAL_GROWTH, NRX1,  &
              I3POINT_CONDEVAP, IEvap
		real(kind=R8SIZE) :: RNTMP,RRTMP,RRP,RRM,RNTMP2,RRTMP2,RRP2,RRM2, GN1,GN2, &
               GN3,GN1P,GMAT,GMAT2, &
							 CDROP(NRX),DELTA_CDROP(NRX),RRS(NRX+1),PSINEW(NRX+1), &
							 PSI_IM,PSI_I,PSI_IP, AOLDCON, ANEWCON, AOLDMASS, ANEWMASS

		INTEGER,PARAMETER :: KRDROP_REMAPING_MIN = 6, KRDROP_REMAPING_MAX = 12
! ... Locals

! >> [KS] 22ndMay19	IF(TPN .LT. 273.15-5.0D0) IDROP=0

! INITIAL VALUES FOR SOME VARIABLES

		NRXP = NRX + 1
!   NRX1 = 24
!   NRX1 = 35
	  NRX1 = NKR

	  DO I=1,NRX
! RN(I), g - new masses after condensation or evaporation
	    IF(RN(I) < 0.0D0) THEN
		     RN(I) = 1.0D-50
	       FI(I) = 0.0D0
	    ENDIF
	 ENDDO

! new change 26.10.09                                         (start)
	DO K=1,NRX
	   RRS(K)=RR(K)
	ENDDO
! new change 26.10.09                                           (end)

	I3POINT_CONDEVAP = I3POINT

	IEvap = 0
	IF(RN(1) < RRS(1)) THEN
! evaporation
	  I3POINT_CONDEVAP = 0
! new change 26.10.09                                         (start)
	  IDROP = 0
! new change 26.10.09                                           (end)
	  NRX1 = NRX
	  IEvap = 1
	ENDIF

	IF(IDROP == 0) I3POINT_CONDEVAP = 0

! new change 26.10.09                                         (start)

	DO K=1,NRX
	   PSI(K)=0.0D0
	   CDROP(K)=0.0D0
	   DELTA_CDROP(K)=0.0D0
	   PSINEW(K)=0.0D0
	ENDDO

	RRS(NRXP)=RRS(NRX)*1024.0D0

	PSINEW(NRXP) = 0.0D0

! new change 26.10.09                                           (end)

	ISIGN_DIFFUSIONAL_GROWTH = 0

	DO K=1,NRX
	   IF(RN(K).NE.RR(K)) THEN
		  ISIGN_DIFFUSIONAL_GROWTH = 1
		  GOTO 2000
	   ENDIF
	ENDDO

 2000   CONTINUE

	IF(ISIGN_DIFFUSIONAL_GROWTH == 1) THEN

! Kovetz-Olund method                                         (start)

! new change 26.10.09                                         (start)
	  DO K=1,NRX1 ! ... [KS] >> NRX1-1
! new change 26.10.09                                           (end)

		 IF(FI(K) > 0.0) THEN
		   IF(DABS(RN(K)-RR(K)) < 1.0D-16) THEN
	         PSINEW(K) = FI(K)*RR(K)
	         CYCLE
       ENDIF

		   I = 1
		   DO WHILE (.NOT.(RRS(I) <= RN(K) .AND. RRS(I+1) >= RN(K)) &
                 .AND.I.LT.NRX1) ! [KS] >> was NRX1-1
                  I = I + 1
       ENDDO

       IF(RN(K).LT.RRS(1)) THEN
          RNTMP=RN(K)
          RRTMP=0.0D0
          RRP=RRS(1)
          GMAT2=(RNTMP-RRTMP)/(RRP-RRTMP)
          PSINEW(1)=PSINEW(1)+FI(K)*RR(K)*GMAT2
		   ELSE

        RNTMP=RN(K)
        RRTMP=RRS(I)
        RRP=RRS(I+1)
        GMAT2=(RNTMP-RRTMP)/(RRP-RRTMP)
        GMAT=(RRP-RNTMP)/(RRP-RRTMP)
        PSINEW(I)=PSINEW(I)+FI(K)*RR(K)*GMAT
        PSINEW(I+1)=PSINEW(I+1)+FI(K)*RR(K)*GMAT2
		   ENDIF
! in case FI(K).NE.0.0D0
		 ENDIF

 3000    CONTINUE

	  ENDDO
! cycle by K

	  DO KR=1,NRX1
       PSI(KR)=PSINEW(KR)
	  ENDDO

	  DO KR=NRX1+1,NRX
       PSI(KR)=FI(KR)
	  ENDDO
! Kovetz-Olund method                                           (end)

! calculation both new total drop concentrations(after KO) and new
! total drop masses (after KO)

! 3point method	                                              (start)
	  IF(I3POINT_CONDEVAP == 1) THEN
	    DO K=1,NRX1-1
		   IF(FI(K) > 0.0) THEN
		      IF(DABS(RN(K)-RR(K)).LT.1.0D-16) THEN
	           PSI(K) = FI(K)*RR(K)
	           GOTO 3001
	         ENDIF

          IF(RRS(2).LT.RN(K)) THEN
             I = 2
             DO WHILE &
                     (.NOT.(RRS(I) <= RN(K) .AND. RRS(I+1) >= RN(K)) &
                     .AND.I.LT.NRX1-1)
                    I = I + 1
         		ENDDO
             RNTMP=RN(K)

             RRTMP=RRS(I)
             RRP=RRS(I+1)
             RRM=RRS(I-1)

             RNTMP2=RN(K+1)

             RRTMP2=RRS(I+1)
             RRP2=RRS(I+2)
             RRM2=RRS(I)

             GN1=(RRP-RNTMP)*(RRTMP-RNTMP)/(RRP-RRM)/ &
                  (RRTMP-RRM)

             GN1P=(RRP2-RNTMP2)*(RRTMP2-RNTMP2)/ &
                   (RRP2-RRM2)/(RRTMP2-RRM2)

             GN2=(RRP-RNTMP)*(RNTMP-RRM)/(RRP-RRTMP)/ &
                   (RRTMP-RRM)

 	           GMAT=(RRP-RNTMP)/(RRP-RRTMP)

             GN3=(RRTMP-RNTMP)*(RRM-RNTMP)/(RRP-RRM)/ &
                                           (RRP-RRTMP)
             GMAT2=(RNTMP-RRTMP)/(RRP-RRTMP)

             PSI_IM = PSI(I-1)+GN1*FI(K)*RR(K)

             PSI_I = PSI(I)+GN1P*FI(K+1)*RR(K+1)+&
                   (GN2-GMAT)*FI(K)*RR(K)

             PSI_IP = PSI(I+1)+(GN3-GMAT2)*FI(K)*RR(K)

             IF(PSI_IM > 0.0D0) THEN

               IF(PSI_IP > 0.0D0) THEN

                 IF(I > 2) THEN
! smoothing criteria
                   IF(PSI_IM > PSI(I-2) .AND. PSI_IM < PSI_I &
                     .AND. PSI(I-2) < PSI(I) .OR. PSI(I-2) >= PSI(I)) THEN

                      PSI(I-1) = PSI_IM

                      PSI(I) = PSI(I) + FI(K)*RR(K)*(GN2-GMAT)

                      PSI(I+1) = PSI_IP
! in case smoothing criteria
                   ENDIF
! in case I.GT.2
                 ENDIF

! in case PSI_IP.GT.0.0D0
					   ELSE
						         EXIT
   	         ENDIF
! in case PSI_IM.GT.0.0D0
				  ELSE
				        EXIT
          ENDIF
! in case I.LT.NRX1-2
!         ENDIF

! in case RRS(2).LT.RN(K)
       ENDIF

! in case FI(K).NE.0.0D0
      ENDIF

 3001 CONTINUE

	    ENDDO
        ! cycle by K

      ! in case I3POINT_CONDEVAP.NE.0
	  ENDIF
! 3 point method                                                (end)

! PSI(K) - new hydrometeor size distribution function

	  DO K=1,NRX1
	     PSI(K)=PSI(K)/RR(K)
	  ENDDO

	  DO K=NRX1+1,NRX
		 PSI(K)=FI(K)
	  ENDDO

	  IF(IDROP == 1) THEN
				DO K=KRDROP_REMAPING_MIN,KRDROP_REMAPING_MAX
					CDROP(K)=3.0D0*COL*PSI(K)*RR(K)
				ENDDO
	       ! KMAX - right boundary spectrum of drop sdf
	        !(KRDROP_REMAP_MIN =< KMAX =< KRDROP_REMAP_MAX)
				DO K=KRDROP_REMAPING_MAX,KRDROP_REMAPING_MIN,-1
				   KMAX=K
				   IF(PSI(K).GT.0.0D0) GOTO 2011
				ENDDO

	 2011  CONTINUE
	! Andrei's new change 28.04.10                                (start)
				DO K=KMAX-1,KRDROP_REMAPING_MIN,-1
	! Andrei's new change 28.04.10                                  (end)
					IF(CDROP(K).GT.0.0D0) THEN
						DELTA_CDROP(K)=CDROP(K+1)/CDROP(K)
							IF(DELTA_CDROP(K).LT.COEFF_REMAPING) THEN
								CDROP(K)=CDROP(K)+CDROP(K+1)
								CDROP(K+1)=0.0D0
							ENDIF
					ENDIF
				ENDDO

				DO K=KRDROP_REMAPING_MIN,KMAX
					PSI(K)=CDROP(K)/(3.0D0*COL*RR(K))
				ENDDO

	! in case IDROP.NE.0
		  ENDIF

! new change 26.10.09                                           (end)

! in case ISIGN_DIFFUSIONAL_GROWTH.NE.0
        ELSE
! in case ISIGN_DIFFUSIONAL_GROWTH.EQ.0
  	  		DO K=1,NRX
  	     		PSI(K)=FI(K)
  	  		ENDDO
       ENDIF

		DO KR=1,NRX
	   		IF(PSI(KR) < 0.0) THEN ! ... (KS)
					IDSD_Negative = 1
					print*, "IDSD_Negative=",IDSD_Negative,"kr",kr
					PRINT*,    'IN SUBROUTINE JERNEWF'
					PRINT*,		'PSI(KR)<0'
					PRINT*,    'BEFORE EXIT'
					PRINT*,    'ISIGN_DIFFUSIONAL_GROWTH'
					PRINT*,     ISIGN_DIFFUSIONAL_GROWTH
					PRINT*,    'I3POINT_CONDEVAP'
					PRINT*,     I3POINT_CONDEVAP
					PRINT*,    'K,RR(K),RN(K),K=1,NRX'
					PRINT*,    (K,RR(K),RN(K),K=1,NRX)
					PRINT*,    'K,RR(K),RN(K),FI(K),PSI(K),K=1,NRX'
					PRINT 304, (K,RR(K),RN(K),FI(K),PSI(K),K=1,NRX)
					PRINT*,		IDROP,Ihydro,Iin,Jin,Kin,Itimestep
          call wrf_error_fatal("fatal error in SUBROUTINE JERNEWF PSI(KR)<0, < min, model stop")
			ENDIF
		ENDDO

  304   FORMAT(1X,I2,2X,4D13.5)

        RETURN
        END SUBROUTINE JERNEWF_KS
! +------------------------------------------------------------------+
	SUBROUTINE JERDFUN_NEW_KS &
       				(xi,xiN,B21_MY, &
        			FI2,PSI2, &
        			TPN,IDROP,FR_LIM,NKR,COL,Ihydro,Iin,Jin,Kin,Itimestep)

	IMPLICIT NONE

! ... Interface
	INTEGER,INTENT(INOUT) :: IDROP, NKR
	INTEGER,INTENT(IN) :: Ihydro,Iin,Jin,Kin,Itimestep
	REAL(kind=R4SIZE),intent(IN) :: FI2(:), B21_MY(:), FR_LIM(:), COL
	REAL(kind=R8SIZE), INTENT(IN) :: TPN, xi(:)
	REAL(kind=R4SIZE),INTENT(INOUT) :: PSI2(:)
	REAL(kind=R8SIZE),INTENT(INOUT) :: xiN(:)
! ... Interface

! ... Locals
	INTEGER :: NR, KR, IDSD_Negative
	REAL(kind=R8SIZE) :: C, DEGREE1, DEGREE2, DEGREE3, D, RATEXI, B, A, &
						           xiR(NKR),FI2R(NKR),PSI2R(NKR),xiNR(NKR)
! ... Locals

	C=2.0D0/3.0D0

	DEGREE1=C/2.0D0
	DEGREE2=C
	DEGREE3=3.0D0/2.0D0

	NR=NKR

	xiR = xi
	FI2R = FI2
	PSI2R = PSI2
	xiNR = xiN

! new drop size distribution functions                             (start)

! drop diffusional growth

	DO KR=1,NKR
	   D = xiR(KR)**DEGREE1
! Andrei's new change of 3.09.10                              (start)
!	   RATExi=C*DEL2N*B21_MY(KR)/D
	   RATExi = C*B21_MY(KR)/D
! Andrei's new change of 3.09.10                                (end)
	   B = xiR(KR)**DEGREE2
	   A = B+RATExi
	   IF(A<0.0D0) THEN
	     xiNR(KR) = 1.0D-50
	   ELSE
	     xiNR(KR) = A**DEGREE3
	   ENDIF
	ENDDO

! recalculation of size distribution functions                (start)

	IDSD_Negative = 0
	CALL JERNEWF_KS &
			(NR,xiR,FI2R,PSI2R,xiNR,ISIGN_3POINT,TPN,IDROP,NKR,COL,IDSD_Negative,Ihydro,Iin,Jin,Kin,Itimestep)
	IF(IDSD_Negative == 1)THEN
		IF(ISIGN_KO_2 == 1) THEN
			! ... (KS) - we do not use Kovatch-Ouland as separate method
	  	!	CALL JERNEWF_KO_KS &
      !  				(NR,xiR,FI2R,PSI2R,xiNR,NKR,COL)
		ENDIF
	ENDIF

	PSI2 = PSI2R

! recalculation of drop size distribution functions                  (end)
! new drop size distribution functions                          (end)

  201	FORMAT(1X,D13.5)

	RETURN
	END SUBROUTINE JERDFUN_NEW_KS
! +---------------------------------------------------------+
	SUBROUTINE Relaxation_Time(TPS,QPS,PP,ROR,DEL1S,DEL2S, &
            							   R1,VR1,FF1in,RLEC,RO1BL, &
            							   R2,VR2,FF2in,RIEC,RO2BL, &
            							   R3,VR3,FF3in,RSEC,RO3BL, &
            							   R4,VR4,FF4in,RGEC,RO4BL, &
            							   R5,VR5,FF5in,RHEC,RO5BL, &
            							   NKR,ICEMAX,COL,DTdyn,NCOND,DTCOND)

	implicit none
! ... Interface
	integer,intent(in) :: NKR,ICEMAX
	integer,intent(out) :: NCOND
	real(kind=R4SIZE),intent(in) :: R1(:),FF1in(:),RLEC(:),RO1BL(:), &
					   R2(:,:),FF2in(:,:),RIEC(:,:),RO2BL(:,:), &
					   R3(NKR),FF3in(:),RSEC(:),RO3BL(:), &
					   R4(NKR),FF4in(:),RGEC(:),RO4BL(:), &
					   R5(NKR),FF5in(:),RHEC(:),RO5BL(:), &
					   ROR,COL,DTdyn,VR1(:),VR2(:,:),VR3(:),VR4(:),VR5(:)
  real(kind=R8SIZE),intent(in) :: TPS,QPS,PP,DEL1S,DEL2S
  real(kind=R4SIZE),intent(out) :: DTCOND
! ... Interface
! ... Local
	integer :: ISYM1, ISYM2(ICEMAX), ISYM3, ISYM4, ISYM5, ISYM_SUM, ICM
  real(kind=R8SIZE),parameter :: AA1_MY = 2.53D12, BB1_MY = 5.42D3, AA2_MY = 3.41D13, &
                                 BB2_MY = 6.13E3, AL1 = 2500.0, AL2 = 2834.0
	real(kind=R8SIZE),parameter :: TAU_Min = 0.1 ! [s]
	real(kind=R8SIZE) :: OPER2, AR1, TAU_RELAX, B5L, B5I, &
						           R1D(NKR), R2D(NKR,ICEMAX), R3D(NKR), R4D(NKR), R5D(NKR), &
                       VR1_d(nkr),VR2_d(nkr,icemax),VR3_d(nkr),VR4_d(nkr),VR5_d(nkr)
	real(kind=R4SIZE) :: B11_MY(NKR), B21_MY(NKR,ICEMAX), B31_MY(NKR), &
			                 B41_MY(NKR), B51_MY(NKR), FL1(NKR), FL3(NKR), FL4(NKR), FL5(NKR), &
                       SFNDUMMY(3), SFN11, SFNI1(ICEMAX), SFNII1, SFN21, SFN31, SFN41, SFN51, SFNI, SFNL, B8L, B8I, RI, PW, &
	                     DOPL, DOPI, TAU_w, TAU_i, phi, RW, PI
! ... Local

		OPER2(AR1)=0.622/(0.622+0.378*AR1)/AR1
    VR1_d = VR1
    VR2_d = VR2
    VR3_d = VR3
    VR4_d = VR4
    VR5_d = VR5


		ISYM1 = 0
		ISYM2 = 0
		ISYM3 = 0
		ISYM4 = 0
		ISYM5 = 0
		IF(sum(FF1in) > 0.0) ISYM1 = 1
		IF(sum(FF2in(:,1)) > 1.0D-10) ISYM2(1) = 1
		IF(sum(FF2in(:,2)) > 1.0D-10) ISYM2(2) = 1
		IF(sum(FF2in(:,3)) > 1.0D-10) ISYM2(3) = 1
		IF(sum(FF3in) > 1.0D-10) ISYM3 = 1
		IF(sum(FF4in) > 1.0D-10) ISYM4 = 1
		IF(sum(FF5in) > 1.0D-10) ISYM5 = 1

		ISYM_SUM = ISYM1 + sum(ISYM2) + ISYM3 + ISYM4  + ISYM5
		IF(ISYM_SUM == 0)THEN
			TAU_RELAX = DTdyn
			NCOND = nint(DTdyn/TAU_RELAX)
		    DTCOND = TAU_RELAX
        RETURN
		ENDIF

		R1D = R1
		R2D = R2
		R3D = R3
		R4D = R4
		R5D = R5
		B8L=1./ROR
   	B8I=1./ROR
		ICM = ICEMAX
		SFN11 = 0.0
		SFNI1 = 0.0
		SFN31 = 0.0
		SFN41 = 0.0
		SFN51 = 0.0
		B11_MY = 0.0
		B21_MY = 0.0
		B31_MY = 0.0
		B41_MY = 0.0
		B51_MY = 0.0


		  ! ... Drops
		  IF(ISYM1 == 1)THEN
  			FL1 = 0.0
  			CALL JERRATE_KS(R1D,TPS,PP,VR1_d,RLEC,RO1BL,B11_MY,1,1,fl1,NKR,ICEMAX)
        sfndummy(1) = SFN11
        CALL JERTIMESC_KS(FF1in,R1D,SFNDUMMY,B11_MY,B8I,1,NKR,ICEMAX,COL)
  			SFN11 = sfndummy(1)
		  ENDIF
		  ! ... IC
		  !IF(sum(ISYM2) > 0) THEN
	  	!	FL1 = 0.0
	  	!	! ... ice crystals
		  !	CALL JERRATE_KS (R2D,TPS,PP,VR2_d,RIEC,RO2BL,B21_MY,3,2,fl1,NKR,ICEMAX)
		  !	CALL JERTIMESC_KS (FF2in,R2D,SFNI1,B21_MY,B8I,ICM,NKR,ICEMAX,COL)
		  !ENDIF
      ! ... Snow
      IF(ISYM3 == 1) THEN
	  		FL3 = 0.0
	  		! ... snow
		  	CALL JERRATE_KS (R3D,TPS,PP,VR3_d,RSEC,RO3BL,B31_MY,1,3,fl3,NKR,ICEMAX)
		  	sfndummy(1) = SFN31
		  	CALL JERTIMESC_KS(FF3in,R3D,SFNDUMMY,B31_MY,B8I,1,NKR,ICEMAX,COL)
	  	  	SFN31 = sfndummy(1)
     	ENDIF
      ! ... Graupel
     IF(ISYM4 == 1) THEN
	  		FL4 = 0.0
	  		! ... graupel
		  	CALL JERRATE_KS(R4D,TPS,PP,VR4_d,RGEC,RO4BL,B41_MY,1,2,fl4,NKR,ICEMAX)

		  	sfndummy(1) = SFN41
		  	CALL JERTIMESC_KS(FF4in,R4D,SFNDUMMY,B41_MY,B8I,1,NKR,ICEMAX,COL)
	  	  	SFN41 = sfndummy(1)
	   ENDIF
      ! ... Hail
      IF(ISYM5 == 1) THEN
        FL5 = 0.0
        ! ... hail
        CALL JERRATE_KS(R5D,TPS,PP,VR5_d,RHEC,RO5BL,B51_MY,1,2,fl5,NKR,ICEMAX)

        sfndummy(1) = SFN51
        CALL JERTIMESC_KS(FF5in,R5D,SFNDUMMY,B51_MY,B8I,1,NKR,ICEMAX,COL)
        SFN51 = sfndummy(1)
  	  ENDIF

		  SFNII1 = 0.0
		  SFN21 = 0.0
		  SFNL = 0.0
		  SFNI = 0.0
		  RI = 0.0
		  PW = 0.0
		  SFNII1 = SFNI1(1)+SFNI1(2)+SFNI1(3)
		  SFN21 = SFNII1 + SFN31 + SFN41 + SFN51
		  SFNL = SFN11  ! Liquid
		  SFNI = SFN21 	! Total Ice

		  B5L=BB1_MY/TPS/TPS
		  B5I=BB2_MY/TPS/TPS
		  DOPL=1.+ DEL1S
		  DOPI=1.+ DEL2S
		  RW=(OPER2(QPS)+B5L*AL1)*DOPL*SFNL
		  RI=(OPER2(QPS)+B5L*AL2)*DOPL*SFNI
		  PW=(OPER2(QPS)+B5I*AL1)*DOPI*SFNL
		  PI=(OPER2(QPS)+B5I*AL2)*DOPI*SFNI

      TAU_w = DTdyn
      TAU_i = DTdyn
      phi = (1.0 + DEL2S)/(1.0 + DEL1S)
      if(PW > 0.0 .or. PI > 0.0) TAU_w = (PW + phi*PI)**(-1.0)
      if(RW > 0.0 .or. RI > 0.0) TAU_i =  phi/(RW + RI*phi)
      TAU_RELAX = DTdyn
		  IF(PW > 0.0 .or. RI > 0.0) TAU_RELAX = (PW + RI)**(-1.0)/3.0
		  IF(PW > 0.0 .and. RI > 0.0) TAU_RELAX = min(TAU_w,TAU_i)/3.0

      if(TAU_RELAX > DTdyn) TAU_RELAX = DTdyn/3.0
		  if(TAU_RELAX < TAU_Min) TAU_RELAX = TAU_Min
      IF(PW <= 0.0 .and. RI <= 0.0) TAU_RELAX = DTdyn

		  !if(TAU_RELAX < DTdyn .and. IDebug_Print_DebugModule==1)then
		  !		print*,"in Relaxation_Time,TAU_RELAX < DTdyn"
			!  	print*,TAU_RELAX
		  !endif

		  !NCOND = nint(DTdyn/TAU_RELAX)
		  NCOND = ceiling(DTdyn/TAU_RELAX)
      DTCOND = TAU_RELAX

	RETURN
	END SUBROUTINE Relaxation_Time
! +------------------------------+
end module module_mp_SBM_Auxiliary
! +-----------------------------------------------------------------------------+
! +-----------------------------------------------------------------------------+
 module module_mp_SBM_Nucleation

 USE module_mp_SBM_Auxiliary,ONLY:POLYSVP

 private
 public JERNUCL01_KS, LogNormal_modes_Aerosol

! Kind paramater
 	INTEGER, PARAMETER, PRIVATE:: R8SIZE = 8
 	INTEGER, PARAMETER, PRIVATE:: R4SIZE = 4

	INTEGER,PARAMETER :: Use_cloud_base_nuc = 1
	real(kind=r8size),PARAMETER::T_NUCL_DROP_MIN = -80.0D0
	real(kind=r8size),PARAMETER::T_NUCL_ICE_MIN = -37.0D0
! Ice nucleation method
! using MEYERS method : ice_nucl_method == 0
! using DE_MOTT method : ice_nucl_method == 1
	INTEGER,PARAMETER :: ice_nucl_method = 0
	INTEGER,PARAMETER :: ISIGN_TQ_ICENUCL = 1
! DELSUPICE_MAX=59%
	DOUBLE PRECISION,PARAMETER::DELSUPICE_MAX = 59.0D0

 contains
! +-----------------------------------------------------------------------------+
 SUBROUTINE JERNUCL01_KS(PSI1_r, PSI2_r, FCCNR_r, 			   &
						            XL_r, XI_r, TT, QQ, 			         &
		                    ROR_r, PP_r, 				               &
			                  SUP1, SUP2,      			  		       &
			                  COL_r, 							               &
			                  SUP2_OLD_r, DSUPICE_XYZ_r, 		     &
			                  RCCN_r, DROPRADII_r, NKR, NKR_aerosol, ICEMAX, ICEPROCS, &
			                  Win_r, Is_This_CloudBase, RO_SOLUTE, IONS, MWAERO, &
			                  Iin, Jin, Kin)


	implicit none

 	integer,intent(in) :: 	 Kin, Jin, Iin, NKR, NKR_aerosol, ICEMAX, ICEPROCS, Is_This_CloudBase,IONS
 	real(kind=r4size),intent(in) :: XL_r(:), XI_r(:,:), ROR_r, PP_r, COL_r, Win_r, &
							             SUP2_OLD_r, DSUPICE_XYZ_r, RCCN_r(:), DROPRADII_r(:)
  real(kind=r4size),intent(in) ::	 	   MWAERO, RO_SOLUTE
 	real(kind=r4size),intent(inout) :: 	 PSI1_r(:),PSI2_r(:,:),FCCNR_r(:)
 	real(kind=r8size),intent(inout) :: TT, QQ, SUP1,SUP2

 ! ... Locals
 	integer :: KR, ICE, K
 	real(kind=r8size) :: DROPCONCN(NKR), ARG_1, COL3, RORI, TPN, QPN, TPC, AR1, AR2, OPER3,           &
						           SUM_ICE, DEL2N, FI2(NKR,ICEMAX), TFREEZ_OLD, DTFREEZXZ, RMASSIAA_NUCL, RMASSIBB_NUCL, &
		                   FI2_K, xi_K, FI2R2, DELMASSICE_NUCL, ES1N, ES2N, EW1N
  real(kind=r8size),parameter :: AL2 = 2834.0D0
  real(kind=r8size) :: PSI1(NKR),PSI2(NKR,ICEMAX),FCCNR(NKR_aerosol),ROR,XL(NKR),XI(NKR,ICEMAX),PP,COL, &
						           SUP2_OLD,DSUPICE_XYZ,Win, RCCN(NKR_aerosol),DROPRADII(NKR)
	real(kind=r4size) :: TPNreal
 ! ... Locals

 	OPER3(AR1,AR2) = AR1*AR2/(0.622D0+0.378D0*AR1)

	! ... Adjust the Imput
	PSI1 = PSI1_r
	PSI2 = PSI2_r
	FCCNR = FCCNR_r
	XL = XL_r
	XI = XI_r
	ROR = ROR_r
	PP = PP_r
	COL = COL_r
	SUP2_OLD = SUP2_OLD_r
	DSUPICE_XYZ = DSUPICE_XYZ_r
	RCCN = RCCN_r
	DROPRADII = DROPRADII_r
  Win = Win_r

	COL3 = 3.0D0*COL
	RORI = 1.0D0/ROR

! ... Drop Nucleation (start)
	TPN = TT
	QPN = QQ

	TPC = TT - 273.15D0

	IF(SUP1>0.0D0 .AND. TPC>T_NUCL_DROP_MIN) THEN
		if(sum(FCCNR) > 0.0)then
			DROPCONCN = 0.0D0
			CALL WATER_NUCLEATION (COL, NKR_aerosol, PSI1, FCCNR, xl, TT, QQ, ROR, SUP1, DROPCONCN, &
							 	   PP, Is_This_CloudBase, Win, RO_SOLUTE, RCCN, IONS,MWAERO)
		endif
		! ... Transfer drops to Ice-Crystals via direct homogenous nucleation
		IF(TPC <= -38.0D0) THEN
		  SUM_ICE = 0.0D0
		  DO KR=1,NKR
			  PSI2(KR,2) = PSI2(KR,2) + PSI1(KR)
			  SUM_ICE = SUM_ICE + COL3*xl(KR)*xl(KR)*PSI1(KR)
			  PSI1(KR) = 0.0D0
		  END DO
		  ARG_1 = 334.0D0*SUM_ICE*RORI
		  TT = TT + ARG_1
		ENDIF
	ENDIF
! ... Drop nucleation (end)
! ... Nucleation of crystals (start)
	DEL2N = 100.0D0*SUP2
	TPC = TT-273.15D0

	IF(TPC < 0.0D0 .AND. TPC >= T_NUCL_ICE_MIN .AND. DEL2N > 0.0D0) THEN

		DO KR=1,NKR
			DO ICE=1,ICEMAX
				FI2(KR,ICE)=PSI2(KR,ICE)
			ENDDO
		ENDDO

	if(ice_nucl_method == 0) then
	  CALL ICE_NUCL (PSI2,xi,SUP2,TT,DSUPICE_XYZ,SUP2_OLD,ICEMAX,NKR,COL)
	endif

	IF(ISIGN_TQ_ICENUCL == 1) THEN
		RMASSIAA_NUCL=0.0D0
		RMASSIBB_NUCL=0.0D0

		! before ice crystal nucleation
		DO K=1,NKR
			DO ICE=1,ICEMAX
			  FI2_K=FI2(K,ICE)
			  xi_K=xi(K,ICE)
			  FI2R2=FI2_K*xi_K*xi_K
			  RMASSIBB_NUCL=RMASSIBB_NUCL+FI2R2
			ENDDO
		ENDDO

		RMASSIBB_NUCL = RMASSIBB_NUCL*COL3*RORI

		IF(RMASSIBB_NUCL < 0.0D0) RMASSIBB_NUCL = 0.0D0

		! after ice crystal nucleation
		DO K=1,NKR
			DO ICE=1,ICEMAX
			  FI2_K=PSI2(K,ICE)
			  xi_K=xi(K,ICE)
			  FI2R2=FI2_K*xi_K*xi_K
			  RMASSIAA_NUCL=RMASSIAA_NUCL+FI2R2
			ENDDO
		ENDDO

		RMASSIAA_NUCL = RMASSIAA_NUCL*COL3*RORI

		IF(RMASSIAA_NUCL < 0.0D0) RMASSIAA_NUCL=0.0D0

		DELMASSICE_NUCL = RMASSIAA_NUCL-RMASSIBB_NUCL

		QPN = QQ-DELMASSICE_NUCL
		QQ = QPN

		TPN = TT + AL2*DELMASSICE_NUCL
		TT = TPN

		TPNreal = TPN
		ES1N = POLYSVP(TPNreal,0)
		ES2N = POLYSVP(TPNreal,1)

		EW1N = OPER3(QPN,PP)

		SUP1 = EW1N/ES1N-1.0D0
		SUP2 = EW1N/ES2N-1.0D0

	  ! in case ISIGN_TQ_ICENUCL/=0
	  ENDIF

	! in case TPC<0.AND.TPC>=T_NUCL_ICE_MIN.AND.DEL2N>0.D0
	ENDIF

! ... Nucleation of crystals (end)

	! ... Output
	PSI1_r = PSI1
	PSI2_r = PSI2
	FCCNR_r = FCCNR

 RETURN
 END SUBROUTINE JERNUCL01_KS
! +-------------------------------------------------------------------------------------------------------------------------+
 SUBROUTINE WATER_NUCLEATION (COL, NKR, PSI1, FCCNR, xl, TT, QQ, ROR, SUP1,     &
                              DROPCONCN, PP, Is_This_CloudBase, Win, RO_SOLUTE, &
                              RCCN, IONS, MWAERO)

!===================================================================!
!                                                                   !
! DROP NUCLEATION SCHEME                                            !
!                                                                   !
! Authors: Khain A.P. & Pokrovsky A.G. July 2002 at Huji, Israel    !
!                                                                   !
!===================================================================!
 implicit none

! PSI1(KR), 1/g/cm3 - non conservative drop size distribution function
! FCCNR(KR), 1/cm^3 - aerosol(CCN) non conservative, size distribution function
! xl((KR), g        - drop bin masses

  integer,intent(in) :: 			Is_This_CloudBase, NKR, IONS
  real(kind=r8size),intent(in) :: 	xl(:), ROR, PP, Win, RCCN(:), COL
  real(kind=r8size),intent(inout) :: FCCNR(:), PSI1(:), DROPCONCN(:), QQ, TT, SUP1
  real(kind=r4size),intent(in) :: 	 RO_SOLUTE, MWAERO

  ! ... Locals
    integer :: 			IMAX, I, NCRITI, KR
    real(kind=r8size) :: DX,AR2,RCRITI,DEG01,RORI,CCNCONC(NKR),AKOE,BKOE, AR1, OPER3, RCCN_MINIMUM, &
						             DLN1, DLN2, RMASSL_NUCL, ES1N, EW1N
   real(kind=r8size),parameter :: AL1 = 2500.0D0
	 real(kind=r4size) :: TTreal
  ! ... Locals

	 OPER3(AR1,AR2)=AR1*AR2/(0.622D0+0.378D0*AR1)

   DROPCONCN(:) = 0.0D0

	DEG01 = 1.0D0/3.0D0
	RORI=1.0/ROR

	!RO_SOLUTE=2.16D0

	! imax - right CCN spectrum boundary
	IMAX = NKR
	DO I=IMAX,1,-1
	   IF(FCCNR(I) > 0.0D0) THEN
		 IMAX = I
		 exit
	   ENDIF
	ENDDO

	NCRITI=0
	! every iteration we will nucleate one bin, then we will check the new supersaturation
	! and new Rcriti.
    do while (IMAX>=NCRITI)
	     CCNCONC = 0.0

	! akoe & bkoe - constants in Koehler equation
		    AKOE=3.3D-05/TT
        !BKOE=2.0D0*4.3D0/(22.9D0+35.5D0)
	      BKOE = ions*4.3/mwaero
        BKOE=BKOE*(4.0D0/3.0D0)*3.141593D0*RO_SOLUTE

        if(Use_cloud_base_nuc == 1) then
	        if(Is_This_CloudBase == 1) then
	            CALL Cloud_Base_Super (FCCNR, RCCN, TT, PP, Win, NKR, RCRITI, RO_SOLUTE, IONS, MWAERO, COL)
          else
 	            ! rcriti, cm - critical radius of "dry" aerosol
	            RCRITI = (AKOE/3.0D0)*(4.0D0/BKOE/SUP1/SUP1)**DEG01
	        endif
       else ! ismax_cloud_base==0
            ! rcriti, cm - critical radius of "dry" aerosol
            RCRITI=(AKOE/3.0D0)*(4.0D0/BKOE/SUP1/SUP1)**DEG01
       endif

        IF(RCRITI >= RCCN(IMAX)) EXIT ! nothing to nucleate

        ! find the minimum bin to nucleate
        NCRITI = IMAX
        do while (RCRITI<=RCCN(NCRITI) .and. NCRITI>1)
            NCRITI=NCRITI-1
        enddo

		! rccn_minimum - minimum aerosol(ccn) radius
        RCCN_MINIMUM = RCCN(1)/10000.0D0
		! calculation of ccnconc(ii)=fccnr(ii)*col - aerosol(ccn) bin
		!                                            concentrations,
		!                                            ii=imin,...,imax
		! determination of ncriti   - number bin in which is located rcriti
		! calculation of ccnconc(ncriti)=fccnr(ncriti)*dln1/(dln1+dln2),
		! where,
		! dln1=Ln(rcriti)-Ln(rccn_minimum)
		! dln2=Ln(rccn(1)-Ln(rcriti)
		! calculation of new value of fccnr(ncriti)

        ! each iteration we nucleate the last bin
        IF (NCRITI==IMAX-1) then
            if (NCRITI>1) then
               DLN1=DLOG(RCRITI)-DLOG(RCCN(IMAX-1))
               DLN2=COL-DLN1
	           CCNCONC(IMAX)=DLN2*FCCNR(IMAX)
	           FCCNR(IMAX)=FCCNR(IMAX)*DLN1/COL
            else ! NCRITI==1
               DLN1=DLOG(RCRITI)-DLOG(RCCN_MINIMUM)
               DLN2=DLOG(RCCN(1))-DLOG(RCRITI)
	           CCNCONC(IMAX)=DLN2*FCCNR(IMAX)
	           FCCNR(IMAX)=FCCNR(IMAX)*DLN1/(DLN1+DLN2)
            endif
        else
             CCNCONC(IMAX) = COL*FCCNR(IMAX)
             FCCNR(IMAX)=0.0D0
        endif

        ! calculate the mass change due to nucleation
        RMASSL_NUCL=0.0D0
        if (IMAX <= NKR-7) then ! we pass it to drops mass grid
	   		    DROPCONCN(1) = DROPCONCN(1)+CCNCONC(IMAX)
           	RMASSL_NUCL = RMASSL_NUCL+CCNCONC(IMAX)*XL(1)*XL(1)
        else
	   		    DROPCONCN(8-(NKR-IMAX)) = DROPCONCN(8-(NKR-IMAX))+CCNCONC(IMAX)
           	RMASSL_NUCL = RMASSL_NUCL + CCNCONC(IMAX)*XL(8-(NKR-IMAX))*XL(8-(NKR-IMAX))
        endif
        RMASSL_NUCL = RMASSL_NUCL*COL*3.0*RORI

        ! prepering to check if we need to nucleate the next bin
        IMAX = IMAX-1

   ! cycle IMAX>=NCRITI
   end do

   ! ... Intergarting for including the previous nucleated drops
   IF(sum(DROPCONCN) > 0.0)THEN
	    DO KR = 1,8
	       DX = 3.0D0*COL*xl(KR)
	       PSI1(KR) = PSI1(KR)+DROPCONCN(KR)/DX
 	    ENDDO
   ENDIF

 RETURN
 END SUBROUTINE WATER_NUCLEATION
! +--------------------------------------------------------------------------+
!====================================================================!
!                                                                    !
! ICE NUCLEATION SCHEME                                              !
!                                                                    !
! Authors: Khain A.P. & Pokrovsky A.G. July 2002 at Huji, Israel     !
!                                                                    !
!====================================================================!

  SUBROUTINE ICE_NUCL (PSI2,xi,SUP2,TT,DSUPICE_XYZ,SUP2_OLD,ICEMAX,NKR,COL)

	implicit none

	integer,intent(in) :: NKR, ICEMAX
	real(kind=r8size),intent(in) :: xi(:,:), DSUPICE_XYZ, COL
	real(kind=r8size),intent(inout) :: PSI2(:,:),TT,SUP2,SUP2_OLD

	! ... Locals
	integer :: KR,ICE,ITYPE
	real(kind=r8size) :: FI2(NKR,ICEMAX), CONCI_BFNUCL(ICEMAX), CONCI_AFNUCL(ICEMAX)
	real(kind=r8size),parameter :: A1 = -0.639D0, B1 = 0.1296D0, A2 = -2.8D0, B2 = 0.262D0, &
								                 TEMP1 = -5.0D0, TEMP2 = -2.0D0, TEMP3 = -20.0D0

	! C1_MEY=0.001 1/cm^3
	real(kind=r8size),PARAMETER::C1_MEY = 1.0D-3
	real(kind=r8size),PARAMETER::C2_MEY = 0.0D0
	INTEGER,PARAMETER :: NRGI = 2
	real(kind=r8size) :: C1,C2,TPC,DEL2N,DEL2NN,HELEK1,HELEK2,FF1BN,FACT,DSUP2N,DELTACD,DELTAF, &
			                 ADDF,DELCONCI_AFNUCL,TPCC,DX
	! ... Locals

	C1=C1_MEY
	C2=C2_MEY

	! size distribution functions of crystals before ice nucleation

	DO KR=1,NKR
	   DO ICE=1,ICEMAX
		  FI2(KR,ICE)=PSI2(KR,ICE)
	   ENDDO
	ENDDO

	! calculation concentration of crystals before ice nucleation

	DO ICE=1,ICEMAX
	   CONCI_BFNUCL(ICE)=0.0D0
	   DO KR=1,NKR
		  CONCI_BFNUCL(ICE)=CONCI_BFNUCL(ICE)+ &
		  					3.0D0*COL*PSI2(KR,ICE)*xi(KR,ICE)
	   ENDDO
	ENDDO

	! type of ice with nucleation                                (start)

        TPC = TT-273.15D0
        ITYPE=0

        IF((TPC>-4.0D0).OR.(TPC<=-8.1D0.AND.TPC>-12.7D0).OR. &
        	(TPC<=-17.8D0.AND.TPC>-22.4D0)) THEN
          	ITYPE=2
        ELSE
          IF((TPC<=-4.0D0.AND.TPC>-8.1D0) &
        	 .OR.(TPC<=-22.4D0)) THEN
            	ITYPE=1
          ELSE
            	ITYPE=3
          ENDIF
        ENDIF

	! type of ice with nucleation                                  (end)

	! new crystal size distribution function                     (start)
        ICE=ITYPE
        IF (TPC < TEMP1) THEN
          DEL2N = 100.0D0*SUP2
          DEL2NN = DEL2N
          IF( DEL2N > DELSUPICE_MAX) DEL2NN = DELSUPICE_MAX
          HELEK1 = C1*DEXP(A1+B1*DEL2NN)
	      ELSE
          HELEK1 = 0.0D0
        ENDIF

        IF(TPC < TEMP2) THEN
          TPCC = TPC
          IF(TPCC < TEMP3) TPCC = TEMP3
          HELEK2 = C2*DEXP(A2-B2*TPCC)
        ELSE
          HELEK2 = 0.0D0
        ENDIF

        FF1BN = HELEK1+HELEK2
        FACT=1.0D0
        DSUP2N = (SUP2-SUP2_OLD+DSUPICE_XYZ)*100.0D0
        SUP2_OLD = SUP2 ! ### (KS) : We calculate SUP2_OLD outside of JERNUCL01

	      IF(DSUP2N > DELSUPICE_MAX) DSUP2N = DELSUPICE_MAX

        DELTACD = FF1BN*B1*DSUP2N

        IF(DELTACD>=FF1BN) DELTACD=FF1BN

        IF(DELTACD>0.0D0) THEN
          DELTAF=DELTACD*FACT
		  ! concentration of ice crystals
          if(CONCI_BFNUCL(ICE)<=helek1) then
          	DO KR=1,NRGI-1
             	DX=3.0D0*xi(KR,ICE)*COL
             	ADDF=DELTAF/DX
             	PSI2(KR,ICE)=PSI2(KR,ICE)+ADDF
          	ENDDO
          endif
        ENDIF

		! calculation of crystal concentration after ice nucleation

        DO ICE=1,ICEMAX
           CONCI_AFNUCL(ICE)=0.0D0
           DO KR=1,NKR
              CONCI_AFNUCL(ICE)=CONCI_AFNUCL(ICE)+ &
              3.0D0*COL*PSI2(KR,ICE)*xi(KR,ICE)
           END DO
           DELCONCI_AFNUCL=DABS(CONCI_AFNUCL(ICE)-CONCI_BFNUCL(ICE))
           IF(DELCONCI_AFNUCL>10.0D0) THEN
             PRINT*,    'IN SUBROUTINE ICE_NUCL, AFTER NUCLEATION'
             PRINT*,    'BECAUSE DELCONCI_AFNUCL > 10/cm^3'
             PRINT*,    'CONCI_BFNUCL(ICE),CONCI_AFNUCL(ICE)'
             PRINT 202,  CONCI_BFNUCL(ICE),CONCI_AFNUCL(ICE)
             PRINT*,    'DELTACD,DSUP2N,FF1BN,B1,DSUPICEXZ,SUP2'
             PRINT 206,  DELTACD,DSUP2N,FF1BN,B1,DSUPICE_XYZ,SUP2
             PRINT*,    'KR,   FI2(KR,ICE),   PSI2(KR,ICE), KR=1,NKR'
             PRINT 302, (KR,   FI2(KR,ICE),   PSI2(KR,ICE), KR=1,NKR)
             PRINT*, 'STOP 099 : DELCONCI_AFNUCL(ICE) > 10/cm^3'
             STOP 099
           ENDIF
        END DO

! new crystal size distribution function                       (end)


  202	FORMAT(1X,2D13.5)
  206	FORMAT(1X,6D13.5)
  302   FORMAT(1X,I2,2X,2D13.5)

        RETURN
        END SUBROUTINE ICE_NUCL

! SUBROUTINE ICE_NUCL
! +-------------------------------------------------------------------------------------------------+
	SUBROUTINE Cloud_Base_Super (FCCNR, RCCN, TT, PP, Wbase, NKR, RCRITI, RO_SOLUTE, IONS, MWAERO, &
								              COL)

	implicit none

! RCCN(NKR),  cm- aerosol's radius

! FCCNR(KR), 1/cm^3 - aerosol(CCN) non conservative, size
!                     distribution function in point with X,Z
!                     coordinates, KR=1,...,NKR
	integer,intent(in) :: 				   NKR, IONS
	real(kind=r8size),intent(in) ::  TT, PP, Wbase, RCCN(:), COL
	real(kind=r8size),intent(inout) :: 	FCCNR(:), RCRITI
	real(kind=r4size),intent(in) ::  MWAERO, RO_SOLUTE

	! ... Locals
	  integer :: NR, NN, KR
	  real(kind=r8size) :: PL(NKR), supmax(NKR), AKOE, BKOE, C3, PR, CCNCONACT, DL1, DL2, &
	  					            TPC
	! ... Locals

	CALL supmax_COEFF(AKOE,BKOE,C3,PP,TT,RO_SOLUTE,IONS,MWAERO)

! supmax calculation

! 'Analytical estimation of droplet concentration at cloud base', eq.21, 2012
! calculation of right side hand of equation for S_MAX
! while wbase>0, calculation PR

   	PR = C3*wbase**(0.75D0)

! calculation supersaturation in cloud base

   	SupMax = 999.0
	  PL = 0.0
    NN = -1
    DO NR=2,NKR
	   supmax(NR)=DSQRT((4.0D0*AKOE**3.0D0)/(27.0D0*RCCN(NR)**3.0D0*BKOE))
	! calculation CCNCONACT- the concentration of ccn that were activated
	! following nucleation
	! CCNCONACT=N from the paper
	! 'Analytical estimation of droplet concentration at cloud base', eq.19, 2012
	! CCNCONACT, 1/cm^3- concentration of activated CCN = new droplet concentration
	! CCNCONACT=FCCNR(KR)*COL
	! COL=Ln2/3

		CCNCONACT=0.0D0

	! NR represents the number of bin in which rcriti is located
	! from NR bin to NKR bin goes to droplets

		DO KR=NR,NKR
			CCNCONACT = CCNCONACT + COL*FCCNR(KR)
		ENDDO

	! calculate LHS of equation for S_MAX
	! when PL<PR ccn will activate

		PL(NR)=supmax(NR)*(DSQRT(CCNCONACT))
		IF(PL(NR).LE.PR) THEN
			NN = NR
			EXIT
		ENDIF

	 END DO ! NR

   if (nn == -1) then
    print*,"PR, Wbase [cm/s], C3",PR,wbase,C3
    print*,"PL",PL
    CALL wrf_error_fatal ( 'NN is not defined in cloud base routine, model stop' )
   endif

	! linear interpolation- finding radius criti of aerosol between
	! bin number (nn-1) to (nn)
	! 1) finding the difference between pl and pr in the left and right over the
	! final bin.

	DL1 = dabs(PL(NN-1)-PR) ! left side in the final bin
	DL2 = dabs(PL(NN)-PR)   ! right side in the final bin

	! 2) fining the left part of bin that will not activate
	!	DLN1=COL*DL1/(DL2+DL1)
	! 3)finding the right part of bin that activate
	!	DLN2=COL-DLN1
	! 4)finding radius criti of aerosol- RCRITI

	RCRITI = RCCN(NN-1)*dexp(COL*DL1/(DL1+DL2))

	! end linear interpolation

	RETURN
	END SUBROUTINE Cloud_Base_Super
! +-------------------------------------------------------------------+
 	SUBROUTINE supmax_COEFF (AKOE,BKOE,C3,PP,TT,RO_SOLUTE,IONS,MWAERO)

		implicit none

! akoe, cm- constant in Koehler equation
! bkoe    - constant in Koehler equation
! F, cm^-2*s-  from Koehler equation
! C3 - coefficient depends on thermodynamical parameters
! PP, (DYNES/CM/CM)- PRESSURE
! TT, (K)- temperature

  integer,intent(in) :: IONS
	real(kind=r8size) ,intent(in) :: 	PP, TT
	real(kind=r8size) ,intent(out) :: AKOE, BKOE, C3
	real,intent(in) :: 				MWAERO, RO_SOLUTE

	! ... Local
		real(kind=r8size) ,parameter :: RV_MY = 461.5D4, CP = 1005.0D4, G = 9.8D2, RD_MY = 287.0D4, & ![cgs]
									                  PI = 3.141593D0
    real(kind=r8size) :: PZERO,TZERO,ALW1,SW,RO_W,HC,EW,RO_V,DV,RO_A,FL,FR,F,TPC,QV,A1,A2, &
							           C1,C2,DEG01,DEG02
	! ... Local

     TPC = TT-273.15d0

! CGS :
! RV_MY, CM*CM/SEC/SEC/KELVIN - INDIVIDUAL GAS CONSTANT
!                               FOR WATER VAPOUR
	!RV_MY=461.5D4

! CP,  CM*CM/SEC/SEC/KELVIN- SPECIFIC HEAT CAPACITY OF
!	                            MOIST AIR AT CONSTANT PRESSURE
	!CP=1005.0D4

! G,  CM/SEC/SEC- ACCELERATION OF GRAVITY
	!G=9.8D2

! RD_MY, CM*CM/SEC/SEC/KELVIN - INDIVIDUAL GAS CONSTANT
!                               FOR DRY AIR
	!RD_MY=287.0D4

! AL2_MY, CM*CM/SEC/SEC - LATENT HEAT OF SUBLIMATION

!	AL2_MY=2.834D10

! PZERO, DYNES/CM/CM - REFERENCE PRESSURE
	PZERO=1.01325D6

! TZERO, KELVIN - REFERENCE TEMPERATURE
	TZERO=273.15D0

! AL1_MY, CM*CM/SEC/SEC - LATENT HEAT OF VAPORIZATION
! ALW1=AL1_MY - ALW1 depends on temperature
! ALW1, [m^2/sec^2] -latent heat of vaporization-

	ALW1 = -6.143419998D-2*tpc**(3.0D0)+1.58927D0*tpc**(2.0D0) &
         -2.36418D3*tpc+2.50079D6
! ALW1, [cm^2/sec^2]

	ALW1 = ALW1*10.0D3

! Sw, [N*m^-1] - surface tension of water-air interface

 	IF(tpc.LT.-5.5D0) THEN
		Sw = 5.285D-11*tpc**(6.0D0)+6.283D-9*tpc**(5.0D0)+ &
        2.933D-7*tpc**(4.0D0)+6.511D-6*tpc**(3.0D0)+ &
        6.818D-5*tpc**(2.0D0)+1.15D-4*tpc+7.593D-2
 	ELSE
	   Sw = -1.55D-4*tpc+7.566165D-2
 	ENDIF

! Sw, [g/sec^2]
  	Sw = Sw*10.0D2

! RO_W, [kg/m^3] - density of liquid water
	IF (tpc.LT.0.0D0) THEN
            RO_W= -7.497D-9*tpc**(6.0D0)-3.6449D-7*tpc**(5.0D0) &
                  -6.9987D-6*tpc**(4.0D0)+1.518D-4*tpc**(3.0D0) &
                  -8.486D-3*tpc**(2.0D0)+6.69D-2*tpc+9.9986D2

	ELSE

            RO_W=(-3.932952D-10*tpc**(5.0D0)+1.497562D-7*tpc**(4.0D0) &
                 -5.544846D-5*tpc**(3.0D0)-7.92221D-3*tpc**(2.0D0)+ &
                 1.8224944D1*tpc+9.998396D2)/(1.0D0+1.8159725D-2*tpc)
	ENDIF

! RO_W, [g/cm^3]
   	RO_W=RO_W*1.0D-3

! HC, [kg*m/kelvin*sec^3] - coefficient of air heat conductivity
	HC=7.1128D-5*tpc+2.380696D-2

! HC, [g*cm/kelvin*sec^3]
	HC=HC*10.0D4

! ew, water vapor pressure ! ... KS (kg/m2/sec)

	ew = 6.38780966D-9*tpc**(6.0D0)+2.03886313D-6*tpc**(5.0D0)+ &
           3.02246994D-4*tpc**(4.0D0)+2.65027242D-2*tpc**(3.0D0)+ &
           1.43053301D0*tpc**(2.0D0)+4.43986062D1*tpc+6.1117675D2

! ew, [g/cm*sec^2]

	ew=ew*10.0D0

! akoe & bkoe - constants in Koehler equation

	!RO_SOLUTE=2.16D0
	AKOE=2.0D0*Sw/(RV_MY*RO_W*(tpc+TZERO))
	!BKOE=2.0D0*4.3D0/(22.9D0+35.5D0)
	BKOE = ions*4.3/mwaero
	BKOE=BKOE*(4.0D0/3.0D0)*pi*RO_SOLUTE

! RO_V, g/cm^3 - density of water vapor
!                calculate from equation of state for water vapor
	RO_V = ew/(RV_MY*(tpc+TZERO))

! DV,  [cm^2/sec] - coefficient of diffusion

! 'Pruppacher, H.R., Klett, J.D., 1997. Microphysics of Clouds and Precipitation'
! 'page num 503, eq. 13-3'
	DV = 0.211D0*(PZERO/PP)*((tpc+TZERO)/TZERO)**(1.94D0)

! QV,  g/g- water vapor mixing ratio
! ro_a, g/cm^3 - density of air, from equation of state
	RO_A=PZERO/((tpc+TZERO)*RD_MY)

! F, s/m^2 - coefficient depending on thermodynamics parameters
!            such as temperature, thermal conductivity
!            of air, etc
! left side of F equation
	FL=(RO_W*ALW1**(2.0D0))/(HC*RV_MY*(tpc+TZERO)**(2.0D0))

! right side of F equation
	FR = RO_W*RV_MY*(tpc+TZERO)/(ew*DV)
	F = FL + FR

! QV, g/g - water vapor mixing ratio
	QV=RO_V/RO_A

! A1,A2 -  terms from equation describing changes of
!          supersaturation in an adiabatic cloud air
!	   parcel
! A1, [cm^-1] - constant
! A2, [-]     - constant

	A1=(G*ALW1/(CP*RV_MY*(tpc+TZERO)**(2.0D0)))-(G/(RD_MY*(tpc+TZERO)))
	A2=(1.0D0/QV)+(ALW1**(2.0D0))/(CP*RV_MY*(tpc+TZERO)**(2.0D0))

! C1,C2,C3,C4- constant parameters

	C1=1.058D0
	C2=1.904D0
	DEG01=1.0D0/3.0D0
	DEG02=1.0D0/6.0D0
	C3=C1*(F*A1/3.0D0)**(0.75D0)*DSQRT(3.0D0*RO_A/(4.0D0*pi*RO_W*A2))
  !C4=(C2-C1)**(DEG01)*(F*A1/3.0D0)**(0.25D0)*RO_A**(DEG02)* &
  !      DSQRT(3.0D0/(4.0D0*pi*RO_W*A2))

	RETURN
	END SUBROUTINE SupMax_COEFF
! +----------------------------------------------------------------------------------------------------+
	SUBROUTINE LogNormal_modes_Aerosol(FCCNR_CON,FCCNR_MAR,NKR_local,COL,XL,XCCN,RCCN,RO_SOLUTE,Scale_Fa,IType)

	implicit none
! ... Interface
		integer,intent(in) :: NKR_local, Itype
		real(kind=r4size) ,intent(in) :: XL(:), COL, RO_SOLUTE, Scale_Fa
		real(kind=r8size) ,intent(out) :: FCCNR_CON(:), FCCNR_MAR(:)
		real(kind=r4size) ,intent(out) :: XCCN(:),RCCN(:)
! ... Interface
! ... Local
		integer :: mode_num, KR
		integer,parameter :: modemax = 3
		real(kind=r8size)  :: ccncon1, ccncon2, ccncon3, radius_mean1, radius_mean2, radius_mean3, &
							            sig1, sig2, sig3,													 &
							            ccncon(modemax), sig(modemax), radius_mean(modemax)
		real(kind=r8size)  :: CONCCCNIN, FCCNR_tmp(NKR_local), DEG01, X0DROP, &
						              XOCCN, X0, R0, RCCN_MICRON, S_KR, S(NKR_local), X0CCN, ROCCN(NKR_local), &
	                        RO_SOLUTE_Ammon, RO_SOLUTE_NaCl,arg11,arg12,arg13,arg21,arg22,arg23, & 
                          arg31,arg32,arg33,dNbydlogR_norm1,dNbydlogR_norm2,dNbydlogR_norm3


		real(kind=r8size) ,PARAMETER :: RCCN_MAX = 0.4D-4         ! [cm]
		real(kind=r8size) ,PARAMETER :: RCCN_MIN = 0.003D-4		! [cm]
		! ... Minimal radii for dry aerosol for the 3 log normal distribution
	 	real(kind=r8size) ,PARAMETER :: RCCN_MIN_3LN = 0.00048D-4 ! [cm]
		real(kind=r8size) ,PARAMETER :: PI = 3.14159265D0
! ... Local

	! ... Calculating the CCN radius grid
	!RO_SOLUTE_NaCl = 2.16D0  ! [g/cm3]
	!RO_SOLUTE_Ammon = 1.79	 ! [g/cm3]
	DEG01 = 1.0D0/3.0D0
	X0DROP = XL(2)
	X0CCN = X0DROP/(2.0**(NKR_local))
	DO KR = NKR_local,1,-1
	   ROCCN(KR) = RO_SOLUTE
	   X0 = X0CCN*2.0D0**(KR)
	   R0 = (3.0D0*X0/4.0D0/3.141593D0/ROCCN(KR))**DEG01
	   XCCN(KR) = X0
	   RCCN(KR) = R0
	ENDDO

	IF(IType == 1) THEN ! Maritime regime

		ccncon1 = 340.000
		radius_mean1 = 0.00500D-04
		sig1 = 1.60000

		ccncon2 = 60.0000
		radius_mean2 = 0.03500D-04
		sig2 = 2.00000

		ccncon3 = 3.10000
		radius_mean3 = 0.31000D-04
		sig3 = 2.70000

	ELSE IF(IType == 2) THEN ! Continental regime

		ccncon1 = 1000.000
		radius_mean1 = 0.00800D-04
		sig1 = 1.60000

		ccncon2 = 800.0000
		radius_mean2 = 0.03400D-04
		sig2 = 2.10000

		ccncon3 = 0.72000
		radius_mean3 = 0.46000D-04
		sig3 = 2.20000

	ENDIF

	FCCNR_tmp = 0.0
  CONCCCNIN = 0.0

  arg11 = ccncon1/(sqrt(2.0D0*pi)*log(sig1))
  arg21 = ccncon2/(sqrt(2.0D0*pi)*log(sig2))
  arg31 = ccncon3/(sqrt(2.0D0*pi)*log(sig3))
  
  dNbydlogR_norm1 = 0.0
  dNbydlogR_norm2 = 0.0
  dNbydlogR_norm3 = 0.0
  do kr = NKR_local,1,-1
      if(RCCN(kr) > RCCN_MIN_3LN .and. RCCN(kr) < RCCN_MAX)then
          arg12 = (log(RCCN(kr)/radius_mean1))**2.0
          arg13 = 2.0D0*((log(sig1))**2.0);
          dNbydlogR_norm1 = arg11*exp(-arg12/arg13)*(log(2.0)/3.0)
          arg22 = (log(RCCN(kr)/radius_mean2))**2.0
          arg23 = 2.0D0*((log(sig2))**2.0)
          dNbydlogR_norm2 = dNbydlogR_norm1 + arg21*exp(-arg22/arg23)*(log(2.0)/3.0)
          arg32 = (log(RCCN(kr)/radius_mean3))**2.0
          arg33 = 2.0D0*((log(sig3))**2.0)
          dNbydlogR_norm3 = dNbydlogR_norm2 + arg31*exp(-arg32/arg33)*(log(2.0)/3.0);
          FCCNR_tmp(kr) = dNbydlogR_norm3/col
      endif
enddo
  
  CONCCCNIN = col*sum(FCCNR_tmp(:))
  print*,'CONCCCNIN',CONCCCNIN
  if(IType == 1) FCCNR_MAR = Scale_Fa*FCCNR_tmp
  if(IType == 2) FCCNR_CON = Scale_Fa*FCCNR_tmp

	RETURN
	END SUBROUTINE LogNormal_modes_Aerosol
! +---------------------------------------+
 end module module_mp_SBM_Nucleation
 ! +----------------------------------------------------------------------------+
 ! +----------------------------------------------------------------------------+
  MODULE module_mp_fast_sbm

  USE module_mp_SBM_polar_radar,ONLY:polar_hucm
  USE module_mp_SBM_BreakUp,ONLY:Spont_Rain_BreakUp,BreakUp_Snow,KR_SNOW_MIN,KR_SNOW_MAX
  USE module_mp_SBM_Nucleation,ONLY:JERNUCL01_KS, LogNormal_modes_Aerosol
  USE module_mp_SBM_Auxiliary,ONLY:JERRATE_KS,JERTIMESC_KS,JERSUPSAT_KS,  &
                                   JERDFUN_KS,JERDFUN_NEW_KS,POLYSVP,Relaxation_Time
  USE scatt_tables,ONLY:faf1,fbf1,fab1,fbb1,         &
 						            faf3,fbf3,fab3,fbb3,         &
             						faf4,fbf4,fab4,fbb4,         &
             						faf5,fbf5,fab5,fbb5,         &
             						LOAD_TABLES,                 &
             						temps_water,temps_fd,temps_crystals,  &
             						temps_snow,temps_graupel,temps_hail,  &
             						fws_fd,fws_crystals,fws_snow,		      &
             						fws_graupel,fws_hail, 		            &
             						usetables,                            &
                         twolayer_hail,twolayer_graupel,twolayer_fd,twolayer_snow,rpquada,usequad

 PRIVATE

 PUBLIC FAST_SBM,FAST_HUCMINIT

 ! Kind paramater
 INTEGER, PARAMETER, PRIVATE:: R8SIZE = 8
 INTEGER, PARAMETER, PRIVATE:: R16SIZE = 16 
 INTEGER, PARAMETER, PRIVATE:: R4SIZE = 4

 ! JacobS: Hard coding the bin-wise indices for the NMM core
 INTEGER, PRIVATE,PARAMETER ::  p_ff1i01=2, p_ff1i33=34,p_ff5i01=35,p_ff5i33=67,p_ff6i01=68,&
                                p_ff6i33=100,p_ff8i01=101,p_ff8i43=143

 ! JacobS: Hard coding for the polarimetric operator output array 
  INTEGER, PRIVATE,PARAMETER :: r_p_ff1i01=2, r_p_ff1i06=07,r_p_ff2i01=08,r_p_ff2i06=13,r_p_ff3i01=14,&
                                r_p_ff3i06=19,r_p_ff4i01=20,r_p_ff4i06=25,r_p_ff5i01=26,r_p_ff5i06=31,r_p_ff6i01=32,r_p_ff6i06=37,&
                                r_p_ff7i01=38,r_p_ff7i06=43,r_p_ff8i01=44,r_p_ff8i06=49,r_p_ff9i01=50,r_p_ff9i06=55

 INTEGER,PARAMETER :: IBREAKUP = 1
 INTEGER,PARAMETER :: Snow_BreakUp_On = 1
 INTEGER,PARAMETER :: Spont_Rain_BreakUp_On = 1
 LOGICAL,PARAMETER :: CONSERV = .TRUE.
 INTEGER,PARAMETER :: JIWEN_FAN_MELT = 1
 LOGICAL,PARAMETER :: IPolar_HUCM = .TRUE.
 INTEGER,PARAMETER :: hail_opt = 1
 INTEGER,PARAMETER :: ILogNormal_modes_Aerosol = 1

 REAL,PARAMETER :: DX_BOUND = 1433
 REAL(kind=r8size), PARAMETER ::  SCAL = 1.d0
 INTEGER,PARAMETER :: ICEPROCS = 1
 INTEGER,PARAMETER :: ICETURB = 0, LIQTURB = 0

 INTEGER,PARAMETER :: icempl=1,ICEMAX=3,NCD=33,NHYDR=5,NHYDRO=7    &
             					,K0_LL=8,KRMIN_LL=1,KRMAX_LL=19,L0_LL=6                  &
             					,IEPS_400=1,IEPS_800=0,IEPS_1600=0                       &
             					,K0L_GL=16,K0G_GL=16                                     &
             					,KRMINL_GL=1,KRMAXL_GL=24                                &
             					,KRMING_GL=1,KRMAXG_GL=33                                &
             					,KRDROP=15,KRBREAK=17,KRICE=18                           & ! KRDROP=Bin 15 --> 50um
             					!,NKR=43,JMAX=43,NRG=2,JBREAK=28,BR_MAX=43,KRMIN_BREAKUP=31,NKR_aerosol=43   ! 43 bins
             					,NKR=33,JMAX=33,NRG=2,JBREAK=18,BR_MAX=33,KRMIN_BREAKUP=31,NKR_aerosol=43    ! 33 bins

 REAL(kind=r4size) :: dt_coll
 REAL,PARAMETER :: C1_MEY=0.00033,C2_MEY=0.0,COL=0.23105, &
                   p1=1000000.0,p2=750000.0,p3=500000.0,  &
                   ALCR = 0.5, &
                   ALCR_G = 100.0 ! ... [KS] forcing no transition from graupel to hail in this version
 INTEGER :: NCOND, NCOLL
 INTEGER,PARAMETER :: kr_icempl=9

 REAL(kind=r4size) :: &
 					 RADXX(NKR,NHYDR-1),MASSXX(NKR,NHYDR-1),DENXX(NKR,NHYDR-1) &
 					,MASSXXO(NKR,NHYDRO),DENXXO(NKR,NHYDRO),VRI(NKR)           &
          ,XX(nkr),ROCCN(nkr),FCCNR_MIX(NKR),FCCNR(NKR)

 REAL(kind=r8size),DIMENSION (NKR) :: FF1R_D,XL_D,VR1_D &
 							,FF3R_D,XS_D,VR3_D,VTS_D,FLIQFR_SD,RO3BL_D &
 							,FF4R_D,XG_D,VR4_D,VTG_D,FLIQFR_GD,RO4BL_D &
 							,FF5R_D,XH_D,VR5_D,VTH_D,FLIQFR_HD,RO5BL_D &
 							,XS_MELT_D,XG_MELT_D,XH_MELT_D,VR_TEST,FRIMFR_SD,RF3R

 ! ... SBMRADAR VARIABLES
 REAL(kind=r8size),DIMENSION (nkr,icemax) :: XI_MELT_D &
							,FF2R_D,XI_D,VR2_D,VTC_D,FLIQFR_ID,RO2BL_D
 REAL(kind=r8size) :: T_NEW_D,rhocgs_D,pcgs_D,DT_D,qv_old_D,qv_d

 REAL(kind=r4size),private :: C2,C3,C4
 REAL(kind=r8size),private ::  &
 	            xl_mg(nkr),xs_mg(nkr),xg_mg(nkr),xh_mg(nkr) &
             ,xi1_mg(nkr),xi2_mg(nkr),xi3_mg(nkr)

 ! ----------------------------------------------------------------------------------+
 ! ... WRFsbm_Init
 ! ... Holding Lookup tables and memory arrays for the FAST_SBM module
         REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:)::                             &
                                          bin_mass,tab_colum,tab_dendr,tab_snow,bin_log
         REAL (KIND=R4SIZE), ALLOCATABLE, DIMENSION(:) ::                            &
                                          RLEC,RSEC,RGEC,RHEC,XL,XS,XG,XH,VR1,VR3,VR4,VR5
         REAL (KIND=R4SIZE), ALLOCATABLE, DIMENSION(:,:)::                           &
                                          RIEC,XI,VR2
         REAL (KIND=R4SIZE), ALLOCATABLE ::                              &
                                          COEFIN(:),SLIC(:,:),TLIC(:,:), &
                                          YWLL_1000MB(:,:),YWLL_750MB(:,:),YWLL_500MB(:,:)
         REAL (KIND=R4SIZE), ALLOCATABLE ::                              &
                                         YWLI_300MB(:,:,:),YWLI_500MB(:,:,:),YWLI_750MB(:,:,:),              &
                                         YWLG_300MB(:,:),YWLG_500MB(:,:),YWLG_750MB(:,:),YWLG(:,:),          &
                                         YWLH_300MB(:,:),YWLH_500MB(:,:),YWLH_750MB(:,:),                    &
                                         YWLS_300MB(:,:),YWLS_500MB(:,:),YWLS_750MB(:,:),                    &
                                         YWII_300MB(:,:,:,:),YWII_500MB(:,:,:,:),YWII_750MB(:,:,:,:),        &
                                         YWII_300MB_tmp(:,:,:,:),YWII_500MB_tmp(:,:,:,:),YWII_750MB_tmp(:,:,:,:),        &
                                         YWIS_300MB(:,:,:),YWIS_500MB(:,:,:),YWIS_750MB(:,:,:),              &
                                         YWSG_300MB(:,:),YWSG_500MB(:,:),YWSG_750MB(:,:),                    &
                                         YWSS_300MB(:,:),YWSS_500MB(:,:),YWSS_750MB(:,:)

         REAL (KIND=R4SIZE), ALLOCATABLE ::                  &
                                         RO1BL(:), RO2BL(:,:), RO3BL(:), RO4BL(:), RO5BL(:),                 &
                                         RADXXO(:,:)

         INTEGER,ALLOCATABLE ::              ima(:,:)
         REAL (KIND=R8SIZE), ALLOCATABLE ::  chucm(:,:)

         REAL (KIND=R8SIZE), ALLOCATABLE ::  BRKWEIGHT(:),ECOALMASSM(:,:), Prob(:),Gain_Var_New(:,:),NND(:,:)
         REAL (KIND=R4SIZE), ALLOCATABLE ::  DROPRADII(:),PKIJ(:,:,:),QKJ(:,:)
         INTEGER ::          ikr_spon_break

         REAL (KIND=R8SIZE), ALLOCATABLE ::  cwll(:,:), &
                                             cwli_1(:,:),cwli_2(:,:),cwli_3(:,:),        &
                                             cwil_1(:,:),cwil_2(:,:),cwil_3(:,:),        &
                                             cwlg(:,:),cwlh(:,:),cwls(:,:),              &
                                             cwgl(:,:),cwhl(:,:),cwsl(:,:),              &
                                             cwii_1_1(:,:),cwii_1_2(:,:),cwii_1_3(:,:),  &
                                             cwii_2_1(:,:),cwii_2_2(:,:),cwii_2_3(:,:),  &
                                             cwii_3_1(:,:),cwii_3_2(:,:),cwii_3_3(:,:),  &
                                             cwis_1(:,:),cwis_2(:,:),cwis_3(:,:),        &
                                             cwsi_1(:,:),cwsi_2(:,:),cwsi_3(:,:),        &
                                             cwig_1(:,:),cwig_2(:,:),cwig_3(:,:),        &
                                             cwih_1(:,:),cwih_2(:,:),cwih_3(:,:),        &
                                             cwsg(:,:),cwss(:,:)
         REAL(kind=r8size),ALLOCATABLE ::  FCCNR_MAR(:),FCCNR_CON(:)
         REAL(kind=r4size),ALLOCATABLE :: Scale_CCN_Factor,XCCN(:),RCCN(:),FCCN(:)

 ! ... WRFsbm_Init
 ! --------------------------------------------------------------------------------+

 INTEGER :: icloud

 ! ### (KS) - CCN related
 ! -----------------------------------------------------------------------
 !REAL (KIND=R4SIZE), parameter :: mwaero = 22.9 + 35.5 ! sea salt
 real(kind=r4size),parameter :: mwaero = 115.0
 !integer,parameter :: ions = 2        	! sea salt
 integer,parameter  :: ions = 3         ! ammonium-sulfate
 !real(KIND=R4SIZE),parameter :: RO_SOLUTE = 2.16   	! sea salt
 real(kind=r4size),parameter ::  RO_SOLUTE = 1.79  	! ammonium-sulfate
 ! ----------------------------------------------------------------------
 REAL (KIND=R4SIZE) :: FR_LIM(NKR), FRH_LIM(NKR)

   CONTAINS
 !-----------------------------------------------------------------------
       SUBROUTINE FAST_SBM (w,u,v,th_old,                                &
      &                      chem_new,n_chem,                            &
      &                      itimestep,DT,DX,DY,                         &
      &                      dz8w,rho_phy,p_phy,pi_phy,th_phy,           &
      &                      xland,domain_id,ivgtyp,xlat,xlong,          &
      &                      QV,QC,QR,QI,QS,QG,QV_OLD,                   &
      &                      QNC,QNR,QNI,QNS,QNG,QNA,                    &
      &                      ids,ide, jds,jde, kds,kde,                  &
      &                      ims,ime, jms,jme, kms,kme,                  &
      &                      its,ite, jts,jte, kts,kte,                  &
      &                      diagflag,                                   &
      &                      sbmradar,num_sbmradar,                      &
      &                      sbm_diagnostics,                            &
      &                      RAINNC,RAINNCV,SNOWNC,SNOWNCV,GRAUPELNC,GRAUPELNCV,SR)
 !-----------------------------------------------------------------------
       IMPLICIT NONE
 !-----------------------------------------------------------------------
 	INTEGER :: KR,IKL,ICE

 	INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
 	&                     ,IMS,IME,JMS,JME,KMS,KME                    &
 	&                     ,ITS,ITE,JTS,JTE,KTS,KTE                    &
 	&                     ,ITIMESTEP,N_CHEM,NUM_SBMRADAR,domain_id    &
 	&                     ,sbm_diagnostics

 	REAL, INTENT(IN) 	    :: DT,DX,DY
 	REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), &
 	INTENT(IN   ) ::                                 &
 							  U, &
 							  V, &
 							  W

 	REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,n_chem),INTENT(INOUT)   :: chem_new
 	REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_sbmradar),INTENT(INOUT)   :: sbmradar
 	REAL,    DIMENSION( ims:ime , kms:kme , jms:jme ),               &
 		       INTENT(INOUT) ::                                          &
 						  qv, 		&
 						  qv_old, 	&
 						  th_old, 	&
 						  qc, 		&
 						  qr, 		&
 						  qi,	 	   &
 						  qs, 		&
 						  qg, 		&
 						  qnc, 		&
 						  qnr, 		&
              qni,    &
 						  qns, 		&
 						  qng, 		&
 						  qna

       REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN)   :: XLAND
       LOGICAL, OPTIONAL, INTENT(IN) :: diagflag

       INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(IN)::   IVGTYP
       REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )    :: XLAT, XLONG
       REAL, INTENT(IN),     DIMENSION(ims:ime, kms:kme, jms:jme)::      &
      &                      dz8w,p_phy,pi_phy,rho_phy
       REAL, INTENT(INOUT),  DIMENSION(ims:ime, kms:kme, jms:jme)::      &
      &                      th_phy
       REAL, INTENT(INOUT),  DIMENSION(ims:ime,jms:jme), OPTIONAL ::     &
      &      RAINNC,RAINNCV,SNOWNC,SNOWNCV,GRAUPELNC,GRAUPELNCV,SR

 !-----------------------------------------------------------------------
 !     LOCAL VARS
 !-----------------------------------------------------------------------

       REAL (KIND=R4SIZE),  DIMENSION(its-1:ite+1, kts:kte, jts-1:jte+1)::  &
                                                  t_new,t_old,zcgs,rhocgs,pcgs

       INTEGER :: I,J,K,KFLIP
       INTEGER :: KRFREEZ

       REAL (KIND=R4SIZE),PARAMETER :: Z0IN=2.0E5,ZMIN=2.0E5

       REAL (KIND=R4SIZE) :: EPSF2D, &
      &        TAUR1,TAUR2,EPS_R1,EPS_R2,ANC1IN, &
      &        PEPL,PEPI,PERL,PERI,ANC1,ANC2,PARSP, &
      &        AFREEZMY,BFREEZMY,BFREEZMAX, &
      &        TCRIT,TTCOAL, &
      &        EPSF1,EPSF3,EPSF4, &
      &        SUP2_OLD, DSUPICEXZ,TFREEZ_OLD,DTFREEZXZ, &
      &        AA1_MY,BB1_MY,AA2_MY,BB2_MY, &
      &        DTIME,DTCOND,DTNEW,DTCOLL, &
      &        A1_MYN, BB1_MYN, A2_MYN, BB2_MYN
      DATA A1_MYN, BB1_MYN, A2_MYN, BB2_MYN  &
      &      /2.53,5.42,3.41E1,6.13/
      DATA AA1_MY,BB1_MY,AA2_MY,BB2_MY/2.53E12,5.42E3,3.41E13,6.13E3/
             !QSUM,ISUM,QSUM1,QSUM2,CCNSUM1,CCNSUM2
      DATA KRFREEZ,BFREEZMAX,ANC1,ANC2,PARSP,PEPL,PEPI,PERL,PERI, &
      &  TAUR1,TAUR2,EPS_R1,EPS_R2,TTCOAL,AFREEZMY,&
      &  BFREEZMY,EPSF1,EPSF3,EPSF4,TCRIT/21,&
      &  0.6600E00, &
      &  1.0000E02,1.0000E02,0.9000E02, &
      &  0.6000E00,0.6000E00,1.0000E-03,1.0000E-03, &
      &  0.5000E00,0.8000E00,0.1500E09,0.1500E09, &
      &  2.3315E02,0.3333E-04,0.6600E00, &
      &  0.1000E-02,0.1000E-05,0.1000E-05, &
      &  2.7015E02/

      REAL (KIND=R4SIZE),DIMENSION (nkr) :: FF1IN,FF3IN,FF4IN,FF5IN,&
      &              FF1R,FF3R,FF4R,FF5R,FLIQFR_S,FRIMFR_S,FLIQFR_G,FLIQFR_H, &
      &              FF1R_NEW,FF3R_NEW,FF4R_NEW,FF5R_NEW
      REAL (KIND=R4SIZE),DIMENSION (nkr) :: FL3R,FL4R,FL5R,FL3R_NEW,FL4R_NEW,FL5R_NEW

      REAL (KIND=R4SIZE),DIMENSION (nkr,icemax) :: FF2IN,FF2R,FLIQFR_I

      REAL (KIND=R4SIZE) :: XI_MELT(NKR,ICEMAX),XS_MELT(NKR),XG_MELT(NKR),XH_MELT(NKR)
 !!!! NOTE: ZCGS AND OTHER VARIABLES ARE ALSO DIMENSIONED IN FALFLUXHUCM
      REAL (KIND=R8SIZE) :: DEL1NR,DEL2NR,DEL12R,DEL12RD,ES1N,ES2N,EW1N,EW1PN
      REAL (KIND=R8SIZE) :: DELSUP1,DELSUP2,DELDIV1,DELDIV2
      REAL (KIND=R8SIZE) :: TT,QQ,TTA,QQA,PP,DPSA,DELTATEMP,DELTAQ
      REAL (KIND=R8SIZE) :: DIV1,DIV2,DIV3,DIV4,DEL1IN,DEL2IN,DEL1AD,DEL2AD
      REAL (KIND=R4SIZE) :: DEL_BB,DEL_BBN,DEL_BBR, TTA_r
      REAL (KIND=R4SIZE) :: FACTZ,CONCCCN_XZ,CONCDROP
      REAL (KIND=R4SIZE) :: SUPICE(KTE),AR1,AR2, &
      					& DERIVT_X,DERIVT_Y,DERIVT_Z,DERIVS_X,DERIVS_Y,DERIVS_Z, &
      					& ES2NPLSX,ES2NPLSY,EW1NPLSX,EW1NPLSY,UX,VX, &
      					& DEL2INPLSX,DEL2INPLSY,DZZ(KTE)
 	 INTEGER KRR,I_START,I_END,J_START,J_END
      REAL (KIND=R4SIZE) :: DTFREEZ_XYZ(ITE,KTE,JTE),DSUPICE_XYZ(ITE,KTE,JTE)

      REAL (KIND=R4SIZE) :: DXHUCM,DYHUCM
      REAL (KIND=R4SIZE) :: FMAX1,FMAX2(ICEMAX),FMAX3,FMAX4,FMAX5
 	INTEGER ISYM1,ISYM2(ICEMAX),ISYM3,ISYM4,ISYM5
 	INTEGER DIFFU
 	REAL (KIND=R4SIZE) :: DELTAW
 	REAL (KIND=R4SIZE) :: zcgs_z(kts:kte),pcgs_z(kts:kte),rhocgs_z(kts:kte),ffx_z(kts:kte,nkr)
 	REAL (KIND=R4SIZE) :: z_full
 	REAL (KIND=R4SIZE) :: VRX(kts:kte,NKR)

 	REAL (KIND=R4SIZE) :: VR1_Z(NKR,KTS:KTE), FACTOR_P, VR1_Z3D(NKR,ITS:ITE,KTS:KTE,JTS:JTE)
 	REAL (KIND=R4SIZE) :: VR2_ZC(NKR,KTS:KTE), VR2_Z(NKR,ICEMAX)
 	REAL (KIND=R4SIZE) :: VR2_ZP(NKR,KTS:KTE)
 	REAL (KIND=R4SIZE) :: VR2_ZD(NKR,KTS:KTE)
 	REAL (KIND=R4SIZE) :: VR3_Z(NKR,KTS:KTE), VR3_Z3D(NKR,ITS:ITE,KTS:KTE,JTS:JTE)
 	REAL (KIND=R4SIZE) :: VR4_Z(NKR,KTS:KTE), VR4_Z3D(NKR,ITS:ITE,KTS:KTE,JTS:JTE)
 	REAL (KIND=R4SIZE) :: VR5_Z(NKR,KTS:KTE), VR5_Z3D(NKR,ITS:ITE,KTS:KTE,JTS:JTE)
 	REAL (KIND=R4SIZE) :: BulkDen_Snow(NKR,ITS:ITE,KTS:KTE,JTS:JTE) ! Local array for snow density

 	REAL (KIND=R4SIZE), PARAMETER :: RON=8.E6, GON=5.E7,PI=3.14159265359
 	REAL (KIND=R4SIZE) :: EFF_N,EFF_D
     REAL (KIND=R4SIZE) :: EFF_NI(its:ite,kts:kte,jts:jte),eff_di(its:ite,kts:kte,jts:jte)
 	REAL (KIND=R4SIZE) :: EFF_NQIC,eff_DQIC
 	REAL (KIND=R4SIZE) :: EFF_NQIP,eff_DQIP
 	REAL (KIND=R4SIZE) :: EFF_NQID,eff_DQID
 	REAL (KIND=R4SIZE) :: lambda,chi0,xi1,xi2,xi3,xi4,xi5,r_e,chi_3,f1,f2,volume,surface_area,xi6,ft,chi_e,ft_bin
 	REAL (KIND=R4SIZE), DIMENSION(kts:kte)::                            &
 						  qv1d, qr1d, nr1d, qs1d, ns1d, qg1d, ng1d, t1d, p1d
 	REAL (KIND=R4SIZE), DIMENSION(kts:kte):: dBZ

 	REAL (KIND=R4SIZE) :: nzero,son,nzero_less
 	parameter (son=2.E7)
 	REAL (KIND=R4SIZE) :: raddumb(nkr),massdumb(nkr)
 	REAL (KIND=R4SIZE) :: hydrosum

 	integer imax,kmax,jmax
 	REAL (KIND=R4SIZE) :: gmax,tmax,qmax,divmax,rainmax,qnmax,inmax,knmax,hydro,difmax, tdif, tt_old, w_stag, w_stag_my, qq_old,teten,es
 	integer  print_int
 	parameter (print_int=300)

 	integer t_print,i_print,j_print,k_print
 	REAL (KIND=R8SIZE), DIMENSION(kts:kte):: zmks_1d
 	REAL (KIND=R8SIZE) :: dx_dbl, dy_dbl
 	INTEGER,DIMENSION (nkr) :: melt_snow,melt_graupel,melt_hail,melt_ice
 	!DOUBLE PRECISION,DIMENSION (nkr) :: dmelt_snow,dmelt_graupel,dmelt_hail,dmelt_ice
 	INTEGER ihucm_flag
 	REAL (KIND=R4SIZE) :: NSNOW_ADD

 	! ... Polar-HUCM
 	INTEGER,PARAMETER :: n_radar = 10
 	integer :: ijk, Mod_Flag
 	REAL (KIND=R8SIZE),PARAMETER :: wavelength = 11.0D0 ! ### (KS) - Rhyzkov uses this wavelength (NEXRAD)
 	INTEGER :: IWL
 	REAL (KIND=R4SIZE) :: DIST_SING
 	REAL (KIND=R8SIZE) :: BKDEN_Snow(NKR)
 	REAL (KIND=R8SIZE) ::  DISTANCE,FL1_FD(NKR),BULK(NKR), BulkDens_Snow(NKR)
 	REAL (KIND=R8SIZE) ::  FF1_FD(NKR),FFL_FD(NKR),OUT1(n_radar),OUT2(n_radar),OUT3(n_radar),OUT4(n_radar),OUT5(n_radar), &
 						   OUT6(n_radar),OUT7(n_radar),OUT8(n_radar),OUT9(n_radar), FL1R_FD(NKR)
 	REAL (KIND=R8SIZE) :: rate_shed_per_grau_grampersec(NKR), rate_shed_per_hail_grampersec(NKR), rhoair_max

 	integer :: count_H, count_G, count_S_l, count_S_r

 	REAL (KIND=R8SIZE) :: RMin_G
 	integer :: KR_GRAUP_MAX_BLAHAK, KR_G_TO_H

 	! ... Cloud Base .........................................................
 	REAL (KIND=R8SIZE) ::	SUP_WATER, ES1N_KS, ES1N_dummy, ES2N_dummy
 	logical :: K_found
 	integer ::	KZ_Cloud_Base(its:ite,jts:jte), IS_THIS_CLOUDBASE,KR_Small_Ice
 	! ........................................................................
 	REAL (KIND=R4SIZE) :: qna0(its:ite,kts:kte,jts:jte), fr_hom, w_stagm, CollEff_out, FACT
 	REAL (KIND=R4SIZE) :: FACTZ_new(KMS:KME,NKR), TT_r
 ! ### (KS) ............................................................................................
 	INTEGER :: NZ,NZZ,II,JJ

  XS_d = XS

  if (itimestep.eq.1)then
    if (iceprocs.eq.1) call wrf_message(" FAST SBM: ICE PROCESES ACTIVE ")
    if (iceprocs.eq.0) call wrf_message(" FAST SBM: LIQUID PROCESES ONLY")
  end if

  NCOND = 3
  NCOLL = 1
  DTCOND = DT/REAL(NCOND)
  DTCOLL = DT/REAL(NCOLL)
  dt_coll = DTCOLL

  DEL_BB=BB2_MY-BB1_MY
  DEL_BBN=BB2_MYN-BB1_MYN
  DEL_BBR=BB1_MYN/DEL_BBN

 if (conserv)then
    DO j = jts,jte
       DO i = its,ite
          DO k = kts,kte

            rhocgs(I,K,J)=rho_phy(I,K,J)*0.001
 				! ... Drops
   				  KRR=0
   				  DO KR=p_ff1i01,p_ff1i33
			        KRR=KRR+1
		          chem_new(I,K,J,KR)=chem_new(I,K,J,KR)*RHOCGS(I,K,J)/COL/XL(KRR)/XL(KRR)/3.0
   				  END DO
 				! ... Snow
   				  KRR=0
   				  DO KR=p_ff5i01,p_ff5i33
  			      KRR=KRR+1
  			      chem_new(I,K,J,KR)=chem_new(I,K,J,KR)*RHOCGS(I,K,J)/COL/XS(KRR)/XS(KRR)/3.0
   				  END DO
 				! ... Aerosols
   				  KRR=0
   				  DO KR=p_ff8i01,p_ff8i43
  		          KRR=KRR+1
   					    chem_new(I,K,J,KR) = chem_new(I,K,J,KR)*RHOCGS(I,K,J)/1000.0
   				  END DO
 				!  ... Hail or Graupel [same registry adresses]
           if(hail_opt == 1) then
             KRR=0
             DO KR=p_ff6i01,p_ff6i33
                 KRR=KRR+1
                 chem_new(I,K,J,KR)=chem_new(I,K,J,KR)*RHOCGS(I,K,J)/COL/XH(KRR)/XH(KRR)/3.0
             END DO

           else
             KRR=0
             DO KR=p_ff6i01,p_ff6i33
                 KRR=KRR+1
                 chem_new(I,K,J,KR)=chem_new(I,K,J,KR)*RHOCGS(I,K,J)/COL/XG(KRR)/XG(KRR)/3.0
             END DO
           endif

 				END DO ! K
 			END DO	! I
 		END DO ! J
  end if

  DXHUCM=100.*DX
  DYHUCM=100.*DY

  I_START=MAX(1,ITS-1)
  J_START=MAX(1,JTS-1)
  I_END=MIN(IDE-1,ITE+1)
  J_END=MIN(JDE-1,JTE+1)

   DO j = j_start,j_end
      DO i = i_start,i_end
         z_full=0.
         DO k = kts,kte
            pcgs(I,K,J)=P_PHY(I,K,J)*10.
            rhocgs(I,K,J)=rho_phy(I,K,J)*0.001
            zcgs(I,K,J)=z_full+0.5*dz8w(I,K,J)*100
            !height(i,k,j) = 1.0e-2*zcgs(i,k,j) ! in [m]
            z_full=z_full+dz8w(i,k,j)*100.
         ENDDO
      ENDDO
   ENDDO

 ! +---------------------------------------+
 ! ... Initial Aerosol distribution
 ! +---------------------------------------+
 	  if (itimestep == 1)then
 	   FACTZ_new = 0.0
 	   DO j = jts,jte
 		  DO i = its,ite
 			DO k = kts,kte
 			   rhoair_max = rhocgs(i,1,j) ! [g/cm3]
 			   if(ILogNormal_modes_Aerosol == 1)then
 				  IF (zcgs(I,K,J) .LE. ZMIN)THEN
 					  FACTZ = 1.0
 				  ELSE
 					  FACTZ=EXP(-(zcgs(I,K,J)-ZMIN)/Z0IN)
 				  END IF
 				  ! ... CCN
 				  KRR = 0
 				  DO KR = p_ff8i01,p_ff8i43
 					KRR = KRR + 1
   					if (xland(i,j) == 1)then
   						! chem_new(I,K,J,KR)=FCCNR_CON(KRR)*FACTZ
   						chem_new(I,K,J,KR) = (FCCNR_CON(KRR)/rhoair_max)*rhocgs(i,k,j) ! ... distributed vertically as [#/g]
             else
              ! chem_new(I,K,J,KR)=FCCNR_MAR(KRR)*FACTZ
   						chem_new(I,K,J,KR) = (FCCNR_MAR(KRR)/rhoair_max)*rhocgs(i,k,j) ! ... distributed vertically as [#/g]
   					endif
 				  END DO
 				endif
 			end do
 		  end do
 	   end do
 	 end if

! +--------------------------------------------+
 ! ... Aerosols boundary conditions
 !    (for 3D application running with MPI)   
 ! +--------------------------------------------+
#if (defined(DM_PARALLEL))
    if (itimestep > 1 .and. domain_id == 1)then
        DO j = jts,jte
         DO k = kts,kte
           DO i = its,ite
		          rhoair_max = rhocgs(i,1,j) ! [g/cm3]
              if (i <= 5 .or. i >= IDE-5 .OR. &
                  & j <= 5 .or. j >= JDE-5)THEN
                  if(ILogNormal_modes_Aerosol == 1)then
                    IF (zcgs(I,K,J).LE.ZMIN) THEN
                       FACTZ = 1.0
                    ELSE
                       FACTZ=EXP(-(zcgs(I,K,J)-ZMIN)/Z0IN)
                    END IF
                    ! ... CCN
	                  KRR = 0
                    DO kr = p_ff8i01,p_ff8i43
                      KRR = KRR + 1
           						if (xland(i,j) == 1)then
           							! chem_new(I,K,J,KR)=FCCNR_CON(KRR)*FACTZ
           							chem_new(I,K,J,KR) = (FCCNR_CON(KRR)/rhoair_max)*rhocgs(i,k,j)
                       else
                        ! chem_new(I,K,J,KR)=FCCNR_MAR(KRR)*FACTZ
           							chem_new(I,K,J,KR) = (FCCNR_MAR(KRR)/rhoair_max)*rhocgs(i,k,j)
           						endif
                   END DO
	               endif
              end if
           end do
         end do
       end do
     end if
#endif

     if (itimestep == 1)then
        DO j = j_start,j_end
           DO k = kts,kte
             DO i = i_start,i_end
                th_old(i,k,j)=th_phy(i,k,j)
                qv_old(i,k,j)=qv(i,k,j)
              END DO
     	     END DO
        END DO
     end if

     DO j = j_start,j_end
        DO k = kts,kte
           DO i = i_start,i_end
              t_new(i,k,j) = th_phy(i,k,j)*pi_phy(i,k,j)
              !tempc(i,k,j)= t_new(i,k,j)-273.16
              t_old(i,k,j) = th_old(i,k,j)*pi_phy(i,k,j)
           END DO
        END DO
     END DO

     IF(ICEPROCS == 1)THEN
      KZ_Cloud_Base = 0
      DO j = jts,jte
          DO i = its,ite
            K_found = .FALSE.
            DO k = kts,kte

              ES1N = AA1_MY*EXP(-BB1_MY/T_NEW(I,K,J))
              EW1N = QV(I,K,J)*pcgs(I,K,J)/(0.622+0.378*QV(I,K,J))
              SUP_WATER = EW1N/ES1N - 1.0
              if(k.lt.kte)then
                  w_stag_my 	= 50.*(w(i,k,j)+w(i,k+1,j))
                else
                  w_stag_my = 100*w(i,k,j)
              end if
              if(SUP_WATER > 0.0D0 .and. w_stag_my > 0.1*1.0D2 .and. K_found .eqv. .FALSE. .and. K > 2 .and. zcgs(I,K,J) < 3.0*1.0D5)then
                KZ_Cloud_Base(I,J) = K ! K-level index of cloud base
                K_found = .TRUE.
              endif

              IF(K.EQ.KTE)THEN
                DZZ(K)=(zcgs(I,K,J)-zcgs(I,K-1,J))
                ELSE IF(K.EQ.1)THEN
                DZZ(K)=(zcgs(I,K+1,J)-zcgs(I,K,J))
              ELSE
                DZZ(K)=(zcgs(I,K+1,J)-zcgs(I,K-1,J))
              END IF
              ES2N=AA2_MY*EXP(-BB2_MY/T_OLD(I,K,J))
              EW1N=QV_OLD(I,K,J)*pcgs(I,K,J)/(0.622+0.378*QV_OLD(I,K,J))
              SUPICE(K)=EW1N/ES2N-1.
              IF(SUPICE(K).GT.0.5) SUPICE(K)=.5
            END DO
            DO k = kts,kte
              IF(T_OLD(I,K,J).GE.238.15.AND.T_OLD(I,K,J).LT.274.15) THEN
              if (k.lt.kte)then
                  w_stag=50.*(w(i,k,j)+w(i,k+1,j))
              else
                  w_stag=100*w(i,k,j)
              end if
              IF (I.LT.IDE-1.AND.J.LT.JDE-1)THEN
                UX=25.*(U(I,K,J)+U(I+1,K,J)+U(I,K,J+1)+U(I+1,K,J+1))
                VX=25.*(V(I,K,J)+V(I+1,K,J)+V(I,K,J+1)+V(I+1,K,J+1))
              ELSE
                UX=U(I,K,J)*100.
                VX=V(I,K,J)*100.
              END IF
              IF(K.EQ.1) DERIVT_Z=(T_OLD(I,K+1,J)-T_OLD(I,K,J))/DZZ(K)
              IF(K.EQ.KTE) DERIVT_Z=(T_OLD(I,K,J)-T_OLD(I,K-1,J))/DZZ(K)
              IF(K.GT.1.AND.K.LT.KTE) DERIVT_Z= &
                                    (T_OLD(I,K+1,J)-T_OLD(I,K-1,J))/DZZ(K)
              IF (I.EQ.1)THEN
                DERIVT_X=(T_OLD(I+1,K,J)-T_OLD(I,K,J))/(DXHUCM)
              ELSE IF (I.EQ.IDE-1)THEN
                DERIVT_X=(T_OLD(I,K,J)-T_OLD(I-1,K,J))/(DXHUCM)
              ELSE
                DERIVT_X=(T_OLD(I+1,K,J)-T_OLD(I-1,K,J))/(2.*DXHUCM)
              END IF
              IF (J.EQ.1)THEN
                DERIVT_Y=(T_OLD(I,K,J+1)-T_OLD(I,K,J))/(DYHUCM)
              ELSE IF (J.EQ.JDE-1)THEN
                  DERIVT_Y=(T_OLD(I,K,J)-T_OLD(I,K,J-1))/(DYHUCM)
              ELSE
                  DERIVT_Y=(T_OLD(I,K,J+1)-T_OLD(I,K,J-1))/(2.*DYHUCM)
              END IF
                DTFREEZ_XYZ(I,K,J) = DT*(VX*DERIVT_Y+ &
                                  UX*DERIVT_X+w_stag*DERIVT_Z)
              ELSE ! IF(T_OLD(I,K,J).GE.238.15.AND.T_OLD(I,K,J).LT.274.15)
                DTFREEZ_XYZ(I,K,J)=0.
              ENDIF
              IF(SUPICE(K).GE.0.02.AND.T_OLD(I,K,J).LT.268.15) THEN
                IF (I.LT.IDE-1)THEN
                    ES2NPLSX=AA2_MY*EXP(-BB2_MY/T_OLD(I+1,K,J))
                    EW1NPLSX=QV_OLD(I+1,K,J)*pcgs(I+1,K,J)/ &
                              (0.622+0.378*QV_OLD(I+1,K,J))
                ELSE
                    ES2NPLSX = AA2_MY*EXP(-BB2_MY/T_OLD(I,K,J))
                    EW1NPLSX = QV_OLD(I,K,J)*pcgs(I,K,J)/ &
                              (0.622+0.378*QV_OLD(I,K,J))
                END IF
                IF (ES2NPLSX.EQ.0)THEN
                  DEL2INPLSX=0.5
                ELSE
                  DEL2INPLSX=EW1NPLSX/ES2NPLSX-1.
                END IF
                IF(DEL2INPLSX.GT.0.5) DEL2INPLSX=.5
                IF (I.GT.1)THEN
                  ES2N=AA2_MY*EXP(-BB2_MY/T_OLD(I-1,K,J))
                  EW1N=QV_OLD(I-1,K,J)*pcgs(I-1,K,J)/(0.622+0.378*QV_OLD(I-1,K,J))
                ELSE
                  ES2N=AA2_MY*EXP(-BB2_MY/T_OLD(I,K,J))
                  EW1N=QV_OLD(I,K,J)*pcgs(I,K,J)/(0.622+0.378*QV_OLD(I,K,J))
                END IF
                DEL2IN=EW1N/ES2N-1.
                IF(DEL2IN.GT.0.5) DEL2IN=.5
                IF (I.GT.1.AND.I.LT.IDE-1)THEN
                    DERIVS_X=(DEL2INPLSX-DEL2IN)/(2.*DXHUCM)
                ELSE
                    DERIVS_X=(DEL2INPLSX-DEL2IN)/(DXHUCM)
                END IF
                IF (J.LT.JDE-1)THEN
                  ES2NPLSY=AA2_MY*EXP(-BB2_MY/T_OLD(I,K,J+1))
                  EW1NPLSY=QV_OLD(I,K,J+1)*pcgs(I,K,J+1)/(0.622+0.378*QV_OLD(I,K,J+1))
                ELSE
                  ES2NPLSY=AA2_MY*EXP(-BB2_MY/T_OLD(I,K,J))
                  EW1NPLSY=QV_OLD(I,K,J)*pcgs(I,K,J)/(0.622+0.378*QV_OLD(I,K,J))
                END IF
                DEL2INPLSY=EW1NPLSY/ES2NPLSY-1.
                IF(DEL2INPLSY.GT.0.5) DEL2INPLSY=.5
                IF (J.GT.1)THEN
                  ES2N=AA2_MY*EXP(-BB2_MY/T_OLD(I,K,J-1))
                  EW1N=QV_OLD(I,K,J-1)*pcgs(I,K,J-1)/(0.622+0.378*QV_OLD(I,K,J-1))
                ELSE
                  ES2N=AA2_MY*EXP(-BB2_MY/T_OLD(I,K,J))
                  EW1N=QV_OLD(I,K,J)*pcgs(I,K,J)/(0.622+0.378*QV_OLD(I,K,J))
                END IF
                DEL2IN=EW1N/ES2N-1.
                IF(DEL2IN.GT.0.5) DEL2IN=.5
                IF (J.GT.1.AND.J.LT.JDE-1)THEN
                    DERIVS_Y=(DEL2INPLSY-DEL2IN)/(2.*DYHUCM)
                ELSE
                    DERIVS_Y=(DEL2INPLSY-DEL2IN)/(DYHUCM)
                END IF
                IF (K.EQ.1)DERIVS_Z=(SUPICE(K+1)-SUPICE(K))/DZZ(K)
                IF (K.EQ.KTE)DERIVS_Z=(SUPICE(K)-SUPICE(K-1))/DZZ(K)
                IF(K.GT.1.and.K.LT.KTE) DERIVS_Z=(SUPICE(K+1)-SUPICE(K-1))/DZZ(K)
                IF (I.LT.IDE-1.AND.J.LT.JDE-1)THEN
                UX=25.*(U(I,K,J)+U(I+1,K,J)+U(I,K,J+1)+U(I+1,K,J+1))
                VX=25.*(V(I,K,J)+V(I+1,K,J)+V(I,K,J+1)+V(I+1,K,J+1))
              ELSE
                UX=U(I,K,J)*100.
                VX=V(I,K,J)*100.
              END IF
              DSUPICE_XYZ(I,K,J)=(UX*DERIVS_X+VX*DERIVS_Y+ &
                                  w_stag*DERIVS_Z)*DTCOND
              ELSE
                DSUPICE_XYZ(I,K,J)=0.0
              END IF
              END DO
            END DO
          END DO
        ENDIF

       do j = jts,jte
          do k = kts,kte
             do i = its,ite

            ! ... correcting Look-up-table Terminal velocities
            FACTOR_P = DSQRT(1.0D6/PCGS(I,K,J))
            VR2_ZC(1:nkr,K) = VR2(1:nkr,1)*FACTOR_P
            VR2_ZP(1:nkr,K) = VR2(1:nkr,2)*FACTOR_P
            VR2_ZD(1:nkr,K) = VR2(1:nkr,3)*FACTOR_P
            VR1_Z(1:nkr,K) =  VR1(1:nkr)*FACTOR_P
            VR3_Z(1:nkr,K) = VR3(1:nkr)*FACTOR_P
            VR4_Z(1:nkr,K) = VR4(1:nkr)*FACTOR_P
            VR5_Z(1:nkr,k) = VR5(1:nkr)*FACTOR_P
            VR1_Z3D(1:nkr,I,K,J) = VR1(1:nkr)*FACTOR_P
            VR3_Z3D(1:nkr,I,K,J) = VR3(1:nkr)*FACTOR_P
            VR4_Z3D(1:nkr,I,K,J) = VR4(1:nkr)*FACTOR_P
            VR5_Z3D(1:nkr,I,K,J) = VR5(1:nkr)*FACTOR_P

     			! ... Liquid
     			  KRR = 0
     			  DO kr = p_ff1i01,p_ff1i33
     				 KRR = KRR + 1
     				 FF1R(KRR) = chem_new(I,K,J,KR)
     				 IF (FF1R(KRR) < 0.0)FF1R(KRR) = 0.0
     			  END DO
     			! ... CCN
     			  KRR = 0
     			  DO kr=p_ff8i01,p_ff8i43
     				 KRR = KRR + 1
     				 FCCN(KRR) = chem_new(I,K,J,KR)
     				 if (fccn(krr) < 0.0)fccn(krr) = 0.0
     			  END DO

   				! no explicit Ice Crystals in FSBM
   				 FF2R(:,1) = 0.0
   				 FF2R(:,2) = 0.0
   				 FF2R(:,3) = 0.0

   				! ... Snow
   				KRR=0
   				DO kr=p_ff5i01,p_ff5i33
   					KRR=KRR+1
   					FF3R(KRR)=chem_new(I,K,J,KR)
   					if (ff3r(krr) < 0.0)ff3r(krr) = 0.0
   				END DO

          ! ... Hail or Graupel
          if(hail_opt == 1)then
           KRR=0
           DO kr=p_ff6i01,p_ff6i33
               KRR=KRR+1
               FF5R(KRR) = chem_new(I,K,J,KR)
               if (ff5r(krr) < 0.0)ff5r(krr) = 0.0
               FF4R(KRR) = 0.0
           ENDDO
          else
           KRR=0
           DO kr=p_ff6i01,p_ff6i33
               KRR=KRR+1
               FF4R(KRR) = chem_new(I,K,J,KR)
               if (ff4r(krr) < 0.0)ff4r(krr) = 0.0
               FF5R(KRR) = 0.0
           ENDDO
          endif

! +---------------------------------------------+
! Neucliation, Condensation, Collisions
! +---------------------------------------------+
          IF (T_OLD(I,K,J).GT.193.15)THEN
             TT=T_OLD(I,K,J)
             QQ=QV_OLD(I,K,J)
             IF(QQ.LE.0.0) QQ = 1.D-10
             PP=pcgs(I,K,J)
             TTA=T_NEW(I,K,J)
             QQA=QV(I,K,J)

             IF (QQA.LE.0) call wrf_message("WARNING: FAST SBM, QQA < 0")
             IF (QQA.LE.0) print*,'I,J,K,Told,Tnew,QQA = ',I,J,K,TT,TTA,QQA
             IF (QQA.LE.0) QQA = 1.0D-10

              ES1N = AA1_MY*DEXP(-BB1_MY/TT)
              ES2N = AA2_MY*DEXP(-BB2_MY/TT)
              EW1N=QQ*PP/(0.622+0.378*QQ)
              DIV1=EW1N/ES1N
              DEL1IN=EW1N/ES1N-1.
              DIV2=EW1N/ES2N
              DEL2IN=EW1N/ES2N-1.
              ES1N=AA1_MY*DEXP(-BB1_MY/TTA)
              ES2N=AA2_MY*DEXP(-BB2_MY/TTA)
              EW1N=QQA*PP/(0.622+0.378*QQA)
              DIV3=EW1N/ES1N
              DEL1AD=EW1N/ES1N-1.
              DIV4=EW1N/ES2N
              DEL2AD=EW1N/ES2N-1.
              SUP2_OLD=DEL2IN

              IF(del1ad > 0.0 .or. del2ad > 0.0 .or. (sum(FF1R)+sum(FF3R)+sum(FF4R)+sum(FF5R)) > 1.0e-20)THEN
                ! JacobS: commented for this version                
                !                CALL Relaxation_Time(TT,QQ,PP,rhocgs(I,K,J),DEL1IN,DEL2IN, &
                !                                      XL,VR1_Z(:,K),FF1R,RLEC,RO1BL, &
                !                                      XI,VR2_Z,FF2R,RIEC,RO2BL, &
                !                                      XS,VR3_Z(:,K),FF3R,RSEC,RO3BL, &
                !                                      XG,VR4_Z(:,K),FF4R,RGEC,RO4BL, &
                !                                      XH,VR5_Z(:,k),FF5R,RHEC,RO5BL, &
                !                                      NKR,ICEMAX,COL,DT,NCOND,DTCOND)
                  DELSUP1=(DEL1AD-DEL1IN)/NCOND
                  DELSUP2=(DEL2AD-DEL2IN)/NCOND
                  DELDIV1=(DIV3-DIV1)/NCOND
                  DELDIV2=(DIV4-DIV2)/NCOND
                  DELTATEMP = 0
                  DELTAQ = 0
                  tt_old = TT
                  qq_old = qq
                  DIFFU=1

                  IF (DIV1.EQ.DIV3) DIFFU = 0
                  IF (DIV2.EQ.DIV4) DIFFU = 0

                  DTNEW = 0.0
                  DO IKL=1,NCOND
                    DTCOND = min(DT-DTNEW,DTCOND)
                    DTNEW = DTNEW + DTCOND

                    IF (DIFFU.NE.0)THEN
                      IF (DIFFU.NE.0)THEN
                          DEL1IN = DEL1IN+DELSUP1
                          DEL2IN = DEL2IN+DELSUP2
                          DIV1 = DIV1+DELDIV1
                          DIV2 = DIV2+DELDIV2
                      END IF
                      IF (DIV1.GT.DIV2.AND.TT.LE.265)THEN
                        DIFFU=0
                      END IF
                      IF (DIFFU == 1)THEN
                        DEL1NR=A1_MYN*(100.*DIV1)
                        DEL2NR=A2_MYN*(100.*DIV2)
                        IF (DEL2NR.EQ.0)print*,'ikl = ',ikl
                        IF (DEL2NR.EQ.0)print*,'div1,div2 = ',div1,div2
                        IF (DEL2NR.EQ.0)print*,'i,j,k = ',i,j,k
                        IF (DEL2NR.EQ.0)call wrf_error_fatal("fatal error in module_mp_fast_sbm (DEL2NR.EQ.0) , model stop ")
                        DEL12R=DEL1NR/DEL2NR
                        DEL12RD=DEL12R**DEL_BBR
                        EW1PN=AA1_MY*100.*DIV1*DEL12RD/100.
                        TT=-DEL_BB/DLOG(DEL12R)
                        QQ=0.622*EW1PN/(PP-0.378*EW1PN)

                        IF(DEL1IN > 0.0 .OR. DEL2IN > 0.0)THEN
! +------------------------------------------+
! Droplet nucleation :
! +------------------------------------------+
                          DO KR=1,NKR
                            FF1IN(KR)=FF1R(KR)
                              DO ICE=1,ICEMAX
                                FF2IN(KR,ICE)=FF2R(KR,ICE)
                              ENDDO
                            ENDDO
                          Is_This_CloudBase = 0
                          IF(KZ_Cloud_Base(I,J) == K .and. col*sum(FF1IN*XL) < 5.0) Is_This_CloudBase = 1
                          if (k.lt.kte)then
                            w_stag_my 	= 50.*(w(i,k,j)+w(i,k+1,j))
                          else
                            w_stag_my = 100*w(i,k,j)
                          end if
                          CALL JERNUCL01_KS(FF1IN,FF2IN,FCCN 		  &
                                            ,XL,XI,TT,QQ       					    &
                                            ,rhocgs(I,K,J),pcgs(I,K,J) 			&
                                            ,DEL1IN,DEL2IN     			        &
                                            ,COL 								            &
                                            ,SUP2_OLD,DSUPICE_XYZ(I,K,J) 		&
                                            ,RCCN,DROPRADII,NKR,NKR_aerosol,ICEMAX,ICEPROCS &
                                            ,W_Stag_My,Is_This_CloudBase,RO_SOLUTE,IONS,MWAERO &
                                            ,I,J,K)
                        
                        
                          DO KR=1,NKR
                            FF1R(KR)=FF1IN(KR)
                            DO ICE=1,ICEMAX
                              FF3R(KR) = FF3R(KR) + FF2IN(KR,ICE)
                              FF2IN(KR,ICE) = 0.0
                              FF2R(KR,ICE) = 0.0
                            END DO
                          END DO
                        END IF

                        FMAX1=0.
                        FMAX2=0.
                        FMAX3=0.
                        FMAX4=0.
                        FMAX5=0.
                        DO KR=1,NKR
                          FF1IN(KR)=FF1R(KR)
                          FMAX1=AMAX1(FF1R(KR),FMAX1)
                          FF3IN(KR)=FF3R(KR)
                          FMAX3=AMAX1(FF3R(KR),FMAX3)
                          FF4IN(KR)=FF4R(KR)
                          FMAX4=AMAX1(FF4R(KR),FMAX4)
                          FF5IN(KR)=FF5R(KR)
                          FMAX5=AMAX1(FF5R(KR),FMAX5)
                          DO ICE=1,ICEMAX
                            FF2IN(KR,ICE)=FF2R(KR,ICE)
                            FMAX2(ICE)=AMAX1(FF2R(KR,ICE),FMAX2(ICE)) ! ### (KS) FMAX2(3)
                          END DO
                        END DO
                      ISYM1=0
                      ISYM2=0
                      ISYM3=0
                      ISYM4=0
                      ISYM5=0
                      IF(FMAX1 > 0)ISYM1 = 1
                      IF (ICEPROCS == 1)THEN
                        IF(FMAX2(1) > 1.E-10)ISYM2(1) = 1
                        IF(FMAX2(2) > 1.E-10)ISYM2(2) = 1
                        IF(FMAX2(3) > 1.E-10)ISYM2(3) = 1
                        IF(FMAX3 > 1.E-10)ISYM3 = 1
                        IF(FMAX4 > 1.E-10)ISYM4 = 1
                        IF(FMAX5 > 1.E-10)ISYM5 = 1
                      END IF

                      IF(ISYM1==1 .AND. ((TT-273.15)>-0.187 .OR.(sum(ISYM2)==0 .AND. &
                          ISYM3==0 .AND. ISYM4==0 .AND. ISYM5==0)))THEN

                          ! ... only warm phase
                          CALL ONECOND1(TT,QQ,PP,rhocgs(I,K,J) &
                                        ,VR1_Z(:,K),pcgs(I,K,J) &
                                        ,DEL1IN,DEL2IN,DIV1,DIV2 &
                                        ,FF1R,FF1IN,XL,RLEC,RO1BL &
                                        ,AA1_MY,BB1_MY,AA2_MY,BB2_MY &
                                        ,C1_MEY,C2_MEY &
                                        ,COL,DTCOND,ICEMAX,NKR,ISYM1 &
                                        ,ISYM2,ISYM3,ISYM4,ISYM5,I,J,K,W(i,k,j),DX,Itimestep)

                        ELSE IF(ISYM1==0 .AND. (TT-273.15)<-0.187 .AND. &
                                (sum(ISYM2)>1 .OR. ISYM3==1 .OR. ISYM4==1 .OR. ISYM5==1))THEN
                                  !IF (TT.GT.213.15)THEN
                                    VR2_Z(:,1) = VR2_ZC(:,K)
                                    VR2_Z(:,2) = VR2_ZP(:,K)
                                    VR2_Z(:,3) = VR2_ZD(:,K)
                                    CALL ONECOND2(TT,QQ,PP,rhocgs(I,K,J) &
                                    ,VR2_Z,VR3_Z(:,K),VR4_Z(:,K),VR5_Z(:,K),pcgs(I,K,J) &
                                    ,DEL1IN,DEL2IN,DIV1,DIV2 &
                                    ,FF2R,FF2IN,XI,RIEC,RO2BL &
                                    ,FF3R,FF3IN,XS,RSEC,RO3BL &
                                    ,FF4R,FF4IN,XG,RGEC,RO4BL &
                                    ,FF5R,FF5IN,XH,RHEC,RO5BL &
                                    ,AA1_MY,BB1_MY,AA2_MY,BB2_MY &
                                    ,C1_MEY,C2_MEY &
                                    ,COL,DTCOND,ICEMAX,NKR &
                                    ,ISYM1,ISYM2,ISYM3,ISYM4,ISYM5,I,J,K,W(i,k,j),DX,Itimestep)
                                !END IF
                          ELSE IF(ISYM1==1 .AND. (TT-273.15)<-0.187 .AND. &
                              (sum(ISYM2)>1 .OR. ISYM3==1 .OR. ISYM4==1 .OR. ISYM5==1))THEN
                              IF (TT > 233.15)THEN
                                VR2_Z(:,1) = VR2_ZC(:,K)
                                VR2_Z(:,2) = VR2_ZP(:,K)
                                VR2_Z(:,3) = VR2_ZD(:,K)
                                CALL ONECOND3(TT,QQ,PP,rhocgs(I,K,J) &
                                ,VR1_Z(:,K),VR2_Z,VR3_Z(:,K),VR4_Z(:,K),VR5_Z(:,K),pcgs(I,K,J) &
                                ,DEL1IN,DEL2IN,DIV1,DIV2 &
                                ,FF1R,FF1IN,XL,RLEC,RO1BL &
                                ,FF2R,FF2IN,XI,RIEC,RO2BL &
                                ,FF3R,FF3IN,XS,RSEC,RO3BL &
                                ,FF4R,FF4IN,XG,RGEC,RO4BL &
                                ,FF5R,FF5IN,XH,RHEC,RO5BL &
                                ,AA1_MY,BB1_MY,AA2_MY,BB2_MY &
                                ,C1_MEY,C2_MEY &
                                ,COL,DTCOND,ICEMAX,NKR &
                                ,ISYM1,ISYM2,ISYM3,ISYM4,ISYM5,I,J,K,W(i,k,j),DX,Itimestep)
                              ENDIF
                            END IF
                          END IF ! DIFF.NE.0
                      END IF ! DIFFU.NE.0
                  END DO ! NCOND - end of NCOND loop
! +----------------------------------+
! Collision-Coallescnce
! +----------------------------------+
                  DO IKL = 1,NCOLL
                    IF ( TT >= 233.15 ) THEN
                      FLIQFR_SD = 0.0
                      FLIQFR_GD = 0.0
                      FLIQFR_HD = 0.0
                      FRIMFR_SD = 0.0
                      CALL COAL_BOTT_NEW (FF1R,FF2R,FF3R,     				    &
                                FF4R,FF5R,TT,QQ,PP, 					            &
                                rhocgs(I,K,J),dt_coll,TCRIT,TTCOAL, 	    &
                                FLIQFR_SD,FLIQFR_GD,FLIQFR_HD,FRIMFR_SD,  &
                                DEL1IN, DEL2IN, 			        	          &
                                I,J,K,Itimestep,CollEff_out)

                    END IF
                  END DO ! NCOLL - end of NCOLL loop
                                   
                  T_new(i,k,j) = tt
                  qv(i,k,j) = qq
                   
            ! in case Sw,Si,mass  
            ENDIF 
        ! in case T_OLD(I,K,J).GT.213.15
        END IF
 ! +-------------------------------- +
 ! Immediate Freezing
 ! +---------------------------------+
 				IF(T_NEW(i,k,j) < 273.15 .and. ICEPROCS == 1)THEN
 					CALL FREEZ &
 							(FF1R,XL,FF2R,XI,FF3R,XS,FF4R,XG,FF5R,XH, &
 							 T_NEW(I,K,J),DT,rhocgs(I,K,J), 			  &
 							 COL,AFREEZMY,BFREEZMY,BFREEZMAX, 		  &
 							 KRFREEZ,ICEMAX,NKR)
 				ENDIF
! --------------------------------------------------------------+
! Jiwen Fan Melting (melting along a constant time scale)
! --------------------------------------------------------------+
		    IF (JIWEN_FAN_MELT == 1 .and. T_NEW(i,k,j) > 273.15 .and. ICEPROCS == 1) THEN
				   CALL J_W_MELT(FF1R,XL,FF2R,XI,FF3R,XS,FF4R,XG,FF5R,XH, &
						             T_NEW(I,K,J),DT,rhocgs(I,K,J),COL,ICEMAX,NKR)
        END IF

        DO KR=1,NKR
          DO ICE=1,ICEMAX
            FF3R(KR)=FF3R(KR) + FF2R(KR,ICE)
            FF2R(KR,ICE) = 0.0
          END DO
          if(hail_opt == 1)then
            FF5R(KR) = FF5R(KR) + FF4R(KR)
            FF4R(KR) = 0.0
          else
            FF4R(KR) = FF4R(KR) + FF5R(KR)
            FF5R(KR) = 0.0
          endif
        END DO

 ! +---------------------------+
 ! Spontanaous Rain Breakup
! +----------------------------+
 			IF (Spont_Rain_BreakUp_On == 1 .AND. (SUM(FF1R) > 43.0*1.0D-30) )THEN
 					FF1R_D(:) = FF1R(:)
 					XL_D(:) = XL(:)
 					CALL Spont_Rain_BreakUp (DT ,FF1R_D, XL_D, Prob, Gain_Var_New, NND, NKR, ikr_spon_break)
		      FF1R(:) = FF1R_D(:)
 			END IF

 ! -----------------------------------------------------------+
 ! ... Snow BreakUp
 ! -----------------------------------------------------------+
 		IF (Snow_BreakUp_On == 1 .and. ICEPROCS == 1 .and. sum(FF3R(KR_SNOW_MIN:NKR))> (NKR-KR_SNOW_MIN)*1.0D-30)THEN

 			DO KR=1,NKR
 				FF3R_D(KR) = FF3R(KR)
 			END DO
			IF (KR_SNOW_MAX <= NKR) CALL BreakUp_Snow (TT_r,FF3R_D,FLIQFR_SD,xs_d,FRIMFR_SD,NKR)
 					DO KR=1,NKR
 						FF3R(KR) = FF3R_D(KR)
 					END DO
 		END IF

    ! Update temperature at the end of MP
  	th_phy(i,k,j) = t_new(i,k,j)/pi_phy(i,k,j)

    ! ... Drops
	  KRR = 0
	  DO kr = p_ff1i01,p_ff1i33
		 KRR = KRR+1
		 chem_new(I,K,J,KR) = FF1R(KRR)
	  END DO
	  ! ... CCN
	  KRR = 0
	  DO kr=p_ff8i01,p_ff8i43
		  KRR=KRR+1
		  chem_new(I,K,J,KR)=FCCN(KRR)
	  END DO
	  IF (ICEPROCS == 1)THEN
	  ! ... Snow
		  KRR = 0
		  DO kr=p_ff5i01,p_ff5i33
			  KRR=KRR+1
			  chem_new(I,K,J,KR)=FF3R(KRR)
		  END DO
     ! ... Hail/ Graupel
      if(hail_opt == 1)then
       KRR = 0
       DO KR=p_ff6i01,p_ff6i33
           KRR=KRR+1
           chem_new(I,K,J,KR) = FF5R(KRR)
       END DO
      else
       KRR = 0
       DO KR=p_ff6i01,p_ff6i33
           KRR=KRR+1
           chem_new(I,K,J,KR) = FF4R(KRR)
       END DO
      endif
      ! ICEPROCS == 1
 		   END IF

       END DO
      END DO
     END DO

! +-----------------------------+
! Hydrometeor Sedimentation
! +-----------------------------+
       do j = jts,jte
       	do i = its,ite
 ! ... Drops ...
 			    do k = kts,kte
            rhocgs_z(k)=rhocgs(i,k,j)
            pcgs_z(k)=pcgs(i,k,j)
            zcgs_z(k)=zcgs(i,k,j)
            vrx(k,:)=vr1_z3D(:,i,k,j)
            krr=0
            do kr=p_ff1i01,p_ff1i33
              krr=krr+1
              ffx_z(k,krr)=chem_new(i,k,j,kr)/rhocgs(i,k,j)
            end do
          end do
          call FALFLUXHUCM_Z(ffx_z,VRX,RHOCGS_z,PCGS_z,ZCGS_z,DT,kts,kte,nkr)
          do k = kts,kte
            krr=0
            do kr=p_ff1i01,p_ff1i33
              krr=krr+1
              chem_new(i,k,j,kr)=ffx_z(k,krr)*rhocgs(i,k,j)
            end do
          end do
       		if(iceprocs == 1)then
 ! ... Snow ...
          do k = kts,kte
            rhocgs_z(k)=rhocgs(i,k,j)
            pcgs_z(k)=pcgs(i,k,j)
            zcgs_z(k)=zcgs(i,k,j)
            vrx(k,:)=vr3_z3D(:,i,k,j)
            krr=0
            do kr=p_ff5i01,p_ff5i33
              krr=krr+1
              ffx_z(k,krr)=chem_new(i,k,j,kr)/rhocgs(i,k,j)
            end do
          end do
          call FALFLUXHUCM_Z(ffx_z,VRX,RHOCGS_z,PCGS_z,ZCGS_z,DT,kts,kte,nkr)
          do k = kts,kte
            krr=0
            do kr=p_ff5i01,p_ff5i33
              krr=krr+1
              chem_new(i,k,j,kr)=ffx_z(k,krr)*rhocgs(i,k,j)
            end do
          end do
 ! ... Hail or Graupel ...
          do k = kts,kte
            rhocgs_z(k)=rhocgs(i,k,j)
            pcgs_z(k)=pcgs(i,k,j)
            zcgs_z(k)=zcgs(i,k,j)
            if(hail_opt == 1)then
              vrx(k,:) = vr5_z3D(:,i,k,j)
            else
              vrx(k,:) = vr4_z3D(:,i,k,j)
            endif
            krr=0
            do kr=p_ff6i01,p_ff6i33
              krr=krr+1
              ffx_z(k,krr)=chem_new(i,k,j,kr)/rhocgs(i,k,j)
            end do
          end do
          call FALFLUXHUCM_Z(ffx_z,VRX,RHOCGS_z,PCGS_z,ZCGS_z,DT,kts,kte,nkr)
          do k = kts,kte
            krr=0
            do kr=p_ff6i01,p_ff6i33
              krr=krr+1
              chem_new(i,k,j,kr)=ffx_z(k,krr)*rhocgs(i,k,j)
            end do
          end do
 			end if ! if (iceprocs == 1)
   	end do
   end do

    gmax=0
    qmax=0
    imax=0
    kmax=0
    qnmax=0
    inmax=0
    knmax=0
    DO j = jts,jte
      DO k = kts,kte
        DO i = its,ite
          QC(I,K,J) = 0.0
          QR(I,K,J) = 0.0
          QI(I,K,J) = 0.0
          QS(I,K,J) = 0.0
          QG(I,K,J) = 0.0
          QNC(I,K,J) = 0.0
          QNR(I,K,J) = 0.0
          QNI(I,K,J) = 0.0
          QNS(I,K,J) = 0.0
          QNG(I,K,J) = 0.0
          QNA(I,K,J) = 0.0

          tt = th_phy(i,k,j)*pi_phy(i,k,j)

          ! ... Drop output
          KRR = 0
          DO KR = p_ff1i01,p_ff1i33
            KRR=KRR+1
            IF (KRR < KRDROP)THEN
              QC(I,K,J) = QC(I,K,J) &
              + (1./RHOCGS(I,K,J))*COL*chem_new(I,K,J,KR)*XL(KRR)*XL(KRR)*3
              QNC(I,K,J) = QNC(I,K,J) &
              + COL*chem_new(I,K,J,KR)*XL(KRR)*3.0/rhocgs(I,K,J)*1000.0 ! #/kg
            ELSE
              QR(I,K,J) = QR(I,K,J) &
              + (1./RHOCGS(I,K,J))*COL*chem_new(I,K,J,KR)*XL(KRR)*XL(KRR)*3.0
              QNR(I,K,J) = QNR(I,K,J) &
              + COL*chem_new(I,K,J,KR)*XL(KRR)*3/rhocgs(I,K,J)*1000.0 ! #/kg
            END IF
          END DO

 			  KRR=0
 			  IF (ICEPROCS == 1)THEN
 			  ! ... Snow output
 			   	KRR=0
 			   	DO  KR=p_ff5i01,p_ff5i33
 					KRR=KRR+1
 					 if (KRR <= KRICE)THEN
 						 QI(I,K,J) = QI(I,K,J) &
 					                 +(1.0/RHOCGS(I,K,J))*COL*chem_new(I,K,J,KR)*XS(KRR)*XS(KRR)*3.0
 						 QNI(I,K,J) = QNI(I,K,J) &
         				  			  + COL*chem_new(I,K,J,KR)*XS(KRR)*3.0/rhocgs(I,K,J)*1000.0 ! #/kg
 					 else
 						 QS(I,K,J) = QS(I,K,J) &
 									+ (1.0/RHOCGS(I,K,J))*COL*chem_new(I,K,J,KR)*XS(KRR)*XS(KRR)*3.0
 						 QNS(I,K,J) = QNS(I,K,J) &
 								 	+ COL*chem_new(I,K,J,KR)*XS(KRR)*3.0/rhocgs(I,K,J)*1000.0 ! #/kg
 					endif
 			   END DO

 			   ! ... Hail / Graupel output
          KRR=0
          DO  KR=p_ff6i01,p_ff6i33
            KRR=KRR+1
            ! ... Hail or Graupel
            if(hail_opt == 1)then
              QG(I,K,J)=QG(I,K,J) &
              +(1.0/RHOCGS(I,K,J))*COL*chem_new(I,K,J,KR)*XH(KRR)*XH(KRR)*3.0
              QNG(I,K,J)=QNG(I,K,J) &
              +COL*chem_new(I,K,J,KR)*XH(KRR)*3.0/rhocgs(I,K,J)*1000.0 ! #/kg
            else
              QG(I,K,J)=QG(I,K,J) &
              +(1.0/RHOCGS(I,K,J))*COL*chem_new(I,K,J,KR)*XG(KRR)*XG(KRR)*3.0
              QNG(I,K,J)=QNG(I,K,J) &
              +COL*chem_new(I,K,J,KR)*XG(KRR)*3.0/rhocgs(I,K,J)*1000.0 ! #/kg
            endif
          END DO
   		END IF !IF (ICEPROCS.EQ.1)THEN

      KRR = 0
      DO  KR = p_ff8i01,p_ff8i43
      	KRR = KRR + 1
      	QNA(I,K,J) = QNA(I,K,J) &
      			   	+ COL*chem_new(I,K,J,KR)/rhocgs(I,K,J)*1000.0   ! #/kg
      END DO

 		END DO
   END DO
  END DO

 998   format(' ',10(f10.1,1x))

  DO j = jts,jte
    DO i = its,ite
      RAINNCV(I,J) = 0.0
      SNOWNCV(I,J) = 0.0
      GRAUPELNCV(I,J) = 0.0
      krr=0
      DO KR=p_ff1i01,p_ff1i33
        krr=krr+1
        DELTAW = VR1_Z(KRR,1)
        RAINNC(I,J) = RAINNC(I,J) &
          +10.0*(3./RO1BL(KRR))*COL*DT*DELTAW* &
          chem_new(I,1,J,KR)*XL(KRR)*XL(KRR)
        RAINNCV(I,J) = RAINNCV(I,J) &
          +10.0*(3./RO1BL(KRR))*COL*DT*DELTAW* &
          chem_new(I,1,J,KR)*XL(KRR)*XL(KRR)
      END DO
      KRR=0
      DO KR=p_ff5i01,p_ff5i33
        KRR=KRR+1
        DELTAW = VR3_Z(KRR,1)
        RAINNC(I,J)=RAINNC(I,J) &
          +10.0*(3./RO1BL(KRR))*COL*DT*DELTAW* &
          chem_new(I,1,J,KR)*XS(KRR)*XS(KRR)
        RAINNCV(I,J)=RAINNCV(I,J) &
          +10.0*(3./RO1BL(KRR))*COL*DT*DELTAW* &
          chem_new(I,1,J,KR)*XS(KRR)*XS(KRR)
        SNOWNC(I,J) = SNOWNC(I,J) &
        + 10*(3./RO1BL(KRR))*COL*DT*DELTAW* &
        chem_new(I,1,J,KR)*XS(KRR)*XS(KRR)
       SNOWNCV(I,J) = SNOWNCV(I,J) &
       + 10*(3./RO1BL(KRR))*COL*DT*DELTAW* &
       chem_new(I,1,J,KR)*XS(KRR)*XS(KRR)
     END DO
     KRR=0
     DO KR=p_ff6i01,p_ff6i33
       KRR=KRR+1
       if(hail_opt == 1)then
         DELTAW = VR5_Z(KRR,1)
         RAINNC(I,J) = RAINNC(I,J) &
         +10.0*(3./RO1BL(KRR))*COL*DT*DELTAW* &
         chem_new(I,1,J,KR)*XH(KRR)*XH(KRR)
       RAINNCV(I,J) = RAINNCV(I,J) &
         +10.0*(3./RO1BL(KRR))*COL*DT*DELTAW* &
         chem_new(I,1,J,KR)*XH(KRR)*XH(KRR)
       GRAUPELNC(I,J) = GRAUPELNC(I,J) &
       + 10*(3./RO1BL(KRR))*COL*DT*DELTAW* &
       chem_new(I,1,J,KR)*XH(KRR)*XH(KRR)
     GRAUPELNCV(I,J) = GRAUPELNCV(I,J) &
     + 10*(3./RO1BL(KRR))*COL*DT*DELTAW* &
     chem_new(I,1,J,KR)*XH(KRR)*XH(KRR)
   else
     DELTAW = VR4_Z(KRR,1)
     RAINNC(I,J) = RAINNC(I,J) &
      +10.0*(3./RO1BL(KRR))*COL*DT*DELTAW* &
      chem_new(I,1,J,KR)*XG(KRR)*XG(KRR)
    RAINNCV(I,J) = RAINNCV(I,J) &
      +10.0*(3./RO1BL(KRR))*COL*DT*DELTAW* &
      chem_new(I,1,J,KR)*XG(KRR)*XG(KRR)
    GRAUPELNC(I,J) = GRAUPELNC(I,J) &
      + 10*(3./RO1BL(KRR))*COL*DT*DELTAW* &
      chem_new(I,1,J,KR)*XG(KRR)*XG(KRR)
    GRAUPELNCV(I,J) = GRAUPELNCV(I,J) &
      + 10*(3./RO1BL(KRR))*COL*DT*DELTAW* &
      chem_new(I,1,J,KR)*XG(KRR)*XG(KRR)
  endif
  END DO
! ..........................................
! ... Polarimetric Forward Radar Operator
! ..........................................
  if ( PRESENT (diagflag) ) then
    if( diagflag .and. IPolar_HUCM .and. (sbm_diagnostics==1) ) then

      dx_dbl = dx
      dy_dbl = dy
      do k = kts,kte
      zmks_1d(k) = zcgs(i,k,j)*0.01
      end do
      DIST_SING = ((i-ide/2)**2+(j-jde/2)**2)**(0.5)
      DISTANCE = 1.D5

      do k=kts,kte
        FF2R_d = 0.0 	 	
        FLIQFR_SD = 0.0
        FLIQFR_GD = 0.0
        FLIQFR_HD = 0.0
        FF1_FD = 0.0
        FL1_FD = 0.0
        BKDEN_Snow(:) = RO3BL(:)
        RO2BL_D(:,:) = RO2BL(:,:)
        RO2BL_D(:,:) = RO2BL(:,:)

! ... Drops
        KRR=0
        do kr = p_ff1i01,p_ff1i33 
          KRR=KRR+1
          FF1R_D(KRR) = (1./RHOCGS(I,K,J))*chem_new(I,K,J,KR)*XL(KRR)*XL(KRR)*3.0
          if (FF1R_D(KRR) < 1.0D-20) FF1R_D(KRR) = 0.0
        end do
        if (ICEPROCS == 1)then
! ... SNOW
          KRR=0
          do kr=p_ff5i01,p_ff5i33
            KRR=KRR+1
            FF3R_D(KRR)=(1./RHOCGS(I,K,J))*chem_new(I,K,J,KR)*XS(KRR)*XS(KRR)*3.0
            FF3R (KRR) = chem_new(I,K,J,KR)
            if (ff3r_D(krr) < 1.0D-20) ff3r_D(krr) = 0.0
          end do
! ... Graupel or Hail
          KRR=0
          if(hail_opt == 0)then
            do kr = p_ff6i01,p_ff6i33
              KRR=KRR+1
              FF4R_D(KRR) = (1./RHOCGS(I,K,J))*chem_new(I,K,J,KR)*XG(KRR)*XG(KRR)*3.0
              FF4R(KRR) = chem_new(I,K,J,KR)
              if (FF4R_D(KRR) < 1.0D-20) FF4R_D(KRR)= 0.0
              FF5R_d(KRR) = 0.0
            end do
          else
            do kr=p_ff6i01,p_ff6i33
              KRR=KRR+1
              FF5R_D(KRR)=(1./RHOCGS(I,K,J))*chem_new(I,K,J,KR)*XH(KRR)*XH(KRR)*3.0
              FF5R(KRR)=chem_new(I,K,J,KR)
              if (ff5r_d(krr) < 1.0D-20) ff5r_d(krr)=0.0
              FF4R_d(KRR) = 0.0
            end do
          endif
        ! in caseICEPROCS.EQ.1
        end if

        rhocgs_d = rhocgs(I,K,J)
        T_NEW_D = T_NEW(I,K,J)

        IWL = 1
        ICLOUD = 0

 			  CALL polar_hucm &
                 				(FF1R_D, FF2R_D, FF3R_D, FF4R_D, FF5R_D, FF1_FD, 		    &
                 				FLIQFR_SD, FLIQFR_GD, FLIQFR_HD, FL1_FD, 				        &
                 				BKDEN_Snow, T_NEW_D, rhocgs_D, wavelength, iwl,         &
                 				distance, dx_dbl, dy_dbl, zmks_1d, 					            &
                 				out1, out2, out3, out4, out5, out6, out7, out8, out9,   &
                 				bin_mass, tab_colum, tab_dendr, tab_snow, bin_log, 		  &
                 				ijk, i, j, k, kts, kte, NKR, ICEMAX, icloud, itimestep, &
                 				faf1,fbf1,fab1,fbb1, 									    &
                 				faf3,fbf3,fab3,fbb3,         							&
                 				faf4,fbf4,fab4,fbb4,         							&
                 				faf5,fbf5,fab5,fbb5,         							&
                 				temps_water,temps_fd,temps_crystals,  	  &
                 				temps_snow,temps_graupel,temps_hail,  		&
                 				fws_fd,fws_crystals,fws_snow,		  				&
                 				fws_graupel,fws_hail,usetables)


   			KRR=0
   			DO KR=r_p_ff1i01,r_p_ff1i06
   				KRR=KRR+1
   				sbmradar(I,K,J,KR) = out1(KRR)
   			END DO
   			KRR=0
   			DO KR=r_p_ff2i01,r_p_ff2i06
   				KRR=KRR+1
   				sbmradar(I,K,J,KR)=out2(KRR)
   			END DO
   			KRR=0
   			DO KR=r_p_ff3i01,r_p_ff3i06
   				KRR=KRR+1
   				sbmradar(I,K,J,KR)=out3(KRR)
   			END DO
   			KRR=0
   			DO KR=r_p_ff4i01,r_p_ff4i06
   				KRR=KRR+1
   				sbmradar(I,K,J,KR)=out4(KRR)
   			END DO
   			KRR=0
   			DO KR=r_p_ff5i01,r_p_ff5i06
   				KRR=KRR+1
   				sbmradar(I,K,J,KR)=out5(KRR)
   			END DO
   			KRR=0
   			DO KR=r_p_ff6i01,r_p_ff6i06
   				KRR=KRR+1
   				sbmradar(I,K,J,KR)=out6(KRR)
   			END DO
   			KRR=0
   			DO KR=r_p_ff7i01,r_p_ff7i06
   				KRR=KRR+1
   				sbmradar(I,K,J,KR)=out7(KRR)
   			END DO
   			KRR=0
   			DO KR=r_p_ff8i01,r_p_ff8i06
   				KRR=KRR+1
   				sbmradar(I,K,J,KR)=out8(KRR)
   			END DO
   			KRR=0
   			DO KR=r_p_ff9i01,r_p_ff9i06
   				KRR=KRR+1
   				sbmradar(I,K,J,KR)=out9(KRR)
   			END DO

 		 ! cycle by K
 		 end do
 		! diagflag .and. IPolar_HUCM
 		endif
 	! PRESENT(diagflag)
 	endif

   ! cycle by I
   END DO
 ! cycle by J
 END DO

   do j=jts,jte
   do k=kts,kte
   do i=its,ite
      th_old(i,k,j)=th_phy(i,k,j)
      qv_old(i,k,j)=qv(i,k,j)
   end do
   end do
   end do

   if (conserv)then
 		  DO j = jts,jte
 		     DO i = its,ite
 		        DO k = kts,kte
         		  rhocgs(I,K,J)=rho_phy(I,K,J)*0.001
         		  krr=0
         		  DO KR=p_ff1i01,p_ff1i33
         			    krr=krr+1
         		       chem_new(I,K,J,KR)=chem_new(I,K,J,KR)/RHOCGS(I,K,J)*COL*XL(KRR)*XL(KRR)*3.0
         		        if (qc(i,k,j)+qr(i,k,j).lt.1.e-13)chem_new(I,K,J,KR)=0.0
         		  END DO
         		  KRR=0
         		  DO KR=p_ff5i01,p_ff5i33
         		   KRR=KRR+1
         		   chem_new(I,K,J,KR)=chem_new(I,K,J,KR)/RHOCGS(I,K,J)*COL*XS(KRR)*XS(KRR)*3.0
         		   if (qs(i,k,j).lt.1.e-13)chem_new(I,K,J,KR)=0.0
         		  END DO
         		  ! ... CCN
         		  KRR=0
         		  DO KR=p_ff8i01,p_ff8i43
         		   KRR=KRR+1
         		   chem_new(I,K,J,KR)=chem_new(I,K,J,KR)/RHOCGS(I,K,J)*1000.0
         		  END DO
              ! ... Hail / Graupel
              if(hail_opt == 1)then
                 KRR=0
                 DO KR=p_ff6i01,p_ff6i33
                     KRR=KRR+1
                     chem_new(I,K,J,KR)=chem_new(I,K,J,KR)/RHOCGS(I,K,J)*COL*XH(KRR)*XH(KRR)*3.0
                     if (qg(i,k,j) < 1.e-13) chem_new(I,K,J,KR) = 0.0
                 END DO
               else
                 KRR=0
                 DO KR=p_ff6i01,p_ff6i33
                     KRR=KRR+1
                     chem_new(I,K,J,KR)=chem_new(I,K,J,KR)/RHOCGS(I,K,J)*COL*XG(KRR)*XG(KRR)*3.0
                     if (qg(i,k,j) < 1.e-13) chem_new(I,K,J,KR) = 0.0
                 END DO
               endif

 		          END DO
 		         END DO
 		        END DO
       END IF

   RETURN
   END SUBROUTINE FAST_SBM
 ! +-------------------------------------------------------------+
   SUBROUTINE FALFLUXHUCM_Z(chem_new,VR1,RHOCGS,PCGS,ZCGS,DT, &
   						               kts,kte,nkr)

     IMPLICIT NONE

 	   integer,intent(in) :: kts,kte,nkr
 	   real(kind=r4size),intent(inout) :: chem_new(:,:)
 	   real(kind=r4size),intent(in) :: rhocgs(:),pcgs(:),zcgs(:),VR1(:,:),DT

 	  ! ... Locals
 	  integer :: I,J,K,KR
    real(kind=r4size) :: TFALL,DTFALL,VFALL(KTE),DWFLUX(KTE)
    integer :: IFALL,N,NSUB

 ! FALLING FLUXES FOR EACH KIND OF CLOUD PARTICLES: C.G.S. UNIT
 ! ADAPTED FROM GSFC CODE FOR HUCM
 !  The flux at k=1 is assumed to be the ground so FLUX(1) is the
 ! flux into the ground. DWFLUX(1) is at the lowest half level where
 ! Q(1) etc are defined. The formula for FLUX(1) uses Q(1) etc which
 ! is actually half a grid level above it. This is what is meant by
 ! an upstream method. Upstream in this case is above because the
 ! velocity is downwards.
 ! USE UPSTREAM METHOD (VFALL IS POSITIVE)

       DO KR=1,NKR
        IFALL=0
        DO k = kts,kte
           IF(chem_new(K,KR).GE.1.E-20)IFALL=1
        END DO
        IF (IFALL.EQ.1)THEN
         TFALL=1.E10
         DO K=kts,kte
          ! [KS] VFALL(K) = VR1(K,KR)*SQRT(1.E6/PCGS(K))
 		       VFALL(K) = VR1(K,KR) ! ... [KS] : The pressure effect is taken into account at the beggining of the calculations
           TFALL=AMIN1(TFALL,ZCGS(K)/(VFALL(K)+1.E-20))
         END DO
         IF(TFALL.GE.1.E10)STOP
         NSUB=(INT(2.0*DT/TFALL)+1)
         DTFALL=DT/NSUB

         DO N=1,NSUB
           DO K=KTS,KTE-1
             DWFLUX(K)=-(RHOCGS(K)*VFALL(K)*chem_new(k,kr)- &
             RHOCGS(K+1)* &
             VFALL(K+1)*chem_new(K+1,KR))/(RHOCGS(K)*(ZCGS(K+1)- &
             ZCGS(K)))
           END DO
 ! NO Z ABOVE TOP, SO USE THE SAME DELTAZ
           DWFLUX(KTE)=-(RHOCGS(KTE)*VFALL(KTE)* &
      &                 chem_new(kte,kr))/(RHOCGS(KTE)*(ZCGS(KTE)-ZCGS(KTE-1)))
           DO K=kts,kte
            chem_new(k,kr)=chem_new(k,kr)+DWFLUX(K)*DTFALL
           END DO
         END DO
        END IF
       END DO

       RETURN
       END SUBROUTINE FALFLUXHUCM_Z
 ! +----------------------------------+
   SUBROUTINE FAST_HUCMINIT(DT)

    USE module_mp_SBM_BreakUp,ONLY:Spontanous_Init
 	  USE module_mp_SBM_Collision,ONLY:courant_bott_KS
 	  USE module_domain
 	  USE module_dm

 	  IMPLICIT NONE

    real(kind=r4size),intent(in) :: DT

    LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
    LOGICAL :: opened
    CHARACTER*80 errmess
    integer :: I,J,KR,IType,HUJISBM_UNIT1
    real(kind=r4size) :: dlnr,ax,deg01,CONCCCNIN,CONTCCNIN

 	  character(len=256),parameter :: dir_43 = "SBM_input_43", dir_33 = "SBM_input_33"
 	  character(len=256) :: input_dir,Fname

 	 if(nkr == 33) input_dir = trim(dir_33)
 	 if(nkr == 43) input_dir = trim(dir_43)

     call wrf_message(" FAST SBM: INITIALIZING WRF_HUJISBM ")
     call wrf_message(" FAST SBM: ****** WRF_HUJISBM ******* ")

 ! LookUpTable #1
 ! +-------------------------------------------------------+
 	if (.NOT. ALLOCATED(bin_mass)) ALLOCATE(bin_mass(nkr))
 	if (.NOT. ALLOCATED(tab_colum)) ALLOCATE(tab_colum(nkr))
 	if (.NOT. ALLOCATED(tab_dendr)) ALLOCATE(tab_dendr(nkr))
 	if (.NOT. ALLOCATED(tab_snow)) ALLOCATE(tab_snow(nkr))
 	if (.NOT. ALLOCATED(bin_log)) ALLOCATE(bin_log(nkr))

 	dlnr=dlog(2.d0)/(3.d0)

 	hujisbm_unit1 = -1
 	IF ( wrf_dm_on_monitor() ) THEN
 		DO i = 20,99
 			INQUIRE ( i , OPENED = opened )
 			IF ( .NOT. opened ) THEN
 				hujisbm_unit1 = i
 				GOTO 2060
 			ENDIF
 		ENDDO
 	2060  CONTINUE
 	ENDIF

#if (defined(DM_PARALLEL))
 		CALL wrf_dm_bcast_bytes( hujisbm_unit1 , IWORDSIZE )
#endif
 	IF ( hujisbm_unit1 < 0 ) THEN
     	CALL wrf_error_fatal ( 'module_mp_FAST-SBM: Table-1 -- FAST_SBM_INIT: '// 			&
 							              'Can not find unused fortran unit to read in lookup table, model stop' )
 	ENDIF

 	IF ( wrf_dm_on_monitor() ) THEN
 			WRITE(errmess, '(A,I2)') 'module_mp_FAST-SBM : Table-1 -- opening "BLKD_SDC.dat" on unit',hujisbm_unit1
 			CALL wrf_debug(150, errmess)
 			OPEN(UNIT=hujisbm_unit1,FILE=trim(input_dir)//"/BLKD_SDC.dat",FORM="FORMATTED",STATUS="OLD",ERR=2070)
 			DO kr=1,NKR
 				READ(hujisbm_unit1,*) bin_mass(kr),tab_colum(kr),tab_dendr(kr),tab_snow(kr)
 				bin_log(kr) = log10(bin_mass(kr))
 			ENDDO
 	ENDIF

#define DM_BCAST_MACRO_R4(A) CALL wrf_dm_bcast_bytes(A, size(A)*R4SIZE)
#define DM_BCAST_MACRO_R8(A) CALL wrf_dm_bcast_bytes(A, size(A)*R8SIZE)
#define DM_BCAST_MACRO_R16(A) CALL wrf_dm_bcast_bytes(A, size(A)*R16SIZE)

#if (defined(DM_PARALLEL))
    DM_BCAST_MACRO_R8(bin_mass)
 	  DM_BCAST_MACRO_R8(tab_colum)
 	  DM_BCAST_MACRO_R8(tab_dendr)
 	  DM_BCAST_MACRO_R8(tab_snow)
 	  DM_BCAST_MACRO_R8(bin_log)
#endif

     WRITE(errmess, '(A,I2)') 'FAST_SBM_INIT : succesfull reading Table-1'
     CALL wrf_debug(000, errmess)
 ! +-----------------------------------------------------------------------+

 ! LookUpTable #2
 ! +----------------------------------------------+
     if (.NOT. ALLOCATED(RLEC)) ALLOCATE(RLEC(nkr))
     if (.NOT. ALLOCATED(RIEC)) ALLOCATE(RIEC(nkr,icemax))
     if (.NOT. ALLOCATED(RSEC)) ALLOCATE(RSEC(nkr))
     if (.NOT. ALLOCATED(RGEC)) ALLOCATE(RGEC(nkr))
     if (.NOT. ALLOCATED(RHEC)) ALLOCATE(RHEC(nkr))

     hujisbm_unit1 = -1
     IF ( wrf_dm_on_monitor() ) THEN
         DO i = 31,99
             INQUIRE ( i , OPENED = opened )
             IF ( .NOT. opened ) THEN
                 hujisbm_unit1 = i
                 GOTO 2061
             ENDIF
         ENDDO
     2061  CONTINUE
     ENDIF

#if (defined(DM_PARALLEL))
 	CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , IWORDSIZE )
#endif
     IF ( hujisbm_unit1 < 0 ) THEN
         CALL wrf_error_fatal ( 'module_mp_FAST-SBM: Table-2 -- FAST_SBM_INIT: '// 			&
                               'Can not find unused fortran unit to read in lookup table,model stop' )
     ENDIF

 IF ( wrf_dm_on_monitor() ) THEN
 	WRITE(errmess, '(A,I2)') 'module_mp_FAST-SBM : Table-2 -- opening capacity.asc on unit',hujisbm_unit1
 	CALL wrf_debug(150, errmess)
 	OPEN(UNIT=hujisbm_unit1,FILE=trim(input_dir)//"/capacity33.asc",FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!OPEN(UNIT=hujisbm_unit1,FILE=trim(input_dir)//"/capacity43.asc",FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	900	FORMAT(6E13.5)
 	READ(hujisbm_unit1,900) RLEC,RIEC,RSEC,RGEC,RHEC
 END IF

#if (defined(DM_PARALLEL))
     DM_BCAST_MACRO_R4(RLEC)
     DM_BCAST_MACRO_R4(RIEC)
     DM_BCAST_MACRO_R4(RSEC)
     DM_BCAST_MACRO_R4(RGEC)
     DM_BCAST_MACRO_R4(RHEC)
#endif

     WRITE(errmess, '(A,I2)') 'FAST_SBM_INIT : succesfull reading Table-2'
     CALL wrf_debug(000, errmess)
 ! +----------------------------------------------------------------------+

 ! LookUpTable #3
 ! +-----------------------------------------------+
     if (.NOT. ALLOCATED(XL)) ALLOCATE(XL(nkr))
     if (.NOT. ALLOCATED(XI)) ALLOCATE(XI(nkr,icemax))
     if (.NOT. ALLOCATED(XS)) ALLOCATE(XS(nkr))
     if (.NOT. ALLOCATED(XG)) ALLOCATE(XG(nkr))
     if (.NOT. ALLOCATED(XH)) ALLOCATE(XH(nkr))

     hujisbm_unit1 = -1
     IF ( wrf_dm_on_monitor() ) THEN
       DO i = 31,99
         INQUIRE ( i , OPENED = opened )
         IF ( .NOT. opened ) THEN
           hujisbm_unit1 = i
           GOTO 2062
         ENDIF
       ENDDO
     2062 CONTINUE
     ENDIF

#if (defined(DM_PARALLEL))
     CALL wrf_dm_bcast_bytes ( hujisbm_unit1, IWORDSIZE )
#endif

     IF ( hujisbm_unit1 < 0 ) THEN
         CALL wrf_error_fatal ( 'module_mp_FAST_SBM: Table-3 -- FAST_SBM_INIT: '// 		&
                              'Can not find unused fortran unit to read in lookup table,model stop' )
     ENDIF
     IF ( wrf_dm_on_monitor() ) THEN
         WRITE(errmess, '(A,I2)') 'module_mp_FAST_SBM : Table-3 -- opening masses.asc on unit ',hujisbm_unit1
         CALL wrf_debug(150, errmess)
         OPEN(UNIT=hujisbm_unit1,FILE=trim(input_dir)//"/masses33.asc",FORM="FORMATTED",STATUS="OLD",ERR=2070)
         !OPEN(UNIT=hujisbm_unit1,FILE=trim(input_dir)//"/masses43.asc",FORM="FORMATTED",STATUS="OLD",ERR=2070)
         READ(hujisbm_unit1,900) XL,XI,XS,XG,XH
         CLOSE(hujisbm_unit1)
     ENDIF

#if (defined(DM_PARALLEL))
   	DM_BCAST_MACRO_R4(XL)
     DM_BCAST_MACRO_R4(XI)
     DM_BCAST_MACRO_R4(XS)
     DM_BCAST_MACRO_R4(XG)
     DM_BCAST_MACRO_R4(XH)
#endif

      WRITE(errmess, '(A,I2)') 'FAST_SBM_INIT : succesfull reading Table-3'
      CALL wrf_debug(000, errmess)
 ! +-------------------------------------------------------------------------+

 ! LookUpTable #4
 ! TERMINAL VELOSITY :
 ! +---------------------------------------------------+
     if (.NOT. ALLOCATED(VR1)) ALLOCATE(VR1(nkr))
     if (.NOT. ALLOCATED(VR2)) ALLOCATE(VR2(nkr,icemax))
     if (.NOT. ALLOCATED(VR3)) ALLOCATE(VR3(nkr))
     if (.NOT. ALLOCATED(VR4)) ALLOCATE(VR4(nkr))
     if (.NOT. ALLOCATED(VR5)) ALLOCATE(VR5(nkr))

     hujisbm_unit1 = -1
     IF ( wrf_dm_on_monitor() ) THEN
       DO i = 31,99
         INQUIRE ( i , OPENED = opened )
         IF ( .NOT. opened ) THEN
           hujisbm_unit1 = i
           GOTO 2063
         ENDIF
       ENDDO
     2063   CONTINUE
     ENDIF

#if (defined(DM_PARALLEL))
     CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , IWORDSIZE )
#endif
     IF ( hujisbm_unit1 < 0 ) THEN
         CALL wrf_error_fatal ( 'module_mp_FAST_SBM: Table-4 -- FAST_SBM_INIT: '// 										&
                                 'Can not find unused fortran unit to read in lookup table,model stop' )
     ENDIF

     IF ( wrf_dm_on_monitor() ) THEN
         WRITE(errmess, '(A,I2)') 'module_mp_FAST_SBM : Table-4 -- opening termvels.asc on unit ',hujisbm_unit1
         CALL wrf_debug(150, errmess)
         OPEN(UNIT=hujisbm_unit1,FILE=trim(input_dir)//"/termvels33_corrected.asc",FORM="FORMATTED",STATUS="OLD",ERR=2070)
         !OPEN(UNIT=hujisbm_unit1,FILE=trim(input_dir)//"/termvels43_corrected.asc",FORM="FORMATTED",STATUS="OLD",ERR=2070)
         READ(hujisbm_unit1,900) VR1,VR2,VR3,VR4,VR5
        CLOSE(hujisbm_unit1)
     ENDIF

#if (defined(DM_PARALLEL))
 	DM_BCAST_MACRO_R4(VR1)
     DM_BCAST_MACRO_R4(VR2)
     DM_BCAST_MACRO_R4(VR3)
     DM_BCAST_MACRO_R4(VR4)
     DM_BCAST_MACRO_R4(VR5)
#endif
     WRITE(errmess, '(A,I2)') 'FAST_SBM_INIT : succesfull reading Table-4'
     CALL wrf_debug(000, errmess)
 ! +----------------------------------------------------------------------+


 ! LookUpTable #5
 ! CONSTANTS :
 ! +---------------------------------------------------+
     if (.NOT. ALLOCATED(SLIC)) ALLOCATE(SLIC(nkr,6))
     if (.NOT. ALLOCATED(TLIC)) ALLOCATE(TLIC(nkr,2))
     if (.NOT. ALLOCATED(COEFIN)) ALLOCATE(COEFIN(nkr))

     hujisbm_unit1 = -1
     IF ( wrf_dm_on_monitor() ) THEN
       DO i = 31,99
         INQUIRE ( i , OPENED = opened )
         IF ( .NOT. opened ) THEN
           hujisbm_unit1 = i
           GOTO 2065
         ENDIF
       ENDDO
       hujisbm_unit1 = -1
     2065     CONTINUE
     ENDIF

#if (defined(DM_PARALLEL))
 		CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , IWORDSIZE )
#endif

     IF ( hujisbm_unit1 < 0 ) THEN
         CALL wrf_error_fatal ( 'module_mp_FAST_SBM: Table-5 -- FAST_SBM_INIT: '// 										&
                                'Can not find unused fortran unit to read in lookup table,model stop' )
     ENDIF

     IF ( wrf_dm_on_monitor() ) THEN
         WRITE(errmess, '(A,I2)') 'module_mp_FAST_SBM : Table-5 -- opening constants.asc on unit  ',hujisbm_unit1
         CALL wrf_debug(150, errmess)
         OPEN(UNIT=hujisbm_unit1,FILE=trim(input_dir)//"/constants33.asc",FORM="FORMATTED",STATUS="OLD",ERR=2070)
         !OPEN(UNIT=hujisbm_unit1,FILE=trim(input_dir)//"/constants43.asc",FORM="FORMATTED",STATUS="OLD",ERR=2070)
         READ(hujisbm_unit1,900) SLIC,TLIC,COEFIN
      CLOSE(hujisbm_unit1)
     END IF

#if (defined(DM_PARALLEL))
 	  DM_BCAST_MACRO_R4(SLIC)
    DM_BCAST_MACRO_R4(TLIC)
    DM_BCAST_MACRO_R4(COEFIN)
#endif
     WRITE(errmess, '(A,I2)') 'FAST_SBM_INIT : succesfull reading Table-5'
     CALL wrf_debug(000, errmess)
 ! +----------------------------------------------------------------------+

 ! LookUpTable #6
 ! KERNELS DEPENDING ON PRESSURE :
 ! +------------------------------------------------------------------+
     if (.NOT. ALLOCATED(YWLL_1000MB)) ALLOCATE(YWLL_1000MB(nkr,nkr))
     if (.NOT. ALLOCATED(YWLL_750MB)) ALLOCATE(YWLL_750MB(nkr,nkr))
     if (.NOT. ALLOCATED(YWLL_500MB)) ALLOCATE(YWLL_500MB(nkr,nkr))

     hujisbm_unit1 = -1
     IF ( wrf_dm_on_monitor() ) THEN
       DO i = 31,99
         INQUIRE ( i , OPENED = opened )
         IF ( .NOT. opened ) THEN
           hujisbm_unit1 = i
           GOTO 2066
         ENDIF
       ENDDO
       hujisbm_unit1 = -1
     2066     CONTINUE
     ENDIF

#if (defined(DM_PARALLEL))
 		CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , IWORDSIZE )
#endif
     IF ( hujisbm_unit1 < 0 ) THEN
         CALL wrf_error_fatal ( 'module_mp_FAST_SBM: Table-6 -- FAST_SBM_INIT: '// 			&
                                 'Can not find unused fortran unit to read in lookup table,model stop' )
     ENDIF
     IF ( wrf_dm_on_monitor() ) THEN
         WRITE(errmess, '(A,I2)') 'module_mp_FAST_SBM : Table-6 -- opening kernels_z.asc on unit  ',hujisbm_unit1
         CALL wrf_debug(150, errmess)
         Fname = trim(input_dir)//'/kernLL_z33.asc'
         !Fname = trim(input_dir)//'/kernLL_z43.asc'
         OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
         READ(hujisbm_unit1,900) YWLL_1000MB,YWLL_750MB,YWLL_500MB
         CLOSE(hujisbm_unit1)
     END IF

   	DO I=1,NKR
   		DO J=1,NKR
   			IF(I > 33 .OR. J > 33) THEN
   				YWLL_1000MB(I,J) = 0.0
   				YWLL_750MB(I,J) =  0.0
   				YWLL_500MB(I,J) =  0.0
   			ENDIF
   		ENDDO
   	ENDDO

#if (defined(DM_PARALLEL))
 	DM_BCAST_MACRO_R4(YWLL_1000MB)
     DM_BCAST_MACRO_R4(YWLL_750MB)
     DM_BCAST_MACRO_R4(YWLL_500MB)
#endif

     WRITE(errmess, '(A,I2)') 'FAST_SBM_INIT : succesfull reading Table-6'
     CALL wrf_debug(000, errmess)
 ! +-----------------------------------------------------------------------+

 ! LookUpTable #7
 ! COLLISIONS KERNELS :
 ! +-----------------------------------------------------------------------+
 ! ... Drops - IC
 if (.NOT. ALLOCATED(YWLI_300MB)) ALLOCATE(YWLI_300MB(nkr,nkr,icemax))
 if (.NOT. ALLOCATED(YWLI_500MB)) ALLOCATE(YWLI_500MB(nkr,nkr,icemax))
 if (.NOT. ALLOCATED(YWLI_750MB)) ALLOCATE(YWLI_750MB(nkr,nkr,icemax))

 ! ... Drops - Graupel
 if (.NOT. ALLOCATED(YWLG_300MB)) ALLOCATE(YWLG_300MB(nkr,nkr))
 if (.NOT. ALLOCATED(YWLG_500MB)) ALLOCATE(YWLG_500MB(nkr,nkr))
 if (.NOT. ALLOCATED(YWLG_750MB)) ALLOCATE(YWLG_750MB(nkr,nkr))
 !if (.NOT. ALLOCATED(YWLG)) ALLOCATE(YWLG(nkr,nkr))

 ! ... Drops - Hail
 if (.NOT. ALLOCATED(YWLH_300MB)) ALLOCATE(YWLH_300MB(nkr,nkr))
 if (.NOT. ALLOCATED(YWLH_500MB)) ALLOCATE(YWLH_500MB(nkr,nkr))
 if (.NOT. ALLOCATED(YWLH_750MB)) ALLOCATE(YWLH_750MB(nkr,nkr))

 ! ... Drops - Snow
 if (.NOT. ALLOCATED(YWLS_300MB)) ALLOCATE(YWLS_300MB(nkr,nkr))
 if (.NOT. ALLOCATED(YWLS_500MB)) ALLOCATE(YWLS_500MB(nkr,nkr))
 if (.NOT. ALLOCATED(YWLS_750MB)) ALLOCATE(YWLS_750MB(nkr,nkr))

 ! ... IC - IC
 if (.NOT. ALLOCATED(YWII_300MB)) ALLOCATE(YWII_300MB(nkr,nkr,icemax,icemax))
 if (.NOT. ALLOCATED(YWII_500MB)) ALLOCATE(YWII_500MB(nkr,nkr,icemax,icemax))
 if (.NOT. ALLOCATED(YWII_750MB)) ALLOCATE(YWII_750MB(nkr,nkr,icemax,icemax))

 ! ... IC - SNow
 if (.NOT. ALLOCATED(YWIS_300MB)) ALLOCATE(YWIS_300MB(nkr,nkr,icemax))
 if (.NOT. ALLOCATED(YWIS_500MB)) ALLOCATE(YWIS_500MB(nkr,nkr,icemax))
 if (.NOT. ALLOCATED(YWIS_750MB)) ALLOCATE(YWIS_750MB(nkr,nkr,icemax))

 ! ... Snow - Graupel
 if (.NOT. ALLOCATED(YWSG_300MB)) ALLOCATE(YWSG_300MB(nkr,nkr))
 if (.NOT. ALLOCATED(YWSG_500MB)) ALLOCATE(YWSG_500MB(nkr,nkr))
 if (.NOT. ALLOCATED(YWSG_750MB)) ALLOCATE(YWSG_750MB(nkr,nkr))

 ! ... Snow - SNow
 if (.NOT. ALLOCATED(YWSS_300MB)) ALLOCATE(YWSS_300MB(nkr,nkr))
 if (.NOT. ALLOCATED(YWSS_500MB)) ALLOCATE(YWSS_500MB(nkr,nkR))
 if (.NOT. ALLOCATED(YWSS_750MB)) ALLOCATE(YWSS_750MB(nkr,nkr))

     hujisbm_unit1 = -1
     IF ( wrf_dm_on_monitor() ) THEN
     DO i = 31,99
     INQUIRE ( i , OPENED = opened )
     IF ( .NOT. opened ) THEN
       hujisbm_unit1 = i
       GOTO 2067
     ENDIF
     ENDDO
     2067     CONTINUE
     ENDIF

#if (defined(DM_PARALLEL))
     CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , IWORDSIZE )
#endif
 IF ( hujisbm_unit1 < 0 ) THEN
 	CALL wrf_error_fatal ( 'module_mp_FAST_SBM: Table-7 -- FAST_SBM_INIT: '// 			&
 											'Can not find unused fortran unit to read in lookup table,model stop' )
 ENDIF
 ! ... KERNELS DEPENDING ON PRESSURE :
 IF ( wrf_dm_on_monitor() ) THEN
 	WRITE(errmess, '(A,I2)') 'module_mp_WRFsbm : Table-7 -- opening kernels33.asc on unit',hujisbm_unit1
 	CALL wrf_debug(150, errmess)

 	! ... Drop - IC
 	!Fname = trim(input_dir)//'/ckli_300mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWLI_300MB
 	!Fname = trim(input_dir)//'/ckli_500mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWLI_500MB
 	!Fname = trim(input_dir)//'/ckli_750mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWLI_750MB

 	Fname = trim(input_dir)//'/ckli_33_300mb_500mb_750mb.asc'
 	OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	READ(hujisbm_unit1,900) YWLI_300MB,YWLI_500MB,YWLI_750MB
 	CLOSE(hujisbm_unit1)

 	! ... Drop - Graupel
 	!Fname = trim(input_dir)//'/cklg_300mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWLG_300MB
 	!Fname = trim(input_dir)//'/cklg_500mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWLG_500MB
 	!Fname = trim(input_dir)//'/cklg_750mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWLG_750MB

 	Fname = trim(input_dir)//'/cklg_33_300mb_500mb_750mb.asc'
 	OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	READ(hujisbm_unit1,900) YWLG_300MB,YWLG_500MB,YWLG_750MB
 	CLOSE(hujisbm_unit1)

 	! ... Drop - Hail
 	!Fname = trim(input_dir)//'/cklh_300mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWLH_300MB
 	!Fname = trim(input_dir)//'/cklh_500mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWLH_500MB
 	!Fname = trim(input_dir)//'/cklh_750mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWLH_750MB

 	Fname = trim(input_dir)//'/cklh_33_300mb_500mb_750mb.asc'
 	OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	READ(hujisbm_unit1,900) YWLH_300MB,YWLH_500MB,YWLH_750MB
 	CLOSE(hujisbm_unit1)

 	! ... Drop - Snow
 	!Fname = trim(input_dir)//'/ckls_300mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWLS_300MB
 	!Fname = trim(input_dir)//'/ckls_500mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWLS_500MB
 	!Fname = trim(input_dir)//'/ckls_750mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWLS_750MB

 	Fname = trim(input_dir)//'/ckls_33_300mb_500mb_750mb.asc'
 	OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	READ(hujisbm_unit1,900) YWLS_300MB,YWLS_500MB,YWLS_750MB
 	CLOSE(hujisbm_unit1)

 	! ... IC - IC
  !Fname = trim(input_dir)//'/ckii_300mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWII_300MB
 	!Fname = trim(input_dir)//'/ckii_500mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWII_500MB
 	!Fname = trim(input_dir)//'/ckii_750mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWII_750MB
 	!CLOSE(hujisbm_unit1)

 	Fname = trim(input_dir)//'/ckii_33_300mb_500mb_750mb.asc'
 	OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	READ(hujisbm_unit1,900) YWII_300MB,YWII_500MB,YWII_750MB
 	CLOSE(hujisbm_unit1)

 	! ... IC - SNow
 	!Fname = trim(input_dir)//'/ckis_300mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWIS_300MB
 	!Fname = trim(input_dir)//'/ckis_500mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWIS_500MB
 	!Fname = trim(input_dir)//'/ckis_750mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWIS_750MB

 	Fname = trim(input_dir)//'/ckis_33_300mb_500mb_750mb.asc'
 	OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	READ(hujisbm_unit1,900) YWIS_300MB,YWIS_500MB,YWIS_750MB
 	CLOSE(hujisbm_unit1)

 	! ... Snow - Graupel
 	!Fname = trim(input_dir)//'/cksg_300mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWSG_300MB
 	!Fname = trim(input_dir)//'/cksg_500mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWSG_500MB
 	!Fname = trim(input_dir)//'/cksg_750mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWSG_750MB

 	Fname = trim(input_dir)//'/cksg_33_300mb_500mb_750mb.asc'
 	OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	READ(hujisbm_unit1,900) YWSG_300MB,YWSG_500MB,YWSG_750MB
 	CLOSE(hujisbm_unit1)

 	! ... Snow - Snow
 	!Fname = trim(input_dir)//'/ckss_300mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWSS_300MB
 	!Fname = trim(input_dir)//'/ckss_500mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWSS_500MB
 	!Fname = trim(input_dir)//'/ckss_750mb_As'
 	!OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	!READ(hujisbm_unit1,900) YWSS_750MB

 	Fname = trim(input_dir)//'/ckss_33_300mb_500mb_750mb.asc'
 	OPEN(UNIT=hujisbm_unit1,FILE=Fname,FORM="FORMATTED",STATUS="OLD",ERR=2070)
 	READ(hujisbm_unit1,900) YWSS_300MB,YWSS_500MB,YWSS_750MB
  CLOSE(hujisbm_unit1)
 END IF

#if (defined(DM_PARALLEL))
	   DM_BCAST_MACRO_R4(YWLI_300MB)
     DM_BCAST_MACRO_R4(YWLI_500MB)
     DM_BCAST_MACRO_R4(YWLI_750MB)

     DM_BCAST_MACRO_R4(YWLG_300MB)
     DM_BCAST_MACRO_R4(YWLG_500MB)
     DM_BCAST_MACRO_R4(YWLG_750MB)
     !DM_BCAST_MACRO(YWLG)

     DM_BCAST_MACRO_R4(YWLH_300MB)
     DM_BCAST_MACRO_R4(YWLH_500MB)
     DM_BCAST_MACRO_R4(YWLH_750MB)

     DM_BCAST_MACRO_R4(YWLS_300MB)
     DM_BCAST_MACRO_R4(YWLS_500MB)
     DM_BCAST_MACRO_R4(YWLS_750MB)

     DM_BCAST_MACRO_R4(YWII_300MB)
     DM_BCAST_MACRO_R4(YWII_500MB)
     DM_BCAST_MACRO_R4(YWII_750MB)

     DM_BCAST_MACRO_R4(YWIS_300MB)
     DM_BCAST_MACRO_R4(YWIS_500MB)
     DM_BCAST_MACRO_R4(YWIS_750MB)

     DM_BCAST_MACRO_R4(YWSG_300MB)
     DM_BCAST_MACRO_R4(YWSG_500MB)
     DM_BCAST_MACRO_R4(YWSG_750MB)

     DM_BCAST_MACRO_R4(YWSS_300MB)
     DM_BCAST_MACRO_R4(YWSS_500MB)
     DM_BCAST_MACRO_R4(YWSS_750MB)
#endif

     WRITE(errmess, '(A,I2)') 'FAST_SBM_INIT : succesfull reading Table-7'
     CALL wrf_debug(000, errmess)
 ! +-----------------------------------------------------------------------+

 ! LookUpTable #8
 ! BULKDENSITY:
 ! +--------------------------------------------------------------+
     if (.NOT. ALLOCATED(RO1BL)) ALLOCATE(RO1BL(nkr))
     if (.NOT. ALLOCATED(RO2BL)) ALLOCATE(RO2BL(nkr,icemax))
     if (.NOT. ALLOCATED(RO3BL)) ALLOCATE(RO3BL(nkr))
     if (.NOT. ALLOCATED(RO4BL)) ALLOCATE(RO4BL(nkr))
     if (.NOT. ALLOCATED(RO5BL)) ALLOCATE(RO5BL(nkr))

     hujisbm_unit1 = -1
     IF ( wrf_dm_on_monitor() ) THEN
       DO i = 31,99
         INQUIRE ( i , OPENED = opened )
         IF ( .NOT. opened ) THEN
           hujisbm_unit1 = i
           GOTO 2068
         ENDIF
       ENDDO
     2068     CONTINUE
     ENDIF

#if (defined(DM_PARALLEL))
     CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , IWORDSIZE )
#endif
     IF ( hujisbm_unit1 < 0 ) THEN
         CALL wrf_error_fatal ( 'module_mp_FAST_SBM: Table-8 -- FAST_SBM_INIT: '// 			&
                                 'Can not find unused fortran unit to read in lookup table,model stop' )
     ENDIF
     IF ( wrf_dm_on_monitor() ) THEN
         WRITE(errmess, '(A,I2)') 'module_mp_WRFsbm : Table-8 -- opening bulkdens.asc on unit ',hujisbm_unit1
         CALL wrf_debug(150, errmess)
         OPEN(UNIT=hujisbm_unit1,FILE=trim(input_dir)//"/bulkdens33.asc",FORM="FORMATTED",STATUS="OLD",ERR=2070)
         !OPEN(UNIT=hujisbm_unit1,FILE=trim(input_dir)//"/bulkdens43.asc",FORM="FORMATTED",STATUS="OLD",ERR=2070)
         READ(hujisbm_unit1,900) RO1BL,RO2BL,RO3BL,RO4BL,RO5BL
         CLOSE(hujisbm_unit1)
     END IF

#if (defined(DM_PARALLEL))
 	    DM_BCAST_MACRO_R4(RO1BL)
      DM_BCAST_MACRO_R4(RO2BL)
      DM_BCAST_MACRO_R4(RO3BL)
      DM_BCAST_MACRO_R4(RO4BL)
      DM_BCAST_MACRO_R4(RO5BL)
#endif
     WRITE(errmess, '(A,I2)') 'FAST_SBM_INIT : succesfull reading Table-8'
     CALL wrf_debug(000, errmess)
 ! +----------------------------------------------------------------------+

 ! LookUpTable #9
 ! BULKRADII:
 ! +-----------------------------------------------------------+
     if (.NOT. ALLOCATED(RADXXO)) ALLOCATE(RADXXO(nkr,nhydro))
     hujisbm_unit1 = -1
     IF ( wrf_dm_on_monitor() ) THEN
       DO i = 31,99
         INQUIRE ( i , OPENED = opened )
         IF ( .NOT. opened ) THEN
           hujisbm_unit1 = i
           GOTO 2069
         ENDIF
       ENDDO
     2069     CONTINUE
     ENDIF
#if (defined(DM_PARALLEL))
 		CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , IWORDSIZE )
#endif
     IF ( hujisbm_unit1 < 0 ) THEN
      CALL wrf_error_fatal ( 'module_mp_FAST_SBM: Table-9 -- FAST_SBM_INIT: '// 			&
                                 'Can not find unused fortran unit to read in lookup table,model stop' )
     ENDIF
     IF ( wrf_dm_on_monitor() ) THEN
         WRITE(errmess, '(A,I2)') 'module_mp_FAST_SBM : Table-9 -- opening bulkradii.asc on unit',hujisbm_unit1
         CALL wrf_debug(150, errmess)
         OPEN(UNIT=hujisbm_unit1,FILE=trim(input_dir)//"/bulkradii33.asc",FORM="FORMATTED",STATUS="OLD",ERR=2070)
         !OPEN(UNIT=hujisbm_unit1,FILE=trim(input_dir)//"/bulkradii43.asc",FORM="FORMATTED",STATUS="OLD",ERR=2070)
         READ(hujisbm_unit1,*) RADXXO
         CLOSE(hujisbm_unit1)
     END IF

#if (defined(DM_PARALLEL))
       DM_BCAST_MACRO_R4(RADXXO)
#endif
     WRITE(errmess, '(A,I2)') 'FAST_SBM_INIT : succesfull reading Table-9'
     CALL wrf_debug(000, errmess)
 ! +-----------------------------------------------------------------------+

 ! LookUpTable #10
 ! Polar-HUCM Scattering Amplitudes Look-up table :
 ! +-----------------------------------------------------------------------+
  CALL LOAD_TABLES(NKR)  ! (KS) - Loading the scattering look-up-table

 ! ... (KS) - Broadcating Liquid drops
#if (defined(DM_PARALLEL))
   	DM_BCAST_MACRO_R16 ( FAF1 )
   	DM_BCAST_MACRO_R16 ( FBF1 )
   	DM_BCAST_MACRO_R16 ( FAB1 )
   	DM_BCAST_MACRO_R16 ( FBB1 )
   ! ... (KS) - Broadcating Snow
   	DM_BCAST_MACRO_R16 ( FAF3 )
   	DM_BCAST_MACRO_R16 ( FBF3 )
   	DM_BCAST_MACRO_R16 ( FAB3 )
   	DM_BCAST_MACRO_R16 ( FBB3 )
   ! ... (KS) - Broadcating Graupel
   	DM_BCAST_MACRO_R16 ( FAF4 )
   	DM_BCAST_MACRO_R16 ( FBF4 )
   	DM_BCAST_MACRO_R16 ( FAB4 )
   	DM_BCAST_MACRO_R16 ( FBB4 )
   ! ### (KS) - Broadcating Hail
   	DM_BCAST_MACRO_R16 ( FAF5 )
   	DM_BCAST_MACRO_R16 ( FBF5 )
   	DM_BCAST_MACRO_R16 ( FAB5 )
   	DM_BCAST_MACRO_R16 ( FBB5 )
 ! ### (KS) - Broadcating Usetables array
 	  CALL wrf_dm_bcast_integer ( usetables , size ( usetables ) * IWORDSIZE )
#endif
  WRITE(errmess, '(A,I2)') 'module_mp_WRFsbm : succesfull reading Table-10'
  call wrf_message(errmess)
 ! +-----------------------------------------------------------------------+

 ! calculation of the mass(in mg) for categories boundaries :
   ax=2.d0**(1.0)

   do i=1,nkr
   	 xl_mg(i) = xl(i)*1.e3
      xs_mg(i) = xs(i)*1.e3
      xg_mg(i) = xg(i)*1.e3
      xh_mg(i) = xh(i)*1.e3
      xi1_mg(i) = xi(i,1)*1.e3
      xi2_mg(i) = xi(i,2)*1.e3
      xi3_mg(i) = xi(i,3)*1.e3
   enddo

   if (.NOT. ALLOCATED(IMA)) ALLOCATE(IMA(nkr,nkr))
   if (.NOT. ALLOCATED(CHUCM)) ALLOCATE(CHUCM(nkr,nkr))
   chucm  = 0.0d0
   ima = 0
   CALL courant_bott_KS(xl, nkr, chucm, ima, scal) ! ### (KS) : New courant_bott_KS (without XL_MG(0:nkr))
   WRITE(errmess, '(A,I2)') 'FAST_SBM_INIT : succesfull reading "courant_bott_KS" '
   CALL wrf_debug(000, errmess)

  DEG01=1./3.
  CONCCCNIN=0.
  CONTCCNIN=0.
  if (.NOT. ALLOCATED(DROPRADII)) ALLOCATE(DROPRADII(NKR))
  DO KR=1,NKR
  DROPRADII(KR)=(3.0*XL(KR)/4.0/3.141593/1.0)**DEG01
  ENDDO

 ! +-------------------------------------------------------------+
 ! Allocating Aerosols Array
 ! +-------------------------+
 if (.NOT. ALLOCATED(FCCNR_MAR)) ALLOCATE(FCCNR_MAR(NKR_aerosol))
 if (.NOT. ALLOCATED(FCCNR_CON)) ALLOCATE(FCCNR_CON(NKR_aerosol))
 if (.NOT. ALLOCATED(XCCN)) ALLOCATE(XCCN(NKR_aerosol))
 if (.NOT. ALLOCATED(RCCN)) ALLOCATE(RCCN(NKR_aerosol))
 if (.NOT. ALLOCATED(Scale_CCN_Factor)) ALLOCATE(Scale_CCN_Factor)
 if (.NOT. ALLOCATED(FCCN)) ALLOCATE(FCCN(NKR_aerosol))

 	IF(ILogNormal_modes_Aerosol == 1)THEN
 		! ... Initializing the FCCNR_MAR and FCCNR_CON
 		FCCNR_CON = 0.0
 		FCCNR_MAR = 0.0
 		Scale_CCN_Factor = 1.0
 		XCCN = 0.0
 		RCCN = 0.0
 		CALL LogNormal_modes_Aerosol(FCCNR_CON,FCCNR_MAR,NKR_aerosol,COL,XL,XCCN,RCCN,RO_SOLUTE,Scale_CCN_Factor,1)
 		CALL LogNormal_modes_Aerosol(FCCNR_CON,FCCNR_MAR,NKR_aerosol,COL,XL,XCCN,RCCN,RO_SOLUTE,Scale_CCN_Factor,2)
 		WRITE(errmess, '(A,I2)') 'module_mp_WRFsbm : succesfull reading "LogNormal_modes_Aerosol" '
 		CALL wrf_debug(000, errmess)
 	ENDIF
 ! +-------------------------------------------------------------+

 	 if (.NOT. ALLOCATED(PKIJ)) ALLOCATE(PKIJ(JBREAK,JBREAK,JBREAK))
 	 if (.NOT. ALLOCATED(QKJ)) ALLOCATE(QKJ(JBREAK,JBREAK))
 	 if (.NOT. ALLOCATED(ECOALMASSM)) ALLOCATE(ECOALMASSM(NKR,NKR))
 	 if (.NOT. ALLOCATED(BRKWEIGHT)) ALLOCATE(BRKWEIGHT(JBREAK))
    PKIJ = 0.0e0
    QKJ = 0.0e0
    ECOALMASSM = 0.0d0
    BRKWEIGHT = 0.0d0
 	 CALL BREAKINIT_KS(PKIJ,QKJ,ECOALMASSM,BRKWEIGHT,XL,DROPRADII,BR_MAX,JBREAK,JMAX,NKR,VR1) ! Rain Spontanous Breakup
#if (defined(DM_PARALLEL))
 	 	DM_BCAST_MACRO_R4 (PKIJ)
    DM_BCAST_MACRO_R4 (QKJ)
#endif
 	  WRITE(errmess, '(A,I2)') 'FAST_SBM_INIT : succesfull reading BREAKINIT_KS" '
    CALL wrf_debug(000, errmess)
  ! +--------------------------------------------------------------------------------------------------------------------+

   100	FORMAT(10I4)
   101   FORMAT(3X,F7.5,E13.5)
   102	FORMAT(4E12.4)
   105	FORMAT(A48)
   106	FORMAT(A80)
   123	FORMAT(3E12.4,3I4)
   200	FORMAT(6E13.5)
   201   FORMAT(6D13.5)
   300	FORMAT(8E14.6)
   301   FORMAT(3X,F8.3,3X,E13.5)
   302   FORMAT(5E13.5)

 if (.NOT. ALLOCATED(cwll)) ALLOCATE(cwll(nkr,nkr))

 if (.NOT. ALLOCATED(cwli_1)) ALLOCATE(cwli_1(nkr,nkr))
 if (.NOT. ALLOCATED(cwli_2)) ALLOCATE(cwli_2(nkr,nkr))
 if (.NOT. ALLOCATED(cwli_3)) ALLOCATE(cwli_3(nkr,nkr))

 if (.NOT. ALLOCATED(cwil_1)) ALLOCATE(cwil_1(nkr,nkr))
 if (.NOT. ALLOCATED(cwil_2)) ALLOCATE(cwil_2(nkr,nkr))
 if (.NOT. ALLOCATED(cwil_3)) ALLOCATE(cwil_3(nkr,nkr))

 if (.NOT. ALLOCATED(cwlg)) ALLOCATE(cwlg(nkr,nkr))
 if (.NOT. ALLOCATED(cwlh)) ALLOCATE(cwlh(nkr,nkr))
 if (.NOT. ALLOCATED(cwls)) ALLOCATE(cwls(nkr,nkr))
 if (.NOT. ALLOCATED(cwgl)) ALLOCATE(cwgl(nkr,nkr))
 if (.NOT. ALLOCATED(cwhl)) ALLOCATE(cwhl(nkr,nkr))
 if (.NOT. ALLOCATED(cwsl)) ALLOCATE(cwsl(nkr,nkr))

 if (.NOT. ALLOCATED(cwii_1_1)) ALLOCATE(cwii_1_1(nkr,nkr))
 if (.NOT. ALLOCATED(cwii_1_2)) ALLOCATE(cwii_1_2(nkr,nkr))
 if (.NOT. ALLOCATED(cwii_1_3)) ALLOCATE(cwii_1_3(nkr,nkr))
 if (.NOT. ALLOCATED(cwii_2_1)) ALLOCATE(cwii_2_1(nkr,nkr))
 if (.NOT. ALLOCATED(cwii_2_2)) ALLOCATE(cwii_2_2(nkr,nkr))
 if (.NOT. ALLOCATED(cwii_2_3)) ALLOCATE(cwii_2_3(nkr,nkr))
 if (.NOT. ALLOCATED(cwii_3_1)) ALLOCATE(cwii_3_1(nkr,nkr))
 if (.NOT. ALLOCATED(cwii_3_2)) ALLOCATE(cwii_3_2(nkr,nkr))
 if (.NOT. ALLOCATED(cwii_3_3)) ALLOCATE(cwii_3_3(nkr,nkr))

 if (.NOT. ALLOCATED(cwis_1)) ALLOCATE(cwis_1(nkr,nkr))
 if (.NOT. ALLOCATED(cwis_2)) ALLOCATE(cwis_2(nkr,nkr))
 if (.NOT. ALLOCATED(cwis_3)) ALLOCATE(cwis_3(nkr,nkr))
 if (.NOT. ALLOCATED(cwsi_1)) ALLOCATE(cwsi_1(nkr,nkr))
 if (.NOT. ALLOCATED(cwsi_2)) ALLOCATE(cwsi_2(nkr,nkr))
 if (.NOT. ALLOCATED(cwsi_3)) ALLOCATE(cwsi_3(nkr,nkr))

 if (.NOT. ALLOCATED(cwig_1)) ALLOCATE(cwig_1(nkr,nkr))
 if (.NOT. ALLOCATED(cwig_2)) ALLOCATE(cwig_2(nkr,nkr))
 if (.NOT. ALLOCATED(cwig_3)) ALLOCATE(cwig_3(nkr,nkr))

 if (.NOT. ALLOCATED(cwih_1)) ALLOCATE(cwih_1(nkr,nkr))
 if (.NOT. ALLOCATED(cwih_2)) ALLOCATE(cwih_2(nkr,nkr))
 if (.NOT. ALLOCATED(cwih_3)) ALLOCATE(cwih_3(nkr,nkr))

 if (.NOT. ALLOCATED(cwsg)) ALLOCATE(cwsg(nkr,nkr))
 if (.NOT. ALLOCATED(cwss)) ALLOCATE(cwss(nkr,nkr))

   cwll(:,:) = 0.0e0
   cwli_1(:,:) = 0.0e0 ; cwli_2(:,:) = 0.0e0 ; cwli_3(:,:) = 0.0e0
   cwil_1(:,:) = 0.0e0 ; cwil_2(:,:) = 0.0e0 ; cwil_3(:,:) = 0.0e0
   cwlg(:,:) = 0.0e0 ; cwlh(:,:) = 0.0e0 ; cwls(:,:) = 0.0e0
   cwgl(:,:) = 0.0e0 ; cwhl(:,:) = 0.0e0 ; cwsl(:,:) = 0.0e0
   cwii_1_1(:,:) = 0.0e0 ; cwii_1_2(:,:) = 0.0e0 ; cwii_1_3(:,:) = 0.0e0
   cwii_2_1(:,:) = 0.0e0 ; cwii_2_2(:,:) = 0.0e0 ; cwii_2_3(:,:) = 0.0e0
   cwii_3_1(:,:) = 0.0e0 ; cwii_3_2(:,:) = 0.0e0 ; cwii_3_3(:,:) = 0.0e0
   cwis_1(:,:) = 0.0e0 ; cwis_2(:,:) = 0.0e0 ; cwis_3(:,:) = 0.0e0
   cwsi_1(:,:) = 0.0e0 ; cwsi_2(:,:) = 0.0e0 ; cwsi_3(:,:) = 0.0e0
   cwig_1(:,:) = 0.0e0 ; cwig_2(:,:) = 0.0e0 ; cwig_3(:,:) = 0.0e0
   cwih_1(:,:) = 0.0e0 ; cwih_2(:,:) = 0.0e0 ; cwih_3(:,:) = 0.0e0
   cwsg(:,:) = 0.0e0 ; cwss(:,:) = 0.0e0

   call Kernals_KS(dt,nkr,7.6E6)

 !+---+-----------------------------------------+
 if (.NOT. ALLOCATED( Prob)) ALLOCATE( Prob(NKR))
 if (.NOT. ALLOCATED(Gain_Var_New)) ALLOCATE(Gain_Var_New(NKR,NKR))
 if (.NOT. ALLOCATED(NND)) ALLOCATE(NND(NKR,NKR))
  Prob = 0.0
  Gain_Var_New = 0.0
  NND = 0.0
  call Spontanous_Init(dt, XL, DROPRADII, Prob, Gain_Var_New, NND, NKR, ikr_spon_break)
  WRITE(errmess, '(A,I2)') 'FAST_SBM_INIT : succesfull reading "Spontanous_Init" '
  CALL wrf_debug(000, errmess)

  return
  2070  continue

      WRITE( errmess , '(A,I4)' )                                          &
                 'module_mp_FAST_SBM_INIT: error opening hujisbm_DATA on unit,model stop ' &
                 &, hujisbm_unit1
      CALL wrf_error_fatal(errmess)

  END SUBROUTINE FAST_HUCMINIT
 ! -----------------------------------------------------------------+
  subroutine Kernals_KS(dtime_coal,nkr,p_z)

  implicit none

  integer :: nkr
  real(kind=r4size),intent(in) :: dtime_coal,p_z

  ! ### Locals
  integer :: i,j
  real(kind=r4size),parameter :: p1=1.0e6,p2=0.75e6,p3=0.50e6,p4=0.3e6
  real(kind=r4size) :: dlnr, scal, dtimelnr, pdm, p_1, p_2, p_3, ckern_1, ckern_2, &
  					  ckern_3

 ! p1=1.00D6 dynes/cm^2 = 1000.0 mb
 ! p2=0.75D6 dynes/cm^2 =  750.0 mb
 ! p3=0.50D6 dynes/cm^2 =  500.0 mb
 ! p4=0.30D6 dynes/cm^2 =  300.0 mb

  scal = 1.0
 	dlnr = dlog(2.0d0)/(3.0d0*scal)
 	dtimelnr = dtime_coal*dlnr

 	p_1=p1
 	p_2=p2
 	p_3=p3
 	do i=1,nkr
 		do j=1,nkr
 			! 1. water - water
 			ckern_1 = YWLL_1000mb(i,j)
 			ckern_2 = YWLL_750mb(i,j)
 			ckern_3 = YWLL_500mb(i,j)
 			cwll(i,j) = ckern_z(p_z,p_1,p_2,p_3,ckern_1,ckern_2,ckern_3)*dtime_coal*dlnr
 		end do
 	end do

 	! ... ECOALMASSM is from "BreakIniit_KS"
 	DO I=1,NKR
 	 DO J=1,NKR
 		CWLL(I,J) = ECOALMASSM(I,J)*CWLL(I,J)
 	 END DO
  END DO

 	p_1=p2
 	p_2=p3
 	p_3=p4

 	if(p_z >= p_1) then
 		do j=1,nkr
 	  		do i=1,nkr
 				cwli_1(i,j) = ywli_750mb(i,j,1)*dtimelnr
 				cwli_2(i,j) = ywli_750mb(i,j,2)*dtimelnr
 				cwli_3(i,j) = ywli_750mb(i,j,3)*dtimelnr
 				cwlg(i,j) = ywlg_750mb(i,j)*dtimelnr
 				cwlh(i,j) = ywlh_750mb(i,j)*dtimelnr
 				cwls(i,j) = ywls_750mb(i,j)*dtimelnr
 				cwii_1_1(i,j) = ywii_750mb(i,j,1,1)*dtimelnr
 				cwii_1_2(i,j) = ywii_750mb(i,j,1,2)*dtimelnr
 				cwii_1_3(i,j) = ywii_750mb(i,j,1,3)*dtimelnr
 				cwii_2_1(i,j) = ywii_750mb(i,j,2,1)*dtimelnr
 				cwii_2_2(i,j) = ywii_750mb(i,j,2,2)*dtimelnr
 				cwii_2_3(i,j) = ywii_750mb(i,j,2,3)*dtimelnr
 				cwii_3_1(i,j) = ywii_750mb(i,j,3,1)*dtimelnr
 				cwii_3_2(i,j) = ywii_750mb(i,j,3,2)*dtimelnr
 				cwii_3_3(i,j) = ywii_750mb(i,j,3,3)*dtimelnr
 				cwis_1(i,j) = ywis_750mb(i,j,1)*dtimelnr
 				cwis_2(i,j) = ywis_750mb(i,j,2)*dtimelnr
 				cwis_3(i,j) = ywis_750mb(i,j,3)*dtimelnr
 				cwsg(i,j) = ywsg_750mb(i,j)*dtimelnr
 				cwss(i,j) = ywss_750mb(i,j)*dtimelnr
 	  		end do
 		end do
 	endif

 	if (p_z <= p_3) then
 		do j=1,nkr
 		  do i=1,nkr
 			cwli_1(i,j) = ywli_300mb(i,j,1)*dtimelnr
 			cwli_2(i,j) = ywli_300mb(i,j,2)*dtimelnr
 			cwli_3(i,j) = ywli_300mb(i,j,3)*dtimelnr
 			cwlg(i,j) = ywlg_300mb(i,j)*dtimelnr
 			cwlh(i,j) = ywlh_300mb(i,j)*dtimelnr
 			cwls(i,j) = ywls_300mb(i,j)*dtimelnr
 			cwii_1_1(i,j) = ywii_300mb(i,j,1,1)*dtimelnr
 			cwii_1_2(i,j) = ywii_300mb(i,j,1,2)*dtimelnr
 			cwii_1_3(i,j) = ywii_300mb(i,j,1,3)*dtimelnr
 			cwii_2_1(i,j) = ywii_300mb(i,j,2,1)*dtimelnr
 			cwii_2_2(i,j) = ywii_300mb(i,j,2,2)*dtimelnr
 			cwii_2_3(i,j) = ywii_300mb(i,j,2,3)*dtimelnr
 			cwii_3_1(i,j) = ywii_300mb(i,j,3,1)*dtimelnr
 			cwii_3_2(i,j) = ywii_300mb(i,j,3,2)*dtimelnr
 			cwii_3_3(i,j) = ywii_300mb(i,j,3,3)*dtimelnr
 			cwis_1(i,j) = ywis_300mb(i,j,1)*dtimelnr
 			cwis_2(i,j) = ywis_300mb(i,j,2)*dtimelnr
 			cwis_3(i,j) = ywis_300mb(i,j,3)*dtimelnr
 			cwsg(i,j) = ywsg_300mb(i,j)*dtimelnr
 			cwss(i,j) = ywss_300mb(i,j)*dtimelnr
 		  end do
 		end do
 	  endif

 	  if (p_z <  p_1  .and. p_z >= p_2) then
 		pdm = (p_z-p_2)/(p_1-p_2)
 		do j=1,nkr
 		  do i=1,nkr
 		  	ckern_1=ywli_750mb(i,j,1)
 			ckern_2=ywli_500mb(i,j,1)
 			cwli_1(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr

 			ckern_1=ywli_750mb(i,j,2)
 			ckern_2=ywli_500mb(i,j,2)
 			cwli_2(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr

 			ckern_1=ywli_750mb(i,j,3)
 			ckern_2=ywli_500mb(i,j,3)
 			cwli_3(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr

 			ckern_1=ywlg_750mb(i,j)
 			ckern_2=ywlg_500mb(i,j)
 			cwlg(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr

 			ckern_1=ywlh_750mb(i,j)
 			ckern_2=ywlh_500mb(i,j)
 			cwlh(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr

 			ckern_1=ywls_750mb(i,j)
 			ckern_2=ywls_500mb(i,j)
 			cwls(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr

 			ckern_1=ywii_750mb(i,j,1,1)
 			ckern_2=ywii_500mb(i,j,1,1)
 			cwii_1_1(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr

 			ckern_1=ywii_750mb(i,j,1,2)
 			ckern_2=ywii_500mb(i,j,1,2)
 			cwii_1_2(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr

 			ckern_1=ywii_750mb(i,j,1,3)
 			ckern_2=ywii_500mb(i,j,1,3)
 			cwii_1_3(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr

 			ckern_1=ywii_750mb(i,j,2,1)
 			ckern_2=ywii_500mb(i,j,2,1)
 			cwii_2_1(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr


 			ckern_1=ywii_750mb(i,j,2,2)
 			ckern_2=ywii_500mb(i,j,2,2)
 			cwii_2_2(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr

 			ckern_1=ywii_750mb(i,j,2,3)
 			ckern_2=ywii_500mb(i,j,2,3)
 			cwii_2_3(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr

 			ckern_1=ywii_750mb(i,j,3,1)
 			ckern_2=ywii_500mb(i,j,3,1)
 			cwii_3_1(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr

 			ckern_1=ywii_750mb(i,j,3,2)
 			ckern_2=ywii_500mb(i,j,3,2)
 			cwii_3_2(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr

 			ckern_1=ywii_750mb(i,j,3,3)
 			ckern_2=ywii_500mb(i,j,3,3)
 			cwii_3_3(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr

 			ckern_1=ywis_750mb(i,j,1)
 			ckern_2=ywis_500mb(i,j,1)
 			cwis_1(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr

 			ckern_1=ywis_750mb(i,j,2)
 			ckern_2=ywis_500mb(i,j,2)
 			cwis_2(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr

 			ckern_1=ywis_750mb(i,j,3)
 			ckern_2=ywis_500mb(i,j,3)
 			cwis_3(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr

 			ckern_1=ywsg_750mb(i,j)
 			ckern_2=ywsg_500mb(i,j)
 			cwsg(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr

 			ckern_1=ywss_750mb(i,j)
 			ckern_2=ywss_500mb(i,j)
 			cwss(i,j)=(ckern_2+(ckern_1-ckern_2)*pdm)*dtimelnr
 		   end do
 		 end do
 	   endif

  		if (p_z <  p_2  .and. p_z >  p_3) then
 		   pdm = (p_z-p_3)/(p_2-p_3)
 		   do j=1,nkr
 		     do i=1,nkr

 			  ckern_2=ywli_500mb(i,j,1)
 			  ckern_3=ywli_300mb(i,j,1)
 			  cwli_1(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

 			  ckern_2=ywli_500mb(i,j,2)
 			  ckern_3=ywli_300mb(i,j,2)
 			  cwli_2(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

 			  ckern_2=ywli_500mb(i,j,3)
 			  ckern_3=ywli_300mb(i,j,3)
 			  cwli_3(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

 			  ckern_2=ywlg_500mb(i,j)
 			  ckern_3=ywlg_300mb(i,j)
 			  cwlg(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

   			ckern_2=ywlh_500mb(i,j)
   			ckern_3=ywlh_300mb(i,j)
   			cwlh(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

   			ckern_2=ywls_500mb(i,j)
   			ckern_3=ywls_300mb(i,j)
   			cwls(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

   			ckern_2=ywii_500mb(i,j,1,1)
   			ckern_3=ywii_300mb(i,j,1,1)
   			cwii_1_1(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

   			ckern_2=ywii_500mb(i,j,1,2)
   			ckern_3=ywii_300mb(i,j,1,2)
   			cwii_1_2(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

   			ckern_2=ywii_500mb(i,j,1,3)
   			ckern_3=ywii_300mb(i,j,1,3)
   			cwii_1_3(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

   			ckern_2=ywii_500mb(i,j,2,1)
   			ckern_3=ywii_300mb(i,j,2,1)
   			cwii_2_1(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

   			ckern_2=ywii_500mb(i,j,2,2)
   			ckern_3=ywii_300mb(i,j,2,2)
   			cwii_2_2(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

   			ckern_2=ywii_500mb(i,j,2,3)
   			ckern_3=ywii_300mb(i,j,2,3)
   			cwii_2_3(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

   			ckern_2=ywii_500mb(i,j,3,1)
   			ckern_3=ywii_300mb(i,j,3,1)
   			cwii_3_1(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

   			ckern_2=ywii_500mb(i,j,3,2)
   			ckern_3=ywii_300mb(i,j,3,2)
   			cwii_3_2(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

   			ckern_2=ywii_500mb(i,j,3,3)
   			ckern_3=ywii_300mb(i,j,3,3)
   			cwii_3_3(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

   			ckern_2=ywis_500mb(i,j,1)
   			ckern_3=ywis_300mb(i,j,1)
   			cwis_1(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

   			ckern_2=ywis_500mb(i,j,2)
   			ckern_3=ywis_300mb(i,j,2)
   			cwis_2(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

   			ckern_2=ywis_500mb(i,j,3)
   			ckern_3=ywis_300mb(i,j,3)
   			cwis_3(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

   			ckern_2=ywsg_500mb(i,j)
   			ckern_3=ywsg_300mb(i,j)
   			cwsg(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

   			ckern_2=ywss_500mb(i,j)
   			ckern_3=ywss_300mb(i,j)
   			cwss(i,j)=(ckern_3+(ckern_2-ckern_3)*pdm)*dtimelnr

 		   end do
 		 end do
   endif

 		do i=1,nkr
 		 do j=1,nkr
 ! columns - water
 		  cwil_1(i,j)=cwli_1(j,i)
 ! plates - water
 		  cwil_2(i,j)=cwli_2(j,i)
 ! dendrites - water
 		  cwil_3(i,j)=cwli_3(j,i)
 ! 3. graupel - water
 		  cwgl(i,j)=cwlg(j,i)
 ! 4. hail - water
 		  cwhl(i,j)=cwlh(j,i)
 ! 5. snow - water
 		  cwsl(i,j)=cwls(j,i)
 ! 7.snow - crystals :
 ! snow - columns
 		  cwsi_1(i,j)=cwis_1(j,i)
 ! snow - plates
 		  cwsi_2(i,j)=cwis_2(j,i)
 ! snow - dendrites
 		  cwsi_3(i,j)=cwis_3(j,i)
 		 end do
 	  end do


  return
  end subroutine Kernals_KS

 ! ------------------------------------------------------------+
  real function ckern_z (p_z,p_1,p_2,p_3,ckern_1,ckern_2,ckern_3)

 	implicit none

 	real(kind=r4size),intent(in) :: p_z,p_1,p_2,p_3,ckern_1, &
 									ckern_2,ckern_3

 	if(p_z>=p_1) ckern_z = ckern_1
 	!if(p_z==p_2) ckern_z=ckern_2
 	if(p_z<=p_3) ckern_z = ckern_3
 	if(p_z<p_1 .and. p_z>=p_2) ckern_z = ckern_2 + (ckern_1-ckern_2)*(p_z-p_2)/(p_1-p_2)
 	if(p_z<p_2 .and. p_z>p_3) ckern_z = ckern_3 + (ckern_2-ckern_3)*(p_z-p_3)/(p_2-p_3)

  return
  end function ckern_z
 ! -------------------------------------------------------------+
  SUBROUTINE FREEZ(FF1,XL,FF2,XI,FF3,XS,FF4,XG,FF5,XH, &
      					   TIN,DT,RO,COL,AFREEZMY,BFREEZMY,    &
			             BFREEZMAX,KRFREEZ,ICEMAX,NKR)

 	  IMPLICIT NONE

 	    INTEGER KR,ICE,ICE_TYPE
      REAL COL,AFREEZMY,BFREEZMY,BFREEZMAX
      INTEGER KRFREEZ,ICEMAX,NKR
      REAL DT,RO,YKK,PF,PF_1,DEL_T,TT_DROP,ARG_1,YK2,DF1,BF,ARG_M, &
      	   TT_DROP_AFTER_FREEZ,CFREEZ,SUM_ICE,TIN,TTIN,AF,FF_MAX,F1_MAX, &
      	   F2_MAX,F3_MAX,F4_MAX,F5_MAX

      REAL FF1(NKR),XL(NKR),FF2(NKR,ICEMAX) &
           ,XI(NKR,ICEMAX),FF3(NKR),XS(NKR),FF4(NKR) &
           ,XG(NKR),FF5(NKR),XH(NKR)

 	TTIN=TIN
 	DEL_T	=TTIN-273.15
 	ICE_TYPE=2
 	F1_MAX=0.
 	F2_MAX=0.
 	F3_MAX=0.
 	F4_MAX=0.
 	F5_MAX=0.
 	DO KR=1,NKR
   	F1_MAX=AMAX1(F1_MAX,FF1(KR))
   	F3_MAX=AMAX1(F3_MAX,FF3(KR))
   	F4_MAX=AMAX1(F4_MAX,FF4(KR))
   	F5_MAX=AMAX1(F5_MAX,FF5(KR))
   	DO ICE=1,ICEMAX
        	F2_MAX=AMAX1(F2_MAX,FF2(KR,ICE))
    ENDDO
 	  FF_MAX=AMAX1(F2_MAX,F3_MAX,F4_MAX,F5_MAX)
  ENDDO
 !
 !******************************* FREEZING ****************************
 !
        IF(DEL_T.LT.0.AND.F1_MAX.NE.0) THEN
 			      SUM_ICE=0.
		        AF	= AFREEZMY
	          CFREEZ	=(BFREEZMAX-BFREEZMY)/XL(NKR)
 !
 !***************************** MASS LOOP **************************
 !
          	DO  KR	=1,NKR
 	 			        ARG_M	=XL(KR)
 			          BF	=BFREEZMY+CFREEZ*ARG_M
        		    PF_1	=AF*EXP(-BF*DEL_T)
        		    PF	=ARG_M*PF_1
 			          YKK	=EXP(-PF*DT)
        		    DF1	=FF1(KR)*(1.-YKK)
 			          YK2	=DF1
        		    FF1(KR)=FF1(KR)*YKK
 			          IF(KR.LE.KRFREEZ)  THEN
 				           FF2(KR,ICE_TYPE)=FF2(KR,ICE_TYPE)+YK2
                ELSE
				           FF5(KR)	=FF5(KR)+YK2
                ENDIF
		            SUM_ICE=SUM_ICE+YK2*3.*XL(KR)*XL(KR)*COL
 !
 !************************ END OF "MASS LOOP" **************************
 !
 	 		       ENDDO
 !
 !************************** NEW TEMPERATURE *************************
 !
	           ARG_1	=333.*SUM_ICE/RO
       		   TT_DROP_AFTER_FREEZ=TTIN+ARG_1
		         TIN	=TT_DROP_AFTER_FREEZ
 !
 !************************** END OF "FREEZING" ****************************
 !
 		   ENDIF
 !
    	RETURN
 	END SUBROUTINE FREEZ
 ! ----------------------------------------------------------------+
  SUBROUTINE J_W_MELT(FF1,XL,FF2,XI,FF3,XS,FF4,XG,FF5,XH &
                     ,TIN,DT,RO,COL,ICEMAX,NKR)

 	  IMPLICIT NONE

 	  integer,intent(in) :: NKR,ICEMAX
 	  real(kind=R4size),intent(in)    :: DT,COL,RO
 	  real(kind=R4size),intent(inout) :: FF1(:),XL(:),FF2(:,:),XI(:,:),FF3(:),XS(:),FF4(:),XG(:), &
 	  									                 FF5(:),XH(:),Tin

 	  !  ... Locals
      integer :: KR,ICE,ICE_TYPE
      real(kind=R4size) :: ARG_M,TT_DROP,ARG_1,TT_DROP_AFTER_FREEZ,DF1,DN,DN0, &
       						         A,B,DTFREEZ,SUM_ICE,FF_MAX,F1_MAX,F2_MAX,F3_MAX,F4_MAX,F5_MAX, &
		                       DEL_T,meltrate,gamma
 	  ! ... Locals

    	gamma=4.4
 	   	DEL_T = TIN-273.15
   		ICE_TYPE = 2
   		F1_MAX=0.
   		F2_MAX=0.
   		F3_MAX=0.
   		F4_MAX=0.
   		F5_MAX=0.
   		DO KR=1,NKR
   			F1_MAX=AMAX1(F1_MAX,FF1(KR))
   			F3_MAX=AMAX1(F3_MAX,FF3(KR))
   			F4_MAX=AMAX1(F4_MAX,FF4(KR))
   			F5_MAX=AMAX1(F5_MAX,FF5(KR))
   			DO ICE=1,ICEMAX
   				F2_MAX=AMAX1(F2_MAX,FF2(KR,ICE))
   		  END DO
 				FF_MAX=AMAX1(F2_MAX,F3_MAX,F4_MAX,F5_MAX)
       END DO
 				SUM_ICE=0.
 				IF(DEL_T.GE.0.AND.FF_MAX.NE.0) THEN
   				DO KR = 1,NKR
          		ARG_M = 0.0
           		DO ICE = 1,ICEMAX
        			   IF (ICE ==1) THEN
              			IF (KR .le. 10) THEN
                  			ARG_M = ARG_M+FF2(KR,ICE)
		                    FF2(KR,ICE) = 0.0
  			            ELSE IF (KR .gt. 10 .and. KR .lt. 18) THEN
                  			meltrate = 0.5/50.
                  			FF2(KR,ICE)=FF2(KR,ICE)-FF2(KR,ICE)*(meltrate*dt)
                  			ARG_M=ARG_M+FF2(KR,ICE)*(meltrate*dt)
              			ELSE
                  			meltrate = 0.683/120.
                  			FF2(KR,ICE)=FF2(KR,ICE)-FF2(KR,ICE)*(meltrate*dt)
                  			ARG_M=ARG_M+FF2(KR,ICE)*(meltrate*dt)
              			ENDIF
            			ENDIF
            			IF (ICE ==2 .or. ICE ==3) THEN
               			  IF (kr .le. 12) THEN
                       			FF2(KR,ICE)=0.
                       			ARG_M = ARG_M+FF2(KR,ICE)
             			    ELSE IF (kr .gt. 12 .and. kr .lt. 20) THEN
                       			meltrate = 0.5/50.
                       			FF2(KR,ICE)=FF2(KR,ICE)-FF2(KR,ICE)*(meltrate*dt)
                       			ARG_M=ARG_M+FF2(KR,ICE)*(meltrate*dt)
                			ELSE
                        			meltrate = 0.683/120.
                       			FF2(KR,ICE)=FF2(KR,ICE)-FF2(KR,ICE)*(meltrate*dt)
                       			ARG_M=ARG_M+FF2(KR,ICE)*(meltrate*dt)
                			ENDIF
              			ENDIF
           		END DO  ! Do ice
  				         ! ... Snow
                  IF (kr .le. 14) THEN
                     ARG_M = ARG_M + FF3(KR)
  		               FF3(KR) = 0.0
                  ELSE IF (kr .gt. 14 .and. kr .lt. 22) THEN
                     meltrate = 0.5/50.
                     FF3(KR)=FF3(KR)-FF3(KR)*(meltrate*dt)
                     ARG_M=ARG_M+FF3(KR)*(meltrate*dt)
                  ELSE
                     meltrate = 0.683/120.
                     FF3(KR)=FF3(KR)-FF3(KR)*(meltrate*dt)
                     ARG_M=ARG_M+FF3(KR)*(meltrate*dt)
                  ENDIF
  	             ! ... Graupel/Hail
                  IF (kr .le. 13) then
                      ARG_M = ARG_M+FF4(KR)+FF5(KR)
  		                FF4(KR)=0.
                      FF5(KR)=0.
                  ELSE IF (kr .gt. 13 .and. kr .lt. 23) THEN
                      meltrate = 0.5/50.
                      FF4(KR)=FF4(KR)-FF4(KR)*(meltrate*dt)
                      FF5(KR)=FF5(KR)-FF5(KR)*(meltrate*dt)
                      ARG_M=ARG_M+(FF4(KR)+FF5(KR))*(meltrate*dt)
                  ELSE
                      meltrate = 0.683/120.
                     FF4(KR)=FF4(KR)-FF4(KR)*(meltrate*dt)
                     FF5(KR)=FF5(KR)-FF5(KR)*(meltrate*dt)
                     ARG_M=ARG_M+(FF4(KR)+FF5(KR))*(meltrate*dt)
                  ENDIF

                    FF1(KR) = FF1(KR) + ARG_M
                    SUM_ICE=SUM_ICE+ARG_M*3.*XL(KR)*XL(KR)*COL
          	END DO

            ARG_1=333.*SUM_ICE/RO
            TIN = TIN - ARG_1
          ENDIF

    	RETURN
 	END SUBROUTINE J_W_MELT
 ! +----------------------------------------------------------------------------+
   SUBROUTINE ONECOND1 &
				 & (TT,QQ,PP,ROR &
				 & ,VR1,PSINGLE &
				 & ,DEL1N,DEL2N,DIV1,DIV2 &
				 & ,FF1,PSI1,R1,RLEC,RO1BL &
				 & ,AA1_MY,BB1_MY,AA2_MY,BB2_MY &
				 & ,C1_MEY,C2_MEY &
				 & ,COL,DTCOND,ICEMAX,NKR,ISYM1 &
				   ,ISYM2,ISYM3,ISYM4,ISYM5,Iin,Jin,Kin,W_in,DX_in,Itimestep)

        IMPLICIT NONE


       INTEGER NKR,ICEMAX, ISYM1, ISYM2(ICEMAX),ISYM3,ISYM4,ISYM5, Iin, Jin, Kin, &
 	  		  sea_spray_no_temp_change_per_grid, Itimestep
       REAL    COL,VR1(NKR),PSINGLE &
      &       ,AA1_MY,BB1_MY,AA2_MY,BB2_MY &
      &       ,DTCOND, W_in,DX_in

       REAL C1_MEY,C2_MEY
       INTEGER I_ABERGERON,I_BERGERON, &
      & KR,ICE,ITIME,KCOND,NR,NRM, &
      & KLIMIT, &
      & KM,KLIMITL
       REAL AL1,AL2,D,GAM,POD, &
      & RV_MY,CF_MY,D_MYIN,AL1_MY,AL2_MY,ALC,DT0LREF,DTLREF, &
      & A1_MYN, BB1_MYN, A2_MYN, BB2_MYN,DT,DTT,XRAD, &
      & TPC1, TPC2, TPC3, TPC4, TPC5, &
      & EPSDEL, EPSDEL2,DT0L, DT0I,&
      & ROR, &
      & CWHUCM,B6,B8L,B8I, &
      & DEL1,DEL2,DEL1S,DEL2S, &
      & TIMENEW,TIMEREV,SFN11,SFN12, &
      & SFNL,SFNI,B5L,B5I,B7L,B7I,DOPL,DOPI,RW,RI,QW,PW, &
      & PI,QI,DEL1N0,DEL2N0,D1N0,D2N0,DTNEWL,DTNEWL1,D1N,D2N, &
      & DEL_R1,DT0L0,DT0I0, &
      & DTNEWL0, &
      & DTNEWL2
        REAL DT_WATER_COND,DT_WATER_EVAP

        INTEGER K
 ! NEW ALGORITHM OF CONDENSATION (12.01.00)

       REAL  FF1_OLD(NKR),SUPINTW(NKR)
       DOUBLE PRECISION DSUPINTW(NKR),DD1N,DB11_MY,DAL1,DAL2
       DOUBLE PRECISION COL3,RORI,TPN,TPS,QPN,QPS,TOLD,QOLD &
      &                  ,FI1_K,FI2_K,FI3_K,FI4_K,FI5_K &
      &                  ,R1_K,R2_K,R3_K,R4_K,R5_K &
      &                  ,FI1R1,FI2R2,FI3R3,FI4R4,FI5R5 &
      &                  ,RMASSLAA,RMASSLBB,RMASSIAA,RMASSIBB &
      &                  ,ES1N,ES2N,EW1N,ARGEXP &
      &                  ,TT,QQ,PP &
      &                  ,DEL1N,DEL2N,DIV1,DIV2 &
      &                  ,OPER2,OPER3,AR1,AR2

        DOUBLE PRECISION DELMASSL1

 ! DROPLETS

         REAL R1(NKR) &
      &           ,RLEC(NKR),RO1BL(NKR) &
      &           ,FI1(NKR),FF1(NKR),PSI1(NKR) &
      &           ,B11_MY(NKR),B12_MY(NKR)

 ! WORK ARRAYS

 ! NEW ALGORITHM OF MIXED PHASE FOR EVAPORATION


 	REAL DTIMEO(NKR),DTIMEL(NKR) &
      &           ,TIMESTEPD(NKR)

 ! NEW ALGORITHM (NO TYPE OF ICE)

 	REAL :: FL1(NKR), sfndummy(3), R1N(NKR)
 	INTEGER :: IDROP

 	DOUBLE PRECISION :: R1D(NKR),R1ND(NKR)

 	OPER2(AR1)=0.622/(0.622+0.378*AR1)/AR1
 	OPER3(AR1,AR2)=AR1*AR2/(0.622+0.378*AR1)

 	DATA AL1 /2500./, AL2 /2834./, D /0.211/ &
      &      ,GAM /1.E-4/, POD /10./

 	DATA RV_MY,CF_MY,D_MYIN,AL1_MY,AL2_MY &
      &      /461.5,0.24E-1,0.211E-4,2.5E6,2.834E6/

 	DATA A1_MYN, BB1_MYN, A2_MYN, BB2_MYN &
      &      /2.53,5.42,3.41E1,6.13/

 	DATA TPC1, TPC2, TPC3, TPC4, TPC5 &
      &      /-4.0,-8.1,-12.7,-17.8,-22.4/


 	DATA EPSDEL, EPSDEL2 /0.1E-03,0.1E-03/

 	DATA DT0L, DT0I /1.E20,1.E20/

 	DOUBLE PRECISION :: DEL1_d , DEL2_d, RW_d , PW_d, RI_d, PI_d, D1N_d, D2N_d, &
 						VR1_d(NKR)

 sfndummy = 0.0
 B12_MY = 0.0
 B11_MY = 0.0

  I_ABERGERON=0
  I_BERGERON=0
  COL3=3.0*COL
 ITIME=0
 KCOND=0
 DT_WATER_COND=0.4
 DT_WATER_EVAP=0.4
 ITIME=0
 KCOND=0
 DT0LREF=0.2
 DTLREF=0.4

 NR=NKR
 NRM=NKR-1
 DT=DTCOND
 DTT=DTCOND
 XRAD=0.

  CWHUCM=0.
 XRAD=0.
 B6=CWHUCM*GAM-XRAD
 B8L=1./ROR
 B8I=1./ROR
 RORI=1./ROR

 DO KR=1,NKR
    FF1_OLD(KR)=FF1(KR)
    SUPINTW(KR)=0.0
    DSUPINTW(KR)=0.0
 ENDDO

 TPN=TT
 QPN=QQ
 DO KR=1,NKR
     FI1(KR)=FF1(KR)
 END DO

 ! WARM MP (CONDENSATION OR EVAPORATION) (BEGIN)
 TIMENEW=0.
 ITIME=0

 TOLD = TPN
 QOLD = QPN
 R1D = R1
 R1ND = R1D
 SFNL = 0.0
 SFN11 = 0.0

 56  ITIME = ITIME+1
 TIMEREV = DT-TIMENEW
 TIMEREV = DT-TIMENEW
 DEL1 = DEL1N
 DEL2 = DEL2N
 DEL1S = DEL1N
 DEL2S = DEL2N
 TPS = TPN
 QPS = QPN

 IF(ISYM1 == 1)THEN
 	FL1 = 0.0
 	VR1_d = VR1
 	CALL JERRATE_KS &
 				(R1D,TPS,PP,VR1_d,RLEC,RO1BL,B11_MY,1,1,fl1,NKR,ICEMAX)
 	sfndummy(1)=SFN11
 	CALL JERTIMESC_KS(FI1,R1D,SFNDUMMY,B11_MY,B8L,1,NKR,ICEMAX,COL)
 	SFN11 = sfndummy(1)
 ENDIF

 SFN12 = 0.0
 SFNL = SFN11 + SFN12
 SFNI = 0.

 B5L=BB1_MY/TPS/TPS
 B5I=BB2_MY/TPS/TPS
 B7L=B5L*B6
 B7I=B5I*B6
 DOPL=1.+DEL1S
 DOPI=1.+DEL2S
 RW=(OPER2(QPS)+B5L*AL1)*DOPL*SFNL
 RI=(OPER2(QPS)+B5L*AL2)*DOPL*SFNI
 QW=B7L*DOPL
 PW=(OPER2(QPS)+B5I*AL1)*DOPI*SFNL
 PI=(OPER2(QPS)+B5I*AL2)*DOPI*SFNI
 QI=B7I*DOPI

 IF(RW.NE.RW .or. PW.NE.PW)THEN
    print*, 'NaN In ONECOND1'
    call wrf_error_fatal("fatal error in ONECOND1 (RW or PW are NaN), model stop")
 ENDIF

 KCOND=10
 IF(DEL1N >= 0.0D0) KCOND=11

   IF(KCOND == 11) THEN
   	  DTNEWL = DT
      DTNEWL = DT
      DTNEWL = AMIN1(DTNEWL,TIMEREV)
      TIMENEW = TIMENEW + DTNEWL
      DTT = DTNEWL

   	  IF (DTT < 0.0) call wrf_error_fatal("fatal error in ONECOND1-DEL1N>0:(DTT<0), model stop")

     	DEL1_d = DEL1
     	DEL2_d = DEL2
     	RW_d = RW
     	PW_d = PW
     	RI_d = RI
     	PI_d = PI

 	    CALL JERSUPSAT_KS(DEL1_d,DEL2_d,DEL1N,DEL2N, &
             					  RW_d,PW_d,RI_d,PI_d, &
             					  DTT,D1N_d,D2N_d,0.0,0.0, &
             					  ISYM1,ISYM2,ISYM3,ISYM4,ISYM5)
     	DEL1 = DEL1_d
     	DEL2 = DEL2_d
     	RW = RW_d
     	PW = PW_d
     	RI = RI_d
     	PI = PI_d
     	D1N = D1N_d
     	D2N = D2N_d

     	IF(ISYM1 == 1)THEN
     		IDROP = ISYM1
     		CALL JERDFUN_KS(R1D, R1ND, B11_MY, FI1, PSI1, fl1, D1N, &
     						        ISYM1, 1, 1, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 1, Iin, Jin ,Kin, Itimestep)
     	ENDIF

     	IF((DEL1.GT.0.AND.DEL1N.LT.0) &
       		&.AND.ABS(DEL1N).GT.EPSDEL) THEN
             		call wrf_error_fatal("fatal error in ONECOND1-1 (DEL1.GT.0.AND.DEL1N.LT.0), model stop")
     	ENDIF

    ! IN CASE : KCOND.EQ.11
    ELSE

 	    ! EVAPORATION - ONLY WATER
 	    ! IN CASE : KCOND.NE.11
    	DTIMEO = DT
      DTNEWL = DT
      DTNEWL = AMIN1(DTNEWL,TIMEREV)
      TIMENEW = TIMENEW + DTNEWL
      DTT = DTNEWL

 	    IF (DTT < 0.0) call wrf_error_fatal("fatal error in ONECOND1-DEL1N<0:(DTT<0), model stop")

 	    DEL1_d = DEL1
 	    DEL2_d = DEL2
 	    RW_d = RW
 	    PW_d = PW
 	    RI_d = RI
 	    PI_d = PI
 	    CALL JERSUPSAT_KS(DEL1_d,DEL2_d,DEL1N,DEL2N, &
 					  RW_d,PW_d,RI_d,PI_d, &
 					  DTT,D1N_d,D2N_d,0.0,0.0, &
 					  ISYM1,ISYM2,ISYM3,ISYM4,ISYM5)
     	DEL1 = DEL1_d
     	DEL2 = DEL2_d
     	RW = RW_d
     	PW = PW_d
     	RI = RI_d
     	PI = PI_d
     	D1N = D1N_d
     	D2N = D2N_d

      IF(ISYM1 == 1)THEN
 	      IDROP = ISYM1
 	      CALL JERDFUN_KS(R1D, R1ND, B11_MY, &
				              FI1, PSI1, fl1, D1N, &
 					            ISYM1, 1, 1, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 1, Iin, Jin ,Kin, Itimestep)
      ENDIF

      IF((DEL1.LT.0.AND.DEL1N.GT.0) &
        .AND.ABS(DEL1N).GT.EPSDEL) THEN
         call wrf_error_fatal("fatal error in ONECOND1-2 (DEL1.LT.0.AND.DEL1N.GT.0), model stop")
      ENDIF

    ENDIF


 RMASSLBB=0.
 RMASSLAA=0.

 ! ... before JERNEWF (ONLY WATER)
 DO K=1,NKR
  FI1_K = FI1(K)
  R1_K = R1(K)
  FI1R1 = FI1_K*R1_K*R1_K
  RMASSLBB = RMASSLBB+FI1R1
 ENDDO
 RMASSLBB = RMASSLBB*COL3*RORI
 IF(RMASSLBB.LE.0.) RMASSLBB=0.
 ! ... after JERNEWF (ONLY WATER)
 DO K=1,NKR
  FI1_K=PSI1(K)
  R1_K=R1(K)
  FI1R1=FI1_K*R1_K*R1_K
  RMASSLAA=RMASSLAA+FI1R1
 END DO
 RMASSLAA=RMASSLAA*COL3*RORI
 IF(RMASSLAA.LE.0.) RMASSLAA=0.

 DELMASSL1 = RMASSLAA - RMASSLBB
 QPN = QPS - DELMASSL1
 DAL1 = AL1
 TPN = TPS + DAL1*DELMASSL1

 IF(ABS(DAL1*DELMASSL1) > 3.0 )THEN
 	print*,"ONECOND1-in(start)"
	print*,"I=",Iin,"J=",Jin,"Kin",Kin,"W",w_in,"DX",dx_in
 	print*,"DELMASSL1",DELMASSL1,"DT",DTT
 	print*,"DEL1N,DEL2N,DEL1,DEL2,D1N,D2N,RW,PW,RI,PI,DT"
 	print*,DEL1N,DEL2N,DEL1,DEL2,D1N,D2N,RW,PW,RI,PI,DTT
 	print*,"TPS",TPS,"QPS",QPS
	print*,'FI1 before',FI1,'PSI1 after',PSI1
 	print*,"ONECOND1-in(end)"
 	call wrf_error_fatal("fatal error in ONECOND1-in (ABS(DAL1*DELMASSL1) > 3.0), model stop")
 ENDIF

 ! ... SUPERSATURATION (ONLY WATER)
 ARGEXP=-BB1_MY/TPN
 ES1N=AA1_MY*DEXP(ARGEXP)
 ARGEXP=-BB2_MY/TPN
 ES2N=AA2_MY*DEXP(ARGEXP)
 EW1N=OPER3(QPN,PP)
 IF(ES1N == 0.0D0)THEN
          DEL1N=0.5
          DIV1=1.5
 ELSE
          DIV1 = EW1N/ES1N
          DEL1N = EW1N/ES1N-1.
 END IF
 IF(ES2N == 0.0D0)THEN
          DEL2N=0.5
          DIV2=1.5
 ELSE
          DEL2N = EW1N/ES2N-1.
          DIV2 = EW1N/ES2N
 END IF
 IF(ISYM1 == 1) THEN
 	DO KR=1,NKR
           SUPINTW(KR)=SUPINTW(KR)+B11_MY(KR)*D1N
           DD1N=D1N
           DB11_MY=B11_MY(KR)
           DSUPINTW(KR)=DSUPINTW(KR)+DB11_MY*DD1N
 	ENDDO
 ENDIF

 ! ... REPEATE TIME STEP (ONLY WATER: CONDENSATION OR EVAPORATION)
 IF(TIMENEW.LT.DT) GOTO 56

 57  CONTINUE

 IF(ISYM1 == 1) THEN
    CALL JERDFUN_NEW_KS (R1D,R1ND,SUPINTW, &
 					FF1_OLD,PSI1, &
 					TPN,IDROP,FR_LIM, NKR, COL,1,Iin,Jin,Kin,Itimestep)
 ENDIF ! in case ISYM1/=0

 RMASSLAA=0.0
 RMASSLBB=0.0

 DO K=1,NKR
  FI1_K=FF1_OLD(K)
  R1_K=R1(K)
  FI1R1=FI1_K*R1_K*R1_K
  RMASSLBB=RMASSLBB+FI1R1
 ENDDO
 RMASSLBB=RMASSLBB*COL3*RORI
 IF(RMASSLBB.LT.0.0) RMASSLBB=0.0

 DO K=1,NKR
  FI1_K=PSI1(K)
  R1_K=R1(K)
  FI1R1=FI1_K*R1_K*R1_K
  RMASSLAA=RMASSLAA+FI1R1
 ENDDO
 RMASSLAA=RMASSLAA*COL3*RORI
 IF(RMASSLAA.LT.0.0) RMASSLAA=0.0
 DELMASSL1 = RMASSLAA-RMASSLBB

 QPN = QOLD - DELMASSL1
 DAL1 = AL1
 TPN = TOLD + DAL1*DELMASSL1

 IF(ABS(DAL1*DELMASSL1) > 5.0 )THEN
 	print*,"ONECOND1-out (start)"
 	print*,"I=",Iin,"J=",Jin,"Kin",Kin,"W",w_in,"DX",dx_in
 	print*,"DEL1N,DEL2N,D1N,D2N,RW,PW,RI,PI,DT"
 	print*,DEL1N,DEL2N,D1N,D2N,RW,PW,RI,PI,DTT
 	print*,"I=",Iin,"J=",Jin,"Kin",Kin
 	print*,"TPS=",TPS,"QPS=",QPS,"delmassl1",delmassl1
 	print*,"DAL1=",DAL1
 	print*,RMASSLBB,RMASSLAA
 	print*,"FI1",FI1
 	print*,"PSI1",PSI1
 	print*,"ONECOND1-out (end)"
 	IF(ABS(DAL1*DELMASSL1) > 5.0 )THEN
 		call wrf_error_fatal("fatal error in ONECOND1-out (ABS(DAL1*DELMASSL1) > 5.0), model stop")
 	ENDIF
 ENDIF

 ! ... SUPERSATURATION
 ARGEXP=-BB1_MY/TPN
 ES1N=AA1_MY*DEXP(ARGEXP)
 ARGEXP=-BB2_MY/TPN
 ES2N=AA2_MY*DEXP(ARGEXP)
 EW1N=OPER3(QPN,PP)
 IF(ES1N == 0.0D0)THEN
  	DEL1N=0.5
  	DIV1=1.5
 	call wrf_error_fatal("fatal error in ONECOND1 (ES1N.EQ.0), model stop")
 ELSE
    DIV1=EW1N/ES1N
    DEL1N=EW1N/ES1N-1.
 END IF
 IF(ES2N.EQ.0)THEN
    DEL2N=0.5
    DIV2=1.5
   call wrf_error_fatal("fatal error in ONECOND1 (ES2N.EQ.0), model stop")
 ELSE
    DEL2N=EW1N/ES2N-1.
    DIV2=EW1N/ES2N
 END IF

 TT=TPN
 QQ=QPN
 DO KR=1,NKR
  FF1(KR)=PSI1(KR)
 ENDDO

 RETURN
 END SUBROUTINE ONECOND1
 ! +----------------------------------------------------------------------------+
 SUBROUTINE ONECOND2 &
 					 & (TT,QQ,PP,ROR  &
 					 & ,VR2,VR3,VR4,VR5,PSINGLE &
 					 & ,DEL1N,DEL2N,DIV1,DIV2 &
 					 & ,FF2,PSI2,R2,RIEC,RO2BL &
 					 & ,FF3,PSI3,R3,RSEC,RO3BL &
 					 & ,FF4,PSI4,R4,RGEC,RO4BL &
 					 & ,FF5,PSI5,R5,RHEC,RO5BL &
 					 & ,AA1_MY,BB1_MY,AA2_MY,BB2_MY &
 					 & ,C1_MEY,C2_MEY &
 					 & ,COL,DTCOND,ICEMAX,NKR &
 					 & ,ISYM1,ISYM2,ISYM3,ISYM4,ISYM5, &
 					 	Iin,Jin,Kin,W_in,DX_in,Itimestep)

    IMPLICIT NONE

       INTEGER NKR,ICEMAX,ISYM1, Iin, Jin, Kin, Itimestep
       REAL    COL,VR2(NKR,ICEMAX),VR3(NKR),VR4(NKR) &
      &           ,VR5(NKR),PSINGLE &
      &       ,AA1_MY,BB1_MY,AA2_MY,BB2_MY &
      &       ,DTCOND,W_in,DX_in

       REAL C1_MEY,C2_MEY
       INTEGER I_MIXCOND,I_MIXEVAP,I_ABERGERON,I_BERGERON, &
      & KR,ICE,ITIME,ICM,KCOND,NR,NRM,INUC, &
      & ISYM2(ICEMAX),ISYM3,ISYM4,ISYM5,KP,KLIMIT, &
      & KM,ITER,KLIMITL,KLIMITG,KLIMITH,KLIMITI_1,KLIMITI_2,KLIMITI_3, &
      & NCRITI
       REAL AL1,AL2,D,GAM,POD, &
      & RV_MY,CF_MY,D_MYIN,AL1_MY,AL2_MY,ALC,DT0LREF,DTLREF, &
      & A1_MYN, BB1_MYN, A2_MYN, BB2_MYN,DT,DTT,XRAD, &
      & TPC1, TPC2, TPC3, TPC4, TPC5, &
      & EPSDEL, DT0L, DT0I, &
      & ROR, &
      & DEL1NUC,DEL2NUC, &
      & CWHUCM,B6,B8L,B8I,RMASSGL,RMASSGI, &
      & DEL1,DEL2,DEL1S,DEL2S, &
      & TIMENEW,TIMEREV,SFN11,SFN12, &
      & SFNL,SFNI,B5L,B5I,B7L,B7I,DOPL,DOPI,OPERQ,RW,RI,QW,PW, &
      & PI,QI,D1N0,D2N0,DTNEWL,DTNEWL1,D1N,D2N, &
      & DEL_R1,DT0L0,DT0I0,SFN31,SFN32,SFN52, &
      & SFNII1,SFN21,SFN22,DTNEWI3,DTNEWI4,DTNEWI5,DTNEWI2_1, &
      & DTNEWI2_2,DTNEWI1,DEL_R2,DEL_R4,DEL_R5,SFN41,SFN42, &
      & SNF51,DTNEWI2_3,DTNEWI2,DTNEWI_1,DTNEWI_2, &
      & DTNEWL0,DTNEWG1,DTNEWH1,DTNEWI_3, &
      & DTNEWL2,SFN51,SFNII2,DEL_R3,DTNEWI
        REAL DT_WATER_COND,DT_WATER_EVAP,DT_ICE_COND,DT_ICE_EVAP, &
      &  DT_MIX_COND,DT_MIX_EVAP,DT_MIX_BERGERON,DT_MIX_ANTIBERGERON

        INTEGER K

       DOUBLE PRECISION DD1N,DB11_MY,DAL1,DAL2
       DOUBLE PRECISION COL3,RORI,TPN,TPS,QPN,QPS,TOLD,QOLD &
      &                  ,FI1_K,FI2_K,FI3_K,FI4_K,FI5_K &
      &                  ,R1_K,R2_K,R3_K,R4_K,R5_K &
      &                  ,FI1R1,FI2R2,FI3R3,FI4R4,FI5R5 &
      &                  ,RMASSLAA,RMASSLBB,RMASSIAA,RMASSIBB &
      &                  ,ES1N,ES2N,EW1N,ARGEXP &
      &                  ,TT,QQ,PP &
      &                  ,DEL1N,DEL2N,DIV1,DIV2 &
      &                  ,OPER2,OPER3,AR1,AR2

        DOUBLE PRECISION DELTAQ1,DELMASSI1,DELMASSL1

         CHARACTER*70 CPRINT

 ! CRYSTALS

 	REAL R2(NKR,ICEMAX) &
      &           ,RIEC(NKR,ICEMAX) &
      &           ,RO2BL(NKR,ICEMAX) &
      &           ,FI2(NKR,ICEMAX),PSI2(NKR,ICEMAX) &
      &           ,FF2(NKR,ICEMAX) &
      &           ,B21_MY(NKR,ICEMAX),B22_MY(NKR,ICEMAX)

 ! SNOW
         REAL R3(NKR) &
      &           ,RSEC(NKR),RO3BL(NKR) &
      &           ,FI3(NKR),FF3(NKR),PSI3(NKR) &
      &           ,B31_MY(NKR),B32_MY(NKR)

 ! GRAUPELS

         REAL R4(NKR) &
      &           ,RGEC(NKR),RO4BL(NKR) &
      &           ,FI4(NKR),FF4(NKR),PSI4(NKR) &
      &           ,B41_MY(NKR),B42_MY(NKR)

 ! HAIL
         REAL R5(NKR) &
      &           ,RHEC(NKR),RO5BL(NKR) &
      &           ,FI5(NKR),FF5(NKR),PSI5(NKR) &
      &           ,B51_MY(NKR),B52_MY(NKR)

 ! CCN

 	REAL DTIMEG(NKR),DTIMEH(NKR)

 	REAL DEL2D(ICEMAX),DTIMEO(NKR),DTIMEL(NKR) &

      &           ,DTIMEI_1(NKR),DTIMEI_2(NKR),DTIMEI_3(NKR) &
      &           ,SFNI1(ICEMAX),SFNI2(ICEMAX) &
      &           ,TIMESTEPD(NKR) &
      &           ,FI1REF(NKR),PSI1REF(NKR) &
      &           ,FI2REF(NKR,ICEMAX),PSI2REF(NKR,ICEMAX)&
      &           ,FCCNRREF(NKR)

 	REAL :: FL1(NKR), sfndummy(3), FL3(NKR), FL4(NKR), FL5(NKR), &
 					R2N(NKR,ICEMAX), R3N(NKR), R4N(NKR), R5N(NKR)
 	INTEGER :: IDROP, ISYMICE
 	DOUBLE PRECISION :: R2D(NKR,ICEMAX),R3D(NKR), R4D(NKR), R5D(NKR), &
 			    R2ND(NKR,ICEMAX),R3ND(NKR), R4ND(NKR), R5ND(NKR), &
 			    VR2_d(NKR,ICEMAX), VR3_d(NKR), VR4_d(NKR), VR5_d(NKR)

 	OPER2(AR1)=0.622/(0.622+0.378*AR1)/AR1
 	OPER3(AR1,AR2)=AR1*AR2/(0.622+0.378*AR1)

 	DATA AL1 /2500./, AL2 /2834./, D /0.211/ &
      &      ,GAM /1.E-4/, POD /10./

 	DATA RV_MY,CF_MY,D_MYIN,AL1_MY,AL2_MY &
      &      /461.5,0.24E-1,0.211E-4,2.5E6,2.834E6/

 	DATA A1_MYN, BB1_MYN, A2_MYN, BB2_MYN &
      &      /2.53,5.42,3.41E1,6.13/

 	DATA TPC1, TPC2, TPC3, TPC4, TPC5 &
      &      /-4.0,-8.1,-12.7,-17.8,-22.4/

 	DATA EPSDEL/0.1E-03/

 	DATA DT0L, DT0I /1.E20,1.E20/

 	DOUBLE PRECISION :: DEL1_d, DEL2_d, RW_d, PW_d, RI_d, PI_d, D1N_d, D2N_d

 	B22_MY = 0.0
 	B32_MY = 0.0
 	B42_MY = 0.0
 	B52_MY = 0.0

 	B21_MY = 0.0
 	B31_MY = 0.0
 	B41_MY = 0.0
 	B51_MY = 0.0

 	SFNDUMMY = 0.0
 	R2D = R2
 	R3D = R3
 	R4D = R4
 	R5D = R5
 	R2ND = R2D
 	R3ND = R3D
 	R4ND = R4D
 	R5ND = R5D

 	SFNI1 = 0.0
 	SFN31 = 0.0
 	SFN41 = 0.0
 	SFN51 = 0.0

 	I_MIXCOND=0
 	I_MIXEVAP=0
 	I_ABERGERON=0
 	I_BERGERON=0
 	COL3=3.0*COL
 	ICM=ICEMAX
 	ITIME=0
 	KCOND=0
 	DT_WATER_COND=0.4
 	DT_WATER_EVAP=0.4
 	DT_ICE_COND=0.4
 	DT_ICE_EVAP=0.4
 	DT_MIX_COND=0.4
 	DT_MIX_EVAP=0.4
 	DT_MIX_BERGERON=0.4
 	DT_MIX_ANTIBERGERON=0.4
 	ICM=ICEMAX
 	ITIME=0
 	KCOND=0
 	DT0LREF=0.2
 	DTLREF=0.4

 	NR=NKR
 	NRM=NKR-1
 	DT=DTCOND
 	DTT=DTCOND
 	XRAD=0.

 	CWHUCM=0.
 	XRAD=0.
 	B6=CWHUCM*GAM-XRAD
 	B8L=1./ROR
 	B8I=1./ROR
 	RORI=1./ROR

 	TPN=TT
 	QPN=QQ

   	DO ICE=1,ICEMAX
 		 SFNI1(ICE)=0.
 		 SFNI2(ICE)=0.
 		 DEL2D(ICE)=0.
   	ENDDO

   	TIMENEW = 0.
   	ITIME = 0

 ! ONLY ICE (CONDENSATION OR EVAPORATION) :

   46 ITIME = ITIME + 1

 	  TIMEREV=DT-TIMENEW

 	  DEL1=DEL1N
 	  DEL2=DEL2N
 	  DEL1S=DEL1N
 	  DEL2S=DEL2N
 	  DEL2D(1)=DEL2N
 	  DEL2D(2)=DEL2N
 	  DEL2D(3)=DEL2N
 	  TPS=TPN
 	  QPS=QPN
 	  DO KR=1,NKR
 		 FI3(KR)=PSI3(KR)
 		 FI4(KR)=PSI4(KR)
 		 FI5(KR)=PSI5(KR)
 		 DO ICE=1,ICEMAX
 			FI2(KR,ICE)=PSI2(KR,ICE)
 		 ENDDO
 	  ENDDO

 	  IF(sum(ISYM2) > 0) THEN
 	    FL1 = 0.0
 	    VR2_d = VR2
 	  ! ... ice crystals
 		  CALL JERRATE_KS (R2D,TPS,PP,VR2_d,RIEC,RO2BL,B21_MY,3,2,fl1,NKR,ICEMAX)

 		  CALL JERTIMESC_KS (FI2,R2D,SFNI1,B21_MY,B8I,ICM,NKR,ICEMAX,COL)
 	  ENDIF
 	  IF(ISYM3 == 1) THEN
 	    FL3 = 0.0
 	    VR3_d = VR3
 	  ! ... snow
 		  CALL JERRATE_KS (R3D,TPS,PP,VR3_d,RSEC,RO3BL,B31_MY,1,3,fl3,NKR,ICEMAX)

 		  sfndummy(1) = SFN31
 		  CALL JERTIMESC_KS(FI3,R3D,SFNDUMMY,B31_MY,B8I,1,NKR,ICEMAX,COL)
 	  	  SFN31 = sfndummy(1)
 	  ENDIF
 	  IF(ISYM4 == 1) THEN
 	    FL4 = 0.0
 	    VR4_d = VR4
 	  ! ... graupel
 		  CALL JERRATE_KS(R4D,TPS,PP,VR4_d,RGEC,RO4BL,B41_MY,1,2,fl4,NKR,ICEMAX)

 		  sfndummy(1) = SFN41
 		  CALL JERTIMESC_KS(FI4,R4D,SFNDUMMY,B41_MY,B8I,1,NKR,ICEMAX,COL)
 	  	  SFN41 = sfndummy(1)
 	  ENDIF
 	  IF(ISYM5 == 1) THEN
 	    FL5 = 0.0
 	    VR5_d = VR5
 	  ! ... hail
 		  CALL JERRATE_KS(R5D,TPS,PP,VR5_d,RHEC,RO5BL,B51_MY,1,2,fl5,NKR,ICEMAX)

 		  sfndummy(1) = SFN51
 		  CALL JERTIMESC_KS(FI5,R5D,SFNDUMMY,B51_MY,B8I,1,NKR,ICEMAX,COL)
 	  	  SFN51 = sfndummy(1)
 	  ENDIF


 	  SFNII1 = SFNI1(1) + SFNI1(2) + SFNI1(3)
 	  SFN21 = SFNII1 + SFN31 + SFN41 + SFN51
 	  SFNL = 0.0
 	  SFN22 = 0.0
 	  SFNI = SFN21 + SFN22

   	B5L=BB1_MY/TPS/TPS
   	B5I=BB2_MY/TPS/TPS
   	B7L=B5L*B6
   	B7I=B5I*B6
   	DOPL=1.+DEL1S
   	DOPI=1.+DEL2S
   	OPERQ=OPER2(QPS)
   	RW=(OPERQ+B5L*AL1)*DOPL*SFNL
   	QW=B7L*DOPL
   	PW=(OPERQ+B5I*AL1)*DOPI*SFNL
   	RI=(OPERQ+B5L*AL2)*DOPL*SFNI
   	PI=(OPERQ+B5I*AL2)*DOPI*SFNI
   	QI=B7I*DOPI

     KCOND=20
     IF(DEL2N > 0.0) KCOND=21

   	IF(RW.NE.RW .or. PW.NE.PW)THEN
 	    print*, 'NaN In ONECOND2'
 	    call wrf_error_fatal("fatal error in ONECOND2 (RW or PW are NaN), model stop")
   	ENDIF

 ! ... (ONLY ICE)
   	IF(KCOND == 21)  THEN
 			! ... ONLY_ICE: CONDENSATION
       DTNEWL = DT
       DTNEWL = AMIN1(DTNEWL,TIMEREV)
       TIMENEW = TIMENEW + DTNEWL
       DTT = DTNEWL

 			IF (DTT < 0.0) call wrf_error_fatal("fatal error in ONECOND2-DEL2N>0:(DTT<0), model stop")

 			DEL1_d = DEL1
 			DEL2_d = DEL2
 			RW_d = RW
 			PW_d = PW
 			RI_d = RI
 			PI_d = PI
 			CALL JERSUPSAT_KS(DEL1_d,DEL2_d,DEL1N,DEL2N, &
 											  RW_d,PW_d,RI_d,PI_d, &
 											  DTT,D1N_d,D2N_d,0.0,0.0, &
 											  ISYM1,ISYM2,ISYM3,ISYM4,ISYM5)
 			DEL1 = DEL1_d
 			DEL2 = DEL2_d
 			RW = RW_d
 			PW = PW_d
 			RI = RI_d
 			PI = PI_d
 			D1N = D1N_d
 			D2N = D2N_d

 			IF(sum(ISYM2) > 0)THEN
 				IDROP = 0
 				FL1 = 0.0
 				IF(ISYM2(1) == 1) THEN
 				  CALL JERDFUN_KS(R2D(:,1), R2ND(:,1), B21_MY(:,1), &
 								  FI2(:,1), PSI2(:,1), fl1, D2N, &
 								  ISYM2(1), ICM, 1, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 21, Iin, Jin ,Kin, Itimestep)
 				ENDIF
 				IF(ISYM2(2) == 1) THEN
 				  CALL JERDFUN_KS(R2D(:,2), R2ND(:,2), B21_MY(:,2), &
 								  FI2(:,2), PSI2(:,2), fl1, D2N, &
 								  ISYM2(2), ICM, 2, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 22, Iin, Jin ,Kin, Itimestep)
 				ENDIF
 				IF(ISYM2(3) == 1) THEN
 				  CALL JERDFUN_KS(R2D(:,3), R2ND(:,3), B21_MY(:,3), &
 								  FI2(:,3), PSI2(:,3), fl1, D2N, &
 								  ISYM2(3), ICM, 3, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 23, Iin, Jin ,Kin, Itimestep)

 				! IN CASE : ISYM2.NE.0
 				ENDIF
 			ENDIF

 			IF(ISYM3 == 1) THEN
 				IDROP = 0
 				FL3 = 0.0
 				CALL JERDFUN_KS(R3D, R3ND, B31_MY, &
 								FI3, PSI3, fl3, D2N, &
 								ISYM3, 1, 3, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 3, Iin, Jin ,Kin, Itimestep)
 			ENDIF


 			IF(ISYM4 == 1) THEN
 			 	IDROP = 0
 			 	FL4 = 0.0
 			 	CALL JERDFUN_KS(R4D, R4ND, B41_MY, &
 								FI4, PSI4, fl4, D2N, &
 								ISYM4, 1, 4, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 4, Iin, Jin ,Kin, Itimestep)
 				! IN CASE : ISYM4.NE.0
 			ENDIF

 			IF(ISYM5 == 1) THEN
 			 IDROP = 0
 			 FL5 = 0.0
 			 CALL JERDFUN_KS(R5D, R5ND, B51_MY, &
 							FI5, PSI5, fl5, D2N, &
 							ISYM5, 1, 5, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 5, Iin, Jin ,Kin, Itimestep)
 			! IN CASE : ISYM5.NE.0
 			ENDIF

 			IF((DEL2.GT.0.AND.DEL2N.LT.0) &
          		.AND.ABS(DEL2N).GT.EPSDEL) THEN
                 call wrf_error_fatal("fatal error in module_mp_fast_sbm (DEL2.GT.0.AND.DEL2N.LT.0), model stop")
 			ENDIF

 	  ELSE
 	  ! ... IN CASE KCOND.NE.21
 			! ONLY ICE: EVAPORATION
        DTNEWL = DT
        DTNEWL = AMIN1(DTNEWL,TIMEREV)
        TIMENEW = TIMENEW + DTNEWL
        DTT = DTNEWL

 			  IF (DTT < 0.0) call wrf_error_fatal("fatal error in ONECOND2-DEL2N<0:(DTT<0), model stop")

   			DEL1_d = DEL1
   			DEL2_d = DEL2
   			RW_d = RW
   			PW_d = PW
   			RI_d = RI
   			PI_d = PI
   			CALL JERSUPSAT_KS(DEL1_d,DEL2_d,DEL1N,DEL2N, &
   												RW_d,PW_d,RI_d,PI_d, &
   					  						DTT,D1N_d,D2N_d,0.0,0.0, &
   						  					ISYM1,ISYM2,ISYM3,ISYM4,ISYM5)
   		 	DEL1 = DEL1_d
   			DEL2 = DEL2_d
   			RW = RW_d
   			PW = PW_d
   			RI = RI_d
   			PI = PI_d
   			D1N = D1N_d
   			D2N = D2N_d

 			IF(sum(ISYM2) > 0) THEN
 			  IDROP = 0
 			  FL1 = 0.0
 			  IF(ISYM2(1)==1)THEN
 			  	CALL JERDFUN_KS(R2D(:,1), R2ND(:,1), B21_MY(:,1), &
 							    FI2(:,1), PSI2(:,1), fl1, D2N, &
 							    ISYM2(1), ICM, 1, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 21, Iin, Jin ,Kin, Itimestep)
 		  	  ENDIF
 			  IF(ISYM2(2)==1)THEN
 		      	CALL JERDFUN_KS(R2D(:,2), R2ND(:,2), B21_MY(:,2), &
 							    FI2(:,2), PSI2(:,2), fl1, D2N, &
 								ISYM2(2), ICM, 2, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 22, Iin, Jin ,Kin, Itimestep)
 		      ENDIF
 			  IF(ISYM2(3)==1)THEN
 		      	CALL JERDFUN_KS(R2D(:,3), R2ND(:,3), B21_MY(:,3), &
 								FI2(:,3), PSI2(:,3), fl1, D2N, &
 							    ISYM2(3), ICM, 3, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 23, Iin, Jin ,Kin, Itimestep)
 			 ENDIF
 			ENDIF

       IF(ISYM3 == 1) THEN
 			! ... SNOW
 				IDROP = 0
 				FL3 = 0.0
 				CALL JERDFUN_KS(R3D, R3ND, B31_MY, &
 								FI3, PSI3, fl3, D2N, &
 								ISYM3, 1, 3, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 3, Iin, Jin ,Kin, Itimestep)
 			! IN CASE : ISYM3.NE.0
       ENDIF

     IF(ISYM4 == 1) THEN
     ! ... GRAUPELS (ONLY_ICE: EVAPORATION)
         ! ... New JERDFUN
         IDROP = 0
         FL4 = 0.0
         CALL JERDFUN_KS(R4D, R4ND, B41_MY, &
                         FI4, PSI4, fl4, D2N, &
                         ISYM4, 1, 4, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 4, Iin, Jin ,Kin, Itimestep)
     ! IN CASE : ISYM4.NE.0
     ENDIF

       IF(ISYM5 == 1) THEN
         ! ... HAIL (ONLY_ICE: EVAPORATION)
           ! ... New JERDFUN
           IDROP = 0
           FL5 = 0.0
           CALL JERDFUN_KS(R5D, R5ND, B51_MY, &
                           FI5, PSI5, fl5, D2N, &
                           ISYM5, 1, 5, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 5, Iin, Jin ,Kin, Itimestep)
             ! IN CASE : ISYM5.NE.0
       ENDIF

       IF((DEL2.LT.0.AND.DEL2N.GT.0) &
            .AND.ABS(DEL2N).GT.EPSDEL) THEN
             call wrf_error_fatal("fatal error in module_mp_fast_sbm (DEL2.LT.0.AND.DEL2N.GT.0), model stop")
       ENDIF

 		 ! IN CASE : KCOND.NE.21
     ENDIF

 ! MASSES
 	  RMASSIBB=0.0
 	  RMASSIAA=0.0

 	  DO K=1,NKR
 		 DO ICE = 1,ICEMAX
 			FI2_K = FI2(K,ICE)
 			R2_K = R2(K,ICE)
 			FI2R2 = FI2_K*R2_K*R2_K
 			RMASSIBB = RMASSIBB + FI2R2
 	 	 ENDDO
 		 FI3_K=FI3(K)
 		 FI4_K=FI4(K)
 		 FI5_K=FI5(K)
 		 R3_K=R3(K)
 		 R4_K=R4(K)
 		 R5_K=R5(K)
 		 FI3R3=FI3_K*R3_K*R3_K
 		 FI4R4=FI4_K*R4_K*R4_K
 		 FI5R5=FI5_K*R5_K*R5_K
 		 RMASSIBB=RMASSIBB+FI3R3
 		 RMASSIBB=RMASSIBB+FI4R4
 		 RMASSIBB=RMASSIBB+FI5R5
 	  ENDDO
 	  RMASSIBB=RMASSIBB*COL3*RORI
 	  IF(RMASSIBB.LT.0.0) RMASSIBB=0.0

 	  DO K=1,NKR
 		 DO ICE =1,ICEMAX
 			FI2_K=PSI2(K,ICE)
 			R2_K=R2(K,ICE)
 			FI2R2=FI2_K*R2_K*R2_K
 			RMASSIAA=RMASSIAA+FI2R2
 		 ENDDO
 		 FI3_K = PSI3(K)
 		 FI4_K = PSI4(K)
 		 FI5_K = PSI5(K)
 		 R3_K=R3(K)
 		 R4_K=R4(K)
 		 R5_K=R5(K)
 		 FI3R3=FI3_K*R3_K*R3_K
 		 FI4R4=FI4_K*R4_K*R4_K
 		 FI5R5=FI5_K*R5_K*R5_K
 		 RMASSIAA=RMASSIAA+FI3R3
 		 RMASSIAA=RMASSIAA+FI4R4
 		 RMASSIAA=RMASSIAA+FI5R5
 	  ENDDO
 		RMASSIAA = RMASSIAA*COL3*RORI

 		IF(RMASSIAA.LT.0.0) RMASSIAA=0.0

 		DELMASSI1 = RMASSIAA-RMASSIBB
 		QPN = QPS-DELMASSI1
 		DAL2 = AL2
 		TPN = TPS+DAL2*DELMASSI1

 	  IF(ABS(DAL2*DELMASSI1) > 5.0 )THEN
      print*,"ONECOND2-out (start)"
      print*,"I=",Iin,"J=",Jin,"Kin",Kin,"W",w_in,"DX",dx_in
      print*,"DEL1N,DEL2N,D1N,D2N,RW,PW,RI,PI,DT"
      print*,DEL1N,DEL2N,D1N,D2N,RW,PW,RI,PI,DTT
      print*,"TPS=",TPS,"QPS=",QPS,"delmassi1",delmassi1
      print*,"DAL1=",DAL2
      print*,RMASSIBB,RMASSIAA
      print*,"FI2_1",FI2(:,1)
      print*,"FI2_2",FI2(:,2)
      print*,"FI2_3",FI2(:,3)
      print*,"FI3",FI3
      print*,"FI4",FI4
      print*,"FI5",FI5
      print*,"PSI2_1",PSI2(:,1)
      print*,"PSI2_2",PSI2(:,2)
      print*,"PSI2_3",PSI2(:,3)
      print*,"PSI3",PSI3
      print*,"PSI4",PSI4
      print*,"PSI5",PSI5
      print*,"ONECOND2-out (end)"
      IF(ABS(DAL2*DELMASSI1) > 5.0 )THEN
      call wrf_error_fatal("fatal error in ONECOND2-out (ABS(DAL2*DELMASSI1) > 5.0), model stop")
 		ENDIF
 	  ENDIF

 ! ... SUPERSATURATION
 	  ARGEXP=-BB1_MY/TPN
 	  ES1N=AA1_MY*DEXP(ARGEXP)
 	  ARGEXP=-BB2_MY/TPN
 	  ES2N=AA2_MY*DEXP(ARGEXP)
 	  EW1N=OPER3(QPN,PP)
 	  IF(ES1N == 0.0)THEN
 	   DEL1N=0.5
 	   DIV1=1.5
 	   call wrf_error_fatal("fatal error in ONECOND2 (ES1N.EQ.0), model stop")
 	  ELSE
 	   DIV1=EW1N/ES1N
 	   DEL1N=EW1N/ES1N-1.
 	  END IF
 	  IF(ES2N == 0.0)THEN
 	   DEL2N=0.5
 	   DIV2=1.5
 	   call wrf_error_fatal("fatal error in ONECOND2 (ES2N.EQ.0), model stop")
 	  ELSE
 	   DEL2N=EW1N/ES2N-1.
 	   DIV2=EW1N/ES2N
 	  END IF

 !  END OF TIME SPLITTING
 ! (ONLY ICE: CONDENSATION OR EVAPORATION)
  	IF(TIMENEW.LT.DT) GOTO 46

 		TT=TPN
 		QQ=QPN
 		DO KR=1,NKR
 			DO ICE=1,ICEMAX
 				FF2(KR,ICE)=PSI2(KR,ICE)
 			ENDDO
 			FF3(KR)=PSI3(KR)
 			FF4(KR)=PSI4(KR)
 			FF5(KR)=PSI5(KR)
 		ENDDO

   RETURN
   END SUBROUTINE ONECOND2
 ! +----------------------------------------------------------------------------+
         SUBROUTINE ONECOND3 &
 						 & (TT,QQ,PP,ROR &
 						 & ,VR1,VR2,VR3,VR4,VR5,PSINGLE &
 						 & ,DEL1N,DEL2N,DIV1,DIV2 &
 						 & ,FF1,PSI1,R1,RLEC,RO1BL &
 						 & ,FF2,PSI2,R2,RIEC,RO2BL &
 						 & ,FF3,PSI3,R3,RSEC,RO3BL &
 						 & ,FF4,PSI4,R4,RGEC,RO4BL &
 						 & ,FF5,PSI5,R5,RHEC,RO5BL &
 						 & ,AA1_MY,BB1_MY,AA2_MY,BB2_MY &
 						 & ,C1_MEY,C2_MEY &
 						 & ,COL,DTCOND,ICEMAX,NKR &
 						 & ,ISYM1,ISYM2,ISYM3,ISYM4,ISYM5, &
 						 	Iin,Jin,Kin,W_in,DX_in, Itimestep)

        IMPLICIT NONE
        INTEGER ICEMAX,NKR,KR,ITIME,ICE,KCOND,K &
      &           ,ISYM1,ISYM2(ICEMAX),ISYM3,ISYM4,ISYM5, Kin, Iin, Jin, Itimestep
        INTEGER KLIMITL,KLIMITG,KLIMITH,KLIMITI_1, &
      &  KLIMITI_2,KLIMITI_3
        INTEGER I_MIXCOND,I_MIXEVAP,I_ABERGERON,I_BERGERON
        REAL ROR,VR1(NKR),VR2(NKR,ICEMAX),VR3(NKR),VR4(NKR) &
      &           ,VR5(NKR),PSINGLE &
      &           ,AA1_MY,BB1_MY,AA2_MY,BB2_MY &
      &           ,C1_MEY,C2_MEY &
      &           ,COL,DTCOND,W_in,DX_in

 ! DROPLETS

         REAL R1(NKR)&
      &           ,RLEC(NKR),RO1BL(NKR) &
      &           ,FI1(NKR),FF1(NKR),PSI1(NKR) &
      &           ,B11_MY(NKR),B12_MY(NKR)

 ! CRYSTALS

 	REAL R2(NKR,ICEMAX) &
      &           ,RIEC(NKR,ICEMAX) &
      &           ,RO2BL(NKR,ICEMAX) &
      &           ,FI2(NKR,ICEMAX),PSI2(NKR,ICEMAX) &
      &           ,FF2(NKR,ICEMAX) &
      &           ,B21_MY(NKR,ICEMAX),B22_MY(NKR,ICEMAX) &
      &           ,RATE2(NKR,ICEMAX),DEL_R2M(NKR,ICEMAX)

 ! SNOW
         REAL R3(NKR) &
      &           ,RSEC(NKR),RO3BL(NKR) &
      &           ,FI3(NKR),FF3(NKR),PSI3(NKR) &
      &           ,B31_MY(NKR),B32_MY(NKR) &
      &           ,DEL_R3M(NKR)

 ! GRAUPELS

         REAL R4(NKR) &
      &           ,RGEC(NKR),RO4BL(NKR) &
      &           ,FI4(NKR),FF4(NKR),PSI4(NKR) &
      &           ,B41_MY(NKR),B42_MY(NKR) &
      &           ,DEL_R4M(NKR)

 ! HAIL
         REAL R5(NKR) &
      &           ,RHEC(NKR),RO5BL(NKR) &
      &           ,FI5(NKR),FF5(NKR),PSI5(NKR) &
      &           ,B51_MY(NKR),B52_MY(NKR) &
      &           ,DEL_R5M(NKR)

       DOUBLE PRECISION DD1N,DB11_MY,DAL1,DAL2
       DOUBLE PRECISION COL3,RORI,TPN,TPS,QPN,QPS,TOLD,QOLD &
      &                  ,FI1_K,FI2_K,FI3_K,FI4_K,FI5_K &
      &                  ,R1_K,R2_K,R3_K,R4_K,R5_K &
      &                  ,FI1R1,FI2R2,FI3R3,FI4R4,FI5R5 &
      &                  ,RMASSLAA,RMASSLBB,RMASSIAA,RMASSIBB &
      &                  ,ES1N,ES2N,EW1N,ARGEXP &
      &                  ,TT,QQ,PP,DEL1N0,DEL2N0 &
      &                  ,DEL1N,DEL2N,DIV1,DIV2 &
      &                  ,OPER2,OPER3,AR1,AR2

        DOUBLE PRECISION DELTAQ1,DELMASSI1,DELMASSL1

        REAL A1_MYN, BB1_MYN, A2_MYN, BB2_MYN
         DATA A1_MYN, BB1_MYN, A2_MYN, BB2_MYN &
      &      /2.53,5.42,3.41E1,6.13/
        REAL B8L,B8I,SFN11,SFN12,SFNL,SFNI
        REAL B5L,B5I,B7L,B7I,B6,DOPL,DEL1S,DEL2S,DOPI,RW,QW,PW, &
      &  RI,PI,QI,SFNI1(ICEMAX),SFNI2(ICEMAX),AL1,AL2
        REAL D1N,D2N,DT0L, DT0I,D1N0,D2N0
        REAL SFN21,SFN22,SFNII1,SFNII2,SFN31,SFN32,SFN41,SFN42,SFN51, &
      &  SFN52
        REAL DEL1,DEL2
        REAL  TIMEREV,DT,DTT,TIMENEW
        REAL DTIMEG(NKR),DTIMEH(NKR),totccn_before,totccn_after

        REAL DEL2D(ICEMAX),DTIMEO(NKR),DTIMEL(NKR) &
      &           ,DTIMEI_1(NKR),DTIMEI_2(NKR),DTIMEI_3(NKR)
        REAL DT_WATER_COND,DT_WATER_EVAP,DT_ICE_COND,DT_ICE_EVAP, &
      &  DT_MIX_COND,DT_MIX_EVAP,DT_MIX_BERGERON,DT_MIX_ANTIBERGERON
        REAL DTNEWL0,DTNEWL1,DTNEWI1,DTNEWI2_1,DTNEWI2_2,DTNEWI2_3, &
      & DTNEWI2,DTNEWI_1,DTNEWI_2,DTNEWI3,DTNEWI4,DTNEWI5, &
      & DTNEWL,DTNEWL2,DTNEWG1,DTNEWH1
        REAL TIMESTEPD(NKR)

        DATA AL1 /2500./, AL2 /2834./
        REAL EPSDEL,EPSDEL2
        DATA EPSDEL, EPSDEL2 /0.1E-03,0.1E-03/

 	   REAL :: FL1(NKR), FL2(NKR,ICEMAX), FL3(NKR), FL4(NKR), FL5(NKR), SFNDUMMY(3), &
 	   		   R1N(NKR), R2N(NKR,ICEMAX), R3N(NKR), R4N(NKR), R5N(NKR)
 	   INTEGER :: IDROP, ICM, ISYMICE
 	   DOUBLE PRECISION :: R1D(NKR),R2D(NKR,ICEMAX),R3D(NKR), R4D(NKR), R5D(NKR), &
 			       R1ND(NKR),R2ND(NKR,ICEMAX),R3ND(NKR), R4ND(NKR), R5ND(NKR)


 	   DATA DT0L, DT0I /1.E20,1.E20/

 	   DOUBLE PRECISION :: DEL1_d, DEL2_d , RW_d, PW_d , RI_d , PI_d , D1N_d, D2N_d, &
 				VR1_d(NKR), VR2_d(NKR,ICEMAX), VR3_d(NKR), VR4_d(NKR), VR5_d(NKR), &
 				TTinput,QQinput,DEL1Ninput,DEL2Ninput

        OPER2(AR1)=0.622/(0.622+0.378*AR1)/AR1
        OPER3(AR1,AR2)=AR1*AR2/(0.622+0.378*AR1)



 TTinput = TT
 QQinput = QQ
 DEL1Ninput = DEL1N
 DEL2Ninput = DEL2N

 B12_MY = 0.0
 B22_MY = 0.0
 B32_MY = 0.0
 B42_MY = 0.0
 B52_MY = 0.0

 B21_MY = 0.0
 B31_MY = 0.0
 B41_MY = 0.0
 B51_MY = 0.0

 ICM = ICEMAX
 R1D = R1
 R2D = R2
 R3D = R3
 R4D = R4
 R5D = R5
 R1ND = R1D
 R2ND = R2D
 R3ND = R3D
 R4ND = R4D
 R5ND = R5D

 VR1_d = VR1
 VR2_d = VR2
 VR3_d = VR3
 VR4_d = VR4
 VR5_d = VR5

 SFN11 = 0.0
 SFNI1 = 0.0
 SFN31 = 0.0
 SFN41 = 0.0
 SFN51 = 0.0

 DT_WATER_COND=0.4
 DT_WATER_EVAP=0.4
 DT_ICE_COND=0.4
 DT_ICE_EVAP=0.4
 DT_MIX_COND=0.4
 DT_MIX_EVAP=0.4
 DT_MIX_BERGERON=0.4
 DT_MIX_ANTIBERGERON=0.4

 I_MIXCOND=0
 I_MIXEVAP=0
 I_ABERGERON=0
 I_BERGERON=0

 ITIME = 0
 TIMENEW = 0.0
 DT = DTCOND
 DTT = DTCOND

 B6=0.
 B8L=1./ROR
 B8I=1./ROR

 RORI=1.D0/ROR
  COL3=3.D0*COL
 TPN=TT
 QPN=QQ

 16  ITIME = ITIME + 1
 IF((TPN-273.15).GE.-0.187) GO TO 17
 TIMEREV = DT - TIMENEW
 DEL1 = DEL1N
 DEL2 = DEL2N
 DEL1S = DEL1N
 DEL2S = DEL2N

 DEL2D(1) = DEL2N
 DEL2D(2) = DEL2N
 DEL2D(3) = DEL2N
 TPS = TPN
 QPS = QPN
 DO KR = 1,NKR
 	FI1(KR) = PSI1(KR)
 	FI3(KR) = PSI3(KR)
 	FI4(KR) = PSI4(KR)
 	FI5(KR) = PSI5(KR)
 	DO ICE = 1,ICEMAX
 		FI2(KR,ICE) = PSI2(KR,ICE)
 	ENDDO
 ENDDO

 IF(ISYM1 == 1)THEN
  FL1 = 0.0
 	CALL JERRATE_KS &
      	 	(R1D,TPS,PP,VR1_d,RLEC,RO1BL,B11_MY,1,1,fl1,NKR,ICEMAX)

 	sfndummy(1) = SFN11
 	CALL JERTIMESC_KS(FI1,R1D,SFNDUMMY,B11_MY,B8L,1,NKR,ICEMAX,COL)
 	SFN11 = sfndummy(1)
 ENDIF

 IF(sum(ISYM2) > 0) THEN
 		FL1 = 0.0
 		! ... ice crystals
   	CALL JERRATE_KS (R2D,TPS,PP,VR2_d,RIEC,RO2BL,B21_MY,3,2,fl1,NKR,ICEMAX)
   	CALL JERTIMESC_KS (FI2,R2D,SFNI1,B21_MY,B8I,ICM,NKR,ICEMAX,COL)
 ENDIF
 IF(ISYM3 == 1) THEN
 		FL3 = 0.0
 		! ... snow
 		CALL JERRATE_KS (R3D,TPS,PP,VR3_d,RSEC,RO3BL,B31_MY,1,3,fl3,NKR,ICEMAX)
 		sfndummy(1) = SFN31
 		CALL JERTIMESC_KS(FI3,R3D,SFNDUMMY,B31_MY,B8I,1,NKR,ICEMAX,COL)
   	SFN31 = sfndummy(1)
 ENDIF
 IF(ISYM4 == 1) THEN
 		FL4 = 0.0
 		! ... graupel
 		CALL JERRATE_KS(R4D,TPS,PP,VR4_d,RGEC,RO4BL,B41_MY,1,2,fl4,NKR,ICEMAX)
 		sfndummy(1) = SFN41
 		CALL JERTIMESC_KS(FI4,R4D,SFNDUMMY,B41_MY,B8I,1,NKR,ICEMAX,COL)
 		SFN41 = sfndummy(1)
 ENDIF
 IF(ISYM5 == 1) THEN
 		FL5 = 0.0
 		! ... hail
 		CALL JERRATE_KS(R5D,TPS,PP,VR5_d,RHEC,RO5BL,B51_MY,1,2,fl5,NKR,ICEMAX)
 		sfndummy(1) = SFN51
 		CALL JERTIMESC_KS(FI5,R5D,SFNDUMMY,B51_MY,B8I,1,NKR,ICEMAX,COL)
 		SFN51 = sfndummy(1)
 ENDIF

 	SFNII1 = SFNI1(1) + SFNI1(2) + SFNI1(3)
 	SFN21 = SFNII1 + SFN31 + SFN41 + SFN51
 	SFN12 = 0.0
 	SFNL = SFN11 + SFN12
 	SFN22 = 0.0
 	SFNI = SFN21 + SFN22

 	B5L=BB1_MY/TPS/TPS
 	B5I=BB2_MY/TPS/TPS
 	B7L=B5L*B6
 	B7I=B5I*B6
 	DOPL=1.+DEL1S
 	DOPI=1.+DEL2S
 	RW=(OPER2(QPS)+B5L*AL1)*DOPL*SFNL
 	QW=B7L*DOPL
 	PW=(OPER2(QPS)+B5I*AL1)*DOPI*SFNL
 	RI=(OPER2(QPS)+B5L*AL2)*DOPL*SFNI
 	PI=(OPER2(QPS)+B5I*AL2)*DOPI*SFNI
 	QI=B7I*DOPI

 	IF(RW.NE.RW .or. PW.NE.PW)THEN
 	  print*, 'NaN In ONECOND3'
 	  call wrf_error_fatal("fatal error in ONECOND3 (RW or PW are NaN), model stop")
 	ENDIF

 	! DEL1 > 0, DEL2 < 0    (ANTIBERGERON MIXED PHASE - KCOND=50)
 	! DEL1 < 0 AND DEL2 < 0 (EVAPORATION MIXED_PHASE - KCOND=30)
 	! DEL1 > 0 AND DEL2 > 0 (CONDENSATION MIXED PHASE - KCOND=31)
 	! DEL1 < 0, DEL2 > 0    (BERGERON MIXED PHASE - KCOND=32)

  KCOND=50
 	IF(DEL1N .LT. 0.0 .AND. DEL2N .LT. 0.0) KCOND=30
 	IF(DEL1N .GT. 0.0 .AND. DEL2N .GT. 0.0) KCOND=31
 	IF(DEL1N .LT. 0.0 .AND. DEL2N .GT. 0.0) KCOND=32

 	IF(KCOND == 50) THEN
 		DTNEWL = DT
    DTNEWL = AMIN1(DTNEWL,TIMEREV)
    TIMENEW = TIMENEW + DTNEWL
    DTT = DTNEWL

 		! ... Incase the Anti-Bregeron regime we do not call diffusional-growth
 		PRINT*, "Anti-Bregeron Regime, No DIFFU"
 		PRINT*,  DEL1, DEL2, TT, QQ, Kin
 		GO TO 17
 	  ! IN CASE : KCOND = 50
  ENDIF
  IF(KCOND == 31) THEN
		! ... DEL1 > 0 AND DEL2 > 0 (CONDENSATION MIXED PHASE - KCOND=31)
		! ... CONDENSATION MIXED PHASE (BEGIN)
	  DTNEWL = DT
    DTNEWL = AMIN1(DTNEWL,TIMEREV)
    TIMENEW = TIMENEW + DTNEWL
    DTT = DTNEWL
		! CONDENSATION MIXED PHASE (END)
	! IN CASE : KCOND = 31
  ENDIF
   IF(KCOND == 30) THEN
 		! ... DEL1 < 0 AND DEL2 < 0 (EVAPORATION MIXED_PHASE - KCOND=30)
 		! ... EVAPORATION MIXED PHASE (BEGIN)
 		DTNEWL = DT
    DTNEWL = AMIN1(DTNEWL,TIMEREV)
    TIMENEW = TIMENEW + DTNEWL
    DTT = DTNEWL
 	! EVAPORATION MIXED PHASE (END)
 	! IN CASE : KCOND = 30
 	ENDIF
 	IF(KCOND == 32) THEN
 		! ... IF(DEL1N < 0.0 .AND. DEL2N > 0.0) KCOND=32
 		! ... BERGERON MIXED PHASE (BEGIN)
 		DTNEWL = DT
    DTNEWL = AMIN1(DTNEWL,TIMEREV)
    TIMENEW = TIMENEW + DTNEWL
    DTT = DTNEWL
 	! BERGERON MIXED PHASE (END)
 	! IN CASE : KCOND = 32
 	ENDIF

   IF (DTT < 0.0) call wrf_error_fatal("fatal error in ONECOND3:(DTT<0), model stop")

 	DEL1_d = DEL1
 	DEL2_d = DEL2
 	RW_d = RW
 	PW_d = PW
 	RI_d = RI
 	PI_d = PI
 	CALL JERSUPSAT_KS(DEL1_d,DEL2_d,DEL1N,DEL2N, &
 						RW_d,PW_d,RI_d,PI_d, &
 						DTT,D1N_d,D2N_d,0.0,0.0, &
 						ISYM1,ISYM2,ISYM3,ISYM4,ISYM5)
 	DEL1 = DEL1_d
 	DEL2 = DEL2_d
 	RW = RW_d
 	PW = PW_d
 	RI = RI_d
 	PI = PI_d
 	D1N = D1N_d
 	D2N = D2N_d

 	IF(ISYM1 == 1) THEN
 		! DROPLETS
 		! DROPLET DISTRIBUTION FUNCTION
 		IDROP = ISYM1
 		FL1 = 0.0
 		CALL JERDFUN_KS(R1D, R1ND, B11_MY, &
 						FI1, PSI1, fl1, D1N, &
 						ISYM1, 1, 1, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 1, Iin, Jin ,Kin, Itimestep)
 		! IN CASE ISYM1.NE.0
 	ENDIF
 	IF(sum(ISYM2) > 0) THEN
 		! CRYSTALS
 		IDROP = 0
 		FL1 = 0.0
 		IF(ISYM2(1)==1)THEN
 			CALL JERDFUN_KS(R2D(:,1), R2ND(:,1), B21_MY(:,1), &
 						    FI2(:,1), PSI2(:,1), fl1, D2N, &
 							ISYM2(1), ICM, 1, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 21, Iin, Jin ,Kin, Itimestep)
 		ENDIF
 		IF(ISYM2(2)==1)THEN
 	  		CALL JERDFUN_KS(R2D(:,2), R2ND(:,2), B21_MY(:,2), &
 						    FI2(:,2), PSI2(:,2), fl1, D2N, &
 							ISYM2(2), ICM, 2, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 22, Iin, Jin ,Kin, Itimestep)
 		ENDIF
 		IF(ISYM2(3)==1)THEN
 	  		CALL JERDFUN_KS(R2D(:,3), R2ND(:,3), B21_MY(:,3), &
 						    FI2(:,3), PSI2(:,3), fl1, D2N, &
 							ISYM2(3), ICM, 3, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 23, Iin, Jin ,Kin, Itimestep)
 		ENDIF
 	ENDIF

 	IF(ISYM3 == 1) THEN
 		! SNOW
 		IDROP = 0
 		FL3 = 0.0
 		CALL JERDFUN_KS(R3D, R3ND, B31_MY, &
 						FI3, PSI3, fl3, D2N, &
 						ISYM3, 1, 3, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 3, Iin, Jin ,Kin, Itimestep)
 	! IN CASE ISYM3.NE.0
 	ENDIF

 	IF(ISYM4 == 1) THEN
 	! GRAUPELS
 		IDROP = 0
 		FL4 = 0.0
 		CALL JERDFUN_KS(R4D, R4ND, B41_MY, &
 						FI4, PSI4, fl4, D2N, &
 						ISYM4, 1, 4, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 4, Iin, Jin ,Kin, Itimestep)

 	! IN CASE ISYM4.NE.0
 	ENDIF

    IF(ISYM5 == 1) THEN
     ! HAIL
 	  IDROP = 0
 	  FL5 = 0.0
 	  CALL JERDFUN_KS(R5D, R5ND, B51_MY, &
 					FI5, PSI5, fl5, D2N, &
 					ISYM5, 1, 5, TPN, IDROP, FR_LIM, FRH_LIM, ICEMAX, NKR, COL, 5, Iin, Jin ,Kin, Itimestep)
  ! IN CASE ISYM5.NE.0
  ENDIF

 RMASSLBB=0.D0
 RMASSIBB=0.D0
 RMASSLAA=0.D0
 RMASSIAA=0.D0

 DO K=1,NKR
  FI1_K=FI1(K)
  R1_K=R1(K)
  FI1R1=FI1_K*R1_K*R1_K
  RMASSLBB=RMASSLBB+FI1R1
  DO ICE =1,ICEMAX
 	FI2_K=FI2(K,ICE)
 	R2_K=R2(K,ICE)
 	FI2R2=FI2_K*R2_K*R2_K
 	RMASSIBB=RMASSIBB+FI2R2
  ENDDO
 	 FI3_K=FI3(K)
 	 FI4_K=FI4(K)
 	 FI5_K=FI5(K)
 	 R3_K=R3(K)
 	 R4_K=R4(K)
 	 R5_K=R5(K)
 	 FI3R3=FI3_K*R3_K*R3_K
 	 FI4R4=FI4_K*R4_K*R4_K
 	 FI5R5=FI5_K*R5_K*R5_K
 	 RMASSIBB=RMASSIBB+FI3R3
 	 RMASSIBB=RMASSIBB+FI4R4
 	 RMASSIBB=RMASSIBB+FI5R5
   ENDDO
   RMASSIBB=RMASSIBB*COL3*RORI
   IF(RMASSIBB.LT.0.0) RMASSIBB=0.0
   RMASSLBB=RMASSLBB*COL3*RORI
   IF(RMASSLBB.LT.0.0) RMASSLBB=0.0
   DO K=1,NKR
 	 FI1_K=PSI1(K)
 	 R1_K=R1(K)
 	 FI1R1=FI1_K*R1_K*R1_K
 	 RMASSLAA=RMASSLAA+FI1R1
 	 DO ICE =1,ICEMAX
 		FI2(K,ICE)=PSI2(K,ICE)
 		FI2_K=FI2(K,ICE)
 		R2_K=R2(K,ICE)
 		FI2R2=FI2_K*R2_K*R2_K
 		RMASSIAA=RMASSIAA+FI2R2
 	 ENDDO
 	 FI3_K=PSI3(K)
 	 FI4_K=PSI4(K)
 	 FI5_K=PSI5(K)
 	 R3_K=R3(K)
 	 R4_K=R4(K)
 	 R5_K=R5(K)
 	 FI3R3=FI3_K*R3_K*R3_K
 	 FI4R4=FI4_K*R4_K*R4_K
 	 FI5R5=FI5_K*R5_K*R5_K
 	 RMASSIAA=RMASSIAA+FI3R3
 	 RMASSIAA=RMASSIAA+FI4R4
 	 RMASSIAA=RMASSIAA+FI5R5
   ENDDO
 	RMASSIAA=RMASSIAA*COL3*RORI
 	IF(RMASSIAA.LE.0.0) RMASSIAA=0.0
 	RMASSLAA=RMASSLAA*COL3*RORI
 	IF(RMASSLAA.LT.0.0) RMASSLAA=0.0

 	DELMASSL1=RMASSLAA-RMASSLBB
 	DELMASSI1=RMASSIAA-RMASSIBB
 	DELTAQ1=DELMASSL1+DELMASSI1
 	QPN=QPS-DELTAQ1
 	DAL1=AL1
 	DAL2=AL2
 	TPN = TPS + DAL1*DELMASSL1+DAL2*DELMASSI1

 	IF(ABS(DAL1*DELMASSL1+DAL2*DELMASSI1) > 5.0 )THEN
 		print*,"ONECOND3-input-start"
 		print*,"TTinput",TTinput,"QQinput",QQinput,"PP",PP
 		print*,'DEL1Ninput',DEL1Ninput,'DEL2Ninput',DEL2Ninput
 		print*,"ROR",ROR,'VR1',VR1,'PSINGLE',PSINGLE
 		print*,'DIV1',DIV1,'DIV2',DIV2
 		print*,'R1',R1,'RLEC',RLEC,'RO1BL',RO1BL
 		print*,'const',AA1_MY,BB1_MY,AA2_MY,BB2_MY
 		print*,'const',C1_MEY,C2_MEY,COL
 		print*,'DTCOND',DTCOND,'ICEMAX',ICEMAX,'NKR',NKR
 		print*,'ISYM1',ISYM1,'ISYM2',ISYM2,'ISYM3',ISYM3,'ISYM4',ISYM4,'ISYM5',ISYM5
 		print*,Iin,Jin,Kin,W_in,DX_in
 		print*,"ONECOND3-input-end"

 		print*,"ONECOND3-out (start)"
 		print*,"I=",Iin,"J=",Jin,"Kin",Kin,"W",w_in,"DX",dx_in
 		print*,"DEL1N,DEL2N,D1N,D2N,RW,PW,RI,PI,DT"
 		print*,DEL1N,DEL2N,D1N,D2N,RW,PW,RI,PI,DTT
 		print*,"TPS=",TPS,"TPN=",TPN,"QPS=",QPS,"delmassl1",delmassl1,"delmassi1",delmassi1
 		print*,"DAL2=",DAL2,"DAL1=",DAL1
 		print*,RMASSLAA,RMASSLBB
 		print*,RMASSIAA,RMASSIBB
 		print*,"FI1",FI1
 		print*,"FI3",FI3
 		print*,"FI4",FI4
 		print*,"FI5",FI5
 		print*,"PSI1",PSI1
 		print*,"R1D",R1D,"R1ND",R1ND
 		print*,"PSI3",PSI3
 		print*,"R3D",R3D,"R3ND",R3ND
 		print*,"PSI4",PSI4
 		print*,"R4D",R4D,"R4ND",R4ND
 		print*,"PSI5",PSI5
 		print*,"R5D",R5D,"R5ND",R5ND
 		print*,"ONECOND3-out (end)"
 		IF(ABS(DAL1*DELMASSL1+DAL2*DELMASSI1) > 5.0 )THEN
 			call wrf_error_fatal("fatal error in ONECOND3-out (ABS(DAL1*DELMASSL1+DAL2*DELMASSI1) > 5.0), model stop")
 		ENDIF
 	ENDIF

 ! SUPERSATURATION
 	ARGEXP=-BB1_MY/TPN
 	ES1N=AA1_MY*DEXP(ARGEXP)
 	ARGEXP=-BB2_MY/TPN
 	ES2N=AA2_MY*DEXP(ARGEXP)
 	EW1N=OPER3(QPN,PP)
 	IF(ES1N == 0.0)THEN
 	 DEL1N=0.5
 	 DIV1=1.5
 	 print*,'es1n onecond3 = 0'
 	 call wrf_error_fatal("fatal error in ONECOND3 (ES1N.EQ.0), model stop")
 	ELSE
 	 DIV1=EW1N/ES1N
 	 DEL1N=EW1N/ES1N-1.
 	END IF
 	IF(ES2N == 0.0)THEN
 	 DEL2N=0.5
 	 DIV2=1.5
 	 print*,'es2n onecond3 = 0'
 	 call wrf_error_fatal("fatal error in ONECOND3 (ES2N.EQ.0), model stop")
 	ELSE
 	 DEL2N=EW1N/ES2N-1.
 	 DIV2=EW1N/ES2N
 	END IF
 	! END OF TIME SPLITTING

 	IF(TIMENEW < DT) GOTO 16
 	17 CONTINUE

 	TT=TPN
 	QQ=QPN
 	DO KR=1,NKR
 	   FF1(KR)=PSI1(KR)
 	   DO ICE=1,ICEMAX
 	      FF2(KR,ICE)=PSI2(KR,ICE)
 	   ENDDO
 	   FF3(KR)=PSI3(KR)
 	   FF4(KR)=PSI4(KR)
 	   FF5(KR)=PSI5(KR)
 	ENDDO

   RETURN
   END SUBROUTINE ONECOND3
 ! +---------------------------------------------------------+
 	SUBROUTINE COAL_BOTT_NEW(FF1R,FF2R,FF3R,                      &
				FF4R,FF5R,TT,QQ,PP,RHO,dt_coll,TCRIT,TTCOAL,&
			      	FLIQFR_S,FLIQFR_G,FLIQFR_H,FRIMFR_S,        &
			    	DEL1in, DEL2in,                             &
  		              	Iin,Jin,Kin,itimestep,CollEff)

    use module_mp_SBM_Collision,only:coll_xyy_lwf,coll_xyx_lwf,coll_xxx_lwf,    &
				     coll_xyz_lwf, modkrn_KS, coll_breakup_KS, 	&
				     coll_xxy_lwf

     implicit none

     integer,intent(in) :: Iin,Jin,Kin,itimestep
     real(kind=r4size),intent(in) :: tcrit,ttcoal,dt_coll
     real(kind=r4size),intent(inout) :: ff1r(:),ff2r(:,:),ff3r(:),ff4r(:),  &
                                        ff5r(:),colleff
     real(kind=r8size),intent(inout) :: fliqfr_s(:),fliqfr_g(:),fliqfr_h(:), &
                                       frimfr_s(:),del1in,del2in,tt,qq
     real(kind=r8size),intent(in) :: pp

 	   integer :: KR,ICE,icol_drop,icol_snow,icol_graupel,icol_hail, &
      		      icol_column,icol_plate,icol_dendrite,icol_drop_brk
     real(kind=r8size) :: g1(nkr),g2(nkr,icemax),g3(nkr),g4(nkr),g5(nkr), &
                          gdumb(JMAX),gdumb_bf_breakup(JMAX),xl_dumb(JMAX), &
                          g_orig(nkr),g2_1(nkr),g2_2(nkr),g2_3(nkr)
     real(kind=r4size) :: cont_fin_drop,dconc,conc_icempl,deldrop,t_new, &
      	                 delt_new,cont_fin_ice,conc_old,conc_new,cont_init_ice, &
  		                   cont_init_drop,ALWC,T_new_real,PP_r,rho,ES1N,ES2N,EW1N
     real(kind=r4size),parameter :: tt_no_coll=273.16

     integer :: I,J,IT,NDIV
     real(kind=r8size) :: break_drop_bef,break_drop_aft,dtbreakup,break_drop_per, &
                          prdkrn,fl1(nkr),rf1(nkr),rf3(nkr),fl3(nkr), &
                          fl4(nkr),fl5(nkr),fl2_1(nkr),fl2_2(nkr),fl2_3(nkr), &
                          rf2(nkr),rf4(nkr),rf5(nkr),conc_drop_old, conc_drop_new, &
                          dconc_drop, dm_rime(nkr), conc_plate_icempl, &
                          col3, cont_coll_drop
     real(kind=r8size),parameter :: prdkrn1 = 1.0d0
     real(kind=r4size),parameter :: prdkrn1_r = 1.0
 	   integer,parameter :: icempl = 1
 	   real(kind=r8size),parameter :: t_ice_mpl = 270.15D0 ! for ice multiplication in temp > 268.15
 	   real(kind=r8size),PARAMETER :: g_lim = 1.0D-19*1.0D3,AA1_MY = 2.53E12,  &
                                    BB1_MY = 5.42E3, AA2_MY = 3.41E13 ,BB2_MY = 6.13E3

    icol_drop_brk=0
    icol_drop=0
    icol_snow=0
    icol_graupel=0
    icol_hail=0
    icol_column=0
    icol_plate=0
    icol_dendrite=0
    t_new = tt

    PP_r = PP
    call Kernals_KS(dt_coll,nkr,PP_r)
    !CALL MODKRN_KS(TT,QQ,PP,RHO,PRDKRN,TTCOAL,1,1,Iin,Jin,Kin)
    CALL MODKRN_KS(TT,QQ,PP,RHO,PRDKRN,TTCOAL,11,1,Iin,Jin,Kin)

 	  CollEff = PRDKRN

    DO KR=1,NKR
      G1(KR)=FF1R(KR)*3.*XL(KR)*XL(KR)*1.E3
      G2(KR,1)=FF2R(KR,1)*3*xi(KR,1)*XI(KR,1)*1.e3
      G2(KR,2)=FF2R(KR,2)*3.*xi(KR,2)*XI(KR,2)*1.e3
      G2(KR,3)=FF2R(KR,3)*3.*xi(KR,3)*XI(KR,3)*1.e3
      G3(KR)=FF3R(KR)*3.*xs(kr)*xs(kr)*1.e3
      G4(KR)=FF4R(KR)*3.*xg(kr)*xg(kr)*1.e3
      G5(KR)=FF5R(KR)*3.*xh(kr)*xh(kr)*1.e3
      g2_1(kr)=g2(KR,1)
      g2_2(KR)=g2(KR,2)
      g2_3(KR)=g2(KR,3)
      if(kr .gt. KRMIN_BREAKUP .and. g1(kr) > g_lim) icol_drop_brk = 1
      IF (IBREAKUP.NE.1) icol_drop_brk = 0
      if(g1(kr).gt.g_lim) icol_drop=1
      if(g2_1(kr).gt.g_lim) icol_column = 1
      if(g2_2(kr).gt.g_lim) icol_plate = 1
      if(g2_3(kr).gt.g_lim) icol_dendrite = 1
      if(g3(kr).gt.g_lim) icol_snow = 1
      if(g4(kr).gt.g_lim) icol_graupel = 1
      if(g5(kr).gt.g_lim) icol_hail = 1
    END DO

 	  fl1 = 1.0
 	  fl3(:) = FLIQFR_S(:)
 	  fl4(:) = FLIQFR_G(:)
 	  fl5(:) = FLIQFR_H(:)
 	  rf1 = 1.0
 	  rf3(:) = FRIMFR_S(:)
 	  rf4(:) = 0.0
 	  rf5(:) = 0.0


 ! calculation of initial hydromteors content in g/cm**3 :
  cont_init_drop = 0.0
  cont_init_ice = 0.0
  cont_init_drop = sum(g1(1:nkr))
  cont_init_ice = sum(g3(1:nkr)) + sum(g4(1:nkr)) + sum(g5(1:nkr))
  do ice=1,icemax
      cont_init_ice = cont_init_ice + sum(g2(1:nkr,ice))
  enddo
  cont_init_drop=col*cont_init_drop*1.e-3
  cont_init_ice=col*cont_init_ice*1.e-3
! calculation of alwc in g/m**3
  alwc=cont_init_drop*1.e6
! calculation interactions :
! droplets - droplets and droplets - ice :
! water-water = water

  if (icol_drop.eq.1)then
! ... Drop-Drop collisions
  fl1 = 1.0
  call coll_xxx_lwf (G1,fl1,CWLL,XL_MG,CHUCM,IMA,1.d0,NKR)
! ... Breakup
  if(icol_drop_brk == 1)then
    ndiv = 1
    10     	continue
    do it = 1,ndiv
      dtbreakup = dt_coll/ndiv
      if (it == 1)then
        do kr=1,JMAX
          gdumb(kr)= g1(kr)*1.D-3
          gdumb_bf_breakup(kr) =  g1(kr)*1.D-3
          xl_dumb(kr)=xl_mg(KR)*1.D-3
        end do
        break_drop_bef=0.d0
        do kr=1,JMAX
          break_drop_bef = break_drop_bef+g1(kr)*1.D-3
        end do
      end if

      call coll_breakup_KS(gdumb, xl_dumb, JMAX, dtbreakup, JBREAK, PKIJ, QKJ, NKR, NKR)
    end do

    do KR=1,NKR
      FF1R(KR) = (1.0d3*GDUMB(KR))/(3.0*XL(KR)*XL(KR)*1.E3)
      if(FF1R(KR) < 0.0)then
        if(ndiv < 8)then
          ndiv = 2*ndiv
          go to 10
        else
          !print*,"noBreakUp",Iin,Jin,Kin,Itimestep,ndiv
          go to 11
          !call wrf_error_fatal("in coal_bott af-coll_breakup - FF1R/GDUMB < 0.0")
        endif
      endif
      if(FF1R(kr) .ne. FF1R(kr)) then
        print*,kr,GDUMB(kr),GDUMB_BF_BREAKUP(kr),XL(kr)
        print*,IT,NDIV, DTBREAKUP
        print*,GDUMB
        print*,GDUMB_BF_BREAKUP
        call wrf_error_fatal("in coal_bott af-coll_breakup - FF1R NaN, model stop")
      endif
    enddo

    break_drop_aft=0.0d0
    do kr=1,JMAX
      break_drop_aft=break_drop_aft+gdumb(kr)
    end do
    break_drop_per=break_drop_aft/break_drop_bef
    if (break_drop_per > 1.001)then
      ndiv=ndiv*2
      GO TO 10
    else
      do kr=1,JMAX
        g1(kr) = gdumb(kr)*1.D3
      end do
    end if
  ! if icol_drop_brk.eq.1
  end if
! if icol_drop.eq.1
end if

11   continue
 ! +--------------------------------------------------------+
 ! Negative temperature collisions block (start)
 ! +---------------------------------------------------------+
 	if(tt <= 273.15)then
 		if(icol_drop == 1)then
 			! ... interactions between drops and snow
 			!	    drop - snow = graupel/hail
 			!	    snow - drop = snow
      !          or
 			!     snow - drop = graupel/hail
  		if (icol_snow == 1)then
 				rf1 = 1.0
 				rf5 = 0.0
 				rf4 = 0.0
 				if(hail_opt == 1)then
 					call coll_xyz_lwf(g1,g3,g5,rf1,rf3,rf5,cwls,xl_mg,xs_mg, &
             	         	   	      	 chucm,ima,prdkrn1,nkr,0)
 				else
 					call coll_xyz_lwf(g1,g3,g4,rf1,rf3,rf4,cwls,xl_mg,xs_mg, &
             	         	   		  	  chucm,ima,prdkrn1,nkr,0)
 				endif
		    rf1 = 1.0
        rf5 = 0.0
        rf4 = 0.0
		    if(alwc < alcr) then
    			call coll_xyx_lwf(g3,g1,rf3,rf1,cwsl,xs_mg,xl_mg, &
         	         		      chucm,ima,prdkrn1,nkr,1,dm_rime)
		    else
 					if(hail_opt == 1)then
 						call coll_xyz_lwf(g3,g1,g5,rf3,rf1,rf5,cwsl,xs_mg,xl_mg, &
									           chucm,ima,prdkrn1,nkr,1)
 					else
 						call coll_xyz_lwf(g3,g1,g4,rf3,rf1,rf4,cwsl,xs_mg,xl_mg, &
 										         chucm,ima,prdkrn1,nkr,1)
 					endif
 				endif
 			! in case : icolxz_snow.ne.0
 			end if

  		if (icol_graupel == 1) then
 			! ... interactions between drops and graupel
 			!     drops - graupel = graupel
 			!     graupel - drops = graupel
 			!     drops - graupel = hail (no transition in FSBM)
 			!     graupel - drop = hail (no transition in FSBM)
    		if(alwc < alcr_g) then
  		    rf1 = 1.0
  		    rf4 = 0.0
  				call coll_xyy_lwf(g1,g4,rf1,rf4,cwlg,xl_mg,xg_mg, &
    		 	     	         	  chucm,ima,prdkrn1,nkr,0)
   					! ... for ice multiplication
   					conc_old = 0.0
       			conc_new = 0.0
   					do kr = kr_icempl,nkr
   						conc_old = conc_old+col*g1(kr)/xl_mg(kr)
       			end do
       			rf1 = 1.0
       			rf4 = 0.0
   					call coll_xyx_lwf(g4,g1,rf4,rf1,cwgl,xg_mg,xl_mg, &
   		 							           chucm,ima,prdkrn1,nkr,1,dm_rime)
  			else
          rf1 = 1.0
          rf5 = 0.0
          rf4 = 0.0
 					call coll_xyz_lwf(g1,g4,g5,rf1,rf4,rf5,cwlg,xl_mg,xg_mg, &
           			      			  chucm,ima,prdkrn1,nkr,1)
 					! ... for ice multiplication
 					conc_old = 0.0
   			  conc_new = 0.0
 					do kr = kr_icempl,nkr
				    conc_old = conc_old+col*g1(kr)/xl_mg(kr)
   				enddo
          rf1 = 1.0
          rf5 = 0.0
          rf4 = 0.0
 					call coll_xyz_lwf(g4,g1,g5,rf4,rf1,rf5,cwgl,xg_mg,xl_mg, &
           			            chucm,ima,prdkrn1,nkr,1)
    		end if
 			! in case icol_graup == 1
  		endif

  		if(icol_hail == 1) then
   			! interactions between drops and hail
   			! drops - hail = hail
   			! hail - water = hail
         rf1 = 1.0
         rf5 = 0.0
  	     call coll_xyy_lwf(g1,g5,rf1,rf5,cwlh,xl_mg,xh_mg, &
             		          chucm,ima,prdkrn1,nkr,0)
  			 ! ... for ice multiplication
  			 conc_old = 0.0
       	 conc_new = 0.0
  			 do kr = kr_icempl,nkr
  			  conc_old = conc_old+col*g1(kr)/xl_mg(kr)
  	     enddo
   			rf1 = 1.0
   			rf5 = 0.0
  			call coll_xyx_lwf(g5,g1,rf5,rf1,cwhl,xh_mg,xl_mg, &
                 			   chucm,ima,prdkrn1,nkr,1,dm_rime)
   		! in case icol_hail == 1
  		endif

 			if((icol_graupel == 1 .or. icol_hail == 1) .and. icempl == 1) then
 				if(tt .ge. 265.15 .and. tt .le. tcrit) then
 				! ... ice-multiplication (H-M) :
 					do kr = kr_icempl,nkr
 					   conc_new=conc_new+col*g1(kr)/xl_mg(kr)
 					enddo
 					dconc = conc_old-conc_new
 					if(tt .le. 268.15) then
 						conc_icempl=dconc*4.e-3*(265.15-tt)/(265.15-268.15)
 					endif
 					if(tt .gt. 268.15) then
 						conc_icempl=dconc*4.e-3*(tcrit-tt)/(tcrit-268.15)
 					endif
 					!g2_2(1)=g2_2(1)+conc_icempl*xi2_mg(1)/col
 					 g3(1)=g3(1)+conc_icempl*xs_mg(1)/col ! [KSS] >> FAST-sbm has small snow as IC
 				! in case t.ge.265.15 :
 				endif
 			! in case icempl=1
 			endif
 		! if icol_drop.eq.1
 		endif

 		if(icol_snow == 1) then
 		! ... interactions between snowflakes
			call coll_xxx_lwf(g3,rf3,cwss,xs_mg,chucm,ima,prdkrn,nkr)
 		! in case icolxz_snow.ne.0
 		endif

 	  ! in case : t > TTCOAL
  	endif ! if tt <= 273.15
 ! Negative temp. collision block (end)
 ! +-----------------------------------------------+

    cont_fin_drop=0.
    cont_fin_ice=0.
    do kr=1,nkr
      g2(kr,1)=g2_1(kr)
      g2(kr,2)=g2_2(kr)
      g2(kr,3)=g2_3(kr)
      cont_fin_drop=cont_fin_drop+g1(kr)
      cont_fin_ice=cont_fin_ice+g3(kr)+g4(kr)+g5(kr)
      do ice=1,icemax
         cont_fin_ice=cont_fin_ice+g2(kr,ice)
      enddo
    enddo
    cont_fin_drop=col*cont_fin_drop*1.e-3
    cont_fin_ice=col*cont_fin_ice*1.e-3
    deldrop=cont_init_drop-cont_fin_drop ! [g/cm**3]
 ! riming temperature correction (rho in g/cm**3) :
     if(t_new <= 273.15) then
       if(deldrop > 0.0) then
          t_new = t_new + 320.*deldrop/rho
          ES1N = POLYSVP(t_new,0)
          ES2N = POLYSVP(t_new,1)
          EW1N = QQ*PP/(0.622+0.378*QQ)
          DEL1in = EW1N/ES1N - 1.0
          DEL2in = EW1N/ES2N - 1.0
       else
  	     ! if deldrop < 0
         if(abs(deldrop).gt.cont_init_drop*0.05) then
           call wrf_error_fatal("fatal error in module_mp_fast_sbm (abs(deldrop).gt.cont_init_drop), model stop")
         endif
       endif
      endif

 61   continue
 ! recalculation of density function f1,f3,f4,f5 in  units [1/(g*cm**3)] :
     DO KR=1,NKR
        FF1R(KR)=G1(KR)/(3.*XL(KR)*XL(KR)*1.E3)
        if((FF1R(kr) .ne. FF1R(kr)) .or. FF1R(kr) < 0.0)then
	 	       print*,"G1",G1
 		 	     call wrf_error_fatal("stop at end coal_bott - FF1R NaN or FF1R < 0.0, model stop")
	      endif
        FF3R(KR)=G3(KR)/(3.*xs(kr)*xs(kr)*1.e3)
          if((FF3R(kr) .ne. FF3R(kr)) .or. FF3R(kr) < 0.0)then
           call wrf_error_fatal("stop at end coal_bott - FF3R NaN or FF3R < 0.0, model stop")
          endif
 		   if(hail_opt == 0)then
 		 	   FF4R(KR)=G4(KR)/(3.*xg(kr)*xg(kr)*1.e3)
      	 if((FF4R(kr) .ne. FF4R(kr)) .or. FF4R(kr) < 0.0) then
          call wrf_error_fatal("stop at end coal_bott - FF4R NaN or FF4R < 0.0, model stop")
         end if
      else
 		 	   FF5R(KR)=G5(KR)/(3.*xh(kr)*xh(kr)*1.e3)
		     if((FF5R(kr) .ne. FF5R(kr)) .or. FF5R(kr) < 0.0) then
           call wrf_error_fatal("stop at end coal_bott - FF5R NaN or FF5R < 0.0, model stop")
         endif
 		 endif
 		END DO
 15   CONTINUE

 	FLIQFR_S(:) = fl3(:)
 	FLIQFR_G(:) = fl4(:)
 	FLIQFR_H(:) = fl5(:)
 	FRIMFR_S(:) = rf3(:)

 	if (abs(tt-t_new).gt.5.0) then
 		call wrf_error_fatal("fatal error in module_mp_FAST_sbm Del_T 5 K, model stop")
 	endif

  tt = t_new

 	RETURN
 	END SUBROUTINE COAL_BOTT_NEW
 ! ..................................................................................................
     SUBROUTINE BREAKINIT_KS(PKIJ,QKJ,ECOALMASSM,BRKWEIGHT,XL_r,DROPRADII,BR_MAX,JBREAK,JMAX,NKR,VR1)

     USE module_domain
     USE module_dm

     IMPLICIT NONE

 ! ... Interface
     integer,intent(in) :: br_max, JBREAK, NKR, JMAX
     real(kind=r8size),intent(inout) :: ECOALMASSM(:,:),BRKWEIGHT(:)
     real,intent(in) :: XL_r(:), DROPRADII(:), VR1(:)
     real(kind=r4size),intent(inout) :: PKIJ(:,:,:),QKJ(:,:)
 ! ... Interface

     !REAL :: XL_r(size(NKR))
     INTEGER :: hujisbm_unit1
     LOGICAL, PARAMETER :: PRINT_diag=.FALSE.
     LOGICAL :: opened
     LOGICAL , EXTERNAL :: wrf_dm_on_monitor
     CHARACTER*80 errmess

 !.....INPUT VARIABLES
 !
 !     GT    : MASS DISTRIBUTION FUNCTION
 !     XT_MG : MASS OF BIN IN MG
 !     JMAX  : NUMBER OF BINS

 !.....LOCAL VARIABLES

     DOUBLE PRECISION :: XL_d(NKR), DROPRADII_d(NKR), VR1_d(NKR)
     INTEGER :: IE,JE,KE
     INTEGER,PARAMETER :: AP = 1
     INTEGER :: I,J,K,JDIFF
     REAL :: RPKIJ(JBREAK,JBREAK,JBREAK),RQKJ(JBREAK,JBREAK)
     REAL :: PI,D0,HLP
     DOUBLE PRECISION :: M(0:JBREAK),ALM
     REAL :: DBREAK(JBREAK),GAIN,LOSS

 !.....DECLARATIONS FOR INIT
     INTEGER :: IP,KP,JP,KQ,JQ
     REAL :: XTJ

     CHARACTER*256 FILENAME_P,FILENAME_Q, file_p, file_q

     xl_d = xl_r

     IE = JBREAK
     JE = JBREAK
     KE = JBREAK

     if(nkr == 43) file_p = 'SBM_input_43/'//'coeff_p43.dat'
     if(nkr == 43) file_q = 'SBM_input_43/'//'coeff_q43.dat'
     if(nkr == 33) file_p = 'SBM_input_33/'//'coeff_p_new_33.dat' ! new Version 33 (taken from 43bins)
     if(nkr == 33) file_q = 'SBM_input_33/'//'coeff_q_new_33.dat' ! new Version 33   (taken from 43 bins)

     hujisbm_unit1 = -1
     IF ( wrf_dm_on_monitor() ) THEN
         DO i = 20,99
             INQUIRE ( i , OPENED = opened )
             IF ( .NOT. opened ) THEN
                 hujisbm_unit1 = i
                 GOTO 2061
             ENDIF
         ENDDO
         2061     CONTINUE
     ENDIF

     CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , IWORDSIZE )

     IF ( hujisbm_unit1 < 0 ) THEN
       CALL wrf_error_fatal ( 'Can not find unused fortran unit to read in BREAKINIT_KS lookup table, model stop' )
     ENDIF

     IF ( wrf_dm_on_monitor() ) THEN
       OPEN(UNIT=hujisbm_unit1,FILE=trim(file_p),         &
       !OPEN(UNIT=hujisbm_unit1,FILE="coeff_p.asc",       &
            FORM="FORMATTED",STATUS="OLD",ERR=2070)

         DO K=1,KE
             DO I=1,IE
                 DO J=1,I
                 READ(hujisbm_unit1,'(3I6,1E16.8)') KP,IP,JP,PKIJ(KP,IP,JP) ! PKIJ=[g^3*cm^3/s]
                 ENDDO
             ENDDO
         ENDDO
         CLOSE(hujisbm_unit1)
     END IF

     hujisbm_unit1 = -1
     IF ( wrf_dm_on_monitor() ) THEN
       DO i = 20,99
         INQUIRE ( i , OPENED = opened )
         IF ( .NOT. opened ) THEN
           hujisbm_unit1 = i
           GOTO 2062
         ENDIF
       ENDDO
       2062     CONTINUE
     ENDIF

     CALL wrf_dm_bcast_bytes ( hujisbm_unit1 , IWORDSIZE )

     IF ( hujisbm_unit1 < 0 ) THEN
       CALL wrf_error_fatal ( 'Can not find unused fortran unit to read in BREAKINIT_KS lookup table, model stop' )
     ENDIF

     IF ( wrf_dm_on_monitor() ) THEN
      OPEN(UNIT=hujisbm_unit1,FILE=trim(file_q),    &
           FORM="FORMATTED",STATUS="OLD",ERR=2070)
          DO K=1,KE
             DO J=1,JE
                READ(hujisbm_unit1,'(2I6,1E16.8)') KQ,JQ,QKJ(KQ,JQ)
             ENDDO
          ENDDO
      CLOSE(hujisbm_unit1)
     END IF

     DROPRADII_d = DROPRADII
     vr1_d = vr1
     DO J=1,NKR
         DO I=1,NKR
             ECOALMASSM(I,J)=ECOALMASS(xl_d(I), xl_d(J), DROPRADII_d, vr1_d, NKR)
          ENDDO
     ENDDO
 ! Correction of coalescence efficiencies for drop collision kernels

     DO J=25,31
         ECOALMASSM(NKR,J)=0.1D-29
     ENDDO

       RETURN
 2070  continue
       WRITE( errmess , '(A,I4)' )                                          &
        'module_FAST_SBM: error opening hujisbm_DATA on unit, model stop'  &
        , hujisbm_unit1
       CALL wrf_error_fatal(errmess)
       END SUBROUTINE BREAKINIT_KS

 !coalescence efficiency as function of masses
 !----------------------------------------------------------------------------+
     double precision FUNCTION ecoalmass(x1, x2, DROPRADII, VR1_BREAKUP, NKR)

     implicit none
     integer,intent(in) :: NKR
     real(kind=r8size),intent(in) :: DROPRADII(NKR), VR1_BREAKUP(NKR), x1, x2

     real(kind=r8size),PARAMETER :: zero=0.0d0,one=1.0d0,eps=1.0d-10
     real(kind=r8size) :: rho, PI, akPI, Deta, Dksi

     rho=1.0d0             ! [rho]=g/cm^3

     PI=3.1415927d0
     akPI=6.0d0/PI

     Deta = (akPI*x1/rho)**(1.0d0/3.0d0)
     Dksi = (akPI*x2/rho)**(1.0d0/3.0d0)

     ecoalmass = ecoaldiam(Deta, Dksi, DROPRADII, VR1_BREAKUP, NKR)

     RETURN
     END FUNCTION ecoalmass
 !coalescence efficiency as function of diameters
 !---------------------------------------------------------------------------+
     double precision FUNCTION ecoaldiam(Deta,Dksi,DROPRADII,VR1_BREAKUP,NKR)

     implicit none
     integer,intent(in) :: NKR
     real(kind=r8size),intent(in) :: DROPRADII(nkr), VR1_BREAKUP(nkr),Deta,Dksi

     real(kind=r8size) :: Dgr, Dkl, Rgr, RKl, q, qmin, qmax, e, x, e1, e2, sin1, cos1
     real(kind=r8size),PARAMETER :: zero=0.0d0,one=1.0d0,eps=1.0d-30,PI=3.1415927d0

     Dgr=dmax1(Deta,Dksi)
     Dkl=dmin1(Deta,Dksi)

     Rgr=0.5d0*Dgr
     Rkl=0.5d0*Dkl

     q=0.5d0*(Rkl+Rgr)

     qmin=250.0d-4
     qmax=500.0d-4

     if(Dkl<100.0d-4) then

         e=1.0d0

          elseif (q<qmin) then

          e = ecoalOchs(Dgr,Dkl,DROPRADII, VR1_BREAKUP, NKR)

     elseif(q>=qmin.and.q<qmax) then

         x=(q-qmin)/(qmax-qmin)

         sin1=dsin(PI/2.0d0*x)
         cos1=dcos(PI/2.0d0*x)

         e1=ecoalOchs(Dgr, Dkl, DROPRADII, VR1_BREAKUP, NKR)
         e2=ecoalLowList(Dgr, Dkl, DROPRADII, VR1_BREAKUP, NKR)

         e=cos1**2*e1+sin1**2*e2

     elseif(q>=qmax) then

         e=ecoalLowList(Dgr, Dkl, DROPRADII, VR1_BREAKUP, NKR)

     else

         e=0.999d0

     endif

     ecoaldiam=dmax1(dmin1(one,e),eps)

 RETURN
 END FUNCTION ecoaldiam
 !coalescence efficiency (Low & List)
 !----------------------------------------------------------------------------+
     double precision FUNCTION ecoalLowList(Dgr,Dkl,DROPRADII,VR1_BREAKUP,NKR)

     implicit none

     integer,intent(in) :: NKR
     real(kind=r8size),intent(in) :: DROPRADII(NKR), VR1_BREAKUP(NKR)
     real(kind=r8size),intent(inout) :: Dgr, Dkl

     real(kind=r8size) :: sigma, aka, akb, dSTSc, ST, Sc, ET, CKE, qq0, qq1, qq2, Ecl, W1, W2, DC
     real(kind=r8size),PARAMETER :: epsi=1.d-20

 ! 1 J = 10^7 g cm^2/s^2

     sigma=72.8d0    ! Surface Tension,[sigma]=g/s^2 (7.28E-2 N/m)
     aka=0.778d0      ! Empirical Constant
     akb=2.61d-4      ! Empirical Constant,[b]=2.61E6 m^2/J^2

     CALL collenergy(Dgr,Dkl,CKE,ST,Sc,W1,W2,Dc,DROPRADII,VR1_BREAKUP,NKR)

     dSTSc=ST-Sc         ! Diff. of Surf. Energies   [dSTSc] = g*cm^2/s^2
     ET=CKE+dSTSc        ! Coal. Energy,             [ET]    =     "

     IF(ET<50.0d0) THEN    ! ET < 5 uJ (= 50 g*cm^2/s^2)

         qq0=1.0d0+(Dkl/Dgr)
         qq1=aka/qq0**2
         qq2=akb*sigma*(ET**2)/(Sc+epsi)
         Ecl=qq1*dexp(-qq2)

     !if(i_breakup==24.and.j_breakup==25) then
     !print*, 'IF(ET<50.0d0) THEN'
     !print*, 'Ecl=qq1*dexp(-qq2)'
     !print*, 'qq1,qq2,Ecl'
     !print*,  qq1,qq2,Ecl
     !endif

     ELSE

         Ecl=0.0d0

     ENDIF

     ecoalLowList=Ecl

     RETURN
     END FUNCTION ecoalLowList

 !coalescence efficiency (Beard and Ochs)
 !---------------------------------------------------------------------------+
     double precision FUNCTION ecoalOchs(D_l,D_s,DROPRADII, VR1_BREAKUP,NKR)

     implicit none

     integer,intent(in) :: NKR
     real(kind=r8size),intent(in) :: DROPRADII(NKR), VR1_BREAKUP(NKR), D_l, D_s

     real(kind=r8size) :: PI, sigma, R_s, R_l, p, vTl, vTs, dv, Weber_number, pa1, pa2, pa3, g, x, e
     real(kind=r8size),PARAMETER :: epsf=1.d-30 , FPMIN=1.d-30

     PI=3.1415927d0
     sigma=72.8d0       ! Surface Tension [sigma] = g/s^2 (7.28E-2 N/m)
                    ! Alles in CGS (1 J = 10^7 g cm^2/s^2)
     R_s=0.5d0*D_s
     R_l=0.5d0*D_l
     p=R_s/R_l

     vTl=vTBeard(D_l,DROPRADII, VR1_BREAKUP,NKR)

     vTs=vTBeard(D_s,DROPRADII, VR1_BREAKUP,NKR)

     dv=dabs(vTl-vTs)

     if(dv<FPMIN) dv=FPMIN

     Weber_number=R_s*dv**2/sigma

     pa1=1.0d0+p
     pa2=1.0d0+p**2
     pa3=1.0d0+p**3

     g=2**(3.0d0/2.0d0)/(6.0d0*PI)*p**4*pa1/(pa2*pa3)
     x=Weber_number**(0.5d0)*g

     e=0.767d0-10.14d0*x

     ecoalOchs=e

     RETURN
     END FUNCTION ecoalOchs
 !ecoalOchs
 !Calculating the Collision Energy
 !------------------------------------------------------------------------------+
     SUBROUTINE COLLENERGY(Dgr,Dkl,CKE,ST,Sc,W1,W2,Dc,DROPRADII,VR1_BREAKUP,NKR)


     implicit none
     integer,intent(in) :: NKR
     real(kind=r8size),intent(in) :: DROPRADII(NKR), VR1_BREAKUP(NKR)
     real(kind=r8size),intent(inout) :: Dgr, Dkl, CKE, ST, Sc, W1, W2, Dc

     real(kind=r8size) :: PI, rho, sigma, ak10, Dgka2, Dgka3, v1, v2, dv, Dgkb3
     real(kind=r8size),PARAMETER :: epsf = 1.d-30, FPMIN = 1.d-30

     !EXTERNAL vTBeard

     PI=3.1415927d0
     rho=1.0d0            ! Water Density,[rho]=g/cm^3
     sigma=72.8d0         ! Surf. Tension,(H2O,20°C)=7.28d-2 N/m
                      ! [sigma]=g/s^2
     ak10=rho*PI/12.0d0

     Dgr=dmax1(Dgr,epsf)
     Dkl=dmax1(Dkl,epsf)

     Dgka2=(Dgr**2)+(Dkl**2)

     Dgka3=(Dgr**3)+(Dkl**3)

     if(Dgr/=Dkl) then

         v1=vTBeard(Dgr,DROPRADII, VR1_BREAKUP,NKR)
         v2=vTBeard(Dkl,DROPRADII, VR1_BREAKUP,NKR)
         dv=(v1-v2)
         if(dv<FPMIN) dv=FPMIN
         dv=dv**2
         if(dv<FPMIN) dv=FPMIN
         Dgkb3=(Dgr**3)*(Dkl**3)
         CKE=ak10*dv*Dgkb3/Dgka3            ! Collision Energy [CKE]=g*cm^2/s^2

 !if(i_breakup==24.and.j_breakup==25) then
 !print*, 'Dgr,Dkl'
 !print*,  Dgr,Dkl
 !print*, 'Dgkb3,Dgka2,Dgka3,ak10'
 !print*,  Dgkb3,Dgka2,Dgka3,ak10
 !print*, 'v1,v2,dv,CKE'
 !print*,  v1,v2,dv,CKE
 !endif

     else

         CKE = 0.0d0

     endif

     ST=PI*sigma*Dgka2                 ! Surf.Energy (Parent Drop)
     Sc=PI*sigma*Dgka3**(2.0d0/3.0d0)  ! Surf.Energy (coal.System)

     W1=CKE/(Sc+epsf)                  ! Weber Number 1
     W2=CKE/(ST+epsf)                  ! Weber Number 2

     Dc=Dgka3**(1.0d0/3.0d0)           ! Diam. of coal. System

 !if(i_breakup==24.and.j_breakup==25) then
 !print*, 'ST,Sc,W1,W2,dc'
 !print*,  ST,Sc,W1,W2,dc
 !endif

     RETURN
     END SUBROUTINE COLLENERGY
 !COLLENERGY
 !Calculating Terminal Velocity (Beard-Formula)
 !------------------------------------------------------------------------+
 ! new change 23.07.07                                         (start)
     double precision FUNCTION vTBeard(diam,DROPRADII, VR1_BREAKUP, NKR)

     implicit none

     integer,intent(in) :: NKR
     real(kind=r8size),intent(in) :: DROPRADII(NKR), VR1_BREAKUP(NKR), diam

     integer :: kr
     real(kind=r8size) :: aa

     aa   = diam/2.0d0           ! Radius in cm

     IF(aa <= DROPRADII(1)) vTBeard=VR1_BREAKUP(1)
     IF(aa > DROPRADII(NKR)) vTBeard=VR1_BREAKUP(NKR)

     DO KR=1,NKR-1
         IF(aa>DROPRADII(KR).and.aa<=DROPRADII(KR+1)) then
             vTBeard=VR1_BREAKUP(KR+1)
         ENDIF
     ENDDO

     RETURN
     END FUNCTION vTBeard
     !vTBeard
 ! new change 23.07.07                                           (end)
 !........................................................................
       END MODULE module_mp_fast_sbm
#endif
