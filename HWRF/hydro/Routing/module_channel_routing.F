MODULE module_channel_routing
#ifdef MPP_LAND
  USE module_mpp_land
#endif
  IMPLICIT NONE

  contains

! ------------------------------------------------
!   FUNCTION MUSKING
! ------------------------------------------------
	REAL FUNCTION MUSKING(qup,quc,qdp,dt,Km,X)

	IMPLICIT NONE

!--local variables
        REAL    :: C1, C2, C3
        REAL    :: Km          !K travel time in hrs in reach
        REAL    :: X          !weighting factors 0<=X<=0.5 
        REAL    :: dt         !routing period in hrs
        REAL    :: avgbf      !average base flow for initial condition
        REAL    :: qup        !inflow from previous timestep
        REAL    :: quc        !inflow  of current timestep
        REAL    :: qdp        !outflow of previous timestep
        REAL    :: dth        !timestep in hours

        dth = dt/3600    !hours in timestep
        C1 = (dth - 2*Km*X)/(2*Km*(1-X)+dth)
        C2 = (dth+2*Km*X)/(2*Km*(1-X)+dth)
        C3 = (2*Km*(1-X)-dth)/(2*Km*(1-X)+dth)
        MUSKING = (C1*qup)+(C2*quc)+(C3*qdp)

! ----------------------------------------------------------------
  END FUNCTION MUSKING
! ----------------------------------------------------------------

! ------------------------------------------------
!   SUBROUTINE LEVELPOOL
! ------------------------------------------------

SUBROUTINE LEVELPOOL(ln,qi0,qi1,qo1,ql,dt,H,ar,we,wc,wl,oe,oc,oa)

    !! ----------------------------  argument variables
    !! All elevations should be relative to a common base (often belev(k))

    real, intent(INOUT) :: H       ! water elevation height (m)
    real, intent(IN)    :: dt      ! routing period [s]
    real, intent(IN)    :: qi0     ! inflow at previous timestep (cms)
    real, intent(IN)    :: qi1     ! inflow at current timestep (cms)
    real, intent(OUT)   :: qo1     ! outflow at current timestep
    real, intent(IN)    :: ql      ! lateral inflow
    real, intent(IN)    :: ar      ! area of reservoir (km^2)
    real, intent(IN)    :: we      ! weir elevation, max depth of reservoir before overtop (m)
    real, intent(IN)    :: wc      ! weir coeff.
    real, intent(IN)    :: wl      ! weir length (m)
    real, intent(IN)    :: oe      ! orifice elevation
    real, intent(IN)    :: oc      ! orifice coeff.
    real, intent(IN)    :: oa      ! orifice area (m^2)
    integer, intent(IN) :: ln      ! lake number

    !! ----------------------------  local variables
    real :: sap                    ! local surface area values
    real :: discharge              ! storage discharge m^3/s
    real :: tmp1, tmp2
    real :: dh, dh1, dh2, dh3      ! height function and 3 order RK
    real :: It, Itdt_3, Itdt_2_3

    !! ----------------------------  subroutine body: from chow, mad mays. pg. 252
    !! -- determine from inflow hydrograph

    It = qi0
    Itdt_3   = (qi0 + (qi1 + ql))/3
    Itdt_2_3 = (qi0 + (qi1 + ql))/3 + Itdt_3

    !-- determine Q(dh) from elevation-discharge relationship
    !-- and dh1
    dh = H - we
    if (dh .gt. 0.0 ) then              !! orifice and overtop discharge
        tmp1 = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
        tmp2 = wc * wl * dh * sqrt(dh)
        discharge = tmp1 + tmp2
        sap = (ar * 1000.0**2) * (1 + (H - we) / H)
    else if ( H .gt. oe ) then     !! only orifice flow,not full
        discharge = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
        sap = ar * 1000.0**2
    else
        discharge = 0.0
        sap = ar * 1000.0**2
    endif
    dh1 = ((It - discharge)/sap)*dt

    !-- determine Q(H + dh1/3) from elevation-discharge relationship
    !-- dh2
    dh = (H+dh1/3) - we
    if (dh .gt. 0.0 ) then              !! orifice and overtop discharge
        tmp1 = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
        tmp2 = wc * wl * dh * sqrt(dh)
        discharge = tmp1 + tmp2
        sap = (ar * 1000.0**2) * (1 + (H - we) / H)
    else if ( H .gt. oe ) then     !! only orifice flow,not full
        discharge = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
        sap = ar * 1000.0**2
    else
        discharge = 0.0
        sap = ar * 1000.0**2
    endif
    dh2 = ((Itdt_3 - discharge)/sap)*dt

    !-- determine Q(H + 2/3 dh2) from elevation-discharge relationship
    !-- dh3
    dh = (H + (0.667*dh2)) - we
    if (dh .gt. 0.0 ) then              !! orifice and overtop discharge
        tmp1 = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
        tmp2 = wc * wl * dh * sqrt(dh)
        discharge = tmp1 + tmp2
        sap = (ar * 1000.0**2) * (1 + (H - we) / H)
    else if ( H .gt. oe ) then     !! only orifice flow,not full
        discharge = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
        sap = ar * 1000.0**2
    else
        discharge = 0.0
        sap = ar * 1000.0**2
    endif
    dh3 = ((Itdt_2_3 - discharge)/sap)*dt

    !-- determine dh and H
    dh = (dh1/4.) + (0.75*dh3)
    H = H + dh

    !-- compute final discharge
    dh = H - we

    if (dh .gt. 0.0 ) then              !! orifice and overtop discharge
        tmp1 = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
        tmp2 = wc * wl * dh * sqrt(dh)
        discharge = tmp1 + tmp2
        sap = (ar * 1000.0**2) * (1 + (H - we) / H)
    else if ( H .gt. oe ) then     !! only orifice flow,not full
        discharge = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
        sap = ar * 1000.0**2
    else
        discharge = 0.0
        sap = ar * 1000.0**2
    endif

    qo1  = discharge  ! return the flow rate from reservoir

23 format('botof H dh orf wr Q',f8.4,2x,f8.4,2x,f8.3,2x,f8.3,2x,f8.2)
24 format('ofonl H dh sap Q ',f8.4,2x,f8.4,2x,f8.0,2x,f8.2)

  RETURN

! ----------------------------------------------------------------
  END SUBROUTINE LEVELPOOL
! ----------------------------------------------------------------


! ------------------------------------------------
!   FUNCTION Diffusive wave
! ------------------------------------------------
        REAL FUNCTION DIFFUSION(nod,z1,z2,h1,h2,dx,n, &
                                Bw, Cs)
        IMPLICIT NONE
!-- channel geometry and characteristics
        REAL    :: Bw         !-bottom width (meters)
        REAL    :: Cs         !-Channel side slope slope
        REAL    :: dx         !-channel lngth (m)
        REAL    :: n          !-mannings coefficient
        REAL    :: R          !-Hydraulic radius
        REAL    :: AREA       !- wetted area
        REAL    :: h1,h2      !-tmp height variables
        REAL    :: z1,z2      !-z1 is 'from', z2 is 'to' elevations
        REAL    :: z          !-channel side distance
        REAL    :: w          !-upstream weight
        REAL    :: Ku,Kd      !-upstream and downstream conveyance
        REAL    :: Kf         !-final face conveyance
        REAL    :: Sf         !-friction slope
        REAL    :: sgn        !-0 or 1 
        INTEGER :: nod         !- node

        if (n.le.0.0.or.Cs.le.0.or.Bw.le.0) then
#ifdef HYDRO_D
         print *, "error in Diffusion function ->channel coefficients"
         print *, "nod, n, Cs, Bw", nod, n, Cs, Bw 
         call hydro_stop()
#endif
        endif

!        Sf = ((z1+h1)-(z2+h2))/dx  !-- compute the friction slope
       !if(z1 .eq. z2) then
       ! Sf = ((z1-(z2-0.01))+(h1-h2))/dx  !-- compute the friction slope
       !else
         Sf = ((z1-z2)+(h1-h2))/dx  !-- compute the friction slope
       !endif

        sgn = SGNf(Sf)             !-- establish sign

        w = 0.5*(sgn + 1.)         !-- compute upstream or downstream weighting
        
        z = 1/Cs                   !--channel side distance (m)
        R = ((Bw+z*h1)*h1)/(Bw+2*h1*sqrt(1+z**2)) !-- Hyd Radius
        AREA = (Bw+z*h1)*h1        !-- Flow area
        Ku = (1/n)*(R**(2./3.))*AREA     !-- convenyance

        R = ((Bw+z*h2)*h2)/(Bw+2*h2*sqrt(1+z**2)) !-- Hyd Radius
        AREA = (Bw+z*h2)*h2        !-- Flow area
        Kd = (1/n)*(R**(2./3.))*AREA     !-- convenyance

        Kf =  (1-w)*Kd + w*Ku      !-- conveyance 
        DIFFUSION = Kf * sqrt(abs(Sf))*sgn


100     format('z1,z2,h1,h2,kf,Dif, Sf, sgn  ',f8.3,2x,f8.3,2x,f8.4,2x,f8.4,2x,f8.3,2x,f8.3,2x,f8.3,2x,f8.0)

  END FUNCTION DIFFUSION
! ----------------------------------------------------------------

! ------------------------------------------------
!   FUNCTION MUSKINGUM CUNGE
! ------------------------------------------------
        REAL FUNCTION MUSKINGCUNGE(index,qup, quc, qdp, ql,&
                                   dt,So,dx,n,Cs,Bw)
        IMPLICIT NONE

!--local variables
        REAL    :: C1, C2, C3, C4
        REAL    :: Km         !K travel time in hrs in reach
        REAL    :: X          !weighting factors 0<=X<=0.5
        REAL    :: dt         !routing period in  seconds
        REAL    :: qup        !flow upstream previous timestep
        REAL    :: quc        !flow upstream current timestep
        REAL    :: qdp        !flow downstream previous timestep
        REAL    :: ql         !lateral inflow through reach (m^3/sec)
        REAL    :: Ck         ! wave celerity (m/s)
        REAL    :: qp         ! peak flow

!-- channel geometry and characteristics
        REAL    :: Bw         ! bottom width (meters)
        REAL    :: Cs         ! Channel side slope slope
        REAL    :: So         ! Channel bottom slope
        REAL    :: dx         ! channel lngth (m)
        REAL    :: n          ! mannings coefficient
        REAL    :: Tw         ! top width at peak flow
        REAL    :: AREA       ! Cross sectional area m^2
        REAL    :: Z          ! trapezoid distance (m)
        REAL    :: R          ! Hydraulic radius
        REAL    :: WP         ! wetted perimmeter
        REAL    :: h          ! depth of flow
        REAL    :: Qj         ! intermediate flow estimate
        REAL    :: D,D1       ! diffusion coeff
        REAL    :: dtr        ! required timestep, minutes
        REAL    :: error,shapefn, sh1, sh2, sh3
        REAL    :: hp         !courant, previous height
        INTEGER :: maxiter    !maximum number of iterations

!-- local variables.. needed if channel is sub-divded
        REAL    :: c,b
        REAL    :: dxlocal
        INTEGER :: i,index     !-- channel segment counter
        INTEGER :: ChnSegments !-- number of channel sub-sections

        c = 0.2407  !-- coefficnets for finding dx/Ckdt
        b = 1.16065

        z = 1/Cs              !channel side distance (m)
        h = sqrt(quc+ql)*0.1   !-- assume a initial depth (m)
        qp = quc + ql

        if (n.le.0.or.So.le.0.or.z.le.0.or.Bw.le.0) then
#ifdef HYDRO_D
          print*, "error in channel coefficients -> Muskingum cunge"
          call hydro_stop()
#endif
        end if

        error = 1.0
        maxiter = 0

        if (quc .gt.0) then  !--top of link must have some water in it
         do while (error .gt. 0.01 .and. maxiter < 100)  !-- first estimate depth at top of channel
          maxiter = maxiter + 1
          !---trapezoidal channel shape function
          shapefn = SHAPE(Bw,z,h)
          Qj = FLOW(n,So,Bw,h,z)
          h = h - (1-quc/Qj)/(shapefn)
          error = abs((Qj - quc)/quc)
         end do
        endif

        maxiter = 0
!------- approximate flow and depth at the bottom of the channel
        if (ql .eq.0 .and. quc .eq. 0) then  !-- no water to route
          Qj=0.0
        else
          error = 1.0                        !--reset the error
          Tw = Bw + 2*z*h                    !--top width of the channel inflow
          Ck = (sqrt(So)/n)*(5/3)*h**0.667  !-- pg 287 Chow, Mdt, Mays
          X = 0.5-(qp/(2*Tw*So*Ck*dx))
          if (X.le.0) then 
#ifdef HYDRO_D
           print *, "Muskingum weighting factor is less than 0"
#endif
          endif

        if ( dx/(Ck*dt) .le. c*LOG(X)+b) then  !-- Bedient and Huber pg. 296
            ChnSegments = 1
            dxlocal = dx
         else
           dxlocal = fnDX(qp,Tw,So, Ck,dx,dt) !-- find appropriate channel length
           X = 0.5-(qp/(2*Tw*So*Ck*dxlocal))
           if(FRACTION(dx/dxlocal) .le. 0.5) then  !-- round up 
            ChnSegments = NINT(dx/dxlocal) + 1
           else
            ChnSegments = NINT(dx/dxlocal)
           endif
          dxlocal = dx/ChnSegments !-- compute segment length, which will
        endif

        do i = 1, ChnSegments
         error = 1.0                        !--reset the error

         do while (error .gt. 0.01 .and. maxiter < 500)

           if (qp.gt.2*(2*Tw*So*Ck*dxlocal)) then
#ifdef HYDRO_D
            print *, "ERROR IN Musking Cunge,X <0 ", X
            print *, "X,Qp,Tw,So,Ck,Dxlocal",X,Qp,Tw,So,Ck,Dxlocal
#endif
           endif

           Km = dxlocal/Ck                      !-- minutes,Muskingum Param
           D = (Km*(1 - X) + dt/2)              !-- minutes
           C1 = (Km*X + dt/2)/D
           C2 = (dt/2 - Km*X)/D
           C3 = (Km*(1-X)-dt/2)/D
           C4 = (ql/ChnSegments*dt)/D           !-- lateral inflow is along each channel sub-section

           MUSKINGCUNGE = (C1*qup)+(C2*quc)+(C3*qdp)+C4 !-- pg 295 Bedient huber assume flows from previous
                                                        !--previous values same in each segment,a good assumption?
           if (MUSKINGCUNGE .lt. 0) then !-- only outflow
#ifdef HYDRO_D
            print *, "ERROR: musking cunge is negative"
            print *, "D, C1+C2+C3,C4, MsCng",D,C1+C2+C3,C4,Muskingcunge
            print *, "qup, quc, qdp, ql",qup,quc,qdp,ql,i,ChnSegments
#endif
            Qj = 0.0
            error = 0.001
           else 
!---trapezoidal channel shape function
            shapefn = SHAPE(Bw,z,h)
            Qj = FLOW(n,So,Bw,h,z)
            h = h - (1-MUSKINGCUNGE/Qj)/(shapefn)
            error = abs((Qj - MUSKINGCUNGE)/MUSKINGCUNGE)
            if (h<0.00001) error=0.001  !--very small flow depths to route
            Tw = Bw+2*z*h
            hp=h
            maxiter = maxiter + 1
           endif 
          enddo !-- while error condtion number of 
            if (ChnSegments .gt.1) then
             quc =  MUSKINGCUNGE !-- update condition for next channel length upstream
            endif
        enddo !-- number of channel segment loops
       endif

       MUSKINGCUNGE = Qj

         if(index .eq. 1 .or. index .eq. 2 .or. index .eq. 6) then
#ifdef HYDRO_D
           write(*,13) index, ql,quc,qup,Qj,qdp
#endif
         endif

10      format('Tw,h,Z, latflow,usf',f3.1,2x,f8.4,2x,f4.1,2x,f5.4,2x,f5.4)
11      format('h, Qj, Musking, error',f8.4,2x,f8.4,2x,f8.4,2x,f8.4)
12      format('X, Km, Ck, dtcrv',f8.2,2x,f8.1,2x,f8.1,2x,f6.4)
13      format('ql,quc,qup,qdc,qdp',i2,2x,f8.2,2x,f8.2,2x,f8.2,2x,f8.2,2x,f8.2)

! ----------------------------------------------------------------
  END FUNCTION MUSKINGCUNGE
! ----------------------------------------------------------------

! ------------------------------------------------
!   FUNCTION KINEMATIC
! ------------------------------------------------
	REAL FUNCTION KINEMATIC()

	IMPLICIT NONE

! -------- DECLARATIONS -----------------------
 
!	REAL, INTENT(OUT), DIMENSION(IXRT,JXRT)	:: OVRGH

        KINEMATIC = 1       
!----------------------------------------------------------------
  END FUNCTION KINEMATIC
!----------------------------------------------------------------


! ------------------------------------------------
!   SUBROUTINE drive_CHANNEL
! ------------------------------------------------
     Subroutine drive_CHANNEL(KT, IXRT,JXRT, SUBRTSWCRT, &
       QSUBRT, LAKEINFLORT, QSTRMVOLRT, TO_NODE, FROM_NODE, &
       TYPEL, ORDER, MAXORDER, NLINKS, CH_NETLNK, CH_NETRT, &
       LAKE_MSKRT, DT, DTRT, MUSK, MUSX, QLINK, &
       HLINK, ELRT, CHANLEN, MannN, So, ChSSlp, Bw, &
       RESHT, HRZAREA, LAKEMAXH, WEIRC, WEIRL, ORIFICEC, ORIFICEA, &
       ORIFICEE, ZELEV, CVOL, NLAKES, QLAKEI, QLAKEO, LAKENODE, &
       dist, QINFLOWBASE, CHANXI, CHANYJ, channel_option, RETDEP_CHAN &
       ,node_area &
#ifdef MPP_LAND 
       ,lake_index,link_location,mpp_nlinks,nlinks_index,yw_mpp_nlinks &
#endif
       )

       IMPLICIT NONE

! -------- DECLARATIONS ------------------------

        INTEGER, INTENT(IN) :: IXRT,JXRT,channel_option
        INTEGER, INTENT(IN) :: NLINKS,NLAKES  
        integer, INTENT(INOUT) :: KT   ! flag of cold start (1) or continue run.
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)    :: QSUBRT
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)    :: QSTRMVOLRT
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)    :: LAKEINFLORT
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)    :: ELRT
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)    :: QINFLOWBASE
        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT) :: CH_NETLNK
        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT) :: CH_NETRT


        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT) :: LAKE_MSKRT
        INTEGER, INTENT(IN), DIMENSION(NLINKS)    :: ORDER, TYPEL !--link
        INTEGER, INTENT(IN), DIMENSION(NLINKS)    :: TO_NODE, FROM_NODE
        INTEGER, INTENT(IN), DIMENSION(NLINKS)    :: CHANXI, CHANYJ
        REAL,    INTENT(IN), DIMENSION(NLINKS)    :: ZELEV  !--elevatio of nodes
        REAL, INTENT(INOUT), DIMENSION(NLINKS)    :: CVOL
        REAL, INTENT(IN), DIMENSION(NLINKS)       :: MUSK, MUSX
        REAL, INTENT(IN), DIMENSION(NLINKS)       :: CHANLEN
        REAL, INTENT(IN), DIMENSION(NLINKS)       :: So, MannN
        REAL, INTENT(IN), DIMENSION(NLINKS)       :: ChSSlp,Bw  !--properties of nodes or links
        REAL                                      :: Km, X
        REAL , INTENT(INOUT), DIMENSION(NLINKS,2) :: QLINK
        REAL , INTENT(INOUT), DIMENSION(NLINKS)   :: HLINK
        REAL, INTENT(IN)                          :: DT    !-- model timestep
        REAL, INTENT(IN)                          :: DTRT  !-- routing timestep
        REAL                                      :: DTCT, dist(ixrt,jxrt,9)
        REAL                                      :: RETDEP_CHAN
        INTEGER, INTENT(IN)                       :: MAXORDER, SUBRTSWCRT
        REAL , INTENT(IN), DIMENSION(NLINKS)   :: node_area

        !-- lake params
        REAL, INTENT(IN), DIMENSION(NLINKS)       :: HRZAREA  !-- horizontal area (km^2)
        REAL, INTENT(IN), DIMENSION(NLINKS)       :: LAKEMAXH !-- maximum lake depth  (m^2)
        REAL, INTENT(IN), DIMENSION(NLINKS)       :: WEIRC    !-- weir coefficient
        REAL, INTENT(IN), DIMENSION(NLINKS)       :: WEIRL    !-- weir length (m)
        REAL, INTENT(IN), DIMENSION(NLINKS)       :: ORIFICEC !-- orrifice coefficient
        REAL, INTENT(IN), DIMENSION(NLINKS)       :: ORIFICEA !-- orrifice area (m^2)
        REAL, INTENT(IN), DIMENSION(NLINKS)       :: ORIFICEE !-- orrifce elevation (m)

        REAL, INTENT(INOUT), DIMENSION(NLINKS)    :: RESHT    !-- reservoir height (m)
        REAL, INTENT(INOUT), DIMENSION(NLINKS)    :: QLAKEI   !-- lake inflow (cms)
        REAL,                DIMENSION(NLINKS)    :: QLAKEIP  !-- lake inflow previous timestep (cms)
        REAL, INTENT(INOUT), DIMENSION(NLINKS)    :: QLAKEO   !-- outflow from lake used in diffusion scheme
        INTEGER, INTENT(IN), DIMENSION(NLINKS)    :: LAKENODE !-- outflow from lake used in diffusion scheme
        REAL, DIMENSION(NLINKS)                   :: QLateral !--lateral flux
        REAL, DIMENSION(NLINKS)                   :: QSUM     !--mass bal of node
        REAL, DIMENSION(NLINKS)                   :: QLLAKE   !-- lateral inflow to lake in diffusion scheme

!-- Local Variables
        INTEGER                     :: i,j,k,m,kk,KRT,node
        INTEGER                     :: DT_STEPS            !-- number of timestep in routing
        REAL                        :: qu,qd               !--upstream, downstream flow
        REAL                        :: bo               !--critical depth, bnd outflow just for testing

        REAL, DIMENSION(NLINKS,2)   :: QLINKPREV         !-- temporarily store qlink value
        REAL ,DIMENSION(NLINKS)     :: HLINKTMP,CVOLTMP  !-- temporarily store head values and volume values
        REAL ,DIMENSION(NLINKS)     :: CD                !-- critical depth
        real, DIMENSION(IXRT,JXRT) :: tmp
        real, dimension(nlinks) :: tmp2

#ifdef MPP_LAND
        integer lake_index(nlakes)
        integer nlinks_index(nlinks)
        integer mpp_nlinks, iyw, yw_mpp_nlinks
        integer link_location(ixrt,jxrt)
        real     ywtmp(ixrt,jxrt)
#endif
        integer flag

        QLAKEIP = 0
        QLINKPREV = 0
        HLINKTMP = 0
        CVOLTMP = 0
        CD = 0  

!yw        node = 3924
        node = 1


        QLateral = 0
        QSUM     = 0
        QLLAKE   = 0
        

        IF(channel_option .ne. 3) then   !--muskingum methods ROUTE ON DT timestep, not DTRT!!
#ifdef MPP_LAND
#ifdef HYDRO_D
   write(6,*) "Error: not parallelized"
   call hydro_stop() 
#endif
#endif
         DT_STEPS = 1

         DO KRT=1,DT_STEPS                 !-- route over routing timestep
          do k = 1, NLINKS
             QLateral(k)=0                        !--initial lateral flux to 0 for this reach
             do i = 1, IXRT
               do j = 1, JXRT
                !--------river grid points
                !!!!  IS THIS CORREECT BECAUSE CH_NETRT IS JUST A 0,1?????
                 if ( (CH_NETRT(i,j) .eq. k) .and. (LAKE_MSKRT(i,j) .eq. -9999)) then
                   !--------river grid points
                   !-- convert total volume into flow rate across reach (m3/sec)
                   !-- QSUBRT and QSTRMVOLRT are mm for the DT interval, so
                   !-- you need to divided by the timestep fraction and
                   !-- multiply by DXRT^2 1m/1000mmm/DT 
                   QLateral(k) = QLateral(k) + ((QSUBRT(i,j)+QSTRMVOLRT(i,j))/DT_STEPS &
                      *dist(i,j,9)/1000/DT) + QINFLOWBASE(i,j)
                 elseif ( (LAKE_MSKRT(i,j) .eq. k)) then !-lake grid
                   !-- convert total volume into flow rate across reach (m3/sec)
                   QLateral(k) = QLateral(k) + (LAKEINFLORT(i,j)/DT_STEPS &
                      *dist(i,j,9)/1000/DT) + QINFLOWBASE(i,j)
                 endif
               end do
             end do
          end do
          
!----------  route order 1 reaches which have no upstream inflow
          do k=1, NLINKS
            if (ORDER(k) .eq. 1) then  !-- first order stream has no headflow

               if (KT .eq. 1) then  !-- initial slug of water in unpstream cells
                 qd = QLINK(k,1)
                 KT = KT + 1
               else
                 qd = QLINK(k,2)        !-- downstream outflow, previous timestep
                 QLINK(k,1) = 0
               endif

                if(TYPEL(k) .eq. 1) then    !-- level pool route of reservoir
                 !CALL LEVELPOOL(1,0.0, 0.0, qd, QLINK(k,2), QLateral(k), &
                 ! DT, RESHT(k), HRZAREA(k), LAKEMAXH(k), &
                 ! WEIRC(k), WEIRL(k), ORIFICEE(i), ORIFICEC(k), ORIFICEA(k) )
                elseif (channel_option .eq. 1) then
                  Km = MUSK(k)
                  X = MUSX(k)
                  QLINK(k,2) = MUSKING(QLINK(k,1), QLateral(k), qd, DT, Km, X) !--current outflow
                elseif (channel_option .eq. 2) then !-- upstream is assumed constant initial condition
                  QLINK(k,2) = MUSKINGCUNGE(k,QLINK(k,1),QLINK(k,1), qd, &
                                  QLateral(k), DT, So(k), CHANLEN(k), &
                                  MannN(k), ChSSLP(k), Bw(k))

                else
#ifdef HYDRO_D
                 print *, "No channel option selected"
                 call hydro_stop() 
#endif
                endif
            endif
          end do

      !---------- route other reaches, with upstream inflow
          do kk = 2, MAXORDER
             do k = 1, NLINKS
                qu  = 0
                if (ORDER(k) .eq. kk) then    !--do the orders sequentially
                   qd = QLINK(k,2)            !--downstream flow previous timestep

                   do m = 1, NLINKS
                      if (TO_NODE(m) .eq. FROM_NODE(k)) then
                        qu = qu + QLINK(m,2)  !--upstream previous timestep
                      endif
                   end do ! do m


                   if(TYPEL(k) .eq. 1) then   !--link is a reservoir
                   ! CALL LEVELPOOL(1,QLINK(k,1), qu, qd, QLINK(k,2), &
                   !  QLateral(k), DT, RESHT(k), HRZAREA(k), LAKEMAXH(k), &
                   !  WEIRC(k), WEIRL(k),ORIFICEE(k),  ORIFICEC(k), ORIFICEA(k))
                   elseif (channel_option .eq. 1) then  !muskingum routing
                       Km = MUSK(k)
                       X = MUSX(k)
                       QLINK(k,2) = MUSKING(QLINK(k,1),qu,qd,DT,Km,X)
                   elseif (channel_option .eq. 2) then ! muskingum cunge
                       QLINK(k,2) = MUSKINGCUNGE(k,QLINK(k,1), qu, qd, &
                                    QLateral(k), DT, So(k),  CHANLEN(k), &
                                    MannN(k), ChSSlp(k), Bw(k) )
                   else
#ifdef HYDRO_D
                    print *, " no channel option selected"
                    call hydro_stop() 
#endif
                   endif
                   QLINK(k,1) = qu    !save inflow to reach at current timestep
                                      !to be used as inflow from previous timestep
                                      !on next iteration
                endif     !--order == kk
             end do       !--k links
          end do          !--kk order

#ifdef HYDRO_D
          print *, "END OF ALL REACHES...",KRT,DT_STEPS
#endif

         END DO !-- krt timestep for muksingumcunge routing

!yw begin
         elseif(channel_option .eq. 3) then   !--- route using the diffusion scheme on nodes not links
#ifdef MPP_LAND
         call MPP_CHANNEL_COM_REAL(Link_location,ixrt,jxrt,HLINK,NLINKS,99)
         call MPP_CHANNEL_COM_REAL(Link_location,ixrt,jxrt,CVOL,NLINKS,99)
#endif
 
         KRT = 0                  !-- initialize the time counter
         DTCT = DTRT              !-- initialize the routing timestep to the timestep in namelist (s)
         HLINKTMP = HLINK         !-- temporary storage of the water elevations (m)
         CVOLTMP = CVOL           !-- temporary storage of the volume of water in channel (m^3)
         QLAKEIP = QLAKEI         !-- temporary lake inflow from previous timestep  (cms)


!        call MPP_CHANNEL_COM_REAL(Link_location,ixrt,jxrt,ZELEV,NLINKS,99)
crnt:    DO                      !-- loop on the courant condition
         QSUM   = 0              !-- initialize the total flow out of each cell to zero
         QLAKEI = 0              !-- set the lake inflow as zero
         QLLAKE = 0              !-- initialize each lake's lateral inflow to zero  
         DT_STEPS=INT(DT/DTCT)   !-- fix the timestep
         QLateral = 0. 


!-- vectorize
!--------------------- 
#ifdef MPP_LAND
         DO iyw = 1,yw_MPP_NLINKS
         i = nlinks_index(iyw)
#else
         DO i = 1,NLINKS
#endif
           if(node_area(i) .eq. 0) then
#ifdef HYDRO_D
               write(6,*) "Error: node_area(i) is zero. i=", i
               call hydro_stop() 
#endif
           endif

           if((CH_NETRT(CHANXI(i), CHANYJ(i) ) .eq. 0) .and. &
              (LAKE_MSKRT(CHANXI(i),CHANYJ(i)) .lt.0) ) then !--a reg. node
              QLateral(CH_NETLNK(CHANXI(i),CHANYJ(i))) =  &
! await subsfc exchg   ((QSUBRT(CHANXI(i),CHANYJ(i))+QSTRMVOLRT(CHANXI(i),CHANYJ(i))+&
                ((QSTRMVOLRT(CHANXI(i),CHANYJ(i))+&
                 QINFLOWBASE(CHANXI(i),CHANYJ(i))) &
                   /DT_STEPS*node_area(i)/1000/DTCT)
	if(Qlateral(CH_NETLNK(CHANXI(i),CHANYJ(i))).lt.0.) then
#ifdef HYDRO_D
               print*, "i, CHANXI(i),CHANYJ(i) = ", i, CHANXI(i),CHANYJ(i)
               print *, "NEGATIVE Lat inflow...",QLateral(CH_NETLNK(CHANXI(i),CHANYJ(i))), &
                         QSUBRT(CHANXI(i),CHANYJ(i)),QSTRMVOLRT(CHANXI(i),CHANYJ(i)), &
                         QINFLOWBASE(CHANXI(i),CHANYJ(i))
               call hydro_stop() 
#endif
        end if
            elseif(LAKE_MSKRT(CHANXI(i),CHANYJ(i)) .gt. 0 .and. &
               (LAKE_MSKRT(CHANXI(i),CHANYJ(i)) .ne. 9999)) then !--a lake node
              QLLAKE(LAKE_MSKRT(CHANXI(i),CHANYJ(i))) = &
                 QLLAKE(LAKE_MSKRT(CHANXI(i),CHANYJ(i))) + &
                 (LAKEINFLORT(CHANXI(i),CHANYJ(i))+ &
                 QINFLOWBASE(CHANXI(i),CHANYJ(i)) &
                 /DT_STEPS*node_area(i)/1000/DTCT)
            elseif(CH_NETRT(CHANXI(i),CHANYJ(i)) .gt. 0) then  !pour out of lake
                 QLateral(CH_NETLNK(CHANXI(i),CHANYJ(i))) =  &
                   QLAKEO(CH_NETRT(CHANXI(i),CHANYJ(i)))  !-- previous timestep
          endif
        ENDDO


#ifdef MPP_LAND
    call MPP_CHANNEL_COM_REAL(Link_location,ixrt,jxrt,QLateral,NLINKS,99)
    call MPP_CHANNEL_COM_REAL(LAKE_MSKRT   ,ixrt,jxrt,QLLAKE,NLINKS,99)
#endif


          !-- compute conveyances, with known depths (just assign to QLINK(,1)
          !--QLINK(,2) will not be used), QLINK is the flow across the node face
          !-- units should be m3/second.. consistent with QL (lateral flow)

#ifdef MPP_LAND
         DO iyw = 1,yw_MPP_NLINKS
         i = nlinks_index(iyw)
#else
           DO i = 1,NLINKS
#endif
           if (TYPEL(i) .eq. 0 .AND. HLINKTMP(FROM_NODE(i)) .gt. RETDEP_CHAN) then 
            
           if(from_node(i) .ne. to_node(i) ) &  ! added by Wei Yu
              QLINK(i,1)=DIFFUSION(i,ZELEV(FROM_NODE(i)),ZELEV(TO_NODE(i)), &
                HLINKTMP(FROM_NODE(i)),HLINKTMP(TO_NODE(i)), &
                CHANLEN(i), MannN(i), Bw(i), ChSSlp(i))

            else !--  we are just computing critical depth for outflow points
               QLINK(i,1) =0.
            endif
          ENDDO

#ifdef MPP_LAND
    call MPP_CHANNEL_COM_REAL(Link_location,ixrt,jxrt,QLINK(:,1),NLINKS,99)
#endif
 
          !-- compute total flow across face, into node
#ifdef MPP_LAND
         DO iyw = 1,yw_mpp_nlinks
         i = nlinks_index(iyw)
#else
          DO i = 1,NLINKS                                                 !-- inflow to node across each face
#endif
           if(TYPEL(i) .eq. 0) then                                       !-- only regular nodes have to attribute
            QSUM(TO_NODE(i)) = QSUM(TO_NODE(i)) + QLINK(i,1)
           endif
          END DO

#ifdef MPP_LAND
    call MPP_CHANNEL_COM_REAL(Link_location,ixrt,jxrt,qsum,NLINKS,99)
#endif

#ifdef MPP_LAND
         DO iyw = 1,yw_mpp_nlinks
         i = nlinks_index(iyw)
#else
          DO i = 1,NLINKS                                                 !-- outflow from node across each face
#endif
            QSUM(FROM_NODE(i)) = QSUM(FROM_NODE(i)) - QLINK(i,1)
          END DO
#ifdef MPP_LAND
    call MPP_CHANNEL_COM_REAL(Link_location,ixrt,jxrt,qsum,NLINKS,99)
#endif

         flag = 99
#ifdef MPP_LAND
         DO iyw = 1,yw_MPP_NLINKS
             i = nlinks_index(iyw)
#else
          DO i = 1, NLINKS                                                !--- compute volume and depth at each node
#endif
 
           if( TYPEL(i).eq.0 .and. CVOLTMP(i) .ge. 0.001 .and.(CVOLTMP(i)-QSUM(i)*DTCT)/CVOLTMP(i) .le. -0.01 ) then  
            flag = -99
            goto 999  
            endif 
          enddo 

999 continue
#ifdef MPP_LAND
        call mpp_same_int1(flag)
#endif


        if(flag < 0 )   then   
             if(DTCT .gt. 0.001) then                  !-- timestep in seconds
              DTCT = DTCT/2                            !-- 1/2 timestep
              KRT = 0                                  !-- restart counter
              HLINKTMP = HLINK                         !-- set head and vol to start value of timestep
              CVOLTMP = CVOL
              CYCLE crnt                               !-- start cycle over with smaller timestep
             else
#ifdef HYDRO_D
              write(6,*) "Error ..... with small DTCT",DTCT
#endif
              DTCT = 0.1
              HLINKTMP = HLINK                          !-- set head and volume to start values of timestep
              CVOLTMP  = CVOL
              goto 998  
             end if
        endif 

998 continue


#ifdef MPP_LAND
         DO iyw = 1,yw_MPP_NLINKS
            i = nlinks_index(iyw)
#else
         DO i = 1, NLINKS                                                !--- compute volume and depth at each node
#endif
 
            if(TYPEL(i) .eq. 0) then                   !--  regular channel grid point, compute volume
              CVOLTMP(i) = CVOLTMP(i) + (QSUM(i) + QLateral(i) )* DTCT
              if(CVOLTMP(i) .lt. 0) then 
#ifdef HYDRO_D
                print *, "warning! channel volume less than 0:i,CVOL,QSUM,QLat", &
                               i, CVOLTMP(i),QSUM(i),QLateral(i),HLINK(i)
#endif
                CVOLTMP(i) =0 
              endif

            elseif(TYPEL(i) .eq. 1) then               !-- pour point, critical depth downstream 

              if (QSUM(i)+QLateral(i) .lt. 0) then
              else

               CD(i) =CRITICALDEPTH(i,abs(QSUM(i)+QLateral(i)), Bw(i), 1./ChSSlp(i))

              endif
               ! change in volume is inflow, lateral flow, and outflow 
               CVOLTMP(i) = CVOLTMP(i) + (QSUM(i) + QLateral(i) - &
               !yw DIFFUSION(i,ZELEV(i),ZELEV(i)-(So(i)*DXRT),HLINKTMP(i), &
               DIFFUSION(i,ZELEV(i),ZELEV(i)-(So(i)*CHANLEN(i)),HLINKTMP(i), &
               CD(i),CHANLEN(i), MannN(i), Bw(i), ChSSlp(i)) ) * DTCT

            elseif (TYPEL(i) .eq. 2) then              !--- into a reservoir, assume critical depth
              if (QSUM(i)+QLateral(i) .lt. 0) then
#ifdef HYDRO_D
               print *, i, 'CrtDpth Qsum+QLat into lake< 0',QSUM(i), QLateral(i)
#endif
              else
               CD(i) =CRITICALDEPTH(i,abs(QSUM(i)+QLateral(i)), Bw(i), 1./ChSSlp(i))
              endif
 
              !-- compute volume in reach (m^3)
              CVOLTMP(i) = CVOLTMP(i) + (QSUM(i) + QLateral(i) - &
              DIFFUSION(i,ZELEV(i),ZELEV(i)-(So(i)*CHANLEN(i)),HLINKTMP(i), &
              CD(i) ,CHANLEN(i), MannN(i), Bw(i), ChSSlp(i)) ) * DTCT
 
              !-- compute flow rate into lake from all contributing nodes (cms)
              QLAKEI(LAKENODE(i)) = QLAKEI(LAKENODE(i)) + QLINK(FROM_NODE(i),1)

            else
#ifdef HYDRO_D
              print *, "this node does not have a type.. error"
              call hydro_stop() 
#endif
            endif
           
           if(TYPEL(i) == 0) then !-- regular channel node, finalize head and flow
            HLINKTMP(i) = HEAD(i, CVOLTMP(i)/CHANLEN(i),Bw(i),1/ChSSlp(i))  !--updated depth 
           else
            HLINKTMP(i) = CD(i)  !!!   CRITICALDEPTH(i,QSUM(i)+QLateral(i), Bw(i), 1./ChSSlp(i)) !--critical depth is head
           endif 

           END DO  !--- done processing all the links

#ifdef MPP_LAND
    call MPP_CHANNEL_COM_REAL(Link_location,ixrt,jxrt,CVOLTMP,NLINKS,99)
    call MPP_CHANNEL_COM_REAL(Link_location,ixrt,jxrt,CD,NLINKS,99)
    call MPP_CHANNEL_COM_REAL(LAKE_MSKRT,ixrt,jxrt,QLAKEI,NLAKES,99)
    call MPP_CHANNEL_COM_REAL(Link_location,ixrt,jxrt,HLINKTMP,NLINKS,99)
#endif 

!         call check_channel(92,CVOLTMP,nlinks_index,mpp_nlinks,nlinks)
!         call check_channel(91,CD,nlinks_index,mpp_nlinks,nlinks)
!         call check_channel(55,QLAKEI,nlinks_index,mpp_nlinks,nlinks)
!         call check_channel(56,HLINKTMP,nlinks_index,mpp_nlinks,nlinks)


           do i = 1, NLAKES !-- mass balances of lakes
#ifdef MPP_LAND
            if(lake_index(i) .gt. 0)  then
#endif
              CALL LEVELPOOL(i,QLAKEIP(i), QLAKEI(i), QLAKEO(i), QLLAKE(i), &
                DTCT, RESHT(i), HRZAREA(i), LAKEMAXH(i), WEIRC(i), &
                WEIRL(i), ORIFICEE(i), ORIFICEC(i), ORIFICEA(i))
                QLAKEIP(i) = QLAKEI(i)  !-- store total lake inflow for this timestep
#ifdef MPP_LAND
            endif
#endif
           enddo
#ifdef MPP_LAND
    call MPP_CHANNEL_COM_REAL(LAKE_MSKRT   ,ixrt,jxrt,QLLAKE,NLINKS,99)
    call MPP_CHANNEL_COM_REAL(LAKE_MSKRT,ixrt,jxrt,RESHT,NLAKES,99)
    call MPP_CHANNEL_COM_REAL(LAKE_MSKRT,ixrt,jxrt,QLAKEO,NLAKES,99)
    call MPP_CHANNEL_COM_REAL(LAKE_MSKRT,ixrt,jxrt,QLAKEI,NLAKES,99)
    call MPP_CHANNEL_COM_REAL(LAKE_MSKRT,ixrt,jxrt,QLAKEIP,NLAKES,99)
#endif


#ifdef MPP_LAND
         DO iyw = 1,yw_MPP_NLINKS
            i = nlinks_index(iyw)
#else
         DO i = 1, NLINKS                                                !--- compute volume and depth at each node
#endif
            if(TYPEL(i) == 0) then !-- regular channel node, finalize head and flow
               QLINK(i,1)=DIFFUSION(i,ZELEV(FROM_NODE(i)),ZELEV(TO_NODE(i)), &
                  HLINKTMP(FROM_NODE(i)),HLINKTMP(TO_NODE(i)), &
                  CHANLEN(i), MannN(i), Bw(i), ChSSlp(i))
            endif
         enddo

#ifdef MPP_LAND
          call MPP_CHANNEL_COM_REAL(Link_location,ixrt,jxrt,QLINK(:,1),NLINKS,99)
#endif

           KRT = KRT + 1                     !-- iterate on the timestep
           IF(KRT .eq. DT_STEPS) EXIT crnt   !-- up to the maximum time in interval

          END DO crnt  !--- DTCT timestep of DT_STEPS
 
           HLINK = HLINKTMP                 !-- update head based on final solution in timestep
           CVOL  = CVOLTMP                  !-- update volume
        else                                            !-- no channel option apparently selected
#ifdef HYDRO_D
         print *, "no channel option selected"
         call hydro_stop() 
#endif
        endif

#ifdef HYDRO_D
         write(6,*) "finished call drive_CHANNEL"
#endif

        if (KT .eq. 1) KT = KT + 1
         

 END SUBROUTINE drive_CHANNEL
! ----------------------------------------------------------------

!--================== utility functions
     REAL FUNCTION SHAPE(Bw,z,h)
     REAL :: Bw, z, h
     REAL :: sh1, sh2, sh3
          !---trapezoidal channel shape function
          sh1 = (Bw+2*z*h)*(5*Bw + 6*h*sqrt(1+z**2))
          sh2 = 4*z*h**2*sqrt(1+z**2)
          sh3 = (3*h*(Bw+z*h)*(Bw+2*h*sqrt(1+z**2)))
          if (sh3 .eq. 0) then
           SHAPE = 0
           else
           SHAPE  = (sh1+sh2)/sh3
          endif
     END FUNCTION SHAPE

     REAL FUNCTION FLOW(n,So,Bw,h,z)
     REAL :: n,So, Bw, z, h
     REAL :: WP, AREA
          WP = Bw + 2*h*sqrt(1+h**2)             !-- wetted perimeter
          AREA = (Bw+z*h)*h                      !-- Flow area
          if (WP .le.0) then
#ifdef HYDRO_D
           print *, "Wetter perimeter is zero, will get divide by zero error"
           call hydro_stop() 
#endif
          else
           FLOW = (1/n)*sqrt(So)*(AREA**(5./3.)/(WP**(2./3.)))
          endif
     END FUNCTION FLOW

!-=======================================
     REAL FUNCTION AREAf(AREA,Bw,h,z)
     REAL :: AREA, Bw, z, h
       AREAf = (Bw+z*h)*h-AREA       !-- Flow area
     END FUNCTION AREAf

!-====critical depth function  ==========
     REAL FUNCTION CDf(Q,Bw,h,z)
     REAL :: Q, Bw, z, h
       if(h .le. 0) then
#ifdef HYDRO_D
         print *, "head is zero, will get division by zero error"
         call hydro_stop() 
#endif
       else
       CDf = (Q/((Bw+z*h)*h))/(sqrt(9.81*(((Bw+z*h)*h)/(Bw+2*z*h))))-1  !--critical depth function
       endif
     END FUNCTION CDf

!=======find flow depth in channel with bisection Chapra pg. 131
    REAL FUNCTION HEAD(index,AREA,Bw,z)  !-- find the water elevation given wetted area, 
                                         !--bottom widith and side channel.. index was for debuggin
     REAL :: Bw,z,AREA,test           
     REAL :: hl, hu, hr, hrold
     REAL :: fl, fr,error                !-- function evaluation
     INTEGER :: maxiter,index

     error = 1.0
     maxiter = 0
     hl = 0.00001   !-- minimum depth is small
     hu = 30.  !-- assume maximum depth is 30 meters

    if (AREA .lt. 0.00001) then 
     hr = 0.
    else
      do while ((AREAf(AREA,BW,hl,z)*AREAf(AREA,BW,hu,z)).gt.0 .and. maxiter .lt. 100) 
       !-- allows for larger , smaller heads 
       if(AREA .lt. 1.) then
        hl=hl/2
       else
        hu = hu * 2
       endif
       maxiter = maxiter + 1
        
      end do

      maxiter =0
      hr = 0
      fl = AREAf(AREA,Bw,hl,z)
      do while (error .gt. 0.0001 .and. maxiter < 1000)
        hrold = hr
        hr = (hl+hu)/2
        fr =  AREAf(AREA,Bw,hr,z)
        maxiter = maxiter + 1
         if (hr .ne. 0) then
          error = abs((hr - hrold)/hr)
         endif
        test = fl * fr
         if (test.lt.0) then
           hu = hr
         elseif (test.gt.0) then
           hl=hr
           fl = fr
         else
           error = 0.0
         endif
      end do
     endif
     HEAD = hr

22   format("i,hl,hu,Area",i5,2x,f12.8,2x,f6.3,2x,f6.3,2x,f6.3,2x,f9.1,2x,i5)

    END FUNCTION HEAD
!=================================
     REAL FUNCTION MANNING(h1,n,Bw,Cs)

     REAL :: Bw,h1,Cs,n
     REAL :: z, AREA,R,Kd

     z=1/Cs
     R = ((Bw+z*h1)*h1)/(Bw+2*h1*sqrt(1+z**2)) !-- Hyd Radius
     AREA = (Bw+z*h1)*h1        !-- Flow area
     Kd = (1/n)*(R**(2./3.))*AREA     !-- convenyance
#ifdef HYDRO_D
     print *,"head, kd",  h1,Kd
#endif
     MANNING = Kd
     
     END FUNCTION MANNING

!=======find flow depth in channel with bisection Chapra pg. 131
     REAL FUNCTION CRITICALDEPTH(lnk,Q,Bw,z)  !-- find the critical depth
     REAL :: Bw,z,Q,test
     REAL :: hl, hu, hr, hrold
     REAL :: fl, fr,error   !-- function evaluation
     INTEGER :: maxiter
     INTEGER :: lnk

     error = 1.0
     maxiter = 0
     hl = 1e-5   !-- minimum depth is 0.00001 meters
!    hu = 35.       !-- assume maximum  critical depth 25 m
     hu = 100.       !-- assume maximum  critical depth 25 m

     if(CDf(Q,BW,hl,z)*CDf(Q,BW,hu,z) .gt. 0) then
      if(Q .gt. 0.001) then
#ifdef HYDRO_D
        print *, "interval won't work to find CD of lnk ", lnk
        print *, "Q, hl, hu", Q, hl, hu
        print *, "cd lwr, upr", CDf(Q,BW,hl,z), CDf(Q,BW,hu,z)
        call hydro_stop() 
#endif
      else
        Q = 0.0
      endif
     endif

     hr = 0.
     fl = CDf(Q,Bw,hl,z)

     if (Q .eq. 0.) then
       hr = 0.
     else
      do while (error .gt. 0.0001 .and. maxiter < 1000)
        hrold = hr
        hr = (hl+hu)/2
        fr =  CDf(Q,Bw,hr,z)
        maxiter = maxiter + 1
         if (hr .ne. 0) then
          error = abs((hr - hrold)/hr)
         endif
        test = fl * fr
         if (test.lt.0) then
           hu = hr
         elseif (test.gt.0) then
           hl=hr
           fl = fr
         else
           error = 0.0
         endif

       end do
      endif

     CRITICALDEPTH = hr

     END FUNCTION CRITICALDEPTH
!================================================
     REAL FUNCTION SGNf(val)  !-- function to return the sign of a number
     REAL:: val

     if (val .lt. 0) then
       SGNf= -1.
     elseif (val.gt.0) then
       SGNf= 1.
     else
       SGNf= 0.
     endif

     END FUNCTION SGNf
!================================================

     REAL FUNCTION fnDX(qp,Tw,So,Ck,dx,dt) !-- find channel sub-length for MK method
     REAL    :: qp,Tw,So,Ck,dx, dt,test
     REAL    :: dxl, dxu, dxr, dxrold
     REAL    :: fl, fr, error
     REAL    :: X
     INTEGER :: maxiter

     error = 1.0
     maxiter =0
     dxl = dx*0.9  !-- how to choose dxl???
     dxu = dx
     dxr=0

     do while (fnDXCDT(qp,Tw,So,Ck,dxl,dt)*fnDXCDT(qp,Tw,So,Ck,dxu,dt) .gt. 0 &
               .and. dxl .gt. 10)  !-- don't let dxl get too small
      dxl = dxl/1.1
     end do
     
      
     fl = fnDXCDT(qp,Tw,So,Ck,dxl,dt)
     do while (error .gt. 0.0001 .and. maxiter < 1000)
        dxrold = dxr
        dxr = (dxl+dxu)/2
        fr =  fnDXCDT(qp,Tw,So,Ck,dxr,dt)
        maxiter = maxiter + 1
         if (dxr .ne. 0) then
          error = abs((dxr - dxrold)/dxr)
         endif
        test = fl * fr
         if (test.lt.0) then
           dxu = dxr
         elseif (test.gt.0) then
           dxl=dxr
           fl = fr
         else
           error = 0.0
         endif
      end do
     FnDX = dxr

    END FUNCTION fnDX
!================================================
     REAL FUNCTION fnDXCDT(qp,Tw,So,Ck,dx,dt) !-- function to help find sub-length for MK method
     REAL    :: qp,Tw,So,Ck,dx,dt,X
     REAL    :: c,b  !-- coefficients on dx/cdt log approximation function
     
     c = 0.2407
     b = 1.16065
     X = 0.5-(qp/(2*Tw*So*Ck*dx))
     if (X .le.0) then 
      fnDXCDT = -1 !0.115
     else
      fnDXCDT = (dx/(Ck*dt)) - (c*LOG(X)+b)  !-- this function needs to converge to 0
     endif
     END FUNCTION fnDXCDT
! ----------------------------------------------------------------------

    subroutine check_channel(unit,cd,nlinks_index,mpp_nlinks,nlinks)
         integer :: unit,mpp_nlinks,nlinks,nlinks_index(nlinks),i
         real cd(nlinks)
#ifdef MPP_LAND
         call write_chanel_real(cd,nlinks_index,mpp_nlinks,nlinks)
         if(my_id .eq. IO_id) then
           write(unit,*) cd
         endif
#endif
         return
    end subroutine check_channel
END MODULE module_channel_routing
