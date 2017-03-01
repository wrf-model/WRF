!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

MODULE module_channel_routing
#ifdef MPP_LAND
  USE module_mpp_land
  use MODULE_mpp_ReachLS, only : updatelinkv,                   & 
                                 ReachLS_write_io, gbcastvalue, &
                                 gbcastreal2

#endif
  IMPLICIT NONE

  contains

! ------------------------------------------------
!   FUNCTION MUSKING
! ------------------------------------------------
	REAL FUNCTION MUSKING(idx,qup,quc,qdp,dt,Km,X)

	IMPLICIT NONE

!--local variables
        REAL    :: C1, C2, C3
        REAL    :: Km         !K travel time in hrs in reach
        REAL    :: X          !weighting factors 0<=X<=0.5 
        REAL    :: dt         !routing period in hrs
        REAL    :: avgbf      !average base flow for initial condition
        REAL    :: qup        !inflow from previous timestep
        REAL    :: quc        !inflow  of current timestep
        REAL    :: qdp        !outflow of previous timestep
        REAL    :: dth        !timestep in hours
        INTEGER :: idx       ! index

        dth = dt/3600    !hours in timestep
        C1 = (dth - 2*Km*X)/(2*Km*(1-X)+dth)
        C2 = (dth+2*Km*X)/(2*Km*(1-X)+dth)
        C3 = (2*Km*(1-X)-dth)/(2*Km*(1-X)+dth)
        MUSKING = (C1*quc)+(C2*qup)+(C3*qdp)

! ----------------------------------------------------------------
  END FUNCTION MUSKING
! ----------------------------------------------------------------

! ------------------------------------------------
!   SUBROUTINE LEVELPOOL
! ------------------------------------------------

SUBROUTINE LEVELPOOL(ln,qi0,qi1,qo1,ql,dt,H,ar,we,maxh,wc,wl,oe,oc,oa)

    !! ----------------------------  argument variables
    !! All elevations should be relative to a common base (often belev(k))

    real, intent(INOUT) :: H       ! water elevation height (m)
    real, intent(IN)    :: dt      ! routing period [s]
    real, intent(IN)    :: qi0     ! inflow at previous timestep (cms)
    real, intent(IN)    :: qi1     ! inflow at current timestep (cms)
    real, intent(OUT)   :: qo1     ! outflow at current timestep
    real, intent(IN)    :: ql      ! lateral inflow
    real, intent(IN)    :: ar      ! area of reservoir (km^2)
    real, intent(IN)    :: we      ! bottom of weir elevation
    real, intent(IN)    :: wc      ! weir coeff.
    real, intent(IN)    :: wl      ! weir length (m)
    real, intent(IN)    :: oe      ! orifice elevation
    real, intent(IN)    :: oc      ! orifice coeff.
    real, intent(IN)    :: oa      ! orifice area (m^2)
    real, intent(IN)    :: maxh    ! max depth of reservoir before overtop (m)                     
    integer, intent(IN) :: ln      ! lake number

    !!DJG Add lake option switch here...move up to namelist in future versions...
    integer :: LAKE_OPT            ! Lake model option (move to namelist later)
    real    :: Htmp                ! Temporary assign of incoming lake el. (m)

    !! ----------------------------  local variables
    real :: sap                    ! local surface area values
    real :: discharge              ! storage discharge m^3/s
    real :: tmp1, tmp2
    real :: dh, dh1, dh2, dh3      ! height function and 3 order RK
    real :: It, Itdt_3, Itdt_2_3
    real :: maxWeirDepth           !maximum capacity of weir
    !! ----------------------------  subroutine body: from chow, mad mays. pg. 252
    !! -- determine from inflow hydrograph


    !!DJG Set hardwire for LAKE_OPT...move specification of this to namelist in
    !future versions...
    LAKE_OPT = 2
    Htmp = H   !temporary set of incoming lake water elevation...
    
    
    !!DJG IF-block for lake model option  1 - outflow=inflow, 2 - Chow et al level
    !pool, .....
    if (LAKE_OPT.eq.1) then     ! If-block for simple pass through scheme....
       
       qo1 = qi1                 ! Set outflow equal to inflow at current time      
       H = Htmp                  ! Set new lake water elevation to incoming lake el.
       
    else if (LAKE_OPT.eq.2) then   ! If-block for Chow et al level pool scheme
 
    It = qi0
    Itdt_3   = (qi0 + (qi1 + ql))/3
    Itdt_2_3 = (qi0 + (qi1 + ql))/3 + Itdt_3
    maxWeirDepth =  maxh - we   

    !-- determine Q(dh) from elevation-discharge relationship
    !-- and dh1
    dh = H - we
    if (dh .gt. maxWeirDepth) then 
       dh = maxWeirDepth 
    endif

    if (dh .gt. 0.0 ) then              !! orifice and overtop discharge
        tmp1 = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
        tmp2 = wc * wl * (dh ** 2./3.)
        discharge = tmp1 + tmp2

        if (H .gt. 0.0) then
          sap = (ar * 1.0E6 ) * (1 + (H - we) / H)
        else
          sap  = 0.0
        endif

    else if ( H .gt. oe ) then     !! only orifice flow,not full
        discharge = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
        sap = ar * 1.0E6
    else
        discharge = 0.0
        sap = ar * 1.0E6
    endif

    if (sap .gt. 0) then 
      dh1 = ((It - discharge)/sap)*dt
    else
      dh1 = 0.0
    endif

    !-- determine Q(H + dh1/3) from elevation-discharge relationship
    !-- dh2
    dh = (H+dh1/3) - we
    if (dh .gt. maxWeirDepth) then 
       dh = maxWeirDepth 
    endif

    if (dh .gt. 0.0 ) then              !! orifice and overtop discharge
        tmp1 = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
        tmp2 = wc * wl * (dh ** 2./3.) 
        discharge = tmp1 + tmp2

        if (H .gt. 0.0) then 
         sap = (ar * 1.0E6 ) * (1 + (H - we) / H)
        else
         sap  = 0.0
        endif

    else if ( H .gt. oe ) then     !! only orifice flow,not full
        discharge = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
        sap = ar * 1.0E6
    else
        discharge = 0.0
        sap = ar * 1.0E6
    endif

    if (sap .gt. 0.0) then 
     dh2 = ((Itdt_3 - discharge)/sap)*dt
    else
     dh2 = 0.0
    endif

    !-- determine Q(H + 2/3 dh2) from elevation-discharge relationship
    !-- dh3
    dh = (H + (0.667*dh2)) - we
    if (dh .gt. maxWeirDepth) then 
       dh = maxWeirDepth 
    endif

    if (dh .gt. 0.0 ) then              !! orifice and overtop discharge
        tmp1 = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
        tmp2 = wc * wl * (dh ** 2./3.) 
        discharge = tmp1 + tmp2

        if (H .gt. 0.0) then
         sap = (ar * 1.0E6 ) * (1 + (H - we) / H)
        else
         sap = 0.0
        endif 

    else if ( H .gt. oe ) then     !! only orifice flow,not full
        discharge = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
        sap = ar * 1.0E6
    else
        discharge = 0.0
        sap = ar * 1.0E6
    endif
    
    if (sap .gt. 0.0) then 
      dh3 = ((Itdt_2_3 - discharge)/sap)*dt
    else
      dh3 = 0.0
    endif

    !-- determine dh and H
    dh = (dh1/4.) + (0.75*dh3)
    H = H + dh

    !-- compute final discharge
    dh = H - we
    if (dh .gt. maxWeirDepth) then 
       dh = maxWeirDepth 
    endif
    if (dh .gt. 0.0 ) then              !! orifice and overtop discharge
        tmp1 = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
        tmp2 = wc * wl * (dh ** 2./3.)
        discharge = tmp1 + tmp2

        if (H .gt. 0.0) then 
         sap = (ar * 1.0E6 ) * (1 + (H - we) / H)
        else
         sap = 0.0
        endif

    else if ( H .gt. oe ) then     !! only orifice flow,not full
        discharge = oc * oa * sqrt(2 * 9.81 * ( H - oe ) )
        sap = ar * 1.0E6
    else
        discharge = 0.0
        sap = ar * 1.0E6
    endif

    if(H .ge. maxh) then  ! overtop condition
     discharge = qi1
     H = maxh
    endif

    qo1  = discharge  ! return the flow rate from reservoir

23 format('botof H dh orf wr Q',f8.4,2x,f8.4,2x,f8.3,2x,f8.3,2x,f8.2)
24 format('ofonl H dh sap Q ',f8.4,2x,f8.4,2x,f8.0,2x,f8.2)

 
     ELSE   ! ELSE for LAKE_OPT....
     ENDIF  ! ENDIF for LAKE_OPT....
 
  RETURN

! ----------------------------------------------------------------
  END SUBROUTINE LEVELPOOL
! ----------------------------------------------------------------


! ------------------------------------------------
!   FUNCTION Diffusive wave
! ------------------------------------------------
        REAL FUNCTION DIFFUSION(nod,z1,z20,h1,h2,dx,n, &
                                Bw, Cs)
        IMPLICIT NONE
!-- channel geometry and characteristics
        REAL    :: Bw         !-bottom width (meters)
        REAL    :: Cs         !-Channel side slope slope
        REAL    :: dx         !-channel lngth (m)
        REAL,intent(in)    :: n          !-mannings coefficient
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
        REAL ::  z20, dzx

! added by Wei Yu for bad data.

        dzx = (z1 - z20)/dx
        if(dzx .lt. 0.002) then
           z2 = z1 - dx*0.002  
        else
           z2 = z20
        endif
!end 

        if (n.le.0.0.or.Cs.le.0.or.Bw.le.0) then
         print *, "Error in Diffusion function ->channel coefficients"
         print *, "nod, n, Cs, Bw", nod, n, Cs, Bw 
         call hydro_stop("In DIFFUSION() - Error channel coefficients.")
        endif

!        Sf = ((z1+h1)-(z2+h2))/dx  !-- compute the friction slope
       !if(z1 .eq. z2) then
       ! Sf = ((z1-(z2-0.01))+(h1-h2))/dx  !-- compute the friction slope
       !else
!         Sf = ((z1-z2)+(h1-h2))/dx  !-- compute the friction slope
       !endif

!modifieed by Wei Yu for false geography data
         if(abs(z1-z2) .gt. 1.0E5) then
#ifdef HYDRO_D
             print*, "WARNING: huge slope rest to 0 for channel grid.", z1,z2
#endif
             Sf = ((h1-h2))/dx  !-- compute the friction slope
         else
             Sf = ((z1-z2)+(h1-h2))/dx  !-- compute the friction slope
         endif
!end  modfication

        sgn = SGNf(Sf)             !-- establish sign

        w = 0.5*(sgn + 1.)         !-- compute upstream or downstream weighting
        
        z = 1/Cs                   !--channel side distance (m)
        R = ((Bw+z*h1)*h1)/(Bw+2*h1*sqrt(1+z*z)) !-- Hyd Radius
        AREA = (Bw+z*h1)*h1        !-- Flow area
        Ku = (1/n)*(R**(2./3.))*AREA     !-- convenyance

        R = ((Bw+z*h2)*h2)/(Bw+2*h2*sqrt(1+z*z)) !-- Hyd Radius
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
        REAL FUNCTION MUSKINGCUNGE(idx,qup, quc, qdp, ql,&
                                   dt,So,dx,n,Cs,Bw)
        IMPLICIT NONE

!--local variables
        REAL    :: C1, C2, C3, C4
        REAL    :: Km          !K travel time in hrs in reach
        REAL    :: X          !weighting factors 0<=X<=0.5
        REAL    :: dt         !routing period in  seconds
        REAL    :: qup        !flow upstream previous timestep
        REAL    :: quc        !flow upstream current timestep
        REAL    :: qdp        !flow downstream previous timestep
!       REAL    :: qdc        !flow downstream current timestep
        REAL    :: ql         !lateral inflow through reach (m^3/sec)
        REAL    :: Ck         ! wave celerity (m/s)

!-- channel geometry and characteristics
        REAL    :: Bw         ! bottom width (meters)
        REAL    :: Cs         ! Channel side slope slope
        REAL    :: So         ! Channel bottom slope %
        REAL    :: dx         ! channel lngth (m)
        REAL    :: n          ! mannings coefficient
        REAL    :: Tw         ! top width at peak flow
        REAL    :: AREA       ! Cross sectional area m^2
        REAL    :: Z          ! trapezoid distance (m)
        REAL    :: R          ! Hydraulic radius
        REAL    :: WP         ! wetted perimmeter
        REAL    :: h          ! depth of flow
        REAL    :: h_0,h_1    ! secant method estimates
        REAL    :: Qj_0       ! secant method estimates
        REAL    :: Qj         ! intermediate flow estimate
        REAL    :: D,D1       ! diffusion coeff
        REAL    :: dtr        ! required timestep, minutes
        REAL    :: error
        REAL    :: hp         !courant, previous height
        INTEGER :: maxiter    !maximum number of iterations


!-- local variables.. needed if channel is sub-divded
        REAL    :: a,b,c
        INTEGER :: i,idx     !-- channel segment counter

!yw add
        goto 101
        C1   = 0
        C2   = 0
        C3   = 0
        C4   = 0
        Km   = 0
        X    = 0        !weighting factors 0<=X<=0.5
        Ck   = 0
        Tw   = 0       ! top width at peak flow
        AREA = 0      ! Cross sectional area m^2
        Z    = 0         ! trapezoid distance (m)
        R    = 0         ! Hydraulic radius
        WP   = 0         ! wetted perimmeter
         h   = 0        ! depth of flow
        h_0  = 0
        h_1  = 0       ! secant method estimates
        Qj_0 = 0      ! secant method estimates
        D    = 0
        D1   = 0        ! diffusion coeff
        dtr   = 0       ! required timestep, minutes
        error = 1.0
        hp    = 0        !courant, previous height
        maxiter = 0
        a = 0
101     continue
!end yw

        c = 0.52    !-- coefficnets for finding dx/Ckdt
        b = 1.15
        
        if(Cs .eq.0) then 
         z = 1.0 
        else 
         z = 1/Cs              !channel side distance (m)
        endif

        !qC = quc + ql !current upstream in reach

        if (n .le.0 .or. So .le. 0 .or. z .le. 0 .or. Bw .le. 0) then
          print*, "Error in channel coefficients -> Muskingum cunge",n,So,z,Bw
          call hydro_stop("In MUSKINGCUNGE() - Error in channel coefficients")
        end if

        error   = 1.0
        maxiter = 0
        a = 0.0

        if ((quc+ql) .lt. 100) then 
          b=5 
        else 
         b= 20
        endif

!-------------  Secant Method
        h    =  (a+b)/2  !- upper interval
        h_0  = 0.0       !- lower interval
        Qj_0 = 0.0       !- initial flow of lower interval

        do while ((error .gt. 0.05 .and. maxiter .le. 100 .and. h .gt. 0.01))  

          !----- lower interval  --------------------
           Tw = Bw + 2*z*h_0                    !--top width of the channel inflow
           Ck = (sqrt(So)/n)*(5./3.)*(h_0**0.667)   !-- pg 287 Chow, Mdt, Mays
           if(Ck .gt. 0.0) then
             Km = dx/Ck                       !-- seconds Muskingum Param
             if(Km .lt. dt) then           
               Km = dt
             endif
           else 
             Km = dt
           endif
 
           if(Tw*So*Ck*dx .eq. 0.0) then 
             X = 0.25
           else
             X = 0.5-(Qj_0/(2*Tw*So*Ck*dx))
           endif
  
           if(X .le. 0.0) then 
             X = 0.25
           elseif(X .gt. 0.35) then
             X = 0.35
           endif
  
           D = (Km*(1 - X) + dt/2)              !--seconds
            if(D .eq. 0.0) then 
              print *, "FATAL ERROR: D is 0 in MUSKINGCUNGE", Km, X, dt,D
              call hydro_stop("In MUSKINGCUNGE() - D is 0.")
           endif 
  
           C1 =  (Km*X + dt/2)/D
           C2 =  (dt/2 - Km*X)/D
           C3 =  (Km*(1-X)-dt/2)/D
           C4 =  (ql*dt)/D                       !-- ql already multipled by the dx length

           if(h_0 .le. 0.0) then 
             AREA= 0.0
             WP = 0.0
           else
            AREA = (Bw * h_0 + z * (h_0*h_0) )
            WP   = (Bw * h_0 + z * (h_0*h_0)) / (Bw + 2 * h_0 * sqrt(1+z*z))
           endif
           
           if(WP .le. 0.0) then 
              Qj_0 =  ((C1*qup)+(C2*quc)+(C3*qdp) + C4)
           else 
              Qj_0 =  ((C1*qup)+(C2*quc)+(C3*qdp) + C4) - ((1/n) * AREA * (WP**(2./3.)) * sqrt(So)) !f(x)
           endif
           
           !--upper interval -----------
           Tw = Bw + 2*z*h                    !--top width of the channel inflow
           Ck = (sqrt(So)/n)*(5./3.)*(h**0.667)   !-- pg 287 Chow, Mdt, Mays
           if(Ck .gt. 0.0) then
             Km = dx/Ck                       !-- seconds Muskingum Param
             if(Km .lt. dt) then           
               Km = dt
             endif
           else 
             Km = dt
           endif
 
           if(Tw*So*Ck*dx .eq. 0.0) then 
             X = 0.25
           else
             X = 0.5-(((C1*qup)+(C2*quc)+(C3*qdp) + C4)/(2*Tw*So*Ck*dx))
           endif
  
           if(X .le. 0.0) then 
             X = 0.25
           elseif(X .gt. 0.35) then
             X = 0.35
           endif
  
           D = (Km*(1 - X) + dt/2)              !--seconds
            if(D .eq. 0.0) then 
              print *, "FATAL ERROR: D is 0 in MUSKINGCUNGE", Km, X, dt,D
              call hydro_stop("In MUSKINGCUNGE() - D is 0.")
           endif 
  
           C1 =  (Km*X + dt/2)/D
           C2 =  (dt/2 - Km*X)/D
           C3 =  (Km*(1-X)-dt/2)/D
           C4 =  (ql*dt)/D                       !-- ql already multipled by the dx length

           if(h .le. 0) then 
            AREA = 0.0
            WP   = 0.0
           else
            AREA = (Bw * h + z * (h*h) )
            WP   = (Bw * h + z * (h*h)) / (Bw + 2 * h * sqrt(1+z*z))
           endif 
           
           if(WP .le. 0.0) then 
             Qj =  ((C1*qup)+(C2*quc)+(C3*qdp) + C4)
           else
             Qj =  ((C1*qup)+(C2*quc)+(C3*qdp) + C4) -((1/n) * AREA * (WP**(2./3.)) * sqrt(So))
           endif

           if(Qj_0-Qj .ne. 0.0) then
             h_1 = h - ((Qj * (h_0 - h))/(Qj_0 - Qj)) !update h, 3rd estimate
              if(h_1 .lt. 0.0) then
                h_1 = h
              endif
           else
             h_1 = h
           endif

           error = abs((h_1 - h)/h) !error is new estatimate and 2nd estimate

!           if(idx .eq. 626) then 
!             write(6,*) h_0,h,h_1,error
!           endif

           h_0  = h 
           h    = h_1
           maxiter = maxiter + 1

      end do

      if((maxiter .ge. 100 .and. error .gt. 0.05) .or. h .gt. 100) then 

         print*, "WARNING:"
         print*, "id,err,iters,h", idx, error, maxiter, h
         print*, "n,z,B,So,dx,X,dt,Km",n,z,Bw,So,dx,X,dt,Km
         print*, "qup,quc,qdp,ql", qup,quc,qdp,ql
         if(h.gt.100) then
              print*, "FATAL ERROR: Water Elevation Calculation is Diverging"
              call hydro_stop("In MUSKINGCUNGE() - Water Elevation Calculation is Diverging")
         endif
      endif

!      if(idx .eq. 626) then 
!         write(6,*) ((C1*qup)+(C2*quc)+(C3*qdp) + C4) !-- pg 295 Bedient huber
!      endif

!     MUSKINGCUNGE =  h 
!yw      MUSKINGCUNGE =  ((C1*qup)+(C2*quc)+(C3*qdp) + C4) !-- pg 295 Bedient huber

!yw added for test

      if(((C1*qup)+(C2*quc)+(C3*qdp) + C4) .lt. 0.0) then
         MUSKINGCUNGE =  MAX( ( (C1*qup)+(C2*quc) + C4),((C1*qup)+(C3*qdp) + C4) )
      else
          MUSKINGCUNGE =  ((C1*qup)+(C2*quc)+(C3*qdp) + C4) !-- pg 295 Bedient huber
      endif


! ----------------------------------------------------------------

  END FUNCTION MUSKINGCUNGE


  SUBROUTINE SUBMUSKINGCUNGE(qdc,vel,idx,qup,quc,qdp,ql,dt,So,dx,n,Cs,Bw)

        IMPLICIT NONE

        REAL, intent(IN)       :: dt         !routing period in  seconds
        REAL, intent(IN)       :: qup        !flow upstream previous timestep
        REAL, intent(IN)       :: quc        !flow upstream current timestep
        REAL, intent(IN)       :: qdp        !flow downstream previous timestep
        REAL, intent(INOUT)    :: qdc        !flow downstream current timestep
        REAL, intent(IN)       :: ql         !lateral inflow through reach (m^3/sec)
        REAL, intent(IN)       :: Bw         ! bottom width (meters)
        REAL, intent(IN)       :: Cs         ! Channel side slope slope
        REAL, intent(IN)       :: So         ! Channel bottom slope %
        REAL, intent(IN)       :: dx         ! channel lngth (m)
        REAL, intent(IN)       :: n          ! mannings coefficient
        REAL, intent(INOUT)    :: vel        ! mannings coefficient
        INTEGER, intent(IN)    :: idx        ! channel id

!--local variables
        REAL    :: C1, C2, C3, C4
        REAL    :: Km          !K travel time in hrs in reach
        REAL    :: X           !weighting factors 0<=X<=0.5
        REAL    :: Ck          ! wave celerity (m/s)

!-- channel geometry and characteristics
        REAL    :: Tw         ! top width at peak flow
        REAL    :: AREA       ! Cross sectional area m^2
        REAL    :: Z          ! trapezoid distance (m)
        REAL    :: R          ! Hydraulic radius
        REAL    :: WP         ! wetted perimmeter
        REAL    :: h          ! depth of flow
        REAL    :: h_0,h_1    ! secant method estimates
        REAL    :: Qj_0       ! secant method estimates
        REAL    :: Qj         ! intermediate flow estimate
        REAL    :: D,D1       ! diffusion coeff
        REAL    :: dtr        ! required timestep, minutes
        REAL    :: error
        REAL    :: hp         !courant, previous height
        INTEGER :: maxiter    !maximum number of iterations

!-- local variables.. needed if channel is sub-divded
        REAL    :: a,b,c
        INTEGER :: i          !-- channel segment counter

!yw add
        goto 101
        C1   = 0
        C2   = 0
        C3   = 0
        C4   = 0
        Km   = 0
        X    = 0        !weighting factors 0<=X<=0.5
        Ck   = 0
        Tw   = 0       ! top width at peak flow
        AREA = 0      ! Cross sectional area m^2
        Z    = 0         ! trapezoid distance (m)
        R    = 0         ! Hydraulic radius
        WP   = 0         ! wetted perimmeter
         h   = 0        ! depth of flow
        h_0  = 0
        h_1  = 0       ! secant method estimates
        Qj_0 = 0      ! secant method estimates
        D    = 0
        D1   = 0        ! diffusion coeff
        dtr   = 0       ! required timestep, minutes
        error = 1.0
        hp    = 0        !courant, previous height
        maxiter = 0
        a = 0
101     continue
!end yw

        c = 0.52    !-- coefficnets for finding dx/Ckdt
        b = 1.15
        
        if(Cs .eq.0) then 
         z = 1.0 
        else 
         z = 1/Cs              !channel side distance (m)
        endif

        !qC = quc + ql !current upstream in reach

        if (n .le.0 .or. So .le. 0 .or. z .le. 0 .or. Bw .le. 0) then
          print*, "Error in channel coefficients -> Muskingum cunge",n,So,z,Bw
          call hydro_stop("In MUSKINGCUNGE() - Error in channel coefficients")
        end if

        error   = 1.0
        maxiter = 0
        a = 0.0

        if ((quc+ql) .lt. 100) then 
          b=5 
        else 
         b= 20
        endif

!-------------  Secant Method
        h    =  (a+b)/2  !- upper interval
        h_0  = 0.0       !- lower interval
        Qj_0 = 0.0       !- initial flow of lower interval

        do while ((error .gt. 0.05 .and. maxiter .le. 100 .and. h .gt. 0.01))  

          !----- lower interval  --------------------
           Tw = Bw + 2*z*h_0                    !--top width of the channel inflow
           Ck = (sqrt(So)/n)*(5./3.)*(h_0**0.667)   !-- pg 287 Chow, Mdt, Mays
           if(Ck .gt. 0.0) then
             Km = dx/Ck                       !-- seconds Muskingum Param
             if(Km .lt. dt) then           
               Km = dt
             endif
           else 
             Km = dt
           endif
 
           if(Tw*So*Ck*dx .eq. 0.0) then 
             X = 0.25
           else
             X = 0.5-(Qj_0/(2*Tw*So*Ck*dx))
           endif
  
           if(X .le. 0.0) then 
             X = 0.25
           elseif(X .gt. 0.35) then
             X = 0.35
           endif
  
           D = (Km*(1 - X) + dt/2)              !--seconds
            if(D .eq. 0.0) then 
              print *, "FATAL ERROR: D is 0 in MUSKINGCUNGE", Km, X, dt,D
              call hydro_stop("In MUSKINGCUNGE() - D is 0.")
           endif 
  
           C1 =  (Km*X + dt/2)/D
           C2 =  (dt/2 - Km*X)/D
           C3 =  (Km*(1-X)-dt/2)/D
           C4 =  (ql*dt)/D                       !-- ql already multipled by the dx length

           if(h_0 .le. 0.0) then 
             AREA= 0.0
             WP = 0.0
           else
            AREA = (Bw * h_0 + z * (h_0*h_0) )
            WP   = (Bw * h_0 + z * (h_0*h_0)) / (Bw + 2 * h_0 * sqrt(1+z*z))
           endif
           
           if(WP .le. 0.0) then 
              Qj_0 =  ((C1*qup)+(C2*quc)+(C3*qdp) + C4)
           else 
              Qj_0 =  ((C1*qup)+(C2*quc)+(C3*qdp) + C4) - ((1/n) * AREA * (WP**(2./3.)) * sqrt(So)) !f(x)
           endif
           
           !--upper interval -----------
           Tw = Bw + 2*z*h                    !--top width of the channel inflow
           Ck = (sqrt(So)/n)*(5./3.)*(h**0.667)   !-- pg 287 Chow, Mdt, Mays
           if(Ck .gt. 0.0) then
             Km = dx/Ck                       !-- seconds Muskingum Param
             if(Km .lt. dt) then           
               Km = dt
             endif
           else 
             Km = dt
           endif
 
           if(Tw*So*Ck*dx .eq. 0.0) then 
             X = 0.25
           else
             X = 0.5-(((C1*qup)+(C2*quc)+(C3*qdp) + C4)/(2*Tw*So*Ck*dx))
           endif
  
           if(X .le. 0.0) then 
             X = 0.25
           elseif(X .gt. 0.35) then
             X = 0.35
           endif
  
           D = (Km*(1 - X) + dt/2)              !--seconds
            if(D .eq. 0.0) then 
              print *, "FATAL ERROR: D is 0 in MUSKINGCUNGE", Km, X, dt,D
              call hydro_stop("In MUSKINGCUNGE() - D is 0.")
           endif 
  
           C1 =  (Km*X + dt/2)/D
           C2 =  (dt/2 - Km*X)/D
           C3 =  (Km*(1-X)-dt/2)/D
           C4 =  (ql*dt)/D                       !-- ql already multipled by the dx length

           if(h .le. 0) then 
            AREA = 0.0
            WP   = 0.0
           else
            AREA = (Bw * h + z * (h*h) )
            WP   = (Bw * h + z * (h*h)) / (Bw + 2 * h * sqrt(1+z*z))
           endif 
           
           if(WP .le. 0.0) then 
             Qj =  ((C1*qup)+(C2*quc)+(C3*qdp) + C4)
           else
             Qj =  ((C1*qup)+(C2*quc)+(C3*qdp) + C4) -((1/n) * AREA * (WP**(2./3.)) * sqrt(So))
           endif

           if(Qj_0-Qj .ne. 0.0) then
             h_1 = h - ((Qj * (h_0 - h))/(Qj_0 - Qj)) !update h, 3rd estimate
              if(h_1 .lt. 0.0) then
                h_1 = h
              endif
           else
             h_1 = h
           endif

           error = abs((h_1 - h)/h) !error is new estatimate and 2nd estimate

!           if(idx .eq. 626) then 
!             write(6,*) h_0,h,h_1,error
!           endif

           h_0  = h 
           h    = h_1
           maxiter = maxiter + 1

      end do

      if((maxiter .ge. 100 .and. error .gt. 0.05) .or. h .gt. 100) then 

         print*, "WARNING:"
         print*, "id,err,iters,h", idx, error, maxiter, h
         print*, "n,z,B,So,dx,X,dt,Km",n,z,Bw,So,dx,X,dt,Km
         print*, "qup,quc,qdp,ql", qup,quc,qdp,ql
         if(h.gt.100) then
              print*, "FATAL ERROR: Water Elevation Calculation is Diverging"
              call hydro_stop("In MUSKINGCUNGE() - Water Elevation Calculation is Diverging")
         endif
      endif

!yw added for test
      if(((C1*qup)+(C2*quc)+(C3*qdp) + C4) .lt. 0.0) then
!       MUSKINGCUNGE =  MAX( ( (C1*qup)+(C2*quc) + C4),((C1*qup)+(C3*qdp) + C4) )
        qdc = MAX( ( (C1*qup)+(C2*quc) + C4),((C1*qup)+(C3*qdp) + C4) )

      else
!       MUSKINGCUNGE =  ((C1*qup)+(C2*quc)+(C3*qdp) + C4) !-- pg 295 Bedient huber
        qdc =  ((C1*qup)+(C2*quc)+(C3*qdp) + C4) !-- pg 295 Bedient huber

      endif

      Tw = Bw + (2*z*h)
      R = (h*(Bw + Tw) / 2) / (Bw + 2*(((Tw - Bw) / 2)**2 + h**2)**0.5)    
      vel =  (1./n) * (R **(2./3.)) * sqrt(So)  ! average velocity in m/s

! ----------------------------------------------------------------
!END FUNCTION MUSKINGCUNGE
END SUBROUTINE SUBMUSKINGCUNGE
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
! ------------------------------------------------
     Subroutine drive_CHANNEL(latval,lonval,KT, IXRT,JXRT, SUBRTSWCRT, &
       QSUBRT, LAKEINFLORT, QSTRMVOLRT, TO_NODE, FROM_NODE, &
       TYPEL, ORDER, MAXORDER, NLINKS, CH_NETLNK, CH_NETRT, CH_LNKRT, &
       LAKE_MSKRT, DT, DTCT, DTRT_CH,MUSK, MUSX, QLINK, &
       HLINK, ELRT, CHANLEN, MannN, So, ChSSlp, Bw, &
       RESHT, HRZAREA, LAKEMAXH, WEIRH, WEIRC, WEIRL, ORIFICEC, ORIFICEA, &
       ORIFICEE, ZELEV, CVOL, NLAKES, QLAKEI, QLAKEO, LAKENODE, &
       dist, QINFLOWBASE, CHANXI, CHANYJ, channel_option, RETDEP_CHAN, &
       NLINKSL, LINKID, node_area  &
#ifdef MPP_LAND 
       , lake_index,link_location,mpp_nlinks,nlinks_index,yw_mpp_nlinks  &
       , LNLINKSL, LLINKID  &
       , gtoNode,toNodeInd,nToNodeInd &
#endif
       , CH_LNKRT_SL &
       ,gwBaseSwCRT, gwHead, qgw_chanrt, gwChanCondSw, gwChanCondConstIn, &
       gwChanCondConstOut)


       IMPLICIT NONE

! -------- DECLARATIONS ------------------------

        INTEGER, INTENT(IN) :: IXRT,JXRT,channel_option
        INTEGER, INTENT(IN) :: NLINKS,NLAKES, NLINKSL
        integer, INTENT(INOUT) :: KT   ! flag of cold start (1) or continue run.
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)    :: QSUBRT
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)    :: QSTRMVOLRT
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)    :: LAKEINFLORT
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)    :: ELRT
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)    :: QINFLOWBASE
        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT) :: CH_NETLNK

        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT) :: CH_NETRT
        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT) :: CH_LNKRT
        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT) :: CH_LNKRT_SL

       real , dimension(ixrt,jxrt):: latval,lonval

        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT) :: LAKE_MSKRT
        INTEGER, INTENT(IN), DIMENSION(NLINKS)    :: ORDER, TYPEL !--link
        INTEGER, INTENT(IN), DIMENSION(NLINKS)    :: TO_NODE, FROM_NODE
        INTEGER, INTENT(IN), DIMENSION(NLINKS)    :: CHANXI, CHANYJ
        REAL,    INTENT(IN), DIMENSION(NLINKS)    :: ZELEV  !--elevation of nodes
        REAL, INTENT(INOUT), DIMENSION(NLINKS)    :: CVOL
        REAL, INTENT(IN), DIMENSION(NLINKS)       :: MUSK, MUSX
        REAL, INTENT(IN), DIMENSION(NLINKS)       :: CHANLEN
        REAL, INTENT(IN), DIMENSION(NLINKS)       :: So, MannN
        REAL, INTENT(IN), DIMENSION(NLINKS)       :: ChSSlp,Bw  !--properties of nodes or links
        REAL                                      :: Km, X
        REAL , INTENT(INOUT), DIMENSION(:,:) :: QLINK
        REAL ,  DIMENSION(NLINKS,2) :: tmpQLINK
        REAL , INTENT(INOUT), DIMENSION(NLINKS)   :: HLINK
        REAL, INTENT(IN)                          :: DT    !-- model timestep
        REAL, INTENT(IN)                          :: DTRT_CH  !-- routing timestep
        REAL, INTENT(INOUT)                       :: DTCT
        real                                      :: minDTCT !BF minimum routing timestep
        REAL                                      :: dist(ixrt,jxrt,9)
        REAL                                      :: RETDEP_CHAN
        INTEGER, INTENT(IN)                       :: MAXORDER, SUBRTSWCRT, &
                                                     gwBaseSwCRT, gwChanCondSw
        real, intent(in)                          :: gwChanCondConstIn, gwChanCondConstOut ! aquifer-channel conductivity constant from namelist                                             
        REAL , INTENT(IN), DIMENSION(NLINKS)   :: node_area

!DJG GW-chan coupling variables...
        REAL, DIMENSION(NLINKS)                   :: dzGwChanHead
        REAL, DIMENSION(NLINKS)                   :: Q_GW_CHAN_FLUX     !DJG !!! Change 'INTENT' to 'OUT' when ready to update groundwater state...
        REAL, DIMENSION(IXRT,JXRT)                :: ZWATTBLRT          !DJG !!! Match with subsfce/gw routing & Change 'INTENT' to 'INOUT' when ready to update groundwater state...
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: gwHead            !DJG !!! groundwater head from Fersch-2d gw implementation...units (m ASL)
        REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: qgw_chanrt         !DJG !!! Channel-gw flux as used in Fersch 2d gw implementation...units (m^3/s)...Change 'INTENT' to 'OUT' when ready to update groundwater state...
         


        !-- lake params
        REAL, INTENT(IN), DIMENSION(NLAKES)       :: HRZAREA  !-- horizontal area (km^2)
        REAL, INTENT(IN), DIMENSION(NLAKES)       :: LAKEMAXH !-- maximum lake depth  (m^2)
        REAL, INTENT(IN), DIMENSION(NLAKES)       :: WEIRH    !-- lake depth  (m^2)
        REAL, INTENT(IN), DIMENSION(NLAKES)       :: WEIRC    !-- weir coefficient
        REAL, INTENT(IN), DIMENSION(NLAKES)       :: WEIRL    !-- weir length (m)
        REAL, INTENT(IN), DIMENSION(NLAKES)       :: ORIFICEC !-- orrifice coefficient
        REAL, INTENT(IN), DIMENSION(NLAKES)       :: ORIFICEA !-- orrifice area (m^2)
        REAL, INTENT(IN), DIMENSION(NLAKES)       :: ORIFICEE !-- orrifce elevation (m)

        REAL, INTENT(INOUT), DIMENSION(NLAKES)    :: RESHT    !-- reservoir height (m)
        REAL, INTENT(INOUT), DIMENSION(NLAKES)    :: QLAKEI   !-- lake inflow (cms)
        REAL,                DIMENSION(NLAKES)    :: QLAKEIP  !-- lake inflow previous timestep (cms)
        REAL, INTENT(INOUT), DIMENSION(NLAKES)    :: QLAKEO   !-- outflow from lake used in diffusion scheme
        INTEGER, INTENT(IN), DIMENSION(NLINKS)    :: LAKENODE !-- outflow from lake used in diffusion scheme
        INTEGER, INTENT(IN), DIMENSION(NLINKS)    :: LINKID   !--  id of channel elements for linked scheme
        REAL, DIMENSION(NLINKS)                   :: QLateral !--lateral flow
        REAL, DIMENSION(NLINKS)                   :: QSUM     !--mass bal of node
        REAL, DIMENSION(NLAKES)                   :: QLLAKE   !-- lateral inflow to lake in diffusion scheme

!-- Local Variables
        INTEGER                     :: i,j,k,t,m,jj,kk,KRT,node
        INTEGER                     :: DT_STEPS               !-- number of timestep in routing
        REAL                        :: Qup,Quc                !--Q upstream Previous, Q Upstream Current, downstream Previous
        REAL                        :: bo                     !--critical depth, bnd outflow just for testing
        REAL                        :: AREA,WP                !--wetted area and perimiter for MuskingC. routing

        REAL ,DIMENSION(NLINKS)     :: HLINKTMP,CVOLTMP       !-- temporarily store head values and volume values
        REAL ,DIMENSION(NLINKS)     :: CD                     !-- critical depth
        real, DIMENSION(IXRT,JXRT)  :: tmp
        real, dimension(nlinks)     :: tmp2

#ifdef MPP_LAND
        integer lake_index(nlakes)
        integer nlinks_index(nlinks)
        integer mpp_nlinks, iyw, yw_mpp_nlinks
        integer link_location(ixrt,jxrt)
        real     ywtmp(ixrt,jxrt)
        integer LNLINKSL
        INTEGER, dimension(LNLINKSL) :: LLINKID
        real*8,  dimension(LNLINKSL) :: LQLateral
!        real*4,  dimension(LNLINKSL) :: LQLateral
        integer, dimension(:) ::  toNodeInd
        integer, dimension(:,:) ::  gtoNode
        integer  :: nToNodeInd
        real, dimension(nToNodeInd,2) :: gQLINK
#else
        REAL*8, DIMENSION(NLINKS)                   :: LQLateral !--lateral flow
#endif
        integer flag

        integer :: n, kk2, nt, nsteps  ! tmp 

        QLAKEIP = 0
        HLINKTMP = 0
        CVOLTMP = 0
        CD = 0  
        node = 1
        QLateral = 0
        QSUM     = 0
        QLLAKE   = 0


!yw      print *, "DRIVE_channel,option,nlinkl,nlinks!!", channel_option,NLINKSL,NLINKS
         
      dzGwChanHead = 0.

   IF(channel_option .ne. 3) then   !--muskingum methods ROUTE ON DT timestep, not DTRT!!

         nsteps = (DT+0.5)/DTRT_CH

#ifdef MPP_LAND
         LQLateral = 0          !-- initial lateral flow to 0 for this reach
         DO iyw = 1,yw_MPP_NLINKS
         jj = nlinks_index(iyw)
          !--------river grid points, convert depth in mm to rate across reach in m^3/sec
              if( .not. (  (CHANXI(jj) .eq. 1 .and. left_id .ge. 0) .or. &
                           (CHANXI(jj) .eq. ixrt .and. right_id .ge. 0) .or. &
                           (CHANYJ(jj) .eq. 1 .and. down_id .ge. 0) .or. &
                           (CHANYJ(jj) .eq. jxrt .and. up_id .ge. 0)      &
                   ) ) then
                  if (CH_LNKRT_SL(CHANXI(jj),CHANYJ(jj)) .gt. 0) then
                     k = CH_LNKRT_SL(CHANXI(jj),CHANYJ(jj))
                     LQLateral(k) = LQLateral(k)+((QSTRMVOLRT(CHANXI(jj),CHANYJ(jj))+QINFLOWBASE(CHANXI(jj),CHANYJ(jj)))/1000 & 
                            *node_area(jj)/DT)
                  elseif ( (LAKE_MSKRT(CHANXI(jj),CHANYJ(jj)) .gt. 0)) then !-lake grid
                      k = LAKE_MSKRT(CHANXI(jj),CHANYJ(jj))
                      LQLateral(k) = LQLateral(k) +((LAKEINFLORT(CHANXI(jj),CHANYJ(jj))+QINFLOWBASE(CHANXI(jj),CHANYJ(jj)))/1000 &
                               *node_area(jj)/DT)
                  endif
              endif
         end do  ! jj


!   assign LQLATERAL to QLATERAL
       call updateLinkV(LQLateral, QLateral(1:NLINKSL))

#else
         LQLateral = 0          !-- initial lateral flow to 0 for this reach
         do jj = 1, NLINKS
          !--------river grid points, convert depth in mm to rate across reach in m^3/sec

                  if (CH_LNKRT_SL(CHANXI(jj),CHANYJ(jj)) .gt. 0 ) then
                     k = CH_LNKRT_SL(CHANXI(jj),CHANYJ(jj))
                     LQLateral(k) = LQLateral(k)+((QSTRMVOLRT(CHANXI(jj),CHANYJ(jj))+QINFLOWBASE(CHANXI(jj),CHANYJ(jj)))/1000 & 
                            *node_area(jj)/DT)
                  elseif ( (LAKE_MSKRT(CHANXI(jj),CHANYJ(jj)) .gt. 0)) then !-lake grid
                      k = LAKE_MSKRT(CHANXI(jj),CHANYJ(jj))
                      LQLateral(k) = LQLateral(k) +((LAKEINFLORT(CHANXI(jj),CHANYJ(jj))+QINFLOWBASE(CHANXI(jj),CHANYJ(jj)))/1000 &
                               *node_area(jj)/DT)
                  endif   

          end do  ! jj
          QLateral = LQLateral
#endif

!       QLateral = QLateral / nsteps

   do nt = 1, nsteps

 
!----------  route order 1 reaches which have no upstream inflow
        do k=1, NLINKSL
           if (ORDER(k) .eq. 1) then  !-- first order stream has no headflow


              if(TYPEL(k) .eq. 1) then    !-- level pool route of reservoir
                  !CALL LEVELPOOL(1,0.0, 0.0, qd, QLINK(k,2), QLateral(k), &
                  ! DT, RESHT(k), HRZAREA(k), LAKEMAXH(k), &
                  ! WEIRC(k), WEIRL(k), ORIFICEE(i), ORIFICEC(k), ORIFICEA(k) )
              elseif (channel_option .eq. 1) then
                   Km  = MUSK(k)
                   X   = MUSX(k)
                   QLINK(k,2) = MUSKING(k,0.0, QLateral(k), QLINK(k,1), DTRT_CH, Km, X) !--current outflow
              elseif (channel_option .eq. 2) then !-- upstream is assumed constant initial condition

                 ! HLINK(k) = MUSKINGCUNGE(k,0.0,0.0,QLINK(k,1), &
                   QLINK(k,2) = MUSKINGCUNGE(k,0.0,0.0,QLINK(k,1), &
                         QLateral(k), DTRT_CH, So(k), CHANLEN(k), &
                         MannN(k), ChSSlp(k), Bw(k))

                 ! AREA = (Bw(k) * HLINK(k) + 1/ChSSlp(k) * HLINK(k)**2)
                 ! WP   = (Bw(k) * HLINK(k) + 1/ChSSlp(k) * HLINK(k)**2) / (Bw(k) + 2 * HLINK(k) * sqrt(1+(1/ChSSlp(k))**2))
                 ! QLINK(k,2) = 1/MannN(k) * AREA * WP**(2./3.) * sqrt(So(k))

              else
                  print *, "FATAL ERROR: No channel option selected"
                  call hydro_stop("In drive_CHANNEL() -No channel option selected ") 
              endif
           endif
        end do

#ifdef MPP_LAND
       gQLINK = 0
       call gbcastReal2(toNodeInd,nToNodeInd,QLINK(1:NLINKSL,2), NLINKSL, gQLINK(:,2))
       call gbcastReal2(toNodeInd,nToNodeInd,QLINK(1:NLINKSL,1), NLINKSL, gQLINK(:,1))
#endif

      !---------- route other reaches, with upstream inflow
       tmpQlink = 0
       do k = 1,NLINKSL
          if (ORDER(k) .gt. 1 ) then  !-- exclude first order stream 
             Quc  = 0
             Qup  = 0

#ifdef MPP_LAND
!using mapping index
               do n = 1, gtoNODE(k,1)
                  m = gtoNODE(k,n+1)
!yw                  if (LINKID(k) .eq. m) then
                    Quc = Quc + gQLINK(m,2)  !--accum of upstream inflow of current timestep (2)
                    Qup = Qup + gQLINK(m,1)  !--accum of upstream inflow of previous timestep (1)

                      !     if(LINKID(k) .eq. 3259 .or. LINKID(k) .eq. 3316 .or. LINKID(k) .eq. 3219) then
                      !       write(6,*) "id,Uc,Up",LINKID(k),Quc,Qup
                      !       call flush(6)
                      !     endif

!yw                  endif
                end do ! do i

#else
               do m = 1, NLINKSL
                  if (LINKID(k) .eq. TO_NODE(m)) then
                    Quc = Quc + QLINK(m,2)  !--accum of upstream inflow of current timestep (2)
                    Qup = Qup + QLINK(m,1)  !--accum of upstream inflow of previous timestep (1)
                  endif
               end do ! do m
#endif
                   
                if(TYPEL(k) .eq. 1) then   !--link is a reservoir

                   ! CALL LEVELPOOL(1,QLINK(k,1), Qup, QLINK(k,1), QLINK(k,2), &
                   !  QLateral(k), DT, RESHT(k), HRZAREA(k), LAKEMAXH(k), &
                   !  WEIRC(k), WEIRL(k),ORIFICEE(k),  ORIFICEC(k), ORIFICEA(k))

                   elseif (channel_option .eq. 1) then  !muskingum routing
                       Km = MUSK(k)
                       X = MUSX(k)
                       tmpQLINK(k,2) = MUSKING(k,Qup,(Quc+QLateral(k)),QLINK(k,1),DTRT_CH,Km,X) !upstream plust lateral inflow 
                   elseif (channel_option .eq. 2) then ! muskingum cunge

                    !HLINK(k) =   MUSKINGCUNGE(k,Qup, Quc, QLINK(k,1), &
                    tmpQLINK(k,2) = MUSKINGCUNGE(k,Qup, Quc, QLINK(k,1), &
                                    QLateral(k), DTRT_CH, So(k),  CHANLEN(k), &
                                    MannN(k), ChSSlp(k), Bw(k) )

                    ! AREA = (Bw(k) * HLINK(k) + 1/ChSSLP(k) * HLINK(k)**2)
                    ! WP   = (Bw(k) * HLINK(k) + 1/ChSSLP(k) * HLINK(k)**2) / (Bw(k) + 2 * HLINK(k) * sqrt(1+(1/ChSSLP(k))**2))
                    ! tmpQLINK(k,2) = ((1/MannN(k)) * AREA * WP**(2./3.) * sqrt(So(k)))

                   else
                    print *, "FATAL ERROR: no channel option selected"
                    call hydro_stop("In drive_CHANNEL() - no channel option selected") 
                   endif
            endif !!! order(1) .ne. 1
         end do       !--k links

!yw check
!        gQLINK = 0.0
!        call ReachLS_write_io(tmpQLINK(:,2), gQLINK(:,2))
!        call ReachLS_write_io(tmpQLINK(:,1), gQLINK(:,1))
!        write(6,*) " io_id = ", io_id
!        if(my_id .eq. io_id) then
!            write(71,*) gQLINK(:,1)
!            call flush(71)
!            call flush(72)
!        endif

          do k = 1, NLINKSL
            if(TYPEL(k) .ne. 1) then
               QLINK(k,2) = tmpQLINK(k,2)
            endif
            QLINK(k,1) = QLINK(k,2)    !assing link flow of current to be previous for next time step
         end do

!#ifdef MPP_LAND 
!         call ReachLS_write_io(QLINK(:,2),buf1)
!         if(my_id .eq. IO_id) write(73,*) buf1
!#else
!         write(73,*) QLINK(1:NLINKSL,2)
!#endif

#ifdef HYDRO_D
          print *, "END OF ALL REACHES...",KRT,DT_STEPS
#endif

   end do  ! nsteps

!    END DO !-- krt timestep for muksingumcunge routing

   elseif(channel_option .eq. 3) then   !--- route using the diffusion scheme on nodes not links

#ifdef MPP_LAND
         call MPP_CHANNEL_COM_REAL(Link_location,ixrt,jxrt,HLINK,NLINKS,99)
         call MPP_CHANNEL_COM_REAL(Link_location,ixrt,jxrt,CVOL,NLINKS,99)
#endif

         KRT = 0                  !-- initialize the time counter
         minDTCT = 0.01           ! define minimum routing sub-timestep (s), simulation will end with smaller timestep
         DTCT = min(max(DTCT*2.0, minDTCT),DTRT_CH)
       
         HLINKTMP = HLINK         !-- temporary storage of the water elevations (m)
         CVOLTMP = CVOL           !-- temporary storage of the volume of water in channel (m^3)
         QLAKEIP = QLAKEI         !-- temporary lake inflow from previous timestep  (cms)

!        call check_channel(77,HLINKTMP,1,nlinks)
!        call MPP_CHANNEL_COM_REAL(Link_location,ixrt,jxrt,ZELEV,NLINKS,99)
 crnt:    DO                      !-- loop on the courant condition
         QSUM   = 0              !-- initialize the total flow out of each cell to zero
         QLAKEI = 0              !-- set the lake inflow as zero
         QLLAKE = 0              !-- initialize each lake's lateral inflow to zero  
         DT_STEPS=INT(DT/DTCT)   !-- fix the timestep
         QLateral = 0. 
!DJG GW-chan coupling variables...
         if(gwBaseSwCRT == 3) then
	  Q_GW_CHAN_FLUX = 0.
	  qgw_chanrt     = 0.
         end if
         
!         ZWATTBLRT=1.0   !--HARDWIRE, remove this and pass in from subsfc/gw routing routines...


!-- vectorize
!--------------------- 
#ifdef MPP_LAND
         DO iyw = 1,yw_MPP_NLINKS
         i = nlinks_index(iyw)
#else
         DO i = 1,NLINKS
#endif
          
           if(node_area(i) .eq. 0) then
               write(6,*) "FATAL ERROR: node_area(i) is zero. i=", i
               call hydro_stop("In drive_CHANNEL() - Error node_area") 
           endif

           

nodeType:if((CH_NETRT(CHANXI(i), CHANYJ(i) ) .eq. 0) .and. &
              (LAKE_MSKRT(CHANXI(i),CHANYJ(i)) .lt.0) ) then !--a reg. node
              
gwOption:   if(gwBaseSwCRT == 3) then

             ! determine potential gradient between groundwater head and channel stage
             ! units in (m)
             dzGwChanHead(i) = gwHead(CHANXI(i),CHANYJ(i)) - (HLINK(i)+ZELEV(i)) 

             if(gwChanCondSw .eq. 0) then
	       
                qgw_chanrt(CHANXI(i),CHANYJ(i)) = 0.
	        
             else if(gwChanCondSw .eq. 1 .and. dzGwChanHead(i) > 0) then
	       
	       ! channel bed interface, units in (m^3/s), flux into channel...
	       ! BF todo: consider channel width
                qgw_chanrt(CHANXI(i),CHANYJ(i)) = gwChanCondConstIn * dzGwChanHead(i) &
                                                * CHANLEN(i) * 2. 

             else if(gwChanCondSw .eq. 1 .and. dzGwChanHead(i) < 0) then
	       
	       ! channel bed interface, units in (m^3/s), flux out of channel...
	       ! BF todo: consider channel width
                qgw_chanrt(CHANXI(i),CHANYJ(i)) = max(-0.005, gwChanCondConstOut * dzGwChanHead(i) &
                                                * CHANLEN(i) * 2.)
!              else if(gwChanCondSw .eq. 2 .and. dzGwChanHead(i) > 0) then  TBD: exponential dependency
!              else if(gwChanCondSw .eq. 2 .and. dzGwChanHead(i) > 0) then
	       
             else
	       
	        qgw_chanrt(CHANXI(i),CHANYJ(i)) = 0.
	        
             end if
             
             Q_GW_CHAN_FLUX(i) = qgw_chanrt(CHANXI(i),CHANYJ(i))
!             if ( i .eq. 1001 ) then
!                print *, Q_GW_CHAN_FLUX(i), dzGwChanHead(i), ELRT(CHANXI(i),CHANYJ(i)), HLINK(i), ZELEV(i)
!             end if
!              if ( Q_GW_CHAN_FLUX(i) .lt. 0. ) then   !-- temporary hardwire for only allowing flux into channel...REMOVE later...
!                 Q_GW_CHAN_FLUX(i) = 0.
! 	        qgw_chanrt(CHANXI(i),CHANYJ(i)) = 0.
!              end if
            
            else
	      Q_GW_CHAN_FLUX(i) = 0.
	    end if gwOption


              QLateral(CH_NETLNK(CHANXI(i),CHANYJ(i))) =  &
!DJG  awaiting gw-channel exchg...  Q_GW_CHAN_FLUX(i)+& ...obsolete-> ((QSUBRT(CHANXI(i),CHANYJ(i))+&
                Q_GW_CHAN_FLUX(i)+&
                ((QSTRMVOLRT(CHANXI(i),CHANYJ(i))+&
                 QINFLOWBASE(CHANXI(i),CHANYJ(i))) &
                   /DT_STEPS*node_area(i)/1000/DTCT)
	if((QLateral(CH_NETLNK(CHANXI(i),CHANYJ(i))).lt.0.) .and. (gwChanCondSw == 0)) then
#ifdef HYDRO_D
               print*, "i, CHANXI(i),CHANYJ(i) = ", i, CHANXI(i),CHANYJ(i)
               print *, "NEGATIVE Lat inflow...",QLateral(CH_NETLNK(CHANXI(i),CHANYJ(i))), &
                         QSUBRT(CHANXI(i),CHANYJ(i)),QSTRMVOLRT(CHANXI(i),CHANYJ(i)), &
                         QINFLOWBASE(CHANXI(i),CHANYJ(i))
#endif
        end if
            elseif(LAKE_MSKRT(CHANXI(i),CHANYJ(i)) .gt. 0 .and. &
               (LAKE_MSKRT(CHANXI(i),CHANYJ(i)) .ne. -9999)) then !--a lake node
              QLLAKE(LAKE_MSKRT(CHANXI(i),CHANYJ(i))) = &
                 QLLAKE(LAKE_MSKRT(CHANXI(i),CHANYJ(i))) + &
                 (LAKEINFLORT(CHANXI(i),CHANYJ(i))+ &
                 QINFLOWBASE(CHANXI(i),CHANYJ(i)) &
                 /DT_STEPS*node_area(i)/1000/DTCT)
            elseif(CH_NETRT(CHANXI(i),CHANYJ(i)) .gt. 0) then  !pour out of lake
                 QLateral(CH_NETLNK(CHANXI(i),CHANYJ(i))) =  &
                   QLAKEO(CH_NETRT(CHANXI(i),CHANYJ(i)))  !-- previous timestep
          endif nodeType
        ENDDO


#ifdef MPP_LAND
    call MPP_CHANNEL_COM_REAL(Link_location,ixrt,jxrt,QLateral,NLINKS,99)
    if(NLAKES .gt. 0) then
       call MPP_CHANNEL_COM_REAL(LAKE_MSKRT   ,ixrt,jxrt,QLLAKE,NLAKES,99)
    endif
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
               if(from_node(i) .ne. to_node(i) .and. (to_node(i) .gt. 0) .and.(from_node(i) .gt. 0) ) &  ! added by Wei Yu
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
    call MPP_CHANNEL_COM_REAL(Link_location,ixrt,jxrt,qsum,NLINKS,0)
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
#ifdef HYDRO_D
            write(6,*) "******* start diag ***************"
            write(6,*) "Unstable at node ",i, "i=",CHANXI(i),"j=",CHANYJ(i)
            write(6,*) "Unstatble at node ",i, "lat=",latval(CHANXI(i),CHANYJ(i)), "lon=",lonval(CHANXI(i),CHANYJ(i))
            write(6,*) "TYPEL, CVOLTMP, QSUM, QSUM*DTCT",TYPEL(i), CVOLTMP(i), QSUM(i), QSUM(i)*DTCT
            write(6,*) "qsubrt, qstrmvolrt,qlink",QSUBRT(CHANXI(i),CHANYJ(i)),QSTRMVOLRT(CHANXI(i),CHANYJ(i)),qlink(i,1),qlink(i,2)
!              write(6,*) "current nodes, z, h", ZELEV(FROM_NODE(i)),HLINKTMP(FROM_NODE(i))
!           if(TO_NODE(i) .gt. 0) then
!              write(6,*) "to nodes, z, h", ZELEV(TO_NODE(i)), HLINKTMP(TO_NODE(i))
!           else
!              write(6,*) "no to nodes   "
!           endif
               write(6,*) "CHANLEN(i), MannN(i), Bw(i), ChSSlp(i) ", CHANLEN(i), MannN(i), Bw(i), ChSSlp(i)
            write(6,*) "*******end of  diag ***************"
#endif
            
            goto 999  
            endif 
          enddo 

999 continue
#ifdef MPP_LAND
        call mpp_same_int1(flag)
#endif


        if(flag < 0  .and. DTCT >0.1)   then   
             
             ! call smoth121(HLINK,nlinks,maxv_p,pnode,to_node)

             if(DTCT .gt. minDTCT) then                !-- timestep in seconds
              DTCT = max(DTCT/2 , minDTCT)             !-- 1/2 timestep
              KRT = 0                                  !-- restart counter
              HLINKTMP = HLINK                         !-- set head and vol to start value of timestep
              CVOLTMP = CVOL
              CYCLE crnt                               !-- start cycle over with smaller timestep
             else
              write(6,*) "Courant error with smallest routing timestep DTCT: ",DTCT
!              call hydro_stop("drive_CHANNEL")
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
              if((CVOLTMP(i) .lt. 0) .and. (gwChanCondSw == 0)) then 
#ifdef HYDRO_D
                print *, "WARNING! channel volume less than 0:i,CVOL,QSUM,QLat", &
                               i, CVOLTMP(i),QSUM(i),QLateral(i),HLINK(i)
#endif
                CVOLTMP(i) =0 
              endif

            elseif(TYPEL(i) .eq. 1) then               !-- pour point, critical depth downstream 

              if (QSUM(i)+QLateral(i) .lt. 0) then
              else

!DJG remove to have const. flux b.c....   CD(i) =CRITICALDEPTH(i,abs(QSUM(i)+QLateral(i)), Bw(i), 1./ChSSlp(i))
                  CD(i) = HLINKTMP(i)  !This is a temp hardwire for flow depth for the pour point...
               endif

               ! change in volume is inflow, lateral flow, and outflow 
               !yw DIFFUSION(i,ZELEV(i),ZELEV(i)-(So(i)*DXRT),HLINKTMP(i), &
                   CVOLTMP(i) = CVOLTMP(i) + (QSUM(i) + QLateral(i) - &
                       DIFFUSION(i,ZELEV(i),ZELEV(i)-(So(i)*CHANLEN(i)),HLINKTMP(i), &
                       CD(i),CHANLEN(i), MannN(i), Bw(i), ChSSlp(i)) ) * DTCT
            elseif (TYPEL(i) .eq. 2) then              !--- into a reservoir, assume critical depth
              if ((QSUM(i)+QLateral(i) .lt. 0) .and. (gwChanCondSw == 0)) then
#ifdef HYDRO_D
               print *, i, 'CrtDpth Qsum+QLat into lake< 0',QSUM(i), QLateral(i)
#endif
              else
!DJG remove to have const. flux b.c....    CD(i) =CRITICALDEPTH(i,abs(QSUM(i)+QLateral(i)), Bw(i), 1./ChSSlp(i))
               CD(i) = HLINKTMP(i)  !This is a temp hardwire for flow depth for the pour point...
              endif
 
              !-- compute volume in reach (m^3)
                   CVOLTMP(i) = CVOLTMP(i) + (QSUM(i) + QLateral(i) - &
                          DIFFUSION(i,ZELEV(i),ZELEV(i)-(So(i)*CHANLEN(i)),HLINKTMP(i), &
                             CD(i) ,CHANLEN(i), MannN(i), Bw(i), ChSSlp(i)) ) * DTCT
              !-- compute flow rate into lake from all contributing nodes (cms)
              QLAKEI(LAKENODE(i)) = QLAKEI(LAKENODE(i)) + QLINK(FROM_NODE(i),1)

            else
              print *, "FATAL ERROR: This node does not have a type.. error TYPEL =", TYPEL(i)
              call hydro_stop("In drive_CHANNEL() - error TYPEL") 
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
    if(NLAKES .gt. 0) then
       call MPP_CHANNEL_COM_REAL(LAKE_MSKRT,ixrt,jxrt,QLAKEI,NLAKES,99)
    endif
    call MPP_CHANNEL_COM_REAL(Link_location,ixrt,jxrt,HLINKTMP,NLINKS,99)
#endif 
!   call check_channel(83,CVOLTMP,1,nlinks)
!   call check_channel(84,CD,1,nlinks)
!   call check_channel(85,HLINKTMP,1,nlinks)
!   call check_lake(86,QLAKEI,lake_index,nlakes)

      



           do i = 1, NLAKES !-- mass balances of lakes
#ifdef MPP_LAND
            if(lake_index(i) .gt. 0)  then
#endif
              CALL LEVELPOOL(i,QLAKEIP(i), QLAKEI(i), QLAKEO(i), QLLAKE(i), &
                DTCT, RESHT(i), HRZAREA(i), WEIRH(i), LAKEMAXH(i), WEIRC(i), &
                WEIRL(i), ORIFICEE(i), ORIFICEC(i), ORIFICEA(i))
                QLAKEIP(i) = QLAKEI(i)  !-- store total lake inflow for this timestep
#ifdef MPP_LAND
            endif
#endif
           enddo
#ifdef MPP_LAND
    if(NLAKES .gt. 0) then
       call MPP_CHANNEL_COM_REAL(LAKE_MSKRT,ixrt,jxrt,QLLAKE,NLAKES,99)
       call MPP_CHANNEL_COM_REAL(LAKE_MSKRT,ixrt,jxrt,RESHT,NLAKES,99)
       call MPP_CHANNEL_COM_REAL(LAKE_MSKRT,ixrt,jxrt,QLAKEO,NLAKES,99)
       call MPP_CHANNEL_COM_REAL(LAKE_MSKRT,ixrt,jxrt,QLAKEI,NLAKES,99)
       call MPP_CHANNEL_COM_REAL(LAKE_MSKRT,ixrt,jxrt,QLAKEIP,NLAKES,99)
    endif
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
        else                                !-- no channel option apparently selected
         print *, "FATAL ERROR: no channel option selected"
         call hydro_stop("In drive_CHANNEL() - no channel option selected") 
        endif

#ifdef HYDRO_D
         write(6,*) "finished call drive_CHANNEL"
#endif

        if (KT .eq. 1) KT = KT + 1
         

 END SUBROUTINE drive_CHANNEL
! ----------------------------------------------------------------

!-=======================================
     REAL FUNCTION AREAf(AREA,Bw,h,z)
     REAL :: AREA, Bw, z, h
       AREAf = (Bw+z*h)*h-AREA       !-- Flow area
     END FUNCTION AREAf

!-====critical depth function  ==========
     REAL FUNCTION CDf(Q,Bw,h,z)
     REAL :: Q, Bw, z, h
       if(h .le. 0) then
         print *, "FATAL ERROR: head is zero, will get division by zero error"
         call hydro_stop("In CDf() - head is zero") 
       else
       CDf = (Q/((Bw+z*h)*h))/(sqrt(9.81*(((Bw+z*h)*h)/(Bw+2*z*h))))-1  !--critical depth function
       endif
     END FUNCTION CDf

!=======find flow depth in channel with bisection Chapra pg. 131
    REAL FUNCTION HEAD(idx,AREA,Bw,z)  !-- find the water elevation given wetted area, 
                                         !--bottom widith and side channel.. index was for debuggin
     REAL :: Bw,z,AREA,test           
     REAL :: hl, hu, hr, hrold
     REAL :: fl, fr,error                !-- function evaluation
     INTEGER :: maxiter, idx

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
     R = ((Bw+z*h1)*h1)/(Bw+2*h1*sqrt(1+z*z)) !-- Hyd Radius
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
        ! call hydro_stop("In CRITICALDEPTH()") 
        CRITICALDEPTH = -9999
        return
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

    subroutine check_lake(unit,cd,lake_index,nlakes)
         use module_RT_data, only: rt_domain
         implicit none 
         integer :: unit,nlakes,i,lake_index(nlakes)
         real cd(nlakes)
#ifdef MPP_LAND
         call write_lake_real(cd,lake_index,nlakes)
#endif
         write(unit,*) cd
          call flush(unit)
         return
    end subroutine check_lake

    subroutine check_channel(unit,cd,did,nlinks)
         use module_RT_data, only: rt_domain
#ifdef MPP_LAND
  USE module_mpp_land
#endif
         implicit none 
         integer :: unit,nlinks,i, did
         real cd(nlinks)
#ifdef MPP_LAND
         real g_cd(rt_domain(did)%gnlinks)
         call write_chanel_real(cd,rt_domain(did)%map_l2g,rt_domain(did)%gnlinks,nlinks,g_cd)
         if(my_id .eq. IO_id) then
            write(unit,*) "rt_domain(did)%gnlinks = ",rt_domain(did)%gnlinks
           write(unit,*) g_cd
         endif
#else
           write(unit,*) cd
#endif
          call flush(unit)
          close(unit)
         return
    end subroutine check_channel
    subroutine smoth121(var,nlinks,maxv_p,from_node,to_node)
        implicit none
        integer,intent(in) ::  nlinks, maxv_p
        integer, intent(in), dimension(nlinks):: to_node
        integer, intent(in), dimension(nlinks):: from_node(nlinks,maxv_p)
        real, intent(inout), dimension(nlinks) :: var
        real, dimension(nlinks) :: vartmp
        integer :: i,j  , k, from,to
        integer :: plen
              vartmp = 0
              do i = 1, nlinks
                 to = to_node(i)
                 plen = from_node(i,1)
                 if(plen .gt. 1) then 
                     do k = 1, plen-1 
                         from = from_node(i,k+1)
                         if(to .gt. 0) then
                            vartmp(i) = vartmp(i)+0.25*(var(from)+2.*var(i)+var(to))
                         else
                            vartmp(i) = vartmp(i)+(2.*var(i)+var(from))/3.0
                         endif
                     end do
                     vartmp(i) = vartmp(i) /(plen-1)
                 else
                         if(to .gt. 0) then
                            vartmp(i) = vartmp(i)+(2.*var(i)+var(to)/3.0)
                         else
                            vartmp(i) = var(i)
                         endif
                 endif
              end do
              var = vartmp 
        return
    end subroutine smoth121

!   SUBROUTINE drive_CHANNEL for NHDPLUS
! ------------------------------------------------

     Subroutine drive_CHANNEL_RSL(UDMP_OPT,KT, IXRT,JXRT,  &
        LAKEINFLORT, QSTRMVOLRT, TO_NODE, FROM_NODE, &
        TYPEL, ORDER, MAXORDER,   CH_LNKRT, &
        LAKE_MSKRT, DT, DTCT, DTRT_CH,MUSK, MUSX, QLINK, &
        CHANLEN, MannN, So, ChSSlp, Bw, &
        RESHT, HRZAREA, LAKEMAXH, WEIRH, WEIRC, WEIRL, ORIFICEC, ORIFICEA, &
        ORIFICEE,  CVOL, QLAKEI, QLAKEO, LAKENODE, &
        QINFLOWBASE, CHANXI, CHANYJ, channel_option,  &
        nlinks,NLINKSL, LINKID, node_area, qout_gwsubbas, &
        LAKEIDA, LAKEIDM, NLAKES, LAKEIDX, &
#ifdef MPP_LAND 
        nlinks_index,mpp_nlinks,yw_mpp_nlinks, &
        LNLINKSL, &
        gtoNode,toNodeInd,nToNodeInd,   &
#endif
         CH_LNKRT_SL, landRunOff  & 
#ifdef WRF_HYDRO_NUDGING
       , nudge &
#endif

       ,  accLndRunOff, accQLateral, accStrmvolrt, accBucket   &
       , QLateral, velocity &
       ,nsize , OVRTSWCRT, SUBRTSWCRT      )

       use module_UDMAP, only: LNUMRSL, LUDRSL

#ifdef WRF_HYDRO_NUDGING
       use module_stream_nudging,  only: setup_stream_nudging,  & 
                                         nudge_term_all,        &
                                         nudgeWAdvance
#endif 


       IMPLICIT NONE

! -------- DECLARATIONS ------------------------

        INTEGER, INTENT(IN) :: IXRT,JXRT,channel_option, OVRTSWCRT, SUBRTSWCRT
        INTEGER, INTENT(IN) :: NLAKES, NLINKSL, nlinks
        integer, INTENT(INOUT) :: KT   ! flag of cold start (1) or continue run.
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)    :: QSTRMVOLRT
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)    :: LAKEINFLORT
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT)    :: QINFLOWBASE
        real, dimension(ixrt,jxrt) :: landRunOff

        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT) :: CH_LNKRT
        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT) :: CH_LNKRT_SL

        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT) :: LAKE_MSKRT
        INTEGER, INTENT(IN), DIMENSION(:)         :: ORDER, TYPEL !--link
        INTEGER, INTENT(IN), DIMENSION(:)     :: TO_NODE, FROM_NODE
        INTEGER, INTENT(IN), DIMENSION(:)     :: CHANXI, CHANYJ
        REAL, INTENT(IN), DIMENSION(:)        :: MUSK, MUSX
        REAL, INTENT(IN), DIMENSION(:)        :: CHANLEN
        REAL, INTENT(IN), DIMENSION(:)        :: So, MannN
        REAL, INTENT(IN), DIMENSION(:)        :: ChSSlp,Bw  !--properties of nodes or links
        REAL                                      :: Km, X
        REAL , INTENT(INOUT), DIMENSION(:,:)  :: QLINK
#ifdef WRF_HYDRO_NUDGING
        real, intent(out),    dimension(:)    :: nudge
#endif
        REAL, DIMENSION(:), intent(out)       :: QLateral, velocity !--lateral flow
        real, dimension(:), intent(out)       :: accLndRunOff, accQLateral, accStrmvolrt, accBucket 

        REAL ,  DIMENSION(NLINKSL,2) :: tmpQLINK
        REAL, INTENT(IN)                          :: DT    !-- model timestep
        REAL, INTENT(IN)                          :: DTRT_CH  !-- routing timestep
        REAL, INTENT(INOUT)                       :: DTCT
        real                                      :: minDTCT !BF minimum routing timestep
        INTEGER, INTENT(IN)                       :: MAXORDER
        REAL , INTENT(IN), DIMENSION(:)   :: node_area

!DJG GW-chan coupling variables...
        REAL, DIMENSION(NLINKS)                   :: dzGwChanHead
        REAL, DIMENSION(NLINKS)                   :: Q_GW_CHAN_FLUX     !DJG !!! Change 'INTENT' to 'OUT' when ready to update groundwater state...
        REAL, DIMENSION(IXRT,JXRT)                :: ZWATTBLRT          !DJG !!! Match with subsfce/gw routing & Change 'INTENT' to 'INOUT' when ready to update groundwater state...

        !-- lake params

        REAL, INTENT(IN), DIMENSION(:)       :: HRZAREA  !-- horizontal area (km^2)
        REAL, INTENT(IN), DIMENSION(:)       :: LAKEMAXH !-- maximum lake depth  (m^2)
        REAL, INTENT(IN), DIMENSION(:)       :: WEIRH    !--  lake depth  (m^2)
        REAL, INTENT(IN), DIMENSION(:)       :: WEIRC    !-- weir coefficient
        REAL, INTENT(IN), DIMENSION(:)       :: WEIRL    !-- weir length (m)
        REAL, INTENT(IN), DIMENSION(:)       :: ORIFICEC !-- orrifice coefficient
        REAL, INTENT(IN), DIMENSION(:)       :: ORIFICEA !-- orrifice area (m^2)
        REAL, INTENT(IN), DIMENSION(:)       :: ORIFICEE !-- orrifce elevation (m)
        INTEGER, INTENT(IN), DIMENSION(:)    :: LAKEIDM  !-- NHDPLUS lakeid for lakes to be modeled

        REAL, INTENT(INOUT), DIMENSION(:)    :: RESHT    !-- reservoir height (m)
        REAL, INTENT(INOUT), DIMENSION(:)    :: QLAKEI   !-- lake inflow (cms)
        REAL,                DIMENSION(NLAKES)    :: QLAKEIP  !-- lake inflow previous timestep (cms)
        REAL, INTENT(INOUT), DIMENSION(NLAKES)    :: QLAKEO   !-- outflow from lake used in diffusion scheme

        INTEGER, INTENT(IN), DIMENSION(:)    :: LAKENODE !-- outflow from lake used in diffusion scheme
        INTEGER, INTENT(IN), DIMENSION(:)   :: LINKID   !--  id of channel elements for linked scheme
        INTEGER, INTENT(IN), DIMENSION(:)   :: LAKEIDA  !--  (don't need) NHDPLUS lakeid for all lakes in domain
        INTEGER, INTENT(IN), DIMENSION(:)   :: LAKEIDX  !--  the sequential index of the lakes id by com id

        REAL, DIMENSION(NLINKS)                   :: QSUM     !--mass bal of node
        REAL, DIMENSION(NLAKES)                   :: QLLAKE   !-- lateral inflow to lake in diffusion scheme
        integer :: nsize

!-- Local Variables
        INTEGER                      :: i,j,k,t,m,jj,ii,lakeid, kk,KRT,node, UDMP_OPT
        INTEGER                      :: DT_STEPS               !-- number of timestep in routing
        REAL                         :: Qup,Quc                !--Q upstream Previous, Q Upstream Current, downstream Previous
        REAL                         :: bo                     !--critical depth, bnd outflow just for testing

        REAL ,DIMENSION(NLINKS)                          :: CD    !-- critical depth
        real, DIMENSION(IXRT,JXRT)                       :: tmp
        real, dimension(nlinks)                          :: tmp2
        REAL, INTENT(INOUT), DIMENSION(:)           :: CVOL

#ifdef MPP_LAND
        real*8,  dimension(LNLINKSL) :: LQLateral
        real*8,  dimension(LNLINKSL) :: tmpLQLateral
        real,  dimension(NLINKSL)    :: tmpQLateral
        integer nlinks_index(:)
        integer  iyw, yw_mpp_nlinks, mpp_nlinks
        real     ywtmp(ixrt,jxrt)
        integer LNLINKSL
        integer, dimension(:)         ::  toNodeInd
        integer, dimension(:,:)       ::  gtoNode
        integer  :: nToNodeInd
        real, dimension(nToNodeInd,2) :: gQLINK
#else
        real*8,  dimension(NLINKS) :: tmpLQLateral
        real,  dimension(NLINKSL) :: tmpQLateral
        real,  dimension(NLINKSL) :: LQLateral
#endif
        integer flag

        integer :: n, kk2, nt, nsteps  ! tmp 
        real, dimension(:) :: qout_gwsubbas
        real, allocatable,dimension(:) :: tmpQLAKEO, tmpQLAKEI, tmpRESHT
  

        real, dimension(NLINKS) ::  lcLndRunOff, lcQLateral, lcStrmvolrt, lcBucket  ! local variables




#ifdef MPP_LAND
        if(my_id .eq. io_id) then
#endif
            allocate(tmpQLAKEO(NLAKES))
            allocate(tmpQLAKEI(NLAKES))
            allocate(tmpRESHT(NLAKES))
#ifdef MPP_LAND
        endif
#endif

        QLAKEIP = 0
        CD = 0  
        node = 1
        QLateral = 0
        QSUM     = 0
        QLLAKE   = 0
        dzGwChanHead = 0.

#ifdef WRF_HYDRO_NUDGING
         !! Initialize nudging for the current timestep.
         !! This establishes the data structure used to solve the nudges. 
         call setup_stream_nudging(0)  !! always zero b/c at beginning of hydro timestep
#endif /* WRF_HYDRO_NUDGING */

         nsteps = (DT+0.5)/DTRT_CH
         LQLateral = 0          !-- initial lateral flow to 0 for this reach


        tmpLQLateral = 0
        tmpQLateral = 0

        ! NHDPLUS maping
    if(OVRTSWCRT .eq. 0)      then
        do k = 1, LNUMRSL
           ! get from land grid runoff
             do m = 1, LUDRSL(k)%ncell  
                ii =  LUDRSL(k)%cell_i(m)
                jj =  LUDRSL(k)%cell_j(m)
                LQLateral(k) = LQLateral(k)+landRunOff(ii,jj)*LUDRSL(k)%cellweight(m)/1000 & 
                              *LUDRSL(k)%cellArea(m)/DT
                tmpLQLateral(k) = tmpLQLateral(k)+landRunOff(ii,jj)*LUDRSL(k)%cellweight(m)/1000 & 
                              *LUDRSL(k)%cellArea(m)/DT
             end do
        end do
#ifdef MPP_LAND
        call updateLinkV(tmpLQLateral, tmpQLateral)
#endif
        if(NLINKSL .gt. 0) then
            accLndRunOff(1:NLINKSL) = accLndRunOff(1:NLINKSL) + tmpQLateral(1:NLINKSL) * DT
        endif
        tmpLQLateral = 0
        tmpQLateral = 0
     endif

     if(OVRTSWCRT .ne. 0 .or. SUBRTSWCRT .ne. 0 ) then
        do k = 1, LNUMRSL
              ! get from channel grid
              do m = 1, LUDRSL(k)%ngrids
                  ii =  LUDRSL(k)%grid_i(m)
                  jj =  LUDRSL(k)%grid_j(m)
                  LQLateral(k) = LQLateral(k) + QSTRMVOLRT(ii,jj)*LUDRSL(k)%weight(m)/1000 & 
                          *LUDRSL(k)%nodeArea(m)/DT
                  tmpLQLateral(k) = tmpLQLateral(k) + QSTRMVOLRT(ii,jj)*LUDRSL(k)%weight(m)/1000 & 
                          *LUDRSL(k)%nodeArea(m)/DT
              end do
        end do
#ifdef MPP_LAND
        call updateLinkV(tmpLQLateral, tmpQLateral)
#endif
        if(NLINKSL .gt. 0) then
            accStrmvolrt(1:NLINKSL) = accStrmvolrt(1:NLINKSL) + tmpQLateral(1:NLINKSL) * DT
        endif
     endif


#ifdef MPP_LAND
       call updateLinkV(LQLateral, QLateral(1:NLINKSL))
#else
       call hydro_stop("fatal error: NHDPlus only works for parallel now.")
       QLateral = LQLateral
#endif

     if(NLINKSL .gt. 0) then
        QLateral(1:NLINKSL) = QLateral(1:NLINKSL) + qout_gwsubbas(1:NLINKSL)
     endif

     ! accQLateral  = accLndRunOff + QLateral * DT 
     if(NLINKSL .gt. 0) then
        accQLateral(1:NLINKSL)  = accQLateral(1:NLINKSL) + QLateral(1:NLINKSL) * DT 
        accBucket(1:NLINKSL) = accBucket(1:NLINKSL) + qout_gwsubbas(1:NLINKSL) * DT
     endif

!       QLateral = QLateral / nsteps

   do nt = 1, nsteps
 
#ifdef MPP_LAND

       gQLINK = 0
       call gbcastReal2(toNodeInd,nToNodeInd,QLINK(1:NLINKSL,2), NLINKSL, gQLINK(:,2))
       call gbcastReal2(toNodeInd,nToNodeInd,QLINK(1:NLINKSL,1), NLINKSL, gQLINK(:,1)) 
      !---------- route other reaches, with upstream inflow
#endif

       tmpQlink = 0
#ifdef MPP_LAND
       if(my_id .eq. io_id) then
#endif
          tmpQLAKEO = QLAKEO
          tmpQLAKEI = QLAKEI
          tmpRESHT = RESHT
#ifdef MPP_LAND
       endif
#endif


       DO k = 1,NLINKSL

        Quc  = 0
        Qup  = 0

        !process as standard link or a lake inflow link, or lake outflow link
        ! link flowing out of lake, accumulate all the inflows with the revised TO_NODEs
        ! TYPEL = -999 stnd; TYPEL=1 outflow from lake; TYPEL = 3 inflow to a lake

        if(TYPEL(k) .ne. 2) then ! don't process internal lake links only

#ifdef MPP_LAND
!using mapping index
           do n = 1, gtoNODE(k,1)
              m = gtoNODE(k,n+1)
                if(gQLINK(m,2) .gt. 0)   Quc = Quc + gQLINK(m,2)  !--accum of upstream inflow of current timestep (2)
                if(gQLINK(m,1) .gt. 0)   Qup = Qup + gQLINK(m,1)  !--accum of upstream inflow of previous timestep (1)
           end do ! do i
#else
           do m = 1, NLINKSL

               if (LINKID(k) .eq. TO_NODE(m)) then
                 Quc = Quc + QLINK(m,2)  !--accum of upstream inflow of current timestep (2)
                 Qup = Qup + QLINK(m,1)  !--accum of upstream inflow of previous timestep (1)
               endif
           end do ! do m
#endif
        endif !note that we won't process type 2 links, since they are internal to a lake


!yw ### process each link k,
!       There is a situation that different k point to the same LAKEIDX
!        if(TYPEL(k) .eq. 1 .and. LAKEIDX(k) .gt. 0) then   !--link is a reservoir
        if(TYPEL(k) .eq. 1 ) then   !--link is a reservoir
             
             lakeid = LAKEIDX(k)
           if(lakeid .ge. 0) then
             CALL LEVELPOOL(lakeid,Qup, Quc, tmpQLINK(k,2), &
               QLateral(k), DT, RESHT(lakeid), HRZAREA(lakeid), WEIRH(lakeid), LAKEMAXH(lakeid), &
               WEIRC(lakeid), WEIRL(lakeid),ORIFICEE(lakeid), ORIFICEC(lakeid), ORIFICEA(lakeid))
             
               QLAKEO(lakeid)  = tmpQLINK(k,2) !save outflow to lake
               QLAKEI(lakeid)  = Quc           !save inflow to lake
           endif
105  continue
            

        elseif (channel_option .eq. 1) then  !muskingum routing
               Km = MUSK(k)
               X = MUSX(k)
               tmpQLINK(k,2) = MUSKING(k,Qup,(Quc+QLateral(k)),QLINK(k,1),DTRT_CH,Km,X) !upstream plust lateral inflow 

        elseif (channel_option .eq. 2) then ! muskingum cunge, don't process internal lake nodes TYP=2
!              tmpQLINK(k,2) = MUSKINGCUNGE(k,Qup, Quc, QLINK(k,1), &
!                  QLateral(k), DTRT_CH, So(k),  CHANLEN(k), &
!                  MannN(k), ChSSlp(k), Bw(k) )

              CALL SUBMUSKINGCUNGE(tmpQLINK(k,2),velocity(k), k,Qup, Quc, QLINK(k,1), &
                   QLateral(k), DTRT_CH, So(k), CHANLEN(k), &
                   MannN(k), ChSSlp(k), Bw(k) )
                
        else
#ifdef HYDRO_D
                    print *, " no channel option selected"
#endif
                    call hydro_stop("drive_CHANNEL") 
        endif

       END DO        !--k links

#ifdef MPP_LAND
       call updateLake_seq(QLAKEO,nlakes,tmpQLAKEO)
       call updateLake_seq(QLAKEI,nlakes,tmpQLAKEI)
       call updateLake_seq(RESHT,nlakes,tmpRESHT)
#endif

       do k = 1, NLINKSL !tmpQLINK?
          if(TYPEL(k) .ne. 2) then !only the internal lake nodes don't have info.. but need to save QLINK of lake out too
             QLINK(k,2) = tmpQLINK(k,2)
          endif
            QLINK(k,1) = QLINK(k,2)    !assigng link flow of current to be previous for next time step
       end do


#ifdef WRF_HYDRO_NUDGING         
         if(.not. nudgeWAdvance) call nudge_term_all(qlink, nudge, int(nt*dtrt_ch))
#endif /* WRF_HYDRO_NUDGING */


!#ifdef HYDRO_D
!          print *, "END OF ALL REACHES...",KRT,DT_STEPS
!#endif

    end do  ! nsteps

    if (KT .eq. 1) KT = KT + 1

#ifdef MPP_LAND
    if(my_id .eq. io_id)      then 
       if(allocated(tmpQLAKEO))  deallocate(tmpQLAKEO)
       if(allocated(tmpQLAKEI))  deallocate(tmpQLAKEI)
       if(allocated(tmpRESHT))  deallocate(tmpRESHT)
    endif
#endif        

   if (KT .eq. 1) KT = KT + 1

 END SUBROUTINE drive_CHANNEL_RSL

! ----------------------------------------------------------------

END MODULE module_channel_routing

#ifdef MPP_LAND
 subroutine checkReach(ii,  inVar)
   use module_mpp_land
   use module_RT_data, only: rt_domain
   use MODULE_mpp_ReachLS, only : updatelinkv,                   &
                                 ReachLS_write_io, gbcastvalue, &
                                 gbcastreal2
   implicit none
   integer :: ii
   real,dimension(rt_domain(1)%nlinksl) :: inVar
   real:: g_var(rt_domain(1)%gnlinksl)
   call ReachLS_write_io(inVar, g_var)
   if(my_id .eq. io_id) then
      write(ii,*) g_var
      call flush(ii)
   endif
 end subroutine checkReach
#endif
