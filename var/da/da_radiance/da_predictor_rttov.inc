subroutine da_predictor_rttov(pred,npred,temp,hum,T_skin)

   implicit none

   ! temp - 43 level temperatures
   ! hum  - 43 level moistures
   ! T-skin - model skin temperature

   ! pred(1) - 1000-300 thickness 43 1005.43-521.46 thickness
   ! pred(2) - 200-50 thickness   43  194.36-56.73  thickness
   ! pred(3) - T_skin
   ! pred(4) - total column precipitable water
   ! pred(5) - 10 -2 thickness    43  10.37-2.61 thickness
   ! pred(6) - 50 -10thickness    43  45.29-10.37 thickness

   integer, parameter :: JPRTLEV = 43

   integer, intent(in)  :: npred
   real,    intent(in)  :: temp(JPRTLEV), hum(JPRTLEV), T_skin
   real,    intent(out) :: pred(npred)

   real, parameter :: Kth = gas_constant*0.5/gravity
   real, parameter :: Kpc = 100.0*0.5/gravity

   real :: tv(JPRTLEV)

   logical, SAVE :: FIRST = .true.

   real :: PLEVS(JPRTLEV) =  (/ 0.10,    0.29,    0.69,   1.42,    2.61, &
                                4.41,    6.95,   10.37,  14.81,   20.40, &
                                27.26,   35.51,   45.29,  56.73,   69.97, &
                                85.18,  102.05,  122.04, 143.84,  167.95, &
                               194.36,  222.94,  253.71, 286.60,  321.50, &
                               358.28,  396.81,  436.95, 478.54,  521.46, &
                               565.54,  610.60,  656.43, 702.73,  749.12, &
                               795.09,  839.95,  882.80, 922.46,  957.44, &
                               985.88, 1005.43, 1013.25 /)

   real, SAVE :: DLP(JPRTLEV-1), DP(JPRTLEV-1)

   if (trace_use) call da_trace_entry("da_predictor_rttov")

   ! If first time compute log P(i)/P(i-1) and P(i)-P(i-1)
   if (FIRST) then
      DLP(1:JPRTLEV-1) = LOG(PLEVS(2:JPRTLEV)) - LOG(PLEVS(1:JPRTLEV-1))
      DP(1:JPRTLEV-1) = PLEVS(2:JPRTLEV) - PLEVS(1:JPRTLEV-1)
      FIRST = .false.
   end if

   ! 1.0 Convert all temperatures to virtual temperatures
   ! ----------------------------------------------------
   tv =  temp*(1.0+0.608*hum)

   ! 2.0 Construct averages for NESDIS thick layers
   ! ----------------------------------------------

   pred(1) = Kth*sum((tv(26:42) + tv(25:41))*DLP(25:41))
   pred(2) = Kth*sum((tv(14:21) + tv(13:20))*DLP(13:20))
   pred(3) = T_skin
   pred(4) = Kpc*sum((hum(2:43) + hum(1:42))*DP(1:42))

   if (npred == 6) then
      pred(5) = Kth*sum((tv(5:8)  + tv(4:7))*DLP(4:7))
      pred(6) = Kth*sum((tv(8:13)  + tv(7:12))*DLP(7:12))
   end if

   if (trace_use) call da_trace_exit("da_predictor_rttov")

end subroutine da_predictor_rttov


