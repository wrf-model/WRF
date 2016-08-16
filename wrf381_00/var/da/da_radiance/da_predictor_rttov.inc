subroutine da_predictor_rttov(inst, pred,npred,nlevels,temp,hum,t_skin)

   implicit none

   ! temp - RTM level temperatures (k)
   ! hum  - RTM level moistures    (kg/kg)
   ! t-skin - model skin temperature (k)
   ! nlevels - number of RTM levels (1:nlevels)
   ! pf   - RTM full level pressure  (hpa)

   ! pred(1) - 1000-300 thickness
   ! pred(2) - 200-50 thickness
   ! pred(3) - t_skin
   ! pred(4) - total column precipitable water

   integer, intent(in)  :: inst, npred, nlevels
   real,    intent(in)  :: temp(nlevels), hum(nlevels), t_skin
   real,    intent(out) :: pred(npred)

   real, parameter :: kth = gas_constant/gravity
   real, parameter :: kpc = 100.0/gravity
   real :: pf(nlevels)
   real :: tv(nlevels-1), qm(nlevels-1)
   real :: dlp(nlevels-1), dp(nlevels-1)
   real    :: add_thk
   integer :: itmp(1), k
   integer :: index1000, index300, index200, index50

   if (trace_use) call da_trace_entry("da_predictor_rttov")

   pf = coefs(inst)%coef%ref_prfl_p

   do k = 1, nlevels-1
      ! full-level to half-level
      qm(k)=0.5*(hum(k)+hum(k+1))
   end do

   dlp(1:nlevels-1) = log(pf(2:nlevels)) - log(pf(1:nlevels-1))
    dp(1:nlevels-1) =     pf(2:nlevels)  -     pf(1:nlevels-1)

   ! 0.0 find the pressure level index that is closest to 
   ! 1000hPa, 300hPa, 200hPa, and 50hPa respectively

   itmp(1:1) = minloc(abs(pf(:)-1000.0))
   index1000 = itmp(1)
   itmp(1:1) = minloc(abs(pf(:)-300.0))
   index300  = itmp(1)
   itmp(1:1) = minloc(abs(pf(:)-200.0))
   index200  = itmp(1)
   itmp(1:1) = minloc(abs(pf(:)-50.0))
   index50   = itmp(1)

   ! 1.0 convert all temperatures to virtual temperatures
   !     and full-level to half-level
   ! ----------------------------------------------------
   do k = 1, nlevels-1
      tv(k) = 0.5*(temp(k)+temp(k+1))*(1.0+0.608*qm(k))
   end do

   if ( (1000.0 > pf(nlevels)) ) then
      add_thk = kth*( tv(nlevels-1)*(log(1000.0)-log(pf(nlevels))) )  ! approximation
   else
      add_thk = 0.0
   end if

   ! 2.0 construct averages for nesdis thick layers
   ! ----------------------------------------------

   pred(1) = kth*sum( tv(index300+1:index1000)*dlp(index300+1:index1000) ) + add_thk
   pred(2) = kth*sum( tv(index50+1:index200)*dlp(index50+1:index200) )
   pred(3) = t_skin
   pred(4) = kpc*sum( qm(1:nlevels-1)*dp(1:nlevels-1) )

   if (trace_use) call da_trace_exit("da_predictor_rttov")

end subroutine da_predictor_rttov

