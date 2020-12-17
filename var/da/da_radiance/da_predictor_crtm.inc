subroutine da_predictor_crtm(pred,npred,nlevels,temp,hum,t_skin,pf)

   implicit none

   ! temp - model level temperatures (k)
   ! hum  - model level moistures    (g/kg)
   ! t-skin - model skin temperature (k)
   ! nlevels - number of model levels (0:model top)
   ! pm   - model level pressure (hpa)
   ! pf   - full level pressure  (hpa)

   ! pred(1) - 1000-300 thickness
   ! pred(2) - 200-50 thickness
   ! pred(3) - t_skin
   ! pred(4) - total column precipitable water

   integer, intent(in)  :: npred,nlevels
   real,    intent(in)  :: temp(nlevels), hum(nlevels), t_skin
   real,    intent(in)  :: pf(0:nlevels)
   real,    intent(out) :: pred(npred)

   real, parameter :: kth = gas_constant/gravity
   real, parameter :: kpc = 100.0/gravity
   real :: tv(nlevels), qm(nlevels)
   real :: dlp(nlevels), dp(nlevels)
   real    :: add_thk
   integer :: itmp(1)
   integer :: index1000, index300, index200, index50

   if (trace_use) call da_trace_entry("da_predictor_crtm")

   qm=hum*0.001  ! g/kg to kg/kg

   dlp(1:nlevels) = log(pf(1:nlevels)) - log(pf(0:nlevels-1))
    dp(1:nlevels) =     pf(1:nlevels)  -     pf(0:nlevels-1)

   ! 0.0 find the pressure level index that is closest to 
   ! 1000hPa, 300hPa, 200hPa, and 50hPa respectively

   ! note: pf levels are 0:nlevels, 
   ! return values of minloc are 1:nlevels+1
   itmp(1:1) = minloc(abs(pf(:)-1000.0))
   index1000 = itmp(1)-1
   itmp(1:1) = minloc(abs(pf(:)-300.0))
   index300  = itmp(1)-1
   itmp(1:1) = minloc(abs(pf(:)-200.0))
   index200  = itmp(1)-1
   itmp(1:1) = minloc(abs(pf(:)-50.0))
   index50   = itmp(1)-1

   ! 1.0 convert all temperatures to virtual temperatures
   ! ----------------------------------------------------
   tv = temp*(1.0+0.608*qm)

   if ( (1000.0 > pf(nlevels)) ) then
      add_thk = kth*( tv(nlevels)*(log(1000.0)-log(pf(nlevels))) )  ! approximation
   else
      add_thk = 0.0
   end if

   ! 2.0 construct averages for nesdis thick layers
   ! ----------------------------------------------

   pred(1) = kth*sum( tv(index300+1:index1000)*dlp(index300+1:index1000) ) + add_thk
   pred(2) = kth*sum( tv(index50+1:index200)*dlp(index50+1:index200) )
   pred(3) = t_skin
   pred(4) = kpc*sum( qm(1:nlevels)*dp(1:nlevels) )

   if (trace_use) call da_trace_exit("da_predictor_crtm")

end subroutine da_predictor_crtm

