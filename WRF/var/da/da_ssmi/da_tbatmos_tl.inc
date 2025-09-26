subroutine da_tbatmos_tl(ifreq,theta,p0,wv,hwv,ta,gamma,lw,zcld,   &
   tbup,tbdn,tauatm,                         &
   TGL_theta,TGL_p0,TGL_wv,TGL_hwv,TGL_ta,TGL_gamma,   &
   TGL_lw,TGL_zcld,TGL_tbup,TGL_tbdn,        &
   TGL_tauatm                               )

   !-----------------------------------------------------------------
   ! Purpose: TBD
   ! Input  : TGL_p0,TGL_wv,TGL_hwv,TGL_ta,TGL_gamma,TGL_lw,TGL_zcld 
   !          TGL_theta (somtime theta is a variable)
   ! Output : TGL_tbup,TGL_tbdn,TGL_tauatm,tbup,tbdn,tauatm
   ! -----------------------------------------------------------------

   implicit none

   integer,intent(in)  :: ifreq
   real   ,intent(in)  :: theta,p0,wv,hwv,ta,gamma,lw,zcld
   real   ,intent(in)  :: TGL_p0,TGL_wv,TGL_hwv,TGL_ta,TGL_gamma,TGL_lw,TGL_zcld,TGL_theta
   real   ,intent(out) :: tbup,tbdn,tauatm,TGL_tbup,TGL_tbdn, TGL_tauatm

   real :: mu,hdn,hup,hdninf,hupinf,TGL_mu

   real :: b1(4),b2(4),b3(4)
   real :: c(4),d1(4),d2(4),d3(4),zeta(4),kw0(4),kw1(4),kw2(4),kw3(4)
   real :: tau,tau1,tau2,taucld
   real :: tcld,tc,em,em1
   real :: sigv,sigo,sig,sig1,sigcld
   real :: teff1dn,teff1up,teffdn,teffup
   real :: tbcld,tbclrdn,tbclrup,tb1dn,tb1up,tb2dn,tb2up
   real :: otbar,tc2,tc3,hv,ho,alph
   real :: TGL_sigv,TGL_otbar,TGL_sigo,TGL_tcld,TGL_tc,TGL_tc2,TGL_tc3
   real :: TGL_sigcld,TGL_taucld,TGL_tbcld,TGL_hv,TGL_ho
   real :: TGL_hdn,TGL_hup,TGL_hdninf,TGL_sig,TGL_sig1,TGL_tau,TGL_tau1
   real :: TGL_tau2,TGL_em1,TGL_teff1dn,TGL_hupinf,TGL_em,TGL_teff1up
   real :: TGL_teffdn,TGL_teffup,TGL_tbclrdn,TGL_tbclrup,TGL_tb1dn,TGL_tb1up
   real :: TGL_tb2dn,TGL_tb2up,TGL_alph

   data b1/-.46847e-1,-.57752e-1,-.18885,.10990/
   data b2/.26640e-4,.31662e-4,.9832e-4,.60531e-4/
   data b3/.87560e+1,.10961e+2,.36678e+2,-.37578e+2/
   data c/ .9207,   1.208,     .8253,     .8203/
   data zeta/4.2,4.2,4.2,2.9/
   data d1/-.35908e+1,-.38921e+1,-.43072e+1,-.17020e+0/
   data d2/ .29797e-1, .31054e-1, .32801e-1, .13610e-1/
   data d3/-.23174e-1,-.23543e-1,-.24101e-1,-.15776e+0/
   data kw0/ .786e-1, .103,    .267,    .988/
   data kw1/-.230e-2,-.296e-2,-.673e-2,-.107e-1/
   data kw2/ .448e-4, .557e-4, .975e-4,-.535e-4/
   data kw3/-.464e-6,-.558e-6,-.724e-6, .115e-5/

   if (trace_use) call da_trace_entry("da_tbatmos_tl")

   ! mu = secant(theta)
   ! somtime theta is a variable

   mu     = 1.0/cos(theta*0.0174533)
   TGL_mu = mu*mu*0.0174533*TGL_theta*sin(theta*0.0174533)

   ! get water vapor optical depth

   call da_sigma_v_tl(ifreq,p0,wv,hwv,ta,gamma,sigv,   &
                        TGL_p0,TGL_wv,TGL_hwv,TGL_ta,    &
                        TGL_gamma,TGL_sigv              )

   ! otbar = one over "mean" temperature

       otbar =   1.0/(ta - gamma*zeta(ifreq))
   TGL_otbar = - otbar*otbar*(TGL_ta - TGL_gamma*zeta(ifreq))

   ! sigo = dry air optical depth

       sigo = b1(ifreq) + b2(ifreq)*    p0  + b3(ifreq)*    otbar
   TGL_sigo =             b2(ifreq)*TGL_p0  + b3(ifreq)*TGL_otbar

   ! cloud parameters

       tcld   =     ta - gamma*zcld
   TGL_tcld   = TGL_ta - TGL_gamma*zcld - gamma*TGL_zcld
         tc   =     tcld - t_kelvin
     TGL_tc   = TGL_tcld
        tc2   = tc*tc
    TGL_tc2   = 2.0*tc*TGL_tc
        tc3   = tc2*tc
    TGL_tc3   = TGL_tc2*tc + tc2*TGL_tc
       sigcld =  (kw0(ifreq) + tc*kw1(ifreq) + tc2*kw2(ifreq) +  &
                   tc3*kw3(ifreq))*lw
   TGL_sigcld =  (TGL_tc *kw1(ifreq) + TGL_tc2*kw2(ifreq) +  &
                   TGL_tc3*kw3(ifreq))*lw &
               + (kw0(ifreq) + tc*kw1(ifreq) + tc2*kw2(ifreq) +  &
                   tc3*kw3(ifreq))*TGL_lw

       taucld =   exp(-mu*sigcld)
   TGL_taucld = - (TGL_mu*sigcld + mu*TGL_sigcld)*taucld
        tbcld =   (1.0 - taucld)*tcld
    TGL_tbcld = - TGL_taucld*tcld + (1.0 - taucld)*TGL_tcld

   ! hv, ho = effective absorber scale heights for vapor, dry air

       hv = c(ifreq)*    hwv
   TGL_hv = c(ifreq)*TGL_hwv
       ho = d1(ifreq) + d2(ifreq)*    ta + d3(ifreq)*    gamma
   TGL_ho =             d2(ifreq)*TGL_ta + d3(ifreq)*TGL_gamma

   ! get effective emission heights for layer 1 and total atmosphere

   call da_effht_tl(ho,hv,sigo,sigv,mu,zcld,hdn,hup,        &
                  hdninf,hupinf,                          &
                  TGL_ho,TGL_hv,TGL_sigo,TGL_sigv,TGL_mu, &
                  TGL_zcld,TGL_hdn,TGL_hup,TGL_hdninf,    &
                  TGL_hupinf                             )

   ! atmospheric transmittances in layer one and two, and combined

        sig =     sigo +     sigv

    TGL_sig = TGL_sigo + TGL_sigv
       sig1 = sigo*(1.0-exp(-zcld/ho)) + sigv*(1.0-exp(-zcld/hv))
   TGL_sig1 =   TGL_sigo*(1.0-exp(-zcld/ho))  &
              + TGL_sigv*(1.0-exp(-zcld/hv))  &
              + sigo*(TGL_zcld/ho - zcld*TGL_ho/(ho*ho))*exp(-zcld/ho) &
              + sigv*(TGL_zcld/hv - zcld*TGL_hv/(hv*hv))*exp(-zcld/hv)
       tau  =  exp(-mu*sig)
   TGL_tau  = -(TGL_mu*sig + mu*TGL_sig) * tau
       tau1 =  exp(-mu*sig1)
   TGL_tau1 = -(mu*TGL_sig1 + TGL_mu*sig1) *tau1
       tau2 =  tau/tau1
   TGL_tau2 =  TGL_tau/tau1 - tau2*TGL_tau1/tau1

   ! atmospheric "emissivity"

       em1  =   1.0 - tau1
   TGL_em1  = - TGL_tau1
       em   =   1.0 - tau
   TGL_em   = - TGL_tau

   ! downwelling and upwelling brightness temperature for each layer

       teff1dn =     ta - gamma*hdn
   TGL_teff1dn = TGL_ta - TGL_gamma*hdn - gamma*TGL_hdn
       teff1up =     ta - gamma*hup
   TGL_teff1up = TGL_ta - TGL_gamma*hup - gamma*TGL_hup
        teffdn =     ta - gamma*hdninf
    TGL_teffdn = TGL_ta - TGL_gamma*hdninf - gamma*TGL_hdninf
        teffup =     ta - gamma*hupinf
    TGL_teffup = TGL_ta - TGL_gamma*hupinf - gamma*TGL_hupinf
       tbclrdn = teffdn*em
   TGL_tbclrdn = TGL_teffdn*em + teffdn*TGL_em
       tbclrup = teffup*em
   TGL_tbclrup = TGL_teffup*em + teffup*TGL_em

        tb1dn = em1*teff1dn
   TGL_tb1dn = TGL_em1*teff1dn + em1*TGL_teff1dn
        tb1up = em1*teff1up
   TGL_tb1up = TGL_em1*teff1up + em1*TGL_teff1up
        tb2dn = (tbclrdn - tb1dn)/tau1
   TGL_tb2dn = (TGL_tbclrdn - TGL_tb1dn)/tau1 - tb2dn*TGL_tau1/tau1
        tb2up =      tbclrup - tau2*tb1up
   TGL_tb2up =  TGL_tbclrup - TGL_tau2*tb1up - tau2*TGL_tb1up

   ! total downwelling and upwelling brightness temperature and transmittance

       tbdn  =     tb1dn + tau1*(tbcld + taucld*tb2dn)
   TGL_tbdn  = TGL_tb1dn + TGL_tau1*(tbcld + taucld*tb2dn) &
                         + tau1*(TGL_tbcld + TGL_taucld*tb2dn + taucld*TGL_tb2dn)
       tbup  =     tb2up + tau2*(tbcld + taucld*tb1up)
   TGL_tbup  = TGL_tb2up + TGL_tau2*(tbcld + taucld*tb1up) &
                         + tau2*(TGL_tbcld + TGL_taucld*tb1up + taucld*TGL_tb1up)
       tauatm = tau*taucld
   TGL_tauatm = TGL_tau*taucld + tau*TGL_taucld
   !
   ! the following lines apply an ad hoc correction to improve fit 
   ! at large angles and/or high gaseous opacities 
   !  (downwelling brightness temperatures only)

   alph = (0.636619*atan(mu*sig))**2
   if (abs(sig) .gt. 0.0) then
      TGL_alph = 2.0*0.636619*0.636619* &
                 (TGL_mu*sig + mu*TGL_sig)*atan(mu*sig)/(1.0+mu*mu*sig*sig)
   else
      TGL_alph = 0.0
   end if
   TGL_tbdn = - TGL_alph*tbdn + (1.0-alph)*TGL_tbdn &
              + TGL_em*alph*ta + em*TGL_alph*ta + em*alph*TGL_ta 
   tbdn = (1.0-alph)*tbdn + em*alph*ta

   if (trace_use) call da_trace_exit("da_tbatmos_tl")

end subroutine da_tbatmos_tl


