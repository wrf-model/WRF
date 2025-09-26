subroutine da_tbatmos_adj(ifreq,theta,p0,wv,hwv,ta,gamma,lw,zcld,   &
   tbup,tbdn,tauatm, ADJ_theta,ADJ_p0,ADJ_wv,ADJ_hwv,ADJ_ta,ADJ_gamma,   &
   ADJ_lw,ADJ_zcld,ADJ_tbup,ADJ_tbdn, ADJ_tauatm)

   implicit none

   !-----------------------------------------------------------------
   ! Purpose: TBD
   ! Output : ADJ_p0,ADJ_wv,ADJ_hwv,ADJ_ta,ADJ_gamma,ADJ_lw,ADJ_zcld 
   !          ADJ_theta (somtime theta is a variable)
   ! Input  : ADJ_tbup,ADJ_tbdn,ADJ_tauatm
   ! Output mean fields : tbup,tbdn,tauatm
   !-----------------------------------------------------------------

   integer, intent(in)    :: ifreq
   real,    intent(in)    :: theta,p0,wv,hwv,ta,gamma,lw,zcld
   real,    intent(inout) :: ADJ_p0,ADJ_wv,ADJ_hwv,ADJ_ta, ADJ_gamma,ADJ_lw,ADJ_zcld,ADJ_theta
   real,    intent(inout) :: ADJ_tbup,ADJ_tbdn,ADJ_tauatm
   real,    intent(out)   :: tbup,tbdn,tauatm

   real :: tbdn_save

   real :: mu,hdn,hup,hdninf,hupinf,ADJ_mu

   real :: b1(4),b2(4),b3(4)
   real :: c(4),d1(4),d2(4),d3(4),zeta(4),kw0(4),kw1(4),kw2(4),kw3(4)
   real :: tau,tau1,tau2,taucld
   real :: tcld,tc,em,em1
   real :: sigv,sigo,sig,sig1,sigcld
   real :: teff1dn,teff1up,teffdn,teffup
   real :: tbcld,tbclrdn,tbclrup,tb1dn,tb1up,tb2dn,tb2up
   real :: otbar,tc2,tc3,hv,ho,alph
   real :: ADJ_sigv,ADJ_otbar,ADJ_sigo,ADJ_tcld,ADJ_tc,ADJ_tc2,ADJ_tc3
   real :: ADJ_sigcld,ADJ_taucld,ADJ_tbcld,ADJ_hv,ADJ_ho
   real :: ADJ_hdn,ADJ_hup,ADJ_hdninf,ADJ_sig,ADJ_sig1,ADJ_tau,ADJ_tau1
   real :: ADJ_tau2,ADJ_em1,ADJ_teff1dn,ADJ_hupinf,ADJ_em,ADJ_teff1up
   real :: ADJ_teffdn,ADJ_teffup,ADJ_tbclrdn,ADJ_tbclrup,ADJ_tb1dn,ADJ_tb1up
   real :: ADJ_tb2dn,ADJ_tb2up,ADJ_alph

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

   if (trace_use) call da_trace_entry("da_tbatmos_adj")

   mu=0.0;hdn=0.0;hup=0.0;hdninf=0.0;hupinf=0.0;ADJ_mu=0.0
   tcld=0.0;tc=0.0;em=0.0;em1=0.0
   sigv=0.0;sigo=0.0;sig=0.0;sig1=0.0;sigcld=0.0
   teff1dn=0.0;teff1up=0.0;teffdn=0.0;teffup=0.0
   tbcld=0.0;tbclrdn=0.0;tbclrup=0.0;tb1dn=0.0;tb1up=0.0;tb2dn=0.0;tb2up=0.0
   otbar=0.0;tc2=0.0;tc3=0.0;hv=0.0;ho=0.0;alph=0.0
   ADJ_sigv=0.0;ADJ_otbar=0.0;ADJ_sigo=0.0;ADJ_tcld=0.0;
   ADJ_tc=0.0;ADJ_tc2=0.0;ADJ_tc3=0.0
   ADJ_sigcld=0.0;ADJ_taucld=0.0;ADJ_tbcld=0.0;ADJ_hv=0.0;ADJ_ho=0.0
   ADJ_hdn=0.0;ADJ_hup=0.0;ADJ_hdninf=0.0;ADJ_sig=0.0;ADJ_sig1=0.0
   ADJ_tau=0.0;ADJ_tau1=0.0
   ADJ_tau2=0.0;ADJ_em1=0.0;ADJ_teff1dn=0.0;ADJ_hupinf=0.0;ADJ_em=0.0
   ADJ_teff1up=0.0;ADJ_teffdn=0;ADJ_teffup=0.0;ADJ_tbclrdn=0.0
   ADJ_tbclrup=0.0;ADJ_tb1dn=0.0;ADJ_tb1up=0.0
   ADJ_tb2dn=0.0;ADJ_tb2up=0.0;ADJ_alph=0.0
   tau=0.0;tau1=0.0;tau2=0.0;taucld=0.0
   tcld=0.0;tc=0.0;em=0.0;em1=0.0
   sigv=0.0;sigo=0.0;sig=0.0;sig1=0.0;sigcld=0.0
   teff1dn=0.0;teff1up=0.0;teffdn=0.0;teffup=0.0


   ! mu = secant(theta)
   ! somtime theta is a variable

   mu     = 1.0/cos(theta*0.0174533)

   ! get water vapor optical depth

   call cal_sigma_v(ifreq,p0,wv,hwv,ta,gamma,sigv)

   ! otbar = one over "mean" temperature

   otbar =   1.0/(ta - gamma*zeta(ifreq))

   ! sigo = dry air optical depth

   sigo = b1(ifreq) + b2(ifreq)*    p0  + b3(ifreq)*    otbar

   ! cloud parameters

   tcld   =     ta - gamma*zcld
   tc     =     tcld - t_kelvin
   tc2    =     tc*tc
   tc3    =     tc2*tc
   sigcld =  ( kw0(ifreq) + tc*kw1(ifreq) + tc2*kw2(ifreq) +  &
               tc3*kw3(ifreq) )*lw
   taucld =   exp(-mu*sigcld)
   tbcld  =   (1.0 - taucld)*tcld

   ! hv, ho = effective absorber scale heights for vapor, dry air

   hv = c(ifreq)*   hwv
   ho = d1(ifreq) + d2(ifreq)* ta + d3(ifreq)* gamma

   ! get effective emission heights for layer 1 and total atmosphere


   call effht(ho,hv,sigo,sigv,mu,zcld,hdn,hup, hdninf,hupinf)

   ! atmospheric transmittances in layer one and two, and combined

   sig =     sigo +     sigv
   sig1 = sigo*(1.0-exp(-zcld/ho)) + sigv*(1.0-exp(-zcld/hv))
   tau  =  exp(-mu*sig)
   tau1 =  exp(-mu*sig1)
   tau2 =  tau/tau1
 
   ! atmospheric "emissivity"

   em1  =   1.0 - tau1
   em   =   1.0 - tau

   ! downwelling and upwelling brightness temperature for each layer

   teff1dn =     ta - gamma*hdn
   teff1up =     ta - gamma*hup
   teffdn =     ta - gamma*hdninf
   teffup =     ta - gamma*hupinf
   tbclrdn = teffdn*em
   tbclrup = teffup*em

   tb1dn = em1*teff1dn
   tb1up = em1*teff1up
   tb2dn = (tbclrdn - tb1dn)/tau1
   tb2up =      tbclrup - tau2*tb1up

   ! total downwelling and upwelling brightness temperature and transmittance

   tbdn  =     tb1dn + tau1*(tbcld + taucld*tb2dn)
   tbup  =     tb2up + tau2*(tbcld + taucld*tb1up)
   tauatm = tau*taucld

   ! the following lines apply an ad hoc correction to improve fit 
   ! at large angles and/or high gaseous opacities 
   !  (downwelling brightness temperatures only)

   alph = (0.636619*atan(mu*sig))**2
   tbdn_save = tbdn
   tbdn = (1.0-alph)*tbdn + em*alph*ta


   ! start

   tbdn = tbdn_save

   ADJ_alph = - ADJ_tbdn*tbdn
   ADJ_em   = ADJ_tbdn*alph*ta
   ADJ_alph = em*ADJ_tbdn*ta + ADJ_alph
   ADJ_ta   = em*alph*ADJ_tbdn + ADJ_ta
   ADJ_tbdn = (1.0-alph)*ADJ_tbdn 

   if (abs(sig) .gt. 0.0) then
      ADJ_mu  = 2.0*0.636619*0.636619*ADJ_alph*sig*atan(mu*sig)/(1.0+mu*mu*sig*sig)
      ADJ_sig = 2.0*0.636619*0.636619*mu*ADJ_alph*atan(mu*sig)/(1.0+mu*mu*sig*sig)
   else
      ADJ_mu  = 0.0
      ADJ_sig = 0.0
   end if

   ADJ_tau    = ADJ_tauatm*taucld
   ADJ_taucld = tau*ADJ_tauatm
   ADJ_tb2up  = ADJ_tbup
   ADJ_tau2   = ADJ_tbup*(tbcld + taucld*tb1up)
   ADJ_tbcld  = tau2*ADJ_tbup
   ADJ_taucld = tau2*ADJ_tbup*tb1up  + ADJ_taucld
   ADJ_tb1up  = tau2*taucld*ADJ_tbup
   ADJ_tb1dn = ADJ_tbdn
   ADJ_tau1  = ADJ_tbdn*(tbcld + taucld*tb2dn)
   ADJ_tbcld = tau1*ADJ_tbdn   + ADJ_tbcld
   ADJ_taucld = tau1*ADJ_tbdn*tb2dn  + ADJ_taucld
   ADJ_tb2dn  = tau1*taucld*ADJ_tbdn

   ADJ_tbclrup =   ADJ_tb2up
   ADJ_tau2    = - ADJ_tb2up*tb1up + ADJ_tau2
   ADJ_tb1up   = - tau2*ADJ_tb2up  + ADJ_tb1up

   ADJ_tbclrdn =   ADJ_tb2dn/tau1
   ADJ_tb1dn   = - ADJ_tb2dn/tau1  + ADJ_tb1dn
   ADJ_tau1    = - tb2dn*ADJ_tb2dn/tau1 + ADJ_tau1

   ADJ_em1     = ADJ_tb1up*teff1up
   ADJ_teff1up = em1*ADJ_tb1up

   ADJ_em1     = ADJ_tb1dn*teff1dn + ADJ_em1
   ADJ_teff1dn = em1*ADJ_tb1dn

   ADJ_teffup  = ADJ_tbclrup*em
   ADJ_em      = teffup*ADJ_tbclrup + ADJ_em

   ADJ_teffdn  = ADJ_tbclrdn*em
   ADJ_em      = teffdn*ADJ_tbclrdn + ADJ_em

   ADJ_ta      =   ADJ_teffup + ADJ_ta
   ADJ_gamma   = - ADJ_teffup*hupinf + ADJ_gamma
   ADJ_hupinf  = - gamma*ADJ_teffup

   ADJ_ta      =   ADJ_teffdn + ADJ_ta
   ADJ_gamma   = - ADJ_teffdn*hdninf + ADJ_gamma
   ADJ_hdninf  = - gamma*ADJ_teffdn

   ADJ_ta    =   ADJ_teff1up + ADJ_ta
   ADJ_gamma = - ADJ_teff1up*hup + ADJ_gamma
   ADJ_hup   = - gamma*ADJ_teff1up

   ADJ_ta    =   ADJ_teff1dn + ADJ_ta
   ADJ_gamma = - ADJ_teff1dn*hdn + ADJ_gamma
   ADJ_hdn   = - gamma*ADJ_teff1dn

   ADJ_tau  = - ADJ_em + ADJ_tau

   ADJ_tau1 = - ADJ_em1 + ADJ_tau1

   ADJ_tau  =   ADJ_tau2/tau1 + ADJ_tau
   ADJ_tau1 = - tau2*ADJ_tau2/tau1 + ADJ_tau1

   ADJ_sig1 = - mu*ADJ_tau1*tau1
   ADJ_mu   = - ADJ_tau1*sig1*tau1 + ADJ_mu
   
   ADJ_mu   = - ADJ_tau*sig*tau + ADJ_mu
   ADJ_sig  = - mu*ADJ_tau*tau + ADJ_sig
 
   ADJ_sigo = ADJ_sig1*(1.0-exp(-zcld/ho))
   ADJ_sigv = ADJ_sig1*(1.0-exp(-zcld/hv)) 
   ADJ_zcld = sigo*ADJ_sig1/ho*exp(-zcld/ho) + ADJ_zcld
   ADJ_ho   = - sigo*zcld*ADJ_sig1/(ho*ho)*exp(-zcld/ho)
   ADJ_zcld = sigv*ADJ_sig1/hv*exp(-zcld/hv) + ADJ_zcld
   ADJ_hv   = - sigv*zcld*ADJ_sig1/(hv*hv)*exp(-zcld/hv)

   ADJ_sigo = ADJ_sig + ADJ_sigo
   ADJ_sigv = ADJ_sig + ADJ_sigv

   call da_effht_adj(ho,hv,sigo,sigv,mu,zcld,hdn,hup,        &
                  hdninf,hupinf,                          &
                  ADJ_ho,ADJ_hv,ADJ_sigo,ADJ_sigv,ADJ_mu, &
                  ADJ_zcld,ADJ_hdn,ADJ_hup,ADJ_hdninf,    &
                  ADJ_hupinf                              )

   ADJ_ta    = d2(ifreq)*ADJ_ho + ADJ_ta
   ADJ_gamma = d3(ifreq)*ADJ_ho + ADJ_gamma
   ADJ_hwv   = c(ifreq)*ADJ_hv  + ADJ_hwv

   ADJ_taucld = - ADJ_tbcld*tcld + ADJ_taucld
   ADJ_tcld = (1.0 - taucld)*ADJ_tbcld

   ADJ_mu     = - ADJ_taucld*sigcld*taucld + ADJ_mu
   ADJ_sigcld = - mu*ADJ_taucld*taucld

   ADJ_tc  = ADJ_sigcld*kw1(ifreq)*lw
   ADJ_tc2 = ADJ_sigcld*kw2(ifreq)*lw
   ADJ_tc3 = ADJ_sigcld*kw3(ifreq)*lw
   ADJ_lw  = (kw0(ifreq)+tc*kw1(ifreq)+tc2*kw2(ifreq)+tc3*kw3(ifreq)) &
             *ADJ_sigcld + ADJ_lw

   ADJ_tc2  = ADJ_tc3*tc + ADJ_tc2
   ADJ_tc   = tc2*ADJ_tc3 + ADJ_tc
   ADJ_tc   = 2.0*tc*ADJ_tc2 + ADJ_tc

   ADJ_tcld = ADJ_tc + ADJ_tcld

   ADJ_ta    =   ADJ_tcld + ADJ_ta
   ADJ_gamma = - ADJ_tcld*zcld + ADJ_gamma
   ADJ_zcld  = - gamma*ADJ_tcld + ADJ_zcld

   ADJ_p0    = b2(ifreq)*ADJ_sigo + ADJ_p0
   ADJ_otbar = b3(ifreq)*ADJ_sigo

   ADJ_ta    = - otbar*otbar*ADJ_otbar + ADJ_ta
   ADJ_gamma =   otbar*otbar*ADJ_otbar*zeta(ifreq) + ADJ_gamma

   call da_sigma_v_adj(ifreq,p0,wv,hwv,ta,gamma,sigv,   &
                        ADJ_p0,ADJ_wv,ADJ_hwv,ADJ_ta,    &
                        ADJ_gamma,ADJ_sigv               )

   ADJ_theta = mu*mu*0.0174533*ADJ_mu*sin(theta*0.0174533) + ADJ_theta

   if (trace_use) call da_trace_exit("da_tbatmos_adj")

end subroutine da_tbatmos_adj


