subroutine da_tb_adj(ifreq,theta,p0,ta,gamma,sst,wv,hwv,u,alw,zcld,            &
!                  tbv,tbh,                                                  &
                  ADJ_p0,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_wv,                   &
                  ADJ_hwv,ADJ_u,ADJ_alw,ADJ_zcld,ADJ_tbv,ADJ_tbh            )

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! Output : ADJ_p0,  ADJ_ta,   ADJ_gamma, ADJ_sst, ADJ_wv, ADJ_hwv, ADJ_u
   !          ADJ_alw, ADJ_zcld
   ! Input  : ADJ_tbv, ADJ_tbh,  tbv,  tbh
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in   ) :: ifreq
   real   , intent(in   ) :: theta,p0,ta,gamma,sst,wv,hwv,u,alw,zcld
   real   , intent(inout) :: ADJ_p0,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_wv,     &
                             ADJ_hwv,ADJ_u,ADJ_alw,ADJ_zcld
   real   , intent(in   ) :: ADJ_tbv,ADJ_tbh 
!   real   , intent(in   ) :: tbv,tbh

   real :: freq(4),ebiasv(4),ebiash(4),cf1(4),cf2(4),cg(4)

   real :: f,costhet,gx2,tbup,tbdn,tauatm,sigma,remv,remh
   real :: effangv,effangh,tbdnv,foam,foam_save,emissv,emissh 
   real :: dum
   real :: refv,refh,semv,semh,scatv,scath,tbdnh
   real :: ADJ_gx2,ADJ_tbup,ADJ_tbdn,ADJ_tauatm,ADJ_sigma
   real :: ADJ_tremv,ADJ_remh,ADJ_effangv,ADJ_effangh
   real :: ADJ_tbdnh,ADJ_dum,ADJ_foam,ADJ_emissv
   real :: ADJ_emissh,ADJ_refv,ADJ_refh,ADJ_semv
   real :: ADJ_semh,ADJ_scatv,ADJ_scath,ADJ_remv,ADJ_tbdnv
   real :: ADJ_theta

   real :: fem

   data freq/19.35,22.235,37.0,85.5/

   ! empirical bias corrections for surface emissivity

   data ebiasv/0.0095, 0.005,-0.014, -0.0010/
   data ebiash/0.004,   0.0,-0.023, 0.043/


   data cf1/0.0015,0.004,0.0027,0.005/
   data cg/4.50e-3, 5.200e-3, 5.5e-3, 7.2e-3 /

   data cf2/0.0023,0.000,0.0002,-0.006/

   ! 'foam' emissivity
   data fem /1.0/

   if (trace_use) call da_trace_entry("da_tb_adj")

   f=0.0;costhet=0.0;gx2=0.0;tbup=0.0;tbdn=0.0;tauatm=0.0
   sigma=0.0;remv=0.0;remh=0.0;effangv=0.0;effangh=0.0
   tbdnv=0.0;foam=0.0;foam_save=0.0;emissv=0.0;emissh=0.0
   dum=0.0;refv=0.0;refh=0.0;semv=0.0;semh=0.0;scatv=0.0
   scath=0.0;tbdnh=0.0;ADJ_gx2=0.0;ADJ_tbup=0.0;ADJ_tbdn=0.0
   ADJ_tauatm=0.0;ADJ_sigma=0.0;ADJ_tremv=0.0;ADJ_remh=0.0
   ADJ_effangv=0.0;ADJ_effangh=0.0;ADJ_tbdnh=0.0;ADJ_dum=0.0
   ADJ_foam=0.0;ADJ_emissv=0.0;ADJ_emissh=0.0;ADJ_refv=0.0
   ADJ_refh=0.0;ADJ_semv=0.0;ADJ_semh=0.0;ADJ_scatv=0.0
   ADJ_scath=0.0;ADJ_remv=0.0;ADJ_tbdnv=0.0
   ADJ_theta =0.0


   ! write (unit=stdout,fmt=*) 'ifreq',ifreq,theta,p0,ta,gamma,sst,wv,hwv,u,alw,zcld,          &
   !              tbv,tbh,                                                  &
   !              ADJ_p0,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_wv,                   &
   !               ADJ_hwv,ADJ_u,ADJ_alw,ADJ_zcld,ADJ_tbv,ADJ_tbh            

   f = freq(ifreq)
   costhet = cos(theta*0.017453)

   ! effective surface slope variance

   gx2 = cg(ifreq)*    u

   ! get upwelling atmospheric brightness temperature

   call tbatmos(ifreq,theta,p0,wv,hwv,ta,gamma,alw,zcld, tbup,tbdn,tauatm)

   ! convert transmittance to optical depth

   sigma = -alog(tauatm)*costhet

   ! get rough surface emissivity

   call roughem(ifreq,gx2,sst,theta,remv,remh)

   ! get effective zenith angles for scattered radiation at surface

   call effang(ifreq,theta,gx2,sigma,effangv,effangh)

   ! get effective sky brightness temperatures for scattered radiation

   call tbatmos(ifreq,effangv,p0,wv,hwv,ta,gamma,alw,zcld, dum,tbdnv,dum)

   call tbatmos(ifreq,effangh,p0,wv,hwv,ta,gamma,alw,zcld, dum,tbdnh,dum)

   ! compute 'foam' coverage

   foam = cf1(ifreq)*    u

   if (u .gt. 5.0) then
      foam_save = foam
      foam =     foam + cf2(ifreq)*(   u-5.0)
   end if

   ! compute surface emissivities and reflectivity

   emissv =     foam*fem + (1.0 - foam)*(remv + ebiasv(ifreq))
   emissh =     foam*fem + (1.0 - foam)*(remh + ebiash(ifreq))
   refv =   1.0 - emissv
   refh =   1.0 - emissh

   ! compute surface emission term

   semv = sst*emissv
   semh = sst*emissh

   ! compute surface scattering term

   scatv = refv*tbdnv
   scath = refh*tbdnh

   ! combine to get space-observed brightness temperature

   ! tbv =     tbup + tauatm*(semv + scatv)
   ! tbh =     tbup + tauatm*(semh + scath)


   ! start
   ! write (unit=stdout,fmt=*) 'ifreq 1',ADJ_p0,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_wv,  &
   !                 ADJ_hwv,ADJ_u,ADJ_alw,ADJ_zcld,ADJ_tbv,ADJ_tbh            


   ADJ_tbup   = ADJ_tbh                    !!! first
   ADJ_tauatm = ADJ_tbh*(semh + scath)     !!! first
   ADJ_semh   = tauatm*ADJ_tbh             !!! first
   ADJ_scath  = tauatm*ADJ_tbh             !!! first

   ADJ_tbup   = ADJ_tbv                  + ADJ_tbup
   ADJ_tauatm = ADJ_tbv*(semv + scatv)   + ADJ_tauatm
   ADJ_semv   = tauatm*ADJ_tbv             !!! first
   ADJ_scatv  = tauatm*ADJ_tbv             !!! first

   ADJ_refh   = ADJ_scath*tbdnh            !!! first
   ADJ_tbdnh  = refh*ADJ_scath             !!! first
   ADJ_refv   = ADJ_scatv*tbdnv            !!! first
   ADJ_tbdnv  = refv*ADJ_scatv             !!! first
   ADJ_sst    = ADJ_semh*emissh          + ADJ_sst
   ADJ_emissh = sst*ADJ_semh               !!! first
   ADJ_sst    = ADJ_semv*emissv          + ADJ_sst
   ADJ_emissv = sst*ADJ_semv               !!! first

   ADJ_emissh = - ADJ_refh               + ADJ_emissh
   ADJ_emissv = - ADJ_refv               + ADJ_emissv

   ADJ_foam   =   ADJ_emissh*fem                      !!! first
   ADJ_foam   = - ADJ_emissh*(remh + ebiash(ifreq)) + ADJ_foam
   ADJ_remh   =   (1.0 - foam)*ADJ_emissh             !!! first
   ADJ_foam   =   ADJ_emissv*fem                    + ADJ_foam
   ADJ_foam   = - ADJ_emissv*(remv + ebiasv(ifreq)) + ADJ_foam
   ADJ_remv   =   (1.0 - foam)*ADJ_emissv             !!! first

   if (u .gt. 5.0) then
     ADJ_u = cf2(ifreq)*ADJ_foam  + ADJ_u
     foam=foam_save
   end if
   ADJ_u = cf1(ifreq)*ADJ_foam    + ADJ_u
   
   ADJ_dum = 0.0
   dum     = 0.0
   ADJ_effangh = 0.0
   call da_tbatmos_adj(ifreq,effangh,p0,wv,hwv,ta,gamma,alw,    &
                       zcld,dum,tbdnh,dum,                      &
                       ADJ_effangh,ADJ_p0,ADJ_wv,ADJ_hwv,       &
                       ADJ_ta,ADJ_gamma,ADJ_alw,ADJ_zcld,       &
                       ADJ_dum,ADJ_tbdnh,ADJ_dum                )
   dum     = 0.0
   ADJ_dum = 0.0

   ! write (unit=stdout,fmt=*) 'ifreq 2',ADJ_p0,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_wv,  &
   !                 ADJ_hwv,ADJ_u,ADJ_alw,ADJ_zcld,ADJ_tbv,ADJ_tbh            

   ADJ_effangv = 0.0
   call da_tbatmos_adj(ifreq,effangv,p0,wv,hwv,ta,gamma,alw,    &
                       zcld,dum,tbdnv,dum,                      &
                       ADJ_effangv,ADJ_p0,ADJ_wv,ADJ_hwv,       &
                       ADJ_ta,ADJ_gamma,ADJ_alw,ADJ_zcld,       & 
                       ADJ_dum,ADJ_tbdnv,ADJ_dum                )

   ADJ_gx2=0.0
   ADJ_sigma=0.0
   ! write (unit=stdout,fmt=*) 'ifreq 3',ADJ_p0,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_wv,  &
   !                 ADJ_hwv,ADJ_u,ADJ_alw,ADJ_zcld,ADJ_tbv,ADJ_tbh            

   call da_effang_adj(ifreq,theta,gx2,sigma,effangv,effangh,    &
                      ADJ_gx2,ADJ_sigma,ADJ_effangv,ADJ_effangh )

   call da_roughem_adj(ifreq,gx2,sst,theta,remv,remh,         &
                       ADJ_gx2,ADJ_sst,ADJ_remv,ADJ_remh      )

   ADJ_tauatm = - costhet*ADJ_sigma/tauatm + ADJ_tauatm


   ! write (unit=stdout,fmt=*) 'ifreq 4',ADJ_p0,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_wv,  &
   !              ADJ_hwv,ADJ_u,ADJ_alw,ADJ_zcld,ADJ_tbv,ADJ_tbh            

   call da_tbatmos_adj(ifreq,theta,p0,wv,hwv,ta,gamma,alw,zcld, &
                       tbup,tbdn,tauatm,                        &
                       ADJ_theta,ADJ_p0,ADJ_wv,ADJ_hwv,ADJ_ta,ADJ_gamma,  &
                       ADJ_alw,ADJ_zcld,ADJ_tbup,ADJ_tbdn,      &
                       ADJ_tauatm                               )

   ADJ_theta=0.0   ! first

   ADJ_u = cg(ifreq)*ADJ_gx2 + ADJ_u

   ! write (unit=stdout,fmt=*) 'ifreq 5',ADJ_p0,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_wv,  &
   !              ADJ_hwv,ADJ_u,ADJ_alw,ADJ_zcld,ADJ_tbv,ADJ_tbh            
   ! end

   if (trace_use) call da_trace_exit("da_tb_adj")

end subroutine da_tb_adj


