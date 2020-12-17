subroutine da_tb_tl(ifreq,theta,p0,ta,gamma,sst,wv,hwv,u,alw,zcld,            &
!  tbv,tbh,                                                  &
  TGL_p0,TGL_ta,TGL_gamma,TGL_sst,TGL_wv,                   &
  TGL_hwv,TGL_u,TGL_alw,TGL_zcld,TGL_tbv,TGL_tbh           )\

   !---------------------------------------------------------------------------
   ! Purpose: TBD
   ! Input  : TGL_p0,  TGL_ta,   TGL_gamma, TGL_sst, TGL_wv, TGL_hwv, TGL_u
   !          TGL_alw, TGL_zcld
   ! Output : TGL_tbv, TGL_tbh,  tbv,  tbh
   ! ---------------------------------------------------------------------------

   implicit none

   integer, intent(in) :: ifreq
   real,    intent(in) :: theta,p0,ta,gamma,sst,wv,hwv,u,alw,zcld
   real,    intent(in) :: TGL_p0,TGL_ta,TGL_gamma,TGL_sst,TGL_wv, TGL_hwv,TGL_u,TGL_alw,TGL_zcld
!   real   , intent(in) :: tbv,tbh
   real,    intent(out) :: TGL_tbv,TGL_tbh 

   real :: freq(4),ebiasv(4),ebiash(4),cf1(4),cf2(4),cg(4)

   real :: f,costhet,gx2,tbup,tbdn,tauatm,sigma,remv,remh
   real :: effangv,effangh,tbdnv,dum,foam,emissv,emissh 
   real :: refv,refh,semv,semh,scatv,scath,tbdnh
   real :: TGL_gx2,TGL_tbup,TGL_tbdn,TGL_tauatm,TGL_sigma
   real :: TGL_remh,TGL_effangv,TGL_effangh
   ! real :: TGL_tremv
   real :: TGL_tbdnh,TGL_dum,TGL_foam,TGL_emissv
   real :: TGL_emissh,TGL_refv,TGL_refh,TGL_semv
   real :: TGL_semh,TGL_scatv,TGL_scath,TGL_remv,TGL_tbdnv
   real :: TGL_theta

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

   if (trace_use) call da_trace_entry("da_tb_tl")

   f = freq(ifreq)
   costhet = cos(theta*0.017453)

   ! effective surface slope variance

   gx2 = cg(ifreq)*    u
   TGL_gx2 = cg(ifreq)*TGL_u

   ! get upwelling atmospheric brightness temperature

   TGL_theta=0.0
   call da_tbatmos_tl(ifreq,theta,p0,wv,hwv,ta,gamma,alw,zcld, &
                       tbup,tbdn,tauatm,                        &
                       TGL_theta,TGL_p0,TGL_wv,TGL_hwv,TGL_ta,TGL_gamma,  &
                       TGL_alw,TGL_zcld,TGL_tbup,TGL_tbdn,      &
                       TGL_tauatm                              )

   ! convert transmittance to optical depth

   sigma = -alog(tauatm)*costhet
   TGL_sigma = -costhet*TGL_tauatm/tauatm

   ! get rough surface emissivity

   call da_roughem_tl(ifreq,gx2,sst,theta,remv,remh,         &
                       TGL_gx2,TGL_sst,TGL_remv,TGL_remh     )

   ! get effective zenith angles for scattered radiation at surface

   call da_effang_tl(ifreq,theta,gx2,sigma,effangv,effangh,    &
                      TGL_gx2,TGL_sigma,TGL_effangv,TGL_effangh)

   ! get effective sky brightness temperatures for scattered radiation

   call da_tbatmos_tl(ifreq,effangv,p0,wv,hwv,ta,gamma,alw,    &
                       zcld,dum,tbdnv,dum,                      &
                       TGL_effangv,TGL_p0,TGL_wv,TGL_hwv,       &
                       TGL_ta,TGL_gamma,TGL_alw,TGL_zcld,       & 
                       TGL_dum,TGL_tbdnv,TGL_dum               )

   call da_tbatmos_tl(ifreq,effangh,p0,wv,hwv,ta,gamma,alw,    &
                       zcld,dum,tbdnh,dum,                      &
                       TGL_effangh,TGL_p0,TGL_wv,TGL_hwv,       &
                       TGL_ta,TGL_gamma,TGL_alw,TGL_zcld,       &
                       TGL_dum,TGL_tbdnh,TGL_dum               )

   ! compute 'foam' coverage

   foam = cf1(ifreq)*    u
   TGL_foam = cf1(ifreq)*TGL_u

   if (u .gt. 5.0) then
      TGL_foam = TGL_foam + cf2(ifreq)*TGL_u
      foam =     foam + cf2(ifreq)*(  u-5.0)
   end if

   ! compute surface emissivities and reflectivity

   emissv     =     foam*fem + (1.0 - foam)*(remv + ebiasv(ifreq))
   TGL_emissv = TGL_foam*fem - TGL_foam*(remv + ebiasv(ifreq)) &
                                + (1.0 - foam)*TGL_remv
   emissh     =     foam*fem + (1.0 - foam)*(remh + ebiash(ifreq))
   TGL_emissh = TGL_foam*fem - TGL_foam*(remh + ebiash(ifreq)) &
                                + (1.0 - foam)*TGL_remh
   refv     =   1.0 - emissv
   TGL_refv = - TGL_emissv
   refh     =   1.0 - emissh
   TGL_refh = - TGL_emissh

   ! compute surface emission term

   semv     = sst*emissv
   TGL_semv = TGL_sst*emissv + sst*TGL_emissv
   semh     = sst*emissh
   TGL_semh = TGL_sst*emissh + sst*TGL_emissh

   ! compute surface scattering term

   scatv     = refv*tbdnv
   TGL_scatv = TGL_refv*tbdnv + refv*TGL_tbdnv
   scath     = refh*tbdnh
   TGL_scath = TGL_refh*tbdnh + refh*TGL_tbdnh

   ! combine to get space-observed brightness temperature

   ! tbv     =     tbup + tauatm*(semv + scatv)
   TGL_tbv = TGL_tbup + TGL_tauatm*(semv + scatv)   &
                         + tauatm*(TGL_semv + TGL_scatv)
   ! tbh     =     tbup + tauatm*(semh + scath)
   TGL_tbh = TGL_tbup + TGL_tauatm*(semh + scath)   &
                         + tauatm*(TGL_semh + TGL_scath)

   if (trace_use) call da_trace_exit("da_tb_tl")

end subroutine da_tb_tl


