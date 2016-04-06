subroutine da_effht_adj(ho,hv,sigo,sigv,mu,zcld,hdn,hup,hdninf,hupinf, &
   ADJ_ho,ADJ_hv,ADJ_sigo,ADJ_sigv,ADJ_mu,        &
   ADJ_zcld,ADJ_hdn,ADJ_hup,ADJ_hdninf,ADJ_hupinf )

   implicit none

   !--------------------------------------------------------------------
   ! Purpose: TBD
   ! Output  : ADJ_ho, ADJ_hv, ADJ_sigo, ADJ_sigv, ADJ_zcld, ADJ_mu
   ! Input   : ADJ_hdn, ADJ_hup, ADJ_hdninf, ADJ_hupinf
   !--------------------------------------------------------------------

   real, intent(in)    :: ho,hv,sigo,sigv,mu,zcld
   real, intent(inout) :: ADJ_ho,ADJ_hv,ADJ_sigo,ADJ_sigv,ADJ_zcld, ADJ_mu
   real, intent(inout) :: hdn,hup,hdninf,hupinf
   real, intent(in)    :: ADJ_hdn,ADJ_hup,ADJ_hdninf,ADJ_hupinf

   real :: gint,zgint,hint,zhint
   real :: ginf,zginf,hinf,zhinf
   real :: ADJ_gint,ADJ_zgint,ADJ_hint,ADJ_zhint
   real :: ADJ_ginf,ADJ_zginf,ADJ_hinf,ADJ_zhinf
   real :: ADJ_mu2,ADJ_halfmu,ADJ_sixthmu2,ADJ_etnthmu2
   real :: ADJ_quartmu,ADJ_halfmu2

   real :: hoinv,hvinv,chio,chiv,ezho,ezhv,alpha,alph2,alph3
   real :: chio_save,chiv_save,dplus_save,dmin_save
   real :: beta,beta2,beta3,mu2,mualph,cplus,cmin,dplus,dmin
   real :: chiov,chivv,chioo,chioov,chiovv,chiooo,chivvv
   real :: h11,h21,h12,newh11
   real :: sigoo,sigov,sigvv,sigooo,sigoov,sigovv,sigvvv
   real :: ezhoo,ezhov,ezhvv,ezhooo,ezhoov,ezhovv,ezhvvv
   real :: s,sprim,t,tprim,u,uprim,term1,term2,term3
   real :: halfmu,halfmu2,sixthmu2,etnthmu2,quartmu

   real :: ADJ_hoinv,ADJ_hvinv,ADJ_chio,ADJ_chiv,ADJ_ezho
   real :: ADJ_ezhv,ADJ_alpha,ADJ_alph2,ADJ_alph3
   real :: ADJ_beta,ADJ_beta2,ADJ_beta3,ADJ_mualph
   real :: ADJ_cplus,ADJ_cmin,ADJ_dplus,ADJ_dmin
   real :: ADJ_chiov,ADJ_chivv,ADJ_chioo,ADJ_chioov
   real :: ADJ_chiovv,ADJ_chiooo,ADJ_chivvv
   real :: ADJ_h11,ADJ_h21,ADJ_h12,ADJ_newh11
   real :: ADJ_sigoo,ADJ_sigov,ADJ_sigvv,ADJ_sigooo
   real :: ADJ_sigoov,ADJ_sigovv,ADJ_sigvvv
   real :: ADJ_ezhoo,ADJ_ezhov,ADJ_ezhvv,ADJ_ezhooo
   real :: ADJ_ezhoov,ADJ_ezhovv,ADJ_ezhvvv
   real :: ADJ_s,ADJ_sprim,ADJ_t,ADJ_tprim
   real :: ADJ_u,ADJ_uprim,ADJ_term1,ADJ_term2,ADJ_term3

   if (trace_use) call da_trace_entry("da_effht_adj")

    ! initial zero
   gint=0.0
   zgint=0.0
   hint=0.0
   zhint=0.0
   ginf=0.0
   zginf=0.0
   hinf=0.0
   zhinf=0.0
   hoinv=0.0
   hvinv=0.0
   chio=0.0
   chiv=0.0
   ezho=0.0
   ezhv=0.0
   alpha=0.0
   alph2=0.0
   alph3=0.0
   chio_save=0.0
   chiv_save=0.0
   dplus_save=0.0
   dmin_save=0.0
   beta=0.0
   beta2=0.0
   beta3=0.0
   mu2=0.0
   mualph=0.0
   cplus=0.0
   cmin=0.0
   dplus=0.0
   dmin=0.0
   chiov=0.0
   chivv=0.0
   chioo=0.0
   chioov=0.0
   chiovv=0.0
   chiooo=0.0
   chivvv=0.0
   h11=0.0
   h21=0.0
   h12=0.0
   newh11=0.0
   sigoo=0.0
   sigov=0.0
   sigvv=0.0
   sigooo=0.0
   sigoov=0.0
   sigovv=0.0
   sigvvv=0.0
   ezhoo=0.0
   ezhov=0.0
   ezhvv=0.0
   ezhooo=0.0
   ezhoov=0.0
   ezhovv=0.0
   ezhvvv=0.0
   s=0.0
   sprim=0.0
   t=0.0
   tprim=0.0
   u=0.0
   uprim=0.0
   term1=0.0
   term2=0.0
   term3=0.0
   halfmu=0.0
   halfmu2=0.0
   sixthmu2=0.0
   etnthmu2=0.0
   quartmu=0.0

   ! WHY?
   ! ADJ_ho=0.0
   ! ADJ_hv=0.0
   ! ADJ_sigo=0.0
   ! ADJ_sigv=0.0
   ! ADJ_zcld=0.0
   ! ADJ_mu=0.0

   ADJ_gint=0.0
   ADJ_zgint=0.0
   ADJ_hint=0.0
   ADJ_zhint=0.0
   ADJ_ginf=0.0
   ADJ_zginf=0.0
   ADJ_hinf=0.0
   ADJ_zhinf=0.0
   ADJ_mu2=0.0
   ADJ_halfmu=0.0
   ADJ_sixthmu2=0.0
   ADJ_etnthmu2=0.0
   ADJ_quartmu=0.0
   ADJ_halfmu2=0.0
   ADJ_hoinv=0.0
   ADJ_hvinv=0.0
   ADJ_chio=0.0
   ADJ_chiv=0.0
   ADJ_ezho=0.0
   ADJ_ezhv=0.0
   ADJ_alpha=0.0
   ADJ_alph2=0.0
   ADJ_alph3=0.0
   ADJ_beta=0.0
   ADJ_beta2=0.0
   ADJ_beta3=0.0
   ADJ_mualph=0.0
   ADJ_cplus=0.0
   ADJ_cmin=0.0
   ADJ_dplus=0.0
   ADJ_dmin=0.0
   ADJ_chiov=0.0
   ADJ_chivv=0.0
   ADJ_chioo=0.0
   ADJ_chioov=0.0
   ADJ_chiovv=0.0
   ADJ_chiooo=0.0
   ADJ_chivvv=0.0
   ADJ_h11=0.0
   ADJ_h21=0.0
   ADJ_h12=0.0
   ADJ_newh11=0.0
   ADJ_sigoo=0.0
   ADJ_sigov=0.0
   ADJ_sigvv=0.0
   ADJ_sigooo=0.0
   ADJ_sigoov=0.0
   ADJ_sigovv=0.0
   ADJ_sigvvv=0.0
   ADJ_ezhoo=0.0
   ADJ_ezhov=0.0
   ADJ_ezhvv=0.0
   ADJ_ezhooo=0.0
   ADJ_ezhoov=0.0
   ADJ_ezhovv=0.0
   ADJ_ezhvvv=0.0
   ADJ_s=0.0
   ADJ_sprim=0.0
   ADJ_t=0.0
   ADJ_tprim=0.0
   ADJ_u=0.0
   ADJ_uprim=0.0
   ADJ_term1=0.0
   ADJ_term2=0.0
   ADJ_term3=0.0

   ! base fields

   hoinv =  1.0d0/ho
   hvinv =  1.0d0/hv
   chio = zcld*hoinv
   chiv = zcld*hvinv
   ezho = sigo*exp(-chio)
   ezhv = sigv*exp(-chiv)
   alpha = sigo + sigv
   alph2 = alpha*alpha
   alph3 = alpha*alph2
   beta = ezho + ezhv
   beta2 = beta*beta
   beta3 = beta*beta2

   mu2        = mu*mu
   halfmu     = 0.5d0*    mu
   sixthmu2   =     mu2/6.0d0
   etnthmu2   =     mu2/18.0d0
   quartmu    = 0.25d0*    mu
   halfmu2    = 0.5d0*    mu2

   mualph = mu*alpha
   cplus  = 1.0d0 +     mualph
   cmin   = 1.0d0 -     mualph
   dplus  = halfmu2*alph2
   dmin   =     dplus
  
   dplus_save = dplus
   dplus  =     cplus +     dplus
  
   dmin_save  = dmin
   dmin   =     cmin  +     dmin

   h11    =     hoinv +     hvinv
   h21    =  1.0d0/(h11 + hvinv)
   h12    =  1.0d0/(h11 + hoinv)
   newh11 =  1.0d0/h11
  
   chiov  = 1.0d0 +     chio +     chiv
   chioo  = 1.0d0 +     chio +     chio
   chivv  = 1.0d0 +     chiv +     chiv
   chioov =     chioo +     chiv
   chiovv =     chio  +     chivv
   chiooo =     chioo +     chio
   chivvv =     chivv +     chiv
   chio_save = chio
   chio   = 1.0d0 +     chio
   chiv_save = chiv
   chiv   = 1.0d0 +     chiv
   sigov  = sigo*sigv
   sigoo  = sigo*sigo
   sigvv  = sigv*sigv
   sigooo = sigoo*sigo
   sigoov = sigoo*sigv
   sigovv = sigo*sigvv
   sigvvv = sigvv*sigv
   ezhoo  = ezho*ezho
   ezhov  = ezho*ezhv
   ezhvv  = ezhv*ezhv
   ezhovv = ezho*ezhvv
   ezhoov = ezhoo*ezhv
   ezhooo = ezhoo*ezho
   ezhvvv = ezhvv*ezhv
   s      = sigo*ho + sigv*hv

   sprim  = ezho*ho*chio + ezhv*hv*chiv
   t      = sigoo*ho + 4.0d0*sigov*newh11 + sigvv*hv

   tprim  = ezhoo*ho*chioo + 4.0d0*ezhov*newh11*chiov + ezhvv*hv*chivv
   u      = sigooo*ho + 9.0d0*(sigovv*h21+sigoov*h12) + sigvvv*hv

   uprim  = ezhvvv*hv*chivvv +  &
            9.0d0*(ezhovv*h21*chiovv + ezhoov*h12*chioov) + &
            ezhooo*ho*chiooo

   term1  =     s -     sprim
   term2  = quartmu*(t - tprim)
   term3  = etnthmu2*(    u -     uprim)
   zgint  = dmin*term1 +  cmin*term2 + term3
   zhint  = -dplus*term1 + cplus*term2 - term3
   term2  = quartmu * t
   term3  = etnthmu2*u
   zginf  = dmin*s +  cmin*term2 + term3
   zhinf  = -dplus*s + cplus*term2 - term3

   term1  =     alpha -     beta
   term2  = halfmu*(    alph2 -     beta2)
   term3  = sixthmu2*(    alph3 -     beta3)
   gint   = dmin*term1 +  cmin*term2 + term3
   hint   = -dplus*term1 + cplus*term2 - term3
   term2  = halfmu*alph2
   term3  = sixthmu2*alph3
   ginf   = dmin*alpha +  cmin*term2 + term3
   hinf   = -dplus*alpha + cplus*term2 - term3
   hdn    = zgint/gint
   hup    = zhint/hint
   hdninf = zginf/ginf
   hupinf = zhinf/hinf

   ! start

   ADJ_zhinf  =   ADJ_hupinf/hinf        + ADJ_zhinf
   ADJ_hinf   = - hupinf*ADJ_hupinf/hinf + ADJ_hinf

   ADJ_zginf  =   ADJ_hdninf/ginf        + ADJ_zginf
   ADJ_ginf   = - hdninf*ADJ_hdninf/ginf + ADJ_ginf

   ADJ_zhint  =   ADJ_hup/hint     + ADJ_zhint
   ADJ_hint   = - hup*ADJ_hup/hint + ADJ_hint

   ADJ_zgint  =   ADJ_hdn/gint       + ADJ_zgint
   ADJ_gint   = - hdn * ADJ_hdn/gint + ADJ_gint

   ADJ_dplus  = - ADJ_hinf*alpha  + ADJ_dplus 
   ADJ_alpha = - dplus*ADJ_hinf   + ADJ_alpha
   ADJ_cplus =   ADJ_hinf*term2   + ADJ_cplus
   ADJ_term2 =   cplus*ADJ_hinf 
   ADJ_term3 = - ADJ_hinf         

   ADJ_dmin   =   ADJ_ginf*alpha + ADJ_dmin
   ADJ_alpha  =   dmin*ADJ_ginf  + ADJ_alpha
   ADJ_cmin   =   ADJ_ginf*term2 + ADJ_cmin
   ADJ_term2  =   cmin*ADJ_ginf  + ADJ_term2
   ADJ_term3  =   ADJ_ginf       + ADJ_term3

   ADJ_sixthmu2 = ADJ_term3*alph3    + ADJ_sixthmu2
   ADJ_alph3    = sixthmu2*ADJ_term3 + ADJ_alph3

   ADJ_halfmu =  ADJ_term2*alph2  + ADJ_halfmu
   ADJ_alph2  =  halfmu*ADJ_term2 + ADJ_alph2

   ! new term2,3

   term2 = halfmu*(alph2 - beta2)
   term3 = sixthmu2*(alph3 - beta3)

   ADJ_dplus  = - ADJ_hint*term1 + ADJ_dplus
   ADJ_term1  = - dplus*ADJ_hint 
   ADJ_cplus  =   ADJ_hint*term2 + ADJ_cplus
   ADJ_term2  =   cplus*ADJ_hint 
   ADJ_term3  = - ADJ_hint

   ADJ_dmin   = ADJ_gint*term1 + ADJ_dmin
   ADJ_term1  = dmin*ADJ_gint  + ADJ_term1
   ADJ_cmin   = ADJ_gint*term2 + ADJ_cmin
   ADJ_term2  = cmin*ADJ_gint  + ADJ_term2
   ADJ_term3  = ADJ_gint       + ADJ_term3

   ADJ_sixthmu2 =    ADJ_term3*(alph3 - beta3) + ADJ_sixthmu2
   ADJ_alph3    =    sixthmu2*ADJ_term3        + ADJ_alph3
   ADJ_beta3    =  - sixthmu2*ADJ_term3        + ADJ_beta3
    
   ADJ_halfmu =   ADJ_term2*(alph2 - beta2) + ADJ_halfmu
   ADJ_alph2  =   halfmu*ADJ_term2          + ADJ_alph2
   ADJ_beta2  = - halfmu*ADJ_term2          + ADJ_beta2

   ADJ_alpha  =   ADJ_term1 + ADJ_alpha
   ADJ_beta   = - ADJ_term1 + ADJ_beta

   ! new term2,3

   term2 = quartmu*t
   term3 = etnthmu2*u

   ADJ_dplus = - ADJ_zhinf*s     + ADJ_dplus
   ADJ_s = - dplus*ADJ_zhinf + ADJ_s
   ADJ_cplus =   ADJ_zhinf*term2 + ADJ_cplus
   ADJ_term2 =   cplus*ADJ_zhinf
   ADJ_term3 = - ADJ_zhinf 

   ADJ_dmin   = ADJ_zginf*s     + ADJ_dmin
   ADJ_s      = dmin*ADJ_zginf  + ADJ_s
   ADJ_cmin   = ADJ_zginf*term2 + ADJ_cmin
   ADJ_term2  = cmin*ADJ_zginf  + ADJ_term2
   ADJ_term3  = ADJ_zginf       + ADJ_term3
   
   ADJ_etnthmu2 = ADJ_term3*u        + ADJ_etnthmu2
   ADJ_u        = etnthmu2*ADJ_term3 + ADJ_u
   ADJ_quartmu = ADJ_term2*t         + ADJ_quartmu   
   ADJ_t       = quartmu*ADJ_term2   + ADJ_t

   ! new term1,2,3

   term1 = s - sprim
   term2 = quartmu*(t - tprim)
   term3 = etnthmu2*(u - uprim)

   ADJ_dplus = - ADJ_zhint*term1 + ADJ_dplus
   ADJ_term1 = - dplus*ADJ_zhint
   ADJ_cplus =   ADJ_zhint*term2 + ADJ_cplus
   ADJ_term2 =   cplus*ADJ_zhint
   ADJ_term3 = - ADJ_zhint

   ADJ_dmin  = ADJ_zgint*term1 + ADJ_dmin
   ADJ_term1  = dmin*ADJ_zgint  + ADJ_term1
   ADJ_cmin  = ADJ_zgint*term2 + ADJ_cmin
   ADJ_term2  = cmin*ADJ_zgint  + ADJ_term2
   ADJ_term3  = ADJ_zgint       + ADJ_term3

   ADJ_etnthmu2 =   ADJ_term3*(u - uprim) + ADJ_etnthmu2
   ADJ_u        =   etnthmu2*ADJ_term3    + ADJ_u
   ADJ_uprim    = - etnthmu2*ADJ_term3    + ADJ_uprim

   ADJ_quartmu =   ADJ_term2*(t - tprim) + ADJ_quartmu
   ADJ_t       =   quartmu*ADJ_term2     + ADJ_t
   ADJ_tprim   = - quartmu*ADJ_term2     + ADJ_tprim 

   ADJ_s      =   ADJ_term1 + ADJ_s
   ADJ_sprim  = - ADJ_term1 + ADJ_sprim


   ADJ_ezhvvv = ADJ_uprim*hv*chivvv            + ADJ_ezhvvv
   ADJ_hv     = ezhvvv*ADJ_uprim*chivvv        + ADJ_hv
   ADJ_chivvv = ezhvvv*hv*ADJ_uprim            + ADJ_chivvv
   ADJ_ezhovv = 9.0d0*ADJ_uprim*h21*chiovv     + ADJ_ezhovv
   ADJ_h21    = 9.0d0*ezhovv*ADJ_uprim*chiovv  + ADJ_h21
   ADJ_chiovv = 9.0d0*ezhovv*h21*ADJ_uprim     + ADJ_chiovv
   ADJ_ezhoov = 9.0d0*ADJ_uprim*h12*chioov     + ADJ_ezhoov
   ADJ_h12    = 9.0d0*ezhoov*ADJ_uprim*chioov  + ADJ_h12
   ADJ_chioov = 9.0d0*ezhoov*h12*ADJ_uprim     + ADJ_chioov
   ADJ_ezhooo = ADJ_uprim*ho*chiooo            + ADJ_ezhooo
   ADJ_ho     = ezhooo*ADJ_uprim*chiooo        + ADJ_ho
   ADJ_chiooo = ezhooo*ho*ADJ_uprim            + ADJ_chiooo

   ADJ_sigooo = ADJ_u*ho                       + ADJ_sigooo
   ADJ_ho     = sigooo*ADJ_u                   + ADJ_ho 
   ADJ_sigovv = 9.0d0*ADJ_u*h21                + ADJ_sigovv
   ADJ_h21    = 9.0d0*sigovv*ADJ_u             + ADJ_h21
   ADJ_sigoov = 9.0d0*ADJ_u*h12                + ADJ_sigoov
   ADJ_h12    = 9.0d0*sigoov*ADJ_u             + ADJ_h12
   ADJ_sigvvv = ADJ_u*hv                       + ADJ_sigvvv
   ADJ_hv     = sigvvv*ADJ_u                   + ADJ_hv
 
   ADJ_ezhoo  = ADJ_tprim*ho*chioo             + ADJ_ezhoo
   ADJ_ho     = ezhoo*ADJ_tprim*chioo          + ADJ_ho
   ADJ_chioo  = ezhoo*ho*ADJ_tprim             + ADJ_chioo
   ADJ_ezhov  = 4.0d0*ADJ_tprim*newh11*chiov   + ADJ_ezhov
   ADJ_newh11 = 4.0d0*ezhov*ADJ_tprim*chiov    + ADJ_newh11
   ADJ_chiov  = 4.0d0*ezhov*newh11*ADJ_tprim   + ADJ_chiov
   ADJ_ezhvv  = ADJ_tprim*hv*chivv             + ADJ_ezhvv
   ADJ_hv     = ezhvv*ADJ_tprim*chivv          + ADJ_hv
   ADJ_chivv  = ezhvv*hv*ADJ_tprim             + ADJ_chivv

   ADJ_sigoo  = ADJ_t*ho           + ADJ_sigoo
   ADJ_ho     = sigoo*ADJ_t        + ADJ_ho
   ADJ_sigov  = 4.0d0*ADJ_t*newh11 + ADJ_sigov
   ADJ_newh11 = 4.0d0*sigov*ADJ_t  + ADJ_newh11
   ADJ_sigvv  = ADJ_t*hv           + ADJ_sigvv
   ADJ_hv     = sigvv*ADJ_t        + ADJ_hv

   ADJ_ezho   = ADJ_sprim*ho*chio   + ADJ_ezho
   ADJ_ho     = ezho*ADJ_sprim*chio + ADJ_ho
   ADJ_chio   = ezho*ho*ADJ_sprim   + ADJ_chio
   ADJ_ezhv   = ADJ_sprim*hv*chiv   + ADJ_ezhv
   ADJ_hv     = ezhv*ADJ_sprim*chiv + ADJ_hv
   ADJ_chiv   = ezhv*hv*ADJ_sprim   + ADJ_chiv

   ADJ_sigo   = ADJ_s*ho     + ADJ_sigo
   ADJ_ho     = sigo*ADJ_s   + ADJ_ho
   ADJ_sigv   = ADJ_s*hv     + ADJ_sigv
   ADJ_hv     = sigv*ADJ_s   + ADJ_hv

   ADJ_ezhvv  = ADJ_ezhvvv*ezhv   + ADJ_ezhvv
   ADJ_ezhv   = ezhvv*ADJ_ezhvvv  + ADJ_ezhv
   ADJ_ezhoo  = ADJ_ezhooo*ezho   + ADJ_ezhoo
   ADJ_ezho   = ezhoo*ADJ_ezhooo  + ADJ_ezho
   ADJ_ezhoo  = ADJ_ezhoov*ezhv   + ADJ_ezhoo 
   ADJ_ezhv   = ezhoo*ADJ_ezhoov  + ADJ_ezhv
   ADJ_ezho   = ADJ_ezhovv*ezhvv  + ADJ_ezho
   ADJ_ezhvv  = ezho*ADJ_ezhovv   + ADJ_ezhvv
   ADJ_ezhv   = 2.0*ezhv*ADJ_ezhvv + ADJ_ezhv
   ADJ_ezho   = ADJ_ezhov*ezhv    + ADJ_ezho
   ADJ_ezhv   = ezho*ADJ_ezhov    + ADJ_ezhv
   ADJ_ezho   = 2.0*ezho*ADJ_ezhoo + ADJ_ezho
   ADJ_sigvv  = ADJ_sigvvv*sigv   + ADJ_sigvv
   ADJ_sigv   = sigvv*ADJ_sigvvv  + ADJ_sigv
   ADJ_sigo   = ADJ_sigovv*sigvv  + ADJ_sigo
   ADJ_sigvv  = sigo*ADJ_sigovv   + ADJ_sigvv
   ADJ_sigoo  = ADJ_sigoov*sigv   + ADJ_sigoo
   ADJ_sigv   = sigoo*ADJ_sigoov  + ADJ_sigv
   ADJ_sigoo  = ADJ_sigooo*sigo   + ADJ_sigoo
   ADJ_sigo   = sigoo*ADJ_sigooo  + ADJ_sigo
   ADJ_sigv   = 2.0*sigv*ADJ_sigvv + ADJ_sigv
   ADJ_sigo   = 2.0*sigo*ADJ_sigoo + ADJ_sigo
   ADJ_sigo   = ADJ_sigov*sigv    + ADJ_sigo
   ADJ_sigv   = sigo*ADJ_sigov    + ADJ_sigv

   ! WHY?
   !  ADJ_chiv   =         ADJ_chiv 
   ! ADJ_chio   =         ADJ_chio 

   ! new chio chiv

   chio   = chio_save
   chiv   = chiv_save

   ADJ_chivv  = ADJ_chivvv + ADJ_chivv
   ADJ_chiv   = ADJ_chivvv + ADJ_chiv

   ADJ_chioo  = ADJ_chiooo + ADJ_chioo
   ADJ_chio   = ADJ_chiooo + ADJ_chio

   ADJ_chio   = ADJ_chiovv + ADJ_chio
   ADJ_chivv  = ADJ_chiovv + ADJ_chivv

   ADJ_chioo  = ADJ_chioov + ADJ_chioo
   ADJ_chiv   = ADJ_chioov + ADJ_chiv

   ADJ_chiv   = ADJ_chivv + ADJ_chiv
   ADJ_chiv   = ADJ_chivv + ADJ_chiv

   ADJ_chio   = ADJ_chioo + ADJ_chio
   ADJ_chio   = ADJ_chioo + ADJ_chio

   ADJ_chio   =  ADJ_chiov + ADJ_chio
   ADJ_chiv   =  ADJ_chiov + ADJ_chiv

   ADJ_h11    = -1.0d0*newh11*newh11*ADJ_newh11 + ADJ_h11

   ADJ_h11    = -1.0d0*h12*h12*ADJ_h12 + ADJ_h11
   ADJ_hoinv  = -1.0d0*h12*h12*ADJ_h12 + ADJ_hoinv

   ADJ_h11    = -1.0d0*h21*h21*ADJ_h21 + ADJ_h11
   ADJ_hvinv  = -1.0d0*h21*h21*ADJ_h21 + ADJ_hvinv

   ADJ_hoinv  = ADJ_h11 + ADJ_hoinv
   ADJ_hvinv  = ADJ_h11 + ADJ_hvinv

   ADJ_cmin   = ADJ_dmin  + ADJ_cmin
   ! ADJ_dmin   = ADJ_dmin
   dmin   = dmin_save
 
   ADJ_cplus  = ADJ_dplus + ADJ_cplus
   ! ADJ_dplus  = ADJ_dplus
   dplus      = dplus_save

   ADJ_dplus  = ADJ_dmin           + ADJ_dplus
   ADJ_halfmu2 = ADJ_dplus*alph2   + ADJ_halfmu2
   ADJ_alph2   = halfmu2*ADJ_dplus + ADJ_alph2
   ADJ_mualph = - ADJ_cmin         + ADJ_mualph
   ADJ_mualph = ADJ_cplus          + ADJ_mualph
   ADJ_mu     = ADJ_mualph*alpha   + ADJ_mu
   ADJ_alpha  = mu*ADJ_mualph      + ADJ_alpha

   ADJ_mu2  = 0.5d0*ADJ_halfmu2   + ADJ_mu2
   ADJ_mu   = 0.25d0*ADJ_quartmu  + ADJ_mu
   ADJ_mu2  = ADJ_etnthmu2/18.0d0 + ADJ_mu2
   ADJ_mu2  = ADJ_sixthmu2/6.0d0  + ADJ_mu2
   ADJ_mu   = 0.5d0*ADJ_halfmu    + ADJ_mu
   ADJ_mu   = 2.0*mu*ADJ_mu2       + ADJ_mu

   ADJ_beta = ADJ_beta3*beta2               + ADJ_beta
   ADJ_beta2 = beta*ADJ_beta3               + ADJ_beta2
   ADJ_beta = 2.0*beta*ADJ_beta2             + ADJ_beta
   ADJ_ezho = ADJ_beta                      + ADJ_ezho
   ADJ_ezhv = ADJ_beta                      + ADJ_ezhv
   ADJ_alpha = ADJ_alph3*alph2              + ADJ_alpha
   ADJ_alph2 = alpha*ADJ_alph3              + ADJ_alph2
   ADJ_alpha = 2.0*alpha*ADJ_alph2           + ADJ_alpha
   ADJ_sigo = ADJ_alpha                     + ADJ_sigo
   ADJ_sigv = ADJ_alpha                     + ADJ_sigv
   ADJ_sigv =  ADJ_ezhv*exp(-chiv)          + ADJ_sigv
   ADJ_chiv = -ADJ_ezhv*ezhv                + ADJ_chiv
   ADJ_sigo =  ADJ_ezho*exp(-chio)          + ADJ_sigo
   ADJ_chio = -ADJ_ezho*ezho                + ADJ_chio
   ADJ_zcld = ADJ_chiv*hvinv                + ADJ_zcld
   ADJ_hvinv= zcld*ADJ_chiv                 + ADJ_hvinv
   ADJ_zcld  = ADJ_chio*hoinv               + ADJ_zcld
   ADJ_hoinv = zcld*ADJ_chio                + ADJ_hoinv
   ADJ_hv    = -1.0d0*hvinv*hvinv*ADJ_hvinv + ADJ_hv
   ADJ_ho    = -1.0d0*hoinv*hoinv*ADJ_hoinv + ADJ_ho

   if (trace_use) call da_trace_exit("da_effht_adj")

end subroutine da_effht_adj


