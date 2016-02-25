subroutine da_effht_tl(ho,hv,sigo,sigv,mu,zcld,hdn,hup,hdninf,hupinf, &
                     TGL_ho,TGL_hv,TGL_sigo,TGL_sigv,TGL_mu,        &
                     TGL_zcld,TGL_hdn,TGL_hup,TGL_hdninf,TGL_hupinf)

   !--------------------------------------------------------------------
   ! Purpose: TBD
   ! Input  : TGL_ho, TGL_hv, TGL_sigo, TGL_sigv, TGL_mu, TGL_zcld
   ! Output : TGL_hdn, hdn, TGL_hup, hup, 
   !         TGL_hdninf, hdninf, TGL_hupinf, hupinf
   !--------------------------------------------------------------------

   implicit none

   real,   intent(in)  :: ho,hv,sigo,sigv,mu,zcld
   real,   intent(in)  :: TGL_ho,TGL_hv,TGL_sigo,TGL_sigv,TGL_zcld, TGL_mu
   real,   intent(out) :: hdn,hup,hdninf,hupinf
   real,   intent(out) :: TGL_hdn,TGL_hup,TGL_hdninf,TGL_hupinf

   real :: gint,zgint,hint,zhint
   real :: ginf,zginf,hinf,zhinf
   real :: TGL_gint,TGL_zgint,TGL_hint,TGL_zhint
   real :: TGL_ginf,TGL_zginf,TGL_hinf,TGL_zhinf
   real :: TGL_mu2,TGL_halfmu,TGL_sixthmu2,TGL_etnthmu2
   real :: TGL_quartmu,TGL_halfmu2

   real :: hoinv,hvinv,chio,chiv,ezho,ezhv,alpha,alph2,alph3
   real :: beta,beta2,beta3,mu2,mualph,cplus,cmin,dplus,dmin
   real :: chiov,chivv,chioo,chioov,chiovv,chiooo,chivvv
   real :: h11,h21,h12,newh11
   real :: sigoo,sigov,sigvv,sigooo,sigoov,sigovv,sigvvv
   real :: ezhoo,ezhov,ezhvv,ezhooo,ezhoov,ezhovv,ezhvvv
   real :: s,sprim,t,tprim,u,uprim,term1,term2,term3
   real :: halfmu,halfmu2,sixthmu2,etnthmu2,quartmu

   real :: TGL_hoinv,TGL_hvinv,TGL_chio,TGL_chiv,TGL_ezho
   real :: TGL_ezhv,TGL_alpha,TGL_alph2,TGL_alph3
   real :: TGL_beta,TGL_beta2,TGL_beta3,TGL_mualph
   real :: TGL_cplus,TGL_cmin,TGL_dplus,TGL_dmin
   real :: TGL_chiov,TGL_chivv,TGL_chioo,TGL_chioov
   real :: TGL_chiovv,TGL_chiooo,TGL_chivvv
   real :: TGL_h11,TGL_h21,TGL_h12,TGL_newh11
   real :: TGL_sigoo,TGL_sigov,TGL_sigvv,TGL_sigooo
   real :: TGL_sigoov,TGL_sigovv,TGL_sigvvv
   real :: TGL_ezhoo,TGL_ezhov,TGL_ezhvv,TGL_ezhooo
   real :: TGL_ezhoov,TGL_ezhovv,TGL_ezhvvv
   real :: TGL_s,TGL_sprim,TGL_t,TGL_tprim
   real :: TGL_u,TGL_uprim,TGL_term1,TGL_term2,TGL_term3

   if (trace_use) call da_trace_entry("da_effht_tl")

       hoinv =  1.0d0/ho
   TGL_hoinv = -1.0d0*hoinv*hoinv*TGL_ho

       hvinv =  1.0d0/hv
   TGL_hvinv = -1.0d0*hvinv*hvinv*TGL_hv

        chio = zcld*hoinv
    TGL_chio = TGL_zcld*hoinv + zcld*TGL_hoinv

           chiv = zcld*hvinv
    TGL_chiv = TGL_zcld*hvinv + zcld*TGL_hvinv

        ezho = sigo*exp(-chio)
    TGL_ezho = TGL_sigo*exp(-chio)-TGL_chio*ezho

        ezhv = sigv*exp(-chiv)
    TGL_ezhv = TGL_sigv*exp(-chiv)-TGL_chiv*ezhv

       alpha = sigo + sigv
   TGL_alpha = TGL_sigo + TGL_sigv

       alph2 = alpha*alpha
   TGL_alph2 = 2.0*alpha*TGL_alpha

       alph3 = alpha*alph2
   TGL_alph3 = TGL_alpha*alph2+alpha*TGL_alph2

        beta = ezho + ezhv
    TGL_beta = TGL_ezho + TGL_ezhv

       beta2 = beta*beta
   TGL_beta2 = 2.0*beta*TGL_beta

       beta3 = beta*beta2
   TGL_beta3 = TGL_beta*beta2+beta*TGL_beta2

       mu2        = mu*mu
   TGL_mu2        = 2.0*mu*TGL_mu
       halfmu     = 0.5d0*    mu
   TGL_halfmu     = 0.5d0*TGL_mu
       sixthmu2   =     mu2/6.0d0
   TGL_sixthmu2   = TGL_mu2/6.0d0
       etnthmu2   =     mu2/18.0d0
   TGL_etnthmu2   = TGL_mu2/18.0d0
       quartmu    = 0.25d0*    mu
   TGL_quartmu    = 0.25d0*TGL_mu
       halfmu2    = 0.5d0*    mu2
   TGL_halfmu2    = 0.5d0*TGL_mu2

          mualph = mu*alpha
   TGL_mualph = TGL_mu*alpha + mu*TGL_alpha

       cplus  = 1.0d0 +     mualph
   TGL_cplus  =         TGL_mualph

       cmin   = 1.0d0 -     mualph
   TGL_cmin   =       - TGL_mualph

       dplus  = halfmu2*alph2
   TGL_dplus  = TGL_halfmu2*alph2 + halfmu2*TGL_alph2

       dmin   =     dplus
   TGL_dmin   = TGL_dplus

   TGL_dplus  = TGL_cplus + TGL_dplus
       dplus  =     cplus +     dplus

   TGL_dmin   = TGL_cmin  + TGL_dmin
       dmin   =     cmin  +     dmin


       h11    =     hoinv +     hvinv
   TGL_h11    = TGL_hoinv + TGL_hvinv

       h21    =  1.0d0/(h11 + hvinv)
   TGL_h21    = -1.0d0*h21*h21*(TGL_h11+TGL_hvinv)

       h12    =  1.0d0/(h11 + hoinv)
   TGL_h12    = -1.0d0*h12*h12*(TGL_h11 + TGL_hoinv)

       newh11 =  1.0d0/h11
   TGL_newh11 = -1.0d0*newh11*newh11*TGL_h11

       chiov  = 1.0d0 +     chio +     chiv
   TGL_chiov  =         TGL_chio + TGL_chiv

       chioo  = 1.0d0 +     chio +     chio
   TGL_chioo  =         TGL_chio + TGL_chio

       chivv  = 1.0d0 +     chiv +     chiv
   TGL_chivv  =         TGL_chiv + TGL_chiv

       chioov =     chioo +     chiv
   TGL_chioov = TGL_chioo + TGL_chiv

          chiovv =     chio  +     chivv
   TGL_chiovv = TGL_chio  + TGL_chivv

       chiooo =     chioo +     chio
   TGL_chiooo = TGL_chioo + TGL_chio

       chivvv =     chivv +     chiv
   TGL_chivvv = TGL_chivv + TGL_chiv

   TGL_chio   =         TGL_chio
       chio   = 1.0d0 +     chio

   TGL_chiv   =         TGL_chiv
       chiv   = 1.0d0 +     chiv

       sigov  = sigo*sigv
   TGL_sigov  = TGL_sigo*sigv + sigo*TGL_sigv

       sigoo  = sigo*sigo
   TGL_sigoo  = 2.0*sigo*TGL_sigo

       sigvv  = sigv*sigv
   TGL_sigvv  = 2.0*sigv*TGL_sigv

       sigooo = sigoo*sigo
   TGL_sigooo = TGL_sigoo*sigo + sigoo*TGL_sigo

       sigoov = sigoo*sigv
   TGL_sigoov = TGL_sigoo*sigv + sigoo*TGL_sigv

       sigovv = sigo*sigvv
   TGL_sigovv = TGL_sigo*sigvv + sigo*TGL_sigvv

       sigvvv = sigvv*sigv
   TGL_sigvvv = TGL_sigvv*sigv + sigvv*TGL_sigv

       ezhoo  = ezho*ezho
   TGL_ezhoo  = 2.0*ezho*TGL_ezho

       ezhov  = ezho*ezhv
   TGL_ezhov  = TGL_ezho*ezhv + ezho*TGL_ezhv

       ezhvv  = ezhv*ezhv
   TGL_ezhvv  = 2.0*ezhv*TGL_ezhv

       ezhovv = ezho*ezhvv
   TGL_ezhovv = TGL_ezho*ezhvv + ezho*TGL_ezhvv

       ezhoov = ezhoo*ezhv
   TGL_ezhoov = TGL_ezhoo*ezhv + ezhoo*TGL_ezhv

       ezhooo = ezhoo*ezho
   TGL_ezhooo = TGL_ezhoo*ezho + ezhoo*TGL_ezho

       ezhvvv = ezhvv*ezhv
   TGL_ezhvvv = TGL_ezhvv*ezhv + ezhvv*TGL_ezhv

       s      = sigo*ho + sigv*hv
   TGL_s      = TGL_sigo*ho + sigo*TGL_ho + TGL_sigv*hv + sigv*TGL_hv

       sprim  = ezho*ho*chio + ezhv*hv*chiv
   TGL_sprim  = TGL_ezho*ho*chio + ezho*TGL_ho*chio + ezho*ho*TGL_chio + &
                TGL_ezhv*hv*chiv + ezhv*TGL_hv*chiv + ezhv*hv*TGL_chiv

       t      = sigoo*ho + 4.0d0*sigov*newh11 + sigvv*hv
   TGL_t      = TGL_sigoo*ho + sigoo*TGL_ho + &
                4.0d0*(TGL_sigov*newh11 + sigov*TGL_newh11) + &
                TGL_sigvv*hv + sigvv*TGL_hv

       tprim  = ezhoo*ho*chioo + 4.0d0*ezhov*newh11*chiov + ezhvv*hv*chivv
   TGL_tprim  = TGL_ezhoo*ho*chioo +ezhoo*TGL_ho*chioo + ezhoo*ho*TGL_chioo + &
                4.0d0*(TGL_ezhov*newh11*chiov +    &
                       ezhov*TGL_newh11*chiov +    &
                       ezhov*newh11*TGL_chiov ) + &
                TGL_ezhvv*hv*chivv + ezhvv*TGL_hv*chivv + ezhvv*hv*TGL_chivv

       u      = sigooo*ho + 9.0d0*(sigovv*h21+sigoov*h12) + sigvvv*hv
   TGL_u      = TGL_sigooo*ho + sigooo*TGL_ho + &
                9.0d0*(TGL_sigovv*h21 + sigovv*TGL_h21 +    &
                       TGL_sigoov*h12 + sigoov*TGL_h12 ) + &
                TGL_sigvvv*hv + sigvvv*TGL_hv

       uprim  = ezhvvv*hv*chivvv +  &
                9.0d0*(ezhovv*h21*chiovv + ezhoov*h12*chioov) + &
                ezhooo*ho*chiooo
   TGL_uprim  = TGL_ezhvvv*hv*chivvv +ezhvvv*TGL_hv*chivvv+ ezhvvv*hv*TGL_chivvv+  &
                 9.0d0*(TGL_ezhovv*h21*chiovv +     &
                        ezhovv*TGL_h21*chiovv +     &
                        ezhovv*h21*TGL_chiovv +     &
                        TGL_ezhoov*h12*chioov +     &
                        ezhoov*TGL_h12*chioov +     &
                        ezhoov*h12*TGL_chioov  ) + &
                 TGL_ezhooo*ho*chiooo + ezhooo*TGL_ho*chiooo + ezhooo*ho*TGL_chiooo

       term1  =     s -     sprim
   TGL_term1  = TGL_s - TGL_sprim

       term2  = quartmu*(t - tprim)
   TGL_term2  = TGL_quartmu*(t - tprim) + quartmu*(TGL_t - TGL_tprim) 

       term3  = etnthmu2*(   u -     uprim)
   TGL_term3  = TGL_etnthmu2*(u - uprim) + etnthmu2*(TGL_u - TGL_uprim)

       zgint  = dmin*term1 +  cmin*term2 + term3
   TGL_zgint  = TGL_dmin*term1 + dmin*TGL_term1 + &
                TGL_cmin*term2 + cmin*TGL_term2 + TGL_term3

    zhint  = -dplus*term1 + cplus*term2 - term3
   TGL_zhint  = -TGL_dplus*term1 - dplus*TGL_term1 + &
                 TGL_cplus*term2 + cplus*TGL_term2 - TGL_term3

       term2  = quartmu * t
   TGL_term2  = TGL_quartmu*t + quartmu*TGL_t

       term3  = etnthmu2*u
   TGL_term3  = TGL_etnthmu2*u + etnthmu2*TGL_u

       zginf  = dmin*s +  cmin*term2 + term3
   TGL_zginf  = TGL_dmin*s + dmin*TGL_s +  &
                TGL_cmin*term2 + cmin*TGL_term2 + TGL_term3

       zhinf  = -dplus*s + cplus*term2 - term3
   TGL_zhinf  = -TGL_dplus*s - dplus*TGL_s + &
                 TGL_cplus*term2 + cplus*TGL_term2 - TGL_term3

       term1  =     alpha -     beta
   TGL_term1  = TGL_alpha - TGL_beta

       term2  = halfmu*(   alph2 -     beta2)
   TGL_term2  = TGL_halfmu*(alph2 - beta2) + halfmu*(TGL_alph2 - TGL_beta2)

       term3  = sixthmu2*(   alph3 -     beta3)
   TGL_term3  = TGL_sixthmu2*(alph3 - beta3) + sixthmu2*(TGL_alph3 - TGL_beta3)

       gint   = dmin*term1 +  cmin*term2 + term3
   TGL_gint   = TGL_dmin*term1 + dmin*TGL_term1 + &
                TGL_cmin*term2 + cmin*TGL_term2 + TGL_term3

       hint   = -dplus*term1 + cplus*term2 - term3
   TGL_hint   = -TGL_dplus*term1 - dplus*TGL_term1 + &
                 TGL_cplus*term2 + cplus*TGL_term2 - TGL_term3

       term2  = halfmu*alph2
   TGL_term2  = TGL_halfmu*alph2 + halfmu*TGL_alph2

       term3  = sixthmu2*alph3
   TGL_term3  = TGL_sixthmu2*alph3 + sixthmu2*TGL_alph3

       ginf   = dmin*alpha +  cmin*term2 + term3
   TGL_ginf   = TGL_dmin*alpha + dmin*TGL_alpha +  &
                TGL_cmin*term2 + cmin*TGL_term2 + TGL_term3

       hinf   = -dplus*alpha + cplus*term2 - term3
   TGL_hinf   = -TGL_dplus*alpha - dplus*TGL_alpha + &
                 TGL_cplus*term2 + cplus*TGL_term2 - TGL_term3

       hdn    = zgint/gint
   TGL_hdn    = TGL_zgint/gint - hdn * TGL_gint/gint

       hup    = zhint/hint
   TGL_hup    = TGL_zhint/hint - hup*TGL_hint/hint

       hdninf = zginf/ginf
   TGL_hdninf = TGL_zginf/ginf - hdninf*TGL_ginf/ginf

       hupinf = zhinf/hinf
   TGL_hupinf = TGL_zhinf/hinf - hupinf*TGL_hinf/hinf

   if (trace_use) call da_trace_exit("da_effht_tl")

end subroutine da_effht_tl


