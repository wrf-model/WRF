!******************************************
!
! subroutine EFFHT(HO,HV,SIGO,SIGV,MU,ZCLD,HDN,HUP,HDNinF,HUPinF)
!
! This subroutine returns "effective emitting heights" for an atmosphere
! with gaseous absorption approximated by two exponentially-decaying
! profiles with distinct scale heights.  For applications at
! SSM/I frequencies (i.e., 19, 22, 37, 86 GHz), these absorption profiles 
! correspond to water vapor and dry air absorption, respectively.  
!
! Input parameters: HO, HV  --  scale heights of absorption corresponding to 
!                               dry air and water vapor (km):
!
!    SIGO, SIGV --  total optical depth due to each constituent
!                      (the present model is valid only for (sigo+sigv < 1.0) )
! 
!         MU    --  secant(theta), where theta is the path angle from vertical
!
!        ZCLD   --  upper limit (km) of the atmospheric layer for which
!                   hdn, hup are to be calculated.  Layer extends down to the
!                   surface.  These parameters permit the separate calculation
!                   brightness temperature contributions from a lower and
!                   upper atmospheric layer, separated by a cloud layer at
!                   height zcld.
!
! Output parameters:  
!
!  HDN, HUP     --  "effective emitting height" of an atmospheric layer
!            bounded below by the surface and above at height zcld.
!            "Effective emitting temperature" for upward and downward
!            microwave radiation emitted by that layer is then given by
!            (Ta - gamma*hup) and (Ta - gamma*hdn), respectively, where
!            Ta and gamma are effective surface temperature (deg K)
!            and lapse rate (K / km), respectively.  Brightness temperatures
!            due to emission from this layer are then (1 - tau)(Ta - gamma*hup)
!            and (1 - tau)(Ta - gamma*hdn), respectively, where tau is
!            the slant path transmittance of the layer at angle theta
!
! HDinF,HUPinF  -- same as hdn and hup, but for the entire
!                  atmosphere  (i.e., zcld set to infinity)
!
!
!      This version of EFFHT executes 83 floating point multiplications,
!      11 floating point divisions, and 2 calls to exp() .
!
!----------------------------------------------
      subroutine effht(ho,hv,sigo,sigv,mu,zcld,hdn,hup,hdninf,hupinf)
!
      real ho,hv,sigo,sigv,mu,zcld,gint,zgint,hint,zhint, &
           ginf,zginf,hinf,zhinf,hdn,hup,hdninf,hupinf
!
      real   hoinv,hvinv,chio,chiv,ezho,ezhv,alpha,alph2,alph3
      real   beta,beta2,beta3,mu2,mualph,cplus,cmin,dplus,dmin
      real   chiov,chivv,chioo,chioov,chiovv,chiooo,chivvv
      real   h11,h21,h12
      real   sigoo,sigov,sigvv,sigooo,sigoov,sigovv,sigvvv
      real   ezhoo,ezhov,ezhvv,ezhooo,ezhoov,ezhovv,ezhvvv
      real   s,sprim,t,tprim,u,uprim,term1,term2,term3
      real   halfmu,halfmu2,sixthmu2,etnthmu2,quartmu
!
!
      hoinv = 1.0d0/ho
      hvinv = 1.0d0/hv
      chio = zcld*hoinv
      chiv = zcld*hvinv
      ezho = sigo*exp(-chio)
      ezhv = sigv*exp(-chiv)
      alpha = sigo + sigv
      alph2 = alpha*alpha
      alph3 = alpha*alph2
      beta =  ezho + ezhv
      beta2 = beta*beta
      beta3 = beta*beta2
!
      mu2 = mu*mu
      halfmu = 0.5d0*mu
      sixthmu2 = mu2/6.0d0
      etnthmu2 = mu2/18.0d0
      quartmu = 0.25d0*mu
      halfmu2 = 0.5d0*mu2
      mualph = mu*alpha
      cplus = 1.0d0 + mualph
      cmin  = 1.0d0 - mualph
      dplus = halfmu2*alph2
      dmin  = dplus
      dplus = cplus + dplus
      dmin  = cmin  + dmin
!
      h11 = hoinv + hvinv
      h21 = 1.0d0/(h11 + hvinv)
      h12 = 1.0d0/(h11 + hoinv)
      h11 = 1.0d0/h11
      chiov = 1.0d0 + chio + chiv
      chioo = 1.0d0 + chio + chio
      chivv = 1.0d0 + chiv + chiv
      chioov = chioo + chiv
      chiovv = chio + chivv
      chiooo = chioo + chio
      chivvv = chivv + chiv
      chio = 1.0d0 + chio
      chiv = 1.0d0 + chiv
      sigov = sigo*sigv
      sigoo = sigo*sigo
      sigvv = sigv*sigv
      sigooo = sigoo*sigo
      sigoov = sigoo*sigv
      sigovv = sigo*sigvv
      sigvvv = sigvv*sigv
      ezhoo = ezho*ezho
      ezhov = ezho*ezhv
      ezhvv = ezhv*ezhv
      ezhovv = ezho*ezhvv
      ezhoov = ezhoo*ezhv
      ezhooo = ezhoo*ezho
      ezhvvv = ezhvv*ezhv
      s     = sigo*ho   +    sigv*hv
      sprim = ezho*ho*chio + ezhv*hv*chiv
      t     = sigoo*ho    +    4.0d0*sigov*h11     +   sigvv*hv
      tprim = ezhoo*ho*chioo + 4.0d0*ezhov*h11*chiov + ezhvv*hv*chivv
      u     = sigooo*ho+9.0d0*(sigovv*h21+sigoov*h12)+sigvvv*hv
      uprim = ezhvvv*hv*chivvv +  &
           9.0d0*(ezhovv*h21*chiovv + ezhoov*h12*chioov) + &
           ezhooo*ho*chiooo
!
      term1 = s - sprim
      term2 = quartmu*(t - tprim)
      term3 = etnthmu2*(u - uprim)
      zgint = dmin*term1 +  cmin*term2 + term3
      zhint = -dplus*term1 + cplus*term2 - term3
      term2 = quartmu*t
      term3 = etnthmu2*u
      zginf = dmin*s +  cmin*term2 + term3
      zhinf = -dplus*s + cplus*term2 - term3
      term1 = alpha - beta
      term2 = halfmu*(alph2 - beta2)
      term3 = sixthmu2*(alph3 - beta3)
      gint  = dmin*term1 +  cmin*term2 + term3
      hint  = -dplus*term1 + cplus*term2 - term3
      term2 = halfmu*alph2
      term3 = sixthmu2*alph3
      ginf  = dmin*alpha +  cmin*term2 + term3
      hinf  = -dplus*alpha + cplus*term2 - term3
      hdn   = zgint/gint
      hup   = zhint/hint
      hdninf = zginf/ginf
      hupinf = zhinf/hinf

      end subroutine effht

