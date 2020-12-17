!==============================================================
!
!  SYNOPSIS OF subroutineS TB, TBATMOS
!
!===============================================================
!  subroutine TBATMOS(ifREQ,THETA,P0,WV,HWV,TA,GAMMA,LW,ZCLD,TBUP,
!                    TBDN,TAUATM)
!
!  This routine calculates the upwelling and downwelling microwave 
!  atmospheric brightness temperatures and transmittance at SSM/I
!  frequencies.  It is a generalization of "TBCLEAR" and includes a 
!  cloud layer of known height and total liquid water content.
!------------------------------------------
!  Input :   ifREQ = (1,2,3, or 4 ) for (19.35, 22.235, 37, or 85.5 GHz)
!            THETA = incidence angle (deg.)
!
!                                                       ( approximate range )
!            P0    = surface pressure (mb)                      (940 -1030)
!            WV    = precipitable water (kg/m**2)                 (0 - 70)
!            HWV   = water vapor density scale height (km)      (0.5 - 3.0)
!            TA, GAMMA = "effective" surface air temperature 
!                               and lapse rate (K ; km)
!                     T(z) = Ta - gamma*z              (263 - 303 ; 4.0 - 6.5)
!
!            LW, ZCLD  = total cloud liquid water content (kg/m**2) and height (km)
!
!  Output :  TBUP, TBDN = atmospheric component of upwelling and downwelling
!                         brightness temperature (K)
!            TAUATM    = total atmospheric transmittance at specified incidence angle.
!
!  Subroutines called : EFFHT, SIGMA_V
!
!
!===================================================================
!
!   NOTES
!
!===================================================================
!
!
!    1) This model is described in detail by Petty (1990).  
!    Frequency dependent
!    coefficients of the model are based on radiative transfer
!    integrations of 16,893 radiosonde profiles from 29 ships, islands and
!    coastal stations, representing all major climatic regimes and all
!    seasons.  Absorption coefficients used in these integrations were
!    obtained from an adaptation of the Millimeter-wave Propagation Model
!    (MPM) of Liebe (1985) with updated line parameters (Liebe 1988,
!    personal communication).  Empirical corrections to total water
!    absorption values have been added, based on comparisons with
!    radiosondes.
!    
!    2) The frequency-dependent component of the model is only 
!    intended to give meaningful results for
!    combinations of input parameters which are meteorologically and
!    physically reasonable under coastal or maritime conditions near sea
!    level.  The user must ensure that input values are not only
!    individually reasonable, but also are mutually consistent.  See
!    Petty (1990) for details.
!    
!    3) Computed brightness temperatures from this model are generally
!    valid only for (gaseous optical depth)/cos(theta) of order unity or
!    less.  This is usually of concern only for theta > 75 deg. in a moist
!    atmosphere at 22 and 85 GHz.  An ad hoc correction has been added to
!    TBCLEAR which greatly improves the calculated downwelling brightness
!    temperature for near-grazing angles, so that its contribution to the
!    diffuse reflected component from the sea surface may be correctly
!    estimated.  No such correction is applied to upwelling brightness
!    temperatures, since there is no contribution to SSM/I observations
!    from angles other than at the nadir angle.  The correction is also
!    not yet available for downwelling brightness temperatures from
!    TBATMOS. 
!                  
!
!    The author welcomes feedback from other workers concerning the
!    accuracy and overall performance of the routines presented here.
!    
!    
!    References:
!    
!       Liebe, H.J., 1985 :  An updated model for millimeter
!         wave propagation in moist air.  Radio Science, 20, pp. 1069-1089
!    
!       Liebe, H.J., 1988, personal communication: updated absorption line
!         parameters. 
!    
!       Petty, G.W. 1990:  On the Response of the SSM/I to the 
!         Marine Environment --- Implications for Atmospheric Parameter
!         Retrievals.  Ph.D. Dissertation, Dept. of Atmospheric Sciences,
!         University of Washington
!
!       Petty, G.W., and K.B. Katsaros, 1992: The response
!        of the Special Sensor Microwave/Imager to the
!        marine environment. Part I: An analytic model for
!        the atmospheric component of observed brightness
!        temperatures.  J. Atmos. Ocean. Tech., 9, 746--761
!       
!       Petty, G.W., and K.B. Katsaros, 1993: The response
!        of the Special Sensor Microwave/Imager to the
!        marine environment. Part II: A parameterization of
!        roughness effects on sea surface emission and
!        reflection.  Submitted to J. Atmos. Ocean. Tech.
!       
!    
!===============================================================
!
!  fortran source code listings for TBATMOS,
!                       SIGMA_V, EFFHT
!
!===============================================================
!  subroutine TBATMOS(ifREQ,THETA,P0,WV,HWV,TA,GAMMA,LW,ZCLD,TBUP,
!                    TBDN,TAUATM)
!
!  This routine calculates the upwelling and downwelling microwave 
!  atmospheric brightness temperatures and transmittance at an SSM/I     
!  frequency.  It is a generalization of "TBCLEAR" and includes a 
!  cloud layer of known height and total liquid water content.
!
!  Input :   ifREQ = (1,2,3, or 4 ) for (19.35, 22.235, 37, or 85.5 GHz)
!            THETA = incidence angle (deg.)
!
!                                                       ( approximate range )
!            P0    = surface pressure (mb)                      (940 -1030)
!            WV    = precipitable water (kg/m**2)                 (0 - 70)
!            HWV   = water vapor density scale height (km)      (0.5 - 3.0)
!            TA, GAMMA = "effective" surface air temperature 
!                               and lapse rate (K ; km)
!                     T(z) = Ta - gamma*z              (263 - 303 ; 4.0 - 6.5)
!
!            LW, ZCLD  = total cloud liquid water content (kg/m**2) and height (km)
!
!  Output :  TBUP, TBDN = atmospheric component of upwelling and downwelling
!                         brightness temperature (K)
!            TAUATM    = total atmospheric transmittance at specified incidence angle.
!
!  Subroutines called : EFFHT, SIGMA_V
!------------------------------------------------------------
      subroutine tbatmos(ifreq,theta,p0,wv,hwv,ta,gamma,lw,zcld, &
          tbup,tbdn,tauatm)
      integer ifreq
      real theta,p0,wv,hwv,ta,gamma,lw,zcld,tbup,tbdn,tauatm
      real mu,hdn,hup,hdninf,hupinf
!
      real b1(4),b2(4),b3(4)
      real c(4),d1(4),d2(4),d3(4),zeta(4),kw0(4),kw1(4),kw2(4),kw3(4)
      real tau,tau1,tau2,taucld
      real tcld,tc,em,em1
      real sigv,sigo,sig,sig1,sigcld
      real teff1dn,teff1up,teffdn,teffup
      real tbcld,tbclrdn,tbclrup,tb1dn,tb1up,tb2dn,tb2up
      real otbar,tc2,tc3,hv,ho,alph
!
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
! mu = secant(theta)
      mu = 1.0/cos(theta*0.0174533)
! get water vapor optical depth
!=====
      call cal_sigma_v(ifreq,p0,wv,hwv,ta,gamma,sigv)
! otbar = one over "mean" temperature
      otbar = 1.0/(ta - gamma*zeta(ifreq))
! sigo = dry air optical depth
      sigo = b1(ifreq) + b2(ifreq)*p0  + b3(ifreq)*otbar
! cloud parameters
      tcld   = ta - gamma*zcld
      tc = tcld - t_kelvin
      tc2 = tc*tc
      tc3 = tc2*tc
      sigcld = (kw0(ifreq) + tc*kw1(ifreq) + tc2*kw2(ifreq) +  &
           tc3*kw3(ifreq))*lw
      taucld = exp(-mu*sigcld)
      tbcld = (1.0 - taucld)*tcld
! hv, ho = effective absorber scale heights for vapor, dry air
      hv = c(ifreq)*hwv
      ho = d1(ifreq) + d2(ifreq)*ta + d3(ifreq)*gamma
! get effective emission heights for layer 1 and total atmosphere
      call effht(ho,hv,sigo,sigv,mu,zcld,hdn,hup, &
         hdninf,hupinf)
! atmospheric transmittances in layer one and two, and combined
      sig = sigo + sigv
      sig1 = sigo*(1.0-exp(-zcld/ho)) + sigv*(1.0-exp(-zcld/hv))
      tau  = exp(-mu*sig)
      tau1 = exp(-mu*sig1)
      tau2 = tau/tau1
! atmospheric "emissivity"
      em1  = 1.0 - tau1
      em   = 1.0 - tau
! downwelling and upwelling brightness temperature for each layer
      teff1dn = ta - gamma*hdn
      teff1up = ta - gamma*hup
      teffdn = ta - gamma*hdninf
      teffup = ta - gamma*hupinf
      tbclrdn = teffdn*em
      tbclrup = teffup*em
!
      tb1dn = em1*teff1dn
      tb1up = em1*teff1up
      tb2dn = (tbclrdn - tb1dn)/tau1
      tb2up =  tbclrup - tau2*tb1up
! total downwelling and upwelling brightness temperature and transmittance
      tbdn  = tb1dn + tau1*(tbcld + taucld*tb2dn)
      tbup  = tb2up + tau2*(tbcld + taucld*tb1up)
      tauatm = tau*taucld
!
! the following lines apply an ad hoc correction to improve fit 
! at large angles and/or high gaseous opacities 
!  (downwelling brightness temperatures only)

      alph = (0.636619*atan(mu*sig))**2
      tbdn = (1.0-alph)*tbdn + em*alph*ta
!
      end subroutine tbatmos
