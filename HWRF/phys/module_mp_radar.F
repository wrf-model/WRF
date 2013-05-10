!+---+-----------------------------------------------------------------+
!..This set of routines facilitates computing radar reflectivity.
!.. This module is more library code whereas the individual microphysics
!.. schemes contains specific details needed for the final computation,
!.. so refer to location within each schemes calling the routine named
!.. rayleigh_soak_wetgraupel.
!.. The bulk of this code originated from Ulrich Blahak (Germany) and
!.. was adapted to WRF by G. Thompson.  This version of code is only
!.. intended for use when Rayleigh scattering principles dominate and
!.. is not intended for wavelengths in which Mie scattering is a
!.. significant portion.  Therefore, it is well-suited to use with
!.. 5 or 10 cm wavelength like USA NEXRAD radars.
!.. This code makes some rather simple assumptions about water
!.. coating on outside of frozen species (snow/graupel).  Fraction of
!.. meltwater is simply the ratio of mixing ratio below melting level
!.. divided by mixing ratio at level just above highest T>0C.  Also,
!.. immediately 90% of the melted water exists on the ice's surface
!.. and 10% is embedded within ice.  No water is "shed" at all in these
!.. assumptions. The code is quite slow because it does the reflectivity
!.. calculations based on 50 individual size bins of the distributions.
!+---+-----------------------------------------------------------------+

MODULE module_mp_radar

      USE module_wrf_error

      PUBLIC :: rayleigh_soak_wetgraupel
      PUBLIC :: radar_init
      PRIVATE :: m_complex_water_ray
      PRIVATE :: m_complex_ice_maetzler
      PRIVATE :: m_complex_maxwellgarnett
      PRIVATE :: get_m_mix_nested
      PRIVATE :: get_m_mix
      PRIVATE :: WGAMMA
      PRIVATE :: GAMMLN


      INTEGER, PARAMETER, PUBLIC:: nrbins = 50
      DOUBLE PRECISION, DIMENSION(nrbins+1), PUBLIC:: xxDx
      DOUBLE PRECISION, DIMENSION(nrbins), PUBLIC:: xxDs,xdts,xxDg,xdtg
      DOUBLE PRECISION, PARAMETER, PUBLIC:: lamda_radar = 0.10           ! in meters
      DOUBLE PRECISION, PUBLIC:: K_w, PI5, lamda4
      COMPLEX*16, PUBLIC:: m_w_0, m_i_0
      DOUBLE PRECISION, DIMENSION(nrbins+1), PUBLIC:: simpson
      DOUBLE PRECISION, DIMENSION(3), PARAMETER, PUBLIC:: basis =       &
                           (/1.d0/3.d0, 4.d0/3.d0, 1.d0/3.d0/)
      REAL, DIMENSION(4), PUBLIC:: xcre, xcse, xcge, xcrg, xcsg, xcgg
      REAL, PUBLIC:: xam_r, xbm_r, xmu_r, xobmr
      REAL, PUBLIC:: xam_s, xbm_s, xmu_s, xoams, xobms, xocms
      REAL, PUBLIC:: xam_g, xbm_g, xmu_g, xoamg, xobmg, xocmg
      REAL, PUBLIC:: xorg2, xosg2, xogg2

      INTEGER, PARAMETER, PUBLIC:: slen = 20
      CHARACTER(len=slen), PUBLIC::                                     &
              mixingrulestring_s, matrixstring_s, inclusionstring_s,    &
              hoststring_s, hostmatrixstring_s, hostinclusionstring_s,  &
              mixingrulestring_g, matrixstring_g, inclusionstring_g,    &
              hoststring_g, hostmatrixstring_g, hostinclusionstring_g

!..Single melting snow/graupel particle 90% meltwater on external sfc
      DOUBLE PRECISION, PARAMETER:: melt_outside_s = 0.9d0
      DOUBLE PRECISION, PARAMETER:: melt_outside_g = 0.9d0

      CHARACTER*256:: radar_debug

CONTAINS

!+---+-----------------------------------------------------------------+
!+---+-----------------------------------------------------------------+
!+---+-----------------------------------------------------------------+

      subroutine radar_init

      IMPLICIT NONE
      INTEGER:: n
      PI5 = 3.14159*3.14159*3.14159*3.14159*3.14159
      lamda4 = lamda_radar*lamda_radar*lamda_radar*lamda_radar
      m_w_0 = m_complex_water_ray (lamda_radar, 0.0d0)
      m_i_0 = m_complex_ice_maetzler (lamda_radar, 0.0d0)
      K_w = (ABS( (m_w_0*m_w_0 - 1.0) /(m_w_0*m_w_0 + 2.0) ))**2

      do n = 1, nrbins+1
         simpson(n) = 0.0d0
      enddo
      do n = 1, nrbins-1, 2
         simpson(n) = simpson(n) + basis(1)
         simpson(n+1) = simpson(n+1) + basis(2)
         simpson(n+2) = simpson(n+2) + basis(3)
      enddo

      do n = 1, slen
         mixingrulestring_s(n:n) = char(0)
         matrixstring_s(n:n) = char(0)
         inclusionstring_s(n:n) = char(0)
         hoststring_s(n:n) = char(0)
         hostmatrixstring_s(n:n) = char(0)
         hostinclusionstring_s(n:n) = char(0)
         mixingrulestring_g(n:n) = char(0)
         matrixstring_g(n:n) = char(0)
         inclusionstring_g(n:n) = char(0)
         hoststring_g(n:n) = char(0)
         hostmatrixstring_g(n:n) = char(0)
         hostinclusionstring_g(n:n) = char(0)
      enddo

      mixingrulestring_s = 'maxwellgarnett'
      hoststring_s = 'air'
      matrixstring_s = 'water'
      inclusionstring_s = 'spheroidal'
      hostmatrixstring_s = 'icewater'
      hostinclusionstring_s = 'spheroidal'

      mixingrulestring_g = 'maxwellgarnett'
      hoststring_g = 'air'
      matrixstring_g = 'water'
      inclusionstring_g = 'spheroidal'
      hostmatrixstring_g = 'icewater'
      hostinclusionstring_g = 'spheroidal'

!..Create bins of snow (from 100 microns up to 2 cm).
      xxDx(1) = 100.D-6
      xxDx(nrbins+1) = 0.02d0
      do n = 2, nrbins
         xxDx(n) = DEXP(DFLOAT(n-1)/DFLOAT(nrbins) &
                  *DLOG(xxDx(nrbins+1)/xxDx(1)) +DLOG(xxDx(1)))
      enddo
      do n = 1, nrbins
         xxDs(n) = DSQRT(xxDx(n)*xxDx(n+1))
         xdts(n) = xxDx(n+1) - xxDx(n)
      enddo

!..Create bins of graupel (from 100 microns up to 5 cm).
      xxDx(1) = 100.D-6
      xxDx(nrbins+1) = 0.05d0
      do n = 2, nrbins
         xxDx(n) = DEXP(DFLOAT(n-1)/DFLOAT(nrbins) &
                  *DLOG(xxDx(nrbins+1)/xxDx(1)) +DLOG(xxDx(1)))
      enddo
      do n = 1, nrbins
         xxDg(n) = DSQRT(xxDx(n)*xxDx(n+1))
         xdtg(n) = xxDx(n+1) - xxDx(n)
      enddo


!..The calling program must set the m(D) relations and gamma shape
!.. parameter mu for rain, snow, and graupel.  Easily add other types
!.. based on the template here.  For majority of schemes with simpler
!.. exponential number distribution, mu=0.

      xcre(1) = 1. + xbm_r
      xcre(2) = 1. + xmu_r
      xcre(3) = 1. + xbm_r + xmu_r
      xcre(4) = 1. + 2.*xbm_r + xmu_r
      do n = 1, 4
         xcrg(n) = WGAMMA(xcre(n))
      enddo
      xorg2 = 1./xcrg(2)

      xcse(1) = 1. + xbm_s
      xcse(2) = 1. + xmu_s
      xcse(3) = 1. + xbm_s + xmu_s
      xcse(4) = 1. + 2.*xbm_s + xmu_s
      do n = 1, 4
         xcsg(n) = WGAMMA(xcse(n))
      enddo
      xosg2 = 1./xcsg(2)

      xcge(1) = 1. + xbm_g
      xcge(2) = 1. + xmu_g
      xcge(3) = 1. + xbm_g + xmu_g
      xcge(4) = 1. + 2.*xbm_g + xmu_g
      do n = 1, 4
         xcgg(n) = WGAMMA(xcge(n))
      enddo
      xogg2 = 1./xcgg(2)

      xobmr = 1./xbm_r
      xoams = 1./xam_s
      xobms = 1./xbm_s
      xocms = xoams**xobms
      xoamg = 1./xam_g
      xobmg = 1./xbm_g
      xocmg = xoamg**xobmg


      end subroutine radar_init

!+---+-----------------------------------------------------------------+
!+---+-----------------------------------------------------------------+

      COMPLEX*16 FUNCTION m_complex_water_ray(lambda,T)

!      Complex refractive Index of Water as function of Temperature T
!      [deg C] and radar wavelength lambda [m]; valid for
!      lambda in [0.001,1.0] m; T in [-10.0,30.0] deg C
!      after Ray (1972)

      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN):: T,lambda
      DOUBLE PRECISION:: epsinf,epss,epsr,epsi
      DOUBLE PRECISION:: alpha,lambdas,sigma,nenner
      COMPLEX*16, PARAMETER:: i = (0d0,1d0)
      DOUBLE PRECISION, PARAMETER:: PIx=3.1415926535897932384626434d0

      epsinf  = 5.27137d0 + 0.02164740d0 * T - 0.00131198d0 * T*T
      epss    = 78.54d+0 * (1.0 - 4.579d-3 * (T - 25.0)                 &
              + 1.190d-5 * (T - 25.0)*(T - 25.0)                        &
              - 2.800d-8 * (T - 25.0)*(T - 25.0)*(T - 25.0))
      alpha   = -16.8129d0/(T+273.16) + 0.0609265d0
      lambdas = 0.00033836d0 * exp(2513.98d0/(T+273.16)) * 1e-2

      nenner = 1.d0+2.d0*(lambdas/lambda)**(1d0-alpha)*sin(alpha*PIx*0.5) &
             + (lambdas/lambda)**(2d0-2d0*alpha)
      epsr = epsinf + ((epss-epsinf) * ((lambdas/lambda)**(1d0-alpha)   &
           * sin(alpha*PIx*0.5)+1d0)) / nenner
      epsi = ((epss-epsinf) * ((lambdas/lambda)**(1d0-alpha)            &
           * cos(alpha*PIx*0.5)+0d0)) / nenner                           &
           + lambda*1.25664/1.88496
      
      m_complex_water_ray = SQRT(CMPLX(epsr,-epsi))
      
      END FUNCTION m_complex_water_ray

!+---+-----------------------------------------------------------------+
      
      COMPLEX*16 FUNCTION m_complex_ice_maetzler(lambda,T)
      
!      complex refractive index of ice as function of Temperature T
!      [deg C] and radar wavelength lambda [m]; valid for
!      lambda in [0.0001,30] m; T in [-250.0,0.0] C
!      Original comment from the Matlab-routine of Prof. Maetzler:
!      Function for calculating the relative permittivity of pure ice in
!      the microwave region, according to C. Maetzler, "Microwave
!      properties of ice and snow", in B. Schmitt et al. (eds.) Solar
!      System Ices, Astrophys. and Space Sci. Library, Vol. 227, Kluwer
!      Academic Publishers, Dordrecht, pp. 241-257 (1998). Input:
!      TK = temperature (K), range 20 to 273.15
!      f = frequency in GHz, range 0.01 to 3000
         
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN):: T,lambda
      DOUBLE PRECISION:: f,c,TK,B1,B2,b,deltabeta,betam,beta,theta,alfa

      c = 2.99d8
      TK = T + 273.16
      f = c / lambda * 1d-9

      B1 = 0.0207
      B2 = 1.16d-11
      b = 335.0d0
      deltabeta = EXP(-10.02 + 0.0364*(TK-273.16))
      betam = (B1/TK) * ( EXP(b/TK) / ((EXP(b/TK)-1)**2) ) + B2*f*f
      beta = betam + deltabeta
      theta = 300. / TK - 1.
      alfa = (0.00504d0 + 0.0062d0*theta) * EXP(-22.1d0*theta)
      m_complex_ice_maetzler = 3.1884 + 9.1e-4*(TK-273.16)
      m_complex_ice_maetzler = m_complex_ice_maetzler                   &
                             + CMPLX(0.0d0, (alfa/f + beta*f)) 
      m_complex_ice_maetzler = SQRT(CONJG(m_complex_ice_maetzler))
      
      END FUNCTION m_complex_ice_maetzler

!+---+-----------------------------------------------------------------+

      subroutine rayleigh_soak_wetgraupel (x_g, a_geo, b_geo, fmelt,    &
                     meltratio_outside, m_w, m_i, lambda, C_back,       &
                     mixingrule,matrix,inclusion,                       &
                     host,hostmatrix,hostinclusion)

      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(in):: x_g, a_geo, b_geo, fmelt, lambda,  &
                                     meltratio_outside
      DOUBLE PRECISION, INTENT(out):: C_back
      COMPLEX*16, INTENT(in):: m_w, m_i
      CHARACTER(len=*), INTENT(in):: mixingrule, matrix, inclusion,     &
                                     host, hostmatrix, hostinclusion

      COMPLEX*16:: m_core, m_air
      DOUBLE PRECISION:: D_large, D_g, rhog, x_w, xw_a, fm, fmgrenz,    &
                         volg, vg, volair, volice, volwater,            &
                         meltratio_outside_grenz, mra
      INTEGER:: error
      DOUBLE PRECISION, PARAMETER:: PIx=3.1415926535897932384626434d0

!     refractive index of air:
      m_air = (1.0d0,0.0d0)

!     Limiting the degree of melting --- for safety: 
      fm = DMAX1(DMIN1(fmelt, 1.0d0), 0.0d0)
!     Limiting the ratio of (melting on outside)/(melting on inside):
      mra = DMAX1(DMIN1(meltratio_outside, 1.0d0), 0.0d0)

!    ! The relative portion of meltwater melting at outside should increase
!    ! from the given input value (between 0 and 1)
!    ! to 1 as the degree of melting approaches 1,
!    ! so that the melting particle "converges" to a water drop.
!    ! Simplest assumption is linear:
      mra = mra + (1.0d0-mra)*fm

      x_w = x_g * fm

      D_g = a_geo * x_g**b_geo

      if (D_g .ge. 1d-12) then

       vg = PIx/6. * D_g**3
       rhog = DMAX1(DMIN1(x_g / vg, 900.0d0), 10.0d0)
       vg = x_g / rhog
      
       meltratio_outside_grenz = 1.0d0 - rhog / 1000.

       if (mra .le. meltratio_outside_grenz) then
        !..In this case, it cannot happen that, during melting, all the
        !.. air inclusions within the ice particle get filled with
        !.. meltwater. This only happens at the end of all melting.
        volg = vg * (1.0d0 - mra * fm)
 
       else
        !..In this case, at some melting degree fm, all the air
        !.. inclusions get filled with meltwater.
        fmgrenz=(900.0-rhog)/(mra*900.0-rhog+900.0*rhog/1000.)

        if (fm .le. fmgrenz) then
         !.. not all air pockets are filled:
         volg = (1.0 - mra * fm) * vg
        else
         !..all air pockets are filled with meltwater, now the
         !.. entire ice sceleton melts homogeneously:
         volg = (x_g - x_w) / 900.0 + x_w / 1000.
        endif

       endif

       D_large  = (6.0 / PIx * volg) ** (1./3.)
       volice = (x_g - x_w) / (volg * 900.0)
       volwater = x_w / (1000. * volg)
       volair = 1.0 - volice - volwater
      
       !..complex index of refraction for the ice-air-water mixture
       !.. of the particle:
       m_core = get_m_mix_nested (m_air, m_i, m_w, volair, volice,      &
                         volwater, mixingrule, host, matrix, inclusion, &
                         hostmatrix, hostinclusion, error)
       if (error .ne. 0) then
        C_back = 0.0d0
        return
       endif

       !..Rayleigh-backscattering coefficient of melting particle: 
       C_back = (ABS((m_core**2-1.0d0)/(m_core**2+2.0d0)))**2           &
                * PI5 * D_large**6 / lamda4

      else
       C_back = 0.0d0
      endif

      end subroutine rayleigh_soak_wetgraupel

!+---+-----------------------------------------------------------------+

      complex*16 function get_m_mix_nested (m_a, m_i, m_w, volair,      &
                     volice, volwater, mixingrule, host, matrix,        &
                     inclusion, hostmatrix, hostinclusion, cumulerror)

      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(in):: volice, volair, volwater
      COMPLEX*16, INTENT(in):: m_a, m_i, m_w
      CHARACTER(len=*), INTENT(in):: mixingrule, host, matrix,          &
                     inclusion, hostmatrix, hostinclusion
      INTEGER, INTENT(out):: cumulerror

      DOUBLE PRECISION:: vol1, vol2
      COMPLEX*16:: mtmp
      INTEGER:: error

      !..Folded: ( (m1 + m2) + m3), where m1,m2,m3 could each be
      !.. air, ice, or water

      cumulerror = 0
      get_m_mix_nested = CMPLX(1.0d0,0.0d0)

      if (host .eq. 'air') then

       if (matrix .eq. 'air') then
        write(radar_debug,*) 'GET_M_MIX_NESTED: bad matrix: ', matrix
        CALL wrf_debug(150, radar_debug)
        cumulerror = cumulerror + 1
       else
        vol1 = volice / MAX(volice+volwater,1d-10)
        vol2 = 1.0d0 - vol1
        mtmp = get_m_mix (m_a, m_i, m_w, 0.0d0, vol1, vol2,             &
                         mixingrule, matrix, inclusion, error)
        cumulerror = cumulerror + error
          
        if (hostmatrix .eq. 'air') then
         get_m_mix_nested = get_m_mix (m_a, mtmp, 2.0*m_a,              &
                         volair, (1.0d0-volair), 0.0d0, mixingrule,     &
                         hostmatrix, hostinclusion, error)
         cumulerror = cumulerror + error
        elseif (hostmatrix .eq. 'icewater') then
         get_m_mix_nested = get_m_mix (m_a, mtmp, 2.0*m_a,              &
                         volair, (1.0d0-volair), 0.0d0, mixingrule,     &
                         'ice', hostinclusion, error)
         cumulerror = cumulerror + error
        else
         write(radar_debug,*) 'GET_M_MIX_NESTED: bad hostmatrix: ',        &
                           hostmatrix
         CALL wrf_debug(150, radar_debug)
         cumulerror = cumulerror + 1
        endif
       endif

      elseif (host .eq. 'ice') then

       if (matrix .eq. 'ice') then
        write(radar_debug,*) 'GET_M_MIX_NESTED: bad matrix: ', matrix
        CALL wrf_debug(150, radar_debug)
        cumulerror = cumulerror + 1
       else
        vol1 = volair / MAX(volair+volwater,1d-10)
        vol2 = 1.0d0 - vol1
        mtmp = get_m_mix (m_a, m_i, m_w, vol1, 0.0d0, vol2,             &
                         mixingrule, matrix, inclusion, error)
        cumulerror = cumulerror + error

        if (hostmatrix .eq. 'ice') then
         get_m_mix_nested = get_m_mix (mtmp, m_i, 2.0*m_a,              &
                         (1.0d0-volice), volice, 0.0d0, mixingrule,     &
                         hostmatrix, hostinclusion, error)
         cumulerror = cumulerror + error
        elseif (hostmatrix .eq. 'airwater') then
         get_m_mix_nested = get_m_mix (mtmp, m_i, 2.0*m_a,              &
                         (1.0d0-volice), volice, 0.0d0, mixingrule,     &
                         'air', hostinclusion, error)
         cumulerror = cumulerror + error          
        else
         write(radar_debug,*) 'GET_M_MIX_NESTED: bad hostmatrix: ',        &
                           hostmatrix
         CALL wrf_debug(150, radar_debug)
         cumulerror = cumulerror + 1
        endif
       endif

      elseif (host .eq. 'water') then

       if (matrix .eq. 'water') then
        write(radar_debug,*) 'GET_M_MIX_NESTED: bad matrix: ', matrix
        CALL wrf_debug(150, radar_debug)
        cumulerror = cumulerror + 1
       else
        vol1 = volair / MAX(volice+volair,1d-10)
        vol2 = 1.0d0 - vol1
        mtmp = get_m_mix (m_a, m_i, m_w, vol1, vol2, 0.0d0,             &
                         mixingrule, matrix, inclusion, error)
        cumulerror = cumulerror + error

        if (hostmatrix .eq. 'water') then
         get_m_mix_nested = get_m_mix (2*m_a, mtmp, m_w,                &
                         0.0d0, (1.0d0-volwater), volwater, mixingrule, &
                         hostmatrix, hostinclusion, error)
         cumulerror = cumulerror + error
        elseif (hostmatrix .eq. 'airice') then
         get_m_mix_nested = get_m_mix (2*m_a, mtmp, m_w,                &
                         0.0d0, (1.0d0-volwater), volwater, mixingrule, &
                         'ice', hostinclusion, error)
         cumulerror = cumulerror + error          
        else
         write(radar_debug,*) 'GET_M_MIX_NESTED: bad hostmatrix: ',         &
                           hostmatrix
         CALL wrf_debug(150, radar_debug)
         cumulerror = cumulerror + 1
        endif
       endif

      elseif (host .eq. 'none') then

       get_m_mix_nested = get_m_mix (m_a, m_i, m_w,                     &
                       volair, volice, volwater, mixingrule,            &
                       matrix, inclusion, error)
       cumulerror = cumulerror + error
        
      else
       write(radar_debug,*) 'GET_M_MIX_NESTED: unknown matrix: ', host
       CALL wrf_debug(150, radar_debug)
       cumulerror = cumulerror + 1
      endif

      IF (cumulerror .ne. 0) THEN
       write(radar_debug,*) 'GET_M_MIX_NESTED: error encountered'
       CALL wrf_debug(150, radar_debug)
       get_m_mix_nested = CMPLX(1.0d0,0.0d0)    
      endif

      end function get_m_mix_nested

!+---+-----------------------------------------------------------------+

      COMPLEX*16 FUNCTION get_m_mix (m_a, m_i, m_w, volair, volice,     &
                     volwater, mixingrule, matrix, inclusion, error)

      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(in):: volice, volair, volwater
      COMPLEX*16, INTENT(in):: m_a, m_i, m_w
      CHARACTER(len=*), INTENT(in):: mixingrule, matrix, inclusion
      INTEGER, INTENT(out):: error

      error = 0
      get_m_mix = CMPLX(1.0d0,0.0d0)

      if (mixingrule .eq. 'maxwellgarnett') then
       if (matrix .eq. 'ice') then
        get_m_mix = m_complex_maxwellgarnett(volice, volair, volwater,  &
                           m_i, m_a, m_w, inclusion, error)
       elseif (matrix .eq. 'water') then
        get_m_mix = m_complex_maxwellgarnett(volwater, volair, volice,  &
                           m_w, m_a, m_i, inclusion, error)
       elseif (matrix .eq. 'air') then
        get_m_mix = m_complex_maxwellgarnett(volair, volwater, volice,  &
                           m_a, m_w, m_i, inclusion, error)
       else
        write(radar_debug,*) 'GET_M_MIX: unknown matrix: ', matrix
        CALL wrf_debug(150, radar_debug)
        error = 1
       endif

      else
       write(radar_debug,*) 'GET_M_MIX: unknown mixingrule: ', mixingrule
       CALL wrf_debug(150, radar_debug)
       error = 2
      endif

      if (error .ne. 0) then
       write(radar_debug,*) 'GET_M_MIX: error encountered'
       CALL wrf_debug(150, radar_debug)
      endif

      END FUNCTION get_m_mix

!+---+-----------------------------------------------------------------+

      COMPLEX*16 FUNCTION m_complex_maxwellgarnett(vol1, vol2, vol3,    &
                     m1, m2, m3, inclusion, error)

      IMPLICIT NONE

      COMPLEX*16 :: m1, m2, m3
      DOUBLE PRECISION :: vol1, vol2, vol3
      CHARACTER(len=*) :: inclusion

      COMPLEX*16 :: beta2, beta3, m1t, m2t, m3t
      INTEGER, INTENT(out) :: error

      error = 0

      if (DABS(vol1+vol2+vol3-1.0d0) .gt. 1d-6) then
       write(radar_debug,*) 'M_COMPLEX_MAXWELLGARNETT: sum of the ',       &
              'partial volume fractions is not 1...ERROR'
       CALL wrf_debug(150, radar_debug)
       m_complex_maxwellgarnett=CMPLX(-999.99d0,-999.99d0)
       error = 1
       return
      endif

      m1t = m1**2
      m2t = m2**2
      m3t = m3**2

      if (inclusion .eq. 'spherical') then
       beta2 = 3.0d0*m1t/(m2t+2.0d0*m1t)
       beta3 = 3.0d0*m1t/(m3t+2.0d0*m1t)
      elseif (inclusion .eq. 'spheroidal') then
       beta2 = 2.0d0*m1t/(m2t-m1t) * (m2t/(m2t-m1t)*LOG(m2t/m1t)-1.0d0)
       beta3 = 2.0d0*m1t/(m3t-m1t) * (m3t/(m3t-m1t)*LOG(m3t/m1t)-1.0d0)
      else
       write(radar_debug,*) 'M_COMPLEX_MAXWELLGARNETT: ',                  &
                         'unknown inclusion: ', inclusion
       CALL wrf_debug(150, radar_debug)
       m_complex_maxwellgarnett=DCMPLX(-999.99d0,-999.99d0)
       error = 1
       return
      endif

      m_complex_maxwellgarnett = &
       SQRT(((1.0d0-vol2-vol3)*m1t + vol2*beta2*m2t + vol3*beta3*m3t) / &
       (1.0d0-vol2-vol3+vol2*beta2+vol3*beta3))

      END FUNCTION m_complex_maxwellgarnett

!+---+-----------------------------------------------------------------+
      REAL FUNCTION GAMMLN(XX)
!     --- RETURNS THE VALUE LN(GAMMA(XX)) FOR XX > 0.
      IMPLICIT NONE
      REAL, INTENT(IN):: XX
      DOUBLE PRECISION, PARAMETER:: STP = 2.5066282746310005D0
      DOUBLE PRECISION, DIMENSION(6), PARAMETER:: &
               COF = (/76.18009172947146D0, -86.50532032941677D0, &
                       24.01409824083091D0, -1.231739572450155D0, &
                      .1208650973866179D-2, -.5395239384953D-5/)
      DOUBLE PRECISION:: SER,TMP,X,Y
      INTEGER:: J

      X=XX
      Y=X
      TMP=X+5.5D0
      TMP=(X+0.5D0)*LOG(TMP)-TMP
      SER=1.000000000190015D0
      DO 11 J=1,6
        Y=Y+1.D0
        SER=SER+COF(J)/Y
11    CONTINUE
      GAMMLN=TMP+LOG(STP*SER/X)
      END FUNCTION GAMMLN
!  (C) Copr. 1986-92 Numerical Recipes Software 2.02
!+---+-----------------------------------------------------------------+
      REAL FUNCTION WGAMMA(y)

      IMPLICIT NONE
      REAL, INTENT(IN):: y

      WGAMMA = EXP(GAMMLN(y))

      END FUNCTION WGAMMA

!+---+-----------------------------------------------------------------+
END MODULE module_mp_radar
!+---+-----------------------------------------------------------------+
