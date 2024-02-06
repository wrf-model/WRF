!=================================================================================================================
 module mp_radar
 use ccpp_kind_types,only: kind_phys

 implicit none
 private
 public:: radar_init, &
          rayleigh_soak_wetgraupel

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

 integer, parameter, private :: R4KIND = selected_real_kind(6)
 integer, parameter, private :: R8KIND = selected_real_kind(12)

 integer,parameter,public:: nrbins = 50
 integer,parameter,public:: slen = 20
 character(len=slen), public:: &
              mixingrulestring_s, matrixstring_s, inclusionstring_s,   &
              hoststring_s, hostmatrixstring_s, hostinclusionstring_s, &
              mixingrulestring_g, matrixstring_g, inclusionstring_g,   &
              hoststring_g, hostmatrixstring_g, hostinclusionstring_g

 complex(kind=R8KIND),public:: m_w_0, m_i_0

 double precision,dimension(nrbins+1),public:: xxdx
 double precision,dimension(nrbins),public:: xxds,xdts,xxdg,xdtg
 double precision,parameter,public:: lamda_radar = 0.10           ! in meters
 double precision,public:: k_w,pi5,lamda4

 double precision, dimension(nrbins+1), public:: simpson
 double precision, dimension(3), parameter, public:: basis =       &
                           (/1.d0/3.d0, 4.d0/3.d0, 1.d0/3.d0/)

 real(kind=kind_phys),public,dimension(4):: xcre,xcse,xcge,xcrg,xcsg,xcgg
 real(kind=kind_phys),public:: xam_r,xbm_r,xmu_r,xobmr
 real(kind=kind_phys),public:: xam_s,xbm_s,xmu_s,xoams,xobms,xocms
 real(kind=kind_phys),public:: xam_g,xbm_g,xmu_g,xoamg,xobmg,xocmg
 real(kind=kind_phys),public:: xorg2,xosg2,xogg2


!..Single melting snow/graupel particle 90% meltwater on external sfc
 character(len=256):: radar_debug

 double precision,parameter,public:: melt_outside_s = 0.9d0
 double precision,parameter,public:: melt_outside_g = 0.9d0


 contains


!=================================================================================================================
 subroutine radar_init
 implicit none
!=================================================================================================================

 integer:: n

!-----------------------------------------------------------------------------------------------------------------

 pi5 = 3.14159*3.14159*3.14159*3.14159*3.14159
 lamda4 = lamda_radar*lamda_radar*lamda_radar*lamda_radar
 m_w_0 = m_complex_water_ray (lamda_radar, 0.0d0)
 m_i_0 = m_complex_ice_maetzler (lamda_radar, 0.0d0)
 k_w = (abs( (m_w_0*m_w_0 - 1.0) /(m_w_0*m_w_0 + 2.0) ))**2

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
 xxdx(1) = 100.d-6
 xxdx(nrbins+1) = 0.02d0
 do n = 2, nrbins
    xxdx(n) = dexp(real(n-1,kind=R8KIND)/real(nrbins,kind=R8KIND) &
            * dlog(xxdx(nrbins+1)/xxdx(1)) +dlog(xxdx(1)))
 enddo
 do n = 1, nrbins
    xxds(n) = dsqrt(xxdx(n)*xxdx(n+1))
    xdts(n) = xxdx(n+1) - xxdx(n)
 enddo

!..create bins of graupel (from 100 microns up to 5 cm).
 xxdx(1) = 100.d-6
 xxdx(nrbins+1) = 0.05d0
 do n = 2, nrbins
    xxdx(n) = dexp(real(n-1,kind=R8KIND)/real(nrbins,kind=R8KIND) &
            * dlog(xxdx(nrbins+1)/xxdx(1)) +dlog(xxdx(1)))
 enddo
 do n = 1, nrbins
    xxdg(n) = dsqrt(xxdx(n)*xxdx(n+1))
    xdtg(n) = xxdx(n+1) - xxdx(n)
 enddo


!.. The calling program must set the m(D) relations and gamma shape
!.. parameter mu for rain, snow, and graupel.  Easily add other types
!.. based on the template here.  For majority of schemes with simpler
!.. exponential number distribution, mu=0.

 xcre(1) = 1. + xbm_r
 xcre(2) = 1. + xmu_r
 xcre(3) = 4. + xmu_r
 xcre(4) = 7. + xmu_r
 do n = 1, 4
    xcrg(n) = wgamma(xcre(n))
 enddo
 xorg2 = 1./xcrg(2)

 xcse(1) = 1. + xbm_s
 xcse(2) = 1. + xmu_s
 xcse(3) = 4. + xmu_s
 xcse(4) = 7. + xmu_s
 do n = 1, 4
    xcsg(n) = wgamma(xcse(n))
 enddo
 xosg2 = 1./xcsg(2)

 xcge(1) = 1. + xbm_g
 xcge(2) = 1. + xmu_g
 xcge(3) = 4. + xmu_g
 xcge(4) = 7. + xmu_g
 do n = 1, 4
    xcgg(n) = wgamma(xcge(n))
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

!=================================================================================================================
 subroutine rayleigh_soak_wetgraupel(x_g,a_geo,b_geo,fmelt,meltratio_outside,m_w,m_i,lambda,c_back, &
                                     mixingrule,matrix,inclusion,host,hostmatrix,hostinclusion)
 implicit none
!=================================================================================================================

!--- input arguments:
 character(len=*), intent(in):: mixingrule, matrix, inclusion, &
                                host, hostmatrix, hostinclusion

 complex(kind=R8KIND),intent(in):: m_w, m_i

 double precision, intent(in):: x_g, a_geo, b_geo, fmelt, lambda, meltratio_outside

!--- output arguments:
 double precision,intent(out):: c_back

!--- local variables:
 integer:: error

 complex(kind=R8KIND):: m_core, m_air

 double precision, parameter:: pix=3.1415926535897932384626434d0
 double precision:: d_large, d_g, rhog, x_w, xw_a, fm, fmgrenz, &
                         volg, vg, volair, volice, volwater,    &
                         meltratio_outside_grenz, mra

!-----------------------------------------------------------------------------------------------------------------

!refractive index of air:
 m_air = (1.0d0,0.0d0)

!Limiting the degree of melting --- for safety: 
 fm = dmax1(dmin1(fmelt, 1.0d0), 0.0d0)
!Limiting the ratio of (melting on outside)/(melting on inside):
 mra = dmax1(dmin1(meltratio_outside, 1.0d0), 0.0d0)

!The relative portion of meltwater melting at outside should increase
!from the given input value (between 0 and 1)
!to 1 as the degree of melting approaches 1,
!so that the melting particle "converges" to a water drop.
!Simplest assumption is linear:
 mra = mra + (1.0d0-mra)*fm

 x_w = x_g * fm

 d_g = a_geo * x_g**b_geo

 if(D_g .ge. 1d-12) then

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

    d_large  = (6.0 / pix * volg) ** (1./3.)
    volice = (x_g - x_w) / (volg * 900.0)
    volwater = x_w / (1000. * volg)
    volair = 1.0 - volice - volwater
      
    !..complex index of refraction for the ice-air-water mixture
    !.. of the particle:
    m_core = get_m_mix_nested (m_air, m_i, m_w, volair, volice,      &
                      volwater, mixingrule, host, matrix, inclusion, &
                      hostmatrix, hostinclusion, error)
    if (error .ne. 0) then
       c_back = 0.0d0
       return
    endif

    !..rayleigh-backscattering coefficient of melting particle: 
    c_back = (abs((m_core**2-1.0d0)/(m_core**2+2.0d0)))**2           &
           * pi5 * d_large**6 / lamda4

 else
    c_back = 0.0d0
 endif

 end subroutine rayleigh_soak_wetgraupel

!=================================================================================================================
 real(kind=kind_phys) function wgamma(y)
 implicit none
!=================================================================================================================

!--- input arguments:
 real(kind=kind_phys),intent(in):: y

!-----------------------------------------------------------------------------------------------------------------

 wgamma = exp(gammln(y))

 end function wgamma

!=================================================================================================================
 real(kind=kind_phys) function gammln(xx)
 implicit none
!(C) Copr. 1986-92 Numerical Recipes Software 2.02
!=================================================================================================================

!--- inout arguments: 
 real(kind=kind_phys),intent(in):: xx

!--- local variables:
 integer:: j

 double precision,parameter:: stp = 2.5066282746310005d0
 double precision,dimension(6), parameter:: &
    cof = (/76.18009172947146d0, -86.50532032941677d0, &
            24.01409824083091d0, -1.231739572450155d0, &
            .1208650973866179d-2, -.5395239384953d-5/)
 double precision:: ser,tmp,x,y

!-----------------------------------------------------------------------------------------------------------------

!--- returns the value ln(gamma(xx)) for xx > 0.
 x = xx
 y = x
 tmp = x+5.5d0
 tmp = (x+0.5d0)*log(tmp)-tmp
 ser = 1.000000000190015d0
 do j = 1,6
    y=y+1.d0
    ser=ser+cof(j)/y
 enddo

 gammln=tmp+log(stp*ser/x)

 end function gammln
      
!=================================================================================================================
 complex(kind=R8KIND) function get_m_mix_nested (m_a, m_i, m_w, volair,      &
                volice, volwater, mixingrule, host, matrix,        &
                inclusion, hostmatrix, hostinclusion, cumulerror)
 implicit none
!=================================================================================================================

!--- input arguments:
 character(len=*),intent(in):: mixingrule, host, matrix,          &
                   inclusion, hostmatrix, hostinclusion

 complex(kind=R8KIND),intent(in):: m_a, m_i, m_w

 double precision,intent(in):: volice, volair, volwater

!--- output arguments:
 integer,intent(out):: cumulerror

!--- local variables:
 integer:: error

 complex(kind=R8KIND):: mtmp

  double precision:: vol1, vol2

!-----------------------------------------------------------------------------------------------------------------

!..Folded: ( (m1 + m2) + m3), where m1,m2,m3 could each be air, ice, or water
 cumulerror = 0
 get_m_mix_nested = cmplx(1.0d0,0.0d0)

 if (host .eq. 'air') then
    if (matrix .eq. 'air') then
       write(radar_debug,*) 'GET_M_MIX_NESTED: bad matrix: ', matrix
!      call physics_message(radar_debug)
       cumulerror = cumulerror + 1
    else
       vol1 = volice / MAX(volice+volwater,1d-10)
       vol2 = 1.0d0 - vol1
       mtmp = get_m_mix (m_a, m_i, m_w, 0.0d0, vol1, vol2,              &
                         mixingrule, matrix, inclusion, error)
       cumulerror = cumulerror + error
          
       if (hostmatrix .eq. 'air') then
          get_m_mix_nested = get_m_mix (m_a, mtmp, 2.0*m_a,              &
                             volair, (1.0d0-volair), 0.0d0, mixingrule,  &
                             hostmatrix, hostinclusion, error)
          cumulerror = cumulerror + error
       elseif (hostmatrix .eq. 'icewater') then
          get_m_mix_nested = get_m_mix (m_a, mtmp, 2.0*m_a,              &
                             volair, (1.0d0-volair), 0.0d0, mixingrule,  &
                             'ice', hostinclusion, error)
          cumulerror = cumulerror + error
       else
          write(radar_debug,*) 'GET_M_MIX_NESTED: bad hostmatrix: ', hostmatrix
!         call physics_message(radar_debug)
          cumulerror = cumulerror + 1
       endif
    endif

 elseif (host .eq. 'ice') then

    if (matrix .eq. 'ice') then
       write(radar_debug,*) 'GET_M_MIX_NESTED: bad matrix: ', matrix
!      call physics_message(radar_debug)
       cumulerror = cumulerror + 1
    else
       vol1 = volair / MAX(volair+volwater,1d-10)
       vol2 = 1.0d0 - vol1
       mtmp = get_m_mix (m_a, m_i, m_w, vol1, 0.0d0, vol2,               &
                         mixingrule, matrix, inclusion, error)
       cumulerror = cumulerror + error

       if (hostmatrix .eq. 'ice') then
          get_m_mix_nested = get_m_mix (mtmp, m_i, 2.0*m_a,              &
                             (1.0d0-volice), volice, 0.0d0, mixingrule,  &
                             hostmatrix, hostinclusion, error)
          cumulerror = cumulerror + error
       elseif (hostmatrix .eq. 'airwater') then
          get_m_mix_nested = get_m_mix (mtmp, m_i, 2.0*m_a,              &
                             (1.0d0-volice), volice, 0.0d0, mixingrule,  &
                             'air', hostinclusion, error)
          cumulerror = cumulerror + error          
       else
          write(radar_debug,*) 'GET_M_MIX_NESTED: bad hostmatrix: ', hostmatrix
!         call physics_message(radar_debug)
          cumulerror = cumulerror + 1
       endif
    endif

 elseif (host .eq. 'water') then

    if (matrix .eq. 'water') then
       write(radar_debug,*) 'GET_M_MIX_NESTED: bad matrix: ', matrix
!      call physics_message(radar_debug)
       cumulerror = cumulerror + 1
    else
       vol1 = volair / MAX(volice+volair,1d-10)
       vol2 = 1.0d0 - vol1
       mtmp = get_m_mix (m_a, m_i, m_w, vol1, vol2, 0.0d0,               &
                         mixingrule, matrix, inclusion, error)
       cumulerror = cumulerror + error

       if (hostmatrix .eq. 'water') then
          get_m_mix_nested = get_m_mix (2*m_a, mtmp, m_w,                &
                         0.0d0, (1.0d0-volwater), volwater, mixingrule,  &
                         hostmatrix, hostinclusion, error)
          cumulerror = cumulerror + error
       elseif (hostmatrix .eq. 'airice') then
          get_m_mix_nested = get_m_mix (2*m_a, mtmp, m_w,                &
                         0.0d0, (1.0d0-volwater), volwater, mixingrule,  &
                         'ice', hostinclusion, error)
          cumulerror = cumulerror + error          
       else
          write(radar_debug,*) 'GET_M_MIX_NESTED: bad hostmatrix: ', hostmatrix
!         call physics_message(radar_debug)
          cumulerror = cumulerror + 1
       endif
    endif

 elseif (host .eq. 'none') then

    get_m_mix_nested = get_m_mix (m_a, m_i, m_w,                         &
                       volair, volice, volwater, mixingrule,             &
                       matrix, inclusion, error)
    cumulerror = cumulerror + error
        
 else
    write(radar_debug,*) 'GET_M_MIX_NESTED: unknown matrix: ', host
!   call physics_message(radar_debug)
    cumulerror = cumulerror + 1
 endif

 if (cumulerror .ne. 0) then
    write(radar_debug,*) 'get_m_mix_nested: error encountered'
!   call physics_message(radar_debug)
    get_m_mix_nested = cmplx(1.0d0,0.0d0)    
 endif

 end function get_m_mix_nested

!=================================================================================================================
 complex(kind=R8KIND) function get_m_mix (m_a, m_i, m_w, volair, volice, &
                      volwater, mixingrule, matrix, inclusion, &
                      error)
 implicit none
!=================================================================================================================

!--- input arguments:
 character(len=*),intent(in):: mixingrule, matrix, inclusion

 complex(kind=R8KIND), intent(in):: m_a, m_i, m_w

 double precision, intent(in):: volice, volair, volwater

!--- output arguments:
 integer,intent(out):: error

!-----------------------------------------------------------------------------------------------------------------
 error = 0
 get_m_mix = cmplx(1.0d0,0.0d0)

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
!      call physics_message(radar_debug)
       error = 1
    endif

 else
    write(radar_debug,*) 'GET_M_MIX: unknown mixingrule: ', mixingrule
!   call physics_message(radar_debug)
    error = 2
 endif

 if (error .ne. 0) then
    write(radar_debug,*) 'GET_M_MIX: error encountered'
!   call physics_message(radar_debug)
 endif

 end function get_m_mix

!=================================================================================================================
 complex(kind=R8KIND) function m_complex_maxwellgarnett(vol1, vol2, vol3, &
                                  m1, m2, m3, inclusion, error)
 implicit none
!=================================================================================================================

!--- input arguments:
 character(len=*),intent(in):: inclusion

 complex(kind=R8KIND),intent(in):: m1,m2,m3

 double precision,intent(in):: vol1,vol2,vol3


!--- output arguments:
 integer,intent(out):: error

!--- local variables:
 complex(kind=R8KIND) :: beta2, beta3, m1t, m2t, m3t

!-----------------------------------------------------------------------------------------------------------------

 error = 0

 if (dabs(vol1+vol2+vol3-1.0d0) .gt. 1d-6) then
    write(radar_debug,*) 'M_COMPLEX_MAXWELLGARNETT: sum of the ', &
                    'partial volume fractions is not 1...ERROR'
!   call physics_message(radar_debug)
    m_complex_maxwellgarnett = CMPLX(-999.99d0,-999.99d0)
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
    write(radar_debug,*) 'M_COMPLEX_MAXWELLGARNETT: ', 'unknown inclusion: ', inclusion
!   call physics_message(radar_debug)
    m_complex_maxwellgarnett=cmplx(-999.99d0,-999.99d0,kind=R8KIND)
    error = 1
    return
 endif

 m_complex_maxwellgarnett = sqrt(((1.0d0-vol2-vol3)*m1t + vol2*beta2*m2t + vol3*beta3*m3t) / &
                            (1.0d0-vol2-vol3+vol2*beta2+vol3*beta3))

 end function m_complex_maxwellgarnett

!=================================================================================================================
 complex(kind=R8KIND) function m_complex_water_ray(lambda,t)
 implicit none
!=================================================================================================================

!complex refractive Index of Water as function of Temperature T
![deg C] and radar wavelength lambda [m]; valid for
!lambda in [0.001,1.0] m; T in [-10.0,30.0] deg C
!after Ray (1972)

!--- input arguments:
 double precision,intent(in):: t,lambda

!--- local variables:
 double precision,parameter:: pix=3.1415926535897932384626434d0
 double precision:: epsinf,epss,epsr,epsi
 double precision:: alpha,lambdas,sigma,nenner
 complex(kind=R8KIND),parameter:: i = (0d0,1d0)

!-----------------------------------------------------------------------------------------------------------------

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
      * cos(alpha*PIx*0.5)+0d0)) / nenner                          &
      + lambda*1.25664/1.88496
      
 m_complex_water_ray = sqrt(cmplx(epsr,-epsi))
      
 end function m_complex_water_ray

!=================================================================================================================
 complex(kind=R8KIND) function m_complex_ice_maetzler(lambda,t)
 implicit none
!=================================================================================================================
      
!complex refractive index of ice as function of Temperature T
![deg C] and radar wavelength lambda [m]; valid for
!lambda in [0.0001,30] m; T in [-250.0,0.0] C
!Original comment from the Matlab-routine of Prof. Maetzler:
!Function for calculating the relative permittivity of pure ice in
!the microwave region, according to C. Maetzler, "Microwave
!properties of ice and snow", in B. Schmitt et al. (eds.) Solar
!System Ices, Astrophys. and Space Sci. Library, Vol. 227, Kluwer
!Academic Publishers, Dordrecht, pp. 241-257 (1998). Input:
!TK = temperature (K), range 20 to 273.15
!f = frequency in GHz, range 0.01 to 3000
         
!--- input arguments:
 double precision,intent(in):: t,lambda

!--- local variables:
 double precision:: f,c,tk,b1,b2,b,deltabeta,betam,beta,theta,alfa

!-----------------------------------------------------------------------------------------------------------------

 c = 2.99d8
 tk = t + 273.16
 f = c / lambda * 1d-9

 b1 = 0.0207
 b2 = 1.16d-11
 b = 335.0d0
 deltabeta = exp(-10.02 + 0.0364*(tk-273.16))
 betam = (b1/tk) * ( exp(b/tk) / ((exp(b/tk)-1)**2) ) + b2*f*f
 beta = betam + deltabeta
 theta = 300. / tk - 1.
 alfa = (0.00504d0 + 0.0062d0*theta) * exp(-22.1d0*theta)
 m_complex_ice_maetzler = 3.1884 + 9.1e-4*(tk-273.16)
 m_complex_ice_maetzler = m_complex_ice_maetzler                   &
                        + cmplx(0.0d0, (alfa/f + beta*f)) 
 m_complex_ice_maetzler = sqrt(conjg(m_complex_ice_maetzler))
      
 end function m_complex_ice_maetzler

!=================================================================================================================
 end module mp_radar
!=================================================================================================================
