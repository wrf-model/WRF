!--------------------------------------------------------------------------------
!M+
! NAME:
!       NESDIS_OCEANEM_Module
!
! PURPOSE:
!       Module containing the MW_microwave open ocean forward and tangent linear adjoint emissivity models
!
! REFERENCES:
!
!   [1] Hollinger, J. P., Passive microwave measurements of sea surface roughness, IEEE Transactions on
!       Geoscience Electronics, GE-9(3), 165-169, 1971.
!
!   [2] Klein, L.A., and C.T. Swift, An improved model for the dielectric constant of sea water at microwave
!       frequencies, IEEE J. Oceanic Eng., OE-2, 104-111, 1977.
!
!   [3] Stogryn, A., The emissivity of sea foam at microwave frequencies, J. Geophys. Res., 77, 1658-1666, 1972.
!
!   [4] Yan. B. and F. Weng, Application of AMSR-E measurements for tropical cyclone studies,
!       Part I: Retrieval of sea surface temperature and wind speed, submitted to JGR, 2005
!
! CATEGORY:
!       Surface : MW Surface Open Ocean Emissivity
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE NESDIS_OCEANEM_Module
!
! MODULES:
!       Type_Kinds:          Module containing definitions for kinds of variable types.
!
! CONTAINS:
!
!   PUBLIC SUBPROGRAM:
!
!       NESDIS_OCeanEM  : Subroutine to calculate open ocean MW-emissivity and sensitivity of emissivity
!                               to ocean surface temperature and wind speed
!
!
!   PRIVATE SUBPROGRAMs:
!
!       EPSP    : Function to calculate the real part of the dielectric constant for saline water
!
!       EPSPP   : Function to calculate the imaginery part of the dielectric constant for saline water
!
!      OceanEM_TL_SSTW : Subroutine to calculate sensitivities of Emissivity_H and Emissivity_V to SST and SSW
!
!      depsp_dt : Function to calculate the sensitivity of EPSP to SST
!
!      depspp_dt: Function to calculate the sensitivity of EPSPP to SST
!
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! CREATION HISTORY:
!       Written by:     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov (30-May-2005)
!
!
!       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
!
!      Fixed bugs : Banghua Yan, Yong Han, and Quan Liu (September-10-2005)
!
!
!  Copyright (C) 2005 Fuzhong Weng and Banghua Yan
!
!  This program is free software; you can redistribute it and/or modify it under the terms of the GNU
!  General Public License as published by the Free Software Foundation; either version 2 of the License,
!  or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
!  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
!  License for more details.
!
!  You should have received a copy of the GNU General Public License along with this program; if not, write
!  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!--------------------------------------------------------------------------------

MODULE NESDIS_OCEANEM_Module


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE

  real(fp_kind), parameter :: zero = 0.0_fp_kind
  real(fp_kind), parameter :: one = 1.0_fp_kind
  real(fp_kind), parameter :: pi = 3.14159_fp_kind
  real(fp_kind), parameter :: Salinity_default = 35.5_fp_kind
  real(fp_kind), parameter :: SST_min = 270.0_fp_kind
  real(fp_kind), parameter :: SST_max = 330.0_fp_kind
  real(fp_kind), parameter :: wind_min = 0.0_fp_kind
  real(fp_kind), parameter :: wind_max = 100.0_fp_kind

  ! ------------
  ! Visibilities
  ! ------------


  PRIVATE



  PUBLIC  :: NESDIS_OCeanEM



CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!-------------------------------------------------------------------------------------------------------------
!
! NAME:
!       NESDIS_OCeanEM
!
! PURPOSE:
!       Subroutine to simulate microwave open ocean emissivity
!
! REFERENCES:
!
!   [1] Hollinger, J. P., Passive microwave measurements of sea surface roughness, IEEE Transactions on
!       Geoscience Electronics, GE-9(3), 165-169, 1971.
!
!   [2] Klein, L.A., and C.T. Swift, An improved model for the dielectric constant of sea water at
!       microwave frequencies, IEEE J. Oceanic Eng., OE-2, 104-111, 1977.
!
!   [3] Stogryn, A., The emissivity of sea foam at microwave frequencies, J. Geophys. Res., 77,
!       1658-1666, 1972.
!
!   [4] Yan. B. and F. Weng, Application of AMSR-E measurements for tropical cyclone studies,
!       Part I: Retrieval of sea surface temperature and wind speed, submitted to JGR, 2005
!
! CATEGORY:
!       CRTM : Surface : MW OPEN OCEAN EM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL NESDIS_OCeanEM
!
! INPUT ARGUMENTS:
!
!         Frequency                Frequency User defines
!                                  This is the "I" dimension
!                                  UNITS:      GHz
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!
!         Angle                    The angle values in degree
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      Degrees
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1, (I)
!
!         SST                      Ocean surface temperature
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!        Salinity                  Sea water salinity (1/thousand)
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         wind                     Ocean surface wind speed
!                                  UNITS:      m/s
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!  INTERNAL ARGUMENTS:
!
!         foam                     Foam fraction
!                                  UNITS:
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         g,tr                     Emperical functions for wind induced
!                                  UNITS:
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         f                        Frequency
!                                  UNITS:      Hz
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
! OUTPUT ARGUMENTS:
!
!         Emissivity_H:            The surface emissivity at a horizontal polarization.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Emissivity_V:            The surface emissivity at a vertical polarization.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         EH_dSST:                 Sensitivity of Emissivity_H to SST.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      1/Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         EH_dSSW:                 Sensitivity of Emissivity_H to wind.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      1/m/s
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         EV_dSST:                 Sensitivity of Emissivity_V to SST.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      1/Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         EV_dSSW:                 Sensitivity of Emissivity_V to wind.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      1/m/s
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
! CALLS:
!
!       EPSP    : Function to calculate the real part of the dielectric constant for saline water
!
!       EPSPP   : Function to calculate the imaginery part of the dielectric constant for saline water
!
!      OceanEM_TL_SSTW : Subroutine to calculate sensitivities of Emissivity_H and Emissivity_V to SST and SSW
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
!
! CREATION HISTORY:
!       Written by:     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov (28-May-2005)
!
!
!       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
!
!  Copyright (C) 2005 Fuzhong Weng and Banghua Yan
!
!  This program is free software; you can redistribute it and/or modify it under the terms of the GNU
!  General Public License as published by the Free Software Foundation; either version 2 of the License,
!  or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
!  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
!  License for more details.
!
!  You should have received a copy of the GNU General Public License along with this program; if not, write
!  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!
!------------------------------------------------------------------------------------------------------------

     subroutine NESDIS_OCeanEM(Frequency,                                       & ! INPUT
                               Angle,                                           & ! INPUT
                               SST,                                             & ! INPUT
                               wind,                                            & ! INPUT
                               Salinity,                                        & ! INPUT
                               Emissivity_H,                                    & ! OUTPUT
                               Emissivity_V,                                    & ! OUTPUT
                               EH_dSST,                                         & ! OUTPUT
                               EH_dSSW,                                         & ! OUTPUT
                               EV_dSST,                                         & ! OUTPUT
                               EV_dSSW)                                           ! OUTPUT


        use type_kinds, only: fp_kind

        implicit none

        real(fp_kind) :: SST,Frequency,Salinity,theta,Angle, wind

        real(fp_kind) :: Emissivity_H,Emissivity_V,EH_dSST, EH_dSSW, EV_dSST, EV_dSSW

        real(fp_kind) :: f,foam,g,tr,rfoam,ref,rclear

        complex mu, eps, aid1,aid2,aid3,cang,rh,rv

!       Initialization


        IF (SST .LT. SST_min .OR. SST .GE. SST_max) SST = 300.0

        IF (wind .LT. wind_min .OR. wind .ge. wind_max) wind = 10.0

        if (Salinity .le. one) Salinity = Salinity_default

        mu = cmplx (1.0_fp_kind,0.0_fp_kind)

        f = Frequency*1.0e9

        theta = Angle*pi/180.0_fp_kind

        cang = cmplx(theta)

!       complex dielectric properties of saline water

        eps=cmplx (EPSP(SST,Salinity,f),-EPSPP(SST,Salinity,f))

        aid1 = csqrt(mu*eps-csin(cang)**2)

        aid2 = mu*ccos(cang)-aid1

        aid3 = mu*ccos(cang)+aid1

        rh = aid2/aid3

        aid2 = eps*ccos(cang)-aid1

        aid3 = eps*ccos(cang)+aid1

        rv = aid2/aid3

        if(wind.lt.7.0_fp_kind) then

           foam=zero

        else

           foam=0.006_fp_kind*(1.0_fp_kind-exp(-f*1.0e-9/7.5_fp_kind))*(wind-7.0_fp_kind)

        endif

!       correction for wind induced foam free sea surface

        if(foam .lt. zero) foam = zero

        if(foam .gt. one)  foam = one

!       emperical functions for wind induced reflection changes for hp

        g = 1.0_fp_kind - 1.748e-3*Angle-7.336e-5*Angle**2+1.044e-7*Angle**3

        tr = wind*(1.15e-1+3.8e-5*Angle**2)*sqrt(f*1.0e-9)

        rfoam = 1.0_fp_kind-(208.0_fp_kind+1.29e-9*f)/SST*g

        ref = (cabs(rh))**2

        rclear = ref - tr/SST

        Emissivity_H =1.0_fp_kind- (1.0_fp_kind-foam)*rclear-foam*rfoam

!       emperical functions for wind induced reflection changes for vp

        g  = 1.0_fp_kind - 9.946e-4*Angle+3.218e-5*Angle**2 -1.187e-6*Angle**3+7.e-20*Angle**10

        tr = wind*(1.17e-1-2.09e-3*exp(7.32e-2*Angle))*sqrt(f*1.0e-9)

        rfoam = 1.0_fp_kind-(208.0+1.29e-9*f)/SST*g

        ref = ( cabs(rv) )**2

        rclear = ref - tr/SST

        Emissivity_V = 1.0-(1.0-foam)*rclear-foam*rfoam

        if(Emissivity_H .gt. one)  Emissivity_H = one

        if(Emissivity_H .lt. zero) Emissivity_H = zero

        if(Emissivity_V .gt. one)  Emissivity_V = one

        if(Emissivity_V .lt. zero) Emissivity_V = zero


! Calculate EH_dSST, EH_dSSW, EV_dSST, EV_dSSW


        CALL OceanEM_TL_SSTW(Angle,Frequency,SST,wind,Salinity,EH_dSST, EH_dSSW, EV_dSST, EV_dSSW)


        return

        end subroutine NESDIS_OCeanEM

!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################


real function EPSP (t1,s,f)

!-------------------------------------------------------------------------------------------------------------
! PURPOSE:
!       Subroutine to calculates the real part of the dielectric constant for saline water
!
! REFERENCES:
!
!
!   Klein, L.A., and C.T. Swift, An improved model for the dielectric constant of sea water at microwave
!   frequencies, IEEE J. Oceanic Eng., OE-2, 104-111, 1977.
!
!
! INPUT ARGUMENTS:
!
!         f                       Frequency in Hz
!
!
!         t1                       Ocean surface temperature
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp_kind )
!
!         s                        Sea water salinity (1/thousand)
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!  INTERNAL ARGUMENTS:
!
!         t                       Ocean surface temperature in degrees celsius
!
! OUTPUT ARGUMENTS:
!
!         EPSP:            the real part of the dielectric constant for saline water
!
! CREATION HISTORY:
!       Written by:
!
!       Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
!
!       and     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov
!
!------------------------------------------------------------------------------------------------------------




  use type_kinds, only: fp_kind

  implicit none

  real(fp_kind) f,t1,t,t2,eswi,eswo,a,b,esw,tswo,tsw,s


  t=t1-273.0_fp_kind
  t2=(t-25.0_fp_kind)
  eswi = 4.9_fp_kind

  eswo = 87.134_fp_kind-1.949e-1*t-1.276e-2*t*t+2.491e-4*t**3
  a = 1.0_fp_kind+1.613e-5*t*s-3.656e-3*s+3.210e-5*s*s-4.232e-7*s**3
  esw = eswo*a

  tswo = 1.1109e-10-3.824e-12*t+6.938e-14*t**2-5.096e-16*t**3
  b = 1.0_fp_kind+2.282e-5*t*s-7.638e-4*s-7.760e-6*s**2+1.105e-8*s**3
  tsw = tswo*b

  EPSP = eswi +(esw-eswi)/(1.0+(f*tsw)**2)

  return

  end  function EPSP


 real function EPSPP (t1,s,f)

!-------------------------------------------------------------------------------------------------------------
! PURPOSE:
!       Subroutine to calculates the imaginary  part of the dielectric constant for saline water
!
! REFERENCES:
!
!
!   Klein, L.A., and C.T. Swift, An improved model for the dielectric constant of sea water at
!   microwave frequencies, IEEE J. Oceanic Eng., OE-2, 104-111, 1977.
!
!
! INPUT ARGUMENTS:
!
!         f                       Frequency in Hz
!
!
!         t1                       Ocean surface temperature
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp_kind )
!
!         s                        Sea water salinity (1/thousand)
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!  INTERNAL ARGUMENTS:
!
!         t                       Ocean surface temperature in degrees celsius
!
! OUTPUT ARGUMENTS:
!
!         EPSPP:            the imaginary part of the dielectric constant for saline water
!
! CREATION HISTORY:
!       Written by:
!
!       Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
!
!       and     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov
!
!------------------------------------------------------------------------------------------------------------



  use type_kinds, only: fp_kind

  implicit none

  real(fp_kind) s,f,t1,t,t2,eswi,eo,eswo,a,b,d,esw,tswo,tsw,sswo,fi,ssw


  t=t1-273.0_fp_kind

  t2=t-25.0_fp_kind

  eswi = 4.9_fp_kind

  eo = 8.854e-12

  eswo = 87.134_fp_kind-1.949e-1*t-1.276e-2*t*t+2.491e-4*t**3

  a = 1.0_fp_kind+1.613e-5*t*s-3.656e-3*s+3.210e-5*s*s-4.232e-7*s**3

  esw = eswo*a

  tswo = 1.1109e-10-3.824e-12*t+6.938e-14*t**2-5.096e-16*t**3

  b = 1.0+2.282e-5*t*s-7.638e-4*s-7.760e-6*s**2+1.105e-8*s**3

  tsw = tswo*b

  sswo = s*(0.18252-1.4619e-3*s+2.093e-5*s**2-1.282e-7*s**3)

  d = 25.0_fp_kind-t

  fi = d*(2.033e-2+1.266e-4*d+2.464e-6*d**2- s*(1.849e-5-2.551e-7*d+2.551e-8*d*d))

  ssw = sswo*exp(-fi)

  EPSPP = tsw*f*(esw-eswi)/(1.0_fp_kind+(tsw*f)**2)

  EPSPP = EPSPP + ssw/(2.0_fp_kind*pi*eo*f)

  return

  end function EPSPP


 subroutine OceanEM_TL_SSTW(degre,frequency,sst,wind,Salinity,deh_dt,deh_dw,dev_dt,dev_dw)

!-------------------------------------------------------------------------------------------------------------
!
! Program function: compute sensitivities of ev and eh to sst and ssw
!
!                         (i.e., dev_dsst, dev_dssw,deh_dsst,deh_dssw)
!
!
! Programmer:
!
!     Banghua Yan                                                    ORG: NESDIS            date: 12/29/2004
!
!
! Abstract:compute oceanic emissivity at two polarizations
!
!       References:
!
!       (1) Yan. B. and F. Weng, Application of AMSR-E measurements for tropical cyclone studies,
!
!           Part I: Retrieval of sea surface temperature and wind speed, submitted to JGR, 2005
!
!       (2) Klein, L.A., and C.T. Swift, 1977: An improved model for the dielectric constant of
!
!           sea water at microwave frequencies, IEEE J. Oceanic Eng., OE-2, 104-111.
!
!       (3) Stogryn, A., 1972: The emissivity of sea foam at microwave frequencies, J. Geophys. Res.,
!
!            77, 1658-1666.
!
!       (4) Hollinger, J. P., 1971: Passive microwave measurements of sea surface roughness.
!
!            IEEE Transactions on Geoscience Electronics, GE-9(3), 165-169.
!
!
! Input argument list:
!
!         degre    :  incident angle in degree
!
!         sst      : temperature (K)
!
!         Salinity : sea water salinity (1/thousand)
!
!         frequency: (GHz)
!
!         wind     : wind speed (m/s)
!
!
! Output argument list:
!
!        deh_dt    : sensitivity of eh to sst
!
!        deh_dw    : sensitivity of eh to ssw
!
!        dev_dt    : sensitivity of ev to sst
!
!        dev_dw    : sensitivity of ev to ssw
!
!
!
! Internal argument list:
!
!         angle    :  incident angle in radian
!
!         foam     : foam fraction
!
!         g,tr     : emperical functions for wind induced changes in reflection coefficient
!
!         f        : frequency in Hz
!
!         rh       :  surface reflectance in horizontally polarized state
!
!         rv       :                         vertically   ..
!-------------------------------------------------------------------

        use type_kinds, only: fp_kind

        implicit none

        real(fp_kind) ::  frequency,Salinity,t,degre,angle, wind

        real(fp_kind) ::  sst,f,g,tr,foam,rfoam,ref,rclear,dfoam_dw,dtr_dw,drclear_dw,drfoam_dt,drclear_dt

        real(fp_kind) ::  ev,eh,dev_dt,deh_dt,dev_dw,deh_dw

        complex mu, eps, aid1,cang,rh,rv

        complex deps_dt,drh_dt,drv_dt

        complex aid2h,aid3h,aid2v,aid3v


! Inializations

        mu = cmplx (one,zero)

        f = frequency*1.0e9

        angle = degre*pi/180.0_fp_kind

        cang = cmplx(angle)

        t = sst

! Calculate constants for given sst, ssw and f


        eps=cmplx(EPSP(sst,Salinity,f),-EPSPP(sst,Salinity,f))

        deps_dt=cmplx(depsp_dt(sst,Salinity,f),-depspp_dt(sst,Salinity,f))

        aid1 = csqrt(mu*eps-csin(cang)**2)

        aid2h = mu*ccos(cang)-aid1

        aid3h = mu*ccos(cang)+aid1

        rh = aid2h/aid3h


        aid2v = eps*ccos(cang)-aid1

        aid3v = eps*ccos(cang)+aid1

        rv = aid2v/aid3v

        if(wind.lt.7.0_fp_kind) then

           foam=zero

           dfoam_dw=zero
        else

           foam=0.006_fp_kind*(one-exp(-f*1.0e-9/7.5_fp_kind))*(wind-7.0_fp_kind)

           dfoam_dw=0.006_fp_kind*(one-exp(-f*1.0e-9/7.5_fp_kind))
       endif


!       correction for wind induced foam free sea surface

        if(foam .lt. zero) then

           foam=zero


        endif

        if(foam .gt. one)  foam=one



! H components: deh_dt,deh_dw


        g = 1.0_fp_kind - 1.748e-3*degre-7.336e-5*degre**2+ 1.044e-7*degre**3

        tr = wind*(1.15e-1+3.8e-5*degre**2)*sqrt(f*1.0e-9)

        rfoam = 1.0_fp_kind-(208.0+1.29e-9*f)/t*g

        ref = (cabs(rh))**2

        rclear = ref - tr/t

        eh =1.0_fp_kind- (1.0_fp_kind-foam)*rclear-foam*rfoam

        dtr_dw = (1.15e-1+3.8e-5*degre**2)*sqrt(f*1.0e-9)

        drfoam_dt = (208.0_fp_kind+1.29e-9*f)*g/t/t

        drh_dt = -mu*mu*ccos(cang)*deps_dt/(aid3h*aid3h*aid1)

!        drclear_dt = cabs(2.0_fp_kind*rh*drh_dt) + tr/t/t

        drclear_dt = 2.0_fp_kind*( dble(rh)*dble(drh_dt) + aimag(rh)*aimag(drh_dt) ) + tr/t/t

        drclear_dw= -dtr_dw/t

        deh_dt = -(1.0-foam)*drclear_dt - foam*drfoam_dt

        deh_dw = (rclear - rfoam)*dfoam_dw -  (1.0-foam)*drclear_dw

!V components: dev_dt,dev_dw

        g  = 1.0_fp_kind - 9.946e-4*degre+3.218e-5*degre**2 -1.187e-6*degre**3+7.e-20*degre**10

        tr = wind*(1.17e-1-2.09e-3*exp(7.32e-2*degre))*sqrt(f*1.0e-9)

        dtr_dw = (1.17e-1-2.09e-3*exp(7.32e-2*degre))*sqrt(f*1.0e-9)

        rfoam = 1.0_fp_kind-(208.0+1.29e-9*f)/t*g

        drfoam_dt = (208.0_fp_kind+1.29e-9*f)*g/t/t

        ref = ( cabs(rv) )**2


        rclear = ref - tr/t

        ev =one- (one-foam)*rclear-foam*rfoam

        drv_dt = ccos(cang)*deps_dt/(aid3v*aid3v) *(2.0*aid1-mu*eps/aid1)

        drclear_dt = 2.0_fp_kind*( dble(rv)*dble(drv_dt) + aimag(rv)*aimag(drv_dt) ) + tr/t/t

        drclear_dw= -dtr_dw/t

        dev_dt = -(one-foam)*drclear_dt - foam*drfoam_dt

        dev_dw = (rclear - rfoam)*dfoam_dw - (one-foam)*drclear_dw

        return

        end subroutine OceanEM_TL_SSTW


        real function depsp_dt (t1,s,f)

!-------------------------------------------------------------------------------------------------------------
!
! Programmer:
!
!     Banghua Yan                                                          ORG: NESDIS    date: 12/29/2004
!
!
! Abstract:calculates the sensitivity of epsp to sst
!
!          where epsp is the real part of the dielectric constant for saline water
!
!       References:
!
!    (1) Yan. B. and F. Weng, Application of AMSR-E measurements for tropical cyclone studies,
!
!        Part I: Retrieval of sea surface temperature and wind speed, submitted to JGR, 2005
!
!    (2) Klein, L.A., and C.T. Swift, 1977: An improved model for the dielectric constant of
!
!        sea water at microwave frequencies, IEEE J. Oceanic Eng., OE-2, 104-111.
!
!    (3) Ulaby, F.T., Moore, R.K., and Fung, A.K.,1986: Microwave remote sensing, active and passive, III,
!
!        From theory to applications, p.2024-2025.
!
!
! Input argument list:
!
!
!         t1       : temperature (K)
!
!         s        : sea water salinity (1/thousand)
!
!         f        : (Hz)
!
!
! Output argument list:
!
!        depsp_dt  : sensitivity of epsp to sst
!
!----------------------------------------------------------------------------------------------------------

      use type_kinds, only: fp_kind

      implicit none

      real(fp_kind) ::  s,f,t1,t,eswi,eswo,a,b,esw,tswo,tsw

      real(fp_kind) ::  deswo_dt,da_dt,db_dt,desw_dt,dtswo_dt,dtsw_dt,aid

      t=t1-273.0_fp_kind

      eswi = 4.9_fp_kind

      eswo = 87.134_fp_kind-1.949e-1*t-1.276e-2*t*t+2.491e-4*t**3

      deswo_dt = -1.949e-1-2.0*1.276e-2*t+3.0*2.491e-4*t**2

      a = 1.0_fp_kind+1.613e-5*t*s-3.656e-3*s+3.210e-5*s*s-4.232e-7*s**3

      da_dt = 1.613e-5*s

      esw = eswo*a

      desw_dt = a*deswo_dt + eswo*da_dt

      tswo = 1.1109e-10-3.824e-12*t+6.938e-14*t**2-5.096e-16*t**3

      dtswo_dt = -3.824e-12+2.0*6.938e-14*t-3.0*5.096e-16*t**2

      b = 1.0_fp_kind+2.282e-5*t*s-7.638e-4*s-7.760e-6*s**2+1.105e-8*s**3

      db_dt = 2.282e-5*s

      tsw = tswo*b

      dtsw_dt = b*dtswo_dt + tswo*db_dt

      aid = desw_dt*(1.0e+0+(f*tsw)**2) - 2.0e+0*f*f*tsw*(esw-eswi)*dtsw_dt

      depsp_dt = aid/(1.0e+0+(f*tsw)**2)**2


      return

      end function depsp_dt




      real function depspp_dt (t1,s,f)

!-------------------------------------------------------------------------------------------------------------
!
! Programmer:
!
!     Banghua Yan and Fuzhong Weng                      ORG: NESDIS                date: 12/29/2004
!
! Abstract:compute calculates depspp_dt
!
!          where epspp is the imaginair part  of the dielectric constant for saline water
!
!       References:
!
!    (1) Yan. B. and F. Weng, Application of AMSR-E measurements for tropical cyclone studies,
!
!        Part I: Retrieval of sea surface temperature and wind speed, submitted to JGR, 2005
!
!    (2) Klein, L.A., and C.T. Swift, 1977: An improved model for the dielectric constant of
!
!        sea water at microwave frequencies, IEEE J. Oceanic Eng., OE-2, 104-111.
!
!    (3) Ulaby, F.T., Moore, R.K., and Fung, A.K.,1986: Microwave remote sensing, active and passive, III,
!
!        From theory to applications, p.2024-2025.
!
! Input argument list:
!
!
!         t1       : temperature (K)
!
!         s        : sea water salinity (1/thousand)
!
!         f        : (Hz)
!
! Output argument list:
!
!
!      depspp_dt   : sensitivity of epspp to sst
!----------------------------------------------------------------------------------------------------------


      use type_kinds, only: fp_kind

      implicit none

      real(fp_kind) ::  s,f,t1,t,t2,eswi,eo,eswo,a,b,d,esw,tswo,tsw,sswo,fi,ssw,epspp

      real(fp_kind) ::  deswo_dt,da_dt,db_dt,desw_dt,dtswo_dt,dtsw_dt,dfi_dt,dssw_dt,aid

      t=t1-273.0_fp_kind

      t2=t-25.0_fp_kind

      eswi = 4.9_fp_kind

      eo = 8.854e-12

      eswo = 87.134_fp_kind-1.949e-1*t-1.276e-2*t*t+2.491e-4*t**3

      deswo_dt = -1.949e-1-2.0*1.276e-2*t+3.0*2.491e-4*t**2

      a = 1.0_fp_kind+1.613e-5*t*s-3.656e-3*s+3.210e-5*s*s-4.232e-7*s**3

      da_dt = 1.613e-5*s

      esw = eswo*a

      desw_dt = a*deswo_dt + eswo*da_dt

      tswo = 1.1109e-10-3.824e-12*t+6.938e-14*t**2-5.096e-16*t**3

      dtswo_dt = -3.824e-12+2.0*6.938e-14*t-3.0*5.096e-16*t**2

      b = 1.0_fp_kind+2.282e-5*t*s-7.638e-4*s-7.760e-6*s**2+1.105e-8*s**3

      db_dt = 2.282e-5*s

      tsw = tswo*b

      dtsw_dt = b*dtswo_dt + tswo*db_dt

      sswo = s*(0.18252-1.4619e-3*s+2.093e-5*s**2-1.282e-7*s**3)

      d = 25.0_fp_kind-t

      fi = d*(2.033e-2+1.266e-4*d+2.464e-6*d**2 - s*(1.849e-5-2.551e-7*d+2.551e-8*d*d))


      dfi_dt = -   (2.033e-2+1.266e-4*d+2.464e-6*d**2- s*(1.849e-5-2.551e-7*d+2.551e-8*d*d))             &

               - d*(1.266e-4+2.464e-6*d- s*(-2.551e-7+2.0*2.551e-8*d))

      ssw = sswo*exp(-fi)

      dssw_dt = - sswo*exp(-fi)*dfi_dt


      epspp = tsw*f*(esw-eswi)/(1.0_fp_kind+(tsw*f)**2)

      epspp = epspp + ssw/(2.0_fp_kind*pi*eo*f)


      aid = ((esw-eswi)*f*dtsw_dt + tsw*f*desw_dt) * (1.0e+0+(tsw*f)**2) - 2.0e+0*f*f*f*tsw*tsw*(esw-eswi)*dtsw_dt


      depspp_dt = 1.0_fp_kind/(2.0*pi*eo*f)*dssw_dt + aid/(1.0+(tsw*f)**2)**2

      return

      end function depspp_dt


END MODULE NESDIS_OCEANEM_Module
