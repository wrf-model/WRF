!
!-------------------------------------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_FASTEM1
!
! PURPOSE:
!       This module computes ocean emissivity and its jacobian over water. The code is adopted
!          from RTTOV Fastem version 1.
!
! Method:
! FASTEM-1 English and Hewison 1998.
! http://www.metoffice.com/research/interproj/nwpsaf/rtm/evalfastems.pdf
!
! History:
!
! CATEGORY:
!       CRTM : Surface : MW OPEN OCEAN EM
!
! LANGUAGE:
!       Fortran-95
!
! CREATION HISTORY:
!
!   1998-02-20  treadon - gather all emissivity calculations from
!                         setuprad and move into this subroutine
!   2004-07-23  weng,yan,okamoto - incorporate MW land and snow/ice emissivity
!                         models for AMSU-A/B and SSM/I
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-11-22  derber - modify for openMP
!   2005-01-20  okamoto- add preprocessing for ocean MW emissivity jacobian
!   2005-03-05  derber- add adjoint of surface emissivity to this routine
!   2005-07-15  derber- include mhs
!   2005-09-28  derber - modify to handle percent surface type and mixed conditions
!
!   2005-12-15  Modified for CRTM
!               by:     Quanhua Liu, QSS Group Inc.,     Quanhua.Liu@noaa.gov
!                       Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------------------------------------

MODULE CRTM_Fastem1

  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds

  ! -- CRTM modules
  USE CRTM_Parameters
  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: Fastem1


! Explanation for Fastem1_Coef :
! emc(59): Emissivity model data
! Permittivity model data (Lamkaouchi model)
!   [1-3]: Temperature polynomial coefficients for Tau1 - Lamkaouchi (1996)
!   [4-7]: Temperature polynomial coefficients for Tau2 - Lamkaouchi (1996)
!  [8-11]: Temperature polynomial coefficients for Del1 - Lamkaouchi (1996)
! [12-15]: Temperature polynomial coefficients for Del2 - Lamkaouchi (1996)
! [16-17]: Temperature polynomial coefficients for static permittivity - Lamkaouchi (1996)
! [18-19]: Temperature polynomial coefficients for infinite freq. permittivity - Lamkaouchi (1996)
! Pi is stored for good measure
!    [20]: Stored value of Pi
! Bragg scattering correction coefficients
!    [21]: Scaling factor for small scale correction - see English (1997)
! Foam model coefficients for Monahan model
!    [22]: First coefficient in Monahan foam model (neutral stability)  - see English (1997)
!    [23]: Second coefficient in Monahan foam model (neutral stability) - see English (1997)
! Alternative permittivity model (Liebe)
!    [30]: a1 in Liebe's dielectric model - see Liebe (1989)
!    [31]: b1 in Liebe's dielectric model - see Liebe (1989)
!    [32]: c1 in Liebe's dielectric model - see Liebe (1989)
!    [33]: c2 in Liebe's dielectric model - see Liebe (1989)
!    [34]: d1 in Liebe's dielectric model - see Liebe (1989)
!    [35]: d2 in Liebe's dielectric model - see Liebe (1989)
!    [36]: d3 in Liebe's dielectric model - see Liebe (1989)
!    [37]: e1 in Liebe's dielectric model - see Liebe (1989)
!    [38]: e2 in Liebe's dielectric model - see Liebe (1989)
! Version 2 of large scale correction which *DOESÅª* take account of
! hemispherical scattering.
! 1.) Vertical polarisation mode
!    [24]: Term a00 in vertical pol of large scale correction model
!    [25]: Term a01 in vertical pol mode of large scale correction model
!    [26]: Term a02 in vertical pol mode of large scale correction model
!    [27]: Term a10 in vertical pol mode of large scale correction model
!    [28]: Term a11 in vertical pol mode of large scale correction model
!    [29]: Term a12 in vertical pol mode of large scale correction model
!    [30]: Term a20 in vertical pol mode of large scale correction model
!    [31]: Term a21 in vertical pol mode of large scale correction model
!    [32]: Term a22 in vertical pol mode of large scale correction model
!    [33]: Term a30 in vertical pol mode of large scale correction model
!    [34]: Term a31 in vertical pol mode of large scale correction model
!    [35]: Term a32 in vertical pol mode of large scale correction model
!    [36]: Term a40 in vertical pol mode of large scale correction model
!    [37]: Term a41 in vertical pol mode of large scale correction model
!    [38]: Term a42 in vertical pol mode of large scale correction model
!    [39]: Term a50 in vertical pol mode of large scale correction model
!    [40]: Term a51 in vertical pol mode of large scale correction model
!    [41]: Term a52 in vertical pol mode of large scale correction model
! 2. ) Horizontal polarisation mode
!    [42]: Term a00 in horizontal pol mode of large scale correction model
!    [43]: Term a01 in horizontal pol mode of large scale correction model
!    [44]: Term a02 in horizontal pol mode of large scale correction model
!    [45]: Term a10 in horizontal pol mode of large scale correction model
!    [46]: Term a11 in horizontal pol mode of large scale correction model
!    [47]: Term a12 in horizontal pol mode of large scale correction model
!    [48]: Term a20 in horizontal pol mode of large scale correction model
!    [49]: Term a21 in horizontal pol mode of large scale correction model
!    [50]: Term a22 in horizontal pol mode of large scale correction model
!    [51]: Term a30 in horizontal pol mode of large scale correction model
!    [52]: Term a31 in horizontal pol mode of large scale correction model
!    [53]: Term a32 in horizontal pol mode of large scale correction model
!    [54]: Term a40 in horizontal pol mode of large scale correction model
!    [55]: Term a41 in horizontal pol mode of large scale correction model
!    [56]: Term a42 in horizontal pol mode of large scale correction model
!    [57]: Term a50 in horizontal pol mode of large scale correction model
!    [58]: Term a51 in horizontal pol mode of large scale correction model
!    [59]: Term a52 in horizontal pol mode of large scale correction model
  REAL( fp ), PARAMETER :: emc(59) = Reshape( &
   & (/0.175350E+02_fp, -.617670E+00_fp,  .894800E-02_fp,  .318420E+01_fp,&
       0.191890E-01_fp, -.108730E-01_fp,  .258180E-03_fp,  .683960E+02_fp,&
       -.406430E+00_fp,  .228320E-01_fp, -.530610E-03_fp,  .476290E+01_fp,&
       0.154100E+00_fp, -.337170E-01_fp,  .844280E-03_fp,  .782870E+02_fp,&
       -.434630E-02_fp,  .531250E+01_fp, -.114770E-01_fp,  .314159E+01_fp,&
       -.100000E+01_fp,  .195000E-04_fp,  .255000E+01_fp, -.637182E+01_fp,&
       0.253918E-01_fp,  .357569E-04_fp,  .942928E+01_fp, -.332839E-01_fp,&
       -.647724E-04_fp, -.329282E+01_fp,  .965450E-02_fp,  .281588E-04_fp,&
       0.252676E+00_fp,  .343867E-02_fp, -.156362E-04_fp, -.156669E-03_fp,&
       0.139485E-04_fp, -.407633E-07_fp, -.141316E+00_fp, -.356556E-02_fp,&
       0.142869E-04_fp, -.240701E+01_fp, -.563888E-01_fp,  .325227E-03_fp,&
       0.296005E+01_fp,  .704675E-01_fp, -.426440E-03_fp, -.751252E+00_fp,&
       -.191934E-01_fp,  .125937E-03_fp, -.288253E+00_fp, -.102655E-02_fp,&
       0.226701E-05_fp, -.119072E-02_fp, -.263165E-04_fp,  .114597E-06_fp,&
       0.406300E+00_fp,  .200031E-02_fp, -.781635E-05_fp/), (/59/) )


CONTAINS


!-------------------------------------------------------------------------------------------------------------
!M+
! NAME:
!       FASTEM1
!
! PURPOSE:
!       Subroutine to compute ocean emissivity and its derivative to wind speed. This code is adopted from
!          RTTOV Fastem version 1.
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
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar
!
!
!         Sat_Zenith_Angle         The angle values in degree
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      Degrees
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Rank-1, (I)
!
!         SST                      Ocean surface temperature
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar
!
!         Wind_Speed               Ocean surface wind speed
!                                  UNITS:      m/s
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar
!
!
! OUTPUT ARGUMENTS:
!
!         Emissivity:              The surface emissivity at vertical and horizontal polarizations.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  ONE
!
!         dEH_dWindSpeed:          The surface horizontally polarized emissivity derivative to wind speed.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar
!
!         dEV_dWindSpeed:          The surface vertically polarized emissivity derivative to wind speed.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar
!
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
!M-
!------------------------------------------------------------------------------------------------------------

  SUBROUTINE Fastem1(Frequency,                                         & ! INPUT
                     Sat_Zenith_Angle,                                  & ! INPUT
                     SST,                                               & ! INPUT
                     Wind_Speed,                                        & ! INPUT
                     Emissivity,                                        & ! OUTPUT
                     dEH_dWindSpeed,                                    & ! OUTPUT)
                     dEV_dWindSpeed)                                      ! OUTPUT)
! ---------------------------------------------------------------------------------------------------
!
  REAL( fp ), INTENT( IN ) ::  Frequency, Sat_Zenith_Angle
  REAL( fp ), INTENT( IN ) ::  SST, Wind_Speed
  REAL( fp ), INTENT( IN OUT ) :: Emissivity(:), dEH_dWindSpeed, dEV_dWindSpeed

!

! Declare passed variables.

! Declare local variables
  real(fp) zch4,xcorr2v,evertr,ehorzr,xcorr2h,ffoam,zcv2,zcv3
  real(fp) xcorr1,zcv1,zcv4,zch1,zch2,zcv5,zcv6,tau2
  real(fp) wind,ehorz,evert,sec,sec2,freqghz2
  real(fp) u10mps2,usec,tccub,tau1,tc,tcsq,freqghz
  real(fp) u10mps,ps2,pc2,pcc,pss,rvertsi,rverts,rvertsr
  real(fp) rverts5,rhorzs5,xcorr15,ffoam5,evertr5,ehorzr5
  real(fp) perm_real,perm_imag,rhorzsr,zch5,zch6,zch3,rhorzsi
  real(fp) rhorzs,perm_imag2,einf,fen,del2,del1,fen2,perm_real2
  real(fp) perm_imag1,perm_real1,den1,den2
  real(fp) ffoamv,ffoamh,xcorr1h,xcorr1v
  complex(fp) perm1,perm2,rvth,rhth,xperm

!
!          First set constants.  Then perform the calculation.
           wind  = Wind_Speed
           u10mps  = wind
           pcc=cos(Sat_Zenith_Angle * DEGREES_TO_RADIANS)
           pss=sin(Sat_Zenith_Angle * DEGREES_TO_RADIANS)
           ps2=pss*pss
           pc2=pcc*pcc
           freqghz = Frequency
           freqghz2=freqghz*freqghz
           u10mps2=u10mps*u10mps
           sec=one/pcc
           sec2=sec*sec
           usec=u10mps*sec

!          calculate piom (ellison et al.) xperm
!          to calculate xperm of saline water based on piom model.
!          convert from kelvin to centigrate and define quadratic and
!          cubic functions for later polynomials
           tc=SST-273.15_fp
           tcsq=tc*tc
           tccub=tcsq*tc

!          define two relaxation frequencies, tau1 and tau2
           tau1=emc(1)+emc(2)*tc+emc(3)*tcsq
           tau2=emc(4)+emc(5)*tc+emc(6)*tcsq+emc(7)*tccub

!          static xperm estatic=del1+del2+einf
           del1=emc(8)+emc(9)*tc+emc(10)*tcsq+emc(11)*tccub
           del2=emc(12)+emc(13)*tc+emc(14)*tcsq+emc(15)*tccub
           einf=emc(18)+emc(19)*tc

!          calculate xperm using double-debye formula
           fen=two*pi*freqghz*0.001_fp
           fen2=fen**two
           den1=one+fen2*tau1*tau1
           den2=one+fen2*tau2*tau2
           perm_real1=del1/den1
           perm_real2=del2/den2
           perm_imag1=del1*fen*tau1/den1
           perm_imag2=del2*fen*tau2/den2
           perm_real=perm_real1+perm_real2+einf
           perm_imag=perm_imag1+perm_imag2
           xperm=cmplx(perm_real,perm_imag)

!          calculate complex fresnel reflection coefficients
!          to calculate vertical and horizontal polarised reflectivities
!          given xperm at local incidencence angle for all channels
!          and profiles
           perm1 = sqrt(xperm - cmplx(ps2,zero,fp))
           perm2  = xperm*pcc
           rhth = (pcc - perm1)/(pcc + perm1)
           rvth = (perm2 - perm1)/(perm2 + perm1)
           rvertsr=real(rvth,fp)
!           rvertsi=dimag(rvth)
           rvertsi=aimag(rvth)
           rverts=rvertsr*rvertsr+rvertsi*rvertsi
           rhorzsr=real(rhth,fp)
!           rhorzsi=dimag(rhth)
           rhorzsi=aimag(rhth)
           rhorzs=rhorzsr*rhorzsr+rhorzsi*rhorzsi

!          calculate small scale xcorr to reflection coefficients
           xcorr1=exp(emc(21)*u10mps*pc2/freqghz2)

!          calculate large scale geometric correction
!          to calculate a correction to the fresnel reflection coefficients
!          allowing for the presence of large scale roughness

!          jc: six coefficients (constant, u, u^2, sec, sec^2, u*sec)
           zcv1=emc(24)+emc(25)*freqghz+emc(26)*freqghz2
           zcv2=(emc(27)+emc(28)*freqghz+emc(29)*freqghz2)*sec
           zcv3=(emc(30)+emc(31)*freqghz+emc(32)*freqghz2)*sec2
           zcv4=(emc(33)+emc(34)*freqghz+emc(35)*freqghz2)*u10mps
           zcv5=(emc(36)+emc(37)*freqghz+emc(38)*freqghz2)*u10mps2
           zcv6=(emc(39)+emc(40)*freqghz+emc(41)*freqghz2)*usec
           zch1=emc(42)+emc(43)*freqghz+emc(44)*freqghz2
           zch2=(emc(45)+emc(46)*freqghz+emc(47)*freqghz2)*sec
           zch3=(emc(48)+emc(49)*freqghz+emc(50)*freqghz2)*sec2
           zch4=(emc(51)+emc(52)*freqghz+emc(53)*freqghz2)*u10mps
           zch5=(emc(54)+emc(55)*freqghz+emc(56)*freqghz2)*u10mps2
           zch6=(emc(57)+emc(58)*freqghz+emc(59)*freqghz2)*usec

!          calculate correction for this polarisation
           xcorr2v=.01_fp*(zcv1+zcv2+zcv3+zcv4+zcv5+zcv6)
           xcorr2h=.01_fp*(zch1+zch2+zch3+zch4+zch5+zch6)

           evertr=one-rverts*xcorr1+xcorr2v
           ehorzr=one-rhorzs*xcorr1+xcorr2h

!        write(769,'(6f12.6)') xcorr1,xcorr2v,xcorr2h,usec,u10mps,evertr
!          calculate foam emissivity correction
           ffoam=emc(22)*(u10mps**emc(23))
           evert=evertr - ffoam*evertr+ ffoam
           ehorz=ehorzr - ffoam*ehorzr + ffoam

!        write(769,'(6f12.6)') ffoam,evertr,ehorzr,evert,ehorz
           rverts5 = rverts
           rhorzs5 = rhorzs
           xcorr15 = xcorr1
           ffoam5 = ffoam
           evertr5 = evert
           ehorzr5 = ehorz
           Emissivity(1) = evertr5
           Emissivity(2) = ehorzr5

!        write(769,'(5E15.6)') SST,Wind_Speed,Sat_Zenith_Angle,Emissivity(1),Emissivity(2)
!
!          Begin K matrix calculation

!          Combine horizontal and vertical polarizations.
           ehorz = ONE
           evert = ONE
!          calculate corrected emissivity from corrected refectivity
           ehorzr=ehorz - ffoam5*ehorz
           ffoamh =-ehorz*ehorzr5 + ehorz
           evertr=evert - ffoam5*evert
           ffoamv =-evert*evertr5 + evert

!          calculate corrected emissivity from corrected refectivity
           rhorzs = -ehorzr*xcorr15
           xcorr1h = -rhorzs5*ehorzr
           xcorr2h = ehorzr
           rverts = -evertr*xcorr15
           xcorr1v = -rverts5*evertr
           xcorr2v = evertr

!          calculate foam emissivity correction
!          calculate correction for this polarisation
           zch4=.01_fp*xcorr2h
           zch5=.01_fp*xcorr2h
           zch6=.01_fp*xcorr2h
           zcv4=.01_fp*xcorr2v
           zcv5=.01_fp*xcorr2v
           zcv6=.01_fp*xcorr2v

!          calculate large scale geometric correction
!          to calculate a correction to the fresnel reflection coefficients
!          allowing for the presence of large scale roughness

          dEH_dWindSpeed = emc(23)*ffoam5/wind*ffoamh                   +  &
                    zch4*(emc(51)+emc(52)*freqghz+emc(53)*freqghz2)     +  &
                    xcorr1h*emc(21)*pc2/freqghz2*xcorr15           +  &
                    zch6*(emc(57)+emc(58)*freqghz+emc(59)*freqghz2)*sec +  &
                    zch5*(emc(54)+emc(55)*freqghz+emc(56)*freqghz2)*two*wind


          dEV_dWindSpeed = emc(23)*ffoam5/wind*ffoamv                   +  &
                    zcv4*(emc(33)+emc(34)*freqghz+emc(35)*freqghz2)     +  &
                    xcorr1v*emc(21)*pc2/freqghz2*xcorr15           +  &
                    zcv6*(emc(39)+emc(40)*freqghz+emc(41)*freqghz2)*sec +  &
                    zcv5*(emc(36)+emc(37)*freqghz+emc(38)*freqghz2)*two*wind

 END SUBROUTINE Fastem1

END MODULE CRTM_Fastem1
