!
! NESDIS_AMSRE_SNOWEM_Module
!
! Module containing the AMSR-E microwave snow emissivity model
!
! References:
!       Yan,B., F.Weng and K.Okamoto,2004: "A microwave snow emissivity model",
!         8th Specialist Meeting on Microwave Radiometry and Remote Sensing Applications,
!         24-27 February, 2004, Rome, Italy.
!
! CREATION HISTORY:
!       Written by:     Banghua Yan, 26-May-2005, banghua.yan@noaa.gov
!                       Fuzhong Weng, fuzhong.weng@noaa.gov
!
!       Modified by:    Banghua Yan, 10-Sep-2005
!                       Quanhua Liu, quanhua.liu@noaa.gov
!                       Yong Han, yong.han@noaa.gov
!

MODULE NESDIS_AMSRE_SNOWEM_Module


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds, ONLY: fp
  USE NESDIS_LandEM_Module
  USE NESDIS_SnowEM_Parameters
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: NESDIS_AMSRE_SNOW


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: NESDIS_AMSRE_SNOWEM_Module.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'


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
!       NESDIS_AMSRE_SNOW
!
! PURPOSE:
!       Subroutine to simulate microwave emissivity over snow conditions from AMSRE measurements
!
! REFERENCES:
!       Yan, B., F. Weng and K.Okamoto,2004: "A microwave snow emissivity model, 8th Specialist Meeting on
!       Microwave Radiometry and Remote Sension Applications,24-27 February, 2004, Rome, Italy.
!
! CATEGORY:
!       CRTM : Surface : MW SNOW EM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL NESDIS_AMSRE_SNOW
!
! INPUT ARGUMENTS:
!
!         Frequency                Frequency User defines
!                                  This is the "I" dimension
!                                  UNITS:      GHz
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar
!
!         User_Angle               The angle value user defines (in degree).
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      Degrees
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Rank-1, (I)
!
!         TV[1:6]                  AMSRE V-POL Brightness temperatures at six frequencies.
!
!         tv(1): Vertically polarized AMSR-E brighness temperature at 6.925 GHz
!         tv(2):                                                      10.65 GHz
!         tv(3):                                                      18.7  GHz
!         tv(4):                                                      23.8  GHz
!         tv(5):                                                      36.5  GHz
!         tv(6):                                                      89    GHz
!
!         TH[1:6]                  AMSRE H-POL Brightness temperatures at six frequencies.
!
!         th(1): Horizontally polarized AMSR-E brighness temperature at 6.925 GHz
!         th(2):                                                        10.65 GHz
!         th(3):                                                        18.7  GHz
!         th(4):                                                        23.8  GHz
!         th(5):                                                        36.5  GHz
!         th(6):                                                        89    GHz
!
!         Ts                       The surface temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar
!
!         Tsnow                    The snow temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar
!
!
! OUTPUT ARGUMENTS:
!
!         Emissivity_H:            The surface emissivity at a horizontal polarization.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar
!
!         Emissivity_V:            The surface emissivity at a vertical polarization.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar
!
!
! INTERNAL ARGUMENTS:
!
!         Satellite_Angle          The angle values of AMSRE measurements (in degree).
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      Degrees
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Rank-1, (I)
!
!
! CALLS:
!
!       AMSRE_Snow_TB   : Subroutine to calculate the snow microwave emissivity from AMSRE TB
!
!       AMSRE_Snow_TBTS : Subroutine to calculate the snow microwave emissivity from AMSRE TB & TS
!
!
! PROGRAM HISTORY LOG:
!   2004-09-20  yan,b -  implement the algorithm for snow emissivity
!   2005-05-29  yan,b -  modify the code for CRTM
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

 subroutine NESDIS_AMSRE_SNOW(Frequency,                              & ! INPUT
                              User_Angle,                             & ! INPUT
                              tv,                                     & ! INPUT
                              th,                                     & ! INPUT
                              Ts,                                     & ! INPUT
                              Tsnow,                                  & ! INPUT
                              Emissivity_H,                           & ! OUTPUT
                              Emissivity_V)                             ! OUTPUT

real(fp),parameter    :: Satellite_Angle = 55.0_fp
integer,parameter :: nch = 6
integer           :: ich
real(fp)              :: Ts,Tsnow,Frequency,User_Angle,em_vector(2),tv(nch),th(nch)
real(fp)              :: esh1,esv1,esh2,esv2,desh,desv,dem
real(fp), intent(out) :: Emissivity_V,Emissivity_H

!  Initialization

   Emissivity_H = 0.82_fp

   Emissivity_V = 0.85_fp


do ich =1, nch

   if ( tv(ich) .le. 100.0_fp .or. tv(ich) .ge. 330.0_fp) return

   if ( th(ich) .le. 50.0_fp .or. th(ich) .ge. 330.0_fp) return

enddo


! EMISSIVITY AT SATELLITE'S MEASUREMENT ANGLE

if (Tsnow .le. 100.0_fp .or. Tsnow .ge. 280.0_fp) Tsnow = Ts

IF( Ts .le. 100.0_fp .or. Ts .ge. 280.0_fp) THEN

   call AMSRE_Snow_TB(Frequency,Satellite_Angle,tv,th,em_vector)

ELSE


  call AMSRE_Snow_TBTS(Frequency,Satellite_Angle,tv,th,Ts,Tsnow,em_vector)

ENDIF



! Get the emissivity angle dependence

  call NESDIS_LandEM(Satellite_Angle,Frequency,0.0_fp,0.0_fp,Ts,Tsnow,0.0_fp,9,13,10.0_fp,esh1,esv1)

  call NESDIS_LandEM(User_Angle,Frequency,0.0_fp,0.0_fp,Ts,Tsnow,0.0_fp,9,13,10.0_fp,esh2,esv2)

  desh = esh1 - esh2

  desv = esv1 - esv2

  dem = ( desh + desv ) * 0.5_fp

! Emissivity at User's Angle

  Emissivity_H = em_vector(1) - dem;  Emissivity_V = em_vector(2)- dem

  if (Emissivity_H > one)         Emissivity_H = one

  if (Emissivity_V > one)         Emissivity_V = one

  if (Emissivity_H < 0.3_fp) Emissivity_H = 0.3_fp

  if (Emissivity_V < 0.3_fp) Emissivity_V = 0.3_fp


 end subroutine NESDIS_AMSRE_SNOW


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PRIVATE MODULE ROUTINES ##                      ##
!##                                                                            ##
!################################################################################
!################################################################################

 subroutine AMSRE_Snow_TB(frequency,theta,tv,th,em_vector)

!**********************************************************************************************
! Programmer:
!
!     Banghua Yan and Fuzhong Weng   ORG: NESDIS              Date: 2004-09-20
!
! Abstract:
!
!     Simulate emissivity between 5.0 and 150 GHz from AMSR-E Measurements over snow conditions
!
! Input argument list:
!
!    tv(1): Vertically polarized AMSR-E brighness temperature at 6.925 GHz
!    tv(2):                                                      10.65 GHz
!    tv(3):                                                      18.7  GHz
!    tv(4):                                                      23.8  GHz
!    tv(5):                                                      36.5  GHz
!    tv(6):                                                     89    GHz

!    th(1): Horizontally polarized AMSR-E brighness temperature at 6.925 GHz
!    th(2):                                                        10.65 GHz
!    th(3):                                                        18.7  GHz
!    th(4):                                                        23.8  GHz
!    th(5):                                                       36.5  GHz
!    th(6):                                                       89    GHz
!
!
!    frequency: frequency in GHz
!
!    theta  : local zenith angle in degree  (55.0 for AMSR-E)
!
!
! Output argument lists
!
!    em_vector(1) : horizontally polarization emissivity
!    em_vector(2) : vertically polarization emissivity
!
! Optional output argument lists:
!
!   ntype         : snow type
!
! Remarks:
!
!  Questions/comments: Please send to Fuzhong.Weng@noaa.gov and Banghua.Yan@noaa.gov
!
! Attributes:
!
!   language: f90
!
!   machine:  ibm rs/6000 sp
!
!*********************************************************************************************

  integer, parameter :: nch = 7, ncoe = 6
  real(fp)   :: frequency,theta,em_vector(*),tv(*),th(*),freq(nch)
  real(fp)   :: ev(nch),eh(nch)
  real(fp)   :: coev(nch*10),coeh(nch*10),coefv(nch*10),coefh(nch*10)

  integer :: ich,i,k,ntype

  logical :: flat_spectra


 data (coev(k),k=1,7)  / 8.840557e-001, 3.561198e-003,-4.302742e-003, 2.649990e-003,-1.912893e-003, 1.341689e-004, 1.877616e-004/
 data (coev(k),k=11,17) / 8.319984e-001,-7.535350e-004, 3.242696e-004, 2.765726e-003,-2.067418e-003, 1.075270e-004, 1.497867e-004/
 data (coev(k),k=21,27) / 7.650196e-001,-8.233568e-004,-3.498505e-003, 7.448430e-003,-2.524732e-003, 1.516532e-004, 2.584488e-005/
 data (coev(k),k=31,37) / 7.228488e-001,-6.230614e-004,-3.799057e-003, 3.749636e-003, 1.399819e-003, 2.698579e-004,-6.742759e-005/
 data (coev(k),k=41,47) / 6.156652e-001,-5.883146e-004,-3.333063e-003, 3.667140e-003,-2.991581e-003, 4.810395e-003,-2.150518e-004/
 data (coev(k),k=51,57) / 3.154989e-001,-3.425970e-004,-1.911767e-003, 6.281439e-003,-6.676328e-003, 9.924634e-004, 4.132920e-003/

 data (coeh(k),k=1,7) / 3.994931e-001, 3.684208e-003,-1.832803e-003, 2.146675e-003,-2.813293e-003, 7.331906e-004, 2.593694e-004/
 data (coeh(k),k=11,17) / 3.840929e-001,-5.748140e-004, 2.487696e-003, 2.379701e-003,-2.890248e-003, 6.132196e-004, 2.242150e-004/
 data (coeh(k),k=21,27) / 3.579773e-001,-5.436757e-004,-1.775741e-003, 7.405810e-003,-3.449545e-003, 5.576746e-004, 1.357095e-004/
 data (coeh(k),k=31,37) / 3.316483e-001,-3.029299e-004,-2.246369e-003, 4.313248e-003,-2.664806e-004, 8.617016e-004, 5.748698e-005/
 data (coeh(k),k=41,47) / 2.695112e-001,-1.114215e-004,-2.342238e-003, 4.295431e-003,-4.323894e-003, 5.142900e-003, 4.303168e-006/
 data (coeh(k),k=51,57) / 1.258819e-001, 1.196912e-004,-1.814465e-003, 8.010936e-003,-8.648453e-003, 1.040926e-003, 4.490778e-003/


!flat tb spectra
data (coefv(k),k=1,7)  /  8.219503e-001, 2.766497e-003,-9.911774e-004, 3.003414e-003,-4.972361e-003,-2.045928e-004, 1.008243e-003/
data (coefv(k),k=11,17) /  7.804756e-001,-1.381776e-003, 3.396505e-003, 2.925542e-003,-4.738939e-003,-3.922246e-004, 9.632953e-004/
data (coefv(k),k=21,27) /  7.235172e-001,-1.210548e-003,-8.104717e-004, 7.129067e-003,-4.618963e-003,-3.342331e-004, 8.263457e-004/
data (coefv(k),k=31,37) /  6.700897e-001,-7.157563e-004,-1.354621e-003, 3.363885e-003,-5.987657e-004,-3.381111e-004, 8.169239e-004/
data (coefv(k),k=41,47) /  5.666955e-001,-8.361950e-004,-6.007613e-004, 2.830450e-003,-4.383359e-003, 4.023651e-003, 5.438970e-004/
data (coefv(k),k=51,57) /  2.368191e-001, 2.217985e-004,-1.561750e-004, 3.289375e-003,-5.068380e-003,-1.738502e-005, 4.539167e-003/

data (coefh(k),k=1,7)  /  4.095972e-001, 2.794753e-003, 7.741484e-004, 4.295706e-003,-7.258374e-003, 1.032814e-003, 5.469676e-004/
data (coefh(k),k=11,17) /  3.856025e-001,-1.316854e-003, 4.839702e-003, 4.288884e-003,-7.353134e-003, 1.263679e-003, 5.608465e-004/
data (coefh(k),k=21,27) /  3.460698e-001,-1.512676e-003, 1.082807e-003, 8.299286e-003,-7.249998e-003, 1.380021e-003, 4.326424e-004/
data (coefh(k),k=31,37) /  3.019493e-001,-1.343551e-003, 1.101839e-003, 4.824021e-003,-4.152114e-003, 1.785116e-003, 3.856031e-004/
data (coefh(k),k=41,47) /  2.322160e-001,-1.220926e-003, 1.058828e-003, 4.418171e-003,-7.636273e-003, 5.860614e-003, 3.927489e-004/
data (coefh(k),k=51,57) /  4.933417e-002,-6.050295e-004, 1.081823e-003, 5.377281e-003,-8.467035e-003, 1.319075e-003, 4.847272e-003/




! Initialization

  freq = FREQUENCY_AMSRE

  ev = 0.9
  eh = 0.9

  flat_spectra =.false.


if ( (abs(th(1)-th(2)) .le. 10.0) .and. (abs(th(1)-th(3)) .le. 10.0) .and.  &

     (abs(th(1)-th(4)) .le. 10.0) .and. (abs(th(1)-th(5)) .le. 10.0) .and.  &

     (abs(th(1)-th(6)) .le. 10.0)  ) flat_spectra =.true.


if (.not. flat_spectra) then

   DO ich = 1, nch-1

      ev(ich) = coev(1+(ich-1)*10)

      eh(ich) = coeh(1+(ich-1)*10)

      DO i=2,ncoe+1

         ev(ich) = ev(ich) + coev((ich-1)*10 + i)*tv(i-1)

         eh(ich) = eh(ich) + coeh((ich-1)*10 + i)*th(i-1)

      ENDDO

    ENDDO

else


   DO ich = 1, nch-1

      ev(ich) = coefv(1+(ich-1)*10)

      eh(ich) = coefh(1+(ich-1)*10)

      DO i=2,ncoe+1

         ev(ich) = ev(ich) + coefv((ich-1)*10 + i)*tv(i-1)

         eh(ich) = eh(ich) + coefh((ich-1)*10 + i)*th(i-1)

      ENDDO

    ENDDO

endif


! Extrapolate emissivity at 150 GHz based upon various spectrum table


  call snowemiss_extrapolate(ev,eh,theta,ntype)


! Interpolate emissivity at a certain frequency

  do ich=1,nch

     if (frequency .le. freq(1)) then

         em_vector(1) = eh(1)

         em_vector(2) = ev(1)

         exit

      endif

     if (frequency .ge. freq(nch)) then

         em_vector(1) = eh(nch)

         em_vector(2) = ev(nch)

         exit

      endif


      if (frequency .le. freq(ich)) then

           em_vector(1) = eh(ich-1) + &

                          (frequency-freq(ich-1))*(eh(ich) - eh(ich-1))/(freq(ich)-freq(ich-1))

           em_vector(2) = ev(ich-1) + &

                          (frequency-freq(ich-1))*(ev(ich) - ev(ich-1))/(freq(ich)-freq(ich-1))

          exit

      endif

  enddo

 end subroutine  AMSRE_Snow_TB



 subroutine AMSRE_Snow_TBTS(frequency,theta,tv,th,tskin,tsnow,em_vector)

!**********************************************************************************************
! Programmer:
!
!     Banghua Yan and Fuzhong Weng   ORG: NESDIS              Date: 2004-09-20
!
! Abstract:
!
!     Simulate emissivity between 5.0 and 150 GHz from AMSR-E Measurements and surface
!
! temperatures over snow conditions
!
! Input argument list:
!
!    tv(1): Vertically polarized AMSR-E brighness temperature at 6.925 GHz
!    tv(2):                                                      10.65 GHz
!    tv(3):                                                      18.7  GHz
!    tv(4):                                                      23.8  GHz
!    tv(5):                                                      36.5  GHz
!    tv(6):                                                     89    GHz

!    th(1): Horizontally polarized AMSR-E brighness temperature at 6.925 GHz
!    th(2):                                                        10.65 GHz
!    th(3):                                                        18.7  GHz
!    th(4):                                                        23.8  GHz
!    th(5):                                                       36.5  GHz
!    th(6):                                                       89    GHz
!
!
!    tskin  : skin temperature  in K
!    tsnow   : snow temperature
!    frequency: frequency in GHz
!    theta  : local zenith angle in degree  (55.0 for AMSR-E)
!
!
! Output argument lists
!
!    em_vector(1) : horizontally polarization emissivity
!    em_vector(2) : vertically polarization emissivity
!
! Optional Output argument lists:
!
!    ntype        : snow types
!
! Remarks:
!
!  Questions/comments: Please send to Fuzhong.Weng@noaa.gov and Banghua.Yan@noaa.gov
!
! Attributes:
!
!   language: f90
!
!   machine:  ibm rs/6000 sp
!
!*********************************************************************************************

 integer, parameter :: nch = 7, ncoe = 7
 real(fp)       :: ts,tskin,tsnow,ff,frequency,theta,em_vector(*),tv(*),th(*),freq(nch)
 real(fp)       :: scale_factor,ev(nch),eh(nch)
 real(fp)       :: coev(nch*10),coeh(nch*10),coefv(nch*10),coefh(nch*10)
 integer    :: ich,i,k,ntype
 logical :: flat_spectra


 data freq/6.925,10.65,18.7,23.8,36.5,89.0,150.0/


 data (coev(k),k=1,8)  / 9.264870e-001, 3.820813e-003, 9.820721e-005,-9.743999e-005, 1.016058e-004,&
               -6.131270e-005,-8.310337e-006,-3.571667e-003/
 data (coev(k),k=11,18) /  8.990802e-001,-1.293630e-004, 4.092177e-003,-1.505218e-004, 1.334235e-004,&
               -5.429488e-005,-1.258312e-005,-3.492572e-003/
 data (coev(k),k=21,28) /  8.927765e-001,-1.042487e-004, 1.094639e-004, 4.123363e-003,-1.447079e-004, &
                1.092342e-004,-4.561015e-005,-3.633246e-003/
 data (coev(k),k=31,38) /  8.806635e-001,-1.486736e-004, 7.260369e-005, 6.746505e-004, 3.348524e-003, &
                4.535990e-004,-1.007199e-004,-3.836484e-003/
 data (coev(k),k=41,48) /  8.367187e-001,-1.257310e-004,-4.742131e-005, 2.337250e-004,-2.282308e-004, &
                4.786773e-003,-5.723923e-005,-3.917339e-003/
 data (coev(k),k=51,58) /  9.211523e-001,-4.696109e-004,-1.854980e-004, 1.344456e-003,-1.323030e-003, &
                6.506959e-004, 5.058027e-003,-4.754969e-003/

 data (coeh(k),k=1,8)  / 7.481983e-001, 3.918302e-003,-2.335011e-005,-1.839539e-005, 1.063956e-004,  &
              -1.205340e-004,-2.554603e-006,-2.909382e-003/
 data (coeh(k),k=11,18) / 7.415185e-001,-1.209328e-005, 3.929017e-003,-4.079404e-005, 1.217362e-004,  &
              -1.115905e-004,-5.050784e-006,-2.904661e-003/
 data (coeh(k),k=21,28) /  7.552724e-001,-3.773039e-005, 7.988982e-006, 4.212262e-003,-2.033198e-004, &
                1.005402e-004,-6.444606e-005,-3.088710e-003/
 data (coeh(k),k=31,38) /  7.822561e-001,-1.710712e-005,-3.380748e-005, 5.470651e-004, 3.367905e-003, &
                5.956915e-004,-1.795434e-004,-3.453751e-003/
 data (coeh(k),k=41,48) /  7.090313e-001,-8.039561e-005, 1.937248e-005, 2.186867e-005,-2.204418e-004, &
                4.859239e-003,-7.098615e-005,-3.406140e-003/
 data (coeh(k),k=51,58) /  8.531166e-001,-3.767062e-004, 2.620507e-004, 3.595719e-004,-1.249933e-003, &
                1.110360e-003, 4.976426e-003,-4.513022e-003/


!flat spectra

data (coefv(k),k=1,8)  /  9.673542e-001, 3.773050e-003, 2.982933e-004,-1.310547e-004,&
               -6.547618e-005, 1.993083e-004, 4.588706e-005,-3.980700e-003/
data (coefv(k),k=11,18) /  9.444128e-001,-4.222268e-004, 4.566803e-003,-7.294824e-005,&
               -4.042911e-005, 7.310942e-006, 7.896336e-005,-3.888028e-003/
data (coefv(k),k=21,28) /  9.421394e-001,-2.707426e-004, 2.003610e-004, 4.169026e-003,&
                2.975004e-005, 9.024655e-005, 3.603073e-005,-4.021325e-003/
data (coefv(k),k=31,38) /  9.366792e-001, 2.520913e-004,-4.105668e-004, 2.993218e-004, &
               4.221681e-003, 1.224792e-004, 5.730433e-005,-4.293167e-003/
data (coefv(k),k=41,48) /  9.075927e-001, 2.239386e-005,-7.102904e-006, 7.015789e-005,&
               -2.269741e-005, 4.491739e-003, 6.655668e-006,-4.192021e-003/
data (coefv(k),k=51,58) /  9.942784e-001, 7.844839e-004,-1.013987e-003, 1.267191e-003,&
               -1.779227e-003, 5.935709e-004, 4.885263e-003,-4.708385e-003/

data (coefh(k),k=1,8)  /  8.603493e-001, 3.890104e-003, 1.506434e-004,-4.509949e-004, &
               8.558589e-004,-2.970560e-004,-1.393584e-004,-3.482814e-003/
data (coefh(k),k=11,18) /  8.450601e-001,-2.547537e-004, 4.212972e-003,-3.644137e-004, &
               5.619098e-004,-3.269107e-006,-9.482537e-005,-3.466812e-003/
data (coefh(k),k=21,28) /  8.444459e-001,-4.808165e-004, 4.215064e-004, 3.659325e-003, &
               5.526873e-004, 1.998726e-004,-1.824186e-004,-3.575493e-003/
data (coefh(k),k=31,38) /  8.732950e-001,-2.407432e-004, 3.569224e-004,-2.240816e-004, &
               4.270996e-003, 5.620209e-004,-2.553201e-004,-3.976354e-003/
data (coefh(k),k=41,48) /  8.312923e-001,-2.531914e-004, 3.071838e-004,-2.332053e-004,&
               -3.701484e-005, 4.884012e-003,-1.277164e-004,-3.879234e-003/
data (coefh(k),k=51,58) /  9.785329e-001, 2.094819e-005, 5.368748e-005, 1.133703e-003,&
               -2.388768e-003, 1.222410e-003, 4.741850e-003,-4.673635e-003/




! Initialization

  ev = 0.9
  eh = 0.9


  flat_spectra =.false.


if ( (abs(th(1)-th(2)) .le. 10.0) .and. (abs(th(1)-th(3)) .le. 10.0) .and.  &

     (abs(th(1)-th(4)) .le. 10.0) .and. (abs(th(1)-th(5)) .le. 10.0) .and.  &

     (abs(th(1)-th(6)) .le. 10.0)  ) flat_spectra =.true.


if (.not. flat_spectra) then


   DO ich = 1, nch-1

      ev(ich) = coev(1+(ich-1)*10)

      eh(ich) = coeh(1+(ich-1)*10)

      DO i=2,ncoe

         ev(ich) = ev(ich) + coev((ich-1)*10 + i)*tv(i-1)

         eh(ich) = eh(ich) + coeh((ich-1)*10 + i)*th(i-1)

      ENDDO

      ff = freq(ich)

      ts = tsnow + (tskin - tsnow)*(ff-6.925)/(89.0-6.925)


      ev(ich) = ev(ich) + coev((ich-1)*10 + ncoe + 1)*ts
      eh(ich) = eh(ich) + coeh((ich-1)*10 + ncoe + 1)*ts

   ENDDO


else

   DO ich = 1, nch-1

      ev(ich) = coefv(1+(ich-1)*10)

      eh(ich) = coefh(1+(ich-1)*10)

      DO i=2,ncoe

         ev(ich) = ev(ich) + coefv((ich-1)*10 + i)*tv(i-1)

         eh(ich) = eh(ich) + coefh((ich-1)*10 + i)*th(i-1)

      ENDDO

      ff = freq(ich)

      ts = tsnow + (tskin - tsnow)*(ff-6.925)/(89.0-6.925)


      ev(ich) = ev(ich) + coefv((ich-1)*10 + ncoe + 1)*ts
      eh(ich) = eh(ich) + coefh((ich-1)*10 + ncoe + 1)*ts

   ENDDO

endif


! Quality control

  do ich=1,nch-1

     if (ev(ich) .gt. 1.0) scale_factor = 1.0/ev(ich)

 enddo


! Extrapolate emissivity at 150 GHz based upon various spectrum table


  call snowemiss_extrapolate(ev,eh,theta,ntype)


! Interpolate emissivity at a certain frequency


  do ich=1,nch

     if (frequency .le. freq(1)) then

         em_vector(1) = eh(1)

         em_vector(2) = ev(1)

         exit

      endif

     if (frequency .ge. freq(nch)) then

         em_vector(1) = eh(nch)

         em_vector(2) = ev(nch)

         exit

      endif


      if (frequency .le. freq(ich)) then

           em_vector(1) = eh(ich-1) + &

                          (frequency-freq(ich-1))*(eh(ich) - eh(ich-1))/(freq(ich)-freq(ich-1))

           em_vector(2) = ev(ich-1) + &

                          (frequency-freq(ich-1))*(ev(ich) - ev(ich-1))/(freq(ich)-freq(ich-1))

          exit

      endif

  enddo



 end subroutine  AMSRE_Snow_TBTS



subroutine snowemiss_extrapolate(ev,eh,theta,ntype)

!**********************************************************************************************
! Programmer:
!
!     Banghua Yan and Fuzhong Weng   ORG: NESDIS              Date: 2004-09-20
!
! Abstract:
!
!     Simulate emissivity at a given frequency based upon various sea ice emissivity look-up tables
!
! Input argument list:
!
!    ev(1): V-POL emissivity  at 6.925 GHz
!    ev(2):                      10.65 GHz
!    ev(3):                      18.7  GHz
!    ev(4):                      23.8  GHz
!    ev(5):                      36.5  GHz
!    ev(6):                      89    GHz

!    eh(1): H-POL emissivity at 6.925 GHz
!    eh(2):                     10.65 GHz
!    eh(3):                     18.7  GHz
!    eh(4):                     23.8  GHz
!    eh(5):                     36.5  GHz
!    eh(6):                     89    GHz
!
!    frequency: frequency in GHz
!
! Output argument lists
!
!    em_vector(1) : horizontally polarization emissivity at a given frequency
!    em_vector(2) : vertically polarization emissivity
!
! Optional Output argument lists:
!
!    ntype        : snow types
!
! Remarks:
!
!  Questions/comments: Please send to Fuzhong.Weng@noaa.gov and Banghua.Yan@noaa.gov
!
! Attributes:
!
!   language: f90
!
!   machine:  ibm rs/6000 sp
!
!*********************************************************************************************

integer, parameter :: nch = 7,nt=16
real(fp)     :: ev(*), eh(*)
real(fp)     :: ew_tab(nt,nch),ev_tab(nt,nch),eh_tab(nt,nch),freq(nch)
real(fp)     :: emiss(nch-1),theta,angle,cons,sins
real(fp)     :: delt0,delt_l,delt_h,delt_all,dmin
integer  :: ich,ip,ntype

! Sixteen candidate snow emissivity spectra

  freq = FREQUENCY_AMSRE

  ew_tab(1, 1: N_FREQ_AMSRE) = WET_SNOW_EM_AMSRE(1:N_FREQ_AMSRE)
  ew_tab(2, 1: N_FREQ_AMSRE) = GRASS_AFTER_SNOW_EM_AMSRE(1:N_FREQ_AMSRE)
  ew_tab(3, 1: N_FREQ_AMSRE) = RS_SNOW_A_EM_AMSRE(1:N_FREQ_AMSRE)
  ew_tab(4, 1: N_FREQ_AMSRE) = POWDER_SNOW_EM_AMSRE(1:N_FREQ_AMSRE)
  ew_tab(5, 1: N_FREQ_AMSRE) = RS_SNOW_B_EM_AMSRE(1:N_FREQ_AMSRE)
  ew_tab(6, 1: N_FREQ_AMSRE) = RS_SNOW_C_EM_AMSRE(1:N_FREQ_AMSRE)
  ew_tab(7, 1: N_FREQ_AMSRE) = RS_SNOW_D_EM_AMSRE(1:N_FREQ_AMSRE)
  ew_tab(8, 1: N_FREQ_AMSRE) = THIN_CRUST_SNOW_EM_AMSRE(1:N_FREQ_AMSRE)
  ew_tab(9, 1: N_FREQ_AMSRE) = RS_SNOW_E_EM_AMSRE(1:N_FREQ_AMSRE)
  ew_tab(10, 1: N_FREQ_AMSRE) = BOTTOM_CRUST_SNOW_A_EM_AMSRE(1:N_FREQ_AMSRE)
  ew_tab(11, 1: N_FREQ_AMSRE) = SHALLOW_SNOW_EM_AMSRE(1:N_FREQ_AMSRE)
  ew_tab(12, 1: N_FREQ_AMSRE) = DEEP_SNOW_EM_AMSRE(1:N_FREQ_AMSRE)
  ew_tab(13, 1: N_FREQ_AMSRE) = CRUST_SNOW_EM_AMSRE(1:N_FREQ_AMSRE)
  ew_tab(14, 1: N_FREQ_AMSRE) = MEDIUM_SNOW_EM_AMSRE(1:N_FREQ_AMSRE)
  ew_tab(15, 1: N_FREQ_AMSRE) = BOTTOM_CRUST_SNOW_B_EM_AMSRE(1:N_FREQ_AMSRE)
  ew_tab(16, 1: N_FREQ_AMSRE) = THICK_CRUST_SNOW_EM_AMSRE(1:N_FREQ_AMSRE)

  ev_tab(1, 1: N_FREQ_AMSRE) = WET_SNOW_EV_AMSRE(1:N_FREQ_AMSRE)
  ev_tab(2, 1: N_FREQ_AMSRE) = GRASS_AFTER_SNOW_EV_AMSRE(1:N_FREQ_AMSRE)
  ev_tab(3, 1: N_FREQ_AMSRE) = RS_SNOW_A_EV_AMSRE(1:N_FREQ_AMSRE)
  ev_tab(4, 1: N_FREQ_AMSRE) = POWDER_SNOW_EV_AMSRE(1:N_FREQ_AMSRE)
  ev_tab(5, 1: N_FREQ_AMSRE) = RS_SNOW_B_EV_AMSRE(1:N_FREQ_AMSRE)
  ev_tab(6, 1: N_FREQ_AMSRE) = RS_SNOW_C_EV_AMSRE(1:N_FREQ_AMSRE)
  ev_tab(7, 1: N_FREQ_AMSRE) = RS_SNOW_D_EV_AMSRE(1:N_FREQ_AMSRE)
  ev_tab(8, 1: N_FREQ_AMSRE) = THIN_CRUST_SNOW_EV_AMSRE(1:N_FREQ_AMSRE)
  ev_tab(9, 1: N_FREQ_AMSRE) = RS_SNOW_E_EV_AMSRE(1:N_FREQ_AMSRE)
  ev_tab(10, 1: N_FREQ_AMSRE) = BOTTOM_CRUST_SNOW_A_EV_AMSRE(1:N_FREQ_AMSRE)
  ev_tab(11, 1: N_FREQ_AMSRE) = SHALLOW_SNOW_EV_AMSRE(1:N_FREQ_AMSRE)
  ev_tab(12, 1: N_FREQ_AMSRE) = DEEP_SNOW_EV_AMSRE(1:N_FREQ_AMSRE)
  ev_tab(13, 1: N_FREQ_AMSRE) = CRUST_SNOW_EV_AMSRE(1:N_FREQ_AMSRE)
  ev_tab(14, 1: N_FREQ_AMSRE) = MEDIUM_SNOW_EV_AMSRE(1:N_FREQ_AMSRE)
  ev_tab(15, 1: N_FREQ_AMSRE) = BOTTOM_CRUST_SNOW_B_EV_AMSRE(1:N_FREQ_AMSRE)
  ev_tab(16, 1: N_FREQ_AMSRE) = THICK_CRUST_SNOW_EV_AMSRE(1:N_FREQ_AMSRE)

  eh_tab(1, 1: N_FREQ_AMSRE) = WET_SNOW_EH_AMSRE(1:N_FREQ_AMSRE)
  eh_tab(2, 1: N_FREQ_AMSRE) = GRASS_AFTER_SNOW_EH_AMSRE(1:N_FREQ_AMSRE)
  eh_tab(3, 1: N_FREQ_AMSRE) = RS_SNOW_A_EH_AMSRE(1:N_FREQ_AMSRE)
  eh_tab(4, 1: N_FREQ_AMSRE) = POWDER_SNOW_EH_AMSRE(1:N_FREQ_AMSRE)
  eh_tab(5, 1: N_FREQ_AMSRE) = RS_SNOW_B_EH_AMSRE(1:N_FREQ_AMSRE)
  eh_tab(6, 1: N_FREQ_AMSRE) = RS_SNOW_C_EH_AMSRE(1:N_FREQ_AMSRE)
  eh_tab(7, 1: N_FREQ_AMSRE) = RS_SNOW_D_EH_AMSRE(1:N_FREQ_AMSRE)
  eh_tab(8, 1: N_FREQ_AMSRE) = THIN_CRUST_SNOW_EH_AMSRE(1:N_FREQ_AMSRE)
  eh_tab(9, 1: N_FREQ_AMSRE) = RS_SNOW_E_EH_AMSRE(1:N_FREQ_AMSRE)
  eh_tab(10, 1: N_FREQ_AMSRE) = BOTTOM_CRUST_SNOW_A_EH_AMSRE(1:N_FREQ_AMSRE)
  eh_tab(11, 1: N_FREQ_AMSRE) = SHALLOW_SNOW_EH_AMSRE(1:N_FREQ_AMSRE)
  eh_tab(12, 1: N_FREQ_AMSRE) = DEEP_SNOW_EH_AMSRE(1:N_FREQ_AMSRE)
  eh_tab(13, 1: N_FREQ_AMSRE) = CRUST_SNOW_EH_AMSRE(1:N_FREQ_AMSRE)
  eh_tab(14, 1: N_FREQ_AMSRE) = MEDIUM_SNOW_EH_AMSRE(1:N_FREQ_AMSRE)
  eh_tab(15, 1: N_FREQ_AMSRE) = BOTTOM_CRUST_SNOW_B_EH_AMSRE(1:N_FREQ_AMSRE)
  eh_tab(16, 1: N_FREQ_AMSRE) = THICK_CRUST_SNOW_EH_AMSRE(1:N_FREQ_AMSRE)


angle = theta*3.14159/180.0

cons = cos(angle)*cos(angle)

sins = sin(angle)*sin(angle)

do ich = 1, nch-1

   emiss(ich) = ev(ich)*cons + eh(ich)*sins

enddo

! Find a spectrum

! INitialization

delt_l   = 0.0

delt_h   = 0.0

dmin = 0.05

delt0 = 10.0

! Initialization of ntype

  ntype = 3

  if (emiss(6) .le. 0.6) ntype = 15

  if (emiss(6) .le. 0.75 .and. emiss(6) .gt. 0.6) ntype =  11

  if (emiss(6) .le. 0.82 .and. emiss(6) .gt. 0.75) ntype =  6

do ip = 1,nt

    delt_l = abs(emiss(1)-ew_tab(ip,1))

    delt_h = abs(emiss(6)-ew_tab(ip,6))

    delt_all = 0.0

    do ich=1,nch-1

       delt_all = delt_all + abs(emiss(ich)-ew_tab(ip,ich))

    enddo

   if ( (delt_l .le. dmin) .and. (delt_h .le. dmin) .and. (delt_all .le. delt0) ) then

        ntype = ip

        delt0 = delt_all

   endif

enddo

ev(nch) = ev(nch-1) - (ev_tab(ntype,nch-1) - ev_tab(ntype,nch))

eh(nch) = eh(nch-1) - (eh_tab(ntype,nch-1) - eh_tab(ntype,nch))


! quality control

  do ich =1, nch

     if (ev(ich) .gt. 1.0) ev(ich) = 1.0

     if (ev(ich) .lt. 0.3) ev(ich) = 0.3

     if (eh(ich) .gt. 1.0) eh(ich) = 1.0

     if (eh(ich) .lt. 0.3) eh(ich) = 0.3

  enddo

end subroutine snowemiss_extrapolate


END MODULE NESDIS_AMSRE_SNOWEM_Module
