!
! NESDIS_AMSRE_SICEEM_Module
!
! Module containing the AMSR-E microwave sea ice emissivity model
!
! References:
!       Yan,B., F.Weng and K.Okamoto,2004: "A microwave snow emissivity model",
!         8th Specialist Meeting on Microwave Radiometry and Remote Sensing Applications,
!         24-27 February, 2004, Rome, Italy.
!
!
! CREATION HISTORY:
!       Written by:     Banghua Yan, 28-May-2005
!                       banghua.yan@noaa.gov
!                       Fuzhong Weng
!                       fuzhong.weng@noaa.gov
!

MODULE NESDIS_AMSRE_SICEEM_Module


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds, ONLY: fp
  USE NESDIS_LandEM_Module
  ! Disable implicit typing
  IMPLICIT NONE

  
  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: NESDIS_AMSRE_SSICEEM


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: NESDIS_AMSRE_SICEEM_Module.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'


  INTEGER, PUBLIC, PARAMETER                          :: N_FREQ= 7

  REAL(fp), PUBLIC, PARAMETER, DIMENSION (N_FREQ)    ::                                &
  FREQUENCY_AMSREALG = (/ 6.925_fp, 10.65_fp, 18.7_fp,23.8_fp,          &
                        36.5_fp, 89.0_fp,157._fp/)

!Define 13 weighted sea ice emissivity spectra

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 RS_ICE_A_EMISS=(/0.93_fp, 0.94_fp, 0.96_fp, 0.97_fp, 0.97_fp,    &
                  0.94_fp, 0.93_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 RS_ICE_B_EMISS=(/0.86_fp, 0.87_fp, 0.90_fp, 0.91_fp, 0.90_fp,    &
                  0.90_fp, 0.89_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 MIXED_NEWICE_SNOW_EMISS=(/0.88_fp, 0.88_fp, 0.89_fp, 0.88_fp,         &
                           0.87_fp, 0.84_fp, 0.82_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 NARE_NEWICE_EMISS=(/0.80_fp, 0.81_fp, 0.81_fp, 0.81_fp, 0.80_fp, &
                     0.79_fp, 0.79_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 BROKEN_ICE_EMISS=(/0.75_fp, 0.78_fp, 0.80_fp, 0.81_fp, 0.80_fp,  &
                    0.77_fp, 0.74_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 FIRST_YEAR_ICE_EMISS=(/0.93_fp, 0.93_fp, 0.92_fp, 0.92_fp,            &
                        0.89_fp, 0.78_fp, 0.69_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 COMPOSITE_PACK_ICE_EMISS=(/0.89_fp, 0.88_fp, 0.87_fp, 0.85_fp,        &
                            0.82_fp, 0.69_fp, 0.59_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 RS_ICE_C_EMISS  =(/0.92_fp, 0.90_fp, 0.83_fp, 0.78_fp, 0.73_fp,  &
                    0.62_fp, 0.58_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 FAST_ICE_EMISS  =(/0.85_fp, 0.85_fp, 0.84_fp, 0.81_fp, 0.78_fp,  &
                    0.63_fp, 0.56_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 RS_ICE_D_EMISS  =(/0.76_fp, 0.76_fp, 0.76_fp, 0.76_fp, 0.74_fp,  &
                    0.65_fp, 0.60_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 RS_ICE_E_EMISS  =(/0.63_fp, 0.65_fp, 0.67_fp, 0.68_fp, 0.70_fp,  &
                    0.74_fp, 0.75_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 RS_ICE_F_EMISS  =(/0.54_fp, 0.60_fp, 0.64_fp, 0.67_fp, 0.70_fp,  &
                    0.71_fp, 0.72_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 GREASE_ICE_EMISS=(/0.49_fp, 0.51_fp, 0.53_fp, 0.55_fp, 0.58_fp,  &
                    0.65_fp, 0.67_fp/)



!Define 13  sea ice V-POL emissivity spectra


 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 RS_ICE_A_EV=(/ 0.96_fp, 0.97_fp, 0.99_fp, 0.99_fp, 0.99_fp,      &
                0.98_fp, 0.97_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 RS_ICE_B_EV=(/0.95_fp, 0.96_fp, 0.99_fp, 0.98_fp, 0.97_fp,       &
               0.94_fp, 0.93_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 MIXED_NEWICE_SNOW_EV=(/0.96_fp, 0.96_fp, 0.95_fp, 0.94_fp,            &
                        0.93_fp, 0.88_fp, 0.86_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 NARE_NEWICE_EV=(/0.88_fp, 0.89_fp, 0.91_fp, 0.91_fp, 0.91_fp,    &
                  0.88_fp, 0.88_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 BROKEN_ICE_EV=(/0.85_fp, 0.87_fp, 0.91_fp, 0.91_fp, 0.91_fp,     &
                 0.87_fp, 0.84_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 FIRST_YEAR_ICE_EV=(/0.98_fp, 0.98_fp, 0.98_fp, 0.97_fp, 0.95_fp, &
                     0.84_fp, 0.75_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 COMPOSITE_PACK_ICE_EV=(/0.98_fp, 0.97_fp, 0.95_fp, 0.93_fp,           &
                         0.89_fp, 0.72_fp, 0.62_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 RS_ICE_C_EV  =(/0.99_fp, 0.96_fp, 0.90_fp, 0.86_fp, 0.75_fp,     &
                 0.66_fp, 0.62_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 FAST_ICE_EV  =(/0.95_fp, 0.95_fp, 0.94_fp, 0.91_fp, 0.85_fp,     &
                 0.69_fp, 0.62_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 RS_ICE_D_EV  =(/0.87_fp, 0.87_fp, 0.88_fp, 0.88_fp, 0.88_fp,     &
                 0.77_fp, 0.72_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 RS_ICE_E_EV  =(/0.77_fp, 0.78_fp, 0.81_fp, 0.82_fp, 0.84_fp,     &
                 0.86_fp, 0.88_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 RS_ICE_F_EV  =(/0.71_fp, 0.73_fp, 0.77_fp, 0.78_fp, 0.81_fp,     &
                 0.86_fp, 0.87_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 GREASE_ICE_EV=(/0.66_fp, 0.67_fp, 0.70_fp, 0.72_fp, 0.76_fp,     &
                 0.82_fp, 0.84_fp/)


!Define 13  sea ice H-POL emissivity spectra


 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 RS_ICE_A_EH=(/ 0.88_fp,  0.92_fp,  0.94_fp,  0.94_fp,  0.95_fp,  &
                0.92_fp,  0.91_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 RS_ICE_B_EH=(/0.81_fp,  0.82_fp,  0.85_fp,  0.86_fp,  0.87_fp,   &
               0.88_fp,  0.87_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 MIXED_NEWICE_SNOW_EH=(/0.83_fp,  0.84_fp,  0.86_fp,  0.85_fp,         &
                        0.84_fp,  0.82_fp,  0.80_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 NARE_NEWICE_EH=(/0.74_fp,  0.75_fp,  0.76_fp,  0.76_fp,               &
                  0.77_fp,  0.73_fp,  0.73_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 BROKEN_ICE_EH=(/0.71_fp,  0.73_fp,  0.76_fp,  0.77_fp,                &
                 0.80_fp,  0.72_fp,  0.69_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 FIRST_YEAR_ICE_EH=(/0.91_fp,  0.90_fp,  0.89_fp,  0.88_fp,            &
                     0.86_fp,  0.76_fp,  0.67_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 COMPOSITE_PACK_ICE_EH=(/0.85_fp,  0.84_fp,  0.83_fp,  0.82_fp,        &
                         0.79_fp,  0.67_fp,  0.57_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 RS_ICE_C_EH  =(/0.90_fp,  0.87_fp,  0.81_fp,  0.78_fp,                &
                 0.69_fp,  0.60_fp,  0.56_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 FAST_ICE_EH  =(/0.80_fp,  0.80_fp,  0.78_fp,  0.76_fp,                &
                 0.72_fp,  0.60_fp,  0.53_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 RS_ICE_D_EH  =(/0.71_fp,  0.71_fp,  0.70_fp,  0.70_fp,                &
                 0.70_fp,  0.59_fp,  0.54_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 RS_ICE_E_EH  =(/0.55_fp,  0.59_fp,  0.60_fp,  0.61_fp,                &
                 0.62_fp,  0.67_fp,  0.69_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 RS_ICE_F_EH  =(/0.48_fp,  0.51_fp,  0.56_fp,  0.57_fp,                &
                 0.60_fp,  0.64_fp,  0.65_fp/)

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ)  ::                                   &
 GREASE_ICE_EH=(/0.42_fp,  0.42_fp,  0.43_fp,  0.45_fp,                &
                 0.49_fp,  0.54_fp,  0.56_fp/)



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
!       NESDIS_AMSRE_SSICEEM
!
! PURPOSE:
!       Subroutine to simulate microwave emissivity over sea ice conditions from AMSRE measurements
!
! REFERENCES:
!       Yan, B., F. Weng and K.Okamoto,2004: "A microwave snow emissivity model, 8th Specialist Meeting on
!       Microwave Radiometry and Remote Sension Applications,24-27 February, 2004, Rome, Italy.
!
! CATEGORY:
!       CRTM : Surface : MW ICE EM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL NESDIS_AMSRE_SSICEEM
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
!         tv(6):                                                     89    GHz
!
!         TH[1:6]                  AMSRE H-POL Brightness temperatures at six frequencies.
!
!         th(1): Horizontally polarized AMSR-E brighness temperature at 6.925 GHz
!         th(2):                                                        10.65 GHz
!         th(3):                                                        18.7  GHz
!         th(4):                                                        23.8  GHz
!         th(5):                                                       36.5  GHz
!         th(6):                                                       89    GHz
!
!         Ts                       The surface temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar
!
!         Tice                     The sea ice temperature.
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
!       AMSRE_Ice_TB   : Subroutine to calculate the sea ice microwave emissivity from AMSRE TB
!
!       AMSRE_Ice_TBTS : Subroutine to calculate the sea ice microwave emissivity from AMSRE TB & TS
!
!
! PROGRAM HISTORY LOG:
!   2004-09-20  yan,b -  implement the algorithm for sea ice emissivity
!   2005-05-29  yan,b -  modify the code for CRTM
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SensorData argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
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

 subroutine NESDIS_AMSRE_SSICEEM(frequency,                                         &  ! INPUT
                                 User_Angle,                                        &  ! INPUT
                                 tv,                                                &  ! INPUT
                                 th,                                                &  ! INPUT
                                 Ts,                                                &  ! INPUT
                                 Tice,                                              &  ! INPUT
                                 Emissivity_H,                                      &  ! OUTPUT
                                 Emissivity_V)                                         ! OUTPUT


real(fp),parameter    :: Satellite_Angle = 55.0

integer,parameter :: nch = 6

real(fp) :: Ts,Tice,frequency,User_Angle,em_vector(2),tv(nch),th(nch)

real(fp) :: esh1,esv1,esh2,esv2,desh,desv,dem

real(fp), intent (out) :: Emissivity_V,Emissivity_H

integer           :: ich

!  Initialization

   Emissivity_H = 0.82_fp

   Emissivity_V = 0.85_fp


do ich =1, nch

   if ( tv(ich) .le. 100.0_fp .or. tv(ich) .ge. 330.0_fp) return

   if ( th(ich) .le. 50.0_fp .or. th(ich) .ge. 330.0_fp) return

enddo

! EMISSIVITY AT SATELLITE'S MEASUREMENT ANGLE

if (Tice .le. 100.0_fp .or. Tice .ge. 277.0_fp) Tice = Ts

IF( Ts .le. 100.0_fp .or. Ts .ge. 300.0_fp) THEN

   call AMSRE_Ice_TB(frequency,Satellite_Angle,tv,th,em_vector)

ELSE


  call AMSRE_Ice_TBTS(frequency,Satellite_Angle,tv,th,Ts,Tice,em_vector)

ENDIF

! Get the emissivity angle dependence

  call NESDIS_LandEM(Satellite_Angle,frequency,0.0_fp,0.0_fp,Ts,Tice,0.0_fp,9,13,10.0_fp,esh1,esv1)

  call NESDIS_LandEM(User_Angle,frequency,0.0_fp,0.0_fp,Ts,Tice,0.0_fp,9,13,10.0_fp,esh2,esv2)

  desh = esh1 - esh2

  desv = esv1 - esv2

  dem = ( desh + desv ) * 0.5_fp
! Emissivity at User's Angle

  Emissivity_H = em_vector(1) - dem;  Emissivity_V = em_vector(2)- dem

  if (Emissivity_H > one)         Emissivity_H = one

  if (Emissivity_V > one)         Emissivity_V = one

  if (Emissivity_H < 0.3_fp) Emissivity_H = 0.3_fp

  if (Emissivity_V < 0.3_fp) Emissivity_V = 0.3_fp


 end subroutine NESDIS_AMSRE_SSICEEM

!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PRIVATE MODULE ROUTINES ##                      ##
!##                                                                            ##
!################################################################################
!################################################################################

 subroutine AMSRE_Ice_TB(frequency,theta,tv,th,em_vector)

!**********************************************************************************************
! Programmer:
!
!     Banghua Yan and Fuzhong Weng   ORG: NESDIS              Date: 2004-09-20
!
! Abstract:
!
!     Simulate emissivity between 5.0 and 150 GHz from AMSR-E Measurements over sea ice conditions
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

  integer,parameter :: nch = N_FREQ, ncoe = 6

  real(fp) :: frequency,theta,em_vector(*),tv(*),th(*),freq(nch)

  real(fp) :: ev(nch),eh(nch)

  real(fp)  :: coev(nch*10),coeh(nch*10)

  integer :: ich,i,k,ntype


 data (coev(k),k=1,7)   /7.991290e-002, 4.331683e-003 ,1.084224e-003,-5.157090e-004,-2.933915e-003, 1.851350e-003,-3.274052e-004/
 data (coev(k),k=11,17) /7.158321e-002, 3.816421e-004 ,5.035995e-003,-4.778362e-004,-2.887104e-003, 1.810522e-003,-3.391957e-004/
 data (coev(k),k=21,27) /  3.270859e-002, 4.530001e-004, 1.045393e-003, 3.966585e-003,-3.404226e-003, 2.036799e-003,-4.181631e-004/
 data (coev(k),k=31,37) / -2.700096e-002, 4.569501e-004, 1.030328e-003, 6.318515e-004,-1.718540e-004, 2.480699e-003,-5.160525e-004/
 data (coev(k),k=41,47) /-8.879322e-002 ,5.363322e-004 ,8.525193e-004 ,5.024619e-004,-4.022416e-003 ,6.835571e-003,-5.370956e-004/
 data (coev(k),k=51,57) / -1.957625e-001, 5.532161e-004, 5.157695e-004, 3.238119e-003,-6.930329e-003, 3.032141e-003, 4.195706e-003/

 data (coeh(k),k=1,7)  /  2.021182e-002, 4.262614e-003,-7.804929e-005, 8.426569e-004,-1.493982e-003 ,2.787050e-004,-1.011866e-004/
 data (coeh(k),k=11,17) / 1.653206e-002 ,3.557150e-004 ,3.835310e-003 ,8.627613e-004,-1.488548e-003 ,2.831013e-004,-1.213957e-004/
 data (coeh(k),k=21,27) / -1.152211e-002, 4.087178e-004,-1.815605e-004, 5.274247e-003,-1.944484e-003, 5.112629e-004,-2.291541e-004/
 data (coeh(k),k=31,37) / -7.221824e-002, 5.213008e-004,-3.094664e-004, 1.806239e-003, 1.400489e-003, 1.048577e-003,-3.927701e-004/
 data (coeh(k),k=41,47) / -1.264792e-001, 5.468592e-004,-4.088940e-004, 1.439717e-003,-2.210939e-003, 5.290267e-003,-3.525910e-004/
 data (coeh(k),k=51,57) / -2.145033e-001, 9.816564e-004,-1.162021e-003, 3.202067e-003,-4.162140e-003, 1.595910e-003, 4.212598e-003/


! Initialization

 freq = FREQUENCY_AMSREALG

  ev = 0.9_fp
  eh = 0.9_fp

DO ich = 1, nch-1

   ev(ich) = coev(1+(ich-1)*10)

   eh(ich) = coeh(1+(ich-1)*10)

   DO i=2,ncoe+1

      ev(ich) = ev(ich) + coev((ich-1)*10 + i)*tv(i-1)

      eh(ich) = eh(ich) + coeh((ich-1)*10 + i)*th(i-1)

   ENDDO

ENDDO


! Extrapolate emissivity at 157 GHz based upon various spectrum table


  call siceemiss_extrapolate(ev,eh,theta,ntype)


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

 end subroutine  AMSRE_Ice_TB



 subroutine AMSRE_Ice_TBTS(frequency,theta,tv,th,tskin,tice,em_vector)

!**********************************************************************************************
! Programmer:
!
!     Banghua Yan and Fuzhong Weng   ORG: NESDIS              Date: 2004-09-20
!
! Abstract:
!
!     Simulate emissivity between 5.0 and 150 GHz from AMSR-E Measurements and surface
!
! temperatures over sea ice conditions
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
!    tice   : sea ice temperature
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
!    ntype        : sea ice types
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


  integer,parameter :: nch = N_FREQ, ncoe = 7
  real(fp)     :: ts,tskin,tice,ff,frequency,theta,em_vector(*),tv(*),th(*),freq(nch)
  real(fp)     :: ev(nch),eh(nch),scale_factor
  real(fp)     :: coev(nch*10),coeh(nch*10)

  integer :: ich,i,k,ntype


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

! Initialization

  freq = FREQUENCY_AMSREALG
  ev = 0.9_fp
  eh = 0.9_fp

DO ich = 1, nch-1

   ev(ich) = coev(1+(ich-1)*10)

   eh(ich) = coeh(1+(ich-1)*10)

   DO i=2,ncoe

      ev(ich) = ev(ich) + coev((ich-1)*10 + i)*tv(i-1)

      eh(ich) = eh(ich) + coeh((ich-1)*10 + i)*th(i-1)

   ENDDO

   ff = freq(ich)

   ts = tice + (tskin - tice)*(ff-6.925_fp)/(89.0_fp-6.925_fp)


   ev(ich) = ev(ich) + coev((ich-1)*10 + ncoe + 1)*ts
   eh(ich) = eh(ich) + coeh((ich-1)*10 + ncoe + 1)*ts

ENDDO


! Quality control

  do ich=1,nch-1

     if (ev(ich) .gt. one) scale_factor = one/ev(ich)

 enddo

! ev_seaice_89 <=?

! part of ocean water
! ev_6<ev(36)<ev(89)

  if ( (ev(1) .lt. ev(5)) .and. (ev(5) .lt. ev(6)) .and. (ev(6) .gt. 0.92_fp) ) then

      scale_factor = 0.92_fp /ev(6)

      do ich=1,nch-1
         ev(ich) = ev(ich)*scale_factor
         eh(ich) = eh(ich)*scale_factor
      enddo

  endif


! Extrapolate emissivity at 157 GHz based upon various spectrum table


  call siceemiss_extrapolate(ev,eh,theta,ntype)


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

 end subroutine  AMSRE_Ice_TBTS



subroutine siceemiss_extrapolate(ev,eh,theta,ntype)

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
!    theta  : local zenith angle in degree  (55.0 for AMSR-E)
!
! Output argument lists
!
!    em_vector(1) : horizontally polarization emissivity at a given frequency
!    em_vector(2) : vertically polarization emissivity
!
! Optional Output argument lists:
!
!    ntype        : sea ice types
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

integer,parameter:: nch = N_FREQ,nt=13
real(fp)  :: ev(*), eh(*)
real(fp)  :: ew_tab(nt,nch),ev_tab(nt,nch),eh_tab(nt,nch),freq(nch)
real(fp)  :: emiss(nch-1),theta,angle,cons,sins
real(fp)  :: delt0,delt_l,delt_h,delt_all,dmin
integer :: ich,ip,ntype

! GET EMISSIVITY/FREQUENCY LOOKUP TABLE DATA

 freq = FREQUENCY_AMSREALG

 ew_tab(1,1:N_FREQ) = RS_ICE_A_EMISS(1:N_FREQ)

 ew_tab(2,1:N_FREQ) = RS_ICE_B_EMISS(1:N_FREQ)

 ew_tab(3,1:N_FREQ) = MIXED_NEWICE_SNOW_EMISS(1:N_FREQ)

 ew_tab(4,1:N_FREQ) = NARE_NEWICE_EMISS(1:N_FREQ)

 ew_tab(5,1:N_FREQ) = BROKEN_ICE_EMISS(1:N_FREQ)

 ew_tab(6,1:N_FREQ) = FIRST_YEAR_ICE_EMISS(1:N_FREQ)

 ew_tab(7,1:N_FREQ) = COMPOSITE_PACK_ICE_EMISS(1:N_FREQ)

 ew_tab(8,1:N_FREQ) = RS_ICE_C_EMISS(1:N_FREQ)

 ew_tab(9,1:N_FREQ) = FAST_ICE_EMISS(1:N_FREQ)

 ew_tab(10,1:N_FREQ) = RS_ICE_D_EMISS(1:N_FREQ)

 ew_tab(11,1:N_FREQ) = RS_ICE_E_EMISS(1:N_FREQ)

 ew_tab(12,1:N_FREQ) = RS_ICE_F_EMISS(1:N_FREQ)

 ew_tab(13,1:N_FREQ) = GREASE_ICE_EMISS(1:N_FREQ)


 ev_tab(1,1:N_FREQ) = RS_ICE_A_EV(1:N_FREQ)

 ev_tab(2,1:N_FREQ) = RS_ICE_B_EV(1:N_FREQ)

 ev_tab(3,1:N_FREQ) = MIXED_NEWICE_SNOW_EV(1:N_FREQ)

 ev_tab(4,1:N_FREQ) = NARE_NEWICE_EV(1:N_FREQ)

 ev_tab(5,1:N_FREQ) = BROKEN_ICE_EV(1:N_FREQ)

 ev_tab(6,1:N_FREQ) = FIRST_YEAR_ICE_EV(1:N_FREQ)

 ev_tab(7,1:N_FREQ) = COMPOSITE_PACK_ICE_EV(1:N_FREQ)

 ev_tab(8,1:N_FREQ) = RS_ICE_C_EV(1:N_FREQ)

 ev_tab(9,1:N_FREQ) = FAST_ICE_EV(1:N_FREQ)

 ev_tab(10,1:N_FREQ) = RS_ICE_D_EV(1:N_FREQ)

 ev_tab(11,1:N_FREQ) = RS_ICE_E_EV(1:N_FREQ)

 ev_tab(12,1:N_FREQ) = RS_ICE_F_EV(1:N_FREQ)

 ev_tab(13,1:N_FREQ) = GREASE_ICE_EV(1:N_FREQ)

 eh_tab(1,1:N_FREQ) = RS_ICE_A_EH(1:N_FREQ)

 eh_tab(2,1:N_FREQ) = RS_ICE_B_EH(1:N_FREQ)

 eh_tab(3,1:N_FREQ) = MIXED_NEWICE_SNOW_EH(1:N_FREQ)

 eh_tab(4,1:N_FREQ) = NARE_NEWICE_EH(1:N_FREQ)

 eh_tab(5,1:N_FREQ) = BROKEN_ICE_EH(1:N_FREQ)

 eh_tab(6,1:N_FREQ) = FIRST_YEAR_ICE_EH(1:N_FREQ)

 eh_tab(7,1:N_FREQ) = COMPOSITE_PACK_ICE_EH(1:N_FREQ)

 eh_tab(8,1:N_FREQ) = RS_ICE_C_EH(1:N_FREQ)

 eh_tab(9,1:N_FREQ) = FAST_ICE_EH(1:N_FREQ)

 eh_tab(10,1:N_FREQ) = RS_ICE_D_EH(1:N_FREQ)

 eh_tab(11,1:N_FREQ) = RS_ICE_E_EH(1:N_FREQ)

 eh_tab(12,1:N_FREQ) = RS_ICE_F_EH(1:N_FREQ)

 eh_tab(13,1:N_FREQ) = GREASE_ICE_EH(1:N_FREQ)

!

angle = theta*3.14159_fp/180.0_fp

cons = cos(angle)*cos(angle)

sins = sin(angle)*sin(angle)

do ich = 1, nch-1

   emiss(ich) = ev(ich)*cons + eh(ich)*sins

enddo

! Find a spectrum

! INitialization

delt_l   = 0.0_fp

delt_h   = 0.0_fp

dmin = 0.05_fp

delt0 = 10.0_fp

! Initialization of ntype

  ntype = 2

  if (emiss(5)+0.01_fp .le. emiss(6)) ntype = 11

  if (emiss(5)-0.01_fp .gt. emiss(6)) ntype =  7

do ip = 1,nt

    delt_l = abs(emiss(1)-ew_tab(ip,1))

    delt_h = abs(emiss(6)-ew_tab(ip,6))

    delt_all = 0.0_fp

    do ich=1,nch-1

       delt_all = delt_all + abs(emiss(ich)-ew_tab(ip,ich))

    enddo

   if ( (delt_l .le. dmin) .and. (delt_h .le. dmin+0.02) .and. (delt_all .le. delt0) ) then

        ntype = ip

        delt0 = delt_all

   endif

enddo

ev(nch) = ev(nch-1) - (ev_tab(ntype,nch-1) - ev_tab(ntype,nch))

eh(nch) = eh(nch-1) - (eh_tab(ntype,nch-1) - eh_tab(ntype,nch))


! quality control

  do ich =1, nch

     if (ev(ich) .gt. one) ev(ich) = one

     if (ev(ich) .lt. 0.3_fp) ev(ich) = 0.3_fp

     if (eh(ich) .gt. 0.98_fp) eh(ich) = 0.98_fp

     if (eh(ich) .lt. 0.3_fp) eh(ich) = 0.30_fp

  enddo

end subroutine siceemiss_extrapolate


END MODULE NESDIS_AMSRE_SICEEM_Module
