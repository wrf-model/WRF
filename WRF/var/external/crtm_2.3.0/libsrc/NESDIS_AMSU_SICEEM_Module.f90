!
! NESDIS_AMSU_SICEEM_Module
!
! Module containing the AMSU microwave sea ice emissivity model
!
! References:
!       Yan,B., F.Weng and K.Okamoto,2004: "A microwave snow emissivity model",
!         8th Specialist Meeting on Microwave Radiometry and Remote Sensing Applications,
!         24-27 February, 2004, Rome, Italy.
!
!
! CREATION HISTORY:
!       Written by:     Banghua Yan, 16-May-2005, banghua.yan@noaa.gov
!                       Fuzhong Weng, fuzhong.weng@noaa.gov
!

MODULE NESDIS_AMSU_SICEEM_Module


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
  PUBLIC :: NESDIS_ICEEM_AMSU


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: NESDIS_AMSU_SICEEM_Module.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'


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
!       NESDIS_ICEEM_AMSU
!
! PURPOSE:
!       Subroutine to simulate microwave emissivity over snow/sea ice conditions from AMSU measurements at
!       window channels.
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
!       CALL ICEEM_AMSU
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
!         Satellite_Angle          The local zenith angle in degree for AMSU measurements.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      Degrees
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Rank-1, (I)
!
!         User_Angle               The local angle value in degree user defines.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      Degrees
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Rank-1, (I)
!
!         Tba                      BRIGHTNESS TEMPERATURES AT FOUR AMSU-A WINDOW CHANNELS
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp )
!                                  DIMENSION   4*1 SCALAR
!
!                        WHICH ARE
!                                  tba[1] = TB at 23.8 GHz
!                                  tba[2] = TB at: 31.4 GHz
!                                  tba[3] = TB at 50.3 GHz
!                                  tba[4] = TB at 89 GHz
!
!         Tbb                      BRIGHTNESS TEMPERATURES AT TWO AMSU-B WINDOW CHANNELS
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp )
!                                  DIMENSION   2*1 SCALAR
!
!                         WHICH ARE
!
!                                  tbb[1] = TB at 89 GHz
!                                  tbb[2] = TB at 150 GHz
!
!
!         Ts = Sea Ice Temperature:        The sea ice surface temperature.
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
!      User_theta                User_Angle in radian
!
!      Satellite_theta           Satellite_Angle in radian
!
! CALLS:
!
!     AMSU_IATs           : Subroutine to calculate sea ice emissivity from AMSU-A and Ts
!
!     AMSU_IBTs           : Subroutine to calculate sea ice emissivity from AMSU-B and Ts
!
!
!
! PROGRAM HISTORY LOG:
!   2004-01-01  yan,b   - implement the algorithm for the ice emissivity
!   2004-03-01  yan,b   - modify the code for SSI
!   2005-05-27  yan,b - modify the code for CRTM

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

subroutine  NESDIS_ICEEM_AMSU(Satellite_Angle,                                               & ! INPUT
                              User_Angle,                                                    & ! INPUT
                              frequency,                                                     & ! INPUT
                              Ts,                                                            & ! INPUT
                              tba,                                                           & ! INPUT
                              tbb,                                                           & ! INPUT
                              Emissivity_H,                                                  & ! OUTPUT
                              Emissivity_V)                                                    ! OUTPUT


  integer,PARAMETER ::  AMSU_IATs_ALG    = 1
  integer,PARAMETER ::  AMSU_IBTs_ALG     = 2
  integer, parameter:: nwcha = 4, nwchb = 2, nwch = 5,nalg = 7
  integer :: input_type,i
  real(fp)    :: Satellite_Angle,User_Angle,Satellite_theta,frequency,Ts
  real(fp)    :: em_vector(2),esh1,esv1,esh2,esv2,desh,desv,dem
  real(fp)    :: tba(nwcha),tbb(nwchb)
  real(fp), intent(out) :: Emissivity_H,Emissivity_V


!  Initialization

  em_vector(1) = 0.82_fp

  em_vector(2) = 0.85_fp

  Satellite_theta = User_Angle*pi/180.0_fp

  input_type = 1


! Check available data

  do i=1,nwcha
     if((tba(i) <= 100.0_fp) .or. (tba(i) >= 320.0_fp) ) then
        input_type = 2
        exit
     end if
  end do


  if (input_type .eq. 2) then
     do i=1,nwchb
        if((tbb(i) <= 100.0_fp) .or. (tbb(i) >= 320.0_fp) ) then
           input_type = 3
           exit
        end if
     end do
  endif


  if ((Ts <= 150.0_fp) .or. (Ts >= 280.0_fp) ) Ts = 260.0


! Emissivity at the local zenith angle of satellite measurements

  GET_option: SELECT CASE (input_type)

  CASE (AMSU_IATs_ALG)

     call AMSU_IATs(frequency,tba,Ts,em_vector)

  CASE (AMSU_IBTs_ALG)

     call AMSU_IBTs(Satellite_theta,frequency,tbb,Ts,em_vector)

  END SELECT GET_option


! Get the emissivity angle dependence

  call NESDIS_LandEM(Satellite_Angle,Frequency,0.0_fp,0.0_fp,Ts,Ts,0.0_fp,9,13,2.0_fp,esh1,esv1)

  call NESDIS_LandEM(User_Angle,Frequency,0.0_fp,0.0_fp,Ts,Ts,0.0_fp,9,13,2.0_fp,esh2,esv2)

  desh = esh1 - esh2

  desv = esv1 - esv2

  dem = ( desh + desv ) * 0.5_fp

! Emissivity at User's Angle

  Emissivity_H = em_vector(1) - dem;  Emissivity_V = em_vector(2)- dem

  if (Emissivity_H > one)         Emissivity_H = one

  if (Emissivity_V > one)         Emissivity_V = one

  if (Emissivity_H < 0.3_fp) Emissivity_H = 0.3_fp

  if (Emissivity_V < 0.3_fp) Emissivity_V = 0.3_fp


end subroutine NESDIS_ICEEM_AMSU


subroutine AMSU_IATs(frequency,tba,ts,em_vector)
!------------------------------------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2004-03-01
!
! abstract:
!         Calculate the emissivity discriminators and interpolate/extrapolate
!  emissivity at required frequency with respect to secenery AMSUA & Ts
!
! program history log:
!
! input argument list:
!
!      frequency        -  frequency in GHz
!      theta            -  local zenith angle in radian (not used here)
!      ts               -  surface temperature
!      tba[1] ~ tba[4]  -  brightness temperature at five AMSU-A window channels:
!                              tba[1] : 23.8 GHz
!                              tba[2] : 31.4 GHz
!                              tba[3] : 50.3 GHz
!                              tba[4] : 89   GHz
!
! output argument list:
!
!   em_vector[1] and [2]  -  emissivity at two polarizations.
!                              set esv = esh here and will be updated
!
! important internal variables:
!
!   coe   - fitting coefficients to estimate discriminator at 23.8 ~ 89   GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!------------------------------------------------------------------------------------------------------------

  integer,parameter:: nch =10,nwch = 5,ncoe = 4
  real(fp)    :: tba(*)
  real(fp)    :: em_vector(*),emissivity,ts,frequency,discriminator(nwch)
  integer :: k,ich
  real(fp)    :: coe(100)

  save coe

! Fitting Coefficients Using Tb1, Tb2, Tb4 and Ts
  coe(1:5) = (/ 9.815214e-001_fp,  3.783815e-003_fp,  &
       6.391155e-004_fp, -9.106375e-005_fp, -4.263206e-003_fp/)
  coe(21:25) = (/ 9.047181e-001_fp, -2.782826e-004_fp,  &
       4.664207e-003_fp, -3.121744e-005_fp, -3.976189e-003_fp/)
  coe(41:45) = (/ 1.163853e+000_fp, -1.419205e-003_fp,  &
       5.505238e-003_fp,  1.506867e-003_fp, -6.157735e-003_fp/)
  coe(61:65) = (/  1.020753e+000_fp, -8.666064e-004_fp,  &
       9.624331e-004_fp,  4.878773e-003_fp, -5.055044e-003_fp/)
  coe(81:85) = (/ 1.438246e+000_fp,  5.667756e-004_fp, &
       -2.621972e-003_fp,  5.928146e-003_fp, -5.856687e-003_fp/)
! save coe


! Calculate emissivity discriminators at five AMSU window channels

  do ich = 1, nwch
     discriminator(ich) = coe(1+(ich-1)*20)
     discriminator(ich) = discriminator(ich) + coe((ich-1)*20 + 2)*tba(1)  &
          + coe((ich-1)*20 + 3)*tba(2)  &
          + coe((ich-1)*20 + 4)*tba(4)  &
          + coe( (ich-1)*20 + 5 )*ts
  end do

  call siem_interpolate(frequency,discriminator,emissivity)

  em_vector(1) = emissivity
  em_vector(2) = emissivity


end subroutine AMSU_IATs



subroutine AMSU_IBTs(theta,frequency,tbb,ts,em_vector)
!------------------------------------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:
!
!   prgmmr:Banghua Yan                  org: nesdis              date: 2004-03-01
!
! abstract:
!         Calculate the emissivity discriminators and interpolate/extrapolate
!  emissivity at required frequency with respect to secenery BTs
!
! program history log:
!
! input argument list:
!
!      frequency        -  frequency in GHz
!      theta            -  local zenith angle
!      ts               -  surface temperature in degree
!      tbb[1] ~ tbb[2]  -  brightness temperature at five AMSU-B window channels:
!                              tbb[1] : 89  GHz
!                              tbb[2] : 150 GHz
!
! output argument list:
!
!   em_vector(1) and (2)  -  emissivity at two polarizations.
!                              set esv = esh here and will be updated
!
! important internal variables:
!
!   coe   - fitting coefficients to estimate discriminator at 31.4 ~ 150  GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!------------------------------------------------------------------------------------------------------------

  integer,parameter:: nch =10,nwch = 5,ncoe = 6
  real(fp)    :: tbb(*),theta
  real(fp)    :: em_vector(*),emissivity,ts,frequency,discriminator(nwch)
  integer :: i,k,ich,nvalid_ch
  real(fp)    :: coe(nch*(ncoe+1))

  save coe

! Fitting Coefficients at 31.4 GHz
  coe(1:7) = (/ 2.239429e+000_fp, -2.153967e-002_fp,  &
       5.785736e-005_fp,  1.366728e-002_fp,    &
       -3.749251e-005_fp, -5.128486e-002_fp, -2.184161e-003_fp/)
  coe(11:17) = (/ 1.768085e+000_fp, -1.643430e-002_fp,  &
       4.850989e-005_fp,  1.288753e-002_fp,   &
       -3.628051e-005_fp, -4.751277e-002_fp, -2.580649e-003_fp/)
  coe(21:27) = (/ 8.910227e-001_fp,  6.170706e-003_fp, &
       -3.772921e-006_fp, -4.146567e-004_fp,   &
       -2.208121e-006_fp, -3.163193e-002_fp, -3.863217e-003_fp/)
! save coe

! Calculate emissivity discriminators at five AMSU window channels
  do ich = 1, nwch-2
     discriminator(ich) = coe(1+(ich-1)*10)
     nvalid_ch = 2
     do i=1,nvalid_ch
        discriminator(ich) = discriminator(ich) + coe((ich-1)*10 + 2*i)*tbb(i) + &
             coe((ich-1)*10 + 2*i+1)*tbb(i)*tbb(i)
     end do
     discriminator(ich) = discriminator(ich) +           &
          coe( (ich-1)*10 + (nvalid_ch+1)*2 )*cos(theta)  +   &
          coe( (ich-1)*10 + (nvalid_ch+1)*2 + 1 )*ts
  end do
  discriminator(4) = 9.278287e-001_fp +  5.549908e-003_fp*tbb(1) &
       - 5.728596e-004_fp*tbb(2) -  4.701641e-003_fp*ts
  discriminator(5) = 1.520531e+000_fp + 1.119648e-003_fp*tbb(1) &
       +  4.518667e-003_fp*tbb(2) - 7.744607e-003_fp*ts

  call siem_interpolate(frequency,discriminator,emissivity)

  em_vector(1) = emissivity
  em_vector(2) = emissivity

end subroutine AMSU_IBTs


subroutine  siem_interpolate(frequency,discriminator,emissivity)
!------------------------------------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:
!
!   prgmmr:Banghua Yan                  org: nesdis              date: 2004-03-01
!
! abstract:
!        (1) Find one snow emissivity spectrum to mimic the emission property of the
! realistic snow condition using a set of discrminators
!        (2) Interpolate/extrapolate emissivity at a required frequency
!
! program history log:
!
! input argument list:
!
!      frequency       - frequency in GHz
!      discriminators  - emissivity discriminators at five AMSU-A & B window channels
!            discriminator[1]   :  emissivity discriminator at 23.8 GHz
!            discriminator[2]   :  emissivity discriminator at 31.4 GHz
!            discriminator[3]   :  emissivity discriminator at 50.3 GHz
!            discriminator[4]   :  emissivity discriminator at 89   GHz
!            discriminator[5]   :  emissivity discriminator at 150  GHz
!
!       Note: discriminator(1) and discriminator(3) are missing value in
!            'AMSU-B & Ts','AMUS-B' and 'MODL' options., which are defined to as -999.9,
! output argument list:
!
!   em_vector[1] and [2]  -  emissivity at two polarizations.
!       seaice_type             -  snow type (reference [2])
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!------------------------------------------------------------------------------------------------------------

  integer,parameter:: ncand = 16,nch =5
  integer:: i
  real(fp)   :: frequency,freq(nch),emissivity,discriminator(*)
  freq = (/23.8_fp, 31.4_fp, 50.3_fp,89.0_fp, 150.0_fp/)

! Estimate sea ice emissivity at a required frequency


  do i = 2, nch
     if(frequency < freq(1))   exit
     if(frequency >= freq(nch)) exit
     if(frequency < freq(i)) then
        emissivity = discriminator(i-1) + (discriminator(i)-discriminator(i-1))* &
             (frequency - freq(i-1))/(freq(i) - freq(i-1))
        exit
     end if

  end do

  if(frequency < freq(1))    emissivity = discriminator(1)

! Assume emissivity = constant at frequencies >= 150 GHz

  if (frequency >= freq(nch)) emissivity = discriminator(nch)

end subroutine siem_interpolate

END MODULE NESDIS_AMSU_SICEEM_Module
