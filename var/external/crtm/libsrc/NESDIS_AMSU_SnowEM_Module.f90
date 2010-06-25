!--------------------------------------------------------------------------------
!M+
! NAME:
!       NESDIS_AMSU_SnowEM_Module
!
! PURPOSE:
!       Module containing the microwave snow emissivity model
!
! REFERENCES:
!       Yan, B., F. Weng and K.Okamoto,2004: "A microwave snow emissivity model, 8th Specialist Meeting on
!
!       Microwave Radiometry and Remote Sension Applications,24-27 February, 2004, Rome, Italy.
!
! CATEGORY:
!       Surface : MW Surface Snow Emissivity from AMSU
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!
!       USE NESDIS_AMSU_SnowEM_Module
!
! MODULES:
!       Type_Kinds:          Module containing definitions for kinds of variable types
!
!       NESDIS_LandEM_Module:Module containing the microwave land emissivity model
!
!       NESDIS_SnowEM_Parameters: Module containing the microwave snow emissivity spectra
!
! CONTAINS:
!
! PUBLIC SUNPROGRAMS:
!
!       NESDIS_AMSU_SNOWEM        : Subroutine to calculate the microwave snow emissivity from AMSU
!
!
! PRIVATE SUBPROGRAMS:
!
!       AMSU_ABTs          : Subroutine to calculate the microwave snow emissivity from AMSU-A/B TB and Ts
!
!       AMSU_ATs           : Subroutine to calculate the microwave snow emissivity from AMSU-A TB and Ts
!
!       AMSU_AB            : Subroutine to calculate the microwave snow emissivity from AMSU-A/B TB
!
!       AMSU_amsua         : Subroutine to calculate the microwave snow emissivity from AMSU-A TB
!
!       AMSU_BTs           : Subroutine to calculate the microwave snow emissivity from AMSU-B TB and Ts
!
!       AMSU_amsub         : Subroutine to calculate the microwave snow emissivity from AMSU-B TB
!
!       AMSU_ALandEM_Snow  : Subroutine to calculate the microwave snow emissivity from Ts and Snow Depth
!
!       em_initialization  : Subroutine to initialization snow emissivity
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
!       Written by:     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov (03-June-2005)
!
!
!       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
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

MODULE NESDIS_AMSU_SnowEM_Module





  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds

  USE NESDIS_LandEM_Module

  USE NESDIS_SnowEM_Parameters

  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------


  PRIVATE



  PUBLIC  :: NESDIS_AMSU_SNOWEM


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
!       NESDIS_AMSU_SNOWEM
!
! PURPOSE:
!       Subroutine to simulate microwave emissivity over snow conditions from AMSU measurements at window
!       channels.
!
! REFERENCES:
!       Yan, B., F. Weng and K.Okamoto,2004: "A microwave snow emissivity model, 8th Specialist Meeting on
!       Microwave Radiometry and Remote Sension Applications,24-27 February, 2004, Rome, Italy.
!
! CATEGORY:
!       CRTM : Surface : MW SNOWEM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL NESDIS_AMSU_SNOWEM
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
!         Satellite_Angle          The local zenith angle in degree for AMSU measurements.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      Degrees
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1, (I)
!
!         User_Angle               The local angle value in degree user defines.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      Degrees
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1, (I)
!
!
!         Tba                      BRIGHTNESS TEMPERATURES AT FOUR AMSU-A WINDOW CHANNELS
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp_kind )
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
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION   2*1 SCALAR
!
!                         WHICH ARE
!
!                                  tbb[1] = TB at 89 GHz
!                                  tbb[2] = TB at 150 GHz
!
!
!         Ts = Land_Temperature:        The land surface temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!
!         Snow_Depth:              The snow depth.
!                                  UNITS:      mm
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
! **** IMPORTANT NOTES ****
!
!        When one variable among  Tba[], Tbb[] and Ts are not available, set -999.0
!
!
!
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
!
! IINTERNAL ARGUMENTS:
!
!       input_type (specific option index = 1 ~ 7):
!
!         input_type = 1 :  AMSU-A & B window channels of Tb and Ts are available (call AMSU_ABTs)
!
!         input_type = 2 :  AMSU-A window channels of Tb and Ts are available     (call AMSU_ATs)
!
!         input_type = 3 :  AMSU-A & B window channels of Tb are available        (call AMSU_AB)
!
!         input_type = 4 :  AMSU-A window channels of Tb are available            (call AMSU_amsua)
!
!         input_type = 5 :  AMSU-B window channels of Tb and Ts are available     (call AMSU_BTs)
!
!         input_type = 6 :  AMSU-B window channels of Tb are available            (call AMSU_amsub)
!
!         input_type = 7 :  snow depth and Ts are available                       (call ALandEM_Snow)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!
!       snow_type  -  snow type (not output here)
!                     1 : Wet Snow
!                     2 : Grass_after_Snow
!                     3 : RS_Snow (A)
!                     4 : Powder Snow
!                     5 : RS_Snow (B)
!                     6 : RS_Snow (C)
!                     7 : RS_Snow (D)
!                     8 : Thin Crust Snow
!                     9 : RS_Snow (E)
!                     10: Bottom Crust Snow (A)
!                     11: Shallow Snow
!                     12: Deep Snow
!                     13: Crust Snow
!                     14: Medium Snow
!                     15: Bottom Crust Snow (B)
!                     16: Thick Crust Snow
!                    999: AMSU measurements are not available or over non-snow conditions
!
! CALLS:
!       AMSU_ABTs          : Subroutine to calculate the microwave snow emissivity from AMSU-A/B TB and Ts
!
!       AMSU_ATs           : Subroutine to calculate the microwave snow emissivity from AMSU-A TB and Ts
!
!       AMSU_AB            : Subroutine to calculate the microwave snow emissivity from AMSU-A/B TB
!
!       AMSU_amsua         : Subroutine to calculate the microwave snow emissivity from AMSU-A TB
!
!       AMSU_BTs           : Subroutine to calculate the microwave snow emissivity from AMSU-B TB and Ts
!
!       AMSU_amsub         : Subroutine to calculate the microwave snow emissivity from AMSU-B TB
!
!       AMSU_ALandEM_Snow  : Subroutine to calculate the microwave snow emissivity from Ts and Snow Depth
!
!       em_initialization  : Subroutine to initialization snow emissivity
!
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
!       Written by:     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov (03-June-2005)
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





subroutine  NESDIS_AMSU_SNOWEM(Satellite_Angle,                             &  ! INPUT
                               User_Angle,                                  &  ! INPUT
                               frequency,                                   &  ! INPUT
                               Snow_Depth,                                  &  ! INPUT
                               Ts,                                          &  ! INPUT
                               tba,                                         &  ! INPUT
                               tbb,                                         &  ! INPUT
                               Emissivity_H,                                &  ! OUPUT
                               Emissivity_V)                                   ! OUTPUT


  use type_kinds, only: ip_kind, fp_kind

  implicit none



  INTEGER(ip_kind),PARAMETER ::  AMSU_ABTs_ALG    = 1

  INTEGER(ip_kind),PARAMETER ::  AMSU_ATs_ALG     = 2

  INTEGER(ip_kind),PARAMETER ::  AMSU_AB_ALG      = 3

  INTEGER(ip_kind),PARAMETER ::  AMSU_amsua_ALG   = 4

  INTEGER(ip_kind),PARAMETER ::  AMSU_BTs_ALG     = 5

  INTEGER(ip_kind),PARAMETER ::  AMSU_amsub_ALG   = 6

  INTEGER(ip_kind),PARAMETER ::  AMSU_ALandEM_Snow_ALG  = 7

  integer(ip_kind), parameter  :: nch = 10, nwcha = 4, nwchb = 2, nwch = 5,nalg = 7

  integer(ip_kind) :: snow_type,input_type,i,np,k

  real(fp_kind)    :: Satellite_Angle,User_Angle,frequency,Ts,Snow_Depth

  real(fp_kind)    :: em_vector(2),esh1,esv1,esh2,esv2,desh,desv,dem

  real(fp_kind)    :: tb(nwch),tba(nwcha),tbb(nwchb)

  real(fp_kind), intent(out) :: Emissivity_H,Emissivity_V

  logical :: INDATA(nalg)



! Initialization

  call em_initialization(frequency,em_vector)

  snow_type  = INVALID_SNOW_TYPE

  input_type = INVALID_SNOW_TYPE

  do k = 1, nalg

     INDATA(k) = .TRUE.

  end do


! Read AMSU & Ts data and set available option

! Get five AMSU-A/B window measurements

  tb(1) = tba(1); tb(2) = tba(2); tb(3) = tba(3); tb(4) = tba(4); tb(5) = tbb(2)

! Check available data

  if((Ts <= 150.0_fp_kind) .or. (Ts >= 290.0_fp_kind) ) then

     INDATA(1:2) = .false.;   INDATA(5)  = .false.;  INDATA(7) = .false.

  end if

  do i=1,nwcha

     if((tba(i) <= 100.0_fp_kind) .or. (tba(i) >= 290.0_fp_kind) ) then

        INDATA(1:4)   = .false.

        exit

     end if

  end do

  do i=1,nwchb

     if((tbb(i) <= 100.0_fp_kind) .or. (tbb(i) >= 290.0_fp_kind) ) then

        INDATA(1)  = .false.;  INDATA(3) = .false.;  INDATA(5:6)  = .false.

        exit

     end if

  end do

  if((Snow_Depth < 0.0_fp_kind) .or. (Snow_Depth >= 3000.0_fp_kind)) INDATA(7) = .false.

  if((frequency >= 80._fp_kind) .and. (INDATA(5))) then

     INDATA(2:3) = .false.

  end if

! Check input type and call a specific Option/subroutine

  do np = 1, nalg

     if (INDATA(np)) then

        input_type = np

        exit

     end if

  end do

! GET EMISSIVITY AT SATELLITE'S ZENITH ANGLE

  GET_option: SELECT CASE (input_type)

  CASE (AMSU_ABTs_ALG)

     call AMSU_ABTs(frequency,tb,Ts,snow_type,em_vector)

  CASE (AMSU_ATs_ALG)

     call AMSU_ATs(frequency,tba,Ts,snow_type,em_vector)

  CASE (AMSU_AB_ALG)

     call AMSU_AB(frequency,tb,snow_type,em_vector)

  CASE (AMSU_amsua_ALG)

     call AMSU_amsua(frequency,tba,snow_type,em_vector)

  CASE(AMSU_BTs_ALG)

     call AMSU_BTs(frequency,tbb,Ts,snow_type,em_vector)

  CASE(AMSU_amsub_ALG)

     call AMSU_amsub(frequency,tbb,snow_type,em_vector)

  CASE(AMSU_ALandEM_Snow_ALG)

     call AMSU_ALandEM_Snow(Satellite_Angle,frequency,Snow_Depth,Ts,snow_type,em_vector)

  END SELECT GET_option

! Get the emissivity angle dependence

  call NESDIS_LandEM(Satellite_Angle,frequency,0.0_fp_kind,0.0_fp_kind,Ts,Ts,2.0_fp_kind,esh1,esv1)

  call NESDIS_LandEM(User_Angle,frequency,0.0_fp_kind,0.0_fp_kind,Ts,Ts,2.0_fp_kind,esh2,esv2)

  desh = esh1 - esh2

  desv = esv1 - esv2

  dem = ( desh + desv ) * 0.5_fp_kind
! Emissivity at User's Angle

  Emissivity_H = em_vector(1) - dem;  Emissivity_V = em_vector(2)- dem

  if (Emissivity_H > one)         Emissivity_H = one

  if (Emissivity_V > one)         Emissivity_V = one

  if (Emissivity_H < 0.3_fp_kind) Emissivity_H = 0.3_fp_kind

  if (Emissivity_V < 0.3_fp_kind) Emissivity_V = 0.3_fp_kind


  return

end subroutine NESDIS_AMSU_SNOWEM


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################


subroutine em_initialization(frequency,em_vector)

!----------------------------------------------------------------------------------------------------------!
!$$$  subprogram documentation block
!
! subprogram:   AMSU-A/B snow emissivity initialization
!
!   prgmmr:  Banghua Yan                org: nesdis              date: 2003-08-18
!
! abstract: AMSU-A/B snow emissivity initialization
!
! program history log:
!
! input argument list:
!
!      frequency   - frequency in GHz
!
! output argument list:
!
!     em_vector[1] and [2]  -  initial emissivity at two polarizations.
!
! important internal variables:
!
!      freq[1~10]  - ten frequencies for sixteen snow types of emissivity
!      em[1~16,*]  - sixteen snow emissivity spectra
!      snow_type   - snow type
!                    where it is initialized to as the type 4,i.e, Powder Snow
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!----------------------------------------------------------------------------------------------------------!

  use type_kinds, only: fp_kind
  USE NESDIS_SnowEM_Parameters
  implicit none

  integer ::  nch,ncand
  Parameter(nch = 10,ncand=16)
  real(fp_kind)    :: frequency,em_vector(*),freq(nch)
  real(fp_kind)    :: em(ncand,nch)
  real(fp_kind)    :: kratio, bconst,emissivity
  integer :: ich

! Sixteen candidate snow emissivity spectra

  em(1, 1: N_FREQUENCY) = WET_SNOW_EMISS(1:N_FREQUENCY)
  em(2, 1: N_FREQUENCY) = GRASS_AFTER_SNOW_EMISS(1:N_FREQUENCY)
  em(3, 1: N_FREQUENCY) = RS_SNOW_A_EMISS(1:N_FREQUENCY)
  em(4, 1: N_FREQUENCY) = POWDER_SNOW_EMISS(1:N_FREQUENCY)
  em(5, 1: N_FREQUENCY) = RS_SNOW_B_EMISS(1:N_FREQUENCY)
  em(6, 1: N_FREQUENCY) = RS_SNOW_C_EMISS(1:N_FREQUENCY)
  em(7, 1: N_FREQUENCY) = RS_SNOW_D_EMISS(1:N_FREQUENCY)
  em(8, 1: N_FREQUENCY) = THIN_CRUST_SNOW_EMISS(1:N_FREQUENCY)
  em(9, 1: N_FREQUENCY) = RS_SNOW_E_EMISS(1:N_FREQUENCY)
  em(10, 1: N_FREQUENCY) = BOTTOM_CRUST_SNOW_A_EMISS(1:N_FREQUENCY)
  em(11, 1: N_FREQUENCY) = SHALLOW_SNOW_EMISS(1:N_FREQUENCY)
  em(12, 1: N_FREQUENCY) = DEEP_SNOW_EMISS(1:N_FREQUENCY)
  em(13, 1: N_FREQUENCY) = CRUST_SNOW_EMISS(1:N_FREQUENCY)
  em(14, 1: N_FREQUENCY) = MEDIUM_SNOW_EMISS(1:N_FREQUENCY)
  em(15, 1: N_FREQUENCY) = BOTTOM_CRUST_SNOW_B_EMISS(1:N_FREQUENCY)
  em(16, 1: N_FREQUENCY) = THICK_CRUST_SNOW_EMISS(1:N_FREQUENCY)


  freq = FREQUENCY_DEFAULT

! Initialization for emissivity at certain frequency
!    In case of no any inputs available for various options
!    A constant snow type & snow emissivity spectrum is assumed
!                    (e.g., powder) snow_type = 4

! Specify snow emissivity at required frequency
  do ich = 2, nch
     if(frequency <  freq(1))   exit
     if(frequency >= freq(nch)) exit
     if(frequency <  freq(ich)) then
        emissivity = em(4,ich-1) + (em(4,ich) - em(4,ich-1))     &
             *(frequency - freq(ich-1))/(freq(ich) - freq(ich-1))
        exit
     end if
  end do

! Extrapolate to lower frequencies than 4.9GHz
  if (frequency <= freq(1)) then
     kratio = (em(4,2) - em(4,1))/(freq(2) - freq(1))
     bconst = em(4,1) - kratio*freq(1)
     emissivity =  kratio*frequency + bconst
     if(emissivity >  one)         emissivity = one
     if(emissivity <= 0.8_fp_kind) emissivity = 0.8_fp_kind
  end if


! Assume emissivity = constant at frequencies >= 150 GHz
  if (frequency >= freq(nch)) emissivity = em(4,nch)
  em_vector(1) = emissivity
  em_vector(2) = emissivity

  return
end subroutine em_initialization



subroutine  em_interpolate(frequency,discriminator,emissivity,snow_type)

!----------------------------------------------------------------------------------------------------------!
!$$$  subprogram documentation block
!
! subprogram:  determine snow_type and calculate emissivity
!
!   prgmmr:Banghua Yan                 org: nesdis              date: 2003-08-18
!
! abstract: 1. Find one snow emissivity spectrum to mimic the emission
!              property of the realistic snow condition using a set of
!              discrminators
!           2. Interpolate/extrapolate emissivity at a required frequency
!
! program history log:
!
! input argument list:
!
!      frequency        - frequency in GHz
!      discriminators   - emissivity discriminators at five AMSU-A & B window
!                         channels
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
!     emissivity  -  weighted emissivity from both V- and H- Pols.
!     snow_type             - snow type
!
! important internal variables:
!
!     freq[1 ~ 10]  -  ten frequencies for sixteen snow types of emissivity
!     em[1~16,*]    -  sixteen snow emissivity spectra
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!----------------------------------------------------------------------------------------------------------!

  use type_kinds, only: fp_kind
  implicit none

  integer,parameter:: ncand = 16,nch =10
  integer:: ich,ichmin,ichmax,i,k,snow_type
  real(fp_kind)   :: dem,demmin0
  real(fp_kind)   :: em(ncand,nch)
  real(fp_kind)   :: frequency,freq(nch),emissivity,discriminator(*),emis(nch)
  real(fp_kind)   :: cor_factor,adjust_check,kratio, bconst

! Sixteen candidate snow emissivity spectra

  em(1, 1: N_FREQUENCY) = WET_SNOW_EMISS(1:N_FREQUENCY)
  em(2, 1: N_FREQUENCY) = GRASS_AFTER_SNOW_EMISS(1:N_FREQUENCY)
  em(3, 1: N_FREQUENCY) = RS_SNOW_A_EMISS(1:N_FREQUENCY)
  em(4, 1: N_FREQUENCY) = POWDER_SNOW_EMISS(1:N_FREQUENCY)
  em(5, 1: N_FREQUENCY) = RS_SNOW_B_EMISS(1:N_FREQUENCY)
  em(6, 1: N_FREQUENCY) = RS_SNOW_C_EMISS(1:N_FREQUENCY)
  em(7, 1: N_FREQUENCY) = RS_SNOW_D_EMISS(1:N_FREQUENCY)
  em(8, 1: N_FREQUENCY) = THIN_CRUST_SNOW_EMISS(1:N_FREQUENCY)
  em(9, 1: N_FREQUENCY) = RS_SNOW_E_EMISS(1:N_FREQUENCY)
  em(10, 1: N_FREQUENCY) = BOTTOM_CRUST_SNOW_A_EMISS(1:N_FREQUENCY)
  em(11, 1: N_FREQUENCY) = SHALLOW_SNOW_EMISS(1:N_FREQUENCY)
  em(12, 1: N_FREQUENCY) = DEEP_SNOW_EMISS(1:N_FREQUENCY)
  em(13, 1: N_FREQUENCY) = CRUST_SNOW_EMISS(1:N_FREQUENCY)
  em(14, 1: N_FREQUENCY) = MEDIUM_SNOW_EMISS(1:N_FREQUENCY)
  em(15, 1: N_FREQUENCY) = BOTTOM_CRUST_SNOW_B_EMISS(1:N_FREQUENCY)
  em(16, 1: N_FREQUENCY) = THICK_CRUST_SNOW_EMISS(1:N_FREQUENCY)


  freq = FREQUENCY_DEFAULT



! Adjust unreasonable discriminator
  if (discriminator(4) > discriminator(2))    &
       discriminator(4) = discriminator(2) +(discriminator(5) - discriminator(2))*  &
       (150.0_fp_kind - 89.0_fp_kind)/(150.0_fp_kind - 31.4_fp_kind)
  if ( (discriminator(3) /= -999.9_fp_kind) .and.       &
       ( ((discriminator(3)-0.01_fp_kind) > discriminator(2)) .or.     &
       ((discriminator(3)-0.01_fp_kind) < discriminator(4)))    )    &
       discriminator(3) = discriminator(2) +  &
       (discriminator(4) - discriminator(2))*(89.0_fp_kind - 50.3_fp_kind) &
       / (89.0_fp_kind - 31.4_fp_kind)

! Find a snow emissivity spectrum
  if(snow_type .eq. -999) then
     demmin0 = 10.0_fp_kind
     do k = 1, ncand
        dem = zero
        ichmin = 1
        ichmax = 3
        if(discriminator(1) == -999.9_fp_kind) then
           ichmin = 2
           ichmax = 2
        end if
        do ich = ichmin,ichmax
           dem = dem + abs(discriminator(ich) - em(k,ich+4))
        end do
        do ich = 4,5
           dem = dem + abs(discriminator(ich) - em(k,ich+5))
        end do
        if (dem < demmin0) then
           demmin0 = dem
           snow_type = k
        end if
     end do
  end if

! Shift snow emissivity according to discriminator at 31.4 GHz
  cor_factor = discriminator(2) - em(snow_type,6)
  do ich = 1, nch
     emis(ich) = em(snow_type,ich) + cor_factor
     if(emis(ich) .gt. one)         emis(ich) = one
     if(emis(ich) .lt. 0.3_fp_kind) emis(ich) = 0.3_fp_kind
  end do

! Emisivity data quality control
  adjust_check = zero
  do ich = 5, 9
     if (ich .le. 7) then
        if (discriminator(ich - 4) .ne. -999.9_fp_kind) &
             adjust_check = adjust_check + abs(emis(ich) - discriminator(ich - 4))
     else
        if (discriminator(ich - 4) .ne. -999.9_fp_kind)  &
             adjust_check = adjust_check + abs(emis(ich+1) - discriminator(ich - 4))
     end if
  end do

  if (adjust_check >= 0.04_fp_kind) then
     if (discriminator(1) /= -999.9_fp_kind) then
        if (discriminator(1) < emis(4)) then
           emis(5) = emis(4) + &
                (31.4_fp_kind - 23.8_fp_kind) * &
                (discriminator(2) - emis(4))/(31.4_fp_kind - 18.7_fp_kind)
        else
           emis(5) = discriminator(1)
        end if
     end if
     emis(6) = discriminator(2)
     if (discriminator(3) /= -999.9_fp_kind) then
        emis(7) = discriminator(3)
     else
!       In case of missing the emissivity discriminator at 50.3 GHz
        emis(7) = emis(6) + (89.0_fp_kind - 50.3_fp_kind) * &
             (discriminator(4) - emis(6))/(89.0_fp_kind - 31.4_fp_kind)
     end if
     emis(8) = emis(7)
     emis(9) = discriminator(4)
     emis(10) = discriminator(5)
  end if

! Estimate snow emissivity at a required frequency
  do i = 2, nch
     if(frequency <  freq(1))   exit
     if(frequency >= freq(nch)) exit
     if(frequency <  freq(i)) then
        emissivity = emis(i-1) + (emis(i) - emis(i-1))*(frequency - freq(i-1))  &
             /(freq(i) - freq(i-1))
        exit
     end if
  end do

! Extrapolate to lower frequencies than 4.9GHz
  if (frequency <= freq(1)) then
     kratio = (emis(2) - emis(1))/(freq(2) - freq(1))
     bconst = emis(1) - kratio*freq(1)
     emissivity =  kratio*frequency + bconst
     if(emissivity > one)          emissivity = one
     if(emissivity <= 0.8_fp_kind) emissivity = 0.8_fp_kind
  end if

! Assume emissivity = constant at frequencies >= 150 GHz
  if (frequency >= freq(nch)) emissivity = emis(nch)

  return
end subroutine em_interpolate


subroutine AMSU_ABTs(frequency,tb,ts,snow_type,em_vector)

!----------------------------------------------------------------------------------------------------------!
!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr:Banghua Yan                  org: nesdis              date: 2003-08-18
!
! abstract:
!         Calculate the emissivity discriminators and interpolate/extrapolate
!  emissivity at a required frequency with respect to secenery ABTs
!
! program history log:
!
! input argument list:
!
!     frequency        -  frequency in GHz
!     theta            -  local zenith angle (currently, not used here)
!     tb[1] ~ tb[5]    -  brightness temperature at five AMSU window channels:
!                              tb[1] : 23.8 GHz
!                              tb[2] : 31.4 GHz
!                              tb[3] : 50.3 GHz
!                              tb[4] : 89.0 GHz
!                              tb[5] : 150  GHz
!
! output argument list:
!
!      em_vector[1] and [2]  -  emissivity at two polarizations.
!                              set esv = esh here and will be updated
!      snow_type        -  snow type
!
! important internal variables:
!
!     nind           -  number of threshold in decision trees
!                          to identify each snow type  ( = 6)
!     em(1~16,*)     -  sixteen snow emissivity spectra
!     DI_coe         -  coefficients to generate six discriminators to describe
!                       the overall emissivity variability within a wider frequency range
!     threshold      -  thresholds in decision trees to identify snow types
!     index_in       -  six indices to discriminate snow type
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!----------------------------------------------------------------------------------------------------------!

  use type_kinds, only: fp_kind
  implicit none

  integer,parameter:: ncand = 16,nch =10,nthresh=38
  integer,parameter:: nind=6,ncoe=8,nLIcoe=6,nHIcoe=12
  integer:: i,j,k,num,npass,snow_type,md0,md1,nmodel(ncand-1)
  real(fp_kind)   :: frequency,tb150,LI,HI,DS1,DS2,DS3
  real(fp_kind)   :: em(ncand,nch), em_vector(*)
  real(fp_kind)   :: tb(*),freq(nch),DTB(nind-1),DI(nind-1),       &
       DI_coe(nind-1,0:ncoe-1),threshold(nthresh,nind),       &
       index_in(nind),threshold0(nind)
  real(fp_kind)   :: LI_coe(0:nLIcoe-1),HI_coe(0:nHIcoe-1)
  real(fp_kind)   :: ts,emissivity
  real(fp_kind)   :: discriminator(5)
  logical:: pick_status,tindex(nind)
  save      threshold,DI_coe,LI_coe, HI_coe,nmodel

  data  nmodel/5,10,13,16,18,24,30,31,32,33,34,35,36,37,38/

! Fitting coefficients for five discriminators
  data (DI_coe(1,k),k=0,ncoe-1)/ &
       3.285557e-002_fp_kind,  2.677179e-005_fp_kind,  &
       4.553101e-003_fp_kind,  5.639352e-005_fp_kind,  &
       -1.825188e-004_fp_kind,  1.636145e-004_fp_kind,  &
       1.680881e-005_fp_kind, -1.708405e-004_fp_kind/
  data (DI_coe(2,k),k=0,ncoe-1)/ &
       -4.275539e-002_fp_kind, -2.541453e-005_fp_kind,  &
       4.154796e-004_fp_kind,  1.703443e-004_fp_kind,  &
       4.350142e-003_fp_kind,  2.452873e-004_fp_kind,  &
       -4.748506e-003_fp_kind,  2.293836e-004_fp_kind/
  data (DI_coe(3,k),k=0,ncoe-1)/ &
       -1.870173e-001_fp_kind, -1.061678e-004_fp_kind,  &
      2.364055e-004_fp_kind, -2.834876e-005_fp_kind,  &
      4.899651e-003_fp_kind, -3.418847e-004_fp_kind,  &
      -2.312224e-004_fp_kind,  9.498600e-004_fp_kind/
  data (DI_coe(4,k),k=0,ncoe-1)/ &
       -2.076519e-001_fp_kind,  8.475901e-004_fp_kind,  &
       -2.072679e-003_fp_kind, -2.064717e-003_fp_kind,  &
       2.600452e-003_fp_kind,  2.503923e-003_fp_kind,  &
       5.179711e-004_fp_kind,  4.667157e-005_fp_kind/
  data (DI_coe(5,k),k=0,ncoe-1)/ &
       -1.442609e-001_fp_kind, -8.075003e-005_fp_kind,  &
       -1.790933e-004_fp_kind, -1.986887e-004_fp_kind,  &
       5.495115e-004_fp_kind, -5.871732e-004_fp_kind,  &
       4.517280e-003_fp_kind,  7.204695e-004_fp_kind/

! Fitting coefficients for emissivity index at 31.4 GHz
  data  LI_coe/ &
       7.963632e-001_fp_kind,  7.215580e-003_fp_kind,  &
       -2.015921e-005_fp_kind, -1.508286e-003_fp_kind,  &
       1.731405e-005_fp_kind, -4.105358e-003_fp_kind/

! Fitting coefficients for emissivity index at 150 GHz
  data  HI_coe/ &
       1.012160e+000_fp_kind,  6.100397e-003_fp_kind, &
       -1.774347e-005_fp_kind, -4.028211e-003_fp_kind, &
       1.224470e-005_fp_kind,  2.345612e-003_fp_kind, &
       -5.376814e-006_fp_kind, -2.795332e-003_fp_kind, &
       8.072756e-006_fp_kind,  3.529615e-003_fp_kind, &
       1.955293e-006_fp_kind, -4.942230e-003_fp_kind/

! Six thresholds for sixteen candidate snow types
! Note: some snow type contains several possible
!      selections for six thresholds

!1 Wet Snow
  data (threshold(1,k),k=1,6)/0.88_fp_kind,0.86_fp_kind,-999.9_fp_kind,&
       0.01_fp_kind,0.01_fp_kind,200._fp_kind/
  data (threshold(2,k),k=1,6)/0.88_fp_kind,0.85_fp_kind,-999.9_fp_kind,&
       0.06_fp_kind,0.10_fp_kind,200._fp_kind/
  data (threshold(3,k),k=1,6)/0.88_fp_kind,0.83_fp_kind,-0.02_fp_kind,&
       0.12_fp_kind,0.16_fp_kind,204._fp_kind/
  data (threshold(4,k),k=1,6)/0.90_fp_kind,0.89_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,-999.9_fp_kind/
  data (threshold(5,k),k=1,6)/0.92_fp_kind,0.85_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,-999.9_fp_kind/

!2 Grass_after_Snow
  data (threshold(6,k),k=1,6)/0.84_fp_kind,0.83_fp_kind,-999.9_fp_kind,&
       0.08_fp_kind,0.10_fp_kind,195._fp_kind/
  data (threshold(7,k),k=1,6)/0.85_fp_kind,0.85_fp_kind,-999.9_fp_kind,&
       0.10_fp_kind,-999.9_fp_kind,190._fp_kind/
  data (threshold(8,k),k=1,6)/0.86_fp_kind,0.81_fp_kind,-999.9_fp_kind,&
       0.12_fp_kind,-999.9_fp_kind,200._fp_kind/
  data (threshold(9,k),k=1,6)/0.86_fp_kind,0.81_fp_kind,0.0_fp_kind,&
       0.12_fp_kind,-999.9_fp_kind,189._fp_kind/
  data (threshold(10,k),k=1,6)/0.90_fp_kind,0.81_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,195._fp_kind/

!3 RS_Snow (A)
  data (threshold(11,k),k=1,6)/0.80_fp_kind,0.76_fp_kind,-999.9_fp_kind,&
       0.05_fp_kind,-999.9_fp_kind,185._fp_kind/
  data (threshold(12,k),k=1,6)/0.82_fp_kind,0.78_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,0.25_fp_kind,180._fp_kind/
  data (threshold(13,k),k=1,6)/0.90_fp_kind,0.76_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,180._fp_kind/

!4 Powder  Snow
  data (threshold(14,k),k=1,6)/0.89_fp_kind,0.73_fp_kind,-999.9_fp_kind,&
       0.20_fp_kind,-999.9_fp_kind,-999.9_fp_kind/
  data (threshold(15,k),k=1,6)/0.89_fp_kind,0.75_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,-999.9_fp_kind/
  data (threshold(16,k),k=1,6)/0.93_fp_kind,0.72_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,-999.9_fp_kind/

!5 RS_Snow (B)
  data (threshold(17,k),k=1,6)/0.82_fp_kind,0.70_fp_kind,-999.9_fp_kind,&
       0.20_fp_kind,-999.9_fp_kind,160._fp_kind/
  data (threshold(18,k),k=1,6)/0.83_fp_kind,0.70_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,160._fp_kind/

!6 RS_Snow (C)
  data (threshold(19,k),k=1,6)/0.75_fp_kind,0.76_fp_kind,-999.9_fp_kind,&
       0.08_fp_kind,-999.9_fp_kind,172._fp_kind/
  data (threshold(20,k),k=1,6)/0.77_fp_kind,0.72_fp_kind,-999.9_fp_kind,&
       0.12_fp_kind,0.15_fp_kind,175._fp_kind/
  data (threshold(21,k),k=1,6)/0.78_fp_kind,0.74_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,0.20_fp_kind,172._fp_kind/
  data (threshold(22,k),k=1,6)/0.80_fp_kind,0.77_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,170._fp_kind/
  data (threshold(23,k),k=1,6)/0.82_fp_kind,-999.9_fp_kind,-999.9_fp_kind,&
       0.15_fp_kind,0.22_fp_kind,170._fp_kind/
  data (threshold(24,k),k=1,6)/0.82_fp_kind,0.73_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,170._fp_kind/

!7 RS_Snow (D)
  data (threshold(25,k),k=1,6)/0.75_fp_kind,0.70_fp_kind,-999.9_fp_kind,&
       0.15_fp_kind,0.25_fp_kind,167._fp_kind/
  data (threshold(26,k),k=1,6)/0.77_fp_kind,0.76_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,-999.9_fp_kind/
  data (threshold(27,k),k=1,6)/0.80_fp_kind,0.72_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,-999.9_fp_kind/
  data (threshold(28,k),k=1,6)/0.77_fp_kind,0.73_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,-999.9_fp_kind/

  data (threshold(29,k),k=1,6)/0.81_fp_kind,0.71_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,-999.9_fp_kind/
  data (threshold(30,k),k=1,6)/0.82_fp_kind,0.69_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,-999.9_fp_kind/

!8 Thin Crust Snow
  data (threshold(31,k),k=1,6)/0.88_fp_kind,0.58_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,-999.9_fp_kind/

!9 RS_Snow (E)
  data (threshold(32,k),k=1,6)/0.73_fp_kind,0.67_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,-999.9_fp_kind/

!10 Bottom Crust Snow (A)
  data (threshold(33,k),k=1,6)/0.83_fp_kind,0.66_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,-999.9_fp_kind/

!11 Shallow Snow
  data (threshold(34,k),k=1,6)/0.82_fp_kind,0.60_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,-999.9_fp_kind/

!12 Deep Snow
  data (threshold(35,k),k=1,6)/0.77_fp_kind,0.60_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,-999.9_fp_kind/

!13 Crust Snow
  data (threshold(36,k),k=1,6)/0.77_fp_kind,0.7_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,-999.9_fp_kind/

!14 Medium Snow
  data (threshold(37,k),k=1,6)/-999.9_fp_kind,0.55_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,-999.9_fp_kind/

!15 Bottom Crust Snow(B)
  data (threshold(38,k),k=1,6)/0.74_fp_kind,-999.9_fp_kind,-999.9_fp_kind,&
       -999.9_fp_kind,-999.9_fp_kind,-999.9_fp_kind/

!16 Thick Crust Snow
! lowest priority: No constraints

! Sixteen candidate snow emissivity spectra

  em(1, 1: N_FREQUENCY) = WET_SNOW_EMISS(1:N_FREQUENCY)
  em(2, 1: N_FREQUENCY) = GRASS_AFTER_SNOW_EMISS(1:N_FREQUENCY)
  em(3, 1: N_FREQUENCY) = RS_SNOW_A_EMISS(1:N_FREQUENCY)
  em(4, 1: N_FREQUENCY) = POWDER_SNOW_EMISS(1:N_FREQUENCY)
  em(5, 1: N_FREQUENCY) = RS_SNOW_B_EMISS(1:N_FREQUENCY)
  em(6, 1: N_FREQUENCY) = RS_SNOW_C_EMISS(1:N_FREQUENCY)
  em(7, 1: N_FREQUENCY) = RS_SNOW_D_EMISS(1:N_FREQUENCY)
  em(8, 1: N_FREQUENCY) = THIN_CRUST_SNOW_EMISS(1:N_FREQUENCY)
  em(9, 1: N_FREQUENCY) = RS_SNOW_E_EMISS(1:N_FREQUENCY)
  em(10, 1: N_FREQUENCY) = BOTTOM_CRUST_SNOW_A_EMISS(1:N_FREQUENCY)
  em(11, 1: N_FREQUENCY) = SHALLOW_SNOW_EMISS(1:N_FREQUENCY)
  em(12, 1: N_FREQUENCY) = DEEP_SNOW_EMISS(1:N_FREQUENCY)
  em(13, 1: N_FREQUENCY) = CRUST_SNOW_EMISS(1:N_FREQUENCY)
  em(14, 1: N_FREQUENCY) = MEDIUM_SNOW_EMISS(1:N_FREQUENCY)
  em(15, 1: N_FREQUENCY) = BOTTOM_CRUST_SNOW_B_EMISS(1:N_FREQUENCY)
  em(16, 1: N_FREQUENCY) = THICK_CRUST_SNOW_EMISS(1:N_FREQUENCY)


  freq = FREQUENCY_DEFAULT


!***  DEFINE SIX DISCRIMINATORS

  dtb(1) = tb(1) - tb(2)
  dtb(2) = tb(2) - tb(4)
  dtb(3) = tb(2) - tb(5)
  dtb(4) = tb(3) - tb(5)
  dtb(5) = tb(4) - tb(5)
  tb150  = tb(5)

  LI = LI_coe(0)
  do i=0,1
     LI = LI + LI_coe(2*i+1)*tb(i+1) + LI_coe(2*i+2)*tb(i+1)*tb(i+1)
  end do
  LI = LI + LI_coe(nLIcoe-1)*ts

  HI = HI_coe(0)
  do i=0,4
     HI = HI + HI_coe(2*i+1)*tb(i+1) + HI_coe(2*i+2)*tb(i+1)*tb(i+1)
  end do
  HI = HI + HI_coe(nHIcoe-1)*ts

  do num=1,nind-1
     DI(num) = DI_coe(num,0) + DI_coe(num,1)*tb(2)
     do i=1,5
        DI(num) = DI(num) + DI_coe(num,1+i)*DTB(i)
     end do
     DI(num) = DI(num) +  DI_coe(num,ncoe-1)*ts
  end do

!*** DEFINE FIVE INDIES
  !HI = DI(0) - DI(3)
  DS1 = DI(1) + DI(2)
  DS2 = DI(4) + DI(5)
  DS3 = DS1 + DS2 + DI(3)

  index_in(1) = LI
  index_in(2) = HI
  index_in(3) = DS1
  index_in(4) = DS2
  index_in(5) = DS3
  index_in(6) = tb150

!*** IDENTIFY SNOW TYPE


! Initialization
  md0 = 1
  snow_type = ncand
  pick_status = .false.

! Pick one snow type
! Check all possible selections for six thresholds for each snow type
  do i = 1, ncand - 1
     md1 = nmodel(i)
     do j = md0, md1
        npass = 0
        do k = 1 , nind
           threshold0(k) = threshold(j,k)
        end do
        CALL six_indices(nind,index_in,threshold0,tindex)

! Corrections
        if((i == 5)  .and. (index_in(2) >  0.75_fp_kind)) tindex(2) = .false.
        if((i == 5)  .and. (index_in(4) >  0.20_fp_kind)                        &
             .and. (index_in(1) >  0.88_fp_kind)) tindex(1) = .false.
        if((i == 10) .and. (index_in(1) <= 0.83_fp_kind)) tindex(1) = .true.
        if((i == 13) .and. (index_in(2) <  0.52_fp_kind)) tindex(2) = .true.
        do k = 1, nind
           if(.not.tindex(k)) exit
           npass = npass + 1
        end do
        if(npass == nind) exit
     end do

     if(npass == nind) then
        pick_status = .true.
        snow_type  = i
     end if
     if(pick_status) exit
     md0 = md1 + 1
  end do

  discriminator(1) = LI + DI(1)
  discriminator(2) = LI
  discriminator(3) = DI(4) + HI
  discriminator(4) = LI - DI(2)
  discriminator(5) = HI

  call em_interpolate(frequency,discriminator,emissivity,snow_type)

  em_vector(1) = emissivity
  em_vector(2) = emissivity

  return
end subroutine AMSU_ABTs



subroutine six_indices(nind,index_in,threshold,tindex)

!----------------------------------------------------------------------------------------------------------!
!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
!
! abstract:
!
! program history log:
!
! input argument list:
!
!      nind        -  Number of threshold in decision trees
!                     to identify each snow type  ( = 6)
!      index_in    -  six indices to discriminate snow type
!      threshold   -  Thresholds in decision trees to identify snow types
!
! output argument list:
!
!      tindex      - state vaiable to show surface snow emissivity feature
!              tindex[ ] = .T.: snow emissivity feature matches the
!                                corresponding threshold for certain snow type
!              tindex[ ] = .F.: snow emissivity feature doesn't match the
!                                corresponding threshold for certain snow type
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!----------------------------------------------------------------------------------------------------------!

  use type_kinds, only: fp_kind
  implicit none

  integer ::  i,nind
  real(fp_kind)    ::  index_in(*),threshold(*)
  logical ::  tindex(*)

  do i=1,nind
     tindex(i) = .false.
     if (threshold(i) .eq. -999.9_fp_kind) then
        tindex(i) = .true.
     else
        if ( (i .le. 2) .or. (i .gt. (nind-1)) ) then
           if (index_in(i) .ge. threshold(i)) tindex(i) = .true.
        else
           if (index_in(i) .le. threshold(i)) tindex(i) = .true.
        end if
     end if
  end do
  return

end subroutine six_indices


subroutine AMSU_AB(frequency,tb,snow_type,em_vector)

!----------------------------------------------------------------------------------------------------------!
!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
!
! abstract:
!         Calculate the emissivity discriminators and interpolate/extrapolate
!  emissivity at required frequency with respect to option AMSUAB
!
! program history log:
!
! input argument list:
!
!      frequency    -  frequency in GHz
!      theta        -  local zenith angle (not used here)
!      tb[1]~tb[5]  -  brightness temperature at five AMSU-A & B window channels:
!                              tb[1] : 23.8 GHz
!                              tb[2] : 31.4 GHz
!                              tb[3] : 50.3 GHz
!                              tb[4] : 89   GHz
!                              tb[5] : 150  GHz
!
! output argument list:
!
!     em_vector[1] and [2] - emissivity at two polarizations.
!                            set esv = esh here and will be updated
!     snow_type       - snow type (reference [2])
!
! important internal variables:
!
!     coe    - fitting coefficients to estimate discriminator at 23.8 ~ 150  GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!----------------------------------------------------------------------------------------------------------!
  use type_kinds, only: fp_kind
  implicit none

  integer,parameter:: nch =10,nwch = 5,ncoe = 10
  real(fp_kind)    :: tb(*),frequency
  real(fp_kind)    :: em_vector(*),emissivity,discriminator(nwch)
  integer :: i,snow_type,k,ich,nvalid_ch
  real(fp_kind)  :: coe(nwch*(ncoe+1))


! Fitting Coefficients at 23.8 GHz: Using Tb1 ~ Tb3
  data (coe(k),k=1,7)/&
       -1.326040e+000_fp_kind,  2.475904e-002_fp_kind, &
       -5.741361e-005_fp_kind, -1.889650e-002_fp_kind, &
       6.177911e-005_fp_kind,  1.451121e-002_fp_kind, &
       -4.925512e-005_fp_kind/

! Fitting Coefficients at 31.4 GHz: Using Tb1 ~ Tb3
  data (coe(k),k=12,18)/ &
       -1.250541e+000_fp_kind,  1.911161e-002_fp_kind, &
       -5.460238e-005_fp_kind, -1.266388e-002_fp_kind, &
       5.745064e-005_fp_kind,  1.313985e-002_fp_kind, &
       -4.574811e-005_fp_kind/

! Fitting Coefficients at 50.3 GHz: Using Tb1 ~ Tb3
  data (coe(k),k=23,29)/  &
       -1.246754e+000_fp_kind,  2.368658e-002_fp_kind, &
       -8.061774e-005_fp_kind, -3.206323e-002_fp_kind, &
       1.148107e-004_fp_kind,  2.688353e-002_fp_kind, &
       -7.358356e-005_fp_kind/

! Fitting Coefficients at 89 GHz: Using Tb1 ~ Tb4
  data (coe(k),k=34,42)/ &
       -1.278780e+000_fp_kind,  1.625141e-002_fp_kind, &
       -4.764536e-005_fp_kind, -1.475181e-002_fp_kind, &
       5.107766e-005_fp_kind,  1.083021e-002_fp_kind, &
       -4.154825e-005_fp_kind,  7.703879e-003_fp_kind, &
       -6.351148e-006_fp_kind/

! Fitting Coefficients at 150 GHz: Using Tb1 ~ Tb5
  data (coe(k),k=45,55)/&
     -1.691077e+000_fp_kind,  3.352403e-002_fp_kind, &
     -7.310338e-005_fp_kind, -4.396138e-002_fp_kind, &
     1.028994e-004_fp_kind,  2.301014e-002_fp_kind, &
     -7.070810e-005_fp_kind,  1.270231e-002_fp_kind, &
     -2.139023e-005_fp_kind, -2.257991e-003_fp_kind, &
     1.269419e-005_fp_kind/

  save coe

! Calculate emissivity discriminators at five AMSU window channels
  do ich = 1, nwch
     discriminator(ich) = coe(1+(ich-1)*11)
     if (ich .le. 3) nvalid_ch = 3
     if (ich .eq. 4) nvalid_ch = 4
     if (ich .eq. 5) nvalid_ch = 5
     do i=1,nvalid_ch
        discriminator(ich) = discriminator(ich) + coe((ich-1)*11 + 2*i)*tb(i) +  &
             coe((ich-1)*11 + 2*i+1)*tb(i)*tb(i)
     end do
  end do
!  Identify one snow emissivity spectrum and interpolate/extrapolate emissivity
!  at a required frequency
  call em_interpolate(frequency,discriminator,emissivity,snow_type)

  em_vector(1) = emissivity
  em_vector(2) = emissivity

  return
end subroutine AMSU_AB


subroutine AMSU_ATs(frequency,tba,ts,snow_type,em_vector)

!-----------------------------------------------------------------------------------------------------------!
!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr:Banghua Yan                 org: nesdis              date: 2003-08-18
!
! abstract:
!         Calculate the emissivity discriminators and interpolate/extrapolate
!  emissivity at required frequency with respect to secenery AMSUAB
!
! program history log:
!
! input argument list:
!
!      frequency        -  frequency in GHz
!      theta            -  local zenith angle (not used here)
!      ts               -  surface temperature
!      tba[1] ~ tba[4]  -  brightness temperature at five AMSU-A window channels:
!                              tba[1] : 23.8 GHz
!                              tba[2] : 31.4 GHz
!                              tba[3] : 50.3 GHz
!                              tba[4] : 89   GHz
! output argument list:
!
!     em_vector[1] and [2]  -  emissivity at two polarizations.
!                              set esv = esh here and will be updated
!     snow_type        -  snow type (reference [2])
!
! important internal variables:
!
!     coe      - fitting coefficients to estimate discriminator at 23.8 ~ 150  GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!-----------------------------------------------------------------------------------------------------------!

  use type_kinds, only: fp_kind
  implicit none

  integer,parameter:: nch =10,nwch = 5,ncoe = 9
  real(fp_kind)    :: tba(*)
  real(fp_kind)    :: em_vector(*),emissivity,ts,frequency,discriminator(nwch)
  integer :: snow_type,i,k,ich,nvalid_ch
  real(fp_kind)  :: coe(nch*(ncoe+1))


! Fitting Coefficients at 23.8 GHz: Using Tb1, Tb2 and Ts
  data (coe(k),k=1,6)/ &
       8.210105e-001_fp_kind,  1.216432e-002_fp_kind,  &
       -2.113875e-005_fp_kind, -6.416648e-003_fp_kind,  &
       1.809047e-005_fp_kind, -4.206605e-003_fp_kind/

! Fitting Coefficients at 31.4 GHz: Using Tb1, Tb2 and Ts
  data (coe(k),k=11,16)/ &
       7.963632e-001_fp_kind,  7.215580e-003_fp_kind,  &
       -2.015921e-005_fp_kind, -1.508286e-003_fp_kind,  &
       1.731405e-005_fp_kind, -4.105358e-003_fp_kind/

! Fitting Coefficients at 50.3 GHz: Using Tb1, Tb2, Tb3 and Ts
  data (coe(k),k=21,28)/ &
       1.724160e+000_fp_kind,  5.556665e-003_fp_kind, &
       -2.915872e-005_fp_kind, -1.146713e-002_fp_kind, &
       4.724243e-005_fp_kind,  3.851791e-003_fp_kind, &
       -5.581535e-008_fp_kind, -5.413451e-003_fp_kind/

! Fitting Coefficients at 89 GHz: Using Tb1 ~ Tb4 and Ts
  data (coe(k),k=31,40)/ &
       9.962065e-001_fp_kind,  1.584161e-004_fp_kind, &
       -3.988934e-006_fp_kind,  3.427638e-003_fp_kind, &
       -5.084836e-006_fp_kind, -6.178904e-004_fp_kind, &
       1.115315e-006_fp_kind,  9.440962e-004_fp_kind, &
       9.711384e-006_fp_kind, -4.259102e-003_fp_kind/

! Fitting Coefficients at 150 GHz: Using Tb1 ~ Tb4 and Ts
  data (coe(k),k=41,50)/ &
       -5.244422e-002_fp_kind,  2.025879e-002_fp_kind,  &
       -3.739231e-005_fp_kind, -2.922355e-002_fp_kind, &
       5.810726e-005_fp_kind,  1.376275e-002_fp_kind, &
       -3.757061e-005_fp_kind,  6.434187e-003_fp_kind, &
       6.190403e-007_fp_kind, -2.944785e-003_fp_kind/

  save coe

! Calculate emissivity discriminators at five AMSU window channels
  DO ich = 1, nwch
     discriminator(ich) = coe(1+(ich-1)*10)
     if (ich .le. 2) nvalid_ch = 2
     if (ich .eq. 3) nvalid_ch = 3
     if (ich .ge. 4) nvalid_ch = 4
     do i=1,nvalid_ch
        discriminator(ich) = discriminator(ich) + coe((ich-1)*10 + 2*i)*tba(i) +  &
             coe((ich-1)*10 + 2*i+1)*tba(i)*tba(i)
     end do
     discriminator(ich) = discriminator(ich) + coe( (ich-1)*10 + (nvalid_ch+1)*2 )*ts
  end do

  call em_interpolate(frequency,discriminator,emissivity,snow_type)

  em_vector(1) = emissivity
  em_vector(2) = emissivity

  return
end subroutine AMSU_ATs


subroutine AMSU_amsua(frequency,tba,snow_type,em_vector)

!------------------------------------------------------------------------------------------------------------!
!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
!
! abstract:
!         Calculate the emissivity discriminators and interpolate/extrapolate
!  emissivity at required frequency with respect to secenery AMSUA
!
! program history log:
!
! input argument list:
!
!      frequency      -  frequency in GHz
!      theta          -  local zenith angle (not used here)
!      tba[1]~tba[4]  -  brightness temperature at five AMSU-A window channels:
!                            tba[1] : 23.8 GHz
!                            tba[2] : 31.4 GHz
!                            tba[3] : 50.3 GHz
!                            tba[4] : 89   GHz
!
! output argument list:
!
!     em_vector(1) and (2)  -  emissivity at two polarizations.
!                              set esv = esh here and will be updated
!     snow_type        -  snow type
!
! important internal variables:
!
!     coe      - fitting coefficients to estimate discriminator at 23.8 ~ 150  GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!--------------------------------------------------------------------------------------------------------!

  use type_kinds, only: fp_kind
  implicit none

  integer,parameter:: nch =10,nwch = 5,ncoe = 8
  real(fp_kind)    :: tba(*)
  real(fp_kind)    :: em_vector(*),emissivity,frequency,discriminator(nwch)
  integer :: snow_type,i,k,ich,nvalid_ch
  real(fp_kind)  :: coe(50)


! Fitting Coefficients at 23.8 GHz: Using Tb1 ~ Tb3
  data (coe(k),k=1,7)/ &
       -1.326040e+000_fp_kind,  2.475904e-002_fp_kind, -5.741361e-005_fp_kind, &
       -1.889650e-002_fp_kind,  6.177911e-005_fp_kind,  1.451121e-002_fp_kind, &
       -4.925512e-005_fp_kind/

! Fitting Coefficients at 31.4 GHz: Using Tb1 ~ Tb3
  data (coe(k),k=11,17)/ &
       -1.250541e+000_fp_kind,  1.911161e-002_fp_kind, -5.460238e-005_fp_kind, &
       -1.266388e-002_fp_kind,  5.745064e-005_fp_kind,  1.313985e-002_fp_kind, &
       -4.574811e-005_fp_kind/

! Fitting Coefficients at 50.3 GHz: Using Tb1 ~ Tb3
  data (coe(k),k=21,27)/ &
       -1.246754e+000_fp_kind,  2.368658e-002_fp_kind, -8.061774e-005_fp_kind, &
       -3.206323e-002_fp_kind,  1.148107e-004_fp_kind,  2.688353e-002_fp_kind, &
       -7.358356e-005_fp_kind/

! Fitting Coefficients at 89 GHz: Using Tb1 ~ Tb4
  data (coe(k),k=31,39)/ &
       -1.278780e+000_fp_kind, 1.625141e-002_fp_kind, -4.764536e-005_fp_kind, &
       -1.475181e-002_fp_kind, 5.107766e-005_fp_kind,  1.083021e-002_fp_kind, &
       -4.154825e-005_fp_kind,  7.703879e-003_fp_kind, -6.351148e-006_fp_kind/

! Fitting Coefficients at 150 GHz: Using Tb1 ~ Tb4
  data (coe(k),k=41,49)/ &
       -1.624857e+000_fp_kind, 3.138243e-002_fp_kind, -6.757028e-005_fp_kind, &
       -4.178496e-002_fp_kind, 9.691893e-005_fp_kind,  2.165964e-002_fp_kind, &
       -6.702349e-005_fp_kind, 1.111658e-002_fp_kind, -1.050708e-005_fp_kind/

  save coe


! Calculate emissivity discriminators at five AMSU window channels
  do ich = 1, nwch
     discriminator(ich) = coe(1+(ich-1)*10)
     if (ich .le. 2) nvalid_ch = 3
     if (ich .ge. 3) nvalid_ch = 4
     do i=1,nvalid_ch
        discriminator(ich) = discriminator(ich) + coe((ich-1)*10 + 2*i)*tba(i) +  &
             coe((ich-1)*10 + 2*i+1)*tba(i)*tba(i)
     end do
  end do

! Quality Control
  if(discriminator(4) .gt. discriminator(2))   &
       discriminator(4) = discriminator(2) + (150.0_fp_kind - 89.0_fp_kind)*  &
       (discriminator(5) - discriminator(2))/ &
       (150.0_fp_kind - 31.4_fp_kind)

! Quality control at 50.3 GHz
  if((discriminator(3) .gt. discriminator(2)) .or.  &
       (discriminator(3) .lt. discriminator(4)))      &
       discriminator(3) = discriminator(2) + (89.0_fp_kind - 50.3_fp_kind)*   &
       (discriminator(4) - discriminator(2))/(89.0_fp_kind - 31.4_fp_kind)

  call em_interpolate(frequency,discriminator,emissivity,snow_type)

  em_vector(1) = emissivity
  em_vector(2) = emissivity

  return
end subroutine AMSU_amsua


subroutine AMSU_BTs(frequency,tbb,ts,snow_type,em_vector)

!-------------------------------------------------------------------------------------------------------!
!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
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
!      theta            -  local zenith angle (not used here)
!      ts               -  surface temperature in degree
!      tbb[1] ~ tbb[2]  -  brightness temperature at five AMSU-B window channels:
!                              tbb[1] : 89  GHz
!                              tbb[2] : 150 GHz
!
! output argument list:
!
!     em_vector(1) and (2)  -  emissivity at two polarizations.
!                              set esv = esh here and will be updated
!     snow_type        -  snow type
!
! important internal variables:
!
!     coe      - fitting coefficients to estimate discriminator at 31.4 ~ 150  GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!-------------------------------------------------------------------------------------------------------!
  use type_kinds, only: fp_kind
  implicit none

  integer,parameter:: nch =10,nwch = 3,ncoe = 5
  real(fp_kind)    :: tbb(*)
  real(fp_kind)    :: em_vector(*),emissivity,ts,frequency,ed0(nwch),discriminator(5)
  integer :: snow_type,i,k,ich,nvalid_ch
  real(fp_kind)  :: coe(nch*(ncoe+1))


! Fitting Coefficients at 31.4 GHz: Using Tb4, Tb5 and Ts
  data (coe(k),k=1,6)/ 3.110967e-001_fp_kind,  1.100175e-002_fp_kind, -1.677626e-005_fp_kind,    &
       -4.020427e-003_fp_kind,  9.242240e-006_fp_kind, -2.363207e-003_fp_kind/
! Fitting Coefficients at 89 GHz: Using Tb4, Tb5 and Ts
  data (coe(k),k=11,16)/  1.148098e+000_fp_kind,  1.452926e-003_fp_kind,  1.037081e-005_fp_kind, &
       1.340696e-003_fp_kind, -5.185640e-006_fp_kind, -4.546382e-003_fp_kind /
! Fitting Coefficients at 150 GHz: Using Tb4, Tb5 and Ts
  data (coe(k),k=21,26)/ 1.165323e+000_fp_kind, -1.030435e-003_fp_kind,  4.828009e-006_fp_kind,  &
       4.851731e-003_fp_kind, -2.588049e-006_fp_kind, -4.990193e-003_fp_kind/
  save coe

! Calculate emissivity discriminators at five AMSU window channels
  do ich = 1, nwch
     ed0(ich) = coe(1+(ich-1)*10)
     nvalid_ch = 2
     do i=1,nvalid_ch
        ed0(ich) = ed0(ich) + coe((ich-1)*10 + 2*i)*tbb(i) +   &
             coe((ich-1)*10 + 2*i+1)*tbb(i)*tbb(i)
     end do
     ed0(ich) = ed0(ich) + coe( (ich-1)*10 + (nvalid_ch+1)*2 )*ts
  end do

! Quality control
  if(ed0(2) .gt. ed0(1))     &
       ed0(2) = ed0(1) + (150.0_fp_kind - 89.0_fp_kind)*(ed0(3) - ed0(1)) / &
       (150.0_fp_kind - 31.4_fp_kind)

! Match the format of the input variable
! Missing value at 23.8 GHz
  discriminator(1) = -999.9_fp_kind;  discriminator(2) = ed0(1)
! Missing value at 50.3 GHz
  discriminator(3) = -999.9_fp_kind; discriminator(4) = ed0(2); discriminator(5) = ed0(3)

  call em_interpolate(frequency,discriminator,emissivity,snow_type)

  em_vector(1) = emissivity
  em_vector(2) = emissivity

  return
end subroutine AMSU_BTs


subroutine AMSU_amsub(frequency,tbb,snow_type,em_vector)


!-------------------------------------------------------------------------------------------------------!
!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
!
! abstract:
!         Calculate the emissivity discriminators and interpolate/extrapolate
!  emissivity at required frequency with respect to secenery AMSUB
!
! program history log:
!
! input argument list:
!
!      frequency        -  frequency in GHz
!      theta            -  local zenith angle (not used here)
!      tbb[1] ~ tbb[2]  -  brightness temperature at five AMSU-B window channels:
!                              tbb[1] : 89  GHz
!                              tbb[2] : 150 GHz
!
! output argument list:
!     em_vector(1) and (2)  -  emissivity at two polarizations.
!                              set esv = esh here and will be updated
!     snow_type        -  snow type (reference [2])
!
! important internal variables:
!
!     coe    - fitting coefficients to estimate discriminator at 31.4 ~ 150  GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!-------------------------------------------------------------------------------------------------------!
  use type_kinds, only: fp_kind
  implicit none

  integer,parameter:: nch =10,nwch = 3,ncoe = 4
  real(fp_kind)    :: tbb(*)
  real(fp_kind)    :: em_vector(*),emissivity,frequency,ed0(nwch),discriminator(5)
  integer :: snow_type,i,k,ich,nvalid_ch
  real(fp_kind)  :: coe(50)


! Fitting Coefficients at 31.4 GHz: Using Tb4, Tb5
  data (coe(k),k=1,5)/-4.015636e-001_fp_kind,9.297894e-003_fp_kind, -1.305068e-005_fp_kind, &
       3.717131e-004_fp_kind, -4.364877e-006_fp_kind/
! Fitting Coefficients at 89 GHz: Using Tb4, Tb5
  data (coe(k),k=11,15)/-2.229547e-001_fp_kind, -1.828402e-003_fp_kind,1.754807e-005_fp_kind, &
       9.793681e-003_fp_kind, -3.137189e-005_fp_kind/
! Fitting Coefficients at 150 GHz: Using Tb4, Tb5
  data (coe(k),k=21,25)/-3.395416e-001_fp_kind,-4.632656e-003_fp_kind,1.270735e-005_fp_kind, &
       1.413038e-002_fp_kind,-3.133239e-005_fp_kind/
  save coe

! Calculate emissivity discriminators at five AMSU window channels
  do ich = 1, nwch
     ed0(ich) = coe(1+(ich-1)*10)
     nvalid_ch = 2
     do i=1,nvalid_ch
        ed0(ich) = ed0(ich) + coe((ich-1)*10 + 2*i)*tbb(i) +  &
             coe((ich-1)*10 + 2*i+1)*tbb(i)*tbb(i)
     end do
  end do

! Quality Control
  if(ed0(2) .gt. ed0(1))     &
       ed0(2) = ed0(1) + (150.0_fp_kind - 89.0_fp_kind) * &
       (ed0(3) - ed0(1))/(150.0_fp_kind - 31.4_fp_kind)

! Match the format of the input variable
! Missing value at 23.8 GHz
  discriminator(1) = -999.9_fp_kind; discriminator(2) = ed0(1)
! Missing value at 50.3 GHz
  discriminator(3) = -999.9_fp_kind; discriminator(4) = ed0(2); discriminator(5) = ed0(3)

  call em_interpolate(frequency,discriminator,emissivity,snow_type)

  em_vector(1) = emissivity
  em_vector(2) = emissivity

  return
end subroutine AMSU_amsub


subroutine AMSU_ALandEM_Snow(theta,frequency,snow_depth,ts,snow_type,em_vector)


!------------------------------------------------------------------------------------------------------------
!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
!
! abstract:
!         Calculate the emissivity at required frequency with respect to option MODL
!   using the NESDIS_LandEM and a bias correction algorithm, where the original NESDIS_LandEM with a
!   bias correction algorithm is referred to as value-added NESDIS_LandEM or AlandEM.
!
! program history log:
!
! input argument list:
!
!      frequency        -  frequency in GHz
!      theta            -  local zenith angle in degree
!      snow_depth       -  snow depth in mm
!      ts           -  surface temperature
!
! output argument list:
!
!     em_vector(1) and (2)  -  emissivity at two polarizations.
!                              set esv = esh here and will be updated
!       snow_type        -  snow type
!
! important internal variables:
!
!    esv_3w and esh_3w   -  initial emissivity discriminator at two polarizations
!                           at three AMSU window channels computed using NESDIS_LandEM
!    esv_3w[1] and esh_3w[1] : 31.4 GHz
!    esv_3w[2] and esh_3w[2] : 89   GHz
!    esv_3w[3] and esh_3w[3] : 150  GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!------------------------------------------------------------------------------------------------------------

  USE NESDIS_LandEM_Module, only: NESDIS_LandEM

  use type_kinds, only: fp_kind

  implicit none

  integer :: nw_ind
  parameter(nw_ind=3)
  real(fp_kind) theta, frequency, freq,snow_depth, ts, em_vector(2)
  real(fp_kind) esv,esh,esh0,esv0,theta0
  integer snow_type,ich
  real(fp_kind)   freq_3w(nw_ind),esh_3w(nw_ind),esv_3w(nw_ind)
  complex  eair
  data   freq_3w/31.4_fp_kind,89.0_fp_kind,150.0_fp_kind/

  eair = cmplx(one,-zero)

  snow_type = -999

  call NESDIS_LandEM(theta, frequency,0.0_fp_kind,0.0_fp_kind,ts,ts,snow_depth,esh0,esv0)

  theta0 = theta
  do ich = 1, nw_ind
     freq =freq_3w(ich)
     theta = theta0
     call NESDIS_LandEM(theta, freq,0.0_fp_kind,0.0_fp_kind,ts,ts,snow_depth,esh,esv)
     esv_3w(ich) = esv
     esh_3w(ich) = esh
  end do

  call ems_adjust(theta,frequency,snow_depth,ts,esv_3w,esh_3w,em_vector,snow_type)

  return

end subroutine AMSU_ALandEM_Snow



subroutine ems_adjust(theta,frequency,depth,ts,esv_3w,esh_3w,em_vector,snow_type)


!------------------------------------------------------------------------------------------------------------
!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
!
! abstract:
!         Calculate the emissivity discriminators and interpolate/extrapolate
!
!  emissivity at required frequency with respect to secenery MODL
!
! program history log:
!
! input argument list:
!
!      frequency   -  frequency in GHz
!      theta       -  local zenith angle in degree
!      depth       -  snow depth in mm
!      ts          -  surface temperature
!
! output argument list:
!
!     em_vector(1) and (2)  -  emissivity at two polarizations.
!                              set esv = esh here and will be updated
!     snow_type        -  snow type
!
! important internal variables:
!
!     dem_coe  -  fitting coefficients to compute discriminator correction value
!              dem_coe[1,*]   : 31.4 GHz
!              dem_coe[2,*]   : 89   GHz
!              dem_coe[3,*]   : 150  GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!------------------------------------------------------------------------------------------------------------

  use type_kinds, only: fp_kind,Double

  implicit none

  integer,parameter:: nch=10,nw_3=3

  integer,parameter:: ncoe=6

  real(fp_kind),parameter  :: earthrad = 6374._fp_kind, satheight = 833.4_fp_kind

  integer         :: snow_type,ich,k

  real(fp_kind)    :: theta,frequency,depth,ts,esv_3w(*),esh_3w(*)

  real(fp_kind)    :: discriminator(5),emmod(nw_3),dem(nw_3)

  real(fp_kind)    :: emissivity,em_vector(2)

  real(Double)  :: dem_coe(nw_3,0:ncoe-1),sinthetas,costhetas,deg2rad

  save  dem_coe

  data (dem_coe(1,k),k=0,ncoe-1)/ 2.306844e+000_Double, -7.287718e-003_Double, &

       -6.433248e-004_Double,  1.664216e-005_Double,  &

       4.766508e-007_Double, -1.754184e+000_Double/

  data (dem_coe(2,k),k=0,ncoe-1)/ 3.152527e+000_Double, -1.823670e-002_Double, &

       -9.535361e-004_Double,  3.675516e-005_Double,  &

       9.609477e-007_Double, -1.113725e+000_Double/

  data (dem_coe(3,k),k=0,ncoe-1)/ 3.492495e+000_Double, -2.184545e-002_Double,  &

       6.536696e-005_Double,  4.464352e-005_Double, &

       -6.305717e-008_Double, -1.221087e+000_Double/


!

  deg2rad = 3.14159_fp_kind*pi/180.0_fp_kind

  sinthetas = sin(theta*deg2rad)* earthrad/(earthrad + satheight)

  sinthetas = sinthetas*sinthetas

  costhetas = one - sinthetas

  do ich = 1, nw_3

     emmod(ich) = costhetas*esv_3w(ich) + sinthetas*esh_3w(ich)

  end do

  do ich=1,nw_3

     dem(ich) = dem_coe(ich,0) + dem_coe(ich,1)*ts + dem_coe(ich,2)*depth +   &

          dem_coe(ich,3)*ts*ts + dem_coe(ich,4)*depth*depth         +   &

          dem_coe(ich,5)*emmod(ich)

  end do

  emmod(1) = emmod(1) + dem(1)

  emmod(2) = emmod(2) + dem(2)

  emmod(3) = emmod(3) + dem(3)

! Match the format of the input variable

! Missing value at 23.8 GHz

  discriminator(1) = -999.9_fp_kind

  discriminator(2) = emmod(1)

! Missing value at 50.3 GHz

  discriminator(3) = -999.9_fp_kind

  discriminator(4) = emmod(2)

  discriminator(5) = emmod(3)

  call em_interpolate(frequency,discriminator,emissivity,snow_type)

  em_vector(1) = emissivity

  em_vector(2) = emissivity

  return

end subroutine ems_adjust


END MODULE NESDIS_AMSU_SnowEM_Module
