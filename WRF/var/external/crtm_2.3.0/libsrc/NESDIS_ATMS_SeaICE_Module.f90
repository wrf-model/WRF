!--------------------------------------------------------------------------------
!M+
! NAME:
!       NESDIS_ATMS_SeaICE_Module
!
! PURPOSE:
!       Module containing the seaice-typing algorithm. A general interface is used to call the 
!       seaice-typing algorithm in terms of the input arguments. This Module is used together with 
!       NESDIS_ATMS_SeaICE_LIB Module to implement the library-based seaIce emissivity  model.
!
! REFERENCES:
!       Yan, B., F. Weng and K.Okamoto,2004: "A microwave snow emissivity model, 8th Specialist Meeting on
!
!       Microwave Radiometry and Remote Sension Applications,24-27 February, 2004, Rome, Italy.
!
! CATEGORY:
!       Surface : MW Surface SeaICE Emissivity of ATMS
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!
!       USE NESDIS_ATMS_SeaICE_Module
!
! MODULES:
!       Type_Kinds:               Module containing definitions for kinds of variable types
!
!       NESDIS_LandEM_Module:     Module containing the microwave land emissivity model
!
!       NESDIS_ATMS_SeaICE_LIB:   Module containing the predefined microwave seaice emissivity spectra
!
! CONTAINS:
!
! PUBLIC SUNPROGRAMS:
!
!       NESDIS_ATMS_SEAICE:       Subroutine to calculate the microwave seaice emissivity from ATMS
!
!
! PRIVATE SUBPROGRAMS:
!       These subroutines are used to determine the snow types from the brightness temperatures(TB) 
!       of five ATMS window channels( 23.8 GHz, 31.4 GHz, 50.3 GHz, 88.2 GHz, 165.5 GHz) and/or
!       surface temperature plus snow depth. The five channels are further divided into two 
!       groups: Group-1 ( 23.8 GHz, 31.4 GHz, 50.3 GHz, 88.2 GHz) and Group-2 (88.2 GHz, 165.5GHz), 
!       corresponding to the window channels of AMSU-A and AMSU-B, respectively.
!       Different combinations of available ATMS window-channel and surface observations result
!       in differenet snow-typing algotrithms:   

!       ATMS_SEAICE_ByTBTs      : by the TBs  of all the five ATMS channels and surface temperature (regression-based)
!       ATMS_SEAICE_ByTBTs_D    : by the TBs  of all the five ATMS channels and surface temperature (diagnosis-based)
!       siem_interpolate        : Subroutine to perform frequency interpolation of snow emissivity
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
!       Written by:     Ming Chen, IMSG Inc., Banghua.Yan@noaa.gov (04-28-2012)
!
!
!       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
!
!  Copyright (C) 2012 Fuzhong Weng and Ming Chen
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

MODULE NESDIS_ATMS_SeaICE_Module

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds, ONLY: fp
  USE NESDIS_LandEM_Module
  USE NESDIS_ATMS_SeaICE_LIB
  
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: NESDIS_ATMS_SeaICE

  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: NESDIS_ATMS_SeaICE_Module.f90 21141 2012-09-14 17:40:43Z paul.vandelst@noaa.gov $'


CONTAINS


   SUBROUTINE  NESDIS_ATMS_SeaICE(Satellite_Angle,                                           & ! INPUT
                              User_Angle,                                                    & ! INPUT
                              frequency,                                                     & ! INPUT
                              Ts,                                                            & ! INPUT
                              Tbs,                                                           & ! INPUT
                              Emissivity_H,                                                  & ! OUTPUT
                              Emissivity_V)                                                    ! OUTPUT


      INTEGER, PARAMETER::  nwch = 5
      REAL(fp):: Satellite_Angle,User_Angle,Satellite_theta,frequency,Ts
      REAL(fp):: em_vector(2),esh1,esv1,esh2,esv2,desh,desv,dem
      REAL(fp):: tbs(nwch)
      REAL(fp), INTENT(out) :: Emissivity_H, Emissivity_V


    ! Initialization
      em_vector(1) = 0.82_fp
      em_vector(2) = 0.85_fp
      Satellite_theta = User_Angle*pi/180.0_fp

    ! Check available data
      IF ((Ts <= 150.0_fp) .OR. (Ts >= 280.0_fp) ) Ts = 260.0

    ! Emissivity at the local zenith angle of satellite measurements
      CALL  ATMS_SeaICE_ByTbTs_D(frequency,tbs,Ts,em_vector)
    ! Get the emissivity angle dependence
      CALL NESDIS_LandEM(Satellite_Angle,Frequency,0.0_fp,0.0_fp,Ts,Ts,0.0_fp,9,13,2.0_fp,esh1,esv1)
      CALL NESDIS_LandEM(User_Angle,Frequency,0.0_fp,0.0_fp,Ts,Ts,0.0_fp,9,13,2.0_fp,esh2,esv2)
      desh = esh1 - esh2
      desv = esv1 - esv2
      dem = ( desh + desv ) * 0.5_fp

    ! Emissivity at User's Angle
      Emissivity_H = em_vector(1) - dem;  Emissivity_V = em_vector(2)- dem

      IF(Emissivity_H > ONE)  Emissivity_H = ONE
      IF(Emissivity_V > ONE)  Emissivity_V = ONE

      IF(Emissivity_H < 0.3_fp) Emissivity_H = 0.3_fp
      IF(Emissivity_V < 0.3_fp) Emissivity_V = 0.3_fp


   END SUBROUTINE NESDIS_ATMS_SeaICE



   SUBROUTINE  ATMS_SeaICE_ByTbTs_D(frequency,tb,ts,em_vector)
   
   !----------------------------------------------------------------------------------------------------------!
   !$$$  subprogram documentation block
   !
   ! subprogram: Calculate emissivity by diagnosis-based algorithm
   !
   !
   ! abstract:
   !   Diagnose the snow type, and use the emissivity spectrum of the snow type as the first-guess to diagnose 
   !   the magnitude of necessary adjustment with respect to window-channel TBs and surface temperature Ts. Perfrom
   !   necessary interpolation/extrapolation a required frequency and user angle.
   !
   !
   ! input argument list:
   !
   !     frequency        -  frequency in GHz
   !     theta            -  local zenith angle (currently, not used here)
   !     tb[1] ~ tb[5]    -  brightness temperature at five ATMS window channels:
   !                              tb[1] : 23.8 GHz
   !                              tb[2] : 31.4 GHz
   !                              tb[3] : 50.3 GHz
   !                              tb[4] : 88.2 GHz
   !                              tb[5] : 165.5 GHz
   !
   ! output argument list:
   !
   !      em_vector[1] and [2]  -  emissivity at two polarizations.
   !                              set esv = esh here and will be updated
   !      snow_type        -  snow type
   !
   !
   ! remarks:
   !
   ! program history log:
   !            Ming Chen, IMSG at NOAA/NESDIS/STAR                 date: 2012-04-28
   !
   !
   !----------------------------------------------------------------------------------------------------------!

    INTEGER , PARAMETER  :: ntype = N_SEAICE_TYPES, nch = N_FREQ_ATMS, nwch = 5
    REAL(fp), PARAMETER  :: earthrad = 6374._fp, satheight = 833.4_fp
    INTEGER  :: freq_idx,sice_type
    REAL(fp) :: frequency
    REAL(fp) :: em(nch,ntype), em_vector(:)
    REAL(fp) :: tb(:),freq(nch)
    REAL(fp) :: ts, emissivity
    REAL(fp) :: ediff(ntype), X(nwch),Y(nwch),emw(nwch)
    REAL(fp) :: XX,XY,del,dem,dem2,delta,deltb
    INTEGER  :: minlc(1)
    INTEGER  :: windex(nwch)=(/1,2,3,11,12/)             ! window channel index of the library spectrum
     
    ! Sixteen candidate snow emissivity spectra
    em = SEAICE_EMISS_ATMS_LIB
    freq = FREQUENCY_ATMS

    minlc =minloc(ABS(freq-frequency)); freq_idx=minlc(1)
 
    !*** IDENTIFY SEAICE TYPE
    sice_type = 4 !default
    ediff=abs(Tb(1)/em(1,:)-Tb(2)/em(2,:))+abs(Tb(2)/em(2,:)-Tb(4)/em(11,:)) 
    minlc = minloc(ediff) ; sice_type=minlc(1)

    !*** adjustment from the library values    
    emw=em(windex,sice_type)
    X=1.0/emw ; Y=LOG(Tb/(Ts*emw))
    IF(frequency >100_fp) THEN
      XX=DOT_PRODUCT(X((/1,2,4,5/)),X((/1,2,4,5/)))
      XY=DOT_PRODUCT(X((/1,2,4,5/)),Y((/1,2,4,5/)))
      del=XY/XX
       deltb=Tb(3)-Tb(5)
    ELSE
      XX=DOT_PRODUCT(X((/1,2,4/)),X((/1,2,4/))) 
      XY=DOT_PRODUCT(X((/1,2,4/)),Y((/1,2,4/)))
      del=XY/XX
      deltb=Tb(3)-Tb(4)
    ENDIF
    dem = 0.0_fp; delta = 0.0_fp
    IF(frequency <= 30.0_fp ) dem = .9*del
    IF(frequency > 30._fp .AND. frequency <= 40.0_fp ) dem = 0.9*del
    IF(frequency > 40._fp .AND. frequency <= 50.0_fp ) dem = 0.9*del
    IF(frequency > 50_fp) THEN
       IF(del .LE. 0.0_fp .AND. ABS(deltb) .LT. 30.0_fp) delta=0.5+deltb/50.0
       IF(del .LE. 0.0_fp .AND. ABS(deltb) .GE. 30.0_fp) delta=1.0+deltb/50.0
       IF(del .GT. 0.0_fp .AND. ABS(deltb) .LT. 35.0_fp) delta=1.05-deltb/70.0
       IF(del .GT. 0.0_fp .AND. ABS(deltb) .GE. 35.0_fp) delta=.85-deltb/70.0
       IF(frequency <= 100.0_fp) dem  = 0.9*del+(delta*del-del)*(frequency-50.0)/(100.0-50.0)
       IF(frequency >  100.0_fp) dem  = 0.65*delta*del
    ENDIF
    dem2=dem
   
    emissivity = em(freq_idx,sice_type)+(dem+dem2)/2.0
    IF (emissivity >  1.0_fp )emissivity = 1.0_fp
    IF (emissivity <= 0.3_fp )emissivity = 0.3_fp

    em_vector(1) = emissivity
    em_vector(2) = emissivity

    RETURN

   END SUBROUTINE ATMS_SeaICE_ByTbTs_D


   SUBROUTINE  ATMS_SeaICE_ByTbTs(frequency,tb,ts,em_vector)
   
   !----------------------------------------------------------------------------------------------------------!
   !$$$  subprogram documentation block
   !
   ! subprogram: Calculate emissivity by diagnosis-based algorithm
   !
   !
   ! abstract:
   !   Diagnose the snow type, and use the emissivity spectrum of the snow type as the first-guess to diagnose 
   !   the magnitude of necessary adjustment with respect to window-channel TBs and surface temperature Ts. Perfrom
   !   necessary interpolation/extrapolation a required frequency and user angle.
   !
   !
   ! input argument list:
   !
   !     frequency        -  frequency in GHz
   !     theta            -  local zenith angle (currently, not used here)
   !     tb[1] ~ tb[5]    -  brightness temperature at five ATMS window channels:
   !                              tb[1] : 23.8 GHz
   !                              tb[2] : 31.4 GHz
   !                              tb[3] : 50.3 GHz
   !                              tb[4] : 88.2 GHz
   !                              tb[5] : 165.5 GHz
   !
   ! output argument list:
   !
   !      em_vector[1] and [2]  -  emissivity at two polarizations.
   !                              set esv = esh here and will be updated
   !      snow_type        -  snow type
   !
   !
   ! remarks:
   !
   ! program history log:
   !            Ming Chen, IMSG at NOAA/NESDIS/STAR                 date: 2012-04-28
   !
   !
   !----------------------------------------------------------------------------------------------------------!


    INTEGER, PARAMETER :: nch =10, nwch = 5, ncoe = 4
    REAL(fp) :: tb(:)
    REAL(fp) :: em_vector(*),emissivity,ts,frequency,discriminator(nwch)
    INTEGER  :: ich
    REAL(fp),SAVE :: coe(100)
    REAL(fp) :: X(nwch),Y(nwch)
    REAL(fp) :: XX,XY,del,deltb


    ! Fitting Coefficients Using Tb1, Tb2, Tb4 and Ts
    coe(1:5)  =(/ 9.815214e-001_fp,  3.783815e-003_fp, 6.391155e-004_fp, -9.106375e-005_fp, -4.263206e-003_fp/)
    coe(21:25)=(/ 9.047181e-001_fp, -2.782826e-004_fp, 4.664207e-003_fp, -3.121744e-005_fp, -3.976189e-003_fp/)
    coe(41:45)=(/ 1.163853e+000_fp, -1.419205e-003_fp, 5.505238e-003_fp,  1.506867e-003_fp, -6.157735e-003_fp/)
    coe(61:65)=(/ 1.020753e+000_fp, -8.666064e-004_fp, 9.624331e-004_fp,  4.878773e-003_fp, -5.055044e-003_fp/)
    coe(81:85)=(/ 1.438246e+000_fp,  5.667756e-004_fp,-2.621972e-003_fp,  5.928146e-003_fp, -5.856687e-003_fp/)


    ! Calculate emissivity discriminators at five ATMS window channels

    DO ich = 1, nwch
       discriminator(ich) = coe(1+(ich-1)*20)
       discriminator(ich) = discriminator(ich) + coe((ich-1)*20 + 2)*tb(1)  &
          + coe((ich-1)*20 + 3)*tb(2)  &
          + coe((ich-1)*20 + 4)*tb(4)  &
          + coe( (ich-1)*20 + 5 )*ts
    END DO
 
    X=1.0/discriminator ; Y=LOG(Tb/(Ts*discriminator))
    IF(frequency >100_fp) THEN
       XX=DOT_PRODUCT(X((/1,2,4,5/)),X((/1,2,4,5/)))
       XY=DOT_PRODUCT(X((/1,2,4,5/)),Y((/1,2,4,5/)))
       del=XY/XX
       deltb=Tb(3)-Tb(5)
    ELSE
       XX=DOT_PRODUCT(X((/1,2,4/)),X((/1,2,4/))) 
       XY=DOT_PRODUCT(X((/1,2,4/)),Y((/1,2,4/)))
       del=XY/XX
       deltb=Tb(3)-Tb(4)
    ENDIF
  
    discriminator= discriminator+del
    emissivity = 0.32_fp
    call siem_interpolate(frequency,discriminator,emissivity)

    em_vector(1) = emissivity
    em_vector(2) = emissivity


   END SUBROUTINE ATMS_SeaICE_ByTbTs


   SUBROUTINE siem_interpolate(frequency,discriminator,emissivity)
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
   !      discriminators  - emissivity discriminators at five ATMS window channels
   !            discriminator[1]   :  emissivity discriminator at 23.8 GHz
   !            discriminator[2]   :  emissivity discriminator at 31.4 GHz
   !            discriminator[3]   :  emissivity discriminator at 50.3 GHz
   !            discriminator[4]   :  emissivity discriminator at 89   GHz
   !            discriminator[5]   :  emissivity discriminator at 150  GHz
   !
   !       Note: discriminator(1) and discriminator(3) are missing value in
   !            'ATMS & Ts', and 'MODL' options., which are defined to as -999.9,
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

    INTEGER, PARAMETER :: ncand = 16,nch =5
    INTEGER :: i
    REAL(fp):: frequency,emissivity,discriminator(*)
    REAL(fp):: freq(nch) = (/23.8_fp, 31.4_fp, 50.3_fp,89.0_fp, 150.0_fp/)

    !Estimate sea ice emissivity at a required frequency
    DO i = 2, nch
       IF(frequency < freq(1))   exit
       IF(frequency >= freq(nch)) exit
       IF(frequency < freq(i)) THEN
          emissivity = discriminator(i-1) + (discriminator(i)-discriminator(i-1))* &
             (frequency - freq(i-1))/(freq(i) - freq(i-1))
          exit
       ENDIF
    END DO

    IF(frequency < freq(1)) emissivity = discriminator(1)

    ! Assume emissivity = constant at frequencies >= 150 GHz
    IF(frequency >= freq(nch)) emissivity = discriminator(nch)

   END SUBROUTINE siem_interpolate

END MODULE NESDIS_ATMS_SeaICE_Module
