!--------------------------------------------------------------------------------
!M+
! NAME:
!       NESDIS_SSMI_Module
!
! PURPOSE:
!       Module containing the NESDIS microwave snow and sea ice  emissivity model
!
! REFERENCES:
!       Yan, B., F. Weng and K.Okamoto,2004: "A microwave snow emissivity model, 8th Specialist Meeting on
!
!       Microwave Radiometry and Remote Sension Applications,24-27 February, 2004, Rome, Italy.
!
! CATEGORY:
!       Surface : MW Surface Snow and Sea Ice Emissivity from SSMI
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE NESDIS_SSMI_Module
!
! MODULES:
!       Type_Kinds:          Module containing definitions for kinds of variable types.
!
!       NESDIS_LandEM_Module:Module containing the NESDIS microwave land emissivity model
!
! CONTAINS:
!
!   PUBLIC SUBPROGRAM:
!
!       NESDIS_SSMI_SSICEEM : Subroutine to call NESDIS_SSMI_SSICEEM_CORE
!
!   PRIVATE SUBPROGRAM:
!
!       NESDIS_SSMI_SSICEEM_CORE : Subroutine to calculate the microwave emissivity over snow and sea ice conditions
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
!       Written by:     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov (16-May-2005)
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

MODULE NESDIS_SSMI_Module





  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds

  USE NESDIS_LandEM_Module


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------


  PRIVATE



  PUBLIC  :: NESDIS_SSMI_SSICEEM



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
!       NESDIS_SSMI_SNOWEM
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
!       CRTM : Surface : MW SNOW/ICE EM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL NESDIS_SSMI_SNOWEM
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
!         Angle                    The angle values in degree.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      Degrees
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1, (I)
!
!
!         Tb                      BRIGHTNESS TEMPERATURES AT SEVEN SSMI WINDOW CHANNELS
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION   7*1 SCALAR
!
!                 tb[1] :  at 19.35 GHz  v-polarization
!                 tb[2] :  at 19.35 GHz  h-polarization
!                 tb[3] :  at 22.235 GHz v-polarization
!                 tb[4] :  at 37 GHz     v-polarization
!                 tb[5] :  at 37 GHz     h-polarization
!                 tb[6] :  at 85 GHz     v-polarization
!                 tb[7] :  at 85 GHz     h-polarization
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
!         Snow_status           = .T. (over snow conditions), otherwise other surface conditions
!                                  UNITS:      N/A
!                                  TYPE:       logical
!                                  DIMENSION:  Scalar
!
!         Ice_status           = .T. (over se aice conditions), otherwise other surface conditions
!                                  UNITS:      N/A
!                                  TYPE:       local
!                                  DIMENSION:  Scalar
!
!
!         Depth:                   The snow/sea ice  depth
!                                  UNITS:      mm
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar

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
! INTERNAL ARGUMENTS:
!
!     SSMI_Angle         : local zenith angle in degree
!
!
! CALLS:
!
!       NESDIS_SSMI_SSICEEM_CORE : Subroutine to calculate the microwave emissivity over snow and sea ice conditions
!
!
!
! PROGRAM HISTORY LOG:
!   2004-01-01  yan,b -  implement the algorithm for snow/ice emissivity
!   2004-02-01  yan,b -  modify the code for SSI
!   2005-05-28  okamoto - modify the code for CRTM
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

subroutine NESDIS_SSMI_SSICEEM(frequency,                                         & ! INPUT
                               Angle,                                             & ! INPUT
                               Ts,                                                & ! INPUT
                               tb,                                                & ! INPUT
                               Depth,                                             & ! INPUT
                               Snow_status,                                       & ! INPUT
                               Ice_status,                                        & ! INPUT
                               Emissivity_H,                                      & ! OUTPUT
                               Emissivity_V)                                        ! OUTPUT

  use type_kinds, only: ip_kind, fp_kind

  USE NESDIS_LandEM_Module

  implicit none

  integer(ip_kind), parameter:: nw=7,nwv=4,nwh=3

  real(fp_kind), parameter :: SSMI_Angle= 53.0_fp_kind

  REAL(fp_kind), PARAMETER ::  ev_default = 0.9_fp_kind

  REAL(fp_kind), PARAMETER ::  eh_default = 0.88_fp_kind

  real(fp_kind)     :: Depth,Angle,frequency,Ts,tb(nw),tv(nwv),th(nwh)

  real(fp_kind)     :: em_vector(2),esh1,esv1,esh2,esv2,desh,desv,dem

  real(fp_kind), intent(out) :: Emissivity_H, Emissivity_V

  logical           :: Snow_status,Ice_status


  Emissivity_H  =  eh_default  ;  Emissivity_V  =  ev_default

  tv(1) = tb(1);  tv(2) = tb(3);  tv(3) = tb(4);  tv(4) = tb(6)

  th(1) = tb(2); th(2) = tb(5);  th(3) = tb(7)

! Emissivity at SSMI_Angle

  call NESDIS_SSMI_SSICEEM_CORE(Snow_status,Ice_status,frequency,Ts,tv,th,em_vector)

! Get the emissivity angle dependence

  if (Depth .lt. one_tenth) Depth = one_tenth

  if (Depth .gt. 10.0_fp_kind) Depth = 10.0_fp_kind

  call NESDIS_LandEM(SSMI_Angle,frequency,0.0_fp_kind,0.0_fp_kind,Ts,Ts,Depth,esh1,esv1)

  call NESDIS_LandEM(Angle,frequency,0.0_fp_kind,0.0_fp_kind,Ts,Ts,Depth,esh2,esv2)

  desh = esh1 - esh2

  desv = esv1 - esv2

  dem = ( desh + desv ) * 0.5_fp_kind
! Emissivity at User's Angle

  Emissivity_H = em_vector(1) - dem;  Emissivity_V = em_vector(2)- dem

  if(Emissivity_H.gt.one) Emissivity_H = one

  if(Emissivity_H.lt.0.3_fp_kind) Emissivity_H = 0.3_fp_kind

  if(Emissivity_V.gt.one) Emissivity_V = one

  if(Emissivity_V.lt.0.3_fp_kind) Emissivity_V = 0.3_fp_kind


  return

end subroutine NESDIS_SSMI_SSICEEM


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PRIVATE MODULE ROUTINES ##                      ##
!##                                                                            ##
!################################################################################
!################################################################################

subroutine NESDIS_SSMI_SSICEEM_CORE(Snow_status,Ice_status,frequency,Ts,tv,th,em_vector)

!------------------------------------------------------------------------------------------------------------
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: iceem_amsua  noaa/nesdis SSM/I emissivity model over snow/ice
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2004-02-12
!
! abstract: Simulate microwave emissivity over ocean, sea ice and snow
!           using SSM/I  measurements and surface temperature
!
! program history log:
!
!      01/2004   : Implement the algorithm for snow/ice emissivity to F90 code by Banghua Yan
!      02/2004   : Modify the code for SSI subsystem                           by Banghua Yan
!      07/2004   : Modify the code for GSI subsystem                           by Kozo Okamoto
!      05/2005   : Modify the code for CRTM                                    by Banghua Yan
!
! input argument list:
!
!         Ice_status           = .T. (over se aice conditions) (ntype_index = 1)
!
!         Snow_status          = .T. (over snow conditions)    (ntype_index = 2)
!
!
!      frequency: frequency in GHz
!      Ts       :  scattering layer temperature (K)
!      tv[1] ~ tv[4]: brightness temperature at four SSM/I vertical polarization
!          tv[1] : 19.35  GHz
!          tv[2] : 22.235 GHz
!          tv[3] : 37     GHz
!          tv[4] : 85     GHz
!
!      th[1] ~ th[3]: brightness temperature at three SSM/I horizontal polarization
!          th[1] : 19.35  GHz
!          th[2] : 37     GHz
!          th[3] : 85     GHz
! output argument list:
!
!      em_vector     :  emissivity at two polarizations
!           em_vector[1] = eh
!           em_vector[2] = ev
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!------------------------------------------------------------------------------------------------------------

  use type_kinds, only: ip_kind, fp_kind

  implicit none

  integer(ip_kind),parameter :: ntype = 3, nv=4, nh=3,ncoev=5,ncoeh=4

  integer(ip_kind) :: ntype_index,ich,k,lp,nch

  integer(ip_kind), parameter :: SICEALG = 1
  integer(ip_kind), parameter :: SNOWALG = 2
  integer(ip_kind), parameter :: ALGDEFAULLT = 3

  real(fp_kind), parameter, dimension(nv) ::   &
  freq_v=(/19.35_fp_kind, 22.235_fp_kind, 37.0_fp_kind, 85.0_fp_kind/)

  real(fp_kind), parameter, dimension(nh) ::   &
  freq_h=(/19.35_fp_kind, 37.0_fp_kind, 85.0_fp_kind/)

  real(fp_kind) frequency,Ts,tv(*),th(*),em_vector(*)

  real(fp_kind) ev(nv),eh(nh),ev_22

  real(fp_kind) coe_v(ntype,nv,ncoev),coe_h(ntype,nh,ncoeh),pe , ev_cor,eh_cor

  logical Snow_status,Ice_status,data_invalid



! ice
  data (coe_v(1,1,k),k=1,5)/ -8.722723e-002_fp_kind,  1.064573e-002_fp_kind, &
       -5.333843e-003_fp_kind, -1.394910e-003_fp_kind,  4.007640e-004_fp_kind/
  data (coe_v(1,2,k),k=1,5)/-1.373924e-001_fp_kind,  6.580569e-003_fp_kind, &
       -9.991220e-004_fp_kind, -1.476022e-003_fp_kind,  4.131816e-004_fp_kind/
  data (coe_v(1,3,k),k=1,5)/ -2.329867e-001_fp_kind,  6.419856e-003_fp_kind, &
       -5.260987e-003_fp_kind, 3.342582e-003_fp_kind,  4.139272e-004_fp_kind/
  data (coe_v(1,4,k),k=1,5)/ -3.528638e-001_fp_kind,  6.342649e-003_fp_kind, &
       -5.002575e-003_fp_kind, -1.469298e-003_fp_kind,  5.529711e-003_fp_kind/
  data (coe_h(1,1,k),k=1,4)/ &
       -1.338736e-001_fp_kind,  6.229798e-003_fp_kind, -2.169491e-003_fp_kind,  &
       5.706367e-004_fp_kind/
  data (coe_h(1,2,k),k=1,4)/ &
       -2.747500e-001_fp_kind,  2.041477e-003_fp_kind,  2.581898e-003_fp_kind,  &
       5.924890e-004_fp_kind/
  data (coe_h(1,3,k),k=1,4)/ &
       -3.889575e-001_fp_kind,  2.188889e-003_fp_kind, -2.253243e-003_fp_kind,  &
       5.750499e-003_fp_kind/

!snow
  data (coe_v(2,1,k),k=1,5)/  1.109066e-001_fp_kind,  5.449409e-003_fp_kind,  &
       1.835799e-004_fp_kind, -1.765248e-003_fp_kind, -2.996101e-004_fp_kind/
  data (coe_v(2,2,k),k=1,5)/ 9.356505e-002_fp_kind,  1.320617e-003_fp_kind,  &
       4.449195e-003_fp_kind, -1.786960e-003_fp_kind, -3.479687e-004_fp_kind/
  data (coe_v(2,3,k),k=1,5)/ 6.387097e-002_fp_kind,  1.252447e-003_fp_kind,  &
       1.998846e-004_fp_kind, 2.680219e-003_fp_kind, -3.740141e-004_fp_kind/
  data (coe_v(2,4,k),k=1,5)/ 4.150689e-002_fp_kind,  1.420274e-003_fp_kind,  &
       1.223339e-004_fp_kind, -1.948946e-003_fp_kind,  4.248289e-003_fp_kind/

  data (coe_h(2,1,k),k=1,4)/ &
       8.503807e-002_fp_kind,  5.357374e-003_fp_kind, -1.361660e-003_fp_kind, &
       -3.319696e-004_fp_kind/
  data (coe_h(2,2,k),k=1,4)/ &
       4.200333e-002_fp_kind,  1.278894e-003_fp_kind,  2.963129e-003_fp_kind, &
       -4.087036e-004_fp_kind/
  data (coe_h(2,3,k),k=1,4)/ &
       2.082461e-002_fp_kind,  1.438480e-003_fp_kind, -1.723992e-003_fp_kind,  &
       4.194914e-003_fp_kind/

  save  coe_v,coe_h


! Initialization
!
  ntype_index = ALGDEFAULLT


! snow/sea ice classifications


  if(Ice_status)  ntype_index = SICEALG

  if(Snow_status) ntype_index = SNOWALG


! Initialize

  if(ntype_index == SICEALG) then
     em_vector(1) = 0.4_fp_kind
     em_vector(2) = 0.6_fp_kind
  else if(ntype_index == SNOWALG) then
     em_vector(1) = 0.7_fp_kind
     em_vector(2) = 0.8_fp_kind
  else if(ntype_index == ALGDEFAULLT) then
     em_vector(1) = 0.75_fp_kind
     em_vector(2) = 0.8_fp_kind
  end if

! Data status check
  data_invalid = .False.
  if ( (Ts <= 140.0_fp_kind) .or. (Ts >= 330.0_fp_kind) ) data_invalid = .True.
  do ich = 1, nv
     if ( (tv(ich) .le. 50.0_fp_kind) .or. (tv(ich) .ge. 330.0_fp_kind) )  then
        data_invalid = .True.
        exit
     end if
  end do
  do ich = 1, nh
     if ( (th(ich) <= 50.0_fp_kind) .or. (th(ich) >= 330.0_fp_kind) )  then
        data_invalid = .True.
        exit
     end if
  end do
  if (data_invalid) RETURN


!*** Get intial emissivity for each frequency

! v components
  do ich=1,nv
     ev(ich) =  coe_v(ntype_index,ich,1) + coe_v(ntype_index,ich,2)*tv(1)  &
          + coe_v(ntype_index,ich,3)*tv(2) + coe_v(ntype_index,ich,4)*tv(3)  &
          + coe_v(ntype_index,ich,5)*tv(4)
  end do

! h components
  do ich=1,nh
     eh(ich) =  coe_h(ntype_index,ich,1)
     do lp =2,4
        eh(ich) =  eh(ich) + coe_h(ntype_index,ich,lp)*th(lp-1)
     end do
  end do


! *** Emissivity bias value

  if (ntype_index == 1) then       ! seaice
     pe= 0.011_fp_kind + 3.786080e-003_fp_kind*(tv(1) - th(1)) -  &
          7.217788e-005_fp_kind*(tv(3) - th(2)) +  &
          1.018791e-004_fp_kind*(tv(4) - th(3))
  else            ! snow
     pe=     -0.002_fp_kind + 4.470142e-003_fp_kind*(tv(1) - th(1)) -  &
          1.991876e-004_fp_kind*(tv(3) - th(2)) -  &
          1.704354e-005_fp_kind*(tv(4) - th(3))
  end if

  ev_cor = one - pe*(Ts-tv(1))/(tv(1)-th(1))
  if (ev_cor >  one)         ev_cor = one
  if (ev_cor <= 0.2_fp_kind) ev_cor = 0.2_fp_kind
  eh_cor = ev_cor - pe
  ev_cor = ev(1) - ev_cor
  eh_cor = eh(1) - eh_cor

!*** Calculate emissivity
  do ich=1, nv
     ev(ich) = ev(ich) - ev_cor
     if(ich <= 3) eh(ich) = eh(ich) - eh_cor
  end do


!*** Quality control at 22.235 GHz
  ev_22 = ev(1) + (ev(3)-ev(1))*(22.235_fp_kind-19.35_fp_kind)/(37.0_fp_kind-19.35_fp_kind)
!/\ type
  if( (ev(2) .gt. ev(1)) .and. (ev(2) .gt. ev(3)) ) ev(2) = ev_22
!\/ type
  if( (ev(2) .lt. ev(1)) .and. (ev(2) .lt. ev(3)) ) ev(2) = ev_22


!*** Interpolate emissivity at a certain frequency

! v-component
  nch = 4
  do ich=1,nv
     if(frequency <= freq_v(ich)) then
        nch = ich
        exit
     end if
  end do

  if (nch == 1) then
     em_vector(2) = ev(1)
  else
     if (nch == 4) then
        em_vector(2) = ev(4)
     else
        em_vector(2) = ev(nch-1) + (ev(nch) - ev(nch-1))* &
             (frequency - freq_v(nch-1))/(freq_v(nch) - freq_v(nch-1))
     end if
  end if

! h-component
  nch = 3
  do ich=1,nh
     if(frequency <= freq_h(ich)) then
        nch = ich
        exit
     end if
  end do
  if (nch == 1) then
     em_vector(1) = eh(1)
  else
     if (nch == 3) then
        em_vector(1) = eh(3)
     else
        em_vector(1) = eh(nch-1) + (eh(nch) - eh(nch-1))* &
             (frequency - freq_h(nch-1))/(freq_h(nch) - freq_h(nch-1))
     end if

  end if

end subroutine NESDIS_SSMI_SSICEEM_CORE


END MODULE NESDIS_SSMI_Module
