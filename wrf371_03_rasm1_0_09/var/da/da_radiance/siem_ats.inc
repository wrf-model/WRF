subroutine siem_ats(theta,frequency,tba,ts,seaice_type,em_vector)
!
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
!   2004-10-28  treadon - correct out of bound problems for array coe
!
! input argument list:
!
!      frequency        -  frequency in GHz
!      theta            -  local zenith angle in radian
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
!       seaice_type        -     to be determined
!
! important internal variables:
!
!   coe23   - fitting coefficients to estimate discriminator at 23.8 GHz
!   coe31   - fitting coefficients to estimate discriminator at 31.4 GHz
!   coe50   - fitting coefficients to estimate discriminator at 50.3 GHz
!   coe89   - fitting coefficients to estimate discriminator at 89   GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

!  use kinds, only: r_kind,i_kind
  implicit none
  
  integer(i_kind),parameter:: nch =10,nwch = 5,ncoe = 4
  real(r_kind)    :: tba(*),theta
  real(r_kind)    :: em_vector(*),emissivity,ts,frequency,discriminator(nwch)
  integer(i_kind) :: seaice_type,i,k,ich,nvalid_ch
  real(r_kind)  :: coe23(0:ncoe),coe31(0:ncoe),coe50(0:ncoe),coe89(0:ncoe),coe150(0:ncoe)
  real(r_kind)  :: coe(nch*(ncoe+1))
  
  Equivalence (coe(1),coe23)
  Equivalence (coe(11),coe31)
  Equivalence (coe(21),coe50)
  Equivalence (coe(31),coe89)
  Equivalence (coe(41),coe150)

! Fitting Coefficients Using Tb1, Tb2, Tb4 and Ts
  data coe23/ 9.815214e-001_r_kind,  3.783815e-003_r_kind,  &
       6.391155e-004_r_kind, -9.106375e-005_r_kind, -4.263206e-003_r_kind/
  data coe31/ 9.047181e-001_r_kind, -2.782826e-004_r_kind,  &
       4.664207e-003_r_kind, -3.121744e-005_r_kind, -3.976189e-003_r_kind/
  data coe50/ 1.163853e+000_r_kind, -1.419205e-003_r_kind,  &
       5.505238e-003_r_kind,  1.506867e-003_r_kind, -6.157735e-003_r_kind/
  data coe89/  1.020753e+000_r_kind, -8.666064e-004_r_kind,  &
       9.624331e-004_r_kind,  4.878773e-003_r_kind, -5.055044e-003_r_kind/
  data coe150/ 1.438246e+000_r_kind,  5.667756e-004_r_kind, &
       -2.621972e-003_r_kind,  5.928146e-003_r_kind, -5.856687e-003_r_kind/
  save coe23,coe31,coe50,coe89,coe150
  

! Calculate emissivity discriminators at five AMSU window channels
  
  do ich = 1, nwch
     discriminator(ich) = coe(1+(ich-1)*10)
     discriminator(ich) = discriminator(ich) + coe((ich-1)*10 + 2)*tba(1)  &
          + coe((ich-1)*10 + 3)*tba(2)  &
          + coe((ich-1)*10 + 4)*tba(4)  &
          + coe( (ich-1)*10 + 5 )*ts
  end do
  
  call siem_interpolate(frequency,discriminator,emissivity,seaice_type)
  
  em_vector(1) = emissivity
  em_vector(2) = emissivity
  
  
end subroutine siem_ats
