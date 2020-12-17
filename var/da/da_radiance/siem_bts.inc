subroutine siem_bts(theta,frequency,tbb,ts,seaice_type,em_vector)
!
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
!      theta            -  local zenith angle (not used here)
!      ts               -  surface temperature in degree
!      tbb[1] ~ tbb[2]  -  brightness temperature at five AMSU-B window channels:
!                              tbb[1] : 89  GHz
!                              tbb[2] : 150 GHz
!
! output argument list:
!
!   em_vector(1) and (2)  -  emissivity at two polarizations.
!                              set esv = esh here and will be updated
!       seaice_type        -  ?
!
! important internal variables:
!
!   coe31   - fitting coefficients to estimate discriminator at 31.4 GHz
!   coe89   - fitting coefficients to estimate discriminator at 89   GHz
!   coe150  - fitting coefficients to estimate discriminator at 150  GHz
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
  
  integer(i_kind),parameter:: nch =10,nwch = 5,ncoe = 6
  real(r_kind)    :: tbb(*),theta
  real(r_kind)    :: em_vector(*),emissivity,ts,frequency,discriminator(nwch)
  integer(i_kind) :: seaice_type,i,k,ich,nvalid_ch
  real(r_kind)  :: coe23(0:ncoe),coe31(0:ncoe),coe50(0:ncoe),coe89(0:ncoe-3),coe150(0:ncoe-3)
  real(r_kind)  :: coe(nch*(ncoe+1))
  
  Equivalence (coe(1),coe23)
  Equivalence (coe(11),coe31)
  Equivalence (coe(21),coe50)
  Equivalence (coe(31),coe89)
  Equivalence (coe(41),coe150)

! Fitting Coefficients at 31.4 GHz
  data coe23/ 2.239429e+000_r_kind, -2.153967e-002_r_kind,  &
       5.785736e-005_r_kind,  1.366728e-002_r_kind,    &
       -3.749251e-005_r_kind, -5.128486e-002_r_kind, -2.184161e-003_r_kind/
  data coe31/ 1.768085e+000_r_kind, -1.643430e-002_r_kind,  &
       4.850989e-005_r_kind,  1.288753e-002_r_kind,   &
       -3.628051e-005_r_kind, -4.751277e-002_r_kind, -2.580649e-003_r_kind/
  data coe50/ 8.910227e-001_r_kind,  6.170706e-003_r_kind, &
       -3.772921e-006_r_kind, -4.146567e-004_r_kind,   &
       -2.208121e-006_r_kind, -3.163193e-002_r_kind, -3.863217e-003_r_kind/
  save coe23,coe31,coe50,coe89,coe150

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
  discriminator(4) = 9.278287e-001_r_kind +  5.549908e-003_r_kind*tbb(1) &
       - 5.728596e-004_r_kind*tbb(2) -  4.701641e-003_r_kind*ts
  discriminator(5) = 1.520531e+000_r_kind + 1.119648e-003_r_kind*tbb(1) &
       +  4.518667e-003_r_kind*tbb(2) - 7.744607e-003_r_kind*ts
  
  call siem_interpolate(frequency,discriminator,emissivity,seaice_type)
  
  em_vector(1) = emissivity
  em_vector(2) = emissivity
  
end subroutine siem_bts
