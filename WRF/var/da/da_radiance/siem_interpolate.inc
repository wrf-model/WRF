subroutine  siem_interpolate(frequency,discriminator,emissivity,seaice_type)
!
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
!$$$

!  use kinds, only: r_kind,i_kind
  implicit none
  
  integer(i_kind),parameter:: ncand = 16,nch =5
  integer(i_kind):: ich,ichmin,ichmax,i,j,k,s,seaice_type
  real(r_kind)   :: dem,demmin0
  real(r_kind)   :: em(ncand,nch)
  real(r_kind)   :: frequency,freq(nch),emissivity,discriminator(*)
  real(r_kind)   :: cor_factor,adjust_check,kratio, bconst
  data  freq/23.8_r_kind, 31.4_r_kind, 50.3_r_kind,89.0_r_kind, 150.0_r_kind/
  
! Estimate sea ice emissivity at a required frequency
  seaice_type = -999   ! temporal assumption
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

