subroutine adm_ehv2pem( zlsat0,polar0,pems0, ehorz0,evert0)
!$$$   module documentation block
!                .      .    .                                       .
! module: adm_ehv2pem  calculate Adjoint of combining V and H.pol emissivity
!   prgmmr: okamoto          org: np23                date: 2004-08-06
!
! abstract:  calculate Adjoint of combine vertical and horizontal
!            polarized emissivity adjoint.
!            effective for cross-track scan type sensor such as AMSU
!            In contrast, for ssm/i, pems(V)=evert0, pems(H)=ehorz0,
!            as they are
!
! program history log:
!   2004-08-06  okamoto
!
!   input argument list:
!     zlsat0 - satellite look angle in radians
!     polar  - channel polarization (0=vertical, 1=horizontal, or
!                                      0 to 1=mix of V and H)
!     pems0  - surface emissivity adjoint at obs location
!
!   output argument list:
!     ehorz0 - partial derivative of combined emissivity with respect to emissivity at horizontal polarization
!     evert0 - partial derivative of combined emissivity with respect to emissivity at vertical polarization
!
! attributes:
!   language: cft77
!   machine:  IBM sp
!
!$$$
!  use kinds, only: r_kind,i_kind
!  use constants, only: one,zero
  implicit none

   real(r_kind),intent(in) :: zlsat0,polar0, pems0
   real(r_kind),intent(out):: ehorz0,evert0

   real(r_kind):: pcl2,psl2,term1,term2

    pcl2=cos(zlsat0)**2
    psl2=sin(zlsat0)**2

    term1 = (one-polar0)*pems0
    term2 = polar0*pems0

    evert0 = term2*psl2 + term1*pcl2
    ehorz0 = term2*pcl2 + term1*psl2

   return
 end subroutine adm_ehv2pem
