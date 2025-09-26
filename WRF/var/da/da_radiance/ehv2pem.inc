subroutine ehv2pem( ehorz0,evert0,zlsat0,polar0, pems0)
!$$$   module documentation block
!                .      .    .                                       .
! module: ehv2pem  combine V and H.pol emissivity
!   prgmmr: okamoto          org: np23                date: 2004-08-06
!
! abstract:  combine vertical and horizontal polarized emissivity.
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
!     ehorz0 - emissivity at horizontal polarization
!     evert0 - emissivity at vertical polarization
!
!   output argument list:
!     pems0  - surface emissivity at obs location
!
! attributes:
!   language: cft77
!   machine:  IBM sp
!
!$$$
    implicit none
    real(r_kind):: ehorz0,evert0,zlsat0,polar0, pems0
    real(r_kind):: pcl2,psl2,term1,term2
    pcl2=cos(zlsat0)**2
    psl2=sin(zlsat0)**2
    term1 = evert0*pcl2 + ehorz0*psl2
    term2 = evert0*psl2 + ehorz0*pcl2
    pems0 = (one-polar0)*term1 + polar0*term2
    return
  end subroutine ehv2pem
