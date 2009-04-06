!WRF:MEDIATION_LAYER:FIRE_MODEL

!*** Jan Mandel August 2007 - February 2008
!*** email: jmandel@ucar.edu or Jan.Mandel@gmail.com or Jan.Mandel@cudenver.edu

! Routines dealing with the atmosphere

module module_fr_sfire_atm

use module_model_constants, only: cp,xlv
use module_fr_sfire_util

contains

SUBROUTINE fire_tendency( &
    ids,ide, kds,kde, jds,jde,   & ! dimensions
    ims,ime, kms,kme, jms,jme,   &
    its,ite, kts,kte, jts,jte,   &
    grnhfx,grnqfx,canhfx,canqfx, & ! heat fluxes summed up to  atm grid 
    alfg,alfc,z1can,             & ! coeffients, properties, geometry 
    zs,z_at_w,dz8w,mu,rho,       &
    rthfrten,rqvfrten)             ! theta and Qv tendencies 

! This routine is atmospheric physics 
! it does NOT go into module_fr_sfire_phys because it is not fire physics

! taken from the code by Ned Patton, only order of arguments change to the convention here
! --- this routine takes fire generated heat and moisture fluxes and
!     calculates their influence on the theta and water vapor 
! --- note that these tendencies are valid at the Arakawa-A location

   IMPLICIT NONE

! --- incoming variables

   INTEGER , INTENT(in) :: ids,ide, kds,kde, jds,jde, &
                           ims,ime, kms,kme, jms,jme, &
                           its,ite, kts,kte, jts,jte

   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme ) :: grnhfx,grnqfx  ! W/m^2
   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme ) :: canhfx,canqfx  ! W/m^2
   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme ) :: zs  ! topography (m abv sealvl)
   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme ) :: mu  ! dry air mass (Pa)

   REAL, INTENT(in), DIMENSION( ims:ime,kms:kme,jms:jme ) :: z_at_w ! m abv sealvl
   REAL, INTENT(in), DIMENSION( ims:ime,kms:kme,jms:jme ) :: dz8w   ! dz across w-lvl
   REAL, INTENT(in), DIMENSION( ims:ime,kms:kme,jms:jme ) :: rho    ! density

   REAL, INTENT(in) :: alfg ! extinction depth ground fire heat (m)
   REAL, INTENT(in) :: alfc ! extinction depth crown  fire heat (m)
   REAL, INTENT(in) :: z1can    ! height of crown fire heat release (m)

! --- outgoing variables

   REAL, INTENT(out), DIMENSION( ims:ime,kms:kme,jms:jme ) ::   &
       rthfrten, & ! theta tendency from fire (in mass units)
       rqvfrten    ! Qv tendency from fire (in mass units)

END SUBROUTINE fire_tendency

end module module_fr_sfire_atm
