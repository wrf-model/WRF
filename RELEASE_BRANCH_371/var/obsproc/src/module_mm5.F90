MODULE module_mm5

!------------------------------------------------------------------------------!
! MM5 parameters and reference height/pressure calculation functions
!------------------------------------------------------------------------------!
       INTEGER                   :: maxnes
       INTEGER                   :: idd
       INTEGER                   :: iproj
       INTEGER                   :: ixc,   jxc

       REAL                      :: ptop, ps0, ts0, tlp, pis0, tis0, &
                                    base_pres, base_temp, base_lapse,&
                                    base_strat_temp, base_tropo_pres
       REAL                      :: htop, h_tropo

       REAL                      :: phic, xlonc, truelat1, truelat2
       REAL                      :: xn, pole, psi1,  c2, xcntr, ycntr

       INTEGER, DIMENSION (10)   :: nestix, nestjx, nesti, nestj, numc
       REAL,    DIMENSION (10)   :: dis, xim11, xjm11

!------------------------------------------------------------------------------!

   logical :: domain_check_h = .TRUE.
   logical :: user_defined_area
   real    :: x_left,   x_right, &
              y_bottom, y_top

!------------------------------------------------------------------------------!
CONTAINS

FUNCTION ref_height (pres) RESULT (height)

!------------------------------------------------------------------------------!
!  PURPOSE: To calculate the height from the reference pressure
!
!  METHOD:  2005 MM5 tutorial notes, page 7-8 and 7-9.
!
!  HISTORY: 04/12/07                                    Yong-Run Guo
!
!  PARENT_MODULE: module_recoverh.F90,...
!------------------------------------------------------------------------------

   IMPLICIT NONE

   INCLUDE 'constants.inc'

   REAL, PARAMETER  :: RGAS = gasr, &
                       GRAV = g

   REAL, INTENT(IN) :: pres
   REAL             :: height

   REAL             :: aa, bb, cc, dd
!-----------------------------------------------------------------------------!

     if ( pres >= pis0 ) then
! below tropopause:
     cc = alog(pres/ps0)
     bb = RGAS * ts0 / GRAV
     aa = RGAS * tlp / (2.*GRAV)

       height = -( bb * cc + aa * cc * cc)
     else
! above tropopause
       cc = alog(pres/pis0)
       bb = RGAS * tis0 /GRAV

       height = h_tropo - bb * cc
     endif

END FUNCTION Ref_height

FUNCTION ref_pres (height) Result (pres)
!------------------------------------------------------------------------------!
!  PURPOSE: To calculate the reference pressure from the height.
!
!  METHOD:  2005 MM5 tutorial notes, page 7-8 and 7-9.
!
!  HISTORY: 04/12/07                                    Yong-Run Guo
!
!  PARENT_MODULE: module_recoverp.F90,...
!------------------------------------------------------------------------------!

   IMPLICIT NONE

   INCLUDE 'constants.inc'

   REAL, PARAMETER :: RGAS = gasr, &
                      GRAV = g

   REAL, INTENT(IN) :: height
   REAL             :: pres

   REAL             :: aa, bb, dd, hh
!------------------------------------------------------------------------------!

   IF (htop .GT. 0.) THEN
       hh = min (htop, height)
   ELSE
       hh = height
   ENDIF

   if ( hh <= h_tropo ) then
   bb = RGAS * ts0 / GRAV
   aa = RGAS * tlp / (2.*GRAV)
! below the tropopause:
     dd = (-bb + sqrt(bb * bb - 4.0 * aa * hh))/(2.0*aa)
     pres = ps0 * exp( dd )

   else
! above the tropopause:
     bb = RGAS * tis0 / GRAV
     pres = pis0 * exp( (h_tropo-hh) / bb )

   endif

END FUNCTION ref_pres

!==============================================================================!
END MODULE module_mm5
