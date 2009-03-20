SUBROUTINE da_transform_bal( vp, be, grid )

   IMPLICIT NONE

   TYPE (vp_type), INTENT(INOUT)        :: vp ! work array.
   TYPE (be_type), INTENT(IN)           :: be ! Background errors.
   type (domain) , intent(inout)        :: grid   ! Domain variables.

   INTEGER                              :: i, j, k, kk  ! Loop counters.
   
!-------------------------------------------------------------------
!  [1.0] Initialise:
!-------------------------------------------------------------------
!
!  linear balance btw psi and t-b, Psfc_b and chi_b 
!  [3.1] Calculate t_b from psi

   grid%xa%t(its:ite,jts:jte,kts:kte)=vp%v3(its:ite,jts:jte,kts:kte)

   DO kk = kts,kte
   DO k = kts,kte
   DO j = jts,jte
   DO i= its,ite
      grid%xa%t(i,j,k) = grid%xa%t(i,j,k) + &
                           be%agvz(i,j,k,kk) * vp%v1(i,j,kk)
   END DO
   END DO
   END DO
   END DO

!  [3.2] Calculate chi_b from psi

      DO k = kts,kte
      DO j = jts,jte
      DO i= its,ite
         vp%v2(i,j,k) = vp%v2(i,j,k) + &
                          be%bvz(i,j,k) * vp%v1(i,j,k)
      END DO
      END DO
      END DO

!  [3.3] Calculate Psfc_b from psi

   grid%xa%psfc(its:ite,jts:jte)=vp%v5(its:ite,jts:jte,1)

   DO kk = kts,kte
   DO j = jts,jte
   DO i= its,ite
      grid%xa%psfc(i,j) = grid%xa%psfc(i,j) + &
                      be%wgvz(i,j,kk) * vp%v1(i,j,kk)
   END DO
   END DO
   END DO

!--convert from delt.ln(ps) to delt.ps
   grid%xa%psfc(its:ite,jts:jte) = grid%xa%psfc(its:ite,jts:jte) &
                            * grid%xb%psfc(its:ite,jts:jte)

!  [3.4] Transform psi and chi to u and v:

!  Communicate halo region.
#ifdef DM_PARALLEL
#include "HALO_PSICHI_UV.inc"
#endif

   call da_psichi_to_uv( vp%v1, vp%v2, grid%xb%coefx, &
                         grid%xb%coefy, grid%xa%u, grid%xa%v    )

!  [3.5] treat humidity                         


   IF ( cv_options == 3 ) THEN
      grid%xa%q(its:ite,jts:jte,kts:kte) = vp%v4(its:ite,jts:jte,kts:kte) * &
                                        grid%xb%qs(its:ite,jts:jte,kts:kte)
   ELSE IF ( cv_options_hum == 1 ) THEN

      grid%xa%q(its:ite,jts:jte,kts:kte) = vp%v4(its:ite,jts:jte,kts:kte)

   ELSE IF ( cv_options_hum == 2 ) THEN

      grid%xa%rh(its:ite,jts:jte,kts:kte) = vp%v4(its:ite,jts:jte,kts:kte)

      CALL DA_TPRH_To_Q_Lin( grid )

   END IF

END SUBROUTINE da_transform_bal   

