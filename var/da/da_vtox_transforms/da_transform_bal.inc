SUBROUTINE da_transform_bal( vp, be, grid )

   IMPLICIT NONE

   TYPE (vp_type), INTENT(INOUT)        :: vp ! work array.
   TYPE (be_type), INTENT(IN)           :: be ! Background errors.
   type (domain) , intent(inout)        :: grid   ! Domain variables.

   INTEGER                              :: i, j, k, kk, ij  ! Loop counters.
   
!-------------------------------------------------------------------
!  [1.0] Initialise:
!-------------------------------------------------------------------
!
!  linear balance btw psi and t-b, Psfc_b and chi_b 
!  [3.1] Calculate t_b from psi

   !$OMP PARALLEL DO &
   !$OMP PRIVATE (i, j, k, ij)
   DO ij = 1, grid%num_tiles
      DO k = kts,kte
         DO j = grid%j_start(ij), grid%j_end(ij)
            DO i= its,ite
               grid%xa%t(i,j,k)=vp%v3(i,j,k)
            END DO
         END DO
      END DO
   END DO
   !$OMP END PARALLEL DO

   !$OMP PARALLEL DO &
   !$OMP PRIVATE (i, j, k, kk, ij)
   DO ij = 1, grid%num_tiles
      DO kk = kts,kte
         DO k = kts,kte
            DO j = grid%j_start(ij), grid%j_end(ij)
               DO i= its,ite
                  grid%xa%t(i,j,k) = grid%xa%t(i,j,k) + &
                                     be%agvz(i,j,k,kk) * vp%v1(i,j,kk)
               END DO
            END DO
         END DO
      END DO
   END DO
   !$OMP END PARALLEL DO

!  [3.2] Calculate chi_b from psi

   !$OMP PARALLEL DO &
   !$OMP PRIVATE (i, j, k, ij)
   DO ij = 1, grid%num_tiles
      DO k = kts,kte
         DO j = grid%j_start(ij), grid%j_end(ij)
            DO i= its,ite
               vp%v2(i,j,k) = vp%v2(i,j,k) + &
                              be%bvz(i,j,k) * vp%v1(i,j,k)
            END DO
         END DO
      END DO
   END DO
   !$OMP END PARALLEL DO

!  [3.3] Calculate Psfc_b from psi

   !$OMP PARALLEL DO &
   !$OMP PRIVATE (i, j, ij)
   DO ij = 1, grid%num_tiles
      DO j = grid%j_start(ij), grid%j_end(ij)
         DO i= its,ite
            grid%xa%psfc(i,j)=vp%v5(i,j,1)
         END DO
      END DO
   END DO
   !$OMP END PARALLEL DO


   !$OMP PARALLEL DO &
   !$OMP PRIVATE (i, j, k, ij)
   DO ij = 1, grid%num_tiles
      DO k = kts,kte
         DO j = grid%j_start(ij), grid%j_end(ij)
            DO i= its,ite
               grid%xa%psfc(i,j) = grid%xa%psfc(i,j) + &
                                   be%wgvz(i,j,k) * vp%v1(i,j,k)
            END DO
         END DO
      END DO
   END DO
   !$OMP END PARALLEL DO

!--convert from delt.ln(ps) to delt.ps
   !$OMP PARALLEL DO &
   !$OMP PRIVATE (i, j, ij)
   DO ij = 1, grid%num_tiles
      DO j = grid%j_start(ij), grid%j_end(ij)
         DO i= its,ite
            grid%xa%psfc(i,j) = grid%xa%psfc(i,j) * grid%xb%psfc(i,j) 
         END DO
      END DO
   END DO
   !$OMP END PARALLEL DO

!  [3.4] Transform psi and chi to u and v:

!  Communicate halo region.
#ifdef DM_PARALLEL
#include "HALO_PSICHI_UV.inc"
#endif

   call da_psichi_to_uv( vp%v1, vp%v2, grid%xb%coefx, &
                         grid%xb%coefy, grid%xa%u, grid%xa%v    )

!  [3.5] treat humidity                         


   IF ( cv_options == 3 ) THEN
   !$OMP PARALLEL DO &
   !$OMP PRIVATE (i, j, k, ij)
   DO ij = 1, grid%num_tiles
      DO k = kts,kte
         DO j = grid%j_start(ij), grid%j_end(ij)
            DO i= its,ite
               grid%xa%q(i,j,k) = vp%v4(i,j,k) * grid%xb%qs(i,j,k)
            END DO
         END DO
      END DO
   END DO
   !$OMP END PARALLEL DO
   ELSE IF ( cv_options_hum == 1 ) THEN

      grid%xa%q(its:ite,jts:jte,kts:kte) = vp%v4(its:ite,jts:jte,kts:kte)

   ELSE IF ( cv_options_hum == 2 ) THEN

      grid%xa%rh(its:ite,jts:jte,kts:kte) = vp%v4(its:ite,jts:jte,kts:kte)

      CALL DA_TPRH_To_Q_Lin( grid )

   END IF

END SUBROUTINE da_transform_bal   

