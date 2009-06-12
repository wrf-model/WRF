SUBROUTINE da_transform_bal_adj( vp, be, grid )

   IMPLICIT NONE

   TYPE (vp_type), INTENT(INOUT)        :: vp ! CV on grid structure.out
   TYPE (be_type), INTENT(IN)           :: be ! Background errors.
   type (domain) , intent(inout) :: grid   ! Dimensions and xpose buffers.

   INTEGER                              :: i, j, k, kk, ij, iunit  ! Loop counters.
!-------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------- 
  
!  linear balance btw psi and t-b, Psfc_b and chi_b 


!  [3.4]  adj of Transform psi and chi to u and v:        

#ifdef DM_PARALLEL
#include "HALO_PSICHI_UV_ADJ.inc"
#endif

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( i, j, k, ij )
   DO ij = 1 , grid%num_tiles
      DO k = kts,kte
         DO j = grid%j_start(ij), grid%j_end(ij)
            DO i= its,ite
               vp%v1(i,j,k)=0.
               vp%v2(i,j,k)=0.
            END DO
         END DO
      END DO
   ENDDO
   !$OMP END PARALLEL DO

   call da_psichi_to_uv_adj( grid%xa % u, grid%xa % v, grid%xb % coefx,   &
                            grid%xb % coefy,  vp % v1, vp % v2  )

!  [3.3]  adj Calculate Psfc_b from psi

!--convert from delt.ps to delt.ln(ps)
   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( i, j, ij )
   DO ij = 1 , grid%num_tiles
      DO j = grid%j_start(ij), grid%j_end(ij)
         DO i= its,ite
            grid%xa%psfc(i,j) = grid%xa%psfc(i,j) * grid%xb%psfc(i,j)
         END DO
      END DO
   END DO
   !$OMP END PARALLEL DO

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( i, j, k, ij )
   DO ij = 1 , grid%num_tiles
      DO k = kts,kte
         DO j = grid%j_start(ij), grid%j_end(ij)
            DO i= its,ite
               vp % v1(i,j,k)= vp % v1(i,j,k)+ &
                                be % wgvz(i,j,k) * grid%xa % psfc(i,j)
            END DO
         END DO
      END DO
   END DO
   !$OMP END PARALLEL DO

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( i, j, ij )
   DO ij = 1 , grid%num_tiles
      DO j = grid%j_start(ij), grid%j_end(ij)
         DO i= its,ite
            vp % v5(i,j,1)=grid%xa % psfc(i,j)
         END DO
      END DO
   END DO
   !$OMP END PARALLEL DO

!  [3.2] adj Calculate chi_b from psi

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( i, j, k, ij )
   DO ij = 1 , grid%num_tiles
      DO k = kts,kte
         DO j = grid%j_start(ij), grid%j_end(ij)
            DO i= its,ite
               vp % v1(i,j,k)= vp % v1(i,j,k)+ &
                         be % bvz(i,j,k) * vp % v2(i,j,k) 
            END DO
         END DO
      END DO
   END DO
   !$OMP END PARALLEL DO

!  [3.1] Calculate t_b from psi

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( i, j, k, kk, ij )
   DO ij = 1 , grid%num_tiles
      DO kk = kts,kte
         DO k = kts,kte
            DO j = grid%j_start(ij), grid%j_end(ij)
               DO i= its,ite
                  vp % v1(i,j,kk)= vp % v1(i,j,kk)+ &
                          be % agvz(i,j,k,kk) * grid%xa % t(i,j,k)     
               END DO
            END DO
         END DO
      END DO
   END DO
   !$OMP END PARALLEL DO

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( i, j, k, ij )
   DO ij = 1 , grid%num_tiles
      DO k = kts,kte
         DO j = grid%j_start(ij), grid%j_end(ij)
            DO i= its,ite
               vp % v3(i,j,k)= grid%xa % t(i,j,k)
            END DO
         END DO
      END DO
   END DO
   !$OMP END PARALLEL DO

!  [3.5] treat humidity                         

   IF ( cv_options == 3 ) THEN
   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( i, j, k, ij )
   DO ij = 1 , grid%num_tiles
      DO k = kts,kte
         DO j = grid%j_start(ij), grid%j_end(ij)
            DO i= its,ite
               vp % v4(i,j,k) = grid%xa % q(i,j,k) * grid%xb % qs(i,j,k)
            END DO
         END DO
      END DO
   END DO
   !$OMP END PARALLEL DO

   ELSE IF ( cv_options_hum == 1 ) THEN

      vp % v4(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte)+&
                                         grid%xa % q(its:ite,jts:jte,kts:kte)

   ELSE IF ( cv_options_hum == 2 ) THEN

      CALL DA_TPRH_To_Q_Adj( grid )

      vp % v4(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte)+&
                                         grid%xa % rh(its:ite,jts:jte,kts:kte)

   END IF

END SUBROUTINE da_transform_bal_adj   

