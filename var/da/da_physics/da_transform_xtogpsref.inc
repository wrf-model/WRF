subroutine da_transform_xtogpsref(grid)

   !-------------------------------------------------------------------
   ! Purpose: TBD
   !-------------------------------------------------------------------

   implicit none

   ! input : grid%xb%q, grid%xb%p, grid%xb%t, and xp
   ! output: grid%xb%ref

   type (domain), intent(inout) :: grid
   
   integer :: i, j, k, ij
   real    :: partone, parttwo, dividnd

   if (trace_use_dull) call da_trace_entry("da_transform_xtogpsref")

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, i, j, k, partone, dividnd, parttwo)
   do ij = 1 , grid%num_tiles

   do k = kts, kte
      do j = grid%j_start(ij), grid%j_end(ij)
         do i = its, ite
            ! calculate refractivity

            !  1, Hydrostatic part of refractivity:
            !     Note: p in Pascal.

            partone  = 0.776*grid%xb%p(i,j,k)/grid%xb%t(i,j,k)

            !  2, (Wet part) / (hydrostatic part):
            !     Note: grid%xb%q its the specific humidity --- an analysis variable

            dividnd  = grid%xb%t(i,j,k)*(0.622+0.378*grid%xb%q(i,j,k))
            parttwo  = 1.0+coeff*grid%xb%q(i,j,k)/dividnd

            !  3, Refractivity:
            grid%xb%ref(i,j,k)= partone * parttwo
            grid%xb%reflog(i,j,k)= log(grid%xb%ref(i,j,k))
         end do
      end do
   end do

   end do
   !$OMP END PARALLEL DO

   if (trace_use_dull) call da_trace_exit("da_transform_xtogpsref")
   
end subroutine da_transform_xtogpsref


