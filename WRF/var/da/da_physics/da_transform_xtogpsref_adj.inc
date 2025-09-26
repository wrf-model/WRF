subroutine da_transform_xtogpsref_adj(grid)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! input: grid%xb%q, grid%xb%p, grid%xb%t, grid%xa%ref, and xp
   ! output: grid%xa%q, grid%xa%p, grid%xa%t
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout) :: grid

   integer :: i, j, k, is, ie, js, je, ks, ke
   real    :: partone, parttwo, dividnd
   real    :: partone9,parttwo9,dividnd9

   if (trace_use_dull) call da_trace_entry("da_transform_xtogpsref_adj")

   ! 1.0 Get the range for a tile

   is = its; ie = ite
   js = jts; je = jte
   ks = kts; ke = kte      

   if (test_transforms) then
      is = its-1
      js = jts-1

      ie = ite+1
      je = jte+1

      if (is < ids) is = ids
      if (js < jds) js = jds

      if (ie > ide) ie = ide
      if (je > jde) je = jde
   end if

   !------------------------------------------------------------------------------
   !  [2.0] Calculate the adjoint for GPS refractivity:
   !------------------------------------------------------------------------------

   do k = ks, ke
      do j = js, je
         do i = is, ie
            !  Note: p in Pascal, q is the specific humidity

            !   2.1 basic state

            partone9 = 0.776*grid%xb%p(i,j,k)/grid%xb%t(i,j,k)
            dividnd9 = grid%xb%t(i,j,k)*(0.622+0.378*grid%xb%q(i,j,k))
            parttwo9 = 1.0+coeff*grid%xb%q(i,j,k)/dividnd9

            !   2.2 Adjoint of partone and parttwo

            partone = grid%xa%ref(i,j,k) * parttwo9
            parttwo = grid%xa%ref(i,j,k) * partone9

            !   2.3 Adjoint of q and dividnd

            grid%xa%q(i,j,k) = grid%xa%q(i,j,k) + coeff*parttwo/dividnd9
            dividnd=-coeff*grid%xb%q(i,j,k)*parttwo/(dividnd9*dividnd9)

            !   2.4 Adjoint of t, q, and p

            grid%xa%t(i,j,k) = grid%xa%t(i,j,k) + dividnd*(0.622+0.378*grid%xb%q(i,j,k))
            grid%xa%q(i,j,k) = grid%xa%q(i,j,k) + grid%xb%t(i,j,k)*0.378*dividnd

! Zero out the REF impact on xa%p (YRG, 10/08/2009):
!            grid%xa%p(i,j,k) = grid%xa%p(i,j,k) + 0.776*partone/grid%xb%t(i,j,k)
            
            grid%xa%t(i,j,k) = grid%xa%t(i,j,k) - &
               0.776*grid%xb%p(i,j,k)*partone/(grid%xb%t(i,j,k)*grid%xb%t(i,j,k))

            !  [3.0] Clean the grid%xa%ref    

            grid%xa%ref(i,j,k) = 0.0
         end do
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtogpsref_adj")

end subroutine da_transform_xtogpsref_adj


