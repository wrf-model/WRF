subroutine da_pt_to_rho_adj(grid) 

   !---------------------------------------------------------------------------
   !  Purpose: Adjoint of da_pt_to_rho.
   !
   !  Assumptions: 1) Model level stored top down
   !---------------------------------------------------------------------------

   implicit none
   
   type (domain), intent(inout)  :: grid

   integer                       :: i,j,k        ! Loop counter.

   integer                       :: is, ie, js, je

   real                          :: temp

   if (trace_use) call da_trace_entry("da_pt_to_rho_adj")
   
   is = its
   js = jts

   ie = ite
   je = jte

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

   !---------------------------------------------------------------------------
   ! Calculate rho increments:
   !---------------------------------------------------------------------------

   do j=js, je
      do k=kts, kte
         do i=is, ie
            temp = grid%xa%rho(i,j,k) * grid%xb%rho(i,j,k)

            grid%xa%p(i,j,k) = grid%xa%p(i,j,k) + temp/grid%xb%p(i,j,k)
                           
            grid%xa%t(i,j,k) = grid%xa%t(i,j,k) - temp/grid%xb%t(i,j,k)

            grid%xa%rho(i,j,k) = 0.0
         end do        
      end do        
   end do        

   if (trace_use) call da_trace_exit("da_pt_to_rho_adj")

end subroutine da_pt_to_rho_adj


