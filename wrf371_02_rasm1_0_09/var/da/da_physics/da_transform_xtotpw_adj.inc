subroutine da_transform_xtotpw_adj(grid)

   !---------------------------------------------------------------------
   ! Purpose: TBD
   !---------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout) :: grid

   integer :: i, j, k, is, js, ie, je

   real    :: pw, dzpw

   if (trace_use) call da_trace_entry("da_transform_xtotpw_adj")

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

   do j=js, je
      do i=is, ie
         pw = 0.1*grid%xa%tpw(i,j)

         do k=kts, kte
            dzpw = (grid%xb%hf(i,j,k+1)-grid%xb%hf(i,j,k))*pw

            grid%xa%  q(i,j,k)=grid%xa%  q(i,j,k)+dzpw*grid%xb%rho(i,j,k)
            grid%xa%rho(i,j,k)=grid%xa%rho(i,j,k)+dzpw*grid%xb%  q(i,j,k)
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_transform_xtotpw_adj")
 
end subroutine da_transform_xtotpw_adj


