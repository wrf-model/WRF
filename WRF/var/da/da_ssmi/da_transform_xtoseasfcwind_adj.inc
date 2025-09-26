subroutine da_transform_xtoseasfcwind_adj(grid)

   !-------------------------------------------------------------------------
   ! Purpose: Convert (U-V in m/s) components into wind speed (Speed in m/s)
   !-------------------------------------------------------------------------

   implicit none

   type (domain), intent(inout) :: grid

   real    :: const, rgh_fac, var, height
   integer :: i, j, is, js, ie, je

   if (trace_use) call da_trace_entry("da_transform_xtoseasfcwind_adj")

   const = log(10./0.0001)

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
        height = grid%xb%h(i,j,kts) - grid%xb%terr(i,j)
         if (height <= 0.0) then
            message(1) = "Negative height found"
            write(unit=message(2),FMT='(2I6,A,F10.2,A,F10.2)') &
               i,j,' ht = ',grid%xb%h(i,j,kts) ,' terr =  ',grid%xb%terr(i,j)
            call da_error(__FILE__,__LINE__,message(1:2))
         end if

         rgh_fac = const/log(height/0.0001) ! roughness = 0.0001

         var = rgh_fac*rgh_fac/grid%xb%speed(i,j)

         grid%xa%u(i,j,kts)=grid%xa%u(i,j,kts)+var*grid%xa%speed(i,j)*grid%xb%u(i,j,kts)
         grid%xa%v(i,j,kts)=grid%xa%v(i,j,kts)+var*grid%xa%speed(i,j)*grid%xb%v(i,j,kts)
      end do
   end do

   if (trace_use) call da_trace_exit("da_transform_xtoseasfcwind_adj")

end subroutine da_transform_xtoseasfcwind_adj


