subroutine da_transform_xtoseasfcwind_lin(grid)

   !----------------------------------------------------------------------------
   ! Purpose: Convert (U-V in m/s) components into wind speed (Speed in m/s)
   !----------------------------------------------------------------------------

   implicit none

   type (domain), intent(inout) :: grid

   real    :: const, rgh_fac, height
   integer :: i, j, k

   if (trace_use) call da_trace_entry("da_transform_xtoseasfcwind_lin")

   const = log(10.0/0.0001)
   k = grid%xb%kts
    
   do j=jts,jte
      do i=its,ite

        height = grid%xb%h(i,j,k) - grid%xb%terr(i,j)
         if (height <= 0.0) then
            message(1) = "Negative height found"
            write(unit=message(2),FMT='(2I6,A,F10.2,A,F10.2)') &
               i,j,' ht = ',grid%xb%h(i,j,k) ,' terr =  ',grid%xb%terr(i,j)
            call da_error(__FILE__,__LINE__,message(1:2))
         end if

         rgh_fac = const/log(height/0.0001) ! roughness = 0.0001
         grid%xa%speed(i,j) = (grid%xa%u(i,j,k)*grid%xb%u(i,j,k) &
            + grid%xa%v(i,j,k)*grid%xb%v(i,j,k)) * rgh_fac*rgh_fac / grid%xb%speed(i,j)
      end do
   end do

   if (trace_use) call da_trace_exit("da_transform_xtoseasfcwind_lin")

end subroutine da_transform_xtoseasfcwind_lin


