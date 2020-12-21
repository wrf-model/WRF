subroutine da_calculate_grady_chem_surf(iv, re, jo_grad_y)

   !----------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(inout) :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)

   integer :: n, ichem
   
   if (trace_use) call da_trace_entry("da_calculate_grady_chem_surf")       

   do n=1, iv%info(chem_surf)%nlocal
      do ichem = 1, num_chem_surf-1
             if (iv%chem_surf(n)%chem(ichem)%qc < obs_qc_pointer) re%chem_surf(n)%chem(ichem) = 0.0
             jo_grad_y%chem_surf(n)%chem(ichem) = &
               -re%chem_surf(n)%chem(ichem) / (iv%chem_surf(n)%chem(ichem)%error * iv%chem_surf(n)%chem(ichem)%error)
      end do
   end do
   
   if (trace_use) call da_trace_exit("da_calculate_grady_chem_surf")  
     
end subroutine da_calculate_grady_chem_surf


