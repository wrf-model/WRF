subroutine da_calculate_grady_chem_acft(iv, re, jo_grad_y)

   !----------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Innovation vector.
   type (y_type),  intent(inout) :: re          ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)

   integer :: n, ichem, iplt
   
   if (trace_use) call da_trace_entry("da_calculate_grady_chem_acft")       

   do n=1, iv%info(chem_acft)%nlocal
      do ichem = 1, num_chem_acft-1
         do iplt = 1, num_platform
             if (iv%chem_acft(n)%chem(ichem,iplt)%qc < obs_qc_pointer) re%chem_acft(n)%chem(ichem,iplt) = 0.0
             jo_grad_y%chem_acft(n)%chem(ichem,iplt) = &
               -re%chem_acft(n)%chem(ichem,iplt) / (iv%chem_acft(n)%chem(ichem,iplt)%error * iv%chem_acft(n)%chem(ichem,iplt)%error)
         end do
      end do
   end do
   
   if (trace_use) call da_trace_exit("da_calculate_grady_chem_acft")  
     
end subroutine da_calculate_grady_chem_acft


