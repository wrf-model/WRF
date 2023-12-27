subroutine da_calculate_grady_chem_sfc(iv, re, jo_grad_y)

   !-------------------------------------------------------------------------
   ! Purpose: Applies obs inverse on re-vector
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv          ! Innovation vector.
   type (y_type),  intent(inout)  :: re          ! Residual vector.
   type (y_type),  intent(inout)  :: jo_grad_y   ! Grad_y(Jo)

   integer                      :: n, ichem

   if (trace_use_dull) call da_trace_entry("da_calculate_grady_chem_sfc")

   do ichem = PARAM_FIRST_SCALAR ,num_chemic_surf
   do n=1, iv%info(chemic_surf)%nlocal
      if (iv%chemic_surf(n)%chem(ichem)%qc < obs_qc_pointer) re%chemic_surf(n)%chem(ichem) = 0.0

      jo_grad_y%chemic_surf(n)%chem(ichem) = -re%chemic_surf(n)%chem(ichem) / &
                                             (iv%chemic_surf(n)%chem(ichem)%error * iv%chemic_surf(n)%chem(ichem)%error)
   end do
   end do

   if (trace_use_dull) call da_trace_exit("da_calculate_grady_chem_sfc")
     
end subroutine da_calculate_grady_chem_sfc


