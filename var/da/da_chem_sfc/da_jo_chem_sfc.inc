subroutine da_jo_chem_sfc(iv, re, jo_grad_y, jo)

   !-----------------------------------------------------------------------
   ! Purpose: Ensures that exactly the same code is used for both
   ! serial and parallel computations when testing_dm_bitwise_exact is set.
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv         ! Innovation vector.
   type (y_type),  intent(in)    :: re         ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y  ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo         ! Obs cost function.

   integer :: n,ichem

   if (trace_use_dull) call da_trace_entry("da_jo_chem_sfc")

   jo % chemic_surf = 0.0

   do ichem = PARAM_FIRST_SCALAR, num_chemic_surf
   do n=1, iv%info(chemic_surf)%nlocal
      if (iv%info(chemic_surf)%proc_domain(1,n)) then
         jo % chemic_surf = jo % chemic_surf - re%chemic_surf(n)%chem(ichem) * jo_grad_y%chemic_surf(n)%chem(ichem)
     end if
   end do
   end do

   if (trace_use_dull) call da_trace_exit("da_jo_chem_sfc")

end subroutine da_jo_chem_sfc


